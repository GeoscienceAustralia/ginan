use std::sync::atomic::Ordering;
use tauri::Emitter;
use crate::state::PppScanGen;
use super::*;
use super::naming::*;
use super::http::*;

use super::catalogue_parse::*;

pub(crate) fn make_absolute(base: &str, location: &str) -> String {
    if location.starts_with("http://") || location.starts_with("https://") {
        return location.to_string();
    }
    // Scheme + host from base (e.g. "https://cddis.nasa.gov")
    let scheme_host: String = base.splitn(4, '/').take(3).collect::<Vec<_>>().join("/");
    if location.starts_with('/') {
        format!("{scheme_host}{location}")
    } else {
        let base_dir = base.rsplit_once('/').map_or(base, |(dir, _)| dir);
        format!("{base_dir}/{location}")
    }
}

/// Fetch an HTTP directory listing and return all recognisable product filenames.
///
/// Redirects are followed manually so that `authed_get` can re-attach
/// per-host credentials at every hop. reqwest strips `Authorization` on
/// cross-domain redirects (security), which breaks the CDDIS → URS →
/// CDDIS Earthdata OAuth flow: without credentials at the URS hop the
/// server returns a login-page HTML instead of a directory listing.
pub(crate) async fn fetch_directory_listing(
    client: &reqwest::Client,
    url: &str,
) -> Result<Vec<String>, String> {
    let mut current_url = url.to_string();
    for _ in 0..10 {
        let resp = authed_get(client, &current_url)
            .send()
            .await
            .map_err(|e| {
                if e.is_connect() || e.is_timeout() {
                    format!("connection error: {e}")
                } else {
                    format!("{e}")
                }
            })?;

        let status = resp.status();
        if status.is_redirection() {
            let location = resp
                .headers()
                .get("location")
                .and_then(|v| v.to_str().ok())
                .ok_or_else(|| "redirect with no Location header".to_string())?
                .to_string();
            current_url = redirect_target(&current_url, &location);
            continue;
        }
        if !status.is_success() {
            return Err(format!("HTTP {status}"));
        }
        let html = resp.text().await.map_err(|e| format!("read body: {e}"))?;
        return Ok(parse_directory_listing_html(&html));
    }
    Err("too many redirects".to_string())
}

/// Scan the mirrors for the observation window and return the catalogue of
/// (provider, series, project) combinations that actually have products.
///
/// Fetches the GPS-week directory listing from CDDIS — the only product server,
/// and the source the Python `get_product_dataframe()` uses.
/// A single GET instead of 192 HEAD requests; also immune to filename-guessing
/// errors. An unauthenticated request gets CDDIS's login page, which parses to
/// an empty catalogue and signals that credentials are needed.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn scan_ppp_catalogue(
    start_epoch: String,
    end_epoch: String,
    app: tauri::AppHandle,
) -> Result<PppCatalogue, String> {
    let _ = end_epoch;
    let date = parse_date(&start_epoch)?;
    let gps_week = gps_week(date);

    let gen_arc = {
        use tauri::Manager;
        app.state::<PppScanGen>().0.clone()
    };
    let my_gen = gen_arc.fetch_add(1, Ordering::SeqCst) + 1;

    // 30 s timeout: a full directory listing can be larger than a HEAD response.
    // Redirects are disabled so fetch_directory_listing can follow them manually
    // and re-attach per-host credentials at each hop (needed for CDDIS→URS auth).
    let client = build_client_with_timeout(30)?;

    // CDDIS is the only catalogue source. Note that a CDDIS auth-redirect page
    // returns a few navigation links that pass the filename filter but none
    // parse as IGS long-name products; that case surfaces as an empty
    // catalogue and the caller is expected to prompt for Earthdata credentials.
    let mirrors: &[(&str, &str)] = &[("CDDIS", CDDIS_PRODUCTS)];

    // Emit to the console only when this is still the active scan.
    let emit_line = |msg: String| {
        if gen_arc.load(Ordering::Relaxed) == my_gen {
            let _ = app.emit("console-line", msg);
        }
    };
    // Update the status bar; empty string clears it back to idle.
    let emit_status = |msg: String| {
        if gen_arc.load(Ordering::Relaxed) == my_gen {
            let _ = app.emit("ppp-scan-progress", msg);
        }
    };

    emit_line(format!("Scanning GPS week {gps_week} for PPP products..."));

    let mut all_filenames: Vec<String> = Vec::new();

    for (mirror_name, base_url) in mirrors {
        if gen_arc.load(Ordering::Relaxed) != my_gen {
            return Ok(PppCatalogue { entries: vec![] });
        }

        let dir_url = format!("{base_url}/{gps_week:04}/");
        // Show the current server in the status bar instead of a fraction counter.
        emit_status(format!("Fetching from {mirror_name}..."));

        match fetch_directory_listing(&client, &dir_url).await {
            Ok(files) => {
                // Only stop here if the listing contains actual parseable IGS product
                // files. A CDDIS auth-redirect login page returns navigation links that
                // pass `is_product_filename` but won't parse as IGS long-name files.
                let has_products = files.iter().any(|f| parse_igs_filename(f).is_some());
                if has_products {
                    emit_line(format!("  {mirror_name}: {} files", files.len()));
                    all_filenames = files;
                    break;
                }
                emit_line(format!("  {mirror_name}: no product files, trying next mirror"));
            }
            Err(e) => {
                emit_line(format!("  {mirror_name}: {e}, trying next mirror"));
            }
        }
    }

    if gen_arc.load(Ordering::SeqCst) != my_gen {
        return Ok(PppCatalogue { entries: vec![] });
    }

    // Clear the status bar.
    emit_status(String::new());

    if all_filenames.is_empty() {
        emit_line("  All mirrors unreachable -- no PPP products found.".into());
        return Ok(PppCatalogue { entries: vec![] });
    }

    let catalogue = build_catalogue_from_filenames(&all_filenames);
    emit_line(format!(
        "PPP scan complete -- {} valid combinations found",
        catalogue.entries.len()
    ));

    for (i, entry) in catalogue.entries.iter().enumerate() {
        emit_line(format!(
            "  {i}. {}/{}/{}",
            entry.provider, entry.series, entry.project
        ));
    }

    Ok(catalogue)
}
