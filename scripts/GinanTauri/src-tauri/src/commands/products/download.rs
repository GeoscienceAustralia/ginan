use super::clock_sampling::*;
use super::layout::*;
use super::naming::*;
use super::http::*;
use chrono::{Datelike, NaiveDate};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tauri::Emitter;
use crate::state::CancelFlag;
use super::*;

// ─── static support files ────────────────────────────────────────────────────

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn ensure_products(
    products_dir: String,
    app: tauri::AppHandle,
    cancel: tauri::State<'_, CancelFlag>,
) -> Result<(), String> {
    let cancel_flag = cancel.begin();

    let products = PathBuf::from(&products_dir);
    std::fs::create_dir_all(&products)
        .map_err(|e| format!("Cannot create products dir {}: {e}", products.display()))?;
    announce_moved(&app, migrate_flat_layout(&products));
    let root = products.join(STATIC_DIR);

    let missing: Vec<&str> = STATIC_PRODUCTS
        .iter()
        .copied()
        .filter(|rel| !root.join(rel).is_file())
        .collect();

    if missing.is_empty() {
        app.emit("console-line", "All static support files present.")
            .ok();
        return Ok(());
    }

    app.emit(
        "console-line",
        format!("Downloading {} missing support file(s)...", missing.len()),
    )
    .ok();

    let client = build_client()?;
    for rel in missing {
        if cancel_flag.load(Ordering::SeqCst) {
            return Err("Cancelled".to_string());
        }
        let url = format!("{PEANPOD_SERVER}/{rel}.gz");
        let dest = root.join(rel);
        download_and_gunzip(&client, &url, &dest, rel, &app, &cancel_flag).await?;
    }

    app.emit("console-line", "✅ Support files ready.").ok();
    Ok(())
}

// ─── date-dependent products (orbits, clocks, nav, biases) ──────────────────

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
#[allow(clippy::too_many_arguments)] // argument names form the frontend invoke contract
pub async fn ensure_dynamic_products(
    products_dir: String,
    start_epoch: String,
    end_epoch: String,
    provider: Option<String>,
    series: Option<String>,
    project: Option<String>,
    preferred_mirror: Option<String>,
    catalogue_entry: Option<PppCatalogueEntry>,
    rinex_interval_s: Option<f64>,
    app: tauri::AppHandle,
    cancel: tauri::State<'_, CancelFlag>,
) -> Result<u32, String> {
    // Stop is shared with realtime PEA runs. Clear the previous run's signal
    // before checking products for this new file-mode operation.
    let cancel_flag = cancel.begin();

    let root = PathBuf::from(&products_dir);
    std::fs::create_dir_all(&root)
        .map_err(|e| format!("Cannot create products dir {}: {e}", root.display()))?;
    announce_moved(&app, migrate_flat_layout(&root));
    // Every folder the config names must exist before PEA starts, even when
    // nothing lands in it (a week with no 07D product): PEA logs an error for
    // each missing one.
    for folder in window_folders(&start_epoch, &end_epoch) {
        std::fs::create_dir_all(root.join(&folder))
            .map_err(|e| format!("Cannot create {folder}: {e}"))?;
    }

    let start_date = parse_date(&start_epoch)?;
    let end_date = if end_epoch.trim().is_empty() {
        start_date
    } else {
        parse_date(&end_epoch)?
    };

    let selection = PppSelection::from_options(provider, series, project);
    let mirror = preferred_mirror
        .map(|m| m.trim().to_uppercase())
        .filter(|m| !m.is_empty())
        .unwrap_or_else(|| "CDDIS".to_string());

    app.emit(
        "console-line",
        format!(
            "Using PPP source: {} / {} / {} (server: {})",
            selection.provider, selection.series, selection.project, mirror
        ),
    )
    .ok();

    // GinanUI announces the size of the job before it starts (dl_products.py::
    // "{n} files to check or download"). Each day contributes an orbit, a
    // clock, a bias table and a broadcast nav file, present or not — this is
    // the number of files the loop below checks, not the number it fetches.
    let days = (end_date - start_date).num_days() + 1;
    app.emit(
        "product-line",
        format!("📦 {} files to check or download", days.max(0) * 4),
    )
    .ok();

    let client = build_client()?;
    // One CDDIS listing per GPS week, fetched once, so only files that exist
    // are requested. `None` (listing unreadable) falls back to trying every
    // candidate name.
    let mut listings: HashMap<u32, Option<HashSet<String>>> = HashMap::new();
    let mut missing = 0u32;
    let mut day = start_date;
    while day <= end_date {
        if cancel_flag.load(Ordering::SeqCst) {
            return Err("Cancelled".to_string());
        }
        let week = gps_week(day);
        if !listings.contains_key(&week) {
            let listing = week_listing(&client, week).await;
            if listing.is_none() {
                app.emit(
                    "console-line",
                    format!("  Could not list GPS week {week} on CDDIS; trying the usual file names."),
                )
                .ok();
            }
            listings.insert(week, listing);
        }
        missing += download_day(
            &client, &root, day, &selection, &mirror, catalogue_entry.as_ref(),
            listings.get(&week).and_then(Option::as_ref),
            rinex_interval_s, &app, &cancel_flag,
        )
        .await?;
        day = match day.succ_opt() {
            Some(d) => d,
            None => break,
        };
    }

    app.emit("console-line", "✅ Date-dependent products ready.")
        .ok();
    Ok(missing)
}

/// File names in CDDIS's directory for `week`, or `None` when the listing
/// cannot be read or holds no products (for example a login page).
async fn week_listing(client: &reqwest::Client, week: u32) -> Option<HashSet<String>> {
    let url = format!("{CDDIS_PRODUCTS}/{week:04}/");
    let files = super::catalogue_fetch::fetch_directory_listing(client, &url).await.ok()?;
    files
        .iter()
        .any(|f| super::catalogue_parse::parse_igs_filename(f).is_some())
        .then(|| files.into_iter().collect())
}

/// True when `url`'s file is in `listing` (with or without `.gz`), or when
/// there is no listing to check against.
pub(crate) fn listed(listing: Option<&HashSet<String>>, url: &str) -> bool {
    let Some(listing) = listing else { return true };
    let name = url.rsplit('/').next().unwrap_or(url);
    listing.contains(name) || listing.contains(name.trim_end_matches(".gz"))
}

/// Say once what `migrate_flat_layout` moved, so files leaving the products
/// root are not a surprise.
fn announce_moved(app: &tauri::AppHandle, moved: Vec<String>) {
    if !moved.is_empty() {
        app.emit(
            "console-line",
            format!("Moved {} product file(s) into the daily/weekly/static folders.", moved.len()),
        )
        .ok();
    }
}

/// Provider/series/project combination used to construct PPP product filenames.
#[derive(Debug, Clone)]
struct PppSelection {
    provider: String,
    series: String,
    project: String,
}

impl PppSelection {
    fn from_options(
        provider: Option<String>,
        series: Option<String>,
        project: Option<String>,
    ) -> Self {
        Self {
            provider: non_empty(provider).unwrap_or_else(|| "IGS".to_string()),
            series: non_empty(series).unwrap_or_else(|| "FIN".to_string()),
            project: non_empty(project).unwrap_or_else(|| "OPS".to_string()),
        }
    }
}

fn non_empty(s: Option<String>) -> Option<String> {
    s.map(|v| v.trim().to_string()).filter(|v| !v.is_empty())
}

/// Base URLs to try, in order. CDDIS is the only PPP product source, so
/// `preferred` (the persisted UI mirror setting) does not influence the
/// ordering.
pub(crate) fn mirror_order(_preferred: &str) -> [&'static str; 1] {
    [CDDIS_PRODUCTS]
}

/// Label of the clock group in `download_day`; also what the log says.
const CLOCK_KIND: &str = "clock (CLK)";

#[allow(clippy::too_many_arguments)]
async fn download_day(
    client: &reqwest::Client,
    root: &Path,
    date: NaiveDate,
    selection: &PppSelection,
    preferred_mirror: &str,
    catalogue_entry: Option<&PppCatalogueEntry>,
    listing: Option<&HashSet<String>>,
    rinex_interval_s: Option<f64>,
    app: &tauri::AppHandle,
    cancel_flag: &Arc<AtomicBool>,
) -> Result<u32, String> {
    let year = date.year();
    let doy = date.ordinal();
    let gps_week = gps_week(date);

    let ac = selection.provider.as_str();
    let project = selection.project.as_str();
    let primary = selection.series.as_str();
    // Fall back to Rapid for SP3/CLK when the primary series isn't Final/Rapid itself.
    let secondary = match primary {
        "FIN" => "RAP",
        "RAP" => "FIN",
        _ => "FIN",
    };

    // Skip series that aren't published yet for this observation date.
    let primary_ok = series_is_published(primary, date);
    let secondary_ok = series_is_published(secondary, date);
    if !primary_ok {
        app.emit(
            "product-line",
            format!("⏭ Skipping {primary} for {date}: not published yet."),
        )
        .ok();
    }
    if !secondary_ok {
        app.emit(
            "product-line",
            format!("⏭ Skipping {secondary} for {date}: not published yet."),
        )
        .ok();
    }

    // Restrict mirror order to the subset that hosts this provider.
    let allowed = provider_mirrors(ac);
    let mirrors: Vec<&'static str> = mirror_order(preferred_mirror)
        .iter()
        .copied()
        .filter(|base| allowed.iter().any(|name| *name == mirror_name(base)))
        .collect();

    // Build URLs for a filename across all allowed mirrors, keeping only the
    // ones the week's listing has (or that are already on disk), so a series
    // the provider does not publish is never requested.
    let urls_for = |filename: &str| -> Vec<String> {
        mirrors
            .iter()
            .map(|m| product_url(m, gps_week, filename))
            .filter(|u| listed(listing, u) || product_path(root, url_basename(u)).is_file())
            .collect()
    };

    // Primary filenames come from the catalogue when available
    // (correct sample rate guaranteed). Secondary fallbacks still use the
    // long-name template since the catalogue only carries the user-selected
    // series.
    let mut sp3_urls: Vec<String> = Vec::new();
    if primary_ok {
        let sp3_primary = filename_for("ORB", "SP3", catalogue_entry, ac, project, primary, year, doy);
        sp3_urls.extend(urls_for(&sp3_primary));
    }
    if secondary_ok {
        let sp3_secondary = long_name(ac, project, secondary, year, doy, "01D", "15M", "ORB", "SP3");
        sp3_urls.extend(urls_for(&sp3_secondary));
    }

    // A RINEX file sampled under 30 s gets the 05S clock when one is published
    // (see clock_sampling.rs). Only the primary series is tried at 05S: that is
    // the one the user chose, and the secondary is a fallback for availability.
    let fine_clock = primary_ok
        .then(|| fine_clock_to_try(rinex_interval_s, catalogue_entry, ac, project, primary, year, doy))
        .flatten();
    let fine_clk_urls: Vec<String> = fine_clock
        .as_ref()
        .map(|f| urls_for(&f.filename))
        .unwrap_or_default();

    let mut clk_urls: Vec<String> = Vec::new();
    if primary_ok {
        let clk_primary = filename_for("CLK", "CLK", catalogue_entry, ac, project, primary, year, doy);
        clk_urls.extend(urls_for(&clk_primary));
    }
    if secondary_ok {
        let clk_secondary = long_name(ac, project, secondary, year, doy, "01D", "05M", "CLK", "CLK");
        clk_urls.extend(urls_for(&clk_secondary));
    }

    // OSB (observable-specific biases): the chosen series first (some
    // providers, such as GFZ MGX, publish them with their rapid products),
    // then FIN.
    let mut bia_urls: Vec<String> = Vec::new();
    if primary_ok {
        let bia_primary = filename_for("OSB", "BIA", catalogue_entry, ac, project, primary, year, doy);
        bia_urls.extend(urls_for(&bia_primary));
    }
    if primary != "FIN" && series_is_published("FIN", date) {
        let bia_fin = filename_for("OSB", "BIA", catalogue_entry, ac, project, "FIN", year, doy);
        bia_urls.extend(urls_for(&bia_fin));
    }

    let product_groups: &[(&str, Vec<String>)] = &[
        ("orbit (SP3)", sp3_urls),
        (CLOCK_KIND, clk_urls),
        ("bias (BIA)", bia_urls),
    ];

    let mut missing = 0u32;
    for (kind, urls) in product_groups {
        if cancel_flag.load(Ordering::SeqCst) {
            return Err("Cancelled".to_string());
        }
        if urls.is_empty() {
            missing += 1;
            app.emit(
                "product-line",
                format!("⚠ No {kind} from {ac} {project} for {date} on CDDIS, so none was downloaded."),
            )
            .ok();
            continue;
        }
        let is_clock = *kind == CLOCK_KIND;
        if let Some(fine) = fine_clock.as_ref().filter(|_| is_clock) {
            if fetch_fine_clock(client, root, &fine_clk_urls, fine, &date, app, cancel_flag).await? {
                continue;
            }
        }
        let have = try_download_first_available(client, root, urls, kind, &date, app, cancel_flag).await;
        if have.is_none() && !cancel_flag.load(Ordering::SeqCst) {
            missing += 1;
        }
        if let (true, Some(name)) = (is_clock, have) {
            // Also covers a stale 05S next to the 30S this run wants (interval
            // of 30 s or more, or unknown): one clock file per day either way.
            let path = product_path(root, &name);
            keep_single_clock(path.parent().unwrap_or(root), &name, app);
            if let Some(fine) = &fine_clock {
                announce_usual_clock(app, fine, &date, &name);
            }
        }
    }

    // BRDC nav: resolve via the GA REST API (returns a presigned S3 URL).
    let brdc_name = format!("BRDC00IGS_R_{year:04}{doy:03}0000_01D_MN.rnx");
    if !product_path(root, &brdc_name).is_file() {
        if cancel_flag.load(Ordering::SeqCst) {
            return Err("Cancelled".to_string());
        }
        match resolve_brdc_url(client, date, app).await {
            Ok(url) => {
                let dest = product_path(root, &brdc_name);
                if let Err(e) =
                    download_and_gunzip(client, &url, &dest, &brdc_name, app, cancel_flag).await
                {
                    missing += 1;
                    app.emit(
                        "product-line",
                        format!("⚠ BRDC for {date}: download failed, {e}"),
                    )
                    .ok();
                }
            }
            Err(e) => {
                missing += 1;
                app.emit(
                    "product-line",
                    format!("⚠ BRDC for {date}: GA API lookup failed, {e}"),
                )
                .ok();
            }
        }
    }

    Ok(missing)
}

/// Try each URL in order until one is in place, and return that file's name
/// (without `.gz`). A file already in `root` for any candidate counts, with no
/// download. `None` means nothing usable was fetched or the run was cancelled.
pub(crate) async fn try_download_first_available(
    client: &reqwest::Client,
    root: &Path,
    urls: &[String],
    kind: &str,
    date: &NaiveDate,
    app: &tauri::AppHandle,
    cancel_flag: &Arc<AtomicBool>,
) -> Option<String> {
    if let Some(name) = present_candidate(root, urls) {
        return Some(name);
    }
    let mut last_err = String::new();
    for url in urls {
        if cancel_flag.load(Ordering::SeqCst) {
            return None;
        }
        let fname = url_basename(url);
        let dest = product_path(root, fname);
        match download_and_gunzip(client, url, &dest, fname, app, cancel_flag).await {
            Ok(_) => return Some(fname.to_string()),
            Err(e) => {
                let host = extract_host(url);
                app.emit("console-line", format!("  ✗ {host}: {e}")).ok();
                last_err = e;
            }
        }
    }
    app.emit(
        "product-line",
        format!(
            "⚠ Could not fetch {kind} for {date}: {last_err} (tried {} URL(s))",
            urls.len()
        ),
    )
    .ok();
    None
}

/// Name a candidate URL will be saved under: its basename without `.gz`.
pub(crate) fn url_basename(url: &str) -> &str {
    url.rsplit('/')
        .next()
        .map(|s| s.trim_end_matches(".gz"))
        .unwrap_or("download")
}

/// The first candidate (in URL order) whose file already exists in `root`.
pub(crate) fn present_candidate(root: &Path, urls: &[String]) -> Option<String> {
    urls.iter()
        .map(|u| url_basename(u))
        .find(|name| product_path(root, name).is_file())
        .map(str::to_string)
}

/// GA API query listing the daily RINEX 3 broadcast navigation file for `date`.
pub(crate) fn brdc_lookup_url(date: NaiveDate) -> String {
    let start = format!("{}T00:00:00Z", date.format("%Y-%m-%d"));
    let end = format!("{}T23:59:59Z", date.format("%Y-%m-%d"));
    format!(
        "{GA_API}?metadataStatus=valid&stationId=BRDC&fileType=nav\
         &rinexVersion=3&filePeriod=01D&decompress=false\
         &startDate={start}&endDate={end}&tenantId=default"
    )
}

async fn resolve_brdc_url(
    client: &reqwest::Client,
    date: NaiveDate,
    app: &tauri::AppHandle,
) -> Result<String, String> {
    let url = brdc_lookup_url(date);
    // The shared client does not follow redirects (see `build_client`), and GA
    // has moved this endpoint once already. Follow any further move rather than
    // reporting a bare 301 as a failed lookup.
    let resp = match get_following_redirects(client, &url).await {
        Ok(r) => r,
        Err(e) => {
            if e.is_connect() || e.is_timeout() {
                app.emit("connection-fault", ConnectionFaultEvent {
                    host: extract_host(&url),
                    message: e.to_string(),
                }).ok();
            }
            return Err(format!("HTTP error: {e}"));
        }
    };
    let status = resp.status();
    let body = resp.text().await.map_err(|e| format!("read body: {e}"))?;
    if !status.is_success() {
        return Err(format!("HTTP {status}: {body}"));
    }
    pick_brdc_location(&body)
}

/// Pick the daily file's download URL from a GA API response body.
///
/// The response is a JSON array of objects with a `fileLocation` field. Prefer
/// the `01D` entry: a redirect that dropped `filePeriod` from the query would
/// otherwise let an hourly file through as the day's navigation data.
pub(crate) fn pick_brdc_location(body: &str) -> Result<String, String> {
    let entries: Vec<GaEntry> =
        serde_json::from_str(body).map_err(|e| format!("JSON parse: {e}"))?;
    let daily = entries
        .iter()
        .position(|e| e.file_period.as_deref() == Some("01D"))
        .unwrap_or(0);
    entries
        .into_iter()
        .nth(daily)
        .map(|e| e.file_location)
        .ok_or_else(|| "no BRDC file listed".to_string())
}

#[derive(serde::Deserialize)]
struct GaEntry {
    #[serde(rename = "fileLocation")]
    file_location: String,
    #[serde(rename = "filePeriod", default)]
    file_period: Option<String>,
}
