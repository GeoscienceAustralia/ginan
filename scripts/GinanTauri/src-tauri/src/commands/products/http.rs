use flate2::read::GzDecoder;
use futures_util::StreamExt;
use std::io::{Read, Write};
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tauri::Emitter;
use super::*;
use super::catalogue_fetch::make_absolute;

// ─── shared download/decompress ──────────────────────────────────────────────

/// Build the shared HTTP client.
///
/// The cookie store is enabled so the Earthdata/URS redirect handshake works:
/// a request to `cddis.nasa.gov` 302s to `urs.earthdata.nasa.gov`, which sets a
/// session cookie and 302s back to CDDIS. Without a cookie jar that round trip
/// loops. Basic auth is attached per request by [`authed_get`] when the target
/// host is a CDDIS/URS host (credentials read from netrc).
pub fn build_client() -> Result<reqwest::Client, String> {
    build_client_with_timeout(300)
}

/// As [`build_client`], with a custom whole-request timeout.
pub(crate) fn build_client_with_timeout(timeout_secs: u64) -> Result<reqwest::Client, String> {
    reqwest::Client::builder()
        .user_agent("GinanTauri/1.0")
        .timeout(std::time::Duration::from_secs(timeout_secs))
        .cookie_store(true)
        // Disable automatic redirect-following: reqwest strips the Authorization
        // header on cross-domain hops, which breaks the CDDIS → URS → CDDIS
        // OAuth handshake. `get_following_redirects` follows redirects manually
        // and re-attaches per-host credentials via `authed_get` at each hop.
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .map_err(|e| format!("HTTP client init failed: {e}"))
}

/// Start a GET request, attaching Basic auth from netrc when `url` targets a
/// CDDIS/Earthdata host. For every other host the request is left anonymous.
pub fn authed_get(client: &reqwest::Client, url: &str) -> reqwest::RequestBuilder {
    let req = client.get(url);
    match crate::commands::credentials::netrc_auth_for_url(url) {
        Some((user, pass)) => req.basic_auth(user, Some(pass)),
        None => req,
    }
}

/// Resolve the next URL in a redirect chain.
///
/// CDDIS's /proxyauth callback sometimes 301s to http://cddis.nasa.gov/...
/// The session cookie set during the OAuth exchange is marked `Secure`, so it
/// isn't sent on the http hop — CDDIS then treats the request as
/// unauthenticated and serves the Earthdata Login HTML. Upgrade the scheme so
/// the secure cookie survives the redirect chain.
pub(crate) fn redirect_target(current: &str, location: &str) -> String {
    let next = make_absolute(current, location);
    match next.strip_prefix("http://cddis.nasa.gov") {
        Some(rest) => format!("https://cddis.nasa.gov{rest}"),
        None => next,
    }
}

/// Send a GET, manually following any 3xx redirects and re-attaching per-host
/// credentials at each hop via [`authed_get`].
///
/// reqwest strips the `Authorization` header on cross-domain redirects for
/// security, which breaks the CDDIS → `urs.earthdata.nasa.gov` → CDDIS
/// Earthdata OAuth flow. By following manually we can call `authed_get` for
/// every hop so URS gets credentials too.
pub(crate) async fn get_following_redirects(
    client: &reqwest::Client,
    url: &str,
) -> Result<reqwest::Response, reqwest::Error> {
    get_following_redirects_from(client, url, 0).await
}

/// As [`get_following_redirects`], but asking for the body from byte `start`
/// onwards when `start > 0` (used to resume a truncated download).
///
/// The Range header is re-applied on every hop: the redirect chain ends at a
/// different host, and the header has to reach the one actually serving bytes.
pub(crate) async fn get_following_redirects_from(
    client: &reqwest::Client,
    url: &str,
    start: u64,
) -> Result<reqwest::Response, reqwest::Error> {
    let mut current = url.to_string();
    for _ in 0..10 {
        let mut req = authed_get(client, &current);
        if start > 0 {
            req = req.header("Range", format!("bytes={start}-"));
        }
        let resp = req.send().await?;
        if resp.status().is_redirection() {
            if let Some(loc) = resp.headers().get("location").and_then(|v| v.to_str().ok()) {
                current = redirect_target(&current, loc);
                continue;
            }
        }
        return Ok(resp);
    }
    let mut req = authed_get(client, &current);
    if start > 0 {
        req = req.header("Range", format!("bytes={start}-"));
    }
    req.send().await
}

/// Download a `.gz` product and gunzip it to `dest`.
///
/// CDDIS regularly resets the connection part-way through a transfer — the
/// same URL stops at a different offset every time, so plain retries never
/// complete. It does honour `Range`, so instead of restarting we resume from
/// wherever the body died and append until Content-Length is satisfied.
pub(crate) async fn download_and_gunzip(
    client: &reqwest::Client,
    url: &str,
    dest: &Path,
    label: &str,
    app: &tauri::AppHandle,
    cancel_flag: &Arc<AtomicBool>,
) -> Result<(), String> {
    // Enough passes to ride out repeated resets without spinning forever.
    const MAX_PASSES: u32 = 12;

    if let Some(parent) = dest.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("Cannot create {}: {e}", parent.display()))?;
    }

    app.emit("product-line", format!("Starting new download of {label}")).ok();

    let tmp_gz = dest.with_file_name(format!(
        "{}.gz.part",
        dest.file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| "download".to_string())
    ));
    // Never resume onto a part file left by an earlier run — its contents may
    // belong to a different revision of the product.
    let _ = std::fs::remove_file(&tmp_gz);

    let mut downloaded: u64 = 0;
    let mut total: u64 = 0;
    let mut last_percent: u8 = 255;
    // Replaced on every failing pass; the initial value only survives if the
    // loop somehow exits without either succeeding or recording a failure.
    let mut last_err = format!("Download of {label} did not complete");

    for pass in 1..=MAX_PASSES {
        if cancel_flag.load(Ordering::SeqCst) {
            let _ = std::fs::remove_file(&tmp_gz);
            return Err("Cancelled".to_string());
        }

        let resp = match get_following_redirects_from(client, url, downloaded).await {
            Ok(r) => r,
            Err(e) => {
                if e.is_connect() || e.is_timeout() {
                    app.emit("connection-fault", ConnectionFaultEvent {
                        host: extract_host(url),
                        message: e.to_string(),
                    }).ok();
                }
                return Err(format!("GET {url} failed: {e}"));
            }
        };

        let status = resp.status();
        let content_type = resp
            .headers()
            .get("content-type")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("")
            .to_string();

        // Already hold every byte: the server rejects the range as unsatisfiable.
        if status == reqwest::StatusCode::RANGE_NOT_SATISFIABLE && downloaded > 0 {
            break;
        }
        if !status.is_success() {
            return Err(format!("HTTP {status} from {url}"));
        }
        // CDDIS returns 200 with the Earthdata Login HTML when the auth
        // handshake fails. Streaming that into a .gz yields a confusing gunzip
        // error and a 0-byte destination, so fail loudly instead.
        if content_type.starts_with("text/html") || content_type.starts_with("application/xhtml") {
            return Err(format!(
                "got HTML (likely Earthdata login page) from {url}: auth handshake did not complete"
            ));
        }

        // 206 continues the file; a 200 means the range was ignored, so restart.
        let resuming = status == reqwest::StatusCode::PARTIAL_CONTENT;
        if !resuming {
            downloaded = 0;
            total = resp.content_length().unwrap_or(0);
        } else if total == 0 {
            total = downloaded + resp.content_length().unwrap_or(0);
        }

        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .append(resuming)
            .truncate(!resuming)
            .open(&tmp_gz)
            .map_err(|e| format!("Cannot open {}: {e}", tmp_gz.display()))?;

        let mut stream = resp.bytes_stream();
        let mut failed: Option<String> = None;
        while let Some(chunk) = stream.next().await {
            if cancel_flag.load(Ordering::SeqCst) {
                drop(file);
                let _ = std::fs::remove_file(&tmp_gz);
                return Err("Cancelled".to_string());
            }
            let chunk = match chunk {
                Ok(c) => c,
                Err(e) => {
                    failed = Some(format!(
                        "Download error ({label}): {e}, after {downloaded} of {total} bytes from {url}"
                    ));
                    break;
                }
            };
            file.write_all(&chunk)
                .map_err(|e| format!("Write error ({}): {e}", tmp_gz.display()))?;
            downloaded += chunk.len() as u64;
            let percent = (downloaded * 100)
                .checked_div(total)
                .map_or(0, |p| p.min(100) as u8);
            if percent != last_percent {
                last_percent = percent;
                app.emit(
                    "product-progress",
                    ProgressEvent { filename: label, percent, bytes: downloaded, total },
                )
                .ok();
            }
        }
        drop(file);

        match failed {
            None => {
                // Clean end of body. Trust it only if it accounts for the whole
                // file; a short "successful" read is another truncation.
                if total == 0 || downloaded >= total {
                    break;
                }
                last_err = format!(
                    "Download error ({label}): body ended early, {downloaded} of {total} bytes from {url}"
                );
            }
            Some(e) => last_err = e,
        }

        if pass == MAX_PASSES {
            let _ = std::fs::remove_file(&tmp_gz);
            return Err(last_err);
        }
        app.emit(
            "product-line",
            format!("Resuming download of {label} from byte {downloaded}"),
        )
        .ok();
        tokio::time::sleep(std::time::Duration::from_millis(400)).await;
    }

    // Decompression is synchronous file IO on products that can be tens of MB,
    // so keep it off the async runtime's worker threads.
    let (gz, out, name) = (tmp_gz.clone(), dest.to_path_buf(), label.to_string());
    let result = tokio::task::spawn_blocking(move || gunzip_file(&gz, &out, &name))
        .await
        .map_err(|e| format!("Gzip decode ({label}): {e}"))?;

    let _ = std::fs::remove_file(&tmp_gz);
    result
}

/// Gunzip `src` into `dest`. On a decode error the partial `dest` is removed.
fn gunzip_file(src: &Path, dest: &Path, label: &str) -> Result<(), String> {
    let gz_file = std::fs::File::open(src)
        .map_err(|e| format!("Cannot open {}: {e}", src.display()))?;
    let mut decoder = GzDecoder::new(gz_file);
    let mut out = std::fs::File::create(dest)
        .map_err(|e| format!("Cannot create {}: {e}", dest.display()))?;
    let mut buf = [0u8; 64 * 1024];
    loop {
        let n = decoder.read(&mut buf).map_err(|e| {
            let _ = std::fs::remove_file(dest);
            format!("Gzip decode ({label}): {e}")
        })?;
        if n == 0 {
            break;
        }
        out.write_all(&buf[..n])
            .map_err(|e| format!("Write error ({}): {e}", dest.display()))?;
    }
    Ok(())
}
