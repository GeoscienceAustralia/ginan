//! Split out of execution.rs to keep each file under 500 lines.


use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use tauri::{Emitter, Manager};
use futures_util::StreamExt;
use crate::state::AppState;
use super::execution::*;

// ─── helpers ──────────────────────────────────────────────────────────────────

pub(crate) fn build_download_client() -> Result<reqwest::Client, String> {
    reqwest::Client::builder()
        .user_agent("GinanTauri/1.0")
        .timeout(std::time::Duration::from_secs(300))
        .build()
        .map_err(|e| format!("HTTP client init failed: {e}"))
}

/// The app-data directory, the pea binary path within it, and the version-marker
/// file that records which release the binary came from.
pub(crate) fn pea_paths(app: &tauri::AppHandle) -> Result<(PathBuf, PathBuf, PathBuf), String> {
    let exe_name = if cfg!(windows) { "pea.exe" } else { "pea" };
    let app_data_dir = app.path().app_data_dir()
        .map_err(|e| format!("Cannot get app data dir: {e}"))?;
    let pea_path = app_data_dir.join(exe_name);
    let version_file = app_data_dir.join("pea_version.txt");
    Ok((app_data_dir, pea_path, version_file))
}

/// Read the release tag of the currently installed binary, if recorded.
pub(crate) fn read_local_version(version_file: &Path) -> Option<String> {
    if version_file.is_file() {
        std::fs::read_to_string(version_file)
            .ok()
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
    } else {
        None
    }
}

/// Download `target` into `pea_path` and record its tag, emitting the workflow
/// lines the frontend shows. Shared by first-run download and explicit version
/// selection.
pub(crate) async fn install_pea_target(
    app: &tauri::AppHandle,
    client: &reqwest::Client,
    target: &PeaDownloadTarget,
    pea_path: &Path,
    version_file: &Path,
) -> Result<(), String> {
    let verb = if pea_path.is_file() { "Updating" } else { "Downloading" };
    app.emit(
        "console-line",
        format!("{verb} pea binary {} ({})...", target.tag_name, target.asset_name),
    ).ok();

    let cancel = Arc::new(AtomicBool::new(false));
    download_pea_zip(client, &target.download_url, pea_path, app, &cancel).await?;

    std::fs::write(version_file, &target.tag_name)
        .map_err(|e| format!("Cannot write version file: {e}"))?;

    app.emit(
        "console-line",
        format!("✅ pea binary {} ready.", target.tag_name),
    ).ok();
    Ok(())
}

pub(crate) async fn fetch_latest_release(client: &reqwest::Client) -> Result<GithubRelease, String> {
    let url = format!("{GITHUB_API_BASE}/repos/{GINAN_REPO}/releases/latest");
    let resp = client
        .get(&url)
        .header("Accept", "application/vnd.github+json")
        .send()
        .await
        .map_err(|e| format!("GitHub API request failed: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!("GitHub API returned {}", resp.status()));
    }
    resp.json::<GithubRelease>()
        .await
        .map_err(|e| format!("GitHub API parse failed: {e}"))
}

/// Fetch the list of published releases (newest first), used to fall back to an
/// earlier minor version when the latest release lacks a suitable pea asset.
pub(crate) async fn fetch_releases(client: &reqwest::Client) -> Result<Vec<GithubRelease>, String> {
    let url = format!("{GITHUB_API_BASE}/repos/{GINAN_REPO}/releases?per_page=100");
    let resp = client
        .get(&url)
        .header("Accept", "application/vnd.github+json")
        .send()
        .await
        .map_err(|e| format!("GitHub API request failed: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!("GitHub API returned {}", resp.status()));
    }
    resp.json::<Vec<GithubRelease>>()
        .await
        .map_err(|e| format!("GitHub API parse failed: {e}"))
}

/// Candidate pea asset names for the current platform, in fallback order:
/// the standard `ginan-{platform}.zip`, then the GUI build
/// `ginan-gui-{platform}.zip`. Empty when auto-download is unsupported here.
pub(crate) fn platform_asset_candidates() -> Vec<String> {
    if PLATFORM_SUFFIX.is_empty() {
        return Vec::new();
    }
    vec![
        format!("ginan-{PLATFORM_SUFFIX}.zip"),
        format!("ginan-gui-{PLATFORM_SUFFIX}.zip"),
    ]
}

/// Find the first matching pea asset in a release, honouring the candidate order
/// (`ginan-…` preferred over `ginan-gui-…`).
pub(crate) fn find_platform_asset(release: &GithubRelease) -> Option<&GithubAsset> {
    for name in platform_asset_candidates() {
        if let Some(asset) = release.assets.iter().find(|a| a.name == name) {
            return Some(asset);
        }
    }
    None
}

/// Parse a release tag such as `v3.0.0` or `3.1` into a `(major, minor)` pair.
pub(crate) fn parse_major_minor(tag: &str) -> Option<(u32, u32)> {
    let cleaned = tag.trim().trim_start_matches(['v', 'V']);
    let mut parts = cleaned.split(['.', '-', '_']);
    let major = parts.next()?.parse::<u32>().ok()?;
    let minor = parts.next().and_then(|s| s.parse::<u32>().ok()).unwrap_or(0);
    Some((major, minor))
}

/// The release + asset the pea binary should be downloaded from.
pub(crate) struct PeaDownloadTarget {
    pub(crate) tag_name: String,
    pub(crate) asset_name: String,
    pub(crate) download_url: String,
}

/// Resolve which release/asset to download, applying the fallback chain:
///   1. latest release → `ginan-{platform}.zip`
///   2. latest release → `ginan-gui-{platform}.zip`
///   3. the single previous minor version → the same two names
pub(crate) async fn resolve_pea_download(client: &reqwest::Client) -> Result<PeaDownloadTarget, String> {
    let latest = fetch_latest_release(client).await?;

    // 1 & 2: try the latest release's assets (standard, then GUI build).
    if let Some(asset) = find_platform_asset(&latest) {
        return Ok(PeaDownloadTarget {
            tag_name: latest.tag_name.clone(),
            asset_name: asset.name.clone(),
            download_url: asset.browser_download_url.clone(),
        });
    }

    // 3: fall back to exactly one minor version back. Among releases whose
    // (major, minor) is strictly below the latest, take the highest such minor —
    // the immediately previous one — and try its assets.
    let latest_mm = parse_major_minor(&latest.tag_name);
    let releases = fetch_releases(client).await?;

    let prev_minor = releases
        .iter()
        .filter_map(|r| parse_major_minor(&r.tag_name))
        .filter(|mm| matches!(latest_mm, Some(l) if *mm < l))
        .max();

    if let Some(prev) = prev_minor {
        // The newest patch release of that minor (releases are newest-first).
        if let Some(release) = releases
            .iter()
            .find(|r| parse_major_minor(&r.tag_name) == Some(prev))
        {
            if let Some(asset) = find_platform_asset(release) {
                return Ok(PeaDownloadTarget {
                    tag_name: release.tag_name.clone(),
                    asset_name: asset.name.clone(),
                    download_url: asset.browser_download_url.clone(),
                });
            }
        }
    }

    let primary = platform_asset_candidates()
        .into_iter()
        .next()
        .unwrap_or_default();
    Err(format!(
        "No release provides a '{primary}' (or GUI) asset \
         (checked latest {} and the previous minor version).",
        latest.tag_name
    ))
}

/// Downloads a Ginan release zip and extracts the pea binary to `dest`.
pub(crate) async fn download_pea_zip(
    client: &reqwest::Client,
    zip_url: &str,
    dest: &Path,
    app: &tauri::AppHandle,
    cancel_flag: &Arc<AtomicBool>,
) -> Result<(), String> {
    let tmp = dest.with_extension("zip.part");

    let resp = client
        .get(zip_url)
        .send()
        .await
        .map_err(|e| format!("Download request failed: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!("HTTP {} downloading pea zip", resp.status()));
    }
    let total = resp.content_length().unwrap_or(0);

    let mut file = std::fs::File::create(&tmp)
        .map_err(|e| format!("Cannot create temp file: {e}"))?;
    let mut stream = resp.bytes_stream();
    let mut downloaded: u64 = 0;
    let mut last_pct: u8 = 255;

    while let Some(chunk) = stream.next().await {
        if cancel_flag.load(Ordering::SeqCst) {
            drop(file);
            let _ = std::fs::remove_file(&tmp);
            return Err("Cancelled".to_string());
        }
        let chunk = chunk.map_err(|e| format!("Download error: {e}"))?;
        file.write_all(&chunk)
            .map_err(|e| format!("Write error: {e}"))?;
        downloaded += chunk.len() as u64;
        let pct = (downloaded * 100).checked_div(total).map_or(0, |p| p.min(100) as u8);
        if pct != last_pct {
            last_pct = pct;
            app.emit("product-progress", serde_json::json!({
                "filename": "pea",
                "percent": pct,
                "bytes": downloaded,
                "total": total,
            })).ok();
        }
    }
    drop(file);

    let tmp_clone = tmp.clone();
    let dest_clone = dest.to_path_buf();
    tokio::task::spawn_blocking(move || extract_pea_from_zip(&tmp_clone, &dest_clone))
        .await
        .map_err(|e| format!("Extract task error: {e}"))??;

    let _ = std::fs::remove_file(&tmp);
    Ok(())
}

/// Synchronous: extract the `pea` (or `pea.exe`) entry from a zip archive.
pub(crate) fn extract_pea_from_zip(zip_path: &Path, dest: &Path) -> Result<(), String> {
    let exe_name = if cfg!(windows) { "pea.exe" } else { "pea" };

    let file = std::fs::File::open(zip_path)
        .map_err(|e| format!("Cannot open zip: {e}"))?;
    let mut archive = zip::ZipArchive::new(file)
        .map_err(|e| format!("Invalid zip: {e}"))?;

    let mut pea_index = None;
    for i in 0..archive.len() {
        let entry = archive.by_index(i)
            .map_err(|e| format!("Zip read error: {e}"))?;
        let name = entry.name().to_string();
        if !entry.is_dir() && (name == exe_name || name.ends_with(&format!("/{exe_name}"))) {
            pea_index = Some(i);
            break;
        }
    }

    let idx = pea_index.ok_or_else(|| format!("'{exe_name}' not found in zip"))?;
    let mut entry = archive.by_index(idx)
        .map_err(|e| format!("Cannot read pea entry: {e}"))?;

    let mut out = std::fs::File::create(dest)
        .map_err(|e| format!("Cannot create {}: {e}", dest.display()))?;
    std::io::copy(&mut entry, &mut out)
        .map_err(|e| format!("Cannot extract pea: {e}"))?;
    drop(out);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(dest, std::fs::Permissions::from_mode(0o755))
            .map_err(|e| format!("Cannot chmod pea: {e}"))?;
    }

    Ok(())
}

/// How a PEA run ended, as the exit handler reports it to the frontend.
#[derive(Debug, PartialEq)]
pub(crate) enum PeaExit {
    /// Clean exit (`pea-finished`).
    Finished(i32),
    /// The user pressed Stop (`pea-stopped`). Not a failure.
    Stopped,
    /// Anything else (`pea-error`), with a plain-words reason.
    Failed(String),
}

/// Decide what a finished PEA process means. A clean exit is a finish even if
/// Stop was pressed as it ended; otherwise a requested Stop wins, because the
/// status it leaves (SIGTERM on Unix, exit code 1 from `taskkill /F` on
/// Windows) looks just like a crash.
pub(crate) fn classify_exit(s: &std::process::ExitStatus, stop_requested: bool) -> PeaExit {
    if s.success() {
        PeaExit::Finished(s.code().unwrap_or(0))
    } else if stop_requested {
        PeaExit::Stopped
    } else {
        PeaExit::Failed(describe_exit(s))
    }
}

/// Why PEA ended, worded to follow "PEA execution failed: " in the Workflow.
pub(crate) fn describe_exit(s: &std::process::ExitStatus) -> String {
    if let Some(code) = s.code() {
        return format!("exited with code {code}");
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = s.signal() {
            return describe_signal(sig);
        }
    }
    format!("ended abnormally ({s:?})")
}

/// A fatal signal in plain words, still naming it for anyone searching logs.
/// Only reached on Unix, but kept platform-neutral so it is tested everywhere.
#[cfg_attr(not(unix), allow(dead_code))]
pub(crate) fn describe_signal(sig: i32) -> String {
    let name = signal_name(sig);
    let what = match sig {
        11 => "crashed with a segmentation fault",
        7 => "crashed with a bus error",
        4 => "crashed on an illegal instruction",
        8 => "crashed on an arithmetic error",
        6 => "aborted",
        9 => {
            return format!(
                "was killed ({name}, signal {sig}). If you did not stop it, the system may \
                 have run out of memory: `dmesg | tail` shows an OOM kill."
            )
        }
        1 | 2 | 3 | 15 => "was terminated by another process",
        13 => "lost the pipe it writes its output to",
        _ => "was ended by a signal",
    };
    format!("{what} ({name}, signal {sig})")
}

#[cfg_attr(not(unix), allow(dead_code))]
pub(crate) fn signal_name(sig: i32) -> &'static str {
    match sig {
        1 => "SIGHUP",
        2 => "SIGINT",
        3 => "SIGQUIT",
        4 => "SIGILL",
        6 => "SIGABRT",
        7 => "SIGBUS",
        8 => "SIGFPE",
        9 => "SIGKILL",
        11 => "SIGSEGV",
        13 => "SIGPIPE",
        15 => "SIGTERM",
        _ => "unknown",
    }
}

/// The PEA binary a run uses: the user-supplied path first, then auto-discovery.
pub(crate) fn resolve_pea_binary(state: &AppState) -> Result<PathBuf, String> {
    state
        .pea_binary_path
        .as_ref()
        .map(PathBuf::from)
        .or_else(find_pea_path)
        .ok_or_else(|| "PEA binary not found. Use the 'PEA Binary' button to locate it.".to_string())
}

pub(crate) fn find_pea_path() -> Option<PathBuf> {
    let exe_name = if cfg!(windows) { "pea.exe" } else { "pea" };

    // 1. Search PATH
    if let Ok(path_var) = env::var("PATH") {
        for dir in env::split_paths(&path_var) {
            let candidate = dir.join(exe_name);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }

    // 2. Walk up from the running executable looking for <root>/bin/pea
    //    In dev: target/debug/ginan-tauri → up ~6 levels to reach repo root
    if let Ok(exe) = env::current_exe() {
        let mut dir = exe.parent()?.to_path_buf();
        for _ in 0..8 {
            let candidate = dir.join("bin").join(exe_name);
            if candidate.is_file() {
                return Some(candidate);
            }
            dir = match dir.parent() {
                Some(p) => p.to_path_buf(),
                None => break,
            };
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    fn status(raw: i32) -> std::process::ExitStatus {
        use std::os::unix::process::ExitStatusExt;
        std::process::ExitStatus::from_raw(raw)
    }

    #[cfg(windows)]
    fn status(raw: u32) -> std::process::ExitStatus {
        use std::os::windows::process::ExitStatusExt;
        std::process::ExitStatus::from_raw(raw)
    }

    /// A process that exited with `code` (Unix packs it into the high byte).
    fn exited(code: u8) -> std::process::ExitStatus {
        #[cfg(unix)]
        return status((code as i32) << 8);
        #[cfg(windows)]
        return status(code as u32);
    }

    #[test]
    fn a_clean_exit_is_a_finish_even_if_stop_was_pressed() {
        assert_eq!(classify_exit(&exited(0), false), PeaExit::Finished(0));
        assert_eq!(classify_exit(&exited(0), true), PeaExit::Finished(0));
    }

    #[test]
    fn a_requested_stop_is_not_a_failure() {
        // taskkill /F leaves exit code 1 on Windows.
        assert_eq!(classify_exit(&exited(1), true), PeaExit::Stopped);
    }

    #[test]
    fn a_non_zero_exit_without_stop_is_a_failure() {
        assert_eq!(
            classify_exit(&exited(3), false),
            PeaExit::Failed("exited with code 3".into())
        );
    }

    #[cfg(unix)]
    #[test]
    fn signals_are_classified_by_the_stop_flag_not_the_signal() {
        // SIGTERM from Stop vs. SIGTERM from elsewhere.
        assert_eq!(classify_exit(&status(15), true), PeaExit::Stopped);
        assert_eq!(
            classify_exit(&status(15), false),
            PeaExit::Failed("was terminated by another process (SIGTERM, signal 15)".into())
        );
        assert_eq!(
            classify_exit(&status(11), false),
            PeaExit::Failed("crashed with a segmentation fault (SIGSEGV, signal 11)".into())
        );
    }

    #[test]
    fn crash_signals_are_named_in_plain_words() {
        assert_eq!(
            describe_signal(11),
            "crashed with a segmentation fault (SIGSEGV, signal 11)"
        );
        assert_eq!(describe_signal(6), "aborted (SIGABRT, signal 6)");
        assert!(describe_signal(9).starts_with("was killed (SIGKILL, signal 9)"));
        assert!(describe_signal(9).contains("out of memory"));
        assert_eq!(describe_signal(64), "was ended by a signal (unknown, signal 64)");
    }

    #[test]
    fn no_message_suggests_the_stop_button() {
        // Listing "stop button" among the causes would make a Stop read as a
        // crash.
        for sig in [1, 2, 3, 4, 6, 7, 8, 9, 11, 13, 15, 64] {
            assert!(!describe_signal(sig).to_lowercase().contains("stop button"));
        }
    }
}
