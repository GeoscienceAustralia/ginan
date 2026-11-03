use super::execution_helpers::*;
// Other command modules (stream_test) reach this through `execution::`.
pub(crate) use super::execution_helpers::resolve_pea_binary;
use std::path::{Path, PathBuf};
use tauri::{Emitter, Manager};
use tokio::io::AsyncBufReadExt;
use serde::{Deserialize, Serialize};

use crate::state::{AppState, CancelFlag};
use super::pea_versions;

pub(crate) const GINAN_REPO: &str = "GeoscienceAustralia/ginan";
pub(crate) const GITHUB_API_BASE: &str = "https://api.github.com";

// Maps the current compile target to the Ginan release asset suffix.
// The full asset names are derived as `ginan-{suffix}.zip` and, as a fallback,
// `ginan-gui-{suffix}.zip` (see `platform_asset_candidates`).
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub(crate) const PLATFORM_SUFFIX: &str = "linux-x64";
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
pub(crate) const PLATFORM_SUFFIX: &str = "macos-arm64";
#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
pub(crate) const PLATFORM_SUFFIX: &str = "macos-x64";
#[cfg(all(target_os = "windows", target_arch = "x86_64"))]
pub(crate) const PLATFORM_SUFFIX: &str = "windows-x64";
#[cfg(not(any(
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "macos", target_arch = "aarch64"),
    all(target_os = "macos", target_arch = "x86_64"),
    all(target_os = "windows", target_arch = "x86_64"),
)))]
pub(crate) const PLATFORM_SUFFIX: &str = "";

#[derive(Deserialize)]
pub(crate) struct GithubRelease {
    pub(crate) tag_name: String,
    pub(crate) assets: Vec<GithubAsset>,
}

#[derive(Deserialize)]
pub(crate) struct GithubAsset {
    pub(crate) name: String,
    pub(crate) browser_download_url: String,
}

/// Reported to the frontend after resolving the pea binary. `source` is one of
/// `override` (user-supplied path), `existing` (already downloaded, reused as-is)
/// or `downloaded` (fetched this call). `update_available` carries the newer tag
/// when the installed binary is behind the latest resolvable release — the app
/// does not auto-update, so the user is prompted to switch in Settings.
#[derive(Serialize)]
pub struct PeaBinaryStatus {
    path: String,
    version: Option<String>,
    source: String,
    update_available: Option<String>,
    /// The selected managed release, even while a custom binary is in use, so
    /// Settings can show what switching back would give.
    managed_version: Option<String>,
}

/// One selectable pea release: its tag, the asset that would be downloaded for
/// the current platform, and whether it is already in the local cache (so
/// switching to it needs no download).
#[derive(Serialize)]
pub struct PeaVersionInfo {
    tag: String,
    asset: String,
    downloaded: bool,
}

/// Make `path` (a managed binary, `tag` when known) the selected managed release
/// and the binary this session uses.
fn activate_managed(
    state: &std::sync::Mutex<AppState>,
    version_file: &Path,
    path: &Path,
    tag: Option<String>,
    source: &str,
) -> PeaBinaryStatus {
    if let Some(t) = &tag {
        std::fs::write(version_file, t).ok();
    }
    let path = path.to_string_lossy().to_string();
    state.lock().unwrap().pea_binary_path = Some(path.clone());
    PeaBinaryStatus {
        path,
        managed_version: tag.clone(),
        version: tag,
        source: source.to_string(),
        update_available: None,
    }
}

/// Download `target` into its cache folder and select it. A failed install
/// removes the half-written binary, so it is never mistaken for a cached one.
async fn install_and_activate(
    app: &tauri::AppHandle,
    state: &std::sync::Mutex<AppState>,
    client: &reqwest::Client,
    target: &PeaDownloadTarget,
    versions_dir: &Path,
    version_file: &Path,
) -> Result<PeaBinaryStatus, String> {
    let dest = pea_versions::binary_for(versions_dir, &target.tag_name)?;
    std::fs::create_dir_all(dest.parent().unwrap_or(versions_dir))
        .map_err(|e| format!("Cannot create pea cache dir: {e}"))?;
    if let Err(e) = install_pea_target(app, client, target, &dest, version_file).await {
        let _ = std::fs::remove_file(&dest);
        return Err(e);
    }
    Ok(activate_managed(state, version_file, &dest, Some(target.tag_name.clone()), "downloaded"))
}

/// Ensures a pea binary is available, without ever replacing one that already exists.
///
/// - If the user overrode the binary with a file outside the cache, it is used as-is.
/// - Otherwise the selected cached release is reused; the latest release is still
///   checked so `update_available` can prompt the user to switch versions in
///   Settings, but nothing is downloaded automatically.
/// - Only when nothing is cached at all is a release downloaded (latest, with fallback).
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn ensure_default_pea_binary(
    app: tauri::AppHandle,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<PeaBinaryStatus, String> {
    let override_path = state.lock().unwrap().pea_binary_path.clone();
    let (app_data_dir, legacy_path, version_file) = pea_paths(&app)?;
    let versions_dir = pea_versions::versions_dir(&app_data_dir);
    pea_versions::migrate_legacy(&legacy_path, &version_file, &versions_dir);

    // A cached release picked last session becomes the selection, so it is
    // reported with its tag rather than as "Custom". The single-binary path of
    // earlier versions of the app falls through too: migration may just have moved it into the cache. Any
    // other file is a custom build, used as-is with an unknown version.
    let mut managed = None;
    if let Some(p) = &override_path {
        let p = Path::new(p);
        if let Some(tag) = pea_versions::tag_of(&versions_dir, p).filter(|_| p.is_file()) {
            managed = Some((p.to_path_buf(), Some(tag)));
        } else if p != legacy_path {
            return Ok(PeaBinaryStatus {
                path: p.to_string_lossy().to_string(),
                version: None,
                source: "override".to_string(),
                update_available: None,
                managed_version: pea_versions::select_managed(&versions_dir, &legacy_path, &version_file)
                    .and_then(|(_, tag)| tag),
            });
        }
    }
    let managed = managed.or_else(|| pea_versions::select_managed(&versions_dir, &legacy_path, &version_file));
    let supported = !platform_asset_candidates().is_empty();

    if let Some((path, tag)) = managed {
        let mut status = activate_managed(&state, &version_file, &path, tag, "existing");
        // Never auto-replace: only check whether a newer release exists.
        if supported {
            let client = build_download_client()?;
            if let Ok(target) = resolve_pea_download(&client).await {
                if status.version.as_deref() != Some(&target.tag_name) {
                    app.emit(
                        "product-line",
                        format!(
                            "A newer pea binary ({}) is available. Pick a version in Settings → PEA Binary.",
                            target.tag_name
                        ),
                    ).ok();
                    status.update_available = Some(target.tag_name);
                }
            }
        }
        return Ok(status);
    }

    if !supported {
        return Err("Auto-download not supported on this platform. Use the 'PEA Binary' button.".to_string());
    }

    // Nothing cached at all — download one so the app is usable on first run.
    let client = build_download_client()?;
    let target = resolve_pea_download(&client).await.map_err(|e| {
        format!(
            "pea binary not found and no release could be fetched ({e}). \
             Use Settings → PEA Binary to locate it manually."
        )
    })?;
    install_and_activate(&app, &state, &client, &target, &versions_dir, &version_file).await
}

/// List the pea releases selectable for this platform (newest first), each marked
/// with whether it is already downloaded. Powers the version picker in Settings →
/// PEA Binary. Offline, the downloaded releases alone are listed, since those can
/// still be switched to.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn list_pea_versions(app: tauri::AppHandle) -> Result<Vec<PeaVersionInfo>, String> {
    let (app_data_dir, _, _) = pea_paths(&app)?;
    let installed = pea_versions::installed(&pea_versions::versions_dir(&app_data_dir));
    let cached = |tag: &str| PeaVersionInfo { tag: tag.to_string(), asset: String::new(), downloaded: true };

    let releases = if platform_asset_candidates().is_empty() {
        Err("Auto-download not supported on this platform.".to_string())
    } else {
        fetch_releases(&build_download_client()?).await
    };
    let releases = match releases {
        Ok(r) => r,
        Err(_) if !installed.is_empty() => return Ok(installed.iter().map(|t| cached(t)).collect()),
        Err(e) => return Err(e),
    };

    let mut list: Vec<PeaVersionInfo> = releases
        .iter()
        .filter_map(|r| {
            find_platform_asset(r).map(|a| PeaVersionInfo {
                tag: r.tag_name.clone(),
                asset: a.name.clone(),
                downloaded: installed.contains(&r.tag_name),
            })
        })
        .collect();
    // A downloaded release since pulled from GitHub is still usable, so keep it.
    let missing: Vec<_> = installed.iter().filter(|t| !list.iter().any(|v| &v.tag == *t)).collect();
    list.extend(missing.into_iter().map(|t| cached(t)));
    Ok(list)
}

/// Switch to pea release `tag`: instantly when it is already in the cache,
/// otherwise by downloading it first. Either way it becomes the selected managed
/// release, superseding any custom override.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn download_pea_version(
    tag: String,
    app: tauri::AppHandle,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<PeaBinaryStatus, String> {
    let (app_data_dir, _, version_file) = pea_paths(&app)?;
    let versions_dir = pea_versions::versions_dir(&app_data_dir);
    let cached = pea_versions::binary_for(&versions_dir, &tag)?;
    if cached.is_file() {
        app.emit("console-line", format!("Switched to downloaded pea binary {tag}.")).ok();
        return Ok(activate_managed(&state, &version_file, &cached, Some(tag), "existing"));
    }

    if platform_asset_candidates().is_empty() {
        return Err("Auto-download not supported on this platform.".to_string());
    }
    let client = build_download_client()?;
    let releases = fetch_releases(&client).await?;
    let release = releases
        .iter()
        .find(|r| r.tag_name == tag)
        .ok_or_else(|| format!("Release {tag} not found."))?;
    let asset = find_platform_asset(release)
        .ok_or_else(|| format!("Release {tag} has no pea asset for this platform."))?;
    let target = PeaDownloadTarget {
        tag_name: release.tag_name.clone(),
        asset_name: asset.name.clone(),
        download_url: asset.browser_download_url.clone(),
    };
    install_and_activate(&app, &state, &client, &target, &versions_dir, &version_file).await
}

/// Switch back to the managed release from a custom binary: the selected cached
/// release, else the newest cached one, else (nothing ever downloaded) the
/// latest release, fetched now.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn use_managed_pea(
    app: tauri::AppHandle,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<PeaBinaryStatus, String> {
    let (app_data_dir, legacy_path, version_file) = pea_paths(&app)?;
    let versions_dir = pea_versions::versions_dir(&app_data_dir);
    if let Some((path, tag)) = pea_versions::select_managed(&versions_dir, &legacy_path, &version_file) {
        return Ok(activate_managed(&state, &version_file, &path, tag, "existing"));
    }
    if platform_asset_candidates().is_empty() {
        return Err("Auto-download not supported on this platform.".to_string());
    }
    let client = build_download_client()?;
    let target = resolve_pea_download(&client).await?;
    install_and_activate(&app, &state, &client, &target, &versions_dir, &version_file).await
}

/// Store a user-chosen PEA binary path so it survives across Process clicks.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn set_pea_binary(
    path: String,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<(), String> {
    let p = PathBuf::from(&path);
    if !p.is_file() {
        return Err(format!("Not a file: {path}"));
    }
    state.lock().unwrap().pea_binary_path = Some(path);
    Ok(())
}

/// Return the currently stored or auto-discovered PEA binary path.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn find_pea_binary(
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<String, String> {
    // Prefer the user-supplied path
    if let Some(p) = state.lock().unwrap().pea_binary_path.clone() {
        return Ok(p);
    }
    find_pea_path()
        .map(|p| p.to_string_lossy().to_string())
        .ok_or_else(|| "PEA binary not found automatically.".to_string())
}

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn start_pea(
    config_path: String,
    app: tauri::AppHandle,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
    user: Option<String>,
    pass: Option<String>,
) -> Result<(), String> {
    if state.lock().unwrap().is_processing {
        return Err("PEA is already running".to_string());
    }

    let pea = resolve_pea_binary(&state.lock().unwrap())?;
    // Normally done at startup; repeated here in case that was offline. A
    // failure is reported in the Console, and PEA's own error follows.
    let _ = super::pea_runtime::ensure_libomp(&app).await;

    let config_dir = Path::new(&config_path)
        .parent()
        .unwrap_or(Path::new("."))
        .to_path_buf();

    // Echo the command to the Console channel before spawning, mirroring the
    // original GinanUI (Python) which logs the launch line, working directory
    // and a rule to Console right before starting the PEA subprocess.
    app.emit(
        "console-line",
        format!("🚀 Starting PEA: {} --config {}", pea.display(), config_path),
    )
    .ok();
    app.emit(
        "console-line",
        format!("📂 Working directory: {}", config_dir.display()),
    )
    .ok();
    app.emit("console-line", "=".repeat(60)).ok();

    let mut cmd = tokio::process::Command::new(&pea);
    super::pea_runtime::apply_env(&mut cmd, &app);
    cmd.arg("--config").arg(&config_path);

    if let Some(u) = user {
        cmd.arg("--user").arg(u);
    }
    if let Some(p) = pass {
        cmd.arg("--pass").arg(p);
    }

    let mut child = cmd
        .current_dir(&config_dir)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(false)
        .spawn()
        .map_err(|e| format!("Failed to start PEA: {e}"))?;

    let pid = child.id();
    {
        let mut st = state.lock().unwrap();
        st.pea_pid = pid;
        st.is_processing = true;
    }

    let stdout = child.stdout.take();
    let stderr = child.stderr.take();
    let app_out = app.clone();
    let app_err = app.clone();
    let app_done = app.clone();

    if let Some(out) = stdout {
        tokio::spawn(async move {
            let mut lines = tokio::io::BufReader::new(out).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                observe_sisnet(&app_out, &line);
                app_out.emit("pea-line", &line).ok();
            }
        });
    }

    if let Some(err) = stderr {
        tokio::spawn(async move {
            let mut lines = tokio::io::BufReader::new(err).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                observe_sisnet(&app_err, &line);
                app_err.emit("pea-line", format!("[ERR] {line}")).ok();
            }
        });
    }

    tokio::spawn(async move {
        let status = child.wait().await;
        let st = app_done.state::<std::sync::Mutex<AppState>>();
        let stop_requested = {
            let mut guard = st.lock().unwrap();
            guard.pea_pid = None;
            guard.is_processing = false;
            let stopped = pid.is_some() && guard.stopped_pid == pid;
            if stopped {
                guard.stopped_pid = None;
            }
            stopped
        };
        // Free any serial ports held for this run (no-op for file runs).
        crate::commands::streams::release(&app_done);
        match status {
            Ok(s) => match classify_exit(&s, stop_requested) {
                PeaExit::Finished(code) => {
                    app_done.emit("pea-finished", code).ok();
                }
                PeaExit::Stopped => {
                    app_done.emit("pea-stopped", ()).ok();
                }
                PeaExit::Failed(message) => {
                    app_done.emit("pea-error", message).ok();
                }
            },
            Err(e) => {
                app_done.emit("pea-error", e.to_string()).ok();
            }
        }
    });

    Ok(())
}

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn stop_pea(
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
    cancel: tauri::State<'_, CancelFlag>,
) -> Result<(), String> {
    cancel.cancel();

    let pid = {
        let mut guard = state.lock().unwrap();
        // Recorded before the kill so the exit handler, which can run as soon
        // as the process dies, already sees it.
        guard.stopped_pid = guard.pea_pid;
        guard.pea_pid
    };
    if let Some(pid) = pid {
        #[cfg(unix)]
        {
            std::process::Command::new("kill")
                .args(["-TERM", &pid.to_string()])
                .status()
                .ok();
        }
        #[cfg(windows)]
        {
            std::process::Command::new("taskkill")
                .args(["/PID", &pid.to_string(), "/F"])
                .status()
                .ok();
        }
    }
    let mut guard = state.lock().unwrap();
    guard.pea_pid = None;
    guard.is_processing = false;
    Ok(())
}

/// Count PEA's SISNet log lines for the Streams tab: PEA reports its SBAS
/// stream nowhere else (see `sbas_monitor`). The check is a substring test, so
/// every other line costs next to nothing.
fn observe_sisnet(app: &tauri::AppHandle, line: &str) {
    if crate::commands::sbas_monitor::is_sisnet_line(line) {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(0, |d| d.as_millis() as i64);
        app.state::<crate::state::LiveStats>().sbas_console.lock().unwrap().observe(line, now);
    }
}
