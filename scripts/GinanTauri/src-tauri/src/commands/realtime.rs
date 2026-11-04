//! Live streaming run: telemetry tailer + on-disk retention sweep.
//!
//! PEA in realtime mode writes two things worth watching:
//!
//! * `ntrip_stats.jsonl` — one JSON line per NTRIP event (connect, first data,
//!   bytes received, disconnect). The tailer emits each parsed line to the
//!   frontend as an `ntrip-stats` event so the UI can render per-stream health
//!   without polling files or shelling out.
//! * Rotated recording files (`*_OBS.rtcm`, `*_NAV.rtcm`, `*_DEC.json`,
//!   `*_OBS.rnx`) — a live GNSS stream produces tens of megabytes per minute,
//!   so an unmanaged run fills the disk in hours. The sweeper enforces a byte
//!   cap by deleting the oldest rotated files first.
//!
//! The tailer and sweeper are cheap: they only spin up when the caller asks
//! for realtime, and both exit as soon as the PEA process does.

use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::time::Duration;

use serde::Deserialize;
use tauri::{Emitter, Manager};
use tokio::io::{AsyncBufReadExt, AsyncSeekExt};

use crate::commands::config::NTRIP_LOG_FILENAME;
use crate::state::{AppState, CancelFlag, LiveStats};

/// Prefixes the sweeper considers rotated recordings. Matching by suffix keeps
/// the check independent of receiver/date tokens PEA inlines into filenames.
const RECORDING_SUFFIXES: &[&str] = &["_OBS.rtcm", "_NAV.rtcm", "_DEC.json", "_OBS.rnx"];

/// Default disk budget for rotated recordings when the caller passes `None`.
/// Chosen to survive multiple days of a single-caster run (a full six-stream
/// setup at ~10 MB/min hits this in under an hour, so the UI must let heavy
/// users raise it).
const DEFAULT_STORAGE_CAP_BYTES: u64 = 500 * 1024 * 1024;

/// How often the sweeper wakes up. Short enough that a burst of rotated files
/// doesn't blow through the budget between checks, long enough that the loop
/// costs nothing when nothing is rotating.
const SWEEP_INTERVAL: Duration = Duration::from_secs(15);

#[derive(Debug, Deserialize)]
pub struct RealtimeRunOptions {
    pub config_path: String,
    /// The caster URL from the UI (e.g. `https://host/`; the
    /// `<USER>:<PASS>@` placeholder form is also accepted). Only the hostname is
    /// read, to resolve credentials from netrc for CLI flag overrides.
    #[serde(default)]
    pub caster_url: String,
    /// Where PEA writes its outputs — same value that goes into
    /// `outputs.outputs_root`. The sweeper and tailer both operate on this
    /// directory, so getting it wrong silently defeats retention.
    pub output_dir: String,
    /// Ceiling on rotated recording bytes, MB. `None` uses
    /// `DEFAULT_STORAGE_CAP_BYTES`. Zero disables the sweep entirely (useful
    /// for short debug runs where we don't want files disappearing).
    pub storage_cap_mb: Option<u64>,
    /// Line settings for each `serial://` stream, keyed by device path. A
    /// device with no entry uses the script defaults.
    #[serde(default)]
    pub serial: std::collections::HashMap<String, crate::commands::streams::SerialSettings>,
    /// The SBAS mode when the run uses SBAS, so the Streams tab gets its
    /// `sbas` block (see `sbas_monitor`).
    #[serde(default)]
    pub sbas_mode: Option<String>,
}

/// Start PEA in realtime mode and stand up the telemetry + retention loops.
///
/// Reuses `start_pea`'s subprocess handling: the extra work — tailer + sweeper
/// — layers on top rather than reimplementing subprocess plumbing. This matters
/// because both commands must produce the same `pea-line` / `pea-finished` /
/// `pea-error` events; the UI has one handler set for both modes.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn start_pea_realtime(
    options: RealtimeRunOptions,
    app: tauri::AppHandle,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
    cancel: tauri::State<'_, CancelFlag>,
) -> Result<(), String> {
    // A previous Stop leaves the shared flag set. Each realtime run owns a new
    // operation and must re-arm its telemetry/retention loops.
    cancel.begin();
    app.state::<LiveStats>().clear();

    let output_dir = PathBuf::from(&options.output_dir);
    if !output_dir.is_dir() {
        std::fs::create_dir_all(&output_dir)
            .map_err(|e| format!("Cannot create output dir {}: {e}", output_dir.display()))?;
    }

    let cap_bytes = match options.storage_cap_mb {
        Some(0) => 0,
        Some(mb) => mb.saturating_mul(1024 * 1024),
        None => DEFAULT_STORAGE_CAP_BYTES,
    };

    require_caster_login(&options.caster_url, &crate::commands::credentials_ntrip::ntrip_auth_for_host)?;
    let (user, pass) = caster_login(&options.caster_url).unzip();

    crate::commands::streams::prepare(&app, &options.config_path, &options.serial).await?;

    if let Err(e) = crate::commands::execution::start_pea(
        options.config_path,
        app.clone(),
        state,
        user,
        pass,
    ).await {
        crate::commands::streams::release(&app);
        return Err(e);
    }

    spawn_ntrip_stats_tailer(app.clone(), output_dir.clone());
    spawn_live_monitor(app.clone(), output_dir.clone(), std::time::SystemTime::now(), options.sbas_mode);
    if cap_bytes > 0 {
        spawn_retention_sweeper(app.clone(), output_dir, cap_bytes);
    }
    Ok(())
}

/// The netrc login for the caster's host, passed to PEA as `--user`/`--pass`.
pub(crate) fn caster_login(caster_url: &str) -> Option<(String, String)> {
    let host = crate::commands::credentials::ntrip_host_from_url(caster_url)?;
    crate::commands::credentials_ntrip::ntrip_auth_for_host(&host)
}

/// Refuse a run or stream test on a caster with no saved login. PEA needs
/// `user:pass@` in a stream URL to find its host at all (with none it tries
/// `https://:443/MOUNT`), and the caster wants a login anyway; without this,
/// PEA retries silently until the stream test times out. `caster_url` is empty
/// when no stream is on a caster (serial only), which needs no login.
pub(crate) fn require_caster_login(
    caster_url: &str,
    lookup: &dyn Fn(&str) -> Option<(String, String)>,
) -> Result<(), String> {
    if caster_url.trim().is_empty() {
        return Ok(());
    }
    let Some(host) = crate::commands::credentials::ntrip_host_from_url(caster_url) else {
        return Ok(());
    };
    match lookup(&host) {
        Some(_) => Ok(()),
        None => Err(format!(
            "No login is saved for the NTRIP caster {host}, and every stream on it needs one. \
             Add it in Settings › Credentials › NTRIP caster, then try again."
        )),
    }
}

/// Follow `ntrip_stats.jsonl` and forward each JSON line to the frontend.
///
/// The file may not exist yet when we start (PEA creates it on first NTRIP
/// contact), so the loop keeps re-opening until it appears. Once open, it seeks
/// to the end and streams new lines — a `<mm>`-rotation on the recording files
/// doesn't touch this log, so a simple tail-forever is sufficient.
///
/// Exits cleanly when either the CancelFlag is set (stop button) or the log's
/// parent process disappears, detected via `is_processing` on AppState.
fn spawn_ntrip_stats_tailer(app: tauri::AppHandle, output_dir: PathBuf) {
    let log_path = output_dir.join(NTRIP_LOG_FILENAME);
    tokio::spawn(async move {
        let cancel = app.state::<CancelFlag>().0.clone();
        let mut opened: Option<tokio::io::BufReader<tokio::fs::File>> = None;

        loop {
            if cancel.load(Ordering::SeqCst) || !still_processing(&app) {
                break;
            }

            if opened.is_none() {
                match tokio::fs::File::open(&log_path).await {
                    Ok(mut f) => {
                        let _ = f.seek(std::io::SeekFrom::End(0)).await;
                        opened = Some(tokio::io::BufReader::new(f));
                    }
                    Err(_) => {
                        tokio::time::sleep(Duration::from_millis(500)).await;
                        continue;
                    }
                }
            }

            let reader = opened.as_mut().unwrap();
            let mut line = String::new();
            match reader.read_line(&mut line).await {
                Ok(0) => {
                    tokio::time::sleep(Duration::from_millis(250)).await;
                }
                Ok(_) => {
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    // Emit as parsed JSON when possible so the frontend can
                    // destructure fields directly; fall back to raw string on
                    // malformed input rather than dropping data on the floor.
                    match serde_json::from_str::<serde_json::Value>(trimmed) {
                        Ok(v) => {
                            app.emit("ntrip-stats", v).ok();
                        }
                        Err(_) => {
                            app.emit("ntrip-stats-raw", trimmed.to_string()).ok();
                        }
                    }
                }
                Err(_) => {
                    // On IO error, drop the handle and try re-opening. A caster
                    // reconnect / log rotation would trip this.
                    opened = None;
                    tokio::time::sleep(Duration::from_millis(500)).await;
                }
            }
        }
    });
}

/// How often the live monitor reads new decoded RTCM. Short enough that a new
/// epoch reaches the Streams tab well inside even a 1 s epoch interval.
const LIVE_POLL_INTERVAL: Duration = Duration::from_millis(250);
/// Longest gap between snapshots when no new epoch arrives, so a stalled
/// stream still shows its age and falling throughput.
const LIVE_STATS_HEARTBEAT: Duration = Duration::from_secs(5);
/// Minimum spacing of throughput history samples. Polling is much faster
/// than this, so it sets the history's spacing on its own.
const SAMPLE_MIN_MS: i64 = 5_000;
/// History points kept per stream.
const HISTORY_CAP: usize = 60;

/// Latest `live-stream-stats` payload, or `None` before the first one of the
/// current run. Stays available after the run stops (frozen values).
#[tauri::command]
pub fn get_live_stream_stats(live: tauri::State<'_, LiveStats>) -> Option<serde_json::Value> {
    live.latest.lock().unwrap().clone()
}

/// Zero the counters of one mount (`None` = all) and push a fresh snapshot.
/// A no-op when no live run is feeding the loop.
#[tauri::command]
pub fn reset_stream_counters(
    mount: Option<String>,
    live: tauri::State<'_, LiveStats>,
) -> Result<(), String> {
    live.pending_resets.lock().unwrap().push(mount);
    live.wake.notify_one();
    Ok(())
}

fn unix_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_millis() as i64)
}

#[derive(Default)]
struct StreamBytes {
    /// Last seen size per recording file; deltas feed `total`, so deleting a
    /// file never lowers it.
    files: std::collections::HashMap<PathBuf, u64>,
    total: u64,
    last_total: u64,
    last_sample_ms: i64,
    bytes_per_s: Option<f64>,
    history: std::collections::VecDeque<(i64, f64)>,
}

/// Per-stream bytes received, measured from the RTCM recordings PEA writes.
#[derive(Default)]
struct Throughput {
    streams: std::collections::HashMap<String, StreamBytes>,
}

/// Stream a `<STREAM>_..._OBS.rtcm` / `_NAV.rtcm` file belongs to: the longest
/// known mount that prefixes the name, else the text before the first `_`.
fn stream_of(file: &str, mounts: &[String]) -> Option<String> {
    if !(file.ends_with("_OBS.rtcm") || file.ends_with("_NAV.rtcm")) {
        return None;
    }
    let known = mounts
        .iter()
        .filter(|m| file.strip_prefix(m.as_str()).is_some_and(|r| r.starts_with('_')))
        .max_by_key(|m| m.len());
    match known {
        Some(m) => Some(m.clone()),
        None => file.split('_').next().filter(|s| !s.is_empty()).map(String::from),
    }
}

impl Throughput {
    /// Fold in one scan of `(stream, path, size)` for the current recordings.
    fn update(&mut self, sizes: &[(String, PathBuf, u64)], now_ms: i64) {
        let mut seen: std::collections::HashSet<&PathBuf> = std::collections::HashSet::new();
        for (stream, path, size) in sizes {
            seen.insert(path);
            let sb = self.streams.entry(stream.clone()).or_insert_with(|| StreamBytes {
                last_sample_ms: now_ms,
                ..Default::default()
            });
            let prev = sb.files.insert(path.clone(), *size).unwrap_or(0);
            // A smaller size means the file was recreated: all of it is new.
            sb.total += if *size >= prev { *size - prev } else { *size };
        }
        for sb in self.streams.values_mut() {
            sb.files.retain(|p, _| seen.contains(p));
            let dt = now_ms - sb.last_sample_ms;
            if dt >= SAMPLE_MIN_MS {
                let rate = (sb.total - sb.last_total) as f64 / (dt as f64 / 1000.0);
                sb.bytes_per_s = Some(rate);
                sb.history.push_back((now_ms, rate));
                if sb.history.len() > HISTORY_CAP {
                    sb.history.pop_front();
                }
                sb.last_total = sb.total;
                sb.last_sample_ms = now_ms;
            }
        }
    }

    /// Zero totals and history; file sizes are kept so old bytes aren't recounted.
    fn reset(&mut self, mount: Option<&str>, now_ms: i64) {
        for (name, sb) in &mut self.streams {
            if mount.map_or(true, |m| m == name) {
                sb.total = 0;
                sb.last_total = 0;
                sb.last_sample_ms = now_ms;
                sb.bytes_per_s = None;
                sb.history.clear();
            }
        }
    }

    /// `throughput` payload; every name in `mounts` appears, null when no
    /// recording exists for it (`record_rtcm` off).
    fn to_json(&self, mounts: &[String]) -> serde_json::Value {
        let mut out = serde_json::Map::new();
        let names = self.streams.keys().chain(mounts.iter());
        for name in names {
            let v = match self.streams.get(name) {
                Some(sb) => serde_json::json!({
                    "bytes_total": sb.total,
                    "bytes_per_s": sb.bytes_per_s,
                    "history": sb.history.iter().map(|(t, r)| (*t, *r)).collect::<Vec<_>>(),
                }),
                None => serde_json::json!({
                    "bytes_total": null, "bytes_per_s": null, "history": [],
                }),
            };
            out.insert(name.clone(), v);
        }
        serde_json::Value::Object(out)
    }
}

/// Current `(stream, path, size)` of recordings in `dir` written at or after `since`.
fn scan_recordings(
    dir: &Path,
    mounts: &[String],
    since: std::time::SystemTime,
) -> Vec<(String, PathBuf, u64)> {
    let Ok(rd) = std::fs::read_dir(dir) else { return Vec::new() };
    rd.flatten()
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            let stream = stream_of(&name, mounts)?;
            let meta = e.metadata().ok().filter(|m| m.is_file())?;
            (meta.modified().ok()? >= since).then(|| (stream, e.path(), meta.len()))
        })
        .collect()
}

/// Follow the run's decoded RTCM with `rtcm_monitor` and send its snapshot to
/// the Streams tab as `live-stream-stats`, with per-stream throughput from the
/// recordings and `reset_at_ms`.
///
/// A snapshot goes out as soon as a poll sees a new epoch on any stream, so
/// the tab moves with the data, and otherwise every `LIVE_STATS_HEARTBEAT`.
/// Each poll reads only the lines written since the last one and follows the
/// minutely rotation, so the cost stays flat however long the run is.
///
/// With `sbas_mode` set, each snapshot also carries `sbas`: PEA's SISNet log
/// counts and the `.SPP` protection levels, the only SBAS telemetry PEA gives. Needs
/// `decoded_rtcm` output (on by default); without it no file appears and this
/// loop idles until the run ends. Per-satellite fields are dropped before
/// sending, so each event stays a few kB. `reset_stream_counters` wakes the
/// loop early so the reset shows at once.
fn spawn_live_monitor(
    app: tauri::AppHandle,
    output_dir: PathBuf,
    started: std::time::SystemTime,
    sbas_mode: Option<String>,
) {
    tokio::spawn(async move {
        let cancel = app.state::<CancelFlag>().0.clone();
        let running = |app: &tauri::AppHandle| !cancel.load(Ordering::SeqCst) && still_processing(app);

        // Wait for PEA's first file, so following starts at its first record.
        while !has_decoded_file(&output_dir) {
            if !running(&app) {
                return;
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }

        // Start at the beginning of that first file rather than its end.
        let mut follower = crate::commands::rtcm_monitor::Follower::new(&output_dir);
        follower.rewind();
        let mut throughput = Throughput::default();
        let mut spp_levels = crate::commands::sbas_monitor::SppLevels::default();
        let mut reset_at_ms: Option<i64> = None;
        let live = app.state::<LiveStats>();
        // `None` until the first snapshot, which goes out straight away.
        let mut last_sent: Option<(std::time::Instant, u64)> = None;
        while running(&app) {
            let now = unix_ms();
            let resets: Vec<Option<String>> = live.pending_resets.lock().unwrap().drain(..).collect();
            let reset = !resets.is_empty();
            for r in resets {
                match r.as_deref() {
                    Some(m) => follower.monitor.reset_mount(m),
                    None => follower.monitor.reset(),
                }
                throughput.reset(r.as_deref(), now);
                reset_at_ms = Some(now);
            }
            follower.poll();
            let epochs = follower.monitor.new_epochs();
            if snapshot_due(last_sent, epochs, reset, std::time::Instant::now()) {
                last_sent = Some((std::time::Instant::now(), epochs));
                let mut snapshot = follower.monitor.snapshot();
                let mounts: Vec<String> = snapshot["streams"]
                    .as_object()
                    .map(|o| o.keys().cloned().collect())
                    .unwrap_or_default();
                throughput.update(&scan_recordings(&output_dir, &mounts, started), now);
                snapshot["throughput"] = throughput.to_json(&mounts);
                snapshot["reset_at_ms"] = reset_at_ms.into();
                if let Some(mode) = &sbas_mode {
                    spp_levels.poll(&output_dir);
                    let console = live.sbas_console.lock().unwrap().clone();
                    snapshot["sbas"] = serde_json::json!({
                        "mode": mode,
                        "console": console,
                        "levels": spp_levels.to_json(),
                    });
                }
                slim_snapshot(&mut snapshot);
                *live.latest.lock().unwrap() = Some(snapshot.clone());
                app.emit("live-stream-stats", snapshot).ok();
            }
            tokio::select! {
                _ = tokio::time::sleep(LIVE_POLL_INTERVAL) => {}
                _ = live.wake.notified() => {}
            }
        }
    });
}

/// Whether the live loop should send a snapshot now: the first one, after a
/// counter reset, when `epochs` (`Monitor::new_epochs`) has moved since the
/// last one, or once the heartbeat has passed with nothing new.
fn snapshot_due(
    last_sent: Option<(std::time::Instant, u64)>,
    epochs: u64,
    reset: bool,
    now: std::time::Instant,
) -> bool {
    match last_sent {
        None => true,
        Some((at, seen)) => reset || seen != epochs || now.duration_since(at) >= LIVE_STATS_HEARTBEAT,
    }
}

fn has_decoded_file(dir: &Path) -> bool {
    std::fs::read_dir(dir).is_ok_and(|d| {
        d.flatten().any(|e| {
            let n = e.file_name();
            let n = n.to_string_lossy();
            n.starts_with("decoded_rtcm_") && n.ends_with(".json")
        })
    })
}

/// Largest event payload before per-message inter-arrival histories are dropped.
const MAX_PAYLOAD_BYTES: usize = 64 * 1024;

/// Drop the snapshot fields the Streams tab does not show and that grow with
/// the run: per-satellite signal lists (`streams[m].satellites` replaces them)
/// and the list of every file seen. Keeps `streams`, shedding
/// `messages[*].inter_arrival_s` only if the payload exceeds 64 kB.
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
fn slim_snapshot(v: &mut serde_json::Value) {
    if let Some(o) = v.as_object_mut() {
        o.remove("files_seen");
        o.remove("total_by_message");
    }
    if let Some(msm) = v["msm"].as_object_mut() {
        for m in msm.values_mut() {
            if let Some(o) = m.as_object_mut() {
                o.remove("signals_by_satellite");
            }
        }
    }
    let size = serde_json::to_vec(v).map_or(0, |b| b.len());
    if size <= MAX_PAYLOAD_BYTES {
        return;
    }
    eprintln!("[realtime] live snapshot {size} bytes > {MAX_PAYLOAD_BYTES}; dropping inter_arrival_s");
    if let Some(streams) = v["streams"].as_object_mut() {
        for st in streams.values_mut() {
            if let Some(msgs) = st["messages"].as_object_mut() {
                for m in msgs.values_mut() {
                    if let Some(o) = m.as_object_mut() {
                        o.remove("inter_arrival_s");
                    }
                }
            }
        }
    }
}

/// Every `SWEEP_INTERVAL`, list rotated recording files and delete the oldest
/// until total size drops below `cap_bytes`. Ordering is by mtime ascending, so
/// the sweep matches user intuition ("keep the most recent N MB").
///
/// Only files whose name ends in one of `RECORDING_SUFFIXES` are candidates —
/// this deliberately excludes `ntrip_stats.jsonl` (small, no rotation) and any
/// static output like `.POS`, so the sweep never touches a user's positioning
/// results.
fn spawn_retention_sweeper(app: tauri::AppHandle, output_dir: PathBuf, cap_bytes: u64) {
    tokio::spawn(async move {
        let cancel = app.state::<CancelFlag>().0.clone();
        loop {
            if cancel.load(Ordering::SeqCst) || !still_processing(&app) {
                break;
            }
            if let Err(e) = sweep_once(&output_dir, cap_bytes).await {
                app.emit("realtime-sweep-error", e).ok();
            }
            tokio::time::sleep(SWEEP_INTERVAL).await;
        }
    });
}

async fn sweep_once(output_dir: &Path, cap_bytes: u64) -> Result<(), String> {
    let dir = output_dir.to_path_buf();
    let (removed_bytes, removed_files, current_bytes) =
        tokio::task::spawn_blocking(move || sweep_blocking(&dir, cap_bytes))
            .await
            .map_err(|e| format!("sweep join error: {e}"))??;
    if removed_files > 0 {
        eprintln!(
            "[realtime] sweep removed {removed_files} recording files ({removed_bytes} bytes); \
             now at {current_bytes} / cap {cap_bytes} bytes"
        );
    }
    Ok(())
}

#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
fn sweep_blocking(
    output_dir: &Path,
    cap_bytes: u64,
) -> Result<(u64, usize, u64), String> {
    let mut entries: Vec<(PathBuf, u64, std::time::SystemTime)> = Vec::new();
    let mut total: u64 = 0;

    let read = std::fs::read_dir(output_dir)
        .map_err(|e| format!("read_dir {}: {e}", output_dir.display()))?;
    for entry in read.flatten() {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if !RECORDING_SUFFIXES.iter().any(|s| name_str.ends_with(s)) {
            continue;
        }
        let meta = match entry.metadata() {
            Ok(m) if m.is_file() => m,
            _ => continue,
        };
        let size = meta.len();
        let mtime = meta.modified().unwrap_or(std::time::SystemTime::UNIX_EPOCH);
        total += size;
        entries.push((entry.path(), size, mtime));
    }

    if total <= cap_bytes {
        return Ok((0, 0, total));
    }

    // Oldest-first: the last mtime you added is the one most likely still open
    // for writing, so deleting from the tail would race PEA's rotation.
    entries.sort_by_key(|(_, _, t)| *t);

    let mut removed_bytes: u64 = 0;
    let mut removed_files: usize = 0;
    let mut current = total;
    for (path, size, _) in entries {
        if current <= cap_bytes {
            break;
        }
        match std::fs::remove_file(&path) {
            Ok(()) => {
                current = current.saturating_sub(size);
                removed_bytes += size;
                removed_files += 1;
            }
            Err(e) => {
                // Don't abort the sweep on one bad file — a currently-open file
                // on Windows can't be removed, but the next-oldest might be.
                eprintln!("[realtime] could not remove {}: {e}", path.display());
            }
        }
    }

    Ok((removed_bytes, removed_files, current))
}

fn still_processing(app: &tauri::AppHandle) -> bool {
    let st = app.state::<std::sync::Mutex<AppState>>();
    let guard = st.lock().unwrap();
    guard.is_processing
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;
    use std::thread;
    use std::time::{Duration, SystemTime};

    #[test]
    fn a_caster_with_no_saved_login_is_refused_up_front() {
        let none = |_: &str| None;
        let saved = |h: &str| (h == "ntrip.data.gnss.ga.gov.au").then(|| ("u".to_string(), "p".to_string()));
        let err = require_caster_login("https://ntrip.data.gnss.ga.gov.au/", &none).unwrap_err();
        assert!(err.contains("ntrip.data.gnss.ga.gov.au") && err.contains("Settings › Credentials"), "{err}");
        assert!(require_caster_login("https://ntrip.data.gnss.ga.gov.au/", &saved).is_ok());
        // Serial-only runs send no caster.
        assert!(require_caster_login("", &none).is_ok());
    }

    #[test]
    fn sends_a_snapshot_per_new_epoch_and_otherwise_on_the_heartbeat() {
        let t = std::time::Instant::now();
        let soon = t + Duration::from_millis(250);
        assert!(snapshot_due(None, 0, false, t), "first snapshot goes out at once");
        assert!(!snapshot_due(Some((t, 4)), 4, false, soon), "nothing new yet");
        assert!(snapshot_due(Some((t, 4)), 5, false, soon), "a new epoch");
        assert!(snapshot_due(Some((t, 4)), 0, false, soon), "the counter was reset");
        assert!(snapshot_due(Some((t, 4)), 4, true, soon), "a reset was asked for");
        assert!(snapshot_due(Some((t, 4)), 4, false, t + LIVE_STATS_HEARTBEAT), "stalled stream");
    }

    fn write_file(dir: &Path, name: &str, size: usize) -> PathBuf {
        let p = dir.join(name);
        let mut f = fs::File::create(&p).unwrap();
        f.write_all(&vec![0u8; size]).unwrap();
        p
    }

    #[test]
    fn sweep_deletes_oldest_recording_files_until_under_cap() {
        let tmp = tempdir_here("sweep-under-cap");

        // Three 1 KiB recording files, aged apart so mtime ordering is stable.
        let oldest = write_file(&tmp, "R1_2024001_OBS.rtcm", 1024);
        thread::sleep(Duration::from_millis(20));
        let middle = write_file(&tmp, "R1_2024002_OBS.rtcm", 1024);
        thread::sleep(Duration::from_millis(20));
        let newest = write_file(&tmp, "R1_2024003_OBS.rtcm", 1024);

        // Cap = 2 KiB: exactly one file (the oldest) must go.
        let (removed_bytes, removed_files, current) = sweep_blocking(&tmp, 2048).unwrap();
        assert_eq!(removed_files, 1);
        assert_eq!(removed_bytes, 1024);
        assert!(current <= 2048);
        assert!(!oldest.exists(), "oldest file must be removed first");
        assert!(middle.exists(), "middle file must survive");
        assert!(newest.exists(), "newest file must survive");

        cleanup(&tmp);
    }

    #[test]
    fn sweep_ignores_non_recording_files() {
        let tmp = tempdir_here("sweep-ignores");

        // A 5 MiB POS output — must never be touched, even if it dwarfs the cap.
        let pos = write_file(&tmp, "MARKER_2024001.POS", 5 * 1024 * 1024);
        // A stats log — must be preserved even though the cap is tiny.
        let stats = write_file(&tmp, NTRIP_LOG_FILENAME, 4096);

        let (removed_bytes, removed_files, _current) = sweep_blocking(&tmp, 1024).unwrap();
        // Nothing recording-suffixed exists, so the sweep is a no-op even
        // though total on-disk usage is far above the cap.
        assert_eq!(removed_files, 0);
        assert_eq!(removed_bytes, 0);
        assert!(pos.exists(), "static outputs must never be swept");
        assert!(stats.exists(), "telemetry log must never be swept");

        cleanup(&tmp);
    }

    #[test]
    fn sweep_no_op_when_total_under_cap() {
        let tmp = tempdir_here("sweep-noop");
        let a = write_file(&tmp, "R_OBS.rtcm", 512);
        let b = write_file(&tmp, "R_NAV.rtcm", 512);

        let (removed_bytes, removed_files, current) = sweep_blocking(&tmp, 4096).unwrap();
        assert_eq!(removed_bytes, 0);
        assert_eq!(removed_files, 0);
        assert_eq!(current, 1024);
        assert!(a.exists() && b.exists());

        cleanup(&tmp);
    }

    #[test]
    fn throughput_sums_deltas_and_survives_deletion() {
        let mut t = Throughput::default();
        let f1 = PathBuf::from("A_1_OBS.rtcm");
        let f2 = PathBuf::from("A_2_OBS.rtcm");
        t.update(&[("A".into(), f1.clone(), 1000)], 0);
        t.update(&[("A".into(), f1.clone(), 6000)], 5_000);
        // f1 swept, f2 appears
        t.update(&[("A".into(), f2.clone(), 2000)], 10_000);
        let j = t.to_json(&["A".into()]);
        assert_eq!(j["A"]["bytes_total"], 8000);
        let h = j["A"]["history"].as_array().unwrap();
        assert_eq!(h.len(), 2);
        assert_eq!(h[0][1], 1200.0); // 6000 bytes / 5 s
        assert_eq!(h[1][1], 400.0); // 2000 bytes / 5 s
    }

    #[test]
    fn throughput_history_capped_and_null_when_no_recording() {
        let mut t = Throughput::default();
        let f = PathBuf::from("A_1_OBS.rtcm");
        for i in 0..100 {
            t.update(&[("A".into(), f.clone(), i * 10)], i as i64 * 5_000);
        }
        assert_eq!(t.to_json(&[])["A"]["history"].as_array().unwrap().len(), HISTORY_CAP);
        let j = t.to_json(&["B".into()]);
        assert!(j["B"]["bytes_total"].is_null() && j["B"]["bytes_per_s"].is_null());
        assert_eq!(j["B"]["history"].as_array().unwrap().len(), 0);
    }

    #[test]
    fn throughput_reset_scopes_to_mount() {
        let mut t = Throughput::default();
        let (a, b) = (PathBuf::from("A_1_OBS.rtcm"), PathBuf::from("B_1_NAV.rtcm"));
        t.update(&[("A".into(), a.clone(), 100), ("B".into(), b.clone(), 100)], 0);
        t.reset(Some("A"), 1);
        t.update(&[("A".into(), a, 150), ("B".into(), b, 100)], 2);
        let j = t.to_json(&[]);
        assert_eq!(j["A"]["bytes_total"], 50); // old size not recounted
        assert_eq!(j["B"]["bytes_total"], 100);
        t.reset(None, 3);
        assert_eq!(t.to_json(&[])["B"]["bytes_total"], 0);
    }

    #[test]
    fn stream_of_prefers_known_mount_and_ignores_other_files() {
        let mounts = vec!["AB".to_string(), "AB_CD".to_string()];
        assert_eq!(stream_of("AB_CD_x_OBS.rtcm", &mounts).as_deref(), Some("AB_CD"));
        assert_eq!(stream_of("ZZ_x_NAV.rtcm", &mounts).as_deref(), Some("ZZ"));
        assert_eq!(stream_of("ZZ_x_DEC.json", &mounts), None);
    }

    #[test]
    fn slim_snapshot_keeps_streams_and_sheds_inter_arrival_only_when_large() {
        let mut small = serde_json::json!({
            "files_seen": [1], "total_by_message": {},
            "msm": {"m": {"signals_by_satellite": {}, "records": 1}},
            "streams": {"M": {"messages": {"1077": {"inter_arrival_s": [1.0]}}}},
        });
        slim_snapshot(&mut small);
        assert!(small.get("files_seen").is_none());
        assert!(small["msm"]["m"].get("signals_by_satellite").is_none());
        assert!(small["streams"]["M"]["messages"]["1077"].get("inter_arrival_s").is_some());

        let big: Vec<f64> = vec![1.234567; 20_000];
        let mut v = serde_json::json!({"streams": {"M": {"messages": {"1077": {"inter_arrival_s": big, "instances": 3}}}}});
        slim_snapshot(&mut v);
        let m = &v["streams"]["M"]["messages"]["1077"];
        assert!(m.get("inter_arrival_s").is_none());
        assert_eq!(m["instances"], 3);
    }

    /// Fresh, unique tmp dir. `tempfile` isn't in the deps and the sweep
    /// logic is small enough that a hand-rolled unique path is safer than
    /// adding a dev-dep.
    fn tempdir_here(tag: &str) -> PathBuf {
        let ts = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let p = std::env::temp_dir().join(format!("ginan-realtime-{tag}-{ts}"));
        std::fs::create_dir_all(&p).unwrap();
        p
    }

    fn cleanup(p: &Path) {
        let _ = std::fs::remove_dir_all(p);
    }
}
