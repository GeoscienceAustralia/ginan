//! Stream test: a short PEA run before a realtime session, to find out what the
//! streams actually carry.
//!
//! PEA runs the realtime config for a few epochs with only the preprocessor on
//! and `decoded_rtcm` output enabled, writing into `<output>/stream_test/`.
//! `rtcm_monitor` (a Rust port of GA's `rtcm_realtime_monitor.py`) then
//! summarises the decoded records. From that summary the UI takes the
//! constellations and signal codes seen. The summary does not include the
//! observation rate, so the epoch interval is worked out here from the MSM
//! `EpochTimeGPST` values.
//!
//! The test never modifies the real config; it writes its own copy.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::Path;
use std::time::Duration;

use serde::{Deserialize, Serialize};
use tauri::{Emitter, Manager};

use crate::commands::rinex::ConstellationObsCodes;
use crate::commands::streams::SerialSettings;
use crate::state::{AppState, CancelFlag};

const TEST_DIR: &str = "stream_test";
const DEFAULT_TEST_EPOCHS: u32 = 10;
/// Colon-free so it is a valid filename on NTFS as well as on macOS and Linux.
const DECODED_FILENAME: &str = "decoded_rtcm_<YYYY><DDD><HH><mm>_DEC.json";

#[derive(Debug, Deserialize)]
pub struct StreamTestOptions {
    /// The realtime config just written by `write_config`.
    pub config_path: String,
    pub output_dir: String,
    #[serde(default)]
    pub caster_url: String,
    #[serde(default)]
    pub serial: HashMap<String, SerialSettings>,
    /// How many 1-second epochs to watch. Defaults to `DEFAULT_TEST_EPOCHS`.
    pub epochs: Option<u32>,
    /// The SBAS mode when the run uses SBAS: the app then checks SISNet
    /// itself (`sbas_monitor::probe`) alongside the test's PEA run.
    #[serde(default)]
    pub sbas_mode: Option<String>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct StreamStats {
    pub mountpoint: String,
    pub records: u64,
    pub satellites: u64,
    /// Mean seconds between observation epochs; `None` with fewer than two.
    pub interval_s: Option<f64>,
}

#[derive(Debug, Serialize)]
pub struct StreamTestResult {
    /// Constellations with observations, as the UI names them (GPS, GAL, …).
    pub constellations: Vec<String>,
    /// Signal codes seen per constellation, e.g. `gps: ["L1C", "L5Q"]`.
    pub observation_codes: ConstellationObsCodes,
    /// Mean of the per-stream intervals; `None` if no stream had two epochs.
    pub interval_s: Option<f64>,
    pub streams: Vec<StreamStats>,
    /// The monitor's full snapshot (MSM, SSR, ephemeris, totals), shown
    /// as-is in the Streams tab.
    pub snapshot: serde_json::Value,
    /// What the SISNet check found; absent without SBAS.
    pub sbas: Option<crate::commands::sbas_monitor::SbasProbe>,
}

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn test_streams(
    options: StreamTestOptions,
    app: tauri::AppHandle,
    cancel: tauri::State<'_, CancelFlag>,
) -> Result<StreamTestResult, String> {
    let cancel_flag = cancel.begin();
    let dir = Path::new(&options.output_dir).join(TEST_DIR);
    // Start clean so the monitor only reads this test's records.
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).map_err(|e| format!("Cannot create {}: {e}", dir.display()))?;

    let epochs = options.epochs.unwrap_or(DEFAULT_TEST_EPOCHS).max(2);
    let config_path = dir.join("stream_test.yaml");
    write_test_config(Path::new(&options.config_path), &dir, &config_path, epochs)?;
    let config_str = config_path.to_string_lossy().to_string();

    let say = |line: String| {
        app.emit("console-line", format!("[test] {line}")).ok();
    };
    crate::commands::realtime::require_caster_login(
        &options.caster_url,
        &crate::commands::credentials_ntrip::ntrip_auth_for_host,
    )?;
    say(format!("Testing streams for {epochs} epochs"));
    say(format!("Test config: {}", config_path.display()));
    crate::commands::streams::prepare(&app, &config_str, &options.serial).await?;
    // The SISNet check runs beside PEA, which the test config keeps off
    // SISNet, so the one login never holds two sessions.
    let probe = async {
        match &options.sbas_mode {
            Some(mode) => Some(
                crate::commands::sbas_monitor::probe(mode, Duration::from_secs(epochs as u64), cancel_flag.clone())
                    .await,
            ),
            None => None,
        }
    };
    let (run, sbas) = tokio::join!(run_pea(&app, &options.caster_url, &config_str, &dir, epochs), probe);
    crate::commands::streams::release(&app);
    if let Some(p) = &sbas {
        let detail = p.error.clone().unwrap_or_else(|| {
            format!("{} messages in {:.0} s", p.counts.messages, p.seconds)
        });
        app.emit("console-line", format!("[test] SBAS {} (PRN {}): {detail}", p.host, p.prn)).ok();
    }
    let log_path = dir.join("pea.log");
    let secret = crate::commands::realtime::caster_login(&options.caster_url).map(|(_, p)| p);
    let log_tail = || {
        tail_lines(&log_path, LOG_TAIL_LINES)
            .into_iter()
            .map(|l| mask_logins(&l, secret.as_deref()))
            .collect::<Vec<_>>()
    };
    if cancel_flag.load(std::sync::atomic::Ordering::SeqCst) {
        return Err("Stream test stopped".into());
    }
    let status = match run {
        Ok(status) => status,
        Err(e) => {
            // Timed out (or failed to wait): what PEA was doing is in its log.
            let tail = log_tail();
            for line in &tail {
                say(format!("pea.log: {line}"));
            }
            let log = std::fs::read_to_string(&log_path).unwrap_or_default();
            return Err(match stuck_reason(&log) {
                Some(why) => format!("{e}. {why}"),
                None => e,
            });
        }
    };
    let log_tail = log_tail();
    say(format!("PEA exited: {status}"));
    if !status.success() {
        for line in &log_tail {
            say(format!("pea.log: {line}"));
        }
        return Err(pea_failure(&status.to_string(), &log_tail, &log_path));
    }

    let snapshot = run_monitor(&app, &dir);
    let mut result = match summarise(&snapshot) {
        Ok(r) => r,
        Err(e) => {
            // PEA ran but nothing decoded: the end of its log says why.
            say(format!("Decoded files in {}: {}", dir.display(), decoded_files(&dir)));
            for line in &log_tail {
                say(format!("pea.log: {line}"));
            }
            return Err(format!("{e} PEA's log is at {}.", log_path.display()));
        }
    };

    // Fill in each stream's rate from the raw records, then the overall mean.
    let intervals = epoch_intervals(&dir)?;
    for s in &mut result.streams {
        s.interval_s = intervals.get(&s.mountpoint).copied();
    }
    let known: Vec<f64> = result.streams.iter().filter_map(|s| s.interval_s).collect();
    result.interval_s = (!known.is_empty()).then(|| known.iter().sum::<f64>() / known.len() as f64);
    result.snapshot = snapshot;
    result.sbas = sbas;
    Ok(result)
}

/// Copy the realtime config with the test's changes: decode-only, own output
/// directory, a fixed number of 1 s epochs, and every output off except the
/// decoded RTCM the monitor reads.
fn write_test_config(src: &Path, dir: &Path, dest: &Path, epochs: u32) -> Result<(), String> {
    use serde_yaml::Value;
    let text = std::fs::read_to_string(src).map_err(|e| format!("Cannot read {}: {e}", src.display()))?;
    let mut config: Value = serde_yaml::from_str(&text).map_err(|e| format!("YAML error: {e}"))?;

    let outputs = config["outputs"].as_mapping_mut().ok_or("config has no outputs block")?;
    for (_, v) in outputs.iter_mut() {
        if let Some(m) = v.as_mapping_mut() {
            if m.contains_key("output") {
                m.insert("output".into(), false.into());
            }
        }
    }
    outputs.insert("outputs_root".into(), dir.to_string_lossy().to_string().into());
    let mut decoded = serde_yaml::Mapping::new();
    decoded.insert("output".into(), true.into());
    // Explicit time tags, not `<LOGTIME>`: PEA expands `<LOGTIME>` with a
    // colon, which is legal on macOS and Linux but is alternate-data-stream
    // syntax on NTFS, so Windows silently writes the records into a hidden
    // stream and leaves a 0-byte file the `decoded_rtcm_*.json` scan misses.
    // Same pattern the realtime recordings use (see `config/realtime.rs`).
    decoded.insert("filename".into(), DECODED_FILENAME.into());
    outputs.insert("decoded_rtcm".into(), decoded.into());

    // Decoding the streams needs no SBAS, and the app checks SISNet itself
    // during the test: PEA staying off it keeps to one session per login.
    if let Some(sat) = config["inputs"]["satellite_data"].as_mapping_mut() {
        sat.remove("sbas_inputs");
    }
    let po = &mut config["processing_options"];
    for (mode, on) in [("preprocessor", true), ("spp", false), ("ppp", false), ("sbas", false)] {
        po["process_modes"][mode] = on.into();
    }
    po["epoch_control"]["epoch_interval"] = 1.into();
    po["epoch_control"]["max_epochs"] = epochs.into();

    let out = serde_yaml::to_string(&config).map_err(|e| format!("YAML error: {e}"))?;
    std::fs::write(dest, out).map_err(|e| format!("Cannot write {}: {e}", dest.display()))
}

/// Lines of `pea.log` shown in the Console when the test fails.
const LOG_TAIL_LINES: usize = 20;

/// The last `n` lines of a file, or none if it cannot be read.
fn tail_lines(path: &Path, n: usize) -> Vec<String> {
    let text = std::fs::read_to_string(path).unwrap_or_default();
    let lines: Vec<&str> = text.lines().filter(|l| !l.trim().is_empty()).collect();
    lines[lines.len().saturating_sub(n)..].iter().map(|l| l.to_string()).collect()
}

/// `line` with any `scheme://user:pass@` login, and the caster password
/// itself, replaced: PEA can print the URLs it opens, and the Console is
/// copied into bug reports.
fn mask_logins(line: &str, password: Option<&str>) -> String {
    let mut out = String::with_capacity(line.len());
    let mut rest = line;
    while let Some(i) = rest.find("://") {
        let (head, tail) = rest.split_at(i + 3);
        out.push_str(head);
        let end = tail.find(|c: char| c == '/' || c.is_whitespace()).unwrap_or(tail.len());
        match tail[..end].rfind('@') {
            Some(at) => {
                out.push_str("***");
                rest = &tail[at..];
            }
            None => rest = tail,
        }
    }
    out.push_str(rest);
    match password {
        Some(p) if !p.is_empty() => out.replace(p, "***"),
        _ => out,
    }
}

/// Why PEA stopped, from its exit status and the end of its log. A missing
/// shared library (macOS `dyld`, Linux `ld.so`) means PEA never started, which
/// is worth naming, since the fix is outside the app.
fn pea_failure(status: &str, log_tail: &[String], log_path: &Path) -> String {
    let missing = log_tail.iter().find_map(|l| {
        let i = l.find("Library not loaded: ").map(|i| i + 20).or_else(|| {
            l.find("error while loading shared libraries: ").map(|i| i + 38)
        })?;
        let lib = l[i..].split([':', ' ']).next()?.trim();
        Some(lib.rsplit('/').next().unwrap_or(lib).to_string())
    });
    if let Some(lib) = missing {
        let hint = if lib.contains("libomp") && cfg!(target_os = "macos") {
            " On macOS, install it with `brew install libomp`."
        } else {
            ""
        };
        return format!(
            "PEA could not start: the library {lib} is missing.{hint} Or choose another PEA binary in Settings › PEA Binary."
        );
    }
    let last = log_tail.last().map(|l| format!(" Last line of its log: {l}")).unwrap_or_default();
    format!("PEA stopped during the stream test ({status}).{last} See the Console, or {}.", log_path.display())
}

/// Why PEA got nowhere, from its whole log, when that is recognisable.
fn stuck_reason(log: &str) -> Option<String> {
    // PEA's URL parser finds the host only after `user:pass@`.
    if log.contains("connectHandler for https://:") || log.contains("connectHandler for http://:") {
        return Some(
            "PEA could not connect to the streams: their addresses reached it with no host, \
             which happens when they carry no login. Check the caster login in Settings › Credentials."
                .into(),
        );
    }
    if log.contains("401") && log.contains("NTRIP") {
        return Some("The caster refused the login (HTTP 401). Check it in Settings › Credentials.".into());
    }
    None
}

/// The decoded RTCM files the test wrote, for the Console when none decoded.
fn decoded_files(dir: &Path) -> String {
    let names: Vec<String> = std::fs::read_dir(dir)
        .into_iter()
        .flatten()
        .flatten()
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|n| n.starts_with("decoded_rtcm_"))
        .collect();
    if names.is_empty() { "none".into() } else { names.join(", ") }
}

/// Run PEA on the test config. Its output goes to `pea.log` rather than the
/// Console: a 10 s run prints thousands of lines. Stop works as for a normal
/// run, because the pid is registered in `AppState`. Returns how PEA exited.
async fn run_pea(
    app: &tauri::AppHandle,
    caster_url: &str,
    config_path: &str,
    dir: &Path,
    epochs: u32,
) -> Result<std::process::ExitStatus, String> {
    let pea = crate::commands::execution::resolve_pea_binary(
        &app.state::<std::sync::Mutex<AppState>>().lock().unwrap(),
    )?;
    app.emit("console-line", format!("[test] PEA: {}", pea.display())).ok();
    let log = std::fs::File::create(dir.join("pea.log")).map_err(|e| format!("Cannot create pea.log: {e}"))?;
    let _ = crate::commands::pea_runtime::ensure_libomp(app).await;
    let mut cmd = tokio::process::Command::new(pea);
    crate::commands::pea_runtime::apply_env(&mut cmd, app);
    cmd.arg("--config").arg(config_path);
    if let Some((u, p)) = crate::commands::realtime::caster_login(caster_url) {
        cmd.arg("--user").arg(u).arg("--pass").arg(p);
    }
    let mut child = cmd
        .current_dir(dir)
        .stdout(log.try_clone().map_err(|e| e.to_string())?)
        .stderr(log)
        .kill_on_drop(true)
        .spawn()
        .map_err(|e| format!("Failed to start PEA for the stream test: {e}"))?;
    app.state::<std::sync::Mutex<AppState>>().lock().unwrap().pea_pid = child.id();

    // PEA stops itself after `epochs`; the margin covers connecting to casters
    // and waiting for the first data.
    let limit = Duration::from_secs(epochs as u64 + 45);
    let status = tokio::time::timeout(limit, child.wait()).await;
    app.state::<std::sync::Mutex<AppState>>().lock().unwrap().pea_pid = None;
    match status {
        Ok(Ok(st)) => Ok(st), // Stop kills it with a signal; the caller checks the cancel flag first
        Ok(Err(e)) => Err(format!("Stream test PEA failed: {e}")),
        Err(_) => Err(format!(
            "Stream test timed out after {}s; see {}",
            limit.as_secs(),
            dir.join("pea.log").display()
        )),
    }
}

/// Summarise the test output (see `rtcm_monitor`) and report each stream in the Console.
fn run_monitor(app: &tauri::AppHandle, dir: &Path) -> serde_json::Value {
    let snapshot = crate::commands::rtcm_monitor::replay_dir(dir);
    for (mount, m) in snapshot["msm"].as_object().into_iter().flatten() {
        app.emit(
            "console-line",
            format!(
                "[test] {mount}: {} observations, {} satellites, {} invalid, avg SNR {}",
                m["records"], m["satellite_count"], m["invalid_records"], m["avg_snr"]
            ),
        )
        .ok();
    }
    app.emit("console-line", format!("[test] records by type: {}", snapshot["total_by_type"])).ok();
    snapshot
}

/// Constellations, codes and per-stream counts from the monitor's snapshot.
fn summarise(snapshot: &serde_json::Value) -> Result<StreamTestResult, String> {
    let msm = snapshot["msm"].as_object().ok_or("monitor snapshot has no msm section")?;
    if msm.is_empty() {
        return Err("The stream test received no observations. Check the streams and the Console.".into());
    }
    let mut codes: BTreeMap<&str, BTreeSet<String>> = BTreeMap::new();
    let mut streams = Vec::new();
    for (mountpoint, info) in msm {
        streams.push(StreamStats {
            mountpoint: mountpoint.clone(),
            records: info["records"].as_u64().unwrap_or(0),
            satellites: info["satellite_count"].as_u64().unwrap_or(0),
            interval_s: None,
        });
        for (constellation, list) in info["signals_by_constellation"].as_object().into_iter().flatten() {
            // The monitor's names match the UI's; IRN/SBS have no UI and are skipped.
            if let Some(key) = ["GPS", "GAL", "GLO", "BDS", "QZS"].into_iter().find(|c| c == constellation) {
                let set = codes.entry(key).or_default();
                set.extend(list.as_array().into_iter().flatten().filter_map(|c| c.as_str().map(String::from)));
            }
        }
    }
    let take = |c: &str| codes.get(c).map(|s| s.iter().cloned().collect()).unwrap_or_default();
    Ok(StreamTestResult {
        constellations: codes.keys().map(|c| c.to_string()).collect(),
        observation_codes: ConstellationObsCodes {
            gps: take("GPS"),
            gal: take("GAL"),
            glo: take("GLO"),
            bds: take("BDS"),
            qzs: take("QZS"),
        },
        interval_s: None,
        streams,
        snapshot: serde_json::Value::Null,
        sbas: None,
    })
}

/// Mean seconds between distinct MSM epochs, per mountpoint, over every
/// decoded file in `dir`.
fn epoch_intervals(dir: &Path) -> Result<HashMap<String, f64>, String> {
    let mut text = String::new();
    for entry in std::fs::read_dir(dir).map_err(|e| e.to_string())?.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        if name.starts_with("decoded_rtcm_") && name.ends_with(".json") {
            text.push_str(&std::fs::read_to_string(entry.path()).map_err(|e| e.to_string())?);
            text.push('\n');
        }
    }
    Ok(intervals_from_records(&text))
}

fn intervals_from_records(jsonl: &str) -> HashMap<String, f64> {
    let mut epochs: HashMap<String, BTreeSet<i64>> = HashMap::new();
    for line in jsonl.lines() {
        let Ok(rec) = serde_json::from_str::<serde_json::Value>(line) else { continue };
        if rec["type"] != "MSM" {
            continue;
        }
        let (Some(mount), Some(t)) = (rec["Mountpoint"].as_str(), rec["EpochTimeGPST"].as_str()) else { continue };
        if let Ok(dt) = chrono::NaiveDateTime::parse_from_str(t, "%Y-%m-%d %H:%M:%S%.f") {
            // Milliseconds, so sub-second rates (e.g. 5 Hz) survive.
            epochs.entry(mount.to_string()).or_default().insert(dt.and_utc().timestamp_millis());
        }
    }
    epochs
        .into_iter()
        .filter(|(_, e)| e.len() >= 2)
        .map(|(mount, e)| {
            let span = e.last().unwrap() - e.first().unwrap();
            (mount, span as f64 / 1000.0 / (e.len() - 1) as f64)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_a_missing_library_as_the_reason_pea_did_not_start() {
        let log = vec![
            "dyld[76739]: Library not loaded: /opt/homebrew/opt/libomp/lib/libomp.dylib".to_string(),
            "  Referenced from: <67A6> /Users/x/pea".to_string(),
        ];
        let msg = pea_failure("signal: 6 (SIGABRT)", &log, Path::new("/out/stream_test/pea.log"));
        assert!(msg.contains("the library libomp.dylib is missing"), "{msg}");
        assert!(msg.contains("PEA Binary"), "{msg}");

        let linux = vec!["./pea: error while loading shared libraries: libyaml-cpp.so.0.8: cannot open".to_string()];
        assert!(pea_failure("exit status: 127", &linux, Path::new("p")).contains("libyaml-cpp.so.0.8"));

        let other = vec!["Something else went wrong".to_string()];
        let msg = pea_failure("exit status: 1", &other, Path::new("/out/pea.log"));
        assert!(msg.contains("exit status: 1") && msg.contains("Something else went wrong"), "{msg}");
    }

    #[test]
    fn explains_a_test_that_got_nowhere_from_peas_log() {
        let log = "Starting epoch #1\nError: NTRIP - Invalid argument in connectHandler for https://:443/ALIC00AUS0\n";
        assert!(stuck_reason(log).unwrap().contains("no host"));
        assert!(stuck_reason("Error: NTRIP - server responded 401").unwrap().contains("401"));
        assert!(stuck_reason("Starting epoch #1").is_none());
    }

    #[test]
    fn masks_logins_before_log_lines_reach_the_console() {
        assert_eq!(
            mask_logins("open https://simon:s3cret@caster.example/MNT and sisnet://u:p@h:62005/", None),
            "open https://***@caster.example/MNT and sisnet://***@h:62005/"
        );
        assert_eq!(mask_logins("pass=s3cret here", Some("s3cret")), "pass=*** here");
        assert_eq!(mask_logins("no login in https://host/x", None), "no login in https://host/x");
    }

    #[test]
    fn interval_is_mean_gap_between_distinct_epochs() {
        let rec = |m: &str, t: &str| format!(r#"{{"type":"MSM","Mountpoint":"{m}","EpochTimeGPST":"{t}"}}"#);
        let jsonl = [
            rec("RX1", "2026-09-22 08:59:09.00"),
            rec("RX1", "2026-09-22 08:59:09.00"), // same epoch, another satellite
            rec("RX1", "2026-09-22 08:59:10.00"),
            rec("RX1", "2026-09-22 08:59:11.00"),
            rec("SLOW", "2026-09-22 08:59:00.00"),
            rec("SLOW", "2026-09-22 08:59:30.00"),
            rec("ONE", "2026-09-22 08:59:00.00"),
            r#"{"type":"brdcEph","Mountpoint":"RX1","EpochTimeGPST":"2026-09-22 09:00:00.00"}"#.into(),
            "not json".into(),
        ]
        .join("\n");
        let got = intervals_from_records(&jsonl);
        assert_eq!(got.get("RX1"), Some(&1.0));
        assert_eq!(got.get("SLOW"), Some(&30.0));
        assert_eq!(got.get("ONE"), None, "one epoch gives no rate");
    }

    #[test]
    fn test_config_is_decode_only_and_writes_elsewhere() {
        let dir = std::env::temp_dir().join(format!("ginan-stream-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let src = dir.join("rt.yaml");
        std::fs::write(
            &src,
            "outputs:\n  outputs_root: /real/out\n  pos: {output: true}\n  rtcm_obs: {output: true, filename: x}\n\
             inputs:\n  satellite_data:\n    rtcm_inputs: {rtcm_inputs: [BCEP00BKG0]}\n    sbas_inputs: {sisnet_inputs: [\"sisnet://u:p@h:62005/\"]}\n\
             processing_options:\n  process_modes: {preprocessor: true, spp: true, ppp: true, sbas: true}\n  \
             epoch_control: {epoch_interval: 30, wait_next_epoch: 10}\n",
        )
        .unwrap();
        let dest = dir.join("test.yaml");
        write_test_config(&src, &dir, &dest, 10).unwrap();
        let c: serde_yaml::Value = serde_yaml::from_str(&std::fs::read_to_string(&dest).unwrap()).unwrap();
        std::fs::remove_dir_all(&dir).ok();

        assert_eq!(c["outputs"]["outputs_root"].as_str(), dir.to_str());
        assert_eq!(c["outputs"]["pos"]["output"].as_bool(), Some(false));
        assert_eq!(c["outputs"]["rtcm_obs"]["output"].as_bool(), Some(false));
        assert_eq!(c["outputs"]["decoded_rtcm"]["output"].as_bool(), Some(true));
        // The app checks SISNet itself during the test; PEA stays off it.
        assert!(c["inputs"]["satellite_data"]["sbas_inputs"].is_null());
        assert_eq!(c["inputs"]["satellite_data"]["rtcm_inputs"]["rtcm_inputs"][0].as_str(), Some("BCEP00BKG0"));
        assert_eq!(c["processing_options"]["process_modes"]["sbas"].as_bool(), Some(false));
        // `<LOGTIME>` expands with a colon, which NTFS reads as an alternate
        // data stream: the records vanish into it and the file scan sees 0 bytes.
        let decoded_name = c["outputs"]["decoded_rtcm"]["filename"].as_str().unwrap();
        assert_eq!(decoded_name, DECODED_FILENAME);
        assert!(!decoded_name.contains("<LOGTIME>"), "colon-producing tag is not usable on Windows");
        assert!(decoded_name.ends_with(".json"), "the monitor only scans *.json");
        let pm = &c["processing_options"]["process_modes"];
        assert_eq!((pm["preprocessor"].as_bool(), pm["spp"].as_bool(), pm["ppp"].as_bool()), (Some(true), Some(false), Some(false)));
        let ec = &c["processing_options"]["epoch_control"];
        assert_eq!((ec["epoch_interval"].as_u64(), ec["max_epochs"].as_u64()), (Some(1), Some(10)));
        assert_eq!(ec["wait_next_epoch"].as_u64(), Some(10), "other settings are kept");
    }

    #[test]
    fn summarises_constellations_and_codes_from_snapshot() {
        let snap = serde_json::json!({ "msm": {
            "cu.usbserial-110": { "records": 431, "satellite_count": 34, "signals_by_constellation":
                { "GPS": ["L1C", "L5Q"], "GAL": ["L1C", "L5Q"], "IRN": ["L5A"] } },
            "ALIC00AUS0": { "records": 900, "satellite_count": 20, "signals_by_constellation":
                { "GPS": ["L1C", "L2W"] } }
        }});
        let r = summarise(&snap).unwrap();
        assert_eq!(r.constellations, ["GAL", "GPS"]);
        assert_eq!(r.observation_codes.gps, ["L1C", "L2W", "L5Q"]);
        assert!(r.observation_codes.bds.is_empty());
        assert_eq!(r.streams.len(), 2);
        assert!(summarise(&serde_json::json!({ "msm": {} })).is_err());
    }
}
