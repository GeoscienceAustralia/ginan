//! Live input streams PEA can open, and the host-side setup each one needs
//! before PEA starts.
//!
//! PEA picks a stream class from the URI scheme (`src/cpp/pea/inputs.cpp`):
//! `serial://`, `http://` / `https://` / `ntrip://` (all NTRIP), `sisnet://`
//! and `file://`. A bare entry with no scheme is a mountpoint that PEA joins to
//! the caster root. Files are not live streams and are handled by the file
//! workflow, so they are rejected here.
//!
//! NTRIP and SISNeT need no setup: PEA connects itself, and NTRIP logins go in
//! as `--user`/`--pass`. A serial receiver does need setup, which
//! `scripts/serial/start_gnss_stream.sh` provides (see `scripts/serial/README.md`):
//! it turns on the receiver's RTCM output and keeps the port at the right baud
//! for as long as it runs. It stays up for the whole PEA run and is stopped by
//! [`release`] when PEA exits. Windows has no bash or python3 and needs no
//! holder, so it does the same setup natively (`serial_windows.rs`).

use std::collections::{BTreeSet, HashMap};

use serde::Deserialize;
use tauri::{Emitter, Manager};
#[cfg(not(windows))]
use tokio::io::AsyncBufReadExt;

use crate::state::AppState;

/// Bundled at compile time so the app does not need to find the script on disk.
#[cfg(not(windows))]
static SERIAL_SETUP_SCRIPT: &str = include_str!("../../../../serial/start_gnss_stream.sh");

/// Line settings for one serial receiver, from its Direct URI › Serial fields.
/// They only go to the setup script, which puts the port in this mode; the
/// PEA config is not touched. Defaults match the script's own.
#[derive(Clone, Debug, Deserialize)]
#[serde(default)]
pub struct SerialSettings {
    pub baud: u32,
    pub data: u8,
    pub parity: String,
    pub stop: u8,
    pub flow: String,
    pub flush: bool,
    pub flush_duration_ms: u32,
}

impl Default for SerialSettings {
    fn default() -> Self {
        Self {
            baud: 115200,
            data: 8,
            parity: "none".into(),
            stop: 1,
            flow: "none".into(),
            flush: true,
            flush_duration_ms: 0,
        }
    }
}

/// Serial devices on this machine, for the Direct URI port suggestions.
/// macOS lists each port twice (`tty.*` and `cu.*`); only `cu.*` is offered,
/// as it is the one that opens without waiting for a carrier signal.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn list_serial_ports() -> Vec<String> {
    #[cfg(windows)]
    return crate::commands::serial_windows::list_ports();
    #[allow(unreachable_code)]
    let mut ports: Vec<String> = std::fs::read_dir("/dev")
        .map(|dir| {
            dir.flatten()
                .map(|e| e.file_name().to_string_lossy().into_owned())
                .filter(|n| ["cu.", "ttyUSB", "ttyACM"].iter().any(|p| n.starts_with(p)))
                .map(|n| format!("/dev/{n}"))
                .collect()
        })
        .unwrap_or_default();
    ports.sort();
    ports
}

#[derive(Debug, PartialEq)]
pub enum StreamKind {
    Ntrip,
    Sisnet,
    Serial { device: String },
}

/// Classify one `rtcm_inputs` entry the way PEA will.
pub fn classify(uri: &str) -> Result<StreamKind, String> {
    match uri.split_once("://") {
        None => Ok(StreamKind::Ntrip),
        Some(("serial", device)) => Ok(StreamKind::Serial { device: device.to_string() }),
        Some(("http" | "https" | "ntrip", _)) => Ok(StreamKind::Ntrip),
        Some(("sisnet", _)) => Ok(StreamKind::Sisnet),
        Some(("file", _)) => Err(format!("`{uri}` is a file, not a live stream")),
        Some((scheme, _)) => Err(format!("PEA has no stream type for `{scheme}://` ({uri})")),
    }
}

/// Every RTCM stream entry in the config PEA is about to run.
fn stream_uris(config_path: &str) -> Result<Vec<String>, String> {
    let text = std::fs::read_to_string(config_path)
        .map_err(|e| format!("Cannot read {config_path}: {e}"))?;
    let config: serde_yaml::Value =
        serde_yaml::from_str(&text).map_err(|e| format!("YAML error in {config_path}: {e}"))?;
    let inputs = &config["inputs"];
    let lists = [
        &inputs["gnss_observations"]["rtcm_inputs"],
        &inputs["satellite_data"]["rtcm_inputs"]["rtcm_inputs"],
    ];
    Ok(lists
        .iter()
        .filter_map(|v| v.as_sequence())
        .flatten()
        .filter_map(|v| v.as_str().map(str::to_string))
        .collect())
}

/// Get every stream in the config ready to open. Call this before spawning PEA.
/// On error, anything already set up has been stopped again.
pub async fn prepare(
    app: &tauri::AppHandle,
    config_path: &str,
    serial: &HashMap<String, SerialSettings>,
) -> Result<(), String> {
    let mut devices = BTreeSet::new();
    for uri in stream_uris(config_path)? {
        if let StreamKind::Serial { device } = classify(&uri)? {
            devices.insert(device);
        }
    }
    // One setup script per device, even if the same receiver feeds both the
    // observation and the ephemeris list.
    for device in devices {
        let settings = serial.get(&device).cloned().unwrap_or_default();
        if let Err(e) = hold_serial(app, &device, &settings).await {
            release(app);
            return Err(e);
        }
    }
    Ok(())
}

/// Stop every setup script started by [`prepare`], which frees their ports.
pub fn release(app: &tauri::AppHandle) {
    let pgids = std::mem::take(&mut app.state::<std::sync::Mutex<AppState>>().lock().unwrap().stream_holders);
    // Signal the whole process group: bash plus the python child that
    // actually holds the port open. Windows never records any holders.
    #[cfg(unix)]
    for pgid in pgids {
        std::process::Command::new("kill")
            .args(["-TERM", "--", &format!("-{pgid}")])
            .status()
            .ok();
    }
    #[cfg(not(unix))]
    drop(pgids);
}

/// Run the setup script for one device and wait until it reports the port
/// ready. The script keeps running afterwards to hold the port.
async fn hold_serial(
    app: &tauri::AppHandle,
    device: &str,
    serial: &SerialSettings,
) -> Result<(), String> {
    app.emit("console-line", format!("Preparing serial receiver {device}...")).ok();
    hold_serial_platform(app, device, serial).await
}

/// Windows half of [`hold_serial`]: no bash/python holder is needed, the port
/// is configured natively.
#[cfg(windows)]
async fn hold_serial_platform(
    app: &tauri::AppHandle,
    device: &str,
    serial: &SerialSettings,
) -> Result<(), String> {
    let (app, device, serial) = (app.clone(), device.to_string(), serial.clone());
    tokio::task::spawn_blocking(move || {
        crate::commands::serial_windows::prepare(&device, &serial, |line| {
            app.emit("console-line", format!("[serial] {line}")).ok();
        })
        .map_err(|e| format!("Serial setup for {device} failed: {e}"))
    })
    .await
    .map_err(|e| e.to_string())?
}

/// Unix half of [`hold_serial`]: run the bundled bash script as a holder.
#[cfg(not(windows))]
async fn hold_serial_platform(
    app: &tauri::AppHandle,
    device: &str,
    serial: &SerialSettings,
) -> Result<(), String> {
    let mut cmd = tokio::process::Command::new("bash");
    cmd.arg("-c")
        .arg(SERIAL_SETUP_SCRIPT)
        .arg("start_gnss_stream.sh") // $0
        .arg(device)
        .env("BAUD", serial.baud.to_string())
        .env("DATA", serial.data.to_string())
        .env("PARITY", &serial.parity)
        .env("STOP", serial.stop.to_string())
        .env("FLOW", &serial.flow)
        .env("FLUSH", serial.flush.to_string())
        .env("FLUSH_MS", serial.flush_duration_ms.to_string())
        .env_remove("CONFIG") // no CONFIG = prepare and hold only; the app launches PEA itself
        .env("PYTHONUNBUFFERED", "1") // otherwise the preflight report shows up late, in one block
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(false);
    #[cfg(unix)]
    cmd.process_group(0);

    let mut child = cmd
        .spawn()
        .map_err(|e| format!("Cannot start serial setup for {device}: {e}"))?;
    let pgid = child.id().ok_or("Serial setup exited immediately")?;

    let (ready_tx, ready_rx) = tokio::sync::oneshot::channel::<()>();
    if let Some(out) = child.stdout.take() {
        let app = app.clone();
        tokio::spawn(async move {
            let mut ready_tx = Some(ready_tx);
            let mut lines = tokio::io::BufReader::new(out).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                // The script prints "ready   : …" once the port is held.
                if line.starts_with("ready") {
                    if let Some(tx) = ready_tx.take() {
                        tx.send(()).ok();
                    }
                }
                app.emit("console-line", format!("[serial] {line}")).ok();
            }
        });
    }
    if let Some(err) = child.stderr.take() {
        let app = app.clone();
        tokio::spawn(async move {
            let mut lines = tokio::io::BufReader::new(err).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                app.emit("console-line", format!("[serial] warning: {line}")).ok();
            }
        });
    }

    tokio::select! {
        Ok(()) = ready_rx => {
            app.state::<std::sync::Mutex<AppState>>().lock().unwrap().stream_holders.push(pgid);
            // Collect the exit status later so the script does not linger as a zombie.
            tokio::spawn(async move { child.wait().await.ok(); });
            Ok(())
        }
        status = child.wait() => Err(format!(
            "Serial setup for {device} failed ({}); see the Console for the reason.",
            status.map(|s| s.to_string()).unwrap_or_else(|e| e.to_string())
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_every_pea_stream_scheme() {
        assert_eq!(classify("ALIC00AUS0").unwrap(), StreamKind::Ntrip);
        assert_eq!(classify("https://ntrip.data.gnss.ga.gov.au/SSRA00BKG0").unwrap(), StreamKind::Ntrip);
        assert_eq!(classify("ntrip://host/MOUNT").unwrap(), StreamKind::Ntrip);
        assert_eq!(classify("sisnet://host:7777").unwrap(), StreamKind::Sisnet);
        assert_eq!(
            classify("serial:///dev/cu.usbserial-2110").unwrap(),
            StreamKind::Serial { device: "/dev/cu.usbserial-2110".into() }
        );
        assert!(classify("file:///tmp/obs.rtcm").is_err());
        assert!(classify("tcp://host:2101").is_err());
    }

    #[test]
    fn reads_streams_from_both_rtcm_lists() {
        let p = std::env::temp_dir().join(format!("ginan-streams-{}.yaml", std::process::id()));
        std::fs::write(
            &p,
            "inputs:\n  gnss_observations:\n    rtcm_inputs: [serial:///dev/ttyUSB0]\n  \
             satellite_data:\n    rtcm_inputs:\n      rtcm_inputs: [SSRA00BKG0]\n",
        )
        .unwrap();
        let uris = stream_uris(p.to_str().unwrap()).unwrap();
        std::fs::remove_file(&p).ok();
        assert_eq!(uris, ["serial:///dev/ttyUSB0", "SSRA00BKG0"]);
    }
}
