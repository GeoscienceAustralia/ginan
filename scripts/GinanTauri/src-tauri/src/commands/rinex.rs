use flate2::read::GzDecoder;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

/// Per-constellation observation codes extracted from RINEX header
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ConstellationObsCodes {
    pub gps: Vec<String>,
    pub gal: Vec<String>,
    pub glo: Vec<String>,
    pub bds: Vec<String>,
    pub qzs: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RinexData {
    /// RINEX format version (e.g. 2.11, 3.04, 4.00). Parsed from the
    /// "RINEX VERSION / TYPE" header line. None if not present/parseable.
    pub rinex_version: Option<f64>,
    pub marker_name: Option<String>,
    pub receiver_type: Option<String>,
    pub antenna_type: Option<String>,
    /// H, E, N offsets in metres
    pub antenna_offset: [f64; 3],
    /// X, Y, Z approximate position
    pub approx_position: Option<[f64; 3]>,
    /// Epoch string in PEA format: "YYYY-MM-DD HH:MM:SS", always in GPS time
    /// (PEA reads `epoch_control` as GPST). Files kept in another time system
    /// are converted on the way out; see `epoch_to_gps`.
    pub start_epoch: Option<String>,
    pub end_epoch: Option<String>,
    pub epoch_interval: Option<f64>,
    /// Time system the file's epochs were written in ("GPS", "GLO", "GAL",
    /// "BDT", ...), before the conversion above. None when the header omits it.
    pub time_system: Option<String>,
    /// Sorted list of constellation names found: GPS, GAL, GLO, BDS, QZS
    pub constellations: Vec<String>,
    /// Observation codes available per constellation
    pub observation_codes: ConstellationObsCodes,
}

/// Human-readable byte count for console messages.
pub(crate) fn human_bytes(n: u64) -> String {
    const KB: f64 = 1024.0;
    let f = n as f64;
    if f < KB {
        format!("{n} B")
    } else if f < KB * KB {
        format!("{:.1} kB", f / KB)
    } else {
        format!("{:.1} MB", f / (KB * KB))
    }
}

/// Normalise a user-selected observation file into a plain, uncompressed RINEX
/// file on disk, returning the path to use from here on.
///
/// Handles `.gz`, Hatanaka-compressed `.crx` / `.??d`, and the common
/// `.crx.gz` combination. A file that is already plain RINEX is returned
/// unchanged, so this is safe to call for every selection.
///
/// The result is written next to the source with a `.rnx` extension because
/// PEA needs a real path — decoding in memory would not be enough. An existing
/// output newer than the source is reused rather than rewritten.
///
/// Progress is reported on the Console channel (`console-line`) plus
/// `prepare-progress` for the status bar, so a slow conversion is visible
/// rather than looking like a hang. GinanUI's rinex_extractor logs the same
/// detail to its Console pane.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn prepare_rinex_input(app: tauri::AppHandle, path: String) -> Result<String, String> {
    use tauri::Emitter;
    // Decompression and Hatanaka conversion of a daily file take seconds of
    // CPU and file IO; keep them off the async runtime's worker threads.
    tokio::task::spawn_blocking(move || {
        prepare_rinex_file(
            &path,
            &mut |msg| {
                app.emit("console-line", msg).ok();
            },
            &mut |msg| {
                app.emit("prepare-progress", msg).ok();
            },
        )
    })
    .await
    .map_err(|e| format!("RINEX preparation task failed: {e}"))?
}

/// Implementation behind [`prepare_rinex_input`], kept free of Tauri types so
/// it can be unit-tested without an `AppHandle`. `log` writes console lines,
/// `status` drives the status bar (empty string clears it).
pub(crate) fn prepare_rinex_file(
    path: &str,
    log: &mut dyn FnMut(String),
    status: &mut dyn FnMut(String),
) -> Result<String, String> {
    let src = Path::new(path);
    if !src.is_file() {
        return Err(format!("File not found: {path}"));
    }

    let name = src
        .file_name()
        .and_then(|n| n.to_str())
        .ok_or_else(|| "Unreadable file name".to_string())?;
    let lower = name.to_lowercase();

    // Strip a trailing compression suffix to see the real file name.
    let gzipped = lower.ends_with(".gz");
    let stem = if gzipped { &lower[..lower.len() - 3] } else { &lower[..] };

    // Hatanaka files are either *.crx or the RINEX 2 short name *.??d
    // (e.g. alic0010.10d), where the final 'd' replaces the 'o' of an obs file.
    let is_hatanaka = stem.ends_with(".crx")
        || stem
            .rsplit('.')
            .next()
            .is_some_and(|e| e.len() == 3 && e.ends_with('d') && e[..2].chars().all(|c| c.is_ascii_digit()));

    if !gzipped && !is_hatanaka {
        return Ok(path.to_string()); // already plain RINEX
    }

    let src_bytes = src.metadata().map(|m| m.len()).unwrap_or(0);
    log(format!("Preparing {name} ({})...", human_bytes(src_bytes)));

    // Read, decompressing the gzip layer if present.
    let file = File::open(src).map_err(|e| format!("Cannot open file: {e}"))?;
    let mut text = String::new();
    if gzipped {
        use std::io::Read;
        status(format!("Decompressing {name}..."));
        log("  Decompressing gzip...".to_string());
        GzDecoder::new(file)
            .read_to_string(&mut text)
            .map_err(|e| format!("Gzip decode failed: {e}"))?;
        log(format!(
            "  Decompressed to {} of RINEX text",
            human_bytes(text.len() as u64)
        ));
    } else {
        use std::io::Read;
        BufReader::new(file)
            .read_to_string(&mut text)
            .map_err(|e| format!("Cannot read file: {e}"))?;
    }

    // Only decode Hatanaka when the content really is CRINEX — a plain .rnx.gz
    // just needs the gzip layer removed.
    let looks_crinex = text
        .lines()
        .next()
        .is_some_and(|l| l.contains("CRINEX VERS"));
    if looks_crinex {
        log("  Converting Hatanaka CRINEX → RINEX...".to_string());
        // Throttle to whole percent: the callback fires once per epoch, which
        // is thousands of times for a daily file.
        let mut last_pct = usize::MAX;
        text = crate::commands::crinex::crx_to_rnx_with_progress(&text, &mut |done, total| {
            if total == 0 {
                return;
            }
            let pct = done * 100 / total;
            if pct != last_pct {
                last_pct = pct;
                status(format!("Converting {name} · {pct}%"));
            }
        })
        .map_err(|e| format!("Hatanaka (CRX to RNX) decompression failed: {e}"))?;
        log(format!(
            "  Converted to {} of RINEX text",
            human_bytes(text.len() as u64)
        ));
    } else if !gzipped {
        return Ok(path.to_string()); // nothing to do
    }

    // Write alongside the source, preserving the base name.
    let base = name
        .strip_suffix(".gz")
        .or_else(|| name.strip_suffix(".GZ"))
        .unwrap_or(name);
    let base_no_ext = match base.rfind('.') {
        Some(i) => &base[..i],
        None => base,
    };
    let out_path = src.with_file_name(format!("{base_no_ext}.rnx"));

    // Reuse an existing conversion unless the source is newer.
    if let (Ok(out_meta), Ok(src_meta)) = (out_path.metadata(), src.metadata()) {
        if let (Ok(out_t), Ok(src_t)) = (out_meta.modified(), src_meta.modified()) {
            if out_t >= src_t {
                log(format!(
                    "  Reusing existing {}",
                    out_path.file_name().and_then(|n| n.to_str()).unwrap_or("output")
                ));
                return out_path
                    .to_str()
                    .map(|s| s.to_string())
                    .ok_or_else(|| "Unprintable output path".to_string());
            }
        }
    }

    let out_len = text.len() as u64;
    std::fs::write(&out_path, text)
        .map_err(|e| format!("Could not write {}: {e}", out_path.display()))?;
    log(format!(
        "  Wrote {} ({})",
        out_path.file_name().and_then(|n| n.to_str()).unwrap_or("output"),
        human_bytes(out_len)
    ));
    status(String::new());

    out_path
        .to_str()
        .map(|s| s.to_string())
        .ok_or_else(|| "Unprintable output path".to_string())
}
