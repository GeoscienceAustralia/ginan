//! Reads PEA's `.pos` output into plottable series for the Visualization tab.
//!
//! The Python UI shells out to `scripts/plot_pos.py`, which writes one Plotly
//! HTML file per figure that the user then has to open by hand. Here the parse
//! and the statistics happen in Rust and the frontend draws the figures itself,
//! so a finished run has its plots on screen with no export step and no Python
//! toolchain to install.
//!
//! Column layout is fixed by `src/cpp/common/pos.cpp` (PBO Position Time Series
//! 2.0.0), 24 whitespace-separated fields per epoch:
//!
//! ```text
//! 0  time (ISO)   1  decimal year   2..4  X Y Z      5..7   Sx Sy Sz
//! 8..10 Rxy Rxz Ryz              11..13 NLat Elong Height
//! 14..16 dN dE dU                17..19 Sn Se Su
//! 20..22 Rne Rnu Reu             23  soln
//! ```
//!
//! An SBAS run's `.SPP` solution is listed and plotted the same way; its
//! columns are read by `visualisation_spp`.

use super::visualisation_helpers::*;
use super::visualisation_spp::{
    first_spp_reference, parse_spp_line, SolutionFormat, SppReference, SPP_MIN_FIELDS,
};

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::time::Duration;

use serde::Serialize;
use tauri::Emitter;
use tokio::io::{AsyncBufReadExt, AsyncSeekExt};

use crate::state::PosTailGen;

/// Field count a data line must reach before it is considered an epoch.
pub(crate) const POS_MIN_FIELDS: usize = 24;

/// Points handed to the frontend when the caller doesn't ask for a specific
/// budget. A day of 1 Hz kinematic output is ~86k epochs; drawing every one of
/// them costs far more than the canvas can resolve, so the series is decimated
/// to this many for display while the statistics stay computed over all of them.
pub(crate) const DEFAULT_MAX_POINTS: usize = 20_000;

/// One `.pos` or `.SPP` file found in the output directory.
#[derive(Serialize, Debug)]
pub struct PosFileInfo {
    pub path: String,
    pub name: String,
    pub format: SolutionFormat,
    /// PEA writes the backward-smoothed solution to a `*_smoothed.pos`
    /// companion file; the frontend labels the two apart.
    pub smoothed: bool,
    pub size: u64,
    /// Modification time in epoch milliseconds, or null if unavailable.
    pub modified_ms: Option<i64>,
    /// Whether the file holds at least one epoch. PEA creates the smoothed
    /// companion (and every file of a run that never got going) with nothing
    /// or only the header in it; offering those in a picker just leads to a
    /// "nothing to plot" error in every panel.
    pub has_epochs: bool,
}

/// How far into a file [`has_epoch_lines`] looks: the fixed header is ~2 KB,
/// so the first epoch, if there is one, is well inside this.
const EPOCH_SNIFF_BYTES: u64 = 256 * 1024;

/// Whether a solution file has at least one readable epoch line near its start.
fn has_epoch_lines(path: &Path, format: SolutionFormat) -> bool {
    use std::io::Read;
    let Ok(file) = File::open(path) else {
        return false;
    };
    let reader = BufReader::new(file.take(EPOCH_SNIFF_BYTES));
    let mut reference = None;
    reader
        .lines()
        .map_while(Result::ok)
        .any(|line| parse_line(&line, format, &mut reference).is_some())
}

/// Weighted statistics for one component, matching `plot_pos.py`'s
/// `compute_statistics`: inverse-variance weights, weighted mean and standard
/// deviation, unweighted RMS about zero.
#[derive(Serialize, Default, Clone, Copy, Debug)]
pub struct ComponentStats {
    pub weighted_mean: f64,
    pub std_dev: f64,
    pub rms: f64,
    pub min: f64,
    pub max: f64,
    /// Mean of the reported sigmas — the formal precision of the solution.
    pub mean_sigma: f64,
}

#[derive(Serialize, Default, Debug)]
pub struct PosStats {
    pub dn: ComponentStats,
    pub de: ComponentStats,
    pub du: ComponentStats,
}

/// A parsed `.pos` file, decimated for display.
///
/// Components are metres from the a priori NEU reference position; sigmas are
/// the 1σ formal errors of each. Times are epoch milliseconds (GPS time as
/// written, not converted) so the frontend can scale them directly.
#[derive(Serialize, Debug)]
pub struct PosSeries {
    pub path: String,
    pub name: String,
    pub format: SolutionFormat,
    /// 4-character station ID from the header, when present.
    pub station: Option<String>,
    pub smoothed: bool,
    /// Epochs in the file.
    pub total_epochs: usize,
    /// Epochs in the arrays below — smaller than `total_epochs` when decimated.
    pub returned_epochs: usize,
    /// True when `t` and friends are a stride-sample rather than every epoch.
    pub decimated: bool,
    /// Lines that looked like data but could not be parsed.
    pub skipped_lines: usize,
    pub t: Vec<f64>,
    pub dn: Vec<f64>,
    pub de: Vec<f64>,
    pub du: Vec<f64>,
    pub sn: Vec<f64>,
    pub se: Vec<f64>,
    pub su: Vec<f64>,
    /// Absolute WGS84 latitude/longitude, decimal degrees, from the NLat/Elong
    /// columns — the geodetic position the dN/dE/dU offsets are relative to.
    pub lat: Vec<f64>,
    pub lon: Vec<f64>,
    /// Horizontal / vertical protection levels per returned epoch, metres.
    /// Filled for `.SPP`, empty for `.pos`, which has none.
    pub hpl: Vec<f64>,
    pub vpl: Vec<f64>,
    /// Statistics over every epoch in the file, not just the returned ones.
    pub stats: PosStats,
}

/// Lists the `.pos` and `.SPP` files directly under `output_dir`.
///
/// Only the top level is scanned: PEA writes its solution files there, and the
/// `visual/` and archive subdirectories hold generated artefacts and previous
/// runs which would otherwise show up as duplicate entries.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn list_pos_files(output_dir: String) -> Result<Vec<PosFileInfo>, String> {
    let dir = Path::new(&output_dir);
    if !dir.is_dir() {
        return Err(format!("Output directory does not exist: {}", output_dir));
    }

    let entries = std::fs::read_dir(dir)
        .map_err(|e| format!("Cannot read {}: {}", output_dir, e))?;

    let mut files = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let Some(format) = SolutionFormat::of(&path) else {
            continue;
        };

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        let meta = entry.metadata().ok();

        files.push(PosFileInfo {
            smoothed: name.to_lowercase().contains("_smoothed"),
            modified_ms: meta
                .as_ref()
                .and_then(|m| m.modified().ok())
                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                .map(|d| d.as_millis() as i64),
            size: meta.map(|m| m.len()).unwrap_or(0),
            has_epochs: has_epoch_lines(&path, format),
            format,
            path: path.to_string_lossy().into_owned(),
            name,
        });
    }

    // Forward solutions before their smoothed companions, then by name, so the
    // selector order is stable across runs regardless of directory order.
    files.sort_by(|a, b| a.smoothed.cmp(&b.smoothed).then_with(|| a.name.cmp(&b.name)));
    Ok(files)
}

/// Parses one `.pos` or `.SPP` file into display-ready series.
///
/// `max_points` caps how many epochs come back; pass null for the default.
/// Statistics are always computed over the whole file, so decimation changes
/// the drawn line's resolution but never the reported numbers.
// `async`: parsing a large file must not run on (and freeze) the main thread.
#[tauri::command(async)]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn load_pos_series(path: String, max_points: Option<usize>) -> Result<PosSeries, String> {
    let budget = max_points.unwrap_or(DEFAULT_MAX_POINTS).max(2);
    let p = Path::new(&path);
    let format = SolutionFormat::of(p).unwrap_or(SolutionFormat::Pos);
    let file = File::open(p).map_err(|e| format!("Cannot open {}: {}", path, e))?;
    let reader = BufReader::new(file);

    let mut station: Option<String> = None;
    let mut rows: Vec<Epoch> = Vec::new();
    let mut skipped_lines = 0usize;
    let mut spp_reference = None;

    for line in reader.lines() {
        let line = line.map_err(|e| format!("Error reading {}: {}", path, e))?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        // The header is fixed-text; the only field worth keeping is the ID.
        if station.is_none() {
            if let Some(rest) = trimmed.strip_prefix("4-character ID:") {
                let id = rest.trim();
                if !id.is_empty() {
                    station = Some(id.to_string());
                }
                continue;
            }
        }

        // The column-legend line starts with '*'; everything else in the header
        // fails the timestamp parse below and is skipped without comment.
        if trimmed.starts_with('*') {
            continue;
        }

        let fields: Vec<&str> = trimmed.split_whitespace().collect();
        let min_fields = match format {
            SolutionFormat::Pos => POS_MIN_FIELDS,
            SolutionFormat::Spp => SPP_MIN_FIELDS,
        };
        if fields.len() < min_fields || parse_pos_time(fields[0]).is_none() {
            continue; // header prose, not an epoch
        }

        match parse_line(trimmed, format, &mut spp_reference) {
            Some(epoch) => rows.push(epoch),
            // Timestamp parsed, so this was meant to be data — a truncated or
            // corrupt line. Counted and reported rather than silently dropped.
            None => skipped_lines += 1,
        }
    }

    if rows.is_empty() {
        return Err(format!(
            "No position epochs found in {}. The file may be empty or still being written.",
            p.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or(path.clone()),
        ));
    }

    // PEA appends epoch by epoch, so the file is already in time order; sorting
    // guards the case of a file assembled from more than one run.
    rows.sort_by(|a, b| a.t.partial_cmp(&b.t).unwrap_or(std::cmp::Ordering::Equal));

    let stats = PosStats {
        dn: component_stats(rows.iter().map(|r| (r.dn, r.sn))),
        de: component_stats(rows.iter().map(|r| (r.de, r.se))),
        du: component_stats(rows.iter().map(|r| (r.du, r.su))),
    };

    let total_epochs = rows.len();
    let stride = total_epochs.div_ceil(budget).max(1);
    let name = p
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.clone());

    let mut series = PosSeries {
        format,
        smoothed: name.to_lowercase().contains("_smoothed"),
        station,
        total_epochs,
        returned_epochs: 0,
        decimated: stride > 1,
        skipped_lines,
        t: Vec::new(),
        dn: Vec::new(),
        de: Vec::new(),
        du: Vec::new(),
        sn: Vec::new(),
        se: Vec::new(),
        su: Vec::new(),
        lat: Vec::new(),
        lon: Vec::new(),
        hpl: Vec::new(),
        vpl: Vec::new(),
        stats,
        name,
        path,
    };

    for (i, row) in rows.iter().enumerate() {
        // Keep the final epoch whatever the stride lands on, so the drawn line
        // ends where the solution ends.
        if i % stride != 0 && i != total_epochs - 1 {
            continue;
        }
        series.t.push(row.t);
        series.dn.push(row.dn);
        series.de.push(row.de);
        series.du.push(row.du);
        series.sn.push(row.sn);
        series.se.push(row.se);
        series.su.push(row.su);
        series.lat.push(row.lat);
        series.lon.push(row.lon);
        if let Some((h, v)) = row.levels {
            series.hpl.push(h);
            series.vpl.push(v);
        }
    }
    series.returned_epochs = series.t.len();

    Ok(series)
}

// ─── live tail (Visualization tab "live stream" mode) ────────────────────────

/// One epoch pushed to the frontend as it's written, so the Visualization tab
/// can append a point instead of re-reading and re-decimating the whole file
/// on every tick.
#[derive(Serialize, Clone, Debug)]
pub struct PosEpochEvent {
    pub path: String,
    pub t: f64,
    pub dn: f64,
    pub de: f64,
    pub du: f64,
    pub sn: f64,
    pub se: f64,
    pub su: f64,
    pub lat: f64,
    pub lon: f64,
    /// `.SPP` only, like `PosSeries::hpl` / `vpl`.
    pub hpl: Option<f64>,
    pub vpl: Option<f64>,
}

/// Parses one line of a file in `format`; `spp_reference` is the state an
/// `.SPP` file without an a priori position measures its offsets from.
pub(crate) fn parse_line(
    line: &str,
    format: SolutionFormat,
    spp_reference: &mut Option<SppReference>,
) -> Option<Epoch> {
    match format {
        SolutionFormat::Pos => try_parse_epoch_line(line),
        SolutionFormat::Spp => parse_spp_line(line, spp_reference),
    }
}

/// Try to parse one raw `.pos` line as a data epoch. Returns `None` for header
/// lines, blank lines, and lines that don't reach the timestamp/field checks —
/// the same tolerance `load_pos_series` applies, just one line at a time.
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub(crate) fn try_parse_epoch_line(line: &str) -> Option<Epoch> {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('*') {
        return None;
    }
    let fields: Vec<&str> = trimmed.split_whitespace().collect();
    if fields.len() < POS_MIN_FIELDS {
        return None;
    }
    let time = parse_pos_time(fields[0])?;
    parse_epoch(time, &fields)
}

/// Tail `path` for newly appended epochs and emit each as a `pos-epoch` event.
///
/// Mirrors the `ntrip_stats.jsonl` tailer in `realtime.rs`: seek to the
/// current end of file, then poll for new lines every 250ms. Starting a new
/// tail bumps [`PosTailGen`], so only ever one tail loop is actively emitting
/// — the previous one notices the generation mismatch on its next tick and
/// exits, which also covers "switch to a different receiver's .pos file"
/// without an explicit stop call.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn start_pos_tail(
    path: String,
    app: tauri::AppHandle,
    gen: tauri::State<'_, PosTailGen>,
) -> Result<(), String> {
    let my_gen = gen.0.fetch_add(1, Ordering::SeqCst) + 1;
    let gen_flag = gen.0.clone();
    let pos_path = PathBuf::from(&path);
    let format = SolutionFormat::of(&pos_path).unwrap_or(SolutionFormat::Pos);

    tokio::spawn(async move {
        let mut opened: Option<tokio::io::BufReader<tokio::fs::File>> = None;
        let mut spp_reference = None;

        loop {
            if gen_flag.load(Ordering::SeqCst) != my_gen {
                break;
            }

            if opened.is_none() {
                match tokio::fs::File::open(&pos_path).await {
                    Ok(mut f) => {
                        // Offsets must match what `load_pos_series` drew.
                        if format == SolutionFormat::Spp {
                            spp_reference = first_spp_reference(&pos_path);
                        }
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
                    if let Some(epoch) = parse_line(&line, format, &mut spp_reference) {
                        app.emit(
                            "pos-epoch",
                            PosEpochEvent {
                                path: path.clone(),
                                t: epoch.t,
                                dn: epoch.dn,
                                de: epoch.de,
                                du: epoch.du,
                                sn: epoch.sn,
                                se: epoch.se,
                                su: epoch.su,
                                lat: epoch.lat,
                                lon: epoch.lon,
                                hpl: epoch.levels.map(|l| l.0),
                                vpl: epoch.levels.map(|l| l.1),
                            },
                        )
                        .ok();
                    }
                }
                Err(_) => {
                    opened = None;
                    tokio::time::sleep(Duration::from_millis(500)).await;
                }
            }
        }
    });

    Ok(())
}

/// Stop the active `.pos` tail, if any. Bumps [`PosTailGen`] so the running
/// loop exits on its next tick.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn stop_pos_tail(gen: tauri::State<'_, PosTailGen>) {
    gen.0.fetch_add(1, Ordering::SeqCst);
}
