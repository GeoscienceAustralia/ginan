//! Reads PEA's `.TRACE` output into plottable residual series.
//!
//! PEA writes a `.TRACE` next to every `.POS`. The `.POS` only carries the final solution; the `.TRACE`
//! carries what went into it — one prefit/postfit residual per measurement per
//! epoch, tagged with the satellite, receiver and signal it came from. That is
//! the difference between "the answer moved 3 cm" and "G07's L2W is dragging
//! it", so it is worth reading.
//!
//! The residual rows are the `%`-prefixed lines inside the `+RESIDUALS` blocks
//! written by `outputResiduals` in `src/cpp/common/algebra.cpp`. Each is
//! tab-separated, and the four key columns come from `KFKey::operator string`:
//!
//! ```text
//! %  It  Time  Type  Sat  Str  Code  Prefit Res  Postfit Res  Meas Sigma
//!                                                [Prefit Ratio  Postfit Ratio]  [Comments]
//! ```
//!
//! The two ratio columns only appear at `outputs.trace.level` 5 and above, and
//! the comment column at level 2 and above, so a row is 9, 10 or 12 fields
//! wide after the leading `%`. `scripts/plot_trace_res.py` — the Python
//! plotter for the same files — is the reference for how those variants are told
//! apart, and for the rule that only the highest iteration of a repeated
//! measurement counts: PEA re-emits a row per filter iteration, and the last
//! one is the one that made it into the solution.

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use chrono::NaiveDateTime;
use serde::Serialize;

/// Fields a residual row must reach, after the leading `%`, to be an epoch:
/// iteration, time, and the four key columns, then prefit, postfit and sigma.
const RESIDUAL_MIN_FIELDS: usize = 9;

/// Total samples handed to the frontend across every returned series when the
/// caller doesn't ask for a budget. A day of 30 s multi-GNSS output is a few
/// hundred thousand residuals; drawing every one costs far more than the
/// canvas can resolve, so the series are decimated to this many in total while
/// the statistics stay computed over all of them.
const DEFAULT_MAX_POINTS: usize = 20_000;

/// One `.TRACE` file found in the output directory.
#[derive(Serialize, Debug)]
pub struct TraceFileInfo {
    pub path: String,
    pub name: String,
    pub size: u64,
    /// Modification time in epoch milliseconds, or null if unavailable.
    pub modified_ms: Option<i64>,
    /// Whether the file holds any residual rows. A smoothed run's `.TRACE`,
    /// or one from a run with residual output off, is only PEA's config
    /// header; offering it in a file picker just leads to an error.
    pub has_residuals: bool,
}

/// How far into a file [`has_residual_rows`] looks. PEA writes residuals from
/// the first epoch, so a file with none in its first few megabytes has none.
const RESIDUAL_SNIFF_BYTES: u64 = 8 * 1024 * 1024;

/// Whether a `.TRACE` has at least one residual row, looking only at its
/// start so listing a directory of large files stays quick.
fn has_residual_rows(path: &Path) -> bool {
    let Ok(file) = File::open(path) else {
        return false;
    };
    let mut reader = BufReader::new(file.take(RESIDUAL_SNIFF_BYTES));
    let mut line = Vec::new();
    loop {
        line.clear();
        match reader.read_until(b'\n', &mut line) {
            Ok(0) | Err(_) => return false,
            // A complete row, not one PEA is still in the middle of writing.
            Ok(_) if line.starts_with(b"%") && line.ends_with(b"\n") => return true,
            Ok(_) => {}
        }
    }
}

/// What kind of measurement a residual row describes.
///
/// PEA's key type is an open enum (`CODE_MEAS`, `PHAS_MEAS`, and others that
/// may appear in a `RESIDUALS` block), so the raw token is carried through as
/// well — the frontend groups on this, but nothing is dropped for being
/// unrecognised.
#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum MeasKind {
    Code,
    Phase,
    Other,
}

impl MeasKind {
    fn from_type(kind: &str) -> MeasKind {
        // Exact types, not prefixes: `CODE_BIAS` is a bias constraint, not a
        // code measurement, and filing it as one would put it in the code panel.
        let upper = kind.to_ascii_uppercase();
        if upper == "CODE_MEAS" {
            MeasKind::Code
        } else if upper == "PHAS_MEAS" {
            MeasKind::Phase
        } else {
            MeasKind::Other
        }
    }
}

/// Spread of one residual column over every sample in a series.
///
/// Unweighted, unlike [`crate::commands::visualisation::ComponentStats`]: a
/// residual's sigma is the measurement noise the filter assumed, not the
/// precision of the residual itself, so weighting by it would report how much
/// PEA trusted a measurement rather than how far off it was.
#[derive(Serialize, Default, Clone, Copy, Debug)]
pub struct ResidualStats {
    pub mean: f64,
    pub std_dev: f64,
    pub rms: f64,
    pub min: f64,
    pub max: f64,
    /// Mean of the measurement sigmas the filter used for these rows.
    pub mean_sigma: f64,
}

/// One plottable line: the residuals of a single signal from a single
/// satellite at a single receiver, in time order.
#[derive(Serialize, Debug)]
pub struct ResidualSeries {
    /// Stable identity for the frontend to key and select on, e.g.
    /// `"CODE_MEAS|G07|ALIC|L1C"`.
    pub key: String,
    pub sat: String,
    /// `"GPS"`, `"GLO"`, … from the satellite id's leading character; `"Other"`
    /// for an id this build of PEA writes but `satSys.hpp` doesn't map.
    pub constellation: String,
    pub receiver: String,
    pub signal: String,
    /// The raw key type token, e.g. `"CODE_MEAS"`.
    pub meas_type: String,
    pub kind: MeasKind,
    /// Samples in this series in the file, before decimation.
    pub total_samples: usize,
    /// Epoch milliseconds, as written (GPS time, never zone-converted).
    pub t: Vec<f64>,
    pub prefit: Vec<f64>,
    pub postfit: Vec<f64>,
    pub sigma: Vec<f64>,
    /// Over every sample in the file, not just the returned ones.
    pub prefit_stats: ResidualStats,
    pub postfit_stats: ResidualStats,
}

/// One satellite seen in the file, with what it contributed.
#[derive(Serialize, Debug, Clone)]
pub struct SatelliteSummary {
    pub sat: String,
    pub constellation: String,
    /// Signals this satellite was measured on, sorted.
    pub signals: Vec<String>,
    /// Receivers that observed it, sorted.
    pub receivers: Vec<String>,
    /// Residual rows contributed, after the per-iteration de-duplication.
    pub samples: usize,
    /// First and last epoch it appears at, in epoch milliseconds.
    pub first_ms: f64,
    pub last_ms: f64,
    /// RMS of the postfit residuals over every sample — how well the solution
    /// ended up fitting this satellite.
    pub postfit_rms: f64,
}

/// Everything in the file, independent of which series were asked for.
///
/// Always computed over the whole file, so the counts and the satellite list
/// describe the run and not the current filter.
#[derive(Serialize, Debug, Default)]
pub struct TraceIndex {
    pub receivers: Vec<String>,
    pub signals: Vec<String>,
    pub satellites: Vec<SatelliteSummary>,
    /// Distinct epochs carrying at least one residual row.
    pub epochs: usize,
    /// Residual rows kept, after de-duplicating repeated filter iterations.
    pub total_samples: usize,
    pub t_start: f64,
    pub t_end: f64,
}

/// A parsed `.TRACE` file: the index of everything in it, plus the arrays for
/// the requested subset, decimated for display.
#[derive(Serialize, Debug)]
pub struct TraceSeries {
    pub path: String,
    pub name: String,
    pub index: TraceIndex,
    pub series: Vec<ResidualSeries>,
    /// Samples in the arrays above — smaller than the selection's own total
    /// when decimated.
    pub returned_samples: usize,
    /// True when the arrays are a stride-sample rather than every row.
    pub decimated: bool,
    /// Lines that started with `%` but could not be parsed. A partial final
    /// line is not counted — see [`read_residual_rows`].
    pub skipped_lines: usize,
}

/// Lists the `.TRACE` files directly under `output_dir`.
///
/// Only the top level is scanned, for the same reason as `list_pos_files`:
/// PEA writes its output there, and the `visual/` and archive subdirectories
/// hold generated artefacts and previous runs.
///
/// An output directory with no `.TRACE` files in it is an empty list, not an
/// error — trace output is a config toggle (`outputs.trace.output_network`)
/// and having it off is a normal way to run.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn list_trace_files(output_dir: String) -> Result<Vec<TraceFileInfo>, String> {
    let dir = Path::new(&output_dir);
    if !dir.is_dir() {
        return Err(format!("Output directory does not exist: {}", output_dir));
    }

    let entries =
        std::fs::read_dir(dir).map_err(|e| format!("Cannot read {}: {}", output_dir, e))?;

    let mut files = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let is_trace = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.eq_ignore_ascii_case("trace"))
            .unwrap_or(false);
        if !is_trace {
            continue;
        }

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        let meta = entry.metadata().ok();

        files.push(TraceFileInfo {
            modified_ms: meta
                .as_ref()
                .and_then(|m| m.modified().ok())
                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                .map(|d| d.as_millis() as i64),
            size: meta.map(|m| m.len()).unwrap_or(0),
            has_residuals: has_residual_rows(&path),
            path: path.to_string_lossy().into_owned(),
            name,
        });
    }

    // Newest first: a run just finished is the one the user means, and the
    // filename template ends in the epoch hour so name order is close to time
    // order anyway.
    files.sort_by(|a, b| b.modified_ms.cmp(&a.modified_ms).then_with(|| a.name.cmp(&b.name)));
    Ok(files)
}

/// Parses one `.TRACE` file into display-ready residual series.
///
/// `sats` and `receiver` narrow which series come back in `series`; `index` is
/// always built over the whole file, so the caller can populate a selector
/// from the same call that draws the first figure. `max_points` caps the total
/// number of samples across every returned series — pass null for the default.
/// Statistics are always computed over the whole file, so decimation changes
/// the drawn line's resolution but never the reported numbers.
// `async`: parsing a large file must not run on (and freeze) the main thread.
#[tauri::command(async)]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn load_trace_series(
    path: String,
    sats: Option<Vec<String>>,
    receiver: Option<String>,
    max_points: Option<usize>,
) -> Result<TraceSeries, String> {
    let budget = max_points.unwrap_or(DEFAULT_MAX_POINTS).max(2);
    let p = Path::new(&path);
    let (grouped, skipped_lines) = read_residual_rows(p).map_err(|e| e.to_string())?;
    let grouped = &*grouped;

    if grouped.is_empty() {
        return Err(format!(
            "No residual records found in {}. The file may be empty, still being written, or \
             produced with residual output turned off (outputs.trace.output_residuals).",
            file_name(p, &path),
        ));
    }

    let index = build_index(grouped);

    // A `sats` list that is present but empty means "the index only" — the
    // satellite panel asks for exactly that, and it should not be confused
    // with "no filter given".
    let wanted: Option<Vec<String>> = sats.map(|s| s.iter().map(|v| v.to_uppercase()).collect());
    let selected: Vec<&(SeriesKey, Samples)> = grouped
        .iter()
        .filter(|(key, _)| match &wanted {
            Some(list) => list.iter().any(|s| s == &key.sat),
            None => true,
        })
        .filter(|(key, _)| match &receiver {
            Some(r) => r.eq_ignore_ascii_case(&key.receiver),
            None => true,
        })
        .collect();

    // One stride across the whole selection rather than per series, so the
    // payload is bounded by `budget` however many satellites were asked for
    // and every line keeps the same time resolution as its neighbours.
    let selected_samples: usize = selected.iter().map(|(_, s)| s.len()).sum();
    let stride = selected_samples.div_ceil(budget).max(1);

    let mut series: Vec<ResidualSeries> = selected
        .into_iter()
        .map(|(key, samples)| decimate(key, samples, stride))
        .collect();

    // Constellation, then satellite, then receiver and signal: the order the
    // legend and the selector both read in.
    series.sort_by(|a, b| {
        a.constellation
            .cmp(&b.constellation)
            .then_with(|| a.sat.cmp(&b.sat))
            .then_with(|| a.receiver.cmp(&b.receiver))
            .then_with(|| a.kind.cmp(&b.kind))
            .then_with(|| a.signal.cmp(&b.signal))
    });

    let returned_samples = series.iter().map(|s| s.t.len()).sum();

    Ok(TraceSeries {
        name: file_name(p, &path),
        path,
        index,
        series,
        returned_samples,
        decimated: stride > 1,
        skipped_lines,
    })
}

/// The satellites one constellation contributed, in satellite-id order.
#[derive(Serialize, Debug)]
pub struct ConstellationStatus {
    /// `"GPS"`, `"GLO"`, … as [`constellation_of`] names them.
    pub constellation: String,
    pub satellites: Vec<SatelliteSummary>,
}

/// Which satellites a run is using, grouped by constellation.
///
/// Carries its own "there is nothing to show" state instead of failing.
/// Trace output is a config toggle and a live run has no `.TRACE` at all for
/// its first few seconds, so a panel built on this must be able to say why it
/// is empty without the caller treating every quiet moment as an error.
#[derive(Serialize, Debug, Default)]
pub struct SatelliteStatus {
    /// The `.TRACE` this was read from, or null when there wasn't one.
    pub path: Option<String>,
    pub name: Option<String>,
    /// False when nothing could be read — see `message` for which reason.
    pub available: bool,
    /// Why `available` is false, in words the panel can show as-is.
    pub message: Option<String>,
    pub receivers: Vec<String>,
    pub constellations: Vec<ConstellationStatus>,
    /// Distinct epochs carrying residuals.
    pub epochs: usize,
    /// Total satellites across every constellation.
    pub satellite_count: usize,
    pub t_start: f64,
    /// The most recent epoch in the file. A satellite whose `last_ms` is this
    /// value was still contributing when the file was last written, which is
    /// how the panel tells "tracking" from "was tracked earlier".
    pub t_end: f64,
}

impl SatelliteStatus {
    fn unavailable(message: impl Into<String>) -> SatelliteStatus {
        SatelliteStatus { message: Some(message.into()), ..Default::default() }
    }
}

/// Reads which satellites are contributing to a run, grouped by constellation.
///
/// The satellite list can't come from the NTRIP stats file — that only carries
/// byte counts and connect/disconnect events — so it comes from the
/// residual rows of the `.TRACE`: a satellite with a residual at an epoch is a
/// satellite the filter used at that epoch.
///
/// `path` picks a specific file; without it the newest `.TRACE` in
/// `output_dir` is used, which during a run is the one PEA is writing. Reading
/// the whole file each call is deliberate — the file is rewritten per run, not
/// appended across runs, and re-reading is what keeps a live panel honest
/// about satellites that have dropped out since the last poll.
// `async`: parsing a large file must not run on (and freeze) the main thread.
#[tauri::command(async)]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn load_satellite_status(
    output_dir: String,
    path: Option<String>,
) -> Result<SatelliteStatus, String> {
    let chosen = match path {
        Some(p) => p,
        None => {
            if !Path::new(&output_dir).is_dir() {
                return Ok(SatelliteStatus::unavailable(
                    "No output directory yet. Satellite status is read from the .TRACE file PEA \
                     writes there.",
                ));
            }
            // Newest with residuals in it: at the end of a run the smoothed
            // `.TRACE` is written last but holds only the config header.
            let files = list_trace_files(output_dir)?;
            let pick = files.iter().position(|f| f.has_residuals).unwrap_or(0);
            match files.into_iter().nth(pick) {
                Some(f) => f.path,
                None => {
                    return Ok(SatelliteStatus::unavailable(
                        "No .TRACE file in the output directory. Turn on TRACE output in the \
                         sidebar to see which satellites the solution is using.",
                    ))
                }
            }
        }
    };

    let p = Path::new(&chosen);
    let (grouped, _) = read_residual_rows(p).map_err(|e| format!("Cannot read {}: {}", chosen, e))?;
    let grouped = &*grouped;
    let name = file_name(p, &chosen);

    if grouped.is_empty() {
        // A live run's `.TRACE` exists before it has residuals in it, so this
        // is "not yet", not "broken".
        return Ok(SatelliteStatus {
            path: Some(chosen),
            name: Some(name),
            message: Some(
                "No residuals in the trace file yet. It appears as soon as PEA processes an epoch."
                    .to_string(),
            ),
            ..Default::default()
        });
    }

    let index = build_index(grouped);
    let mut constellations: Vec<ConstellationStatus> = Vec::new();
    // `index.satellites` is already sorted by constellation then id, so the
    // grouping is a single pass with no re-sort.
    for sat in index.satellites {
        match constellations.last_mut() {
            Some(last) if last.constellation == sat.constellation => last.satellites.push(sat),
            _ => {
                constellations.push(ConstellationStatus {
                    constellation: sat.constellation.clone(),
                    satellites: vec![sat],
                })
            }
        }
    }

    Ok(SatelliteStatus {
        path: Some(chosen),
        name: Some(name),
        available: true,
        message: None,
        receivers: index.receivers,
        satellite_count: constellations.iter().map(|c| c.satellites.len()).sum(),
        constellations,
        epochs: index.epochs,
        t_start: index.t_start,
        t_end: index.t_end,
    })
}

// ─── parsing ─────────────────────────────────────────────────────────────────

/// Identity of one plottable line. Ordered so the map iterates in a stable,
/// human-sensible order without a sort per call.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
struct SeriesKey {
    sat: String,
    receiver: String,
    meas_type: String,
    signal: String,
}

#[derive(Debug, Clone, Copy)]
struct Sample {
    /// Filter iteration this row came from. Kept so a later row can be
    /// compared against it — see [`read_residual_rows`].
    iteration: i32,
    prefit: f64,
    postfit: f64,
    sigma: f64,
}

/// One series' samples in time order, one per epoch.
type Samples = Vec<(i64, Sample)>;

/// Every residual row in the file, one entry per series in the order the
/// series first appeared. Callers sort what they return.
type Grouped = Vec<(SeriesKey, Samples)>;

/// A `.TRACE` parsed up to `offset`, kept so the next call only reads what
/// was appended since.
///
/// A day of multi-GNSS output is hundreds of megabytes and well over a million
/// rows, so parsing it from scratch on every satellite change — or on every
/// poll of a run that is still writing — would leave the residual view on "Reading…"
/// for minutes. The view asks for the same file again and again with different
/// filters, and PEA only ever appends to it, so the parse is done once and
/// extended.
struct ParsedTrace {
    path: PathBuf,
    grouped: Arc<Grouped>,
    skipped: usize,
    /// Byte offset just past the last complete line consumed.
    offset: u64,
    /// The bytes just before `offset`. If they still read the same, the file
    /// was appended to rather than rewritten by a new run, and parsing can
    /// resume at `offset`.
    tail: Vec<u8>,
    /// Row key text (type, sat, receiver, signal) → index into `grouped`, for
    /// rows the prediction below gets wrong.
    keys: HashMap<String, usize>,
    /// Per series, the series whose row followed it last time. PEA writes an
    /// epoch's rows in the same order every time, so this predicts the next
    /// row's series and spares a hash lookup on nearly every row.
    follows: Vec<usize>,
    /// Series of the previous row, the starting point for that prediction.
    prev: Option<usize>,
    /// The last timestamp text parsed and its value: every row of an epoch
    /// carries the same one, and parsing it is the dearest part of a row.
    last_time: Option<(String, i64)>,
}

/// How many bytes before the resume point are compared to detect a rewrite.
const TAIL_CHECK_BYTES: usize = 4096;

/// Files kept parsed: the one being looked at, plus the newest one the
/// satellite panel polls, which is often a different file.
const CACHED_FILES: usize = 2;

fn trace_cache() -> &'static Mutex<Vec<ParsedTrace>> {
    static CACHE: OnceLock<Mutex<Vec<ParsedTrace>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(Vec::new()))
}

/// Reads the `%` rows out of a `.TRACE`, keeping the highest iteration of each
/// repeated measurement.
///
/// Rows that are not a measurement of a real satellite — PEA's pseudo
/// observations such as the zero-DCB constraint on `G--` — are left out: they
/// are not what a residual plot is read for, and listing them as satellites
/// would put a flat line at zero in front of the user.
///
/// A partial final line — one with no terminating newline — is dropped without
/// being counted as skipped. During a live run the file is being appended to
/// while it is read, so the read almost always lands mid-line; counting that
/// as corruption would put a permanent "1 unreadable line" warning on every
/// streaming run. It is picked up whole on the next read.
fn read_residual_rows(path: &Path) -> Result<(Arc<Grouped>, usize), std::io::Error> {
    let mut file = File::open(path)?;
    let len = file.metadata()?.len();

    let mut cache = trace_cache().lock().unwrap_or_else(|e| e.into_inner());
    let mut entry = match cache.iter().position(|c| c.path == path) {
        Some(i) => cache.remove(i),
        None => ParsedTrace::new(path),
    };
    if !entry.continues_in(&mut file, len)? {
        entry = ParsedTrace::new(path);
    }

    let result = entry.extend_from(&mut file);
    // Most recently used first; a failed read leaves nothing half-built behind.
    if result.is_ok() {
        cache.insert(0, entry);
        cache.truncate(CACHED_FILES);
    }
    result
}

impl ParsedTrace {
    fn new(path: &Path) -> ParsedTrace {
        ParsedTrace {
            path: path.to_path_buf(),
            grouped: Arc::new(Vec::new()),
            skipped: 0,
            offset: 0,
            tail: Vec::new(),
            keys: HashMap::new(),
            follows: Vec::new(),
            prev: None,
            last_time: None,
        }
    }

    /// True when the file still starts with everything parsed so far, so
    /// reading can resume at `offset`.
    fn continues_in(&self, file: &mut File, len: u64) -> Result<bool, std::io::Error> {
        if self.offset == 0 {
            return Ok(true);
        }
        if len < self.offset {
            return Ok(false);
        }
        let mut current = vec![0u8; self.tail.len()];
        file.seek(SeekFrom::Start(self.offset - self.tail.len() as u64))?;
        file.read_exact(&mut current)?;
        Ok(current == self.tail)
    }

    fn extend_from(&mut self, file: &mut File) -> Result<(Arc<Grouped>, usize), std::io::Error> {
        file.seek(SeekFrom::Start(self.offset))?;
        let mut reader = BufReader::with_capacity(1 << 20, file);
        // `make_mut` only copies if an earlier caller still holds the map.
        let grouped = Arc::make_mut(&mut self.grouped);
        let mut line: Vec<u8> = Vec::with_capacity(256);
        let mut key_text = String::with_capacity(64);

        loop {
            line.clear();
            let read = reader.read_until(b'\n', &mut line)?;
            // No newline means end of file mid-write, so this line is
            // incomplete whatever it looks like; it is re-read next time.
            if read == 0 || line.last() != Some(&b'\n') {
                break;
            }
            self.offset += read as u64;
            if line.first() != Some(&b'%') {
                continue;
            }
            let Some(row) = std::str::from_utf8(&line).ok().and_then(RawRow::parse) else {
                self.skipped += 1;
                continue;
            };
            let Some(t) = cached_time(&mut self.last_time, &row.time) else {
                self.skipped += 1;
                continue;
            };
            // A residual is always a measurement of one satellite; a row
            // without a real satellite id is a state or a pseudo observation,
            // and has nothing to plot.
            if row.meas_type.is_empty() || !is_satellite(row.sat) {
                continue;
            }

            let predicted = self.prev.map(|p| self.follows[p]).filter(|&i| i < grouped.len());
            let idx = match predicted {
                Some(i) if matches_key(&grouped[i].0, &row) => i,
                _ => match self.keys.get(row_key_text(&mut key_text, &row)) {
                    Some(&i) => i,
                    None => {
                        grouped.push((
                            SeriesKey {
                                sat: row.sat.to_uppercase(),
                                receiver: row.receiver.to_string(),
                                meas_type: row.meas_type.to_string(),
                                signal: row.signal.to_string(),
                            },
                            Vec::new(),
                        ));
                        self.follows.push(usize::MAX);
                        self.keys.insert(key_text.clone(), grouped.len() - 1);
                        grouped.len() - 1
                    }
                },
            };
            if let Some(p) = self.prev {
                self.follows[p] = idx;
            }
            self.prev = Some(idx);
            file_sample(&mut grouped[idx].1, t, row.sample);
        }

        // Remember how the parsed part ends, to recognise it next time.
        let file = reader.into_inner();
        let keep = (self.offset as usize).min(TAIL_CHECK_BYTES);
        self.tail = vec![0u8; keep];
        if keep > 0 {
            file.seek(SeekFrom::Start(self.offset - keep as u64))?;
            file.read_exact(&mut self.tail)?;
        }

        Ok((Arc::clone(&self.grouped), self.skipped))
    }
}

/// The text a row's series is looked up by, built into a reused buffer.
fn row_key_text<'b>(buf: &'b mut String, row: &RawRow) -> &'b str {
    buf.clear();
    for part in [row.meas_type, row.sat, row.receiver, row.signal] {
        buf.push_str(part);
        buf.push('\t');
    }
    buf
}

/// Whether `key` is the series `row` belongs to, without building its text.
fn matches_key(key: &SeriesKey, row: &RawRow) -> bool {
    key.meas_type == row.meas_type
        && key.sat.eq_ignore_ascii_case(row.sat)
        && key.receiver == row.receiver
        && key.signal == row.signal
}

/// Files one row into its series, keeping one sample per epoch.
///
/// Rows nearly always arrive in time order, so this is an append or an
/// overwrite of the last sample; an out-of-order row is placed by search.
/// `>=` not `>`: the last row written for an epoch is the one that survived
/// the filter's rejection loop, and a re-run of the same iteration number
/// supersedes the earlier attempt.
fn file_sample(samples: &mut Samples, t: i64, sample: Sample) {
    let slot = match samples.last() {
        None => Err(0),
        Some(&(last, _)) if last < t => Err(samples.len()),
        Some(&(last, _)) if last == t => Ok(samples.len() - 1),
        Some(_) => samples.binary_search_by_key(&t, |&(at, _)| at),
    };
    match slot {
        Ok(i) => {
            if sample.iteration >= samples[i].1.iteration {
                samples[i].1 = sample;
            }
        }
        Err(i) => samples.insert(i, (t, sample)),
    }
}

/// A satellite id as PEA writes one (`G07`, `E11`, `C19`): a system letter
/// and a number. PEA's pseudo observations use ids like `G--` for "the
/// constellation as a whole", which are not satellites.
fn is_satellite(sat: &str) -> bool {
    let mut chars = sat.chars();
    matches!(chars.next(), Some(c) if c.is_ascii_alphabetic())
        && !chars.as_str().is_empty()
        && chars.all(|c| c.is_ascii_digit())
}

fn cached_time(cache: &mut Option<(String, i64)>, token: &str) -> Option<i64> {
    if let Some((text, ms)) = cache {
        if text == token {
            return Some(*ms);
        }
    }
    let ms = parse_trace_time(token)?;
    *cache = Some((token.to_string(), ms));
    Some(ms)
}

/// One `%` row, borrowed from the line it was read from.
struct RawRow<'a> {
    time: Cow<'a, str>,
    meas_type: &'a str,
    sat: &'a str,
    receiver: &'a str,
    signal: &'a str,
    sample: Sample,
}

impl<'a> RawRow<'a> {
    /// Splits a residual row into `[iteration, time, type, sat, str, code,
    /// prefit, postfit, sigma, extras…]`, with the leading `%` removed.
    /// `None` for anything that isn't a complete residual row — a truncated
    /// line, a header, or a row whose numbers don't parse.
    ///
    /// Tab-separated when PEA wrote it, which is what makes the empty columns
    /// (a receiver-less key, a signal-less state) survive the split. Files
    /// that have been through a tool that ate the tabs fall back to
    /// whitespace, where the timestamp's date and time arrive as two tokens
    /// and are re-joined.
    fn parse(line: &'a str) -> Option<RawRow<'a>> {
        let body = line.trim_end_matches(['\n', '\r']);
        let body = body.strip_prefix('%').unwrap_or(body);

        let mut fields: [&str; RESIDUAL_MIN_FIELDS] = [""; RESIDUAL_MIN_FIELDS];
        let time: Cow<'a, str>;
        if body.contains('\t') {
            // PEA writes "%\t…", so the separator right after the marker
            // belongs to the marker and is not an empty first column.
            let body = body.strip_prefix('\t').unwrap_or(body);
            let mut n = 0;
            for (slot, f) in fields.iter_mut().zip(body.split('\t')) {
                *slot = f.trim();
                n += 1;
            }
            if n < RESIDUAL_MIN_FIELDS {
                return None;
            }
            time = Cow::Borrowed(fields[1]);
        } else {
            let tokens: Vec<&str> = body.split_whitespace().collect();
            // "2020-01-01 00:00:00.00" is one field but two whitespace tokens;
            // the time half is recognised by its colons, which no other column
            // carries.
            if tokens.len() > 2 && tokens[2].contains(':') {
                if tokens.len() < RESIDUAL_MIN_FIELDS + 1 {
                    return None;
                }
                fields[0] = tokens[0];
                fields[2..].copy_from_slice(&tokens[3..RESIDUAL_MIN_FIELDS + 1]);
                time = Cow::Owned(format!("{} {}", tokens[1], tokens[2]));
            } else {
                if tokens.len() < RESIDUAL_MIN_FIELDS {
                    return None;
                }
                fields.copy_from_slice(&tokens[..RESIDUAL_MIN_FIELDS]);
                time = Cow::Borrowed(fields[1]);
            }
        }

        let num = |i: usize| -> Option<f64> {
            let v: f64 = fields[i].parse().ok()?;
            v.is_finite().then_some(v)
        };

        Some(RawRow {
            time,
            meas_type: fields[2],
            sat: fields[3],
            receiver: fields[4],
            signal: fields[5],
            sample: Sample {
                iteration: fields[0].parse().ok()?,
                prefit: num(6)?,
                postfit: num(7)?,
                // The sigma is the filter's assumed noise, used only for
                // context in the readout; a bad one leaves the residuals
                // themselves perfectly plottable, so it is clamped rather than
                // fatal.
                sigma: num(8).unwrap_or(0.0).max(0.0),
            },
        })
    }
}

/// Parses a TRACE timestamp (`2020-01-01 00:00:00.00`) to epoch milliseconds.
///
/// `GTime::to_string(2)` emits two decimals; the fractional part is treated as
/// optional so a file written at another precision, or converted by hand,
/// still loads.
fn parse_trace_time(token: &str) -> Option<i64> {
    let token = token.trim();
    let dt = NaiveDateTime::parse_from_str(token, "%Y-%m-%d %H:%M:%S%.f")
        .or_else(|_| NaiveDateTime::parse_from_str(token, "%Y-%m-%d %H:%M:%S"))
        // A `.TRACE` assembled from `$POS`-style ISO timestamps uses a 'T'.
        .or_else(|_| NaiveDateTime::parse_from_str(token, "%Y-%m-%dT%H:%M:%S%.f"))
        .ok()?;
    Some(dt.and_utc().timestamp_millis())
}

/// The constellation a satellite id belongs to, per `sysChar` in
/// `src/cpp/common/satSys.hpp`. Unknown prefixes are grouped under "Other"
/// rather than dropped — a satellite PEA used is worth showing even if this
/// build doesn't have a name for its system.
fn constellation_of(sat: &str) -> &'static str {
    match sat.chars().next() {
        Some('G') => "GPS",
        Some('R') => "GLO",
        Some('E') => "GAL",
        Some('C') => "BDS",
        Some('J') => "QZS",
        Some('I') => "IRN",
        Some('S') => "SBS",
        Some('L') => "LEO",
        _ => "Other",
    }
}

// ─── shaping ─────────────────────────────────────────────────────────────────

fn build_index(grouped: &Grouped) -> TraceIndex {
    let mut receivers: Vec<String> = Vec::new();
    let mut signals: Vec<String> = Vec::new();
    let mut epochs: std::collections::BTreeSet<i64> = std::collections::BTreeSet::new();
    // Keyed by satellite id, in id order, so the panel's grouping is stable.
    let mut sats: BTreeMap<String, SatelliteSummary> = BTreeMap::new();
    // Postfit sum-of-squares per satellite, accumulated alongside the summary
    // so the RMS needs no second pass over the samples.
    let mut sum_sq: BTreeMap<String, (f64, usize)> = BTreeMap::new();
    let mut total_samples = 0usize;

    for (key, samples) in grouped {
        if samples.is_empty() {
            continue;
        }
        if !key.receiver.is_empty() && !receivers.contains(&key.receiver) {
            receivers.push(key.receiver.clone());
        }
        if !key.signal.is_empty() && !signals.contains(&key.signal) {
            signals.push(key.signal.clone());
        }
        epochs.extend(samples.iter().map(|&(t, _)| t));
        total_samples += samples.len();

        let first = samples[0].0 as f64;
        let last = samples[samples.len() - 1].0 as f64;

        let entry = sats.entry(key.sat.clone()).or_insert_with(|| SatelliteSummary {
            sat: key.sat.clone(),
            constellation: constellation_of(&key.sat).to_string(),
            signals: Vec::new(),
            receivers: Vec::new(),
            samples: 0,
            first_ms: first,
            last_ms: last,
            postfit_rms: 0.0,
        });
        if !key.signal.is_empty() && !entry.signals.contains(&key.signal) {
            entry.signals.push(key.signal.clone());
        }
        if !key.receiver.is_empty() && !entry.receivers.contains(&key.receiver) {
            entry.receivers.push(key.receiver.clone());
        }
        entry.samples += samples.len();
        entry.first_ms = entry.first_ms.min(first);
        entry.last_ms = entry.last_ms.max(last);

        let acc = sum_sq.entry(key.sat.clone()).or_insert((0.0, 0));
        for (_, s) in samples {
            acc.0 += s.postfit * s.postfit;
            acc.1 += 1;
        }
    }

    let mut satellites: Vec<SatelliteSummary> = sats
        .into_values()
        .map(|mut s| {
            s.signals.sort();
            s.receivers.sort();
            if let Some((sq, n)) = sum_sq.get(&s.sat) {
                if *n > 0 {
                    s.postfit_rms = (sq / *n as f64).sqrt();
                }
            }
            s
        })
        .collect();
    // Constellation first so the panel can section the list without regrouping.
    satellites.sort_by(|a, b| a.constellation.cmp(&b.constellation).then_with(|| a.sat.cmp(&b.sat)));

    receivers.sort();
    signals.sort();

    TraceIndex {
        receivers,
        signals,
        epochs: epochs.len(),
        total_samples,
        t_start: epochs.iter().next().copied().unwrap_or(0) as f64,
        t_end: epochs.iter().next_back().copied().unwrap_or(0) as f64,
        satellites,
    }
}

/// Builds one display series, taking every `stride`th sample and always the
/// last one, so the drawn line ends where the data ends.
fn decimate(key: &SeriesKey, samples: &[(i64, Sample)], stride: usize) -> ResidualSeries {
    let total_samples = samples.len();
    let prefit_stats = residual_stats(samples.iter().map(|(_, s)| (s.prefit, s.sigma)));
    let postfit_stats = residual_stats(samples.iter().map(|(_, s)| (s.postfit, s.sigma)));

    let mut series = ResidualSeries {
        key: format!("{}|{}|{}|{}", key.meas_type, key.sat, key.receiver, key.signal),
        sat: key.sat.clone(),
        constellation: constellation_of(&key.sat).to_string(),
        receiver: key.receiver.clone(),
        signal: key.signal.clone(),
        kind: MeasKind::from_type(&key.meas_type),
        meas_type: key.meas_type.clone(),
        total_samples,
        t: Vec::new(),
        prefit: Vec::new(),
        postfit: Vec::new(),
        sigma: Vec::new(),
        prefit_stats,
        postfit_stats,
    };

    for (i, (t, s)) in samples.iter().enumerate() {
        if i % stride != 0 && i != total_samples - 1 {
            continue;
        }
        series.t.push(*t as f64);
        series.prefit.push(s.prefit);
        series.postfit.push(s.postfit);
        series.sigma.push(s.sigma);
    }
    series
}

/// Mean, population standard deviation and RMS about zero, plus the mean of
/// the measurement sigmas. See [`ResidualStats`] for why this is unweighted.
fn residual_stats<I>(samples: I) -> ResidualStats
where
    I: Iterator<Item = (f64, f64)> + Clone,
{
    let mut n = 0usize;
    let mut sum = 0.0;
    let mut sum_sq = 0.0;
    let mut sum_sigma = 0.0;
    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;

    for (x, sigma) in samples.clone() {
        n += 1;
        sum += x;
        sum_sq += x * x;
        sum_sigma += sigma;
        min = min.min(x);
        max = max.max(x);
    }

    if n == 0 {
        return ResidualStats::default();
    }

    let mean = sum / n as f64;
    let variance = samples.map(|(x, _)| (x - mean) * (x - mean)).sum::<f64>() / n as f64;

    ResidualStats {
        mean,
        std_dev: variance.sqrt(),
        rms: (sum_sq / n as f64).sqrt(),
        min,
        max,
        mean_sigma: sum_sigma / n as f64,
    }
}

fn file_name(p: &Path, fallback: &str) -> String {
    p.file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| fallback.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::path::PathBuf;

    /// A `RESIDUALS` block in the shape `outputResiduals` writes it at trace
    /// level 2: tab separated, comment column present, no ratio columns. Two
    /// epochs of two GPS satellites, one of them on two signals, plus one
    /// Galileo satellite.
    const SAMPLE_TRACE: &str = concat!(
        "\n+RESIDUALS\n",
        "#\tIt\tTime\tType\tSat\tStr\tCode\tPrefit Res\tPostfit Res\tMeas Sigma\tComments\n",
        "%\t 0\t2020-01-01 00:00:00.00\t CODE_MEAS\tG01\tALIC\tL1C\t       0.10000000\t       0.01000000\t      0.30000000\t\n",
        "%\t 0\t2020-01-01 00:00:00.00\t PHAS_MEAS\tG01\tALIC\tL1C\t       0.02000000\t       0.00200000\t      0.00300000\t\n",
        "%\t 0\t2020-01-01 00:00:00.00\t CODE_MEAS\tG07\tALIC\tL2W\t       0.20000000\t       0.03000000\t      0.30000000\t\n",
        "%\t 0\t2020-01-01 00:00:00.00\t CODE_MEAS\tE11\tALIC\tL1C\t       0.30000000\t       0.05000000\t      0.30000000\t\n",
        "-RESIDUALS\n",
        "\n+RESIDUALS\n",
        "%\t 0\t2020-01-01 00:00:30.00\t CODE_MEAS\tG01\tALIC\tL1C\t       0.30000000\t       0.03000000\t      0.30000000\t\n",
        "%\t 0\t2020-01-01 00:00:30.00\t PHAS_MEAS\tG01\tALIC\tL1C\t       0.04000000\t       0.00400000\t      0.00300000\t\n",
        "%\t 0\t2020-01-01 00:00:30.00\t CODE_MEAS\tG07\tALIC\tL2W\t       0.40000000\t       0.05000000\t      0.30000000\t\n",
        "%\t 0\t2020-01-01 00:00:30.00\t CODE_MEAS\tE11\tALIC\tL1C\t       0.50000000\t       0.07000000\t      0.30000000\t\n",
        "-RESIDUALS\n",
    );

    fn unique_tmp_dir(label: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        p.push(format!("ginan_trace_test_{}_{}_{}", label, std::process::id(), nanos));
        std::fs::create_dir_all(&p).expect("create tmp dir");
        p
    }

    fn write_trace(dir: &Path, name: &str, body: &str) -> String {
        let path = dir.join(name);
        let mut f = File::create(&path).unwrap();
        f.write_all(body.as_bytes()).unwrap();
        path.to_string_lossy().into_owned()
    }

    fn find<'a>(s: &'a TraceSeries, key: &str) -> &'a ResidualSeries {
        s.series.iter().find(|r| r.key == key).unwrap_or_else(|| panic!("no series {key}"))
    }

    #[test]
    fn parses_residual_rows_into_per_signal_series() {
        let dir = unique_tmp_dir("parse");
        let path = write_trace(&dir, "ALIC.TRACE", SAMPLE_TRACE);

        let s = load_trace_series(path, None, None, None).expect("parse");
        assert_eq!(s.skipped_lines, 0);
        assert_eq!(s.index.epochs, 2);
        assert_eq!(s.index.total_samples, 8);
        assert_eq!(s.index.receivers, vec!["ALIC"]);
        assert_eq!(s.index.signals, vec!["L1C", "L2W"]);
        // One series per (sat, receiver, meas type, signal).
        assert_eq!(s.series.len(), 4);

        let code = find(&s, "CODE_MEAS|G01|ALIC|L1C");
        assert_eq!(code.kind, MeasKind::Code);
        assert_eq!(code.constellation, "GPS");
        assert_eq!(code.prefit, vec![0.1, 0.3]);
        assert_eq!(code.postfit, vec![0.01, 0.03]);
        assert_eq!(code.sigma, vec![0.3, 0.3]);
        // 30 s apart, in epoch milliseconds.
        assert_eq!(code.t[1] - code.t[0], 30_000.0);

        assert_eq!(find(&s, "PHAS_MEAS|G01|ALIC|L1C").kind, MeasKind::Phase);

        let stats = code.postfit_stats;
        assert!((stats.mean - 0.02).abs() < 1e-12);
        assert!((stats.rms - ((0.01f64.powi(2) + 0.03f64.powi(2)) / 2.0).sqrt()).abs() < 1e-12);
        assert!((stats.min - 0.01).abs() < 1e-12);
        assert!((stats.max - 0.03).abs() < 1e-12);
        assert!((stats.mean_sigma - 0.3).abs() < 1e-12);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn groups_satellites_by_constellation_in_the_index() {
        let dir = unique_tmp_dir("index");
        let path = write_trace(&dir, "ALIC.TRACE", SAMPLE_TRACE);

        let s = load_trace_series(path, None, None, None).expect("parse");
        let ids: Vec<&str> = s.index.satellites.iter().map(|x| x.sat.as_str()).collect();
        // Constellation first, then satellite id: BDS < GAL < GPS alphabetically,
        // and only GAL and GPS are present here.
        assert_eq!(ids, vec!["E11", "G01", "G07"]);

        let g01 = &s.index.satellites[1];
        assert_eq!(g01.constellation, "GPS");
        // Both the code and the phase series contribute rows.
        assert_eq!(g01.samples, 4);
        assert_eq!(g01.signals, vec!["L1C"]);
        assert_eq!(g01.receivers, vec!["ALIC"]);
        assert_eq!(g01.first_ms, s.index.t_start);
        assert_eq!(g01.last_ms, s.index.t_end);

        let e11 = &s.index.satellites[0];
        assert_eq!(e11.constellation, "GAL");
        assert!((e11.postfit_rms - ((0.05f64.powi(2) + 0.07f64.powi(2)) / 2.0).sqrt()).abs() < 1e-12);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn filters_to_the_requested_satellites_but_indexes_all_of_them() {
        let dir = unique_tmp_dir("filter");
        let path = write_trace(&dir, "ALIC.TRACE", SAMPLE_TRACE);

        let s = load_trace_series(path.clone(), Some(vec!["g01".into()]), None, None)
            .expect("parse");
        assert_eq!(s.series.len(), 2);
        assert!(s.series.iter().all(|r| r.sat == "G01"));
        // The index is the whole file regardless of the filter, so a selector
        // built from it still offers the satellites that were filtered out.
        assert_eq!(s.index.satellites.len(), 3);

        // An empty list is "index only", not "no filter".
        let none = load_trace_series(path.clone(), Some(vec![]), None, None).expect("parse");
        assert!(none.series.is_empty());
        assert_eq!(none.index.satellites.len(), 3);

        // A receiver that observed nothing here yields no series either.
        let other = load_trace_series(path, None, Some("BOGT".into()), None).expect("parse");
        assert!(other.series.is_empty());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn keeps_the_highest_iteration_of_a_repeated_measurement() {
        // PEA re-emits a row per filter iteration; only the last one made it
        // into the solution, and the earlier attempts must not become extra
        // points at the same epoch.
        let body = concat!(
            "%\t 0\t2020-01-01 00:00:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.9\t0.3\t\n",
            "%\t 1\t2020-01-01 00:00:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.2\t0.3\t\n",
            // Out of order, and lower — must not displace iteration 1.
            "%\t 0\t2020-01-01 00:00:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.8\t0.3\t\n",
        );
        let dir = unique_tmp_dir("iterations");
        let path = write_trace(&dir, "ALIC.TRACE", body);

        let s = load_trace_series(path, None, None, None).expect("parse");
        let series = find(&s, "CODE_MEAS|G01|ALIC|L1C");
        assert_eq!(series.total_samples, 1);
        assert_eq!(series.postfit, vec![0.2]);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn reads_the_ratio_and_whitespace_variants() {
        // Level 5 adds the two ratio columns before the comment; a file whose
        // tabs have been flattened to spaces still has to parse.
        let body = concat!(
            "%\t 0\t2020-01-01 00:00:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.1\t0.3\t2.5\t0.4\tOUTLIER\n",
            "%  0 2020-01-01 00:00:30.00 CODE_MEAS G01 ALIC L1C 2.0 0.2 0.3\n",
        );
        let dir = unique_tmp_dir("variants");
        let path = write_trace(&dir, "ALIC.TRACE", body);

        let s = load_trace_series(path, None, None, None).expect("parse");
        assert_eq!(s.skipped_lines, 0);
        let series = find(&s, "CODE_MEAS|G01|ALIC|L1C");
        assert_eq!(series.prefit, vec![1.0, 2.0]);
        assert_eq!(series.postfit, vec![0.1, 0.2]);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_half_written_final_line_is_dropped_not_counted_as_corrupt() {
        // What a live run looks like: the reader catches up with the writer
        // mid-line. The complete rows above it must still load, and no
        // corruption warning should appear.
        let mut body = SAMPLE_TRACE.to_string();
        body.push_str("%\t 0\t2020-01-01 00:01:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t0.6");
        let dir = unique_tmp_dir("partial");
        let path = write_trace(&dir, "ALIC.TRACE", &body);

        let s = load_trace_series(path, None, None, None).expect("parse");
        assert_eq!(s.skipped_lines, 0);
        assert_eq!(s.index.epochs, 2);
        assert_eq!(find(&s, "CODE_MEAS|G01|ALIC|L1C").total_samples, 2);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn counts_junk_rows_without_failing_the_load() {
        let mut body = SAMPLE_TRACE.to_string();
        // Short of the required columns, a non-numeric residual, and a NaN —
        // all three reach the `%` test, so all three are reported.
        body.push_str("%\t 0\t2020-01-01 00:01:00.00\tCODE_MEAS\tG01\n");
        body.push_str("%\t 0\t2020-01-01 00:01:00.00\tCODE_MEAS\tG01\tALIC\tL1C\tabc\t0.1\t0.3\t\n");
        body.push_str("%\t 0\t2020-01-01 00:01:00.00\tCODE_MEAS\tG01\tALIC\tL1C\tnan\t0.1\t0.3\t\n");
        // Not a residual row at all: the states block uses '*', and is ignored
        // silently rather than counted.
        body.push_str("*\t 0\t2020-01-01 00:01:00.00\tREC_POS\t   \tALIC\tX\t1.0\t0.1\t0.0\n");
        let dir = unique_tmp_dir("junk");
        let path = write_trace(&dir, "ALIC.TRACE", &body);

        let s = load_trace_series(path, None, None, None).expect("parse");
        assert_eq!(s.skipped_lines, 3);
        assert_eq!(s.index.epochs, 2);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn decimates_to_the_budget_across_the_whole_selection() {
        let mut body = String::new();
        for i in 0..100 {
            for sat in ["G01", "G07"] {
                body.push_str(&format!(
                    "%\t 0\t2020-01-01 00:{:02}:{:02}.00\tCODE_MEAS\t{sat}\tALIC\tL1C\t{i}\t{i}\t0.3\t\n",
                    i / 60,
                    i % 60
                ));
            }
        }
        let dir = unique_tmp_dir("decimate");
        let path = write_trace(&dir, "ALIC.TRACE", &body);

        let s = load_trace_series(path, None, None, Some(20)).expect("parse");
        assert!(s.decimated);
        assert_eq!(s.index.total_samples, 200);
        // Stride 10 over 100 samples per series, plus the kept final sample.
        assert!(s.returned_samples <= 22, "got {}", s.returned_samples);
        let series = find(&s, "CODE_MEAS|G01|ALIC|L1C");
        assert_eq!(*series.postfit.last().unwrap(), 99.0);
        // Statistics ignore decimation: the mean of 0..99 is 49.5.
        assert!((series.postfit_stats.mean - 49.5).abs() < 1e-9);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_file_without_residuals_is_an_error_not_an_empty_plot() {
        // What `outputs.trace.output_residuals: false` leaves behind: a real
        // trace file with states and headers but nothing plottable.
        let dir = unique_tmp_dir("noresiduals");
        let path = write_trace(
            &dir,
            "ALIC.TRACE",
            "+STATES\n*\t 0\t2020-01-01 00:00:00.00\tREC_POS\t\tALIC\tX\t1.0\t0.1\t0.0\n-STATES\n",
        );

        let err = load_trace_series(path, None, None, None).expect_err("expected an error");
        assert!(err.contains("No residual records"), "got {err}");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn lists_trace_files_newest_first_and_ignores_everything_else() {
        let dir = unique_tmp_dir("list");
        write_trace(&dir, "old.TRACE", SAMPLE_TRACE);
        write_trace(&dir, "new.trace", SAMPLE_TRACE);
        write_trace(&dir, "ALIC.POS", "not a trace");
        std::fs::create_dir_all(dir.join("visual")).unwrap();
        write_trace(&dir.join("visual"), "nested.TRACE", SAMPLE_TRACE);

        // Modification times a second apart, so the sort has something to
        // order on that isn't filesystem timestamp granularity.
        let older = std::time::SystemTime::now() - std::time::Duration::from_secs(60);
        std::fs::OpenOptions::new()
            .write(true)
            .open(dir.join("old.TRACE"))
            .and_then(|f| f.set_modified(older))
            .expect("backdate");

        let files = list_trace_files(dir.to_string_lossy().into_owned()).expect("list");
        let names: Vec<&str> = files.iter().map(|f| f.name.as_str()).collect();
        assert_eq!(names, vec!["new.trace", "old.TRACE"]);
        assert!(files.iter().all(|f| f.has_residuals));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_header_only_trace_is_flagged_and_skipped_for_satellite_status() {
        // What a finished run leaves behind: the smoothed `.TRACE` is written
        // last but carries only PEA's config header.
        let dir = unique_tmp_dir("header_only");
        write_trace(&dir, "run.TRACE", SAMPLE_TRACE);
        let older = std::time::SystemTime::now() - std::time::Duration::from_secs(60);
        std::fs::OpenOptions::new()
            .write(true)
            .open(dir.join("run.TRACE"))
            .and_then(|f| f.set_modified(older))
            .expect("backdate");
        write_trace(&dir, "run_smoothed.TRACE", "station    : Network\n+FILE/RAW_CONFIG\ninputs:\n-FILE/RAW_CONFIG\n");

        let files = list_trace_files(dir.to_string_lossy().into_owned()).expect("list");
        assert_eq!(files[0].name, "run_smoothed.TRACE");
        assert!(!files[0].has_residuals);
        assert!(files[1].has_residuals);

        let status = load_satellite_status(dir.to_string_lossy().into_owned(), None).expect("status");
        assert!(status.available);
        assert_eq!(status.name.as_deref(), Some("run.TRACE"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn missing_directory_is_an_error() {
        let err = list_trace_files("/definitely/not/a/real/dir".to_string())
            .expect_err("expected an error");
        assert!(err.contains("does not exist"), "got {err}");
    }

    #[test]
    fn satellite_status_groups_by_constellation_and_marks_the_latest_epoch() {
        let dir = unique_tmp_dir("status");
        write_trace(&dir, "ALIC.TRACE", SAMPLE_TRACE);
        // A satellite that stopped contributing before the end of the run: it
        // must still be listed, with a `last_ms` short of `t_end` so the panel
        // can tell it apart from the ones still being tracked.
        let mut body = SAMPLE_TRACE.to_string();
        body.push_str(
            "%\t 0\t2020-01-01 00:01:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t0.6\t0.06\t0.3\t\n",
        );
        write_trace(&dir, "LATER.TRACE", &body);

        let status = load_satellite_status(
            dir.to_string_lossy().into_owned(),
            Some(dir.join("LATER.TRACE").to_string_lossy().into_owned()),
        )
        .expect("status");

        assert!(status.available);
        assert_eq!(status.message, None);
        assert_eq!(status.satellite_count, 3);
        assert_eq!(status.epochs, 3);
        assert_eq!(status.receivers, vec!["ALIC"]);

        let names: Vec<&str> =
            status.constellations.iter().map(|c| c.constellation.as_str()).collect();
        assert_eq!(names, vec!["GAL", "GPS"]);
        assert_eq!(status.constellations[0].satellites.len(), 1);
        assert_eq!(status.constellations[1].satellites.len(), 2);

        let gps = &status.constellations[1];
        let g01 = gps.satellites.iter().find(|s| s.sat == "G01").unwrap();
        let g07 = gps.satellites.iter().find(|s| s.sat == "G07").unwrap();
        assert_eq!(g01.last_ms, status.t_end);
        assert!(g07.last_ms < status.t_end);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn satellite_status_picks_the_newest_trace_when_no_path_is_given() {
        let dir = unique_tmp_dir("status_newest");
        write_trace(&dir, "old.TRACE", SAMPLE_TRACE);
        write_trace(
            &dir,
            "new.TRACE",
            "%\t 0\t2020-01-01 00:00:00.00\tCODE_MEAS\tR09\tALIC\tL1C\t0.1\t0.01\t0.3\t\n",
        );
        let older = std::time::SystemTime::now() - std::time::Duration::from_secs(60);
        std::fs::OpenOptions::new()
            .write(true)
            .open(dir.join("old.TRACE"))
            .and_then(|f| f.set_modified(older))
            .expect("backdate");

        let status =
            load_satellite_status(dir.to_string_lossy().into_owned(), None).expect("status");
        assert_eq!(status.name.as_deref(), Some("new.TRACE"));
        assert_eq!(status.satellite_count, 1);
        assert_eq!(status.constellations[0].constellation, "GLO");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn satellite_status_reports_nothing_to_show_instead_of_failing() {
        // Trace output turned off: the directory exists, the run wrote .POS
        // files, and there is simply no .TRACE. The panel has to survive that.
        let dir = unique_tmp_dir("status_none");
        write_trace(&dir, "ALIC.POS", "not a trace");

        let status =
            load_satellite_status(dir.to_string_lossy().into_owned(), None).expect("status");
        assert!(!status.available);
        assert!(status.constellations.is_empty());
        assert!(status.message.unwrap().contains("TRACE output"));

        // Trace output on, but the run hasn't reached an epoch yet.
        let empty = write_trace(&dir, "ALIC.TRACE", "+STATES\n-STATES\n");
        let status = load_satellite_status(dir.to_string_lossy().into_owned(), Some(empty))
            .expect("status");
        assert!(!status.available);
        assert!(status.message.unwrap().contains("as soon as PEA processes an epoch"));

        // No output directory at all, which is how the app starts.
        let status =
            load_satellite_status("/definitely/not/a/real/dir".to_string(), None).expect("status");
        assert!(!status.available);
        assert!(status.message.unwrap().contains("No output directory"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn constellations_follow_the_satsys_prefixes() {
        assert_eq!(constellation_of("G01"), "GPS");
        assert_eq!(constellation_of("R09"), "GLO");
        assert_eq!(constellation_of("E11"), "GAL");
        assert_eq!(constellation_of("C22"), "BDS");
        assert_eq!(constellation_of("J03"), "QZS");
        assert_eq!(constellation_of("I05"), "IRN");
        assert_eq!(constellation_of("S31"), "SBS");
        assert_eq!(constellation_of("?99"), "Other");
        assert_eq!(constellation_of(""), "Other");
    }

    #[test]
    fn pseudo_observations_are_not_satellites() {
        // PEA's zero-DCB constraint is written as a CODE_BIAS row against a
        // whole constellation ("G--"). It must neither appear as a satellite
        // nor be filed as a code residual.
        let mut body = SAMPLE_TRACE.to_string();
        body.push_str(
            "%\t 0\t2020-01-01 00:00:30.00\t CODE_BIAS\t G--\tALIC\t   NONE\t      -4.07\t      -2.1e-11\t      1.0e-05\tZero DCB\n",
        );
        body.push_str(
            "%\t 0\t2020-01-01 00:00:30.00\t ION_STEC\t G01\tALIC\t       \t       0.5\t       0.1\t      0.3\t\n",
        );
        let dir = unique_tmp_dir("pseudo");
        let path = write_trace(&dir, "ALIC.TRACE", &body);

        let s = load_trace_series(path, None, None, None).expect("parse");
        assert_eq!(s.skipped_lines, 0);
        assert!(s.index.satellites.iter().all(|x| x.sat != "G--"));
        assert_eq!(s.index.satellites.len(), 3);
        // A real satellite's non-measurement row is kept, as "other".
        assert_eq!(find(&s, "ION_STEC|G01|ALIC|").kind, MeasKind::Other);
        assert_eq!(MeasKind::from_type("CODE_BIAS"), MeasKind::Other);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_growing_file_is_extended_and_a_rewritten_one_is_reparsed() {
        let dir = unique_tmp_dir("grow");
        let first = concat!(
            "%\t 0\t2020-01-01 00:00:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.1\t0.3\t\n",
            "%\t 0\t2020-01-01 00:00:30.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.2\t0.3",
        );
        let path = write_trace(&dir, "ALIC.TRACE", first);
        let s = load_trace_series(path.clone(), None, None, None).expect("parse");
        assert_eq!(find(&s, "CODE_MEAS|G01|ALIC|L1C").postfit, vec![0.1]);

        // PEA finishes the half-written line and appends another epoch.
        let mut f = std::fs::OpenOptions::new().append(true).open(&path).unwrap();
        f.write_all(b"\n%\t 0\t2020-01-01 00:01:00.00\tCODE_MEAS\tG01\tALIC\tL1C\t1.0\t0.3\t0.3\t\n")
            .unwrap();
        drop(f);
        let s = load_trace_series(path.clone(), None, None, None).expect("parse");
        assert_eq!(find(&s, "CODE_MEAS|G01|ALIC|L1C").postfit, vec![0.1, 0.2, 0.3]);
        assert_eq!(s.skipped_lines, 0);

        // A new run truncates and rewrites the file with different content,
        // longer than before: nothing from the old run may survive.
        let rewritten = concat!(
            "%\t 0\t2020-01-02 00:00:00.00\tCODE_MEAS\tE11\tALIC\tL1C\t1.0\t0.7\t0.3\t\n",
            "%\t 0\t2020-01-02 00:00:30.00\tCODE_MEAS\tE11\tALIC\tL1C\t1.0\t0.8\t0.3\t\n",
            "%\t 0\t2020-01-02 00:01:00.00\tCODE_MEAS\tE11\tALIC\tL1C\t1.0\t0.9\t0.3\t\n",
            "%\t 0\t2020-01-02 00:01:30.00\tCODE_MEAS\tE11\tALIC\tL1C\t1.0\t1.0\t0.3\t\n",
        );
        write_trace(&dir, "ALIC.TRACE", rewritten);
        let s = load_trace_series(path, None, None, None).expect("parse");
        assert_eq!(s.series.len(), 1);
        assert_eq!(find(&s, "CODE_MEAS|E11|ALIC|L1C").postfit, vec![0.7, 0.8, 0.9, 1.0]);

        let _ = std::fs::remove_dir_all(&dir);
    }
}
