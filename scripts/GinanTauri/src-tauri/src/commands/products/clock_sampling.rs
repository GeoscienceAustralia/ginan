//! Choosing between the 5-second and 30-second clock products.
//!
//! A RINEX file sampled faster than 30 s is better matched by the `05S` clock
//! (`COD0OPSFIN_20262570000_01D_05S_CLK.CLK`) than by the `30S` one. Only some
//! analysis centres publish 05S clocks, and the GPS-week catalogue cannot say
//! whether one exists for a given day (it keeps a single best file per content
//! type, and a run can span weeks the catalogue never scanned). So the 05S
//! file is simply tried first and the usual clock is the fallback.
//!
//! The generated config never names clock files: `clk_files` is the glob
//! `"*.CLK"` over the products directory. Leaving a 30S and a 05S file for the
//! same day side by side would make PEA load both, so once one of the pair is
//! settled on, the other is removed. Everything in the products directory is a
//! re-downloadable public product, and the glob is the contract with configs the
//! user keeps (`configOverwrite` off), so fixing the directory is the only
//! place that works for them too.

use super::catalogue_parse::{parse_igs_filename, PppCatalogueEntry};
use super::download::{present_candidate, url_basename};
use super::http::download_and_gunzip;
use super::naming::{filename_for, long_name};
use chrono::NaiveDate;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tauri::Emitter;

/// A RINEX file sampled faster than this wants clocks sampled faster than 30 s.
const FINE_CLOCK_BELOW_S: f64 = 30.0;
/// IGS long-name sample field of the 5-second clock products.
pub(crate) const FINE_CLOCK_SAMPLE: &str = "05S";

/// True when the RINEX file's own sampling interval is known and under 30 s.
/// An unknown interval (no INTERVAL header record) or a nonsensical one keeps
/// the usual clock rather than guessing.
pub(crate) fn wants_fine_clock(rinex_interval_s: Option<f64>) -> bool {
    matches!(rinex_interval_s, Some(i) if i.is_finite() && i > 0.0 && i < FINE_CLOCK_BELOW_S)
}

/// The 05S clock to try ahead of the usual one for a day.
#[derive(Debug, PartialEq)]
pub(crate) struct FineClock {
    /// File name without `.gz`.
    pub filename: String,
    /// The RINEX interval that asked for it, for the log.
    pub interval_s: f64,
}

/// The 05S clock to try first, or `None` to leave clock selection untouched.
///
/// `None` when the interval is 30 s or more, unknown, or when the usual clock
/// for this combination already is the 05S file (a catalogue entry that only
/// offers 05S), so the same URL is never tried twice.
#[allow(clippy::too_many_arguments)]
pub(crate) fn fine_clock_to_try(
    rinex_interval_s: Option<f64>,
    entry: Option<&PppCatalogueEntry>,
    ac: &str,
    project: &str,
    series: &str,
    year: i32,
    doy: u32,
) -> Option<FineClock> {
    if !wants_fine_clock(rinex_interval_s) {
        return None;
    }
    let interval_s = rinex_interval_s?;
    let fine = long_name(ac, project, series, year, doy, "01D", FINE_CLOCK_SAMPLE, "CLK", "CLK");
    let usual = filename_for("CLK", "CLK", entry, ac, project, series, year, doy);
    (fine != usual).then_some(FineClock { filename: fine, interval_s })
}

/// Sample field of an IGS long-name product ("30S", "05S", ...).
pub(crate) fn sample_of(filename: &str) -> Option<String> {
    let (_p, _pr, _s, _y, _d, _ts, sample, _c, _e) = parse_igs_filename(filename)?;
    Some(sample)
}

/// Clock files in `root` that the chosen `kept` file makes redundant: the same
/// product (analysis centre, project, series, day, span) at a different
/// sampling, where either `kept` or the other file is the 05S one. Two
/// non-05S samplings side by side (never created by this feature) are left
/// alone, as is every other series, analysis centre or day.
pub(crate) fn superseded_clock_files(root: &Path, kept: &str) -> Vec<PathBuf> {
    let kept = kept.trim_end_matches(".gz");
    let Some(keep) = parse_igs_filename(kept) else {
        return Vec::new();
    };
    let Ok(dir) = std::fs::read_dir(root) else {
        return Vec::new();
    };
    let mut found: Vec<PathBuf> = Vec::new();
    for item in dir.filter_map(Result::ok) {
        let path = item.path();
        let Some(name) = item.file_name().to_str().map(str::to_string) else {
            continue;
        };
        // parse_igs_filename only inspects the first 38 characters, so a
        // `.CLK.gz.part` leftover would parse too; insist on the whole name.
        if name.len() != kept.len() || !path.is_file() {
            continue;
        }
        let Some(other) = parse_igs_filename(&name) else {
            continue;
        };
        let same_product = other.0 == keep.0
            && other.1 == keep.1
            && other.2 == keep.2
            && other.3 == keep.3
            && other.4 == keep.4
            && other.5 == keep.5
            && other.7 == keep.7
            && other.8 == keep.8;
        let fine_pair = (other.6 == FINE_CLOCK_SAMPLE) != (keep.6 == FINE_CLOCK_SAMPLE);
        if same_product && fine_pair {
            found.push(path);
        }
    }
    found.sort();
    found
}

/// What [`remove_superseded_clocks`] did: the names it removed, and the names
/// it could not remove with the reason.
#[derive(Debug, Default, PartialEq)]
pub(crate) struct ClockCleanup {
    pub removed: Vec<String>,
    pub failed: Vec<(String, String)>,
}

/// Delete [`superseded_clock_files`], reporting failures rather than dropping
/// them: a sibling that survives is the state that makes PEA load both clocks.
pub(crate) fn remove_superseded_clocks(root: &Path, kept: &str) -> ClockCleanup {
    remove_superseded_clocks_with(root, kept, |p| std::fs::remove_file(p))
}

/// [`remove_superseded_clocks`] with the removal injected, so a failed delete
/// (a file still open on Windows, a read-only directory) can be tested on every
/// platform.
pub(crate) fn remove_superseded_clocks_with(
    root: &Path,
    kept: &str,
    remove: impl Fn(&Path) -> std::io::Result<()>,
) -> ClockCleanup {
    let mut out = ClockCleanup::default();
    for path in superseded_clock_files(root, kept) {
        let name = path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default();
        match remove(&path) {
            Ok(()) => out.removed.push(name),
            Err(e) => out.failed.push((name, e.to_string())),
        }
    }
    out
}

/// Settle on `kept` as the day's clock: drop the redundant sibling and say so.
/// A sibling that cannot be removed gets a warning naming the file, because the
/// `*.CLK` glob will feed both to PEA until the user deletes it.
pub(crate) fn keep_single_clock(root: &Path, kept: &str, app: &tauri::AppHandle) {
    let cleanup = remove_superseded_clocks(root, kept);
    for name in cleanup.removed {
        app.emit(
            "console-line",
            format!("  Removed superseded clock file {name}: only {kept} is kept for this day"),
        )
        .ok();
    }
    for (name, e) in cleanup.failed {
        app.emit(
            "console-line",
            format!(
                "  ⚠ Removing superseded clock file {name} failed, {e}. PEA will load it alongside {kept}; delete it from {} by hand.",
                root.display()
            ),
        )
        .ok();
    }
}

/// Try the 05S clock for one day. `Ok(true)` means a 05S file is now in `root`
/// and the usual clock should not be fetched; `Ok(false)` means none could be
/// fetched and the caller falls back to the usual clock.
pub(crate) async fn fetch_fine_clock(
    client: &reqwest::Client,
    root: &Path,
    urls: &[String],
    plan: &FineClock,
    date: &NaiveDate,
    app: &tauri::AppHandle,
    cancel_flag: &Arc<AtomicBool>,
) -> Result<bool, String> {
    let mut have = present_candidate(root, urls);
    if have.is_none() {
        for url in urls {
            if cancel_flag.load(Ordering::SeqCst) {
                return Err("Cancelled".to_string());
            }
            let fname = url_basename(url);
            match download_and_gunzip(client, url, &super::layout::product_path(root, fname), fname, app, cancel_flag).await {
                Ok(()) => {
                    have = Some(fname.to_string());
                    break;
                }
                Err(e) => {
                    // A stop mid-download surfaces as an error; it is not a
                    // missing product, so do not fall back to the 30S file.
                    if cancel_flag.load(Ordering::SeqCst) {
                        return Err("Cancelled".to_string());
                    }
                    app.emit(
                        "console-line",
                        format!("  ✗ {}: {e}", super::extract_host(url)),
                    )
                    .ok();
                }
            }
        }
    }
    let Some(name) = have else {
        return Ok(false);
    };
    keep_single_clock(root, &name, app);
    app.emit(
        "product-line",
        format!(
            "ℹ️ Clock sampling for {date}: {FINE_CLOCK_SAMPLE}. The RINEX interval is {} s, \
             so the 5-second clock product is used.",
            plan.interval_s
        ),
    )
    .ok();
    app.emit("console-line", format!("  Clock file: {name}")).ok();
    Ok(true)
}

/// Record the clock that ended up in use after a 05S attempt came up empty.
pub(crate) fn announce_usual_clock(
    app: &tauri::AppHandle,
    plan: &FineClock,
    date: &NaiveDate,
    name: &str,
) {
    let sample = sample_of(name).unwrap_or_else(|| "standard".to_string());
    app.emit(
        "product-line",
        format!(
            "ℹ️ Clock sampling for {date}: {sample}. The RINEX interval is {} s, but no {FINE_CLOCK_SAMPLE} \
             clock product could be fetched, so the standard clock is used.",
            plan.interval_s
        ),
    )
    .ok();
    app.emit("console-line", format!("  Clock file: {name}")).ok();
}
