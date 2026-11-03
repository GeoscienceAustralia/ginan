use super::download::mirror_order;
use super::naming::*;
use super::http::*;
use chrono::{Datelike, NaiveDate};
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use tauri::Emitter;
use super::*;

// ─── BIA file fetching and parsing ──────────────────────────────────────────

/// Scan the products directory for downloaded BIA files and return the observation codes they contain.
/// Falls back to a curated set of common IGS OSB codes if no file is found.
///
/// NOTE: For pre-Process validation of which observation codes a (provider, series, project)
/// combo supports, prefer [`fetch_bia_codes_for_combo`] — it fetches a sample BIA on demand
/// and caches the parsed code-set. This function is the post-download disk reader.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn fetch_bia_codes(
    provider: String,
    series: String,
    project: String,
    products_dir: String,
) -> Result<BiaCodePriorities, String> {
    // Directory scan + file read/parse is blocking IO.
    tokio::task::spawn_blocking(move || {
        read_bia_codes_from_disk(&provider, &series, &project, &products_dir)
    })
    .await
    .map_err(|e| format!("BIA scan task failed: {e}"))?
}

pub(crate) fn read_bia_codes_from_disk(
    provider: &str,
    series: &str,
    project: &str,
    products_dir: &str,
) -> Result<BiaCodePriorities, String> {
    let root = PathBuf::from(products_dir);

    // Match the long-name prefix the dynamic-product downloader uses, e.g. "IGS0OPSFIN".
    let prefix = format!("{}0{}{}", provider.trim(), project.trim(), series.trim());

    if !root.is_dir() {
        return Err(format!("Cannot read products dir: {} does not exist", root.display()));
    }
    let bia_candidates: Vec<PathBuf> = super::layout::dated_product_files(&root)
        .into_iter()
        .filter(|p| {
            p.is_file()
                && p.extension()
                    .and_then(|s| s.to_str())
                    .map(|s| matches!(s.to_uppercase().as_str(), "BIA" | "BSX"))
                    .unwrap_or(false)
        })
        .collect();

    // Prefer files matching the requested provider/project/series prefix; fall back to any.
    let bia_file = bia_candidates
        .iter()
        .filter(|p| {
            p.file_name()
                .and_then(|n| n.to_str())
                .map(|n| n.starts_with(&prefix))
                .unwrap_or(false)
        })
        .max_by_key(|p| {
            p.metadata()
                .and_then(|m| m.modified())
                .unwrap_or(std::time::SystemTime::UNIX_EPOCH)
        })
        .cloned()
        .or_else(|| {
            bia_candidates
                .into_iter()
                .max_by_key(|p| {
                    p.metadata()
                        .and_then(|m| m.modified())
                        .unwrap_or(std::time::SystemTime::UNIX_EPOCH)
                })
        });

    if let Some(path) = bia_file {
        // Lossy decode, as the on-demand fetch does: CODE's files carry a latin-1 byte in
        // the reference text, and a strict UTF-8 read would reject the whole file.
        let bytes = std::fs::read(&path)
            .map_err(|e| format!("Cannot read {}: {e}", path.display()))?;
        return Ok(parse_bia_content(&String::from_utf8_lossy(&bytes)));
    }

    // No BIA file found — return empty arrays. The frontend treats this as
    // "no constellations validated yet" and shows no codes until the user
    // downloads a BIA (via fetch_bia_codes_for_combo or the full Process flow).
    Ok(BiaCodePriorities::default())
}

/// Parse a SINEX BIAS format file and return the observation codes present per constellation.
///
/// SINEX BIAS 1.00 data lines (in the +BIAS/SOLUTION block) use fixed-width fields
/// (0-based columns):
///   col 0:     record indicator (space for data)
///   col 1-3:   BIAS type (OSB, DSB, ISB, …)
///   col 4-5:   two spaces
///   col 6-9:   SVN (4 chars, e.g. "G001"; just the system letter for station biases)
///   col 10:    space
///   col 11-13: PRN (3 chars, e.g. "G01"; just the system letter for station biases)
///   col 14:    space
///   col 15-23: STATION (9 chars; blank for satellite biases)
///   col 24:    space
///   col 25-28: OBS1 (4-char field holding a 3-char code, e.g. "L1C ")
///   col 29:    space
///   col 30-33: OBS2 (same shape; blank for OSB, the second signal of a DSB/ISB pair)
///
/// Some files omit PRN and use SVN only, shifting OBS1 to col 24-26. The parser
/// probes both positions and takes whichever looks like a valid obs code; OBS2 then
/// sits five columns after OBS1 in either layout.
pub(crate) fn parse_bia_content(content: &str) -> BiaCodePriorities {
    use std::collections::HashSet;

    let mut gps: HashSet<String> = HashSet::new();
    let mut gal: HashSet<String> = HashSet::new();
    let mut glo: HashSet<String> = HashSet::new();
    let mut bds: HashSet<String> = HashSet::new();
    let mut qzs: HashSet<String> = HashSet::new();

    let mut in_bias = false;

    for line in content.lines() {
        if line.starts_with("+BIAS/SOLUTION") {
            in_bias = true;
            continue;
        }
        if line.starts_with("-BIAS/SOLUTION") {
            in_bias = false;
            continue;
        }
        if !in_bias || !line.starts_with(' ') || line.starts_with("* ") {
            continue;
        }
        // Minimum length for a record with OBS1
        if line.len() < 28 {
            continue;
        }

        // BIAS type at col 1-3
        let bias_type = line.get(1..4).map(str::trim).unwrap_or("");
        if !matches!(bias_type, "OSB" | "DSB" | "ISB" | "GIM" | "PT") {
            continue;
        }

        // Determine constellation from PRN (col 11) or SVN (col 6) first char
        let sys = {
            let prn_c = line.chars().nth(11).unwrap_or(' ');
            let svn_c = line.chars().nth(6).unwrap_or(' ');
            if "GERCJ".contains(prn_c) {
                prn_c
            } else if "GERCJ".contains(svn_c) {
                svn_c
            } else {
                continue;
            }
        };

        // OBS1: probe col 25-27 (standard OSB/PRN-present layout) then col 24-26 (SVN-only)
        let obs1_start = if line.len() > 27 && is_valid_obs_code(line.get(25..28).unwrap_or("")) {
            25
        } else if line.len() > 26 && is_valid_obs_code(line.get(24..27).unwrap_or("")) {
            24
        } else {
            continue;
        };
        let obs1 = &line[obs1_start..obs1_start + 3];
        add_obs_code(sys, obs1, &mut gps, &mut gal, &mut glo, &mut bds, &mut qzs);

        // OBS2: five columns after OBS1 (4-char field + 1 space), i.e. col 30-32 in the
        // standard layout. Blank for OSB. DSB/ISB records carry the second signal of the
        // pair here, and for those it is the only place the L2/L5 codes appear — reading
        // it one column early (col 29-31) would cut the code to two characters and drop it.
        if let Some(obs2) = line.get(obs1_start + 5..obs1_start + 8) {
            let obs2 = obs2.trim();
            if is_valid_obs_code(obs2) {
                add_obs_code(sys, obs2, &mut gps, &mut gal, &mut glo, &mut bds, &mut qzs);
            }
        }
    }

    // Sort for deterministic output
    let mut result = BiaCodePriorities {
        gps: gps.into_iter().collect(),
        gal: gal.into_iter().collect(),
        glo: glo.into_iter().collect(),
        bds: bds.into_iter().collect(),
        qzs: qzs.into_iter().collect(),
    };
    result.gps.sort();
    result.gal.sort();
    result.glo.sort();
    result.bds.sort();
    result.qzs.sort();
    result
}

/// True if `s` looks like a RINEX observation code (e.g. "C1C", "L5Q").
pub(crate) fn is_valid_obs_code(s: &str) -> bool {
    let b = s.as_bytes();
    b.len() == 3
        && matches!(b[0], b'C' | b'L' | b'D' | b'S' | b'P' | b'Y' | b'I' | b'X')
        && b[1].is_ascii_digit()
        && b[2].is_ascii_alphabetic()
}

pub(crate) fn add_obs_code(
    sys: char,
    code: &str,
    gps: &mut std::collections::HashSet<String>,
    gal: &mut std::collections::HashSet<String>,
    glo: &mut std::collections::HashSet<String>,
    bds: &mut std::collections::HashSet<String>,
    qzs: &mut std::collections::HashSet<String>,
) {
    let owned = code.to_string();
    match sys {
        'G' => { gps.insert(owned); }
        'E' => { gal.insert(owned); }
        'R' => { glo.insert(owned); }
        'C' => { bds.insert(owned); }
        'J' => { qzs.insert(owned); }
        _ => {}
    }
}

// ─── on-demand BIA code-set fetch with disk cache ──────────────────────────

/// Cache TTL for parsed BIA code-set JSON files. The set of observation codes
/// a provider supports is stable across a series, so we don't need to refetch
/// often — 30 days mirrors the cadence at which providers add new codes.
const BIA_CODE_CACHE_TTL_SECS: u64 = 30 * 24 * 60 * 60;

/// Build the cache filename for a `(provider, series, project)` combo.
/// Sanitises each component to uppercase alphanumerics so the result is
/// always a safe filesystem path. e.g. `("igs", "rap", "ops")` → `IGS_RAP_OPS.json`.
pub(crate) fn bia_cache_filename(provider: &str, series: &str, project: &str) -> String {
    fn sanitise(s: &str) -> String {
        s.chars()
            .filter(|c| c.is_ascii_alphanumeric())
            .flat_map(|c| c.to_uppercase())
            .collect()
    }
    format!(
        "{}_{}_{}.json",
        sanitise(provider),
        sanitise(series),
        sanitise(project),
    )
}

/// Read a cached BIA code-set from disk if it exists and is fresh.
/// Returns `Ok(Some(...))` on a fresh hit, `Ok(None)` on miss/stale, `Err` on
/// I/O or parse failure of an existing cache file.
pub(crate) fn read_bia_cache(
    cache_dir: &Path,
    provider: &str,
    series: &str,
    project: &str,
) -> Result<Option<BiaCodePriorities>, String> {
    let path = cache_dir.join(bia_cache_filename(provider, series, project));
    if !path.is_file() {
        return Ok(None);
    }
    let meta = std::fs::metadata(&path)
        .map_err(|e| format!("Cannot stat cache {}: {e}", path.display()))?;
    let age = meta
        .modified()
        .ok()
        .and_then(|t| t.elapsed().ok())
        .map(|d| d.as_secs())
        .unwrap_or(u64::MAX);
    if age > BIA_CODE_CACHE_TTL_SECS {
        return Ok(None);
    }
    let content = std::fs::read_to_string(&path)
        .map_err(|e| format!("Cannot read cache {}: {e}", path.display()))?;
    let parsed: BiaCodePriorities = serde_json::from_str(&content)
        .map_err(|e| format!("Cannot parse cache {}: {e}", path.display()))?;
    Ok(Some(parsed))
}

/// Write a parsed BIA code-set to the cache directory as pretty JSON.
pub(crate) fn write_bia_cache(
    cache_dir: &Path,
    provider: &str,
    series: &str,
    project: &str,
    codes: &BiaCodePriorities,
) -> Result<(), String> {
    std::fs::create_dir_all(cache_dir)
        .map_err(|e| format!("Cannot create cache dir {}: {e}", cache_dir.display()))?;
    let path = cache_dir.join(bia_cache_filename(provider, series, project));
    let json = serde_json::to_string_pretty(codes)
        .map_err(|e| format!("Serialise cache: {e}"))?;
    std::fs::write(&path, json)
        .map_err(|e| format!("Cannot write cache {}: {e}", path.display()))?;
    Ok(())
}

/// Convert a GPS week number to a deterministic mid-week date (Wednesday).
/// The exact date matters little since BIA code-sets are stable across a series.
pub(crate) fn mid_week_date(gps_week: u32) -> NaiveDate {
    let epoch = NaiveDate::from_ymd_opt(1980, 1, 6).unwrap();
    epoch + chrono::Duration::days((gps_week as i64) * 7 + 3)
}

/// Fetch a sample BIA for `(provider, series, project)`, parse it, and return the
/// supported observation codes per constellation. Network is only touched on a
/// cache miss; subsequent calls for the same combo hit the on-disk cache and
/// return instantly. Used by the frontend to validate which codes a provider
/// supports BEFORE the user clicks Process.
///
/// Cache layout: `<products_dir>/.bia_code_cache/<PROV>_<SER>_<PROJ>.json`,
/// pretty JSON of [`BiaCodePriorities`], TTL 30 days.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn fetch_bia_codes_for_combo(
    provider: String,
    series: String,
    project: String,
    gps_week: u32,
    products_dir: String,
    app: tauri::AppHandle,
) -> Result<BiaCodePriorities, String> {
    let root = PathBuf::from(&products_dir);
    let cache_dir = root.join(".bia_code_cache");

    // Cache hit → return immediately, no network.
    if let Some(cached) = read_bia_cache(&cache_dir, &provider, &series, &project)? {
        return Ok(cached);
    }

    // Cache miss — perform network fetch.
    app.emit(
        "console-line",
        format!("Fetching sample BIA for {provider}/{series}/{project}..."),
    )
    .ok();

    std::fs::create_dir_all(&cache_dir)
        .map_err(|e| format!("Cannot create cache dir {}: {e}", cache_dir.display()))?;

    let date = mid_week_date(gps_week);
    let year = date.year();
    let doy = date.ordinal();
    let filename = long_name(
        &provider, &project, &series, year, doy, "01D", "01D", "OSB", "BIA",
    );

    // Intersect the mirror priority order with the set this provider is hosted on.
    let allowed = provider_mirrors(&provider);
    let mirrors: Vec<&'static str> = mirror_order("CDDIS")
        .iter()
        .copied()
        .filter(|base| allowed.iter().any(|name| *name == mirror_name(base)))
        .collect();

    if mirrors.is_empty() {
        return Err(format!(
            "No mirrors available for provider {provider} (allowed: {allowed:?})"
        ));
    }

    let client = build_client()?;
    let tmp_path = cache_dir.join(format!(
        ".tmp_{}_{}_{}.bia",
        provider.trim(),
        series.trim(),
        project.trim(),
    ));
    // A dummy cancel flag — pre-Process validation is short and not user-cancellable yet.
    let cancel_flag: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));

    let mut last_err = String::from("no mirrors attempted");
    for mirror in &mirrors {
        let url = product_url(mirror, gps_week, &filename);
        match download_and_gunzip(&client, &url, &tmp_path, &filename, &app, &cancel_flag).await {
            Ok(()) => {
                // SINEX BIAS files are column-formatted ASCII for the fields the parser cares
                // about, but author/metadata text sometimes contains non-UTF-8 bytes (latin-1
                // names, etc.). Read raw bytes and lossy-decode so the strict UTF-8 check in
                // read_to_string doesn't reject an otherwise-valid file.
                let bytes = match std::fs::read(&tmp_path) {
                    Ok(b) => b,
                    Err(e) => {
                        last_err = format!("read {}: {e}", tmp_path.display());
                        let _ = std::fs::remove_file(&tmp_path);
                        continue;
                    }
                };
                let content = String::from_utf8_lossy(&bytes).into_owned();
                let parsed = parse_bia_content(&content);
                let _ = std::fs::remove_file(&tmp_path);
                write_bia_cache(&cache_dir, &provider, &series, &project, &parsed)?;
                return Ok(parsed);
            }
            Err(e) => {
                last_err = e;
            }
        }
    }

    let _ = std::fs::remove_file(&tmp_path);
    Err(format!(
        "Could not fetch sample BIA for {provider}/{series}/{project}: {last_err}"
    ))
}
