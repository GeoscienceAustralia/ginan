//! Split out of rinex.rs to keep each file under 500 lines.


use flate2::read::GzDecoder;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use super::rinex::*;
use super::rinex_time::epoch_to_gps;
use super::rinex_interval::scan_body;

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn parse_rinex(app: tauri::AppHandle, path: String) -> Result<RinexData, String> {
    use tauri::Emitter;

    // The header parse can fall back to scanning the whole file for the last
    // epoch, so run it as blocking work.
    let data = tokio::task::spawn_blocking(move || parse_rinex_path(&path))
        .await
        .map_err(|e| format!("RINEX parse task failed: {e}"))??;

    // Mirror the original GinanUI (Python) rinex_extractor, which dumps the
    // extracted observation code priorities to the Console channel after
    // parsing. These are low-level technical detail, so they go to Console
    // (console-line), not the Workflow channel.
    let oc = &data.observation_codes;
    app.emit("console-line", "=== Extracted Observation Types (Code Priorities) ===".to_string()).ok();
    app.emit("console-line", format!("GPS (G): {:?}", oc.gps)).ok();
    app.emit("console-line", format!("GAL (E): {:?}", oc.gal)).ok();
    app.emit("console-line", format!("GLO (R): {:?}", oc.glo)).ok();
    app.emit("console-line", format!("BDS (C): {:?}", oc.bds)).ok();
    app.emit("console-line", format!("QZS (J): {:?}", oc.qzs)).ok();
    app.emit("console-line", "======================================================".to_string()).ok();

    Ok(data)
}

/// Pure RINEX parse with no UI side effects — open the file (plain or gzip) and
/// read its header. Shared by the `parse_rinex` command and unit tests.
pub(crate) fn parse_rinex_path(path: &str) -> Result<RinexData, String> {
    let p = Path::new(path);

    // Support both plain and gzip-compressed RINEX files
    let ext = p
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("")
        .to_lowercase();

    if ext == "gz" {
        let file = File::open(p).map_err(|e| format!("Cannot open file: {e}"))?;
        let reader = BufReader::new(GzDecoder::new(file));
        parse_header(reader)
    } else {
        let file = File::open(p).map_err(|e| format!("Cannot open file: {e}"))?;
        let reader = BufReader::new(file);
        parse_header(reader)
    }
}

pub(crate) fn parse_header(reader: impl BufRead) -> Result<RinexData, String> {
    let mut rinex_version: Option<f64> = None;
    let mut marker_name: Option<String> = None;
    let mut receiver_type: Option<String> = None;
    let mut antenna_type: Option<String> = None;
    let mut antenna_offset = [0.0f64; 3];
    let mut approx_position: Option<[f64; 3]> = None;
    let mut start_epoch: Option<String> = None;
    let mut end_epoch: Option<String> = None;
    let mut epoch_interval: Option<f64> = None;
    let mut time_system: Option<String> = None;
    let mut constellations: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut observation_codes = ConstellationObsCodes::default();
    let mut current_system: Option<char> = None; // Track current system for continuation lines

    let mut lines = reader.lines();

    for line_result in lines.by_ref() {
        let line = line_result.map_err(|e| format!("Read error: {e}"))?;

        let label = if line.contains("SYS / # / OBS TYPES") {
            "SYS / # / OBS TYPES".to_string()
        } else if line.len() >= 60 {
            line[60..].trim().to_string()
        } else {
            String::new()
        };

        if label.contains("RINEX VERSION / TYPE") && rinex_version.is_none() {
            if let Ok(v) = line.get(0..9).unwrap_or("").trim().parse::<f64>() {
                rinex_version = Some(v);
            }
            continue;
        }

        if label == "END OF HEADER" {
            break;
        }

        match label.as_str() {
            "MARKER NAME" => {
                let raw = line.get(0..60).unwrap_or(&line).trim().to_string();
                // RINEX v2 uses only the first 4 chars as station ID
                marker_name = Some(
                    if rinex_version.map(|v| v < 3.0).unwrap_or(false) && raw.len() >= 4 {
                        raw[..4].trim().to_string()
                    } else {
                        raw
                    },
                );
            }

            "REC # / TYPE / VERS" => {
                let t = line.get(20..40).unwrap_or("").trim().to_string();
                if !t.is_empty() {
                    receiver_type = Some(t);
                }
            }

            "ANT # / TYPE" => {
                let a = line.get(20..40).unwrap_or("").trim().to_string();
                let b = line.get(40..60).unwrap_or("").trim().to_string();
                let full = if b.is_empty() {
                    a
                } else {
                    format!("{a} {b}")
                };
                if !full.trim().is_empty() {
                    antenna_type = Some(full.trim().to_string());
                }
            }

            "ANTENNA: DELTA H/E/N" => {
                // H=antenna height above marker, E=east, N=north (all in metres)
                if let (Some(h), Some(e), Some(n)) = (
                    parse_f64_col(&line, 0, 14),
                    parse_f64_col(&line, 14, 28),
                    parse_f64_col(&line, 28, 42),
                ) {
                    antenna_offset = [h, e, n];
                }
            }

            "APPROX POSITION XYZ" => {
                if let (Some(x), Some(y), Some(z)) = (
                    parse_f64_col(&line, 0, 14),
                    parse_f64_col(&line, 14, 28),
                    parse_f64_col(&line, 28, 42),
                ) {
                    approx_position = Some([x, y, z]);
                }
            }

            "INTERVAL" => {
                if let Ok(v) = line.get(0..10).unwrap_or("").trim().parse::<f64>() {
                    epoch_interval = Some(v);
                }
            }

            "TIME OF FIRST OBS" => {
                start_epoch = parse_epoch_line(&line, rinex_version.unwrap_or(3.0));
                // 5I6,F13.7,5X,A3: the time system sits in columns 49-51.
                time_system = line
                    .get(48..51)
                    .map(|t| t.trim().to_string())
                    .filter(|t| !t.is_empty());
            }

            "TIME OF LAST OBS" => {
                end_epoch = parse_epoch_line(&line, rinex_version.unwrap_or(3.0));
            }

            "SYS / # / OBS TYPES" => {
                let first = line.chars().next().unwrap_or(' ');

                if first == ' ' {
                    // Continuation line: 6 leading spaces, then up to 13 obs codes.
                    // Use split_whitespace on the whole line — no fixed skip needed.
                    if let Some(sys_char) = current_system {
                        let obs_types = parse_obs_codes_from_line(line.as_str());
                        add_observation_codes(&mut observation_codes, sys_char, obs_types);
                    }
                } else if let Some(name) = system_letter_to_name(first) {
                    // New system line: I1, 2X, I3 (count), 1X, then obs codes.
                    // Pass the full line; parse_observation_types skips the 7-char header.
                    constellations.insert(name.to_string());
                    current_system = Some(first);
                    let obs_types = parse_observation_types(line.as_str());
                    add_observation_codes(&mut observation_codes, first, obs_types);
                }
            }

            // RINEX v2 uses a global obs type list with no system letter
            "# / TYPES OF OBSERV" => {
                // For RINEX v2, parse observation types and apply to GPS (most common case)
                let obs_types = parse_observation_types_v2(&line);
                observation_codes.gps = obs_types;
                // Note: RINEX v2 typically only supports GPS, but mixed systems are possible
                // Full v2 support would require scanning the observation records
            }

            _ => {}
        }
    }

    // INTERVAL and TIME OF LAST OBS are both optional. Without INTERVAL the
    // interval comes from the epoch spacing, so a 1 Hz file still gets the
    // 5-second clocks; without TIME OF LAST OBS the whole body is
    // read for the last epoch.
    if epoch_interval.is_none() || end_epoch.is_none() {
        let version = rinex_version.unwrap_or(3.0);
        let scan = scan_body(&mut lines, version, end_epoch.is_none());
        epoch_interval = epoch_interval.or(scan.interval);
        end_epoch = end_epoch.or(scan.last_epoch);
    }

    // For RINEX v2 files that don't have per-system obs-type headers, we fall back to
    // reporting GPS (the most common v2 case) rather than scanning the whole body.
    if constellations.is_empty() && rinex_version.map(|v| v < 3.0).unwrap_or(false) {
        constellations.insert("GPS".to_string());
    }

    let mut constellation_vec: Vec<String> = constellations.into_iter().collect();
    constellation_vec.sort();

    // Every epoch in the file (header and records) shares one time system.
    // A blank one means GPS, except in a GLONASS-only file where it means GLO.
    let effective_system = time_system.clone().unwrap_or_else(|| {
        if constellation_vec == ["GLO"] { "GLO".into() } else { "GPS".into() }
    });
    start_epoch = start_epoch.map(|e| epoch_to_gps(&e, &effective_system));
    end_epoch = end_epoch.map(|e| epoch_to_gps(&e, &effective_system));

    Ok(RinexData {
        rinex_version,
        marker_name,
        receiver_type,
        antenna_type,
        antenna_offset,
        approx_position,
        start_epoch,
        end_epoch,
        epoch_interval,
        time_system,
        constellations: constellation_vec,
        observation_codes,
    })
}

pub(crate) fn parse_f64_col(line: &str, start: usize, end: usize) -> Option<f64> {
    line.get(start..end.min(line.len()))?.trim().parse().ok()
}

pub(crate) fn system_letter_to_name(c: char) -> Option<&'static str> {
    match c {
        'G' => Some("GPS"),
        'E' => Some("GAL"),
        'R' => Some("GLO"),
        'C' => Some("BDS"),
        'J' => Some("QZS"),
        _ => None,
    }
}

/// Extract valid 3-char RINEX observation codes from any part of a line using whitespace splitting.
/// Used for continuation lines where the only content is obs codes preceded by spaces.
/// RINEX obs codes: first char in [CLDSPY IX], second char is digit, third char is alphabetic.
/// Truncate to col 60 to avoid capturing label text ("SYS / # / OBS TYPES") at cols 60-79.
pub(crate) fn parse_obs_codes_from_line(line: &str) -> Vec<String> {
    let safe = if let Some(idx) = line.find("SYS / # / OBS TYPES") {
        &line[..idx]
    } else {
        line
    };
    safe.split_whitespace()
        .filter(|s| {
            if s.len() != 3 { return false; }
            let b = s.as_bytes();
            matches!(b[0], b'C' | b'L' | b'D' | b'S' | b'P' | b'Y' | b'I' | b'X')
                && b[1].is_ascii_digit()
                && b[2].is_ascii_alphabetic()
        })
        .map(|s| s.to_string())
        .collect()
}

/// Parse observation codes from a RINEX v3+ SYS / # / OBS TYPES new-system line.
/// Format: "G  nn obs1 obs2 ..." — skips the 7-char header (sys + 2X + I3 + 1X).
/// Truncates to label position if present.
pub(crate) fn parse_observation_types(line: &str) -> Vec<String> {
    let safe = if let Some(idx) = line.find("SYS / # / OBS TYPES") {
        &line[..idx]
    } else {
        line
    };
    let obs_part = if safe.len() > 7 { &safe[7..] } else { "" };
    obs_part
        .split_whitespace()
        .filter(|s| {
            if s.len() != 3 { return false; }
            let b = s.as_bytes();
            matches!(b[0], b'C' | b'L' | b'D' | b'S' | b'P' | b'Y' | b'I' | b'X')
                && b[1].is_ascii_digit()
                && b[2].is_ascii_alphabetic()
        })
        .map(|s| s.to_string())
        .collect()
}

/// Parse observation codes from a RINEX v2 # / TYPES OF OBSERV line
/// Format: "     n    obs1obs2obs3..." or "     n    obs1    obs2    obs3..."
pub(crate) fn parse_observation_types_v2(line: &str) -> Vec<String> {
    let mut obs_types = Vec::new();

    // Get number of observation types
    if let Ok(count) = line.get(0..6).unwrap_or("").trim().parse::<i32>() {
        let obs_part = if line.len() > 6 { &line[6..] } else { "" };

        // RINEX v2 observation types can be space-separated or concatenated
        // Try space-separated first (more common in practice)
        let codes: Vec<&str> = obs_part.split_whitespace().collect();
        if codes.len() == count as usize {
            for code in codes {
                obs_types.push(code.to_string());
            }
        } else {
            // Fall back to concatenated format
            // Each type is typically 2 characters (e.g., C1, P1, L1, D1, S1)
            let mut pos = 0;
            let chars: Vec<char> = obs_part.chars().collect();

            for _ in 0..count {
                if pos + 1 < chars.len() {
                    let obs_code = format!("{}{}", chars[pos], chars[pos + 1]);
                    obs_types.push(obs_code);
                    pos += 2;

                    // Skip any spaces
                    while pos < chars.len() && chars[pos] == ' ' {
                        pos += 1;
                    }
                }
            }
        }
    }

    obs_types
}

/// Add observation codes to the appropriate constellation list
pub(crate) fn add_observation_codes(obs_codes: &mut ConstellationObsCodes, system: char, codes: Vec<String>) {
    match system {
        'G' => obs_codes.gps.extend(codes),
        'E' => obs_codes.gal.extend(codes),
        'R' => obs_codes.glo.extend(codes),
        'C' => obs_codes.bds.extend(codes),
        'J' => obs_codes.qzs.extend(codes),
        _ => {} // Unknown system, ignore
    }
}

/// Parse a RINEX epoch line into `"YYYY-MM-DD HH:MM:SS"` format (the format PEA expects).
pub(crate) fn parse_epoch_line(line: &str, version: f64) -> Option<String> {
    if version < 3.0 {
        // v2: whitespace-separated values  YY MM DD HH MM SS.sss TIME_SYS
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 6 {
            return None;
        }
        let y_raw: i32 = parts[0].parse().ok()?;
        // RINEX v2 2-digit year convention: <80 → 2000+, ≥80 → 1900+
        let y = if y_raw >= 1000 {
            y_raw
        } else if y_raw < 80 {
            2000 + y_raw
        } else {
            1900 + y_raw
        };
        let m: i32 = parts[1].parse().ok()?;
        let d: i32 = parts[2].parse().ok()?;
        let h: i32 = parts[3].parse().ok()?;
        let min: i32 = parts[4].parse().ok()?;
        let s: f64 = parts[5].parse().ok()?;
        Some(format!("{y:04}-{m:02}-{d:02} {h:02}:{min:02}:{:02}", s as i32))
    } else {
        // v3/v4: fixed 6-char fields
        let y: i32 = line.get(0..6)?.trim().parse().ok()?;
        let m: i32 = line.get(6..12)?.trim().parse().ok()?;
        let d: i32 = line.get(12..18)?.trim().parse().ok()?;
        let h: i32 = line.get(18..24)?.trim().parse().ok()?;
        let min: i32 = line.get(24..30)?.trim().parse().ok()?;
        let s: f64 = line.get(30..43)?.trim().parse().ok()?;
        Some(format!("{y:04}-{m:02}-{d:02} {h:02}:{min:02}:{:02}", s as i32))
    }
}

/// Scan observation data records to find the last epoch timestamp.
#[cfg(test)]
pub(crate) fn scan_last_epoch(
    lines: &mut std::io::Lines<impl BufRead>,
    version: f64,
) -> Option<String> {
    scan_body(lines, version, true).last_epoch
}

/// Parse a RINEX v3 data epoch line (after the '>' marker).
pub(crate) fn parse_data_epoch_v3(line: &str) -> Option<String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 7 {
        return None;
    }
    let y: i32 = parts[0].parse().ok()?;
    let m: i32 = parts[1].parse().ok()?;
    let d: i32 = parts[2].parse().ok()?;
    let h: i32 = parts[3].parse().ok()?;
    let min: i32 = parts[4].parse().ok()?;
    let s: f64 = parts[5].parse().ok()?;
    let flag: i32 = parts[6].parse().ok()?;

    if flag > 1 || !(1..=12).contains(&m) || !(1..=31).contains(&d) || h > 23 || min > 59 {
        return None;
    }

    Some(format!("{y:04}-{m:02}-{d:02} {h:02}:{min:02}:{:02}", s as i32))
}

/// Parse a RINEX v2 data epoch record line.
pub(crate) fn parse_data_epoch_v2(line: &str) -> Option<String> {
    if !line.starts_with(' ') {
        return None;
    }

    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 8 {
        return None;
    }

    let y_raw: i32 = parts[0].parse().ok()?;
    let m: i32 = parts[1].parse().ok()?;
    let d: i32 = parts[2].parse().ok()?;
    let h: i32 = parts[3].parse().ok()?;
    let min: i32 = parts[4].parse().ok()?;
    let s: f64 = parts[5].parse().ok()?;
    let flag: i32 = parts[6].parse().ok()?;

    if flag > 1
        || !(1..=12).contains(&m)
        || !(1..=31).contains(&d)
        || h > 23
        || min > 59
        || s >= 60.0
    {
        return None;
    }

    let y = if y_raw >= 1000 {
        y_raw
    } else if y_raw < 80 {
        2000 + y_raw
    } else {
        1900 + y_raw
    };

    Some(format!("{y:04}-{m:02}-{d:02} {h:02}:{min:02}:{:02}", s as i32))
}
