//! CRINEX (Hatanaka-compressed RINEX) decompression.
//!
//! A port of the decoding half of RNXCMP's `crx2rnx.c` (Y. Hatanaka, GSI Japan),
//! covering CRINEX 1.0 (RINEX 2) and CRINEX 3.0 (RINEX 3).
//!
//! The reference implementation splits every value across two `long`s — upper
//! digits and lower 5 — solely to avoid 32-bit overflow. `i64` holds the whole
//! scaled integer, so that split is dropped here; the arithmetic is otherwise
//! the same, which is why the difference bookkeeping below looks simpler than
//! the C it came from.
//!
//! Values are carried as integers scaled by 1000 (observations, 3 decimals) or
//! by 1e12 / 1e9 (receiver clock offset, RINEX 3 / RINEX 2).

/// Maximum difference order RNXCMP permits.
const MAX_DIFF_ORDER: usize = 5;

/// One field's difference state. `arc_order < 0` marks a blank field.
#[derive(Clone)]
pub(crate) struct Diff {
    arc_order: i32,
    order: i32,
    vals: [i64; MAX_DIFF_ORDER + 2],
}

impl Default for Diff {
    fn default() -> Self {
        Diff { arc_order: -1, order: -1, vals: [0; MAX_DIFF_ORDER + 2] }
    }
}

/// Apply a CRINEX text difference `ds` onto `s` in place.
///
/// Space keeps the existing character, `&` blanks it, anything else replaces
/// it. A difference longer than the target extends it (with `&` still meaning
/// blank). Mirrors `repair()` in crx2rnx.c.
pub(crate) fn repair(s: &mut Vec<u8>, ds: &[u8]) {
    for (i, &d) in ds.iter().enumerate() {
        if i < s.len() {
            match d {
                b' ' => {}
                b'&' => s[i] = b' ',
                c => s[i] = c,
            }
        } else {
            s.push(if d == b'&' { b' ' } else { d });
        }
    }
}

pub(crate) fn trim_end_in_place(s: &mut Vec<u8>) {
    while s.last() == Some(&b' ') {
        s.pop();
    }
}

/// Right-aligned 14-character observation field, 3 decimals.
///
/// RINEX omits the leading zero for magnitudes below 1 (`.123`, `-.123`), which
/// is what the digit-shuffling at the end of `putfield()` achieves in C.
pub(crate) fn format_obs(scaled: i64) -> String {
    let int_part = scaled / 1000;
    let frac = (scaled % 1000).abs();
    let body = if int_part == 0 {
        let sign = if scaled < 0 { "-" } else { "" };
        format!("{sign}.{frac:03}")
    } else {
        format!("{int_part}.{frac:03}")
    };
    format!("{body:>14}")
}

/// Receiver clock offset field: F15.12 for RINEX 3, F12.9 for RINEX 2.
pub(crate) fn format_clock(scaled: i64, decimals: usize, width: usize) -> String {
    let divisor = 10i64.pow(decimals as u32);
    let int_part = scaled / divisor;
    let frac = (scaled % divisor).abs();
    let sign = if scaled < 0 && int_part == 0 { "-" } else { "" };
    let body = format!("{sign}{int_part}.{frac:0width$}", width = decimals);
    format!("{body:>width$}")
}

/// Split a satellite data line into `ntype` value fields plus the trailing
/// flag string. Fields are single-space separated and may be empty.
///
/// Returns `(fields, flags)`. Mirrors the `'\0'`-stuffing loop in `getdiff()`.
pub(crate) fn split_data_line(line: &[u8], ntype: usize) -> (Vec<&[u8]>, &[u8]) {
    let mut fields = Vec::with_capacity(ntype);
    let mut start = 0usize;
    let mut i = 0usize;
    while fields.len() < ntype {
        if i >= line.len() {
            // End of line. Whatever is left is the *current* field — a line
            // with no trailing flag string ends immediately after its last
            // value, with no separator to close it. Only the fields after that
            // are blank.
            fields.push(&line[start.min(line.len())..]);
            start = line.len();
            continue;
        }
        if line[i] == b' ' {
            fields.push(&line[start..i]);
            start = i + 1;
        }
        i += 1;
    }
    (fields, if start <= line.len() { &line[start..] } else { &[] })
}

/// Parse one difference field: either `N&value` (arc initialisation at order N)
/// or a bare integer continuing the existing arc.
pub(crate) fn parse_field(field: &[u8], prev: Option<&Diff>) -> Result<Diff, String> {
    let mut d = Diff::default();
    let text = std::str::from_utf8(field).map_err(|_| "non-UTF8 in data field".to_string())?;

    let num_str = if text.len() >= 2 && text.as_bytes()[1] == b'&' {
        d.order = -1;
        d.arc_order = text[..1]
            .parse::<i32>()
            .map_err(|_| format!("bad arc order in field '{text}'"))?;
        if d.arc_order as usize > MAX_DIFF_ORDER {
            return Err(format!("arc order {} exceeds maximum", d.arc_order));
        }
        &text[2..]
    } else {
        let p = prev.ok_or_else(|| {
            "new satellite or data sequence without arc initialisation".to_string()
        })?;
        if p.arc_order < 0 {
            return Err("new data sequence without arc initialisation".to_string());
        }
        d.order = p.order;
        d.arc_order = p.arc_order;
        text
    };

    d.vals[0] = num_str
        .parse::<i64>()
        .map_err(|_| format!("bad numeric field '{num_str}'"))?;
    Ok(d)
}

/// Integrate one field against the previous epoch, returning the recovered
/// value. Mirrors the difference recursion in `data()`.
pub(crate) fn integrate(cur: &mut Diff, prev: Option<&Diff>) -> i64 {
    if cur.order < cur.arc_order {
        cur.order += 1;
        for k in 0..cur.order as usize {
            // Only reachable once the arc has history, so prev is present.
            let p = prev.expect("difference arc without previous epoch");
            cur.vals[k + 1] = cur.vals[k] + p.vals[k];
        }
    } else {
        for k in 0..cur.order as usize {
            let p = prev.expect("difference arc without previous epoch");
            cur.vals[k + 1] = cur.vals[k] + p.vals[k + 1];
        }
    }
    cur.vals[cur.order.max(0) as usize]
}

pub(crate) struct Header {
    /// Text to emit verbatim (original RINEX header, CRINEX lines stripped).
    out: String,
    rinex_version: u32,
    crinex_version: u32,
    /// RINEX 2: a single observation count. RINEX 3: count per system letter.
    ntype_rinex2: usize,
    ntype_gnss: [i32; 256],
}

/// Consume the CRINEX + RINEX header, returning it and the index of the first
/// data line.
pub(crate) fn parse_header(lines: &[&str]) -> Result<(Header, usize), String> {
    let mut h = Header {
        out: String::new(),
        rinex_version: 0,
        crinex_version: 0,
        ntype_rinex2: 0,
        ntype_gnss: [-1; 256],
    };

    // The two CRINEX lines come first and are not part of the RINEX output.
    let first = lines.first().ok_or_else(|| "empty input".to_string())?;
    if !first.contains("CRINEX VERS") {
        return Err("not a CRINEX file: missing 'CRINEX VERS / TYPE' line".to_string());
    }
    h.crinex_version = if first.trim_start().starts_with('3') { 3 } else { 1 };
    let mut i = if lines.get(1).is_some_and(|l| l.contains("CRINEX PROG")) { 2 } else { 1 };

    let mut last_sys: Option<u8> = None;
    while i < lines.len() {
        let line = lines[i];
        i += 1;
        h.out.push_str(line);
        h.out.push('\n');

        if line.contains("RINEX VERSION / TYPE") {
            let v: f64 = line.get(0..9).unwrap_or("").trim().parse().unwrap_or(0.0);
            h.rinex_version = if v >= 3.0 { 3 } else { 2 };
        } else if line.contains("# / TYPES OF OBSERV") {
            // RINEX 2, possibly continued over several lines.
            if let Some(n) = line.get(0..6).and_then(|s| s.trim().parse::<usize>().ok()) {
                h.ntype_rinex2 = n;
            }
        } else if line.contains("SYS / # / OBS TYPES") {
            // RINEX 3. A blank system letter marks a continuation line.
            let bytes = line.as_bytes();
            let sys = bytes.first().copied().unwrap_or(b' ');
            if sys != b' ' {
                let n = line
                    .get(3..6)
                    .and_then(|s| s.trim().parse::<i32>().ok())
                    .ok_or_else(|| format!("bad obs type count: {line}"))?;
                h.ntype_gnss[sys as usize] = n;
                last_sys = Some(sys);
            } else if last_sys.is_none() {
                return Err("continuation of SYS / # / OBS TYPES before any system".to_string());
            }
        } else if line.contains("END OF HEADER") {
            return Ok((h, i));
        }
    }
    Err("no END OF HEADER found".to_string())
}

/// Decompress a CRINEX document into plain RINEX (test convenience wrapper).
#[cfg(test)]
pub(crate) fn crx_to_rnx(input: &str) -> Result<String, String> {
    crx_to_rnx_with_progress(input, &mut |_, _| {})
}

/// Decompress a CRINEX document into plain RINEX, reporting
/// `(lines_done, lines_total)` as it goes.
///
/// Called once per epoch so a caller can drive a progress indicator — a full
/// day of 1 Hz observations is a large file and the conversion is not instant.
pub fn crx_to_rnx_with_progress(
    input: &str,
    progress: &mut dyn FnMut(usize, usize),
) -> Result<String, String> {
    let lines: Vec<&str> = input.lines().map(|l| l.trim_end_matches('\r')).collect();
    let total_lines = lines.len();
    let (header, mut idx) = parse_header(&lines)?;
    let mut out = header.out;

    let rinex3 = header.rinex_version >= 3;
    let ep_top_from = if rinex3 { b'>' } else { b'&' };
    let ep_top_to = if rinex3 { b'>' } else { b' ' };
    let nsat_at = if rinex3 { 32 } else { 29 };
    let satlst_at = if rinex3 { 41 } else { 32 };
    // Clock offset resolution and field width differ by RINEX version.
    let (clk_decimals, clk_width) = if rinex3 { (12usize, 15usize) } else { (9, 12) };

    // Epoch line carried across epochs for text differencing.
    let mut epoch_line: Vec<u8> = Vec::new();
    let mut sat_lst_old: Vec<String> = Vec::new();
    // Per-satellite difference state and flags, indexed as of the previous epoch.
    let mut dy0: Vec<Vec<Diff>> = Vec::new();
    let mut flag_prev: Vec<Vec<u8>> = Vec::new();

    let mut clk_prev: Diff = Diff::default();
    let mut clk_arc_order: i32 = -1;
    let mut clk_order: i32 = -1;

    while idx < lines.len() {
        let mut dline = lines[idx].as_bytes().to_vec();
        idx += 1;

        // CRINEX 3 escape lines sit before the epoch record.
        if header.crinex_version == 3 {
            while dline.first() == Some(&b'&') {
                if idx >= lines.len() {
                    return Ok(out);
                }
                dline = lines[idx].as_bytes().to_vec();
                idx += 1;
            }
        }
        if dline.first() == Some(&0x1a) {
            break; // DOS EOF
        }

        if dline.first() == Some(&ep_top_from) {
            dline[0] = ep_top_to;
            // A new epoch record resets the epoch arc and every satellite arc.
            epoch_line.clear();
            sat_lst_old.clear();
            dy0.clear();
            flag_prev.clear();
        }

        repair(&mut epoch_line, &dline);
        trim_end_in_place(&mut epoch_line);

        let epoch_str = String::from_utf8_lossy(&epoch_line).to_string();
        let nsat: usize = epoch_str
            .get(nsat_at..nsat_at + 3)
            .and_then(|s| s.trim().parse().ok())
            .ok_or_else(|| format!("could not read satellite count from '{epoch_str}'"))?;

        // Satellite list: 3 chars each, from a fixed column.
        let mut sat_lst: Vec<String> = Vec::with_capacity(nsat);
        for k in 0..nsat {
            let a = satlst_at + k * 3;
            let s = epoch_str.get(a..a + 3).unwrap_or("   ");
            sat_lst.push(s.to_string());
        }

        // Map each satellite to its index at the previous epoch (None = new).
        let sattbl: Vec<Option<usize>> = sat_lst
            .iter()
            .map(|s| sat_lst_old.iter().position(|o| o == s))
            .collect();

        // Observation counts per satellite.
        let ntypes: Vec<usize> = if rinex3 {
            sat_lst
                .iter()
                .map(|s| {
                    let sys = s.as_bytes().first().copied().unwrap_or(b' ');
                    let n = header.ntype_gnss[sys as usize];
                    if n < 0 {
                        Err(format!("no obs types declared for system '{}'", sys as char))
                    } else {
                        Ok(n as usize)
                    }
                })
                .collect::<Result<_, _>>()?
        } else {
            vec![header.ntype_rinex2; nsat]
        };

        // Clock offset line.
        if idx >= lines.len() {
            return Err("truncated file: expected clock offset line".to_string());
        }
        let clk_line = lines[idx].as_bytes();
        idx += 1;
        if clk_line.is_empty() {
            clk_order = -1;
        } else {
            let text = String::from_utf8_lossy(clk_line).to_string();
            let num = if text.len() >= 2 && text.as_bytes()[1] == b'&' {
                clk_arc_order = text[..1].parse::<i32>().map_err(|_| "bad clock arc order")?;
                clk_order = -1;
                &text[2..]
            } else {
                &text[..]
            };
            let mut clk =
                Diff { arc_order: clk_arc_order, order: clk_order, vals: [0; MAX_DIFF_ORDER + 2] };
            clk.vals[0] = num
                .trim()
                .parse::<i64>()
                .map_err(|_| format!("bad clock offset '{num}'"))?;
            let value = integrate(&mut clk, Some(&clk_prev));
            clk_order = clk.order;
            clk_prev = clk.clone();

            // Emit epoch line with the clock offset appended.
            let head: String = epoch_str.chars().take(41).collect();
            out.push_str(&format!("{head:<41}"));
            out.push_str(&format_clock(value, clk_decimals, clk_width));
            out.push('\n');
        }

        if clk_order < 0 {
            if rinex3 {
                let mut head: String = epoch_str.chars().take(41).collect();
                while head.ends_with(' ') {
                    head.pop();
                }
                out.push_str(&head);
                out.push('\n');
            } else {
                // RINEX 2 keeps up to 12 satellites on the epoch line and wraps
                // the rest onto continuation lines indented by 32 columns.
                let mut head: String = epoch_str.chars().take(68).collect();
                while head.ends_with(' ') {
                    head.pop();
                }
                out.push_str(&head);
                out.push('\n');
                let chars: Vec<char> = epoch_str.chars().collect();
                let mut p = 68usize;
                let mut remaining = nsat as i64 - 12;
                while remaining > 0 {
                    let seg: String = chars.iter().skip(p).take(36).collect();
                    let seg = seg.trim_end();
                    out.push_str(&format!("{:32}{}\n", "", seg));
                    p += 36;
                    remaining -= 12;
                }
            }
        }

        // Satellite data lines.
        let mut dy1: Vec<Vec<Diff>> = Vec::with_capacity(nsat);
        let mut flag_cur: Vec<Vec<u8>> = Vec::with_capacity(nsat);

        for i in 0..nsat {
            if idx >= lines.len() {
                return Err("truncated file: expected satellite data line".to_string());
            }
            let data_line = lines[idx].as_bytes();
            idx += 1;
            let ntype = ntypes[i];
            let (fields, dflag) = split_data_line(data_line, ntype);

            let prev_idx = sattbl[i];

            // Flags carry over from the same satellite's previous epoch.
            let mut flags: Vec<u8> = match prev_idx {
                Some(p) if p < flag_prev.len() => {
                    let mut f = flag_prev[p].clone();
                    f.truncate(ntype * 2);
                    f
                }
                // New satellite: RINEX 2 assumes blanks, RINEX 3 starts empty.
                _ if rinex3 => Vec::new(),
                _ => {
                    let mut f = dflag.to_vec();
                    f.resize(ntype * 2, b' ');
                    f
                }
            };
            repair(&mut flags, dflag);
            flags.resize(ntype * 2, b' ');

            let mut row: Vec<Diff> = Vec::with_capacity(ntype);
            let mut line_out = String::new();
            if rinex3 {
                line_out.push_str(&sat_lst[i]);
            }

            for j in 0..ntype {
                let prev: Option<&Diff> = prev_idx
                    .and_then(|p| dy0.get(p))
                    .and_then(|r| r.get(j));

                if fields[j].is_empty() {
                    // Blank observation.
                    if header.crinex_version == 1 {
                        line_out.push_str(&" ".repeat(16));
                        flags[j * 2] = b' ';
                        flags[j * 2 + 1] = b' ';
                    } else {
                        line_out.push_str(&" ".repeat(14));
                        line_out.push(flags[j * 2] as char);
                        line_out.push(flags[j * 2 + 1] as char);
                    }
                    row.push(Diff::default());
                } else {
                    let mut d = parse_field(fields[j], prev).map_err(|e| {
                        format!(
                            "{e} (satellite {}, observation {} of {}, input line {})",
                            sat_lst[i].trim(),
                            j + 1,
                            ntype,
                            idx
                        )
                    })?;
                    let value = integrate(&mut d, prev);
                    line_out.push_str(&format_obs(value));
                    line_out.push(flags[j * 2] as char);
                    line_out.push(flags[j * 2 + 1] as char);
                    row.push(d);
                }

                // RINEX 2 wraps every 5 observations.
                let wrap = j + 1 == ntype || (!rinex3 && (j + 1) % 5 == 0);
                if wrap {
                    while line_out.ends_with(' ') {
                        line_out.pop();
                    }
                    line_out.push('\n');
                    out.push_str(&line_out);
                    line_out = String::new();
                }
            }

            dy1.push(row);
            flag_cur.push(flags);
        }

        sat_lst_old = sat_lst;
        dy0 = dy1;
        flag_prev = flag_cur;

        progress(idx, total_lines);
    }

    Ok(out)
}
