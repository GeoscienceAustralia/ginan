//! Reading the observation records after the header, for what the header can
//! leave out: the sampling interval (`INTERVAL` is optional) and the last
//! epoch (`TIME OF LAST OBS` is optional too). Split out of `rinex_parse`.

use std::io::BufRead;
use super::rinex_parse::{parse_data_epoch_v2, parse_data_epoch_v3};

/// How many epoch spacings to look at before settling on an interval. Enough
/// to see past a missing epoch or two without reading a whole day's file.
const INTERVAL_SAMPLE_GAPS: usize = 10;

/// What the body scan found.
#[derive(Debug, Default, PartialEq)]
pub(crate) struct BodyScan {
    /// Sampling interval in seconds, from the epoch spacing.
    pub interval: Option<f64>,
    /// Last epoch, as `"YYYY-MM-DD HH:MM:SS"`.
    pub last_epoch: Option<String>,
}

/// Scan the observation records for the interval, and for the last epoch if
/// `want_last` is set. Without `want_last` it stops once the first few epochs
/// are in, so a header that has `TIME OF LAST OBS` but no `INTERVAL` costs a
/// few lines rather than the whole file.
pub(crate) fn scan_body(
    lines: &mut std::io::Lines<impl BufRead>,
    version: f64,
    want_last: bool,
) -> BodyScan {
    let mut instants: Vec<f64> = Vec::with_capacity(INTERVAL_SAMPLE_GAPS + 1);
    let mut last_epoch: Option<String> = None;

    while let Some(Ok(line)) = lines.next() {
        let Some(epoch) = data_epoch(&line, version) else { continue };
        if instants.len() <= INTERVAL_SAMPLE_GAPS {
            if let Some(t) = epoch_seconds(&line, version) {
                instants.push(t);
            }
        }
        if want_last {
            last_epoch = Some(epoch);
        } else if instants.len() > INTERVAL_SAMPLE_GAPS {
            break;
        }
    }

    BodyScan { interval: interval_from_instants(&instants), last_epoch }
}

/// The epoch an observation record opens with, or `None` for any other line.
fn data_epoch(line: &str, version: f64) -> Option<String> {
    if version >= 3.0 {
        if line.starts_with('>') && line.len() >= 35 {
            return parse_data_epoch_v3(&line[1..]);
        }
        None
    } else if line.len() >= 26 && line.starts_with(' ') {
        parse_data_epoch_v2(line)
    } else {
        None
    }
}

/// An epoch record's time as seconds on a continuous scale, keeping the
/// fractional seconds that the `"YYYY-MM-DD HH:MM:SS"` form drops, so a 10 Hz
/// file does not look like one with every epoch at the same time. Only call it
/// on a line `data_epoch` accepted.
fn epoch_seconds(line: &str, version: f64) -> Option<f64> {
    let fields = if version >= 3.0 { &line[1..] } else { line };
    let parts: Vec<&str> = fields.split_whitespace().collect();
    if parts.len() < 6 {
        return None;
    }
    let y_raw: i32 = parts[0].parse().ok()?;
    // RINEX v2 years are two digits: <80 is 20xx, otherwise 19xx.
    let y = if y_raw >= 1000 { y_raw } else if y_raw < 80 { 2000 + y_raw } else { 1900 + y_raw };
    let m: u32 = parts[1].parse().ok()?;
    let d: u32 = parts[2].parse().ok()?;
    let h: f64 = parts[3].parse().ok()?;
    let min: f64 = parts[4].parse().ok()?;
    let s: f64 = parts[5].parse().ok()?;
    let day = chrono::NaiveDate::from_ymd_opt(y, m, d)?;
    let days = chrono::Datelike::num_days_from_ce(&day) as f64;
    Some(days * 86_400.0 + h * 3_600.0 + min * 60.0 + s)
}

/// The sampling interval from consecutive epoch times: the smallest positive
/// spacing, so a missing epoch (a double spacing) does not count. Rounded to the
/// millisecond to shed floating-point noise. `None` with fewer than two epochs.
pub(crate) fn interval_from_instants(instants: &[f64]) -> Option<f64> {
    instants
        .windows(2)
        .map(|w| w[1] - w[0])
        .filter(|gap| *gap > 0.0005)
        .fold(None, |min: Option<f64>, gap| Some(min.map_or(gap, |m| m.min(gap))))
        .map(|gap| (gap * 1000.0).round() / 1000.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::rinex_parse::parse_header;

    fn header(extra: &str) -> String {
        [
            "     3.05           OBSERVATION DATA    M                   RINEX VERSION / TYPE\n",
            "TEST                                                        MARKER NAME\n",
            extra,
            "  2024     1     1     0     0    0.0000000     GPS         TIME OF FIRST OBS\n",
            "                                                            END OF HEADER\n",
        ]
        .concat()
    }

    fn v3_epochs(seconds: &[f64]) -> String {
        seconds
            .iter()
            .map(|t| {
                let (h, rem) = ((t / 3600.0).floor(), t % 3600.0);
                let (m, s) = ((rem / 60.0).floor(), rem % 60.0);
                format!("> 2024  1  1 {h:2} {m:2} {s:10.7}  0  1\nG01  23456789.123\n")
            })
            .collect()
    }

    fn parse(content: &str) -> crate::commands::rinex::RinexData {
        parse_header(std::io::BufReader::new(content.as_bytes())).unwrap()
    }

    #[test]
    fn the_header_interval_wins_when_present() {
        let file = header("    30.000                                                  INTERVAL\n")
            + &v3_epochs(&[0.0, 1.0, 2.0, 3.0]);
        assert_eq!(parse(&file).epoch_interval, Some(30.0));
    }

    #[test]
    fn a_1hz_file_without_interval_gets_one_second() {
        let file = header("") + &v3_epochs(&[0.0, 1.0, 2.0, 3.0, 4.0]);
        assert_eq!(parse(&file).epoch_interval, Some(1.0));
    }

    #[test]
    fn a_30s_file_without_interval_gets_thirty_seconds() {
        let file = header("") + &v3_epochs(&[0.0, 30.0, 60.0, 90.0]);
        assert_eq!(parse(&file).epoch_interval, Some(30.0));
    }

    #[test]
    fn a_missing_epoch_does_not_double_the_interval() {
        // 00:00:02 is missing, so the first spacing is 2 s.
        let file = header("") + &v3_epochs(&[0.0, 2.0, 3.0, 4.0]);
        assert_eq!(parse(&file).epoch_interval, Some(1.0));
    }

    #[test]
    fn spacing_across_a_minute_and_hour_boundary_is_measured_correctly() {
        let file = header("") + &v3_epochs(&[3570.0, 3585.0, 3600.0, 3615.0]);
        assert_eq!(parse(&file).epoch_interval, Some(15.0));
    }

    #[test]
    fn sub_second_sampling_keeps_its_fraction() {
        let file = header("") + &v3_epochs(&[0.0, 0.1, 0.2, 0.3]);
        assert_eq!(parse(&file).epoch_interval, Some(0.1));
    }

    #[test]
    fn a_single_epoch_file_has_no_interval() {
        let file = header("") + &v3_epochs(&[0.0]);
        assert_eq!(parse(&file).epoch_interval, None);
    }

    #[test]
    fn the_last_epoch_is_still_found_when_the_header_lacks_both() {
        let file = header("") + &v3_epochs(&[0.0, 30.0, 60.0, 3600.0]);
        let data = parse(&file);
        assert_eq!(data.epoch_interval, Some(30.0));
        assert_eq!(data.end_epoch.as_deref(), Some("2024-01-01 01:00:00"));
    }

    #[test]
    fn rinex_v2_epochs_are_read_too() {
        let data = concat!(
            " 24  1  1  0  0  0.0000000  0  1G01\n",
            "  23456789.123 7\n",
            " 24  1  1  0  0  5.0000000  0  1G01\n",
            "  23456789.123 7\n",
            " 24  1  1  0  0 10.0000000  0  1G01\n",
        );
        let mut lines = std::io::BufReader::new(data.as_bytes()).lines();
        let scan = scan_body(&mut lines, 2.11, true);
        assert_eq!(scan.interval, Some(5.0));
        assert_eq!(scan.last_epoch.as_deref(), Some("2024-01-01 00:00:10"));
    }

    #[test]
    fn without_want_last_it_stops_after_the_sample() {
        let epochs: Vec<f64> = (0..100).map(f64::from).collect();
        let data = v3_epochs(&epochs);
        let mut lines = std::io::BufReader::new(data.as_bytes()).lines();
        let scan = scan_body(&mut lines, 3.05, false);
        assert_eq!(scan.interval, Some(1.0));
        assert_eq!(scan.last_epoch, None);
        // The rest of the file is left unread.
        assert!(lines.next().is_some());
    }
}
