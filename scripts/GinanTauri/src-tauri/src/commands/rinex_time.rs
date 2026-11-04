//! GPS-time conversion for RINEX epochs, split out of `rinex_parse`.

/// GPS − UTC in seconds, as (first UTC day it applies, offset). GPS time has
/// no leap seconds, so the gap grows by one at each UTC leap second.
const GPS_UTC_LEAPS: [((i32, u32, u32), i64); 18] = [
    ((1981, 7, 1), 1), ((1982, 7, 1), 2), ((1983, 7, 1), 3), ((1985, 7, 1), 4),
    ((1988, 1, 1), 5), ((1990, 1, 1), 6), ((1991, 1, 1), 7), ((1992, 7, 1), 8),
    ((1993, 7, 1), 9), ((1994, 7, 1), 10), ((1996, 1, 1), 11), ((1997, 7, 1), 12),
    ((1999, 1, 1), 13), ((2006, 1, 1), 14), ((2009, 1, 1), 15), ((2012, 7, 1), 16),
    ((2015, 7, 1), 17), ((2017, 1, 1), 18),
];

/// GPS − UTC at a UTC instant.
fn gps_minus_utc(utc: chrono::NaiveDateTime) -> i64 {
    GPS_UTC_LEAPS
        .iter()
        .rev()
        .find(|((y, m, d), _)| {
            chrono::NaiveDate::from_ymd_opt(*y, *m, *d)
                .map(|day| utc.date() >= day)
                .unwrap_or(false)
        })
        .map(|(_, n)| *n)
        .unwrap_or(0)
}

/// Re-express a `"YYYY-MM-DD HH:MM:SS"` epoch from `system` in GPS time.
///
/// GAL, QZS and IRN are steered to GPS time, so they pass through unchanged,
/// as does anything unrecognised. GLONASS time is UTC(SU) + 3 h; BeiDou time
/// started 14 s behind GPS time in 2006 and has stayed there.
pub(crate) fn epoch_to_gps(epoch: &str, system: &str) -> String {
    use chrono::{Duration, NaiveDateTime};
    const FMT: &str = "%Y-%m-%d %H:%M:%S";
    let Ok(t) = NaiveDateTime::parse_from_str(epoch, FMT) else {
        return epoch.to_string();
    };
    let gps = match system {
        "GLO" | "UTC" => {
            let utc = if system == "GLO" { t - Duration::hours(3) } else { t };
            utc + Duration::seconds(gps_minus_utc(utc))
        }
        "BDT" => t + Duration::seconds(14),
        _ => return epoch.to_string(),
    };
    gps.format(FMT).to_string()
}
