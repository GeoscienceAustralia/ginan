//! Split out of visualisation.rs to keep each file under 500 lines.


use chrono::NaiveDateTime;
use super::visualisation::*;

// ─── helpers ──────────────────────────────────────────────────────────────────

pub(crate) struct Epoch {
    pub(crate) t: f64,
    pub(crate) dn: f64,
    pub(crate) de: f64,
    pub(crate) du: f64,
    pub(crate) sn: f64,
    pub(crate) se: f64,
    pub(crate) su: f64,
    pub(crate) lat: f64,
    pub(crate) lon: f64,
    /// Horizontal and vertical protection levels, metres: `.SPP` only.
    pub(crate) levels: Option<(f64, f64)>,
}

/// Parses a POS timestamp (`2020-01-01T00:00:00.000`) to epoch milliseconds.
///
/// The fractional part is optional: `to_ISOstring` emits it at 3 decimals, but
/// a whole-second variant is accepted so hand-made or converted files load.
pub(crate) fn parse_pos_time(token: &str) -> Option<f64> {
    let dt = NaiveDateTime::parse_from_str(token, "%Y-%m-%dT%H:%M:%S%.f")
        .or_else(|_| NaiveDateTime::parse_from_str(token, "%Y-%m-%dT%H:%M:%S"))
        .ok()?;
    Some(dt.and_utc().timestamp_millis() as f64)
}

/// Pulls the six plotted columns out of a data line. Returns None if any is
/// unparseable or non-finite, which drops the epoch rather than poisoning the
/// scales with a NaN.
pub(crate) fn parse_epoch(t: f64, fields: &[&str]) -> Option<Epoch> {
    let num = |i: usize| -> Option<f64> {
        let v: f64 = fields[i].parse().ok()?;
        v.is_finite().then_some(v)
    };
    Some(Epoch {
        t,
        dn: num(14)?,
        de: num(15)?,
        du: num(16)?,
        // Sigmas are only used for whiskers and weights, so a bad one is
        // clamped to zero (an epoch with no stated precision) rather than
        // discarding an otherwise good position.
        sn: num(17).unwrap_or(0.0).max(0.0),
        se: num(18).unwrap_or(0.0).max(0.0),
        su: num(19).unwrap_or(0.0).max(0.0),
        // NLat/Elong (fields 11/12) are the absolute WGS84 position dN/dE are
        // offsets from; required for the map view, not for any statistic.
        lat: num(11)?,
        lon: num(12)?,
        levels: None,
    })
}

/// Inverse-variance-weighted mean and standard deviation, plus RMS about zero.
///
/// Epochs whose sigma is zero or unusable carry no weight; if that leaves no
/// weight at all the statistics fall back to unweighted, so a file written
/// without sigmas still reports a mean instead of NaN.
pub(crate) fn component_stats<I>(samples: I) -> ComponentStats
where
    I: Iterator<Item = (f64, f64)> + Clone,
{
    let mut n = 0usize;
    let mut sum = 0.0;
    let mut sum_sq = 0.0;
    let mut sum_w = 0.0;
    let mut sum_wx = 0.0;
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
        if sigma > 0.0 {
            let w = 1.0 / (sigma * sigma);
            sum_w += w;
            sum_wx += w * x;
        }
    }

    if n == 0 {
        return ComponentStats::default();
    }

    let weighted = sum_w > 0.0;
    let mean = if weighted { sum_wx / sum_w } else { sum / n as f64 };

    let mut acc = 0.0;
    for (x, sigma) in samples {
        let d = x - mean;
        if weighted {
            if sigma > 0.0 {
                acc += d * d / (sigma * sigma);
            }
        } else {
            acc += d * d;
        }
    }
    let denom = if weighted { sum_w } else { n as f64 };

    ComponentStats {
        weighted_mean: mean,
        std_dev: (acc / denom).sqrt(),
        rms: (sum_sq / n as f64).sqrt(),
        min,
        max,
        mean_sigma: sum_sigma / n as f64,
    }
}
