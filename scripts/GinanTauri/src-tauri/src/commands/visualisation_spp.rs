//! Reads PEA's SBAS `.SPP` output alongside `.pos`, so an L1 or DFMC SBAS run
//! (which writes no PPP solution) still has something to plot.
//!
//! Column layout is fixed by `writeSPP` in `src/cpp/sbas/sbas.cpp`. Every
//! missing value is a lone `-`, so the columns stay at the same indices
//! whether or not PEA had an a priori position:
//!
//! ```text
//! 0  time (ISO)   1  decimal year   2..4  X Y Z      5..7  HDOP VDOP GDOP
//! 8..10 - - -     11..13 NLat Elong Height
//! 14..16 dN dE dU (or `-` without an a priori)       17  dH
//! 18 HPL          19 VPL            20..22 - - -
//! ```

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use serde::Serialize;

use super::visualisation_helpers::{parse_pos_time, Epoch};

/// Field count a data line must reach (through VPL) to be an epoch.
pub(crate) const SPP_MIN_FIELDS: usize = 20;

/// Which solution file a path holds, from its extension.
#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum SolutionFormat {
    Pos,
    Spp,
}

impl SolutionFormat {
    pub fn of(path: &Path) -> Option<Self> {
        let ext = path.extension()?.to_str()?;
        if ext.eq_ignore_ascii_case("pos") {
            Some(Self::Pos)
        } else if ext.eq_ignore_ascii_case("spp") {
            Some(Self::Spp)
        } else {
            None
        }
    }
}

/// The epoch an `.SPP` file without an a priori position is measured from, so
/// its dN/dE/dU read as movement since the first fix. Without one the charts
/// would have nothing to plot but absolute ECEF coordinates.
#[derive(Clone, Copy, Debug)]
pub(crate) struct SppReference {
    xyz: [f64; 3],
    sin_lat: f64,
    cos_lat: f64,
    sin_lon: f64,
    cos_lon: f64,
}

impl SppReference {
    fn new(xyz: [f64; 3], lat_deg: f64, lon_deg: f64) -> Self {
        let (lat, lon) = (lat_deg.to_radians(), lon_deg.to_radians());
        Self { xyz, sin_lat: lat.sin(), cos_lat: lat.cos(), sin_lon: lon.sin(), cos_lon: lon.cos() }
    }

    /// North, east, up of `xyz` from the reference, metres.
    fn neu(&self, xyz: [f64; 3]) -> (f64, f64, f64) {
        let [dx, dy, dz] = [xyz[0] - self.xyz[0], xyz[1] - self.xyz[1], xyz[2] - self.xyz[2]];
        let e = -self.sin_lon * dx + self.cos_lon * dy;
        let n = -self.sin_lat * self.cos_lon * dx - self.sin_lat * self.sin_lon * dy + self.cos_lat * dz;
        let u = self.cos_lat * self.cos_lon * dx + self.cos_lat * self.sin_lon * dy + self.sin_lat * dz;
        (n, e, u)
    }
}

/// Parses one `.SPP` line. Without PEA's dN/dE/dU, the first epoch parsed
/// becomes `reference` and the offsets are computed from it. SPP carries no
/// formal errors, so the sigmas are zero (statistics fall back to unweighted);
/// it carries protection levels instead, in `levels`.
pub(crate) fn parse_spp_line(line: &str, reference: &mut Option<SppReference>) -> Option<Epoch> {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('*') {
        return None;
    }
    let fields: Vec<&str> = trimmed.split_whitespace().collect();
    if fields.len() < SPP_MIN_FIELDS {
        return None;
    }
    let t = parse_pos_time(fields[0])?;
    let num = |i: usize| -> Option<f64> {
        let v: f64 = fields[i].parse().ok()?;
        v.is_finite().then_some(v)
    };
    let (lat, lon) = (num(11)?, num(12)?);
    let (dn, de, du) = match (num(14), num(15), num(16)) {
        (Some(n), Some(e), Some(u)) => (n, e, u),
        _ => {
            let xyz = [num(2)?, num(3)?, num(4)?];
            reference.get_or_insert_with(|| SppReference::new(xyz, lat, lon)).neu(xyz)
        }
    };
    let levels = Some((num(18)?, num(19)?));
    Some(Epoch { t, dn, de, du, sn: 0.0, se: 0.0, su: 0.0, lat, lon, levels })
}

/// Every field `writeSPP` writes on an epoch line, trailing `- - -` included.
pub(crate) const SPP_FIELDS: usize = 23;

/// `[gpst_ms, hpl, vpl]` from one `.SPP` line: the horizontal and vertical
/// protection levels PEA computed for that epoch, metres.
pub(crate) fn parse_spp_levels(line: &str) -> Option<[f64; 3]> {
    let fields: Vec<&str> = line.split_whitespace().collect();
    if fields.len() < SPP_MIN_FIELDS {
        return None;
    }
    let t = parse_pos_time(fields[0])?;
    let num = |i: usize| fields[i].parse::<f64>().ok().filter(|v| v.is_finite());
    Some([t, num(18)?, num(19)?])
}

/// The reference a live tail must share with `load_pos_series`: the file's
/// first epoch, or `None` when it has none yet (the first tailed line sets it).
pub(crate) fn first_spp_reference(path: &Path) -> Option<SppReference> {
    let reader = BufReader::new(File::open(path).ok()?);
    let mut reference = None;
    for line in reader.lines().map_while(Result::ok) {
        if parse_spp_line(&line, &mut reference).is_some() {
            break;
        }
    }
    reference
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Header and epochs as `writeSPP` formats them, with an a priori position.
    const WITH_APRIORI: &str = "
*YYYY-MM-DDTHH:MM:SS.SSS YYYY.YYYYYYYYY        X             Y              Z           HDOP       VDOP       GDOP      - - -       N ref.          E ref.       H ref.         dN          dE          dU          dH         HPL        VPL     - - -
 2026-10-27T01:00:00.000 2026.819178082  -4052052.73900  4212835.98200 -2545104.59000      0.7000      1.1000      1.6000 - - -  -23.6701876321  133.8855126012   603.23000     0.41000    -0.22000     1.05000     0.46520     5.10000     8.20000 - - -";

    /// The same epoch twice more, without an a priori: offsets come from XYZ.
    const WITHOUT_APRIORI: &str = "
*YYYY-MM-DDTHH:MM:SS.SSS YYYY.YYYYYYYYY        X             Y              Z           HDOP       VDOP       GDOP      - - -       N recv          E recv       H recv   - - - -     HPL        VPL      - - -
 2026-10-27T01:00:00.000 2026.819178082  -4052052.73900  4212835.98200 -2545104.59000      0.7000      1.1000      1.6000 - - -  -23.6701876321  133.8855126012   603.23000 - - - -     5.10000     8.20000 - - -
 2026-10-27T01:00:01.000 2026.819178114  -4052052.73900  4212835.98200 -2545103.59000      0.7000      1.1000      1.6000 - - -  -23.6701876321  133.8855126012   603.23000 - - - -     5.10000     8.20000 - - -";

    fn parse_all(text: &str) -> Vec<Epoch> {
        let mut reference = None;
        text.lines().filter_map(|l| parse_spp_line(l, &mut reference)).collect()
    }

    #[test]
    fn reads_pea_offsets_when_there_is_an_a_priori() {
        let rows = parse_all(WITH_APRIORI);
        assert_eq!(rows.len(), 1);
        let r = &rows[0];
        assert_eq!((r.dn, r.de, r.du), (0.41, -0.22, 1.05));
        assert_eq!((r.sn, r.se, r.su), (0.0, 0.0, 0.0));
        assert!((r.lat - -23.6701876321).abs() < 1e-12 && (r.lon - 133.8855126012).abs() < 1e-12);
    }

    #[test]
    fn measures_from_the_first_epoch_without_an_a_priori() {
        let rows = parse_all(WITHOUT_APRIORI);
        assert_eq!(rows.len(), 2);
        assert_eq!((rows[0].dn, rows[0].de, rows[0].du), (0.0, 0.0, 0.0));
        // 1 m along +Z at ~23.67°S: mostly north, some up, no east.
        let (n, e, u) = (rows[1].dn, rows[1].de, rows[1].du);
        let lat = (-23.6701876321f64).to_radians();
        assert!((n - lat.cos()).abs() < 1e-9, "n={n}");
        assert!((u - lat.sin()).abs() < 1e-9, "u={u}");
        assert!(e.abs() < 1e-9, "e={e}");
    }

    #[test]
    fn skips_the_legend_and_short_lines() {
        let mut reference = None;
        assert!(parse_spp_line("*YYYY-MM-DD legend", &mut reference).is_none());
        assert!(parse_spp_line(" 2026-10-27T01:00:00.000 2026.8 1 2 3", &mut reference).is_none());
        assert!(reference.is_none());
    }

    #[test]
    fn knows_the_format_from_the_extension() {
        assert_eq!(SolutionFormat::of(Path::new("A_2026300.SPP")), Some(SolutionFormat::Spp));
        assert_eq!(SolutionFormat::of(Path::new("a.pos")), Some(SolutionFormat::Pos));
        assert_eq!(SolutionFormat::of(Path::new("a.TRACE")), None);
    }
}
