use super::catalogue_parse::parse_igs_filename;
use chrono::{NaiveDate, NaiveDateTime};
use super::*;

// ─── helpers ─────────────────────────────────────────────────────────────────

pub(crate) fn parse_date(s: &str) -> Result<NaiveDate, String> {
    let s = s.trim();
    // Accept "YYYY-MM-DD HH:MM:SS" or "YYYY-MM-DDTHH:MM[:SS]" or "YYYY-MM-DD".
    if let Ok(d) = NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        return Ok(d);
    }
    if let Ok(dt) = NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S") {
        return Ok(dt.date());
    }
    if let Ok(dt) = NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S") {
        return Ok(dt.date());
    }
    if let Ok(dt) = NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M") {
        return Ok(dt.date());
    }
    Err(format!("Cannot parse date: {s:?}"))
}

pub(crate) fn gps_week(date: NaiveDate) -> u32 {
    // GPS epoch: 1980-01-06 (Sunday)
    let epoch = NaiveDate::from_ymd_opt(1980, 1, 6).unwrap();
    let days = (date - epoch).num_days();
    (days / 7) as u32
}

#[allow(clippy::too_many_arguments)] // one parameter per IGS long-name field
pub(crate) fn long_name(
    ac: &str,
    project: &str,
    sol: &str,
    year: i32,
    doy: u32,
    timespan: &str,
    sample: &str,
    content: &str,
    ext: &str,
) -> String {
    format!("{ac}0{project}{sol}_{year:04}{doy:03}0000_{timespan}_{sample}_{content}.{ext}")
}

pub(crate) fn product_url(server: &str, gps_week: u32, filename: &str) -> String {
    format!("{server}/{gps_week:04}/{filename}.gz")
}

/// Resolve a product filename. The catalogue is scanned on one representative
/// day, so only reuse its published timespan/sample rate; the requested run's
/// year/day must always be used.
#[allow(clippy::too_many_arguments)]
pub(crate) fn filename_for(
    content: &str,
    ext: &str,
    entry: Option<&PppCatalogueEntry>,
    ac: &str,
    project: &str,
    sol: &str,
    year: i32,
    doy: u32,
) -> String {
    let key = match ext.to_uppercase().as_str() {
        "SP3" => "SP3",
        "CLK" => "CLK",
        "BIA" => "BIA",
        _ => "",
    };
    if !key.is_empty() {
        if let Some(e) = entry {
            if let Some(name) = e.files.get(key) {
                if let Some((_p, _pr, _s, _y, _d, timespan, sample, _c, _e)) =
                    parse_igs_filename(name)
                {
                    return long_name(
                        ac, project, sol, year, doy, &timespan, &sample, content, ext,
                    );
                }
            }
        }
    }
    let (timespan, sample) = match ext.to_uppercase().as_str() {
        "SP3" => ("01D", "15M"),
        "CLK" => ("01D", "30S"),
        "BIA" => ("01D", "01D"),
        _ => ("01D", "01D"),
    };
    long_name(ac, project, sol, year, doy, timespan, sample, content, ext)
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::collections::HashMap;

    #[test]
    fn filename_for_falls_back_to_template_when_no_catalogue() {
        let got = filename_for("ORB", "SP3", None, "IGS", "OPS", "FIN", 2024, 1);
        let expected = long_name("IGS", "OPS", "FIN", 2024, 1, "01D", "15M", "ORB", "SP3");
        assert_eq!(got, expected);

        let got_clk = filename_for("CLK", "CLK", None, "IGS", "OPS", "FIN", 2024, 1);
        let expected_clk = long_name("IGS", "OPS", "FIN", 2024, 1, "01D", "30S", "CLK", "CLK");
        assert_eq!(got_clk, expected_clk);
    }

    #[test]
    fn filename_for_reuses_catalogue_rate_but_not_catalogue_date() {
        let mut files = HashMap::new();
        files.insert(
            "SP3".to_string(),
            "COD0MGXFIN_20260600000_01D_05M_ORB.SP3".to_string(),
        );
        files.insert(
            "CLK".to_string(),
            "COD0MGXFIN_20260600000_01D_30S_CLK.CLK".to_string(),
        );
        files.insert(
            "BIA".to_string(),
            "COD0MGXFIN_20260600000_01D_01D_OSB.BIA".to_string(),
        );
        let entry = PppCatalogueEntry {
            provider: "COD".to_string(),
            series: "FIN".to_string(),
            project: "MGX".to_string(),
            has_sp3: true,
            has_clk: true,
            has_bia: true,
            constellations: Vec::new(),
            files,
        };

        let got = filename_for(
            "ORB",
            "SP3",
            Some(&entry),
            "COD",
            "MGX",
            "FIN",
            2026,
            64,
        );

        assert_eq!(got, "COD0MGXFIN_20260640000_01D_05M_ORB.SP3");
        assert_eq!(
            filename_for(
                "CLK",
                "CLK",
                Some(&entry),
                "COD",
                "MGX",
                "FIN",
                2026,
                64,
            ),
            "COD0MGXFIN_20260640000_01D_30S_CLK.CLK",
        );
        assert_eq!(
            filename_for(
                "OSB",
                "BIA",
                Some(&entry),
                "COD",
                "MGX",
                "FIN",
                2026,
                64,
            ),
            "COD0MGXFIN_20260640000_01D_01D_OSB.BIA",
        );
    }
}
