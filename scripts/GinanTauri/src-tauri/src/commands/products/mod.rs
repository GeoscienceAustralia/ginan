//! Product download, catalogue discovery and bias-table handling.
//!
//! Each submodule holds one stage of the job:
//!
//!   naming     file naming and date arithmetic
//!   http       client construction, auth, download + gunzip
//!   download   the two product-fetching commands and the per-day logic
//!   catalogue  scanning a GPS week to see what products actually exist
//!   clock_sampling  preferring the 05S clock for fast RINEX files
//!   bia        signal-bias tables, their parsing and disk cache
//!
//! Items shared across them live here. Everything the rest of the crate uses
//! is re-exported below, so callers go through `commands::products::…`.

use serde::{Deserialize, Serialize};


const PEANPOD_SERVER: &str = "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products";
/// NASA CDDIS — the only PPP product source. Preferred per
/// Geoscience Australia. Requires Earthdata credentials; HEAD without auth
/// returns 401 (treated as "auth-required, server reachable" by the health
/// check).
const CDDIS_PRODUCTS: &str = "https://cddis.nasa.gov/archive/gnss/products";
/// GA's RINEX file search. The unversioned `/api/rinexFiles` path answers
/// 301 to this one, and the redirect drops `filePeriod`, `decompress` and
/// `tenantId` from the query — so call the versioned path directly.
const GA_API: &str = "https://data.gnss.ga.gov.au/api/v1/rinexFiles";

/// Base URLs that host PPP analysis-center products.
///
/// CDDIS is the sole source so that every product the UI offers comes from
/// one authoritative server. Health checks reuse this list rather than duplicating it.
/// `provider` is accepted for forward compatibility (a future per-provider
/// override would slot in here).
pub fn provider_base_urls(_provider: &str) -> Vec<&'static str> {
    vec![CDDIS_PRODUCTS]
}

/// Days after the observation date before a series is typically published.
/// Skip attempts for dates that are too recent for the requested series.
const SERIES_LATENCY_DAYS: &[(&str, i64)] = &[
    ("ULT", 0),  // ultra-rapid: real-time
    ("RTS", 0),  // real-time streams
    ("RAP", 1),  // rapid: ~next day
    ("R03", 0),  // REPRO3 historical reprocessing, no latency
    ("FIN", 14), // final: ~12-21 days, conservative 14
];

/// True when this series should have its products published by today for the
/// requested observation date. Returns true for unknown series (don't skip).
pub(crate) fn series_is_published(series: &str, obs_date: chrono::NaiveDate) -> bool {
    let today = chrono::Utc::now().date_naive();
    match SERIES_LATENCY_DAYS.iter().find(|(s, _)| s.eq_ignore_ascii_case(series)) {
        Some((_, days)) => (today - obs_date).num_days() >= *days,
        None => true,
    }
}

/// Which mirrors each provider's products are available on. Every provider
/// currently resolves to CDDIS alone; a future non-standard provider can be
/// added here.
pub(crate) fn provider_mirrors(_provider: &str) -> &'static [&'static str] {
    &["CDDIS"]
}

/// Map a base URL constant to its short mirror name.
pub(crate) fn mirror_name(base_url: &str) -> &'static str {
    if base_url == CDDIS_PRODUCTS { "CDDIS" } else { "?" }
}

/// BIA code priorities per constellation extracted from bias files
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct BiaCodePriorities {
    pub gps: Vec<String>,
    pub gal: Vec<String>,
    pub glo: Vec<String>,
    pub bds: Vec<String>,
    pub qzs: Vec<String>,
}

/// Static support files PEA requires. Paths are relative to
/// `products_dir/static` (see `layout`) and must match the defaults in
/// `resources/default_config.yaml`.
const STATIC_PRODUCTS: &[&str] = &[
    "igs20.atx",
    "igs_satellite_metadata.snx",
    "finals.data.iau2000.txt",
    "tables/sat_yaw_bias_rate.snx",
    "tables/qzss_yaw_modes.snx",
    "tables/bds_yaw_modes.snx",
    "tables/OLOAD_GO.BLQ",
    "tables/ALOAD_GO.BLQ",
    "tables/opoleloadcoefcmcor.txt",
    "tables/igrf14coeffs.txt",
    "tables/DE436.1950.2050",
    "tables/gpt_25.grd",
];

#[derive(Clone, Serialize)]
struct ProgressEvent<'a> {
    filename: &'a str,
    percent: u8,
    bytes: u64,
    total: u64,
}

#[derive(Clone, Serialize)]
struct ConnectionFaultEvent {
    host: String,
    message: String,
}

pub(crate) fn extract_host(url: &str) -> String {
    url.splitn(3, '/').nth(2)
        .and_then(|s| s.split('/').next())
        .unwrap_or(url)
        .to_string()
}


pub mod bia;
#[cfg(test)]
mod bia_tests;
pub mod catalogue_fetch;
pub mod catalogue_parse;
#[cfg(test)]
mod catalogue_parse_tests;
pub mod clock_sampling;
#[cfg(test)]
mod clock_sampling_tests;
pub mod download;
pub mod http;
pub mod layout;
pub mod naming;

pub use bia::{fetch_bia_codes, fetch_bia_codes_for_combo};
pub use catalogue_fetch::scan_ppp_catalogue;
pub use catalogue_parse::{PppCatalogue, PppCatalogueEntry};
pub use download::{ensure_dynamic_products, ensure_products};
pub use http::{authed_get, build_client};
pub(crate) use http::get_following_redirects;

#[cfg(test)]
mod tests {

    use super::*;
    use super::download::{listed, mirror_order, present_candidate, url_basename};

    #[test]
    fn listed_keeps_only_files_on_the_week_listing() {
        let listing: std::collections::HashSet<String> =
            ["GFZ0MGXRAP_20262100000_01D_05M_ORB.SP3.gz".to_string()].into_iter().collect();
        let base = "https://cddis.nasa.gov/archive/gnss/products/2429";
        assert!(listed(Some(&listing), &format!("{base}/GFZ0MGXRAP_20262100000_01D_05M_ORB.SP3.gz")));
        assert!(!listed(Some(&listing), &format!("{base}/GFZ0MGXFIN_20262100000_01D_15M_ORB.SP3.gz")));
        assert!(!listed(Some(&listing), &format!("{base}/GFZ0MGXFIN_20262100000_01D_01D_OSB.BIA.gz")));
        // No listing to check against: every candidate is still tried.
        assert!(listed(None, &format!("{base}/GFZ0MGXFIN_20262100000_01D_15M_ORB.SP3.gz")));
    }

    #[test]
    fn series_published_returns_false_for_recent_fin() {
        let today = chrono::Utc::now().date_naive();
        let recent = today - chrono::Duration::days(5);
        assert!(!series_is_published("FIN", recent));
    }

    #[test]
    fn series_published_returns_true_for_old_fin() {
        let today = chrono::Utc::now().date_naive();
        let old = today - chrono::Duration::days(30);
        assert!(series_is_published("FIN", old));
    }

    #[test]
    fn series_published_handles_unknown_series() {
        let today = chrono::Utc::now().date_naive();
        assert!(series_is_published("XYZ", today));
    }

    #[test]
    fn provider_mirrors_are_cddis_only() {
        // Every provider resolves to CDDIS alone.
        assert_eq!(provider_mirrors("COD"), &["CDDIS"]);
        assert_eq!(provider_mirrors("IGS"), &["CDDIS"]);
    }

    #[test]
    fn mirror_order_is_cddis_regardless_of_preference() {
        // The persisted "preferred mirror" setting must not resurrect BKG/IGN.
        assert_eq!(mirror_order("BKG"), [CDDIS_PRODUCTS]);
        assert_eq!(mirror_order("IGN"), [CDDIS_PRODUCTS]);
        assert_eq!(mirror_order("CDDIS"), [CDDIS_PRODUCTS]);
    }

    #[test]
    fn provider_base_urls_are_cddis_only() {
        assert_eq!(provider_base_urls("IGS"), vec![CDDIS_PRODUCTS]);
    }

    #[test]
    fn url_basename_drops_directory_and_gz() {
        assert_eq!(
            url_basename("https://cddis.nasa.gov/archive/gnss/products/2380/COD0OPSFIN_20262570000_01D_30S_CLK.CLK.gz"),
            "COD0OPSFIN_20262570000_01D_30S_CLK.CLK",
        );
    }

    #[test]
    fn present_candidate_reports_first_listed_file_that_exists() {
        let tmp = std::env::temp_dir().join(format!("ginan_present_candidate_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();
        let urls = vec![
            "https://h/2380/A_05S.CLK.gz".to_string(),
            "https://h/2380/B_30S.CLK.gz".to_string(),
            "https://h/2380/C_05M.CLK.gz".to_string(),
        ];
        assert_eq!(present_candidate(&tmp, &urls), None);

        std::fs::write(tmp.join("C_05M.CLK"), b"x").unwrap();
        std::fs::write(tmp.join("B_30S.CLK"), b"x").unwrap();
        // URL order decides, not directory order.
        assert_eq!(present_candidate(&tmp, &urls).as_deref(), Some("B_30S.CLK"));

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn brdc_lookup_uses_the_versioned_ga_endpoint() {
        // The unversioned /api/rinexFiles path 301s, and the client does not
        // follow redirects on its own — so this path must not drift back.
        let date = chrono::NaiveDate::from_ymd_opt(2025, 10, 21).unwrap();
        let url = download::brdc_lookup_url(date);
        assert!(
            url.starts_with("https://data.gnss.ga.gov.au/api/v1/rinexFiles?"),
            "{url}"
        );
        for param in [
            "stationId=BRDC",
            "fileType=nav",
            "rinexVersion=3",
            "filePeriod=01D",
            "startDate=2025-10-21T00:00:00Z",
            "endDate=2025-10-21T23:59:59Z",
        ] {
            assert!(url.contains(param), "missing {param} in {url}");
        }
        assert!(!url.contains(char::is_whitespace), "{url}");
    }

    #[test]
    fn brdc_location_prefers_the_daily_file() {
        let body = r#"[
            {"filePeriod":"01H","fileLocation":"https://s3/BRDC_01H.rnx.gz"},
            {"filePeriod":"01D","fileLocation":"https://s3/BRDC_01D.rnx.gz"}
        ]"#;
        assert_eq!(
            download::pick_brdc_location(body).unwrap(),
            "https://s3/BRDC_01D.rnx.gz"
        );
    }

    #[test]
    fn brdc_location_falls_back_to_first_entry_without_periods() {
        let body = r#"[{"fileLocation":"https://s3/a.gz"},{"fileLocation":"https://s3/b.gz"}]"#;
        assert_eq!(download::pick_brdc_location(body).unwrap(), "https://s3/a.gz");
    }

    #[test]
    fn brdc_location_reports_an_empty_listing() {
        assert_eq!(
            download::pick_brdc_location("[]").unwrap_err(),
            "no BRDC file listed"
        );
    }
}
