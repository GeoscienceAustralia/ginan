//! Tests for `clock_sampling`: which clock a RINEX interval asks for, the URLs
//! that produces, and that one clock file per day is left in the products dir.

use super::catalogue_parse::PppCatalogueEntry;
use super::clock_sampling::*;
use super::naming::{filename_for, gps_week, product_url};
use super::CDDIS_PRODUCTS;
use chrono::NaiveDate;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

// 2026-09-14 is day 257 of 2026, the day in the module docs' example.
const FINE: &str = "COD0OPSFIN_20262570000_01D_05S_CLK.CLK";
const USUAL: &str = "COD0OPSFIN_20262570000_01D_30S_CLK.CLK";

fn fine_for(interval: Option<f64>, entry: Option<&PppCatalogueEntry>) -> Option<FineClock> {
    fine_clock_to_try(interval, entry, "COD", "OPS", "FIN", 2026, 257)
}

fn entry_with_clk(file: &str) -> PppCatalogueEntry {
    let mut files = HashMap::new();
    files.insert("CLK".to_string(), file.to_string());
    PppCatalogueEntry {
        provider: "COD".to_string(),
        series: "FIN".to_string(),
        project: "OPS".to_string(),
        has_sp3: true,
        has_clk: true,
        has_bia: true,
        constellations: Vec::new(),
        files,
    }
}

// ─── the choice ──────────────────────────────────────────────────────────────

#[test]
fn rinex_interval_under_30_s_wants_the_fine_clock() {
    for interval in [0.1, 1.0, 5.0, 15.0, 29.0, 29.999] {
        assert!(wants_fine_clock(Some(interval)), "{interval} s should want 05S");
    }
}

#[test]
fn rinex_interval_of_30_s_or_more_keeps_the_usual_clock() {
    for interval in [30.0, 30.001, 60.0, 300.0] {
        assert!(!wants_fine_clock(Some(interval)), "{interval} s should stay at 30S");
    }
}

#[test]
fn unknown_or_nonsensical_interval_keeps_the_usual_clock() {
    assert!(!wants_fine_clock(None));
    // A zero or negative INTERVAL record is junk, not "very fast".
    for bad in [0.0, -1.0, f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(!wants_fine_clock(Some(bad)), "{bad} must not count as a fast file");
    }
}

#[test]
fn short_interval_tries_the_05s_name_from_the_stakeholder_example() {
    let got = fine_for(Some(1.0), None).expect("1 s should try 05S");
    assert_eq!(got.filename, FINE);
    assert_eq!(got.interval_s, 1.0);
}

#[test]
fn long_or_unknown_interval_tries_no_fine_clock() {
    assert_eq!(fine_for(Some(30.0), None), None);
    assert_eq!(fine_for(Some(60.0), None), None);
    assert_eq!(fine_for(None, None), None);
}

#[test]
fn fine_clock_uses_the_requested_day_not_the_catalogue_day() {
    // The catalogue is scanned on one representative day of the week.
    let entry = entry_with_clk("COD0OPSFIN_20260600000_01D_30S_CLK.CLK");
    let got = fine_for(Some(5.0), Some(&entry)).unwrap();
    assert_eq!(got.filename, FINE);
}

#[test]
fn fine_clock_is_not_tried_twice_when_the_catalogue_already_picks_05s() {
    let entry = entry_with_clk("COD0OPSFIN_20260600000_01D_05S_CLK.CLK");
    assert_eq!(fine_for(Some(1.0), Some(&entry)), None);
}

#[test]
fn fine_and_usual_urls_differ_only_in_the_sampling() {
    let week = gps_week(NaiveDate::from_ymd_opt(2026, 9, 14).unwrap());
    let fine = fine_for(Some(1.0), None).unwrap();
    let usual = filename_for("CLK", "CLK", None, "COD", "OPS", "FIN", 2026, 257);
    assert_eq!(usual, USUAL);
    assert_eq!(
        product_url(CDDIS_PRODUCTS, week, &fine.filename),
        format!("{CDDIS_PRODUCTS}/{week:04}/COD0OPSFIN_20262570000_01D_05S_CLK.CLK.gz"),
    );
    assert_eq!(
        product_url(CDDIS_PRODUCTS, week, &usual),
        format!("{CDDIS_PRODUCTS}/{week:04}/COD0OPSFIN_20262570000_01D_30S_CLK.CLK.gz"),
    );
}

#[test]
fn sample_of_reads_the_sampling_field() {
    assert_eq!(sample_of(FINE).as_deref(), Some("05S"));
    assert_eq!(sample_of(&format!("{USUAL}.gz")).as_deref(), Some("30S"));
    assert_eq!(sample_of("README.txt"), None);
}

// ─── one clock file per day ──────────────────────────────────────────────────

fn tempdir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("ginan_clock_{tag}_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn touch(dir: &Path, names: &[&str]) {
    for n in names {
        std::fs::write(dir.join(n), b"x").unwrap();
    }
}

fn listing(dir: &Path) -> Vec<String> {
    let mut v: Vec<String> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().to_string())
        .collect();
    v.sort();
    v
}

#[test]
fn keeping_the_05s_clock_removes_a_stale_30s_for_the_same_day() {
    let dir = tempdir("keep_fine");
    touch(&dir, &[FINE, USUAL]);
    assert_eq!(remove_superseded_clocks(&dir, FINE).removed, vec![USUAL.to_string()]);
    assert_eq!(listing(&dir), vec![FINE.to_string()]);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn keeping_the_30s_clock_removes_a_stale_05s_for_the_same_day() {
    let dir = tempdir("keep_usual");
    touch(&dir, &[FINE, USUAL]);
    assert_eq!(remove_superseded_clocks(&dir, USUAL).removed, vec![FINE.to_string()]);
    assert_eq!(listing(&dir), vec![USUAL.to_string()]);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_lone_clock_file_is_never_removed() {
    let dir = tempdir("lone");
    touch(&dir, &[USUAL]);
    assert!(remove_superseded_clocks(&dir, USUAL).removed.is_empty());
    assert_eq!(listing(&dir), vec![USUAL.to_string()]);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn other_days_series_centres_and_files_are_left_alone() {
    let dir = tempdir("others");
    let neighbours = [
        // Another day (and so another clock file the same run may need).
        "COD0OPSFIN_20262580000_01D_30S_CLK.CLK",
        "COD0OPSFIN_20262580000_01D_05S_CLK.CLK",
        // Another series, project and analysis centre for the same day.
        "COD0OPSRAP_20262570000_01D_30S_CLK.CLK",
        "COD0MGXFIN_20262570000_01D_30S_CLK.CLK",
        "GFZ0OPSFIN_20262570000_01D_30S_CLK.CLK",
        // Not clock files, and an interrupted download.
        "COD0OPSFIN_20262570000_01D_15M_ORB.SP3",
        "COD0OPSFIN_20262570000_01D_30S_CLK.CLK.gz.part",
        "notes.txt",
    ];
    touch(&dir, &neighbours);
    touch(&dir, &[FINE, USUAL]);

    assert_eq!(remove_superseded_clocks(&dir, FINE).removed, vec![USUAL.to_string()]);

    let mut expected: Vec<String> = neighbours.iter().map(|s| s.to_string()).collect();
    expected.push(FINE.to_string());
    expected.sort();
    assert_eq!(listing(&dir), expected);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn two_non_fine_samplings_are_not_touched() {
    // 30S next to 05M is not something this feature creates; leave it be.
    let dir = tempdir("non_fine");
    touch(&dir, &[USUAL, "COD0OPSFIN_20262570000_01D_05M_CLK.CLK"]);
    assert!(remove_superseded_clocks(&dir, USUAL).removed.is_empty());
    assert_eq!(listing(&dir).len(), 2);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn keeping_05s_also_clears_a_stale_05m_of_the_same_product() {
    let dir = tempdir("fine_vs_05m");
    let stale = "COD0OPSFIN_20262570000_01D_05M_CLK.CLK";
    touch(&dir, &[FINE, stale]);
    assert_eq!(remove_superseded_clocks(&dir, FINE).removed, vec![stale.to_string()]);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn missing_or_unparseable_inputs_remove_nothing() {
    let dir = tempdir("missing");
    touch(&dir, &[FINE, USUAL]);
    assert!(remove_superseded_clocks(&dir, "not-a-product.CLK").removed.is_empty());
    assert!(remove_superseded_clocks(&dir.join("absent"), FINE).removed.is_empty());
    assert_eq!(listing(&dir).len(), 2);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_clock_file_that_cannot_be_removed_is_reported_not_dropped() {
    let dir = tempdir("remove_fails");
    touch(&dir, &[FINE, USUAL]);
    let cleanup = remove_superseded_clocks_with(&dir, FINE, |_| {
        Err(std::io::Error::new(std::io::ErrorKind::PermissionDenied, "file in use"))
    });
    assert!(cleanup.removed.is_empty());
    assert_eq!(cleanup.failed, vec![(USUAL.to_string(), "file in use".to_string())]);
    // Nothing was deleted, so both clocks are still there for the user to see.
    assert_eq!(listing(&dir), vec![FINE.to_string(), USUAL.to_string()]);
    let _ = std::fs::remove_dir_all(&dir);
}
