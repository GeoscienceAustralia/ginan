//! Tests for `catalogue_parse`, split out to keep each file under 500 lines.



use super::catalogue_parse::*;

#[test]
fn test_parse_igs_filename_sp3() {
    let (prov, proj, ser, year, doy, _ts, _smp, content, ext) =
        parse_igs_filename("IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz").unwrap();
    assert_eq!(prov, "IGS");
    assert_eq!(proj, "OPS");
    assert_eq!(ser, "FIN");
    assert_eq!(year, 2024);
    assert_eq!(doy, 1);
    assert_eq!(content, "ORB");
    assert_eq!(ext, "SP3");
}

#[test]
fn test_parse_igs_filename_clk() {
    let (prov, proj, ser, year, doy, _ts, _smp, content, ext) =
        parse_igs_filename("GRG0OPSFIN_20240010000_01D_30S_CLK.CLK.gz").unwrap();
    assert_eq!(prov, "GRG");
    assert_eq!(proj, "OPS");
    assert_eq!(ser, "FIN");
    assert_eq!(year, 2024);
    assert_eq!(doy, 1);
    assert_eq!(content, "CLK");
    assert_eq!(ext, "CLK");
}

#[test]
fn test_parse_igs_filename_bia() {
    let (prov, proj, ser, year, doy, _ts, _smp, content, ext) =
        parse_igs_filename("COD0MGXFIN_20240010000_01D_01D_OSB.BIA.gz").unwrap();
    assert_eq!(prov, "COD");
    assert_eq!(proj, "MGX");
    assert_eq!(ser, "FIN");
    assert_eq!(year, 2024);
    assert_eq!(doy, 1);
    assert_eq!(content, "OSB");
    assert_eq!(ext, "BIA");
}

#[test]
fn test_parse_igs_filename_repro3() {
    // repro3 files use project "R03" with a non-zero separator digit.
    let (prov, proj, ser, year, doy, _ts, _smp, _content, ext) =
        parse_igs_filename("COD0R03FIN_20232620000_01D_01D_OSB.BIA.gz").unwrap();
    assert_eq!(prov, "COD");
    assert_eq!(proj, "R03");
    assert_eq!(ser, "FIN");
    assert_eq!(year, 2023);
    assert_eq!(doy, 262);
    assert_eq!(ext, "BIA");
}

#[test]
fn test_parse_igs_filename_rejects_old_format() {
    // Old format: "igs21630.sp3" — no digit at position 3, no '_' at 10.
    assert!(parse_igs_filename("igs21630.sp3").is_none());
    assert!(parse_igs_filename("igs21630.sp3.Z").is_none());
}

#[test]
fn test_parse_igs_filename_rejects_short() {
    assert!(parse_igs_filename("IGS0OPS.SP3").is_none());
    assert!(parse_igs_filename("SHA512SUMS").is_none());
}

#[test]
fn test_build_catalogue_requires_sp3_clk_bia() {
    // IGS FIN/OPS has all three → kept.
    // GRG FIN/OPS has SP3+CLK but no BIA → excluded (matches Python REQUIRED_FILES).
    // COD FIN/MGX has only SP3 → excluded.
    let filenames: Vec<String> = vec![
        "IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz".into(),
        "IGS0OPSFIN_20240010000_01D_30S_CLK.CLK.gz".into(),
        "IGS0OPSFIN_20240010000_01D_01D_OSB.BIA.gz".into(),
        "GRG0OPSFIN_20240010000_01D_15M_ORB.SP3.gz".into(),
        "GRG0OPSFIN_20240010000_01D_30S_CLK.CLK.gz".into(),
        "COD0MGXFIN_20240010000_01D_15M_ORB.SP3.gz".into(),
    ];
    let cat = build_catalogue_from_filenames(&filenames);
    assert_eq!(cat.entries.len(), 1);
    assert_eq!(cat.entries[0].provider, "IGS");
    assert!(cat.entries[0].has_sp3 && cat.entries[0].has_clk && cat.entries[0].has_bia);
}

#[test]
fn test_build_catalogue_sorted() {
    let filenames: Vec<String> = vec![
        "IGS0OPSRAP_20240010000_01D_15M_ORB.SP3.gz".into(),
        "IGS0OPSRAP_20240010000_01D_30S_CLK.CLK.gz".into(),
        "IGS0OPSRAP_20240010000_01D_01D_OSB.BIA.gz".into(),
        "IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz".into(),
        "IGS0OPSFIN_20240010000_01D_30S_CLK.CLK.gz".into(),
        "IGS0OPSFIN_20240010000_01D_01D_OSB.BIA.gz".into(),
        "COD0MGXFIN_20240010000_01D_15M_ORB.SP3.gz".into(),
        "COD0MGXFIN_20240010000_01D_30S_CLK.CLK.gz".into(),
        "COD0MGXFIN_20240010000_01D_01D_OSB.BIA.gz".into(),
    ];
    let cat = build_catalogue_from_filenames(&filenames);
    assert_eq!(cat.entries.len(), 3);
    let keys: Vec<(&str, &str, &str)> = cat
        .entries
        .iter()
        .map(|e| (e.provider.as_str(), e.series.as_str(), e.project.as_str()))
        .collect();
    assert_eq!(
        keys,
        vec![
            ("COD", "FIN", "MGX"),
            ("IGS", "FIN", "OPS"),
            ("IGS", "RAP", "OPS"),
        ]
    );
}

#[test]
fn test_parse_directory_listing_apache() {
    let html = r#"<!DOCTYPE HTML>
<html><body>
<a href="?C=N&amp;O=D">Name</a>
<a href="../">Parent Directory</a>
<a href="IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz">IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz</a>
<a href="IGS0OPSFIN_20240010000_01D_30S_CLK.CLK.gz">IGS0OPSFIN_20240010000_01D_30S_CLK.CLK.gz</a>
<a href="SHA512SUMS">SHA512SUMS</a>
</body></html>"#;
    let files = parse_directory_listing_html(html);
    assert!(files.iter().any(|f| f.contains("ORB.SP3")));
    assert!(files.iter().any(|f| f.contains("CLK.CLK")));
    // Query string and parent dir must not appear.
    assert!(!files.iter().any(|f| f.starts_with('?')));
    assert!(!files.contains(&"../".to_string()));
}

#[test]
fn test_parse_directory_listing_absolute_hrefs() {
    // Some servers (e.g. BKG) use absolute paths in hrefs.
    let html = r#"<a href="/root_ftp/IGS/products/2300/COD0MGXFIN_20240010000_01D_15M_ORB.SP3.gz">COD0MGXFIN_20240010000_01D_15M_ORB.SP3.gz</a>"#;
    let files = parse_directory_listing_html(html);
    assert!(files.iter().any(|f| f == "COD0MGXFIN_20240010000_01D_15M_ORB.SP3.gz"));
}

#[test]
fn test_parse_directory_listing_cddis_format() {
    let html = r#"<div class="archiveItemTextContainer">IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz 2.7M 2024-01-02</div>"#;
    let files = parse_directory_listing_html(html);
    assert!(files.iter().any(|f| f == "IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz"));
}

#[test]
fn parse_igs_filename_captures_sample_rate() {
    let (_p, _pr, _s, _y, _d, timespan, sample, _c, _e) =
        parse_igs_filename("COD0OPSRAP_20261560000_01D_05M_ORB.SP3.gz").unwrap();
    assert_eq!(timespan, "01D");
    assert_eq!(sample, "05M");
}

#[test]
fn catalogue_picks_finest_sample_rate() {
    let filenames: Vec<String> = vec![
        "COD0OPSRAP_20261560000_01D_05M_ORB.SP3.gz".into(),
        "COD0OPSRAP_20261560000_01D_05M_CLK.CLK.gz".into(),
        "COD0OPSRAP_20261560000_01D_30S_CLK.CLK.gz".into(),
        "COD0OPSRAP_20261560000_01D_01D_OSB.BIA.gz".into(),
    ];
    let cat = build_catalogue_from_filenames(&filenames);
    assert_eq!(cat.entries.len(), 1);
    let clk = cat.entries[0].files.get("CLK").expect("CLK file present");
    assert!(clk.ends_with("_30S_CLK.CLK"), "expected 30S CLK, got {clk}");
    assert!(!clk.ends_with(".gz"), "filename should not include .gz: {clk}");
}

#[test]
fn test_parse_directory_listing_cddis_nested_anchor() {
    // CDDIS often wraps the filename in an <a href="…"> inside the div.
    // A parser that stops at the first '<' would return empty text.
    let html = r#"<div class="archiveItemTextContainer"><a href="/archive/gnss/products/2300/IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz">IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz</a> 2.7M 2024-01-02</div>"#;
    let files = parse_directory_listing_html(html);
    assert!(
        files.iter().any(|f| f == "IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz"),
        "nested-anchor CDDIS format not parsed; got: {files:?}"
    );
}
