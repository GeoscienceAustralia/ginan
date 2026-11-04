//! Tests for `bia`, split out to keep each file under 500 lines.



use super::bia::*;
use chrono::NaiveDate;
use super::*;

const SAMPLE_BIA: &str = "\
%=BIA 1.00 IGS 2024:001:00000 IGS 2024:001:00000 2024:001:86399 S
+BIAS/SOLUTION
*BIAS SVN_ PRN STATION__ OBS1 OBS2 BIAS_START____ BIAS_END______ UNIT __ESTIMATED_VALUE__ _STD_DEV___
 OSB  G001 G01           C1C       2024:001:00000 2024:001:86399 ns    0.000000000000000  0.000000000
 OSB  G001 G01           C1W       2024:001:00000 2024:001:86399 ns    0.000000000000000  0.000000000
 OSB  G001 G01           L1C       2024:001:00000 2024:001:86399 cyc   0.000000000000000  0.000000000
 OSB  E001 E01           C1C       2024:001:00000 2024:001:86399 ns    0.000000000000000  0.000000000
 OSB  E001 E01           C5Q       2024:001:00000 2024:001:86399 ns    0.000000000000000  0.000000000
 DSB  G001     -------- C1C  C1W  2024:001:00000 2024:001:86399 ns   -1.234000000000000  0.012000000
-BIAS/SOLUTION
%=ENDBIA
";
#[test]
fn test_parse_bia_osb_lines() {
    let result = parse_bia_content(SAMPLE_BIA);
    // GPS codes
    assert!(result.gps.contains(&"C1C".to_string()), "GPS C1C missing");
    assert!(result.gps.contains(&"C1W".to_string()), "GPS C1W missing");
    assert!(result.gps.contains(&"L1C".to_string()), "GPS L1C missing");
    // Galileo codes
    assert!(result.gal.contains(&"C1C".to_string()), "GAL C1C missing");
    assert!(result.gal.contains(&"C5Q".to_string()), "GAL C5Q missing");
    // DSB adds both reference codes for GPS
    assert!(result.gps.contains(&"C1C".to_string()), "GPS DSB C1C missing");
}

#[test]
fn test_is_valid_obs_code() {
    assert!(is_valid_obs_code("C1C"));
    assert!(is_valid_obs_code("L5Q"));
    assert!(is_valid_obs_code("D2W"));
    assert!(!is_valid_obs_code("G01")); // PRN, not obs code
    assert!(!is_valid_obs_code("C1"));  // too short
    assert!(!is_valid_obs_code("C1CC")); // too long
}

#[test]
fn bia_cache_key_is_sanitised() {
    assert_eq!(bia_cache_filename("igs", "rap", "ops"), "IGS_RAP_OPS.json");
    // Trims/strips whitespace and non-alphanumerics, uppercases.
    assert_eq!(
        bia_cache_filename(" cod ", "fin", "mgx-1"),
        "COD_FIN_MGX1.json"
    );
    // Already-uppercase, alphanumeric passes through.
    assert_eq!(
        bia_cache_filename("COD", "R03", "OPS"),
        "COD_R03_OPS.json"
    );
}

#[test]
fn bia_cache_hit_returns_cached_codes() {
    let tmp = std::env::temp_dir().join(format!(
        "ginan_bia_cache_hit_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    let codes = BiaCodePriorities {
        gps: vec!["C1C".into(), "L1C".into()],
        gal: vec!["C5Q".into()],
        glo: vec![],
        bds: vec![],
        qzs: vec![],
    };
    write_bia_cache(&tmp, "IGS", "RAP", "OPS", &codes).unwrap();

    let got = read_bia_cache(&tmp, "igs", "rap", "ops")
        .unwrap()
        .expect("expected a cache hit");
    assert_eq!(got.gps, vec!["C1C".to_string(), "L1C".to_string()]);
    assert_eq!(got.gal, vec!["C5Q".to_string()]);
    assert!(got.glo.is_empty());

    // Missing combo returns Ok(None).
    let miss = read_bia_cache(&tmp, "FOO", "BAR", "BAZ").unwrap();
    assert!(miss.is_none());

    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn bia_cache_writes_json() {
    let tmp = std::env::temp_dir().join(format!(
        "ginan_bia_cache_write_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&tmp);

    let codes = BiaCodePriorities {
        gps: vec!["C1W".into()],
        gal: vec![],
        glo: vec![],
        bds: vec![],
        qzs: vec!["L2L".into()],
    };
    // Helper creates the directory if missing.
    write_bia_cache(&tmp, "IGS", "FIN", "OPS", &codes).unwrap();

    let path = tmp.join("IGS_FIN_OPS.json");
    assert!(path.is_file(), "cache file should exist at {}", path.display());

    let raw = std::fs::read_to_string(&path).unwrap();
    let parsed: BiaCodePriorities = serde_json::from_str(&raw).unwrap();
    assert_eq!(parsed.gps, vec!["C1W".to_string()]);
    assert_eq!(parsed.qzs, vec!["L2L".to_string()]);
    // Pretty JSON contains a newline.
    assert!(raw.contains('\n'), "expected pretty-printed JSON");

    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn mid_week_date_is_deterministic_wednesday() {
    // GPS week 0 starts Sunday 1980-01-06, so +3 days = Wednesday 1980-01-09.
    let d = mid_week_date(0);
    assert_eq!(d, NaiveDate::from_ymd_opt(1980, 1, 9).unwrap());
}

// ─── catalogue-discovered filename tests ─────────────────

// ─── real-layout fixtures ─────────────────────
//
// Records below are trimmed excerpts of public IGS analysis-centre products (COD, GFZ and
// ESA, GPS week 2392 / 2350). The parser reads fixed columns, so the lines are kept
// byte-for-byte; only the number of records is cut.

/// COD/GFZ style: satellite and station OSB records, code block then phase block.
const OSB_CODE_AND_PHASE_BIA: &str = "\
%=BIA 1.00 COD 2025:318:04464 MGX 2025:313:00000 2025:314:00000 A 00000011
+BIAS/SOLUTION
*BIAS SVN_ PRN STATION__ OBS1 OBS2 BIAS_START____ BIAS_END______ UNIT __ESTIMATED_VALUE____ _STD_DEV___
 OSB  G080 G01           C1W       2025:313:00000 2025:314:00000 ns                 -5.2127      0.0000
 OSB  G080 G01           C2W       2025:313:00000 2025:314:00000 ns                 -8.5850      0.0000
 OSB  E211 E02           C5Q       2025:313:00000 2025:314:00000 ns                 -2.0621      0.0000
 OSB  J002 J02           C5X       2025:313:00000 2025:314:00000 ns                  0.0396      0.0171
 OSB  R803 R26 ZAMB      C2P       2025:313:00000 2025:314:00000 ns                 -0.6698      0.1209
 OSB  C005 C06           C2I       2025:313:00000 2025:314:03600 ns                -22.1606      1.0179
 OSB  G080 G01           L1C       2025:313:00000 2025:314:00000 ns                -0.94577     0.00000
 OSB  G080 G01           L2W       2025:313:00000 2025:314:00000 ns                -1.50455     0.00000
 OSB  E211 E02           L5Q       2025:313:00000 2025:314:00000 ns                -0.36473     0.00000
 OSB  J002 J02           L5X       2025:313:00000 2025:314:00000 ns                -0.20983     0.00000
 OSB  C005 C06           L2I       2025:313:00000 2025:314:03600 ns                 21.7773      1.0187
-BIAS/SOLUTION
%=ENDBIA
";

/// ESA style: station DSB (code) and ISB (phase) records. The second signal of each
/// pair is in the OBS2 field, and for these records that is the only place L2/L5 appear.
const DSB_ISB_BIA: &str = "\
%=BIA 1.00 ESA 2025:031:00000 IGS 2025:024:86382 2025:026:00282 C 00000005
+BIAS/SOLUTION
*BIAS SVN_ PRN STATION__ OBS1 OBS2 BIAS_START____ BIAS_END______ UNIT __ESTIMATED_VALUE____ _STD_DEV___
 DSB  G    G   ABMF      C1W  C2W  2025:024:86382 2025:025:86382 ns   0.802114520248342E+01 .165607E+00
 DSB  E    E   ABMF      C1C  C5Q  2025:024:86382 2025:025:86382 ns   0.985595497214988E+00 .204891E+00
 DSB  R730 R01 ABMF      C1P  C2P  2025:024:86382 2025:025:86382 ns   -.968429955088642E+01 .203730E+02
 ISB  E    E   ADIS      L1C  L5Q  2025:024:86382 2025:026:00282 ns   -.374751994961139E+02 .800190E-02
 ISB  R730 R01 ADIS      L1C  L2P  2025:024:86382 2025:026:00282 ns   0.128015819461697E+02 .704013E+00
-BIAS/SOLUTION
%=ENDBIA
";

fn strs(v: &[&str]) -> Vec<String> {
    v.iter().map(|s| s.to_string()).collect()
}

#[test]
fn parse_collects_phase_codes_from_osb_phase_block() {
    let r = parse_bia_content(OSB_CODE_AND_PHASE_BIA);
    assert_eq!(r.gps, strs(&["C1W", "C2W", "L1C", "L2W"]));
    assert_eq!(r.gal, strs(&["C5Q", "L5Q"]));
    assert_eq!(r.qzs, strs(&["C5X", "L5X"]));
    assert_eq!(r.bds, strs(&["C2I", "L2I"]));
    // GLONASS only has a station code record here, so no phase code may be invented.
    assert_eq!(r.glo, strs(&["C2P"]));
}

#[test]
fn parse_reads_second_signal_of_dsb_and_isb_records() {
    let r = parse_bia_content(DSB_ISB_BIA);
    // C2W / C5Q / C2P / L5Q / L2P appear only in the OBS2 field.
    assert_eq!(r.gps, strs(&["C1W", "C2W"]));
    assert_eq!(r.gal, strs(&["C1C", "C5Q", "L1C", "L5Q"]));
    assert_eq!(r.glo, strs(&["C1P", "C2P", "L1C", "L2P"]));
    assert!(r.bds.is_empty() && r.qzs.is_empty());
}

#[test]
fn osb_records_leave_obs2_empty_rather_than_reading_the_start_epoch() {
    // The OBS2 columns of an OSB record are blank and the epoch starts right after them;
    // neither a space nor part of "2025:313:00000" may be taken for a code.
    let r = parse_bia_content(OSB_CODE_AND_PHASE_BIA);
    let all: Vec<&String> = [&r.gps, &r.gal, &r.glo, &r.bds, &r.qzs]
        .into_iter()
        .flatten()
        .collect();
    assert!(all.iter().all(|c| c.len() == 3 && c.as_bytes()[1].is_ascii_digit()));
}

#[test]
fn parse_keeps_obs2_in_the_svn_only_layout() {
    // Older writers drop the PRN column and OBS1 moves to col 24; OBS2 moves with it.
    let content = "\
+BIAS/SOLUTION
 DSB  G001     -------- C1C  C2W  2024:001:00000 2024:001:86399 ns   -1.234000000000000  0.012000000
 DSB  E001     -------- L1C  L5Q  2024:001:00000 2024:001:86399 ns   -1.234000000000000  0.012000000
-BIAS/SOLUTION
";
    let r = parse_bia_content(content);
    assert_eq!(r.gps, strs(&["C1C", "C2W"]));
    assert_eq!(r.gal, strs(&["L1C", "L5Q"]));
}

#[test]
fn disk_scan_reads_a_bia_that_is_not_valid_utf8() {
    // CODE's files carry a latin-1 "ä" (byte 0xE4) in the reference text, so a strict
    // UTF-8 read would reject the whole file and report no codes at all.
    let tmp = std::env::temp_dir().join(format!("ginan_bia_latin1_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    let mut bytes = b"%=BIA 1.00 COD 2025:318:04464 MGX 2025:313:00000 2025:314:00000 A 00000001\n".to_vec();
    bytes.extend_from_slice(b"+FILE/COMMENT\n A. J\xE4ggi, 2019\n-FILE/COMMENT\n");
    bytes.extend_from_slice(
        b"+BIAS/SOLUTION\n OSB  G080 G01           L1C       2025:313:00000 2025:314:00000 ns                -0.94577     0.00000\n-BIAS/SOLUTION\n",
    );
    std::fs::write(tmp.join("COD0MGXFIN_20253130000_01D_01D_OSB.BIA"), &bytes).unwrap();

    let got = read_bia_codes_from_disk("COD", "FIN", "MGX", tmp.to_str().unwrap())
        .expect("a latin-1 byte in the header text must not fail the read");
    assert_eq!(got.gps, strs(&["L1C"]));

    let _ = std::fs::remove_dir_all(&tmp);
}
