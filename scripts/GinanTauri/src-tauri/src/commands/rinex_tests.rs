//! Tests for `rinex`, split out to keep each file under 500 lines.

use std::io::BufRead;
use super::rinex::*;
use super::rinex_parse::*;
use super::rinex_time::epoch_to_gps;




#[test]
fn test_parse_observation_types() {
    // Test RINEX v3+ observation type parsing
    let line = "G   12    C1C    L1C    D1C    S1C    C1W    L1W    D1W    S1W    C2W    L2W    D2W    S2W";
    let obs_types = parse_observation_types(line);

    assert_eq!(obs_types.len(), 12);
    assert_eq!(obs_types[0], "C1C");
    assert_eq!(obs_types[1], "L1C");
    assert_eq!(obs_types[11], "S2W");
}

#[test]
fn test_parse_observation_types_v2() {
    // Test RINEX v2 observation type parsing
    let line = "     9    C1    L1    L2    C2    P2    S1    S2    D1    D2";
    let obs_types = parse_observation_types_v2(line);

    assert_eq!(obs_types.len(), 9);
    assert_eq!(obs_types[0], "C1");
    assert_eq!(obs_types[1], "L1");
    assert_eq!(obs_types[8], "D2");
}

#[test]
fn test_add_observation_codes() {
    let mut obs_codes = ConstellationObsCodes::default();
    let gps_codes = vec!["C1C".to_string(), "L1C".to_string()];
    let gal_codes = vec!["C1C".to_string(), "C5Q".to_string()];

    add_observation_codes(&mut obs_codes, 'G', gps_codes);
    add_observation_codes(&mut obs_codes, 'E', gal_codes);

    assert_eq!(obs_codes.gps.len(), 2);
    assert_eq!(obs_codes.gal.len(), 2);
    assert_eq!(obs_codes.glo.len(), 0);
    assert_eq!(obs_codes.gps[0], "C1C");
    assert_eq!(obs_codes.gal[1], "C5Q");
}

#[test]
fn test_system_letter_to_name() {
    assert_eq!(system_letter_to_name('G'), Some("GPS"));
    assert_eq!(system_letter_to_name('E'), Some("GAL"));
    assert_eq!(system_letter_to_name('R'), Some("GLO"));
    assert_eq!(system_letter_to_name('C'), Some("BDS"));
    assert_eq!(system_letter_to_name('J'), Some("QZS"));
    assert_eq!(system_letter_to_name('X'), None);
}

#[test]
fn test_continuation_line_all_codes_captured() {
    // GPS with 14 obs codes — spans 2 lines (max 13 per line in RINEX 3).
    // Lines use standard RINEX 3 fixed-width format: data in cols 0-59, label in cols 60-79.
    // Each obs code field is 4 chars (1 space + 3-char code).
    // sys_line:  I1(sys) + 2X + I3(count) + 1X + 13*(1X+A3) = 7+52=59 data chars, then label.
    // cont_line: 7 leading blanks + 1*(1X+A3) = 11 data chars, padded to 60, then label.
    let sys_line  = "G   14 C1C L1C D1C S1C C1W L1W D1W S1W C2W L2W D2W S2W C5QSYS / # / OBS TYPES\n";
    let cont_line = "       L5Q                                                 SYS / # / OBS TYPES\n";
    let eoh       = "                                                            END OF HEADER\n";

    let fake = format!("{}{}{}", sys_line, cont_line, eoh);
    let result = parse_header(std::io::BufReader::new(fake.as_bytes())).unwrap();
    assert_eq!(result.observation_codes.gps.len(), 14,
        "All 14 GPS obs codes should be captured across the continuation line");
    assert!(result.observation_codes.gps.contains(&"L5Q".to_string()),
        "L5Q from continuation line must be included");
    assert!(result.observation_codes.gps.contains(&"C1C".to_string()),
        "C1C from main line must be included");
}

#[test]
fn test_parse_obs_codes_from_line() {
    // Continuation line with leading spaces
    let line = "       L5Q    C2W    S2W                                         SYS / # / OBS TYPES";
    // We only care about the obs codes, not the label at the end (it won't pass len==3)
    let codes = parse_obs_codes_from_line(line);
    assert!(codes.contains(&"L5Q".to_string()));
    assert!(codes.contains(&"C2W".to_string()));
    assert!(codes.contains(&"S2W".to_string()));
    // Label words like "SYS" are 3 chars alphanumeric — they would slip through.
    // In practice the RINEX label column is positions 60-79, far past where we stop reading.
}

#[test]
fn test_scan_last_epoch_v3() {
    let data = "> 2024  1  1  0  0  0.0000000  0 10\n\
                 G01  23456789.123   123456789.123 7\n\
                 > 2024  1  1  0  0 30.0000000  0 10\n\
                 G01  23456791.789   123456791.789 7\n\
                 > 2024  1  1  0  1  0.0000000  0 10\n\
                 G01  23456792.012   123456792.012 7\n";
    let mut lines = std::io::BufReader::new(data.as_bytes()).lines();
    let result = scan_last_epoch(&mut lines, 3.0);
    assert_eq!(result, Some("2024-01-01 00:01:00".to_string()));
}

#[test]
fn test_scan_last_epoch_v2() {
    let data = concat!(
        " 24  1  1  0  0  0.0000000  0  5G01G02G03G05G07\n",
        "  23456789.123 7  123456789.123 8\n",
        " 24  1  1  0  0 30.0000000  0  5G01G02G03G05G07\n",
        "  23456790.456 7  123456790.456 8\n",
        " 24  1  1  0  1  0.0000000  0  5G01G02G03G05G07\n",
    );
    let mut lines = std::io::BufReader::new(data.as_bytes()).lines();
    let result = scan_last_epoch(&mut lines, 2.12);
    assert_eq!(result, Some("2024-01-01 00:01:00".to_string()));
}

#[test]
fn test_full_parse_without_time_of_last_obs() {
    let content = concat!(
        "     3.05           OBSERVATION DATA    M                   RINEX VERSION / TYPE\n",
        "TEST                                                        MARKER NAME\n",
        "  2024     1     1     0     0    0.0000000     GPS         TIME OF FIRST OBS\n",
        "                                                            END OF HEADER\n",
        "> 2024  1  1  0  0  0.0000000  0  2\n",
        "> 2024  1  1  0 30  0.0000000  0  2\n",
        "> 2024  1  1  1  0  0.0000000  0  2\n",
    );
    let reader = std::io::BufReader::new(content.as_bytes());
    let result = parse_header(reader).unwrap();
    assert_eq!(result.start_epoch, Some("2024-01-01 00:00:00".to_string()));
    assert_eq!(result.end_epoch, Some("2024-01-01 01:00:00".to_string()));
}

#[test]
fn epochs_in_gps_time_are_left_alone() {
    assert_eq!(epoch_to_gps("2024-01-01 00:00:00", "GPS"), "2024-01-01 00:00:00");
    assert_eq!(epoch_to_gps("2024-01-01 00:00:00", "GAL"), "2024-01-01 00:00:00");
}

#[test]
fn glonass_time_is_shifted_to_gps_time() {
    // GLO = UTC + 3 h, and GPS = UTC + 18 s since 2017.
    assert_eq!(epoch_to_gps("2024-01-01 03:00:00", "GLO"), "2024-01-01 00:00:18");
    // Before the 2017 leap second the gap was 17 s.
    assert_eq!(epoch_to_gps("2016-06-01 03:00:00", "GLO"), "2016-06-01 00:00:17");
}

#[test]
fn beidou_time_is_shifted_to_gps_time() {
    assert_eq!(epoch_to_gps("2024-01-01 23:59:50", "BDT"), "2024-01-02 00:00:04");
}

#[test]
fn a_glonass_header_window_comes_out_in_gps_time() {
    let content = concat!(
        "     3.05           OBSERVATION DATA    R                   RINEX VERSION / TYPE\n",
        "  2024     1     1     3     0    0.0000000     GLO         TIME OF FIRST OBS\n",
        "  2024     1     1     4     0    0.0000000     GLO         TIME OF LAST OBS\n",
        "                                                            END OF HEADER\n",
    );
    let result = parse_header(std::io::BufReader::new(content.as_bytes())).unwrap();
    assert_eq!(result.time_system.as_deref(), Some("GLO"));
    assert_eq!(result.start_epoch, Some("2024-01-01 00:00:18".to_string()));
    assert_eq!(result.end_epoch, Some("2024-01-01 01:00:18".to_string()));
}

/// End-to-end: a .crx.gz must be gunzipped, Hatanaka-decoded, and written
/// out as a plain .rnx matching the RNXCMP reference output.
#[test]
fn prepare_rinex_input_unpacks_and_converts_crx_gz() {
    let dir = std::env::temp_dir().join("ginan_prepare_crx_gz_test");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let src = dir.join("sample.crx.gz");
    std::fs::write(&src, include_bytes!("../../fixtures/sample.crx.gz")).unwrap();

    let mut logged: Vec<String> = Vec::new();
    let mut progressed: Vec<String> = Vec::new();
    let out = prepare_rinex_file(
        src.to_str().unwrap(),
        &mut |m| logged.push(m),
        &mut |m| progressed.push(m),
    )
    .expect("prepare_rinex_file failed");

    assert!(out.ends_with("sample.rnx"), "unexpected output path: {out}");

    // The user must see what happened, not a silent pause.
    let log = logged.join("
");
    assert!(log.contains("Decompressing gzip"), "missing gzip line:
{log}");
    assert!(log.contains("Converting Hatanaka"), "missing convert line:
{log}");
    assert!(log.contains("Wrote sample.rnx"), "missing wrote line:
{log}");
    assert!(
        progressed.iter().any(|m| m.contains('%')),
        "no percentage progress reported: {progressed:?}"
    );
    let produced = std::fs::read_to_string(&out).unwrap();
    let expected = include_str!("../../fixtures/sample.rnx");

    let p: Vec<&str> = produced.lines().collect();
    let e: Vec<&str> = expected.lines().map(|l| l.trim_end_matches('\r')).collect();
    assert_eq!(p.len(), e.len(), "line count differs");
    for (n, (a, b)) in p.iter().zip(e.iter()).enumerate() {
        assert_eq!(a, b, "line {} differs", n + 1);
    }

    // The converted file must parse as real RINEX, not just look like it.
    let parsed = parse_rinex_path(&out).expect("parse_rinex failed");
    assert_eq!(parsed.rinex_version, Some(3.01));

    let _ = std::fs::remove_dir_all(&dir);
}

/// A plain .rnx must be handed back untouched — no stray conversion files.
#[test]
fn prepare_rinex_input_passes_through_plain_rinex() {
    let dir = std::env::temp_dir().join("ginan_prepare_plain_test");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let src = dir.join("plain.rnx");
    std::fs::write(&src, include_str!("../../fixtures/sample.rnx")).unwrap();

    let out = prepare_rinex_file(src.to_str().unwrap(), &mut |_| {}, &mut |_| {})
        .expect("prepare_rinex_file failed");

    assert_eq!(out, src.to_str().unwrap());
    let _ = std::fs::remove_dir_all(&dir);
}
