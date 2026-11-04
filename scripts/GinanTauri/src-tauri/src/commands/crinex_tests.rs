//! Tests for `crinex`, split out to keep each file under 500 lines.



use super::crinex::*;

/// Round-trip against the RNXCMP reference fixtures: sample.crx must
/// decompress to exactly sample.rnx.
#[test]
fn decompresses_reference_sample() {
    let crx = include_str!("../../fixtures/sample.crx");
    let expected = include_str!("../../fixtures/sample.rnx");
    let got = crx_to_rnx(crx).expect("decompression failed");

    let got_lines: Vec<&str> = got.lines().collect();
    let exp_lines: Vec<&str> = expected.lines().map(|l| l.trim_end_matches('\r')).collect();

    assert_eq!(
        got_lines.len(),
        exp_lines.len(),
        "line count differs\n--- got ---\n{got}\n--- expected ---\n{expected}"
    );
    for (n, (g, e)) in got_lines.iter().zip(exp_lines.iter()).enumerate() {
        assert_eq!(g, e, "line {} differs\n got: {:?}\n exp: {:?}", n + 1, g, e);
    }
}

#[test]
fn rejects_non_crinex_input() {
    let err = crx_to_rnx("     3.04           OBSERVATION DATA\n").unwrap_err();
    assert!(err.contains("not a CRINEX file"), "unexpected error: {err}");
}

#[test]
fn formats_values_below_one_without_leading_zero() {
    // RINEX writes ".123" / "-.123", not "0.123".
    assert_eq!(format_obs(123).trim(), ".123");
    assert_eq!(format_obs(-123).trim(), "-.123");
    assert_eq!(format_obs(62000).trim(), "62.000");
    assert_eq!(format_obs(130321269801).trim(), "130321269.801");
    assert_eq!(format_obs(130321269801).len(), 14);
}

/// A satellite line whose last observation has no flag string ends right
/// after the value, with no separator to close the field. Reading that as
/// blank loses the observation, and the next epoch then reports
/// "new data sequence without arc initialisation" because the field looks
/// like it was previously absent.
#[test]
fn split_data_line_keeps_final_field_without_trailing_flags() {
    let (fields, flags) = split_data_line(b"3&1 3&2", 2);
    assert_eq!(fields, vec![&b"3&1"[..], &b"3&2"[..]]);
    assert_eq!(flags, b"");

    // With flags present the last field is separator-terminated.
    let (fields, flags) = split_data_line(b"3&1 3&2 08", 2);
    assert_eq!(fields, vec![&b"3&1"[..], &b"3&2"[..]]);
    assert_eq!(flags, b"08");

    // A short line: the present field survives, later ones are blank.
    let (fields, flags) = split_data_line(b"3&1", 3);
    assert_eq!(fields, vec![&b"3&1"[..], &b""[..], &b""[..]]);
    assert_eq!(flags, b"");

    // Consecutive separators mean genuinely blank observations.
    let (fields, flags) = split_data_line(b"3&1  3&3 0808", 3);
    assert_eq!(fields, vec![&b"3&1"[..], &b""[..], &b"3&3"[..]]);
    assert_eq!(flags, b"0808");
}

/// Multi-epoch round trip: the second epoch continues each arc with bare
/// differences, which only works if epoch one's values were read in full.
///
/// Note the second epoch line is a text *difference* (leading blanks, `&`
/// to blank a character) — only the first epoch line starts with `>`,
/// which resets every arc. Its data line carries no trailing flags, which
/// is the case that loses the last observation if read wrongly (see above).
#[test]
fn decompresses_multiple_epochs_without_trailing_flags() {
    let crx = "\
3.0                 COMPACT RINEX FORMAT                    CRINEX VERS   / TYPE
RNX2CRX ver.4.0.8                       08-Apr-21 06:56     CRINEX PROG / DATE
 3.01           OBSERVATION DATA    M (MIXED)           RINEX VERSION / TYPE
G    2 C1C L1C                                              SYS / # / OBS TYPES
  2010     3     5     0     0     0.0000000     GPS        TIME OF FIRST OBS
                                                        END OF HEADER
> 2010 03 05 00 00 30.0000000  0 1       G13

1&24799319672 1&130321269801
            01 &

1000 2000
";
    let out = crx_to_rnx(crx).expect("decompression failed");
    let data: Vec<&str> = out.lines().filter(|l| l.starts_with("G13")).collect();
    assert_eq!(data.len(), 2, "expected two epochs:\n{out}");
    // Epoch 1 values, then epoch 1 + first difference.
    assert!(data[0].contains("24799319.672"), "epoch 1: {}", data[0]);
    assert!(data[0].contains("130321269.801"), "epoch 1: {}", data[0]);
    assert!(data[1].contains("24799320.672"), "epoch 2: {}", data[1]);
    assert!(data[1].contains("130321271.801"), "epoch 2: {}", data[1]);
}

#[test]
fn repair_applies_text_difference() {
    let mut s = b"abcdef".to_vec();
    repair(&mut s, b" X &Z");
    // space keeps, 'X' replaces, '&' blanks, 'Z' replaces.
    assert_eq!(String::from_utf8(s).unwrap(), "aXc Zf");
}
