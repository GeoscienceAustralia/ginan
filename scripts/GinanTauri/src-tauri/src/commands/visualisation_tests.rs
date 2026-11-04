//! Tests for `visualisation`, split out to keep each file under 500 lines.

use std::fs::File;
use std::path::{Path, PathBuf};
use super::visualisation::*;
use super::visualisation_spp::SolutionFormat;
use std::io::Write;




/// Header verbatim from src/cpp/common/pos.cpp, trimmed to the lines the
/// parser has to survive, plus three epochs.
const SAMPLE_POS: &str = "\
PBO Station Position Time Series. Reference Frame : IGS20
Format Version: 2.0.0
4-character ID: ALIC
First Epoch   : 2020-01-01T00:00:00.00
XYZ Reference position :  -4052052.739026 4212835.982224 -2545104.590410 (IGS20)
Start Field Description
dN                      Difference in North component from NEU reference position, meters
End Field Description
*YYYY-MM-DDTHH:MM:SS.SSS YYYY.YYYYYYYYY        X             Y              Z           Sx         Sy        Sz      Rxy     Rxz     Ryz        NLat            Elong        Height         dN          dE          dU        Sn       Se         Su       Rne     Rnu     Reu  soln
 2020-01-01T00:00:00.000 2020.000000000 -4052052.73902 4212835.98222 -2545104.59041   0.00100   0.00100   0.00100   0.100   0.100   0.100    -23.6701036000   133.8855100000    603.34000     0.01000     0.02000     0.03000   0.00100   0.00200   0.00300   0.100   0.100   0.100 ginan
 2020-01-01T00:00:30.000 2020.000000951 -4052052.73802 4212835.98322 -2545104.58941   0.00100   0.00100   0.00100   0.100   0.100   0.100    -23.6701036000   133.8855100000    603.34100     0.03000     0.04000     0.09000   0.00100   0.00200   0.00300   0.100   0.100   0.100 ginan
 2020-01-01T00:01:00.000 2020.000001902 -4052052.73702 4212835.98422 -2545104.58841   0.00100   0.00100   0.00100   0.100   0.100   0.100    -23.6701036000   133.8855100000    603.34200     0.02000     0.03000     0.06000   0.00100   0.00200   0.00300   0.100   0.100   0.100 ginan
";

fn unique_tmp_dir(label: &str) -> PathBuf {
    let mut p = std::env::temp_dir();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    p.push(format!("ginan_pos_test_{}_{}_{}", label, std::process::id(), nanos));
    std::fs::create_dir_all(&p).expect("create tmp dir");
    p
}

fn write_pos(dir: &Path, name: &str, body: &str) -> String {
    let path = dir.join(name);
    let mut f = File::create(&path).unwrap();
    f.write_all(body.as_bytes()).unwrap();
    path.to_string_lossy().into_owned()
}

#[test]
fn parses_header_and_epochs() {
    let dir = unique_tmp_dir("parse");
    let path = write_pos(&dir, "ALIC.pos", SAMPLE_POS);

    let s = load_pos_series(path, None).expect("parse");
    assert_eq!(s.station.as_deref(), Some("ALIC"));
    assert_eq!(s.total_epochs, 3);
    assert_eq!(s.returned_epochs, 3);
    assert!(!s.decimated);
    assert_eq!(s.skipped_lines, 0);
    assert_eq!(s.dn, vec![0.01, 0.03, 0.02]);
    assert_eq!(s.de, vec![0.02, 0.04, 0.03]);
    assert_eq!(s.du, vec![0.03, 0.09, 0.06]);
    assert_eq!(s.sn, vec![0.001, 0.001, 0.001]);
    assert_eq!(s.lat, vec![-23.6701036, -23.6701036, -23.6701036]);
    assert_eq!(s.lon, vec![133.88551, 133.88551, 133.88551]);
    assert!(s.hpl.is_empty() && s.vpl.is_empty(), ".pos has no protection levels");
    // 30 s cadence, so the span is a minute of milliseconds.
    assert_eq!(s.t[2] - s.t[0], 60_000.0);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn equal_sigmas_give_plain_mean_and_rms() {
    let dir = unique_tmp_dir("stats");
    let path = write_pos(&dir, "ALIC.pos", SAMPLE_POS);

    let s = load_pos_series(path, None).expect("parse");
    // Uniform sigmas ⇒ the weighted mean is the arithmetic mean.
    assert!((s.stats.dn.weighted_mean - 0.02).abs() < 1e-12);
    // Population (not sample) spread of 0.01/0.03/0.02 about 0.02.
    assert!((s.stats.dn.std_dev - (2.0f64 / 3.0).sqrt() * 0.01).abs() < 1e-12);
    let expect_rms = ((0.01f64.powi(2) + 0.03f64.powi(2) + 0.02f64.powi(2)) / 3.0).sqrt();
    assert!((s.stats.dn.rms - expect_rms).abs() < 1e-12);
    assert!((s.stats.dn.min - 0.01).abs() < 1e-12);
    assert!((s.stats.dn.max - 0.03).abs() < 1e-12);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tighter_sigma_pulls_the_weighted_mean() {
    // Two epochs at ±1 m; the second is 10× more precise, so the weighted
    // mean must sit close to it while the plain mean would be zero.
    let mut body = String::from("4-character ID: TEST\n*legend\n");
    for (i, (dn, sn)) in [(-1.0, 0.1), (1.0, 0.01)].iter().enumerate() {
        body.push_str(&format!(
            " 2020-01-01T00:00:{:02}.000 2020.0 0 0 0 0 0 0 0 0 0 0 0 0 {dn} 0 0 {sn} 1 1 0 0 0 ginan\n",
            i * 30
        ));
    }
    let dir = unique_tmp_dir("weights");
    let path = write_pos(&dir, "TEST.pos", &body);

    let s = load_pos_series(path, None).expect("parse");
    assert_eq!(s.total_epochs, 2);
    assert!(s.stats.dn.weighted_mean > 0.9, "got {}", s.stats.dn.weighted_mean);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn decimates_to_the_budget_and_keeps_the_last_epoch() {
    let mut body = String::from("4-character ID: TEST\n*legend\n");
    for i in 0..100 {
        body.push_str(&format!(
            " 2020-01-01T00:00:00.{:03} 2020.0 0 0 0 0 0 0 0 0 0 0 0 0 {i} 0 0 0.001 0.001 0.001 0 0 0 ginan\n",
            i
        ));
    }
    let dir = unique_tmp_dir("decimate");
    let path = write_pos(&dir, "TEST.pos", &body);

    let s = load_pos_series(path, Some(10)).expect("parse");
    assert_eq!(s.total_epochs, 100);
    assert!(s.decimated);
    assert!(s.returned_epochs <= 11, "got {}", s.returned_epochs);
    assert_eq!(*s.dn.last().unwrap(), 99.0);
    // Statistics ignore decimation: mean of 0..99 is 49.5.
    assert!((s.stats.dn.weighted_mean - 49.5).abs() < 1e-9);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn counts_corrupt_data_lines_without_failing() {
    let mut body = SAMPLE_POS.to_string();
    body.push_str(" 2020-01-01T00:01:30.000 2020.0 truncated line\n");
    body.push_str(" 2020-01-01T00:02:00.000 2020.0 0 0 0 0 0 0 0 0 0 0 0 0 nan 0 0 0 0 0 0 0 0 ginan\n");
    let dir = unique_tmp_dir("corrupt");
    let path = write_pos(&dir, "ALIC.pos", &body);

    let s = load_pos_series(path, None).expect("parse");
    assert_eq!(s.total_epochs, 3);
    // The truncated line is short of 24 fields so it never reaches the
    // timestamp check; the NaN one does and is reported.
    assert_eq!(s.skipped_lines, 1);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn empty_file_is_an_error_not_an_empty_plot() {
    let dir = unique_tmp_dir("empty");
    let path = write_pos(&dir, "ALIC.pos", "4-character ID: ALIC\n*legend\n");

    let err = load_pos_series(path, None).expect_err("expected an error");
    assert!(err.contains("No position epochs"), "got {err}");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn lists_pos_files_forward_before_smoothed() {
    let dir = unique_tmp_dir("list");
    write_pos(&dir, "ALIC_smoothed.pos", SAMPLE_POS);
    write_pos(&dir, "ALIC.POS", SAMPLE_POS);
    write_pos(&dir, "notes.txt", "ignore me");
    std::fs::create_dir_all(dir.join("visual")).unwrap();
    write_pos(&dir.join("visual"), "nested.pos", SAMPLE_POS);

    let files = list_pos_files(dir.to_string_lossy().into_owned()).expect("list");
    let names: Vec<&str> = files.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names, vec!["ALIC.POS", "ALIC_smoothed.pos"]);
    assert!(!files[0].smoothed);
    assert!(files[1].smoothed);
    assert!(files.iter().all(|f| f.has_epochs));

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn empty_and_header_only_pos_files_are_flagged() {
    let dir = unique_tmp_dir("list_empty");
    write_pos(&dir, "ALIC.POS", SAMPLE_POS);
    write_pos(&dir, "ALIC_smoothed.POS", "");
    write_pos(&dir, "BOGT.POS", "4-character ID: BOGT
*legend
");

    let files = list_pos_files(dir.to_string_lossy().into_owned()).expect("list");
    let flags: Vec<(&str, bool)> = files.iter().map(|f| (f.name.as_str(), f.has_epochs)).collect();
    assert_eq!(flags, vec![("ALIC.POS", true), ("BOGT.POS", false), ("ALIC_smoothed.POS", false)]);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn missing_directory_is_an_error() {
    let err = list_pos_files("/definitely/not/a/real/dir".to_string())
        .expect_err("expected an error");
    assert!(err.contains("does not exist"), "got {err}");
}

/// `fixtures/sample_sbas.spp`: three epochs of an SBAS DFMC run with no a
/// priori position, as `writeSPP` formats them.
const SAMPLE_SPP: &str = include_str!("../../fixtures/sample_sbas.spp");

#[test]
fn lists_sbas_spp_files_with_pos_files() {
    let dir = unique_tmp_dir("list_spp");
    write_pos(&dir, "ALIC.POS", SAMPLE_POS);
    write_pos(&dir, "ALIC_202630001.SPP", SAMPLE_SPP);

    let files = list_pos_files(dir.to_string_lossy().into_owned()).expect("list");
    let found: Vec<(&str, SolutionFormat, bool)> =
        files.iter().map(|f| (f.name.as_str(), f.format, f.has_epochs)).collect();
    assert_eq!(
        found,
        vec![("ALIC.POS", SolutionFormat::Pos, true), ("ALIC_202630001.SPP", SolutionFormat::Spp, true)]
    );

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn loads_an_sbas_spp_file_as_offsets_from_its_first_epoch() {
    let dir = unique_tmp_dir("load_spp");
    let path = write_pos(&dir, "ALIC_202630001.SPP", SAMPLE_SPP);

    let s = load_pos_series(path, None).expect("parse");
    assert_eq!(s.format, SolutionFormat::Spp);
    assert_eq!(s.total_epochs, 3);
    assert_eq!(s.skipped_lines, 0);
    assert_eq!((s.dn[0], s.de[0], s.du[0]), (0.0, 0.0, 0.0));
    assert!(s.dn[1].abs() > 0.1 && s.dn[1].abs() < 1.0, "dn={}", s.dn[1]);
    assert!(s.sn.iter().all(|&v| v == 0.0));
    assert_eq!(s.lat[2], -23.6701903630);
    assert_eq!(s.hpl, vec![5.1, 5.2, 5.0]);
    assert_eq!(s.vpl, vec![8.2, 8.1, 8.3]);

    let _ = std::fs::remove_dir_all(&dir);
}
