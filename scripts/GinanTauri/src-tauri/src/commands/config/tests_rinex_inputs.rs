//! Multi-file RINEX input tests, split out of tests.rs to keep
//! each file under 500 lines.

use super::generate::*;
use super::tests_support::*;
use super::*;

fn generated(inputs: &ConfigInputs, scope: PathScope) -> serde_yaml::Value {
    let yaml = generate_config_yaml(inputs, None, scope).expect("config generates");
    serde_yaml::from_str(&yaml).expect("output must parse")
}

fn rnx_list(config: &serde_yaml::Value) -> Vec<&str> {
    config["inputs"]["gnss_observations"]["rnx_inputs"]
        .as_sequence()
        .expect("rnx_inputs sequence")
        .iter()
        .map(|v| v.as_str().expect("string entry"))
        .collect()
}

fn root(config: &serde_yaml::Value) -> Option<&str> {
    config["inputs"]["gnss_observations"]["gnss_observations_root"].as_str()
}

#[test]
fn single_file_output_is_unchanged() {
    let config = generated(&sample_inputs(), PathScope::Local);
    assert_eq!(root(&config), Some("/home/tester/obs"));
    assert_eq!(rnx_list(&config), ["ALIC00AUS_R_20240010000_01D_30S_MO.rnx"]);
}

#[test]
fn rinex_paths_alone_adds_nothing_for_a_single_file() {
    // The frontend sends the primary in both fields; that must not read as two
    // files.
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec![inputs.rinex_path.clone()];
    let config = generated(&inputs, PathScope::Local);
    assert_eq!(rnx_list(&config), ["ALIC00AUS_R_20240010000_01D_30S_MO.rnx"]);
}

#[test]
fn several_files_in_one_directory_are_all_listed_under_one_root() {
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec![
        "/home/tester/obs/ALIC00AUS_R_20240010000_01D_30S_MO.rnx".to_string(),
        "/home/tester/obs/ALIC00AUS_R_20240020000_01D_30S_MO.rnx".to_string(),
        "/home/tester/obs/ALIC00AUS_R_20240030000_01D_30S_MO.rnx".to_string(),
    ];
    let config = generated(&inputs, PathScope::Local);

    assert_eq!(root(&config), Some("/home/tester/obs"));
    assert_eq!(
        rnx_list(&config),
        [
            "ALIC00AUS_R_20240010000_01D_30S_MO.rnx",
            "ALIC00AUS_R_20240020000_01D_30S_MO.rnx",
            "ALIC00AUS_R_20240030000_01D_30S_MO.rnx",
        ],
    );
}

#[test]
fn primary_leads_whatever_order_rinex_paths_arrives_in() {
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec![
        "/home/tester/obs/DARW00AUS_R_20240010000_01D_30S_MO.rnx".to_string(),
        inputs.rinex_path.clone(),
    ];
    let config = generated(&inputs, PathScope::Local);
    assert_eq!(
        rnx_list(&config),
        [
            "ALIC00AUS_R_20240010000_01D_30S_MO.rnx",
            "DARW00AUS_R_20240010000_01D_30S_MO.rnx",
        ],
    );
}

#[test]
fn files_from_another_directory_keep_their_full_path() {
    // PEA has one observation root, and it does not prefix absolute entries.
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec!["/mnt/other/DARW00AUS_R_20240010000_01D_30S_MO.rnx".to_string()];
    let config = generated(&inputs, PathScope::Local);

    assert_eq!(root(&config), Some("/home/tester/obs"));
    assert_eq!(
        rnx_list(&config),
        [
            "ALIC00AUS_R_20240010000_01D_30S_MO.rnx",
            "/mnt/other/DARW00AUS_R_20240010000_01D_30S_MO.rnx",
        ],
    );
}

#[test]
fn blank_and_repeated_entries_are_dropped() {
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec![
        String::new(),
        "  ".to_string(),
        inputs.rinex_path.clone(),
        "/home/tester/obs/ALIC00AUS_R_20240020000_01D_30S_MO.rnx".to_string(),
        "/home/tester/obs/ALIC00AUS_R_20240020000_01D_30S_MO.rnx".to_string(),
    ];
    let config = generated(&inputs, PathScope::Local);
    assert_eq!(
        rnx_list(&config),
        [
            "ALIC00AUS_R_20240010000_01D_30S_MO.rnx",
            "ALIC00AUS_R_20240020000_01D_30S_MO.rnx",
        ],
    );
}

#[test]
fn export_lists_the_files_exactly_as_the_run_does() {
    // An export runs on this machine, so a file from another
    // directory keeps its full path there too; a bare name would send PEA
    // looking for it under the primary's directory.
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec![
        "/home/tester/obs/ALIC00AUS_R_20240020000_01D_30S_MO.rnx".to_string(),
        "/mnt/elsewhere/DARW00AUS_R_20240010000_01D_30S_MO.rnx".to_string(),
    ];
    let local = generated(&inputs, PathScope::Local);
    let export = generated(&inputs, PathScope::Export);

    assert_eq!(root(&export), Some("/home/tester/obs"));
    assert_eq!(
        rnx_list(&export),
        [
            "ALIC00AUS_R_20240010000_01D_30S_MO.rnx",
            "ALIC00AUS_R_20240020000_01D_30S_MO.rnx",
            "/mnt/elsewhere/DARW00AUS_R_20240010000_01D_30S_MO.rnx",
        ],
    );
    assert_eq!(root(&export), root(&local));
    assert_eq!(rnx_list(&export), rnx_list(&local));
}

#[test]
fn realtime_ignores_selected_files() {
    // A live run has no rnx_inputs at all; a stale selection must not leak in.
    let mut inputs = sample_inputs();
    inputs.rinex_path = String::new();
    inputs.rinex_paths = vec!["/home/tester/obs/ALIC.rnx".to_string()];
    inputs.realtime = Some(realtime_inputs());
    let config = generated(&inputs, PathScope::Local);
    assert!(config["inputs"]["gnss_observations"]["rnx_inputs"].is_null());
}

#[test]
fn older_payload_without_rinex_paths_still_deserialises() {
    // What an earlier version of the app — or a saved call — sends.
    let mut value = serde_json::json!({
        "rinex_path": "/data/ALIC.rnx",
        "output_dir": "/out",
        "products_dir": "/products",
        "marker_name": "ALIC",
        "receiver_type": "",
        "antenna_type": "",
        "antenna_offset": [0.0, 0.0, 0.0],
        "approx_position": null,
        "mode": "Static",
        "constellations": ["GPS"],
        "start_epoch": "2024-01-01 00:00:00",
        "end_epoch": "2024-01-01 23:59:30",
        "epoch_interval": 30.0,
        "output_pos": true,
        "output_gpx": false,
        "output_trace": false,
        "output_snx": false,
        "constellation_obs_codes": null,
    });
    let old: ConfigInputs = serde_json::from_value(value.clone()).expect("old payload parses");
    assert_eq!(old.rinex_files(), ["/data/ALIC.rnx"]);

    value["rinex_paths"] = serde_json::json!(["/data/ALIC.rnx", "/data/DARW.rnx"]);
    let new: ConfigInputs = serde_json::from_value(value).expect("new payload parses");
    assert_eq!(new.rinex_files(), ["/data/ALIC.rnx", "/data/DARW.rnx"]);
}
