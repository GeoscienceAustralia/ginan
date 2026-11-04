//! Tests for the header `export_pea_config` writes above an exported config
//! and for the caster hosts it is built from.

use super::config::{generate_config_yaml, ConfigInputs, PathScope};
use super::ui_config::{export_header, placeholder_hosts};

const COMMAND_LINE: &str = "#   pea --config rt_ppp_config.yaml --user <name> --pass <password>\n";

fn hosts(names: &[&str]) -> Vec<String> {
    names.iter().map(|s| s.to_string()).collect()
}

/// Every line a comment or blank, so PEA and a later import read straight past
/// it, and the YAML under it parses to exactly what it was without the header.
fn assert_is_only_comments(header: &str) {
    for line in header.lines() {
        assert!(line.is_empty() || line.starts_with('#'), "not a comment: {line:?}");
    }
    assert!(header.ends_with("\n\n"), "a blank line sets the header off: {header:?}");
    let yaml = "outputs:\n  outputs_root: /home/tester/results\n";
    let with: serde_yaml::Value = serde_yaml::from_str(&format!("{header}{yaml}")).unwrap();
    let without: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
    assert_eq!(with, without);
}

#[test]
fn file_run_header_says_where_it_came_from_and_asks_for_no_login() {
    let header = export_header("ppp_config.yaml", &[]);

    assert!(header.contains("Exported from the Ginan desktop app"), "{header}");
    assert!(header.contains("Paths in this file are for the machine it was exported on"), "{header}");
    // `pea --config ppp_config.yaml` runs it as it stands: no login lines at all.
    for absent in ["--user", "--pass", "pea --config", "login", "caster"] {
        assert!(!header.contains(absent), "{absent:?} in a file-run header:\n{header}");
    }
    assert_is_only_comments(&header);
}

#[test]
fn single_caster_realtime_header_gives_the_exact_command_and_no_caster_warning() {
    let header = export_header("rt_ppp_config.yaml", &hosts(&["caster.invalid"]));

    assert!(header.contains("Paths in this file are for the machine it was exported on"), "{header}");
    assert!(header.contains("Caster logins are left out"), "{header}");
    assert!(header.contains(COMMAND_LINE), "{header}");
    // PEA does not fail on a missing login, so the header has to say so.
    assert!(header.contains("retries the caster forever"), "{header}");
    assert!(!header.contains("more than one caster"), "one host, no warning:\n{header}");
    assert_is_only_comments(&header);
}

#[test]
fn serial_only_realtime_header_asks_for_no_login() {
    // A receiver on a serial port has no caster to log in to, so the file runs
    // as `pea --config rt_ppp_config.yaml`; asking for a login would mislead.
    let header = export_header("rt_ppp_config.yaml", &[]);
    assert!(header.contains("Paths in this file are for the machine it was exported on"), "{header}");
    for absent in ["--user", "--pass", "retries the caster forever", "more than one caster"] {
        assert!(!header.contains(absent), "{absent:?} in a serial-only header:\n{header}");
    }
    assert_is_only_comments(&header);
}

#[test]
fn two_caster_realtime_header_names_both_and_warns_one_login_goes_to_each() {
    let header = export_header(
        "rt_ppp_config.yaml",
        &hosts(&["ntrip.data.gnss.ga.gov.au", "products.igs-ip.net"]),
    );

    assert!(header.contains(COMMAND_LINE), "{header}");
    assert!(header.contains("retries the caster forever"), "{header}");
    assert!(
        header.contains("more than one caster (ntrip.data.gnss.ga.gov.au, products.igs-ip.net)"),
        "{header}",
    );
    assert!(header.contains("a single --user/--pass is sent to all of them"), "{header}");
    assert_is_only_comments(&header);
}

#[test]
fn file_run_whose_urls_carry_a_placeholder_still_asks_for_the_login() {
    // A placeholder URL that came in through the imported base or an
    // `advanced` entry needs `--user` / `--pass` just the same.
    let header = export_header("ppp_config.yaml", &hosts(&["caster.invalid"]));
    assert!(header.contains("#   pea --config ppp_config.yaml --user <name> --pass <password>\n"), "{header}");
}

#[test]
fn a_file_name_with_a_space_is_quoted_in_the_command() {
    let header = export_header("my config.yaml", &hosts(&["caster.invalid"]));
    assert!(header.contains("#   pea --config \"my config.yaml\" --user <name> --pass <password>\n"), "{header}");
}

#[test]
fn placeholder_hosts_are_distinct_lowercase_and_in_order_of_appearance() {
    let yaml = "inputs:
  gnss_observations:
    gnss_observations_root: https://<USER>:<PASS>@Caster.Invalid/
    rtcm_inputs:
      - ALIC00AUS0
      - http://<USER>:<PASS>@other.invalid:2101/MNT
      - serial:///dev/ttyAMA0
      - https://plain.invalid/MNT
  satellite_data:
    satellite_data_root: https://<USER>:<PASS>@caster.invalid/
";
    // The bare caster URL with no placeholder sends no login, so it is no host
    // the one login would reach.
    assert_eq!(placeholder_hosts(yaml), hosts(&["caster.invalid", "other.invalid"]));
    assert!(placeholder_hosts("not: [valid").is_empty());
}

/// A real-time export on `caster` reading the observation streams `obs`.
fn realtime_export(caster: &str, obs: &[&str]) -> String {
    let inputs: ConfigInputs = serde_json::from_value(serde_json::json!({
        "rinex_path": "",
        "output_dir": "/home/tester/results",
        "products_dir": "/home/tester/products",
        "marker_name": "ALIC",
        "receiver_type": "",
        "antenna_type": "",
        "antenna_offset": [0.0, 0.0, 0.0],
        "approx_position": null,
        "mode": "Static",
        "constellations": ["GPS"],
        "start_epoch": "",
        "end_epoch": "",
        "epoch_interval": 1.0,
        "output_pos": true,
        "output_gpx": false,
        "output_trace": false,
        "output_snx": false,
        "constellation_obs_codes": null,
        "realtime": {
            "caster_url": caster,
            "obs_streams": obs,
            "nav_streams": if caster.is_empty() { vec!["serial:///dev/ttyAMA1"] } else { vec!["BCEP00BKG0", "SSRA00CNE0"] },
            "ssr_antenna_offset": "APC",
            "wait_next_epoch": 10.0,
            "max_rec_latency": 1.0,
            "require_obs": false,
            "record_rtcm": false,
            "record_decoded": false,
            "record_rinex": false,
        },
    }))
    .expect("inputs deserialise");
    generate_config_yaml(&inputs, None, PathScope::Export).expect("config generates")
}

#[test]
fn an_export_on_one_caster_gets_the_single_caster_header() {
    let yaml = realtime_export("https://caster.invalid/", &["ALIC00AUS0"]);
    let found = placeholder_hosts(&yaml);
    assert_eq!(found, hosts(&["caster.invalid"]));
    assert!(!export_header("rt_ppp_config.yaml", &found).contains("more than one caster"));
}

#[test]
fn an_export_with_a_stream_on_a_second_caster_gets_the_warning() {
    let yaml = realtime_export("https://caster.invalid/", &["ALIC00AUS0", "https://typed:typedpw@other.invalid/MNT"]);
    let found = placeholder_hosts(&yaml);
    assert_eq!(found, hosts(&["caster.invalid", "other.invalid"]));
    let header = export_header("rt_ppp_config.yaml", &found);
    assert!(header.contains("more than one caster (caster.invalid, other.invalid)"), "{header}");
    assert!(!header.contains("typedpw") && !yaml.contains("typedpw"));
}

#[test]
fn a_serial_only_export_finds_no_caster_and_gets_no_login_lines() {
    let yaml = realtime_export("", &["serial:///dev/ttyAMA0"]);
    let found = placeholder_hosts(&yaml);
    assert!(found.is_empty(), "{found:?}");
    let header = export_header("rt_ppp_config.yaml", &found);
    assert!(!header.contains("--user") && !header.contains("--pass"), "{header}");
}
