//! Helpers shared by the config test modules.

use super::*;

/// A config whose paths all sit under one recognisable directory, so a test
/// can assert on the whole generated string as well as on single keys.
pub(super) fn sample_inputs() -> ConfigInputs {
    ConfigInputs {
        rinex_path: "/home/tester/obs/ALIC00AUS_R_20240010000_01D_30S_MO.rnx".to_string(),
        rinex_paths: vec![],
        output_dir: "/home/tester/results".to_string(),
        products_dir: "/home/tester/products".to_string(),
        marker_name: "ALIC".to_string(),
        receiver_type: "SEPT POLARX5".to_string(),
        antenna_type: "LEIAR25.R3      NONE".to_string(),
        antenna_offset: [0.0, 0.0, 0.0],
        approx_position: None,
        mode: "Static".to_string(),
        constellations: vec!["GPS".to_string()],
        start_epoch: "2024-01-01 00:00:00".to_string(),
        end_epoch: "2024-01-01 23:59:30".to_string(),
        epoch_interval: 30.0,
        output_pos: true,
        output_gpx: false,
        output_trace: false,
        output_snx: false,
        output_spp: true,
        constellation_obs_codes: None,
        ppp_provider: Some("IGS".to_string()),
        ppp_series: Some("FIN".to_string()),
        ppp_project: Some("OPS".to_string()),
        advanced: None,
        realtime: None,
    }
}

pub(super) fn rnx_input_name(config: &serde_yaml::Value) -> Option<&str> {
    config["inputs"]["gnss_observations"]["rnx_inputs"]
        .as_sequence()?
        .first()?
        .as_str()
}

pub(super) fn load_template() -> serde_yaml::Value {
    serde_yaml::from_str(FILE_TEMPLATE).expect("template must parse")
}

/// Walk a dotted path and return the value, for asserting on output.
pub(super) fn get_path<'a>(v: &'a serde_yaml::Value, path: &str) -> Option<&'a serde_yaml::Value> {
    path.split('.').try_fold(v, |acc, k| acc.get(k))
}

pub(super) fn realtime_inputs() -> RealtimeInputs {
    RealtimeInputs {
        // Plain, matching what the UI sends. The legacy placeholder form
        // has its own dedicated case below.
        caster_url: "https://ntrip.data.gnss.ga.gov.au/".to_string(),
        obs_streams: vec!["ALIC00AUS0".to_string(), "MAW100ATA0".to_string()],
        nav_streams: vec!["BCEP00BKG0".to_string(), "SSRA00CNE0".to_string()],
        ssr_antenna_offset: "APC".to_string(),
        wait_next_epoch: 10.0,
        max_rec_latency: 1.0,
        require_obs: false,
        record_rtcm: true,
        record_decoded: false,
        record_rinex: false,
        sbas: None,
    }
}

pub(super) fn base_inputs() -> ConfigInputs {
    ConfigInputs {
        rinex_path: "/data/ALIC.rnx".to_string(),
        rinex_paths: vec![],
        output_dir: "/out".to_string(),
        products_dir: "/products".to_string(),
        marker_name: "ALIC".to_string(),
        receiver_type: String::new(),
        antenna_type: "TRM59800.00".to_string(),
        antenna_offset: [0.0, 0.0, 0.0],
        approx_position: None,
        mode: "kinematic".to_string(),
        constellations: vec!["GPS".to_string()],
        start_epoch: "2024-01-01 00:00:00".to_string(),
        end_epoch: "2024-01-01 23:59:30".to_string(),
        epoch_interval: 30.0,
        output_pos: true,
        output_gpx: false,
        output_trace: false,
        output_snx: false,
        output_spp: true,
        constellation_obs_codes: None,
        ppp_provider: None,
        ppp_series: None,
        ppp_project: None,
        realtime: None,
        advanced: None,
    }
}

/// `realtime_inputs` on a caster no netrc can hold a login for, so a `Local`
/// config generates the same on every machine (`generate_config_yaml` reads the
/// real netrc). One stream carries a typed login, as a user may paste one.
pub(super) fn offline_realtime_inputs() -> RealtimeInputs {
    RealtimeInputs {
        caster_url: "https://caster.invalid/".to_string(),
        obs_streams: vec![
            "ALIC00AUS0".to_string(),
            "https://typed:typedpw@other.invalid:2101/MNT".to_string(),
            "serial:///dev/ttyAMA0".to_string(),
        ],
        ..realtime_inputs()
    }
}

/// A generated config with the login taken out the same way whatever the
/// scope: every http, https or ntrip URL gets the placeholder login, whether it
/// had a login, a placeholder or none, and the metadata login keys go. Two
/// configs equal under this differ at most in the login.
///
/// Written here rather than borrowed from `credentialed_url`, so the test does
/// not check the code under test against itself.
pub(super) fn without_login(yaml: &str) -> serde_yaml::Value {
    use super::yaml::{delete_val, map_strings};
    let mut config: serde_yaml::Value = serde_yaml::from_str(yaml).expect("output must parse");
    map_strings(&mut config, &placeholder_login);
    delete_val(&mut config, &["outputs", "metadata", "user"]);
    delete_val(&mut config, &["outputs", "metadata", "pass"]);
    config
}

fn placeholder_login(s: &str) -> String {
    let Some(scheme) = ["http", "https", "ntrip"]
        .into_iter()
        .find(|p| s.starts_with(&format!("{p}://")))
    else {
        return s.to_string();
    };
    let rest = &s[scheme.len() + 3..];
    let (authority, path) = rest.split_at(rest.find('/').unwrap_or(rest.len()));
    let host = authority.rsplit_once('@').map_or(authority, |(_, h)| h);
    format!("{scheme}://<USER>:<PASS>@{host}{path}")
}

/// Every string in the config that is an http, https or ntrip URL: the ones
/// PEA would log in to.
pub(super) fn login_urls(v: &serde_yaml::Value) -> Vec<String> {
    let mut out = Vec::new();
    collect_login_urls(v, &mut out);
    out
}

fn collect_login_urls(v: &serde_yaml::Value, out: &mut Vec<String>) {
    match v {
        serde_yaml::Value::String(s) => {
            if ["http://", "https://", "ntrip://"].iter().any(|p| s.starts_with(p)) {
                out.push(s.clone());
            }
        }
        serde_yaml::Value::Sequence(seq) => seq.iter().for_each(|x| collect_login_urls(x, out)),
        serde_yaml::Value::Mapping(map) => map.values().for_each(|x| collect_login_urls(x, out)),
        serde_yaml::Value::Tagged(t) => collect_login_urls(&t.value, out),
        _ => {}
    }
}
