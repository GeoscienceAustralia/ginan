use super::yaml::*;
use super::generate::*;
use super::discover::*;
use super::*;
use super::tests_support::*;

#[test]
fn obsolete_rts_inverter_is_removed_from_runs_and_exports() {
    let mut inputs = sample_inputs();
    inputs.advanced = Some(std::collections::BTreeMap::from([
        ("processing_options.ppp_filter.rts.inverter".to_string(), serde_yaml::Value::String("LDLT".to_string())),
    ]));
    let old_config = "processing_options:\n  ppp_filter:\n    rts:\n      enable: true\n      lag: -1\n      inverter: LDLT\n";
    for scope in [PathScope::Local, PathScope::Export] {
        for base in [None, Some(old_config)] {
            let yaml = generate_config_yaml(&inputs, base, scope).unwrap();
            let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();
            assert!(get_path(&config, "processing_options.ppp_filter.rts.inverter").is_none());
            assert_eq!(get_path(&config, "processing_options.ppp_filter.rts.enable"), Some(&serde_yaml::Value::Bool(true)));
            assert_eq!(get_path(&config, "processing_options.ppp_filter.rts.lag"), Some(&serde_yaml::Value::Number((-1).into())));
        }
    }
}

#[test]
fn discovery_finds_curated_params_and_skips_managed_ones() {
    let template = load_template();
    let mut found = Vec::new();
    walk_params(&template, "", &mut found);
    let paths: Vec<&str> = found.iter().map(|p| p.path.as_str()).collect();

    // Representative curated parameters must be discoverable, or the
    // frontend's curated entries would have nothing to attach to.
    for expected in [
        "receiver_options.global.elevation_mask",
        "processing_options.spp.outlier_screening.raim.enable",
        "processing_options.model_error_handling.ambiguities.reset_on.lli",
    ] {
        assert!(paths.contains(&expected), "missing {expected}");
    }

    // Anything the generator derives must NOT appear, or the user would get
    // a control that the run silently overrides.
    for managed in [
        "inputs.inputs_root",
        "outputs.outputs_root",
        "processing_options.epoch_control.start_epoch",
        "processing_options.gnss_general.sys_options.gps.process",
    ] {
        assert!(!paths.contains(&managed), "should be managed: {managed}");
    }
}

#[test]
fn discovery_reports_usable_types() {
    let template = load_template();
    let mut found = Vec::new();
    walk_params(&template, "", &mut found);
    for p in &found {
        assert!(
            matches!(p.value_type.as_str(), "boolean" | "number" | "string"),
            "{} has unrenderable type {}",
            p.path,
            p.value_type,
        );
        assert!(!p.default_value.is_null(), "{} has a null default", p.path);
    }
    assert!(found.len() > 20, "discovery returned suspiciously few params");
}

#[test]
fn advanced_overrides_write_to_nested_paths() {
    let mut config = load_template();
    let mut advanced = std::collections::BTreeMap::new();
    advanced.insert(
        "receiver_options.global.elevation_mask".to_string(),
        serde_yaml::Value::Number(10.into()),
    );
    advanced.insert(
        "processing_options.spp.outlier_screening.raim.enable".to_string(),
        serde_yaml::Value::Bool(false),
    );

    for (path, value) in &advanced {
        let keys: Vec<&str> = path.split('.').collect();
        set_val(&mut config, &keys, value.clone());
    }

    assert_eq!(
        get_path(&config, "receiver_options.global.elevation_mask"),
        Some(&serde_yaml::Value::Number(10.into())),
        "elevation mask override must land on the templated key",
    );
    assert_eq!(
        get_path(&config, "processing_options.spp.outlier_screening.raim.enable"),
        Some(&serde_yaml::Value::Bool(false)),
        "deeply nested override must not create a sibling branch",
    );
}

#[test]
fn advanced_override_replaces_template_default() {
    // elevation_mask ships as 15; an override must win, not merge.
    let before = load_template();
    assert_eq!(
        get_path(&before, "receiver_options.global.elevation_mask"),
        Some(&serde_yaml::Value::Number(15.into())),
        "template default changed — update this test and the UI default",
    );

    let mut config = load_template();
    set_val(
        &mut config,
        &["receiver_options", "global", "elevation_mask"],
        serde_yaml::Value::Number(7.into()),
    );
    assert_eq!(
        get_path(&config, "receiver_options.global.elevation_mask"),
        Some(&serde_yaml::Value::Number(7.into())),
    );
}

#[test]
fn test_code_priorities_written_to_correct_path() {
    let mut config = load_template();
    let obs = ConstellationObservationSettings {
        gps: vec!["L1W".to_string(), "L2W".to_string(), "L1C".to_string()],
        gal: vec!["L1C".to_string(), "L5Q".to_string()],
        glo: vec![],
        bds: vec![],
        qzs: vec![],
    };
    write_observation_code_priorities(&mut config, &obs);

    // Verify the sequence is at the correct nested path
    let gps_prios = config["processing_options"]["gnss_general"]["sys_options"]["gps"]["code_priorities"]
        .as_sequence()
        .expect("code_priorities must be a sequence");

    assert_eq!(gps_prios.len(), 3);
    assert_eq!(gps_prios[0].as_str(), Some("L1W"), "first = highest priority");
    assert_eq!(gps_prios[1].as_str(), Some("L2W"));
    assert_eq!(gps_prios[2].as_str(), Some("L1C"));

    let gal_prios = config["processing_options"]["gnss_general"]["sys_options"]["gal"]["code_priorities"]
        .as_sequence()
        .expect("gal code_priorities must be a sequence");
    assert_eq!(gal_prios.len(), 2);

    // Empty constellations must NOT overwrite the template defaults
    let glo_prios = config["processing_options"]["gnss_general"]["sys_options"]["glo"]["code_priorities"]
        .as_sequence()
        .expect("glo should still have template defaults");
    assert!(!glo_prios.is_empty(), "template defaults preserved for unset constellation");
}

#[test]
fn test_flat_code_priorities_path_not_created() {
    let mut config = load_template();
    let obs = ConstellationObservationSettings {
        gps: vec!["L1W".to_string()],
        gal: vec![], glo: vec![], bds: vec![], qzs: vec![],
    };
    write_observation_code_priorities(&mut config, &obs);

    // The flat path under gnss_general is wrong and must NOT exist
    assert!(config["processing_options"]["gnss_general"]["code_priorities"].is_null()
        || !config["processing_options"]["gnss_general"].as_mapping()
            .map(|m| m.contains_key(serde_yaml::Value::String("code_priorities".into())))
            .unwrap_or(false),
        "must NOT write to flat gnss_general.code_priorities");
}

fn strs(v: &[&str]) -> Vec<String> {
    v.iter().map(|s| s.to_string()).collect()
}

fn generated(inputs: &ConfigInputs, base: Option<&str>, scope: PathScope) -> serde_yaml::Value {
    let yaml = generate_config_yaml(inputs, base, scope).expect("config generates");
    serde_yaml::from_str(&yaml).expect("output must parse")
}

fn priorities(config: &serde_yaml::Value, sys: &str) -> Vec<String> {
    config["processing_options"]["gnss_general"]["sys_options"][sys]["code_priorities"]
        .as_sequence()
        .expect("code_priorities must be a sequence")
        .iter()
        .map(|v| v.as_str().unwrap().to_string())
        .collect()
}

#[test]
fn test_is_phase_code_accepts_only_l_signals() {
    for ok in ["L1C", "L2W", "L5Q", "L6X", "L7I", "L8Q"] {
        assert!(is_phase_code(ok), "{ok} is a phase code");
    }
    for bad in ["C1C", "D1C", "S1C", "P1", "L1", "L", "L1CC", "LC1", "L1 ", "l1c", ""] {
        assert!(!is_phase_code(bad), "{bad:?} is not a phase code");
    }
}

#[test]
fn test_code_priorities_keep_only_phase_codes_in_order() {
    let mut config = load_template();
    let obs = ConstellationObservationSettings {
        // The mix earlier versions of the app wrote: every observable the RINEX
        // header listed.
        gps: strs(&["C1C", "L1W", "D1C", "S1C", "L2W", "C2W", "L1C"]),
        gal: strs(&["L5Q", "C5Q", "L1C"]),
        glo: vec![],
        bds: strs(&["L2I", "L7I"]),
        qzs: strs(&[]),
    };
    write_observation_code_priorities(&mut config, &obs);

    assert_eq!(priorities(&config, "gps"), strs(&["L1W", "L2W", "L1C"]));
    assert_eq!(priorities(&config, "gal"), strs(&["L5Q", "L1C"]));
    assert_eq!(priorities(&config, "bds"), strs(&["L2I", "L7I"]));
}

#[test]
fn test_code_priorities_with_no_phase_code_keep_template_defaults() {
    let mut config = load_template();
    let before = priorities(&config, "gps");
    let obs = ConstellationObservationSettings {
        gps: strs(&["C1C", "D1C", "S1C"]),
        gal: vec![], glo: vec![], bds: vec![], qzs: vec![],
    };
    write_observation_code_priorities(&mut config, &obs);

    // A list with nothing usable must not blank the section: PEA would use no signals.
    assert_eq!(priorities(&config, "gps"), before);
}

#[test]
fn test_generated_config_writes_only_the_chosen_phase_codes() {
    let mut inputs = base_inputs();
    inputs.constellation_obs_codes = Some(ConstellationObservationSettings {
        gps: strs(&["L2W", "C1C", "L1C"]),
        gal: vec![], glo: vec![], bds: vec![], qzs: vec![],
    });
    let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
    let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

    assert_eq!(priorities(&config, "gps"), strs(&["L2W", "L1C"]));
}

#[test]
fn test_realtime_config_writes_the_same_phase_only_priorities() {
    let mut inputs = base_inputs();
    inputs.realtime = Some(realtime_inputs());
    inputs.constellations = strs(&["GPS", "GAL"]);
    inputs.constellation_obs_codes = Some(ConstellationObservationSettings {
        gps: strs(&["L1C", "C1C", "L2W"]),
        gal: strs(&["L1C", "D1C", "L5Q"]),
        glo: vec![], bds: vec![], qzs: vec![],
    });
    let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
    let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

    assert_eq!(priorities(&config, "gps"), strs(&["L1C", "L2W"]));
    assert_eq!(priorities(&config, "gal"), strs(&["L1C", "L5Q"]));
    // GLONASS had no list: the realtime template's own defaults stay.
    assert!(!priorities(&config, "glo").is_empty());
}

#[test]
fn test_run_config_filename_is_mode_specific() {
    assert_eq!(config_filename(false), "ppp_config.yaml");
    assert_eq!(config_filename(true), "rt_ppp_config.yaml");
}

#[test]
fn test_file_mode_keeps_epoch_bounds_and_rnx_inputs() {
    let inputs = base_inputs();
    let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
    let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

    let ec = &config["processing_options"]["epoch_control"];
    assert!(!ec["start_epoch"].is_null(), "file runs keep their window");
    assert!(!ec["end_epoch"].is_null());
    assert!(config["inputs"]["gnss_observations"]["rtcm_inputs"].is_null());
    assert!(!config["inputs"]["gnss_observations"]["rnx_inputs"].is_null());
}

#[test]
fn test_run_config_keeps_machine_directories() {
    let yaml = generate_config_yaml(&sample_inputs(), None, PathScope::Local)
        .expect("generation must succeed");
    let config: serde_yaml::Value = serde_yaml::from_str(&yaml).expect("output must parse");

    // PEA is handed this file directly, so every root has to be real.
    assert_eq!(config["inputs"]["inputs_root"].as_str(), Some("/home/tester/products"));
    assert_eq!(
        config["inputs"]["gnss_observations"]["gnss_observations_root"].as_str(),
        Some("/home/tester/obs"),
    );
    assert_eq!(config["outputs"]["outputs_root"].as_str(), Some("/home/tester/results"));
}

#[test]
fn exported_file_run_config_keeps_the_roots_a_run_would_use() {
    // An export is the config this machine would run, so PEA needs
    // every root it would get from `write_config`, not a file it cannot start.
    let local = generated(&sample_inputs(), None, PathScope::Local);
    let export = generated(&sample_inputs(), None, PathScope::Export);

    for key in [
        "inputs.inputs_root",
        "inputs.gnss_observations.gnss_observations_root",
        "outputs.outputs_root",
    ] {
        let want = get_path(&local, key).and_then(|v| v.as_str());
        assert!(want.is_some_and(|s| s.starts_with("/home/tester")), "{key} in the run config");
        assert_eq!(get_path(&export, key).and_then(|v| v.as_str()), want, "{key}");
    }
    assert_eq!(
        get_path(&export, "inputs.gnss_observations.rnx_inputs"),
        get_path(&local, "inputs.gnss_observations.rnx_inputs"),
    );
}

#[test]
fn exported_config_takes_this_machines_roots_over_an_imported_base() {
    // Export re-uses the last imported YAML as its base. Its non-UI keys
    // survive, but the roots are this machine's, as in the run config: the
    // author's directories would point PEA at places that are not here.
    let base = "inputs:
    inputs_root: /home/author/products
    gnss_observations:
        gnss_observations_root: /home/author/obs
        rnx_inputs:
            - AUTHORS.rnx
outputs:
    outputs_root: /home/author/results
";
    let yaml = generate_config_yaml(&sample_inputs(), Some(base), PathScope::Export)
        .expect("generation must succeed");
    let config: serde_yaml::Value = serde_yaml::from_str(&yaml).expect("output must parse");

    assert!(!yaml.contains("/home/author"), "the imported roots are replaced:\n{yaml}");
    assert!(!yaml.contains("AUTHORS.rnx"), "the imported file list is replaced:\n{yaml}");
    assert_eq!(config["inputs"]["inputs_root"].as_str(), Some("/home/tester/products"));
    assert_eq!(
        config["inputs"]["gnss_observations"]["gnss_observations_root"].as_str(),
        Some("/home/tester/obs"),
    );
    assert_eq!(config["outputs"]["outputs_root"].as_str(), Some("/home/tester/results"));
    assert_eq!(rnx_input_name(&config), Some("ALIC00AUS_R_20240010000_01D_30S_MO.rnx"));
}

#[test]
fn file_run_export_and_run_config_are_identical() {
    // A file run has no login, so there is nothing for Export to take out.
    let mut inputs = sample_inputs();
    inputs.rinex_paths = vec![
        "/home/tester/obs/ALIC00AUS_R_20240020000_01D_30S_MO.rnx".to_string(),
        "/mnt/elsewhere/DARW00AUS_R_20240010000_01D_30S_MO.rnx".to_string(),
    ];
    assert_eq!(
        generated(&inputs, None, PathScope::Export),
        generated(&inputs, None, PathScope::Local),
    );
}

#[test]
fn export_never_carries_a_metadata_login_from_the_base_or_advanced() {
    // PEA fills `<USER>` / `<PASS>` from these keys, and `--user` / `--pass`
    // on the command line take over only when the keys are absent. Whatever
    // set them, file run or real time, they must not reach an export. The same
    // goes for `user_aliases`, whose `USER2:bob` / `PASS2:hunter2` entries are
    // per-caster logins PEA substitutes for `<USER2>` / `<PASS2>`.
    let base_sets_them = "outputs:
    metadata:
        config_description: from_base
        user: baseuser
        pass: basepass
processing_options:
    user_aliases:
        - USER2:bob
        - PASS2:hunter2
";
    let aliases = serde_yaml::Value::Sequence(vec![
        serde_yaml::Value::String("USER2:bob".into()),
        serde_yaml::Value::String("PASS2:hunter2".into()),
    ]);
    let advanced: std::collections::BTreeMap<String, serde_yaml::Value> = [
        ("outputs.metadata.user".to_string(), serde_yaml::Value::String("advuser".into())),
        ("outputs.metadata.pass".to_string(), serde_yaml::Value::String("advpass".into())),
        ("processing_options.user_aliases".to_string(), aliases),
    ]
    .into_iter()
    .collect();

    for realtime in [false, true] {
        for (base, adv) in [(Some(base_sets_them), None), (None, Some(advanced.clone()))] {
            let mut inputs = sample_inputs();
            if realtime {
                inputs.realtime = Some(offline_realtime_inputs());
            }
            inputs.advanced = adv;
            let yaml = generate_config_yaml(&inputs, base, PathScope::Export).expect("config generates");
            let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

            let metadata = &config["outputs"]["metadata"];
            assert!(metadata.get("user").is_none(), "realtime={realtime}:\n{yaml}");
            assert!(metadata.get("pass").is_none(), "realtime={realtime}:\n{yaml}");
            assert!(
                config["processing_options"].get("user_aliases").is_none(),
                "realtime={realtime}:\n{yaml}",
            );
            for secret in ["baseuser", "basepass", "advuser", "advpass", "bob", "hunter2"] {
                assert!(!yaml.contains(secret), "{secret} leaked, realtime={realtime}:\n{yaml}");
            }
        }
    }
}

#[test]
fn test_run_config_reads_only_its_own_product_folders() {
    let local: serde_yaml::Value = serde_yaml::from_str(
        &generate_config_yaml(&sample_inputs(), None, PathScope::Local).unwrap(),
    )
    .unwrap();
    let sat = &local["inputs"]["satellite_data"];
    // 2024-01-01 is in GPS week 2295.
    let sp3: Vec<&str> = sat["sp3_files"].as_sequence().unwrap().iter().filter_map(|v| v.as_str()).collect();
    assert_eq!(sp3, ["daily/2024-01-01/*.SP3", "weekly/2295/*.SP3"]);
    assert_eq!(sat["nav_files"][0].as_str(), Some("daily/2024-01-01/BRDC*"));
    assert_eq!(sat["clk_files"][0].as_str(), Some("daily/2024-01-01/*.CLK"));
    assert_eq!(sat["bsx_files"][0].as_str(), Some("daily/2024-01-01/*.BIA"));
    assert_eq!(local["inputs"]["atx_files"][0].as_str(), Some("static/igs20.atx"));

    // An export is the run's own config, so it points at the same folders.
    let export: serde_yaml::Value = serde_yaml::from_str(
        &generate_config_yaml(&sample_inputs(), None, PathScope::Export).unwrap(),
    )
    .unwrap();
    assert_eq!(export["inputs"]["satellite_data"], local["inputs"]["satellite_data"]);
}
