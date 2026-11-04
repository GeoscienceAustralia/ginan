//! Realtime-specific config tests, split out to keep each file under 500 lines.

use super::yaml::*;
use super::generate::*;
use super::realtime::*;
use super::*;
    use super::tests_support::*;

mod tests {

    use super::*;
    use crate::commands::credentials::ntrip_host_from_url;


    #[test]
    fn test_realtime_template_uses_rt_ppp_config_name() {
        let mut inputs = base_inputs();
        inputs.realtime = Some(realtime_inputs());
        let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
        let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

        assert_eq!(
            config["outputs"]["metadata"]["config_description"].as_str(),
            Some("rt_ppp_config_<HASH>"),
        );
    }

    #[test]
    fn test_realtime_replaces_file_inputs_with_streams() {
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &realtime_inputs(), PathScope::Local, &|_| None).unwrap();

        let obs = &config["inputs"]["gnss_observations"];
        assert!(obs["rnx_inputs"].is_null(), "file inputs must be removed");
        let streams = obs["rtcm_inputs"].as_sequence().expect("rtcm_inputs sequence");
        assert_eq!(streams.len(), 2);
        assert_eq!(streams[0].as_str(), Some("ALIC00AUS0"));

        // Product globs would send PEA looking for files a live run never fetches.
        let sat = &config["inputs"]["satellite_data"];
        for key in ["nav_files", "clk_files", "bsx_files", "sp3_files"] {
            assert!(sat[key].is_null(), "{key} must be removed for realtime");
        }
    }

    #[test]
    fn test_realtime_nav_streams_nested_under_rtcm_inputs() {
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &realtime_inputs(), PathScope::Local, &|_| None).unwrap();

        // PEA expects satellite_data.rtcm_inputs.rtcm_inputs — the outer key is a
        // settings block, not the list itself.
        let nav = config["inputs"]["satellite_data"]["rtcm_inputs"]["rtcm_inputs"]
            .as_sequence()
            .expect("nested nav rtcm_inputs sequence");
        assert_eq!(nav.len(), 2);
        assert_eq!(nav[1].as_str(), Some("SSRA00CNE0"));
        assert_eq!(
            config["inputs"]["satellite_data"]["rtcm_inputs"]["ssr_antenna_offset"].as_str(),
            Some("APC")
        );
    }

    #[test]
    fn ssr_offset_comes_from_the_mountpoint_name() {
        assert_eq!(ssr_offset_from_mountpoint("SSRA00CNE0"), Some("APC"));
        assert_eq!(ssr_offset_from_mountpoint("SSRC00BKG0"), Some("COM"));
        assert_eq!(ssr_offset_from_mountpoint("ssrc00bkg0"), Some("COM"));
        assert_eq!(
            ssr_offset_from_mountpoint("https://ntrip.data.gnss.ga.gov.au/SSRC00GAA0"),
            Some("COM")
        );
        assert_eq!(ssr_offset_from_mountpoint("BCEP00BKG0"), None);
        assert_eq!(ssr_offset_from_mountpoint("SSR0"), None);
        assert_eq!(ssr_offset_from_mountpoint("SSR"), None);
        assert_eq!(ssr_offset_from_mountpoint("serial:///dev/SSRC"), None);
    }

    #[test]
    fn the_correction_stream_overrides_the_stored_offset() {
        let mut rt = realtime_inputs();
        rt.nav_streams = vec!["BCEP00BKG0".into(), "SSRC00BKG0".into()];
        rt.ssr_antenna_offset = "APC".into();
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &rt, PathScope::Local, &|_| None).unwrap();
        assert_eq!(
            config["inputs"]["satellite_data"]["rtcm_inputs"]["ssr_antenna_offset"].as_str(),
            Some("COM")
        );
    }

    #[test]
    fn the_stored_offset_stands_when_no_stream_names_one() {
        // e.g. imported from a config, with a stream outside the convention.
        let mut rt = realtime_inputs();
        rt.nav_streams = vec!["BCEP00BKG0".into(), "MYCORR".into()];
        rt.ssr_antenna_offset = "COM".into();
        assert_eq!(ssr_antenna_offset(&rt), "COM");

        rt.ssr_antenna_offset = "APC".into();
        assert_eq!(ssr_antenna_offset(&rt), "APC");
    }

    #[test]
    fn test_realtime_sources_orbits_and_clocks_from_ssr() {
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &realtime_inputs(), PathScope::Local, &|_| None).unwrap();

        for model in ["pos", "clock"] {
            let m = &config["satellite_options"]["global"]["models"][model];
            assert_eq!(m["enable"].as_bool(), Some(true), "{model} enabled");
            let sources = m["sources"].as_sequence().expect("sources sequence");
            assert_eq!(sources.len(), 1);
            assert_eq!(sources[0].as_str(), Some("SSR"));
        }
    }

    #[test]
    fn test_realtime_drops_epoch_bounds() {
        let mut inputs = base_inputs();
        inputs.realtime = Some(realtime_inputs());
        let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
        let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

        // A start/end window would make PEA discard live epochs and exit.
        let ec = &config["processing_options"]["epoch_control"];
        assert!(ec["start_epoch"].is_null());
        assert!(ec["end_epoch"].is_null());
        assert_eq!(ec["wait_next_epoch"].as_f64(), Some(10.0));
        assert_eq!(ec["max_rec_latency"].as_f64(), Some(1.0));
        assert_eq!(ec["require_obs"].as_bool(), Some(false));
    }

    fn codes_of(config: &serde_yaml::Value, sys: &str) -> Vec<String> {
        config["processing_options"]["gnss_general"]["sys_options"][sys]["code_priorities"]
            .as_sequence()
            .map(|seq| seq.iter().filter_map(|v| v.as_str().map(str::to_string)).collect())
            .unwrap_or_default()
    }

    fn processes(config: &serde_yaml::Value, sys: &str) -> Option<bool> {
        config["processing_options"]["gnss_general"]["sys_options"][sys]["process"].as_bool()
    }

    #[test]
    fn test_realtime_writes_only_the_chosen_constellations_and_signals() {
        // What the UI sends after the user switched GLO off and L2W/L1W off,
        // and moved L5Q ahead of L1C.
        let mut inputs = base_inputs();
        inputs.realtime = Some(realtime_inputs());
        inputs.constellations = vec!["GPS".to_string(), "GAL".to_string()];
        inputs.constellation_obs_codes = Some(ConstellationObservationSettings {
            gps: vec!["L5Q".to_string(), "L1C".to_string()],
            gal: vec!["L1X".to_string()],
            glo: vec![],
            bds: vec![],
            qzs: vec![],
        });
        let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
        let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

        // The template's own GPS list (L1W, L1C, L2W, L2S) is replaced, not
        // merged into, and the user's order survives.
        assert_eq!(codes_of(&config, "gps"), ["L5Q", "L1C"]);
        assert_eq!(codes_of(&config, "gal"), ["L1X"]);
        assert_eq!(processes(&config, "gps"), Some(true));
        assert_eq!(processes(&config, "gal"), Some(true));
        // The template enables GLO; a deselected system must be switched off.
        assert_eq!(processes(&config, "glo"), Some(false));
        assert_eq!(processes(&config, "bds"), Some(false));
        assert_eq!(processes(&config, "qzs"), Some(false));
    }

    #[test]
    fn test_realtime_without_a_selection_keeps_the_template_systems_and_signals() {
        // Nothing customised and no stream test yet: the UI sends its default
        // constellations and empty code lists.
        let mut inputs = base_inputs();
        inputs.realtime = Some(realtime_inputs());
        inputs.constellations = vec!["GPS".to_string(), "GAL".to_string(), "GLO".to_string()];
        inputs.constellation_obs_codes = Some(ConstellationObservationSettings {
            gps: vec![],
            gal: vec![],
            glo: vec![],
            bds: vec![],
            qzs: vec![],
        });
        let yaml = generate_config_yaml(&inputs, None, PathScope::Local).expect("config generates");
        let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

        for sys in ["gps", "gal", "glo"] {
            assert_eq!(processes(&config, sys), Some(true), "{sys} stays on");
        }
        assert_eq!(codes_of(&config, "gps"), ["L1W", "L1C", "L2W", "L2S"]);
        assert_eq!(codes_of(&config, "gal"), ["L1C", "L1X", "L5Q", "L5X"]);
        assert_eq!(codes_of(&config, "glo"), ["L1C", "L1P", "L2C", "L2P"]);
    }

    #[test]
    fn test_serial_only_omits_caster_roots() {
        let mut rt = realtime_inputs();
        rt.caster_url = String::new();
        rt.obs_streams = vec!["serial:///dev/ttyAMA0".to_string()];
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &rt, PathScope::Local, &|_| None).unwrap();

        // A root would be prefixed onto the already-complete serial URI.
        assert!(config["inputs"]["gnss_observations"]["gnss_observations_root"].is_null());
        assert!(config["inputs"]["satellite_data"]["satellite_data_root"].is_null());
    }

    #[test]
    fn test_realtime_recording_toggles() {
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &realtime_inputs(), PathScope::Local, &|_| None).unwrap();

        assert_eq!(config["outputs"]["ntrip_log"]["output"].as_bool(), Some(true));
        assert_eq!(
            config["outputs"]["ntrip_log"]["filename"].as_str(),
            Some(NTRIP_LOG_FILENAME)
        );

        assert_eq!(config["outputs"]["rtcm_obs"]["output"].as_bool(), Some(true));
        assert_eq!(config["outputs"]["rtcm_nav"]["output"].as_bool(), Some(true));
        assert_eq!(config["outputs"]["decoded_rtcm"]["output"].as_bool(), Some(false));
        assert_eq!(config["outputs"]["rinex_obs"]["output"].as_bool(), Some(false));

        // Rotation tags are what let the sweeper prune whole files.
        let obs_name = config["outputs"]["rtcm_obs"]["filename"].as_str().unwrap();
        assert!(obs_name.contains("<mm>"), "recording names must rotate");
    }

    // Fixture logins only; no test reads the machine's netrc.
    fn fixture(host: &str) -> Option<(String, String)> {
        match host {
            "caster.example" => Some(("fixuser".to_string(), "fixpass".to_string())),
            "other.example" => Some(("otheruser".to_string(), "pa:ss".to_string())),
            "atsign.example" => Some(("fixuser".to_string(), "pa@ss".to_string())),
            "slash.example" => Some(("fixuser".to_string(), "pa/ss".to_string())),
            _ => None,
        }
    }

    fn local(url: &str) -> Result<String, String> {
        credentialed_url(url, PathScope::Local, &fixture)
    }

    #[test]
    fn local_splices_the_stored_login_after_the_scheme_and_keeps_port_and_path() {
        assert_eq!(
            local("https://caster.example/").unwrap(),
            "https://fixuser:fixpass@caster.example/"
        );
        assert_eq!(
            local("http://caster.example:2101/MNT").unwrap(),
            "http://fixuser:fixpass@caster.example:2101/MNT"
        );
        assert_eq!(
            local("ntrip://caster.example:2101/MNT").unwrap(),
            "ntrip://fixuser:fixpass@caster.example:2101/MNT"
        );
        // SISNet logs in with the URL's userinfo too.
        assert_eq!(
            local("sisnet://caster.example:62005/").unwrap(),
            "sisnet://fixuser:fixpass@caster.example:62005/"
        );
    }

    #[test]
    fn local_password_with_a_colon_is_spliced_verbatim() {
        // PEA splits on the first colon, so the rest is the password (verified).
        assert_eq!(
            local("https://other.example/MNT").unwrap(),
            "https://otheruser:pa:ss@other.example/MNT"
        );
    }

    #[test]
    fn local_placeholder_is_filled_when_a_login_is_stored_else_kept() {
        assert_eq!(
            local("https://<USER>:<PASS>@caster.example/").unwrap(),
            "https://fixuser:fixpass@caster.example/"
        );
        let kept = "https://<USER>:<PASS>@unknown.example/";
        assert_eq!(local(kept).unwrap(), kept);
    }

    #[test]
    fn local_leaves_urls_that_must_not_be_touched() {
        for url in [
            "serial:///dev/ttyAMA0",
            "ALIC00AUS0",
            "https://unknown.example/MNT",
            "https://typed:typedpw@caster.example/MNT",
        ] {
            assert_eq!(local(url).unwrap(), url, "{url}");
        }
    }

    #[test]
    fn password_pea_cannot_parse_is_refused_without_echoing_it() {
        for url in ["https://atsign.example/MNT", "https://slash.example/MNT"] {
            let err = local(url).unwrap_err();
            assert!(err.contains("PEA cannot read"), "{err}");
            assert!(err.contains(".example"), "names the host: {err}");
            assert!(!err.contains("pa@ss") && !err.contains("pa/ss"), "{err}");
        }
    }

    #[test]
    fn export_swaps_any_login_for_the_placeholder() {
        for (url, want) in [
            ("https://caster.example/", "https://<USER>:<PASS>@caster.example/"),
            ("http://caster.example:2101/MNT", "http://<USER>:<PASS>@caster.example:2101/MNT"),
            ("https://typed:typedpw@caster.example/MNT", "https://<USER>:<PASS>@caster.example/MNT"),
            ("serial:///dev/ttyAMA0", "serial:///dev/ttyAMA0"),
            ("ALIC00AUS0", "ALIC00AUS0"),
        ] {
            assert_eq!(credentialed_url(url, PathScope::Export, &fixture).unwrap(), want);
        }
    }

    #[test]
    fn realtime_local_run_config_carries_a_login_per_http_entry() {
        let mut rt = realtime_inputs();
        rt.caster_url = "https://caster.example/".to_string();
        rt.obs_streams = vec![
            "ALIC00AUS0".to_string(),
            "https://other.example/MNT".to_string(),
            "serial:///dev/ttyAMA0".to_string(),
        ];
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &rt, PathScope::Local, &fixture).unwrap();

        let obs = &config["inputs"]["gnss_observations"];
        assert_eq!(obs["gnss_observations_root"].as_str(), Some("https://fixuser:fixpass@caster.example/"));
        let list: Vec<&str> = obs["rtcm_inputs"].as_sequence().unwrap().iter().map(|v| v.as_str().unwrap()).collect();
        assert_eq!(list, ["ALIC00AUS0", "https://otheruser:pa:ss@other.example/MNT", "serial:///dev/ttyAMA0"]);
    }

    #[test]
    fn realtime_local_run_is_refused_when_the_caster_password_has_an_at_sign() {
        let mut rt = realtime_inputs();
        rt.caster_url = "https://atsign.example/".to_string();
        let mut config = load_template();
        assert!(apply_realtime_inputs_with(&mut config, &rt, PathScope::Local, &fixture).is_err());
    }

    #[test]
    fn portable_export_never_contains_a_login() {
        // Regression test: `satellite_data_root` must not carry the netrc login
        // into exported files, and neither may anything below, typed or stored.
        let mut inputs = base_inputs();
        let mut rt = realtime_inputs();
        rt.obs_streams = vec!["https://typed:typedpw@other.example/MNT".to_string()];
        inputs.realtime = Some(rt);
        inputs.advanced = Some(
            [("outputs.metadata.note".to_string(), serde_yaml::Value::String("https://adv:advpw@h.example/".into()))]
                .into_iter()
                .collect(),
        );
        let base = "inputs:\n  satellite_data:\n    satellite_data_root: https://base:basepw@h.example/\n";
        let yaml = generate_config_yaml(&inputs, Some(base), PathScope::Export).unwrap();

        for secret in ["typedpw", "typed:", "advpw", "basepw", "base:"] {
            assert!(!yaml.contains(secret), "{secret} leaked:\n{yaml}");
        }
        assert!(
            yaml.contains("satellite_data_root: https://<USER>:<PASS>@ntrip.data.gnss.ga.gov.au/"),
            "{yaml}"
        );
        assert!(yaml.contains("https://<USER>:<PASS>@other.example/MNT"));
    }

    #[test]
    fn export_refuses_a_typed_login_pea_cannot_parse_without_echoing_it() {
        // PEA ends the authority at the first `/`, so in `alice:pa/ss@` the
        // login is not where a placeholder can replace it; written out, the
        // password would land in the file. Export must refuse instead, whether
        // the URL hides behind a space or sits second in one string.
        fn with_stream(url: &str) -> ConfigInputs {
            let mut inputs = base_inputs();
            let mut rt = realtime_inputs();
            rt.caster_url = "https://caster.invalid/".to_string();
            rt.obs_streams = vec!["ALIC00AUS0".to_string(), url.to_string()];
            inputs.realtime = Some(rt);
            inputs
        }
        let mut in_advanced = base_inputs();
        in_advanced.advanced = Some(
            [(
                "outputs.metadata.note".to_string(),
                serde_yaml::Value::String("http://a/x http://bob:hunter2@b/Y".into()),
            )]
            .into_iter()
            .collect(),
        );
        let in_base = "inputs:\n  satellite_data:\n    rtcm_inputs:\n      rtcm_inputs:\n        - ntrip://alice:pa/ss@caster.example/MP\n";

        for (case, inputs, base) in [
            ("obs stream", with_stream("ntrip://alice:pa/ss@caster.example/MP"), None),
            ("base YAML", base_inputs(), Some(in_base)),
            ("obs stream with a space", with_stream("ntrip://alice:p/a ss@caster.example/MP"), None),
            ("advanced value", in_advanced, None),
        ] {
            let err = generate_config_yaml(&inputs, base, PathScope::Export)
                .expect_err(&format!("{case}: an unparseable login must be refused"));
            assert!(err.contains("has a login PEA cannot read"), "{case}: {err}");
            for secret in ["pa/ss", "p/a ss", "alice", "bob", "hunter2"] {
                assert!(!err.contains(secret), "{case}: error echoes {secret:?}: {err}");
            }
        }
    }

    #[test]
    fn realtime_export_keeps_the_roots_a_run_would_use_and_a_placeholder_on_every_url() {
        let mut inputs = sample_inputs();
        inputs.rinex_path = String::new();
        inputs.realtime = Some(offline_realtime_inputs());
        let local: serde_yaml::Value =
            serde_yaml::from_str(&generate_config_yaml(&inputs, None, PathScope::Local).unwrap()).unwrap();
        let yaml = generate_config_yaml(&inputs, None, PathScope::Export).unwrap();
        let export: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

        // The directories are this machine's, as in the run config.
        assert_eq!(export["inputs"]["inputs_root"].as_str(), Some("/home/tester/products"));
        assert_eq!(export["outputs"]["outputs_root"].as_str(), Some("/home/tester/results"));
        assert_eq!(export["inputs"]["inputs_root"], local["inputs"]["inputs_root"]);
        assert_eq!(export["outputs"]["outputs_root"], local["outputs"]["outputs_root"]);
        // The caster roots stay, login swapped, or the mountpoints have no host.
        for key in ["gnss_observations", "satellite_data"] {
            assert_eq!(
                export["inputs"][key][format!("{key}_root").as_str()].as_str(),
                Some("https://<USER>:<PASS>@caster.invalid/"),
                "{key}_root",
            );
        }

        let urls = login_urls(&export);
        assert!(urls.len() >= 3, "two caster roots and a stream URL at least: {urls:?}");
        for url in &urls {
            assert!(url.contains("://<USER>:<PASS>@"), "{url} has no placeholder login:\n{yaml}");
        }
        assert!(!yaml.contains("typedpw"), "{yaml}");
    }

    #[test]
    fn realtime_export_drops_the_template_metadata_login() {
        // The template ships `your_ntrip_user` / `your_ntrip_pass`. PEA would
        // send them to the caster as the login unless `--user` / `--pass` were
        // given, and an export is meant to be run with exactly those.
        let mut inputs = base_inputs();
        inputs.realtime = Some(offline_realtime_inputs());
        let yaml = generate_config_yaml(&inputs, None, PathScope::Export).unwrap();
        let config: serde_yaml::Value = serde_yaml::from_str(&yaml).unwrap();

        let metadata = &config["outputs"]["metadata"];
        assert!(metadata.get("user").is_none(), "{yaml}");
        assert!(metadata.get("pass").is_none(), "{yaml}");
        assert!(!yaml.contains("your_ntrip_user") && !yaml.contains("your_ntrip_pass"), "{yaml}");
        // Only the login goes; the rest of the metadata block is the run's.
        assert_eq!(metadata["config_description"].as_str(), Some("rt_ppp_config_<HASH>"));
    }

    #[test]
    fn realtime_export_and_run_config_differ_only_in_the_login() {
        let mut inputs = base_inputs();
        inputs.realtime = Some(offline_realtime_inputs());
        inputs.advanced = Some(
            [("receiver_options.global.elevation_mask".to_string(), serde_yaml::Value::Number(10.into()))]
                .into_iter()
                .collect(),
        );
        let local = generate_config_yaml(&inputs, None, PathScope::Local).unwrap();
        let export = generate_config_yaml(&inputs, None, PathScope::Export).unwrap();

        // Not trivially equal: the run config keeps the typed login.
        assert_ne!(local, export);
        assert_eq!(without_login(&export), without_login(&local));
    }

    #[test]
    fn inspector_masks_logins_as_stars_and_keeps_everything_else() {
        let yaml = "# keep me\na:\n  root: 'https://fixuser:p''w\"x@caster.example/'\n  list:\n    - http://u:p@h.example:2101/MNT\n    - https://<USER>:<PASS>@h.example/\n    - plain\n    - serial:///dev/ttyAMA0\n";
        let out = mask_yaml_credentials(yaml);
        assert_eq!(
            out,
            "# keep me\na:\n  root: 'https://***@caster.example/'\n  list:\n    - http://***@h.example:2101/MNT\n    - https://<USER>:<PASS>@h.example/\n    - plain\n    - serial:///dev/ttyAMA0\n"
        );
    }

    #[test]
    fn inspector_preview_of_a_local_run_config_masks_a_spliced_login() {
        let mut rt = realtime_inputs();
        rt.caster_url = "https://caster.example/".to_string();
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &rt, PathScope::Local, &fixture).unwrap();
        let real = serde_yaml::to_string(&config).unwrap();
        assert!(real.contains("fixuser:fixpass@caster.example"), "run config keeps the login");
        let shown = mask_yaml_credentials(&real);
        assert!(!shown.contains("fixuser") && !shown.contains("fixpass"), "{shown}");
        assert!(shown.contains("https://***@caster.example/"), "{shown}");
    }

    #[test]
    fn ntrip_host_extraction_ignores_placeholder_tokens() {
        // The template URL must yield the correct hostname before any substitution.
        assert_eq!(
            ntrip_host_from_url("https://<USER>:<PASS>@ntrip.data.gnss.ga.gov.au/"),
            Some("ntrip.data.gnss.ga.gov.au".to_string())
        );
    }



    // ── SBAS ──────────────────────────────────────────────────────────────────

    fn sbas_config(mode: &str, scope: PathScope) -> Result<serde_yaml::Value, String> {
        let mut config = load_template();
        let mut rt = realtime_inputs();
        rt.sbas = Some(SbasInputs { mode: mode.to_string() });
        let login = |host: &str| (host == "sisnet.data.gnss.ga.gov.au")
            .then(|| ("simon".to_string(), "s3cret".to_string()));
        apply_realtime_inputs_with(&mut config, &rt, scope, &login).map(|_| config)
    }

    #[test]
    fn sbas_writes_the_southpan_block_with_the_saved_login() {
        let config = sbas_config("DFMC", PathScope::Local).unwrap();
        let sb = &config["inputs"]["satellite_data"]["sbas_inputs"];
        assert_eq!(
            sb["sisnet_inputs"][0].as_str(),
            Some("sisnet://simon:s3cret@sisnet.data.gnss.ga.gov.au:62005/"),
        );
        assert_eq!(sb["sbas_prn"].as_i64(), Some(122));
        assert_eq!(sb["sbas_message_0"].as_i64(), Some(65));
        assert_eq!(sb["use_do259"].as_bool(), Some(true));

        let po = &config["processing_options"];
        assert_eq!(po["process_modes"]["sbas"].as_bool(), Some(true));
        assert_eq!(po["process_modes"]["ppp"].as_bool(), Some(false));
        assert_eq!(po["sbas"]["mode"].as_str(), Some("DFMC"));
        for model in ["pos", "clock"] {
            assert_eq!(
                config["satellite_options"]["global"]["models"][model]["sources"][0].as_str(),
                Some("SBAS"),
            );
        }
    }

    #[test]
    fn sbas_mode_picks_message_type_0_ppp_and_do259() {
        let l1 = sbas_config("L1", PathScope::Local).unwrap();
        let sb = &l1["inputs"]["satellite_data"]["sbas_inputs"];
        assert_eq!(sb["sbas_message_0"].as_i64(), Some(2));
        assert_eq!(sb["use_do259"].as_bool(), Some(false));

        let pvs = sbas_config("PVS", PathScope::Local).unwrap();
        assert_eq!(pvs["processing_options"]["process_modes"]["ppp"].as_bool(), Some(true));
        assert_eq!(pvs["processing_options"]["sbas"]["mode"].as_str(), Some("PVS"));

        assert!(sbas_config("SSR", PathScope::Local).is_err());
    }

    #[test]
    fn sbas_run_without_a_sisnet_login_is_refused() {
        let mut config = load_template();
        let mut rt = realtime_inputs();
        rt.sbas = Some(SbasInputs { mode: "DFMC".to_string() });
        let err = apply_realtime_inputs_with(&mut config, &rt, PathScope::Local, &|_| None).unwrap_err();
        assert!(err.contains("sisnet.data.gnss.ga.gov.au"), "{err}");
    }

    #[test]
    fn sbas_export_never_contains_the_sisnet_login() {
        let config = sbas_config("DFMC", PathScope::Export).unwrap();
        assert_eq!(
            config["inputs"]["satellite_data"]["sbas_inputs"]["sisnet_inputs"][0].as_str(),
            Some("sisnet://<USER>:<PASS>@sisnet.data.gnss.ga.gov.au:62005/"),
        );
        assert!(!serde_yaml::to_string(&config).unwrap().contains("s3cret"));
    }

    #[test]
    fn a_live_run_without_correction_streams_is_refused() {
        let mut rt = realtime_inputs();
        rt.nav_streams.clear();
        let err = apply_realtime_inputs_with(&mut load_template(), &rt, PathScope::Local, &|_| None).unwrap_err();
        assert!(err.contains("broadcast ephemeris") && err.contains("SSR"), "{err}");

        rt.sbas = Some(SbasInputs { mode: "PVS".to_string() });
        let err = apply_realtime_inputs_with(&mut load_template(), &rt, PathScope::Local, &|_| None).unwrap_err();
        assert!(err.contains("SBAS") && err.contains("broadcast"), "{err}");
    }

    #[test]
    fn spp_output_follows_the_setting_and_only_with_sbas() {
        let yaml = |sbas: Option<&str>, output_spp: bool| -> serde_yaml::Value {
            let mut inputs = base_inputs();
            let mut rt = realtime_inputs();
            rt.sbas = sbas.map(|m| SbasInputs { mode: m.to_string() });
            inputs.realtime = Some(rt);
            inputs.output_spp = output_spp;
            serde_yaml::from_str(&generate_config_yaml(&inputs, None, PathScope::Export).unwrap()).unwrap()
        };
        let on = yaml(Some("DFMC"), true);
        assert_eq!(on["outputs"]["spp"]["output"].as_bool(), Some(true));
        assert_eq!(on["outputs"]["spp"]["filename"].as_str(), Some("<RECEIVER>_<YYYY><DDD><HH><mm>.SPP"));
        // PVS has its own .POS, and can write the SBAS SPP beside it.
        assert_eq!(yaml(Some("PVS"), true)["outputs"]["spp"]["output"].as_bool(), Some(true));
        assert_eq!(yaml(Some("DFMC"), false)["outputs"]["spp"]["output"].as_bool(), Some(false));
        assert_eq!(yaml(None, true)["outputs"]["spp"]["output"].as_bool(), Some(false));
    }

    #[test]
    fn without_sbas_nothing_sbas_is_written() {
        let mut config = load_template();
        apply_realtime_inputs_with(&mut config, &realtime_inputs(), PathScope::Local, &|_| None).unwrap();
        assert!(config["inputs"]["satellite_data"]["sbas_inputs"].is_null());
        assert!(config["processing_options"]["sbas"].is_null());
    }
}
