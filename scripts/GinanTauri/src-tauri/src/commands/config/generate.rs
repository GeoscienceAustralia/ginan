use super::yaml::*;
use super::realtime::*;
use std::path::Path;
use super::*;

/// Build the PEA config YAML string from user inputs without writing to disk.
/// Shared by `write_config` and `export_pea_config`.
///
/// `base_yaml` — if `Some`, parse it instead of the built-in template so that
/// non-UI keys from a previously-imported file are preserved on round-trip.
/// Pass `None` for `write_config` (clean run) and the stored imported YAML for
/// `export_pea_config`.
///
/// `scope` — see `PathScope`. Paths come out the same either way; `Export`
/// only takes the login out, so the exported file is the run's config minus
/// the password.
pub fn generate_config_yaml(
    inputs: &ConfigInputs,
    base_yaml: Option<&str>,
    scope: PathScope,
) -> Result<String, String> {
    let template = if inputs.realtime.is_some() {
        REALTIME_TEMPLATE
    } else {
        FILE_TEMPLATE
    };

    let mut config: serde_yaml::Value =
        serde_yaml::from_str(base_yaml.unwrap_or(template))
            .map_err(|e| format!("Cannot parse base YAML: {e}"))?;

    // --- Inputs ---
    set_val(
        &mut config,
        &["inputs", "inputs_root"],
        serde_yaml::Value::String(inputs.products_dir.clone()),
    );

    if let Some(rt) = &inputs.realtime {
        apply_realtime_inputs(&mut config, rt, scope)?;
    } else {
        let (rnx_dir, rnx_files) = rnx_inputs_layout(inputs);

        set_val(
            &mut config,
            &["inputs", "gnss_observations", "gnss_observations_root"],
            serde_yaml::Value::String(rnx_dir),
        );
        set_val(
            &mut config,
            &["inputs", "gnss_observations", "rnx_inputs"],
            str_seq(&rnx_files),
        );
        set_product_folders(&mut config, inputs);
    }

    // --- Outputs ---
    set_val(
        &mut config,
        &["outputs", "outputs_root"],
        serde_yaml::Value::String(inputs.output_dir.clone()),
    );
    set_val(
        &mut config,
        &["outputs", "pos", "output"],
        serde_yaml::Value::Bool(inputs.output_pos),
    );
    set_val(
        &mut config,
        &["outputs", "gpx", "output"],
        serde_yaml::Value::Bool(inputs.output_gpx),
    );
    set_val(
        &mut config,
        &["outputs", "trace", "output_network"],
        serde_yaml::Value::Bool(inputs.output_trace),
    );
    set_val(
        &mut config,
        &["outputs", "sinex", "output"],
        serde_yaml::Value::Bool(inputs.output_snx),
    );
    // `.SPP` holds the SBAS solution (with its protection levels); without
    // SBAS there is none worth writing, whatever the setting says.
    let spp = inputs.output_spp && inputs.realtime.as_ref().is_some_and(|rt| rt.sbas.is_some());
    set_val(&mut config, &["outputs", "spp", "output"], serde_yaml::Value::Bool(spp));
    if spp {
        set_val(
            &mut config,
            &["outputs", "spp", "filename"],
            "<RECEIVER>_<YYYY><DDD><HH><mm>.SPP".into(),
        );
    }

    // --- Epoch control ---
    // A live stream has no fixed window: a start/end epoch would make PEA skip
    // everything outside it and exit, so both are dropped for realtime runs.
    if inputs.realtime.is_none() && !inputs.start_epoch.is_empty() {
        set_val(
            &mut config,
            &["processing_options", "epoch_control", "start_epoch"],
            serde_yaml::Value::String(inputs.start_epoch.clone()),
        );
    } else {
        delete_val(&mut config, &["processing_options", "epoch_control", "start_epoch"]);
    }
    if inputs.realtime.is_none() && !inputs.end_epoch.is_empty() {
        set_val(
            &mut config,
            &["processing_options", "epoch_control", "end_epoch"],
            serde_yaml::Value::String(inputs.end_epoch.clone()),
        );
    } else {
        delete_val(&mut config, &["processing_options", "epoch_control", "end_epoch"]);
    }
    set_val(
        &mut config,
        &["processing_options", "epoch_control", "epoch_interval"],
        serde_yaml::Value::Number(serde_yaml::Number::from(inputs.epoch_interval)),
    );
    // Remove template null fields that PEA cannot parse
    delete_val(&mut config, &["processing_options", "epoch_control", "epoch_tolerance"]);
    delete_val(&mut config, &["processing_options", "ppp_filter", "periodic_reset"]);

    // --- Mode → process_noise list ---
    // PEA takes process noise as a list; the UI mode maps to a single scalar:
    // Static=0, Kinematic=100, Dynamic=1000.
    let pn = match inputs.mode.as_str() {
        "Kinematic" => 100,
        "Dynamic" => 1000,
        _ => 0, // Static
    };
    set_val(
        &mut config,
        &[
            "estimation_parameters",
            "receivers",
            "global",
            "pos",
            "process_noise",
        ],
        serde_yaml::Value::Sequence(vec![serde_yaml::Value::Number(
            serde_yaml::Number::from(pn),
        )]),
    );

    // --- Constellation toggles ---
    let all = ["gps", "gal", "glo", "bds", "qzs"];
    let selected_lower: Vec<String> = inputs
        .constellations
        .iter()
        .map(|c| constellation_to_yaml_key(c))
        .collect();

    for sys in all {
        let enabled = selected_lower.contains(&sys.to_string());
        set_val(
            &mut config,
            &[
                "processing_options",
                "gnss_general",
                "sys_options",
                sys,
                "process",
            ],
            serde_yaml::Value::Bool(enabled),
        );
    }

    // --- Observation code priorities ---
    if let Some(obs_codes) = &inputs.constellation_obs_codes {
        write_observation_code_priorities(&mut config, obs_codes);
    }

    // --- Receiver block: rename TEST → marker_name ---
    rename_receiver_key(&mut config, "TEST", &inputs.marker_name);

    // Set receiver-specific fields
    let mname = inputs.marker_name.as_str();
    set_val(
        &mut config,
        &["receiver_options", mname, "receiver_type"],
        serde_yaml::Value::String(inputs.receiver_type.clone()),
    );
    set_val(
        &mut config,
        &["receiver_options", mname, "antenna_type"],
        serde_yaml::Value::String(inputs.antenna_type.clone()),
    );

    // Antenna eccentricity offset (H, E, N)
    set_val(
        &mut config,
        &[
            "receiver_options",
            mname,
            "models",
            "eccentricity",
            "offset",
        ],
        serde_yaml::Value::Sequence(
            inputs
                .antenna_offset
                .iter()
                .map(|v| float_val(*v))
                .collect(),
        ),
    );

    // Apriori position (only if non-zero)
    if let Some(pos) = inputs.approx_position {
        if pos.iter().any(|v| *v != 0.0) {
            set_val(
                &mut config,
                &["receiver_options", mname, "apriori_position"],
                serde_yaml::Value::Sequence(
                    pos.iter()
                        .map(|v| float_val(*v))
                        .collect(),
                ),
            );
        }
    }

    // Applied last so an explicitly-set processing parameter beats both the
    // template default and anything derived above.
    if let Some(advanced) = &inputs.advanced {
        for (path, value) in advanced {
            let keys: Vec<&str> = path.split('.').filter(|s| !s.is_empty()).collect();
            if keys.is_empty() {
                continue;
            }
            set_val(&mut config, &keys, value.clone());
        }
    }

    // Removed from PEA's RTS options. Exports from earlier versions of the app
    // and imported overrides can still carry it, so normalise those as well as the bundled template.
    delete_val(&mut config, &["processing_options", "ppp_filter", "rts", "inverter"]);

    // --- Export: no login anywhere ---
    // Later than the overrides above, and by deletion rather than by skipping
    // the writes, because a login can arrive by routes that never pass through
    // `credentialed_url`: `base_yaml` (the file the user imported earlier), an
    // `advanced` entry, and the template itself.
    if scope == PathScope::Export {
        // PEA substitutes `<USER>` / `<PASS>` in every string from these two
        // keys (`replaceTags` in acsConfig.cpp), and its `--user` / `--pass`
        // options are read after the YAML, so with the keys gone the person
        // running the export supplies the login on the command line. Left in,
        // they would be a real password, or the template's `your_ntrip_user`,
        // which PEA would send to the caster as the login.
        delete_val(&mut config, &["outputs", "metadata", "user"]);
        delete_val(&mut config, &["outputs", "metadata", "pass"]);
        // `user_aliases` is a list of `name:value` pairs PEA substitutes for
        // `<NAME>` in strings (acsConfig.cpp, "User definable alias pairs").
        // Some are harmless file-name tags (`STATION:ALIC` for `<STATION>`),
        // and dropping those leaves the tag unresolved; but any pair can carry
        // a login, and nothing in the pair says which it is. All of them go,
        // since a broken file name is fixable and a leaked password is not.
        delete_val(&mut config, &["processing_options", "user_aliases"]);
        // `apply_realtime_inputs` writes placeholders for the streams it owns;
        // this catches a login that arrived in `base_yaml`, an `advanced` entry
        // or a user-typed stream URL.
        map_strings(&mut config, &|s| replace_userinfo(s, USERINFO_PLACEHOLDER));
        // Some logins defeat the sweep. One with a `/` in it ends the authority
        // at that `/`, so the rest of the login and the `@` after it sit in the
        // path where no userinfo rule can see them; a second URL in the same
        // string is never swept at all. Either way the file would carry part of
        // a password. Repairing it would mean guessing where the login ends, so
        // the export is refused instead. The key is named, never the URL,
        // because the URL is the secret.
        if let Some(key) = find_unreadable_login(&config) {
            return Err(format!(
                "A stream URL under {key} has a login PEA cannot read; \
                 remove the login and export again."
            ));
        }
    }

    serde_yaml::to_string(&config).map_err(|e| format!("YAML error: {e}"))
}

/// Point each satellite-data list at the run's own day and week folders (see
/// `products::layout`) rather than the template's whole-directory glob, so PEA
/// loads this window's products and no other day's. An unparsable window keeps
/// the template's globs.
fn set_product_folders(config: &mut serde_yaml::Value, inputs: &ConfigInputs) {
    let folders =
        crate::commands::products::layout::window_folders(&inputs.start_epoch, &inputs.end_epoch);
    if folders.is_empty() {
        return;
    }
    for (key, glob) in [
        ("nav_files", "BRDC*"),
        ("clk_files", "*.CLK"),
        ("bsx_files", "*.BIA"),
        ("sp3_files", "*.SP3"),
    ] {
        let paths: Vec<String> = folders.iter().map(|f| format!("{f}/{glob}")).collect();
        set_val(config, &["inputs", "satellite_data", key], str_seq(&paths));
    }
}

/// The `gnss_observations_root` and `rnx_inputs` entries for a file-mode run.
///
/// PEA has one observation root and a list of files under it, so the primary
/// file's directory becomes the root and every selected file in that directory
/// is listed by bare name. A file from some other directory is listed
/// by its full path: PEA only prefixes the root to relative entries
/// (`conditionalPrefix` in acsConfig.cpp skips absolute paths, and Windows
/// drive paths because of the `:`).
///
/// An export gets the same layout as a run: it is meant to run on this
/// machine, and bare names for files outside the root would point PEA at
/// files that are not there.
fn rnx_inputs_layout(inputs: &ConfigInputs) -> (String, Vec<String>) {
    let mut files = inputs.rinex_files();
    if files.is_empty() {
        // Nothing selected: fall back to the raw field so the output keeps the
        // usual shape, rather than inventing a different one for this case.
        files.push(inputs.rinex_path.as_str());
    }

    let root = Path::new(files[0]).parent().unwrap_or(Path::new("."));
    let mut names: Vec<String> = Vec::new();
    for file in &files {
        let path = Path::new(file);
        let same_dir = path.parent().unwrap_or(Path::new(".")) == root;
        let entry = if same_dir {
            path.file_name().unwrap_or_default().to_string_lossy().to_string()
        } else {
            file.to_string()
        };
        // A guard only: `rinex_files` drops repeated paths, and a file from
        // another directory keeps its full path, so two distinct files cannot
        // produce the same entry. Listing one twice would feed PEA every epoch
        // twice, which is worth the check if `rinex_files` ever changes.
        if !names.contains(&entry) {
            names.push(entry);
        }
    }

    (root.to_string_lossy().to_string(), names)
}

/// Filename PEA appends NTRIP connection events to, as JSON lines. Fixed rather
/// than time-tagged so the telemetry tailer has one path to follow: the file
/// stays small because per-chunk records need trace level 5, which realtime runs
/// do not set. The bulk on disk is the RTCM recordings, which do rotate.
pub const NTRIP_LOG_FILENAME: &str = "ntrip_stats.jsonl";

/// A RINEX 3 phase observable: `L`, a frequency digit and a tracking attribute (`L1C`, `L5Q`).
///
/// PEA's `code_priorities` is a list of phase signals, and a signal that is not on it
/// is not used. Code (`C1C`), Doppler (`D1C`) and strength (`S1C`) observables name the
/// same signals but are not entries in that list, so they must never be written to it.
pub(crate) fn is_phase_code(code: &str) -> bool {
    let b = code.as_bytes();
    b.len() == 3 && b[0] == b'L' && b[1].is_ascii_digit() && b[2].is_ascii_alphabetic()
}

/// Write each constellation's `code_priorities`, keeping only the phase codes it was given,
/// in the order given. A constellation left with none keeps the template's defaults.
pub(crate) fn write_observation_code_priorities(config: &mut serde_yaml::Value, obs_codes: &ConstellationObservationSettings) {
    let pairs: &[(&str, &Vec<String>)] = &[
        ("gps", &obs_codes.gps),
        ("gal", &obs_codes.gal),
        ("glo", &obs_codes.glo),
        ("bds", &obs_codes.bds),
        ("qzs", &obs_codes.qzs),
    ];

    for (sys, codes) in pairs {
        let phase: Vec<String> = codes.iter().filter(|c| is_phase_code(c)).cloned().collect();
        if phase.is_empty() {
            continue;
        }
        set_val(
            config,
            &["processing_options", "gnss_general", "sys_options", sys, "code_priorities"],
            str_seq(&phase),
        );
    }
}
