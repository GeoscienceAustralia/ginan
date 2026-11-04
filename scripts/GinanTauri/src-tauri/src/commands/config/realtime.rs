use super::yaml::*;
use super::generate::*;
use super::*;

/// Point the inputs at live streams and pace the epoch loop for realtime.
///
/// The file-based product inputs are removed rather than left in place: a live
/// session never downloads the `*.SP3`/`*.CLK` products those globs name, and
/// PEA would fail resolving them.
///
/// Logins: `Local` splices the stored (netrc) login into each http/https/ntrip
/// URL that has none, so the password lands only in the config PEA runs.
/// `Export` writes a `<USER>:<PASS>` placeholder in its place, so an exported
/// config never carries a login. See `credentialed_url`.
pub(crate) fn apply_realtime_inputs(
    config: &mut serde_yaml::Value,
    rt: &RealtimeInputs,
    scope: PathScope,
) -> Result<(), String> {
    apply_realtime_inputs_with(config, rt, scope, &|host| {
        crate::commands::credentials_ntrip::ntrip_auth_for_host(host)
    })
}

/// The SSR reference point PEA should assume (`ssr_antenna_offset`).
///
/// PEA does not work this out itself: it reads one global value and logs an
/// error when SSR orbits are used without it (src/cpp/common/ephemeris.cpp).
/// But the stream already says which it is. By mountpoint convention the
/// fourth character of an SSR stream's name gives its reference point,
/// `SSRA...` for the antenna phase centre and `SSRC...` for the centre of mass,
/// so the first correction stream named that way decides. Otherwise the stored
/// value stands (APC by default, generally the more useful one).
pub(crate) fn ssr_antenna_offset(rt: &RealtimeInputs) -> String {
    rt.nav_streams
        .iter()
        .find_map(|s| ssr_offset_from_mountpoint(s))
        .map(str::to_string)
        .unwrap_or_else(|| rt.ssr_antenna_offset.clone())
}

/// `APC`/`COM` from an SSR mountpoint name, or `None` if the name does not
/// follow the `SSRA`/`SSRC` convention. A whole URL is reduced to its last path
/// segment, the mountpoint; a `serial://` device never matches.
pub(crate) fn ssr_offset_from_mountpoint(stream: &str) -> Option<&'static str> {
    if stream.starts_with("serial://") {
        return None;
    }
    let mount = stream.trim_end_matches('/').rsplit('/').next().unwrap_or(stream);
    let mount = mount.as_bytes();
    if mount.len() < 4 || !mount[..3].eq_ignore_ascii_case(b"SSR") {
        return None;
    }
    match mount[3].to_ascii_uppercase() {
        b'A' => Some("APC"),
        b'C' => Some("COM"),
        _ => None,
    }
}

/// `apply_realtime_inputs` with the credential store injected, so tests need no
/// netrc on the machine.
pub(crate) fn apply_realtime_inputs_with(
    config: &mut serde_yaml::Value,
    rt: &RealtimeInputs,
    scope: PathScope,
    lookup: CredentialLookup,
) -> Result<(), String> {
    let resolve = |urls: &[String]| -> Result<Vec<String>, String> {
        urls.iter().map(|u| credentialed_url(u, scope, lookup)).collect()
    };

    // --- Observation streams ---
    delete_val(config, &["inputs", "gnss_observations", "rnx_inputs"]);
    set_val(
        config,
        &["inputs", "gnss_observations", "rtcm_inputs"],
        str_seq(&resolve(&rt.obs_streams)?),
    );

    // The root is prefixed to every mountpoint. Entries that are already whole
    // URIs — a `serial://` device, or a stream on its own host — carry their
    // own prefix, so a pure-serial setup sends no caster URL and gets no root.
    if rt.caster_url.is_empty() {
        delete_val(
            config,
            &["inputs", "gnss_observations", "gnss_observations_root"],
        );
        delete_val(config, &["inputs", "satellite_data", "satellite_data_root"]);
    } else {
        // Add the credentials here so the netrc password never travels through
        // the frontend or lands in an exported config — the UI sends the caster
        // as a plain `scheme://host/` with no login in it.
        let resolved_root = credentialed_url(&rt.caster_url, scope, lookup)?;
        set_val(
            config,
            &["inputs", "gnss_observations", "gnss_observations_root"],
            serde_yaml::Value::String(resolved_root.clone()),
        );
        set_val(
            config,
            &["inputs", "satellite_data", "satellite_data_root"],
            serde_yaml::Value::String(resolved_root),
        );
    }

    // --- Correction and ephemeris streams ---
    // Which stream carries what is only known to the UI (names, the stream
    // test, the user's choice), which holds Process until there is broadcast
    // ephemeris plus SSR or SBAS. With none at all PEA has no orbits, so a
    // config that reaches here that way is refused rather than written.
    if rt.nav_streams.is_empty() {
        return Err(if rt.sbas.is_some() {
            "SBAS corrects broadcast orbits, so a live run with SBAS still needs a broadcast \
             ephemeris stream (e.g. BCEP00BKG0) under Corrections."
                .to_string()
        } else {
            "A live run needs a broadcast ephemeris stream (e.g. BCEP00BKG0) and an SSR \
             corrections stream (e.g. SSRA00CNE0), or SBAS, under Corrections."
                .to_string()
        });
    }
    for key in ["nav_files", "clk_files", "bsx_files", "sp3_files"] {
        delete_val(config, &["inputs", "satellite_data", key]);
    }
    set_val(
        config,
        &["inputs", "satellite_data", "rtcm_inputs", "rtcm_inputs"],
        str_seq(&resolve(&rt.nav_streams)?),
    );
    set_val(
        config,
        &[
            "inputs",
            "satellite_data",
            "rtcm_inputs",
            "ssr_antenna_offset",
        ],
        serde_yaml::Value::String(ssr_antenna_offset(rt)),
    );

    // Orbits and clocks come from SSR corrections (or SBAS, below) applied
    // to broadcast ephemeris rather than from precise product files.
    let source = if rt.sbas.is_some() { "SBAS" } else { "SSR" };
    for model in ["pos", "clock"] {
        set_val(
            config,
            &["satellite_options", "global", "models", model, "enable"],
            serde_yaml::Value::Bool(true),
        );
        set_val(
            config,
            &["satellite_options", "global", "models", model, "sources"],
            str_seq(&[source.to_string()]),
        );
    }
    if let Some(sbas) = &rt.sbas {
        apply_sbas(config, sbas, scope, lookup)?;
    }

    // --- Epoch pacing ---
    let ec = ["processing_options", "epoch_control"];
    set_val(
        config,
        &[ec[0], ec[1], "wait_next_epoch"],
        float_val(rt.wait_next_epoch),
    );
    set_val(
        config,
        &[ec[0], ec[1], "max_rec_latency"],
        float_val(rt.max_rec_latency),
    );
    set_val(
        config,
        &[ec[0], ec[1], "require_obs"],
        serde_yaml::Value::Bool(rt.require_obs),
    );

    // --- Stream telemetry and recordings ---
    set_val(
        config,
        &["outputs", "ntrip_log", "output"],
        serde_yaml::Value::Bool(true),
    );
    set_val(
        config,
        &["outputs", "ntrip_log", "filename"],
        serde_yaml::Value::String(NTRIP_LOG_FILENAME.to_string()),
    );

    // `<mm>` in these names is what makes PEA rotate them minutely, which in
    // turn is what lets the retention sweeper prune whole files.
    let recordings: &[(&str, bool, &str)] = &[
        ("rtcm_obs", rt.record_rtcm, "<STREAM>_<YYYY><DDD><HH><mm>_OBS.rtcm"),
        ("rtcm_nav", rt.record_rtcm, "<STREAM>_<YYYY><DDD><HH><mm>_NAV.rtcm"),
        // One file for every stream, so the live monitor (which follows the
        // newest `decoded_rtcm_*.json`) sees them all.
        ("decoded_rtcm", rt.record_decoded, "decoded_rtcm_<YYYY><DDD><HH><mm>_DEC.json"),
        ("rinex_obs", rt.record_rinex, "<RECEIVER>_<YYYY><DDD><HH><mm>_OBS.rnx"),
    ];
    for (key, enabled, filename) in recordings {
        set_val(
            config,
            &["outputs", key, "output"],
            serde_yaml::Value::Bool(*enabled),
        );
        if *enabled {
            set_val(
                config,
                &["outputs", key, "filename"],
                serde_yaml::Value::String(filename.to_string()),
            );
        }
    }

    // Disable MongoDB by default so PEA does not stall attempting to connect to a local database.
    set_val(
        config,
        &["mongo", "enable"],
        serde_yaml::Value::String("none".to_string()),
    );
    Ok(())
}

/// SouthPAN's SISNet service (Awarua, L5) — the stream in
/// `exampleConfigs/sbas_example_pvs_rt.yaml`.
pub(crate) const SOUTHPAN_SISNET: &str = "sisnet://sisnet.data.gnss.ga.gov.au:62005/";
pub(crate) const SOUTHPAN_HOST: &str = "sisnet.data.gnss.ga.gov.au";
/// The port in `SOUTHPAN_SISNET`, for the app's own SISNet check.
pub(crate) const SOUTHPAN_PORT: u16 = 62005;
/// SouthPAN's PRN.
pub(crate) const SOUTHPAN_PRN: i64 = 122;

/// Write the SBAS block for `sbas.mode`. Everything but the mode is fixed to
/// SouthPAN, as in PEA's SBAS examples: message type 0 is read as 2 on L1 and
/// as 65 on DFMC/PVS, and SouthPAN follows DO-259. PEA's SBAS sanity checker
/// derives the rest (frequency, `pvs_on_dfmc`, constellations, models) from the
/// mode. Only PVS is a PPP solution; L1 and DFMC are SPP, written to `.SPP`
/// when `output_spp` is on (see `generate_config_yaml`).
///
/// SISNet has no anonymous access and PEA misreads a URL with no login, so a
/// run config with no saved login for the SISNet host is refused here rather
/// than started.
fn apply_sbas(
    config: &mut serde_yaml::Value,
    sbas: &SbasInputs,
    scope: PathScope,
    lookup: CredentialLookup,
) -> Result<(), String> {
    let mode = sbas.mode.as_str();
    if !matches!(mode, "L1" | "DFMC" | "PVS") {
        return Err(format!("Unknown SBAS mode {mode:?}: expected L1, DFMC or PVS"));
    }
    let url = credentialed_url(SOUTHPAN_SISNET, scope, lookup)?;
    if scope == PathScope::Local && url == SOUTHPAN_SISNET {
        return Err(format!(
            "No SISNet login is saved for {SOUTHPAN_HOST}. Add it under Settings › Credentials › SISNet \
             (host {SOUTHPAN_HOST}), then try again."
        ));
    }

    let sb = ["inputs", "satellite_data", "sbas_inputs"];
    set_val(config, &[sb[0], sb[1], sb[2], "sisnet_inputs"], str_seq(&[url]));
    set_val(config, &[sb[0], sb[1], sb[2], "sbas_prn"], SOUTHPAN_PRN.into());
    set_val(
        config,
        &[sb[0], sb[1], sb[2], "sbas_message_0"],
        (if mode == "L1" { 2 } else { 65 }).into(),
    );
    set_val(config, &[sb[0], sb[1], sb[2], "use_do259"], (mode != "L1").into());

    set_val(config, &["processing_options", "process_modes", "sbas"], true.into());
    set_val(config, &["processing_options", "process_modes", "ppp"], (mode == "PVS").into());
    set_val(config, &["processing_options", "sbas", "mode"], mode.into());
    Ok(())
}
