use serde::{Deserialize, Serialize};

use super::config::{generate_config_yaml, ConfigInputs, PathScope};
use crate::state::AppState;

// ── Types returned to the frontend after parsing an imported PEA config ────────

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ObservationCodeItem {
    pub code: String,
    pub enabled: bool,
    pub valid: bool,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct ConstellationCodeConfig {
    pub gps: Vec<ObservationCodeItem>,
    pub gal: Vec<ObservationCodeItem>,
    pub glo: Vec<ObservationCodeItem>,
    pub bds: Vec<ObservationCodeItem>,
    pub qzs: Vec<ObservationCodeItem>,
}

/// UI-relevant settings extracted from a PEA config YAML.
/// All fields are optional because any individual field may be absent.
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct PeaConfigExtract {
    pub mode: Option<String>,
    pub start_epoch: Option<String>,
    pub end_epoch: Option<String>,
    pub epoch_interval: Option<f64>,
    pub output_pos: Option<bool>,
    pub output_gpx: Option<bool>,
    pub output_trace: Option<bool>,
    pub output_snx: Option<bool>,
    pub output_spp: Option<bool>,
    pub constellation_codes: Option<ConstellationCodeConfig>,
    /// Full path reconstructed from `gnss_observations_root` + `rnx_inputs[0]`.
    /// `None` when the file does not exist on this machine.
    ///
    /// The products and output directories are deliberately *not* extracted:
    /// they are machine-level settings owned by this install, not something an
    /// imported file gets to overwrite. The observation file is different — it
    /// is the data the config describes, so it is offered back when the very
    /// same file happens to be present here.
    pub rinex_path: Option<String>,
}

// ── Commands ───────────────────────────────────────────────────────────────────

/// Export: generate a full PEA config YAML to the user-chosen path.
/// Uses the last imported YAML (if any) as the base so that non-UI keys are
/// preserved on round-trip; otherwise falls back to the built-in template.
///
/// Exported with `PathScope::Export`: the config a run on this machine would
/// write, directories included, so `pea --config <file>` runs it here. The one
/// difference is the login, which is left out (see `PathScope::Export`); a
/// comment header at the top says so and, for a real-time config, gives the
/// command line that supplies it.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn export_pea_config(
    path: String,
    inputs: ConfigInputs,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<(), String> {
    let base_yaml = state.lock().unwrap().imported_yaml.clone();
    let yaml = generate_config_yaml(&inputs, base_yaml.as_deref(), PathScope::Export)?;
    let file_name = std::path::Path::new(&path)
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| path.clone());
    let header = export_header(&file_name, &placeholder_hosts(&yaml));
    std::fs::write(&path, header + &yaml).map_err(|e| format!("Write error: {e}"))?;
    Ok(())
}

/// The `#` comment block an exported config opens with.
///
/// The file outlives the app session that wrote it and is read by people who
/// may never have seen the export dialog, so it says what it is: paths that
/// only hold on this machine, and, when a login is needed, that it was left
/// out. PEA ignores YAML comments, and an import of the file reads straight
/// past them.
///
/// The login lines follow `hosts` (from `placeholder_hosts`) alone, not the
/// run's mode: a placeholder URL is the only thing `--user` / `--pass` fill,
/// so a file run needs nothing on the command line and neither does a
/// real-time run on serial devices only, while an imported base or an
/// `advanced` entry can put a placeholder into a file run. PEA reads those
/// options after the YAML, so that is the line given, with the file's own
/// name in it. The warnings under it are the two ways that line goes wrong
/// without saying so: PEA does not fail on a missing login, it reconnects
/// forever; and a password is pasted into the URL as it stands, where `@` and
/// `/` break PEA's URL parser (see `check_login_is_url_safe` in yaml.rs) and
/// `~` is expanded to the home directory (`replaceTags` in acsConfig.cpp).
///
/// PEA has one `--user` / `--pass` pair for every placeholder, so when the
/// streams sit on more than one caster the header names them: the same login
/// is sent to each, and a caster with a different account will refuse it.
/// Per-host logins (`user_aliases`) are deliberately not written.
pub(crate) fn export_header(file_name: &str, hosts: &[String]) -> String {
    let mut header = String::from(
        "# Exported from the Ginan desktop app.\n\
         # Paths in this file are for the machine it was exported on.\n",
    );
    if !hosts.is_empty() {
        // Quoted only when it has to be, so the usual name reads as typed.
        let file_name = if file_name.contains(char::is_whitespace) {
            format!("\"{file_name}\"")
        } else {
            file_name.to_string()
        };
        header.push_str(&format!(
            "# Caster logins are left out. Supply yours when running it:\n\
             #   pea --config {file_name} --user <name> --pass <password>\n\
             # Without --user/--pass PEA retries the caster forever rather than failing.\n\
             # A password containing @, / or ~ will not work here.\n",
        ));
    }
    if hosts.len() > 1 {
        header.push_str(&format!(
            "# These streams use more than one caster ({}); \
             a single --user/--pass is sent to all of them.\n",
            hosts.join(", "),
        ));
    }
    header.push('\n');
    header
}

/// The distinct hosts of every URL in `yaml` that carries the `<USER>:<PASS>`
/// placeholder, in the order they first appear.
///
/// Read from the generated config rather than from the inputs, because a
/// placeholder URL can also arrive from the imported base or an `advanced`
/// entry, and PEA will send the one login to those too. Hosts compare without
/// case, as DNS does; a YAML that does not parse yields none.
pub(crate) fn placeholder_hosts(yaml: &str) -> Vec<String> {
    fn walk(v: &serde_yaml::Value, hosts: &mut Vec<String>) {
        match v {
            serde_yaml::Value::String(s)
                if s.contains(&format!("{}@", super::config::yaml::USERINFO_PLACEHOLDER)) =>
            {
                if let Some(host) = crate::commands::credentials::ntrip_host_from_url(s) {
                    let host = host.to_ascii_lowercase();
                    if !hosts.contains(&host) {
                        hosts.push(host);
                    }
                }
            }
            serde_yaml::Value::Sequence(seq) => seq.iter().for_each(|x| walk(x, hosts)),
            serde_yaml::Value::Mapping(m) => m.iter().for_each(|(_, x)| walk(x, hosts)),
            _ => {}
        }
    }
    let mut hosts = Vec::new();
    if let Ok(config) = serde_yaml::from_str::<serde_yaml::Value>(yaml) {
        walk(&config, &mut hosts);
    }
    hosts
}

/// Import: read a PEA config YAML, store the raw content for round-trip fidelity,
/// and extract every UI-relevant field.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn import_pea_config(
    path: String,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<PeaConfigExtract, String> {
    let content =
        std::fs::read_to_string(&path).map_err(|e| format!("Read error: {e}"))?;
    let config: serde_yaml::Value =
        serde_yaml::from_str(&content).map_err(|e| format!("Parse error: {e}"))?;

    // Preserve raw YAML so export can round-trip all non-UI keys
    state.lock().unwrap().imported_yaml = Some(content);

    // Reconstruct the RINEX path from the two parts stored in the config.
    // Only returned when the file actually exists on this machine.
    let rnx_root = get_str(&config, &["inputs", "gnss_observations", "gnss_observations_root"]);
    let rnx_file = config
        .get("inputs")
        .and_then(|v| v.get("gnss_observations"))
        .and_then(|v| v.get("rnx_inputs"))
        .and_then(|v| v.as_sequence())
        .and_then(|s| s.first())
        .and_then(|v| v.as_str())
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());

    let rinex_path = match (&rnx_root, &rnx_file) {
        (Some(root), Some(file)) => {
            let p = std::path::Path::new(root).join(file);
            if p.exists() { Some(p.to_string_lossy().to_string()) } else { None }
        }
        _ => None,
    };

    Ok(PeaConfigExtract {
        mode: extract_mode(&config),
        start_epoch: get_str(&config, &["processing_options", "epoch_control", "start_epoch"]),
        end_epoch: get_str(&config, &["processing_options", "epoch_control", "end_epoch"]),
        epoch_interval: get_f64(&config, &["processing_options", "epoch_control", "epoch_interval"]),
        output_pos: get_bool(&config, &["outputs", "pos", "output"]),
        output_gpx: get_bool(&config, &["outputs", "gpx", "output"]),
        output_trace: get_bool(&config, &["outputs", "trace", "output_network"]),
        output_snx: get_bool(&config, &["outputs", "sinex", "output"]),
        output_spp: get_bool(&config, &["outputs", "spp", "output"]),
        constellation_codes: extract_constellation_codes(&config),
        rinex_path,
    })
}

// ── Extraction helpers ─────────────────────────────────────────────────────────

fn get_str(v: &serde_yaml::Value, keys: &[&str]) -> Option<String> {
    let node = keys.iter().try_fold(v, |acc, k| acc.get(k))?;
    node.as_str()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
}

fn get_bool(v: &serde_yaml::Value, keys: &[&str]) -> Option<bool> {
    keys.iter().try_fold(v, |acc, k| acc.get(k))?.as_bool()
}

fn get_f64(v: &serde_yaml::Value, keys: &[&str]) -> Option<f64> {
    keys.iter().try_fold(v, |acc, k| acc.get(k))?.as_f64()
}

/// Infer processing mode from the `pos.process_noise` value.
/// 0 → Static, 100 → Kinematic, ≥1000 → Dynamic
fn extract_mode(config: &serde_yaml::Value) -> Option<String> {
    let seq = config
        .get("estimation_parameters")?
        .get("receivers")?
        .get("global")?
        .get("pos")?
        .get("process_noise")?
        .as_sequence()?;
    let pn = seq.first()?.as_f64()? as i64;
    Some(match pn {
        100 => "Kinematic",
        n if n >= 1000 => "Dynamic",
        _ => "Static",
    }
    .to_string())
}

/// Extract per-constellation code priorities and active state.
/// Codes listed under `code_priorities` for a constellation with `process: true`
/// are marked enabled; `process: false` constellations have their codes disabled.
fn extract_constellation_codes(config: &serde_yaml::Value) -> Option<ConstellationCodeConfig> {
    let sys_opts = config
        .get("processing_options")?
        .get("gnss_general")?
        .get("sys_options")?;

    let extract_sys = |sys: &str| -> Vec<ObservationCodeItem> {
        let sys_val = match sys_opts.get(sys) {
            Some(v) => v,
            None => return vec![],
        };
        let enabled = sys_val
            .get("process")
            .and_then(|v| v.as_bool())
            .unwrap_or(false);
        sys_val
            .get("code_priorities")
            .and_then(|v| v.as_sequence())
            .into_iter()
            .flatten()
            .filter_map(|item| item.as_str())
            .map(|code| ObservationCodeItem {
                code: code.to_string(),
                enabled,
                valid: true,
            })
            .collect()
    };

    Some(ConstellationCodeConfig {
        gps: extract_sys("gps"),
        gal: extract_sys("gal"),
        glo: extract_sys("glo"),
        bds: extract_sys("bds"),
        qzs: extract_sys("qzs"),
    })
}
