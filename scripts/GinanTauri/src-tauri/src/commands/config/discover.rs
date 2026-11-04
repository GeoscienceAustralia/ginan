use super::*;

/// Name of the config the UI generates inside the output directory.
const FILE_CONFIG_FILENAME: &str = "ppp_config.yaml";
const REALTIME_CONFIG_FILENAME: &str = "rt_ppp_config.yaml";

pub(crate) fn config_filename(realtime: bool) -> &'static str {
    if realtime {
        REALTIME_CONFIG_FILENAME
    } else {
        FILE_CONFIG_FILENAME
    }
}

/// One scalar leaf discovered in the config template.
#[derive(serde::Serialize)]
pub struct DiscoveredParam {
    /// Dotted path — the same key `ConfigInputs::advanced` accepts.
    pub path: String,
    /// "boolean" | "number" | "string" — drives which control the UI renders.
    pub value_type: String,
    pub default_value: serde_json::Value,
}

/// Paths `generate_config_yaml` derives from the run itself. Excluded from
/// discovery: exposing them would give two controls for one value, and the
/// generator would win anyway since it runs before the `advanced` overrides.
///
/// Prefix match, so `receiver_options.` covers the per-marker block whose key
/// is the station name and therefore unknowable here.
const MANAGED_PREFIXES: &[&str] = &[
    "inputs.inputs_root",
    "inputs.gnss_observations.",
    "inputs.atx_files",
    "inputs.igrf_files",
    "inputs.erp_files",
    "inputs.planetary_ephemeris_files",
    "inputs.snx_files",
    "inputs.satellite_data.",
    "inputs.troposphere.",
    "inputs.tides.",
    "outputs.outputs_root",
    "outputs.metadata.",
    "outputs.gpx.output",
    "outputs.pos.output",
    "outputs.sinex.output",
    "outputs.trace.output_network",
    "processing_options.epoch_control.start_epoch",
    "processing_options.epoch_control.end_epoch",
    "processing_options.epoch_control.epoch_interval",
    "processing_options.epoch_control.epoch_tolerance",
    // Constellation enable + code priorities are the Constellations sidebar.
    "processing_options.gnss_general.sys_options.",
    // Per-station block: receiver/antenna/position come from the RINEX header.
    "receiver_options.TEST.",
];

pub(crate) fn is_managed(path: &str) -> bool {
    MANAGED_PREFIXES.iter().any(|p| path.starts_with(p))
}

/// Recursively collect scalar leaves as dotted paths.
///
/// Sequences are skipped: every list in the template is either a file list or
/// a code-priority list, both already driven from elsewhere in the UI, and a
/// generic list editor would be a much bigger control than this is worth.
pub(crate) fn walk_params(value: &serde_yaml::Value, prefix: &str, out: &mut Vec<DiscoveredParam>) {
    let Some(map) = value.as_mapping() else { return };

    for (k, v) in map {
        let Some(key) = k.as_str() else { continue };
        let path = if prefix.is_empty() {
            key.to_string()
        } else {
            format!("{prefix}.{key}")
        };

        if v.is_mapping() {
            walk_params(v, &path, out);
            continue;
        }
        if is_managed(&path) {
            continue;
        }

        let (value_type, default_value) = match v {
            serde_yaml::Value::Bool(b) => ("boolean", serde_json::Value::Bool(*b)),
            serde_yaml::Value::Number(n) => {
                let json = n
                    .as_f64()
                    .and_then(serde_json::Number::from_f64)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null);
                ("number", json)
            }
            serde_yaml::Value::String(s) => {
                ("string", serde_json::Value::String(s.clone()))
            }
            // Sequences, nulls and tagged values have no sensible generic control.
            _ => continue,
        };

        out.push(DiscoveredParam {
            path,
            value_type: value_type.to_string(),
            default_value,
        });
    }
}

/// Every settable scalar in the config template, in template order.
///
/// Lets the settings page stay current with PEA: when a Ginan release adds a
/// config option, it shows up here without anyone editing the frontend's
/// curated table. Curated entries supply nicer labels and help text for the
/// paths they cover; anything else is rendered generically from its type.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn discover_config_params() -> Result<Vec<DiscoveredParam>, String> {
    let template: serde_yaml::Value = serde_yaml::from_str(FILE_TEMPLATE)
        .map_err(|e| format!("Cannot parse config template: {e}"))?;
    let mut out = Vec::new();
    walk_params(&template, "", &mut out);
    Ok(out)
}
