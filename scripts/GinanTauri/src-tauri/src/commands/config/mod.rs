//! Generating and writing the PEA config.
//!
//! Submodules: building the YAML, the realtime overlay, parameter discovery,
//! the commands themselves, and the small YAML-tree helpers they all share.
//!
//! Shared input types live here, and the items the rest of the crate uses are
//! re-exported so callers go through `commands::config`.
pub mod commands;
pub mod discover;
pub mod generate;
pub mod realtime;
pub mod yaml;
#[cfg(test)]
mod tests;
#[cfg(test)]
mod tests_support;
#[cfg(test)]
mod tests_rinex_inputs;
#[cfg(test)]
mod tests_realtime;
pub use commands::{config_file_status, get_config_path, preview_config_yaml, reset_config, write_config, ConfigFileStatus};
pub use discover::{discover_config_params, DiscoveredParam};
pub use generate::generate_config_yaml;
pub use generate::NTRIP_LOG_FILENAME;

use serde::Deserialize;


// Embed the YAML templates at compile time
static FILE_TEMPLATE: &str = include_str!("../../../resources/default_config.yaml");
static REALTIME_TEMPLATE: &str = include_str!("../../../resources/rt_ppp_config.yaml");

/// Constellation-specific observation code settings for configuration generation
#[derive(Debug, Default, Deserialize)]
pub struct ConstellationObservationSettings {
    pub gps: Vec<String>,    // Ordered by priority (first = highest priority)
    pub gal: Vec<String>,
    pub glo: Vec<String>,
    pub bds: Vec<String>,
    pub qzs: Vec<String>,
}

/// Live GNSS stream settings. Present only when the run reads from an NTRIP
/// caster or a local receiver instead of a RINEX file.
///
/// Credentials are deliberately absent: they are read from netrc at write time
/// so they never travel through the frontend or land in an exported config.
#[derive(Debug, Deserialize)]
pub struct RealtimeInputs {
    /// Caster base URL applied as a prefix to every mountpoint, e.g.
    /// `https://ntrip.data.gnss.ga.gov.au/`. No login is carried here — it is
    /// looked up from netrc by hostname at write time; the
    /// `<USER>:<PASS>@` placeholder form is also accepted. Empty when every
    /// stream is a self-contained URI (a serial device, say).
    pub caster_url: String,
    /// Receiver observation streams: NTRIP mountpoints, or full URIs such as
    /// `serial:///dev/ttyAMA0`.
    pub obs_streams: Vec<String>,
    /// SSR correction and broadcast ephemeris mountpoints.
    pub nav_streams: Vec<String>,
    /// Reference point SSR satellite positions correspond to: `APC` or `COM`.
    pub ssr_antenna_offset: String,
    /// Seconds to wait for an epoch before skipping ahead. PEA defaults this to
    /// `epoch_interval + 0.05` when unset.
    pub wait_next_epoch: f64,
    /// Seconds to wait after an epoch's first data before dropping receivers
    /// that still haven't reported.
    pub max_rec_latency: f64,
    /// Exit rather than idle when no observation source is available.
    pub require_obs: bool,
    /// Persist the raw RTCM byte streams as received.
    pub record_rtcm: bool,
    /// Persist per-message decoded RTCM as JSON.
    pub record_decoded: bool,
    /// Persist observations as RINEX.
    pub record_rinex: bool,
    /// SouthPAN SBAS corrections in place of SSR. Absent means none.
    #[serde(default)]
    pub sbas: Option<SbasInputs>,
}

/// The SBAS correction product. Only the mode is the user's: the service
/// (SouthPAN over SISNet) and its message settings are fixed, see
/// `config/realtime.rs`.
#[derive(Debug, Deserialize)]
pub struct SbasInputs {
    /// `L1`, `DFMC` or `PVS` (`processing_options.sbas.mode`).
    pub mode: String,
}

#[derive(Debug, Deserialize)]
pub struct ConfigInputs {
    /// The primary observation file: the one whose header seeds the marker,
    /// receiver, antenna and approximate position below. Empty for realtime.
    pub rinex_path: String,
    /// Every observation file selected for the run, primary included. Absent
    /// from callers that predate multi-file input, in which case the run is
    /// just `rinex_path` — see [`ConfigInputs::rinex_files`].
    #[serde(default)]
    pub rinex_paths: Vec<String>,
    pub output_dir: String,
    pub products_dir: String,
    pub marker_name: String,
    pub receiver_type: String,
    pub antenna_type: String,
    pub antenna_offset: [f64; 3],
    pub approx_position: Option<[f64; 3]>,
    pub mode: String,
    pub constellations: Vec<String>,
    pub start_epoch: String,
    pub end_epoch: String,
    pub epoch_interval: f64,
    pub output_pos: bool,
    pub output_gpx: bool,
    pub output_trace: bool,
    pub output_snx: bool,
    /// The SBAS `.SPP` solution file. Only written while SBAS is used; on
    /// unless turned off, so settings saved by earlier versions of the app keep
    /// writing it.
    #[serde(default = "default_true")]
    pub output_spp: bool,
    /// Optional constellation-specific observation code priorities
    pub constellation_obs_codes: Option<ConstellationObservationSettings>,
    /// PPP product source: analysis center (e.g. "IGS")
    #[serde(default)]
    pub ppp_provider: Option<String>,
    /// PPP product solution series (e.g. "FIN", "RAP")
    #[serde(default)]
    pub ppp_series: Option<String>,
    /// PPP product campaign/project (e.g. "OPS", "MGX")
    #[serde(default)]
    pub ppp_project: Option<String>,
    /// Live stream settings. `None` runs from RINEX files.
    #[serde(default)]
    pub realtime: Option<RealtimeInputs>,
    /// Processing parameters the UI exposes beyond the core inputs above,
    /// keyed by dotted path into the config tree — e.g.
    /// `"receiver_options.global.elevation_mask" => 15`.
    ///
    /// Applied last, so a value set here wins over the template default. Kept
    /// as one open map rather than ~35 named fields so that exposing another
    /// parameter is a one-line change in the frontend's definition table with
    /// no matching Rust, type, or store edit. BTreeMap for deterministic output
    /// ordering — the YAML inspector diffs badly against a shuffling map.
    #[serde(default)]
    pub advanced: Option<std::collections::BTreeMap<String, serde_yaml::Value>>,
}

impl ConfigInputs {
    /// The observation files PEA should read, primary first.
    ///
    /// `rinex_path` always leads, so a caller that only knows the single-file
    /// field keeps working and the primary stays the primary however
    /// `rinex_paths` happens to be ordered. Blank entries and repeats are
    /// dropped: handing PEA the same file twice would register a second stream
    /// on the same receiver and feed it every epoch twice.
    pub fn rinex_files(&self) -> Vec<&str> {
        let mut files: Vec<&str> = Vec::new();
        for path in std::iter::once(&self.rinex_path).chain(self.rinex_paths.iter()) {
            let path = path.as_str();
            if !path.trim().is_empty() && !files.contains(&path) {
                files.push(path);
            }
        }
        files
    }
}

/// Who the generated YAML is for, which decides only how logins are written.
///
/// Both scopes write the same directories, `rnx_inputs` layout and settings:
/// an exported config is the config a run on this machine would write, so
/// `pea --config <file>` runs it here unchanged. Directories are not secrets,
/// and a config stripped of them would not run anywhere without hand-editing.
/// The two callers of `generate_config_yaml` differ only in the login.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathScope {
    /// The login spliced in from netrc. For `write_config`, which PEA runs,
    /// and `preview_config_yaml`, which masks it before display.
    Local,
    /// No login anywhere: every URL carries the `<USER>:<PASS>` placeholder,
    /// and `outputs.metadata.user` / `.pass` and
    /// `processing_options.user_aliases` are removed, so PEA takes the login
    /// from `--user` / `--pass` on its command line. For `export_pea_config`,
    /// whose file leaves the app's control.
    Export,
}

fn default_true() -> bool {
    true
}
