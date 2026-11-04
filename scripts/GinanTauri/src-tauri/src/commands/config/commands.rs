use super::generate::*;
use super::discover::*;
use super::yaml::mask_yaml_credentials;
use std::path::Path;
use tauri::Emitter;
use crate::state::AppState;
use super::*;

/// Render the PEA config for `inputs` without touching the filesystem.
///
/// Backs the YAML inspector: the tab must show exactly what `write_config`
/// would produce, so both go through `generate_config_yaml` rather than the
/// inspector re-deriving anything of its own.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn preview_config_yaml(inputs: ConfigInputs) -> Result<String, String> {
    // Local: the inspector shows the file a run would write, paths and all —
    // except logins, which show as `***@`. The run itself writes them in full.
    generate_config_yaml(&inputs, None, PathScope::Local).map(|y| mask_yaml_credentials(&y))
}

/// What a run would do with an existing config file in the output directory.
#[derive(serde::Serialize)]
pub struct ConfigFileStatus {
    /// Absolute path the UI writes to, whether or not it exists yet.
    pub path: String,
    pub exists: bool,
    /// Contents when it exists — lets the inspector diff "on disk" against
    /// "would be written" without a second round trip.
    pub contents: Option<String>,
}

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn config_file_status(
    output_dir: String,
    realtime: bool,
) -> Result<ConfigFileStatus, String> {
    let path = Path::new(&output_dir).join(config_filename(realtime));
    let exists = path.is_file();
    let contents = if exists {
        // The file a run wrote holds the real login; the inspector shows it masked.
        std::fs::read_to_string(&path).ok().map(|c| mask_yaml_credentials(&c))
    } else {
        None
    };
    Ok(ConfigFileStatus {
        path: path.to_string_lossy().to_string(),
        exists,
        contents,
    })
}

/// Write the generated config into the output directory.
///
/// `overwrite` defaults to true. Passing
/// false keeps the active mode's existing config exactly as it is and returns
/// path unchanged — that is what lets a user hand-edit the YAML for options the
/// UI does not expose and still press Process without the edits being clobbered.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn write_config(
    inputs: ConfigInputs,
    app: tauri::AppHandle,
    overwrite: Option<bool>,
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<String, String> {
    let out_path = Path::new(&inputs.output_dir).join(config_filename(inputs.realtime.is_some()));
    let path_str = out_path.to_string_lossy().to_string();

    // Preserve a hand-edited config when the user has asked us not to overwrite.
    if !overwrite.unwrap_or(true) && out_path.is_file() {
        state.lock().unwrap().config_path = Some(path_str.clone());
        return Ok(path_str);
    }

    let yaml_str = generate_config_yaml(&inputs, None, PathScope::Local)?;

    std::fs::create_dir_all(&inputs.output_dir)
        .map_err(|e| format!("Cannot create output dir: {e}"))?;
    std::fs::write(&out_path, &yaml_str)
        .map_err(|e| format!("Cannot write config: {e}"))?;

    // Config-file handling is low-level detail → Console channel, mirroring the
    // original GinanUI (Python) which logs template/config handling to Console.
    // The Tauri flow always regenerates from the embedded template.
    app.emit(
        "console-line",
        format!("📄 Config generated from template → {path_str}"),
    )
    .ok();

    state.lock().unwrap().config_path = Some(path_str.clone());
    Ok(path_str)
}

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn get_config_path(
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<Option<String>, String> {
    Ok(state.lock().unwrap().config_path.clone())
}

#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn reset_config(
    state: tauri::State<'_, std::sync::Mutex<AppState>>,
) -> Result<(), String> {
    let path = state.lock().unwrap().config_path.clone();
    if let Some(p) = path {
        let _ = std::fs::remove_file(&p);
        state.lock().unwrap().config_path = None;
    }
    Ok(())
}
