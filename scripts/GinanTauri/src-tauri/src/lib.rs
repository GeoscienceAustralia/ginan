pub mod commands;
#[cfg(feature = "profiling")]
mod profiling;
pub mod state;

use state::{AppState, CancelFlag, LiveStats, PosTailGen, PppScanGen};

/// Fit the main window to the screen it opens on.
///
/// The configured 1200x800 is comfortable on a desktop monitor and nearly
/// full-screen on a 13" laptop. That size is also what macOS restores to when
/// the user leaves full screen, so on a small display "exit full screen" barely
/// changed anything.
///
/// Opens at `SCREEN_FRACTION` of the monitor instead, clamped so it is never
/// larger than the configured default and never below the layout's minimum.
/// A failure to read the monitor leaves the configured size alone.
fn fit_window_to_screen(app: &tauri::App) {
    use tauri::{LogicalSize, Manager};

    /// Portion of the screen to occupy. Leaves room for the menu bar, the dock
    /// and whatever else the user has open beside the app.
    const SCREEN_FRACTION: f64 = 0.7;
    /// Never larger than the configured default, so big monitors are unchanged.
    const MAX_W: f64 = 1200.0;
    const MAX_H: f64 = 800.0;
    /// Matches minWidth/minHeight in tauri.conf.json: below this the sidebar
    /// and workspace stop fitting side by side.
    const MIN_W: f64 = 900.0;
    const MIN_H: f64 = 600.0;

    let Some(window) = app.get_webview_window("main") else {
        return;
    };

    // current_monitor can fail or report nothing (headless, racing display
    // changes). Either way the configured size is a fine fallback.
    let Ok(Some(monitor)) = window.current_monitor() else {
        return;
    };

    // Monitor size is in physical pixels; the window API takes logical ones.
    let scale = monitor.scale_factor();
    let screen = monitor.size().to_logical::<f64>(scale);

    let width = (screen.width * SCREEN_FRACTION).clamp(MIN_W, MAX_W);
    let height = (screen.height * SCREEN_FRACTION).clamp(MIN_H, MAX_H);

    let _ = window.set_size(LogicalSize::new(width, height));
    // Resizing from the top-left corner leaves the window off-centre.
    let _ = window.center();
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let builder = tauri::Builder::default();
    #[cfg(feature = "profiling")]
    let builder = profiling::install(builder);

    builder
        .setup(|app| {
            fit_window_to_screen(app);
            Ok(())
        })
        .plugin(tauri_plugin_store::Builder::default().build())
        .plugin(tauri_plugin_dialog::init())
        .manage(std::sync::Mutex::new(AppState::default()))
        .manage(CancelFlag::default())
        .manage(PppScanGen::default())
        .manage(PosTailGen::default())
        .manage(LiveStats::default())
        .invoke_handler(tauri::generate_handler![
            commands::rinex_parse::parse_rinex,
            commands::rinex::prepare_rinex_input,
            commands::config::commands::write_config,
            commands::opener::open_external,
            commands::config::commands::preview_config_yaml,
            commands::config::commands::config_file_status,
            commands::config::discover::discover_config_params,
            commands::config::commands::get_config_path,
            commands::config::commands::reset_config,
            commands::execution::ensure_default_pea_binary,
            commands::pea_runtime::ensure_pea_runtime,
            commands::execution::list_pea_versions,
            commands::execution::download_pea_version,
            commands::execution::use_managed_pea,
            commands::execution::set_pea_binary,
            commands::execution::find_pea_binary,
            commands::execution::start_pea,
            commands::execution::stop_pea,
            commands::realtime::start_pea_realtime,
            commands::realtime::get_live_stream_stats,
            commands::realtime::reset_stream_counters,
            commands::ntrip::fetch_ntrip_sourcetable,
            commands::ntrip::verify_ntrip_credentials,
            commands::streams::list_serial_ports,
            commands::stream_test::test_streams,
            commands::station::get_station_info,

            commands::products::download::ensure_products,
            commands::products::download::ensure_dynamic_products,
            commands::products::catalogue_fetch::scan_ppp_catalogue,
            commands::products::bia::fetch_bia_codes,
            commands::products::bia::fetch_bia_codes_for_combo,

            commands::ui_config::export_pea_config,
            commands::ui_config::import_pea_config,

            commands::health::check_ppp_provider_health,

            commands::credentials::validate_cddis_credentials,
            commands::credentials::save_cddis_credentials,
            commands::credentials_earthdata::forget_cddis_credentials,
            commands::credentials::test_cddis_connection,
            commands::credentials_ntrip::check_cddis_connectivity,
            commands::credentials_ntrip::list_ntrip_hosts,
            commands::credentials_ntrip::save_ntrip_credentials,
            commands::credentials_ntrip::forget_ntrip_credentials,

            commands::antex::validate_antenna,
            commands::antex::find_antex_file,

            commands::visualisation::list_pos_files,
            commands::visualisation::load_pos_series,
            commands::visualisation::start_pos_tail,
            commands::visualisation::stop_pos_tail,

            commands::trace::list_trace_files,
            commands::trace::load_trace_series,
            commands::trace::load_satellite_status,
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
