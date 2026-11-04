//! Performance instrumentation. Compiled only with `--features profiling`
//! (see `tauri.profiling.conf.json`), so a normal build carries none of it.
//!
//! Three things are recorded while the app runs:
//! - span timings (wall clock) for every command, the realtime loops and
//!   Tauri's own IPC and event plumbing, written to `profile.folded` in the
//!   app log directory and turned into a flamegraph on request;
//! - a sampling CPU profile of the backend, started and stopped from the
//!   frontend;
//! - the frontend's own measurements, which arrive as `perf` log lines.

use tauri_plugin_tracing::{Builder, LevelFilter, Target};

pub fn install(builder: tauri::Builder<tauri::Wry>) -> tauri::Builder<tauri::Wry> {
    builder
        .plugin(
            Builder::new()
                // Tauri's IPC spans are TRACE. Opening only our own code and
                // Tauri keeps reqwest and friends from flooding the log.
                .with_max_level(LevelFilter::INFO)
                .with_target("ginan_tauri_lib", LevelFilter::TRACE)
                .with_target("tauri", LevelFilter::TRACE)
                // No webview target: forwarding every log line to the page
                // would add work to the thing being measured.
                .targets([Target::Stdout, Target::LogDir { file_name: None }])
                .with_flamegraph()
                .with_default_subscriber()
                .build(),
        )
        .plugin(tauri_plugin_profiling::init())
}
