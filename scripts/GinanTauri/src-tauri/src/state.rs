use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Arc;

/// Shared cancellation flag — checked by download loops, set by stop_pea.
pub struct CancelFlag(pub Arc<AtomicBool>);

impl CancelFlag {
    /// Start a new cancellable operation. A previous Stop must not poison the
    /// next file or realtime run.
    pub fn begin(&self) -> Arc<AtomicBool> {
        self.0.store(false, Ordering::SeqCst);
        self.0.clone()
    }

    pub fn cancel(&self) {
        self.0.store(true, Ordering::SeqCst);
    }
}

/// Monotonically increasing generation counter for PPP catalogue scans.
/// Incremented each time a new scan starts; each scan only emits progress
/// events (and commits its result) if the generation hasn't advanced past its own.
pub struct PppScanGen(pub Arc<AtomicU32>);

impl Default for PppScanGen {
    fn default() -> Self {
        Self(Arc::new(AtomicU32::new(0)))
    }
}

/// Same generation-counter pattern as [`PppScanGen`], for the Visualization
/// tab's live `.pos` tail: starting a new tail (a different file, or a
/// restart) bumps the generation so the previous tailer's loop notices on its
/// next tick and exits, without needing a handle back to that task.
pub struct PosTailGen(pub Arc<AtomicU32>);

impl Default for PosTailGen {
    fn default() -> Self {
        Self(Arc::new(AtomicU32::new(0)))
    }
}

impl Default for CancelFlag {
    fn default() -> Self {
        Self(Arc::new(AtomicBool::new(false)))
    }
}

/// Live Streams-tab data shared between the live-monitor loop and commands.
#[derive(Default)]
pub struct LiveStats {
    /// Last emitted `live-stream-stats` payload. Cleared when a run starts and
    /// kept after it stops so the UI can show frozen values.
    pub latest: std::sync::Mutex<Option<serde_json::Value>>,
    /// Reset requests (`None` = all mounts) for the loop to apply.
    pub pending_resets: std::sync::Mutex<Vec<Option<String>>>,
    /// Wakes the loop so a reset is emitted immediately.
    pub wake: tokio::sync::Notify,
    /// PEA's SISNet log lines this run, fed by its stdout/stderr readers.
    pub sbas_console: std::sync::Mutex<crate::commands::sbas_monitor::SbasConsole>,
}

impl LiveStats {
    /// Forget the previous run's snapshot and any unapplied resets.
    pub fn clear(&self) {
        *self.latest.lock().unwrap() = None;
        self.pending_resets.lock().unwrap().clear();
        *self.sbas_console.lock().unwrap() = Default::default();
    }
}

#[derive(Default)]
pub struct AppState {
    pub pea_pid: Option<u32>,
    pub is_processing: bool,
    pub config_path: Option<String>,
    pub pea_binary_path: Option<String>,
    /// Raw YAML from the last `import_pea_config` call. Used as the base for
    /// `export_pea_config` so that non-UI keys are preserved on round-trip.
    pub imported_yaml: Option<String>,
    /// Process groups of the serial setup scripts holding ports open for the
    /// current realtime run. See `commands::streams`.
    pub stream_holders: Vec<u32>,
    /// PID of the PEA run the user pressed Stop on. Its exit handler checks it
    /// to report "stopped" instead of a failure: the exit status alone cannot
    /// tell a Stop from a crash (SIGTERM on Unix, exit code 1 from taskkill on
    /// Windows). Keyed by PID, not a bool, so a run started straight after a
    /// Stop is not mistaken for the stopped one.
    pub stopped_pid: Option<u32>,
}

#[cfg(test)]
mod tests {
    use super::CancelFlag;
    use std::sync::atomic::Ordering;

    #[test]
    fn beginning_a_run_clears_previous_cancellation() {
        let cancel = CancelFlag::default();
        cancel.cancel();
        assert!(cancel.0.load(Ordering::SeqCst));

        let flag_for_run = cancel.begin();

        assert!(!flag_for_run.load(Ordering::SeqCst));
        assert!(!cancel.0.load(Ordering::SeqCst));
    }
}
