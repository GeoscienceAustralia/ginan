import { useEffect, useState } from "react";
import { STREAM_TEST_SECONDS, useAppStore } from "../store/appStore";

/** Seconds since `startedAt`, ticking while `startedAt` is set. */
function useElapsedSeconds(startedAt: number | null): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (startedAt == null) return;
    setNow(Date.now());
    const id = setInterval(() => setNow(Date.now()), 250);
    return () => clearInterval(id);
  }, [startedAt]);
  return startedAt == null ? 0 : Math.max(0, (now - startedAt) / 1000);
}

export default function StatusBar() {
  const status = useAppStore((s) => s.status);
  const isProcessing = useAppStore((s) => s.isProcessing);
  const cddisStatus = useAppStore((s) => s.cddisStatus);
  const cddisStatusMessage = useAppStore((s) => s.cddisStatusMessage);
  const productsDir = useAppStore((s) => s.productsDir);
  const peaUpdateAvailable = useAppStore((s) => s.peaUpdateAvailable);
  const setPeaUpdateModalOpen = useAppStore((s) => s.setPeaUpdateModalOpen);
  const activity = useAppStore((s) => s.activity);
  const activityHidden = useAppStore((s) => s.activityHidden);
  const showActivity = useAppStore((s) => s.showActivity);
  const testStartedAt = useAppStore((s) => (s.streamTestStatus === "running" ? s.streamTestStartedAt : null));
  const elapsed = useElapsedSeconds(testStartedAt);

  // Startup CDDIS state outranks the general status line — reporting "Ready"
  // while connectivity is unverified or broken would be misleading. A run in
  // progress outranks everything, since its messages are the live ones.
  const { text, busy } = (() => {
    // A stream test, whether from Monitor stream or the start of Process, is
    // the thing being waited on: count its watch out, then say it is
    // finishing, since connecting and decoding run past the watch itself.
    if (testStartedAt != null) {
      const s = Math.floor(elapsed);
      return {
        text:
          s < STREAM_TEST_SECONDS
            ? `Monitoring streams... ${s} s of ${STREAM_TEST_SECONDS} s`
            : `Monitoring streams... finishing (${s} s)`,
        busy: true,
      };
    }
    if (isProcessing) return { text: status, busy: true };

    if (cddisStatus === "checking") return { text: "Checking CDDIS...", busy: true };

    if (cddisStatus === "error") {
      return {
        text: `CDDIS: ${cddisStatusMessage ?? "credentials missing or invalid"}`,
        busy: false,
      };
    }

    // The check is gated on a products directory, so without one it never runs
    // and cddisStatus stays "idle". Say what's actually needed instead of
    // "Ready" — but only replace that bland default, never a real message such
    // as "Pea binary unavailable: …".
    if (cddisStatus === "idle" && !productsDir && status === "Ready") {
      return { text: "Set a products directory in Settings", busy: false };
    }

    return { text: status, busy: false };
  })();

  return (
    <div className="statusbar" data-tour="status-bar">
      {busy && <div className="spinner" />}
      <span>{text}</span>
      {testStartedAt != null && (
        <span
          className="statusbar-progress"
          role="progressbar"
          aria-label="Stream test"
          aria-valuemin={0}
          aria-valuemax={STREAM_TEST_SECONDS}
          aria-valuenow={Math.min(STREAM_TEST_SECONDS, Math.floor(elapsed))}
        >
          <span style={{ width: `${Math.min(100, (elapsed / STREAM_TEST_SECONDS) * 100)}%` }} />
        </span>
      )}
      {activity && !activity.finished && activityHidden && (
        <button
          type="button"
          className="statusbar-update"
          onClick={showActivity}
          aria-label={`Show progress: ${activity.title}`}
          title={`Show progress: ${activity.title}`}
        >
          Show opening progress
        </button>
      )}
      {/* Stays visible regardless of the main status line: the app never
          auto-updates the pea binary, so this reopens the update popup after
          it's been dismissed. */}
      {peaUpdateAvailable && !isProcessing && (
        <button
          type="button"
          className="statusbar-update"
          onClick={() => setPeaUpdateModalOpen(true)}
          title={`pea ${peaUpdateAvailable} is available - review update`}
        >
          pea {peaUpdateAvailable} available →
        </button>
      )}
    </div>
  );
}
