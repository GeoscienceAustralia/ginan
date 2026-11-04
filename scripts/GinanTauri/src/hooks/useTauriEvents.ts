import { useEffect } from "react";
import { listen } from "@tauri-apps/api/event";
import { invoke } from "@tauri-apps/api/core";
import type { PosFileInfo } from "../types";
import type { LiveSnapshot } from "../types/streams";
import { resultsDir, useAppStore } from "../store/appStore";
import { WF } from "../config/workflowSteps";

/**
 * GinanUI runs its plotting straight off the back of a finished run
 * (`main_window.py::_run_visualisation`) and reports what it found in the
 * Workflow pane. The Visualization tab draws the same `.pos` files natively, so
 * the equivalent step here is to list them and log the same lines — whether or
 * not that tab happens to be open.
 */
async function reportPositionPlots(
  appendWorkflowLine: (line: string) => void,
) {
  const outputDir = resultsDir(useAppStore.getState());
  if (!outputDir) return;

  appendWorkflowLine(WF.generatingPosPlots());
  let found: PosFileInfo[];
  try {
    found = await invoke<PosFileInfo[]>("list_pos_files", { outputDir });
  } catch (e) {
    appendWorkflowLine(WF.warning(String(e)));
    appendWorkflowLine(WF.noVisualisations());
    return;
  }

  if (found.length === 0) {
    appendWorkflowLine(WF.noPosFiles(outputDir));
    appendWorkflowLine(WF.noPositionPlots());
    appendWorkflowLine(WF.noVisualisations());
    return;
  }

  appendWorkflowLine(WF.foundPosFiles(found.length, outputDir));
  found.forEach((f) => appendWorkflowLine(WF.posFileEntry(f.name)));
  appendWorkflowLine(WF.posPlotsGenerated(found.length));
}

/**
 * Ask the backend for the latest live snapshot (it survives tab switches in
 * managed state) and store it. Safe to call anywhere; a missing snapshot or a
 * failed call leaves the store as it was.
 */
export async function pullLiveStreamStats(): Promise<void> {
  try {
    const snap = await invoke<LiveSnapshot | null>("get_live_stream_stats");
    if (snap) useAppStore.getState().setLiveSnapshot(snap);
  } catch {
    // Command absent or no run yet: the next live-stream-stats event fills it.
  }
}

export function useTauriEvents() {
  const appendLines = useAppStore((s) => s.appendLines);
  const appendWorkflowLine = useAppStore((s) => s.appendWorkflowLine);
  const appendWorkflowLines = useAppStore((s) => s.appendWorkflowLines);
  const setIsProcessing = useAppStore((s) => s.setIsProcessing);
  const setStatus = useAppStore((s) => s.setStatus);
  const setConnectionFault = useAppStore((s) => s.setConnectionFault);
  // The post-open fetch chain reports itself through these same channels, so
  // each listener also feeds the activity panel. The store drops the update
  // when the named step isn't running, so no listener has to check first.
  const updateActivityStep = useAppStore((s) => s.updateActivityStep);
  const updateActiveActivityStep = useAppStore((s) => s.updateActiveActivityStep);
  // A run is the only thing that writes .pos files, so the end of one is when
  // the Visualization tab needs to look again. An errored run counts: PEA may
  // have written epochs before it stopped, and those are worth plotting.
  const bumpPosEpoch = useAppStore((s) => s.bumpPosEpoch);
  const setLiveSnapshot = useAppStore((s) => s.setLiveSnapshot);
  const applyNtripEvent = useAppStore((s) => s.applyNtripEvent);

  useEffect(() => {
    let cancelled = false;
    const unlisteners: Array<() => void> = [];

    // Buffer line events and flush at most once per animation frame.
    // Without this, PEA's per-epoch output (hundreds of lines/sec) re-renders
    // the console on every line and freezes the UI.
    //
    // Two independent buffers, one per log channel (see appStore): raw PEA
    // stdout/stderr feeds the Console channel, product-download progress feeds
    // the Workflow channel — mirroring the original GinanUI split.
    let consoleBuffer: string[] = [];
    let workflowBuffer: string[] = [];
    let scheduled = false;
    const flush = () => {
      scheduled = false;
      if (consoleBuffer.length > 0) {
        const batch = consoleBuffer;
        consoleBuffer = [];
        appendLines(batch);
      }
      if (workflowBuffer.length > 0) {
        const batch = workflowBuffer;
        workflowBuffer = [];
        appendWorkflowLines(batch);
      }
    };
    const schedule = () => {
      if (!scheduled) {
        scheduled = true;
        requestAnimationFrame(flush);
      }
    };
    const queueConsole = (line: string) => {
      consoleBuffer.push(line);
      schedule();
    };
    const queueWorkflow = (line: string) => {
      workflowBuffer.push(line);
      schedule();
    };

    const setup = async () => {
      // Raw PEA subprocess output → Console channel.
      const u1 = await listen<string>("pea-line", (e) => queueConsole(e.payload));
      if (cancelled) { u1(); return; }
      unlisteners.push(u1);

      // Low-level technical detail from the backend (PEA command echo, config
      // generation, RINEX obs-type dump) → Console channel, mirroring the
      // original GinanUI (Python) which routed this detail to Console.
      const u9 = await listen<string>("console-line", (e) => queueConsole(e.payload));
      if (cancelled) { u9(); return; }
      unlisteners.push(u9);

      const u2 = await listen<number>("pea-finished", (e) => {
        flush();
        setIsProcessing(false);
        setStatus(`Done - exit code ${e.payload}`);
        // High-level completion → Workflow; the exit code is Console detail.
        // GinanUI has one step for a clean finish and one for a failure.
        appendWorkflowLine(
          e.payload === 0
            ? WF.peaCompleted()
            : WF.peaFailed(`exited with code ${e.payload}`),
        );
        queueConsole(`\n✅ PEA finished with exit code ${e.payload}`);
        bumpPosEpoch();
        void reportPositionPlots(appendWorkflowLine);
      });
      if (cancelled) { u2(); return; }
      unlisteners.push(u2);

      const u3 = await listen<string>("pea-error", (e) => {
        flush();
        setIsProcessing(false);
        setStatus(`Error: ${e.payload}`);
        appendWorkflowLine(WF.peaFailed(String(e.payload)));
        queueConsole(`\n❌ PEA error: ${e.payload}`);
        bumpPosEpoch();
        void reportPositionPlots(appendWorkflowLine);
      });
      if (cancelled) { u3(); return; }
      unlisteners.push(u3);

      // The user pressed Stop. The backend sends this instead of pea-error,
      // since the kill looks like a crash from the exit status alone. Stop
      // already logged its own Workflow steps (RunControls.handleStop), so
      // this only settles state and leaves a Console marker. Whatever PEA
      // wrote before the kill is still worth listing.
      const u12 = await listen("pea-stopped", () => {
        flush();
        setIsProcessing(false);
        setStatus("Stopped");
        queueConsole("\n🛑 PEA stopped by you");
        bumpPosEpoch();
        void reportPositionPlots(appendWorkflowLine);
      });
      if (cancelled) { u12(); return; }
      unlisteners.push(u12);

      // Product downloads are part of the workflow the user follows.
      const u4 = await listen<string>("product-line", (e) => queueWorkflow(e.payload));
      if (cancelled) { u4(); return; }
      unlisteners.push(u4);

      const u5 = await listen<{ filename: string; percent: number }>(
        "product-progress",
        (e) => {
          setStatus(`Downloading ${e.payload.filename} · ${e.payload.percent}%`);
          // Whichever step is running owns the download (the sample BIA on
          // open, the SP3/CLK set on a run), so it takes the percentage.
          updateActiveActivityStep({
            detail: `Downloading ${e.payload.filename}`,
            percent: e.payload.percent,
          });
        },
      );
      if (cancelled) { u5(); return; }
      unlisteners.push(u5);

      const u6 = await listen<{ host: string; message: string }>(
        "connection-fault",
        (e) => setConnectionFault(
          `Cannot reach ${e.payload.host}. Check your internet connection.`
        ),
      );
      if (cancelled) { u6(); return; }
      unlisteners.push(u6);

      // Decompress / Hatanaka-convert progress. Empty string = finished.
      const u8 = await listen<string>("prepare-progress", (e) => {
        if (e.payload) {
          setStatus(e.payload);
          updateActivityStep("prepare", { detail: e.payload });
        }
      });
      if (cancelled) { u8(); return; }
      unlisteners.push(u8);

      const u7 = await listen<string>(
        "ppp-scan-progress",
        (e) => {
          // Empty string = scan finished / cleared; non-empty = current server name.
          if (e.payload) {
            setStatus(e.payload);
            updateActivityStep("scan", { detail: e.payload });
          }
        },
      );
      if (cancelled) { u7(); return; }
      unlisteners.push(u7);

      // Streams tab data lives in the store so it survives tab switches.
      const u10 = await listen<LiveSnapshot>("live-stream-stats", (e) => setLiveSnapshot(e.payload));
      if (cancelled) { u10(); return; }
      unlisteners.push(u10);

      const u11 = await listen<unknown>("ntrip-stats", (e) => applyNtripEvent(e.payload));
      if (cancelled) { u11(); return; }
      unlisteners.push(u11);

      void pullLiveStreamStats();
    };

    setup();

    return () => {
      cancelled = true;
      unlisteners.forEach((u) => u());
    };
  }, [appendLines, appendWorkflowLine, appendWorkflowLines, setIsProcessing, setStatus, updateActivityStep, updateActiveActivityStep, bumpPosEpoch, setLiveSnapshot, applyNtripEvent]);
}
