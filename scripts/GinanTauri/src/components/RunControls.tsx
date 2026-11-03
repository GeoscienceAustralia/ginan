import { invoke } from "@tauri-apps/api/core";
import { useShallow } from "zustand/react/shallow";
import {
  useAppStore, buildConfigInputs, expectedEpochCount, runOutputDir, runStreamTest,
  serialForProducts, streamFingerprint,
} from "../store/appStore";
import { PlayIcon, SatelliteIcon, StopIcon } from "./icons";
import { WF } from "../config/workflowSteps";
import type { NtripAuthResult } from "../types";
import { runBlockers } from "./runGate";
import RunBlockers from "./RunBlockers";

/**
 * Process / Stop pair. Lives in the header next to the config actions — this
 * is the only place the run is started or cancelled.
 */
export default function RunControls() {
  // Only what this component reads directly. Everything that feeds the
  // generated config comes from `buildConfigInputs`, which reads the store
  // itself — see the Config YAML tab, which previews the same object.
  const {
    rinexPath, rinexData, outputDir, productsDir,
    startEpoch, endEpoch, epochInterval,
    pppSelection, pppCatalogue,
    isProcessing, peaBinaryPath, pppServer,
    configOverwrite, offlineMode,
    clearConsole, setIsProcessing, setStatus, appendLine, appendWorkflowLine,
    cddisStatus, startEpochProgress,
    inputMode, realtime, realtimeStorageCapMb, inputProducts,
    streamTestStatus, streamTestFingerprint,
  } = useAppStore(
    useShallow((s) => ({
      rinexPath: s.rinexPath,
      rinexData: s.rinexData,
      outputDir: s.outputDir,
      productsDir: s.productsDir,
      startEpoch: s.startEpoch,
      endEpoch: s.endEpoch,
      epochInterval: s.epochInterval,
      pppSelection: s.pppSelection,
      pppCatalogue: s.pppCatalogue,
      isProcessing: s.isProcessing,
      peaBinaryPath: s.peaBinaryPath,
      pppServer: s.pppServer,
      configOverwrite: s.configOverwrite,
      offlineMode: s.offlineMode,
      clearConsole: s.clearConsole,
      setIsProcessing: s.setIsProcessing,
      setStatus: s.setStatus,
      appendLine: s.appendLine,
      appendWorkflowLine: s.appendWorkflowLine,
      cddisStatus: s.cddisStatus,
      startEpochProgress: s.startEpochProgress,
      inputMode: s.inputMode,
      realtime: s.realtime,
      realtimeStorageCapMb: s.realtimeStorageCapMb,
      inputProducts: s.inputProducts,
      streamTestStatus: s.streamTestStatus,
      streamTestFingerprint: s.streamTestFingerprint,
    })),
  );

  const handleProcess = async () => {
    // Built by the store helper, which is also what the Config YAML tab
    // previews — so what the user inspects is exactly what gets written.
    // In realtime mode the helper skips the RINEX/epoch gates and attaches
    // the `realtime` block instead.
    // The three guards GinanUI logs before it starts a run, in the same order,
    // so a refused Process click leaves a reason in the Workflow tab rather
    // than only a line of status text.
    if (isProcessing) {
      appendWorkflowLine(WF.alreadyProcessing());
      return;
    }
    // Fixed once here: the default name carries the minute, and a realtime
    // start writes the config more than once.
    const runDir = runOutputDir(useAppStore.getState());
    const inputs = buildConfigInputs(runDir);
    if (!inputs) {
      appendWorkflowLine(WF.selectInputsFirst());
      setStatus(
        inputMode === "realtime"
          ? "Add at least one observation stream and an output directory first"
          : "Select a RINEX file and output directory first",
      );
      return;
    }
    // Realtime pulls its corrections from the caster, so only a file-based run
    // is blocked by being offline — the same split GinanUI makes.
    if (inputMode !== "realtime" && offlineMode) {
      appendWorkflowLine(WF.cannotProcessOffline());
      setStatus("Processing requires an internet connection");
      return;
    }

    try {
      // Only the raw PEA output is per-run. The Workflow tab keeps the whole
      // session, as GinanUI's Workflow pane does — the steps that led up to
      // this run (file opened, catalogue scanned, biases validated) are part
      // of the record, and the panel has its own Clear button.
      clearConsole();
      setIsProcessing(true);
      useAppStore.getState().setRunDir(runDir);
      // Static support files are downloaded once at app startup (see App.tsx),
      // so they are not re-fetched per Process click.

      // Write the config first so the user's current settings land on disk
      // even if the dynamic-product download fails or is cancelled.
      // Config-file generation is a low-level detail → Console channel.
      setStatus("Writing config...");
      let configPath = await invoke<string>("write_config", {
        inputs,
        overwrite: configOverwrite,
      });
      appendLine(
        configOverwrite
          ? `Config written to: ${configPath}`
          : `Reusing existing config (overwrite off): ${configPath}`,
      );

      // Realtime runs get their orbits/clocks over SSR — there are no
      // date-bounded product files to download, so the dynamic-products stage
      // is skipped entirely. Trying to run it would fail on missing SP3s.
      if (inputMode === "realtime") {
        const serial = serialForProducts(inputProducts);

        // Check the caster login before PEA is involved.
        //
        // PEA's own failure for a rejected login is silence: it connects, gets
        // a 401, never receives an observation, and the stream test below hits
        // its timeout with nothing to show for it. That is hard to diagnose,
        // because the timeout names neither the caster nor the login. A rejection stops the run here instead; anything else
        // only warns, since an unreachable caster has refused nothing and the
        // stream test is the better judge of that.
        if (realtime.caster_url) {
          setStatus("Checking the caster login...");
          try {
            const auth = await invoke<NtripAuthResult>("verify_ntrip_credentials", {
              casterUrl: realtime.caster_url,
              mountpoint:
                inputProducts.obs.find((p) => p.type === "ntrip" && p.value !== "")?.value ??
                null,
            });
            if (auth.state === "rejected") {
              appendWorkflowLine(WF.casterRejected(auth.detail));
              setStatus(`${auth.host} rejected the saved login - check it in Settings › Credentials`);
              setIsProcessing(false);
              return;
            }
            if (auth.state === "inconclusive") {
              appendWorkflowLine(WF.casterUncheckable(auth.detail));
            }
          } catch (e) {
            appendWorkflowLine(WF.casterUncheckable(String(e)));
          }
        }

        // Watch the streams briefly first, then take the constellations and
        // epoch interval from what actually arrived rather than the defaults.
        configPath = await runStreamTest(runDir);

        setStatus("Starting real-time data stream...");
        appendWorkflowLine("📡 Starting PEA in realtime mode...");
        // A stream has no end epoch, so only the running count is shown.
        startEpochProgress(null);
        await invoke("start_pea_realtime", {
          options: {
            config_path: configPath,
            caster_url: realtime.caster_url,
            output_dir: runDir,
            storage_cap_mb: realtimeStorageCapMb,
            serial,
            sbas_mode: realtime.sbas?.mode ?? null,
          },
        });
        setStatus("Running real-time data stream...");
      } else {
        setStatus("Checking date-dependent products...");
        appendWorkflowLine(WF.startingDownloads());
        appendLine(
          `Checking SP3/CLK/BRDC/BIA for observation date(s) - ${pppSelection.provider}/${pppSelection.series}/${pppSelection.project}...`,
        );
        const catalogueEntry =
          pppCatalogue?.entries.find(
            (e) =>
              e.provider === pppSelection.provider &&
              e.series === pppSelection.series &&
              e.project === pppSelection.project,
          ) ?? null;
        // Products ensure_dynamic_products could not fetch (0 when all are in place).
        let missing: number | undefined;
        try {
          missing = await invoke<number>("ensure_dynamic_products", {
            productsDir: inputs.products_dir,
            startEpoch: inputs.start_epoch,
            endEpoch: inputs.end_epoch,
            provider: pppSelection.provider,
            series: pppSelection.series,
            project: pppSelection.project,
            preferredMirror: pppServer,
            catalogueEntry,
            // The RINEX file's own INTERVAL, not `epochInterval`: that one is
            // the processing interval the user may edit, and it also falls
            // back to 30 when the header has none. null means "unknown" and
            // keeps the usual 30 s clocks; under 30 s the downloader prefers
            // the 5 s clock products.
            rinexIntervalS: rinexData?.epoch_interval ?? null,
          });
        } catch (e) {
          // Only a download failure earns GinanUI's download-error step; the
          // outer catch keeps the generic warning for everything else.
          if (String(e) !== "Cancelled") {
            appendWorkflowLine(WF.downloadError(String(e)));
          }
          throw e;
        }
        appendWorkflowLine(missing ? WF.downloadsIncomplete(missing) : WF.downloadsFinished());

        setStatus("Running PEA...");
        appendWorkflowLine(WF.startingPea());
        startEpochProgress(expectedEpochCount(startEpoch, endEpoch, epochInterval));

        await invoke("start_pea", { configPath });
      }
    } catch (e) {
      setIsProcessing(false);
      if (String(e) === "Cancelled") {
        // handleStop already logged the stop sequence — don't repeat it here.
        setStatus("Stopped");
      } else {
        setStatus(`Error: ${e}`);
        appendWorkflowLine(WF.warning(String(e)));
      }
    }
  };

  const handleStop = async () => {
    appendWorkflowLine(WF.stopRequested());
    try {
      appendWorkflowLine(WF.stoppingPea());
      await invoke("stop_pea");
      setIsProcessing(false);
      setStatus("Stopped");
      appendWorkflowLine(WF.stopped());
    } catch (e) {
      setIsProcessing(false);
      setStatus(`Stop failed: ${e}`);
      appendWorkflowLine(WF.warning(String(e)));
    }
  };

  // Every unmet precondition, in the order worth fixing them (runGate.ts).
  // All of them go in the tooltip, and RunBlockers lists them with a way to
  // each fix for anyone who cannot hover a disabled button.
  const blockers = runBlockers({
    peaBinaryPath, outputDir, productsDir, rinexPath, startEpoch, cddisStatus,
    inputMode, realtime, inputProducts,
  });
  // Monitor stream must have passed for exactly these inputs; any change since makes it stale.
  const streamTestStale =
    streamTestStatus === "passed" && streamTestFingerprint !== streamFingerprint({ realtime, inputProducts });
  // Monitor stream sits beside Process for as long as it is the next step.
  const needsMonitor = inputMode === "realtime" && (streamTestStatus !== "passed" || streamTestStale);
  if (inputMode === "realtime") {
    if (streamTestStatus !== "passed") {
      blockers.push({
        id: "stream-monitor",
        message: "Run Monitor stream first to detect the constellations",
        fix: { kind: "tab", tab: "streams", label: "Show Streams" },
      });
    } else if (streamTestFingerprint !== streamFingerprint({ realtime, inputProducts })) {
      blockers.push({
        id: "stream-monitor-stale",
        message: "Settings changed since the last Monitor stream - run it again",
        fix: { kind: "tab", tab: "streams", label: "Show Streams" },
      });
    }
  }

  const canProcess = !isProcessing && blockers.length === 0;
  const title = isProcessing
    ? "Processing..."
    : blockers.length > 0
    ? blockers.map((b) => b.message).join("\n")
    : "Run PEA with the current configuration";

  // One button: Process while idle, Stop while PEA is running, so the bar
  // never offers an action that cannot be taken.
  if (isProcessing) {
    return (
      <button className="btn-danger" onClick={handleStop} title="Stop the running PEA process">
        <StopIcon /> Stop
      </button>
    );
  }

  const monitoring = streamTestStatus === "running";
  // runStreamTest needs an output directory and a stream to write a config for.
  const canMonitor = !monitoring && !!outputDir && realtime.obs_streams.length > 0;

  return (
    <>
      {blockers.length > 0 && <RunBlockers blockers={blockers} />}
      {needsMonitor && (
        <button
          className="btn-monitor"
          onClick={() => void runStreamTest().catch(() => {})}
          disabled={!canMonitor}
          aria-busy={monitoring || undefined}
          title={
            monitoring
              ? "Watching each mountpoint for about 10 s..."
              : canMonitor
              ? "Check the streams for about 10 s and fill in their constellations, codes and interval. Process needs this first."
              : "Add an observation stream and an output directory first"
          }
        >
          <SatelliteIcon /> {monitoring ? "Monitoring..." : streamTestStale ? "Monitor again" : "Monitor stream"}
        </button>
      )}
      <button
        className="btn-primary"
        onClick={handleProcess}
        disabled={!canProcess}
        title={title}
      >
        <PlayIcon /> Process
      </button>
    </>
  );
}
