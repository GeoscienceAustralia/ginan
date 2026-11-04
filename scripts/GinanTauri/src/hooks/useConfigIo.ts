import { useEffect, useRef, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { open, save } from "@tauri-apps/plugin-dialog";
import {
  useAppStore,
  buildConfigInputs,
  epochToDatetimeLocal,
} from "../store/appStore";
import type { PeaConfigExtract, RinexData } from "../types";

/** The row a message belongs to, so it renders beside the button pressed. */
export type ConfigIoRow = "import-config" | "export-config" | "reset-config";

export type ConfigIoFeedback = {
  row: ConfigIoRow;
  tone: "ok" | "warn" | "error";
  text: string;
  /** Full path, hung off the message as a tooltip when there is one. */
  detail?: string;
};

/** How long a message stays up before clearing itself. */
const FEEDBACK_MS = 8000;

/**
 * Import / export / reset of the whole UI config, as offered in Settings.
 *
 * State is read through `useAppStore.getState()` at click time rather than
 * closed over at render time: Settings builds its rows inside a `useMemo` that
 * does not list these handlers, so a render-time closure would keep exporting
 * whatever the config looked like when the memo last ran.
 *
 * Each action also reports its outcome through `feedback`. `setStatus` alone is
 * not enough from here: the status bar sits behind the settings surface — and
 * is not rendered at all until an observation file is open — so these three
 * buttons otherwise appear to do nothing whatsoever.
 */
export function useConfigIo() {
  const [feedback, setFeedback] = useState<ConfigIoFeedback | null>(null);
  const timer = useRef<number | null>(null);

  const clearFeedback = () => {
    if (timer.current !== null) window.clearTimeout(timer.current);
    setFeedback(null);
  };

  // A message reports something that has already finished, so it expires on its
  // own rather than sitting in the panel until the next action.
  const report = (
    row: ConfigIoRow,
    tone: ConfigIoFeedback["tone"],
    text: string,
    detail?: string,
  ) => {
    if (timer.current !== null) window.clearTimeout(timer.current);
    setFeedback({ row, tone, text, detail });
    timer.current = window.setTimeout(() => setFeedback(null), FEEDBACK_MS);
  };

  useEffect(
    () => () => {
      if (timer.current !== null) window.clearTimeout(timer.current);
    },
    [],
  );

  const exportConfig = async () => {
    // The same object a run writes and the Config YAML tab previews, so an
    // exported file cannot drift from what this configuration actually does,
    // the processing parameters and directories included. The backend writes
    // the config the run would, paths and all, and only swaps the caster
    // logins for `<USER>:<PASS>`.
    const inputs = buildConfigInputs();
    if (!inputs) {
      report(
        "export-config",
        "warn",
        "Nothing to export yet - open an observation file and set an output directory",
      );
      return;
    }

    const path = await save({
      filters: [{ name: "YAML Config", extensions: ["yaml", "yml"] }],
      // Named like the file a run writes into the output directory.
      defaultPath: inputs.realtime ? "rt_ppp_config.yaml" : "ppp_config.yaml",
    });
    if (!path) return;

    const { setStatus } = useAppStore.getState();

    try {
      await invoke("export_pea_config", { path, inputs });
      const fileName = path.split(/[\\/]/).pop();
      setStatus(`Config exported: ${fileName}`);
      report("export-config", "ok", `Exported to ${fileName}`, path);
    } catch (e) {
      setStatus(`Export error: ${e}`);
      report("export-config", "error", `Export failed: ${e}`);
    }
  };

  const importConfig = async () => {
    const path = await open({
      multiple: false,
      filters: [{ name: "YAML Config", extensions: ["yaml", "yml"] }],
    });
    if (!path || typeof path !== "string") return;

    const {
      setRinexPath, setRinexData, clearExtraRinex, setStartEpoch, setEndEpoch,
      setEpochInterval, setConstellationCodes,
      setStatus, applyImportedConfig,
    } = useAppStore.getState();

    try {
      const config = await invoke<PeaConfigExtract>("import_pea_config", { path });
      // Apply the settings (mode, epochs, output formats, constellation codes)
      // synchronously via the store. Directories are not among them — see
      // `applyImportedConfig`.
      applyImportedConfig(config);

      const fileName = path.split(/[\\/]/).pop();
      if (config.rinex_path) {
        // RINEX path was found on this machine — reload it, then re-apply
        // any epoch / constellation overrides from the imported config.
        // An imported config names one observation file; it replaces the
        // selection rather than joining files that were already open.
        clearExtraRinex();
        setRinexPath(config.rinex_path);
        try {
          const data = await invoke<RinexData>("parse_rinex", { path: config.rinex_path });
          setRinexData(data);
          // setRinexData resets epochs and codes from the file; put back the
          // values that were explicitly stored in the imported config.
          if (config.start_epoch != null) setStartEpoch(epochToDatetimeLocal(config.start_epoch));
          if (config.end_epoch != null) setEndEpoch(epochToDatetimeLocal(config.end_epoch));
          if (config.epoch_interval != null) setEpochInterval(config.epoch_interval);
          if (config.constellation_codes != null) setConstellationCodes(config.constellation_codes);
          setStatus(`Config imported: ${fileName} - RINEX reloaded`);
          report("import-config", "ok", `Imported ${fileName} - RINEX reloaded`, path);
        } catch {
          setStatus(`Config imported: ${fileName} - RINEX not found on this machine`);
          // Half a success: the settings landed, the observation file did not.
          report("import-config", "warn", `Imported ${fileName} - RINEX could not be reloaded`, path);
        }
      } else {
        setStatus(`Config imported: ${fileName}`);
        report("import-config", "ok", `Imported ${fileName}`, path);
      }
    } catch (e) {
      setStatus(`Import error: ${e}`);
      report("import-config", "error", `Import failed: ${e}`);
    }
  };

  const resetConfig = () => {
    useAppStore.getState().resetConfig();
    // Everything this touches lives in the sidebar, which Settings is covering
    // — without a word here the button looks inert.
    report("reset-config", "ok", "Processing mode, time window and output formats reset");
  };

  return { importConfig, exportConfig, resetConfig, feedback, clearFeedback };
}
