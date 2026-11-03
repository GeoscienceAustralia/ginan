import { useCallback, useEffect, useRef, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore, buildConfigInputs } from "../store/appStore";
import { CheckIcon, AlertIcon, ExportIcon, ResetIcon } from "./icons";

interface ConfigFileStatus {
  path: string;
  exists: boolean;
  contents: string | null;
}

/**
 * YAML inspector: the exact config a run would hand to PEA, plus control over
 * whether pressing Process rewrites it.
 *
 * Lives on the Configuration page in Settings, next to the import/export/reset
 * actions it belongs with.
 *
 * The preview goes through the same `generate_config_yaml` the writer uses (via
 * `preview_config_yaml`), and the inputs come from the shared
 * `buildConfigInputs`, so it cannot drift from what actually gets written.
 *
 * Why the overwrite switch matters: PEA accepts far more options than this UI
 * exposes. Turning overwrite off lets a user hand-edit the active mode's YAML for
 * something the sidebar has no control for — the 4.1.2 dry-run and loading
 * options, say — and still press Process without the edits being clobbered.
 */
export default function ConfigYamlInspector() {
  const outputDir = useAppStore((s) => s.outputDir);
  const rinexPath = useAppStore((s) => s.rinexPath);
  const inputMode = useAppStore((s) => s.inputMode);
  const realtimeReady = useAppStore((s) => s.realtime.obs_streams.length > 0);
  const configOverwrite = useAppStore((s) => s.configOverwrite);
  const setConfigOverwrite = useAppStore((s) => s.setConfigOverwrite);

  // Re-render the preview whenever anything that feeds the config changes.
  const configSignature = useAppStore((s) =>
    JSON.stringify([
      s.mode, s.markerName, s.receiverType, s.antennaType, s.antennaOffset,
      s.startEpoch, s.endEpoch, s.epochInterval,
      s.outputPos, s.outputGpx, s.outputTrace, s.outputSnx, s.outputSpp,
      s.constellationCodes, s.pppSelection, s.productsDir, s.outputDir, s.runName,
      s.rinexPath, s.extraRinex.map((f) => f.path), s.inputMode, s.realtime,
    ]),
  );

  const [yaml, setYaml] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [fileStatus, setFileStatus] = useState<ConfigFileStatus | null>(null);
  const [copied, setCopied] = useState(false);

  /**
   * What a press of Refresh did, shown next to the button.
   *
   * The effect below already regenerates on every change that feeds the
   * config, so a press almost always produces byte-identical YAML and nothing
   * on screen moves. The work Refresh alone does is re-reading the file on
   * disk, which no amount of in-app state can signal. So the button reports
   * its outcome rather than relying on the output visibly changing.
   */
  const [pressed, setPressed] = useState<"busy" | "updated" | "same" | null>(null);

  /** Last result, to tell "regenerated, same bytes" from "actually changed". */
  const lastResult = useRef<string | null>(null);

  const refresh = useCallback(async (manual = false) => {
    const inputs = buildConfigInputs();
    if (!inputs) {
      setYaml(null);
      setError(null);
      if (manual) setPressed(null);
      return;
    }
    if (manual) setPressed("busy");

    let nextYaml: string | null = null;
    let nextError: string | null = null;
    try {
      nextYaml = await invoke<string>("preview_config_yaml", { inputs });
    } catch (e) {
      nextError = String(e);
    }
    setYaml(nextYaml);
    setError(nextError);

    let nextStatus: ConfigFileStatus | null = null;
    try {
      nextStatus = await invoke<ConfigFileStatus>("config_file_status", {
        outputDir: inputs.output_dir,
        realtime: inputs.realtime != null,
      });
    } catch {
      nextStatus = null;
    }
    setFileStatus(nextStatus);

    // The file on disk counts: a run, or a hand edit outside the app, changes
    // the notice below without changing a byte of the preview.
    const signature = JSON.stringify([nextYaml, nextError, nextStatus]);
    const changed = lastResult.current !== null && signature !== lastResult.current;
    lastResult.current = signature;
    if (manual) setPressed(changed ? "updated" : "same");
  }, []);

  useEffect(() => {
    void refresh();
  }, [configSignature, refresh]);

  // Clear the outcome a moment after it appears, so it reads as a response to
  // the press rather than as permanent state.
  useEffect(() => {
    if (pressed !== "updated" && pressed !== "same") return;
    const t = setTimeout(() => setPressed(null), 2500);
    return () => clearTimeout(t);
  }, [pressed]);

  const copy = async () => {
    if (!yaml) return;
    await navigator.clipboard.writeText(yaml);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };

  // File mode needs a loaded observation; realtime mode gets observations from
  // its stream list. Both need an output directory.
  if (
    !outputDir ||
    (inputMode === "file" && !rinexPath) ||
    (inputMode === "realtime" && !realtimeReady)
  ) {
    return (
      <div className="yaml-empty">
        {!outputDir
          ? "Choose an output directory to preview the PEA config."
          : inputMode === "file"
          ? "Load a RINEX observation file to preview its PEA config."
          : "Configure an observation stream to preview its PEA config."}
      </div>
    );
  }

  // Only meaningful when we are not going to rewrite the file anyway.
  const onDiskDiffers =
    !configOverwrite &&
    fileStatus?.exists &&
    fileStatus.contents != null &&
    yaml != null &&
    fileStatus.contents !== yaml;

  return (
    <div className="yaml-panel">
      <div className="yaml-header">
        <span>Config YAML</span>
        <div className="yaml-actions">
          <label className="yaml-overwrite" title="Rewrite the active mode's config on every run">
            <input
              type="checkbox"
              checked={configOverwrite}
              onChange={(e) => setConfigOverwrite(e.target.checked)}
            />
            Overwrite on run
          </label>
          {pressed === "updated" && <span className="yaml-pressed">Updated</span>}
          {pressed === "same" && <span className="yaml-pressed is-quiet">No changes</span>}
          <button
            className="btn-ghost"
            onClick={() => void refresh(true)}
            disabled={pressed === "busy"}
            title="Regenerate the preview and re-read the file on disk"
          >
            <ResetIcon size={13} /> {pressed === "busy" ? "Refreshing..." : "Refresh"}
          </button>
          <button className="btn-ghost" onClick={copy} title="Copy YAML to clipboard">
            {copied ? <CheckIcon size={13} /> : <ExportIcon size={13} />}
            {copied ? "Copied" : "Copy"}
          </button>
        </div>
      </div>

      <div className="yaml-meta">
        <code>
          {fileStatus?.path ??
            `${outputDir}/${inputMode === "realtime" ? "rt_ppp_config.yaml" : "ppp_config.yaml"}`}
        </code>
        {fileStatus?.exists ? (
          <span className="yaml-tag">exists on disk</span>
        ) : (
          <span className="yaml-tag yaml-tag-new">not written yet</span>
        )}
      </div>

      {!configOverwrite && (
        <div className={`yaml-notice${onDiskDiffers ? " is-warn" : ""}`}>
          <AlertIcon size={13} />
          {onDiskDiffers ? (
            <span>
              Overwrite is off and the file on disk <strong>differs</strong> from the
              preview below. The next run will use the file on disk, not this.
            </span>
          ) : (
            <span>
              Overwrite is off - an existing config is reused as-is. Hand edits are
              preserved.
            </span>
          )}
        </div>
      )}

      {error ? (
        <div className="yaml-output">
          <span className="err-line">Could not generate config: {error}</span>
        </div>
      ) : (
        <pre className="yaml-output">{yaml ?? "Generating..."}</pre>
      )}
    </div>
  );
}
