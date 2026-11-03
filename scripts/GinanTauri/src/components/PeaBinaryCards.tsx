import { useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";
import { usePickers } from "../hooks/usePickers";
import PeaVersionControl from "./PeaVersionControl";
import { AlertIcon, FileIcon } from "./icons";
import type { PeaBinaryStatus } from "../types";

/**
 * Settings → PEA Binary, as two choices: the app-managed release and a custom
 * binary. They behave as a radio group: the active card is highlighted, and
 * clicking the other one switches to it.
 *
 * - Managed: back to the selected downloaded release (instant, from the local
 *   cache), or the latest release if nothing has been downloaded yet.
 * - Custom: back to the last custom binary used, or a file picker if there
 *   isn't one (or it has gone).
 *
 * The controls inside a card (version picker, file button) keep their own
 * actions; a click on them never counts as a click on the card.
 */
export default function PeaBinaryCards() {
  const peaBinaryPath = useAppStore((s) => s.peaBinaryPath);
  const peaVersion = useAppStore((s) => s.peaVersion);
  const peaUpdateAvailable = useAppStore((s) => s.peaUpdateAvailable);
  const lastCustomPeaPath = useAppStore((s) => s.lastCustomPeaPath);
  const managedPeaVersion = useAppStore((s) => s.managedPeaVersion);
  const { pickPeaBinary } = usePickers();

  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Only managed downloads carry a release tag, so no tag means custom.
  const customActive = !!peaBinaryPath && !peaVersion;
  const managedActive = !!peaBinaryPath && !!peaVersion;

  const switchToManaged = async () => {
    setBusy(true);
    setError(null);
    const s = useAppStore.getState();
    try {
      const info = await invoke<PeaBinaryStatus>("use_managed_pea");
      s.setPeaBinaryPath(info.path);
      s.setPeaVersion(info.version);
      s.setPeaUpdateAvailable(null);
      s.setStatus(`pea ${info.version ?? "managed"} ready`);
    } catch (e) {
      setError(String(e));
    } finally {
      setBusy(false);
    }
  };

  const switchToCustom = async () => {
    setBusy(true);
    setError(null);
    const s = useAppStore.getState();
    try {
      if (lastCustomPeaPath) {
        try {
          await invoke("set_pea_binary", { path: lastCustomPeaPath });
          s.setPeaBinaryPath(lastCustomPeaPath);
          s.setPeaVersion(null);
          s.setPeaUpdateAvailable(null);
          s.setStatus(`PEA binary: ${lastCustomPeaPath.split(/[\\/]/).pop()}`);
          return;
        } catch {
          // Moved or deleted since: forget it and ask for a file instead.
          s.setLastCustomPeaPath(null);
        }
      }
      await pickPeaBinary();
    } finally {
      setBusy(false);
    }
  };

  // The whole card takes a mouse click (except on the controls inside it); the
  // top section is the radio for keyboard and screen readers. The controls sit
  // outside that radio because a radio's contents are hidden from assistive tech.
  const cardClick = (active: boolean, onSelect: () => void) => (e: React.MouseEvent) => {
    if (active || busy || (e.target as HTMLElement).closest("button, select, a, input")) return;
    onSelect();
  };
  const radioProps = (label: string, active: boolean, onSelect: () => void) => ({
    className: "pea-card-select",
    role: "radio",
    "aria-label": label,
    "aria-checked": active,
    tabIndex: 0,
    onKeyDown: (e: React.KeyboardEvent) => {
      if (e.key !== "Enter" && e.key !== " ") return;
      e.preventDefault();
      if (!active && !busy) onSelect();
    },
  });

  // Each card's text is fixed: it describes that option, not whether it is the
  // one running. Active is shown by the border, the dot and the pill alone, and
  // the pill keeps its space when hidden so nothing shifts on a switch.
  return (
    <div className="pea-cards-wrap">
      <div className="pea-cards" role="radiogroup" aria-label="PEA binary source">
        <div
          className={`pea-card${managedActive ? " is-active" : ""}`}
          onClick={cardClick(managedActive, switchToManaged)}
        >
          <div {...radioProps("Managed release", managedActive, switchToManaged)}>
          <header className="pea-card-head">
            <span className="pea-card-dot" aria-hidden="true" />
            <h3>Managed release</h3>
            <span className="pea-card-active" aria-hidden={!managedActive}>Active</span>
          </header>

          <p className="pea-card-status">
            <span className="pea-card-version">{managedPeaVersion ?? "Not downloaded"}</span>
            {peaUpdateAvailable && (
              <span className="pea-card-note">{peaUpdateAvailable} available</span>
            )}
          </p>
          <p className="pea-card-desc">
            Downloaded and kept by the app. Versions you download stay on disk,
            so switching between them is instant.
          </p>
          </div>

          <div className="pea-card-control">
            <PeaVersionControl />
          </div>
        </div>

        <div
          className={`pea-card${customActive ? " is-active" : ""}`}
          onClick={cardClick(customActive, switchToCustom)}
        >
          <div {...radioProps("Custom binary", customActive, switchToCustom)}>
          <header className="pea-card-head">
            <span className="pea-card-dot" aria-hidden="true" />
            <h3>Custom binary</h3>
            <span className="pea-card-active" aria-hidden={!customActive}>Active</span>
          </header>

          <p className="pea-card-status">
            <span className="pea-card-version">Version unknown</span>
          </p>
          <p className="pea-card-desc">
            A pea build you already have. Not updated by the app.
          </p>
          </div>

          <div className="pea-card-control">
            <button className="btn-secondary" onClick={pickPeaBinary}>
              <FileIcon />
              Choose file...
            </button>
          </div>

          <code className="pea-card-path">{lastCustomPeaPath ?? "No file chosen"}</code>
        </div>
      </div>

      {error && (
        <p className="pea-cards-error" role="alert">
          <AlertIcon size={14} /> {error}
        </p>
      )}
    </div>
  );
}
