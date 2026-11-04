import { useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";
import { AlertIcon } from "./icons";
import type { PeaBinaryStatus } from "../types";

/**
 * Popup shown when a newer pea release is available. The app never replaces an
 * installed binary on its own, so this is how the user is asked to update.
 * Updating downloads the newer release via `download_pea_version`; the modal
 * stays open on failure so the error is visible, and dismissing leaves the
 * current binary untouched (the status-bar pill reopens this later).
 */
export default function PeaUpdateModal() {
  const open = useAppStore((s) => s.peaUpdateModalOpen);
  const setOpen = useAppStore((s) => s.setPeaUpdateModalOpen);
  const peaVersion = useAppStore((s) => s.peaVersion);
  const peaUpdateAvailable = useAppStore((s) => s.peaUpdateAvailable);
  const setPeaBinaryPath = useAppStore((s) => s.setPeaBinaryPath);
  const setPeaVersion = useAppStore((s) => s.setPeaVersion);
  const setPeaUpdateAvailable = useAppStore((s) => s.setPeaUpdateAvailable);
  const setStatus = useAppStore((s) => s.setStatus);

  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Nothing to prompt about if the modal is closed or the update already cleared.
  if (!open || !peaUpdateAvailable) return null;

  const dismiss = () => {
    setError(null);
    setOpen(false);
  };

  const handleUpdate = async () => {
    setBusy(true);
    setError(null);
    try {
      const info = await invoke<PeaBinaryStatus>("download_pea_version", {
        tag: peaUpdateAvailable,
      });
      setPeaBinaryPath(info.path);
      setPeaVersion(info.version);
      setPeaUpdateAvailable(null);
      setStatus(`pea ${info.version ?? peaUpdateAvailable} ready`);
      setBusy(false);
      setOpen(false);
    } catch (e) {
      setBusy(false);
      setError(typeof e === "string" ? e : String(e));
    }
  };

  return (
    <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="PEA Binary Update">
      <div className="modal-card">
        <h2>PEA binary update available</h2>
        <p className="modal-desc">
          You can also pick a specific version later in Settings → PEA Binary.
        </p>

        <div className="field-row">
          <label>Installed</label>
          <span className="pea-update-version">{peaVersion ?? "Unknown"}</span>
        </div>
        <div className="field-row">
          <label>Available</label>
          <span className="pea-update-version pea-update-version--new">
            {peaUpdateAvailable}
          </span>
        </div>

        {error && (
          <div className="connection-warning" style={{ borderRadius: "var(--radius-sm)", border: "1px solid var(--warning-line)" }}>
            <span className="connection-warning-icon"><AlertIcon size={14} /></span>
            <span className="connection-warning-text">{error}</span>
          </div>
        )}

        <div className="modal-actions">
          <button className="btn-ghost" onClick={dismiss} disabled={busy}>
            Not now
          </button>
          <button className="btn-primary" onClick={handleUpdate} disabled={busy}>
            {busy ? "Updating..." : `Update to ${peaUpdateAvailable}`}
          </button>
        </div>
      </div>
    </div>
  );
}
