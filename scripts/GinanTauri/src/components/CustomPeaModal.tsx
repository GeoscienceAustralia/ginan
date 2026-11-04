import { useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";
import { AlertIcon } from "./icons";
import type { PeaBinaryStatus, PeaVersionInfo } from "../types";

/**
 * Notice shown when the binary in use is not the app-managed release, usually a
 * pea already installed on the machine. Its version is unknown, so without this
 * the user would assume they are running the latest release.
 *
 * Opened automatically once per custom path (App startup). "Keep" remembers the path so the same binary is
 * never raised again at launch; switching downloads the latest release through
 * `download_pea_version`, which replaces the override.
 */
export default function CustomPeaModal() {
  const open = useAppStore((s) => s.customPeaModalOpen);
  const peaBinaryPath = useAppStore((s) => s.peaBinaryPath);
  const peaVersion = useAppStore((s) => s.peaVersion);
  const dismissCustomPeaNotice = useAppStore((s) => s.dismissCustomPeaNotice);
  const setCustomPeaModalOpen = useAppStore((s) => s.setCustomPeaModalOpen);
  const setPeaBinaryPath = useAppStore((s) => s.setPeaBinaryPath);
  const setPeaVersion = useAppStore((s) => s.setPeaVersion);
  const setPeaUpdateAvailable = useAppStore((s) => s.setPeaUpdateAvailable);
  const setStatus = useAppStore((s) => s.setStatus);

  const [latest, setLatest] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const isCustom = !!peaBinaryPath && !peaVersion;

  // Look up the latest release only when the notice is actually showing; the
  // list is newest-first, as in the Settings version picker.
  useEffect(() => {
    if (!open || !isCustom) return;
    let active = true;
    setError(null);
    invoke<PeaVersionInfo[]>("list_pea_versions")
      .then((list) => active && setLatest(list[0]?.tag ?? null))
      .catch((e) => active && setError(`Could not check for releases: ${e}`));
    return () => {
      active = false;
    };
  }, [open, isCustom]);

  if (!open || !isCustom) return null;

  const keep = () => {
    setError(null);
    dismissCustomPeaNotice(peaBinaryPath);
  };

  const handleSwitch = async () => {
    if (!latest) return;
    setBusy(true);
    setError(null);
    try {
      const info = await invoke<PeaBinaryStatus>("download_pea_version", { tag: latest });
      setPeaBinaryPath(info.path);
      setPeaVersion(info.version);
      setPeaUpdateAvailable(null);
      setStatus(`pea ${info.version ?? latest} ready`);
      setBusy(false);
      setCustomPeaModalOpen(false);
    } catch (e) {
      setBusy(false);
      setError(typeof e === "string" ? e : String(e));
    }
  };

  return (
    <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="Custom PEA Binary">
      <div className="modal-card">
        <h2>Using a custom PEA binary</h2>
        <p className="modal-desc">
          This isn't the release Ginan manages, so its version is unknown and may
          not be the latest. You can change this later in Settings → PEA Binary.
        </p>

        <div className="field-row">
          <label>In use</label>
          <span className="custom-pea-path">{peaBinaryPath}</span>
        </div>
        <div className="field-row">
          <label>Latest</label>
          <span className="pea-update-version pea-update-version--new">
            {latest ?? (error ? "Unavailable" : "Checking...")}
          </span>
        </div>

        {error && (
          <div className="connection-warning" style={{ borderRadius: "var(--radius-sm)", border: "1px solid var(--warning-line)" }}>
            <span className="connection-warning-icon"><AlertIcon size={14} /></span>
            <span className="connection-warning-text">{error}</span>
          </div>
        )}

        <div className="modal-actions">
          <button className="btn-ghost" onClick={keep} disabled={busy}>
            Keep this binary
          </button>
          <button className="btn-primary" onClick={handleSwitch} disabled={busy || !latest}>
            {busy ? "Downloading..." : latest ? `Switch to ${latest}` : "Switch to latest"}
          </button>
        </div>
      </div>
    </div>
  );
}
