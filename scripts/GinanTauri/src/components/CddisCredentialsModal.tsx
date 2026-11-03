import { useState } from "react";
import { useAppStore } from "../store/appStore";
import { AlertIcon } from "./icons";
import {
  EarthdataCredentialFields,
  EarthdataLinks,
  saveAndTestCddisCredentials,
} from "./EarthdataCredentials";

/**
 * Modal for entering NASA Earthdata credentials. Saves them to netrc via
 * `save_cddis_credentials`, then verifies them with `test_cddis_connection`.
 * On success the app leaves offline mode; the modal stays open on failure so
 * the user can correct the password.
 */
export default function CddisCredentialsModal() {
  const open = useAppStore((s) => s.credentialsModalOpen);
  const setOpen = useAppStore((s) => s.setCredentialsModalOpen);
  const setOfflineMode = useAppStore((s) => s.setOfflineMode);
  const bumpCddisEpoch = useAppStore((s) => s.bumpCddisEpoch);

  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  if (!open) return null;

  const close = () => {
    setError(null);
    setUsername("");
    setPassword("");
    setOpen(false);
  };

  const handleSave = async () => {
    setBusy(true);
    setError(null);
    try {
      await saveAndTestCddisCredentials(username, password);
      setOfflineMode(false);
      // Re-run the startup chain (connectivity → static products → ATX)
      // now that valid credentials exist. Without this the "No Earthdata
      // credentials in netrc" banner and the Process-button gate stay stuck on
      // their startup "error" state until the app is restarted.
      bumpCddisEpoch();
      setBusy(false);
      close();
    } catch (e) {
      setBusy(false);
      const message = typeof e === "string" ? e : String(e);
      setError(message);
      // The saved pair may already have changed. A failed verification must
      // not leave the previous login's green connection indicator behind.
      setOfflineMode(true);
      useAppStore.getState().setCddisStatus("error", message);
      bumpCddisEpoch();
    }
  };

  return (
    <div className="modal-overlay" role="dialog" aria-modal="true" aria-label="CDDIS Credentials">
      <div className="modal-card">
        <h2>Earthdata / CDDIS Credentials</h2>
        <p className="modal-desc">
          NASA CDDIS is the sole source of PPP products, so a free NASA
          Earthdata account is required to download them. If you don't have one
          yet, register using the link below - it takes a couple of minutes.
          Credentials are stored locally in your netrc file and never sent
          anywhere except NASA.
        </p>

        <EarthdataLinks className="modal-links" />

        <EarthdataCredentialFields
          username={username}
          password={password}
          onUsernameChange={setUsername}
          onPasswordChange={setPassword}
          disabled={busy}
        />

        {error && (
          <div className="connection-warning" style={{ borderRadius: "var(--radius-sm)", border: "1px solid var(--warning-line)" }}>
            <span className="connection-warning-icon"><AlertIcon size={14} /></span>
            <span className="connection-warning-text">{error}</span>
          </div>
        )}

        <div className="modal-actions">
          <button className="btn-ghost" onClick={close} disabled={busy}>
            Cancel
          </button>
          <button
            className="btn-primary"
            onClick={handleSave}
            disabled={busy || !username.trim() || !password}
          >
            {busy ? "Verifying..." : "Save & Test"}
          </button>
        </div>
      </div>
    </div>
  );
}
