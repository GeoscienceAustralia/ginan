import { useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";

/** Explicit sign-out is separate from dismissing the credential editor. */
export default function CddisSignOut() {
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const signOut = async () => {
    setBusy(true);
    setError(null);
    try {
      await invoke("forget_cddis_credentials");
      const state = useAppStore.getState();
      state.setOfflineMode(true);
      state.setCddisStatus("error", "Signed out of Earthdata. Set credentials to download products.");
      // Invalidates an in-flight startup check that used the previous login.
      state.bumpCddisEpoch();
    } catch (e) {
      setError(`Could not sign out: ${String(e)}`);
    } finally {
      setBusy(false);
    }
  };

  return <>
    <button className="btn-secondary" disabled={busy} onClick={() => void signOut()}>
      {busy ? "Signing out..." : "Sign out of Earthdata"}
    </button>
    {error && <span role="alert">{error}</span>}
  </>;
}
