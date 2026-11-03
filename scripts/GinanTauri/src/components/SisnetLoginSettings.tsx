import { useCallback, useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { SISNET_HOST } from "./SbasStreamBody";

/**
 * The SISNet login for SouthPAN's SBAS corrections, on Settings › Credentials.
 * Saved to netrc like the caster's (`save_ntrip_credentials` with the SISNet
 * host) and spliced in by the backend when a config is written; it is never
 * held in the store.
 */
export default function SisnetLoginSettings() {
  const [hasLogin, setHasLogin] = useState<boolean | null>(null);
  const [editing, setEditing] = useState(false);
  const [user, setUser] = useState("");
  const [pass, setPass] = useState("");
  const [error, setError] = useState<string | null>(null);

  const refresh = useCallback(() => {
    invoke<string[]>("list_ntrip_hosts")
      .then((hosts) => setHasLogin(hosts.includes(SISNET_HOST)))
      .catch(() => setHasLogin(false));
  }, []);
  useEffect(refresh, [refresh]);

  const save = async () => {
    try {
      await invoke("save_ntrip_credentials", { host: SISNET_HOST, username: user, password: pass });
      setUser("");
      setPass("");
      setError(null);
      setEditing(false);
      refresh();
    } catch (e) {
      setError(String(e));
    }
  };

  if (editing) {
    return (
      <div className="sisnet-login">
        <input
          className="rt-input"
          placeholder="Username"
          autoComplete="off"
          value={user}
          onChange={(e) => setUser(e.target.value)}
        />
        <input
          className="rt-input"
          type="password"
          placeholder="Password"
          autoComplete="off"
          value={pass}
          onChange={(e) => setPass(e.target.value)}
        />
        <div>
          <button className="btn-secondary" onClick={save} disabled={!user.trim() || !pass}>
            Save
          </button>{" "}
          <button className="btn-secondary" onClick={() => setEditing(false)}>Cancel</button>
        </div>
        {error && <div className="rt-hint">{error}</div>}
      </div>
    );
  }
  return (
    <div className="settings-inline">
      <span className={`status-pill${hasLogin ? " status-pill--ok" : hasLogin === false ? " status-pill--warn" : ""}`}>
        {hasLogin === null ? "Checking..." : hasLogin ? `Saved for ${SISNET_HOST}` : "No login saved"}
      </span>
      <button className="btn-secondary" onClick={() => setEditing(true)}>
        {hasLogin ? "Change login..." : "Add login..."}
      </button>
    </div>
  );
}
