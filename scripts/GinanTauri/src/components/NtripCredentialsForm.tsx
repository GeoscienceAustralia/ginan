import { useCallback, useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";

const DEFAULT_HOST = "ntrip.data.gnss.ga.gov.au";

/**
 * NTRIP caster credentials: save one host's login, and forget hosts already
 * stored.
 *
 * These are *not* the CDDIS/Earthdata credentials — real-time streams come from
 * a caster (ntrip.data.gnss.ga.gov.au by default) with its own account. Mixing
 * the two up is the easy mistake here, which is why the host field is shown
 * even though it is almost always the default.
 *
 * Credentials go to the OS-level store via the backend; nothing is held in this
 * component beyond the in-flight password, which is cleared on save.
 *
 * Used by the real-time onboarding step. It writes through the same backend
 * commands as the caster sign-in on Settings › Credentials.
 */
export default function NtripCredentialsForm({
  compact = false,
}: {
  /** Drops the saved-hosts list, for the tighter onboarding layout. */
  compact?: boolean;
}) {
  const [host, setHost] = useState(DEFAULT_HOST);
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [savedHosts, setSavedHosts] = useState<string[]>([]);
  const [message, setMessage] = useState<{ text: string; ok: boolean } | null>(null);
  const [busy, setBusy] = useState(false);

  const refreshHosts = useCallback(() => {
    invoke<string[]>("list_ntrip_hosts")
      .then(setSavedHosts)
      .catch(() => setSavedHosts([]));
  }, []);

  useEffect(refreshHosts, [refreshHosts]);

  const save = async () => {
    const trimmedHost = host.trim();
    if (!trimmedHost || !username.trim()) {
      setMessage({ text: "Host and username are required.", ok: false });
      return;
    }
    setBusy(true);
    try {
      await invoke("save_ntrip_credentials", {
        host: trimmedHost,
        username: username.trim(),
        password,
      });
      // Never keep the password around once it has been handed over.
      setPassword("");
      setMessage({ text: `Saved credentials for ${trimmedHost}`, ok: true });
      refreshHosts();
    } catch (e) {
      setMessage({ text: `Save failed: ${e}`, ok: false });
    } finally {
      setBusy(false);
    }
  };

  const forget = async (h: string) => {
    try {
      await invoke("forget_ntrip_credentials", { host: h });
      setMessage({ text: `Removed credentials for ${h}`, ok: true });
    } catch (e) {
      setMessage({ text: `Could not remove ${h}: ${e}`, ok: false });
    }
    refreshHosts();
  };

  return (
    <div className="ntrip-creds">
      <div className="ntrip-creds-fields">
        <label className="ntrip-creds-field">
          <span>Caster host</span>
          <input
            type="text"
            value={host}
            onChange={(e) => setHost(e.target.value)}
            placeholder={DEFAULT_HOST}
            spellCheck={false}
          />
        </label>
        <label className="ntrip-creds-field">
          <span>Username</span>
          <input
            type="text"
            autoComplete="username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            spellCheck={false}
          />
        </label>
        <label className="ntrip-creds-field">
          <span>Password</span>
          <input
            type="password"
            autoComplete="current-password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            onKeyDown={(e) => e.key === "Enter" && void save()}
          />
        </label>
      </div>

      <div className="ntrip-creds-actions">
        <button className="btn-primary" onClick={save} disabled={busy}>
          {busy ? "Saving..." : "Save credentials"}
        </button>
        {message && (
          <span className={`ntrip-creds-msg${message.ok ? " is-ok" : " is-err"}`}>
            {message.text}
          </span>
        )}
      </div>

      {!compact && savedHosts.length > 0 && (
        <div className="ntrip-creds-saved">
          <span className="ntrip-creds-saved-label">Saved casters</span>
          <ul>
            {savedHosts.map((h) => (
              <li key={h}>
                <code>{h}</code>
                <button
                  className="btn-link"
                  onClick={() => forget(h)}
                  title={`Remove stored credentials for ${h}`}
                >
                  forget
                </button>
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
