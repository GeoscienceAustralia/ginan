import { useEffect, useRef, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore, DEFAULT_CASTER_URL } from "../store/appStore";
import type { InputSection, NtripAuthResult } from "../types";
import InfoTip from "./InfoTip";
import { ResetIcon } from "./icons";
import { formatTime, timeZoneAbbr } from "../utils/time";
import {
  casterUrlIssue,
  extractHost,
  looksLikeCorrectionMount,
  parseNtripUrl,
} from "../config/ntrip";

/**
 * The NTRIP caster + sign-in surface, on the Settings › Credentials page.
 *
 * The caster URL and the login are machine setup, not per-run input, so they
 * live here with the other credentials rather than in the streaming config
 * panel. The streaming panel keeps only the two stream lists and a
 * per-list "mountpoints here / full URI here" toggle, and reads the fetched
 * sourcetable back out of the store.
 *
 * The caster URL held here is a plain `scheme://host/` with no login in it; the
 * backend splices the netrc login in when it writes a run's config, and an
 * exported YAML gets `<USER>:<PASS>` in its place, so nothing secret reaches it.
 */
/** Green only once the caster has actually accepted the login. */
function statusTone(
  hasLogin: boolean,
  auth: { state: string },
): "is-ok" | "is-warn" | "is-bad" {
  if (!hasLogin) return "is-warn";
  if (auth.state === "accepted") return "is-ok";
  if (auth.state === "rejected") return "is-bad";
  return "is-warn";
}

export default function NtripCasterSettings() {
  const casterUrl = useAppStore((s) => s.casterUrl);
  const timeZone = useAppStore((s) => s.timeZone);
  const setCasterUrl = useAppStore((s) => s.setCasterUrl);
  const sourcetableStatus = useAppStore((s) => s.ntripSourcetableStatus);
  const fetchSourcetable = useAppStore((s) => s.fetchNtripSourcetable);
  const addProduct = useAppStore((s) => s.addProduct);
  const removeProduct = useAppStore((s) => s.removeProduct);

  const [savedHosts, setSavedHosts] = useState<string[]>([]);
  const [credsOpen, setCredsOpen] = useState(false);
  const [credsHost, setCredsHost] = useState("");
  const [credsUser, setCredsUser] = useState("");
  const [credsPass, setCredsPass] = useState("");
  const [credsMessage, setCredsMessage] = useState<string | null>(null);
  const [credsBusy, setCredsBusy] = useState(false);
  const [checkedAt, setCheckedAt] = useState<number | null>(null);
  const [parseOpen, setParseOpen] = useState(false);
  const [parseUrl, setParseUrl] = useState("");
  const [parseMessage, setParseMessage] = useState<string | null>(null);
  const parseInputRef = useRef<HTMLInputElement>(null);

  /**
   * What the caster said about the stored login, if it has been asked.
   *
   * Saving credentials writes them to netrc and contacts nothing, so a stored
   * login on its own only means "saved", never "works". `unknown` is the
   * honest state until the caster has
   * answered, and it is where a login loaded from netrc starts.
   */
  const [auth, setAuth] = useState<
    | { state: "unknown" }
    | { state: "checking" }
    | { state: "accepted" }
    | { state: "rejected"; detail: string }
    | { state: "inconclusive"; detail: string }
  >({ state: "unknown" });

  const refreshHosts = () => {
    invoke<string[]>("list_ntrip_hosts").then(setSavedHosts).catch(() => setSavedHosts([]));
  };
  useEffect(refreshHosts, []);

  const casterHost = extractHost(casterUrl);
  const hasLogin = casterHost !== null && savedHosts.includes(casterHost);
  useEffect(() => {
    setAuth({ state: "unknown" });
    setCheckedAt(null);
  }, [casterUrl]);
  const checkId = useRef(0);
  useEffect(() => () => { checkId.current++; }, []);
  // A mountpoint the user already configured makes the check cover the stream
  // the run will actually ask for; without one the backend picks from the
  // sourcetable.
  const obsMount = useAppStore(
    (s) => s.inputProducts.obs.find((p) => p.type === "ntrip" && p.value !== "")?.value,
  );
  const warning = casterUrlIssue(casterUrl);
  const loading = sourcetableStatus.state === "loading";

  const openCredsModal = () => {
    setCredsHost(casterHost ?? "");
    setCredsUser("");
    setCredsPass("");
    setCredsMessage(null);
    setCredsOpen(true);
  };

  /**
   * Ask the caster whether the stored login works.
   *
   * The save flow closes on success; failed checks stay in the dialog.
   */
  const checkLogin = async (mountpoint?: string, url = casterUrl): Promise<NtripAuthResult | null> => {
    if (!url) return null;
    const id = ++checkId.current;
    setAuth({ state: "checking" });
    try {
      const result = await invoke<NtripAuthResult>("verify_ntrip_credentials", {
        casterUrl: url,
        mountpoint: mountpoint ?? obsMount ?? null,
      });
      if (id !== checkId.current || useAppStore.getState().casterUrl !== url) return null;
      setCheckedAt(Date.now());
      setAuth(
        result.state === "accepted"
          ? { state: "accepted" }
          : { state: result.state, detail: result.detail },
      );
      return result;
    } catch (e) {
      if (id !== checkId.current || useAppStore.getState().casterUrl !== url) return null;
      setCheckedAt(Date.now());
      // The command itself failed, which says nothing about the password.
      setAuth({ state: "inconclusive", detail: String(e) });
      return null;
    }
  };

  const saveCredentials = async () => {
    setCredsBusy(true);
    setCredsMessage(null);
    try {
      await invoke("save_ntrip_credentials", {
        host: credsHost,
        username: credsUser,
        password: credsPass,
      });
      setCredsPass("");
      refreshHosts();
      // Saved is not signed in. Only the caster can say that, so ask it before
      // claiming anything.
      setCredsMessage(`Saved. Checking the login with ${credsHost}...`);
      const result = await checkLogin();
      if (result?.state === "accepted") {
        setCredsUser("");
        setCredsOpen(false);
        return;
      }
      setCredsMessage(
        result === null
          ? `Login saved for ${credsHost}, but it could not be checked.`
          : result.state === "rejected"
          ? `${result.detail} The login is saved, but it will not work for a run.`
          : `Login saved for ${credsHost}, but it could not be checked: ${result.detail}`,
      );
    } catch (e) {
      setCredsMessage(`Could not save the login: ${e}`);
    } finally {
      setCredsBusy(false);
    }
  };

  const forgetCredentials = async (host: string) => {
    try {
      await invoke("forget_ntrip_credentials", { host });
      checkId.current++;
      setAuth({ state: "unknown" });
      setCheckedAt(null);
      refreshHosts();
    } catch {
      /* the button just refreshes on failure */
    }
  };

  const openParseModal = () => {
    setParseUrl("");
    setParseMessage(null);
    setParseOpen(true);
    setTimeout(() => parseInputRef.current?.focus(), 50);
  };

  const applyParsedUrl = async () => {
    const parsed = parseNtripUrl(parseUrl.trim());
    if (!parsed) {
      setParseMessage("Could not parse - expected https://user:pass@host/ or https://user:pass@host/MOUNT");
      return;
    }
    const { casterRoot, host, user, pass, mount } = parsed;
    setCasterUrl(casterRoot);
    if (mount) {
      const section: InputSection = looksLikeCorrectionMount(mount) ? "corr" : "obs";
      // An empty NTRIP placeholder in that section is replaced, not kept beside the new mountpoint.
      for (const p of useAppStore.getState().inputProducts[section]) {
        if (p.type === "ntrip" && p.value === "") removeProduct(section, p.id);
      }
      addProduct(section, "ntrip", mount);
    }
    if (user && pass) {
      try {
        await invoke("save_ntrip_credentials", { host, username: user, password: pass });
        refreshHosts();
        setParseMessage(`Caster set to ${host}, login saved. Checking it...`);
        const result = await checkLogin(mount ?? undefined, casterRoot);
        setParseMessage(
          result === null
            ? `Caster set to ${host}. The login was saved but could not be checked.`
            : result.state === "accepted"
            ? `Caster set to ${host} and signed in.${mount ? ` Mountpoint ${mount} added.` : ""}`
            : result.state === "rejected"
            ? `Caster set to ${host}, but it rejected the login in the URL. ${result.detail}`
            : `Caster set to ${host}. The login was saved but could not be checked: ${result.detail}`,
        );
      } catch (e) {
        setParseMessage(`Caster set, but the sign-in could not be saved: ${e}`);
      }
    } else {
      setParseMessage(`Caster set to ${host}.${mount ? ` Mountpoint ${mount} added.` : ""} No login found in the URL - use Sign in.`);
    }
  };

  return (
    <div className="ntrip-caster">
      <input
        className={`rt-input${warning ? " rt-input-warn" : ""}`}
        type="text"
        value={casterUrl}
        onChange={(e) => setCasterUrl(e.target.value)}
        // Leaving the field empty puts the GA caster back, rather than a
        // placeholder that looks filled in but vanishes on the first keystroke.
        onBlur={() => {
          if (!casterUrl.trim()) setCasterUrl(DEFAULT_CASTER_URL);
        }}
        placeholder={DEFAULT_CASTER_URL}
        aria-label="Caster URL"
      />
      {warning && <div className="rt-warning">{warning}</div>}

      {casterHost && (
        <div className={`ntrip-caster-status ${statusTone(hasLogin, auth)}`}>
          {!hasLogin
            ? `Not signed in to ${casterHost}`
            : auth.state === "checking"
            ? `Checking the login with ${casterHost}...`
            : auth.state === "accepted"
            ? `Signed in to ${casterHost}`
            : auth.state === "rejected"
            ? `${casterHost} rejected the saved login. ${auth.detail}`
            : auth.state === "inconclusive"
            ? `Login saved for ${casterHost}, not checked: ${auth.detail}`
            : `Login saved for ${casterHost} - not checked yet`}
          {hasLogin && auth.state !== "checking" && (
            <button className="btn-link ntrip-check" onClick={() => void checkLogin()}>
              Check now
            </button>
          )}
        </div>
      )}
      {hasLogin && checkedAt && auth.state !== "checking" && (
        <div className="rt-hint" role="status">
          Last login check completed at {formatTime(checkedAt, timeZone, { seconds: true })} {timeZoneAbbr(checkedAt, timeZone)}.
        </div>
      )}

      <div className="ntrip-caster-actions">
        <button
          className={hasLogin ? "btn-secondary" : "btn-primary"}
          onClick={openCredsModal}
          disabled={!casterHost}
          title={casterHost ? `Store the username and password for ${casterHost} on this machine` : "Set a caster URL first"}
        >
          {hasLogin ? "Update sign-in" : "Sign in"}
        </button>
        <button
          className="btn-secondary"
          onClick={openParseModal}
          title="Paste one complete NTRIP URL and let the app split out the server, the login and the mountpoint"
        >
          Paste full URL...
        </button>
        <span className="ntrip-caster-browse">
          <button
            className="btn-secondary"
            onClick={fetchSourcetable}
            disabled={loading || !casterHost}
            title="Read the caster's sourcetable so the stream fields can suggest mountpoints"
          >
            {loading ? "Browsing..." : "Browse streams"}
          </button>
          <button
            className="btn-icon"
            onClick={fetchSourcetable}
            disabled={loading || !casterHost}
            aria-label="Refresh the stream list"
            title="Fetch the sourcetable again"
          >
            <ResetIcon size={14} />
          </button>
        </span>
      </div>

      {sourcetableStatus.state === "error" && (
        <div className="rt-warning">{sourcetableStatus.message}</div>
      )}
      {sourcetableStatus.state === "ok" && (
        <div className="rt-hint">
          {sourcetableStatus.count} stream{sourcetableStatus.count === 1 ? "" : "s"} on{" "}
          {sourcetableStatus.host} - the stream fields now suggest mountpoints.
        </div>
      )}

      {savedHosts.length > 0 && (
        <ul className="rt-hosts-list">
          {savedHosts.map((h) => (
            <li key={h}>
              {h}{" "}
              <button
                className="btn-secondary rt-sign-out"
                onClick={() => forgetCredentials(h)}
                title={`Remove the stored login for ${h}`}
              >
                Sign out
              </button>
            </li>
          ))}
        </ul>
      )}

      {credsOpen && (
        <div className="rt-modal-backdrop" onClick={() => { if (!credsBusy) { setCredsPass(""); setCredsOpen(false); } }}>
          <div className="rt-modal" role="dialog" aria-modal="true" aria-label="Sign in to the caster" onClick={(e) => e.stopPropagation()}>
            <h3>Sign in to the caster</h3>
            <label className="rt-label">Host</label>
            <input
              className="rt-input"
              type="text"
              value={credsHost}
              readOnly
              placeholder="ntrip.data.gnss.ga.gov.au"
            />
            <label className="rt-label">Username</label>
            <input
              className="rt-input"
              type="text"
              value={credsUser}
              disabled={credsBusy}
              onChange={(e) => setCredsUser(e.target.value)}
            />
            <label className="rt-label">Password</label>
            <input
              className="rt-input"
              type="password"
              value={credsPass}
              disabled={credsBusy}
              onChange={(e) => setCredsPass(e.target.value)}
            />
            {credsMessage && <p className="rt-message">{credsMessage}</p>}
            <div className="rt-row">
              <button className="btn-primary" onClick={saveCredentials} disabled={credsBusy || !credsUser.trim() || !credsPass}>
                {credsBusy ? "Verifying..." : "Save"}
              </button>
              <button className="btn-secondary" disabled={credsBusy} onClick={() => { setCredsPass(""); setCredsOpen(false); }}>
                Close
              </button>
            </div>
          </div>
        </div>
      )}

      {parseOpen && (
        <div className="rt-modal-backdrop" onClick={() => setParseOpen(false)}>
          <div className="rt-modal" onClick={(e) => e.stopPropagation()}>
            <h3>
              Paste a full NTRIP URL
              <InfoTip text="The server, the login and the mountpoint are separated into the right places automatically." />
            </h3>
            <label className="rt-label">Full URL</label>
            <input
              ref={parseInputRef}
              className="rt-input"
              type="text"
              value={parseUrl}
              onChange={(e) => { setParseUrl(e.target.value); setParseMessage(null); }}
              placeholder="https://user:pass@ntrip.data.gnss.ga.gov.au/MOUNT"
            />
            {parseMessage && <p className="rt-message">{parseMessage}</p>}
            <div className="rt-row">
              <button className="btn-primary" onClick={applyParsedUrl}>
                Apply
              </button>
              <button className="btn-secondary" onClick={() => setParseOpen(false)}>
                Close
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
