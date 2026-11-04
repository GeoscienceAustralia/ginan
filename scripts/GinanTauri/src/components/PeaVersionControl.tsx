import { useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";
import type { PeaBinaryStatus, PeaVersionInfo } from "../types";
import { PackageIcon, AlertIcon } from "./icons";

/**
 * Inline version picker for the auto-managed pea binary (Settings → PEA Binary).
 *
 * Renders as a single row — version dropdown plus a download button — meant to
 * sit alongside the binary-location override. The installed version tag is shown
 * by the settings row itself, so it is not repeated here.
 *
 * The dropdown pre-selects the installed managed release, the version the card
 * shows beside its heading, and falls back to the latest release (what a first
 * launch downloads) when none is installed. Switching is on demand only — the app never
 * replaces an existing binary on its own. Downloads report progress through the
 * shared product-progress events, so the status bar drives the percentage.
 */
const ACTION_LABELS = ["Download & use", "Downloading...", "Switching...", "In use", "Use"];

/** The installed release if it is listed, otherwise the latest (the list is newest-first). */
const defaultTag = (list: PeaVersionInfo[], installed: string | null) =>
  list.find((v) => v.tag === installed)?.tag ?? list[0]?.tag ?? "";

export default function PeaVersionControl() {
  const peaVersion = useAppStore((s) => s.peaVersion);
  const managedPeaVersion = useAppStore((s) => s.managedPeaVersion);
  const setPeaBinaryPath = useAppStore((s) => s.setPeaBinaryPath);
  const setPeaVersion = useAppStore((s) => s.setPeaVersion);
  const setPeaUpdateAvailable = useAppStore((s) => s.setPeaUpdateAvailable);
  const setStatus = useAppStore((s) => s.setStatus);

  const [versions, setVersions] = useState<PeaVersionInfo[] | null>(null);
  const [selected, setSelected] = useState<string>("");
  const [loading, setLoading] = useState(false);
  const [downloading, setDownloading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Fetch the list once when the control first mounts (i.e. the page is opened).
  useEffect(() => {
    let active = true;
    setLoading(true);
    setError(null);
    invoke<PeaVersionInfo[]>("list_pea_versions")
      .then((list) => {
        if (!active) return;
        setVersions(list);
      })
      .catch((e) => active && setError(String(e)))
      .finally(() => active && setLoading(false));
    return () => {
      active = false;
    };
  }, []);

  // Select the installed release once the list arrives, and follow it if it
  // changes elsewhere (startup detection, the update prompt). Keyed on the list
  // arriving rather than its contents, so marking a release downloaded does not
  // undo the user's pick.
  const listed = versions !== null;
  useEffect(() => {
    if (versions) setSelected(defaultTag(versions, managedPeaVersion));
  }, [listed, managedPeaVersion]);

  const download = async () => {
    if (!selected || downloading) return;
    setDownloading(true);
    setError(null);
    try {
      const info = await invoke<PeaBinaryStatus>("download_pea_version", {
        tag: selected,
      });
      setPeaBinaryPath(info.path);
      setPeaVersion(info.version);
      // Whatever we just installed is now current — clear any stale prompt.
      setPeaUpdateAvailable(null);
      setVersions((list) =>
        list?.map((v) => (v.tag === selected ? { ...v, downloaded: true } : v)) ?? list,
      );
      setStatus(`pea ${info.version ?? selected} ready`);
    } catch (e) {
      setError(String(e));
      setStatus(`Could not download pea ${selected}: ${e}`);
    } finally {
      setDownloading(false);
    }
  };

  // "In use" only while the managed release is what's running: with a custom
  // binary active, peaVersion is null and every release is a switch.
  const isInstalled = !!selected && selected === peaVersion;
  const isDownloaded = !!versions?.find((v) => v.tag === selected)?.downloaded;
  const label = downloading
    ? isDownloaded ? "Switching..." : "Downloading..."
    : isInstalled ? "In use" : isDownloaded ? "Use" : "Download & use";

  return (
    <div className="pea-version">
      <select
        className="pea-version-select"
        value={selected}
        disabled={loading || downloading || !versions || versions.length === 0}
        onChange={(e) => setSelected(e.target.value)}
        aria-label="Available pea versions"
      >
        {loading && <option value="">Loading...</option>}
        {!loading && (!versions || versions.length === 0) && (
          <option value="">No versions available</option>
        )}
        {versions?.map((v) => (
          <option key={v.tag} value={v.tag}>
            {v.tag}
            {v.tag === peaVersion ? " (in use)" : v.downloaded ? " (downloaded)" : ""}
          </option>
        ))}
      </select>
      <button
        className="btn-secondary"
        onClick={download}
        disabled={downloading || loading || !selected || isInstalled}
        title={
          isInstalled
            ? "This version is in use"
            : isDownloaded
              ? "Already downloaded: switches instantly"
              : undefined
        }
      >
        <PackageIcon />
        {/* Every label is laid out in the same spot and only the current one
            shows, so the button is always as wide as the longest and never
            resizes as the selection changes. */}
        <span className="pea-version-labels">
          {ACTION_LABELS.map((l) => (
            <span key={l} aria-hidden={l !== label} className={l === label ? undefined : "is-hidden"}>
              {l}
            </span>
          ))}
        </span>
      </button>
      {error && (
        <span className="status-pill status-pill--warn" role="status" title={error}>
          <AlertIcon size={14} /> Failed
        </span>
      )}
    </div>
  );
}
