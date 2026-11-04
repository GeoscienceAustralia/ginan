import { useEffect, useRef } from "react";
import { invoke } from "@tauri-apps/api/core";
import { open } from "@tauri-apps/plugin-dialog";
import { useAppStore } from "../store/appStore";
import type { RinexData, PppCatalogue } from "../types";
import { setAppTitle, BASE_TITLE } from "./useAppTitle";
import { allRinexPaths, withRinexSpan } from "../store/rinexFiles";
import { WF } from "../config/workflowSteps";

/**
 * Extensions the file dialog offers and drag-and-drop accepts. Shared so a
 * dropped file is held to the same standard as a picked one.
 *
 * `gz` covers any gzipped RINEX; `crx` and the `??d` short names are
 * Hatanaka-compressed. Both are unpacked by `prepare_rinex_input`.
 */
export const RINEX_EXTENSIONS = [
  "rnx", "gz", "obs", "crx",
  "22o", "23o", "24o", "21o", "20o",
  // Hatanaka-compressed RINEX 2 short names (…10d, …23d).
  "22d", "23d", "24d", "21d", "20d",
];

/**
 * True when `path` looks like a RINEX observation file we can open.
 *
 * Drag-and-drop has no dialog filter to lean on, so this is the only gate
 * between the OS and the load chain. Deliberately generous — matching the
 * dialog's "All Files" escape hatch is not possible here, so anything with a
 * plausible extension is allowed through and real validation happens in
 * `prepare_rinex_input` / `parse_rinex`, which report errors properly.
 */
export function looksLikeRinex(path: string): boolean {
  const ext = path.split(".").pop()?.toLowerCase();
  if (!ext) return false;
  // RINEX 2 short names end in a 2-digit year + o/d (e.g. .23o, .21d).
  return RINEX_EXTENSIONS.includes(ext) || /^\d{2}[od]$/.test(ext);
}

/**
 * Scan the mirrors for which PPP products actually exist over a RINEX
 * observation window and populate the catalogue. Failures are non-fatal — the
 * Products tab falls back to an empty catalogue (showing "No PPP products
 * available") rather than the pre-load placeholder.
 *
 * Module-level rather than a hook member so both `usePickers` and
 * `useCatalogueRescan` can trigger it without duplicating the logic.
 */
export async function scanPppCatalogue(
  data: RinexData,
  displayMarker: string,
  isStale: () => boolean = () => false,
) {
  const {
    setPppCatalogue, setStatus, appendWorkflowLine,
    beginActivityStep, resolveActivityStep, endActivity,
  } = useAppStore.getState();

  if (!data.start_epoch) {
    setPppCatalogue({ entries: [] });
    appendWorkflowLine(WF.noDateRange());
    resolveActivityStep("scan", "skipped", "File has no start epoch");
    endActivity();
    return;
  }
  setStatus("Scanning CDDIS for PPP products...");
  // The three steps GinanUI logs around its analysis-centre scan. Per-mirror
  // and per-combination detail stays on the Console channel, as it does there.
  appendWorkflowLine(WF.scanningArchive());
  appendWorkflowLine(WF.retrievingValidProducts());
  beginActivityStep("scan", "Contacting mirrors...");
  try {
    const catalogue = await invoke<PppCatalogue>("scan_ppp_catalogue", {
      startEpoch: data.start_epoch,
      endEpoch: data.end_epoch ?? "",
    });
    // The file was removed or replaced while the scan ran.
    if (isStale()) return;
    setPppCatalogue(catalogue);
    const providers = [...new Set(catalogue.entries.map((e) => e.provider))].sort();
    const providerCount = providers.length;
    appendWorkflowLine(
      providerCount > 0 ? WF.foundProviders(providers) : WF.noValidProviders(),
    );
    setStatus(
      `Loaded: ${displayMarker} · ${data.constellations.join(", ")} · ${providerCount} PPP provider(s) available`,
    );
    resolveActivityStep(
      "scan",
      "done",
      `${providerCount} provider(s), ${catalogue.entries.length} product combination(s)`,
    );
    // With no products there is no bias table to fetch either, so nothing
    // downstream will close the activity — do it here.
    if (catalogue.entries.length === 0) {
      resolveActivityStep("bia", "skipped", "No products for this window");
      endActivity();
    }
  } catch (e) {
    if (isStale()) return;
    setPppCatalogue({ entries: [] });
    appendWorkflowLine(WF.scanFailed(String(e)));
    setStatus(`PPP product scan failed: ${e}`);
    resolveActivityStep("scan", "error", String(e));
    endActivity(`Could not reach the product mirrors: ${e}`);
  }
}

/**
 * When credentials transition to valid, the credentials modal bumps
 * `cddisEpoch`. Re-run the PPP catalogue scan so providers that were invisible
 * on the earlier anonymous scan (CDDIS returns a login page, which yields an
 * empty catalogue) get picked up, without the user having to reload the RINEX.
 * The backend's generation counter makes a redundant scan self-cancelling, so
 * this is safe even mid-scan.
 *
 * The mount guard skips the initial render so we don't double-scan alongside
 * the scan that already fires when a RINEX is parsed.
 *
 * Mount exactly once (App) — `usePickers` is called from several components,
 * so this deliberately does not live in there.
 */
export function useCatalogueRescan() {
  const cddisEpoch = useAppStore((s) => s.cddisEpoch);
  const didMount = useRef(false);

  useEffect(() => {
    if (!didMount.current) {
      didMount.current = true;
      return;
    }
    const { rinexData, markerName, extraRinex, offlineMode } = useAppStore.getState();
    if (offlineMode) return;
    if (rinexData?.start_epoch) {
      void scanPppCatalogue(
        withRinexSpan(rinexData, extraRinex),
        markerName || rinexData.marker_name || "unknown",
      );
    }
  }, [cddisEpoch]);
}

/** Bumped by every load and by `cancelRinexLoad`; see `loadRinexFile`. */
let loadTicket = 0;

/**
 * Abandon any RINEX load in flight (the user removed the file). Its results are
 * dropped when they arrive, and its progress overlay is closed.
 */
export function cancelRinexLoad() {
  loadTicket++;
  useAppStore.getState().endActivity();
}

/** File name of a path, for status lines. */
function baseName(path: string): string | undefined {
  return path.split(/[\\/]/).pop();
}

/** Same file, whichever way the OS spelled the separators. */
const sameFile = (a: string, b: string) => a.replace(/\\/g, "/") === b.replace(/\\/g, "/");

/**
 * Validate the antenna against the loaded ATX file. null = unknown (no ATX yet
 * or the RINEX header had no antenna_type).
 */
async function checkAntenna(data: RinexData): Promise<void> {
  const { atxPath, setAntennaValid, appendWorkflowLine } = useAppStore.getState();
  if (atxPath && data.antenna_type && data.antenna_type.trim() !== "") {
    const valid = await invoke<boolean>("validate_antenna", {
      name: data.antenna_type,
      atxPath,
    }).catch(() => null);
    setAntennaValid(valid);
    appendWorkflowLine(
      valid ? WF.antennaVerified() : WF.antennaUnverified(atxPath),
    );
  } else {
    setAntennaValid(null);
    if (!atxPath) appendWorkflowLine(WF.antennaCheckSkipped());
  }
}

/**
 * The status-line name for the current selection: the primary's marker, plus
 * how many further files ride along with it.
 */
function selectionLabel(marker: string): string {
  const extras = useAppStore.getState().extraRinex.length;
  return extras > 0 ? `${marker} +${extras} more` : marker;
}

/**
 * Scan for products over the window the whole selection covers, not just the
 * primary file's. Products are downloaded for the store's start..end window,
 * which `addRinexFile` / `removeRinexFile` keep equal to that span, so the
 * catalogue has to be scanned for the same days.
 */
async function rescanForSelection(): Promise<void> {
  const { rinexData, extraRinex, markerName } = useAppStore.getState();
  if (!rinexData) return;
  await scanPppCatalogue(
    withRinexSpan(rinexData, extraRinex),
    selectionLabel(markerName || rinexData.marker_name || "unknown"),
  );
}

/**
 * Prepare and read each of `paths`, adding every file that parses to the
 * selection. A file that cannot be read (or is already selected) is reported
 * and skipped — one bad file in a batch does not sink the rest. Every file goes
 * through the same `prepare_rinex_input` → `parse_rinex` pair as the primary,
 * so a compressed or Hatanaka extra is unpacked exactly as a single file is.
 *
 * Returns how many were added and how many were skipped.
 */
async function addExtraFiles(
  paths: string[],
  // The primary load's guard: a load that has been overtaken or cancelled must
  // not add its extra files to whatever selection replaced it.
  isStale: () => boolean = () => false,
): Promise<{ added: number; skipped: number }> {
  const { appendWorkflowLine, setStatus } = useAppStore.getState();
  let added = 0;
  let skipped = 0;
  for (const selected of paths) {
    const shortName = baseName(selected);
    appendWorkflowLine(WF.rinexSelected(selected));
    try {
      setStatus(`Preparing ${shortName}...`);
      const prepared = await invoke<string>("prepare_rinex_input", { path: selected });
      if (allRinexPaths(useAppStore.getState()).some((p) => sameFile(p, prepared))) {
        appendWorkflowLine(WF.warning(`${shortName} is already selected`));
        skipped++;
        continue;
      }
      setStatus(`Parsing RINEX: ${baseName(prepared)}`);
      const data = await invoke<RinexData>("parse_rinex", { path: prepared });
      if (isStale()) break;
      useAppStore.getState().addRinexFile({ path: prepared, data });
      added++;
    } catch (e) {
      appendWorkflowLine(WF.warning(`Could not read ${shortName}: ${e}`));
      skipped++;
    }
  }
  return { added, skipped };
}

/** "2 added, 1 skipped" for an activity step's detail line. */
function describeAdded({ added, skipped }: { added: number; skipped: number }): string {
  return skipped > 0 ? `${added} added, ${skipped} skipped` : `${added} added`;
}

/**
 * Load RINEX observation files from absolute paths: prepare (gunzip /
 * Hatanaka-decode) → parse the header → scan CDDIS → fetch the bias table.
 * `selected` becomes the primary file and replaces whatever was open; `more`
 * are further files processed together with it.
 *
 * Separate from `pickRinex` so drag-and-drop can reuse the identical chain — a
 * dropped file must behave exactly like a picked one, including the activity
 * steps and the catalogue reset below.
 */
async function loadRinexFile(selected: string, more: string[] = []): Promise<void> {
  // Every load takes a ticket. A later load, or `cancelRinexLoad`, invalidates
  // it, and a stale load stops publishing: it must not put a removed or
  // replaced file back into the store. Pick and drop both come through here.
  const ticket = ++loadTicket;
  const isStale = () => ticket !== loadTicket;
  const {
    setRinexPath, setRinexData, clearExtraRinex, setStatus, appendWorkflowLine,
    setPppCatalogue, setBia, setCodeIntersection,
    startActivity, beginActivityStep, resolveActivityStep, endActivity,
  } = useAppStore.getState();
  const shortName = baseName(selected);
  appendWorkflowLine(WF.rinexSelected(selected));

  // Drop everything derived from the previous file *before* preparing this
  // one. The BIA effect keys on (pppSelection, pppCatalogue, startEpoch): if
  // the old catalogue is still in the store when setRinexData publishes the
  // new epoch, it fetches a BIA for the new week against the old catalogue
  // entry and asks the mirror for a product that does not exist — which
  // surfaces as "error decoding response body" on an HTML 404 page.
  // Nulling the catalogue makes that effect bail out until the rescan below
  // publishes a catalogue that actually matches this file.
  setPppCatalogue(null);
  setBia(null);
  setCodeIntersection(null);

  // The chain below is mostly network work against the product mirrors and
  // can run for a while. Publish it as steps so the user watches it happen
  // instead of guessing from a one-line status.
  startActivity(
    more.length > 0 ? `Opening ${shortName} and ${more.length} more` : `Opening ${shortName}`,
    [
      { key: "prepare", label: "Prepare observation file" },
      { key: "parse",   label: "Read RINEX header" },
      ...(more.length > 0 ? [{ key: "extra", label: "Read the other observation files" }] : []),
      { key: "scan",    label: "Scan CDDIS for products" },
      { key: "bia",     label: "Fetch signal bias table" },
    ],
  );

  // Unpack .gz and decode Hatanaka (.crx / .??d) up front. Returns the
  // original path untouched when the file is already plain RINEX, so the
  // rest of the flow — and the config written for PEA — always sees a real
  // .rnx on disk.
  let path = selected;
  try {
    setStatus(`Preparing ${shortName}...`);
    beginActivityStep("prepare", "Checking compression...");
    // Stage messages and percentage progress arrive from the backend on the
    // console-line / prepare-progress events, so nothing is logged here.
    path = await invoke<string>("prepare_rinex_input", { path: selected });
    resolveActivityStep("prepare", "done", path === selected ? "Already plain RINEX" : null);
  } catch (e) {
    if (isStale()) return;
    appendWorkflowLine(WF.warning(`Could not prepare ${shortName}: ${e}`));
    setStatus(`Could not read ${shortName}: ${e}`);
    resolveActivityStep("prepare", "error", String(e));
    endActivity(`Could not read ${shortName}: ${e}`);
    return;
  }

  if (isStale()) return;
  // A new primary starts a new selection. Cleared here rather than at the top
  // so a file that fails to prepare leaves the previous selection alone.
  clearExtraRinex();
  setRinexPath(path);
  setStatus(`Parsing RINEX: ${baseName(path)}`);

  try {
    beginActivityStep("parse");
    const data = await invoke<RinexData>("parse_rinex", { path });
    if (isStale()) return;
    setRinexData(data);
    resolveActivityStep(
      "parse",
      "done",
      `${data.marker_name ?? "unknown"} · ${data.constellations.join(", ")}`,
    );
    await checkAntenna(data);
    // Populated straight from the parsed header, so this is GinanUI's
    // "metadata extracted" step and its observation-code step in one place.
    appendWorkflowLine(WF.rinexMetadataExtracted());
    const populated = (["gps", "gal", "glo", "bds", "qzs"] as const)
      .filter((k) => (data.observation_codes?.[k] ?? []).length > 0)
      .map((k) => k.toUpperCase());
    appendWorkflowLine(
      populated.length > 0 ? WF.obsCodesPopulated(populated) : WF.noObsCodes(),
    );
    // Display the clipped/normalised marker that the store actually uses
    // (v3/v4 markers like "ALIC00AUS" are shortened to "ALIC").
    const displayMarker =
      useAppStore.getState().markerName || data.marker_name || "unknown";
    setStatus(`Loaded: ${displayMarker} · ${data.constellations.join(", ")}`);

    // The other files only widen the window and join the PEA input list; the
    // primary alone has seeded the fields above.
    if (more.length > 0) {
      beginActivityStep("extra");
      const result = await addExtraFiles(more, isStale);
      if (isStale()) return;
      resolveActivityStep("extra", "done", describeAdded(result));
    }
    const selectionData = withRinexSpan(data, useAppStore.getState().extraRinex);
    await scanPppCatalogue(selectionData, selectionLabel(displayMarker), isStale);
  } catch (e) {
    if (isStale()) return;
    appendWorkflowLine(WF.rinexMetadataError(String(e)));
    // Parse failed: reset the title so a stale filename doesn't linger after a
    // bad/corrupted file is selected. The success path is handled by
    // useAppTitle, which also covers config-restored files.
    await setAppTitle(BASE_TITLE);
    setStatus(`Parse error: ${e}`);
    resolveActivityStep("parse", "error", String(e));
    endActivity(`${shortName} could not be read: ${e}`);
  }
}

/**
 * Add `paths` to the files already open, widening the time window and
 * rescanning for products over the new span. With nothing open yet this is
 * just a load, so the first of them becomes the primary.
 */
async function addRinexFiles(paths: string[]): Promise<void> {
  if (paths.length === 0) return;
  const state = useAppStore.getState();
  if (!state.rinexPath || !state.rinexData) {
    await loadRinexFile(paths[0], paths.slice(1));
    return;
  }

  const { startActivity, beginActivityStep, resolveActivityStep, endActivity, setStatus } = state;
  startActivity(
    paths.length === 1 ? `Adding ${baseName(paths[0])}` : `Adding ${paths.length} files`,
    [
      { key: "extra", label: "Read the new observation files" },
      { key: "scan",  label: "Scan CDDIS for products" },
      { key: "bia",   label: "Fetch signal bias table" },
    ],
  );
  beginActivityStep("extra");
  const result = await addExtraFiles(paths);
  if (result.added === 0) {
    // Nothing joined the selection, so the window, catalogue and bias table
    // are all still right — there is nothing to rescan.
    resolveActivityStep("extra", "error", describeAdded(result));
    setStatus("No new observation files were added");
    endActivity("No new observation files were added");
    return;
  }
  resolveActivityStep("extra", "done", describeAdded(result));
  await rescanForSelection();
}

/**
 * Drop one file from the selection. Removing the primary promotes the next
 * file (its header then seeds the fields); removing the last one leaves the
 * app with no observation file. Either way the window shrinks to what is left
 * and products are rescanned for it.
 */
async function removeRinexFile(path: string): Promise<void> {
  const before = useAppStore.getState();
  const wasPrimary = path === before.rinexPath;
  before.removeRinexFile(path);

  const after = useAppStore.getState();
  if (!after.rinexData) {
    after.setStatus("No observation file selected");
    await setAppTitle(BASE_TITLE);
    return;
  }

  after.startActivity("Updating products for the remaining files", [
    { key: "scan", label: "Scan CDDIS for products" },
    { key: "bia",  label: "Fetch signal bias table" },
  ]);
  if (wasPrimary) await checkAntenna(after.rinexData);
  await rescanForSelection();
}

/** The file dialog for observation files, multi-select. Empty when cancelled. */
async function chooseRinexFiles(): Promise<string[]> {
  const selected = await open({
    multiple: true,
    filters: [
      {
        name: "RINEX Observation Files",
        extensions: RINEX_EXTENSIONS,
      },
      { name: "All Files", extensions: ["*"] },
    ],
  });
  if (!selected) return [];
  return Array.isArray(selected) ? selected : [selected];
}

/** Pick one or more files, replacing whatever is open. */
async function pickRinex(): Promise<void> {
  const paths = await chooseRinexFiles();
  if (paths.length === 0) return;
  await loadRinexFile(paths[0], paths.slice(1));
}

/** Pick one or more files to add to those already open. */
async function pickMoreRinex(): Promise<void> {
  await addRinexFiles(await chooseRinexFiles());
}

async function pickOutputDir(): Promise<void> {
  const dir = await open({ directory: true });
  if (dir && typeof dir === "string") {
    const { setOutputDir, appendWorkflowLine } = useAppStore.getState();
    setOutputDir(dir);
    appendWorkflowLine(WF.outputDirSelected(dir));
  }
}

async function pickProductsDir(): Promise<void> {
  const dir = await open({ directory: true });
  if (dir && typeof dir === "string") useAppStore.getState().setProductsDir(dir);
}

async function pickPeaBinary(): Promise<void> {
  const path = await open({ multiple: false });
  if (!path || typeof path !== "string") return;
  const {
    setPeaBinaryPath,
    setPeaVersion,
    setPeaUpdateAvailable,
    dismissCustomPeaNotice,
    setLastCustomPeaPath,
    setStatus,
  } = useAppStore.getState();
  try {
    await invoke("set_pea_binary", { path });
    setPeaBinaryPath(path);
    // A picked file is custom: its version is unknown, so the managed release's
    // tag and update offer no longer describe what's running. Clearing the tag
    // is what marks it custom everywhere in Settings.
    setPeaVersion(null);
    setPeaUpdateAvailable(null);
    // Chosen on purpose, so the "not the managed release" notice has nothing
    // to tell the user about this path on the next launch.
    dismissCustomPeaNotice(path);
    setLastCustomPeaPath(path);
    setStatus(`PEA binary: ${baseName(path)}`);
  } catch (e) {
    setStatus(`Invalid PEA binary: ${e}`);
  }
}

const PICKERS = {
  pickRinex,
  pickMoreRinex,
  loadRinexFile,
  addRinexFiles,
  removeRinexFile,
  pickOutputDir,
  pickProductsDir,
  pickPeaBinary,
};

/**
 * The four "pick a location" actions, in one place because they are split
 * across two surfaces: the RINEX file and output directory live in the sidebar
 * (Input / Output sections), the products directory and pea binary in Settings.
 *
 * Every action reads the store when it runs, so the hook subscribes to nothing
 * and returns the same stable functions on every render — callers are not
 * re-rendered by unrelated store updates, and effects that depend on
 * `loadRinexFile` are not torn down and re-subscribed each render.
 */
export function usePickers() {
  return PICKERS;
}
