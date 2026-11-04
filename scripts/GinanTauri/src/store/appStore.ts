import { create } from "zustand";
import { invoke } from "@tauri-apps/api/core";
import { load } from "@tauri-apps/plugin-store";
import type {
  RinexData,
  ProcessingMode,
  ConfigInputs,
  ConstellationCodeConfig,
  ConstellationObsCodes,
  BiaValidationState,
  BiaCodePriorities,
  CodeIntersection,
  Constellation,
  ObservationCodeItem,
  PeaConfigExtract,
  PppSelection,
  PppCatalogue,
  RealtimeInputs,
  SbasMode,
  SerialSettings,
  CorrectionRole,
  InputProduct,
  InputSection,
  ProductType,
  StreamTestResult,
  SatelliteStatus,
  InputMode,
} from "../types";
import { extractHost, type NtripStream } from "../config/ntrip";
import { isPausedBySbas, rolesFromFamilies, streamMount } from "../config/correctionRoles";
import { reduceNtripEvent } from "../hooks/ntripReducer";
import type { LiveSnapshot, NtripConnection, StreamTestStatus } from "../types/streams";
import { deviceTimeZone, isValidTimeZone } from "../utils/time";
import { allRinexPaths, rinexSpan, type RinexFile } from "./rinexFiles";
import { DEFAULT_PPP_SELECTION, DEFAULT_SERIAL_SETTINGS } from "../types";
import {
  changedParams,
  defsByPath,
  mergeParamGroups,
  type DiscoveredParam,
} from "../config/processingParams";
import { biaPhaseSignals, phaseCodes, phaseConstellationCodes } from "../config/obsCodes";

/** GA's public caster — see the note below on why it is the default. */
export const DEFAULT_CASTER_URL = "https://ntrip.data.gnss.ga.gov.au/";

/**
 * Realtime defaults are opinionated toward first-run success:
 *   - GA's public NTRIP caster is the default because it publishes every stream
 *     the shipped nav_streams below rely on (BCEP + SSRA). It is stored as a
 *     plain `scheme://host/` URL: the login is looked up from netrc by hostname
 *     when the config is written, so nothing has to be typed into the URL
 *     itself.
 *   - `wait_next_epoch` = 10 s tolerates a slow caster without stalling the
 *     epoch loop; PEA otherwise defaults to `epoch_interval + 0.05` which is
 *     too tight in practice.
 *   - Only raw RTCM recording is on by default; decoded JSON and RINEX are
 *     larger and rarely wanted for a first test run.
 */
export const DEFAULT_REALTIME_INPUTS: RealtimeInputs = {
  caster_url: DEFAULT_CASTER_URL,
  obs_streams: ["ALIC00AUS0"],
  nav_streams: ["BCEP00BKG0", "SSRA00CNE0"],
  ssr_antenna_offset: "APC",
  wait_next_epoch: 10,
  max_rec_latency: 1,
  require_obs: false,
  record_rtcm: true,
  record_decoded: true, // the Streams tab's live stats are built from it
  record_rinex: false,
};

/**
 * Per-stream kind, read straight off the stream string so there is no second
 * piece of state to keep in sync. `ntrip` entries are bare mountpoint codes
 * that the caster URL is prefixed onto; `serial` entries are a complete
 * `serial:///dev/…` URI and PEA skips the prefix for them (anything with a
 * `:` in it). Each stream picks its own kind, so one list can mix both.
 */
export type StreamKind = "ntrip" | "serial";
export const SERIAL_PREFIX = "serial://";

export function streamKind(stream: string): StreamKind {
  return stream.startsWith(SERIAL_PREFIX) ? "serial" : "ntrip";
}

/**
 * The caster URL the backend should actually see. It only prefixes mountpoints,
 * so it is blank unless at least one stream is an NTRIP mountpoint —
 * `apply_realtime_inputs` keys the whole file-vs-stream root decision off this
 * being empty.
 */
export function effectiveCasterUrl(
  casterUrl: string,
  rt: Pick<RealtimeInputs, "obs_streams" | "nav_streams">,
): string {
  return [...rt.obs_streams, ...rt.nav_streams].some((s) => streamKind(s) === "ntrip")
    ? casterUrl
    : "";
}

/* ── Input products ────────────────────────────────────────────────────────── */

/** Types that become a stream URI in the realtime block. File and SBAS do not. */
const isLiveProduct = (p: InputProduct) =>
  p.type === "ntrip" || p.type === "http" || p.type === "serial";

let productSeq = 0;
/** A fresh product id, stable for the card's life — the React key. */
export function newProductId(): string {
  return `prod-${++productSeq}`;
}

export function makeProduct(
  type: ProductType,
  value = "",
  serial?: SerialSettings,
): InputProduct {
  return serial ? { id: newProductId(), type, value, serial } : { id: newProductId(), type, value };
}

/** Parse a stored stream string (the legacy shape) into a product's type and value. */
function parseStream(stream: string): Pick<InputProduct, "type" | "value"> {
  return streamKind(stream) === "serial"
    ? { type: "serial", value: stream.slice(SERIAL_PREFIX.length) }
    : { type: "ntrip", value: stream };
}

/** The `realtime` list entry for a live product; empty placeholders pass through. */
function productStream(p: InputProduct): string {
  return p.type === "serial" ? SERIAL_PREFIX + p.value : p.value;
}

/**
 * Legacy list edit: rebuild a section's live products from stream strings,
 * matching by index so a card keeps its id and serial settings while its
 * value changes. Non-live products (file, sbas) are left where they are.
 */
function reparse(existing: InputProduct[], streams: string[]): InputProduct[] {
  const live = existing.filter(isLiveProduct);
  const rest = existing.filter((p) => !isLiveProduct(p));
  return [
    ...rest,
    ...streams.map((stream, i) => {
      const parsed = parseStream(stream);
      const old = live[i];
      if (!old) return makeProduct(parsed.type, parsed.value);
      return { ...old, ...parsed };
    }),
  ];
}

/** The products the shipped realtime defaults describe. */
export function defaultInputProducts(): Record<InputSection, InputProduct[]> {
  return {
    obs: DEFAULT_REALTIME_INPUTS.obs_streams.map((s) => makeProduct("ntrip", s)),
    corr: DEFAULT_REALTIME_INPUTS.nav_streams.map((s) => makeProduct("ntrip", s)),
  };
}

/**
 * The single place `realtime` stream fields and `streamSerial` are computed
 * from the product lists, so they cannot drift from them. `ntrip` and `http`
 * contribute their value, `serial` its `serial://` URI, `file` nothing. A
 * Corrections `sbas` product becomes `realtime.sbas` (its value is the mode),
 * and pauses the SSR-only streams it replaces: they stay in `products` but are
 * left out of `nav_streams`. The caster URL is kept only while some product is `ntrip`:
 * `apply_realtime_inputs` keys the whole file-vs-stream root decision off it
 * being empty. `base` supplies every non-stream realtime field.
 */
export function deriveRealtime(
  products: Record<InputSection, InputProduct[]>,
  casterUrl: string,
  base: RealtimeInputs,
): { realtime: RealtimeInputs; streamSerial: { obs: SerialSettings[]; corr: SerialSettings[] } } {
  const obs = products.obs.filter(isLiveProduct);
  const sbas = products.corr.find((p) => p.type === "sbas");
  const corr = products.corr.filter((p) => isLiveProduct(p) && !isPausedBySbas(p, !!sbas));
  const settings = (list: InputProduct[]) => list.map((p) => p.serial ?? DEFAULT_SERIAL_SETTINGS);
  return {
    realtime: {
      ...base,
      obs_streams: obs.map(productStream),
      nav_streams: corr.map(productStream),
      caster_url: [...obs, ...corr].some((p) => p.type === "ntrip") ? casterUrl : "",
      sbas: sbas ? { mode: (sbas.value || "PVS") as SbasMode } : undefined,
    },
    streamSerial: { obs: settings(obs), corr: settings(corr) },
  };
}

/** The shipped defaults as a full set of store fields, caster included. */
function freshRealtime() {
  const products = defaultInputProducts();
  return {
    inputProducts: products,
    realtime: { ...DEFAULT_REALTIME_INPUTS },
    streamSerial: deriveRealtime(products, DEFAULT_CASTER_URL, DEFAULT_REALTIME_INPUTS).streamSerial,
  };
}

/** `inputProducts` plus everything derived from it, for one `set()`. */
function withProducts(
  s: Pick<AppStore, "casterUrl" | "realtime">,
  products: Record<InputSection, InputProduct[]>,
) {
  return { inputProducts: products, ...deriveRealtime(products, s.casterUrl, s.realtime) };
}

/**
 * Each `serial://` product's line settings, keyed by device path — what
 * `start_pea_realtime` and `test_streams` look serial ports up by.
 */
export function serialForProducts(
  products: Record<InputSection, InputProduct[]>,
): Record<string, SerialSettings> {
  const d = deriveRealtime(products, "", DEFAULT_REALTIME_INPUTS);
  return serialByDevice(
    [d.realtime.obs_streams, d.streamSerial.obs],
    [d.realtime.nav_streams, d.streamSerial.corr],
  );
}

/**
 * 500 MB matches the backend default. A UI number field is easier to reason
 * about in MB than the byte-typed backend value; the store hands MB to the
 * backend, which converts on the way in.
 */
export const DEFAULT_STORAGE_CAP_MB = 500;

const STORE_FILE = "app-settings.json";
const KEYS = {
  // Deliberately never written — the app always starts with no observation
  // file. Kept only so a value left by an earlier version of the app can be
  // deleted on load.
  rinexPath: "rinexPath",
  outputDir: "outputDir",
  // The subdirectory of `outputDir` the last run wrote to, so the result views
  // reopen on it after a restart.
  runDir: "runDir",
  productsDir: "productsDir",
  mode: "mode",
  startEpoch: "startEpoch",
  endEpoch: "endEpoch",
  epochInterval: "epochInterval",
  outputPos: "outputPos",
  outputGpx: "outputGpx",
  outputTrace: "outputTrace",
  outputSnx: "outputSnx",
  outputSpp: "outputSpp",
  constellationCodes: "constellationCodes",
  pppProvider: "pppProvider",
  pppSeries: "pppSeries",
  pppProject: "pppProject",
  configOverwrite: "configOverwrite",
  processingParams: "processingParams",
  // The binary the app should use: either a user-picked custom file or the
  // managed download in the app data dir. Persisted so the choice survives a
  // restart and a stale value is replaced when a new version is downloaded.
  peaBinaryPath: "peaBinaryPath",
  // The custom binary whose "not the managed release" notice was last
  // dismissed. Keyed by path, so picking a different custom binary is noticed
  // once more, but the same one is never raised again at launch.
  customPeaNoticeSeen: "customPeaNoticeSeen",
  // The last custom binary used, kept after switching to the managed release so
  // Settings → PEA Binary can switch straight back to it.
  lastCustomPeaPath: "lastCustomPeaPath",
  // First-run state. Absent on an install that predates onboarding, which is
  // why loadPersistedConfig falls back to "has a products directory" rather
  // than to `false` — see there.
  onboardingDone: "onboardingDone",
  completedTours: "completedTours",
  // Live-stream setup, edited in Settings and persisted; the realtime block
  // itself is session-only.
  casterUrl: "casterUrl",
  // IANA zone every date and time is displayed in. Absent until the user picks
  // one, which means "the device's zone".
  timeZone: "timeZone",
} as const;

/** Write one key to the settings file; null/undefined deletes it. */
async function persist(key: string, value: unknown) {
  const store = await load(STORE_FILE, { defaults: {}, autoSave: true });
  if (value === null || value === undefined) {
    await store.delete(key);
  } else {
    await store.set(key, value);
  }
}

/** What `loadPersistedConfig` restores and `hydrate` applies. */
export interface PersistedConfig {
  outputDir: string | null;
  runDir: string | null;
  productsDir: string;
  mode: ProcessingMode;
  // null means "never been stored" — use RINEX-derived values on startup
  startEpoch: string | null;
  endEpoch: string | null;
  epochInterval: number | null;
  outputPos: boolean;
  outputGpx: boolean;
  outputTrace: boolean;
  outputSnx: boolean;
  /** The SBAS `.SPP` solution; offered, and written, only while SBAS is used. */
  outputSpp: boolean;
  constellationCodes: ConstellationCodeConfig | null;
  pppSelection: PppSelection;
  configOverwrite: boolean;
  processingParams: Record<string, unknown>;
  peaBinaryPath: string | null;
  customPeaNoticeSeen: string | null;
  lastCustomPeaPath: string | null;
  onboardingDone: boolean;
  completedTours: TourKey[];
  casterUrl: string;
  timeZone: string;
}

/**
 * Machine-level setup that survives a restart: directories, the pea binary, the
 * product selection and the RINEX-derived overrides. The observation file
 * itself is *not* part of this — every session starts on the welcome screen
 * with nothing loaded.
 */
export async function loadPersistedConfig(): Promise<PersistedConfig> {
  const store = await load(STORE_FILE, { defaults: {}, autoSave: true });
  // Drop a path written by an earlier version of the app, so upgrading doesn't leave a file
  // that quietly reloads itself on every launch.
  await store.delete(KEYS.rinexPath);
  const productsDir = (await store.get<string>(KEYS.productsDir)) ?? "";
  const onboardingDone = await store.get<boolean>(KEYS.onboardingDone);
  return {
    outputDir: (await store.get<string>(KEYS.outputDir)) ?? null,
    runDir: (await store.get<string>(KEYS.runDir)) ?? null,
    productsDir,
    mode: (await store.get<ProcessingMode>(KEYS.mode)) ?? "Static",
    startEpoch: (await store.get<string>(KEYS.startEpoch)) ?? null,
    endEpoch: (await store.get<string>(KEYS.endEpoch)) ?? null,
    epochInterval: (await store.get<number>(KEYS.epochInterval)) ?? null,
    outputPos: (await store.get<boolean>(KEYS.outputPos)) ?? true,
    outputGpx: (await store.get<boolean>(KEYS.outputGpx)) ?? false,
    outputTrace: (await store.get<boolean>(KEYS.outputTrace)) ?? false,
    outputSnx: (await store.get<boolean>(KEYS.outputSnx)) ?? false,
    outputSpp: (await store.get<boolean>(KEYS.outputSpp)) ?? true,
    constellationCodes:
      (await store.get<ConstellationCodeConfig>(KEYS.constellationCodes)) ??
      null,
    pppSelection: {
      provider: (await store.get<string>(KEYS.pppProvider)) ?? DEFAULT_PPP_SELECTION.provider,
      series: (await store.get<string>(KEYS.pppSeries)) ?? DEFAULT_PPP_SELECTION.series,
      project: (await store.get<string>(KEYS.pppProject)) ?? DEFAULT_PPP_SELECTION.project,
    },
    // Defaults to true: every run rewrites the config.
    configOverwrite: (await store.get<boolean>(KEYS.configOverwrite)) ?? true,
    // Sparse: only parameters the user moved off the template default.
    processingParams:
      (await store.get<Record<string, unknown>>(KEYS.processingParams)) ?? {},
    peaBinaryPath: (await store.get<string>(KEYS.peaBinaryPath)) ?? null,
    customPeaNoticeSeen: (await store.get<string>(KEYS.customPeaNoticeSeen)) ?? null,
    lastCustomPeaPath: (await store.get<string>(KEYS.lastCustomPeaPath)) ?? null,
    // An install that predates onboarding has no flag, and putting an existing
    // user through first-run setup would be wrong. A products directory is only
    // ever set by hand, so its presence is the evidence that this machine is
    // already configured.
    onboardingDone: onboardingDone ?? productsDir !== "",
    completedTours: (await store.get<TourKey[]>(KEYS.completedTours)) ?? [],
    casterUrl: (await store.get<string>(KEYS.casterUrl)) ?? DEFAULT_CASTER_URL,
    timeZone: storedTimeZone(await store.get<string>(KEYS.timeZone)),
  };
}

/** A stored zone this runtime no longer knows falls back to the device's. */
function storedTimeZone(tz: string | undefined | null): string {
  return tz && isValidTimeZone(tz) ? tz : deviceTimeZone();
}

type Theme = "dark" | "light";

/** Pages in the settings view, in nav order. */
export type SettingsPageKey =
  | "config"
  | "products"
  | "output"
  | "binary"
  | "credentials"
  /** Earlier versions of the app had a separate page for the caster settings;
   *  they live on "credentials". Accepted and resolved there, so an old deep
   *  link still lands. */
  | "livestream"
  | "appearance"
  | "help"
  | "about";

/** The page a key actually opens: the caster settings live on Credentials. */
const resolveSettingsPage = (p: SettingsPageKey): SettingsPageKey =>
  p === "livestream" ? "credentials" : p;

/** Sections of the sidebar accordion, in display order. */
export type ConfigSectionKey = "input" | "general" | "constellations" | "output";
export type WorkspaceTabKey = "workflows" | "console" | "streams" | "visualization";

/* ── First run: onboarding and the guided tours ────────────────────────────── */

/**
 * "unknown" is the pre-hydrate state and is not the same as "done": the
 * persisted flag is read asynchronously, and anything that must not fire ahead
 * of onboarding (the startup credentials prompt) keys off that distinction
 * rather than racing the read.
 */
export type OnboardingStatus = "unknown" | "active" | "done";

/**
 * Three tours, because the app has three shapes. Most of the window — sidebar,
 * tabs, run controls, status bar — does not exist until a RINEX is open or
 * realtime mode is picked, so "welcome" covers what is on screen straight after
 * setup and "workspace" runs itself the first time a file is loaded.
 *
 * "streaming" is the same shell driven from an NTRIP caster rather than a file.
 * It is a separate list rather than a flag on the workspace steps because the
 * two modes disagree about more than wording: a streaming run has no epoch
 * window, downloads no SP3/CLK products, and takes its constellations from
 * REALTIME_DEFAULT_CONSTELLATIONS rather than a RINEX header — so the Products
 * and Constellations steps are not merely worded for files, they describe
 * panels a streaming run never reads.
 */
export type TourKey = "welcome" | "workspace" | "streaming";

/* ── Activity: the long fetch that follows opening a file ──────────────────── */

export type ActivityStepState = "pending" | "active" | "done" | "error" | "skipped";

/** One line in the activity panel. `detail` carries the live sub-status. */
export interface ActivityStep {
  key: string;
  label: string;
  state: ActivityStepState;
  detail: string | null;
  /** 0–100 when the backend reports a real fraction, else null (indeterminate). */
  percent: number | null;
}

/**
 * Opening a RINEX file kicks off a chain of network work — mirror listings,
 * bias tables. This models that chain so it can be shown as steps with live
 * state rather than only as console lines and a one-line status.
 */
export interface Activity {
  title: string;
  steps: ActivityStep[];
  /** True once the chain has stopped, successfully or not. */
  finished: boolean;
  error: string | null;
}

function getInitialTheme(): Theme {
  const manual = localStorage.getItem("ginan-theme-manual") === "true";
  const saved = localStorage.getItem("ginan-theme") as Theme | null;
  if (manual && saved) return saved;
  return window.matchMedia("(prefers-color-scheme: dark)").matches
    ? "dark"
    : "light";
}

/**
 * Apply a patch to one step of the running activity, returning a zustand
 * partial. Absent activity, unknown key, or a step whose state is outside
 * `onlyWhen` all collapse to "change nothing", which is what lets the activity
 * actions be called blind.
 */
function patchStep(
  activity: Activity | null,
  key: string,
  patch: Partial<ActivityStep>,
  onlyWhen?: ActivityStepState[],
): { activity?: Activity } {
  if (!activity) return {};
  const step = activity.steps.find((s) => s.key === key);
  if (!step) return {};
  if (onlyWhen && !onlyWhen.includes(step.state)) return {};
  return {
    activity: {
      ...activity,
      steps: activity.steps.map((s) => (s.key === key ? { ...s, ...patch } : s)),
    },
  };
}

function getInitialSidebarCollapsed(): boolean {
  return localStorage.getItem("ginan-sidebar-collapsed") === "true";
}

/**
 * Rail width bounds, in px. The minimum is set by the widest control in the
 * sidebar — the Antenna Offset H/E/N grid — below which its cells start to
 * clip; the maximum keeps the workspace the larger half on a 1280px screen.
 */
export const SIDEBAR_MIN_WIDTH = 280;
export const SIDEBAR_MAX_WIDTH = 560;
export const SIDEBAR_DEFAULT_WIDTH = 360;

function clampSidebarWidth(px: number): number {
  return Math.min(SIDEBAR_MAX_WIDTH, Math.max(SIDEBAR_MIN_WIDTH, Math.round(px)));
}

function getInitialSidebarWidth(): number {
  // Number(null) and Number("") are both 0, so a missing or junk entry falls
  // through to the default rather than clamping up to the minimum.
  const saved = Number(localStorage.getItem("ginan-sidebar-width"));
  return Number.isFinite(saved) && saved > 0 ? clampSidebarWidth(saved) : SIDEBAR_DEFAULT_WIDTH;
}

interface AppStore {
  // File paths
  rinexPath: string | null;
  outputDir: string | null;
  /**
   * Name of the next run's subdirectory under `outputDir`. Session-only; empty
   * means `defaultRunName`. See `runOutputDir`.
   */
  runName: string;
  /** Where the last run wrote its results; the result views read from here. */
  runDir: string | null;
  productsDir: string;

  // Parsed RINEX metadata
  rinexData: RinexData | null;

  // Further observation files processed together with the primary
  // (`rinexPath` / `rinexData`). Session-only, like the primary. The primary
  // alone seeds the header-derived fields below.
  extraRinex: RinexFile[];

  // User-editable RINEX-derived fields. Seeded from rinexData on load
  // but kept independent so the user can override.
  // IMPORTANT: receiverType and antennaType must preserve whitespace
  // exactly as the user enters / RINEX provides it — never trim.
  markerName: string;
  receiverType: string;
  antennaType: string;
  antennaOffset: [number, number, number]; // H, E, N

  // Config fields
  mode: ProcessingMode;
  selectedConstellations: string[];
  startEpoch: string;
  endEpoch: string;
  epochInterval: number;
  outputPos: boolean;
  outputGpx: boolean;
  outputTrace: boolean;
  outputSnx: boolean;
  /** The SBAS `.SPP` solution; offered, and written, only while SBAS is used. */
  outputSpp: boolean;

  // When false, a run reuses an existing ppp_config.yaml instead of rewriting
  // it — the escape hatch for hand-edited configs (see the YAML tab).
  configOverwrite: boolean;

  // Processing parameters keyed by dotted YAML path. Sparse: a path is present
  // only once the user moves it off the template default, so the generated
  // config stays close to the shipped template. See config/processingParams.ts.
  processingParams: Record<string, unknown>;

  // Every settable scalar found in the PEA config template, fetched once at
  // startup. Lets the settings page pick up config options added by a future
  // PEA release without a frontend change. Empty until discovery returns.
  discoveredParams: DiscoveredParam[];

  // Constellation observation codes
  constellationCodes: ConstellationCodeConfig;
  availableObsCodes: ConstellationObsCodes | null;
  biaValidation: BiaValidationState;

  // PPP product source selection
  pppSelection: PppSelection;

  // Catalogue of valid (provider, series, project) combos for the loaded RINEX
  // window, produced by scan_ppp_catalogue. Null until a RINEX is scanned.
  pppCatalogue: PppCatalogue | null;

  // Download source. CDDIS is the only PPP product server; this field is
  // always "CDDIS" and the backend ignores any other value.
  pppServer: string;

  // Theme
  theme: Theme;

  // IANA zone used to display every date and time (Settings → Appearance).
  // Display only — stored, sent and written times stay UTC/GPS.
  timeZone: string;

  // Connection faults
  connectionFault: string | null;

  // PPP provider reachability cache (filled by check_ppp_provider_health)
  providerHealth: Record<string, { status: ProviderHealthStatus; checkedAt: number }>;

  // Earthdata/CDDIS credential state. offlineMode is true when no usable
  // credentials exist (or the user dismissed the modal). CDDIS is the only
  // product server, so offline means no product downloads at all — the user is
  // prompted to sign in or create a free Earthdata account.
  offlineMode: boolean;
  credentialsModalOpen: boolean;

  // Settings view. UI-only (never persisted): which settings page is showing
  // and whether the settings surface covers the workspace.
  settingsOpen: boolean;
  settingsPage: SettingsPageKey;

  // First run. `onboardingStatus` starts "unknown" and is resolved by hydrate;
  // `completedTours` records every tour that has been shown — completed, skipped
  // or dismissed — persisted so each auto-starts at most once. The Help button
  // still replays them on request. `activeTour` is on screen now — UI-only.
  onboardingStatus: OnboardingStatus;
  completedTours: TourKey[];
  activeTour: TourKey | null;

  // Which sidebar accordion section is open. In the store rather than local to
  // ConfigPanel so the tour can open the section it is describing.
  configSection: ConfigSectionKey | null;

  // A tab something outside the workspace wants shown (the run blockers'
  // "Show Streams"). Workspace switches to it and clears the request.
  workspaceTabRequest: WorkspaceTabKey | null;

  // Sidebar rail. UI-only, but persisted to localStorage (same route as
  // `theme`) — a rail the user shut or resized should stay that way across
  // restarts rather than springing back on every launch.
  sidebarCollapsed: boolean;
  sidebarWidth: number;

  // Startup validation state. cddisStatus drives the Process button gate
  // and a banner in the UI. atxPath is the highest-priority ANTEX file found in
  // productsDir after ensure_products completes; null when not yet located.
  // "idle" = the connectivity check has not started (it is gated on
  // productsDir). Distinct from "checking" so the UI can tell "not yet run"
  // apart from "in progress" — otherwise a missing products directory looks
  // like a check that never finishes.
  cddisStatus: "idle" | "checking" | "ok" | "error";
  cddisStatusMessage: string | null;
  // Invalidates pending credential checks. A successful save re-runs startup;
  // removal or a failed update pauses it until credentials are valid again.
  cddisEpoch: number;
  /** Explicit credential removal/failure pauses checks until a successful update. */
  cddisChecksPaused: boolean;
  atxPath: string | null;

  // Antenna check on RINEX load — null = unknown/skipped (no ATX or no antenna_type).
  antennaValid: boolean | null;

  // BIA code support set for the current provider/series/project combo and
  // the per-constellation intersection with RINEX-observed codes.
  bia: BiaCodePriorities | null;
  codeIntersection: Record<string, CodeIntersection> | null;

  // Realtime input mode. `file` runs from RINEX; `realtime` streams from an
  // NTRIP caster (or serial receiver) and swaps the whole `inputs` block on
  // the backend. Session-only (not persisted).
  inputMode: InputMode;
  /**
   * True once the user has been inside the main shell this session, either by
   * loading a file or by entering realtime mode.
   *
   * Session-only and deliberately not persisted: a fresh launch with nothing
   * loaded should still land on the welcome screen. It exists so that leaving
   * realtime mode does not throw the user back out to that screen when there
   * is no RINEX file loaded.
   */
  hasEnteredWorkspace: boolean;
  /**
   * The Input sidebar's cards, per section — the source of truth for what a
   * run reads. `realtime` stream lists and `streamSerial` below are derived
   * from it (see `deriveRealtime`) and never edited on their own.
   */
  inputProducts: Record<InputSection, InputProduct[]>;
  /** Appends a product and returns its id. `value` is the mountpoint, device path or URL. */
  /** `role`: the Corrections group it is added under (see `correctionRoles`). */
  addProduct: (section: InputSection, type: ProductType, value?: string, role?: CorrectionRole) => string;
  /** Edit a product in place. Its id and type never change. */
  updateProduct: (
    section: InputSection,
    id: string,
    patch: Partial<Pick<InputProduct, "value" | "serial">>,
  ) => void;
  removeProduct: (section: InputSection, id: string) => void;
  /** Swap a whole section; products without an id get one. */
  replaceSection: (
    section: InputSection,
    products: Array<Omit<InputProduct, "id"> & { id?: string }>,
  ) => void;
  /**
   * Derived from `inputProducts`: the output the Rust contract reads. Only the
   * non-stream fields are edited directly, via `updateRealtime`.
   */
  realtime: RealtimeInputs;
  /**
   * Live-stream caster setup, edited on the Settings › Credentials page and
   * persisted. `realtime.caster_url` is derived from it and the stream lists
   * via `effectiveCasterUrl` whenever either changes — the panel and the
   * backend only ever read the derived value.
   */
  casterUrl: string;
  /**
   * Last fetched caster sourcetable, shared so the streaming panel's mountpoint
   * fields can suggest codes while the fetch is triggered from Settings.
   */
  ntripSourcetable: NtripStream[];
  ntripSourcetableStatus:
    | { state: "idle" }
    | { state: "loading" }
    | { state: "error"; message: string }
    | { state: "ok"; count: number; host: string };
  /**
   * Ceiling on rotated recording bytes (MB) enforced by the retention sweeper.
   * 0 disables the sweep entirely — useful for short debug runs where files
   * disappearing under the user's feet would be confusing.
   */
  realtimeStorageCapMb: number;
  /**
   * Derived from `inputProducts`: serial line settings, one per stream. Entry i
   * belongs to `realtime.obs_streams[i]` / `realtime.nav_streams[i]`; products
   * with no settings of their own read as the defaults. Handed to the serial
   * setup script only.
   */
  streamSerial: { obs: SerialSettings[]; corr: SerialSettings[] };

  // Runtime
  // Two log channels, mirroring the original GinanUI (Python) which split
  // output into a "Workflow" tab (high-level status the user follows) and a
  // "Console" tab (raw PEA subprocess output + low-level technical detail).
  consoleLines: string[];
  workflowLines: string[];
  isProcessing: boolean;
  status: string;
  peaBinaryPath: string | null;
  // Release tag of the installed pea binary, when known (null for a custom
  // override or before the binary is resolved).
  peaVersion: string | null;
  // The managed release switching back would use. Kept while a custom binary
  // is active (peaVersion null), so the Managed card's text never changes.
  managedPeaVersion: string | null;
  // A newer release than the installed binary, when one exists. The app never
  // auto-updates, so this drives the update prompt (popup + Settings).
  peaUpdateAvailable: string | null;
  // Whether the update prompt popup is showing. Auto-opened once on startup when
  // an update is found, and re-openable from the status-bar pill.
  peaUpdateModalOpen: boolean;
  // Path of the custom binary the user has already been told about (persisted),
  // and whether that notice is showing. See CustomPeaModal.
  customPeaNoticeSeen: string | null;
  customPeaModalOpen: boolean;
  // Last custom binary used (persisted), so the Custom card can switch back.
  lastCustomPeaPath: string | null;

  /** Bumped when a run finishes, so the Visualization tab re-reads the output
   *  directory and picks up the .pos files that run just wrote. */
  posEpoch: number;

  /** PEA's epoch count for the current (or last) run, parsed from its
   *  "Processed epoch #N" lines. `total` is null when the window is open-ended
   *  (realtime, or no start/end epoch set). Null when no run has started. */
  epochProgress: { processed: number; total: number | null } | null;

  /** The load/fetch chain currently worth showing, or null when idle. */
  activity: Activity | null;
  /** Hiding the panel preserves its progress until the load chain finishes. */
  activityHidden: boolean;

  // Actions
  setRinexPath: (p: string | null) => void;
  setOutputDir: (d: string | null) => void;
  setRunName: (name: string) => void;
  setRunDir: (dir: string) => void;
  setProductsDir: (d: string) => void;
  setRinexData: (data: RinexData) => void;
  /** Forget every file but the primary, e.g. when a new primary replaces the lot. */
  clearExtraRinex: () => void;
  /** Add one more observation file and widen the time window to cover it. */
  addRinexFile: (file: RinexFile) => void;
  /** Drop one file by path; removing the primary promotes the next file. */
  removeRinexFile: (path: string) => void;
  setMarkerName: (v: string) => void;
  setReceiverType: (v: string) => void;
  setAntennaType: (v: string) => void;
  setAntennaOffset: (axis: 0 | 1 | 2, value: number) => void;
  setMode: (m: ProcessingMode) => void;
  toggleConstellation: (c: string) => void;
  setStartEpoch: (e: string) => void;
  setEndEpoch: (e: string) => void;
  setEpochInterval: (n: number) => void;
  setOutputFormat: (fmt: "pos" | "gpx" | "trace" | "snx" | "spp", val: boolean) => void;
  setConfigOverwrite: (val: boolean) => void;
  setProcessingParam: (path: string, value: unknown) => void;
  resetProcessingParam: (path: string) => void;
  resetAllProcessingParams: () => void;
  loadDiscoveredParams: () => Promise<void>;
  // Console channel — raw PEA output / low-level technical detail.
  appendLine: (line: string) => void;
  appendLines: (lines: string[]) => void;
  clearConsole: () => void;
  // Workflow channel — high-level status the user follows.
  appendWorkflowLine: (line: string) => void;
  appendWorkflowLines: (lines: string[]) => void;
  clearWorkflow: () => void;
  setIsProcessing: (v: boolean) => void;
  setStatus: (s: string) => void;
  setPeaBinaryPath: (p: string | null) => void;
  setPeaVersion: (v: string | null) => void;
  setManagedPeaVersion: (v: string | null) => void;
  setPeaUpdateAvailable: (v: string | null) => void;
  setPeaUpdateModalOpen: (v: boolean) => void;
  setCustomPeaModalOpen: (v: boolean) => void;
  setLastCustomPeaPath: (p: string | null) => void;
  /** Close the custom-binary notice and remember `path`, so it isn't raised
   *  again on later launches. */
  dismissCustomPeaNotice: (path: string) => void;
  bumpPosEpoch: () => void;
  startEpochProgress: (total: number | null) => void;

  // Activity. Every one of these is a no-op when nothing is running or the
  // named step has already been resolved, so callers (event listeners, the BIA
  // effect) can fire them unconditionally without checking first.
  startActivity: (title: string, steps: { key: string; label: string }[]) => void;
  beginActivityStep: (key: string, detail?: string | null) => void;
  updateActivityStep: (key: string, patch: { detail?: string | null; percent?: number | null }) => void;
  /** For events that belong to "whatever is running" — downloads, mostly. */
  updateActiveActivityStep: (patch: { detail?: string | null; percent?: number | null }) => void;
  resolveActivityStep: (
    key: string,
    state: "done" | "error" | "skipped",
    detail?: string | null,
  ) => void;
  endActivity: (error?: string | null) => void;
  clearActivity: () => void;
  hideActivity: () => void;
  showActivity: () => void;

  // Settings view
  setSettingsOpen: (v: boolean) => void;
  setSettingsPage: (p: SettingsPageKey) => void;
  openSettings: (p?: SettingsPageKey) => void;

  // First run
  /** Leave first-run setup and remember it, so it never shows again. */
  completeOnboarding: () => void;
  /** Put the app back to a fresh install's first launch (Settings → Replay). */
  restartOnboarding: () => void;
  startTour: (tour: TourKey) => void;
  /** End the active tour, recording it as seen so it never auto-starts again. */
  endTour: () => void;
  setConfigSection: (key: ConfigSectionKey | null) => void;
  requestWorkspaceTab: (tab: WorkspaceTabKey | null) => void;

  // Sidebar rail
  toggleSidebar: () => void;
  /** `commit` writes the width to localStorage; a drag only commits on release. */
  setSidebarWidth: (px: number, commit?: boolean) => void;

  // Constellation observation code actions
  setAvailableObsCodes: (codes: ConstellationObsCodes) => void;
  updateConstellationCode: (
    constellation: Constellation,
    index: number,
    update: Partial<ObservationCodeItem>
  ) => void;
  setConstellationCodeValidity: (
    constellation: Constellation,
    index: number,
    valid: boolean
  ) => void;
  reorderConstellationCodes: (
    constellation: Constellation,
    fromIndex: number,
    toIndex: number
  ) => void;
  setBiaValidation: (state: Partial<BiaValidationState>) => void;
  validateCodesAgainstBia: () => Promise<void>;
  /**
   * Seed the per-code selection from an observation. Every code starts enabled,
   * in the order given — unless `keepSelection` is set, which carries over what
   * the user has already switched off or reordered (see `mergeObservedCodes`).
   */
  initializeConstellationCodes: (
    codes: ConstellationObsCodes,
    options?: { keepSelection?: boolean },
  ) => void;
  setConstellationEnabled: (constellation: Constellation, enabled: boolean) => void;
  setConstellationCodes: (codes: ConstellationCodeConfig) => void;

  resetConfig: () => void;
  hydrate: (config: PersistedConfig) => void;
  applyImportedConfig: (config: PeaConfigExtract) => void;

  // PPP product source selection
  setPppProvider: (provider: string) => void;
  setPppSeries: (series: string) => void;
  setPppProject: (project: string) => void;

  // PPP catalogue
  setPppCatalogue: (catalogue: PppCatalogue | null) => void;
  getAvailableProviders: () => string[];
  getAvailableSeries: (provider: string) => string[];
  getAvailableSeriesForProject: (provider: string, project: string) => string[];
  getAvailableProjects: (provider: string, series: string) => string[];
  getAvailableProjectsForProvider: (provider: string) => string[];
  // Constellations the user may toggle: RINEX-present ∩ published by the
  // current PPP selection. Permissive (RINEX-present only) when the catalogue
  // is absent or the matching entry has no constellation info.
  availableConstellationsForCurrentSelection: () => Set<Constellation>;

  setTheme: (t: Theme, manual?: boolean) => void;
  setTimeZone: (tz: string) => void;
  setConnectionFault: (msg: string) => void;
  clearConnectionFault: () => void;

  setProviderHealth: (provider: string, status: ProviderHealthStatus, checkedAt?: number) => void;

  // Earthdata/CDDIS credential state
  setOfflineMode: (v: boolean) => void;
  setCredentialsModalOpen: (v: boolean) => void;

  // Startup / load-chain state
  setCddisStatus: (status: "idle" | "checking" | "ok" | "error", message?: string | null) => void;
  bumpCddisEpoch: () => void;
  setAtxPath: (path: string | null) => void;
  setAntennaValid: (valid: boolean | null) => void;
  setBia: (bia: BiaCodePriorities | null) => void;
  setCodeIntersection: (intersection: Record<string, CodeIntersection> | null) => void;

  setInputMode: (m: InputMode) => void;
  /**
   * Compat shim: `obs_streams` / `nav_streams` in the patch are parsed back
   * into products (by index, keeping ids and serial settings); every other
   * field is a plain realtime knob.
   */
  updateRealtime: (patch: Partial<RealtimeInputs>) => void;
  setRealtimeStorageCapMb: (mb: number) => void;
  /** Compat shim: settings are matched to the section's live products by index. */
  setStreamSerial: (which: "obs" | "corr", next: SerialSettings[]) => void;
  /**
   * Fill the Constellations tab and the epoch interval from a stream test,
   * the way `setRinexData` does from a RINEX header.
   */
  applyStreamTest: (result: StreamTestResult) => void;
  /** Last stream test result, shown in the Streams tab. */
  streamTest: StreamTestResult | null;
  /** Stream test lifecycle, set inside `runStreamTest` so Start-initiated failures are kept too. */
  streamTestStatus: StreamTestStatus;
  streamTestError: string | null;
  /** Unix ms when the last test finished (passed or failed). */
  streamTestAt: number | null;
  /** Unix ms when the running test started; null when none is running. */
  streamTestStartedAt: number | null;
  /** `streamFingerprint` at the moment the last test passed; null until one has. */
  streamTestFingerprint: string | null;
  setStreamTestState: (p: { status: StreamTestStatus; error?: string | null; at?: number | null }) => void;
  /** Latest `live-stream-stats` payload; kept after a run stops so values stay frozen. */
  liveSnapshot: LiveSnapshot | null;
  /** Local `Date.now()` when `liveSnapshot` arrived; lets the UI tick ages between 5 s snapshots. */
  liveSnapshotAt: number | null;
  setLiveSnapshot: (snap: LiveSnapshot | null) => void;
  /** Latest `.TRACE` satellite summary, polled by useStreamsModel. */
  satelliteStatus: SatelliteStatus | null;
  setSatelliteStatus: (s: SatelliteStatus | null) => void;
  /** Per-mount NTRIP connection state from `ntrip-stats`, keyed by mountpoint. */
  ntripConnections: Record<string, NtripConnection>;
  applyNtripEvent: (payload: unknown) => void;
  /** Drop live state (snapshot + connections); called when a run starts. */
  clearLiveState: () => void;
  /** Reset backend counters (all mounts, or one) and the matching retry counts. */
  resetStreamCounters: (mount?: string) => Promise<void>;
  /** Settings › Credentials: the caster URL. Re-derives `realtime.caster_url`. */
  setCasterUrl: (url: string) => void;
  /** Fetch the caster's sourcetable into `ntripSourcetable`. */
  fetchNtripSourcetable: () => Promise<void>;
}

export type ProviderHealthStatus = "up" | "down" | "unknown";

export const useAppStore = create<AppStore>((set, get) => ({
  rinexPath: null,
  outputDir: null,
  runName: "",
  runDir: null,
  productsDir: "",

  rinexData: null,
  extraRinex: [],

  // Editable RINEX-derived fields. Seeded by setRinexData; user can override.
  markerName: "",
  receiverType: "",
  antennaType: "",
  antennaOffset: [0, 0, 0],

  mode: "Static",
  selectedConstellations: ["GPS"],
  startEpoch: "",
  endEpoch: "",
  epochInterval: 30,
  outputPos: true,
  outputGpx: false,
  outputTrace: false,
  outputSnx: false,
  outputSpp: true,
  configOverwrite: true,
  processingParams: {},
  discoveredParams: [],

  constellationCodes: {
    gps: [],
    gal: [],
    glo: [],
    bds: [],
    qzs: [],
  },
  availableObsCodes: null,
  biaValidation: {
    isLoading: false,
  },

  pppSelection: DEFAULT_PPP_SELECTION,

  pppCatalogue: null,
  pppServer: "CDDIS",

  theme: getInitialTheme(),
  timeZone: deviceTimeZone(),
  connectionFault: null,

  providerHealth: {},

  offlineMode: false,
  credentialsModalOpen: false,

  settingsOpen: false,
  settingsPage: "products",

  onboardingStatus: "unknown",
  completedTours: [],
  activeTour: null,
  // Input sits above the accordion rather than in it; General is the first
  // section, so open it by default.
  configSection: "general",
  workspaceTabRequest: null,

  sidebarCollapsed: getInitialSidebarCollapsed(),
  sidebarWidth: getInitialSidebarWidth(),

  cddisStatus: "idle",
  cddisStatusMessage: null,
  cddisEpoch: 0,
  cddisChecksPaused: false,
  atxPath: null,
  antennaValid: null,
  bia: null,
  codeIntersection: null,

  inputMode: "file",
  hasEnteredWorkspace: false,
  ...freshRealtime(),
  casterUrl: DEFAULT_CASTER_URL,
  ntripSourcetable: [],
  ntripSourcetableStatus: { state: "idle" },
  realtimeStorageCapMb: DEFAULT_STORAGE_CAP_MB,
  streamTest: null,
  streamTestStatus: "idle",
  streamTestError: null,
  streamTestAt: null,
  streamTestStartedAt: null,
  streamTestFingerprint: null,
  setStreamTestState: ({ status, error = null, at = null }) =>
    set({
      streamTestStatus: status,
      streamTestError: error,
      streamTestAt: at,
      streamTestStartedAt: status === "running" ? Date.now() : null,
    }),
  liveSnapshot: null,
  liveSnapshotAt: null,
  setLiveSnapshot: (snap) => set({ liveSnapshot: snap, liveSnapshotAt: snap ? Date.now() : null }),
  satelliteStatus: null,
  setSatelliteStatus: (st) => set({ satelliteStatus: st }),
  ntripConnections: {},
  applyNtripEvent: (payload) =>
    set((s) => {
      const next = reduceNtripEvent(s.ntripConnections, payload, Date.now());
      return next === s.ntripConnections ? {} : { ntripConnections: next };
    }),
  clearLiveState: () => set({ liveSnapshot: null, liveSnapshotAt: null, ntripConnections: {} }),
  resetStreamCounters: async (mount) => {
    await invoke("reset_stream_counters", { mount: mount ?? null });
    set((s) => {
      const conns: Record<string, NtripConnection> = {};
      for (const [k, v] of Object.entries(s.ntripConnections)) {
        conns[k] = !mount || mount === k ? { ...v, retryCount: 0 } : v;
      }
      return { ntripConnections: conns };
    });
  },

  addProduct: (section, type, value = "", role) => {
    const product = role ? { ...makeProduct(type, value), role } : makeProduct(type, value);
    set((s) =>
      withProducts(s, { ...s.inputProducts, [section]: [...s.inputProducts[section], product] }),
    );
    return product.id;
  },
  updateProduct: (section, id, patch) =>
    set((s) =>
      withProducts(s, {
        ...s.inputProducts,
        [section]: s.inputProducts[section].map((p) => {
          if (p.id !== id) return p;
          const next = { ...p, ...patch };
          // What a test saw belongs to the stream it saw, not a new mountpoint.
          if (patch.value !== undefined && patch.value !== p.value) delete next.detected;
          return next;
        }),
      }),
    ),
  removeProduct: (section, id) =>
    set((s) =>
      withProducts(s, {
        ...s.inputProducts,
        [section]: s.inputProducts[section].filter((p) => p.id !== id),
      }),
    ),
  replaceSection: (section, products) =>
    set((s) =>
      withProducts(s, {
        ...s.inputProducts,
        [section]: products.map((p) => ({ ...p, id: p.id ?? newProductId() })),
      }),
    ),

  consoleLines: [],
  workflowLines: [],
  isProcessing: false,
  status: "Ready",
  peaBinaryPath: null,
  peaVersion: null,
  managedPeaVersion: null,
  peaUpdateAvailable: null,
  peaUpdateModalOpen: false,
  customPeaNoticeSeen: null,
  customPeaModalOpen: false,
  lastCustomPeaPath: null,
  posEpoch: 0,
  epochProgress: null,
  activity: null,
  activityHidden: false,

  hydrate: (config) => {
    const products = defaultInputProducts();
    const update: Partial<AppStore> = {
      outputDir: config.outputDir,
      runDir: config.runDir,
      productsDir: config.productsDir,
      mode: config.mode,
      outputPos: config.outputPos,
      outputGpx: config.outputGpx,
      outputTrace: config.outputTrace,
      outputSnx: config.outputSnx,
      outputSpp: config.outputSpp,
      pppSelection: config.pppSelection,
      configOverwrite: config.configOverwrite,
      processingParams: config.processingParams,
      onboardingStatus: config.onboardingDone ? "done" : "active",
      completedTours: config.completedTours,
      customPeaNoticeSeen: config.customPeaNoticeSeen,
      lastCustomPeaPath: config.lastCustomPeaPath,
      casterUrl: config.casterUrl,
      timeZone: config.timeZone,
      // Session start, so realtime is still the default — fold the restored
      // caster setup into it so the first realtime run uses the saved value.
      ...deriveRealtime(products, config.casterUrl, DEFAULT_REALTIME_INPUTS),
      inputProducts: products,
    };
    // Only apply RINEX-derived fields if they were previously stored
    if (config.startEpoch !== null) update.startEpoch = config.startEpoch;
    if (config.endEpoch !== null) update.endEpoch = config.endEpoch;
    if (config.epochInterval !== null) update.epochInterval = config.epochInterval;
    if (config.constellationCodes !== null) {
      update.constellationCodes = phaseConstellationCodes(config.constellationCodes);
    }
    set(update);
  },

  applyImportedConfig: (config) => {
    // PEA epochs use "YYYY-MM-DD HH:MM:SS"; convert to datetime-local for the store
    const startEpoch = config.start_epoch != null
      ? epochToDatetimeLocal(config.start_epoch)
      : null;
    const endEpoch = config.end_epoch != null
      ? epochToDatetimeLocal(config.end_epoch)
      : null;

    const update: Partial<AppStore> = {};
    if (config.mode != null) update.mode = config.mode as ProcessingMode;
    if (startEpoch != null) update.startEpoch = startEpoch;
    if (endEpoch != null) update.endEpoch = endEpoch;
    if (config.epoch_interval != null) update.epochInterval = config.epoch_interval;
    if (config.output_pos != null) update.outputPos = config.output_pos;
    if (config.output_gpx != null) update.outputGpx = config.output_gpx;
    if (config.output_trace != null) update.outputTrace = config.output_trace;
    if (config.output_snx != null) update.outputSnx = config.output_snx;
    if (config.output_spp != null) update.outputSpp = config.output_spp;
    // An imported file may carry the C/D/S entries earlier versions of the app wrote.
    const importedCodes = config.constellation_codes != null
      ? phaseConstellationCodes(config.constellation_codes)
      : null;
    if (importedCodes) update.constellationCodes = importedCodes;
    // Directories are deliberately absent: the products and output locations
    // belong to this machine, and an imported config — which may well have come
    // from someone else's — does not get to move them.
    // rinex_path is handled by the caller (useConfigIo) since it requires async
    // RINEX parsing.
    set(update);

    if (config.mode != null) persist(KEYS.mode, config.mode);
    if (startEpoch != null) persist(KEYS.startEpoch, startEpoch);
    if (endEpoch != null) persist(KEYS.endEpoch, endEpoch);
    if (config.epoch_interval != null) persist(KEYS.epochInterval, config.epoch_interval);
    if (config.output_pos != null) persist(KEYS.outputPos, config.output_pos);
    if (config.output_gpx != null) persist(KEYS.outputGpx, config.output_gpx);
    if (config.output_trace != null) persist(KEYS.outputTrace, config.output_trace);
    if (config.output_snx != null) persist(KEYS.outputSnx, config.output_snx);
    if (config.output_spp != null) persist(KEYS.outputSpp, config.output_spp);
    if (importedCodes) persist(KEYS.constellationCodes, importedCodes);
  },

  // Session-only: not persisted, so a restart always comes up with no file.
  setRinexPath: (path) => set({ rinexPath: path }),
  setOutputDir: (dir) => {
    persist(KEYS.outputDir, dir);
    // The last run lived under the old directory; the views fall back to the
    // new one until a run writes there.
    persist(KEYS.runDir, null);
    set({ outputDir: dir, runDir: null });
  },
  setRunName: (name) => set({ runName: name }),
  setRunDir: (dir) => {
    persist(KEYS.runDir, dir);
    set({ runDir: dir });
  },
  setProductsDir: (dir) => {
    persist(KEYS.productsDir, dir);
    set({ productsDir: dir });
  },

  setRinexData: (data) => {
    set({
      rinexData: data,
      // Loading a file is the other way into the shell (see hasEnteredWorkspace).
      hasEnteredWorkspace: true,
      // Seed editable fields from the parsed RINEX. Marker is clipped to 4 chars
      // for RINEX v3/v4 (e.g. "ALIC00AUS" → "ALIC"). v2 markers are already 4
      // chars and are left untouched. We do NOT trim receiver/antenna — whitespace
      // is meaningful for the client.
      markerName: clipMarkerForVersion(data.marker_name ?? "", data.rinex_version),
      receiverType: data.receiver_type ?? "",
      antennaType: data.antenna_type ?? "",
      antennaOffset: data.antenna_offset ?? [0, 0, 0],
      selectedConstellations:
        data.constellations.length > 0 ? data.constellations : ["GPS"],
      startEpoch: epochToDatetimeLocal(data.start_epoch),
      endEpoch: epochToDatetimeLocal(data.end_epoch),
      epochInterval: data.epoch_interval ?? 30,
    });
    persist(KEYS.startEpoch, epochToDatetimeLocal(data.start_epoch));
    persist(KEYS.endEpoch, epochToDatetimeLocal(data.end_epoch));
    persist(KEYS.epochInterval, data.epoch_interval ?? 30);

    const store = get();
    if (data.observation_codes) {
      store.setAvailableObsCodes(data.observation_codes);
      store.initializeConstellationCodes(data.observation_codes);
    }
  },

  clearExtraRinex: () => set({ extraRinex: [] }),

  addRinexFile: (file) => {
    const s = get();
    if (file.path === s.rinexPath || s.extraRinex.some((f) => f.path === file.path)) return;
    set(selectionPatch(s.rinexData, [...s.extraRinex, file]));
  },

  removeRinexFile: (path) => {
    const s = get();
    if (path === s.rinexPath) {
      const [next, ...rest] = s.extraRinex;
      if (!next) {
        set({ rinexPath: null, rinexData: null, ...selectionPatch(null, []) });
        return;
      }
      // The next file becomes the primary, so the header-derived fields are
      // re-seeded from it rather than left describing a file that is gone.
      set({ rinexPath: next.path });
      get().setRinexData(next.data);
      set(selectionPatch(next.data, rest));
      return;
    }
    if (!s.extraRinex.some((f) => f.path === path)) return;
    set(selectionPatch(s.rinexData, s.extraRinex.filter((f) => f.path !== path)));
  },

  // Note: do NOT trim — preserve exact whitespace.
  setMarkerName: (v) => set({ markerName: v }),
  setReceiverType: (v) => set({ receiverType: v }),
  setAntennaType: (v) => set({ antennaType: v }),
  setAntennaOffset: (axis, value) =>
    set((s) => {
      const next: [number, number, number] = [
        s.antennaOffset[0],
        s.antennaOffset[1],
        s.antennaOffset[2],
      ];
      next[axis] = value;
      return { antennaOffset: next };
    }),

  setMode: (m) => {
    persist(KEYS.mode, m);
    set({ mode: m });
  },

  toggleConstellation: (c) =>
    set((s) => ({
      selectedConstellations: s.selectedConstellations.includes(c)
        ? s.selectedConstellations.filter((x) => x !== c)
        : [...s.selectedConstellations, c],
    })),

  setStartEpoch: (e) => {
    persist(KEYS.startEpoch, e);
    set({ startEpoch: e });
  },
  setEndEpoch: (e) => {
    persist(KEYS.endEpoch, e);
    set({ endEpoch: e });
  },
  setEpochInterval: (n) => {
    persist(KEYS.epochInterval, n);
    set({ epochInterval: n });
  },

  setOutputFormat: (fmt, val) => {
    if (fmt === "pos") { persist(KEYS.outputPos, val); return set({ outputPos: val }); }
    if (fmt === "gpx") { persist(KEYS.outputGpx, val); return set({ outputGpx: val }); }
    if (fmt === "trace") { persist(KEYS.outputTrace, val); return set({ outputTrace: val }); }
    if (fmt === "snx") { persist(KEYS.outputSnx, val); return set({ outputSnx: val }); }
    if (fmt === "spp") { persist(KEYS.outputSpp, val); return set({ outputSpp: val }); }
  },

  setConfigOverwrite: (val) => {
    persist(KEYS.configOverwrite, val);
    set({ configOverwrite: val });
  },

  setProcessingParam: (path, value) =>
    set((s) => {
      const next = { ...s.processingParams, [path]: value };
      persist(KEYS.processingParams, next);
      return { processingParams: next };
    }),

  // Drop the key entirely rather than storing the default, so "unset" and
  // "explicitly set to the default value" stay the same thing.
  resetProcessingParam: (path) =>
    set((s) => {
      const next = { ...s.processingParams };
      delete next[path];
      persist(KEYS.processingParams, next);
      return { processingParams: next };
    }),

  resetAllProcessingParams: () => {
    persist(KEYS.processingParams, {});
    set({ processingParams: {} });
  },

  // Non-fatal: on failure the page falls back to the curated table, which is
  // still correct — it just won't show parameters added by a newer PEA.
  loadDiscoveredParams: async () => {
    try {
      const params = await invoke<DiscoveredParam[]>("discover_config_params");
      set({ discoveredParams: params });
    } catch (e) {
      console.error("discover_config_params failed:", e);
    }
  },

  appendLine: (line) =>
    set((s) => ({ consoleLines: capLines([...s.consoleLines, line]) })),

  appendLines: (lines) =>
    set((s) => {
      if (lines.length === 0) return {};
      // Epoch progress rides on the same update as the Console, so a
      // "Processed epoch #N" line can never show there without also moving
      // the Workflow tab's counter.
      let latest: number | null = null;
      for (const line of lines) {
        const m = PROCESSED_EPOCH.exec(line);
        if (m) latest = Number(m[1]);
      }
      return {
        consoleLines: capLines([...s.consoleLines, ...lines]),
        ...(latest !== null && {
          epochProgress: {
            processed: latest,
            total: s.epochProgress?.total ?? null,
          },
        }),
      };
    }),

  clearConsole: () => set({ consoleLines: [] }),

  appendWorkflowLine: (line) =>
    set((s) => ({ workflowLines: capLines([...s.workflowLines, line]) })),

  appendWorkflowLines: (lines) =>
    set((s) => ({
      workflowLines:
        lines.length === 0
          ? s.workflowLines
          : capLines([...s.workflowLines, ...lines]),
    })),

  clearWorkflow: () => set({ workflowLines: [], epochProgress: null }),

  setIsProcessing: (v) => set({ isProcessing: v }),
  setStatus: (s) => set({ status: s }),
  setPeaBinaryPath: (path) => {
    // Persist so the active binary (a download or a custom pick) survives a
    // restart, and a stale override is overwritten the moment a new version is
    // downloaded.
    persist(KEYS.peaBinaryPath, path);
    set({ peaBinaryPath: path });
  },
  // A tagged binary is always a managed one, so it is also the managed release.
  setPeaVersion: (v) => set(v ? { peaVersion: v, managedPeaVersion: v } : { peaVersion: null }),
  setManagedPeaVersion: (v) => set({ managedPeaVersion: v }),
  setPeaUpdateAvailable: (v) => set({ peaUpdateAvailable: v }),
  setPeaUpdateModalOpen: (v) => set({ peaUpdateModalOpen: v }),
  setCustomPeaModalOpen: (v) => set({ customPeaModalOpen: v }),
  setLastCustomPeaPath: (p) => {
    persist(KEYS.lastCustomPeaPath, p);
    set({ lastCustomPeaPath: p });
  },
  dismissCustomPeaNotice: (path) => {
    persist(KEYS.customPeaNoticeSeen, path);
    set({ customPeaNoticeSeen: path, customPeaModalOpen: false });
  },
  bumpPosEpoch: () => set((s) => ({ posEpoch: s.posEpoch + 1 })),
  startEpochProgress: (total) => set({ epochProgress: { processed: 0, total } }),

  startActivity: (title, steps) =>
    set({
      activityHidden: false,
      activity: {
        title,
        steps: steps.map((s) => ({ ...s, state: "pending", detail: null, percent: null })),
        finished: false,
        error: null,
      },
    }),

  beginActivityStep: (key, detail = null) =>
    set((s) => patchStep(s.activity, key, { state: "active", detail, percent: null })),

  // Detail/percent only apply while the step is running — a late event from a
  // superseded scan must not re-animate a step that already settled.
  updateActivityStep: (key, patch) =>
    set((s) => patchStep(s.activity, key, patch, ["active"])),

  updateActiveActivityStep: (patch) =>
    set((s) => {
      const active = s.activity?.steps.find((st) => st.state === "active");
      return active ? patchStep(s.activity, active.key, patch, ["active"]) : {};
    }),

  resolveActivityStep: (key, state, detail) =>
    set((s) =>
      patchStep(s.activity, key, {
        state,
        percent: null,
        ...(detail !== undefined ? { detail } : {}),
      }),
    ),

  endActivity: (error = null) =>
    set((s) => {
      if (!s.activity) return {};
      if (s.activityHidden) return { activity: null, activityHidden: false };
      return {
        activity: {
          ...s.activity,
          // Anything never reached is reported as skipped rather than left
          // spinning — an unresolved step reads as "still working".
          steps: s.activity.steps.map((st) =>
            st.state === "pending" || st.state === "active"
              ? { ...st, state: error ? "skipped" : "done", percent: null }
              : st,
          ),
          finished: true,
          error: error ?? s.activity.error,
        },
      };
    }),

  clearActivity: () => set({ activity: null, activityHidden: false }),
  hideActivity: () => set((s) =>
    s.activity && !s.activity.finished
      ? { activityHidden: true }
      : { activity: null, activityHidden: false },
  ),
  showActivity: () => set({ activityHidden: false }),

  setTheme: (t, manual = false) => {
    localStorage.setItem("ginan-theme", t);
    if (manual) localStorage.setItem("ginan-theme-manual", "true");
    document.documentElement.setAttribute("data-theme", t);
    set({ theme: t });
  },
  setTimeZone: (tz) => {
    if (!isValidTimeZone(tz)) return;
    persist(KEYS.timeZone, tz);
    set({ timeZone: tz });
  },
  setConnectionFault: (msg) => set({ connectionFault: msg }),
  clearConnectionFault: () => set({ connectionFault: null }),

  setProviderHealth: (provider, status, checkedAt) => {
    const ts = checkedAt ?? Math.floor(Date.now() / 1000);
    set((s) => ({
      providerHealth: { ...s.providerHealth, [provider]: { status, checkedAt: ts } },
    }));
  },

  setOfflineMode: (v) => set({ offlineMode: v }),
  setCredentialsModalOpen: (v) => set({ credentialsModalOpen: v }),

  setSettingsOpen: (v) => set({ settingsOpen: v }),
  setSettingsPage: (p) => set({ settingsPage: resolveSettingsPage(p) }),
  // Open settings, optionally jumping straight to the page that owns a given
  // setting (used by the path strip so a chip deep-links to where it's set).
  openSettings: (p) =>
    set(p ? { settingsOpen: true, settingsPage: resolveSettingsPage(p) } : { settingsOpen: true }),

  completeOnboarding: () => {
    persist(KEYS.onboardingDone, true);
    set({ onboardingStatus: "done" });
  },

  // Replaying setup also clears the tour history: the point of running it again
  // is to see the whole first-launch experience, tours included.
  restartOnboarding: () => {
    persist(KEYS.onboardingDone, false);
    persist(KEYS.completedTours, []);
    set({
      onboardingStatus: "active",
      completedTours: [],
      activeTour: null,
      settingsOpen: false,
      credentialsModalOpen: false,
    });
  },

  // A tour owns the screen, so nothing else may be over it when it starts.
  startTour: (tour) =>
    set({ activeTour: tour, settingsOpen: false, credentialsModalOpen: false }),

  endTour: () => {
    const tour = get().activeTour;
    // Record the tour as seen whether it was completed, skipped or dismissed:
    // once it has auto-started, it must never pop up on its own again. The Help
    // button still replays it on demand — startTour bypasses this gate.
    if (tour && !get().completedTours.includes(tour)) {
      const next = [...get().completedTours, tour];
      persist(KEYS.completedTours, next);
      set({ completedTours: next, activeTour: null });
      return;
    }
    set({ activeTour: null });
  },

  setConfigSection: (key) => set({ configSection: key }),
  requestWorkspaceTab: (tab) => set({ workspaceTabRequest: tab }),

  toggleSidebar: () => {
    const next = !get().sidebarCollapsed;
    localStorage.setItem("ginan-sidebar-collapsed", String(next));
    set({ sidebarCollapsed: next });
  },

  // A drag calls this on every pointermove, so the localStorage write is
  // opt-in — only the released width is worth putting on disk.
  setSidebarWidth: (px, commit = false) => {
    const width = clampSidebarWidth(px);
    if (commit) localStorage.setItem("ginan-sidebar-width", String(width));
    set({ sidebarWidth: width });
  },

  setCddisStatus: (status, message = null) =>
    set({ cddisStatus: status, cddisStatusMessage: message }),
  bumpCddisEpoch: () => set((s) => ({
    cddisEpoch: s.cddisEpoch + 1,
    cddisChecksPaused: s.offlineMode,
  })),
  setAtxPath: (path) => set({ atxPath: path }),
  setAntennaValid: (valid) => set({ antennaValid: valid }),
  setBia: (bia) => set({ bia }),
  setCodeIntersection: (intersection) => set({ codeIntersection: intersection }),

  setInputMode: (m) =>
    set((s) => {
      // A running PEA was started with this mode's inputs; switching would
      // clear them underneath it.
      if (s.inputMode === m || s.isProcessing) return {};
      return {
        inputMode: m,
        // Entering realtime counts as entering the shell, so switching back to
        // file mode with no RINEX loaded keeps the user here.
        hasEnteredWorkspace: s.hasEnteredWorkspace || m === "realtime",
        // Clear competing input data to prevent leaked state
        rinexPath: m === "realtime" ? null : s.rinexPath,
        rinexData: m === "realtime" ? null : s.rinexData,
        extraRinex: m === "realtime" ? [] : s.extraRinex,
        // Leaving realtime drops the whole live setup, serial settings
        // included; entering it re-syncs the caster from the persisted setup —
        // DEFAULT_REALTIME_INPUTS and the Settings value can have drifted apart.
        ...(m === "file"
          ? freshRealtime()
          : withProducts(s, s.inputProducts)),
      };
    }),
  updateRealtime: (patch) =>
    set((s) => {
      const { obs_streams, nav_streams, ...knobs } = patch;
      const products = { ...s.inputProducts };
      if (obs_streams) products.obs = reparse(products.obs, obs_streams);
      if (nav_streams) products.corr = reparse(products.corr, nav_streams);
      return withProducts({ casterUrl: s.casterUrl, realtime: { ...s.realtime, ...knobs } }, products);
    }),
  setCasterUrl: (url) =>
    set((s) => {
      persist(KEYS.casterUrl, url);
      return { casterUrl: url, ...withProducts({ ...s, casterUrl: url }, s.inputProducts) };
    }),
  fetchNtripSourcetable: async () => {
    const { casterUrl } = get();
    const host = extractHost(casterUrl);
    if (!host) {
      set({
        ntripSourcetableStatus: { state: "error", message: "Set a caster URL first" },
      });
      return;
    }
    set({ ntripSourcetableStatus: { state: "loading" } });
    try {
      const streams = await invoke<NtripStream[]>("fetch_ntrip_sourcetable", {
        casterUrl,
      });
      set({
        ntripSourcetable: streams,
        ntripSourcetableStatus: { state: "ok", count: streams.length, host },
      });
    } catch (e) {
      set({
        ntripSourcetable: [],
        ntripSourcetableStatus: { state: "error", message: String(e) },
      });
    }
  },
  setRealtimeStorageCapMb: (mb) =>
    set({ realtimeStorageCapMb: Math.max(0, Math.floor(mb)) }),
  setStreamSerial: (which, next) =>
    set((s) => {
      const section: InputSection = which;
      let i = 0;
      const products = s.inputProducts[section].map((p) =>
        isLiveProduct(p) ? { ...p, serial: next[i++] } : p,
      );
      return withProducts(s, { ...s.inputProducts, [section]: products });
    }),
  applyStreamTest: (result) => {
    // Record what each tested Corrections stream carried, so its role no
    // longer rests on its name. Streams the test did not reach keep theirs.
    const tested = (result.snapshot as { streams?: Record<string, { families?: Record<string, number> }> }).streams ?? {};
    set((s) => {
      const corr = s.inputProducts.corr.map((p) => {
        const families = isLiveProduct(p) ? tested[streamMount(p)]?.families : undefined;
        const detected = families ? rolesFromFamilies(families) : [];
        return detected.length > 0 ? { ...p, detected } : p;
      });
      return { streamTest: result, ...withProducts(s, { ...s.inputProducts, corr }) };
    });
    const store = get();
    store.setAvailableObsCodes(result.observation_codes);
    // Every Process click and Streams-tab Test runs this, so a plain re-seed
    // would put back every signal the user had just switched off.
    store.initializeConstellationCodes(result.observation_codes, { keepSelection: true });
    // The interval field takes whole seconds, 1 or more.
    if (result.interval_s != null) {
      store.setEpochInterval(Math.max(1, Math.round(result.interval_s)));
    }
  },

  setAvailableObsCodes: (codes) => set({ availableObsCodes: codes }),

  initializeConstellationCodes: (codes, options) => {
    const previous = get().constellationCodes;
    // The header lists code, phase, Doppler and strength for every signal; only the
    // phase ones are selectable (see config/obsCodes.ts). Narrow before either path,
    // so a realtime re-seed that keeps the selection cannot re-admit C/D/S codes.
    const items = (
      key: keyof ConstellationCodeConfig,
      list: string[],
    ): ObservationCodeItem[] => {
      const phase = phaseCodes(list);
      return options?.keepSelection
        ? mergeObservedCodes(previous[key] ?? [], phase)
        : phase.map((code) => ({ code, enabled: true, valid: true }));
    };
    const constellationCodes: ConstellationCodeConfig = {
      gps: items("gps", codes.gps),
      gal: items("gal", codes.gal),
      glo: items("glo", codes.glo),
      bds: items("bds", codes.bds),
      qzs: items("qzs", codes.qzs),
    };
    set({ constellationCodes });
    persist(KEYS.constellationCodes, constellationCodes);
  },

  setConstellationCodes: (codes) => {
    const phaseOnly = phaseConstellationCodes(codes);
    persist(KEYS.constellationCodes, phaseOnly);
    set({ constellationCodes: phaseOnly });
  },

  updateConstellationCode: (constellation, index, update) => {
    const constellationKey =
      constellation.toLowerCase() as keyof ConstellationCodeConfig;
    set((state) => {
      const newCodes = { ...state.constellationCodes };
      const currentCodes = [...newCodes[constellationKey]];
      if (currentCodes[index]) {
        currentCodes[index] = { ...currentCodes[index], ...update };
        newCodes[constellationKey] = currentCodes;
      }
      const loadedStatus = buildLoadedStatus(state.rinexData, newCodes, state.isProcessing);
      return loadedStatus
        ? { constellationCodes: newCodes, status: loadedStatus }
        : { constellationCodes: newCodes };
    });
    persist(KEYS.constellationCodes, get().constellationCodes);
  },

  setConstellationCodeValidity: (constellation, index, valid) => {
    const key = constellation.toLowerCase() as keyof ConstellationCodeConfig;
    const item = get().constellationCodes[key][index];
    if (!item) return;
    // A code the BIA drops starts deselected, but only on that transition:
    // re-validating must not undo a user who ticked an invalid code back on.
    const update: Partial<ObservationCodeItem> =
      item.valid && !valid ? { valid, enabled: false } : { valid };
    get().updateConstellationCode(constellation, index, update);
  },

  reorderConstellationCodes: (constellation, fromIndex, toIndex) => {
    const constellationKey =
      constellation.toLowerCase() as keyof ConstellationCodeConfig;
    set((state) => {
      const newCodes = { ...state.constellationCodes };
      const currentCodes = [...newCodes[constellationKey]];
      const [movedItem] = currentCodes.splice(fromIndex, 1);
      currentCodes.splice(toIndex, 0, movedItem);
      newCodes[constellationKey] = currentCodes;
      return { constellationCodes: newCodes };
    });
    persist(KEYS.constellationCodes, get().constellationCodes);
  },

  setConstellationEnabled: (constellation, enabled) => {
    const key = constellation.toLowerCase() as keyof ConstellationCodeConfig;
    set((state) => {
      const newCodes = { ...state.constellationCodes };
      newCodes[key] = newCodes[key].map((item) => ({ ...item, enabled }));
      const loadedStatus = buildLoadedStatus(state.rinexData, newCodes, state.isProcessing);
      return loadedStatus
        ? { constellationCodes: newCodes, status: loadedStatus }
        : { constellationCodes: newCodes };
    });
    persist(KEYS.constellationCodes, get().constellationCodes);
  },

  setBiaValidation: (state) =>
    set((current) => ({
      biaValidation: { ...current.biaValidation, ...state },
    })),

  resetConfig: () => set({
    mode: "Static",
    selectedConstellations: ["GPS"],
    startEpoch: "",
    endEpoch: "",
    epochInterval: 30,
    outputPos: true,
    outputGpx: false,
    outputTrace: false,
    outputSnx: false,
    outputSpp: true,
    status: "Config reset",
  }),

  setPppProvider: (provider) => {
    persist(KEYS.pppProvider, provider);
    set((s) => ({ pppSelection: { ...s.pppSelection, provider } }));
    // Cascade: re-derive series, then project, if the current ones are not
    // published by the newly chosen provider in the catalogue.
    if (get().pppCatalogue) {
      const seriesList = get().getAvailableSeries(provider);
      if (seriesList.length > 0 && !seriesList.includes(get().pppSelection.series)) {
        get().setPppSeries(seriesList[0]);
      } else {
        // Series unchanged but project may still be invalid under it.
        const projectList = get().getAvailableProjects(provider, get().pppSelection.series);
        if (projectList.length > 0 && !projectList.includes(get().pppSelection.project)) {
          get().setPppProject(projectList[0]);
        }
      }
    }
  },
  setPppSeries: (series) => {
    persist(KEYS.pppSeries, series);
    set((s) => ({ pppSelection: { ...s.pppSelection, series } }));
    // Cascade: re-derive project if the current one isn't valid for this series.
    if (get().pppCatalogue) {
      const projectList = get().getAvailableProjects(get().pppSelection.provider, series);
      if (projectList.length > 0 && !projectList.includes(get().pppSelection.project)) {
        get().setPppProject(projectList[0]);
      }
    }
  },
  setPppProject: (project) => {
    persist(KEYS.pppProject, project);
    set((s) => {
      // Cross-filter: if the current series isn't valid for the new project, snap to first valid.
      const seriesList = get().getAvailableSeriesForProject(s.pppSelection.provider, project);
      const series = seriesList.includes(s.pppSelection.series) ? s.pppSelection.series : (seriesList[0] ?? s.pppSelection.series);
      persist(KEYS.pppSeries, series);
      return { pppSelection: { ...s.pppSelection, project, series } };
    });
  },

  setPppCatalogue: (catalogue) => {
    set({ pppCatalogue: catalogue });
    // Reconcile the current selection against the new catalogue so the UI never
    // shows a combo that doesn't exist. Empty/null catalogues leave it alone.
    const providers = get().getAvailableProviders();
    if (providers.length === 0) return;
    const sel = get().pppSelection;
    const provider = providers.includes(sel.provider) ? sel.provider : providers[0];
    const seriesList = get().getAvailableSeries(provider);
    const series =
      seriesList.includes(sel.series) || seriesList.length === 0 ? sel.series : seriesList[0];
    const projectList = get().getAvailableProjects(provider, series);
    const project =
      projectList.includes(sel.project) || projectList.length === 0 ? sel.project : projectList[0];
    if (provider !== sel.provider || series !== sel.series || project !== sel.project) {
      persist(KEYS.pppProvider, provider);
      persist(KEYS.pppSeries, series);
      persist(KEYS.pppProject, project);
      set({ pppSelection: { provider, series, project } });
    }
  },

  getAvailableProviders: () =>
    catalogueValues(get().pppCatalogue, "provider", () => true),
  getAvailableSeries: (provider) =>
    catalogueValues(get().pppCatalogue, "series", (e) => e.provider === provider),
  getAvailableSeriesForProject: (provider, project) =>
    catalogueValues(
      get().pppCatalogue,
      "series",
      (e) => e.provider === provider && e.project === project,
    ),
  getAvailableProjects: (provider, series) =>
    catalogueValues(
      get().pppCatalogue,
      "project",
      (e) => e.provider === provider && e.series === series,
    ),
  getAvailableProjectsForProvider: (provider) =>
    catalogueValues(get().pppCatalogue, "project", (e) => e.provider === provider),

  availableConstellationsForCurrentSelection: () => {
    const { availableObsCodes, pppCatalogue, pppSelection, inputMode } = get();

    // Constellations present in the loaded RINEX file.
    const ALL: Constellation[] = ["GPS", "GAL", "GLO", "BDS", "QZS"];
    const rinexSet = new Set<Constellation>();
    if (inputMode === "realtime") {
      // A realtime run has no RINEX header to enumerate: the RTCM streams
      // decide which systems turn up, and only at runtime. Starting from the
      // empty set here would leave nothing for the PPP catalogue to narrow,
      // and every constellation would be written to the config as
      // `process: false` — PEA would then have no satellites, SPP would fail
      // every epoch, and no receiver position state would ever be estimated.
      for (const c of ALL) rinexSet.add(c);
    } else if (availableObsCodes) {
      for (const c of ALL) {
        const key = c.toLowerCase() as keyof ConstellationObsCodes;
        if ((availableObsCodes[key]?.length ?? 0) > 0) rinexSet.add(c);
      }
    }

    if (!pppCatalogue) return rinexSet; // no catalogue yet → permissive
    const entry = pppCatalogue.entries.find(
      (e) =>
        e.provider === pppSelection.provider &&
        e.series === pppSelection.series &&
        e.project === pppSelection.project
    );
    // Unknown combo or header unavailable → permissive (over-allow, never hide).
    if (!entry || entry.constellations.length === 0) return rinexSet;

    const published = new Set(entry.constellations);
    return new Set([...rinexSet].filter((c) => published.has(c)));
  },

  validateCodesAgainstBia: async () => {
    const state = get();
    const { productsDir } = state;
    const { provider, series, project } = state.pppSelection;

    state.setBiaValidation({ isLoading: true, error: undefined });

    try {
      // Prefer the BIA already fetched by the useBiaValidation hook
      // (fetch_bia_codes_for_combo, which handles network + cache). Only fall
      // back to the on-disk reader (fetch_bia_codes) when that hasn't run yet:
      // it returns empty arrays when no .BIA file is in products_dir, which
      // would mark every code invalid.
      let biaCodes: ConstellationObsCodes;
      if (state.bia) {
        biaCodes = {
          gps: state.bia.gps,
          gal: state.bia.gal,
          glo: state.bia.glo,
          bds: state.bia.bds,
          qzs: state.bia.qzs,
        };
      } else {
        biaCodes = await invoke<ConstellationObsCodes>("fetch_bia_codes", {
          provider,
          series,
          project,
          productsDir,
        });
      }

      state.setBiaValidation({ isLoading: false, validCodes: biaCodes });

      (Object.keys(state.constellationCodes) as (keyof ConstellationCodeConfig)[]).forEach(
        (key) => {
          const validSet = biaPhaseSignals(biaCodes[key]);
          state.constellationCodes[key].forEach((item, index) => {
            state.setConstellationCodeValidity(
              key.toUpperCase() as Constellation,
              index,
              validSet.has(item.code)
            );
          });
        }
      );
    } catch (error) {
      state.setBiaValidation({
        isLoading: false,
        error:
          error instanceof Error
            ? error.message
            : "Failed to validate against BIA",
      });
    }
  },
}));

/**
 * One constellation's code list rebuilt from a fresh observation, keeping what
 * the user already did to it.
 *
 * A realtime run re-observes the streams on every Process click, and the list
 * the user edits is the one that observation produced. Signals still seen keep
 * their `enabled` flag and their position; ones no longer seen are dropped;
 * new ones go on the end. A constellation the user switched off as a whole stays
 * off, so a signal that turns up late does not quietly half-enable it.
 */
function mergeObservedCodes(
  previous: ObservationCodeItem[],
  observed: string[],
): ObservationCodeItem[] {
  const seen = new Set(observed);
  const kept = previous.filter((item) => seen.has(item.code));
  const known = new Set(previous.map((item) => item.code));
  const switchedOff = previous.length > 0 && previous.every((item) => !item.enabled);
  const added = [...seen]
    .filter((code) => !known.has(code))
    .map((code) => ({ code, enabled: !switchedOff, valid: true }));
  return [...kept, ...added];
}

export function getEnabledCodesInOrder(codes: ObservationCodeItem[]): string[] {
  return codes.filter((item) => item.enabled).map((item) => item.code);
}

/**
 * Enabled phase (L) codes per constellation, in priority order.
 *
 * `allowed` restricts the result to constellations the selected PPP product
 * actually publishes. Pass it whenever the output is destined for a pea config
 * — a constellation the product cannot bias-correct is shown disabled in the
 * UI, so writing its codes into the config would contradict what the user sees.
 * Omit it to serialise the raw user selection.
 */
export function getConstellationObsCodesForConfig(
  constellationCodes: ConstellationCodeConfig,
  allowed?: Set<Constellation>
): ConstellationObsCodes {
  // Phase codes only, even if the store somehow holds others: `code_priorities`
  // is PEA's list of phase signals, so a C/D/S entry would be written as a signal
  // it cannot use.
  const codesFor = (
    key: keyof ConstellationCodeConfig,
    constellation: Constellation
  ): string[] =>
    allowed && !allowed.has(constellation)
      ? []
      : phaseCodes(getEnabledCodesInOrder(constellationCodes[key]));

  return {
    gps: codesFor("gps", "GPS"),
    gal: codesFor("gal", "GAL"),
    glo: codesFor("glo", "GLO"),
    bds: codesFor("bds", "BDS"),
    qzs: codesFor("qzs", "QZS"),
  };
}

/**
 * Constellations that are both enabled by the user and publishable by the
 * current PPP selection — i.e. exactly what should reach the pea config.
 */
export function getEffectiveConstellations(
  constellationCodes: ConstellationCodeConfig,
  allowed: Set<Constellation>
): string[] {
  return (["GPS", "GAL", "GLO", "BDS", "QZS"] as Constellation[])
    .filter((c) => allowed.has(c))
    .filter((c) => {
      const key = c.toLowerCase() as keyof ConstellationCodeConfig;
      return constellationCodes[key].some((item) => item.enabled);
    });
}

/**
 * Constellations a realtime run processes when the user has picked none.
 *
 * These are the systems Ginan's own `rt_ppp_config.yaml` enables, and the ones
 * the shipped template carries `code_priorities` for. Enabling a system the
 * template has no priorities for would leave PEA with no signals to select.
 */
export const REALTIME_DEFAULT_CONSTELLATIONS: Constellation[] = ["GPS", "GAL", "GLO"];

/**
 * Constellations for a realtime run.
 *
 * `constellationCodes` is seeded from a RINEX header, which a realtime run never
 * has, so the per-code selection stays empty and `getEffectiveConstellations`
 * returns nothing at all. Fall back to the defaults above, still narrowed to
 * what the selected PPP product publishes.
 */
export function getRealtimeConstellations(
  constellationCodes: ConstellationCodeConfig,
  allowed: Set<Constellation>
): string[] {
  const chosen = getEffectiveConstellations(constellationCodes, allowed);
  if (chosen.length > 0) return chosen;
  return REALTIME_DEFAULT_CONSTELLATIONS.filter((c) => allowed.has(c));
}

/**
 * Default run name: local date, hour and minute, then the input source, e.g.
 * `2026-10-22_1131_rinex`. Sortable, and a filename on every platform.
 */
export function defaultRunName(mode: InputMode, now = new Date()): string {
  const p = (n: number) => String(n).padStart(2, "0");
  const stamp =
    `${now.getFullYear()}-${p(now.getMonth() + 1)}-${p(now.getDate())}_` +
    `${p(now.getHours())}${p(now.getMinutes())}`;
  return `${stamp}_${mode === "realtime" ? "realtime" : "rinex"}`;
}

/**
 * The directory a run started now would write to: `outputDir/<run name>`. The
 * name is kept to one path segment — separators and characters Windows
 * rejects become `_`, and a name of only dots falls back to the default.
 */
export function runOutputDir(
  s: Pick<AppStore, "outputDir" | "runName" | "inputMode">,
  now = new Date(),
): string {
  const name = s.runName.trim().replace(/[\\/:*?"<>|]/g, "_");
  const safe = /^\.*$/.test(name) ? defaultRunName(s.inputMode, now) : name;
  return `${s.outputDir}/${safe}`;
}

/**
 * SBAS corrections only process in Dynamic mode, so while a realtime run has
 * an SBAS product the run uses Dynamic whatever `mode` holds. `mode` itself is
 * left alone, so removing SBAS (or leaving realtime) restores the user's pick.
 */
export function sbasForcesDynamic(s: Pick<AppStore, "inputMode" | "realtime">): boolean {
  return s.inputMode === "realtime" && !!s.realtime.sbas;
}

/** The processing mode a run uses: `mode`, unless SBAS forces Dynamic. */
export function effectiveMode(s: Pick<AppStore, "mode" | "inputMode" | "realtime">): ProcessingMode {
  return sbasForcesDynamic(s) ? "Dynamic" : s.mode;
}

/** Where the result views read from: the last run, else the output root. */
export const resultsDir = (s: Pick<AppStore, "runDir" | "outputDir">) =>
  s.runDir ?? s.outputDir;

/**
 * Assemble the exact `ConfigInputs` a run would send to `write_config`.
 *
 * Single source of truth for RunControls and the YAML inspector. The inspector
 * is only worth having if it shows what will actually be written, so it must
 * not re-derive any of this — if the two ever diverge the tab starts lying.
 *
 * Returns null when the run preconditions aren't met (no RINEX file or no
 * output directory), which is the same gate RunControls applies.
 */
export function buildConfigInputs(outputDir?: string): ConfigInputs | null {
  const s = useAppStore.getState();
  const isRealtime = s.inputMode === "realtime";
  // Realtime streams supply their own observations, so a RINEX path is not a
  // precondition — an output directory and at least one stream are.
  if (!s.outputDir) return null;
  if (isRealtime ? s.realtime.obs_streams.length === 0 : !s.rinexPath) return null;

  const allowed = s.availableConstellationsForCurrentSelection();

  return {
    rinex_path: s.rinexPath ?? "",
    rinex_paths: allRinexPaths(s),
    output_dir: outputDir ?? runOutputDir(s),
    products_dir: s.productsDir || s.outputDir,
    // CRITICAL: receiver/antenna are not trimmed — exact whitespace round-trips.
    marker_name: s.markerName || s.rinexData?.marker_name || "UNKN",
    receiver_type: s.receiverType,
    antenna_type: s.antennaType,
    antenna_offset: s.antennaOffset,
    approx_position: s.rinexData?.approx_position ?? null,
    mode: effectiveMode(s),
    constellations: isRealtime
      ? getRealtimeConstellations(s.constellationCodes, allowed)
      : getEffectiveConstellations(s.constellationCodes, allowed),
    start_epoch: datetimeLocalToEpoch(s.startEpoch),
    end_epoch: datetimeLocalToEpoch(s.endEpoch),
    epoch_interval: s.epochInterval,
    output_pos: s.outputPos,
    output_gpx: s.outputGpx,
    output_trace: s.outputTrace,
    output_snx: s.outputSnx,
    output_spp: s.outputSpp,
    constellation_obs_codes: getConstellationObsCodesForConfig(
      s.constellationCodes,
      allowed,
    ),
    ppp_provider: s.pppSelection.provider,
    ppp_series: s.pppSelection.series,
    ppp_project: s.pppSelection.project,
    // Only values moved off the template default travel to the backend, so an
    // untouched config generates byte-identically to one with no overrides.
    // Defaults come from the merged set so auto-discovered parameters are
    // compared against their real template value, not treated as unknown.
    advanced: changedParams(
      s.processingParams,
      defsByPath(mergeParamGroups(s.discoveredParams)),
    ),
    // Present only for realtime runs; its absence selects the RINEX path in
    // the backend config writer.
    ...(isRealtime ? { realtime: s.realtime } : {}),
  };
}

/**
 * Distinct, sorted values of one selection field across the catalogue entries
 * that pass `match`. Empty when no catalogue has been scanned yet.
 */
function catalogueValues(
  cat: PppCatalogue | null,
  field: keyof PppSelection,
  match: (e: PppCatalogue["entries"][number]) => boolean,
): string[] {
  if (!cat) return [];
  return Array.from(new Set(cat.entries.filter(match).map((e) => e[field]))).sort();
}

const MAX_CONSOLE_LINES = 50000;
function capLines(arr: string[]): string[] {
  return arr.length > MAX_CONSOLE_LINES
    ? arr.slice(arr.length - MAX_CONSOLE_LINES)
    : arr;
}

/**
 * The store update for a changed set of observation files: the new extras, the
 * time window widened (or narrowed) to the whole set, and everything derived
 * from the old window dropped.
 *
 * The catalogue and bias table are cleared in the same update as the window
 * for the reason `loadRinexFile` gives: the BIA effect keys on the start
 * epoch, so a new week arriving beside the old catalogue would ask a mirror
 * for a product that does not exist. The caller rescans afterwards.
 */
function selectionPatch(primary: RinexData | null, extras: RinexFile[]) {
  const span = rinexSpan([...(primary ? [primary] : []), ...extras.map((f) => f.data)]);
  const patch: Partial<AppStore> = {
    extraRinex: extras,
    pppCatalogue: null,
    bia: null,
    codeIntersection: null,
  };
  if (span.start) {
    patch.startEpoch = epochToDatetimeLocal(span.start);
    persist(KEYS.startEpoch, patch.startEpoch);
  }
  if (span.end) {
    patch.endEpoch = epochToDatetimeLocal(span.end);
    persist(KEYS.endEpoch, patch.endEpoch);
  }
  return patch;
}

function buildLoadedStatus(
  rinexData: RinexData | null,
  codes: ConstellationCodeConfig,
  isProcessing: boolean,
): string | null {
  if (!rinexData || isProcessing) return null;
  const active = (Object.keys(codes) as Array<keyof ConstellationCodeConfig>)
    .filter((k) => codes[k].some((c) => c.enabled))
    .map((k) => k.toUpperCase());
  const marker = (rinexData.marker_name ?? "unknown").slice(0, 4);
  return `Loaded: ${marker} · ${active.length > 0 ? active.join(", ") : "none"}`;
}

/**
 * Convert PEA epoch "YYYY-MM-DD HH:MM:SS" → datetime-local input value
 * "YYYY-MM-DDTHH:MM:SS". Seconds are preserved so the input (with step="1")
 * renders HH:MM:SS rather than truncating to HH:MM.
 */
export function epochToDatetimeLocal(epoch: string | null): string {
  if (!epoch) return "";
  return epoch.replace(" ", "T").slice(0, 19);
}

/** PEA's per-epoch log line (src/cpp/pea/main.cpp): "Processed epoch #42 - …". */
const PROCESSED_EPOCH = /Processed epoch #(\d+)/;

/**
 * How many epochs PEA will process between two datetime-local values at the
 * given interval (both ends inclusive), or null when the window is unset or
 * invalid. Both values are read as UTC — only their difference matters.
 */
export function expectedEpochCount(
  start: string,
  end: string,
  intervalSeconds: number,
): number | null {
  if (!start || !end || !(intervalSeconds > 0)) return null;
  const t0 = Date.parse(datetimeLocalToEpoch(start).replace(" ", "T") + "Z");
  const t1 = Date.parse(datetimeLocalToEpoch(end).replace(" ", "T") + "Z");
  if (Number.isNaN(t0) || Number.isNaN(t1) || t1 < t0) return null;
  return Math.floor((t1 - t0) / 1000 / intervalSeconds) + 1;
}

/**
 * Convert a datetime-local input value back to PEA epoch format.
 * Pads HH:MM → HH:MM:00 if the browser omitted seconds.
 */
export function datetimeLocalToEpoch(dt: string): string {
  if (!dt) return "";
  const withSeconds = dt.length === 16 ? dt + ":00" : dt;
  return withSeconds.replace("T", " ");
}

/**
 * Clip a station marker name to the canonical 4-char form when the RINEX file
 * is v3 or v4 (e.g. "ALIC00AUS" → "ALIC"). v2 markers are already 4 chars by
 * convention and are returned unchanged. If version is unknown, return as-is.
 */
export function clipMarkerForVersion(
  marker: string,
  version: number | null | undefined
): string {
  if (!marker) return "";
  if (version != null && version >= 3.0 && marker.length > 4) {
    return marker.slice(0, 4);
  }
  return marker;
}

/**
 * Each `serial://` stream's line settings, keyed by device path — the key
 * `start_pea_realtime` looks them up by. Lists are paired with their
 * index-aligned settings; a missing entry means the defaults.
 */
export function serialByDevice(
  ...lists: [string[], SerialSettings[]][]
): Record<string, SerialSettings> {
  const out: Record<string, SerialSettings> = {};
  for (const [streams, settings] of lists) {
    streams.forEach((uri, i) => {
      if (uri.startsWith("serial://")) {
        out[uri.slice("serial://".length)] ??= settings[i] ?? DEFAULT_SERIAL_SETTINGS;
      }
    });
  }
  return out;
}

/**
 * Run the stream test (`test_streams`) on the current realtime setup and apply
 * what it finds: constellations, codes, epoch interval, and the full stats for
 * the Streams tab. Writes the config before the test (the test reads its
 * streams from it) and again after, so it carries the new values. Returns the
 * config path. Used by Start and by the Streams tab's Test button.
 *
 * `outputDir` is where the config goes: Start passes its run directory; the
 * Test button leaves it out, so a test writes to the output root rather than
 * creating a run directory of its own.
 */
/**
 * How long the stream test watches the streams, in seconds: the backend's
 * `DEFAULT_TEST_EPOCHS` of 1 s epochs (`commands/stream_test.rs`). Connecting
 * and decoding come on top, so a test runs a little longer than this.
 */
export const STREAM_TEST_SECONDS = 10;

/** The settings a stream test depends on. Constellations/interval are outputs of the test, so excluded. */
export function streamFingerprint(s: Pick<AppStore, "realtime" | "inputProducts">): string {
  return JSON.stringify([s.realtime, s.inputProducts]);
}

export async function runStreamTest(outputDir?: string): Promise<string> {
  const s = useAppStore.getState();
  // A new run starts from a clean live view; Start calls this first.
  s.clearLiveState();
  s.setStreamTestState({ status: "running" });
  try {
    const path = await runStreamTestInner(outputDir);
    s.setStreamTestState({ status: "passed", at: Date.now() });
    useAppStore.setState({ streamTestFingerprint: streamFingerprint(useAppStore.getState()) });
    return path;
  } catch (e) {
    const error = e instanceof Error ? e.message : String(e);
    s.setStreamTestState({ status: "failed", error, at: Date.now() });
    // The Streams tab shows the error too, but the Workflow and Console are
    // where a failed Process is followed up.
    s.appendWorkflowLine(`Stream test failed: ${error}`);
    s.appendLine(`[test] Failed: ${error}`);
    throw e;
  }
}

async function runStreamTestInner(outputDir?: string): Promise<string> {
  const s = useAppStore.getState();
  const dir = outputDir ?? s.outputDir ?? undefined;
  const inputs = buildConfigInputs(dir);
  if (!inputs || !s.outputDir) {
    throw new Error("Add at least one observation stream and an output directory first");
  }
  const write = (i: ConfigInputs) =>
    invoke<string>("write_config", { inputs: i, overwrite: s.configOverwrite });

  s.setStatus("Testing streams...");
  s.appendWorkflowLine("Testing streams to see which satellites they carry...");
  const configPath = await write(inputs);
  const result = await invoke<StreamTestResult>("test_streams", {
    options: {
      config_path: configPath,
      output_dir: s.outputDir,
      caster_url: s.realtime.caster_url,
      serial: serialForProducts(s.inputProducts),
      // The backend checks SISNet itself during the test when SBAS is on.
      sbas_mode: s.realtime.sbas?.mode ?? null,
    },
  });
  s.applyStreamTest(result);

  for (const st of result.streams) {
    s.appendWorkflowLine(
      `   ${st.mountpoint}: ${st.satellites} satellites, ` +
        (st.interval_s != null ? `an epoch every ${st.interval_s.toFixed(1)} s` : "rate unknown"),
    );
  }
  s.appendWorkflowLine(
    `Constellations: ${result.constellations.join(", ")}` +
      (result.interval_s != null
        ? ` · interval set to ${Math.max(1, Math.round(result.interval_s))} s`
        : ""),
  );

  const tested = buildConfigInputs(dir);
  return tested ? write(tested) : configPath;
}
