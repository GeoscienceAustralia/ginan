export interface RinexData {
  rinex_version: number | null; // e.g. 2.11, 3.04, 4.00
  marker_name: string | null;
  receiver_type: string | null;
  antenna_type: string | null;
  antenna_offset: [number, number, number]; // H, E, N
  approx_position: [number, number, number] | null; // X, Y, Z
  start_epoch: string | null; // "YYYY-MM-DD HH:MM:SS", GPS time
  end_epoch: string | null;
  epoch_interval: number | null;
  /** Time system the file was written in ("GPS", "GLO", "BDT", ...); the
   *  epochs above are already converted from it to GPS time. */
  time_system?: string | null;
  constellations: string[]; // ["GPS", "GAL", ...]
  observation_codes?: ConstellationObsCodes; // available observation codes from the header
}

export interface ConfigInputs {
  /** The primary observation file — the one whose header seeds the fields below. */
  rinex_path: string;
  /**
   * Every selected observation file, primary first. The backend writes each one
   * under `inputs.gnss_observations.rnx_inputs`. Empty for realtime runs.
   */
  rinex_paths: string[];
  output_dir: string;
  products_dir: string;
  marker_name: string;
  receiver_type: string;
  antenna_type: string;
  antenna_offset: [number, number, number];
  approx_position: [number, number, number] | null;
  mode: string;
  constellations: string[];
  start_epoch: string;
  end_epoch: string;
  epoch_interval: number;
  output_pos: boolean;
  output_gpx: boolean;
  output_trace: boolean;
  output_snx: boolean;
  /** The SBAS `.SPP` solution; only written while SBAS is used. */
  output_spp: boolean;
  constellation_obs_codes?: ConstellationObsCodes; // Priority-ordered enabled codes per constellation
  ppp_provider: string;
  ppp_series: string;
  ppp_project: string;
  /**
   * Present only in realtime runs; `undefined` runs the RINEX-file path.
   * The backend uses this to swap `inputs.gnss_observations.rnx_inputs` for
   * `rtcm_inputs`, drop the epoch window, and pace the epoch loop.
   */
  realtime?: RealtimeInputs;
  /** Processing parameters keyed by dotted YAML path — see config/processingParams.ts. */
  advanced?: Record<string, unknown>;
}

/**
 * Live GNSS stream configuration. Mirrors the Rust `RealtimeInputs` struct.
 *
 * Credentials are deliberately absent. `caster_url` is a plain `scheme://host/`
 * string with no login in it; the backend looks the login up from netrc by
 * hostname and splices it in at config write time, so no NTRIP password ever
 * travels through the frontend or an exported YAML.
 *
 * An empty `caster_url` means "no root to prefix" — every entry in
 * `obs_streams`/`nav_streams` is then expected to be a complete URI.
 */
export interface RealtimeInputs {
  caster_url: string;
  obs_streams: string[];
  nav_streams: string[];
  ssr_antenna_offset: "APC" | "COM";
  wait_next_epoch: number;
  max_rec_latency: number;
  require_obs: boolean;
  record_rtcm: boolean;
  record_decoded: boolean;
  record_rinex: boolean;
  /** SouthPAN SBAS corrections in place of SSR; absent means none. */
  sbas?: { mode: SbasMode };
}

/** `processing_options.sbas.mode`: L1 and DFMC are SPP, PVS is PPP. */
export type SbasMode = "L1" | "DFMC" | "PVS";

/**
 * Line settings for `serial://` streams. Sent with `start_pea_realtime` to the
 * serial setup script only; the PEA config is not changed.
 */
export interface SerialSettings {
  baud: number;
  data: 5 | 6 | 7 | 8;
  parity: "none" | "even" | "odd";
  stop: 1 | 2;
  flow: "none" | "rtscts" | "xonxoff";
  flush: boolean;
  flush_duration_ms: number;
}

/** Same defaults as scripts/serial/start_gnss_stream.sh. */
export const DEFAULT_SERIAL_SETTINGS: SerialSettings = {
  baud: 115200,
  data: 8,
  parity: "none",
  stop: 1,
  flow: "none",
  flush: true,
  flush_duration_ms: 0,
};

/** The kinds of input product a sidebar section can hold. */
export type ProductType = "file" | "http" | "ntrip" | "serial" | "sbas";

/** The two product lists in the Input sidebar: observations and corrections. */
export type InputSection = "obs" | "corr";

/**
 * One card in an input section. `value` is what the user edits: a mountpoint
 * for `ntrip`, a device path (no `serial://`) for `serial`, a full URL for
 * `http`. `serial` holds the line settings and is only read for `serial`
 * products. A `file` product has no path of its own: it is a view over the
 * store's `rinexPath` / `rinexData`.
 */
/** What a Corrections stream carries: broadcast ephemeris or SSR corrections. */
export type CorrectionRole = "bce" | "ssr";

export interface InputProduct {
  id: string;
  type: ProductType;
  value: string;
  serial?: SerialSettings;
  /** Corrections only: the group it was added under, for when neither the
   *  stream test nor its name says what it carries. */
  role?: CorrectionRole;
  /** Corrections only: what the last stream test saw on this stream. */
  detected?: CorrectionRole[];
}

/** One stream's figures from the pre-run stream test (`test_streams`). */
export interface StreamTestStats {
  mountpoint: string;
  records: number;
  satellites: number;
  interval_s: number | null;
}

/** What the pre-run stream test found across all observation streams. */
export interface StreamTestResult {
  constellations: string[];
  observation_codes: ConstellationObsCodes;
  /** Mean seconds between observation epochs, averaged over the streams. */
  interval_s: number | null;
  streams: StreamTestStats[];
  /** The monitor's full snapshot — the fields the Streams tab shows. */
  snapshot: MonitorSnapshot;
  /** The app's own SISNet check, run beside the test when SBAS is on. */
  sbas?: SbasProbe | null;
}

/** What SISNet sent, counted by the app (`sbas_monitor::SbasCounts`). */
export interface SbasCounts {
  messages: number;
  crc_errors: number;
  invalid_lines: number;
  /** Good messages per SBAS message type, keyed by the type number. */
  by_type: Record<string, number>;
  first_gpst_ms: number | null;
  last_gpst_ms: number | null;
}

/** `sbas_monitor::SbasProbe`: the stream test's SISNet check. */
export interface SbasProbe extends SbasCounts {
  host: string;
  prn: number;
  mode: string;
  state: "ok" | "no_login" | "unreachable" | "auth_failed" | "start_failed" | "no_data";
  /** Never holds the login. */
  error: string | null;
  seconds: number;
  messages_per_s: number | null;
}

/** The parts of the `rtcm_monitor` snapshot (src-tauri) the UI reads. */
export interface MonitorSnapshot {
  total_by_type: Record<string, number>;
  /** Records per type over the last `window_seconds` (live monitor only). */
  window_by_type?: Record<string, number>;
  window_msm_by_mountpoint?: Record<string, number>;
  window_seconds?: number;
  runtime_seconds?: number;
  malformed_lines: number;
  unknown_records: number;
  msm: Record<
    string,
    {
      records: number;
      invalid_records: number;
      avg_snr: number | null;
      last_epoch: string | null;
      satellite_count: number;
      constellations: Record<string, number>;
      signals_by_constellation: Record<string, string[]>;
    }
  >;
  ssr: Record<
    string,
    { records: number; satellites: string[]; codes: string[]; message_counts: Record<string, number> }
  >;
  brdc_ephemeris: Record<string, { records: number; satellites: string[] }>;
}

export type InputMode = "file" | "realtime";

export interface PppSelection {
  provider: string;
  series: string;
  project: string;
}

/**
 * One (provider, series, project) combination discovered on CDDIS for
 * the loaded observation window, with which product types are available.
 * Produced by the `scan_ppp_catalogue` Tauri command.
 */
export interface PppCatalogueEntry {
  provider: string;
  series: string;
  project: string;
  has_sp3: boolean;
  has_clk: boolean;
  has_bia: boolean;
  /** Constellations this combo's SP3 carries (e.g. ["GPS","GLO"]). Empty when
   *  the orbit header couldn't be read — treated as permissive (allow all). */
  constellations: string[];
  /** Actual filenames discovered on the mirror per content type ("SP3", "CLK", "BIA").
   *  Used by the downloader to fetch real files instead of template-guessed ones.
   *  Empty for catalogue entries from older backend versions. */
  files: Record<string, string>;
}

export interface PppCatalogue {
  entries: PppCatalogueEntry[];
}

/** All providers, series, and projects the backend can probe. Used as a
 *  fallback while the catalogue scan is in progress. */
export const PPP_PROVIDERS = [
  "IGS", "COD", "EMR", "ESA", "GFZ", "GRG", "JPL", "MIT", "NGS", "SIO", "USN", "WHU",
] as const;

export const PPP_SERIES_LABELS: Record<string, string> = {
  FIN: "Final",
  RAP: "Rapid",
  ULT: "Ultra-Rapid",
  NRT: "Near-Real-Time",
};

export const PPP_PROJECT_LABELS: Record<string, string> = {
  OPS: "Operational",
  MGX: "Multi-GNSS",
  DEMO: "Demo",
  REPRO3: "Reprocessing 3",
};

/** First-launch fallback used before any RINEX file has been scanned. */
export const DEFAULT_PPP_SELECTION: PppSelection = {
  provider: "IGS",
  series: "FIN",
  project: "OPS",
};

export type ProcessingMode = "Static" | "Kinematic" | "Dynamic";
export type Constellation = "GPS" | "GAL" | "GLO" | "BDS" | "QZS";

/**
 * Raw observation codes available per constellation from RINEX file
 */
export interface ConstellationObsCodes {
  gps: string[];
  gal: string[];
  glo: string[];
  bds: string[];
  qzs: string[];
}

/**
 * What a caster said when asked to open a mountpoint with the stored login.
 *
 * `inconclusive` is deliberately separate from `rejected`: a caster that cannot
 * be reached has not refused anything, and telling someone their password is
 * wrong when the network is down sends them to reset a working login.
 */
export type NtripAuthResult =
  | { state: "accepted"; host: string; mountpoint: string }
  | { state: "rejected"; host: string; detail: string }
  | { state: "inconclusive"; host: string; detail: string };

/**
 * Individual observation code item with state
 */
export interface ObservationCodeItem {
  code: string;
  enabled: boolean;
  valid: boolean; // Based on BIA validation
}

/**
 * Complete constellation code configuration with user settings
 */
export interface ConstellationCodeConfig {
  gps: ObservationCodeItem[];
  gal: ObservationCodeItem[];
  glo: ObservationCodeItem[];
  bds: ObservationCodeItem[];
  qzs: ObservationCodeItem[];
}

/**
 * UI-relevant settings extracted from a PEA config YAML on import.
 * All fields are optional — any may be absent in the file.
 */
export interface PeaConfigExtract {
  mode: string | null;
  start_epoch: string | null;
  end_epoch: string | null;
  epoch_interval: number | null;
  output_pos: boolean | null;
  output_gpx: boolean | null;
  output_trace: boolean | null;
  output_snx: boolean | null;
  output_spp: boolean | null;
  constellation_codes: ConstellationCodeConfig | null;
  /**
   * Reconstructed RINEX path; null if the file doesn't exist on this machine.
   *
   * The products and output directories are not part of this: an imported file
   * never gets to move this install's directories.
   */
  rinex_path: string | null;
}

/**
 * Code priorities per constellation as returned by the backend
 * `fetch_bia_codes_for_combo` command. Mirrors the Rust `BiaCodePriorities`
 * struct in `commands/products.rs`.
 */
export interface BiaCodePriorities {
  gps: string[];
  gal: string[];
  glo: string[];
  bds: string[];
  qzs: string[];
}

/**
 * Per-constellation intersection of RINEX-observed obs codes with BIA-supported
 * obs codes. Populated by the useBiaValidation hook.
 */
export interface CodeIntersection {
  usable: string[];   // codes present in both RINEX and BIA
  rinexOnly: string[]; // RINEX-observed codes the BIA does not support
  biaOnly: string[];  // BIA-supported codes not present in RINEX
}

/** State of the manual "Validate BIA" action in the constellation filter. */
export interface BiaValidationState {
  isLoading: boolean;
  lastProvider?: string;
  lastSeries?: string;
  lastProject?: string;
  validCodes?: ConstellationObsCodes;
  error?: string;
}

/**
 * Outcome of resolving the pea binary (ensure_default_pea_binary /
 * download_pea_version). `source` is "override" (user-supplied path),
 * "existing" (a previously downloaded binary, reused as-is) or "downloaded"
 * (fetched just now). `update_available` names a newer release when the
 * installed binary is behind — the app never auto-updates, so this drives the
 * prompt to switch versions in Settings.
 */
export interface PeaBinaryStatus {
  path: string;
  version: string | null;
  source: "override" | "existing" | "downloaded";
  update_available: string | null;
  /** The selected managed release, even while a custom binary is in use. */
  managed_version?: string | null;
}

/** One selectable pea release for the current platform. */
export interface PeaVersionInfo {
  tag: string;
  asset: string;
  /** Already in the local cache, so switching to it needs no download. */
  downloaded: boolean;
}

/**
 * A `.pos` solution file found in the output directory. Mirrors the Rust
 * `PosFileInfo` in `commands/visualisation.rs`.
 */
/** Which solution a file holds: PEA's `.pos`, or an SBAS run's `.SPP`. */
export type SolutionFormat = "pos" | "spp";

export interface PosFileInfo {
  path: string;
  name: string;
  format: SolutionFormat;
  /** True for PEA's `*_smoothed.pos` backward-smoothed companion file. */
  smoothed: boolean;
  size: number;
  modified_ms: number | null;
  /** False for a file with no epochs in it yet — empty or header only. */
  has_epochs: boolean;
}

/** Inverse-variance-weighted statistics for one position component. */
export interface ComponentStats {
  weighted_mean: number;
  std_dev: number;
  rms: number;
  min: number;
  max: number;
  mean_sigma: number;
}

export interface PosStats {
  dn: ComponentStats;
  de: ComponentStats;
  du: ComponentStats;
}

/**
 * A parsed `.pos` file ready to plot. Components are metres from the a priori
 * NEU reference position, sigmas their 1σ formal errors, and `t` epoch
 * milliseconds of GPS time as written in the file (never zone-converted).
 *
 * The arrays are decimated to a display budget when the file is long; `stats`
 * is always computed over every epoch, so the numbers on screen describe the
 * whole solution and not the drawn sample.
 */
export interface PosSeries {
  path: string;
  name: string;
  /** `spp` carries no formal errors: its sigmas are all zero. */
  format: SolutionFormat;
  station: string | null;
  smoothed: boolean;
  total_epochs: number;
  returned_epochs: number;
  decimated: boolean;
  skipped_lines: number;
  t: number[];
  dn: number[];
  de: number[];
  du: number[];
  sn: number[];
  se: number[];
  su: number[];
  /** Absolute WGS84 latitude/longitude, decimal degrees, per epoch. */
  lat: number[];
  lon: number[];
  /** Horizontal / vertical protection levels per epoch, metres: `spp` only, empty for `pos`. */
  hpl: number[];
  vpl: number[];
  stats: PosStats;
}

/** Which component a chart series carries. */
export type PosComponentKey = "dn" | "de" | "du";

/**
 * One epoch pushed by the backend's `.pos` tail (`start_pos_tail`), emitted as
 * a `pos-epoch` event. Same fields as one slice of {@link PosSeries}'s arrays.
 */
export interface PosEpochEvent {
  path: string;
  t: number;
  dn: number;
  de: number;
  du: number;
  sn: number;
  se: number;
  su: number;
  lat: number;
  lon: number;
  /** `spp` only. */
  hpl: number | null;
  vpl: number | null;
}

/**
 * A `.TRACE` file found in the output directory. Mirrors the Rust
 * `TraceFileInfo` in `commands/trace.rs`.
 */
export interface TraceFileInfo {
  path: string;
  name: string;
  size: number;
  modified_ms: number | null;
  /** False for a file holding only PEA's config header, no residual rows. */
  has_residuals: boolean;
}

/** Which measurement a residual series carries. */
export type MeasKind = "code" | "phase" | "other";

/**
 * Spread of one residual column over a series. Unweighted, unlike
 * {@link ComponentStats}: a residual's sigma is the noise PEA assumed for the
 * measurement, not the precision of the residual itself.
 */
export interface ResidualStats {
  mean: number;
  std_dev: number;
  rms: number;
  min: number;
  max: number;
  mean_sigma: number;
}

/**
 * One plottable line from a `.TRACE`: the residuals of a single signal from a
 * single satellite at a single receiver. `t` is epoch milliseconds of GPS time
 * as written in the file (never zone-converted), matching {@link PosSeries}.
 */
export interface ResidualSeries {
  /** `"CODE_MEAS|G07|ALIC|L1C"` — stable across reloads, safe as a React key. */
  key: string;
  sat: string;
  /** "GPS", "GLO", "GAL", "BDS", "QZS", "IRN", "SBS", "LEO" or "Other". */
  constellation: string;
  receiver: string;
  signal: string;
  meas_type: string;
  kind: MeasKind;
  total_samples: number;
  t: number[];
  prefit: number[];
  postfit: number[];
  sigma: number[];
  prefit_stats: ResidualStats;
  postfit_stats: ResidualStats;
}

/** One satellite seen in a `.TRACE`, with what it contributed. */
export interface SatelliteSummary {
  sat: string;
  constellation: string;
  signals: string[];
  receivers: string[];
  samples: number;
  first_ms: number;
  last_ms: number;
  postfit_rms: number;
}

/**
 * Everything in the file, independent of which series were requested — always
 * computed over the whole `.TRACE`, so a selector built from it still offers
 * the satellites the current filter excluded.
 */
export interface TraceIndex {
  receivers: string[];
  signals: string[];
  satellites: SatelliteSummary[];
  epochs: number;
  total_samples: number;
  t_start: number;
  t_end: number;
}

/**
 * A parsed `.TRACE` ready to plot. The arrays are decimated to a display
 * budget shared across every returned series; each series' `*_stats` is always
 * computed over every sample in the file, not the drawn subset.
 */
export interface TraceSeries {
  path: string;
  name: string;
  index: TraceIndex;
  series: ResidualSeries[];
  returned_samples: number;
  decimated: boolean;
  skipped_lines: number;
}

/** The satellites one constellation contributed. */
export interface ConstellationStatus {
  constellation: string;
  satellites: SatelliteSummary[];
}

/**
 * Which satellites a run is using, grouped by constellation, from the
 * `load_satellite_status` command.
 *
 * Carries its own "nothing to show" state rather than failing: trace output is
 * a config toggle and a live run has no `.TRACE` for its first few seconds, so
 * `available: false` with a `message` is a normal result, not an error.
 */
export interface SatelliteStatus {
  path: string | null;
  name: string | null;
  available: boolean;
  message: string | null;
  receivers: string[];
  constellations: ConstellationStatus[];
  epochs: number;
  satellite_count: number;
  t_start: number;
  /** The most recent epoch in the file; a satellite whose `last_ms` matches it
   *  was still contributing when the file was last written. */
  t_end: number;
}
