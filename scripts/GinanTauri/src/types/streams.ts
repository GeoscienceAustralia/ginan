/**
 * Contract for the Streams tab. The data layer (useStreamsModel,
 * store, Tauri listeners) produces these shapes; the UI under
 * src/components/streams/ only consumes them. Rust payload shapes are in
 * the `Live*` interfaces and mirror what rtcm_monitor.rs / realtime.rs emit.
 */
import type { MonitorSnapshot, SatelliteStatus, SbasProbe, StreamTestResult } from "./index";

// ---------------------------------------------------------------- Rust payloads

/** Per-message-type stats for one mountpoint (rtcm_monitor `streams[m].messages[n]`). */
export interface LiveMessageStats {
  name: string;
  instances: number;
  records: number;
  /** Last arrival as ms on PEA's GPST clock. Compare only with `snapshot_gpst_ms`. */
  last_rx_ms: number | null;
  /** Median of `inter_arrival_s`, null until two instances arrived. */
  periodicity_s: number | null;
  /** Up to 60 most recent inter-arrival gaps, oldest first. */
  inter_arrival_s: number[];
  /** SSR update interval advertised in the message, if any. */
  nominal_udi_s: number | null;
  last_sats: string[];
  last_signals: string[];
}

export interface LiveSatelliteRx {
  constellation: string;
  cn0_avg: number | null;
  signals: string[];
  last_rx_ms: number | null;
}

export interface LiveStreamStats {
  role_hint: "OBS" | "CORR" | "NAV" | null;
  families: Record<string, number>;
  window_families: Record<string, number>;
  records: number;
  window_records: number;
  epochs_per_s: number | null;
  last_rx_ms: number | null;
  sats_by_constellation: Record<string, number>;
  messages: Record<string, LiveMessageStats>;
  satellites: Record<string, LiveSatelliteRx>;
}

export interface LiveThroughput {
  /** Null when `record_rtcm` is off (no recording to measure). */
  bytes_total: number | null;
  bytes_per_s: number | null;
  /** [unix_ms, bytes_per_s] samples every 5 s, ≤ 60, oldest first. */
  history: [number, number][];
}

/** PEA's SISNet log lines this run (`sbas_monitor::SbasConsole`). */
export interface LiveSbasConsole {
  auth_failures: number;
  start_failures: number;
  invalid_lines: number;
  crc_errors: number;
  /** Only counted when PEA logs at debug level. */
  messages: number;
  last_error: string | null;
  /** Local unix ms. */
  last_error_at_ms: number | null;
}

/** The `sbas` block of a live snapshot, present when the run uses SBAS. */
export interface LiveSbas {
  mode: string;
  console: LiveSbasConsole;
  /** From the newest `.SPP`; null until one has an epoch (PVS writes none). */
  levels: { file: string | null; points: [number, number, number][] } | null;
}

/** The `live-stream-stats` event / `get_live_stream_stats` payload. */
export interface LiveSnapshot extends MonitorSnapshot {
  sbas?: LiveSbas;
  snapshot_gpst_ms?: number | null;
  streams?: Record<string, LiveStreamStats>;
  throughput?: Record<string, LiveThroughput>;
  /** Unix ms of the last `reset_stream_counters` (any mount), null if never. */
  reset_at_ms?: number | null;
}

/** `get_station_info(outputDir, mount)` result. */
export interface StationInfo {
  mount: string;
  station_id: number | null;
  ecef_x: number | null;
  ecef_y: number | null;
  ecef_z: number | null;
  antenna_height: number | null;
  antenna_desc: string | null;
  antenna_setup_id: number | null;
  antenna_serial: string | null;
  receiver_type: string | null;
  receiver_firmware: string | null;
  receiver_serial: string | null;
  /** Message numbers the values came from, e.g. [1006, 1033]. */
  messages: number[];
  source_file: string | null;
  /** mtime of `source_file`, unix ms. */
  updated_ms: number | null;
}

// ---------------------------------------------------------------- NTRIP telemetry

export type NtripConnState = "connecting" | "streaming" | "reconnecting" | "failed" | "idle";

export interface NtripConnection {
  mount: string;
  state: NtripConnState;
  retryCount: number;
  lastHttpStatus: number | null;
  lastError: string | null;
  /** Unix ms of the last 200 response. */
  connectedSince: number | null;
  /** Unix ms of the last event of any kind for this mount. */
  lastEventAt: number;
  chunksReceived: number | null;
}

// ---------------------------------------------------------------- View model

export type StreamRole = "OBS" | "CORR" | "NAV";

/** What a stream card / header shows. `receiving` = no NTRIP lines (serial) but data arrives. */
export type StreamStatus =
  | "connecting"
  | "streaming"
  | "receiving"
  | "reconnecting"
  | "stale"
  | "failed"
  | "idle";

export type MessageGroup = "msm" | "ephemeris" | "ssr" | "station" | "bias" | "other";

export interface MessageView {
  msgNo: string;
  name: string;
  /** Long description from src/config/rtcmMessages.ts. */
  description: string;
  group: MessageGroup;
  /** "GPS" | "GAL" | "GLO" | "BDS" | "QZS" | null */
  constellation: string | null;
  instances: number;
  /** 0–1 share of this stream's instances. */
  share: number;
  lastRxMs: number | null;
  ageS: number | null;
  periodicityS: number | null;
  /** age > 1.5 × periodicity */
  late: boolean;
  interArrivalS: number[];
  lastSats: string[];
  lastSignals: string[];
}

export interface SatelliteView {
  sat: string;
  constellation: string;
  signals: string[];
  cn0: number | null;
  /** From the .TRACE; null when no trace is available. */
  inSolution: boolean | null;
  postfitRms: number | null;
  samples: number | null;
  lastEpochMs: number | null;
  dropped: boolean;
}

export interface StreamView {
  mount: string;
  role: StreamRole | null;
  status: StreamStatus;
  retryCount: number;
  lastError: string | null;
  httpStatus: number | null;
  casterHost: string | null;
  protocol: string | null;
  formatDetails: string | null;
  /** Sourcetable identifier/city, e.g. "Alice Springs". */
  location: string | null;
  bytesTotal: number | null;
  bytesPerSec: number | null;
  /** bytes/s, oldest first, ≤ 60 points. */
  throughputHistory: number[];
  lastEventAgeMs: number | null;
  records: number;
  windowRecords: number;
  epochsPerSec: number | null;
  avgSnr: number | null;
  invalid: number;
  satsByConstellation: Record<string, number>;
  satCount: number;
  messages: MessageView[];
}

export type StreamsPhase =
  | "file-mode"
  | "idle"
  | "test-running"
  | "test-failed"
  | "waiting"
  | "live"
  | "stopped";

export type StreamTestStatus = "idle" | "running" | "passed" | "failed";

export interface RecordsMixEntry {
  family: "ssr" | "msm" | "ephemeris" | "station" | "other";
  label: string;
  count: number;
  ratePerS: number;
}

/** The SBAS card: what the stream test's SISNet check and the run report. */
export interface SbasView {
  mode: string;
  /** The last stream test's SISNet check; null before one with SBAS on. */
  probe: SbasProbe | null;
  /** This run's (or the last run's) `sbas` snapshot block. */
  live: LiveSbas | null;
}

export interface StreamsModel {
  phase: StreamsPhase;
  /** Present while the realtime inputs use SBAS. */
  sbas: SbasView | null;
  isProcessing: boolean;
  recordDecodedOff: boolean;
  runtimeS: number | null;
  resetAtMs: number | null;
  casterHosts: string[];
  kpis: {
    bytesTotal: number | null;
    bytesPerSec: number | null;
    recordsPerSec: number | null;
    satsInSolution: number | null;
    satsSeen: number | null;
    malformed: number;
    unknown: number;
  };
  mix: RecordsMixEntry[];
  streams: StreamView[];
  test: {
    status: StreamTestStatus;
    error: string | null;
    atMs: number | null;
    result: StreamTestResult | null;
  };
  satelliteStatus: SatelliteStatus | null;
  /** Received (decoded MSM) satellites for a mount joined with the .TRACE solution. */
  satellitesFor: (mount: string) => SatelliteView[];
  runStreamTest: () => Promise<void>;
  resetCounters: (mount?: string) => Promise<void>;
}
