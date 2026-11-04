import { useEffect, useMemo, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { resultsDir, runStreamTest, SERIAL_PREFIX, useAppStore } from "../store/appStore";
import { describeMessage } from "../config/rtcmMessages";
import type { NtripStream } from "../config/ntrip";
import type { InputMode, SatelliteStatus } from "../types";
import type {
  LiveSnapshot,
  LiveStreamStats,
  MessageView,
  NtripConnection,
  RecordsMixEntry,
  SatelliteView,
  StreamRole,
  StreamStatus,
  StreamTestStatus,
  StreamView,
  StreamsModel,
  StreamsPhase,
} from "../types/streams";
import type { StreamTestResult } from "../types";

/** Everything the model derives from; plain data so it can be unit-tested. */
export interface StreamsInputs {
  inputMode: InputMode;
  isProcessing: boolean;
  realtime: {
    caster_url: string;
    obs_streams: string[];
    nav_streams: string[];
    record_decoded: boolean;
    record_rtcm: boolean;
    sbas?: { mode: string };
  };
  sourcetable: NtripStream[];
  test: { status: StreamTestStatus; error: string | null; atMs: number | null; result: StreamTestResult | null };
  snapshot: LiveSnapshot | null;
  /** Local `Date.now()` the snapshot arrived. */
  snapshotAtMs: number | null;
  connections: Record<string, NtripConnection>;
  satelliteStatus: SatelliteStatus | null;
  /** Local `Date.now()`; only used for local-clock facts and ticking GPST forward. */
  nowMs: number;
  runStreamTest: () => Promise<void>;
  resetCounters: (mount?: string) => Promise<void>;
}

const LATE_FACTOR = 1.5;
const STALE_FACTOR = 3;

const lc = (s: string) => s.toLowerCase();

function hostPort(url: string): string | null {
  try {
    const u = new URL(url);
    const port = u.port || (u.protocol === "https:" ? "443" : u.protocol === "http:" ? "80" : "");
    return port ? `${u.hostname}:${port}` : u.hostname;
  } catch {
    return null;
  }
}

function recordFamily(key: string): RecordsMixEntry["family"] {
  const k = lc(key);
  if (k === "msm") return "msm";
  if (k.startsWith("ssr")) return "ssr";
  if (k.startsWith("brdc") || k.startsWith("eph")) return "ephemeris";
  if (k.startsWith("station")) return "station";
  return "other";
}

const FAMILY_LABEL: Record<RecordsMixEntry["family"], string> = {
  msm: "MSM observations",
  ssr: "SSR corrections",
  ephemeris: "Ephemeris",
  station: "Station",
  other: "Other",
};

function buildMix(snap: LiveSnapshot | null): RecordsMixEntry[] {
  if (!snap) return [];
  const win = snap.window_seconds ?? 60;
  const acc = new Map<RecordsMixEntry["family"], { count: number; window: number }>();
  const keys = new Set([...Object.keys(snap.total_by_type ?? {}), ...Object.keys(snap.window_by_type ?? {})]);
  for (const k of keys) {
    const f = recordFamily(k);
    const e = acc.get(f) ?? { count: 0, window: 0 };
    e.count += snap.total_by_type?.[k] ?? 0;
    e.window += snap.window_by_type?.[k] ?? 0;
    acc.set(f, e);
  }
  return [...acc.entries()]
    .filter(([, v]) => v.count > 0)
    .map(([family, v]) => ({ family, label: FAMILY_LABEL[family], count: v.count, ratePerS: win > 0 ? v.window / win : 0 }));
}

function buildMessages(stats: LiveStreamStats | undefined, gpstNow: number | null): MessageView[] {
  if (!stats) return [];
  const entries = Object.entries(stats.messages ?? {});
  const total = entries.reduce((n, [, m]) => n + m.instances, 0);
  return entries
    .map(([msgNo, m]): MessageView => {
      const info = describeMessage(msgNo);
      const ageS = gpstNow != null && m.last_rx_ms != null ? Math.max(0, (gpstNow - m.last_rx_ms) / 1000) : null;
      const periodicityS = m.periodicity_s ?? m.nominal_udi_s ?? null;
      return {
        msgNo,
        name: info.group === "other" && m.name ? m.name : info.name,
        description: info.description,
        group: info.group,
        constellation: info.constellation,
        instances: m.instances,
        share: total > 0 ? m.instances / total : 0,
        lastRxMs: m.last_rx_ms,
        ageS,
        periodicityS,
        late: ageS != null && periodicityS != null && periodicityS > 0 && ageS > LATE_FACTOR * periodicityS,
        interArrivalS: m.inter_arrival_s ?? [],
        lastSats: m.last_sats ?? [],
        lastSignals: m.last_signals ?? [],
      };
    })
    .sort((a, b) => Number(a.msgNo) - Number(b.msgNo));
}

function deriveStatus(
  conn: NtripConnection | undefined,
  stats: LiveStreamStats | undefined,
  isProcessing: boolean,
  gpstNow: number | null,
): StreamStatus {
  if (!isProcessing) return "idle";
  const hasData = !!stats && stats.records > 0;
  const periods = Object.values(stats?.messages ?? {})
    .map((m) => m.periodicity_s)
    .filter((p): p is number => p != null && p > 0);
  const typical = periods.length ? Math.min(...periods) : null;
  const isStale =
    hasData && typical != null && gpstNow != null && stats!.last_rx_ms != null &&
    (gpstNow - stats!.last_rx_ms) / 1000 > STALE_FACTOR * typical;
  if (conn) {
    if (conn.state === "streaming") return isStale ? "stale" : "streaming";
    if (conn.state === "idle") return hasData ? (isStale ? "stale" : "receiving") : "connecting";
    return conn.state;
  }
  if (hasData) return isStale ? "stale" : "receiving";
  return "connecting";
}

export function buildStreamsModel(i: StreamsInputs): StreamsModel {
  const snap = i.snapshot;
  const rt = i.realtime;
  const elapsedS = i.isProcessing && i.snapshotAtMs != null ? Math.max(0, (i.nowMs - i.snapshotAtMs) / 1000) : 0;
  // GPST "now": the snapshot's own clock advanced by local elapsed time; never Date.now() directly.
  const gpstNow = snap?.snapshot_gpst_ms != null ? snap.snapshot_gpst_ms + elapsedS * 1000 : null;

  let phase: StreamsPhase;
  if (i.inputMode === "file") phase = "file-mode";
  else if (i.test.status === "running") phase = "test-running";
  else if (i.isProcessing) phase = snap ? "live" : "waiting";
  else if (snap) phase = "stopped";
  else if (i.test.status === "failed") phase = "test-failed";
  else phase = "idle";

  const obsSet = new Set(rt.obs_streams);
  const navSet = new Set(rt.nav_streams);
  const mounts = [
    ...rt.obs_streams,
    ...rt.nav_streams,
    ...Object.keys(snap?.streams ?? {}),
    ...Object.keys(i.connections),
  ].filter((m, idx, a) => m && a.indexOf(m) === idx);
  const byMount = new Map(i.sourcetable.map((s) => [lc(s.mountpoint), s]));
  const casterHost = hostPort(rt.caster_url);
  const th = snap?.throughput ?? {};

  const streams: StreamView[] = i.inputMode === "file" ? [] : mounts.map((mount) => {
    const serial = mount.startsWith(SERIAL_PREFIX);
    const st = snap?.streams?.[mount];
    const conn = i.connections[mount];
    const src = byMount.get(lc(mount));
    let role: StreamRole | null = null;
    if (obsSet.has(mount)) role = "OBS";
    else if (st?.role_hint) role = st.role_hint;
    else if (src?.category === "ssr") role = "CORR";
    else if (src?.category === "obs" && !navSet.has(mount)) role = "OBS";
    const t = th[mount];
    const msm = snap?.msm?.[mount];
    const sats = st?.sats_by_constellation ?? {};
    return {
      mount,
      role,
      status: deriveStatus(conn, st, i.isProcessing, gpstNow),
      retryCount: conn?.retryCount ?? 0,
      lastError: conn?.lastError ?? null,
      httpStatus: conn?.lastHttpStatus ?? null,
      casterHost: serial ? mount.slice(SERIAL_PREFIX.length) : casterHost,
      protocol: src?.format || null,
      formatDetails: src?.format_details || null,
      location: src?.identifier || null,
      bytesTotal: t?.bytes_total ?? null,
      bytesPerSec: t?.bytes_per_s ?? null,
      throughputHistory: (t?.history ?? []).map((h) => h[1]),
      lastEventAgeMs: conn ? Math.max(0, i.nowMs - conn.lastEventAt) : null,
      records: st?.records ?? 0,
      windowRecords: st?.window_records ?? 0,
      epochsPerSec: st?.epochs_per_s ?? null,
      avgSnr: msm?.avg_snr ?? null,
      invalid: msm?.invalid_records ?? 0,
      satsByConstellation: sats,
      satCount: Object.values(sats).reduce((n, v) => n + v, 0),
      messages: buildMessages(st, gpstNow),
    };
  });

  const sums = (k: "bytes_total" | "bytes_per_s") => {
    const vals = Object.values(th).map((t) => t[k]).filter((v): v is number => v != null);
    return vals.length ? vals.reduce((a, b) => a + b, 0) : null;
  };
  const ss = i.satelliteStatus?.available ? i.satelliteStatus : null;
  const winTotal = snap?.window_by_type ? Object.values(snap.window_by_type).reduce((a, b) => a + b, 0) : null;
  const hosts = casterHost && streams.some((s) => !s.mount.startsWith(SERIAL_PREFIX)) ? [casterHost] : [];

  const satellitesFor = (mount: string): SatelliteView[] => {
    const rx = snap?.streams?.[mount]?.satellites ?? {};
    const trace = new Map(ss?.constellations.flatMap((c) => c.satellites).map((s) => [s.sat, s]) ?? []);
    return Object.entries(rx)
      .map(([sat, r]): SatelliteView => {
        const t = trace.get(sat);
        const dropped = !!(ss && t && t.last_ms < ss.t_end);
        return {
          sat,
          constellation: r.constellation,
          signals: r.signals,
          cn0: r.cn0_avg,
          inSolution: ss ? !!t && !dropped : null,
          postfitRms: t?.postfit_rms ?? null,
          samples: t?.samples ?? null,
          lastEpochMs: t?.last_ms ?? null,
          dropped,
        };
      })
      .sort((a, b) => a.sat.localeCompare(b.sat));
  };

  return {
    phase,
    sbas:
      i.inputMode === "realtime" && rt.sbas
        ? { mode: rt.sbas.mode, probe: i.test.result?.sbas ?? null, live: snap?.sbas ?? null }
        : null,
    isProcessing: i.isProcessing,
    recordDecodedOff: rt.record_decoded === false,
    runtimeS: snap?.runtime_seconds != null ? snap.runtime_seconds + elapsedS : null,
    resetAtMs: snap?.reset_at_ms ?? null,
    casterHosts: hosts,
    kpis: {
      bytesTotal: sums("bytes_total"),
      bytesPerSec: sums("bytes_per_s"),
      recordsPerSec: winTotal != null ? winTotal / (snap?.window_seconds || 60) : null,
      satsInSolution: ss ? ss.constellations.reduce((n, c) => n + c.satellites.filter((s) => s.last_ms >= ss.t_end).length, 0) : null,
      satsSeen: ss ? ss.satellite_count : null,
      malformed: snap?.malformed_lines ?? 0,
      unknown: snap?.unknown_records ?? 0,
    },
    mix: buildMix(snap),
    streams,
    test: { ...i.test },
    satelliteStatus: i.satelliteStatus,
    satellitesFor,
    runStreamTest: i.runStreamTest,
    resetCounters: i.resetCounters,
  };
}

/* ---- shared .TRACE poller: one timer / throttled fetch however many components use the hook ---- */

const POLL_MS = 5000;
let users = 0;
let timer: ReturnType<typeof setInterval> | null = null;
let lastFetchAt = 0;

async function fetchSatelliteStatus() {
  const now = Date.now();
  if (now - lastFetchAt < 1000) return;
  lastFetchAt = now;
  const { setSatelliteStatus } = useAppStore.getState();
  const outputDir = resultsDir(useAppStore.getState());
  try {
    setSatelliteStatus(await invoke<SatelliteStatus>("load_satellite_status", { outputDir: outputDir ?? "", path: null }));
  } catch {
    // Unreadable trace: keep the previous value.
  }
}

function useSatelliteStatusPolling() {
  const outputDir = useAppStore(resultsDir);
  const posEpoch = useAppStore((s) => s.posEpoch);
  useEffect(() => {
    users++;
    if (users === 1) {
      timer = setInterval(() => {
        if (useAppStore.getState().isProcessing) void fetchSatelliteStatus();
      }, POLL_MS);
    }
    return () => {
      if (--users === 0 && timer) {
        clearInterval(timer);
        timer = null;
      }
    };
  }, []);
  useEffect(() => {
    void fetchSatelliteStatus();
  }, [outputDir, posEpoch]);
}

/** The only data entry point for the Streams tab UI. */
export function useStreamsModel(): StreamsModel {
  const inputMode = useAppStore((s) => s.inputMode);
  const isProcessing = useAppStore((s) => s.isProcessing);
  const realtime = useAppStore((s) => s.realtime);
  const sourcetable = useAppStore((s) => s.ntripSourcetable);
  const streamTest = useAppStore((s) => s.streamTest);
  const testStatus = useAppStore((s) => s.streamTestStatus);
  const testError = useAppStore((s) => s.streamTestError);
  const testAt = useAppStore((s) => s.streamTestAt);
  // While a test runs its start time stands in, so the Streams tab can count it.
  const testStartedAt = useAppStore((s) => s.streamTestStartedAt);
  const snapshot = useAppStore((s) => s.liveSnapshot);
  const snapshotAtMs = useAppStore((s) => s.liveSnapshotAt);
  const connections = useAppStore((s) => s.ntripConnections);
  const satelliteStatus = useAppStore((s) => s.satelliteStatus);
  const resetStreamCounters = useAppStore((s) => s.resetStreamCounters);

  useSatelliteStatusPolling();

  // 1 s local tick so "ago" values and uptime advance between 5 s snapshots.
  const [nowMs, setNowMs] = useState(() => Date.now());
  useEffect(() => {
    if (!isProcessing) return;
    setNowMs(Date.now());
    const id = setInterval(() => setNowMs(Date.now()), 1000);
    return () => clearInterval(id);
  }, [isProcessing]);

  return useMemo(
    () =>
      buildStreamsModel({
        inputMode,
        isProcessing,
        realtime,
        sourcetable,
        test: {
          status: testStatus,
          error: testError,
          atMs: testStatus === "running" ? testStartedAt : testAt,
          result: streamTest,
        },
        snapshot,
        snapshotAtMs,
        connections,
        satelliteStatus,
        nowMs,
        // Failures are kept in the store (streamTestError); callers need not catch.
        runStreamTest: async () => {
          try {
            await runStreamTest();
          } catch {
            /* surfaced via model.test */
          }
        },
        resetCounters: resetStreamCounters,
      }),
    [inputMode, isProcessing, realtime, sourcetable, streamTest, testStatus, testError, testAt, testStartedAt, snapshot, snapshotAtMs, connections, satelliteStatus, nowMs, resetStreamCounters],
  );
}
