import { describe, expect, it } from "vitest";
import { buildStreamsModel, type StreamsInputs } from "./useStreamsModel";
import type { LiveSbas, LiveSnapshot, NtripConnection } from "../types/streams";
import type { SatelliteStatus, SbasProbe, StreamTestResult } from "../types";

const conn = (o: Partial<NtripConnection> = {}): NtripConnection => ({
  mount: "ALIC00AUS0", state: "streaming", retryCount: 0, lastHttpStatus: 200, lastError: null,
  connectedSince: 0, lastEventAt: 9_000, chunksReceived: null, ...o,
});

const snap = (o: Partial<LiveSnapshot> = {}): LiveSnapshot => ({
  total_by_type: { MSM: 900, ssrOrbit: 50, brdcEph: 10 },
  window_by_type: { MSM: 600, ssrOrbit: 30, brdcEph: 0 },
  window_seconds: 60,
  runtime_seconds: 100,
  malformed_lines: 2,
  unknown_records: 3,
  msm: { ALIC00AUS0: { records: 900, invalid_records: 4, avg_snr: 44, last_epoch: null, satellite_count: 2, constellations: {}, signals_by_constellation: {} } },
  ssr: {},
  brdc_ephemeris: {},
  snapshot_gpst_ms: 1_000_000,
  streams: {
    ALIC00AUS0: {
      role_hint: "OBS", families: {}, window_families: {}, records: 900, window_records: 600, epochs_per_s: 1,
      last_rx_ms: 999_000, sats_by_constellation: { GPS: 2 },
      messages: {
        "1077": { name: "MSM7_GPS", instances: 75, records: 800, last_rx_ms: 999_000, periodicity_s: 1, inter_arrival_s: [1], nominal_udi_s: null, last_sats: ["G05"], last_signals: ["1C"] },
        "1019": { name: "x", instances: 25, records: 25, last_rx_ms: 990_000, periodicity_s: 5, inter_arrival_s: [], nominal_udi_s: null, last_sats: [], last_signals: [] },
      },
      satellites: {
        G05: { constellation: "GPS", cn0_avg: 45, signals: ["1C"], last_rx_ms: 999_000 },
        G07: { constellation: "GPS", cn0_avg: 40, signals: ["1C"], last_rx_ms: 999_000 },
      },
    },
  },
  throughput: { ALIC00AUS0: { bytes_total: 5000, bytes_per_s: 100, history: [[1, 90], [2, 100]] } },
  reset_at_ms: null,
  ...o,
});

const trace: SatelliteStatus = {
  path: null, name: "x", available: true, message: null, receivers: [], epochs: 10, satellite_count: 2, t_start: 0, t_end: 100,
  constellations: [{ constellation: "GPS", satellites: [
    { sat: "G05", constellation: "GPS", signals: [], receivers: [], samples: 9, first_ms: 0, last_ms: 100, postfit_rms: 0.01 },
    { sat: "G07", constellation: "GPS", signals: [], receivers: [], samples: 3, first_ms: 0, last_ms: 50, postfit_rms: 0.02 },
  ] }],
};

const base = (o: Partial<StreamsInputs> = {}): StreamsInputs => ({
  inputMode: "realtime", isProcessing: true,
  realtime: { caster_url: "https://ntrip.example.org/", obs_streams: ["ALIC00AUS0"], nav_streams: ["SSRA00CNE0"], record_decoded: true, record_rtcm: true },
  sourcetable: [
    { mountpoint: "ALIC00AUS0", identifier: "Alice Springs", format: "RTCM 3.3", format_details: "1077(1)", carrier: "2", nav_system: "GPS", network: "x", country: "AUS", category: "obs" },
    { mountpoint: "SSRA00CNE0", identifier: "CNES", format: "RTCM 3", format_details: "", carrier: "0", nav_system: "", network: "x", country: "FRA", category: "ssr" },
  ],
  test: { status: "idle", error: null, atMs: null, result: null },
  snapshot: null, snapshotAtMs: null, connections: {}, satelliteStatus: null, nowMs: 10_000,
  runStreamTest: async () => {}, resetCounters: async () => {},
  ...o,
});

describe("buildStreamsModel phases", () => {
  it.each([
    ["file-mode", { inputMode: "file" as const }],
    ["idle", { isProcessing: false }],
    ["test-running", { test: { status: "running" as const, error: null, atMs: null, result: null } }],
    ["test-failed", { isProcessing: false, test: { status: "failed" as const, error: "boom", atMs: 1, result: null } }],
    ["waiting", {}],
    ["live", { snapshot: snap(), snapshotAtMs: 10_000 }],
    ["stopped", { isProcessing: false, snapshot: snap(), snapshotAtMs: 10_000 }],
  ])("%s", (phase, o) => {
    expect(buildStreamsModel(base(o)).phase).toBe(phase);
  });
});

describe("buildStreamsModel streams", () => {
  const m = buildStreamsModel(base({ snapshot: snap(), snapshotAtMs: 10_000, connections: { ALIC00AUS0: conn() } }));
  const alic = m.streams.find((s) => s.mount === "ALIC00AUS0")!;
  const ssr = m.streams.find((s) => s.mount === "SSRA00CNE0")!;

  it("metadata and roles", () => {
    expect(alic).toMatchObject({ role: "OBS", casterHost: "ntrip.example.org:443", protocol: "RTCM 3.3", formatDetails: "1077(1)", location: "Alice Springs", status: "streaming" });
    expect(ssr.role).toBe("CORR");
    expect(ssr.status).toBe("connecting");
    expect(m.casterHosts).toEqual(["ntrip.example.org:443"]);
  });

  it("messages: share, late measured on GPST, sorted", () => {
    expect(alic.messages.map((x) => x.msgNo)).toEqual(["1019", "1077"]);
    const eph = alic.messages[0];
    const msm = alic.messages[1];
    expect(msm.share).toBeCloseTo(0.75);
    expect(msm.ageS).toBe(1);
    expect(msm.late).toBe(false);
    expect(eph.ageS).toBe(10);
    expect(eph.late).toBe(true); // 10 s > 1.5 * 5 s
    expect(eph.group).toBe("ephemeris");
  });

  it("throughput, kpis and mix", () => {
    expect(alic.throughputHistory).toEqual([90, 100]);
    expect(alic.satCount).toBe(2);
    expect(alic.invalid).toBe(4);
    expect(m.kpis).toMatchObject({ bytesTotal: 5000, bytesPerSec: 100, recordsPerSec: 10.5, malformed: 2, unknown: 3, satsInSolution: null });
    expect(m.mix.map((x) => x.family)).toEqual(["msm", "ssr", "ephemeris"]);
    expect(m.mix[0].ratePerS).toBe(10);
  });

  it("ticks GPST forward with local elapsed time while running", () => {
    const later = buildStreamsModel(base({ snapshot: snap(), snapshotAtMs: 10_000, nowMs: 14_000 }));
    expect(later.streams[0].messages[1].ageS).toBe(5);
  });
});

describe("status derivation", () => {
  const mk = (o: Partial<StreamsInputs>) => buildStreamsModel(base({ snapshot: snap(), snapshotAtMs: 10_000, ...o })).streams[0].status;
  it("receiving without NTRIP lines, idle when stopped", () => {
    expect(mk({})).toBe("receiving");
    expect(mk({ isProcessing: false })).toBe("idle");
  });
  it("stale when decoded data older than 3x periodicity", () => {
    const s = snap();
    s.streams!.ALIC00AUS0.last_rx_ms = 990_000; // 10 s old vs 1 s typical
    expect(mk({ snapshot: s, connections: { ALIC00AUS0: conn() } })).toBe("stale");
  });
  it("passes NTRIP failure states through with retry info", () => {
    const m = buildStreamsModel(base({ connections: { ALIC00AUS0: conn({ state: "reconnecting", retryCount: 3, lastError: "reset" }) } }));
    expect(m.streams[0]).toMatchObject({ status: "reconnecting", retryCount: 3, lastError: "reset", lastEventAgeMs: 1000 });
    expect(buildStreamsModel(base({ connections: { ALIC00AUS0: conn({ state: "failed", lastHttpStatus: 401 }) } })).streams[0]).toMatchObject({ status: "failed", httpStatus: 401 });
  });
});

describe("satellitesFor", () => {
  it("joins received satellites with the trace", () => {
    const m = buildStreamsModel(base({ snapshot: snap(), snapshotAtMs: 10_000, satelliteStatus: trace }));
    const [g5, g7] = m.satellitesFor("ALIC00AUS0");
    expect(g5).toMatchObject({ sat: "G05", cn0: 45, inSolution: true, postfitRms: 0.01, samples: 9, dropped: false });
    expect(g7).toMatchObject({ sat: "G07", inSolution: false, dropped: true });
    expect(m.kpis).toMatchObject({ satsInSolution: 1, satsSeen: 2 });
  });
  it("leaves solution fields null without a trace", () => {
    const m = buildStreamsModel(base({ snapshot: snap(), snapshotAtMs: 10_000 }));
    expect(m.satellitesFor("ALIC00AUS0")[0]).toMatchObject({ inSolution: null, postfitRms: null });
  });
});

const testResult = (): StreamTestResult => ({
  constellations: ["GPS"], observation_codes: { gps: [], gal: [], glo: [], bds: [], qzs: [] }, interval_s: 1, streams: [],
  snapshot: { total_by_type: {}, malformed_lines: 0, unknown_records: 0, msm: {}, ssr: {}, brdc_ephemeris: {} },
});

describe("buildStreamsModel SBAS", () => {
  const probe: SbasProbe = {
    host: "sisnet.data.gnss.ga.gov.au", prn: 122, mode: "PVS", state: "ok", error: null, seconds: 10, messages_per_s: 1,
    messages: 10, crc_errors: 0, invalid_lines: 0, by_type: { "32": 6, "34": 4 }, first_gpst_ms: 0, last_gpst_ms: 9000,
  };
  const live: LiveSbas = {
    mode: "PVS",
    console: { auth_failures: 0, start_failures: 0, invalid_lines: 0, crc_errors: 1, messages: 0, last_error: "A message failed its CRC check", last_error_at_ms: 5000 },
    levels: null,
  };
  const withSbas = (o: Partial<StreamsInputs> = {}) =>
    base({ realtime: { ...base().realtime, sbas: { mode: "PVS" } }, ...o });

  it("has no SBAS card without SBAS, or in a file run", () => {
    expect(buildStreamsModel(base()).sbas).toBeNull();
    expect(buildStreamsModel(withSbas({ inputMode: "file" })).sbas).toBeNull();
  });

  it("takes the stream test's SISNet check and the run's sbas block", () => {
    const result = { ...testResult(), sbas: probe };
    const m = buildStreamsModel(
      withSbas({ test: { status: "passed", error: null, atMs: 1, result }, snapshot: snap({ sbas: live }), snapshotAtMs: 10_000 }),
    );
    expect(m.sbas).toEqual({ mode: "PVS", probe, live });
  });
});
