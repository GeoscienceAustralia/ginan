/**
 * Sample StreamsModel numbers (now = 14:16:43 GPST, uptime 00:14:32), used by
 * the Streams UI tests.
 */
import type {
  MessageView,
  SatelliteView,
  StreamView,
  StreamsModel,
} from "../../../types/streams";
import type { StreamTestResult } from "../../../types";

const noop = async () => {};

function msg(msgNo: string, name: string, group: MessageView["group"], constellation: string | null, instances: number, total: number, periodicityS: number | null, ageS: number, late = false): MessageView {
  return {
    msgNo, name, description: name, group, constellation, instances,
    share: instances / total, lastRxMs: 0, ageS, periodicityS, late,
    interArrivalS: [], lastSats: [], lastSignals: [],
  };
}

const alicMsgs = [
  msg("1077", "GPS MSM7", "msm", "GPS", 872, 3826, 1, 0.4),
  msg("1097", "Galileo MSM7", "msm", "GAL", 872, 3826, 1, 0.4),
  msg("1087", "GLONASS MSM7", "msm", "GLO", 872, 3826, 1, 0.4),
  msg("1127", "BeiDou MSM7", "msm", "BDS", 872, 3826, 1, 0.4),
  msg("1006", "Station position + height", "station", null, 87, 3826, 10, 3.1),
  msg("1033", "Receiver + antenna descriptors", "station", null, 87, 3826, 10, 3.1),
  msg("1230", "GLONASS code-phase biases", "bias", "GLO", 87, 3826, 10, 3.1),
  msg("1019", "GPS ephemeris", "ephemeris", "GPS", 28, 3826, 30, 80, true),
  msg("1046", "Galileo F/NAV ephemeris", "ephemeris", "GAL", 28, 3826, 30, 12),
  msg("1042", "BeiDou ephemeris", "ephemeris", "BDS", 28, 3826, 30, 80, true),
];

const alic: StreamView = {
  mount: "ALIC00AUS0", role: "OBS", status: "streaming", retryCount: 0, lastError: null, httpStatus: 200,
  casterHost: "ntrip.data.gnss.ga.gov.au:443", protocol: "RTCM 3.3 · MSM7", formatDetails: null, location: "Alice Springs, NT",
  bytesTotal: 1_400_000, bytesPerSec: 1600, throughputHistory: wave(1), lastEventAgeMs: 400,
  records: 3826, windowRecords: 240, epochsPerSec: 1.0, avgSnr: 42.8, invalid: 0,
  satsByConstellation: { GPS: 12, GAL: 9, GLO: 7, BDS: 6 }, satCount: 34, messages: alicMsgs,
};

const ssra: StreamView = {
  mount: "SSRA00CNE0", role: "CORR", status: "streaming", retryCount: 0, lastError: null, httpStatus: 200,
  casterHost: "ntrip.data.gnss.ga.gov.au:443", protocol: "RTCM 3 SSR · CNES real-time", formatDetails: null, location: null,
  bytesTotal: 960_000, bytesPerSec: 1100, throughputHistory: wave(3), lastEventAgeMs: 900,
  records: 4300, windowRecords: 300, epochsPerSec: null, avgSnr: null, invalid: 0,
  satsByConstellation: { GPS: 20, GAL: 12, GLO: 12, BDS: 8 }, satCount: 52,
  messages: [
    msg("1060", "GPS combined orbit + clock", "ssr", "GPS", 900, 4300, 5, 0.9),
    msg("1066", "GLONASS combined orbit + clock", "ssr", "GLO", 900, 4300, 5, 0.9),
    msg("1240", "Galileo combined orbit + clock", "ssr", "GAL", 900, 4300, 5, 0.9),
    msg("1258", "BeiDou combined orbit + clock", "ssr", "BDS", 900, 4300, 5, 0.9),
    msg("1059", "GPS code biases", "ssr", "GPS", 350, 4300, 10, 4),
    msg("1265", "GPS phase biases", "ssr", "GPS", 350, 4300, 10, 4),
  ],
};

const bcep: StreamView = {
  mount: "BCEP00BKG0", role: "NAV", status: "reconnecting", retryCount: 3, lastError: "reset by peer", httpStatus: 200,
  casterHost: "ntrip.data.gnss.ga.gov.au:443", protocol: "RTCM 3 · broadcast ephemeris", formatDetails: null, location: null,
  bytesTotal: 210_000, bytesPerSec: 0, throughputHistory: wave(5, true), lastEventAgeMs: 48_200,
  records: 400, windowRecords: 0, epochsPerSec: null, avgSnr: null, invalid: 0,
  satsByConstellation: { GPS: 31, GAL: 24, GLO: 22, BDS: 28 }, satCount: 105,
  messages: [
    msg("1019", "GPS ephemeris", "ephemeris", "GPS", 100, 400, 30, 48),
    msg("1045", "Galileo F/NAV ephemeris", "ephemeris", "GAL", 100, 400, 30, 48),
    msg("1020", "GLONASS ephemeris", "ephemeris", "GLO", 100, 400, 30, 48),
    msg("1042", "BeiDou ephemeris", "ephemeris", "BDS", 100, 400, 30, 48),
  ],
};

function wave(seed: number, drop = false): number[] {
  return Array.from({ length: 61 }, (_, i) => {
    const v = 0.55 + 0.25 * Math.sin(i * 0.37 + seed) + 0.12 * Math.sin(i * 1.7 + seed * 2);
    return drop && i > 50 ? 0 : Math.round(v * 2000);
  });
}

const satellites: SatelliteView[] = [
  { sat: "G05", constellation: "GPS", signals: ["1C", "2W"], cn0: 44.1, inSolution: true, postfitRms: 0.012, samples: 800, lastEpochMs: 0, dropped: false },
  { sat: "E11", constellation: "GAL", signals: ["1X", "5Q"], cn0: 45.0, inSolution: true, postfitRms: 0.009, samples: 790, lastEpochMs: 0, dropped: false },
];

const testResult: StreamTestResult = {
  constellations: ["GPS", "GAL", "GLO", "BDS"],
  observation_codes: { gps: [], gal: [], glo: [], bds: [], qzs: [] },
  interval_s: 1,
  streams: [{ mountpoint: "ALIC00AUS0", records: 40, satellites: 34, interval_s: 1 }],
  snapshot: { total_by_type: {}, malformed_lines: 0, unknown_records: 0, msm: {}, ssr: {}, brdc_ephemeris: {} },
};

export const sampleModel: StreamsModel = {
  phase: "live",
  sbas: null,
  isProcessing: true,
  recordDecodedOff: false,
  runtimeS: 872,
  resetAtMs: null,
  casterHosts: ["ntrip.data.gnss.ga.gov.au:443"],
  kpis: { bytesTotal: 2_600_000, bytesPerSec: 2700, recordsPerSec: 9.6, satsInSolution: 31, satsSeen: 34, malformed: 0, unknown: 0 },
  mix: [
    { family: "ssr", label: "SSR corrections", count: 300, ratePerS: 5.0 },
    { family: "msm", label: "Observations (MSM)", count: 240, ratePerS: 4.0 },
    { family: "station", label: "Station + biases", count: 18, ratePerS: 0.3 },
    { family: "ephemeris", label: "Broadcast ephemeris", count: 16, ratePerS: 0.27 },
  ],
  streams: [alic, ssra, bcep],
  test: { status: "passed", error: null, atMs: Date.UTC(2026, 9, 15, 14, 2, 11), result: testResult },
  satelliteStatus: null,
  satellitesFor: () => satellites,
  runStreamTest: noop,
  resetCounters: noop,
};

const placeholder = (s: StreamView, status: StreamView["status"]): StreamView => ({
  ...s, status, bytesTotal: null, bytesPerSec: null, throughputHistory: [], lastEventAgeMs: null,
  records: 0, windowRecords: 0, epochsPerSec: null, avgSnr: null, satsByConstellation: {}, satCount: 0, messages: [],
});

export const fileModel: StreamsModel = { ...sampleModel, phase: "file-mode", isProcessing: false, streams: [], mix: [], casterHosts: [], runtimeS: null, test: { status: "idle", error: null, atMs: null, result: null } };
export const idleModel: StreamsModel = { ...fileModel, phase: "idle" };
export const waitingModel: StreamsModel = {
  ...sampleModel, phase: "waiting", runtimeS: 6, mix: [],
  kpis: { bytesTotal: null, bytesPerSec: null, recordsPerSec: null, satsInSolution: null, satsSeen: null, malformed: 0, unknown: 0 },
  streams: [placeholder(alic, "streaming"), placeholder(ssra, "connecting"), placeholder(bcep, "connecting")],
};
export const waitingDecodedOffModel: StreamsModel = { ...waitingModel, recordDecodedOff: true };
export const testRunningModel: StreamsModel = {
  ...idleModel, phase: "test-running", isProcessing: true,
  streams: [placeholder(alic, "streaming"), placeholder(ssra, "streaming"), placeholder(bcep, "connecting")],
  test: { status: "running", error: null, atMs: Date.UTC(2026, 9, 15, 14, 2, 11), result: null },
};
export const testFailedModel: StreamsModel = {
  ...idleModel, phase: "test-failed",
  streams: [alic, { ...ssra, status: "failed", httpStatus: 401, lastError: "401 Unauthorized" }, { ...bcep, status: "failed", lastError: "No data" }],
  test: { status: "failed", error: "2 of 3 mountpoints didn't deliver data.", atMs: Date.UTC(2026, 9, 15, 14, 2, 11), result: testResult },
};
export const stoppedModel: StreamsModel = {
  ...sampleModel, phase: "stopped", isProcessing: false,
  streams: sampleModel.streams.map((s) => ({ ...s, status: "idle" as const })),
};
