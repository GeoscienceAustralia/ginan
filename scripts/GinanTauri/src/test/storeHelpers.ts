/**
 * Fixtures and store reset for the app-store-backed tests.
 *
 * `useAppStore` is a module singleton, so a test that mutates it would leak
 * into the next one. The snapshot is taken at import time — before any test has
 * run — and `resetAppStore` replaces the whole state object with it, actions
 * included (zustand's action closures are stable, so restoring them wholesale
 * is safe).
 */
import { useAppStore } from "../store/appStore";
import { phaseCodes } from "../config/obsCodes";
import type {
  ConstellationCodeConfig,
  ConstellationObsCodes,
  PppCatalogue,
  PppCatalogueEntry,
  RinexData,
  StreamTestResult,
} from "../types";

const INITIAL_STATE = { ...useAppStore.getState() };

export function resetAppStore() {
  useAppStore.setState({ ...INITIAL_STATE }, true);
}

/** Convenience alias — reads better inside a test body. */
export const store = () => useAppStore.getState();

/* ── fixtures ───────────────────────────────────────────────────────────── */

/**
 * What a RINEX 3 header lists: a code, phase and Doppler observable per signal.
 * Only the L entries survive into the constellation lists, so the phase codes
 * here are GPS L1C/L2W/L5Q and Galileo L1X/L5X.
 */
export function makeObsCodes(
  overrides: Partial<ConstellationObsCodes> = {},
): ConstellationObsCodes {
  return {
    gps: ["C1C", "L1C", "D1C", "C2W", "L2W", "L5Q"],
    gal: ["C1X", "L1X", "L5X"],
    glo: [],
    bds: [],
    qzs: [],
    ...overrides,
  };
}

export function makeRinexData(overrides: Partial<RinexData> = {}): RinexData {
  return {
    rinex_version: 3.04,
    marker_name: "ALIC00AUS",
    receiver_type: "SEPT POLARX5 ",
    antenna_type: "LEIAR25.R3      NONE",
    antenna_offset: [0.0015, 0, 0],
    approx_position: [-4052052.7, 4212836.0, -2545105.0],
    start_epoch: "2024-01-02 00:00:00",
    end_epoch: "2024-01-02 23:59:30",
    epoch_interval: 30,
    constellations: ["GPS", "GAL"],
    observation_codes: makeObsCodes(),
    ...overrides,
  };
}

/**
 * All phase codes enabled and valid, which is the state `initializeConstellationCodes`
 * produces: the header's C/D/S observables are not offered.
 */
export function makeConstellationCodes(
  codes: ConstellationObsCodes = makeObsCodes(),
): ConstellationCodeConfig {
  const map = (list: string[]) =>
    phaseCodes(list).map((code) => ({ code, enabled: true, valid: true }));
  return {
    gps: map(codes.gps),
    gal: map(codes.gal),
    glo: map(codes.glo),
    bds: map(codes.bds),
    qzs: map(codes.qzs),
  };
}

export function makeCatalogueEntry(
  overrides: Partial<PppCatalogueEntry> = {},
): PppCatalogueEntry {
  return {
    provider: "IGS",
    series: "FIN",
    project: "OPS",
    has_sp3: true,
    has_clk: true,
    has_bia: true,
    constellations: ["GPS", "GAL"],
    files: {},
    ...overrides,
  };
}

export function makeCatalogue(
  entries: Partial<PppCatalogueEntry>[] = [{}],
): PppCatalogue {
  return { entries: entries.map(makeCatalogueEntry) };
}

/** What `test_streams` returns for one healthy GPS+Galileo observation stream. */
export function makeStreamTestResult(
  overrides: Partial<StreamTestResult> = {},
): StreamTestResult {
  return {
    constellations: ["GPS", "GAL"],
    observation_codes: makeObsCodes(),
    interval_s: 1.02,
    streams: [{ mountpoint: "ALIC00AUS0", records: 60, satellites: 18, interval_s: 1.02 }],
    snapshot: {
      total_by_type: { "1077": 30, "1097": 30 },
      malformed_lines: 0,
      unknown_records: 0,
      msm: {},
      ssr: {},
      brdc_ephemeris: {},
    },
    ...overrides,
  };
}

/** Resolve after the queued rAF/microtask flushes the buffered event batches. */
export function flushFrame(): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, 0));
}
