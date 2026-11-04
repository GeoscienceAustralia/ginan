import { describe, expect, it } from "vitest";
import {
  biaPhaseSignals,
  isPhaseCode,
  phaseCodes,
  phaseConstellationCodes,
} from "./obsCodes";

describe("isPhaseCode", () => {
  it("accepts RINEX 3 phase observables", () => {
    for (const code of ["L1C", "L1W", "L2L", "L5Q", "L6X", "L7I", "L8Q"]) {
      expect(isPhaseCode(code)).toBe(true);
    }
  });

  it("rejects code, Doppler and strength observables of the same signals", () => {
    for (const code of ["C1C", "D1C", "S1C", "C5Q", "P1", "X1C"]) {
      expect(isPhaseCode(code)).toBe(false);
    }
  });

  it("rejects strings that only look like one", () => {
    for (const code of ["", "L", "L1", "L1CC", "LC1", "l1c", "L1 ", " L1C", "L!C"]) {
      expect(isPhaseCode(code)).toBe(false);
    }
  });

  it("rejects values that are not strings", () => {
    for (const code of [undefined, null, 1, {}, ["L1C"]]) {
      expect(isPhaseCode(code)).toBe(false);
    }
  });
});

describe("phaseCodes", () => {
  it("keeps phase codes in their original order", () => {
    expect(phaseCodes(["C1C", "L2W", "D1C", "L1C", "S1C"])).toEqual(["L2W", "L1C"]);
  });

  it("treats a missing list as empty", () => {
    expect(phaseCodes(undefined)).toEqual([]);
    expect(phaseCodes(null)).toEqual([]);
  });
});

describe("biaPhaseSignals", () => {
  it("covers a signal by its phase bias", () => {
    expect([...biaPhaseSignals(["L1C", "L2W"])].sort()).toEqual(["L1C", "L2W"]);
  });

  it("covers a signal by its code bias alone, as a code-only table gives for GLONASS", () => {
    expect([...biaPhaseSignals(["C1C", "C1P", "C2C", "C2P"])].sort()).toEqual([
      "L1C",
      "L1P",
      "L2C",
      "L2P",
    ]);
  });

  it("does not list a signal twice when the table has both biases", () => {
    expect([...biaPhaseSignals(["C1C", "L1C", "C5Q"])].sort()).toEqual(["L1C", "L5Q"]);
  });

  it("ignores anything that is not a code or phase bias", () => {
    expect(biaPhaseSignals(["D1C", "S1C", "X1C", "C1", "L1CC", "", "G01"]).size).toBe(0);
  });

  it("treats a missing list as empty", () => {
    expect(biaPhaseSignals(undefined).size).toBe(0);
    expect(biaPhaseSignals(null).size).toBe(0);
  });
});

describe("phaseConstellationCodes", () => {
  const item = (code: string, enabled = true, valid = true) => ({ code, enabled, valid });

  it("keeps each surviving entry's enabled and valid state and the user's order", () => {
    const out = phaseConstellationCodes({
      gps: [item("L2W", false, true), item("C1C"), item("L1C", true, false)],
      gal: [],
      glo: [],
      bds: [],
      qzs: [],
    });
    expect(out.gps).toEqual([item("L2W", false, true), item("L1C", true, false)]);
  });

  it("fills in constellations a stored selection does not have", () => {
    expect(phaseConstellationCodes({ gps: [item("L1C")] })).toEqual({
      gps: [item("L1C")],
      gal: [],
      glo: [],
      bds: [],
      qzs: [],
    });
  });

  it("returns an empty selection for nothing at all", () => {
    const empty = { gps: [], gal: [], glo: [], bds: [], qzs: [] };
    expect(phaseConstellationCodes(null)).toEqual(empty);
    expect(phaseConstellationCodes(undefined)).toEqual(empty);
  });

  it("skips malformed entries and non-array lists instead of throwing", () => {
    const stored = {
      gps: [null, undefined, "L1C", { enabled: true }, { code: 5 }, item("L5Q")],
      gal: "L1C",
      glo: { 0: item("L1P") },
    } as unknown as Parameters<typeof phaseConstellationCodes>[0];
    expect(phaseConstellationCodes(stored)).toEqual({
      gps: [item("L5Q")],
      gal: [],
      glo: [],
      bds: [],
      qzs: [],
    });
  });
});
