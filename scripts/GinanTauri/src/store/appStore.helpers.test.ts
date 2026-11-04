import { describe, expect, it } from "vitest";
import {
  DEFAULT_CASTER_URL,
  DEFAULT_REALTIME_INPUTS,
  DEFAULT_STORAGE_CAP_MB,
  REALTIME_DEFAULT_CONSTELLATIONS,
  SIDEBAR_DEFAULT_WIDTH,
  SIDEBAR_MAX_WIDTH,
  SIDEBAR_MIN_WIDTH,
  clipMarkerForVersion,
  datetimeLocalToEpoch,
  effectiveCasterUrl,
  epochToDatetimeLocal,
  getConstellationObsCodesForConfig,
  getEffectiveConstellations,
  getEnabledCodesInOrder,
  getRealtimeConstellations,
} from "./appStore";
import type { Constellation } from "../types";
import { makeConstellationCodes, makeObsCodes } from "../test/storeHelpers";

describe("epochToDatetimeLocal", () => {
  it("swaps the space for a T and keeps the seconds", () => {
    expect(epochToDatetimeLocal("2024-01-02 03:04:05")).toBe("2024-01-02T03:04:05");
  });

  it("truncates anything past the seconds field", () => {
    expect(epochToDatetimeLocal("2024-01-02 03:04:05.500")).toBe("2024-01-02T03:04:05");
  });

  it("maps a missing epoch to the empty input value", () => {
    expect(epochToDatetimeLocal(null)).toBe("");
    expect(epochToDatetimeLocal("")).toBe("");
  });
});

describe("datetimeLocalToEpoch", () => {
  it("swaps the T back for a space", () => {
    expect(datetimeLocalToEpoch("2024-01-02T03:04:05")).toBe("2024-01-02 03:04:05");
  });

  it("pads seconds the browser omitted", () => {
    expect(datetimeLocalToEpoch("2024-01-02T03:04")).toBe("2024-01-02 03:04:00");
  });

  it("maps an empty input to an empty epoch", () => {
    expect(datetimeLocalToEpoch("")).toBe("");
  });

  it("round-trips with epochToDatetimeLocal", () => {
    const epoch = "2024-01-02 23:59:30";
    expect(datetimeLocalToEpoch(epochToDatetimeLocal(epoch))).toBe(epoch);
  });
});

describe("clipMarkerForVersion", () => {
  it("clips a RINEX 3 long marker to the canonical four characters", () => {
    expect(clipMarkerForVersion("ALIC00AUS", 3.04)).toBe("ALIC");
    expect(clipMarkerForVersion("ALIC00AUS", 4.0)).toBe("ALIC");
  });

  it("leaves a RINEX 2 marker alone", () => {
    expect(clipMarkerForVersion("ALIC", 2.11)).toBe("ALIC");
    expect(clipMarkerForVersion("LONGNAME", 2.11)).toBe("LONGNAME");
  });

  it("leaves the marker alone when the version is unknown", () => {
    expect(clipMarkerForVersion("ALIC00AUS", null)).toBe("ALIC00AUS");
    expect(clipMarkerForVersion("ALIC00AUS", undefined)).toBe("ALIC00AUS");
  });

  it("does not pad a marker already at or under four characters", () => {
    expect(clipMarkerForVersion("AB", 3.04)).toBe("AB");
    expect(clipMarkerForVersion("", 3.04)).toBe("");
  });
});

describe("effectiveCasterUrl", () => {
  const url = "https://caster.example.com/";

  const rt = (obs: string[], nav: string[]) => ({ obs_streams: obs, nav_streams: nav });

  it("passes the root through while any stream is a mountpoint", () => {
    expect(effectiveCasterUrl(url, rt(["ALIC00AUS0"], ["SSRA00CNE0"]))).toBe(url);
    expect(effectiveCasterUrl(url, rt(["ALIC00AUS0"], ["serial:///dev/ttyUSB1"]))).toBe(url);
    expect(effectiveCasterUrl(url, rt(["serial:///dev/ttyUSB0", "ALIC00AUS0"], []))).toBe(url);
  });

  it("blanks the root when every stream is a serial device", () => {
    // A stray root would be concatenated onto entries that already have a scheme.
    expect(effectiveCasterUrl(url, rt(["serial:///dev/ttyUSB0"], ["serial:///dev/ttyUSB1"]))).toBe("");
  });
});

describe("getEnabledCodesInOrder", () => {
  it("keeps only enabled codes, in list order", () => {
    const codes = [
      { code: "C1C", enabled: true, valid: true },
      { code: "L1C", enabled: false, valid: true },
      { code: "C2W", enabled: true, valid: false },
    ];
    expect(getEnabledCodesInOrder(codes)).toEqual(["C1C", "C2W"]);
  });

  it("ignores validity — a code the BIA lacks is still the user's choice", () => {
    expect(
      getEnabledCodesInOrder([{ code: "C5X", enabled: true, valid: false }]),
    ).toEqual(["C5X"]);
  });

  it("returns nothing for an empty list", () => {
    expect(getEnabledCodesInOrder([])).toEqual([]);
  });
});

describe("getConstellationObsCodesForConfig", () => {
  const codes = makeConstellationCodes(makeObsCodes({ glo: ["C1C", "L1C"] }));

  it("serialises the raw selection when no allow-list is given", () => {
    expect(getConstellationObsCodesForConfig(codes)).toEqual({
      gps: ["L1C", "L2W", "L5Q"],
      gal: ["L1X", "L5X"],
      glo: ["L1C"],
      bds: [],
      qzs: [],
    });
  });

  it("empties constellations the selected product cannot bias-correct", () => {
    const allowed = new Set<Constellation>(["GPS"]);
    expect(getConstellationObsCodesForConfig(codes, allowed)).toEqual({
      gps: ["L1C", "L2W", "L5Q"],
      gal: [],
      glo: [],
      bds: [],
      qzs: [],
    });
  });

  it("drops disabled codes", () => {
    const partial = makeConstellationCodes();
    partial.gps[1].enabled = false;
    expect(getConstellationObsCodesForConfig(partial).gps).toEqual(["L1C", "L5Q"]);
  });

  it("keeps the user's priority order", () => {
    const reordered = makeConstellationCodes();
    reordered.gps.reverse();
    expect(getConstellationObsCodesForConfig(reordered).gps).toEqual(["L5Q", "L2W", "L1C"]);
  });

  it("writes only phase codes even if the store holds code, Doppler or strength entries", () => {
    // A selection an earlier version of the app persisted: every observable the header listed.
    const legacy = makeConstellationCodes();
    legacy.gps = ["C1C", "L1C", "D1C", "S1C", "C2W", "L2W"].map((code) => ({
      code,
      enabled: true,
      valid: true,
    }));
    expect(getConstellationObsCodesForConfig(legacy).gps).toEqual(["L1C", "L2W"]);
  });
});

describe("getEffectiveConstellations", () => {
  it("returns systems both enabled and publishable, in canonical order", () => {
    const codes = makeConstellationCodes(makeObsCodes({ glo: ["C1C", "L1C"] }));
    const allowed = new Set<Constellation>(["GLO", "GPS", "GAL"]);
    expect(getEffectiveConstellations(codes, allowed)).toEqual(["GPS", "GAL", "GLO"]);
  });

  it("excludes a system with every code switched off", () => {
    const codes = makeConstellationCodes();
    codes.gal.forEach((c) => (c.enabled = false));
    const allowed = new Set<Constellation>(["GPS", "GAL"]);
    expect(getEffectiveConstellations(codes, allowed)).toEqual(["GPS"]);
  });

  it("excludes a system the product does not publish", () => {
    const codes = makeConstellationCodes();
    expect(getEffectiveConstellations(codes, new Set<Constellation>(["GAL"]))).toEqual([
      "GAL",
    ]);
  });

  it("returns nothing when nothing is allowed", () => {
    expect(getEffectiveConstellations(makeConstellationCodes(), new Set())).toEqual([]);
  });
});

describe("getRealtimeConstellations", () => {
  const empty = makeConstellationCodes({ gps: [], gal: [], glo: [], bds: [], qzs: [] });
  const allAllowed = new Set<Constellation>(["GPS", "GAL", "GLO", "BDS", "QZS"]);

  it("falls back to the realtime defaults when no RINEX seeded a selection", () => {
    expect(getRealtimeConstellations(empty, allAllowed)).toEqual(
      REALTIME_DEFAULT_CONSTELLATIONS,
    );
  });

  it("narrows the fallback to what the product publishes", () => {
    expect(getRealtimeConstellations(empty, new Set<Constellation>(["GPS", "BDS"]))).toEqual([
      "GPS",
    ]);
  });

  it("prefers an explicit selection over the defaults", () => {
    const codes = makeConstellationCodes(makeObsCodes({ gps: [], gal: ["C1X", "L1X"] }));
    expect(getRealtimeConstellations(codes, allAllowed)).toEqual(["GAL"]);
  });

  it("returns nothing when the product publishes none of the defaults", () => {
    expect(getRealtimeConstellations(empty, new Set<Constellation>(["QZS"]))).toEqual([]);
  });
});

describe("shipped defaults", () => {
  it("uses GA's public caster in both the realtime block and the settings default", () => {
    expect(DEFAULT_REALTIME_INPUTS.caster_url).toBe(DEFAULT_CASTER_URL);
  });

  it("ships streams that the default caster actually publishes", () => {
    expect(DEFAULT_REALTIME_INPUTS.obs_streams.length).toBeGreaterThan(0);
    expect(DEFAULT_REALTIME_INPUTS.nav_streams).toEqual(["BCEP00BKG0", "SSRA00CNE0"]);
  });

  it("records raw and decoded RTCM (the Streams tab's live stats need the decoded log), not RINEX", () => {
    expect(DEFAULT_REALTIME_INPUTS.record_rtcm).toBe(true);
    expect(DEFAULT_REALTIME_INPUTS.record_decoded).toBe(true);
    expect(DEFAULT_REALTIME_INPUTS.record_rinex).toBe(false);
  });

  it("tolerates a slow caster rather than PEA's tight epoch default", () => {
    expect(DEFAULT_REALTIME_INPUTS.wait_next_epoch).toBe(10);
  });

  it("matches the backend's 500 MB storage cap", () => {
    expect(DEFAULT_STORAGE_CAP_MB).toBe(500);
  });

  it("keeps the sidebar default width inside its own bounds", () => {
    expect(SIDEBAR_DEFAULT_WIDTH).toBeGreaterThanOrEqual(SIDEBAR_MIN_WIDTH);
    expect(SIDEBAR_DEFAULT_WIDTH).toBeLessThanOrEqual(SIDEBAR_MAX_WIDTH);
  });

  it("only defaults realtime to systems the shipped template has priorities for", () => {
    expect(REALTIME_DEFAULT_CONSTELLATIONS).toEqual(["GPS", "GAL", "GLO"]);
  });
});
