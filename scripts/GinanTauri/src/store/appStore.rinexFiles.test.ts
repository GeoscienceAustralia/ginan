import { beforeEach, describe, expect, it } from "vitest";
import { buildConfigInputs } from "./appStore";
import { makeCatalogue, makeRinexData, resetAppStore, store } from "../test/storeHelpers";

/**
 * Several observation files in one run. The first file stays the primary and
 * alone seeds the header-derived fields; the rest only widen the time window
 * and join the list handed to PEA.
 */

beforeEach(() => {
  resetAppStore();
});

const A = "/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx";
const B = "/data/ALIC00AUS_R_20240030000_01D_30S_MO.rnx";
const C = "/data/DARW00AUS_R_20240040000_01D_30S_MO.rnx";

function dayOf(d: number, over = {}) {
  const dd = String(d).padStart(2, "0");
  return makeRinexData({
    start_epoch: `2024-01-${dd} 00:00:00`,
    end_epoch: `2024-01-${dd} 23:59:30`,
    ...over,
  });
}

/** The primary loaded, as the open-a-file chain leaves it. */
function loadPrimary() {
  store().setOutputDir("/out");
  store().setProductsDir("/products");
  store().setRinexPath(A);
  store().setRinexData(dayOf(2));
}

describe("addRinexFile", () => {
  beforeEach(loadPrimary);

  it("appends the file and widens the window to cover it", () => {
    store().addRinexFile({ path: B, data: dayOf(3) });

    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
    expect(store().startEpoch).toBe("2024-01-02T00:00:00");
    expect(store().endEpoch).toBe("2024-01-03T23:59:30");
  });

  it("spans min(start) to max(end) whatever order files are added in", () => {
    store().addRinexFile({ path: C, data: dayOf(4) });
    store().addRinexFile({ path: B, data: dayOf(1) });

    expect(store().startEpoch).toBe("2024-01-01T00:00:00");
    expect(store().endEpoch).toBe("2024-01-04T23:59:30");
  });

  it("leaves the header-derived fields on the primary", () => {
    store().addRinexFile({
      path: C,
      data: dayOf(4, { marker_name: "DARW00AUS", receiver_type: "OTHER", antenna_type: "OTHER" }),
    });

    expect(store().markerName).toBe("ALIC");
    expect(store().receiverType).toBe("SEPT POLARX5 ");
    expect(store().antennaType).toBe("LEIAR25.R3      NONE");
    expect(store().rinexData!.marker_name).toBe("ALIC00AUS");
    expect(store().rinexPath).toBe(A);
  });

  it("ignores a file that is already selected", () => {
    store().addRinexFile({ path: B, data: dayOf(3) });
    store().addRinexFile({ path: B, data: dayOf(3) });
    store().addRinexFile({ path: A, data: dayOf(2) });

    expect(store().extraRinex).toHaveLength(1);
  });

  it("drops the catalogue and bias table the old window produced", () => {
    store().setPppCatalogue(makeCatalogue([{}]));
    store().setBia({ gps: ["C1C"], gal: [], glo: [], bds: [], qzs: [] });
    store().setCodeIntersection({ GPS: { usable: [], rinexOnly: [], biaOnly: [] } });

    store().addRinexFile({ path: B, data: dayOf(3) });

    expect(store().pppCatalogue).toBeNull();
    expect(store().bia).toBeNull();
    expect(store().codeIntersection).toBeNull();
  });
});

describe("removeRinexFile", () => {
  beforeEach(() => {
    loadPrimary();
    store().addRinexFile({ path: B, data: dayOf(3) });
    store().addRinexFile({ path: C, data: dayOf(4, { marker_name: "DARW00AUS" }) });
  });

  it("removes an extra and narrows the window back down", () => {
    store().removeRinexFile(C);

    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
    expect(store().startEpoch).toBe("2024-01-02T00:00:00");
    expect(store().endEpoch).toBe("2024-01-03T23:59:30");
  });

  it("leaves the primary and its fields alone when an extra goes", () => {
    store().removeRinexFile(B);

    expect(store().rinexPath).toBe(A);
    expect(store().markerName).toBe("ALIC");
  });

  it("promotes the next file when the primary goes, re-seeding the header fields", () => {
    store().removeRinexFile(A);

    expect(store().rinexPath).toBe(B);
    expect(store().extraRinex.map((f) => f.path)).toEqual([C]);
    expect(store().startEpoch).toBe("2024-01-03T00:00:00");
    expect(store().endEpoch).toBe("2024-01-04T23:59:30");
  });

  it("takes the marker from the promoted file", () => {
    store().removeRinexFile(A);
    store().removeRinexFile(B);

    expect(store().rinexPath).toBe(C);
    expect(store().markerName).toBe("DARW");
  });

  it("empties the selection when the last file goes", () => {
    store().removeRinexFile(B);
    store().removeRinexFile(C);
    store().removeRinexFile(A);

    expect(store().rinexPath).toBeNull();
    expect(store().rinexData).toBeNull();
    expect(store().extraRinex).toEqual([]);
    expect(buildConfigInputs()).toBeNull();
  });

  it("ignores a path that is not selected", () => {
    store().removeRinexFile("/data/nope.rnx");

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex).toHaveLength(2);
    expect(store().endEpoch).toBe("2024-01-04T23:59:30");
  });

  it("drops the catalogue so it is rescanned for the new window", () => {
    store().setPppCatalogue(makeCatalogue([{}]));

    store().removeRinexFile(C);

    expect(store().pppCatalogue).toBeNull();
  });
});

describe("switching to realtime", () => {
  it("clears every selected file, not just the primary", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: dayOf(3) });

    store().setInputMode("realtime");

    expect(store().rinexPath).toBeNull();
    expect(store().extraRinex).toEqual([]);
  });
});

describe("buildConfigInputs with several files", () => {
  beforeEach(() => {
    loadPrimary();
    store().addRinexFile({ path: B, data: dayOf(3) });
    store().addRinexFile({ path: C, data: dayOf(4, { marker_name: "DARW00AUS" }) });
  });

  it("sends every file, primary first, and keeps rinex_path as the primary", () => {
    const inputs = buildConfigInputs()!;

    expect(inputs.rinex_path).toBe(A);
    expect(inputs.rinex_paths).toEqual([A, B, C]);
  });

  it("spans the time window across all files, which is what products are fetched for", () => {
    const inputs = buildConfigInputs()!;

    expect(inputs.start_epoch).toBe("2024-01-02 00:00:00");
    expect(inputs.end_epoch).toBe("2024-01-04 23:59:30");
  });

  it("takes marker and receiver from the primary only", () => {
    const inputs = buildConfigInputs()!;

    expect(inputs.marker_name).toBe("ALIC");
    expect(inputs.receiver_type).toBe("SEPT POLARX5 ");
    expect(inputs.approx_position).toEqual([-4052052.7, 4212836.0, -2545105.0]);
  });
});

describe("buildConfigInputs with one file", () => {
  it("sends the file in both fields, so the backend sees a single entry", () => {
    loadPrimary();
    const inputs = buildConfigInputs()!;

    expect(inputs.rinex_path).toBe(A);
    expect(inputs.rinex_paths).toEqual([A]);
  });

  it("sends no files for a realtime run", () => {
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["ALIC00AUS0"] });

    const inputs = buildConfigInputs()!;

    expect(inputs.rinex_path).toBe("");
    expect(inputs.rinex_paths).toEqual([]);
  });
});
