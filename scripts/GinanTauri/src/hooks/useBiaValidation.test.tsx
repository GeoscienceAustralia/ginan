import { beforeEach, describe, expect, it, vi } from "vitest";
import { renderHook, waitFor } from "@testing-library/react";
import { useBiaValidation } from "./useBiaValidation";
import { useAppStore } from "../store/appStore";
import {
  makeCatalogue,
  makeObsCodes,
  makeRinexData,
  resetAppStore,
  store,
} from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";
import { WF } from "../config/workflowSteps";

// A bias table carries code and phase biases; the header's L1C/L2W/L5Q and L1X/L5X are
// the signals checked against it. L2W has no entry of either kind.
const BIA = {
  gps: ["C1C", "L1C", "C5Q", "L5Q"],
  gal: ["C1X", "L1X"],
  glo: [],
  bds: [],
  qzs: [],
};

beforeEach(() => {
  resetAppStore();
  vi.spyOn(console, "error").mockImplementation(() => {});
});

/** A store where the effect's every precondition is met. */
function ready() {
  store().setProductsDir("/products");
  store().setRinexData(makeRinexData());
  store().initializeConstellationCodes(makeObsCodes());
  store().setPppCatalogue(makeCatalogue([{}]));
}

/** The activity chain the file-open flow leaves for this effect to close. */
function withActivity() {
  store().startActivity("Opening ALIC.rnx", [{ key: "bia", label: "Fetch signal bias table" }]);
}

describe("preconditions", () => {
  it("does nothing at all before a catalogue has been scanned", () => {
    store().setProductsDir("/products");
    renderHook(() => useBiaValidation());
    expect(invokeCount("fetch_bia_codes_for_combo")).toBe(0);
    expect(store().bia).toBeNull();
  });

  it("clears any previous result for an empty catalogue", () => {
    ready();
    useAppStore.setState({ bia: BIA, codeIntersection: { GPS: { usable: [], rinexOnly: [], biaOnly: [] } } });
    store().setPppCatalogue({ entries: [] });

    renderHook(() => useBiaValidation());

    expect(store().bia).toBeNull();
    expect(store().codeIntersection).toBeNull();
  });

  it("skips, and settles the panel, when the selection is not in the catalogue", () => {
    ready();
    withActivity();
    useAppStore.setState({ pppSelection: { provider: "COD", series: "RAP", project: "MGX" } });

    renderHook(() => useBiaValidation());

    expect(invokeCount("fetch_bia_codes_for_combo")).toBe(0);
    expect(store().activity!.steps[0]).toMatchObject({
      state: "skipped",
      detail: "No products for COD/RAP/MGX",
    });
    expect(store().activity!.finished).toBe(true);
  });

  it("skips when the observation window has no usable date", () => {
    store().setProductsDir("/products");
    store().setPppCatalogue(makeCatalogue([{}]));
    withActivity();

    renderHook(() => useBiaValidation());

    expect(invokeCount("fetch_bia_codes_for_combo")).toBe(0);
    expect(store().activity!.steps[0]).toMatchObject({
      state: "skipped",
      detail: "Observation window has no usable date",
    });
  });

  it("skips when no products directory is set", () => {
    store().setRinexData(makeRinexData());
    store().setPppCatalogue(makeCatalogue([{}]));
    withActivity();

    renderHook(() => useBiaValidation());

    expect(invokeCount("fetch_bia_codes_for_combo")).toBe(0);
    expect(store().activity!.steps[0]).toMatchObject({
      state: "skipped",
      detail: "No products directory set",
    });
  });
});

describe("fetching the bias table", () => {
  it("asks for the GPS week that covers the observation start", async () => {
    ready();
    mockInvoke({ fetch_bia_codes_for_combo: BIA });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(invokeCount("fetch_bia_codes_for_combo")).toBe(1));
    // 2024-01-02 is in GPS week 2295, counting whole weeks from 1980-01-06.
    expect(lastInvokeArgs("fetch_bia_codes_for_combo")).toEqual({
      provider: "IGS",
      series: "FIN",
      project: "OPS",
      gpsWeek: 2295,
      productsDir: "/products",
    });
  });

  it("stores the bias table and the per-constellation intersection", async () => {
    ready();
    mockInvoke({ fetch_bia_codes_for_combo: BIA });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(store().bia).toEqual(BIA));
    expect(store().codeIntersection).toEqual({
      GPS: { usable: ["L1C", "L5Q"], rinexOnly: ["L2W"], biaOnly: [] },
      GAL: { usable: ["L1X"], rinexOnly: ["L5X"], biaOnly: [] },
    });
  });

  it("lists a signal the file did not observe once, however many biases the table has for it", async () => {
    ready();
    // C2L and L2L are one signal.
    mockInvoke({
      fetch_bia_codes_for_combo: { ...BIA, gps: [...BIA.gps, "L2L", "C2L"] },
    });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(store().codeIntersection).not.toBeNull());
    expect(store().codeIntersection!.GPS.biaOnly).toEqual(["L2L"]);
  });

  it("does not count the header's Doppler or strength codes as unsupported signals", async () => {
    store().setProductsDir("/products");
    store().setRinexData(
      makeRinexData({
        observation_codes: makeObsCodes({
          gps: ["C1C", "L1C", "D1C", "S1C", "C2W", "L2W", "D2W", "S2W"],
          gal: [],
        }),
      }),
    );
    store().initializeConstellationCodes(store().rinexData!.observation_codes!);
    store().setPppCatalogue(makeCatalogue([{}]));
    withActivity();
    mockInvoke({ fetch_bia_codes_for_combo: BIA });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(store().activity!.finished).toBe(true));
    // L1C and L2W are observed; only L1C has a phase bias. The other six header codes
    // are not signals of the list.
    expect(store().activity!.steps[0]).toMatchObject({
      detail: "1 of 2 phase signals supported",
    });
  });

  it("leaves out constellations the file never observed", async () => {
    ready();
    mockInvoke({ fetch_bia_codes_for_combo: { ...BIA, glo: ["C1C", "L1C"] } });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(store().codeIntersection).not.toBeNull());
    expect(Object.keys(store().codeIntersection!)).toEqual(["GPS", "GAL"]);
  });

  it("flags each observation code against what the bias table supports", async () => {
    ready();
    mockInvoke({ fetch_bia_codes_for_combo: BIA });

    renderHook(() => useBiaValidation());

    await waitFor(() =>
      expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([
        true,
        false,
        true,
      ]),
    );
    expect(store().constellationCodes.gal.map((c) => c.valid)).toEqual([true, false]);
  });

  it("records the tally in the workflow log and the activity step", async () => {
    ready();
    withActivity();
    mockInvoke({ fetch_bia_codes_for_combo: BIA });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(store().activity!.finished).toBe(true));
    expect(store().activity!.steps[0]).toMatchObject({
      state: "done",
      detail: "3 of 5 phase signals supported",
    });
    // One line per step on the Workflow channel...
    expect(store().workflowLines).toEqual(
      expect.arrayContaining([
        WF.validatingBia("IGS", "FIN", "OPS"),
        WF.biaCached("IGS", "FIN", "OPS"),
        WF.biaValidated("IGS", "FIN", "OPS"),
      ]),
    );
    // ...and the per-constellation breakdown on the Console channel.
    const log = store().consoleLines.join("\n");
    expect(log).toContain("BIA validation for IGS/FIN/OPS:");
    expect(log).toContain("GPS: 2 of 3 RINEX phase codes supported (L1C, L5Q)");
    expect(log).toContain("GAL: 1 of 2 RINEX phase codes supported (L1X)");
  });

  it("calls out a constellation with no overlap at all", async () => {
    ready();
    mockInvoke({
      fetch_bia_codes_for_combo: { gps: [], gal: ["L1X"], glo: [], bds: [], qzs: [] },
    });

    renderHook(() => useBiaValidation());

    await waitFor(() =>
      expect(store().consoleLines.join("\n")).toContain("GPS: no overlap with BIA"),
    );
  });

  it("covers a signal the table only has a code bias for", async () => {
    ready();
    mockInvoke({
      fetch_bia_codes_for_combo: { gps: ["C1C", "C2W"], gal: ["C1X"], glo: [], bds: [], qzs: [] },
    });

    renderHook(() => useBiaValidation());

    await waitFor(() =>
      expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([true, true, false]),
    );
    expect(store().constellationCodes.gal.map((c) => c.valid)).toEqual([true, false]);
    expect(store().consoleLines.join("\n")).toContain(
      "GPS: 2 of 3 RINEX phase codes supported (L1C, L2W)",
    );
  });

  it("reports a fetch failure without failing the whole file", async () => {
    ready();
    withActivity();
    mockInvoke({
      fetch_bia_codes_for_combo: () => Promise.reject(new Error("404 on the mirror")),
    });

    renderHook(() => useBiaValidation());

    await waitFor(() => expect(store().activity!.steps[0].state).toBe("error"));
    expect(store().bia).toBeNull();
    expect(store().codeIntersection).toBeNull();
    // The step failed; the file itself did not.
    expect(store().activity!.error).toBeNull();
    expect(store().workflowLines).toContain(WF.biaFetchFailed());
    expect(store().consoleLines.join("\n")).toMatch(/BIA fetch failed.*404 on the mirror/);
  });

  it("does not apply a result that arrives after the effect was torn down", async () => {
    ready();
    let resolve!: (v: typeof BIA) => void;
    mockInvoke({
      fetch_bia_codes_for_combo: () => new Promise<typeof BIA>((r) => (resolve = r)),
    });

    const { unmount } = renderHook(() => useBiaValidation());
    await waitFor(() => expect(invokeCount("fetch_bia_codes_for_combo")).toBe(1));
    unmount();
    resolve(BIA);
    await new Promise((r) => setTimeout(r, 0));

    expect(store().bia).toBeNull();
  });
});
