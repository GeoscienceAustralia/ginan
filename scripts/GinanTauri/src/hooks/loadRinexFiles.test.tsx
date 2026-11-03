import { beforeEach, describe, expect, it } from "vitest";
import { act, renderHook } from "@testing-library/react";
import { usePickers } from "./usePickers";
import { buildConfigInputs } from "../store/appStore";
import { makeCatalogue, makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, invoke, lastInvokeArgs, mockInvoke, open } from "../test/tauriMocks";

/**
 * The open-several-files chain: the first file is the primary and runs the
 * whole single-file chain; the rest are prepared and parsed the same way and
 * only widen the window and the file list handed to PEA.
 */

beforeEach(() => {
  resetAppStore();
  store().setOutputDir("/out");
});

function pickers() {
  const { result } = renderHook(() => usePickers());
  return {
    call: async <T,>(fn: (p: ReturnType<typeof usePickers>) => Promise<T>) => {
      await act(async () => {
        await fn(result.current);
      });
    },
  };
}

const A = "/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx";
const B = "/data/ALIC00AUS_R_20240030000_01D_30S_MO.rnx";
const C = "/data/DARW00AUS_R_20240040000_01D_30S_MO.rnx";

/** One day of RINEX for `marker` on `d` January 2024. */
function dayOf(d: number, marker = "ALIC00AUS") {
  const dd = String(d).padStart(2, "0");
  return makeRinexData({
    marker_name: marker,
    start_epoch: `2024-01-${dd} 00:00:00`,
    end_epoch: `2024-01-${dd} 23:59:30`,
  });
}

/** What each fixture file's header says, keyed by the path parse_rinex is given. */
const HEADERS: Record<string, ReturnType<typeof dayOf>> = {
  [A]: dayOf(2),
  [B]: dayOf(3),
  [C]: dayOf(4, "DARW00AUS"),
};

function mockFiles(over: Record<string, unknown> = {}) {
  mockInvoke({
    prepare_rinex_input: (a: Record<string, unknown>) => a.path,
    parse_rinex: (a: Record<string, unknown>) => HEADERS[a.path as string],
    validate_antenna: true,
    scan_ppp_catalogue: makeCatalogue([{}]),
    ...over,
  });
}

describe("loadRinexFile with further files", () => {
  it("makes the first file the primary and lists the rest after it", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [B, C]));

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex.map((f) => f.path)).toEqual([B, C]);
    expect(buildConfigInputs()!.rinex_paths).toEqual([A, B, C]);
  });

  it("runs every file through prepare and parse, not just the first", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [B, C]));

    expect(invokeCount("prepare_rinex_input")).toBe(3);
    expect(invokeCount("parse_rinex")).toBe(3);
  });

  it("keeps the prepared path of a compressed extra, as it does for the primary", async () => {
    mockFiles({
      prepare_rinex_input: (a: Record<string, unknown>) =>
        a.path === "/data/ALIC00AUS_R_20240030000_01D_30S_MO.crx.gz" ? B : a.path,
    });
    const p = pickers();

    await p.call((k) =>
      k.loadRinexFile(A, ["/data/ALIC00AUS_R_20240030000_01D_30S_MO.crx.gz"]),
    );

    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
    expect(lastInvokeArgs("parse_rinex")).toEqual({ path: B });
  });

  it("seeds the header fields from the primary alone", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [C]));

    expect(store().markerName).toBe("ALIC");
    expect(buildConfigInputs()!.marker_name).toBe("ALIC");
  });

  it("scans for products over the span of every file, not the primary's day", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [B, C]));

    expect(invokeCount("scan_ppp_catalogue")).toBe(1);
    expect(lastInvokeArgs("scan_ppp_catalogue")).toEqual({
      startEpoch: "2024-01-02 00:00:00",
      endEpoch: "2024-01-04 23:59:30",
    });
  });

  it("sets the run window, which is what products are downloaded for, to the same span", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [B, C]));

    const inputs = buildConfigInputs()!;
    expect(inputs.start_epoch).toBe("2024-01-02 00:00:00");
    expect(inputs.end_epoch).toBe("2024-01-04 23:59:30");
  });

  it("adds a step for the other files only when there are some", async () => {
    mockFiles();
    const p = pickers();
    const withExtras = p.call((k) => k.loadRinexFile(A, [B]));
    expect(store().activity!.steps.map((s) => s.key)).toEqual([
      "prepare",
      "parse",
      "extra",
      "scan",
      "bia",
    ]);
    await withExtras;
  });

  it("replaces the previous selection, extras included", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A, [B, C]));

    await p.call((k) => k.loadRinexFile(B));

    expect(store().rinexPath).toBe(B);
    expect(store().extraRinex).toEqual([]);
    expect(buildConfigInputs()!.rinex_paths).toEqual([B]);
    expect(store().endEpoch).toBe("2024-01-03T23:59:30");
  });

  it("keeps the previous selection when the new primary cannot be prepared", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A, [B]));

    mockFiles({ prepare_rinex_input: () => Promise.reject(new Error("not gzip")) });
    await p.call((k) => k.loadRinexFile("/data/bad.rnx.gz", [C]));

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
  });

  it("skips an extra that will not parse and still loads the others", async () => {
    mockFiles({
      parse_rinex: (a: Record<string, unknown>) =>
        a.path === B ? Promise.reject(new Error("truncated header")) : HEADERS[a.path as string],
    });
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [B, C]));

    expect(store().extraRinex.map((f) => f.path)).toEqual([C]);
    expect(store().workflowLines.join("\n")).toMatch(/Could not read ALIC00AUS_R_20240030000.*truncated header/);
    expect(invokeCount("scan_ppp_catalogue")).toBe(1);
    expect(store().activity!.steps.find((s) => s.key === "extra")!.detail).toBe("1 added, 1 skipped");
  });

  it("does not add the primary again when it is repeated in the list", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [A, B]));

    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
  });

  it("labels the status line with how many files ride along", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.loadRinexFile(A, [B, C]));

    expect(store().status).toContain("ALIC +2 more");
  });
});

describe("pickRinex and pickMoreRinex", () => {
  it("loads the first picked file as primary and the rest alongside it", async () => {
    open.mockResolvedValueOnce([A, B]);
    mockFiles();
    const p = pickers();

    await p.call((k) => k.pickRinex());

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
  });

  it("still takes a single path from the dialog", async () => {
    open.mockResolvedValueOnce(A);
    mockFiles();
    const p = pickers();

    await p.call((k) => k.pickRinex());

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex).toEqual([]);
  });

  it("appends to the open files instead of replacing them", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A));
    open.mockResolvedValueOnce([B, C]);

    await p.call((k) => k.pickMoreRinex());

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex.map((f) => f.path)).toEqual([B, C]);
  });

  it("does nothing when the add dialog is cancelled", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A));
    const before = invoke.mock.calls.length;
    open.mockResolvedValueOnce(null);

    await p.call((k) => k.pickMoreRinex());

    expect(invoke.mock.calls.length).toBe(before);
    expect(store().extraRinex).toEqual([]);
  });
});

describe("addRinexFiles", () => {
  it("loads the first as the primary when nothing is open yet", async () => {
    mockFiles();
    const p = pickers();

    await p.call((k) => k.addRinexFiles([A, B]));

    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex.map((f) => f.path)).toEqual([B]);
  });

  it("widens the window and rescans products over the new span", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A));
    expect(lastInvokeArgs("scan_ppp_catalogue")).toMatchObject({
      endEpoch: "2024-01-02 23:59:30",
    });

    await p.call((k) => k.addRinexFiles([C]));

    expect(invokeCount("scan_ppp_catalogue")).toBe(2);
    expect(lastInvokeArgs("scan_ppp_catalogue")).toEqual({
      startEpoch: "2024-01-02 00:00:00",
      endEpoch: "2024-01-04 23:59:30",
    });
    expect(store().endEpoch).toBe("2024-01-04T23:59:30");
  });

  it("does not rescan when nothing new joined the selection", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A));

    await p.call((k) => k.addRinexFiles([A]));

    expect(invokeCount("scan_ppp_catalogue")).toBe(1);
    expect(store().extraRinex).toEqual([]);
    expect(store().pppCatalogue).not.toBeNull();
    expect(store().activity!.finished).toBe(true);
  });
});

describe("removeRinexFile", () => {
  async function loadThree() {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A, [B, C]));
    return p;
  }

  it("drops an extra, narrows the window and rescans for what is left", async () => {
    const p = await loadThree();

    await p.call((k) => k.removeRinexFile(C));

    expect(buildConfigInputs()!.rinex_paths).toEqual([A, B]);
    expect(lastInvokeArgs("scan_ppp_catalogue")).toEqual({
      startEpoch: "2024-01-02 00:00:00",
      endEpoch: "2024-01-03 23:59:30",
    });
  });

  it("promotes the next file when the primary goes and re-checks its antenna", async () => {
    store().setAtxPath("/products/igs20.atx");
    const p = await loadThree();
    const before = invokeCount("validate_antenna");

    await p.call((k) => k.removeRinexFile(A));

    expect(store().rinexPath).toBe(B);
    expect(buildConfigInputs()!.rinex_path).toBe(B);
    expect(invokeCount("validate_antenna")).toBe(before + 1);
  });

  it("leaves the app with no file once the last one is removed, without scanning", async () => {
    mockFiles();
    const p = pickers();
    await p.call((k) => k.loadRinexFile(A));
    const scans = invokeCount("scan_ppp_catalogue");

    await p.call((k) => k.removeRinexFile(A));

    expect(store().rinexPath).toBeNull();
    expect(store().rinexData).toBeNull();
    expect(invokeCount("scan_ppp_catalogue")).toBe(scans);
    expect(store().status).toBe("No observation file selected");
  });
});
