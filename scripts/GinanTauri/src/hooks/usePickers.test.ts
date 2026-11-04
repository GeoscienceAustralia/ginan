import { beforeEach, describe, expect, it } from "vitest";
import { RINEX_EXTENSIONS, looksLikeRinex, scanPppCatalogue } from "./usePickers";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

describe("looksLikeRinex", () => {
  it("accepts every extension the file dialog offers", () => {
    for (const ext of RINEX_EXTENSIONS) {
      expect(looksLikeRinex(`/data/file.${ext}`), ext).toBe(true);
    }
  });

  it("accepts any RINEX 2 short name, not just the years listed", () => {
    expect(looksLikeRinex("/data/alic0010.10o")).toBe(true);
    expect(looksLikeRinex("/data/alic0010.99d")).toBe(true);
  });

  it("is case-insensitive about the extension", () => {
    expect(looksLikeRinex("/data/ALIC.RNX")).toBe(true);
    expect(looksLikeRinex("/data/ALIC.23O")).toBe(true);
  });

  it("rejects anything else", () => {
    expect(looksLikeRinex("/data/notes.txt")).toBe(false);
    expect(looksLikeRinex("/data/config.yaml")).toBe(false);
    expect(looksLikeRinex("/data/alic0010.23x")).toBe(false);
  });

  it("rejects a path with no extension at all", () => {
    // `split(".").pop()` returns the whole basename here, which must not match.
    expect(looksLikeRinex("/data/README")).toBe(false);
    expect(looksLikeRinex("")).toBe(false);
  });
});

describe("scanPppCatalogue", () => {
  /** The activity steps the open-a-file chain publishes before the scan runs. */
  function startActivity() {
    store().startActivity("Opening ALIC.rnx", [
      { key: "scan", label: "Scan CDDIS for products" },
      { key: "bia", label: "Fetch signal bias table" },
    ]);
  }

  it("skips the scan for a file with no start epoch, and settles the panel", async () => {
    startActivity();

    await scanPppCatalogue(makeRinexData({ start_epoch: null }), "ALIC");

    expect(store().pppCatalogue).toEqual({ entries: [] });
    expect(invokeCount("scan_ppp_catalogue")).toBe(0);
    expect(store().activity!.steps[0]).toMatchObject({
      state: "skipped",
      detail: "File has no start epoch",
    });
    expect(store().activity!.finished).toBe(true);
  });

  it("passes the observation window to the backend", async () => {
    mockInvoke({ scan_ppp_catalogue: { entries: [] } });

    await scanPppCatalogue(makeRinexData(), "ALIC");

    expect(lastInvokeArgs("scan_ppp_catalogue")).toEqual({
      startEpoch: "2024-01-02 00:00:00",
      endEpoch: "2024-01-02 23:59:30",
    });
  });

  it("sends an empty end epoch when the header has none", async () => {
    mockInvoke({ scan_ppp_catalogue: { entries: [] } });

    await scanPppCatalogue(makeRinexData({ end_epoch: null }), "ALIC");

    expect(lastInvokeArgs("scan_ppp_catalogue")!.endEpoch).toBe("");
  });

  it("reports the provider count in the status line and the step detail", async () => {
    startActivity();
    mockInvoke({
      scan_ppp_catalogue: {
        entries: [
          { provider: "IGS", series: "FIN", project: "OPS", has_sp3: true, has_clk: true, has_bia: true, constellations: [], files: {} },
          { provider: "COD", series: "FIN", project: "OPS", has_sp3: true, has_clk: true, has_bia: true, constellations: [], files: {} },
        ],
      },
    });

    await scanPppCatalogue(makeRinexData(), "ALIC");

    expect(store().status).toBe("Loaded: ALIC · GPS, GAL · 2 PPP provider(s) available");
    expect(store().activity!.steps[0]).toMatchObject({
      state: "done",
      detail: "2 provider(s), 2 product combination(s)",
    });
  });

  it("closes the panel itself when there is no bias table left to fetch", async () => {
    startActivity();
    mockInvoke({ scan_ppp_catalogue: { entries: [] } });

    await scanPppCatalogue(makeRinexData(), "ALIC");

    expect(store().activity!.steps[1]).toMatchObject({
      state: "skipped",
      detail: "No products for this window",
    });
    expect(store().activity!.finished).toBe(true);
  });

  it("leaves the bias step alone when there are products for it", async () => {
    startActivity();
    mockInvoke({
      scan_ppp_catalogue: {
        entries: [
          { provider: "IGS", series: "FIN", project: "OPS", has_sp3: true, has_clk: true, has_bia: true, constellations: [], files: {} },
        ],
      },
    });

    await scanPppCatalogue(makeRinexData(), "ALIC");

    expect(store().activity!.steps[1].state).toBe("pending");
    expect(store().activity!.finished).toBe(false);
  });

  it("empties the catalogue and stops the panel when the mirrors are unreachable", async () => {
    startActivity();
    mockInvoke({ scan_ppp_catalogue: () => Promise.reject(new Error("network down")) });

    await scanPppCatalogue(makeRinexData(), "ALIC");

    expect(store().pppCatalogue).toEqual({ entries: [] });
    expect(store().status).toMatch(/PPP product scan failed/);
    expect(store().activity!.steps[0].state).toBe("error");
    expect(store().activity!.error).toMatch(/Could not reach the product mirrors/);
  });

  it("works with no activity panel running", async () => {
    mockInvoke({ scan_ppp_catalogue: { entries: [] } });
    await expect(scanPppCatalogue(makeRinexData(), "ALIC")).resolves.toBeUndefined();
    expect(store().activity).toBeNull();
  });
});

describe("RINEX_EXTENSIONS", () => {
  it("covers plain, gzipped and Hatanaka-compressed observations", () => {
    expect(RINEX_EXTENSIONS).toContain("rnx");
    expect(RINEX_EXTENSIONS).toContain("gz");
    expect(RINEX_EXTENSIONS).toContain("crx");
  });

  it("lists no duplicates", () => {
    expect(new Set(RINEX_EXTENSIONS).size).toBe(RINEX_EXTENSIONS.length);
  });

  it("lists every extension lower-case, since the check lower-cases its input", () => {
    for (const ext of RINEX_EXTENSIONS) expect(ext).toBe(ext.toLowerCase());
  });
});
