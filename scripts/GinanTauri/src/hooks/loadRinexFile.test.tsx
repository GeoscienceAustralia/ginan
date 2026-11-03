import { beforeEach, describe, expect, it } from "vitest";
import { act, renderHook, waitFor } from "@testing-library/react";
import { cancelRinexLoad, usePickers } from "./usePickers";
import { BASE_TITLE } from "./useAppTitle";
import { makeCatalogue, makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import {
  invokeCount,
  lastInvokeArgs,
  mockInvoke,
  open,
  setTitle,
} from "../test/tauriMocks";

/**
 * The open-a-file chain: prepare (gunzip / Hatanaka) → parse the header →
 * validate the antenna → scan CDDIS. Drag-and-drop and the file dialog both
 * land here, so it is the single most-travelled path in the app.
 */

beforeEach(() => {
  resetAppStore();
  document.title = BASE_TITLE;
});

/**
 * `usePickers` subscribes to the whole store, so every action it fires re-renders
 * the hook — each call goes through `act` to keep React quiet about it.
 */
function pickers() {
  const { result } = renderHook(() => usePickers());
  return {
    call: async <T,>(fn: (p: ReturnType<typeof usePickers>) => Promise<T>) => {
      await act(async () => {
        await fn(result.current);
      });
    },
    result,
  };
}

/** Every command the happy path calls, in one place. */
function mockHappyPath(over: Record<string, unknown> = {}) {
  mockInvoke({
    prepare_rinex_input: (a: Record<string, unknown>) => a.path,
    parse_rinex: makeRinexData(),
    validate_antenna: true,
    scan_ppp_catalogue: makeCatalogue([{}]),
    ...over,
  });
}

describe("the happy path", () => {
  it("walks prepare → parse → scan and lands the parsed file in the store", async () => {
    mockHappyPath();
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(invokeCount("prepare_rinex_input")).toBe(1);
    expect(invokeCount("parse_rinex")).toBe(1);
    expect(invokeCount("scan_ppp_catalogue")).toBe(1);
    expect(store().rinexPath).toBe("/data/ALIC.rnx");
    expect(store().rinexData).not.toBeNull();
    expect(store().markerName).toBe("ALIC");
  });

  it("publishes the four steps of the chain before doing any work", async () => {
    mockHappyPath();
    const p = pickers();

    const done = p.result.current.loadRinexFile("/data/ALIC.rnx");
    expect(store().activity!.title).toBe("Opening ALIC.rnx");
    expect(store().activity!.steps.map((s) => s.key)).toEqual([
      "prepare",
      "parse",
      "scan",
      "bia",
    ]);
    await act(async () => {
      await done;
    });
  });

  it("uses the prepared path, not the one the user picked", async () => {
    mockHappyPath({ prepare_rinex_input: "/data/ALIC.rnx" });
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.crx.gz"));

    expect(store().rinexPath).toBe("/data/ALIC.rnx");
    expect(lastInvokeArgs("parse_rinex")).toEqual({ path: "/data/ALIC.rnx" });
  });

  it("notes when no unpacking was needed", async () => {
    mockHappyPath();
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(store().activity!.steps[0]).toMatchObject({
      state: "done",
      detail: "Already plain RINEX",
    });
  });

  it("drops everything derived from the previous file before preparing this one", async () => {
    store().setPppCatalogue(makeCatalogue([{}]));
    store().setBia({ gps: ["C1C"], gal: [], glo: [], bds: [], qzs: [] });
    store().setCodeIntersection({ GPS: { usable: [], rinexOnly: [], biaOnly: [] } });

    // A slow prepare holds the chain open right where the stale state would
    // otherwise still be readable by the BIA effect.
    let release!: (v: string) => void;
    mockHappyPath({
      prepare_rinex_input: () => new Promise<string>((r) => (release = r)),
    });
    const p = pickers();

    const done = p.result.current.loadRinexFile("/data/OTHER.rnx");
    expect(store().pppCatalogue).toBeNull();
    expect(store().bia).toBeNull();
    expect(store().codeIntersection).toBeNull();

    release("/data/OTHER.rnx");
    await act(async () => {
      await done;
    });
  });

  it("validates the antenna against the loaded ANTEX", async () => {
    store().setAtxPath("/products/igs20.atx");
    mockHappyPath();
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(lastInvokeArgs("validate_antenna")).toEqual({
      name: "LEIAR25.R3      NONE",
      atxPath: "/products/igs20.atx",
    });
    expect(store().antennaValid).toBe(true);
  });

  it("leaves antenna validity unknown with no ANTEX loaded", async () => {
    mockHappyPath();
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(invokeCount("validate_antenna")).toBe(0);
    expect(store().antennaValid).toBeNull();
  });

  it("leaves antenna validity unknown when the header names no antenna", async () => {
    store().setAtxPath("/products/igs20.atx");
    mockHappyPath({ parse_rinex: makeRinexData({ antenna_type: "   " }) });
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(invokeCount("validate_antenna")).toBe(0);
    expect(store().antennaValid).toBeNull();
  });

  it("treats a failed antenna check as unknown rather than invalid", async () => {
    store().setAtxPath("/products/igs20.atx");
    mockHappyPath({ validate_antenna: () => Promise.reject(new Error("bad ATX")) });
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(store().antennaValid).toBeNull();
  });

  it("reports the clipped marker, which is the one the config will carry", async () => {
    mockHappyPath();
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(store().activity!.steps[1].detail).toBe("ALIC00AUS · GPS, GAL");
    expect(store().status).toContain("Loaded: ALIC");
  });
});

describe("when preparing fails", () => {
  it("stops the chain and says why, without touching the loaded file", async () => {
    mockInvoke({
      prepare_rinex_input: () => Promise.reject(new Error("not gzip")),
    });
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx.gz"));

    expect(invokeCount("parse_rinex")).toBe(0);
    expect(store().rinexPath).toBeNull();
    expect(store().status).toMatch(/Could not read ALIC\.rnx\.gz/);
    expect(store().activity!.steps[0].state).toBe("error");
    expect(store().activity!.error).toMatch(/Could not read ALIC\.rnx\.gz/);
    expect(store().workflowLines.join("\n")).toMatch(/Could not prepare/);
  });
});

describe("when parsing fails", () => {
  it("stops the chain and reports the reason", async () => {
    mockInvoke({
      prepare_rinex_input: (a: Record<string, unknown>) => a.path,
      parse_rinex: () => Promise.reject(new Error("truncated header")),
    });
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(invokeCount("scan_ppp_catalogue")).toBe(0);
    expect(store().status).toMatch(/Parse error: .*truncated header/);
    expect(store().activity!.steps[1].state).toBe("error");
    expect(store().activity!.finished).toBe(true);
  });

  it("resets the window title so a stale filename does not linger", async () => {
    document.title = `${BASE_TITLE} - previous.rnx`;
    mockInvoke({
      prepare_rinex_input: (a: Record<string, unknown>) => a.path,
      parse_rinex: () => Promise.reject(new Error("truncated header")),
    });
    const p = pickers();

    await p.call((k) => k.loadRinexFile("/data/ALIC.rnx"));

    expect(document.title).toBe(BASE_TITLE);
    expect(setTitle).toHaveBeenCalledWith(BASE_TITLE);
  });
});

describe("pickRinex", () => {
  it("offers the RINEX filters plus an all-files escape hatch, and allows several files", async () => {
    open.mockResolvedValueOnce(null);
    const p = pickers();

    await p.call((k) => k.pickRinex());

    const opts = open.mock.calls[0][0] as {
      multiple: boolean;
      filters: { name: string; extensions: string[] }[];
    };
    expect(opts.multiple).toBe(true);
    expect(opts.filters.map((f) => f.name)).toEqual([
      "RINEX Observation Files",
      "All Files",
    ]);
  });

  it("loads whatever the dialog returned", async () => {
    open.mockResolvedValueOnce("/data/ALIC.rnx");
    mockHappyPath();
    const p = pickers();

    await p.call((k) => k.pickRinex());

    await waitFor(() => expect(store().rinexData).not.toBeNull());
  });
});

describe("the directory pickers", () => {
  it("stores a chosen output directory", async () => {
    open.mockResolvedValueOnce("/out");
    const p = pickers();

    await p.call((k) => k.pickOutputDir());

    expect(store().outputDir).toBe("/out");
  });

  it("stores a chosen products directory", async () => {
    open.mockResolvedValueOnce("/products");
    const p = pickers();

    await p.call((k) => k.pickProductsDir());

    expect(store().productsDir).toBe("/products");
  });

  it("leaves the current value alone when the dialog is cancelled", async () => {
    store().setOutputDir("/out");
    open.mockResolvedValueOnce(null);
    const p = pickers();

    await p.call((k) => k.pickOutputDir());

    expect(store().outputDir).toBe("/out");
  });
});

describe("pickPeaBinary", () => {
  it("registers a valid binary with the backend", async () => {
    open.mockResolvedValueOnce("/bin/pea");
    mockInvoke({ set_pea_binary: null });
    const p = pickers();

    await p.call((k) => k.pickPeaBinary());

    expect(lastInvokeArgs("set_pea_binary")).toEqual({ path: "/bin/pea" });
    expect(store().peaBinaryPath).toBe("/bin/pea");
  });

  it("switches from the managed release to the picked file as custom", async () => {
    store().setPeaBinaryPath("/data/pea");
    store().setPeaVersion("v4.2.0");
    store().setPeaUpdateAvailable("v4.3.0");
    open.mockResolvedValueOnce("/opt/pea");
    mockInvoke({ set_pea_binary: null });
    const p = pickers();

    await p.call((k) => k.pickPeaBinary());

    expect(store()).toMatchObject({
      peaBinaryPath: "/opt/pea",
      peaVersion: null,
      peaUpdateAvailable: null,
      // Picked deliberately, so no launch notice about it next time.
      customPeaNoticeSeen: "/opt/pea",
    });
  });

  it("keeps the managed release when the backend rejects the file", async () => {
    store().setPeaBinaryPath("/data/pea");
    store().setPeaVersion("v4.2.0");
    open.mockResolvedValueOnce("/bin/not-pea");
    mockInvoke({ set_pea_binary: () => Promise.reject(new Error("not executable")) });
    const p = pickers();

    await p.call((k) => k.pickPeaBinary());

    expect(store().peaBinaryPath).toBe("/data/pea");
    expect(store().peaVersion).toBe("v4.2.0");
  });

  it("rejects one the backend will not accept", async () => {
    open.mockResolvedValueOnce("/bin/not-pea");
    mockInvoke({ set_pea_binary: () => Promise.reject(new Error("not executable")) });
    const p = pickers();

    await p.call((k) => k.pickPeaBinary());

    expect(store().peaBinaryPath).toBeNull();
    expect(store().status).toMatch(/Invalid PEA binary/);
  });
});

describe("a load that is overtaken", () => {
  /** parse_rinex that stays pending until `release` is called. */
  function slowParse() {
    let release!: () => void;
    const gate = new Promise<void>((r) => (release = r));
    mockHappyPath({ parse_rinex: () => gate.then(() => makeRinexData()) });
    return release;
  }

  it("does not put a removed file back (cancelRinexLoad)", async () => {
    const release = slowParse();
    const p = pickers();

    let loading!: Promise<void>;
    await act(async () => {
      loading = p.result.current.loadRinexFile("/data/ALIC.rnx");
    });
    await waitFor(() => expect(invokeCount("parse_rinex")).toBe(1));

    // The user removes the file while the header is still being read.
    act(() => {
      cancelRinexLoad();
      store().setRinexPath(null);
    });
    await act(async () => {
      release();
      await loading;
    });

    expect(store().rinexData).toBeNull();
    expect(store().rinexPath).toBeNull();
    expect(store().pppCatalogue).toBeNull();
    expect(invokeCount("scan_ppp_catalogue")).toBe(0);
  });

  it("lets only the newest of two loads publish", async () => {
    const release = slowParse();
    const p = pickers();

    let first!: Promise<void>;
    await act(async () => {
      first = p.result.current.loadRinexFile("/data/OLD.rnx");
    });
    await waitFor(() => expect(invokeCount("parse_rinex")).toBe(1));
    // A second pick supersedes the first while it is still parsing.
    mockHappyPath({ parse_rinex: makeRinexData({ marker_name: "NEWW" }) });
    await p.call((k) => k.loadRinexFile("/data/NEW.rnx"));
    await act(async () => {
      release();
      await first;
    });

    expect(store().rinexPath).toBe("/data/NEW.rnx");
    expect(store().rinexData?.marker_name).toBe("NEWW");
  });
});
