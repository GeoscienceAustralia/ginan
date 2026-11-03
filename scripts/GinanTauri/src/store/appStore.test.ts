import { beforeEach, describe, expect, it, vi } from "vitest";
import {
  DEFAULT_CASTER_URL,
  SIDEBAR_MAX_WIDTH,
  SIDEBAR_MIN_WIDTH,
  loadPersistedConfig,
  useAppStore,
} from "./appStore";
import { DEFAULT_PPP_SELECTION } from "../types";
import type { Constellation, PeaConfigExtract } from "../types";
import {
  makeCatalogue,
  makeConstellationCodes,
  makeObsCodes,
  makeRinexData,
  resetAppStore,
  store,
} from "../test/storeHelpers";
import { mockInvoke, readStore, seedStore, storeHas } from "../test/tauriMocks";
import { deviceTimeZone } from "../utils/time";

beforeEach(() => {
  resetAppStore();
});

/** Let the fire-and-forget `persist()` writes settle before asserting on them. */
const flushPersist = () => new Promise((r) => setTimeout(r, 0));

describe("loadPersistedConfig", () => {
  it("returns the shipped defaults for an empty store", async () => {
    const config = await loadPersistedConfig();
    expect(config).toMatchObject({
      outputDir: null,
      productsDir: "",
      mode: "Static",
      startEpoch: null,
      endEpoch: null,
      epochInterval: null,
      outputPos: true,
      outputGpx: false,
      outputTrace: false,
      outputSnx: false,
      outputSpp: true,
      constellationCodes: null,
      configOverwrite: true,
      processingParams: {},
      peaBinaryPath: null,
      customPeaNoticeSeen: null,
      lastCustomPeaPath: null,
      completedTours: [],
      casterUrl: DEFAULT_CASTER_URL,
      timeZone: deviceTimeZone(),
    });
    expect(config.pppSelection).toEqual(DEFAULT_PPP_SELECTION);
  });

  it("reads back a saved time zone, and uses the device's for one this runtime cannot format in", async () => {
    seedStore({ timeZone: "Australia/Perth" });
    expect((await loadPersistedConfig()).timeZone).toBe("Australia/Perth");

    seedStore({ timeZone: "Mars/Olympus_Mons" });
    expect((await loadPersistedConfig()).timeZone).toBe(deviceTimeZone());
  });

  it("reads back what an earlier session wrote", async () => {
    seedStore({
      outputDir: "/out",
      productsDir: "/products",
      mode: "Kinematic",
      epochInterval: 1,
      outputGpx: true,
      pppProvider: "COD",
      pppSeries: "RAP",
      pppProject: "MGX",
      casterUrl: "https://other.example/",
      completedTours: ["welcome"],
    });

    const config = await loadPersistedConfig();

    expect(config.outputDir).toBe("/out");
    expect(config.mode).toBe("Kinematic");
    expect(config.epochInterval).toBe(1);
    expect(config.outputGpx).toBe(true);
    expect(config.pppSelection).toEqual({ provider: "COD", series: "RAP", project: "MGX" });
    expect(config.casterUrl).toBe("https://other.example/");
    expect(config.completedTours).toEqual(["welcome"]);
  });

  it("deletes a rinexPath left by an earlier version of the app, so no file reloads itself", async () => {
    seedStore({ rinexPath: "/old/file.rnx" });
    await loadPersistedConfig();
    expect(storeHas("rinexPath")).toBe(false);
  });

  it("treats a configured products directory as evidence onboarding is done", async () => {
    seedStore({ productsDir: "/products" });
    expect((await loadPersistedConfig()).onboardingDone).toBe(true);
  });

  it("treats a fresh install with no products directory as needing onboarding", async () => {
    expect((await loadPersistedConfig()).onboardingDone).toBe(false);
  });

  it("lets an explicit onboarding flag override the products-directory guess", async () => {
    seedStore({ productsDir: "/products", onboardingDone: false });
    expect((await loadPersistedConfig()).onboardingDone).toBe(false);
  });
});

describe("hydrate", () => {
  const base = {
    outputDir: "/out",
    runDir: null,
    productsDir: "/products",
    mode: "Kinematic" as const,
    startEpoch: null,
    endEpoch: null,
    epochInterval: null,
    outputPos: true,
    outputGpx: true,
    outputTrace: false,
    outputSnx: false,
    outputSpp: true,
    constellationCodes: null,
    pppSelection: { provider: "COD", series: "RAP", project: "OPS" },
    configOverwrite: false,
    processingParams: { "some.path": 1 },
    peaBinaryPath: null,
    customPeaNoticeSeen: null,
    lastCustomPeaPath: null,
    onboardingDone: true,
    completedTours: ["welcome" as const],
    casterUrl: "https://caster.example/",
    timeZone: "UTC",
  };

  it("applies the persisted time zone", () => {
    store().hydrate({ ...base, timeZone: "Australia/Perth" });
    expect(store().timeZone).toBe("Australia/Perth");
  });

  it("applies the persisted machine setup", () => {
    store().hydrate(base);
    const s = store();
    expect(s.outputDir).toBe("/out");
    expect(s.productsDir).toBe("/products");
    expect(s.mode).toBe("Kinematic");
    expect(s.outputGpx).toBe(true);
    expect(s.configOverwrite).toBe(false);
    expect(s.processingParams).toEqual({ "some.path": 1 });
    expect(s.onboardingStatus).toBe("done");
    expect(s.completedTours).toEqual(["welcome"]);
  });

  it("marks an un-onboarded install as active rather than done", () => {
    store().hydrate({ ...base, onboardingDone: false });
    expect(store().onboardingStatus).toBe("active");
  });

  it("folds the restored caster setup into the realtime block", () => {
    store().hydrate(base);
    expect(store().realtime.caster_url).toBe("https://caster.example/");
  });

  it("never restores an ANTEX path; it is re-detected on every startup", () => {
    store().setAtxPath("/products/igs20.atx");
    store().hydrate(base);
    expect(store().atxPath).toBe("/products/igs20.atx");
    resetAppStore();
    store().hydrate(base);
    expect(store().atxPath).toBeNull();
  });

  it("leaves RINEX-derived fields at their defaults when nothing was stored", () => {
    const before = store().epochInterval;
    store().hydrate(base);
    expect(store().startEpoch).toBe("");
    expect(store().epochInterval).toBe(before);
  });

  it("applies RINEX-derived fields that were stored", () => {
    store().hydrate({
      ...base,
      startEpoch: "2024-01-02T00:00:00",
      endEpoch: "2024-01-02T23:59:30",
      epochInterval: 1,
      constellationCodes: makeConstellationCodes(),
    });
    const s = store();
    expect(s.startEpoch).toBe("2024-01-02T00:00:00");
    expect(s.endEpoch).toBe("2024-01-02T23:59:30");
    expect(s.epochInterval).toBe(1);
    expect(s.constellationCodes.gps.map((c) => c.code)).toEqual(["L1C", "L2W", "L5Q"]);
  });

  it("drops code, Doppler and strength entries from a selection an earlier version of the app stored", () => {
    const entry = (code: string, enabled = true) => ({ code, enabled, valid: true });
    store().hydrate({
      ...base,
      constellationCodes: {
        gps: [entry("C1C"), entry("L1C"), entry("D1C"), entry("S1C"), entry("C2W"), entry("L2W", false)],
        gal: [entry("C1X"), entry("S1X")],
        glo: [],
        bds: [],
        qzs: [],
      },
    });
    const { gps, gal } = store().constellationCodes;
    // What the user had chosen survives, in their order, with its enabled state.
    expect(gps).toEqual([entry("L1C"), entry("L2W", false)]);
    expect(gal).toEqual([]);
  });

  it("copes with a stored selection that lacks a constellation or has a malformed entry", () => {
    const stored = {
      gps: [{ code: "L1C", enabled: true, valid: true }, null, { enabled: true }, "L2W"],
      gal: undefined,
    };
    expect(() =>
      store().hydrate({ ...base, constellationCodes: stored as never }),
    ).not.toThrow();
    expect(store().constellationCodes.gps.map((c) => c.code)).toEqual(["L1C"]);
    expect(store().constellationCodes.gal).toEqual([]);
    expect(store().constellationCodes.qzs).toEqual([]);
  });
});

describe("setRinexData", () => {
  it("seeds the editable fields from the parsed header", () => {
    store().setRinexData(makeRinexData());
    const s = store();
    expect(s.markerName).toBe("ALIC");
    expect(s.receiverType).toBe("SEPT POLARX5 ");
    expect(s.antennaType).toBe("LEIAR25.R3      NONE");
    expect(s.antennaOffset).toEqual([0.0015, 0, 0]);
    expect(s.selectedConstellations).toEqual(["GPS", "GAL"]);
    expect(s.startEpoch).toBe("2024-01-02T00:00:00");
    expect(s.endEpoch).toBe("2024-01-02T23:59:30");
    expect(s.epochInterval).toBe(30);
  });

  it("does not trim receiver or antenna values — whitespace is meaningful", () => {
    store().setRinexData(makeRinexData({ receiver_type: "  ODD  " }));
    expect(store().receiverType).toBe("  ODD  ");
  });

  it("falls back to GPS when the header lists no constellations", () => {
    store().setRinexData(makeRinexData({ constellations: [] }));
    expect(store().selectedConstellations).toEqual(["GPS"]);
  });

  it("falls back to a 30 s interval when the header has none", () => {
    store().setRinexData(makeRinexData({ epoch_interval: null }));
    expect(store().epochInterval).toBe(30);
  });

  it("initialises every phase code as enabled and valid", () => {
    store().setRinexData(makeRinexData());
    const s = store();
    // The header's own list is kept whole: it says which constellations were observed.
    expect(s.availableObsCodes).toEqual(makeObsCodes());
    expect(s.constellationCodes.gps).toEqual([
      { code: "L1C", enabled: true, valid: true },
      { code: "L2W", enabled: true, valid: true },
      { code: "L5Q", enabled: true, valid: true },
    ]);
    expect(s.constellationCodes.glo).toEqual([]);
  });

  it("offers only the L signals of the header, not its code, Doppler or strength codes", () => {
    store().setRinexData(
      makeRinexData({
        observation_codes: makeObsCodes({
          gps: ["C1C", "L1C", "D1C", "S1C", "C2W", "L2W", "D2W", "S2W"],
          gal: ["C1X", "D1X"],
        }),
      }),
    );
    const s = store();
    expect(s.constellationCodes.gps.map((c) => c.code)).toEqual(["L1C", "L2W"]);
    // A constellation observed without any phase signal has nothing to select.
    expect(s.constellationCodes.gal).toEqual([]);
  });

  it("leaves the code lists alone when the header carried no observation codes", () => {
    store().setRinexData(makeRinexData({ observation_codes: undefined }));
    expect(store().availableObsCodes).toBeNull();
  });

  it("persists the epoch window so the next launch reopens on it", async () => {
    store().setRinexData(makeRinexData());
    await flushPersist();
    expect(readStore("startEpoch")).toBe("2024-01-02T00:00:00");
    expect(readStore("epochInterval")).toBe(30);
  });
});

describe("applyImportedConfig", () => {
  const extract = (over: Partial<PeaConfigExtract> = {}): PeaConfigExtract => ({
    mode: "Kinematic",
    start_epoch: "2024-03-04 01:02:03",
    end_epoch: "2024-03-04 05:06:07",
    epoch_interval: 5,
    output_pos: false,
    output_gpx: true,
    output_trace: null,
    output_snx: null,
    output_spp: null,
    constellation_codes: null,
    rinex_path: null,
    ...over,
  });

  it("applies the settings an imported file carries", () => {
    store().applyImportedConfig(extract());
    const s = store();
    expect(s.mode).toBe("Kinematic");
    expect(s.startEpoch).toBe("2024-03-04T01:02:03");
    expect(s.endEpoch).toBe("2024-03-04T05:06:07");
    expect(s.epochInterval).toBe(5);
    expect(s.outputPos).toBe(false);
    expect(s.outputGpx).toBe(true);
  });

  it("leaves absent fields untouched", () => {
    store().setOutputFormat("trace", true);
    store().applyImportedConfig(extract({ output_trace: null }));
    expect(store().outputTrace).toBe(true);
  });

  it("never moves this machine's directories", () => {
    store().hydrate({
      outputDir: "/mine/out",
      runDir: null,
      productsDir: "/mine/products",
      mode: "Static",
      startEpoch: null,
      endEpoch: null,
      epochInterval: null,
      outputPos: true,
      outputGpx: false,
      outputTrace: false,
      outputSnx: false,
      outputSpp: true,
      constellationCodes: null,
      pppSelection: DEFAULT_PPP_SELECTION,
      configOverwrite: true,
      processingParams: {},
      peaBinaryPath: null,
      customPeaNoticeSeen: null,
      lastCustomPeaPath: null,
      onboardingDone: true,
      completedTours: [],
      casterUrl: DEFAULT_CASTER_URL,
      timeZone: "UTC",
    });

    store().applyImportedConfig(extract());

    expect(store().outputDir).toBe("/mine/out");
    expect(store().productsDir).toBe("/mine/products");
  });

  it("persists what it applied", async () => {
    store().applyImportedConfig(extract());
    await flushPersist();
    expect(readStore("mode")).toBe("Kinematic");
    expect(readStore("startEpoch")).toBe("2024-03-04T01:02:03");
    expect(readStore("outputGpx")).toBe(true);
  });

  it("keeps only the phase codes of an imported selection, here and on disk", async () => {
    // A config exported by an earlier version of the app listed every observable it had offered.
    const item = (code: string) => ({ code, enabled: true, valid: true });
    store().applyImportedConfig(
      extract({
        constellation_codes: {
          gps: [item("L1W"), item("C1C"), item("L2W")],
          gal: [item("L1C"), item("S1C")],
          glo: [],
          bds: [],
          qzs: [],
        },
      }),
    );
    await flushPersist();
    expect(store().constellationCodes.gps.map((c) => c.code)).toEqual(["L1W", "L2W"]);
    expect(store().constellationCodes.gal.map((c) => c.code)).toEqual(["L1C"]);
    expect(
      (readStore("constellationCodes") as { gps: { code: string }[] }).gps.map((c) => c.code),
    ).toEqual(["L1W", "L2W"]);
  });
});

describe("console and workflow channels", () => {
  it("appends single lines and batches to the right channel", () => {
    store().appendLine("pea says hello");
    store().appendWorkflowLine("downloading");
    expect(store().consoleLines).toEqual(["pea says hello"]);
    expect(store().workflowLines).toEqual(["downloading"]);
  });

  it("keeps an empty batch from re-rendering the console", () => {
    store().appendLine("one");
    const before = store().consoleLines;
    store().appendLines([]);
    expect(store().consoleLines).toBe(before);
  });

  it("caps each channel so a long run cannot grow without bound", () => {
    const many = Array.from({ length: 50_050 }, (_, i) => `line ${i}`);
    store().appendLines(many);
    const lines = store().consoleLines;
    expect(lines).toHaveLength(50_000);
    expect(lines[0]).toBe("line 50");
    expect(lines[lines.length - 1]).toBe("line 50049");
  });

  it("clears each channel independently", () => {
    store().appendLine("a");
    store().appendWorkflowLine("b");
    store().clearConsole();
    expect(store().consoleLines).toEqual([]);
    expect(store().workflowLines).toEqual(["b"]);
  });
});

describe("activity", () => {
  const start = () =>
    store().startActivity("Opening file.rnx", [
      { key: "prepare", label: "Prepare" },
      { key: "parse", label: "Parse" },
      { key: "bia", label: "Bias table" },
    ]);

  it("starts every step pending", () => {
    start();
    const activity = store().activity!;
    expect(activity.title).toBe("Opening file.rnx");
    expect(activity.finished).toBe(false);
    expect(activity.steps.map((s) => s.state)).toEqual(["pending", "pending", "pending"]);
    expect(activity.steps.every((s) => s.detail === null && s.percent === null)).toBe(true);
  });

  it("marks a step active with its detail", () => {
    start();
    store().beginActivityStep("prepare", "Checking compression...");
    expect(store().activity!.steps[0]).toMatchObject({
      state: "active",
      detail: "Checking compression...",
    });
  });

  it("ignores an update aimed at a step that is not running", () => {
    start();
    store().updateActivityStep("parse", { detail: "late event" });
    expect(store().activity!.steps[1].detail).toBeNull();
  });

  it("routes an anonymous update to whichever step is active", () => {
    start();
    store().beginActivityStep("parse");
    store().updateActiveActivityStep({ detail: "Downloading x", percent: 42 });
    expect(store().activity!.steps[1]).toMatchObject({ detail: "Downloading x", percent: 42 });
  });

  it("does nothing when no step is active", () => {
    start();
    store().updateActiveActivityStep({ detail: "nobody's listening" });
    expect(store().activity!.steps.every((s) => s.detail === null)).toBe(true);
  });

  it("clears the progress bar when a step resolves", () => {
    start();
    store().beginActivityStep("prepare");
    store().updateActivityStep("prepare", { percent: 80 });
    store().resolveActivityStep("prepare", "done", "Already plain RINEX");
    expect(store().activity!.steps[0]).toMatchObject({
      state: "done",
      percent: null,
      detail: "Already plain RINEX",
    });
  });

  it("keeps the existing detail when resolve passes none", () => {
    start();
    store().beginActivityStep("prepare", "Checking...");
    store().resolveActivityStep("prepare", "done");
    expect(store().activity!.steps[0].detail).toBe("Checking...");
  });

  it("settles unreached steps as done on a clean finish", () => {
    start();
    store().resolveActivityStep("prepare", "done");
    store().endActivity();
    const activity = store().activity!;
    expect(activity.finished).toBe(true);
    expect(activity.steps.map((s) => s.state)).toEqual(["done", "done", "done"]);
    expect(activity.error).toBeNull();
  });

  it("settles unreached steps as skipped on a failure, keeping the failed one", () => {
    start();
    store().resolveActivityStep("prepare", "error", "boom");
    store().endActivity("Could not read file.rnx");
    const activity = store().activity!;
    expect(activity.steps.map((s) => s.state)).toEqual(["error", "skipped", "skipped"]);
    expect(activity.error).toBe("Could not read file.rnx");
  });

  it("keeps the first error when endActivity is called again with none", () => {
    start();
    store().endActivity("first failure");
    store().endActivity();
    expect(store().activity!.error).toBe("first failure");
  });

  it("ignores every activity action when nothing is running", () => {
    store().beginActivityStep("prepare");
    store().updateActivityStep("prepare", { detail: "x" });
    store().resolveActivityStep("prepare", "done");
    store().endActivity();
    expect(store().activity).toBeNull();
  });

  it("ignores an unknown step key", () => {
    start();
    store().beginActivityStep("no-such-step");
    expect(store().activity!.steps.every((s) => s.state === "pending")).toBe(true);
  });

  it("clears the panel outright", () => {
    start();
    store().clearActivity();
    expect(store().activity).toBeNull();
  });
});

describe("processing parameters", () => {
  it("stores a value moved off the default", async () => {
    store().setProcessingParam("receiver_options.global.elevation_mask", 10);
    expect(store().processingParams).toEqual({
      "receiver_options.global.elevation_mask": 10,
    });
    await flushPersist();
    expect(readStore("processingParams")).toEqual({
      "receiver_options.global.elevation_mask": 10,
    });
  });

  it("drops the key on reset so 'unset' and 'set to the default' stay identical", () => {
    store().setProcessingParam("a.b", 1);
    store().setProcessingParam("c.d", 2);
    store().resetProcessingParam("a.b");
    expect(store().processingParams).toEqual({ "c.d": 2 });
  });

  it("clears every override at once", () => {
    store().setProcessingParam("a.b", 1);
    store().resetAllProcessingParams();
    expect(store().processingParams).toEqual({});
  });

  it("loads the parameters discovered in the template", async () => {
    const discovered = [
      { path: "x.y", value_type: "boolean" as const, default_value: true },
    ];
    mockInvoke({ discover_config_params: discovered });
    await store().loadDiscoveredParams();
    expect(store().discoveredParams).toEqual(discovered);
  });

  it("falls back to the curated table when discovery fails", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    mockInvoke({});
    await store().loadDiscoveredParams();
    expect(store().discoveredParams).toEqual([]);
  });
});

describe("theme, sidebar and tours", () => {
  it("writes the theme onto the document and only records a manual choice", () => {
    store().setTheme("light");
    expect(document.documentElement.getAttribute("data-theme")).toBe("light");
    expect(localStorage.getItem("ginan-theme")).toBe("light");
    expect(localStorage.getItem("ginan-theme-manual")).toBeNull();

    store().setTheme("dark", true);
    expect(localStorage.getItem("ginan-theme-manual")).toBe("true");
  });

  it("persists the sidebar collapsed state on every toggle", () => {
    const initial = store().sidebarCollapsed;
    store().toggleSidebar();
    expect(store().sidebarCollapsed).toBe(!initial);
    expect(localStorage.getItem("ginan-sidebar-collapsed")).toBe(String(!initial));
  });

  it("clamps the sidebar width to its bounds", () => {
    store().setSidebarWidth(50);
    expect(store().sidebarWidth).toBe(SIDEBAR_MIN_WIDTH);
    store().setSidebarWidth(9999);
    expect(store().sidebarWidth).toBe(SIDEBAR_MAX_WIDTH);
    store().setSidebarWidth(400.6);
    expect(store().sidebarWidth).toBe(401);
  });

  it("only writes the released width to disk, not every drag frame", () => {
    store().setSidebarWidth(400);
    expect(localStorage.getItem("ginan-sidebar-width")).toBeNull();
    store().setSidebarWidth(420, true);
    expect(localStorage.getItem("ginan-sidebar-width")).toBe("420");
  });

  it("closes everything that could sit over a tour when one starts", () => {
    useAppStore.setState({ settingsOpen: true, credentialsModalOpen: true });
    store().startTour("workspace");
    expect(store().activeTour).toBe("workspace");
    expect(store().settingsOpen).toBe(false);
    expect(store().credentialsModalOpen).toBe(false);
  });

  it("records a completed tour once", async () => {
    store().startTour("welcome");
    store().endTour();
    store().startTour("welcome");
    store().endTour();
    expect(store().completedTours).toEqual(["welcome"]);
    expect(store().activeTour).toBeNull();
    await flushPersist();
    expect(readStore("completedTours")).toEqual(["welcome"]);
  });

  it("records a tour the user walked out of too, so it never auto-starts again", () => {
    store().startTour("welcome");
    store().endTour();
    expect(store().completedTours).toEqual(["welcome"]);
    expect(store().activeTour).toBeNull();
  });

  it("is a no-op when no tour is running", () => {
    store().endTour();
    expect(store().completedTours).toEqual([]);
  });

  it("clears the tour history when onboarding is replayed", async () => {
    store().startTour("welcome");
    store().endTour();
    store().restartOnboarding();
    expect(store().onboardingStatus).toBe("active");
    expect(store().completedTours).toEqual([]);
    await flushPersist();
    expect(readStore("onboardingDone")).toBe(false);
  });

  it("marks onboarding done", async () => {
    store().completeOnboarding();
    expect(store().onboardingStatus).toBe("done");
    await flushPersist();
    expect(readStore("onboardingDone")).toBe(true);
  });
});

describe("settings navigation", () => {
  it("opens settings without moving off the current page", () => {
    store().setSettingsPage("appearance");
    store().openSettings();
    expect(store()).toMatchObject({ settingsOpen: true, settingsPage: "appearance" });
  });

  it("deep-links straight to a page", () => {
    store().openSettings("binary");
    expect(store()).toMatchObject({ settingsOpen: true, settingsPage: "binary" });
  });

  it("sends the Live Stream page key from earlier versions to Credentials, where the caster settings live", () => {
    store().openSettings("livestream");
    expect(store()).toMatchObject({ settingsOpen: true, settingsPage: "credentials" });
    store().setSettingsPage("livestream");
    expect(store().settingsPage).toBe("credentials");
  });
});

describe("ANTEX path", () => {
  it("leaves an auto-detected path unpersisted", async () => {
    store().setAtxPath("/products/auto.atx");
    await flushPersist();
    expect(storeHas("atxPath")).toBe(false);
  });
});

describe("input mode", () => {
  it("clears the loaded file when switching to realtime", () => {
    store().setRinexPath("/data/file.rnx");
    store().setRinexData(makeRinexData());
    store().setInputMode("realtime");
    expect(store()).toMatchObject({ inputMode: "realtime", rinexPath: null, rinexData: null });
  });

  it("re-syncs the caster from the persisted setup on the way into realtime", () => {
    store().setCasterUrl("https://custom.example/");
    store().setInputMode("realtime");
    expect(store().realtime.caster_url).toBe("https://custom.example/");
  });

  it("resets the realtime block on the way back to file mode", () => {
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["MINE0"] });
    store().setInputMode("file");
    expect(store().realtime.obs_streams).toEqual(["ALIC00AUS0"]);
  });

  it("is a no-op when the mode is already set", () => {
    store().setRinexPath("/data/file.rnx");
    store().setInputMode("file");
    expect(store().rinexPath).toBe("/data/file.rnx");
  });
});

describe("realtime caster and stream sources", () => {
  it("mirrors a caster change into the realtime block and persists it", async () => {
    store().setCasterUrl("https://caster.example/");
    expect(store().casterUrl).toBe("https://caster.example/");
    expect(store().realtime.caster_url).toBe("https://caster.example/");
    await flushPersist();
    expect(readStore("casterUrl")).toBe("https://caster.example/");
  });

  it("blanks the effective caster once every stream is a serial device", () => {
    store().setInputMode("realtime");
    store().setCasterUrl("https://caster.example/");
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"] });
    expect(store().realtime.caster_url).toBe("https://caster.example/");
    store().updateRealtime({ nav_streams: ["serial:///dev/ttyUSB1"] });
    expect(store().realtime.caster_url).toBe("");
  });

  it("restores the caster when a stream goes back to a mountpoint", () => {
    store().setInputMode("realtime");
    store().setCasterUrl("https://caster.example/");
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"], nav_streams: [] });
    store().updateRealtime({ obs_streams: ["ALIC00AUS0"] });
    expect(store().realtime.caster_url).toBe("https://caster.example/");
  });

  it("floors the storage cap at zero and to whole megabytes", () => {
    store().setRealtimeStorageCapMb(-5);
    expect(store().realtimeStorageCapMb).toBe(0);
    store().setRealtimeStorageCapMb(12.9);
    expect(store().realtimeStorageCapMb).toBe(12);
  });
});

describe("fetchNtripSourcetable", () => {
  it("refuses without a caster URL to key the request by", async () => {
    useAppStore.setState({ casterUrl: "" });
    await store().fetchNtripSourcetable();
    expect(store().ntripSourcetableStatus).toEqual({
      state: "error",
      message: "Set a caster URL first",
    });
  });

  it("stores the streams and the host on success", async () => {
    const streams = [
      {
        mountpoint: "ALIC00AUS0",
        identifier: "Alice Springs",
        format: "RTCM 3.3",
        format_details: "",
        carrier: "2",
        nav_system: "GPS+GAL",
        network: "AUSCORS",
        country: "AUS",
        category: "obs" as const,
      },
    ];
    mockInvoke({ fetch_ntrip_sourcetable: streams });

    await store().fetchNtripSourcetable();

    expect(store().ntripSourcetable).toEqual(streams);
    expect(store().ntripSourcetableStatus).toEqual({
      state: "ok",
      count: 1,
      host: "ntrip.data.gnss.ga.gov.au",
    });
  });

  it("reports a failure and empties the table", async () => {
    mockInvoke({ fetch_ntrip_sourcetable: () => Promise.reject(new Error("timeout")) });
    await store().fetchNtripSourcetable();
    expect(store().ntripSourcetable).toEqual([]);
    expect(store().ntripSourcetableStatus).toMatchObject({ state: "error" });
  });
});

describe("constellation codes", () => {
  beforeEach(() => {
    store().initializeConstellationCodes(makeObsCodes());
  });

  it("enables every phase code on initialisation", () => {
    expect(store().constellationCodes.gps).toHaveLength(3);
    expect(store().constellationCodes.gps.every((c) => c.enabled && c.valid)).toBe(true);
  });

  it("updates one code by index", () => {
    store().updateConstellationCode("GPS", 1, { enabled: false });
    expect(store().constellationCodes.gps[1]).toEqual({
      code: "L2W",
      enabled: false,
      valid: true,
    });
  });

  it("ignores an index outside the list", () => {
    const before = store().constellationCodes.gps;
    store().updateConstellationCode("GPS", 99, { enabled: false });
    expect(store().constellationCodes.gps).toEqual(before);
  });

  it("reorders codes to change their priority", () => {
    store().reorderConstellationCodes("GPS", 2, 0);
    expect(store().constellationCodes.gps.map((c) => c.code)).toEqual([
      "L5Q",
      "L1C",
      "L2W",
    ]);
  });

  it("keeps only phase codes when a whole selection is set", async () => {
    const item = (code: string) => ({ code, enabled: true, valid: true });
    store().setConstellationCodes({
      gps: [item("C1C"), item("L1C"), item("D1C")],
      gal: [],
      glo: [],
      bds: [],
      qzs: [],
    });
    await flushPersist();
    expect(store().constellationCodes.gps.map((c) => c.code)).toEqual(["L1C"]);
    expect(
      (readStore("constellationCodes") as { gps: { code: string }[] }).gps.map((c) => c.code),
    ).toEqual(["L1C"]);
  });

  it("toggles a whole constellation at once", () => {
    store().setConstellationEnabled("GPS", false);
    expect(store().constellationCodes.gps.every((c) => !c.enabled)).toBe(true);
    store().setConstellationEnabled("GPS", true);
    expect(store().constellationCodes.gps.every((c) => c.enabled)).toBe(true);
  });

  it("refreshes the status line with the active systems once a file is open", () => {
    store().setRinexData(makeRinexData());
    store().setConstellationEnabled("GAL", false);
    expect(store().status).toBe("Loaded: ALIC · GPS");
  });

  it("reports 'none' when every system is switched off", () => {
    store().setRinexData(makeRinexData());
    store().setConstellationEnabled("GPS", false);
    store().setConstellationEnabled("GAL", false);
    expect(store().status).toBe("Loaded: ALIC · none");
  });

  it("leaves a live run's status alone", () => {
    store().setRinexData(makeRinexData());
    store().setIsProcessing(true);
    store().setStatus("Running PEA...");
    store().setConstellationEnabled("GPS", false);
    expect(store().status).toBe("Running PEA...");
  });
});

describe("PPP selection cascade", () => {
  it("returns nothing until a catalogue has been scanned", () => {
    expect(store().getAvailableProviders()).toEqual([]);
    expect(store().getAvailableSeries("IGS")).toEqual([]);
    expect(store().getAvailableProjects("IGS", "FIN")).toEqual([]);
  });

  it("lists sorted, de-duplicated providers, series and projects", () => {
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "IGS", series: "RAP", project: "OPS" },
        { provider: "COD", series: "FIN", project: "MGX" },
        { provider: "COD", series: "FIN", project: "MGX" },
      ]),
    );
    expect(store().getAvailableProviders()).toEqual(["COD", "IGS"]);
    expect(store().getAvailableSeries("IGS")).toEqual(["FIN", "RAP"]);
    expect(store().getAvailableProjects("IGS", "FIN")).toEqual(["OPS"]);
    expect(store().getAvailableProjectsForProvider("COD")).toEqual(["MGX"]);
    expect(store().getAvailableSeriesForProject("COD", "MGX")).toEqual(["FIN"]);
  });

  it("snaps the selection onto a combo the new catalogue actually has", () => {
    store().setPppCatalogue(
      makeCatalogue([{ provider: "COD", series: "RAP", project: "MGX" }]),
    );
    expect(store().pppSelection).toEqual({
      provider: "COD",
      series: "RAP",
      project: "MGX",
    });
  });

  it("leaves a still-valid selection alone", () => {
    store().setPppCatalogue(makeCatalogue([{}]));
    expect(store().pppSelection).toEqual(DEFAULT_PPP_SELECTION);
  });

  it("leaves the selection alone for an empty catalogue", () => {
    store().setPppCatalogue({ entries: [] });
    expect(store().pppSelection).toEqual(DEFAULT_PPP_SELECTION);
  });

  it("re-derives the series when the chosen provider does not publish it", () => {
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "COD", series: "RAP", project: "OPS" },
      ]),
    );
    store().setPppProvider("COD");
    expect(store().pppSelection).toMatchObject({ provider: "COD", series: "RAP" });
  });

  it("re-derives only the project when the series still stands", () => {
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "COD", series: "FIN", project: "MGX" },
      ]),
    );
    store().setPppProvider("COD");
    expect(store().pppSelection).toEqual({
      provider: "COD",
      series: "FIN",
      project: "MGX",
    });
  });

  it("re-derives the project when the series changes under it", () => {
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "IGS", series: "ULT", project: "MGX" },
      ]),
    );
    store().setPppSeries("ULT");
    expect(store().pppSelection).toMatchObject({ series: "ULT", project: "MGX" });
  });

  it("cross-filters the series when a project is picked that lacks it", () => {
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "IGS", series: "ULT", project: "MGX" },
      ]),
    );
    store().setPppProject("MGX");
    expect(store().pppSelection).toMatchObject({ project: "MGX", series: "ULT" });
  });

  it("persists each part of the selection", async () => {
    store().setPppProvider("GFZ");
    await flushPersist();
    expect(readStore("pppProvider")).toBe("GFZ");
  });
});

describe("availableConstellationsForCurrentSelection", () => {
  const asArray = () => [...store().availableConstellationsForCurrentSelection()].sort();

  it("is empty before any file is loaded", () => {
    expect(asArray()).toEqual([]);
  });

  it("lists only systems the RINEX actually observed", () => {
    store().setAvailableObsCodes(makeObsCodes());
    expect(asArray()).toEqual(["GAL", "GPS"]);
  });

  it("is permissive while no catalogue has been scanned", () => {
    store().setAvailableObsCodes(makeObsCodes({ bds: ["C2I"] }));
    expect(asArray()).toEqual(["BDS", "GAL", "GPS"]);
  });

  it("narrows to what the selected product publishes", () => {
    store().setAvailableObsCodes(makeObsCodes());
    store().setPppCatalogue(makeCatalogue([{ constellations: ["GPS"] }]));
    expect(asArray()).toEqual(["GPS"]);
  });

  it("over-allows rather than hiding when the orbit header could not be read", () => {
    store().setAvailableObsCodes(makeObsCodes());
    store().setPppCatalogue(makeCatalogue([{ constellations: [] }]));
    expect(asArray()).toEqual(["GAL", "GPS"]);
  });

  it("over-allows when the selected combo is not in the catalogue at all", () => {
    store().setAvailableObsCodes(makeObsCodes());
    store().setPppCatalogue(makeCatalogue([{ provider: "COD", constellations: ["GPS"] }]));
    useAppStore.setState({
      pppSelection: { provider: "NOPE", series: "FIN", project: "OPS" },
    });
    expect(asArray()).toEqual(["GAL", "GPS"]);
  });

  it("starts from every system in realtime, where no header enumerates them", () => {
    store().setInputMode("realtime");
    expect(asArray()).toEqual(["BDS", "GAL", "GLO", "GPS", "QZS"]);
  });

  it("still narrows a realtime run to the published set", () => {
    store().setInputMode("realtime");
    store().setPppCatalogue(makeCatalogue([{ constellations: ["GPS", "GAL"] }]));
    expect(asArray()).toEqual(["GAL", "GPS"]);
  });
});

describe("validateCodesAgainstBia", () => {
  beforeEach(() => {
    store().initializeConstellationCodes(makeObsCodes());
  });

  it("uses the already-fetched bias table rather than re-reading from disk", async () => {
    useAppStore.setState({
      bia: { gps: ["C1C", "L1C"], gal: [], glo: [], bds: [], qzs: [] },
    });
    mockInvoke({});

    await store().validateCodesAgainstBia();

    // Judged on the phase bias: L1C is in the table, L2W and L5Q are not.
    expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([true, false, false]);
    expect(store().biaValidation.isLoading).toBe(false);
  });

  it("counts a signal as covered by its code bias alone", async () => {
    // A code-only table (CODE's GLONASS and BeiDou records) still covers the signal.
    useAppStore.setState({
      bia: { gps: ["C1C", "C2W"], gal: [], glo: [], bds: [], qzs: [] },
    });

    await store().validateCodesAgainstBia();

    expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([true, true, false]);
  });

  it("marks a signal invalid when the table has neither its code nor its phase bias", async () => {
    useAppStore.setState({
      bia: { gps: ["C1W", "L2L", "D1C"], gal: [], glo: [], bds: [], qzs: [] },
    });

    await store().validateCodesAgainstBia();

    expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([false, false, false]);
  });

  it("falls back to the on-disk reader when nothing has been fetched yet", async () => {
    mockInvoke({
      fetch_bia_codes: { gps: ["L2W"], gal: ["L1X"], glo: [], bds: [], qzs: [] },
    });

    await store().validateCodesAgainstBia();

    expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([false, true, false]);
    expect(store().constellationCodes.gal.map((c) => c.valid)).toEqual([true, false]);
  });

  it("reports a failure without leaving the spinner up", async () => {
    mockInvoke({ fetch_bia_codes: () => Promise.reject(new Error("no BIA")) });
    await store().validateCodesAgainstBia();
    expect(store().biaValidation).toMatchObject({ isLoading: false, error: "no BIA" });
  });
});

describe("resetConfig", () => {
  it("puts the processing settings back to their defaults", () => {
    store().setMode("Kinematic");
    store().setEpochInterval(1);
    store().setOutputFormat("gpx", true);
    store().toggleConstellation("GAL");

    store().resetConfig();

    expect(store()).toMatchObject({
      mode: "Static",
      selectedConstellations: ["GPS"],
      startEpoch: "",
      endEpoch: "",
      epochInterval: 30,
      outputPos: true,
      outputGpx: false,
      status: "Config reset",
    });
  });

  it("leaves the machine setup alone", () => {
    store().setOutputDir("/out");
    store().setProductsDir("/products");
    store().resetConfig();
    expect(store()).toMatchObject({ outputDir: "/out", productsDir: "/products" });
  });
});

describe("small setters", () => {
  it("toggles a constellation on and off", () => {
    store().toggleConstellation("GAL");
    expect(store().selectedConstellations).toEqual(["GPS", "GAL"]);
    store().toggleConstellation("GPS");
    expect(store().selectedConstellations).toEqual(["GAL"]);
  });

  it("sets one antenna offset axis without disturbing the others", () => {
    store().setAntennaOffset(1, 1.5);
    expect(store().antennaOffset).toEqual([0, 1.5, 0]);
  });

  it("routes each output format to its own flag", () => {
    store().setOutputFormat("snx", true);
    expect(store()).toMatchObject({ outputSnx: true, outputGpx: false });
  });

  it("records provider health with a timestamp", () => {
    store().setProviderHealth("CDDIS", "down", 1700000000);
    expect(store().providerHealth.CDDIS).toEqual({ status: "down", checkedAt: 1700000000 });
  });

  it("stamps provider health with the current time when none is given", () => {
    vi.spyOn(Date, "now").mockReturnValue(1_700_000_500_000);
    store().setProviderHealth("CDDIS", "up");
    expect(store().providerHealth.CDDIS.checkedAt).toBe(1_700_000_500);
  });

  it("bumps the epoch counters the visualization and rescan effects watch", () => {
    store().bumpPosEpoch();
    store().bumpCddisEpoch();
    expect(store().posEpoch).toBe(1);
    expect(store().cddisEpoch).toBe(1);
  });

  it("raises and clears a connection fault", () => {
    store().setConnectionFault("Cannot reach cddis.nasa.gov");
    expect(store().connectionFault).toBe("Cannot reach cddis.nasa.gov");
    store().clearConnectionFault();
    expect(store().connectionFault).toBeNull();
  });

  it("merges partial BIA validation state", () => {
    store().setBiaValidation({ isLoading: true });
    store().setBiaValidation({ error: "nope" });
    expect(store().biaValidation).toMatchObject({ isLoading: true, error: "nope" });
  });
});

describe("constellation code lookup by name", () => {
  it("accepts the constellation in its display casing", () => {
    store().initializeConstellationCodes(makeObsCodes({ qzs: ["C1C", "L1C"] }));
    store().setConstellationEnabled("QZS" as Constellation, false);
    expect(store().constellationCodes.qzs[0].enabled).toBe(false);
  });
});
