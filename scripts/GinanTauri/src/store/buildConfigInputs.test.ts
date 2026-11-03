import { beforeEach, describe, expect, it } from "vitest";
import { buildConfigInputs, defaultRunName, runOutputDir, useAppStore } from "./appStore";
import {
  makeCatalogue,
  makeObsCodes,
  makeRinexData,
  resetAppStore,
  store,
} from "../test/storeHelpers";

/**
 * `buildConfigInputs` is the single source of truth shared by the run button,
 * the YAML inspector and config export — if it drifts, the inspector starts
 * lying about what a run will do. These tests pin its whole contract.
 */

beforeEach(() => {
  resetAppStore();
});

/** A store that satisfies every file-mode precondition. */
function readyForFileRun() {
  store().setOutputDir("/out");
  store().setRunName("run1");
  store().setProductsDir("/products");
  store().setRinexPath("/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx");
  store().setRinexData(makeRinexData());
}

/** A store that satisfies every realtime precondition. */
function readyForRealtimeRun() {
  store().setOutputDir("/out");
  store().setInputMode("realtime");
  store().updateRealtime({ obs_streams: ["ALIC00AUS0"] });
}

describe("preconditions", () => {
  it("returns null with no output directory", () => {
    store().setRinexPath("/data/file.rnx");
    expect(buildConfigInputs()).toBeNull();
  });

  it("returns null in file mode with no observation file", () => {
    store().setOutputDir("/out");
    expect(buildConfigInputs()).toBeNull();
  });

  it("returns null in realtime mode with no observation stream", () => {
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: [] });
    expect(buildConfigInputs()).toBeNull();
  });

  it("does not require an observation file in realtime mode", () => {
    readyForRealtimeRun();
    expect(buildConfigInputs()).not.toBeNull();
  });
});

describe("file-mode inputs", () => {
  beforeEach(readyForFileRun);

  it("carries the paths, metadata and epoch window", () => {
    const inputs = buildConfigInputs()!;
    expect(inputs).toMatchObject({
      rinex_path: "/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx",
      output_dir: "/out/run1",
      products_dir: "/products",
      marker_name: "ALIC",
      mode: "Static",
      start_epoch: "2024-01-02 00:00:00",
      end_epoch: "2024-01-02 23:59:30",
      epoch_interval: 30,
      output_pos: true,
      output_gpx: false,
      ppp_provider: "IGS",
      ppp_series: "FIN",
      ppp_project: "OPS",
    });
  });

  it("has no realtime block, which is what selects the RINEX path in the writer", () => {
    expect(buildConfigInputs()!.realtime).toBeUndefined();
  });

  it("converts the datetime-local epochs back to PEA notation", () => {
    store().setStartEpoch("2024-01-02T06:00");
    const inputs = buildConfigInputs()!;
    expect(inputs.start_epoch).toBe("2024-01-02 06:00:00");
  });

  it("falls back to the output directory when no products directory is set", () => {
    store().setProductsDir("");
    expect(buildConfigInputs()!.products_dir).toBe("/out");
  });

  it("never trims the receiver and antenna values — exact whitespace round-trips", () => {
    store().setReceiverType("  SEPT POLARX5  ");
    store().setAntennaType("LEIAR25.R3      NONE");
    const inputs = buildConfigInputs()!;
    expect(inputs.receiver_type).toBe("  SEPT POLARX5  ");
    expect(inputs.antenna_type).toBe("LEIAR25.R3      NONE");
  });

  it("falls back through the header marker to UNKN", () => {
    store().setMarkerName("");
    expect(buildConfigInputs()!.marker_name).toBe("ALIC00AUS");

    useAppStore.setState({ rinexData: makeRinexData({ marker_name: null }) });
    expect(buildConfigInputs()!.marker_name).toBe("UNKN");
  });

  it("passes the header's a priori position straight through", () => {
    expect(buildConfigInputs()!.approx_position).toEqual([
      -4052052.7, 4212836.0, -2545105.0,
    ]);
  });

  it("carries the enabled observation codes in priority order", () => {
    store().reorderConstellationCodes("GPS", 2, 0);
    store().updateConstellationCode("GPS", 2, { enabled: false });
    const inputs = buildConfigInputs()!;
    expect(inputs.constellation_obs_codes!.gps).toEqual(["L5Q", "L1C"]);
  });

  it("sends only phase codes, however many observables the header listed", () => {
    store().setRinexData(
      makeRinexData({
        observation_codes: makeObsCodes({
          gps: ["C1C", "L1C", "D1C", "S1C", "C2W", "L2W", "D2W", "S2W"],
          gal: ["C1X", "L1X", "D1X", "S1X"],
        }),
      }),
    );
    const obs = buildConfigInputs()!.constellation_obs_codes!;
    expect(obs.gps).toEqual(["L1C", "L2W"]);
    expect(obs.gal).toEqual(["L1X"]);
  });

  it("lists exactly the constellations that still have an enabled code", () => {
    store().setConstellationEnabled("GAL", false);
    expect(buildConfigInputs()!.constellations).toEqual(["GPS"]);
  });

  it("drops a constellation the selected product cannot bias-correct", () => {
    store().setPppCatalogue(makeCatalogue([{ constellations: ["GPS"] }]));
    const inputs = buildConfigInputs()!;
    expect(inputs.constellations).toEqual(["GPS"]);
    expect(inputs.constellation_obs_codes!.gal).toEqual([]);
  });
});

describe("processing parameters", () => {
  beforeEach(readyForFileRun);

  it("sends nothing when every parameter is still at the template default", () => {
    expect(buildConfigInputs()!.advanced).toEqual({});
  });

  it("sends only the parameters moved off the default", () => {
    store().setProcessingParam("receiver_options.global.elevation_mask", 10);
    store().setProcessingParam("processing_options.spp.max_lsq_iterations", 12);
    expect(buildConfigInputs()!.advanced).toEqual({
      "receiver_options.global.elevation_mask": 10,
    });
  });

  it("compares an auto-discovered parameter against its real template default", () => {
    useAppStore.setState({
      discoveredParams: [
        { path: "receiver_options.global.elevation_mask", value_type: "number", default_value: 15 },
        { path: "brand_new.option", value_type: "number", default_value: 7 },
      ],
    });
    store().setProcessingParam("brand_new.option", 7);
    expect(buildConfigInputs()!.advanced).toEqual({});

    store().setProcessingParam("brand_new.option", 9);
    expect(buildConfigInputs()!.advanced).toEqual({ "brand_new.option": 9 });
  });
});

describe("realtime inputs", () => {
  beforeEach(readyForRealtimeRun);

  it("attaches the realtime block", () => {
    const inputs = buildConfigInputs()!;
    expect(inputs.realtime).toMatchObject({
      obs_streams: ["ALIC00AUS0"],
      nav_streams: ["BCEP00BKG0", "SSRA00CNE0"],
      ssr_antenna_offset: "APC",
    });
  });

  it("leaves the RINEX path empty", () => {
    expect(buildConfigInputs()!.rinex_path).toBe("");
    expect(buildConfigInputs()!.approx_position).toBeNull();
  });

  it("falls back to the realtime default constellations with no RINEX to seed them", () => {
    expect(buildConfigInputs()!.constellations).toEqual(["GPS", "GAL", "GLO"]);
  });

  it("narrows the realtime constellations to what the product publishes", () => {
    store().setPppCatalogue(makeCatalogue([{ constellations: ["GPS", "GAL"] }]));
    expect(buildConfigInputs()!.constellations).toEqual(["GPS", "GAL"]);
  });

  it("prefers an explicit code selection over the realtime defaults", () => {
    store().initializeConstellationCodes(makeObsCodes({ gps: [], gal: ["C1X", "L1X"] }));
    expect(buildConfigInputs()!.constellations).toEqual(["GAL"]);
  });

  it("never writes credentials — the caster URL is a bare root", () => {
    store().setCasterUrl("https://ntrip.example.com/");
    expect(buildConfigInputs()!.realtime!.caster_url).toBe("https://ntrip.example.com/");
  });

  it("runs SBAS in Dynamic mode, and the user's mode again once SBAS is removed", () => {
    store().setMode("Static");
    const id = store().addProduct("corr", "sbas");
    expect(buildConfigInputs()!.mode).toBe("Dynamic");
    expect(store().mode).toBe("Static");

    store().removeProduct("corr", id);
    expect(buildConfigInputs()!.mode).toBe("Static");
  });

  it("sends the .SPP setting, on by default", () => {
    expect(buildConfigInputs()!.output_spp).toBe(true);
    store().setOutputFormat("spp", false);
    expect(buildConfigInputs()!.output_spp).toBe(false);
  });

  it("blanks the caster root when both stream lists carry full URIs", () => {
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"], nav_streams: ["serial:///dev/ttyUSB1"] });
    expect(buildConfigInputs()!.realtime!.caster_url).toBe("");
  });
});

describe("stability", () => {
  it("produces an identical object across calls when nothing changed", () => {
    readyForFileRun();
    expect(buildConfigInputs()).toEqual(buildConfigInputs());
  });
});

describe("run directory", () => {
  const at = new Date(2026, 0, 2, 3, 4);
  const dir = (runName: string, inputMode: "file" | "realtime" = "file") =>
    runOutputDir({ outputDir: "/out", runName, inputMode }, at);

  it("defaults to the local date, hour, minute and input source", () => {
    expect(defaultRunName("file", at)).toBe("2026-01-02_0304_rinex");
    expect(dir("")).toBe("/out/2026-01-02_0304_rinex");
    expect(dir("  ", "realtime")).toBe("/out/2026-01-02_0304_realtime");
  });

  it("uses the name the user typed", () => {
    expect(dir(" ALIC static ")).toBe("/out/ALIC static");
  });

  it("keeps the name to one directory under the output root", () => {
    expect(dir("../a/b\\c:d")).toBe("/out/.._a_b_c_d");
    expect(dir("..")).toBe("/out/2026-01-02_0304_rinex");
  });

  it("is what the config is written with", () => {
    readyForFileRun();
    store().setRunName("run1");
    expect(buildConfigInputs()!.output_dir).toBe("/out/run1");
  });
});
