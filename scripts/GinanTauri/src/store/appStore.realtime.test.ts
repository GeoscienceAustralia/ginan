/**
 * Store behaviour for the workspace latch, the persisted pea binary and update
 * prompt, serial stream settings and the pre-run stream test.
 */
import { beforeEach, describe, expect, it } from "vitest";
import { runStreamTest, serialByDevice, loadPersistedConfig } from "./appStore";
import {
  makeObsCodes,
  makeRinexData,
  makeStreamTestResult,
  resetAppStore,
  store,
} from "../test/storeHelpers";
import {
  invokeCount,
  lastInvokeArgs,
  mockInvoke,
  readStore,
  seedStore,
} from "../test/tauriMocks";
import { DEFAULT_SERIAL_SETTINGS } from "../types";

beforeEach(() => {
  resetAppStore();
});

const flushPersist = () => new Promise((r) => setTimeout(r, 0));

describe("hasEnteredWorkspace", () => {
  it("starts false, so a fresh launch lands on the welcome screen", () => {
    expect(store().hasEnteredWorkspace).toBe(false);
  });

  it("latches when a RINEX file is loaded", () => {
    store().setRinexData(makeRinexData());
    expect(store().hasEnteredWorkspace).toBe(true);
  });

  it("latches on entering realtime and survives switching back with nothing loaded", () => {
    store().setInputMode("realtime");
    expect(store().hasEnteredWorkspace).toBe(true);
    store().setInputMode("file");
    expect(store().rinexData).toBeNull();
    expect(store().hasEnteredWorkspace).toBe(true);
  });

  it("is not persisted", async () => {
    store().setInputMode("realtime");
    await flushPersist();
    expect(readStore("hasEnteredWorkspace")).toBeUndefined();
  });
});

describe("pea binary", () => {
  it("persists the chosen binary so it survives a restart", async () => {
    store().setPeaBinaryPath("/custom/pea");
    await flushPersist();
    expect(readStore("peaBinaryPath")).toBe("/custom/pea");
  });

  it("reads the persisted binary back", async () => {
    seedStore({ peaBinaryPath: "/custom/pea" });
    expect((await loadPersistedConfig()).peaBinaryPath).toBe("/custom/pea");
  });

  it("tracks the installed version and any available update", () => {
    store().setPeaVersion("v4.1.0");
    store().setPeaUpdateAvailable("v4.2.0");
    store().setPeaUpdateModalOpen(true);
    expect(store()).toMatchObject({
      peaVersion: "v4.1.0",
      peaUpdateAvailable: "v4.2.0",
      peaUpdateModalOpen: true,
    });
  });
});

describe("serial stream settings", () => {
  it("replaces one list's settings without touching the other", () => {
    store().setStreamSerial("obs", [{ ...DEFAULT_SERIAL_SETTINGS, baud: 9600 }]);
    store().setStreamSerial("corr", [DEFAULT_SERIAL_SETTINGS]);
    expect(store().streamSerial.obs[0].baud).toBe(9600);
    // One entry per live stream; the second default corrections stream has no settings of its own.
    expect(store().streamSerial.corr).toEqual([DEFAULT_SERIAL_SETTINGS, DEFAULT_SERIAL_SETTINGS]);
  });
});

describe("serialByDevice", () => {
  it("keys serial streams by device path and skips everything else", () => {
    const custom = { ...DEFAULT_SERIAL_SETTINGS, baud: 4800 };
    expect(
      serialByDevice([
        ["ALIC00AUS0", "serial:///dev/ttyUSB0", "tcp://host:2101"],
        [DEFAULT_SERIAL_SETTINGS, custom],
      ]),
    ).toEqual({ "/dev/ttyUSB0": custom });
  });

  it("falls back to the defaults for a stream with no settings entry", () => {
    expect(serialByDevice([["serial://COM3"], []])).toEqual({ COM3: DEFAULT_SERIAL_SETTINGS });
  });

  it("keeps the first settings for a device listed twice", () => {
    const first = { ...DEFAULT_SERIAL_SETTINGS, baud: 9600 };
    const second = { ...DEFAULT_SERIAL_SETTINGS, baud: 115200 };
    expect(
      serialByDevice(
        [["serial:///dev/ttyACM0"], [first]],
        [["serial:///dev/ttyACM0"], [second]],
      ),
    ).toEqual({ "/dev/ttyACM0": first });
  });

  it("returns an empty map when nothing is serial", () => {
    expect(serialByDevice([["ALIC00AUS0"], []])).toEqual({});
  });
});

describe("applyStreamTest", () => {
  it("fills the constellation codes and rounds the interval to whole seconds", () => {
    const codes = makeObsCodes({ glo: ["L1C", "C1C"] });
    store().applyStreamTest(makeStreamTestResult({ observation_codes: codes, interval_s: 4.6 }));

    expect(store().streamTest).not.toBeNull();
    expect(store().availableObsCodes).toEqual(codes);
    // The same phase-only list as a RINEX file gives.
    expect(store().constellationCodes?.glo.map((c) => c.code)).toEqual(["L1C"]);
    expect(store().epochInterval).toBe(5);
  });

  it("never sets an interval below one second", () => {
    store().applyStreamTest(makeStreamTestResult({ interval_s: 0.2 }));
    expect(store().epochInterval).toBe(1);
  });

  it("leaves the interval alone when the test could not measure one", () => {
    store().setEpochInterval(30);
    store().applyStreamTest(makeStreamTestResult({ interval_s: null }));
    expect(store().epochInterval).toBe(30);
  });

  describe("when the user has already tuned the selection", () => {
    const seen = makeObsCodes({
      gps: ["L1C", "L2W", "L5Q"],
      gal: ["L1X", "L5Q"],
      glo: ["L1C", "L2C"],
    });
    const flags = (c: { code: string; enabled: boolean }[]) =>
      c.map((i) => `${i.code}:${i.enabled ? "on" : "off"}`);

    /** First stream test, then the edits a user makes in the Constellations tab. */
    function seedThenTune() {
      store().applyStreamTest(makeStreamTestResult({ observation_codes: seen }));
      store().updateConstellationCode("GPS", 1, { enabled: false }); // L2W off
      store().reorderConstellationCodes("GPS", 2, 0); // L5Q first
      store().setConstellationEnabled("GLO", false);
    }

    it("keeps the signals and constellations the user switched off", () => {
      seedThenTune();

      store().applyStreamTest(makeStreamTestResult({ observation_codes: seen }));

      const codes = store().constellationCodes;
      expect(flags(codes.gps)).toEqual(["L5Q:on", "L1C:on", "L2W:off"]);
      expect(flags(codes.glo)).toEqual(["L1C:off", "L2C:off"]);
      expect(flags(codes.gal)).toEqual(["L1X:on", "L5Q:on"]);
    });

    it("persists the kept selection", async () => {
      seedThenTune();
      store().applyStreamTest(makeStreamTestResult({ observation_codes: seen }));
      await flushPersist();
      const saved = readStore("constellationCodes") as { gps: { code: string; enabled: boolean }[] };
      expect(flags(saved.gps)).toEqual(["L5Q:on", "L1C:on", "L2W:off"]);
    });

    it("adds a newly seen signal switched on, below the ones already listed", () => {
      seedThenTune();
      store().applyStreamTest(
        makeStreamTestResult({ observation_codes: { ...seen, gps: [...seen.gps, "L1W"] } }),
      );
      expect(flags(store().constellationCodes.gps)).toEqual([
        "L5Q:on", "L1C:on", "L2W:off", "L1W:on",
      ]);
    });

    it("adds a newly seen signal of a switched-off constellation switched off", () => {
      seedThenTune();
      store().applyStreamTest(
        makeStreamTestResult({ observation_codes: { ...seen, glo: [...seen.glo, "L3Q"] } }),
      );
      expect(flags(store().constellationCodes.glo)).toEqual(["L1C:off", "L2C:off", "L3Q:off"]);
    });

    it("drops a signal the streams no longer carry", () => {
      seedThenTune();
      store().applyStreamTest(
        makeStreamTestResult({ observation_codes: { ...seen, gps: ["L1C", "L5Q"] } }),
      );
      expect(flags(store().constellationCodes.gps)).toEqual(["L5Q:on", "L1C:on"]);
    });

    it("stays phase-only when the streams also carry code, Doppler and strength signals", () => {
      seedThenTune();
      store().applyStreamTest(
        makeStreamTestResult({
          observation_codes: {
            ...seen,
            gps: ["C1C", "L1C", "D1C", "S1C", "C2W", "L2W", "L5Q", "C5Q"],
            glo: ["C1C", "L1C", "L2C"],
          },
        }),
      );
      // The kept-selection path must narrow too, not only the plain re-seed.
      expect(flags(store().constellationCodes.gps)).toEqual(["L5Q:on", "L1C:on", "L2W:off"]);
      expect(flags(store().constellationCodes.glo)).toEqual(["L1C:off", "L2C:off"]);
    });

    it("switches on a constellation seen for the first time", () => {
      store().applyStreamTest(
        makeStreamTestResult({ observation_codes: makeObsCodes({ gps: ["L1C"], gal: [] }) }),
      );
      store().applyStreamTest(
        makeStreamTestResult({ observation_codes: makeObsCodes({ gps: ["L1C"], gal: ["L1X"] }) }),
      );
      expect(flags(store().constellationCodes.gal)).toEqual(["L1X:on"]);
    });
  });
});

describe("runStreamTest", () => {
  const readyForRealtime = () => {
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["ALIC00AUS0", "serial:///dev/ttyUSB0"] });
  };

  it("refuses without an observation stream and an output directory", async () => {
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: [] });
    await expect(runStreamTest()).rejects.toThrow(/at least one observation stream/);
  });

  it("writes the config, tests the streams, applies the result and rewrites the config", async () => {
    readyForRealtime();
    let writes = 0;
    mockInvoke({
      write_config: () => `/out/config_${++writes}.yaml`,
      test_streams: makeStreamTestResult({ interval_s: 2 }),
    });

    const path = await runStreamTest();

    expect(invokeCount("write_config")).toBe(2);
    expect(path).toBe("/out/config_2.yaml");
    expect(lastInvokeArgs("test_streams")).toEqual({
      options: {
        config_path: "/out/config_1.yaml",
        output_dir: "/out",
        caster_url: store().realtime.caster_url,
        serial: { "/dev/ttyUSB0": DEFAULT_SERIAL_SETTINGS },
        sbas_mode: null,
      },
    });
    expect(store().epochInterval).toBe(2);
    // The rewritten config carries the interval the test found.
    const rewritten = lastInvokeArgs("write_config") as { inputs: { epoch_interval: number } };
    expect(rewritten.inputs.epoch_interval).toBe(2);
    const log = store().workflowLines.join("\n");
    expect(log).toMatch(/ALIC00AUS0: 18 satellites, an epoch every 1\.0 s/);
    expect(log).toMatch(/Constellations: GPS, GAL · interval set to 2 s/);
  });

  it("rewrites the config with the selection the user made, not an all-on reseed", async () => {
    readyForRealtime();
    const seen = makeObsCodes({ gps: ["L1C", "L2W", "L5Q"], gal: ["L1X"], glo: ["L1C", "L2C"] });
    store().applyStreamTest(makeStreamTestResult({ observation_codes: seen }));
    store().updateConstellationCode("GPS", 1, { enabled: false }); // L2W off
    store().reorderConstellationCodes("GPS", 2, 0); // L5Q first
    store().setConstellationEnabled("GLO", false);
    mockInvoke({
      write_config: "/out/c.yaml",
      test_streams: makeStreamTestResult({ observation_codes: seen }),
    });

    await runStreamTest();

    const written = lastInvokeArgs("write_config") as {
      inputs: { constellations: string[]; constellation_obs_codes: Record<string, string[]> };
    };
    expect(written.inputs.constellations).toEqual(["GPS", "GAL"]);
    expect(written.inputs.constellation_obs_codes).toMatchObject({
      gps: ["L5Q", "L1C"],
      gal: ["L1X"],
      glo: [],
    });
  });

  it("says 'rate unknown' for a stream with no measured interval", async () => {
    readyForRealtime();
    mockInvoke({
      write_config: "/out/c.yaml",
      test_streams: makeStreamTestResult({
        interval_s: null,
        streams: [{ mountpoint: "ALIC00AUS0", records: 0, satellites: 0, interval_s: null }],
      }),
    });

    await runStreamTest();

    const log = store().workflowLines.join("\n");
    expect(log).toMatch(/ALIC00AUS0: 0 satellites, rate unknown/);
    expect(log).not.toMatch(/interval set to/);
  });

  it("propagates a failed stream test without applying anything", async () => {
    readyForRealtime();
    mockInvoke({
      write_config: "/out/c.yaml",
      test_streams: () => Promise.reject(new Error("caster refused")),
    });

    await expect(runStreamTest()).rejects.toThrow("caster refused");
    expect(store().streamTest).toBeNull();
  });
});
