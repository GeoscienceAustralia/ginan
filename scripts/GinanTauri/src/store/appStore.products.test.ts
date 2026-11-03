/**
 * The Input sidebar's product lists: the source of truth from which the
 * `realtime` stream fields and `streamSerial` are derived, plus the compat
 * shim the legacy list edits go through.
 */
import { beforeEach, describe, expect, it } from "vitest";
import { DEFAULT_CASTER_URL, serialByDevice, serialForProducts } from "./appStore";
import { makeStreamTestResult, resetAppStore, store } from "../test/storeHelpers";
import { DEFAULT_SERIAL_SETTINGS, type InputProduct, type StreamTestResult } from "../types";

beforeEach(() => {
  resetAppStore();
});

/** A product without its id, for comparing against the shipped defaults. */
const shape = ({ type, value }: InputProduct) => ({ type, value });

describe("addProduct", () => {
  it("appends and returns the new product's id", () => {
    const id = store().addProduct("obs", "ntrip", "NEW0");
    const obs = store().inputProducts.obs;
    expect(obs[obs.length - 1]).toEqual({ id, type: "ntrip", value: "NEW0" });
  });

  it("gives every product a unique id across both sections", () => {
    const a = store().addProduct("obs", "ntrip", "A0");
    const b = store().addProduct("obs", "ntrip", "B0");
    store().addProduct("corr", "ntrip", "C0");
    expect(a).not.toBe(b);
    const ids = [...store().inputProducts.obs, ...store().inputProducts.corr].map((p) => p.id);
    expect(new Set(ids).size).toBe(ids.length);
  });

  it("derives the stream lists: ntrip and serial in, http in, file and sbas out", () => {
    store().addProduct("obs", "serial", "/dev/ttyUSB0");
    store().addProduct("obs", "http", "https://h.example/x");
    store().addProduct("corr", "file", "/data/a.rnx");
    store().addProduct("corr", "sbas", "WAAS");
    store().addProduct("corr", "ntrip", "NEW0");

    expect(store().realtime.obs_streams).toEqual([
      "ALIC00AUS0",
      "serial:///dev/ttyUSB0",
      "https://h.example/x",
    ]);
    // SBAS pauses the SSR stream; NEW0 is no known kind, so it is not paused.
    expect(store().realtime.nav_streams).toEqual(["BCEP00BKG0", "NEW0"]);
    // One settings entry per stream sent; file and sbas have none.
    expect(store().streamSerial.obs).toHaveLength(3);
    expect(store().streamSerial.corr).toHaveLength(2);
  });
});

describe("updateProduct", () => {
  it("changes value and serial on the matching id only", () => {
    const custom = { ...DEFAULT_SERIAL_SETTINGS, baud: 4800 };
    const first = store().inputProducts.obs[0];
    const id = store().addProduct("obs", "serial", "/dev/ttyUSB0");

    store().updateProduct("obs", id, { value: "/dev/ttyUSB5", serial: custom });

    expect(store().inputProducts.obs[0]).toEqual(first);
    expect(store().inputProducts.obs[1]).toEqual({
      id,
      type: "serial",
      value: "/dev/ttyUSB5",
      serial: custom,
    });
    expect(store().realtime.obs_streams).toEqual(["ALIC00AUS0", "serial:///dev/ttyUSB5"]);
    expect(store().streamSerial.obs).toEqual([DEFAULT_SERIAL_SETTINGS, custom]);
  });

  it("is a no-op for an unknown id", () => {
    const products = store().inputProducts;
    const realtime = store().realtime;
    store().updateProduct("obs", "no-such-id", { value: "Z" });
    expect(store().inputProducts).toEqual(products);
    expect(store().realtime).toEqual(realtime);
  });
});

describe("removeProduct", () => {
  it("removes by id and leaves the rest of the section in order", () => {
    const first = store().addProduct("obs", "ntrip", "A0");
    store().addProduct("obs", "ntrip", "B0");
    store().removeProduct("obs", first);
    expect(store().realtime.obs_streams).toEqual(["ALIC00AUS0", "B0"]);
    expect(store().realtime.nav_streams).toEqual(["BCEP00BKG0", "SSRA00CNE0"]);
  });

  it("blanks caster_url once the last ntrip product is gone, and restores it on re-add", () => {
    const [obsNtrip] = store().inputProducts.obs;
    store().removeProduct("obs", obsNtrip.id);
    // The corrections section still holds ntrip products, so the caster stays.
    expect(store().realtime.caster_url).toBe(DEFAULT_CASTER_URL);

    for (const p of store().inputProducts.corr) store().removeProduct("corr", p.id);
    expect(store().realtime.caster_url).toBe("");

    store().setCasterUrl("https://c.example/");
    expect(store().realtime.caster_url).toBe("");

    store().addProduct("corr", "ntrip", "BCEP00BKG0");
    expect(store().realtime.caster_url).toBe("https://c.example/");
  });
});

describe("replaceSection", () => {
  it("assigns ids to products without one, keeps given ids, leaves the other section alone", () => {
    const obsBefore = store().inputProducts.obs;
    store().replaceSection("corr", [
      { type: "ntrip", value: "NEW0" },
      { id: "keep-me", type: "serial", value: "/dev/ttyUSB1" },
    ]);

    const [fresh, kept] = store().inputProducts.corr;
    expect(fresh.id.length).toBeGreaterThan(0);
    expect(fresh.id).not.toBe("keep-me");
    expect(kept.id).toBe("keep-me");
    expect(store().inputProducts.obs).toEqual(obsBefore);
    expect(store().realtime.nav_streams).toEqual(["NEW0", "serial:///dev/ttyUSB1"]);
  });
});

describe("caster_url derivation", () => {
  it("follows the product types, not the values", () => {
    store().setCasterUrl("https://c.example/");

    store().replaceSection("obs", [{ type: "serial", value: "/dev/ttyUSB0" }]);
    store().replaceSection("corr", [{ type: "serial", value: "/dev/ttyUSB1" }]);
    expect(store().realtime.caster_url).toBe("");

    store().replaceSection("obs", []);
    store().replaceSection("corr", []);
    store().addProduct("obs", "http", "https://h.example/x");
    expect(store().realtime.caster_url).toBe("");

    store().addProduct("corr", "ntrip", "SSRA00CNE0");
    expect(store().realtime.caster_url).toBe("https://c.example/");
  });
});

describe("placeholder pass-through", () => {
  it("keeps an empty ntrip value as '' and an empty serial value as 'serial://'", () => {
    store().replaceSection("obs", [
      { type: "ntrip", value: "" },
      { type: "serial", value: "" },
    ]);
    expect(store().realtime.obs_streams).toEqual(["", "serial://"]);
  });
});

describe("updateRealtime shim keeps product identity", () => {
  it("keeps the id of a product whose mountpoint changes", () => {
    const id = store().inputProducts.obs[0].id;
    store().updateRealtime({ obs_streams: ["OTHER0"] });
    expect(store().inputProducts.obs[0]).toEqual({ id, type: "ntrip", value: "OTHER0" });
  });

  it("keeps a serial product's settings when its URI changes", () => {
    const custom = { ...DEFAULT_SERIAL_SETTINGS, baud: 9600 };
    const id = store().addProduct("obs", "serial", "/dev/ttyUSB0");
    store().updateProduct("obs", id, { serial: custom });

    store().updateRealtime({ obs_streams: ["ALIC00AUS0", "serial:///dev/ttyUSB7"] });

    expect(store().inputProducts.obs[1]).toEqual({
      id,
      type: "serial",
      value: "/dev/ttyUSB7",
      serial: custom,
    });
    expect(store().streamSerial.obs[1]).toEqual(custom);
  });
});

describe("setInputMode('file') reset", () => {
  it("drops serial settings and restores the shipped product defaults", () => {
    const custom = { ...DEFAULT_SERIAL_SETTINGS, baud: 9600 };
    store().setInputMode("realtime");
    store().addProduct("obs", "serial", "/dev/ttyUSB0");
    store().setStreamSerial("obs", [custom]);
    expect(store().streamSerial.obs[0]).toEqual(custom);

    store().setInputMode("file");

    expect(store().streamSerial.obs).toEqual([DEFAULT_SERIAL_SETTINGS]);
    expect(store().streamSerial.corr).toEqual([DEFAULT_SERIAL_SETTINGS, DEFAULT_SERIAL_SETTINGS]);
    expect({
      obs: store().inputProducts.obs.map(shape),
      corr: store().inputProducts.corr.map(shape),
    }).toEqual({
      obs: [{ type: "ntrip", value: "ALIC00AUS0" }],
      corr: [
        { type: "ntrip", value: "BCEP00BKG0" },
        { type: "ntrip", value: "SSRA00CNE0" },
      ],
    });
  });
});

describe("serialForProducts", () => {
  it("matches serialByDevice over the derived lists, first listing wins for a duplicate device", () => {
    const obsSettings = { ...DEFAULT_SERIAL_SETTINGS, baud: 9600 };
    const corrSettings = { ...DEFAULT_SERIAL_SETTINGS, baud: 115200 };
    store().replaceSection("obs", [
      { type: "ntrip", value: "ALIC00AUS0" },
      { type: "serial", value: "/dev/ttyUSB0", serial: obsSettings },
      { type: "http", value: "https://h.example/x" },
    ]);
    store().replaceSection("corr", [
      { type: "serial", value: "/dev/ttyUSB0", serial: corrSettings },
      { type: "serial", value: "/dev/ttyUSB1" },
      { type: "file", value: "" },
    ]);

    const { realtime, streamSerial, inputProducts } = store();
    const expected = serialByDevice(
      [realtime.obs_streams, streamSerial.obs],
      [realtime.nav_streams, streamSerial.corr],
    );

    expect(serialForProducts(inputProducts)).toEqual(expected);
    expect(expected).toEqual({
      "/dev/ttyUSB0": obsSettings,
      "/dev/ttyUSB1": DEFAULT_SERIAL_SETTINGS,
    });
  });
});

describe("Corrections roles", () => {
  const corr = (value: string) => store().inputProducts.corr.find((p) => p.value === value)!;
  /** A stream test that saw `families` on each mount. */
  const tested = (streams: Record<string, Record<string, number>>) =>
    makeStreamTestResult({
      snapshot: {
        ...makeStreamTestResult().snapshot,
        streams: Object.fromEntries(Object.entries(streams).map(([m, families]) => [m, { families }])),
      } as StreamTestResult["snapshot"],
    });

  it("records what the stream test saw on each Corrections stream", () => {
    store().addProduct("corr", "ntrip", "MYCORR");
    store().applyStreamTest(tested({ MYCORR: { brdcEph: 3, ssrClk: 2 }, ALIC00AUS0: { MSM: 40 } }));
    expect(corr("MYCORR").detected).toEqual(["bce", "ssr"]);
    // Streams the test did not reach keep no finding.
    expect(corr("BCEP00BKG0").detected).toBeUndefined();
  });

  it("forgets the finding when the stream's mountpoint changes", () => {
    const id = store().addProduct("corr", "ntrip", "MYCORR");
    store().applyStreamTest(tested({ MYCORR: { brdcEph: 3 } }));
    store().updateProduct("corr", id, { value: "OTHER" });
    expect(corr("OTHER").detected).toBeUndefined();
  });

  it("remembers the group a stream was added under, and nothing otherwise", () => {
    store().addProduct("corr", "ntrip", "MYCORR", "ssr");
    expect(corr("MYCORR").role).toBe("ssr");
    store().addProduct("corr", "ntrip", "OTHER");
    expect("role" in corr("OTHER")).toBe(false);
  });

  it("sends a stream added under SSR only while there is no SBAS", () => {
    store().addProduct("corr", "ntrip", "MYCORR", "ssr");
    expect(store().realtime.nav_streams).toContain("MYCORR");
    store().addProduct("corr", "sbas");
    expect(store().realtime.nav_streams).toEqual(["BCEP00BKG0"]);
  });
});
