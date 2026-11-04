import { describe, expect, it } from "vitest";
import type { InputProduct } from "../types";
import { isForeignPath, runBlockers, type RunGateState } from "./runGate";

const stream = (value: string, over: Partial<InputProduct> = {}): InputProduct => ({ id: value || "new", type: "ntrip", value, ...over });

/** A live run that can start, with `corr` as its Corrections. */
const live = (corr: InputProduct[]): RunGateState => ({
  peaBinaryPath: "/pea",
  outputDir: "/out",
  productsDir: "/products",
  rinexPath: null,
  startEpoch: null,
  cddisStatus: "idle",
  inputMode: "realtime",
  realtime: { obs_streams: ["ALIC00AUS0"], caster_url: "https://caster.example/" },
  inputProducts: { obs: [{ type: "ntrip" }], corr },
});
const ids = (s: RunGateState) => runBlockers(s).map((b) => b.id);

describe("live run corrections", () => {
  it("passes with broadcast ephemeris and SSR, or with broadcast ephemeris and SBAS", () => {
    expect(ids(live([stream("BCEP00BKG0"), stream("SSRA00CNE0")]))).toEqual([]);
    expect(ids(live([stream("BCEP00BKG0"), { id: "s", type: "sbas", value: "PVS" }]))).toEqual([]);
  });

  it("asks for broadcast ephemeris, even with SBAS", () => {
    expect(ids(live([stream("SSRA00CNE0")]))).toEqual(["bce-stream"]);
    expect(ids(live([{ id: "s", type: "sbas", value: "PVS" }]))).toEqual(["bce-stream"]);
  });

  it("asks for SSR or SBAS", () => {
    expect(ids(live([stream("BCEP00BKG0")]))).toEqual(["ssr-stream"]);
  });

  it("names a stream it cannot place, and says so for one with no mountpoint", () => {
    const blockers = runBlockers(live([stream("BCEP00BKG0"), stream("SSRA00CNE0"), stream("MYCORR"), stream("")]));
    expect(blockers.map((b) => b.id)).toEqual(["role-MYCORR", "role-new"]);
    expect(blockers[0].message).toContain("MYCORR");
    expect(blockers[1].message).toContain("no mountpoint");
  });

  it("places a stream nothing identifies by the group it was added under", () => {
    expect(ids(live([stream("MYEPH", { role: "bce" }), stream("SSRA00CNE0")]))).toEqual([]);
  });

  it("does not apply to a file run", () => {
    const s = live([]);
    expect(ids({ ...s, inputMode: "file", rinexPath: "/a.rnx", startEpoch: "2024-01-01T00:00:00" })).toEqual([]);
  });
});

describe("isForeignPath", () => {
  it("flags a Unix path on Windows, where it would land under the C drive", () => {
    expect(isForeignPath("/home/user/Downloads/outputs", true)).toBe(true);
    expect(isForeignPath("C:\\Users\\me\\outputs", true)).toBe(false);
    expect(isForeignPath("D:/data/outputs", true)).toBe(false);
    expect(isForeignPath("\\\\server\\share\\outputs", true)).toBe(false);
  });

  it("flags a drive path off Windows", () => {
    expect(isForeignPath("C:\\Users\\me\\outputs", false)).toBe(true);
    expect(isForeignPath("/home/me/outputs", false)).toBe(false);
  });

  it("leaves an empty path to the 'not set' check", () => {
    expect(isForeignPath("", true)).toBe(false);
  });
});

describe("runBlockers", () => {
  const base: RunGateState = {
    peaBinaryPath: "pea",
    outputDir: null,
    productsDir: null,
    rinexPath: null,
    startEpoch: null,
    cddisStatus: "ok",
    inputMode: "realtime",
    realtime: { obs_streams: ["ALIC00AUS0"], caster_url: "https://caster" },
    inputProducts: {
      obs: [{ type: "ntrip" }],
      corr: [
        { id: "b", type: "ntrip", value: "BCEP00BKG0" },
        { id: "s", type: "ntrip", value: "SSRA00CNE0" },
      ],
    },
  };

  it("says an output directory from another computer cannot be used", () => {
    // Whichever platform the tests run on, one of these two is foreign there.
    const foreign = isForeignPath("/home/user/outputs") ? "/home/user/outputs" : "C:\\outputs";
    const blockers = runBlockers({ ...base, outputDir: foreign });
    expect(blockers.map((b) => b.id)).toContain("output-foreign");
    expect(blockers.find((b) => b.id === "output-foreign")!.fix).toMatchObject({ kind: "settings", page: "output" });
  });
});
