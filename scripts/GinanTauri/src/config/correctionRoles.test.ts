import { describe, expect, it } from "vitest";
import type { InputProduct } from "../types";
import {
  correctionCoverage,
  correctionRoles,
  isPausedBySbas,
  roleFromName,
  rolesFromFamilies,
  streamMount,
} from "./correctionRoles";

const p = (value: string, over: Partial<InputProduct> = {}): InputProduct => ({ id: value, type: "ntrip", value, ...over });

describe("streamMount", () => {
  it("is the last path segment of an NTRIP or HTTP value, or a serial device's name", () => {
    expect(streamMount(p("BCEP00BKG0"))).toBe("BCEP00BKG0");
    expect(streamMount(p("https://user:pw@host:2101/SSRA00CNE0", { type: "http" }))).toBe("SSRA00CNE0");
    expect(streamMount(p("/dev/ttyUSB1", { type: "serial" }))).toBe("ttyUSB1");
    expect(streamMount(p(""))).toBe("");
  });
});

describe("roleFromName", () => {
  it("knows the IGS / GA names and guesses nothing else", () => {
    expect(roleFromName("BCEP00BKG0")).toBe("bce");
    expect(roleFromName("brdc00wrd0")).toBe("bce");
    expect(roleFromName("SSRA00CNE0")).toBe("ssr");
    expect(roleFromName("SSRC00BKG0")).toBe("ssr");
    expect(roleFromName("MYCORR")).toBeNull();
  });
});

describe("rolesFromFamilies", () => {
  it("reads broadcast ephemeris and any SSR family, ignoring observations", () => {
    expect(rolesFromFamilies({ brdcEph: 4 })).toEqual(["bce"]);
    expect(rolesFromFamilies({ ssrClk: 2, ssrOrb: 2 })).toEqual(["ssr"]);
    expect(rolesFromFamilies({ brdcEph: 1, ssrCodeBias: 1 })).toEqual(["bce", "ssr"]);
    expect(rolesFromFamilies({ MSM: 90, brdcEph: 0 })).toEqual([]);
  });
});

describe("correctionRoles", () => {
  it("takes the stream test, then the name, then the group it was added under", () => {
    expect(correctionRoles(p("SSRA00CNE0", { role: "bce", detected: ["bce", "ssr"] }))).toEqual(["bce", "ssr"]);
    // Added under the wrong group: the name still files it where it belongs.
    expect(correctionRoles(p("SSRA00CNE0", { role: "bce" }))).toEqual(["ssr"]);
    expect(correctionRoles(p("MYCORR", { role: "bce" }))).toEqual(["bce"]);
    expect(correctionRoles(p("MYCORR"))).toEqual([]);
  });
});

describe("isPausedBySbas", () => {
  it("pauses SSR-only streams, never ephemeris, mixed or unknown ones", () => {
    expect(isPausedBySbas(p("SSRA00CNE0"), true)).toBe(true);
    expect(isPausedBySbas(p("SSRA00CNE0"), false)).toBe(false);
    expect(isPausedBySbas(p("BCEP00BKG0"), true)).toBe(false);
    expect(isPausedBySbas(p("MIX", { detected: ["bce", "ssr"] }), true)).toBe(false);
    expect(isPausedBySbas(p("MYCORR"), true)).toBe(false);
  });
});

describe("correctionCoverage", () => {
  const sbas: InputProduct = { id: "s", type: "sbas", value: "PVS" };

  it("needs ephemeris and SSR, or ephemeris and SBAS", () => {
    expect(correctionCoverage([p("BCEP00BKG0"), p("SSRA00CNE0")])).toMatchObject({ bce: true, corrections: true });
    expect(correctionCoverage([p("BCEP00BKG0"), sbas])).toMatchObject({ bce: true, corrections: true, sbas: true });
    expect(correctionCoverage([p("SSRA00CNE0")])).toMatchObject({ bce: false, corrections: true });
    expect(correctionCoverage([p("BCEP00BKG0")])).toMatchObject({ bce: true, corrections: false });
    expect(correctionCoverage([sbas])).toMatchObject({ bce: false, corrections: true });
  });

  it("lets one stream carrying both cover both", () => {
    expect(correctionCoverage([p("MIX", { detected: ["bce", "ssr"] })])).toMatchObject({ bce: true, corrections: true });
  });

  it("lists the streams it cannot place, and ignores files", () => {
    const unknown = p("MYCORR");
    const cover = correctionCoverage([p("BCEP00BKG0"), unknown, { id: "f", type: "file", value: "/x.rnx" }]);
    expect(cover.unknown).toEqual([unknown]);
  });
});
