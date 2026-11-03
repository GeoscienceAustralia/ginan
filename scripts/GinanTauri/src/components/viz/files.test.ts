import { describe, expect, it } from "vitest";
import type { PosFileInfo, TraceFileInfo } from "../../types";
import { pickDefaultPos, posFileLabel, traceFileLabel } from "./files";
import { deviceTimeZone } from "../../utils/time";

/** The fixtures build write times in device-local time, so label in that zone. */
const TZ = deviceTimeZone();

const f = (over: Partial<PosFileInfo> & { name: string }): PosFileInfo => ({
  path: `/out/${over.name}`,
  format: "pos",
  smoothed: false,
  size: 100_000,
  modified_ms: null,
  has_epochs: true,
  ...over,
});

const t = (over: Partial<TraceFileInfo> & { name: string }): TraceFileInfo => ({
  path: `/out/${over.name}`,
  size: 100_000,
  modified_ms: null,
  has_residuals: true,
  ...over,
});

describe("pickDefaultPos", () => {
  it("returns null for no files", () => {
    expect(pickDefaultPos([])).toBeNull();
  });

  it("prefers the newest forward solution over an older or smoothed one", () => {
    const files = [
      f({ name: "old.pos", modified_ms: 1_000 }),
      f({ name: "new_smoothed.pos", smoothed: true, modified_ms: 9_000 }),
      f({ name: "new.pos", modified_ms: 5_000 }),
    ];
    expect(pickDefaultPos(files)).toBe("/out/new.pos");
  });

  it("falls back to a smoothed solution when there is no forward one", () => {
    const files = [f({ name: "fwd_smoothed.pos", smoothed: true, modified_ms: 1_000 })];
    expect(pickDefaultPos(files)).toBe("/out/fwd_smoothed.pos");
  });

  it("treats a missing write time as oldest", () => {
    const files = [f({ name: "unknown.pos" }), f({ name: "dated.pos", modified_ms: 1 })];
    expect(pickDefaultPos(files)).toBe("/out/dated.pos");
  });

  it("does not reorder the caller's array", () => {
    const files = [f({ name: "a.pos", modified_ms: 1 }), f({ name: "b.pos", modified_ms: 2 })];
    pickDefaultPos(files);
    expect(files.map((x) => x.name)).toEqual(["a.pos", "b.pos"]);
  });
});

describe("posFileLabel", () => {
  it("leads with forward or smoothed, then the write time, so a truncated select still tells files apart", () => {
    const ms = new Date(2024, 2, 7, 9, 5).getTime();
    expect(posFileLabel(f({ name: "ALIC_cfg_202400100.POS", modified_ms: ms }), TZ)).toBe(
      "Forward · 7 Mar 09:05 · ALIC_cfg_202400100 · 98 KB",
    );
    expect(
      posFileLabel(f({ name: "ALIC_cfg_202400100_smoothed.POS", smoothed: true, modified_ms: ms }), TZ),
    ).toBe("Smoothed · 7 Mar 09:05 · ALIC_cfg_202400100 · 98 KB");
    expect(posFileLabel(f({ name: "ALIC_202630001.SPP", format: "spp", modified_ms: ms }), TZ)).toBe(
      "SBAS SPP · 7 Mar 09:05 · ALIC_202630001 · 98 KB",
    );
  });

  it("formats sizes in B, KB and MB, and leaves out an unknown write time", () => {
    expect(posFileLabel(f({ name: "a.pos", size: 900 }), TZ)).toBe("Forward · a · 900 B");
    expect(posFileLabel(f({ name: "a.pos", size: 4097 }), TZ)).toBe("Forward · a · 4 KB");
    expect(posFileLabel(f({ name: "a.pos", size: 5 * 1024 * 1024 + 300_000 }), TZ)).toBe("Forward · a · 5.3 MB");
  });
});

describe("labels in the chosen time zone", () => {
  it("shows the write time in the selected zone, not the device's", () => {
    const ms = Date.UTC(2024, 2, 7, 9, 5);
    expect(posFileLabel(f({ name: "a.pos", size: 900, modified_ms: ms }), "UTC")).toBe(
      "Forward · 7 Mar 09:05 · a · 900 B",
    );
    // AEDT (UTC+11) in early March.
    expect(posFileLabel(f({ name: "a.pos", size: 900, modified_ms: ms }), "Australia/Sydney")).toBe(
      "Forward · 7 Mar 20:05 · a · 900 B",
    );
    expect(traceFileLabel(t({ name: "a.TRACE", size: 900, modified_ms: ms }), "UTC")).toBe(
      "7 Mar 09:05 · a · 900 B",
    );
  });
});

describe("traceFileLabel", () => {
  it("marks a smoothed trace and otherwise leads with the write time", () => {
    const ms = new Date(2024, 2, 7, 9, 5).getTime();
    expect(traceFileLabel(t({ name: "cfg_ALIC_202400100.TRACE", modified_ms: ms }), TZ)).toBe(
      "7 Mar 09:05 · cfg_ALIC_202400100 · 98 KB",
    );
    expect(traceFileLabel(t({ name: "cfg_ALIC_202400100_smoothed.TRACE" }), TZ)).toBe(
      "Smoothed · cfg_ALIC_202400100 · 98 KB",
    );
  });
});
