import { describe, expect, it } from "vitest";
import {
  allRinexPaths,
  distinctMarkers,
  rinexSpan,
  selectRinexEnd,
  selectRinexStart,
  withRinexSpan,
} from "./rinexFiles";
import { makeRinexData } from "../test/storeHelpers";

/** One calendar day of RINEX starting at midnight on `d` January 2024. */
function day(d: number, over = {}) {
  const dd = String(d).padStart(2, "0");
  return makeRinexData({
    start_epoch: `2024-01-${dd} 00:00:00`,
    end_epoch: `2024-01-${dd} 23:59:30`,
    ...over,
  });
}

describe("rinexSpan", () => {
  it("is the earliest start to the latest end, whatever order the files arrive in", () => {
    expect(rinexSpan([day(3), day(1), day(2)])).toEqual({
      start: "2024-01-01 00:00:00",
      end: "2024-01-03 23:59:30",
    });
  });

  it("is just the file itself for a single file", () => {
    expect(rinexSpan([day(2)])).toEqual({
      start: "2024-01-02 00:00:00",
      end: "2024-01-02 23:59:30",
    });
  });

  it("skips a file with no epochs instead of losing the whole span", () => {
    const blank = makeRinexData({ start_epoch: null, end_epoch: null });
    expect(rinexSpan([blank, day(2)])).toEqual({
      start: "2024-01-02 00:00:00",
      end: "2024-01-02 23:59:30",
    });
  });

  it("is null, null with nothing to measure", () => {
    expect(rinexSpan([])).toEqual({ start: null, end: null });
  });

  it("covers overlapping files from different stations", () => {
    const a = day(2, { start_epoch: "2024-01-02 06:00:00", end_epoch: "2024-01-02 18:00:00" });
    const b = day(2, { start_epoch: "2024-01-02 00:00:00", end_epoch: "2024-01-02 12:00:00" });
    expect(rinexSpan([a, b])).toEqual({
      start: "2024-01-02 00:00:00",
      end: "2024-01-02 18:00:00",
    });
  });
});

describe("withRinexSpan", () => {
  it("returns the primary untouched when there are no extras", () => {
    const primary = day(2);
    expect(withRinexSpan(primary, [])).toBe(primary);
  });

  it("widens only the window and keeps every header-derived field of the primary", () => {
    const primary = day(2, { marker_name: "ALIC00AUS", receiver_type: "SEPT" });
    const widened = withRinexSpan(primary, [
      { path: "/d/b.rnx", data: day(3, { marker_name: "DARW00AUS", receiver_type: "TRIMBLE" }) },
    ]);
    expect(widened.start_epoch).toBe("2024-01-02 00:00:00");
    expect(widened.end_epoch).toBe("2024-01-03 23:59:30");
    expect(widened.marker_name).toBe("ALIC00AUS");
    expect(widened.receiver_type).toBe("SEPT");
  });
});

describe("allRinexPaths", () => {
  it("lists the primary first and then the extras in order", () => {
    expect(
      allRinexPaths({
        rinexPath: "/d/a.rnx",
        extraRinex: [
          { path: "/d/b.rnx", data: day(2) },
          { path: "/d/c.rnx", data: day(3) },
        ],
      }),
    ).toEqual(["/d/a.rnx", "/d/b.rnx", "/d/c.rnx"]);
  });

  it("is empty with no primary, whatever is left in the extras", () => {
    expect(
      allRinexPaths({ rinexPath: null, extraRinex: [{ path: "/d/b.rnx", data: day(2) }] }),
    ).toEqual([]);
  });
});

describe("selectRinexStart / selectRinexEnd", () => {
  it("read the window of the whole selection", () => {
    const state = {
      rinexPath: "/d/a.rnx",
      rinexData: day(2),
      extraRinex: [{ path: "/d/b.rnx", data: day(5) }],
    };
    expect(selectRinexStart(state)).toBe("2024-01-02 00:00:00");
    expect(selectRinexEnd(state)).toBe("2024-01-05 23:59:30");
  });

  it("are null with no file loaded", () => {
    const state = { rinexPath: null, rinexData: null, extraRinex: [] };
    expect(selectRinexStart(state)).toBeNull();
    expect(selectRinexEnd(state)).toBeNull();
  });
});

describe("distinctMarkers", () => {
  it("reports one station once, however many days of it there are", () => {
    expect(distinctMarkers([day(1), day(2), day(3)])).toEqual(["ALIC"]);
  });

  it("reports each station in file order", () => {
    expect(
      distinctMarkers([
        day(1, { marker_name: "ALIC00AUS" }),
        day(1, { marker_name: "DARW00AUS" }),
        day(2, { marker_name: "ALIC00AUS" }),
      ]),
    ).toEqual(["ALIC", "DARW"]);
  });

  it("compares the four characters PEA keys a receiver on, case-insensitively", () => {
    expect(
      distinctMarkers([day(1, { marker_name: "alic" }), day(2, { marker_name: "ALIC00AUS" })]),
    ).toEqual(["ALIC"]);
  });

  it("ignores a file with no marker rather than counting it as a station", () => {
    expect(distinctMarkers([day(1), day(2, { marker_name: null })])).toEqual(["ALIC"]);
  });
});
