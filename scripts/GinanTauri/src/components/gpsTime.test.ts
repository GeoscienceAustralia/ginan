import { describe, expect, it } from "vitest";
import {
  dayOfYear,
  formatEpoch,
  gpsMinusUtc,
  gpsWeek,
  parseEpoch,
  stepEpoch,
  toStoreEpoch,
} from "./gpsTime";

const at = (s: string) => parseEpoch(s)!;

describe("parseEpoch", () => {
  it("reads PEA's form, the store's form and a bare date", () => {
    const ms = Date.UTC(2024, 0, 2, 3, 4, 5);
    expect(parseEpoch("2024-01-02 03:04:05")).toBe(ms);
    expect(parseEpoch("2024-01-02T03:04:05")).toBe(ms);
    expect(parseEpoch("2024-01-02_03:04:05")).toBe(ms);
    expect(parseEpoch("2024-01-02T03:04")).toBe(Date.UTC(2024, 0, 2, 3, 4));
    expect(parseEpoch("2024-01-02")).toBe(Date.UTC(2024, 0, 2));
  });

  it("rejects out-of-range fields rather than rolling them over", () => {
    expect(parseEpoch("2024-02-30 00:00:00")).toBeNull();
    expect(parseEpoch("2024-01-02 24:00:00")).toBeNull();
    expect(parseEpoch("2024-13-01")).toBeNull();
    expect(parseEpoch("yesterday")).toBeNull();
    expect(parseEpoch("")).toBeNull();
  });
});

describe("formatting", () => {
  it("round-trips without touching the local time zone", () => {
    expect(formatEpoch(at("2024-01-02 23:59:30"))).toBe("2024-01-02 23:59:30");
    expect(toStoreEpoch(at("2024-01-02 23:59:30"))).toBe("2024-01-02T23:59:30");
  });
});

describe("GPS calendar", () => {
  it("counts weeks from 1980-01-06", () => {
    expect(gpsWeek(at("1980-01-06 00:00:00"))).toEqual({ week: 0, day: 0, sow: 0 });
    // 2024-01-02 is a Tuesday in GPS week 2295.
    expect(gpsWeek(at("2024-01-02 00:00:00"))).toEqual({ week: 2295, day: 2, sow: 172_800 });
  });

  it("numbers days of the year from one", () => {
    expect(dayOfYear(at("2024-01-01"))).toBe(1);
    expect(dayOfYear(at("2024-12-31"))).toBe(366);
  });

  it("tracks the leap seconds between GPS time and UTC", () => {
    expect(gpsMinusUtc(at("1980-01-06"))).toBe(0);
    expect(gpsMinusUtc(at("2016-12-31 23:59:59"))).toBe(17);
    // The 2017 leap second lands 18 s into 1 January in GPS time.
    expect(gpsMinusUtc(at("2017-01-01 00:00:17"))).toBe(17);
    expect(gpsMinusUtc(at("2017-01-01 00:00:18"))).toBe(18);
  });
});

describe("stepEpoch", () => {
  it("carries clock fields across midnight", () => {
    expect(formatEpoch(stepEpoch(at("2024-01-01 23:59:59"), "second", 1))).toBe("2024-01-02 00:00:00");
  });

  it("clamps the day when a month step lands on a shorter month", () => {
    expect(formatEpoch(stepEpoch(at("2024-01-31 12:00:00"), "month", 1))).toBe("2024-02-29 12:00:00");
    expect(formatEpoch(stepEpoch(at("2024-01-15 00:00:00"), "month", -1))).toBe("2023-12-15 00:00:00");
  });
});
