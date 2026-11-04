import { act, renderHook } from "@testing-library/react";
import { beforeEach, describe, expect, it } from "vitest";
import { LIVE_PRODUCT_TYPES } from "../components/RealtimeConfigPanel";
import { switchToFile, switchToLive, useProductAdd } from "./useProductAdd";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

/** Products without their generated ids, for comparing against the defaults. */
const streams = (list: { type: string; value: string }[]) =>
  list.map(({ type, value }) => ({ type, value }));

const DEFAULT_OBS = [{ type: "ntrip", value: "ALIC00AUS0" }];
const DEFAULT_CORR = [
  { type: "ntrip", value: "BCEP00BKG0" },
  { type: "ntrip", value: "SSRA00CNE0" },
];

describe("switchToLive", () => {
  it("starts from the default live products in both sections when given no type", () => {
    store().setRinexPath("/data/ALIC.rnx");

    switchToLive();

    expect(store().inputMode).toBe("realtime");
    expect(store().rinexPath).toBeNull();
    expect(streams(store().inputProducts.obs)).toEqual(DEFAULT_OBS);
    expect(streams(store().inputProducts.corr)).toEqual(DEFAULT_CORR);
  });

  it("holds just the new empty product on Observation when given a type", () => {
    switchToLive("serial");

    expect(store().inputMode).toBe("realtime");
    expect(streams(store().inputProducts.obs)).toEqual([{ type: "serial", value: "" }]);
    expect(streams(store().inputProducts.corr)).toEqual(DEFAULT_CORR);
  });
});

describe("switchToFile", () => {
  it("sets the file run and resets the products to the defaults", () => {
    switchToLive("serial");

    switchToFile();

    expect(store().inputMode).toBe("file");
    expect(streams(store().inputProducts.obs)).toEqual(DEFAULT_OBS);
    expect(streams(store().inputProducts.corr)).toEqual(DEFAULT_CORR);
  });
});

describe("HTTP as a live product", () => {
  it("is a live type: enabled in Observation, and a file run crossing to it goes live", () => {
    expect(LIVE_PRODUCT_TYPES).toContain("http");
    switchToFile();
    const { result } = renderHook(() => useProductAdd("obs", LIVE_PRODUCT_TYPES));
    expect(result.current.enabledTypes).toContain("http");

    act(() => result.current.onAdd("http")); // crossing: asks first
    expect(store().inputMode).toBe("file");
    expect(result.current.dialog).toBeTruthy();
  });

  it("switchToLive('http') makes a live run holding just an empty HTTP product", () => {
    switchToFile();
    switchToLive("http");
    expect(store().inputMode).toBe("realtime");
    expect(streams(store().inputProducts.obs)).toEqual([{ type: "http", value: "" }]);
  });

  it("is blocked in Corrections during a file run, enabled in a live run", () => {
    switchToFile();
    const file = renderHook(() => useProductAdd("corr", LIVE_PRODUCT_TYPES));
    expect(file.result.current.enabledTypes).toEqual([]);
    act(() => switchToLive());
    const live = renderHook(() => useProductAdd("corr", LIVE_PRODUCT_TYPES));
    expect(live.result.current.enabledTypes).toContain("http");
  });
});

describe("while PEA is processing", () => {
  it("greys the types that would switch run kind, with a reason", () => {
    switchToFile();
    store().setIsProcessing(true);
    const { result } = renderHook(() => useProductAdd("obs", LIVE_PRODUCT_TYPES));
    expect(result.current.enabledTypes).toEqual(["file"]);
    expect(result.current.disabledReason).toMatch(/stop processing/i);
  });

  it("setInputMode and switchToLive refuse to change the mode", () => {
    switchToFile();
    store().setRinexPath("/data/ALIC.rnx");
    store().setIsProcessing(true);

    store().setInputMode("realtime");
    switchToLive("serial");

    expect(store().inputMode).toBe("file");
    expect(store().rinexPath).toBe("/data/ALIC.rnx");
  });
});
