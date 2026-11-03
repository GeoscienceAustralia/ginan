import { beforeEach, describe, expect, it } from "vitest";
import { act, renderHook, waitFor } from "@testing-library/react";
import { BASE_TITLE, setAppTitle, useAppTitle } from "./useAppTitle";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { setTitle } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  document.title = BASE_TITLE;
});

describe("setAppTitle", () => {
  it("sets both the document title and the native window title", async () => {
    await setAppTitle("Ginan UI - ALIC.rnx");
    expect(document.title).toBe("Ginan UI - ALIC.rnx");
    expect(setTitle).toHaveBeenCalledWith("Ginan UI - ALIC.rnx");
  });

  it("still sets the HTML title when the native call is unavailable", async () => {
    setTitle.mockRejectedValueOnce(new Error("no permission"));
    await expect(setAppTitle("Ginan UI - ALIC.rnx")).resolves.toBeUndefined();
    expect(document.title).toBe("Ginan UI - ALIC.rnx");
  });
});

describe("useAppTitle", () => {
  it("leaves the title alone until a file is parsed", () => {
    renderHook(() => useAppTitle());
    expect(document.title).toBe(BASE_TITLE);
  });

  it("appends the loaded file's name", async () => {
    store().setRinexPath("/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx");
    renderHook(() => useAppTitle());

    act(() => store().setRinexData(makeRinexData()));

    await waitFor(() =>
      expect(document.title).toBe(
        `${BASE_TITLE} - ALIC00AUS_R_20240020000_01D_30S_MO.rnx`,
      ),
    );
  });

  it("takes the basename off a Windows path too", async () => {
    store().setRinexPath("C:\\data\\ALIC.rnx");
    renderHook(() => useAppTitle());

    act(() => store().setRinexData(makeRinexData()));

    await waitFor(() => expect(document.title).toBe(`${BASE_TITLE} - ALIC.rnx`));
  });

  it("falls back to the marker name when there is no path", async () => {
    renderHook(() => useAppTitle());

    act(() => store().setRinexData(makeRinexData()));

    await waitFor(() => expect(document.title).toBe(`${BASE_TITLE} - ALIC00AUS`));
  });

  it("follows a swap to a different file", async () => {
    store().setRinexPath("/data/first.rnx");
    renderHook(() => useAppTitle());
    act(() => store().setRinexData(makeRinexData()));
    await waitFor(() => expect(document.title).toContain("first.rnx"));

    store().setRinexPath("/data/second.rnx");
    act(() => store().setRinexData(makeRinexData({ marker_name: "OTHR" })));

    await waitFor(() => expect(document.title).toBe(`${BASE_TITLE} - second.rnx`));
  });
});
