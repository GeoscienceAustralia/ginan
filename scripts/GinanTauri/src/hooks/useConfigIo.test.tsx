import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, renderHook, waitFor } from "@testing-library/react";
import { useConfigIo } from "./useConfigIo";
import type { PeaConfigExtract } from "../types";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import {
  invokeCount,
  lastInvokeArgs,
  mockInvoke,
  open,
  save,
} from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

/** Everything `buildConfigInputs` needs before an export is possible. */
function readyToExport() {
  store().setOutputDir("/out");
  store().setRunName("run1");
  store().setProductsDir("/products");
  store().setRinexPath("/data/ALIC.rnx");
  store().setRinexData(makeRinexData());
}

const extract = (over: Partial<PeaConfigExtract> = {}): PeaConfigExtract => ({
  mode: "Kinematic",
  start_epoch: "2024-03-04 01:02:03",
  end_epoch: null,
  epoch_interval: 5,
  output_pos: true,
  output_gpx: null,
  output_trace: null,
  output_snx: null,
  output_spp: null,
  constellation_codes: null,
  rinex_path: null,
  ...over,
});

describe("exportConfig", () => {
  it("refuses, with a reason, before there is anything to export", async () => {
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());

    expect(save).not.toHaveBeenCalled();
    expect(result.current.feedback).toMatchObject({
      row: "export-config",
      tone: "warn",
    });
    expect(result.current.feedback!.text).toMatch(/Nothing to export yet/);
  });

  it("does nothing when the save dialog is cancelled", async () => {
    readyToExport();
    save.mockResolvedValueOnce(null);
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());

    expect(invokeCount("export_pea_config")).toBe(0);
    expect(result.current.feedback).toBeNull();
  });

  it("writes the same inputs a run would use", async () => {
    readyToExport();
    save.mockResolvedValueOnce("/home/me/ppp_config.yaml");
    mockInvoke({ export_pea_config: null });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());

    const args = lastInvokeArgs("export_pea_config")!;
    expect(args.path).toBe("/home/me/ppp_config.yaml");
    expect(args.inputs).toMatchObject({
      rinex_path: "/data/ALIC.rnx",
      output_dir: "/out/run1",
      marker_name: "ALIC",
    });
    expect(result.current.feedback).toMatchObject({
      row: "export-config",
      tone: "ok",
      detail: "/home/me/ppp_config.yaml",
    });
    expect(store().status).toBe("Config exported: ppp_config.yaml");
  });

  it("offers ppp_config.yaml for a file run, the name a run writes", async () => {
    readyToExport();
    save.mockResolvedValueOnce(null);
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());

    expect(save).toHaveBeenCalledWith(
      expect.objectContaining({ defaultPath: "ppp_config.yaml" }),
    );
  });

  it("offers rt_ppp_config.yaml for a real-time run, the name a run writes", async () => {
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    expect(store().realtime.obs_streams.length).toBeGreaterThan(0);
    save.mockResolvedValueOnce("/home/me/rt_ppp_config.yaml");
    mockInvoke({ export_pea_config: null });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());

    expect(save).toHaveBeenCalledWith(
      expect.objectContaining({ defaultPath: "rt_ppp_config.yaml" }),
    );
    // The backend picks the real-time template off this field being present.
    expect(lastInvokeArgs("export_pea_config")!.inputs).toHaveProperty("realtime");
  });

  it("reports a write failure", async () => {
    readyToExport();
    save.mockResolvedValueOnce("/home/me/ppp_config.yaml");
    mockInvoke({ export_pea_config: () => Promise.reject(new Error("read-only volume")) });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());

    expect(result.current.feedback).toMatchObject({ row: "export-config", tone: "error" });
    expect(result.current.feedback!.text).toMatch(/read-only volume/);
  });
});

describe("importConfig", () => {
  it("does nothing when the dialog is cancelled", async () => {
    open.mockResolvedValueOnce(null);
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.importConfig());

    expect(invokeCount("import_pea_config")).toBe(0);
  });

  it("applies the settings from a config with no reachable RINEX", async () => {
    open.mockResolvedValueOnce("/home/me/ppp_config.yaml");
    mockInvoke({ import_pea_config: extract() });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.importConfig());

    expect(store().mode).toBe("Kinematic");
    expect(store().startEpoch).toBe("2024-03-04T01:02:03");
    expect(invokeCount("parse_rinex")).toBe(0);
    expect(result.current.feedback).toMatchObject({ row: "import-config", tone: "ok" });
  });

  it("reloads a RINEX the config points at and re-applies the stored overrides", async () => {
    open.mockResolvedValueOnce("/home/me/ppp_config.yaml");
    mockInvoke({
      import_pea_config: extract({ rinex_path: "/data/ALIC.rnx", epoch_interval: 5 }),
      parse_rinex: makeRinexData(),
    });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.importConfig());

    expect(store().rinexPath).toBe("/data/ALIC.rnx");
    expect(store().rinexData).not.toBeNull();
    // setRinexData reset these from the header; the imported values win again.
    expect(store().startEpoch).toBe("2024-03-04T01:02:03");
    expect(store().epochInterval).toBe(5);
    expect(result.current.feedback!.text).toMatch(/RINEX reloaded/);
  });

  it("still applies the settings when the RINEX is missing on this machine", async () => {
    open.mockResolvedValueOnce("/home/me/ppp_config.yaml");
    mockInvoke({
      import_pea_config: extract({ rinex_path: "/gone/ALIC.rnx" }),
      parse_rinex: () => Promise.reject(new Error("no such file")),
    });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.importConfig());

    expect(store().mode).toBe("Kinematic");
    expect(result.current.feedback).toMatchObject({ row: "import-config", tone: "warn" });
    expect(result.current.feedback!.text).toMatch(/could not be reloaded/);
  });

  it("reports a failure to read the config at all", async () => {
    open.mockResolvedValueOnce("/home/me/broken.yaml");
    mockInvoke({ import_pea_config: () => Promise.reject(new Error("invalid YAML")) });
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.importConfig());

    expect(result.current.feedback).toMatchObject({ row: "import-config", tone: "error" });
    expect(store().status).toMatch(/Import error/);
  });
});

describe("resetConfig", () => {
  it("resets the processing settings and says so", () => {
    store().setMode("Kinematic");
    const { result } = renderHook(() => useConfigIo());

    act(() => result.current.resetConfig());

    expect(store().mode).toBe("Static");
    expect(result.current.feedback).toMatchObject({ row: "reset-config", tone: "ok" });
  });
});

describe("feedback lifetime", () => {
  it("expires on its own so a message does not sit there until the next action", async () => {
    vi.useFakeTimers();
    try {
      const { result } = renderHook(() => useConfigIo());

      await act(() => result.current.exportConfig());
      expect(result.current.feedback).not.toBeNull();

      act(() => void vi.advanceTimersByTime(8000));
      expect(result.current.feedback).toBeNull();
    } finally {
      vi.useRealTimers();
    }
  });

  it("can be dismissed by hand", async () => {
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());
    act(() => result.current.clearFeedback());

    expect(result.current.feedback).toBeNull();
  });

  it("replaces the previous message rather than queueing two", async () => {
    const { result } = renderHook(() => useConfigIo());

    await act(() => result.current.exportConfig());
    act(() => result.current.resetConfig());

    expect(result.current.feedback!.row).toBe("reset-config");
  });

  it("reads the store at click time, not at render time", async () => {
    const { result } = renderHook(() => useConfigIo());
    // Rendered with nothing loaded; the file arrives afterwards.
    readyToExport();
    save.mockResolvedValueOnce("/home/me/ppp_config.yaml");
    mockInvoke({ export_pea_config: null });

    await act(() => result.current.exportConfig());

    await waitFor(() => expect(invokeCount("export_pea_config")).toBe(1));
  });
});
