import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import RunControls from "./RunControls";
import { useAppStore, streamFingerprint } from "../store/appStore";
import {
  makeCatalogue,
  makeObsCodes,
  makeRinexData,
  makeStreamTestResult,
  resetAppStore,
  store,
} from "../test/storeHelpers";
import { DEFAULT_SERIAL_SETTINGS } from "../types";
import { WF } from "../config/workflowSteps";
import { invoke, invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

const processBtn = () => screen.getByRole("button", { name: /Process/ });
const stopBtn = () => screen.getByRole("button", { name: /Stop/ });

/** Everything a file run needs before Process becomes pressable. */
function readyForFileRun() {
  useAppStore.setState({ peaBinaryPath: "/bin/pea" });
  store().setOutputDir("/out");
  store().setProductsDir("/products");
  store().setRinexPath("/data/file.rnx");
  store().setRinexData(makeRinexData());
  store().setCddisStatus("ok");
}

function readyForRealtimeRun() {
  useAppStore.setState({ peaBinaryPath: "/bin/pea" });
  store().setOutputDir("/out");
  store().setRunName("run1");
  store().setInputMode("realtime");
  store().updateRealtime({ obs_streams: ["ALIC00AUS0"] });
  markStreamTestPassed();
}

/** Pretend Monitor stream passed for the current settings. */
function markStreamTestPassed() {
  useAppStore.setState({
    streamTestStatus: "passed",
    streamTestFingerprint: streamFingerprint(useAppStore.getState()),
  });
}

describe("the list of what to fix", () => {
  const toggle = () => screen.getByRole("button", { name: /to fix before processing/ });

  it("is offered beside a disabled Process, and not once Process is ready", () => {
    const { rerender } = render(<RunControls />);
    expect(toggle()).toHaveTextContent("4 to fix");
    expect(toggle()).toHaveAttribute("aria-expanded", "false");

    readyForFileRun();
    rerender(<RunControls />);
    expect(screen.queryByRole("button", { name: /to fix before processing/ })).not.toBeInTheDocument();
  });

  it("is not offered during a run", () => {
    store().setIsProcessing(true);
    render(<RunControls />);
    expect(screen.queryByRole("button", { name: /to fix before processing/ })).not.toBeInTheDocument();
  });

  it("opens from the keyboard and lists each blocker", async () => {
    render(<RunControls />);
    toggle().focus();
    await userEvent.keyboard("{Enter}");
    expect(toggle()).toHaveAttribute("aria-expanded", "true");
    const list = screen.getByRole("region", { name: "Why Process is unavailable" });
    expect(list.querySelectorAll("li")).toHaveLength(4);

    await userEvent.keyboard("{Escape}");
    expect(screen.queryByRole("region", { name: "Why Process is unavailable" })).not.toBeInTheDocument();
    expect(toggle()).toHaveFocus();
  });

  it("opens the Settings page that fixes a blocker", async () => {
    render(<RunControls />);
    await userEvent.click(toggle());
    await userEvent.click(screen.getByRole("button", { name: "Open Output" }));
    expect(store()).toMatchObject({ settingsOpen: true, settingsPage: "output" });
    expect(screen.queryByRole("region", { name: "Why Process is unavailable" })).not.toBeInTheDocument();
  });

  it("opens the sidebar on the Input section for a missing file", async () => {
    store().toggleSidebar();
    expect(store().sidebarCollapsed).toBe(true);
    render(<RunControls />);
    await userEvent.click(toggle());
    await userEvent.click(screen.getByRole("button", { name: "Show Input" }));
    expect(store().sidebarCollapsed).toBe(false);
    expect(store().configSection).toBe("input");
  });

  it("closes on a click outside", async () => {
    render(<><RunControls /><p>elsewhere</p></>);
    await userEvent.click(toggle());
    await userEvent.click(screen.getByText("elsewhere"));
    expect(screen.queryByRole("region", { name: "Why Process is unavailable" })).not.toBeInTheDocument();
  });
});

describe("the Process gate", () => {
  const titleLines = () => processBtn().getAttribute("title")!.split("\n");

  it("lists every unmet precondition in the tooltip, not just the first", () => {
    render(<RunControls />);
    expect(processBtn()).toBeDisabled();
    expect(titleLines()).toEqual([
      expect.stringContaining("PEA is not ready"),
      expect.stringContaining("No output directory"),
      expect.stringContaining("No products directory"),
      expect.stringContaining("No RINEX file"),
    ]);
  });

  it("drops each one as it is fixed, and enables Process after the last", () => {
    const { rerender } = render(<RunControls />);

    useAppStore.setState({ peaBinaryPath: "/bin/pea" });
    store().setOutputDir("/out");
    rerender(<RunControls />);
    expect(titleLines()).toEqual([
      expect.stringContaining("No products directory"),
      expect.stringContaining("No RINEX file"),
    ]);

    store().setProductsDir("/products");
    store().setRinexPath("/data/file.rnx");
    rerender(<RunControls />);
    // A file with no start epoch: only now is the epoch worth mentioning.
    expect(titleLines()).toEqual([expect.stringContaining("No start epoch")]);

    store().setRinexData(makeRinexData());
    rerender(<RunControls />);
    // cddisStatus is still "idle", which is not a blocker.
    expect(processBtn()).toBeEnabled();
    expect(processBtn()).toHaveAttribute("title", "Run PEA with the current configuration");
  });

  it("does not send the user to Settings for the RINEX file", () => {
    render(<RunControls />);
    expect(processBtn().getAttribute("title")).not.toMatch(/RINEX file in Settings/);
    expect(processBtn().getAttribute("title")).toMatch(/No RINEX file\. Open one in the Input section\./);
  });

  it("blocks while the CDDIS check is in flight and after it fails", () => {
    readyForFileRun();
    store().setCddisStatus("checking");
    const { rerender } = render(<RunControls />);
    expect(processBtn()).toHaveAttribute("title", "Checking the CDDIS connection...");

    store().setCddisStatus("error", "401");
    rerender(<RunControls />);
    expect(processBtn()).toHaveAttribute("title", expect.stringContaining("CDDIS login is missing"));
  });

  it("skips the file-mode gates in realtime", () => {
    readyForRealtimeRun();
    store().setCddisStatus("error", "401");
    render(<RunControls />);
    expect(processBtn()).toBeEnabled();
  });

  it("requires a passed Monitor stream, and a fresh one, with visible fixes", async () => {
    readyForRealtimeRun();
    useAppStore.setState({ streamTestStatus: "idle" });
    const { rerender } = render(<RunControls />);
    expect(processBtn()).toBeDisabled();
    expect(processBtn()).toHaveAttribute("title", "Run Monitor stream first to detect the constellations");
    await userEvent.click(screen.getByRole("button", { name: /to fix before processing/ }));
    expect(screen.getByRole("region", { name: "Why Process is unavailable" })).toHaveTextContent("Run Monitor stream first");
    // Monitor stream lives on the Streams tab, so the fix asks for that tab.
    await userEvent.click(screen.getByRole("button", { name: "Show Streams" }));
    expect(store().workspaceTabRequest).toBe("streams");

    markStreamTestPassed();
    rerender(<RunControls />);
    expect(processBtn()).toBeEnabled();

    store().updateRealtime({ obs_streams: ["ALIC00AUS0", "DARW00AUS0"] });
    rerender(<RunControls />);
    expect(processBtn()).toBeDisabled();
    expect(processBtn()).toHaveAttribute("title", expect.stringContaining("run it again"));
  });

  it("requires at least one observation stream in realtime", () => {
    useAppStore.setState({ peaBinaryPath: "/bin/pea" });
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: [] });
    render(<RunControls />);
    expect(processBtn()).toHaveAttribute(
      "title",
      expect.stringContaining("No observation stream. Add one in the Input section."),
    );
  });

  it("requires a caster while any observation stream is a mountpoint", () => {
    readyForRealtimeRun();
    store().setCasterUrl("");
    const { rerender } = render(<RunControls />);
    expect(processBtn()).toHaveAttribute("title", expect.stringContaining("No NTRIP caster"));

    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"] });
    markStreamTestPassed();
    rerender(<RunControls />);
    expect(processBtn()).toBeEnabled();
  });

  it("turns Process into Stop while a run is in flight", () => {
    readyForFileRun();
    store().setIsProcessing(true);
    render(<RunControls />);
    expect(screen.queryByRole("button", { name: /Process/ })).not.toBeInTheDocument();
    expect(stopBtn()).toBeEnabled();
  });

  it("only shows Stop during a run", () => {
    readyForFileRun();
    const { rerender } = render(<RunControls />);
    expect(screen.queryByRole("button", { name: /Stop/ })).not.toBeInTheDocument();
    expect(processBtn()).toBeEnabled();

    store().setIsProcessing(true);
    rerender(<RunControls />);
    expect(stopBtn()).toBeEnabled();
  });
});

describe("a file run", () => {
  beforeEach(readyForFileRun);

  it("writes the config, fetches the products and starts PEA, in that order", async () => {
    const calls: string[] = [];
    mockInvoke({
      write_config: (a) => {
        calls.push("write_config");
        expect(a.overwrite).toBe(true);
        return "/out/ppp_config.yaml";
      },
      ensure_dynamic_products: () => {
        calls.push("ensure_dynamic_products");
        return null;
      },
      start_pea: () => {
        calls.push("start_pea");
        return null;
      },
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(calls).toEqual([
      "write_config",
      "ensure_dynamic_products",
      "start_pea",
    ]));
    expect(lastInvokeArgs("start_pea")).toEqual({ configPath: "/out/ppp_config.yaml" });
    expect(store().isProcessing).toBe(true);
  });

  it("writes into a dated run directory when no run name is given", async () => {
    vi.useFakeTimers({ toFake: ["Date"], now: new Date(2026, 9, 22, 11, 31) });
    mockInvoke({ write_config: "/x", ensure_dynamic_products: null, start_pea: null });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(lastInvokeArgs("write_config")).toBeDefined());
    const inputs = lastInvokeArgs("write_config")!.inputs as { output_dir: string; products_dir: string };
    expect(inputs.output_dir).toBe("/out/2026-10-22_1131_rinex");
    // Products stay shared between runs.
    expect(inputs.products_dir).toBe("/products");
    // The result views follow the run.
    expect(store().runDir).toBe("/out/2026-10-22_1131_rinex");
    vi.useRealTimers();
  });

  it("clears the Console before starting but keeps the Workflow session", async () => {
    store().appendLine("old console");
    store().appendWorkflowLine("old workflow");
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(store().consoleLines).not.toContain("old console"));
    // The Workflow tab is the whole session's record, so earlier steps survive.
    expect(store().workflowLines[0]).toBe("old workflow");
    expect(store().workflowLines).toContain(WF.startingDownloads());
  });

  it("passes the catalogue entry for the selected product to the downloader", async () => {
    store().setPppCatalogue(makeCatalogue([{ provider: "IGS", series: "FIN", project: "OPS" }]));
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(lastInvokeArgs("ensure_dynamic_products")).toMatchObject({
      productsDir: "/products",
      provider: "IGS",
      series: "FIN",
      project: "OPS",
      preferredMirror: "CDDIS",
    }));
    const args = lastInvokeArgs("ensure_dynamic_products")!;
    expect(args.catalogueEntry).toMatchObject({ provider: "IGS" });
  });

  it("sends a null catalogue entry when the selected combo was never scanned", async () => {
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() =>
      expect(lastInvokeArgs("ensure_dynamic_products")!.catalogueEntry).toBeNull(),
    );
  });

  // Under 30 s the downloader prefers the 5 s clock products. It
  // decides from the RINEX file's own interval, so these pin what is sent.
  it("sends the RINEX file's own interval to the downloader", async () => {
    store().setRinexData(makeRinexData({ epoch_interval: 1 }));
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() =>
      expect(lastInvokeArgs("ensure_dynamic_products")!.rinexIntervalS).toBe(1),
    );
  });

  it("does not send the processing interval the user may have edited", async () => {
    store().setRinexData(makeRinexData({ epoch_interval: 1 }));
    store().setEpochInterval(30);
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() =>
      expect(lastInvokeArgs("ensure_dynamic_products")!.rinexIntervalS).toBe(1),
    );
  });

  it("sends a null interval when the RINEX header has none", async () => {
    store().setRinexData(makeRinexData({ epoch_interval: null }));
    // The store seeds the processing interval with 30 here; that must not be
    // mistaken for a measured 30 s file.
    expect(store().epochInterval).toBe(30);
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() =>
      expect(lastInvokeArgs("ensure_dynamic_products")!.rinexIntervalS).toBeNull(),
    );
  });

  it("says it is reusing the config when overwrite is off", async () => {
    store().setConfigOverwrite(false);
    mockInvoke({
      write_config: (a) => {
        expect(a.overwrite).toBe(false);
        return "/out/ppp_config.yaml";
      },
      ensure_dynamic_products: null,
      start_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() =>
      expect(store().consoleLines.join("\n")).toMatch(/Reusing existing config/),
    );
  });

  it("stops and reports when writing the config fails", async () => {
    mockInvoke({ write_config: () => Promise.reject(new Error("disk full")) });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(store().isProcessing).toBe(false));
    expect(store().status).toMatch(/disk full/);
    expect(invokeCount("start_pea")).toBe(0);
  });

  it("reports a cancellation as a stop rather than an error", async () => {
    mockInvoke({
      write_config: "/out/ppp_config.yaml",
      ensure_dynamic_products: () => Promise.reject("Cancelled"),
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(store().status).toBe("Stopped"));
    // handleStop logs the stop sequence, so the run path adds nothing — and in
    // particular no download-error or generic warning step.
    expect(store().isProcessing).toBe(false);
    expect(store().status).not.toMatch(/Error/);
    expect(store().workflowLines).toEqual([WF.startingDownloads()]);
  });
});

describe("the Monitor stream button", () => {
  const monitorBtn = () => screen.queryByRole("button", { name: /Monitor stream|Monitor again|Monitoring/ });

  it("sits beside Process until the streams have passed, then goes", () => {
    readyForRealtimeRun();
    useAppStore.setState({ streamTestStatus: "idle", streamTestFingerprint: null });
    const { rerender } = render(<RunControls />);
    expect(monitorBtn()).toHaveTextContent("Monitor stream");
    expect(monitorBtn()).toBeEnabled();
    expect(monitorBtn()).toHaveClass("btn-monitor");

    act(() => markStreamTestPassed());
    rerender(<RunControls />);
    expect(monitorBtn()).toBeNull();
  });

  it("asks to monitor again once the settings change", () => {
    readyForRealtimeRun();
    act(() => store().updateRealtime({ obs_streams: ["ALIC00AUS0", "HOB200AUS0"] }));
    render(<RunControls />);
    expect(monitorBtn()).toHaveTextContent("Monitor again");
  });

  it("runs the stream test", async () => {
    readyForRealtimeRun();
    useAppStore.setState({ streamTestStatus: "idle", streamTestFingerprint: null });
    mockInvoke({ write_config: "/out/rt.yaml", test_streams: makeStreamTestResult() });
    render(<RunControls />);
    await userEvent.click(monitorBtn()!);
    await waitFor(() => expect(invokeCount("test_streams")).toBe(1));
    await waitFor(() => expect(store().streamTestStatus).toBe("passed"));
    expect(invokeCount("start_pea_realtime")).toBe(0);
  });

  it("is not shown for a file run", () => {
    readyForFileRun();
    render(<RunControls />);
    expect(monitorBtn()).toBeNull();
  });
});

describe("a realtime run", () => {
  beforeEach(readyForRealtimeRun);

  it("tests the streams, skips the product download and starts the stream", async () => {
    let writes = 0;
    mockInvoke({
      write_config: () => `/out/rt_ppp_config_${++writes}.yaml`,
      test_streams: makeStreamTestResult(),
      start_pea_realtime: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));
    expect(invokeCount("ensure_dynamic_products")).toBe(0);
    expect(invokeCount("test_streams")).toBe(1);
    // The stream test reads its streams from a written config, and the config
    // is rewritten afterwards so it carries what the test found.
    const testArgs = lastInvokeArgs("test_streams") as { options: Record<string, unknown> };
    expect(testArgs.options).toMatchObject({
      config_path: "/out/rt_ppp_config_2.yaml",
      output_dir: "/out",
      serial: {},
    });
    expect(lastInvokeArgs("start_pea_realtime")).toEqual({
      options: {
        config_path: "/out/rt_ppp_config_3.yaml",
        caster_url: "https://ntrip.data.gnss.ga.gov.au/",
        output_dir: "/out/run1",
        storage_cap_mb: 500,
        serial: {},
        sbas_mode: null,
      },
    });
  });

  it("applies what the stream test found before starting", async () => {
    mockInvoke({
      write_config: "/out/rt.yaml",
      test_streams: makeStreamTestResult({ interval_s: 0.4 }),
      start_pea_realtime: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));
    expect(store().streamTest).not.toBeNull();
    // The interval field takes whole seconds, at least 1.
    expect(store().epochInterval).toBe(1);
    expect(store().workflowLines.join("\n")).toMatch(/ALIC00AUS0: 18 satellites/);
  });

  it("sends each serial stream's line settings keyed by device", async () => {
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"] });
    store().setStreamSerial("obs", [{ ...DEFAULT_SERIAL_SETTINGS, baud: 9600 }]);
    markStreamTestPassed();
    mockInvoke({
      write_config: "/out/rt.yaml",
      test_streams: makeStreamTestResult(),
      start_pea_realtime: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));
    const opts = (lastInvokeArgs("start_pea_realtime") as { options: { serial: unknown } }).options;
    expect(opts.serial).toEqual({ "/dev/ttyUSB0": { ...DEFAULT_SERIAL_SETTINGS, baud: 9600 } });
  });

  type WrittenInputs = {
    constellations: string[];
    constellation_obs_codes: Record<string, string[]>;
  };
  /** The `inputs` of every `write_config` call so far, oldest first. */
  const writtenConfigs = () =>
    invoke.mock.calls
      .filter((c) => c[0] === "write_config")
      .map((c) => (c[1] as { inputs: WrittenInputs }).inputs);

  it("keeps the signals the user switched off, through Process and after Stop", async () => {
    // An earlier stream test filled the lists; the user then trims them.
    const seen = makeObsCodes({ gps: ["L1C", "L2W", "L5Q"], gal: ["L1X"], glo: ["L1C", "L2C"] });
    store().applyStreamTest(makeStreamTestResult({ observation_codes: seen }));
    store().updateConstellationCode("GPS", 1, { enabled: false }); // L2W off
    store().setConstellationEnabled("GLO", false);
    mockInvoke({
      write_config: "/out/rt.yaml",
      test_streams: makeStreamTestResult({ observation_codes: seen }),
      start_pea_realtime: null,
      stop_pea: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());
    await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));
    await userEvent.click(stopBtn());
    await waitFor(() => expect(store().status).toBe("Stopped"));

    // The config PEA is started with, i.e. the one written after the stream test.
    const writes = writtenConfigs();
    const started = writes[writes.length - 1];
    expect(started.constellations).toEqual(["GPS", "GAL"]);
    expect(started.constellation_obs_codes).toMatchObject({
      gps: ["L1C", "L5Q"],
      gal: ["L1X"],
      glo: [],
    });
    // And what the Constellations tab shows once the run is over.
    const { gps, glo } = store().constellationCodes;
    expect(gps.map((c) => c.enabled)).toEqual([true, false, true]);
    expect(glo.every((c) => !c.enabled)).toBe(true);
  });

  it("still starts from the GPS/GAL/GLO defaults when nothing has been customised", async () => {
    mockInvoke({
      write_config: "/out/rt.yaml",
      test_streams: makeStreamTestResult(),
      start_pea_realtime: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());
    await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));

    // The first write feeds the stream test, before any signal list exists.
    const first = writtenConfigs()[0];
    expect(first.constellations).toEqual(["GPS", "GAL", "GLO"]);
    expect(first.constellation_obs_codes).toEqual({ gps: [], gal: [], glo: [], bds: [], qzs: [] });
  });

  /**
   * PEA's own failure for a rejected caster login is silence: it
   * connects, gets a 401, receives no observation, and the stream test hits its
   * timeout naming neither the caster nor the login.
   */
  describe("the caster login check", () => {
    const rejected = {
      state: "rejected",
      host: "ntrip.data.gnss.ga.gov.au",
      detail: "The caster rejected the username or password.",
    };

    it("stops before PEA when the caster rejects the login", async () => {
      mockInvoke({
        write_config: "/out/rt.yaml",
        verify_ntrip_credentials: rejected,
        test_streams: makeStreamTestResult(),
        start_pea_realtime: null,
      });
      render(<RunControls />);

      await userEvent.click(processBtn());

      await waitFor(() => expect(store().status).toMatch(/rejected the saved login/));
      expect(invokeCount("start_pea_realtime")).toBe(0);
      // Not even the stream test runs, since that is the step that would hang.
      expect(invokeCount("test_streams")).toBe(0);
      expect(store().isProcessing).toBe(false);
      expect(store().workflowLines.join("\n")).toMatch(
        /rejected the saved login, so the run was not started/,
      );
    });

    it("checks the mountpoint the run will actually ask for", async () => {
      store().addProduct("obs", "ntrip", "ALIC00AUS0");
      // The added stream changes the settings, so the Monitor stream pass is redone.
      markStreamTestPassed();
      mockInvoke({
        write_config: "/out/rt.yaml",
        verify_ntrip_credentials: rejected,
        start_pea_realtime: null,
      });
      render(<RunControls />);

      await userEvent.click(processBtn());

      await waitFor(() => expect(invokeCount("verify_ntrip_credentials")).toBe(1));
      expect(lastInvokeArgs("verify_ntrip_credentials")).toEqual({
        casterUrl: "https://ntrip.data.gnss.ga.gov.au/",
        mountpoint: "ALIC00AUS0",
      });
    });

    it("warns but still runs when the caster could not be reached", async () => {
      mockInvoke({
        write_config: "/out/rt.yaml",
        verify_ntrip_credentials: {
          state: "inconclusive",
          host: "ntrip.data.gnss.ga.gov.au",
          detail: "Could not reach the caster.",
        },
        test_streams: makeStreamTestResult(),
        start_pea_realtime: null,
      });
      render(<RunControls />);

      await userEvent.click(processBtn());

      // An unreachable caster has refused nothing, and the stream test is the
      // better judge of whether data arrives.
      await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));
      expect(store().workflowLines.join("\n")).toMatch(/Could not check the caster login/);
    });

    it("runs normally when the caster accepts the login", async () => {
      mockInvoke({
        write_config: "/out/rt.yaml",
        verify_ntrip_credentials: {
          state: "accepted",
          host: "ntrip.data.gnss.ga.gov.au",
          mountpoint: "ALIC00AUS0",
        },
        test_streams: makeStreamTestResult(),
        start_pea_realtime: null,
      });
      render(<RunControls />);

      await userEvent.click(processBtn());

      await waitFor(() => expect(invokeCount("start_pea_realtime")).toBe(1));
      expect(store().workflowLines.join("\n")).not.toMatch(/caster login/);
    });
  });

  it("does not start PEA when the stream test fails", async () => {
    mockInvoke({
      write_config: "/out/rt.yaml",
      test_streams: () => Promise.reject(new Error("no data on ALIC00AUS0")),
      start_pea_realtime: null,
    });
    render(<RunControls />);

    await userEvent.click(processBtn());

    await waitFor(() => expect(store().status).toMatch(/no data on ALIC00AUS0/));
    expect(invokeCount("start_pea_realtime")).toBe(0);
    expect(store().isProcessing).toBe(false);
  });
});

describe("Stop", () => {
  it("stops the run", async () => {
    readyForFileRun();
    store().setIsProcessing(true);
    mockInvoke({ stop_pea: null });
    render(<RunControls />);

    await userEvent.click(stopBtn());

    await waitFor(() => expect(store().status).toBe("Stopped"));
    expect(store().isProcessing).toBe(false);
  });

  it("still drops out of the processing state when stopping fails", async () => {
    readyForFileRun();
    store().setIsProcessing(true);
    mockInvoke({ stop_pea: () => Promise.reject(new Error("no such process")) });
    render(<RunControls />);

    await userEvent.click(stopBtn());

    await waitFor(() => expect(store().isProcessing).toBe(false));
    expect(store().status).toMatch(/Stop failed/);
  });
});
