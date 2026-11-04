import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import TraceResidualsView from "./TraceResidualsView";
import { resetAppStore, store } from "../test/storeHelpers";
import { invoke, invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";
import type { ResidualSeries, SatelliteSummary, TraceFileInfo, TraceSeries } from "../types";

/**
 * The `.TRACE` half of the Visualization tab: list the trace files, index the
 * selected one, and draw one satellite at a time. The backend is
 * `list_trace_files` and `load_trace_series`.
 */

const T0 = Date.UTC(2024, 0, 2);

const traceFile = (name: string, has_residuals = true): TraceFileInfo => ({
  path: `/out/${name}`,
  name,
  size: 5_000_000,
  modified_ms: T0,
  has_residuals,
});

function sat(id: string, constellation: string): SatelliteSummary {
  return {
    sat: id,
    constellation,
    signals: ["L1C"],
    receivers: ["ALIC"],
    samples: 120,
    first_ms: T0,
    last_ms: T0 + 3_600_000,
    postfit_rms: 0.004,
  };
}

function residual(satId: string, signal: string, constellation = "GPS"): ResidualSeries {
  const st = { mean: 0.0012, std_dev: 0.003, rms: 0.0031, min: -0.01, max: 0.01, mean_sigma: 0.003 };
  return {
    key: `PHAS_MEAS|${satId}|ALIC|${signal}`,
    sat: satId,
    constellation,
    receiver: "ALIC",
    signal,
    meas_type: "PHAS_MEAS",
    kind: "phase",
    total_samples: 1234,
    t: [T0, T0 + 30_000, T0 + 60_000],
    prefit: [0.1, 0.05, 0.02],
    postfit: [0.001, -0.002, 0.0005],
    sigma: [0.003, 0.003, 0.003],
    prefit_stats: { ...st, rms: 0.06 },
    postfit_stats: st,
  };
}

function trace(over: Partial<TraceSeries> = {}, receivers = ["ALIC"]): TraceSeries {
  return {
    path: "/out/ALIC.TRACE",
    name: "ALIC.TRACE",
    index: {
      receivers,
      signals: ["L1C", "L2W"],
      satellites: [sat("G07", "GPS"), sat("G12", "GPS"), sat("E11", "GAL")],
      epochs: 120,
      total_samples: 9000,
      t_start: T0,
      t_end: T0 + 3_600_000,
    },
    series: [residual("G07", "L1C"), residual("G07", "L2W"), residual("G12", "L1C"), residual("E11", "L1C", "GAL")],
    returned_samples: 9000,
    decimated: false,
    skipped_lines: 0,
    ...over,
  };
}

const satSelect = () => screen.getByRole("combobox", { name: "Satellite" }) as HTMLSelectElement;
const traceSelect = () => screen.getByRole("combobox", { name: "Trace file" }) as HTMLSelectElement;

beforeEach(() => {
  resetAppStore();
  store().setOutputDir("/out");
  vi.spyOn(Element.prototype, "getBoundingClientRect").mockReturnValue({
    x: 0, y: 0, left: 0, top: 0, right: 600, bottom: 300, width: 600, height: 300, toJSON: () => ({}),
  } as DOMRect);
});

describe("TraceResidualsView", () => {
  it("says so, with a retry, when there are no trace files", async () => {
    mockInvoke({ list_trace_files: [] });
    render(<TraceResidualsView />);
    expect(await screen.findByText("No .TRACE files yet")).toBeInTheDocument();
    expect(screen.getByText(/Turn on TRACE output/)).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /Check again/ })).toBeInTheDocument();
  });

  it("reports an unreadable output directory", async () => {
    mockInvoke({ list_trace_files: () => Promise.reject("no such directory") });
    render(<TraceResidualsView />);
    expect(await screen.findByText("Could not read the output directory")).toBeInTheDocument();
    expect(screen.getByText("no such directory")).toBeInTheDocument();
  });

  it("asks for the index alone first, then draws the satellite with the most data", async () => {
    const index = trace().index;
    index.satellites = [sat("G07", "GPS"), { ...sat("G12", "GPS"), samples: 500 }, sat("E11", "GAL")];
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace({ index }) });
    render(<TraceResidualsView />);

    await waitFor(() => expect(invokeCount("load_trace_series")).toBe(2));
    const [first, second] = invoke.mock.calls
      .filter((c) => c[0] === "load_trace_series")
      .map((c) => c[1]);
    // An empty list is "index only": no series arrays for every satellite.
    expect(first).toEqual({ path: "/out/ALIC.TRACE", sats: [], receiver: null, maxPoints: null });
    expect(second).toEqual({ path: "/out/ALIC.TRACE", sats: ["G12"], receiver: null, maxPoints: null });
    expect(satSelect()).toHaveValue("G12");
  });

  it("offers only trace files that have residuals in them", async () => {
    mockInvoke({
      list_trace_files: [traceFile("ALIC_smoothed.TRACE", false), traceFile("ALIC.TRACE")],
      load_trace_series: () => trace(),
    });
    render(<TraceResidualsView />);
    await waitFor(() => expect(satSelect()).toHaveValue("G07"));
    expect([...traceSelect().options].map((o) => o.value)).toEqual(["/out/ALIC.TRACE"]);
  });

  it("explains a directory whose traces hold no residuals instead of offering them", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC_smoothed.TRACE", false)] });
    render(<TraceResidualsView />);
    expect(await screen.findByText("No residuals to plot yet")).toBeInTheDocument();
    expect(screen.getByText(/ALIC_smoothed\.TRACE\) holds only PEA's configuration header/)).toBeInTheDocument();
    expect(invokeCount("load_trace_series")).toBe(0);
  });

  it("steps through the satellites with the previous and next buttons", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace() });
    render(<TraceResidualsView />);
    await waitFor(() => expect(satSelect()).toHaveValue("G07"));

    await userEvent.click(screen.getByRole("button", { name: "Next satellite" }));
    expect(satSelect()).toHaveValue("G12");
    // Wraps from the first back round to the last.
    await userEvent.click(screen.getByRole("button", { name: "Previous satellite" }));
    await userEvent.click(screen.getByRole("button", { name: "Previous satellite" }));
    expect(satSelect()).toHaveValue("E11");
    await waitFor(() => expect(lastInvokeArgs("load_trace_series")).toMatchObject({ sats: ["E11"] }));
  });

  it("groups the satellites by constellation", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace() });
    render(<TraceResidualsView />);
    await waitFor(() => expect(satSelect()).toBeEnabled());

    const groups = [...satSelect().querySelectorAll("optgroup")].map((g) => [
      g.label,
      [...g.querySelectorAll("option")].map((o) => o.value),
    ]);
    expect(groups).toEqual([
      ["GPS", ["G07", "G12"]],
      ["GAL", ["E11"]],
    ]);
  });

  it("tabulates only the chosen satellite's series", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace() });
    render(<TraceResidualsView />);

    const table = await screen.findByRole("table");
    const rows = within(table).getAllByRole("rowheader").map((r) => r.textContent);
    expect(rows).toEqual(["G07 · L1C · phase", "G07 · L2W · phase"]);
    expect(within(table).getAllByText("1,234")).toHaveLength(2);
    expect(screen.getByText(/120 epochs over .* · 3 satellites in file/)).toBeInTheDocument();
  });

  it("re-requests when another satellite is picked", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace() });
    render(<TraceResidualsView />);
    await waitFor(() => expect(satSelect()).toHaveValue("G07"));

    fireEvent.change(satSelect(), { target: { value: "E11" } });

    await waitFor(() =>
      expect(lastInvokeArgs("load_trace_series")).toMatchObject({ sats: ["E11"] }),
    );
    expect(await screen.findByText("E11 · L1C · phase")).toBeInTheDocument();
  });

  it("only offers a receiver filter on a multi-receiver solution", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace() });
    const { unmount } = render(<TraceResidualsView />);
    await screen.findByRole("table");
    expect(screen.queryByRole("combobox", { name: "Receiver" })).not.toBeInTheDocument();
    unmount();

    mockInvoke({
      list_trace_files: [traceFile("NET.TRACE")],
      load_trace_series: () => trace({}, ["ALIC", "HOB2"]),
    });
    render(<TraceResidualsView />);
    await screen.findByRole("table");
    const receiver = screen.getByRole("combobox", { name: "Receiver" }) as HTMLSelectElement;
    expect([...receiver.options].map((o) => o.value)).toEqual(["", "ALIC", "HOB2"]);

    fireEvent.change(receiver, { target: { value: "HOB2" } });
    await waitFor(() => expect(lastInvokeArgs("load_trace_series")).toMatchObject({ receiver: "HOB2" }));
  });

  it("toggles the prefit line, off to begin with", async () => {
    mockInvoke({ list_trace_files: [traceFile("ALIC.TRACE")], load_trace_series: () => trace() });
    render(<TraceResidualsView />);
    const prefit = await screen.findByRole("button", { name: "Prefit" });
    expect(prefit).toHaveAttribute("aria-pressed", "false");
    await userEvent.click(prefit);
    expect(prefit).toHaveAttribute("aria-pressed", "true");
  });

  it("shows a load error in place of the chart", async () => {
    mockInvoke({
      list_trace_files: [traceFile("ALIC.TRACE")],
      load_trace_series: () => Promise.reject("truncated header"),
    });
    render(<TraceResidualsView />);
    expect(await screen.findByText("truncated header")).toBeInTheDocument();
    expect(screen.queryByRole("table")).not.toBeInTheDocument();
  });

  it("notes decimation and skipped lines", async () => {
    mockInvoke({
      list_trace_files: [traceFile("ALIC.TRACE")],
      load_trace_series: () => trace({ decimated: true, returned_samples: 4000, skipped_lines: 3 }),
    });
    render(<TraceResidualsView />);
    expect(await screen.findByText(/Drawn from 4,000 sampled residuals/)).toBeInTheDocument();
    expect(screen.getByText(/3 unreadable lines skipped/)).toBeInTheDocument();
  });

  it("drops the satellite choice when another file is selected", async () => {
    mockInvoke({
      list_trace_files: [traceFile("A.TRACE"), traceFile("B.TRACE")],
      load_trace_series: (args) => trace({ path: args.path as string }),
    });
    render(<TraceResidualsView />);
    await waitFor(() => expect(satSelect()).toHaveValue("G07"));
    fireEvent.change(satSelect(), { target: { value: "G12" } });
    await waitFor(() => expect(lastInvokeArgs("load_trace_series")).toMatchObject({ sats: ["G12"] }));

    fireEvent.change(traceSelect(), { target: { value: "/out/B.TRACE" } });

    await waitFor(() =>
      expect(lastInvokeArgs("load_trace_series")).toMatchObject({ path: "/out/B.TRACE", sats: ["G07"] }),
    );
  });

  it("re-reads on a timer while a realtime run is going", async () => {
    vi.useFakeTimers({ shouldAdvanceTime: true });
    try {
      store().setInputMode("realtime");
      store().setIsProcessing(true);
      mockInvoke({ list_trace_files: [traceFile("rt.TRACE")], load_trace_series: () => trace() });
      render(<TraceResidualsView />);
      await waitFor(() => expect(invokeCount("list_trace_files")).toBe(1));

      await act(async () => {
        vi.advanceTimersByTime(5_000);
      });

      await waitFor(() => expect(invokeCount("list_trace_files")).toBe(2));
    } finally {
      vi.useRealTimers();
    }
  });
});
