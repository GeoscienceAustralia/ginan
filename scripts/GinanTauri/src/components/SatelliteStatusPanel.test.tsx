import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import SatelliteStatusPanel from "./SatelliteStatusPanel";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";
import type { SatelliteStatus, SatelliteSummary } from "../types";

/**
 * Which satellites the filter is using, from `load_satellite_status` over the
 * run's `.TRACE`. A satellite is "contributing" when it has a
 * residual at the newest epoch in the file, and "dropped" otherwise.
 */

const T0 = Date.UTC(2024, 0, 2);
const T_END = T0 + 3_600_000;

function sat(id: string, constellation: string, over: Partial<SatelliteSummary> = {}): SatelliteSummary {
  return {
    sat: id,
    constellation,
    signals: ["L1C", "L2W"],
    receivers: ["ALIC"],
    samples: 1200,
    first_ms: T0,
    last_ms: T_END,
    postfit_rms: 0.0042,
    ...over,
  };
}

function status(over: Partial<SatelliteStatus> = {}): SatelliteStatus {
  return {
    path: "/out/ALIC.TRACE",
    name: "ALIC.TRACE",
    available: true,
    message: null,
    receivers: ["ALIC"],
    constellations: [
      {
        constellation: "GPS",
        satellites: [sat("G07", "GPS"), sat("G12", "GPS", { last_ms: T0 + 1_800_000 })],
      },
      { constellation: "GAL", satellites: [sat("E11", "GAL", { signals: [] })] },
    ],
    epochs: 120,
    satellite_count: 3,
    t_start: T0,
    t_end: T_END,
    ...over,
  };
}

beforeEach(() => {
  resetAppStore();
  store().setOutputDir("/out");
});

describe("SatelliteStatusPanel", () => {
  it("shows a reading state until the backend answers", () => {
    mockInvoke({ load_satellite_status: () => new Promise(() => {}) });
    render(<SatelliteStatusPanel />);
    expect(screen.getByText("Reading the trace file...")).toBeInTheDocument();
    expect(lastInvokeArgs("load_satellite_status")).toEqual({ outputDir: "/out", path: null });
  });

  it("prints the backend's reason when there is no trace yet", async () => {
    mockInvoke({
      load_satellite_status: status({
        available: false,
        message: "No .TRACE file yet. Turn on TRACE output to see satellites.",
        constellations: [],
      }),
    });
    render(<SatelliteStatusPanel />);
    expect(await screen.findByText(/No \.TRACE file yet/)).toBeInTheDocument();
    expect(screen.queryByText(/contributing of/)).not.toBeInTheDocument();
  });

  it("shows an unreadable file as an error", async () => {
    mockInvoke({ load_satellite_status: () => Promise.reject("bad trace header") });
    render(<SatelliteStatusPanel />);
    expect(await screen.findByText("bad trace header")).toBeInTheDocument();
  });

  it("groups satellites by constellation with full names and per-group counts", async () => {
    mockInvoke({ load_satellite_status: status() });
    render(<SatelliteStatusPanel />);

    expect(await screen.findByText("2 contributing of 3 seen")).toBeInTheDocument();
    const headings = screen.getAllByRole("heading", { level: 5 }).map((h) => h.textContent);
    expect(headings).toEqual(["GPS1/2", "Galileo1/1"]);
  });

  it("marks a satellite missing from the newest epoch as dropped", async () => {
    mockInvoke({ load_satellite_status: status() });
    render(<SatelliteStatusPanel />);
    await screen.findByText("2 contributing of 3 seen");

    const g12 = screen.getByRole("rowheader", { name: "G12" }).closest("tr")!;
    expect(g12).toHaveClass("sat-row--dropped");
    expect(within(g12).getByTitle("Dropped out")).toHaveTextContent(/· dropped$/);

    const g07 = screen.getByRole("rowheader", { name: "G07" }).closest("tr")!;
    expect(g07).not.toHaveClass("sat-row--dropped");
    expect(within(g07).getByTitle("Still contributing")).toBeInTheDocument();
    expect(within(g07).getByText("L1C, L2W")).toBeInTheDocument();
    expect(within(g07).getByText("1,200")).toBeInTheDocument();
  });

  it("shows a dash for a satellite with no signals", async () => {
    mockInvoke({ load_satellite_status: status() });
    render(<SatelliteStatusPanel />);
    const e11 = (await screen.findByRole("rowheader", { name: "E11" })).closest("tr")!;
    expect(within(e11).getByText("-")).toBeInTheDocument();
  });

  it("falls back to the raw code for a constellation it has no name for", async () => {
    mockInvoke({
      load_satellite_status: status({
        constellations: [{ constellation: "XYZ", satellites: [sat("X01", "XYZ")] }],
        satellite_count: 1,
      }),
    });
    render(<SatelliteStatusPanel />);
    expect(await screen.findByRole("heading", { level: 5, name: /^XYZ/ })).toBeInTheDocument();
  });

  it("names the file, epochs and receivers underneath", async () => {
    mockInvoke({ load_satellite_status: status() });
    render(<SatelliteStatusPanel />);
    expect(await screen.findByText(/ALIC\.TRACE · 120 epochs to .* · ALIC/)).toBeInTheDocument();
  });

  it("re-reads on Refresh and when a run finishes", async () => {
    mockInvoke({ load_satellite_status: status() });
    render(<SatelliteStatusPanel />);
    await screen.findByText("2 contributing of 3 seen");

    await userEvent.click(screen.getByRole("button", { name: /Refresh/ }));
    await waitFor(() => expect(invokeCount("load_satellite_status")).toBe(2));

    act(() => store().bumpPosEpoch());
    await waitFor(() => expect(invokeCount("load_satellite_status")).toBe(3));
  });

  it("polls while a run is processing", async () => {
    vi.useFakeTimers({ shouldAdvanceTime: true });
    try {
      store().setIsProcessing(true);
      mockInvoke({ load_satellite_status: status() });
      render(<SatelliteStatusPanel />);
      await waitFor(() => expect(invokeCount("load_satellite_status")).toBe(1));

      await act(async () => {
        vi.advanceTimersByTime(5_000);
      });

      await waitFor(() => expect(invokeCount("load_satellite_status")).toBe(2));
    } finally {
      vi.useRealTimers();
    }
  });
});
