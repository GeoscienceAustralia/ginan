import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import PosSolutionView from "./PosSolutionView";
import VisualizationPanel from "./VisualizationPanel";
import VizEmpty, { shortName } from "./VizEmpty";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { emit, invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";
import type { PosFileInfo, PosSeries } from "../types";

/**
 * The `.pos` half of the Visualization tab, driven through its real children
 * (tiling surface, panels, charts). The backend is the two commands it calls:
 * `list_pos_files` for the output directory and `load_pos_series` per file.
 */

const T0 = Date.UTC(2024, 0, 2);

function stats() {
  return { weighted_mean: 0.001, std_dev: 0.01, rms: 0.012, min: -0.03, max: 0.04, mean_sigma: 0.02 };
}

function makeSeries(path = "/out/ALIC.pos", n = 30): PosSeries {
  const idx = [...Array(n).keys()];
  const v = idx.map((i) => 0.05 * Math.exp(-i / 5) + 0.002 * Math.sin(i));
  const sig = idx.map((i) => Math.max(0.01, Math.exp(-i / 4)));
  return {
    path,
    name: path.split("/").pop()!,
    format: "pos",
    hpl: [],
    vpl: [],
    station: "ALIC",
    smoothed: path.includes("smoothed"),
    total_epochs: n,
    returned_epochs: n,
    decimated: false,
    skipped_lines: 0,
    t: idx.map((i) => T0 + i * 30_000),
    dn: v,
    de: v.map((x) => -x),
    du: v.map((x) => 2 * x),
    sn: sig,
    se: sig,
    su: sig,
    lat: idx.map(() => -23.67),
    lon: idx.map(() => 133.88),
    stats: { dn: stats(), de: stats(), du: stats() },
  };
}

function file(name: string, over: Partial<PosFileInfo> = {}): PosFileInfo {
  return {
    path: `/out/${name}`,
    name,
    format: "pos",
    smoothed: name.includes("smoothed"),
    size: 120_000,
    modified_ms: T0,
    // As the backend reports it: PEA's header alone is ~2 KB.
    has_epochs: (over.size ?? 120_000) > 4096,
    ...over,
  };
}

function stubLayout() {
  vi.spyOn(Element.prototype, "getBoundingClientRect").mockReturnValue({
    x: 0, y: 0, left: 0, top: 0, right: 800, bottom: 500, width: 800, height: 500, toJSON: () => ({}),
  } as DOMRect);
}

/** Single-panel layout keeps the DOM small; the stats panel is pure text. */
function useSingleStatsLayout() {
  localStorage.setItem(
    "ginan.viz.layout.v1",
    JSON.stringify({ type: "panel", id: "p-stats", kind: "stats", options: {} }),
  );
}

beforeEach(() => {
  resetAppStore();
  stubLayout();
  store().setOutputDir("/out");
});

describe("VizEmpty", () => {
  it("shows the title and body, and a retry only when asked for", async () => {
    const retry = vi.fn();
    const { rerender } = render(<VizEmpty title="Nothing" body="Run something" />);
    expect(screen.getByText("Nothing")).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /Check again/ })).not.toBeInTheDocument();

    rerender(<VizEmpty title="Nothing" body="Run something" onRetry={retry} />);
    await userEvent.click(screen.getByRole("button", { name: /Check again/ }));
    expect(retry).toHaveBeenCalledTimes(1);
  });

  it("shortens a path to its file name on either separator", () => {
    expect(shortName("/out/run/ALIC.pos")).toBe("ALIC.pos");
    expect(shortName("C:\\out\\ALIC.TRACE")).toBe("ALIC.TRACE");
    expect(shortName(null)).toBe("file");
  });
});

describe("VisualizationPanel", () => {
  it("asks for an output directory before anything else", () => {
    useAppStore.setState({ outputDir: null });
    render(<VisualizationPanel />);
    expect(screen.getByText("No output directory set")).toBeInTheDocument();
    expect(screen.queryByRole("group", { name: "Data source" })).not.toBeInTheDocument();
  });

  it("opens on Position and switches to Residuals", async () => {
    mockInvoke({ list_pos_files: [], list_trace_files: [] });
    render(<VisualizationPanel />);

    expect(await screen.findByText("No .pos files yet")).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Position" })).toHaveClass("is-active");

    await userEvent.click(screen.getByRole("button", { name: "Residuals" }));

    expect(await screen.findByText("No .TRACE files yet")).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Residuals" })).toHaveClass("is-active");
    expect(lastInvokeArgs("list_trace_files")).toEqual({ outputDir: "/out" });
  });
});

describe("PosSolutionView: the file list", () => {
  it("lists the .pos files in the output directory", async () => {
    mockInvoke({
      list_pos_files: [file("ALIC.pos")],
      load_pos_series: makeSeries(),
    });
    useSingleStatsLayout();
    render(<PosSolutionView />);

    const select = await screen.findByRole("combobox", { name: "Solution file" });
    expect(lastInvokeArgs("list_pos_files")).toEqual({ outputDir: "/out" });
    expect(within(select).getAllByRole("option").map((o) => o.textContent)).toEqual([
      expect.stringMatching(/^Forward · .* · ALIC · 117 KB$/),
    ]);
  });

  it("says so, with a retry, when the directory has no solutions", async () => {
    mockInvoke({ list_pos_files: [] });
    render(<PosSolutionView />);

    expect(await screen.findByText("No .pos files yet")).toBeInTheDocument();
    expect(screen.getByText(/Nothing to plot in \/out/)).toBeInTheDocument();

    mockInvoke({ list_pos_files: [file("ALIC.pos")], load_pos_series: makeSeries() });
    await userEvent.click(screen.getByRole("button", { name: /Check again/ }));

    expect(await screen.findByRole("combobox", { name: "Solution file" })).toBeInTheDocument();
  });

  it("reports an unreadable output directory", async () => {
    mockInvoke({ list_pos_files: () => Promise.reject("permission denied") });
    render(<PosSolutionView />);

    expect(await screen.findByText("Could not read the output directory")).toBeInTheDocument();
    expect(screen.getByText("permission denied")).toBeInTheDocument();
  });

  it("defaults to the newest forward solution that has epochs, not the first listed", async () => {
    mockInvoke({
      list_pos_files: [
        file("A_old.pos", { modified_ms: T0 }),
        file("B_empty.pos", { size: 0, modified_ms: T0 + 9e6 }),
        file("C_new.pos", { modified_ms: T0 + 5e6 }),
        file("C_new_smoothed.pos", { modified_ms: T0 + 6e6 }),
      ],
      load_pos_series: (args) => makeSeries(args.path as string),
    });
    useSingleStatsLayout();
    render(<PosSolutionView />);

    await waitFor(() => expect(lastInvokeArgs("load_pos_series")).toEqual({ path: "/out/C_new.pos", maxPoints: null }));
    expect(screen.getByRole("combobox", { name: "Solution file" })).toHaveValue("/out/C_new.pos");
  });

  it("leaves empty and header-only files out of the list", async () => {
    mockInvoke({
      list_pos_files: [file("ok.pos"), file("empty.pos", { size: 0 }), file("hdr.pos", { size: 2200 })],
      load_pos_series: makeSeries("/out/ok.pos"),
    });
    useSingleStatsLayout();
    render(<PosSolutionView />);

    const options = within(await screen.findByRole("combobox", { name: "Solution file" })).getAllByRole("option");
    expect(options.map((o) => (o as HTMLOptionElement).value)).toEqual(["/out/ok.pos"]);
  });

  it("explains a directory whose solutions have no epochs instead of offering them", async () => {
    mockInvoke({ list_pos_files: [file("empty.pos", { size: 0 }), file("hdr.pos", { size: 2200 })] });
    render(<PosSolutionView />);

    expect(await screen.findByText("No position epochs yet")).toBeInTheDocument();
    expect(screen.getByText(/\(empty\.pos, hdr\.pos\) have no epochs in them yet/)).toBeInTheDocument();
    expect(invokeCount("load_pos_series")).toBe(0);
  });

  it("disables figures a solution is too short to draw", async () => {
    mockInvoke({ list_pos_files: [file("one.pos")], load_pos_series: makeSeries("/out/one.pos", 1) });
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await screen.findByRole("table");

    const kinds = within(screen.getByRole("combobox", { name: "Figure shown in this panel" })).getAllByRole("option");
    const disabled = kinds.filter((o) => (o as HTMLOptionElement).disabled).map((o) => o.textContent);
    expect(disabled).toEqual([
      "Time series (needs 2+ epochs)",
      "Error magnitude (needs 2+ epochs)",
      "Formal error (needs 2+ epochs)",
      "Protection levels (SBAS .SPP only)",
      "Histogram (needs 3+ epochs)",
      "Error CDF (needs 3+ epochs)",
    ]);

    await userEvent.click(screen.getByRole("button", { name: /Add a figure/ }));
    expect(screen.getByRole("menuitem", { name: /^Time series/ })).toBeDisabled();
    expect(screen.getByRole("menuitem", { name: /^Summary/ })).toBeEnabled();
  });

  it("plots an SBAS .SPP: protection levels instead of formal errors", async () => {
    const base = makeSeries("/out/ALIC_202630001.SPP");
    const spp: PosSeries = {
      ...base,
      format: "spp",
      sn: base.sn.map(() => 0),
      se: base.se.map(() => 0),
      su: base.su.map(() => 0),
      hpl: base.t.map(() => 5.1),
      vpl: base.t.map(() => 8.2),
    };
    mockInvoke({ list_pos_files: [file("ALIC_202630001.SPP", { format: "spp" })], load_pos_series: spp });
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await screen.findByRole("table");

    expect(screen.getByRole("option", { name: /SBAS SPP/ })).toBeInTheDocument();
    const kinds = within(screen.getByRole("combobox", { name: "Figure shown in this panel" })).getAllByRole("option");
    const disabled = kinds.filter((o) => (o as HTMLOptionElement).disabled).map((o) => o.textContent);
    expect(disabled).toEqual(["Formal error (not in SPP: no formal errors)"]);
    // The mean sigma column has nothing to show.
    expect(within(screen.getByRole("table")).getAllByText("-")).toHaveLength(3);
    expect(screen.getByText(/no formal errors, so means are unweighted/)).toBeInTheDocument();
  });

  it("shows a load failure in the panel", async () => {
    mockInvoke({
      list_pos_files: [file("bad.pos")],
      load_pos_series: () => Promise.reject("line 12: bad epoch"),
    });
    useSingleStatsLayout();
    render(<PosSolutionView />);

    expect(await screen.findByText("line 12: bad epoch")).toBeInTheDocument();
  });

  it("draws the selected solution once it is loaded", async () => {
    mockInvoke({ list_pos_files: [file("ALIC.pos")], load_pos_series: makeSeries() });
    useSingleStatsLayout();
    render(<PosSolutionView />);

    expect(await screen.findByRole("table")).toBeInTheDocument();
    expect(screen.getByText(/ALIC · 30 epochs over/)).toBeInTheDocument();
    // More than one epoch puts the timeline scrubber above the panels.
    expect(screen.getByRole("slider", { name: "Selected time" })).toBeInTheDocument();
  });

  it("re-lists the directory when a run finishes", async () => {
    mockInvoke({ list_pos_files: [file("ALIC.pos")], load_pos_series: makeSeries() });
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await screen.findByRole("table");
    expect(invokeCount("list_pos_files")).toBe(1);

    act(() => store().bumpPosEpoch());

    await waitFor(() => expect(invokeCount("list_pos_files")).toBe(2));
    await waitFor(() => expect(invokeCount("load_pos_series")).toBe(2));
  });

  it("keeps the user's choice across a refresh", async () => {
    mockInvoke({
      list_pos_files: [file("A.pos", { modified_ms: T0 + 1 }), file("B.pos")],
      load_pos_series: (args) => makeSeries(args.path as string),
    });
    useSingleStatsLayout();
    render(<PosSolutionView />);
    const select = await screen.findByRole("combobox", { name: "Solution file" });
    fireEvent.change(select, { target: { value: "/out/B.pos" } });

    await userEvent.click(screen.getByRole("button", { name: "Refresh" }));

    await waitFor(() => expect(invokeCount("list_pos_files")).toBe(2));
    expect(screen.getByRole("combobox", { name: "Solution file" })).toHaveValue("/out/B.pos");
  });
});

describe("PosSolutionView: the toolbar", () => {
  beforeEach(() => {
    mockInvoke({ list_pos_files: [file("ALIC.pos")], load_pos_series: makeSeries() });
  });

  it("links time axes by default and remembers the choice", async () => {
    useSingleStatsLayout();
    render(<PosSolutionView />);
    const link = await screen.findByRole("button", { name: /Link axes/ });
    expect(link).toHaveAttribute("aria-pressed", "true");

    await userEvent.click(link);

    expect(link).toHaveAttribute("aria-pressed", "false");
    expect(localStorage.getItem("ginan.viz.linked")).toBe("false");
  });

  it("opens on the Overview preset with three panels", async () => {
    render(<PosSolutionView />);
    await screen.findByRole("combobox", { name: "Solution file" });
    const tiles = screen.getAllByRole("region");
    expect(tiles.map((t) => t.getAttribute("aria-label"))).toEqual([
      "Time series",
      "Horizontal scatter",
      "Summary",
    ]);
  });

  it("applies a layout preset from the menu", async () => {
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Layout presets" }));

    await userEvent.click(screen.getByRole("menuitem", { name: /2 × 2 grid/ }));

    expect(screen.getAllByRole("region")).toHaveLength(4);
    expect(screen.queryByRole("menu")).not.toBeInTheDocument();
  });

  it("adds a figure from Add view", async () => {
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: /Add a figure/ }));

    await userEvent.click(screen.getByRole("menuitem", { name: /^Summary/ }));

    expect(screen.getAllByRole("region").map((t) => t.getAttribute("aria-label"))).toEqual([
      "Statistics",
      "Summary",
    ]);
  });

  it("closes a menu on Escape", async () => {
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Layout presets" }));
    expect(screen.getByRole("menu")).toBeInTheDocument();

    await userEvent.keyboard("{Escape}");

    expect(screen.queryByRole("menu")).not.toBeInTheDocument();
  });

  it("offers a way back once the last panel is closed, and persists the empty layout", async () => {
    useSingleStatsLayout();
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Close panel" }));

    expect(screen.getByText("No panels open")).toBeInTheDocument();
    expect(localStorage.getItem("ginan.viz.layout.v1")).toBe("null");

    await userEvent.click(screen.getByRole("button", { name: /Restore default layout/ }));
    expect(screen.getAllByRole("region")).toHaveLength(3);
  });

  it("falls back to the default layout when the saved one is corrupt", async () => {
    localStorage.setItem("ginan.viz.layout.v1", "{not json");
    render(<PosSolutionView />);
    await screen.findByRole("combobox", { name: "Solution file" });
    expect(screen.getAllByRole("region")).toHaveLength(3);
  });
});

describe("PosSolutionView: tiles", () => {
  beforeEach(() => {
    mockInvoke({
      list_pos_files: [file("ALIC.pos", { modified_ms: T0 + 1 }), file("ALIC_smoothed.pos")],
      load_pos_series: (args) => makeSeries(args.path as string),
    });
    useSingleStatsLayout();
  });

  it("splits a panel into two of the same figure", async () => {
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Split right" }));

    expect(screen.getAllByRole("region").map((t) => t.getAttribute("aria-label"))).toEqual([
      "Statistics",
      "Statistics",
    ]);
    expect(screen.getByRole("separator", { name: "Resize panels" })).toHaveAttribute("aria-valuenow", "50");
  });

  it("resizes split panels from the keyboard", async () => {
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Split right" }));
    const divider = screen.getByRole("separator", { name: "Resize panels" });

    fireEvent.keyDown(divider, { key: "ArrowRight", shiftKey: true });
    expect(divider).toHaveAttribute("aria-valuenow", "60");

    fireEvent.doubleClick(divider);
    expect(divider).toHaveAttribute("aria-valuenow", "50");
  });

  it("maximises one panel and restores the layout", async () => {
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Split down" }));
    expect(screen.getAllByRole("region")).toHaveLength(2);

    await userEvent.click(screen.getAllByRole("button", { name: "Maximise" })[0]);
    expect(screen.getAllByRole("region")).toHaveLength(1);
    // A maximised panel cannot be split.
    expect(screen.queryByRole("button", { name: "Split right" })).not.toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Restore layout" }));
    expect(screen.getAllByRole("region")).toHaveLength(2);
  });

  it("changes the figure a panel shows", async () => {
    render(<PosSolutionView />);
    const kind = await screen.findByRole("combobox", { name: "Figure shown in this panel" });

    await userEvent.selectOptions(kind, "summary");

    expect(screen.getByRole("region")).toHaveAttribute("aria-label", "Summary");
    expect(screen.getByText("CEP50")).toBeInTheDocument();
  });

  it("pins a panel to another solution from its settings", async () => {
    render(<PosSolutionView />);
    await screen.findByRole("table");
    await userEvent.click(screen.getByRole("button", { name: "Panel settings" }));
    const dialog = screen.getByRole("dialog", { name: "Statistics settings" });
    const source = within(dialog).getByRole("combobox");
    expect(within(source).getAllByRole("option")[0]).toHaveTextContent("Follow toolbar (ALIC.pos)");

    await userEvent.selectOptions(source, "/out/ALIC_smoothed.pos");

    await waitFor(() =>
      expect(lastInvokeArgs("load_pos_series")).toEqual({ path: "/out/ALIC_smoothed.pos", maxPoints: null }),
    );
    expect(await screen.findByTitle("Pinned to ALIC_smoothed.pos")).toBeInTheDocument();
    expect(await screen.findByText("Backward-smoothed solution")).toBeInTheDocument();
  });

  it("closes the settings popover on Escape", async () => {
    render(<PosSolutionView />);
    await userEvent.click(await screen.findByRole("button", { name: "Panel settings" }));
    expect(screen.getByRole("dialog")).toBeInTheDocument();

    await userEvent.keyboard("{Escape}");

    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });
});

describe("PosSolutionView: realtime", () => {
  beforeEach(() => {
    store().setInputMode("realtime");
    useSingleStatsLayout();
  });

  it("has no Live stream switch: live updating is not optional", async () => {
    mockInvoke({ list_pos_files: [file("rt.pos")], load_pos_series: makeSeries("/out/rt.pos") });
    store().setIsProcessing(true);
    render(<PosSolutionView />);
    await screen.findByRole("table");
    expect(screen.queryByRole("checkbox", { name: "Real-time data stream" })).not.toBeInTheDocument();
  });

  it("does not tail when no run is going", async () => {
    mockInvoke({ list_pos_files: [file("rt.pos")], load_pos_series: makeSeries("/out/rt.pos") });
    render(<PosSolutionView />);
    await screen.findByRole("table");
    expect(invokeCount("start_pos_tail")).toBe(0);
  });

  it("tails the file as soon as the run is going and appends each new epoch", async () => {
    mockInvoke({
      list_pos_files: [file("rt.pos")],
      load_pos_series: makeSeries("/out/rt.pos", 5),
      start_pos_tail: null,
      stop_pos_tail: null,
    });
    store().setIsProcessing(true);
    render(<PosSolutionView />);
    await screen.findByText(/5 epochs over/);

    // No toggle to press: tailing starts by itself.
    await waitFor(() => expect(lastInvokeArgs("start_pos_tail")).toEqual({ path: "/out/rt.pos" }));
    // Let the listener subscribe.
    await act(() => new Promise((r) => setTimeout(r, 0)));

    act(() =>
      emit("pos-epoch", {
        path: "/out/rt.pos", t: T0 + 5 * 30_000, dn: 0, de: 0, du: 0, sn: 0.01, se: 0.01, su: 0.01, lat: -23.67, lon: 133.88,
      }),
    );
    // An epoch for another file is ignored.
    act(() =>
      emit("pos-epoch", {
        path: "/out/other.pos", t: T0, dn: 0, de: 0, du: 0, sn: 0, se: 0, su: 0, lat: 0, lon: 0,
      }),
    );

    expect(await screen.findByText(/6 epochs over/)).toBeInTheDocument();

    // Stopping the run stops the tail.
    act(() => store().setIsProcessing(false));
    await waitFor(() => expect(invokeCount("stop_pos_tail")).toBe(1));
  });

  it("polls the directory until PEA has written a file to tail", async () => {
    vi.useFakeTimers({ shouldAdvanceTime: true });
    try {
      mockInvoke({ list_pos_files: [], load_pos_series: makeSeries("/out/rt.pos") });
      store().setIsProcessing(true);
      render(<PosSolutionView />);
      await waitFor(() => expect(invokeCount("list_pos_files")).toBe(1));

      await act(async () => {
        vi.advanceTimersByTime(2_000);
      });

      await waitFor(() => expect(invokeCount("list_pos_files")).toBe(2));
    } finally {
      vi.useRealTimers();
    }
  });
});
