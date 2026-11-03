import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import type { ReactNode } from "react";
import type { PosSeries, ResidualSeries } from "../../types";
import PosTimeSeriesChart from "../PosTimeSeriesChart";
import PosScatterChart from "../PosScatterChart";
import PosMapChart from "../PosMapChart";
import TraceResidualChart from "../TraceResidualChart";
import CdfChart from "./CdfChart";
import HistogramChart from "./HistogramChart";
import TimeLinesChart from "./TimeLinesChart";
import TimelineScrubber from "./TimelineScrubber";
import { PANELS } from "./panels";
import { PANEL_KINDS } from "./layout";
import { VizLinkProvider, VizSelectionProvider, useSelectedTime } from "./VizContext";

/**
 * Smoke and behaviour tests for the canvas charts. jsdom draws nothing, so
 * these check that each figure mounts on realistic data without throwing,
 * exposes its accessible description, and that the controls around the
 * canvases (the timeline scrubber's buttons and keyboard) move the shared
 * selection. The setup file stubs the 2D context; layout is stubbed here.
 */

const T0 = Date.UTC(2024, 0, 2, 0, 0, 0);

function stats(values: number[]) {
  const mean = values.reduce((a, b) => a + b, 0) / values.length;
  const rms = Math.sqrt(values.reduce((a, b) => a + b * b, 0) / values.length);
  return {
    weighted_mean: mean,
    std_dev: Math.sqrt(Math.max(0, rms * rms - mean * mean)),
    rms,
    min: Math.min(...values),
    max: Math.max(...values),
    mean_sigma: 0.02,
  };
}

/** A converging kinematic solution: 30 s epochs, sigmas falling from 1 m to 1 cm. */
function makeSeries(n = 60, overrides: Partial<PosSeries> = {}): PosSeries {
  const idx = [...Array(n).keys()];
  const dn = idx.map((i) => 0.5 * Math.exp(-i / 10) + 0.01 * Math.sin(i));
  const de = idx.map((i) => -0.3 * Math.exp(-i / 10) + 0.01 * Math.cos(i));
  const du = idx.map((i) => 0.8 * Math.exp(-i / 12) + 0.02 * Math.sin(i / 2));
  const sig = idx.map((i) => Math.max(0.01, Math.exp(-i / 8)));
  return {
    path: "/out/ALIC.pos",
    name: "ALIC.pos",
    format: "pos",
    hpl: [],
    vpl: [],
    station: "ALIC",
    smoothed: false,
    total_epochs: n,
    returned_epochs: n,
    decimated: false,
    skipped_lines: 0,
    t: idx.map((i) => T0 + i * 30_000),
    dn,
    de,
    du,
    sn: sig,
    se: sig,
    su: sig.map((s) => s * 1.5),
    lat: idx.map((i) => -23.67 + i * 1e-7),
    lon: idx.map((i) => 133.88 + i * 1e-7),
    stats: { dn: stats(dn), de: stats(de), du: stats(du) },
    ...overrides,
  };
}

function makeResidual(sat: string, signal = "L1C", n = 40): ResidualSeries {
  const idx = [...Array(n).keys()];
  const prefit = idx.map((i) => 0.5 * Math.sin(i / 3));
  const postfit = idx.map((i) => 0.01 * Math.cos(i));
  const st = (v: number[]) => ({ mean: 0, std_dev: 0.01, rms: 0.01, min: Math.min(...v), max: Math.max(...v), mean_sigma: 0.003 });
  return {
    key: `PHAS_MEAS|${sat}|ALIC|${signal}`,
    sat,
    constellation: sat.startsWith("E") ? "GAL" : "GPS",
    receiver: "ALIC",
    signal,
    meas_type: "PHAS_MEAS",
    kind: "phase",
    total_samples: n,
    t: idx.map((i) => T0 + i * 30_000),
    prefit,
    postfit,
    sigma: idx.map(() => 0.003),
    prefit_stats: st(prefit),
    postfit_stats: st(postfit),
  };
}

/** Give every element a box: the charts skip drawing at zero size. */
function stubLayout(width = 640, height = 360) {
  vi.spyOn(Element.prototype, "getBoundingClientRect").mockReturnValue({
    x: 0,
    y: 0,
    left: 0,
    top: 0,
    right: width,
    bottom: height,
    width,
    height,
    toJSON: () => ({}),
  } as DOMRect);
}

function Providers({ children, linked = false }: { children: ReactNode; linked?: boolean }) {
  return (
    <VizLinkProvider linked={linked} setLinked={() => {}} resetKey="k">
      <VizSelectionProvider resetKey="k">{children}</VizSelectionProvider>
    </VizLinkProvider>
  );
}

/** Reads the shared selection back out, so a test can assert on it. */
function SelectionProbe() {
  const { selectedT } = useSelectedTime();
  return <output aria-label="selected">{selectedT === null ? "none" : String(selectedT)}</output>;
}

beforeEach(() => {
  stubLayout();
});

describe("chart smoke tests", () => {
  const series = makeSeries();

  it("draws the time series with an accessible description", () => {
    render(
      <Providers>
        <PosTimeSeriesChart series={series} showSigma showCi components={["dn", "de", "du"]} />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: /Time series of .* for ALIC\.pos, 60 epochs/ })).toBeInTheDocument();
  });

  it("draws only the components asked for", () => {
    render(
      <Providers>
        <PosTimeSeriesChart series={series} showSigma={false} showCi={false} components={["du"]} />
      </Providers>,
    );
    const label = screen.getByRole("img").getAttribute("aria-label")!;
    expect(label).toMatch(/dU/);
    expect(label).not.toMatch(/dN/);
  });

  it("draws the horizontal scatter", () => {
    render(
      <Providers>
        <PosScatterChart series={series} showCi />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: /Horizontal scatter of dE against dN for ALIC\.pos/ })).toBeInTheDocument();
  });

  it("mounts the Leaflet map and tears it down cleanly", () => {
    const { container, unmount } = render(
      <Providers>
        <PosMapChart series={series} />
      </Providers>,
    );
    expect(container.querySelector(".leaflet-container")).not.toBeNull();
    // The OSM attribution link must not navigate the app window.
    const osm = container.querySelector<HTMLAnchorElement>('a[href*="openstreetmap.org"]');
    expect(osm).not.toBeNull();
    expect(osm).toHaveAttribute("target", "_blank");
    expect(() => unmount()).not.toThrow();
  });

  it("mounts the map with no positions", () => {
    expect(() =>
      render(
        <Providers>
          <PosMapChart series={makeSeries(0)} />
        </Providers>,
      ),
    ).not.toThrow();
  });

  it("draws the CDF and summarises it in its label", () => {
    render(
      <Providers>
        <CdfChart series={series} metrics={["h", "v", "3d"]} clipTail />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: /Cumulative distribution of h, v, 3d error for ALIC\.pos/ })).toBeInTheDocument();
  });

  it.each(["dn", "de", "du", "h", "v", "3d"] as const)("draws a %s histogram with its moments", (q) => {
    render(
      <Providers>
        <HistogramChart series={series} quantity={q} showNormal clipTail />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: /Histogram of .* for ALIC\.pos: mean .*, standard deviation/ })).toBeInTheDocument();
  });

  it("draws generic time lines, log scale included", () => {
    render(
      <Providers>
        <TimeLinesChart
          series={series}
          lines={[{ key: "sn", label: "σN", values: series.sn, color: () => "#000", cssColor: "#000" }]}
          refLines={[{ label: "10 cm", value: 0.1, color: () => "#888" }]}
          logY
          ariaLabel="Formal error over time"
        />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: "Formal error over time" })).toBeInTheDocument();
  });

  it("draws residuals for a satellite", () => {
    render(
      <Providers>
        <TraceResidualChart series={[makeResidual("G07"), makeResidual("G07", "L2W")]} name="ALIC.TRACE" showPrefit />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: /residuals from ALIC\.TRACE/ })).toBeInTheDocument();
  });

  it("says so when there are no residuals to draw", () => {
    render(
      <Providers>
        <TraceResidualChart series={[]} name="ALIC.TRACE" showPrefit />
      </Providers>,
    );
    expect(screen.getByText("No residuals for this selection.")).toBeInTheDocument();
  });

  it("survives pointer movement over a chart (hover crosshair and tooltip)", () => {
    render(
      <Providers linked>
        <PosTimeSeriesChart series={series} showSigma showCi components={["dn", "de", "du"]} />
      </Providers>,
    );
    const canvas = screen.getByRole("img");
    expect(() => {
      // jsdom's PointerEvent carries no coordinates, so drive it with a
      // MouseEvent under the pointer event's name.
      fireEvent(canvas, new MouseEvent("pointermove", { bubbles: true, clientX: 320, clientY: 100 }));
      fireEvent.pointerLeave(canvas);
    }).not.toThrow();
  });
});

describe("panel registry", () => {
  const series = makeSeries();

  it.each(PANEL_KINDS)("renders the %s panel body from a series", (kind) => {
    const Body = PANELS[kind].Body;
    expect(() =>
      render(
        <Providers>
          <Body series={series} options={{}} />
        </Providers>,
      ),
    ).not.toThrow();
  });

  /** An SBAS single-point solution: no sigmas, protection levels instead. */
  const spp = makeSeries(60, {
    format: "spp",
    sn: Array(60).fill(0),
    se: Array(60).fill(0),
    su: Array(60).fill(0),
    hpl: Array.from({ length: 60 }, (_, i) => 5 + 0.1 * Math.sin(i)),
    vpl: Array.from({ length: 60 }, (_, i) => 8 + 0.1 * Math.cos(i)),
  });

  it.each(PANEL_KINDS)("renders the %s panel body from an SBAS .SPP series", (kind) => {
    const Body = PANELS[kind].Body;
    expect(() =>
      render(
        <Providers>
          <Body series={spp} options={{}} />
        </Providers>,
      ),
    ).not.toThrow();
  });

  it("draws HPL and VPL on the protection-level panel", () => {
    const Body = PANELS.levels.Body;
    render(
      <Providers>
        <Body series={spp} options={{}} />
      </Providers>,
    );
    expect(screen.getByRole("img", { name: /SBAS protection levels over time/ })).toBeInTheDocument();
  });

  it("shows protection levels, not convergence or sigmas, on an SPP summary", () => {
    const Body = PANELS.summary.Body;
    render(
      <Providers>
        <Body series={spp} options={{}} />
      </Providers>,
    );
    expect(screen.getByText("Protection levels")).toBeInTheDocument();
    expect(screen.queryByText("Convergence")).not.toBeInTheDocument();
    expect(screen.queryByText(/±/)).not.toBeInTheDocument();
    expect(screen.getByText("SBAS SPP")).toBeInTheDocument();
  });

  it("puts the headline numbers on the summary panel", () => {
    const Body = PANELS.summary.Body;
    render(
      <Providers>
        <Body series={series} options={{}} />
      </Providers>,
    );
    for (const label of ["Epochs", "Duration", "Horizontal RMS", "3D RMS", "CEP50", "Convergence"]) {
      expect(screen.getByText(label)).toBeInTheDocument();
    }
    expect(screen.getByText("ALIC")).toBeInTheDocument();
    expect(screen.getByText("Latest epoch")).toBeInTheDocument();
  });

  it("reports convergence as not reached when sigma never drops below the threshold", () => {
    const Body = PANELS.summary.Body;
    const flat = makeSeries(20, { sn: Array(20).fill(1), se: Array(20).fill(1) });
    render(
      <Providers>
        <Body series={flat} options={{ threshold: "0.05" }} />
      </Providers>,
    );
    expect(screen.getByText("Not reached")).toBeInTheDocument();
    expect(screen.getByText("horizontal σ ≤ 5 cm")).toBeInTheDocument();
  });

  it("flags smoothed, sampled and partly unreadable solutions on the statistics panel", () => {
    const Body = PANELS.stats.Body;
    render(
      <Providers>
        <Body
          series={makeSeries(60, { smoothed: true, decimated: true, returned_epochs: 30, skipped_lines: 1 })}
          options={{}}
        />
      </Providers>,
    );
    const table = screen.getByRole("table");
    expect(within(table).getAllByRole("row")).toHaveLength(4);
    expect(screen.getByText("Backward-smoothed solution")).toBeInTheDocument();
    expect(screen.getByText(/Drawn from 30 sampled epochs/)).toBeInTheDocument();
    expect(screen.getByText(/1 unreadable line skipped/)).toBeInTheDocument();
  });

  it("never lets the last component chip be switched off", () => {
    const Options = PANELS.timeseries.Options!;
    const set = vi.fn();
    render(<Options options={{ components: ["dn"] }} set={set} />);
    expect(screen.getByRole("button", { name: "dN" })).toBeDisabled();
    expect(screen.getByRole("button", { name: "dE" })).toHaveAttribute("aria-pressed", "false");
  });

  it("adds a component when its chip is pressed", async () => {
    const Options = PANELS.timeseries.Options!;
    const set = vi.fn();
    render(<Options options={{ components: ["dn"] }} set={set} />);
    await userEvent.click(screen.getByRole("button", { name: "dE" }));
    expect(set).toHaveBeenCalledWith({ components: ["dn", "de"] });
  });

  it("ignores option values it does not recognise", () => {
    const Options = PANELS.sigma.Options!;
    render(<Options options={{ threshold: "bogus", components: ["nope"] }} set={() => {}} />);
    // Falls back to the 10 cm default and every component.
    expect(screen.getByRole("combobox")).toHaveValue("0.1");
    for (const c of ["σN", "σE", "σU"]) {
      expect(screen.getByRole("button", { name: c })).toHaveAttribute("aria-pressed", "true");
    }
  });
});

describe("TimelineScrubber", () => {
  const series = makeSeries(10);

  function renderScrubber() {
    return render(
      <Providers>
        <TimelineScrubber series={series} />
        <SelectionProbe />
      </Providers>,
    );
  }
  const selected = () => screen.getByRole("status", { name: "selected" }).textContent;

  it("starts with no time selected", () => {
    renderScrubber();
    expect(screen.getByRole("slider", { name: "Selected time" })).toHaveAttribute(
      "aria-valuetext",
      "No time selected",
    );
    expect(screen.getByText(/Click the lower strip to pick a time/)).toBeInTheDocument();
  });

  it("steps forward from the start and back again", async () => {
    renderScrubber();

    await userEvent.click(screen.getByRole("button", { name: "Next epoch" }));
    expect(selected()).toBe(String(series.t[0]));
    expect(screen.getByRole("slider")).toHaveAttribute("aria-valuenow", "1");
    expect(screen.getByText("epoch 1 / 10")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Next epoch" }));
    expect(selected()).toBe(String(series.t[1]));

    await userEvent.click(screen.getByRole("button", { name: "Previous epoch" }));
    expect(selected()).toBe(String(series.t[0]));
  });

  it("steps back from the end when nothing is selected", async () => {
    renderScrubber();
    await userEvent.click(screen.getByRole("button", { name: "Previous epoch" }));
    expect(selected()).toBe(String(series.t[9]));
  });

  it("clamps at the last epoch", async () => {
    renderScrubber();
    await userEvent.click(screen.getByRole("button", { name: "Previous epoch" }));
    await userEvent.click(screen.getByRole("button", { name: "Next epoch" }));
    expect(selected()).toBe(String(series.t[9]));
  });

  it("clears the selection from its button and from Escape", async () => {
    renderScrubber();
    await userEvent.click(screen.getByRole("button", { name: "Next epoch" }));
    await userEvent.click(screen.getByRole("button", { name: "Clear selected time" }));
    expect(selected()).toBe("none");

    await userEvent.click(screen.getByRole("button", { name: "Next epoch" }));
    fireEvent.keyDown(screen.getByRole("slider"), { key: "Escape" });
    expect(selected()).toBe("none");
  });

  it("opens on the whole run, not zoomed in", () => {
    renderScrubber();
    // The span readout only appears once the detail strip shows part of the run.
    expect(screen.queryByTitle("Span shown in the detail strip")).not.toBeInTheDocument();
  });

  it("disables the zoom-out controls on the whole run", () => {
    renderScrubber();
    expect(screen.getByRole("button", { name: "Zoom out" })).toBeDisabled();
    expect(screen.getByRole("button", { name: "Show whole run" })).toBeDisabled();
    expect(screen.getByRole("button", { name: "Zoom in" })).toBeEnabled();
  });

  it("zooms in and back out to the whole run", async () => {
    renderScrubber();
    await userEvent.click(screen.getByRole("button", { name: "Zoom in" }));
    expect(screen.getByRole("button", { name: "Show whole run" })).toBeEnabled();

    await userEvent.click(screen.getByRole("button", { name: "Show whole run" }));
    expect(screen.getByRole("button", { name: "Show whole run" })).toBeDisabled();
  });

  it("plays through the run and stops at the end", async () => {
    renderScrubber();
    // Let the one-off auto-focus (a setTimeout(0) after mount, which also
    // stops playback) run first, as it would long before a user could click.
    await act(() => new Promise((r) => setTimeout(r, 0)));
    fireEvent.click(screen.getByRole("button", { name: "Play" }));
    expect(screen.getByRole("button", { name: "Pause" })).toBeInTheDocument();

    // Real timers: playback ticks every 60 ms at 5 epochs a step, so ten
    // epochs take a few ticks.
    await waitFor(() => expect(screen.getByRole("button", { name: "Play" })).toBeInTheDocument(), {
      timeout: 2000,
    });
    expect(selected()).toBe(String(series.t[9]));
  });

  it("offers the playback speeds, starting at 1x", () => {
    renderScrubber();
    const speed = screen.getByRole("combobox", { name: "Playback speed" }) as HTMLSelectElement;
    expect(speed.value).toBe("1");
    expect(speed.options.length).toBeGreaterThan(1);
  });
});
