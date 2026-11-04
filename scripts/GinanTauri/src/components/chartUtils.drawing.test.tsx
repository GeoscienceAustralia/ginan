import { describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { act } from "react";
import {
  LEGEND_TOP_GAP,
  clamp,
  drawCrosshair,
  drawGrid,
  drawLegend,
  drawSelectedRing,
  drawSelectedTime,
  drawSelection,
  drawXAxis,
  drawYAxis,
  formatResidual,
  scale,
  useElementSize,
  type VizTheme,
} from "./chartUtils";

const THEME: VizTheme = {
  surface: "#000",
  text: "#ffffff",
  textMuted: "#888",
  textDim: "#666",
  grid: "#222",
  accent: "#0f0",
  series: ["#f00", "#0f0", "#00f", "#f0f", "#ff0", "#0ff"],
};

/** A 2D context that records every call and property write. */
function recordingCtx() {
  const calls: { name: string; args: unknown[] }[] = [];
  const props: Record<string, unknown>[] = [];
  const state: Record<string, unknown> = {};
  const fn = (name: string, impl?: (...a: unknown[]) => unknown) =>
    vi.fn((...args: unknown[]) => {
      calls.push({ name, args });
      return impl?.(...args);
    });
  const ctx = new Proxy(state, {
    get(target, prop: string) {
      if (prop in target) return target[prop];
      if (prop === "measureText") return fn("measureText", (t) => ({ width: String(t).length * 6 }));
      const f = fn(prop);
      target[prop] = f;
      return f;
    },
    set(target, prop: string, value) {
      target[prop] = value;
      props.push({ [prop]: value });
      return true;
    },
  }) as unknown as CanvasRenderingContext2D;
  const named = (name: string) => calls.filter((c) => c.name === name);
  return { ctx, calls, props, named };
}

describe("formatResidual", () => {
  it("uses millimetres below a metre and metres from one metre up", () => {
    expect(formatResidual(0.0123)).toBe("12.3 mm");
    expect(formatResidual(-0.5)).toMatch(/mm$/);
    expect(formatResidual(1)).toBe("1.000 m");
    expect(formatResidual(-3421.7)).toBe("-3421.700 m");
  });
});

describe("clamp", () => {
  it("bounds a value on both sides", () => {
    expect(clamp(5, 0, 10)).toBe(5);
    expect(clamp(-1, 0, 10)).toBe(0);
    expect(clamp(11, 0, 10)).toBe(10);
  });
});

describe("drawLegend", () => {
  it("draws nothing for no entries", () => {
    const { ctx, calls } = recordingCtx();
    drawLegend(ctx, THEME, [], 0, 0);
    expect(calls).toHaveLength(0);
  });

  it("draws one row per entry, below the top gap, and restores state", () => {
    const { ctx, named } = recordingCtx();
    drawLegend(
      ctx,
      THEME,
      [
        { label: "dN", unit: "m", color: "#f00", swatch: "line" },
        { label: "σ", color: "#f00", swatch: "band", alpha: 0.3 },
        { label: "fix", color: "#0f0", swatch: "marker" },
        { label: "prefit", color: "#00f", swatch: "dashed" },
      ],
      100,
      20,
    );
    expect(named("save")).toHaveLength(1);
    expect(named("restore")).toHaveLength(1);
    // A label for each entry plus the "(m)" unit for the first.
    const texts = named("fillText").map((c) => c.args[0]);
    expect(texts).toEqual(["dN", "(m)", "σ", "fix", "prefit"]);
    // Rows step down from below the zoom-hint gap.
    const firstY = named("fillText")[0].args[2] as number;
    expect(firstY).toBe(20 + LEGEND_TOP_GAP + 8);
    expect(named("fillText")[2].args[2]).toBe(firstY + 16);
    // Swatches drawn the way the series are.
    expect(named("fillRect")).toHaveLength(1);
    expect(named("arc")).toHaveLength(1);
    expect(named("setLineDash").map((c) => c.args[0])).toContainEqual([4, 3]);
  });

  it("places the unit just after the label text", () => {
    const { ctx, named } = recordingCtx();
    drawLegend(ctx, THEME, [{ label: "dU", unit: "m", color: "#f00", swatch: "line" }], 0, 0);
    const [label, unit] = named("fillText");
    expect(unit.args[1]).toBe((label.args[1] as number) + 12 + 4);
  });
});

describe("drawGrid", () => {
  it("draws a crisp gridline per tick and a frame around the plot", () => {
    const { ctx, named } = recordingCtx();
    const x = scale(0, 1, 10, 110);
    const y = scale(0, 1, 90, 10);
    drawGrid(ctx, THEME, x, y, [0, 0.5, 1]);
    const ys = named("moveTo").map((c) => c.args[1]);
    expect(ys).toEqual([90.5, 50.5, 10.5]);
    expect(named("strokeRect")[0].args).toEqual([10.5, 10.5, 100, 80]);
  });
});

describe("drawYAxis / drawXAxis", () => {
  it("labels each y tick in the left gutter", () => {
    const { ctx, named } = recordingCtx();
    drawYAxis(ctx, THEME, scale(0, 1, 100, 0), [0, 1], 2, 62);
    const fills = named("fillText");
    expect(fills.map((c) => c.args[1])).toEqual([54, 54]);
    expect(fills.map((c) => c.args[2])).toEqual([100, 0]);
  });

  it("skips x ticks outside the plot and splits two-line labels", () => {
    const { ctx, named } = recordingCtx();
    const x = scale(0, 100, 0, 200);
    drawXAxis(
      ctx,
      THEME,
      x,
      [
        { value: 50, label: "12:00\n2 Jan" },
        { value: 500, label: "off-plot" },
      ],
      150,
      400,
    );
    const fills = named("fillText");
    expect(fills.map((c) => c.args[0])).toEqual(["12:00", "2 Jan"]);
    expect(fills.map((c) => c.args[2])).toEqual([156, 168]);
  });
});

describe("markers", () => {
  it("draws the crosshair on a half pixel", () => {
    const { ctx, named } = recordingCtx();
    drawCrosshair(ctx, THEME, 10.2, 0, 50);
    expect(named("moveTo")[0].args).toEqual([10.5, 0]);
    expect(named("lineTo")[0].args).toEqual([10.5, 50]);
  });

  it("draws the selected time as a line with a flag", () => {
    const { ctx, named, props } = recordingCtx();
    drawSelectedTime(ctx, THEME, 20, 5, 60);
    expect(props).toContainEqual({ strokeStyle: THEME.accent });
    expect(named("closePath")).toHaveLength(1);
    expect(named("fill")).toHaveLength(1);
  });

  it("rings the selected point", () => {
    const { ctx, named } = recordingCtx();
    drawSelectedRing(ctx, THEME, 3, 4);
    expect(named("arc")[0].args.slice(0, 3)).toEqual([3, 4, 7]);
  });

  it("shades a drag selection whichever way it was dragged", () => {
    const a = recordingCtx();
    drawSelection(a.ctx, THEME, { from: 80, to: 30 }, 10, 60);
    const b = recordingCtx();
    drawSelection(b.ctx, THEME, { from: 30, to: 80 }, 10, 60);
    expect(a.named("fillRect")[0].args).toEqual([30, 10, 50, 50]);
    expect(b.named("fillRect")[0].args).toEqual(a.named("fillRect")[0].args);
  });
});

describe("useElementSize", () => {
  function Probe() {
    const [ref, size] = useElementSize<HTMLDivElement>();
    return (
      <div ref={ref} data-testid="box">
        {size.width}x{size.height}
      </div>
    );
  }

  it("measures synchronously on mount, rounded to whole pixels", () => {
    vi.spyOn(HTMLElement.prototype, "getBoundingClientRect").mockReturnValue({
      width: 120.4,
      height: 80.6,
    } as DOMRect);
    render(<Probe />);
    expect(screen.getByTestId("box")).toHaveTextContent("120x81");
  });

  it("follows the resize observer and disconnects on unmount", () => {
    let callback: ResizeObserverCallback | null = null;
    const disconnect = vi.fn();
    const Original = window.ResizeObserver;
    window.ResizeObserver = class {
      constructor(cb: ResizeObserverCallback) {
        callback = cb;
      }
      observe() {}
      unobserve() {}
      disconnect = disconnect;
    } as unknown as typeof ResizeObserver;
    try {
      const { unmount } = render(<Probe />);
      act(() => {
        callback!(
          [{ contentRect: { width: 300, height: 200 } } as ResizeObserverEntry],
          {} as ResizeObserver,
        );
      });
      expect(screen.getByTestId("box")).toHaveTextContent("300x200");
      unmount();
      expect(disconnect).toHaveBeenCalled();
    } finally {
      window.ResizeObserver = Original;
    }
  });
});
