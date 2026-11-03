import { describe, expect, it, vi } from "vitest";
import {
  PAD,
  drawEdgeSafeLabel,
  formatDuration,
  formatEpoch,
  formatMetres,
  formatMillimetres,
  nearestIndex,
  niceTicks,
  padRange,
  prepareCanvas,
  readVizTheme,
  recencyColor,
  scale,
  stepPrecision,
  timeAxisTicks,
  toData,
  toPx,
  withAlpha,
} from "./chartUtils";

describe("withAlpha", () => {
  it("converts #rrggbb to rgba", () => {
    expect(withAlpha("#3987e5", 0.5)).toBe("rgba(57, 135, 229, 0.5)");
  });

  it("expands the #rgb shorthand", () => {
    expect(withAlpha("#fff", 1)).toBe("rgba(255, 255, 255, 1)");
  });

  it("tolerates surrounding whitespace, as read off a CSS custom property", () => {
    expect(withAlpha("  #000000 ", 0.25)).toBe("rgba(0, 0, 0, 0.25)");
  });

  it("returns non-hex colours untouched", () => {
    expect(withAlpha("rgb(1, 2, 3)", 0.5)).toBe("rgb(1, 2, 3)");
    expect(withAlpha("currentColor", 0.5)).toBe("currentColor");
  });

  it("returns malformed hex untouched rather than emitting NaN channels", () => {
    expect(withAlpha("#12345", 0.5)).toBe("#12345");
    expect(withAlpha("#gggggg", 0.5)).toBe("#gggggg");
  });
});

describe("recencyColor", () => {
  it("anchors the ends of the gradient", () => {
    expect(recencyColor(0)).toBe("rgb(58, 110, 224)");
    expect(recencyColor(1)).toBe("rgb(235, 83, 62)");
  });

  it("interpolates the midpoint", () => {
    expect(recencyColor(0.5)).toBe("rgb(147, 97, 143)");
  });

  it("emits rgba only when an alpha below 1 is asked for", () => {
    expect(recencyColor(0, 0.4)).toBe("rgba(58, 110, 224, 0.4)");
    expect(recencyColor(0, 1)).toBe("rgb(58, 110, 224)");
  });
});

describe("scale / toPx / toData", () => {
  it("maps the data range onto the pixel range", () => {
    const s = scale(0, 10, 0, 100);
    expect(toPx(s, 0)).toBe(0);
    expect(toPx(s, 5)).toBe(50);
    expect(toPx(s, 10)).toBe(100);
  });

  it("handles an inverted pixel range, as the y axis uses", () => {
    const s = scale(0, 1, 200, 0);
    expect(toPx(s, 0)).toBe(200);
    expect(toPx(s, 1)).toBe(0);
  });

  it("round-trips through toData", () => {
    const s = scale(-3, 7, 10, 260);
    for (const v of [-3, 0, 2.5, 7]) {
      expect(toData(s, toPx(s, v))).toBeCloseTo(v, 10);
    }
  });

  it("nudges a zero-width domain apart so a flat series stays mid-axis", () => {
    const s = scale(4, 4, 0, 100);
    expect(s.d0).toBeLessThan(s.d1);
    expect(toPx(s, 4)).toBeCloseTo(50, 10);
  });

  it("uses an absolute pad when the flat value is zero", () => {
    const s = scale(0, 0, 0, 100);
    expect(s.d0).toBe(-0.5);
    expect(s.d1).toBe(0.5);
  });

  it("treats a descending domain as zero-width rather than inverting the axis", () => {
    const s = scale(10, 2, 0, 100);
    expect(s.d0).toBeLessThan(s.d1);
    expect(toPx(s, 10)).toBeCloseTo(50, 10);
  });
});

describe("padRange", () => {
  it("pads outward by the given fraction", () => {
    expect(padRange(0, 10, 0.1)).toEqual([-1, 11]);
  });

  it("defaults to an 8% pad", () => {
    const [lo, hi] = padRange(0, 100);
    expect(lo).toBeCloseTo(-8, 10);
    expect(hi).toBeCloseTo(108, 10);
  });

  it("pads a single value proportionally", () => {
    expect(padRange(5, 5)).toEqual([4.5, 5.5]);
  });

  it("pads a single zero absolutely", () => {
    expect(padRange(0, 0)).toEqual([-0.5, 0.5]);
  });

  it("falls back to a unit range for non-finite input", () => {
    expect(padRange(NaN, 1)).toEqual([-1, 1]);
    expect(padRange(0, Infinity)).toEqual([-1, 1]);
  });
});

describe("niceTicks", () => {
  it("produces 1/2/5 times a power of ten as the step", () => {
    expect(niceTicks(0, 10)).toEqual([0, 5, 10]);
    expect(niceTicks(0, 4)).toEqual([0, 1, 2, 3, 4]);
    expect(niceTicks(0, 100)).toEqual([0, 50, 100]);
  });

  it("stays inside the requested domain", () => {
    for (const t of niceTicks(-0.037, 0.041)) {
      expect(t).toBeGreaterThanOrEqual(-0.037);
      expect(t).toBeLessThanOrEqual(0.041);
    }
  });

  it("returns nothing for an empty or invalid domain", () => {
    expect(niceTicks(5, 5)).toEqual([]);
    expect(niceTicks(10, 1)).toEqual([]);
    expect(niceTicks(NaN, 1)).toEqual([]);
  });

  it("snaps a float-error zero to exactly 0", () => {
    const ticks = niceTicks(-0.3, 0.3);
    expect(ticks).toContain(0);
    expect(ticks.some((t) => Object.is(t, -0))).toBe(false);
  });

  it("returns roughly the requested number of ticks", () => {
    const ticks = niceTicks(0, 1, 5);
    expect(ticks.length).toBeGreaterThanOrEqual(3);
    expect(ticks.length).toBeLessThanOrEqual(11);
  });

  it("emits an ascending, evenly spaced sequence", () => {
    const ticks = niceTicks(-1.234, 5.678);
    expect(ticks.length).toBeGreaterThan(1);
    const step = ticks[1] - ticks[0];
    for (let i = 1; i < ticks.length; i++) {
      expect(ticks[i] - ticks[i - 1]).toBeCloseTo(step, 9);
    }
  });
});

describe("stepPrecision", () => {
  it("gives enough decimals to tell neighbouring ticks apart", () => {
    expect(stepPrecision(1)).toBe(1);
    expect(stepPrecision(0.1)).toBe(2);
    expect(stepPrecision(0.01)).toBe(3);
  });

  it("clamps at zero for large steps and at nine for tiny ones", () => {
    expect(stepPrecision(1000)).toBe(0);
    expect(stepPrecision(1e-30)).toBe(9);
  });

  it("falls back to three decimals for a non-positive step", () => {
    expect(stepPrecision(0)).toBe(3);
    expect(stepPrecision(-1)).toBe(3);
  });
});

describe("formatMetres", () => {
  it("fixes to the requested precision", () => {
    expect(formatMetres(1.23456, 3)).toBe("1.235");
  });

  it("never renders a negative zero", () => {
    expect(formatMetres(-0.0001, 2)).toBe("0.00");
    expect(formatMetres(-0, 3)).toBe("0.000");
  });

  it("keeps a real negative value negative", () => {
    expect(formatMetres(-1.5, 1)).toBe("-1.5");
  });
});

describe("formatMillimetres", () => {
  it("drops decimals above 100 mm", () => {
    expect(formatMillimetres(0.1234)).toBe("123 mm");
  });

  it("keeps one decimal between 10 and 100 mm", () => {
    expect(formatMillimetres(0.0123)).toBe("12.3 mm");
  });

  it("keeps two decimals below 10 mm", () => {
    expect(formatMillimetres(0.00123)).toBe("1.23 mm");
  });

  it("scales negatives by magnitude, not sign", () => {
    expect(formatMillimetres(-0.1234)).toBe("-123 mm");
  });
});

describe("timeAxisTicks", () => {
  const t0 = Date.UTC(2024, 0, 2, 0, 0, 0);

  it("returns nothing for a non-positive span", () => {
    expect(timeAxisTicks(t0, t0, 6, "UTC")).toEqual([]);
    expect(timeAxisTicks(t0 + 1000, t0, 6, "UTC")).toEqual([]);
  });

  it("labels in the chosen time zone, never the machine's own", () => {
    expect(timeAxisTicks(t0, t0 + 3 * 3_600_000, 6, "UTC")[0].label).toBe("00:00");
    // AEDT (UTC+11) in January.
    expect(timeAxisTicks(t0, t0 + 3 * 3_600_000, 6, "Australia/Sydney")[0].label).toBe("11:00");
  });

  it("aligns ticks to a whole-second step boundary", () => {
    const ticks = timeAxisTicks(t0 + 12_345, t0 + 3 * 3_600_000, 6, "UTC");
    for (const t of ticks) expect(t.value % 1000).toBe(0);
  });

  it("shows seconds only for sub-minute steps", () => {
    const fine = timeAxisTicks(t0, t0 + 20_000, 6, "UTC");
    expect(fine[0].label).toMatch(/^\d{2}:\d{2}:\d{2}$/);

    const coarse = timeAxisTicks(t0, t0 + 6 * 3_600_000, 6, "UTC");
    expect(coarse[0].label).toMatch(/^\d{2}:\d{2}$/);
  });

  it("adds the date on a range spanning more than one day", () => {
    const ticks = timeAxisTicks(t0, t0 + 3 * 86_400_000, 6, "UTC");
    expect(ticks[0].label).toContain("\n");
    expect(ticks[0].label).toMatch(/^\d+ [A-Z][a-z]{2}\n/);
  });

  it("keeps every tick inside the range", () => {
    const lo = t0 + 999;
    const hi = t0 + 7 * 3_600_000;
    for (const t of timeAxisTicks(lo, hi, 6, "UTC")) {
      expect(t.value).toBeGreaterThanOrEqual(lo);
      expect(t.value).toBeLessThanOrEqual(hi);
    }
  });
});

describe("formatEpoch", () => {
  it("renders the .pos file's own notation in UTC", () => {
    expect(formatEpoch(Date.UTC(2024, 0, 2, 3, 4, 5), "UTC", { zone: false })).toBe("2024-01-02 03:04:05");
  });

  it("zero-pads every field", () => {
    expect(formatEpoch(Date.UTC(2001, 8, 9, 1, 2, 3), "UTC", { zone: false })).toBe("2001-09-09 01:02:03");
  });

  it("shows the chosen zone, and names it unless asked not to", () => {
    const ms = Date.UTC(2024, 0, 2, 3, 4, 5);
    expect(formatEpoch(ms, "Australia/Sydney", { zone: false })).toBe("2024-01-02 14:04:05");
    expect(formatEpoch(ms, "UTC")).toMatch(/^2024-01-02 03:04:05 \S+$/);
  });
});

describe("formatDuration", () => {
  it("renders seconds, minutes, hours and days", () => {
    expect(formatDuration(45_000)).toBe("45 s");
    expect(formatDuration(45 * 60_000)).toBe("45 m");
    expect(formatDuration(3 * 3_600_000 + 20 * 60_000)).toBe("3 h 20 m");
    expect(formatDuration(2 * 86_400_000 + 3_600_000)).toBe("2 d 1 h");
  });

  it("drops a zero minute remainder from an exact hour", () => {
    expect(formatDuration(3_600_000)).toBe("1 h");
  });

  it("shows an em dash for a non-positive or non-finite duration", () => {
    expect(formatDuration(0)).toBe("-");
    expect(formatDuration(-1)).toBe("-");
    expect(formatDuration(NaN)).toBe("-");
  });
});

describe("nearestIndex", () => {
  const xs = [0, 10, 20, 30, 40];

  it("finds the closest sample", () => {
    expect(nearestIndex(xs, 21)).toBe(2);
    expect(nearestIndex(xs, 26)).toBe(3);
  });

  it("clamps to the ends", () => {
    expect(nearestIndex(xs, -100)).toBe(0);
    expect(nearestIndex(xs, 1000)).toBe(4);
  });

  it("returns -1 for an empty series", () => {
    expect(nearestIndex([], 1)).toBe(-1);
  });

  it("handles a single-sample series", () => {
    expect(nearestIndex([7], 100)).toBe(0);
  });

  it("agrees with a linear scan over a long series", () => {
    const long = Array.from({ length: 500 }, (_, i) => i * 3);
    for (const target of [0, 4, 5, 7, 748, 1497]) {
      const brute = long.reduce(
        (best, v, i) => (Math.abs(v - target) < Math.abs(long[best] - target) ? i : best),
        0,
      );
      expect(nearestIndex(long, target)).toBe(brute);
    }
  });
});

describe("readVizTheme", () => {
  it("falls back to the dark palette with no element", () => {
    const theme = readVizTheme(null);
    expect(theme.series).toHaveLength(6);
    expect(theme.surface).toBe("#2c2c2c");
  });

  it("reads the palette off the element's custom properties", () => {
    const el = document.createElement("div");
    el.style.setProperty("--viz-surface", "#ffffff");
    el.style.setProperty("--viz-series-1", "#111111");
    document.body.appendChild(el);

    const theme = readVizTheme(el);
    expect(theme.surface).toBe("#ffffff");
    expect(theme.series[0]).toBe("#111111");
    // Unset properties keep their fallback rather than becoming "".
    expect(theme.grid).toBe("#444444");
  });
});

describe("prepareCanvas", () => {
  /** Canvas double that counts backing-store writes, which jsdom cannot do. */
  function fakeCanvas() {
    const ctx = {
      setTransform: vi.fn(),
      clearRect: vi.fn(),
    };
    const sizeWrites = { width: 0, height: 0 };
    let w = 0;
    let h = 0;
    const canvas = {
      get width() {
        return w;
      },
      set width(v: number) {
        sizeWrites.width++;
        w = v;
      },
      get height() {
        return h;
      },
      set height(v: number) {
        sizeWrites.height++;
        h = v;
      },
      getContext: vi.fn(() => ctx),
    } as unknown as HTMLCanvasElement;
    return { canvas, ctx, sizeWrites };
  }

  it("returns null for a zero-sized box or a missing canvas", () => {
    const { canvas } = fakeCanvas();
    expect(prepareCanvas(canvas, 0, 100)).toBeNull();
    expect(prepareCanvas(canvas, 100, 0)).toBeNull();
    expect(prepareCanvas(null, 100, 100)).toBeNull();
  });

  it("sizes the backing store for the device pixel ratio", () => {
    const { canvas, ctx } = fakeCanvas();
    const original = window.devicePixelRatio;
    Object.defineProperty(window, "devicePixelRatio", { value: 2, configurable: true });

    const result = prepareCanvas(canvas, 300, 150);

    expect(canvas.width).toBe(600);
    expect(canvas.height).toBe(300);
    expect(ctx.setTransform).toHaveBeenCalledWith(2, 0, 0, 2, 0, 0);
    expect(ctx.clearRect).toHaveBeenCalledWith(0, 0, 300, 150);
    expect(result).toBe(ctx);

    Object.defineProperty(window, "devicePixelRatio", { value: original, configurable: true });
  });

  it("leaves an already correctly sized backing store alone", () => {
    const { canvas, sizeWrites } = fakeCanvas();
    Object.defineProperty(window, "devicePixelRatio", { value: 1, configurable: true });

    prepareCanvas(canvas, 200, 100);
    expect(sizeWrites).toEqual({ width: 1, height: 1 });

    // Resizing the backing store clears it, so an unchanged box must not touch
    // it — a redraw at the same size would otherwise flash.
    prepareCanvas(canvas, 200, 100);
    expect(sizeWrites).toEqual({ width: 1, height: 1 });

    prepareCanvas(canvas, 240, 100);
    expect(sizeWrites).toEqual({ width: 2, height: 2 });
  });
});

describe("drawEdgeSafeLabel", () => {
  function ctxStub(textWidth: number) {
    return {
      measureText: vi.fn(() => ({ width: textWidth })),
      fillText: vi.fn(),
      textAlign: "center" as CanvasTextAlign,
    } as unknown as CanvasRenderingContext2D;
  }

  it("centres a label with room on both sides", () => {
    const ctx = ctxStub(40);
    drawEdgeSafeLabel(ctx, "00:30", 200, 10, 400);
    expect(ctx.textAlign).toBe("center");
    expect(ctx.fillText).toHaveBeenCalledWith("00:30", 200, 10);
  });

  it("right-aligns a label that would overflow the right edge", () => {
    const ctx = ctxStub(40);
    drawEdgeSafeLabel(ctx, "00:30", 395, 10, 400);
    expect(ctx.textAlign).toBe("right");
  });

  it("left-aligns a label that would overflow the left edge", () => {
    const ctx = ctxStub(40);
    drawEdgeSafeLabel(ctx, "00:30", 3, 10, 400);
    expect(ctx.textAlign).toBe("left");
  });
});

describe("PAD", () => {
  it("leaves a left gutter wide enough for a signed metre value", () => {
    expect(PAD.left).toBeGreaterThan(PAD.right);
    expect(PAD.bottom).toBeGreaterThan(PAD.top);
  });
});
