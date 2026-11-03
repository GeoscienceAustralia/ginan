import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import type { MeasKind, ResidualSeries } from "../types";
import {
  PAD,
  legendWidth,
  drawGrid,
  drawLegend,
  drawXAxis,
  drawYAxis,
  formatEpoch,
  formatResidual,
  nearestIndex,
  niceTicks,
  padRange,
  prepareCanvas,
  readVizTheme,
  scale,
  stepPrecision,
  timeAxisTicks,
  toData,
  toPx,
  useElementSize,
  withAlpha,
  type LegendEntry,
  type Scale,
  type VizTheme,
  useCanvasRedraw,
  canvasPoint,
} from "./chartUtils";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * Measurement residuals from a `.TRACE` against time — the figure
 * `scripts/plot_trace_res.py` wrote to an HTML file, drawn in place.
 *
 * Laid out like {@link PosTimeSeriesChart}, and for the same reason: code and
 * phase residuals differ by two orders of magnitude (decimetres against
 * millimetres), so overlaying them on one pair of axes flattens phase into a
 * flat line at zero. One facet per measurement type over a shared time axis
 * keeps each at a scale that shows its own structure.
 *
 * Within a facet, colour is the signal — the comparison that matters is
 * between one satellite's signals, since a single bad frequency is what a
 * residual plot is usually being read for. Postfit is the solid line (what the
 * filter was left with) and prefit the dashed one (what it was handed); the
 * gap between them is the correction the filter applied.
 *
 * Colour is never the only channel: each facet is captioned, carries a legend
 * in the same reserved right gutter the position charts use, and
 * every drawn series is also a row in the statistics table underneath.
 */

const GAP = 12;
/** Below this pixel spacing, markers merge into the line and are dropped. */
const MARKER_MIN_SPACING = 11;
/** A drag shorter than this reads as a click, not a zoom. */
const MIN_DRAG_PX = 6;
/** Palette slots signals are coloured from. Six, because a multi-frequency
 *  satellite carries five signals and colours that repeat within one facet
 *  can't be told apart; signals past the sixth cycle. */
const PALETTE_SLOTS = 6;

const KIND_ORDER: MeasKind[] = ["code", "phase", "other"];
const KIND_LABEL: Record<MeasKind, string> = {
  code: "Code",
  phase: "Phase",
  other: "Other",
};

interface Props {
  /** The series to draw — already narrowed to one satellite's worth. */
  series: ResidualSeries[];
  /** File name, for the canvas description. */
  name: string;
  /** Draw the dashed prefit line alongside the solid postfit one. */
  showPrefit: boolean;
}

export default function TraceResidualChart({ series, name, showPrefit }: Props) {
  const timeZone = useTimeZone();
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  /** Visible time window in epoch ms; null is "all of it". */
  const [view, setView] = useState<[number, number] | null>(null);
  const [hover, setHover] = useState<{ t: number; px: number; py: number } | null>(null);
  const [drag, setDrag] = useState<{ from: number; to: number } | null>(null);

  /** Identity of the current selection — a different satellite or file is a
   *  different time axis, so any zoom into the previous one is meaningless. */
  const selectionId = series.map((s) => s.key).join(",");
  useEffect(() => {
    setView(null);
    setHover(null);
    setDrag(null);
  }, [selectionId]);

  /** Signal → palette slot, assigned in the order signals appear so the same
   *  signal keeps its colour across both facets. */
  const colorOf = useMemo(() => {
    const slots = new Map<string, number>();
    for (const s of series) {
      if (!slots.has(s.signal)) slots.set(s.signal, slots.size % PALETTE_SLOTS);
    }
    return slots;
  }, [series]);

  /** Facets in a fixed order, skipping measurement types this selection has
   *  none of, so a code-only run gets the full canvas for its one panel. */
  const facets = useMemo(
    () =>
      KIND_ORDER.map((kind) => ({ kind, lines: series.filter((s) => s.kind === kind) })).filter(
        (f) => f.lines.length > 0,
      ),
    [series],
  );

  /** Every epoch any drawn series has a sample at, sorted — the grid the
   *  crosshair snaps to. */
  const allT = useMemo(() => {
    const set = new Set<number>();
    for (const s of series) for (const t of s.t) set.add(t);
    return Array.from(set).sort((a, b) => a - b);
  }, [series]);

  const full = useMemo<[number, number]>(
    () => (allT.length > 0 ? [allT[0], allT[allT.length - 1]] : [0, 1]),
    [allT],
  );
  const span: [number, number] = view ?? full;

  const layout = useMemo(() => {
    const count = Math.max(1, facets.length);
    const facetHeight = (size.height - PAD.top - PAD.bottom - GAP * (count - 1)) / count;
    return {
      facetHeight,
      // Right edge stops short of the legend gutter so no series is drawn
      // underneath it.
      x: scale(
        span[0],
        span[1],
        PAD.left,
        Math.max(PAD.left + 1, size.width - PAD.right - legendWidth(size.width)),
      ),
      /** Top edge of facet i. */
      top: (i: number) => PAD.top + i * (facetHeight + GAP),
    };
  }, [size.width, size.height, span[0], span[1], facets.length]);

  /** Index range per series covering the window, one sample either side so
   *  lines reach the frame instead of stopping short of it. */
  const ranges = useMemo(() => {
    const map = new Map<string, [number, number]>();
    for (const s of series) {
      if (s.t.length === 0) continue;
      const lo = Math.max(0, nearestIndex(s.t, span[0]) - 1);
      const hi = Math.min(s.t.length - 1, nearestIndex(s.t, span[1]) + 1);
      map.set(s.key, [lo, hi]);
    }
    return map;
  }, [series, span[0], span[1]]);

  /** Y scale per facet, from the visible slice only — zooming in on a quiet
   *  stretch should resolve it, not keep the whole run's range. */
  const yScales = useMemo(() => {
    return facets.map((facet, i) => {
      let min = Infinity;
      let max = -Infinity;
      for (const s of facet.lines) {
        const range = ranges.get(s.key);
        if (!range) continue;
        for (let k = range[0]; k <= range[1]; k++) {
          const post = s.postfit[k];
          if (post < min) min = post;
          if (post > max) max = post;
          if (showPrefit) {
            const pre = s.prefit[k];
            if (pre < min) min = pre;
            if (pre > max) max = pre;
          }
        }
      }
      // Zero is the reference a residual is read against, so keep it in frame
      // even when a whole panel sits to one side of it.
      min = Math.min(min, 0);
      max = Math.max(max, 0);
      const [lo, hi] = padRange(min, max);
      const top = layout.top(i);
      return scale(lo, hi, top + layout.facetHeight, top);
    });
  }, [facets, ranges, showPrefit, layout]);

  // The canvas only needs the hovered epoch; the pointer height is for the
  // tooltip alone, so moving along a column does not redraw.
  const hoverT = hover?.t ?? null;

  const draw = useCallback(() => {
    const ctx = prepareCanvas(canvasRef.current, size.width, size.height);
    if (!ctx || layout.facetHeight <= 8 || facets.length === 0) return;
    const theme = readVizTheme(wrapRef.current);
    const { x } = layout;
    const xTicks = timeAxisTicks(span[0], span[1], Math.max(2, Math.floor(size.width / 110)), timeZone);

    facets.forEach((facet, i) => {
      const y = yScales[i];
      const yTickCount = Math.max(2, Math.min(5, Math.floor(layout.facetHeight / 26)));
      const yTicks = niceTicks(y.d0, y.d1, yTickCount);
      const precision = stepPrecision(yTicks.length > 1 ? yTicks[1] - yTicks[0] : (y.d1 - y.d0) / 4);

      drawGrid(ctx, theme, x, y, yTicks);
      drawYAxis(ctx, theme, y, yTicks, precision, x.p0);

      ctx.save();
      ctx.beginPath();
      ctx.rect(x.p0, y.p1, x.p1 - x.p0, y.p0 - y.p1);
      ctx.clip();

      // Zero line: the value every residual is being compared against.
      const zero = Math.round(toPx(y, 0)) + 0.5;
      ctx.strokeStyle = withAlpha(theme.text, 0.22);
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(x.p0, zero);
      ctx.lineTo(x.p1, zero);
      ctx.stroke();

      for (const s of facet.lines) {
        const range = ranges.get(s.key);
        if (!range) continue;
        const hue = theme.series[colorOf.get(s.signal) ?? 0];
        if (showPrefit) {
          drawLine(ctx, x, y, s.t, s.prefit, range, withAlpha(hue, 0.55), theme, {
            dashed: true,
            markers: false,
          });
        }
        drawLine(ctx, x, y, s.t, s.postfit, range, hue, theme, { dashed: false, markers: true });
      }

      if (hoverT !== null) {
        for (const s of facet.lines) {
          const k = sampleAt(s, hoverT, span, size.width);
          if (k === null) continue;
          drawHoverMarker(ctx, x, y, s.t[k], s.postfit[k], theme.series[colorOf.get(s.signal) ?? 0], theme);
        }
      }
      ctx.restore();

      drawFacetCaption(ctx, theme, facet.kind, x.p0, y.p1);

      // Per facet, because the same signal can be absent from one of them —
      // one shared legend would list signals a panel doesn't draw.
      drawLegend(
        ctx,
        theme,
        legendFor(facet.lines, colorOf, theme, showPrefit),
        x.p1 + 12,
        y.p1,
        y.p0,
      );
    });

    drawXAxis(ctx, theme, x, xTicks, layout.top(facets.length - 1) + layout.facetHeight, size.width);

    const bottom = layout.top(facets.length - 1) + layout.facetHeight;
    if (hoverT !== null) drawCrosshair(ctx, theme, toPx(x, hoverT), PAD.top, bottom);
    if (drag && Math.abs(drag.to - drag.from) >= MIN_DRAG_PX) {
      drawSelection(ctx, theme, drag, PAD.top, bottom);
    }
  }, [facets, ranges, yScales, colorOf, size, layout, showPrefit, hoverT, drag, span[0], span[1], timeZone]);

  useCanvasRedraw(draw);

  const onPointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const { px, py } = canvasPoint(e);
    if (drag) {
      setDrag({ ...drag, to: clamp(px, layout.x.p0, layout.x.p1) });
      return;
    }
    if (px < layout.x.p0 || px > layout.x.p1 || allT.length === 0) {
      setHover(null);
      return;
    }
    const t = allT[nearestIndex(allT, toData(layout.x, px))];
    setHover({ t, px: toPx(layout.x, t), py });
  };

  const onPointerDown = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (e.button !== 0) return;
    const { px } = canvasPoint(e);
    if (px < layout.x.p0 || px > layout.x.p1) return;
    // Capture keeps the drag alive if the cursor leaves the canvas mid-select.
    // It is an optimisation, not the mechanism — a browser that refuses it must
    // not lose the zoom gesture, so failure is swallowed.
    try {
      e.currentTarget.setPointerCapture(e.pointerId);
    } catch {
      /* not capturable — the drag still works while the cursor stays inside */
    }
    setDrag({ from: px, to: px });
    setHover(null);
  };

  const onPointerUp = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (!drag) return;
    const { px } = canvasPoint(e);
    const to = clamp(px, layout.x.p0, layout.x.p1);
    setDrag(null);
    if (Math.abs(to - drag.from) < MIN_DRAG_PX) return;
    const a = toData(layout.x, Math.min(drag.from, to));
    const b = toData(layout.x, Math.max(drag.from, to));
    // Two epochs is the floor: any narrower and there is nothing to see.
    if (nearestIndex(allT, b) - nearestIndex(allT, a) < 1) return;
    setView([a, b]);
  };

  const zoomed = view !== null;
  const hoverRows = hover
    ? series
        .map((s) => {
          const k = sampleAt(s, hover.t, span, size.width);
          if (k === null) return null;
          return {
            key: s.key,
            label: `${s.signal} ${KIND_LABEL[s.kind].toLowerCase()}`,
            color: `var(--viz-series-${(colorOf.get(s.signal) ?? 0) + 1})`,
            postfit: formatResidual(s.postfit[k]),
            prefit: formatResidual(s.prefit[k]),
          };
        })
        .filter((r): r is NonNullable<typeof r> => r !== null)
    : [];

  if (series.length === 0) {
    return <div className="viz-state">No residuals for this selection.</div>;
  }

  return (
    <div className="viz-chart" ref={wrapRef}>
      <canvas
        ref={canvasRef}
        className="viz-canvas"
        role="img"
        aria-label={`Prefit and postfit measurement residuals from ${name} for ${describe(series)}. The figures are in the statistics table below.`}
        style={{ width: "100%", height: "100%", cursor: drag ? "col-resize" : "crosshair" }}
        onPointerMove={onPointerMove}
        onPointerDown={onPointerDown}
        onPointerUp={onPointerUp}
        onPointerLeave={() => {
          setHover(null);
          setDrag(null);
        }}
        onDoubleClick={() => setView(null)}
      />

      {hover && !drag && hoverRows.length > 0 && (
        <div
          className="viz-tooltip"
          style={{
            left: Math.min(hover.px + 14, Math.max(0, size.width - 210)),
            top: Math.min(hover.py + 12, Math.max(0, size.height - 40 - hoverRows.length * 18)),
          }}
        >
          <div className="viz-tooltip-time">{formatEpoch(hover.t, timeZone)}</div>
          <table>
            <tbody>
              {hoverRows.map((r) => (
                <tr key={r.key}>
                  <td>
                    <span className="viz-key" style={{ background: r.color }} />
                    {r.label}
                  </td>
                  <td className="viz-num">{r.postfit}</td>
                  {showPrefit && <td className="viz-num viz-dim">{r.prefit}</td>}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {zoomed && (
        <button className="viz-reset" onClick={() => setView(null)} title="Show the whole run">
          Reset zoom
        </button>
      )}
      {!zoomed && allT.length > 2 && (
        <span className="viz-hint">Drag to zoom · double-click to reset</span>
      )}
    </div>
  );
}

// ── drawing ──────────────────────────────────────────────────────────────────

/**
 * The sample index of `s` at the hovered time, or null when this series has no
 * sample there. A satellite that was not being tracked at that epoch must show
 * no row rather than a value borrowed from minutes away, so the match has to
 * land within a few pixels' worth of time.
 */
function sampleAt(
  s: ResidualSeries,
  t: number,
  span: [number, number],
  width: number,
): number | null {
  if (s.t.length === 0) return null;
  const k = nearestIndex(s.t, t);
  const perPixel = (span[1] - span[0]) / Math.max(1, width);
  return Math.abs(s.t[k] - t) <= perPixel * 4 ? k : null;
}

function drawLine(
  ctx: CanvasRenderingContext2D,
  x: Scale,
  y: Scale,
  t: number[],
  values: number[],
  [i0, i1]: [number, number],
  color: string,
  theme: VizTheme,
  opts: { dashed: boolean; markers: boolean },
) {
  ctx.save();
  ctx.strokeStyle = color;
  ctx.lineWidth = opts.dashed ? 1.25 : 2;
  ctx.lineJoin = "round";
  ctx.lineCap = "round";
  if (opts.dashed) ctx.setLineDash([4, 3]);
  ctx.beginPath();
  for (let k = i0; k <= i1; k++) {
    const px = toPx(x, t[k]);
    const py = toPx(y, values[k]);
    if (k === i0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  }
  ctx.stroke();
  ctx.setLineDash([]);

  // Markers only once they can be told apart; otherwise they thicken the line
  // into a blob and say nothing the line hasn't.
  const count = i1 - i0 + 1;
  const spacing = count > 1 ? (x.p1 - x.p0) / (count - 1) : Infinity;
  if (opts.markers && spacing >= MARKER_MIN_SPACING) {
    for (let k = i0; k <= i1; k++) {
      ctx.beginPath();
      ctx.arc(toPx(x, t[k]), toPx(y, values[k]), 3.5, 0, Math.PI * 2);
      ctx.fillStyle = color;
      ctx.fill();
      // Surface ring, so overlapping markers stay countable.
      ctx.lineWidth = 2;
      ctx.strokeStyle = theme.surface;
      ctx.stroke();
    }
  }
  ctx.restore();
}

function drawHoverMarker(
  ctx: CanvasRenderingContext2D,
  x: Scale,
  y: Scale,
  t: number,
  value: number,
  color: string,
  theme: VizTheme,
) {
  ctx.save();
  ctx.beginPath();
  ctx.arc(toPx(x, t), toPx(y, value), 4.5, 0, Math.PI * 2);
  ctx.fillStyle = color;
  ctx.fill();
  ctx.lineWidth = 2;
  ctx.strokeStyle = theme.surface;
  ctx.stroke();
  ctx.restore();
}

/** Which measurement type a panel holds, inside its top-left. */
function drawFacetCaption(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  kind: MeasKind,
  x0: number,
  yTop: number,
) {
  ctx.save();
  ctx.font = "600 11px ui-sans-serif, system-ui, sans-serif";
  ctx.fillStyle = theme.text;
  ctx.textAlign = "left";
  ctx.textBaseline = "middle";
  ctx.fillText(`${KIND_LABEL[kind]} residuals`, x0 + 8, yTop + 12);
  ctx.restore();
}

/**
 * One entry per signal drawn in the panel, plus the dashed prefit line when it
 * is shown. Residuals are metres, the same unit the position charts label.
 */
function legendFor(
  lines: ResidualSeries[],
  colorOf: Map<string, number>,
  theme: VizTheme,
  showPrefit: boolean,
): LegendEntry[] {
  const entries: LegendEntry[] = lines.map((s) => ({
    label: s.signal || s.sat,
    unit: "m",
    color: theme.series[colorOf.get(s.signal) ?? 0],
    swatch: "line",
  }));
  if (showPrefit) {
    // Colourless: the dash is what marks a line as prefit, and it applies to
    // every signal in the panel rather than to one of them.
    entries.push({ label: "Prefit", unit: "m", color: theme.textDim, swatch: "dashed" });
  }
  return entries;
}

function drawCrosshair(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  px: number,
  yTop: number,
  yBottom: number,
) {
  ctx.save();
  ctx.strokeStyle = withAlpha(theme.text, 0.28);
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(Math.round(px) + 0.5, yTop);
  ctx.lineTo(Math.round(px) + 0.5, yBottom);
  ctx.stroke();
  ctx.restore();
}

function drawSelection(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  drag: { from: number; to: number },
  yTop: number,
  yBottom: number,
) {
  const x0 = Math.min(drag.from, drag.to);
  const w = Math.abs(drag.to - drag.from);
  ctx.save();
  ctx.fillStyle = withAlpha(theme.text, 0.1);
  ctx.fillRect(x0, yTop, w, yBottom - yTop);
  ctx.strokeStyle = withAlpha(theme.text, 0.35);
  ctx.lineWidth = 1;
  ctx.strokeRect(Math.round(x0) + 0.5, yTop + 0.5, Math.round(w), yBottom - yTop - 1);
  ctx.restore();
}

/** "G07 at ALIC on L1C, L2W" — the canvas description's subject. */
function describe(series: ResidualSeries[]): string {
  const sats = Array.from(new Set(series.map((s) => s.sat)));
  const receivers = Array.from(new Set(series.map((s) => s.receiver).filter(Boolean)));
  const signals = Array.from(new Set(series.map((s) => s.signal).filter(Boolean)));
  const at = receivers.length > 0 ? ` at ${receivers.join(", ")}` : "";
  const on = signals.length > 0 ? ` on ${signals.join(", ")}` : "";
  return `${sats.join(", ")}${at}${on}`;
}

function clamp(v: number, lo: number, hi: number): number {
  return Math.min(hi, Math.max(lo, v));
}
