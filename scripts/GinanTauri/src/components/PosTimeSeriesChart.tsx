import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { drawSigmaEnvelope, drawSeriesLine, drawHoverMarker, drawFacetCaption, legendFor } from "./posTimeSeriesDraw";
import type { PosSeries, PosComponentKey } from "../types";
import {
  PAD,
  legendWidth,
  clamp,
  drawCrosshair,
  drawGrid,
  drawLegend,
  drawSelectedRing,
  drawSelectedTime,
  drawSelection,
  drawXAxis,
  drawYAxis,
  formatEpoch,
  formatMillimetres,
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
  useCanvasRedraw,
  canvasPoint,
} from "./chartUtils";
import { useSelectedIndex, useSelectedTime, useTimeLink } from "./viz/VizContext";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * Position residuals against time — the figure `plot_pos.py` wrote as
 * `*_fig1.html`, drawn in place.
 *
 * The three components sit in their own stacked panels over a shared time axis
 * rather than overlaid on one pair of axes. Overlaid, dN/dE/dU all cluster
 * around zero and the vertical component (typically three times the horizontal
 * scatter) dictates a y range that flattens the other two into a single band.
 * Faceted, each keeps a scale that shows its own structure, and the shared x
 * axis still lines events up across all three.
 *
 * Per panel: the ±1σ formal error as an envelope around the solution (the
 * continuous form of the original's per-epoch error bars, which smear into a
 * solid block at these point counts), the inverse-variance weighted mean as a
 * line, and the ±2σ scatter bounds as a neutral band — anything outside it is
 * in the 5% tail.
 */

export const GAP = 12;
/** Below this pixel spacing, markers merge into the line and are dropped. */
export const MARKER_MIN_SPACING = 11;
/** A drag shorter than this reads as a click, not a zoom. */
const MIN_DRAG_PX = 6;

export interface Facet {
  key: PosComponentKey;
  label: string;
  /** Full name, for the tooltip and the in-panel caption. */
  axis: string;
}

export const FACETS: Facet[] = [
  { key: "dn", label: "dN", axis: "North" },
  { key: "de", label: "dE", axis: "East" },
  { key: "du", label: "dU", axis: "Up" },
];

export const SIGMA_OF: Record<PosComponentKey, "sn" | "se" | "su"> = {
  dn: "sn",
  de: "se",
  du: "su",
};

interface Props {
  series: PosSeries;
  /** Draw the ±1σ formal-error envelope. */
  showSigma: boolean;
  /** Draw the weighted mean and its ±2σ scatter bounds. */
  showCi: boolean;
  /** Which components get a panel; all three when omitted. */
  components?: PosComponentKey[];
}

export default function PosTimeSeriesChart({ series, showSigma, showCi, components }: Props) {
  const timeZone = useTimeZone();
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  // Visible time window (epoch ms, null is "all of it") and hover time. Both
  // are shared with the other time-based panels when their axes are linked;
  // a different file resets them, since it is a different time axis.
  const { view, setView, hoverT, hoverIsMine, setHoverT } = useTimeLink(series.path);
  /** Pointer height, for placing the tooltip in the panel the pointer is in. */
  const [pointerY, setPointerY] = useState(0);
  const [drag, setDrag] = useState<{ from: number; to: number } | null>(null);

  useEffect(() => setDrag(null), [series.path]);

  // Keeps each facet's slot in the palette (dN is always series-1, …) however
  // many of them are shown.
  const facets = useMemo(
    () =>
      FACETS.map((f, slot) => ({ ...f, slot })).filter(
        (f) => !components || components.length === 0 || components.includes(f.key),
      ),
    [components],
  );

  const full = useMemo<[number, number]>(
    () => [series.t[0], series.t[series.t.length - 1]],
    [series],
  );
  const span: [number, number] = view ?? full;

  /** Index range covering the window, one sample either side so lines reach
   *  the frame instead of stopping short of it. */
  const [i0, i1] = useMemo(() => {
    const lo = Math.max(0, nearestIndex(series.t, span[0]) - 1);
    const hi = Math.min(series.t.length - 1, nearestIndex(series.t, span[1]) + 1);
    return [lo, hi];
  }, [series, span[0], span[1]]);

  const layout = useMemo(() => {
    const facetHeight = (size.height - PAD.top - PAD.bottom - GAP * (facets.length - 1)) / facets.length;
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

  /** Y scale per facet, from the visible slice only — zooming in on a quiet
   *  stretch should resolve it, not keep the whole run's range. */
  const yScales = useMemo(() => {
    return facets.map((facet, i) => {
      const values = series[facet.key];
      const sigmas = series[SIGMA_OF[facet.key]];
      let min = Infinity;
      let max = -Infinity;
      for (let k = i0; k <= i1; k++) {
        const v = values[k];
        const s = showSigma ? sigmas[k] : 0;
        if (v - s < min) min = v - s;
        if (v + s > max) max = v + s;
      }
      if (showCi) {
        const st = series.stats[facet.key];
        min = Math.min(min, st.weighted_mean - 2 * st.std_dev);
        max = Math.max(max, st.weighted_mean + 2 * st.std_dev);
      }
      const [lo, hi] = padRange(min, max);
      const top = layout.top(i);
      return scale(lo, hi, top + layout.facetHeight, top);
    });
  }, [series, i0, i1, showSigma, showCi, layout, facets]);

  /** Hovered epoch in this series, if the shared hover time falls inside it. */
  const hoverIndex = useMemo(() => {
    if (hoverT === null || series.t.length === 0) return null;
    if (hoverT < series.t[0] || hoverT > series.t[series.t.length - 1]) return null;
    return nearestIndex(series.t, hoverT);
  }, [hoverT, series]);
  const hoverPx = hoverIndex === null ? null : toPx(layout.x, series.t[hoverIndex]);
  const { setSelectedT } = useSelectedTime();
  const selIndex = useSelectedIndex(series.t);

  const draw = useCallback(() => {
    const ctx = prepareCanvas(canvasRef.current, size.width, size.height);
    if (!ctx || layout.facetHeight <= 8) return;
    const theme = readVizTheme(wrapRef.current);
    const { x } = layout;
    const xTicks = timeAxisTicks(span[0], span[1], Math.max(2, Math.floor(size.width / 110)), timeZone);

    const bottom = layout.top(facets.length - 1) + layout.facetHeight;

    facets.forEach((facet, i) => {
      const y = yScales[i];
      const color = theme.series[facet.slot];
      const yTickCount = Math.max(2, Math.min(5, Math.floor(layout.facetHeight / 26)));
      const yTicks = niceTicks(y.d0, y.d1, yTickCount);
      const precision = stepPrecision(yTicks.length > 1 ? yTicks[1] - yTicks[0] : (y.d1 - y.d0) / 4);

      drawGrid(ctx, theme, x, y, yTicks);
      drawYAxis(ctx, theme, y, yTicks, precision, x.p0);

      ctx.save();
      ctx.beginPath();
      ctx.rect(x.p0, y.p1, x.p1 - x.p0, y.p0 - y.p1);
      ctx.clip();

      const st = series.stats[facet.key];

      if (showCi) {
        // Neutral, not the series hue: this band is the spread of the data,
        // while the hue-tinted envelope below is the solution's own stated
        // precision. Two different claims, so two different fills.
        const upper = toPx(y, st.weighted_mean + 2 * st.std_dev);
        const lower = toPx(y, st.weighted_mean - 2 * st.std_dev);
        ctx.fillStyle = withAlpha(theme.text, 0.06);
        ctx.fillRect(x.p0, upper, x.p1 - x.p0, lower - upper);
      }

      if (showSigma) {
        drawSigmaEnvelope(ctx, x, y, series.t, series[facet.key], series[SIGMA_OF[facet.key]], i0, i1, color);
      }

      if (showCi) {
        ctx.strokeStyle = color;
        ctx.lineWidth = 1;
        ctx.beginPath();
        const py = Math.round(toPx(y, st.weighted_mean)) + 0.5;
        ctx.moveTo(x.p0, py);
        ctx.lineTo(x.p1, py);
        ctx.stroke();
      }

      drawSeriesLine(ctx, x, y, series.t, series[facet.key], i0, i1, color, theme);

      if (selIndex !== null) {
        drawSelectedRing(ctx, theme, toPx(x, series.t[selIndex]), toPx(y, series[facet.key][selIndex]));
      }
      if (hoverIndex !== null) {
        drawHoverMarker(ctx, x, y, series.t[hoverIndex], series[facet.key][hoverIndex], color, theme);
      }
      ctx.restore();

      drawFacetCaption(ctx, theme, color, facet, x.p0, y.p1, st.rms);

      // Per facet, so each swatch carries that panel's own series colour —
      // one shared legend would have to pick a single hue and misrepresent two
      // of the three panels.
      drawLegend(ctx, theme, legendFor(facet, color, theme, showSigma, showCi), x.p1 + 12, y.p1, y.p0);
    });

    drawXAxis(ctx, theme, x, xTicks, bottom, size.width);

    if (selIndex !== null) {
      const sx = toPx(x, series.t[selIndex]);
      if (sx >= x.p0 && sx <= x.p1) drawSelectedTime(ctx, theme, sx, PAD.top, bottom);
    }
    if (hoverPx !== null) drawCrosshair(ctx, theme, hoverPx, PAD.top, bottom);
    if (drag && Math.abs(drag.to - drag.from) >= MIN_DRAG_PX) {
      drawSelection(ctx, theme, drag, PAD.top, bottom);
    }
  }, [series, size, layout, yScales, showSigma, showCi, hoverIndex, hoverPx, selIndex, drag, span[0], span[1], i0, i1, facets, timeZone]);

  useCanvasRedraw(draw);

  const onPointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const { px, py } = canvasPoint(e);
    if (drag) {
      setDrag({ ...drag, to: clamp(px, layout.x.p0, layout.x.p1) });
      return;
    }
    if (px < layout.x.p0 || px > layout.x.p1) {
      setHoverT(null);
      return;
    }
    const index = nearestIndex(series.t, toData(layout.x, px));
    setPointerY(py);
    setHoverT(series.t[index]);
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
    setHoverT(null);
  };

  const onPointerUp = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (!drag) return;
    const { px } = canvasPoint(e);
    const to = clamp(px, layout.x.p0, layout.x.p1);
    setDrag(null);
    // A click, not a drag: select that epoch on the shared timeline.
    if (Math.abs(to - drag.from) < MIN_DRAG_PX) {
      setSelectedT(series.t[nearestIndex(series.t, toData(layout.x, to))]);
      return;
    }
    const a = toData(layout.x, Math.min(drag.from, to));
    const b = toData(layout.x, Math.max(drag.from, to));
    // Two epochs is the floor: any narrower and there is nothing to see.
    const lo = nearestIndex(series.t, a);
    const hi = nearestIndex(series.t, b);
    if (hi - lo < 1) return;
    setView([a, b]);
  };

  const zoomed = view !== null;
  const hoverInfo =
    hoverIndex !== null && hoverIsMine
      ? {
          time: formatEpoch(series.t[hoverIndex], timeZone),
          rows: facets.map((f) => ({
            label: f.label,
            color: `var(--viz-series-${f.slot + 1})`,
            value: formatMillimetres(series[f.key][hoverIndex]),
            sigma: formatMillimetres(series[SIGMA_OF[f.key]][hoverIndex]),
          })),
        }
      : null;

  return (
    <div className="viz-chart" ref={wrapRef}>
      <canvas
        ref={canvasRef}
        className="viz-canvas"
        role="img"
        aria-label={`Time series of ${facets.map((f) => f.label).join(", ")} for ${series.name}, ${series.total_epochs} epochs. The figures are in the statistics panel.`}
        style={{ width: "100%", height: "100%", cursor: drag ? "col-resize" : "crosshair" }}
        onPointerMove={onPointerMove}
        onPointerDown={onPointerDown}
        onPointerUp={onPointerUp}
        onPointerLeave={() => {
          if (hoverIsMine) setHoverT(null);
          setDrag(null);
        }}
        onDoubleClick={() => setView(null)}
      />

      {hoverInfo && !drag && (
        <div
          className="viz-tooltip"
          style={{
            left: Math.min((hoverPx ?? 0) + 14, Math.max(0, size.width - 190)),
            top: Math.min(pointerY + 12, Math.max(0, size.height - 108)),
          }}
        >
          <div className="viz-tooltip-time">{hoverInfo.time}</div>
          <table>
            <tbody>
              {hoverInfo.rows.map((r) => (
                <tr key={r.label}>
                  <td>
                    <span className="viz-key" style={{ background: r.color }} />
                    {r.label}
                  </td>
                  <td className="viz-num">{r.value}</td>
                  <td className="viz-num viz-dim">±{r.sigma}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {zoomed && (
        <button className="viz-reset" onClick={() => setView(null)} title="Show the whole solution">
          Reset zoom
        </button>
      )}
    </div>
  );
}

// ── drawing ──────────────────────────────────────────────────────────────────
