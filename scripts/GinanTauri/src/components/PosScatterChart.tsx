import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import type { PosSeries } from "../types";
import {
  PAD,
  legendWidth,
  drawLegend,
  drawGrid,
  drawYAxis,
  formatEpoch,
  formatMetres,
  formatMillimetres,
  drawEdgeSafeLabel,
  drawSelectedRing,
  nearestIndex,
  niceTicks,
  padRange,
  prepareCanvas,
  readVizTheme,
  recencyColor,
  scale,
  stepPrecision,
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
import { useLinkedHover, useSelectedIndex, useSelectedTime } from "./viz/VizContext";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * Horizontal scatter — dE against dN, the figure `plot_pos.py` wrote as
 * `*_fig2.html`.
 *
 * The two axes hold the same quantity, so they are locked to one scale: the
 * cloud has to read as the shape it is on the ground, and independent axes
 * would stretch a north-south drift into a circle. Whichever component spreads
 * further sets the metres-per-pixel and the other range is grown around its
 * centre to match.
 */

/** Above this many points, dots go small and translucent so density reads. */
const DENSE_POINTS = 4000;
/** Rings are only drawn while individual dots are still countable. */
const RING_MAX_POINTS = 500;
const MIN_DRAG_PX = 8;
/** Cursor-to-point distance that still counts as a hover, in pixels. */
const HIT_RADIUS = 14;

interface Box {
  ex0: number;
  ex1: number;
  ny0: number;
  ny1: number;
}

interface Props {
  series: PosSeries;
  /** Draw the weighted-mean marker and its ±2σ scatter circles. */
  showCi: boolean;
}

export default function PosScatterChart({ series, showCi }: Props) {
  const timeZone = useTimeZone();
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [view, setView] = useState<Box | null>(null);
  const [hover, setHover] = useState<{ index: number; px: number; py: number } | null>(null);
  const [drag, setDrag] = useState<{ x0: number; y0: number; x1: number; y1: number } | null>(null);
  // With time axes linked, the epoch hovered in any other panel is ringed
  // here too, and hovering a point here moves the crosshair everywhere else.
  const linked = useLinkedHover();
  const { setSelectedT } = useSelectedTime();
  const selIndex = useSelectedIndex(series.t);

  useEffect(() => {
    setView(null);
    setHover(null);
    setDrag(null);
  }, [series.path]);

  /** Data bounds over the whole file, widened to take in the ±2σ circles when
   *  they are on, so the outer ring is never cropped by the frame. */
  const fullBox = useMemo<Box>(() => {
    const { de, dn } = series.stats;
    let exMin = de.min;
    let exMax = de.max;
    let nyMin = dn.min;
    let nyMax = dn.max;
    if (showCi) {
      exMin = Math.min(exMin, de.weighted_mean - 2 * de.std_dev);
      exMax = Math.max(exMax, de.weighted_mean + 2 * de.std_dev);
      nyMin = Math.min(nyMin, dn.weighted_mean - 2 * dn.std_dev);
      nyMax = Math.max(nyMax, dn.weighted_mean + 2 * dn.std_dev);
    }
    const [ex0, ex1] = padRange(exMin, exMax);
    const [ny0, ny1] = padRange(nyMin, nyMax);
    return { ex0, ex1, ny0, ny1 };
  }, [series, showCi]);

  const box = view ?? fullBox;

  /** Epoch to highlight: the one under the pointer, else the linked one. */
  // Keyed on the hovered index rather than the hover object, so moving the
  // pointer without changing the nearest point does not redraw the cloud.
  const hoverIndex = hover?.index ?? null;
  const markIndex = useMemo(() => {
    if (hoverIndex !== null) return hoverIndex;
    const t = linked.hoverT;
    if (t === null || linked.hoverIsMine || series.t.length === 0) return null;
    if (t < series.t[0] || t > series.t[series.t.length - 1]) return null;
    return nearestIndex(series.t, t);
  }, [hoverIndex, linked.hoverT, linked.hoverIsMine, series]);

  /** Scales locked to a common metres-per-pixel. */
  const layout = useMemo(() => {
    const px0 = PAD.left;
    // Stop short of the legend gutter so the cloud is never drawn under it.
    const px1 = Math.max(PAD.left + 1, size.width - PAD.right - legendWidth(size.width));
    const py0 = Math.max(PAD.top + 1, size.height - PAD.bottom);
    const py1 = PAD.top;
    const w = px1 - px0;
    const h = py0 - py1;

    const spanE = Math.max(box.ex1 - box.ex0, 1e-9);
    const spanN = Math.max(box.ny1 - box.ny0, 1e-9);
    const perPx = Math.max(spanE / Math.max(w, 1), spanN / Math.max(h, 1));

    const midE = (box.ex0 + box.ex1) / 2;
    const midN = (box.ny0 + box.ny1) / 2;
    const halfE = (perPx * w) / 2;
    const halfN = (perPx * h) / 2;

    return {
      x: scale(midE - halfE, midE + halfE, px0, px1),
      y: scale(midN - halfN, midN + halfN, py0, py1),
      perPx,
    };
  }, [size.width, size.height, box]);

  const draw = useCallback(() => {
    const ctx = prepareCanvas(canvasRef.current, size.width, size.height);
    if (!ctx) return;
    const theme = readVizTheme(wrapRef.current);
    const { x, y } = layout;
    if (x.p1 - x.p0 < 20 || y.p0 - y.p1 < 20) return;

    const yTicks = niceTicks(y.d0, y.d1, Math.max(2, Math.floor((y.p0 - y.p1) / 46)));
    const xTicks = niceTicks(x.d0, x.d1, Math.max(2, Math.floor((x.p1 - x.p0) / 96)));
    const step = yTicks.length > 1 ? yTicks[1] - yTicks[0] : (y.d1 - y.d0) / 4;
    const precision = stepPrecision(step);

    drawGrid(ctx, theme, x, y, yTicks);
    drawVerticalGrid(ctx, theme, x, y, xTicks);
    drawYAxis(ctx, theme, y, yTicks, precision, x.p0);
    drawXTickLabels(ctx, theme, x, xTicks, precision, y.p0, size.width);
    drawAxisTitles(ctx, theme, x, y, size.height);

    ctx.save();
    ctx.beginPath();
    ctx.rect(x.p0, y.p1, x.p1 - x.p0, y.p0 - y.p1);
    ctx.clip();

    const n = series.de.length;
    const dense = n > DENSE_POINTS;
    const radius = dense ? 2 : 3.2;
    const alpha = dense ? 0.4 : 0.72;

    // Colored oldest-to-newest, matching the map view's recency gradient — a
    // run's drift over time reads directly off the cloud's own color.
    for (let k = 0; k < n; k++) {
      const t = n > 1 ? k / (n - 1) : 1;
      const px = toPx(x, series.de[k]);
      const py = toPx(y, series.dn[k]);
      ctx.fillStyle = recencyColor(t, alpha);
      ctx.beginPath();
      ctx.arc(px, py, radius, 0, Math.PI * 2);
      ctx.fill();
    }
    if (n <= RING_MAX_POINTS) {
      ctx.strokeStyle = theme.surface;
      ctx.lineWidth = 1.5;
      for (let k = 0; k < n; k++) {
        ctx.beginPath();
        ctx.arc(toPx(x, series.de[k]), toPx(y, series.dn[k]), radius, 0, Math.PI * 2);
        ctx.stroke();
      }
    }

    if (showCi) drawMeanAndSpread(ctx, theme, x, y, series);

    if (selIndex !== null) {
      drawSelectedRing(ctx, theme, toPx(x, series.de[selIndex]), toPx(y, series.dn[selIndex]));
    }
    if (markIndex !== null) {
      const t = n > 1 ? markIndex / (n - 1) : 1;
      const px = toPx(x, series.de[markIndex]);
      const py = toPx(y, series.dn[markIndex]);
      ctx.beginPath();
      ctx.arc(px, py, 5, 0, Math.PI * 2);
      ctx.fillStyle = recencyColor(t);
      ctx.fill();
      ctx.lineWidth = 2;
      ctx.strokeStyle = theme.surface;
      ctx.stroke();
    }
    ctx.restore();

    // Outside the clip, in the reserved gutter — the cloud never reaches here.
    const legend: LegendEntry[] = [
      { label: "Epoch (older)", unit: "m", color: recencyColor(0), swatch: "marker" },
      { label: "Epoch (newer)", unit: "m", color: recencyColor(1), swatch: "marker" },
    ];
    if (showCi) {
      legend.push({ label: "Weighted mean", unit: "m", color: theme.text, swatch: "marker" });
      legend.push({ label: "±1σ, ±2σ spread", unit: "m", color: withAlpha(theme.text, 0.32), swatch: "line" });
    }
    drawLegend(ctx, theme, legend, layout.x.p1 + 12, layout.y.p1);

    if (drag && dragIsZoom(drag)) drawSelection(ctx, theme, drag);
  }, [series, size, layout, showCi, markIndex, selIndex, drag]);

  useCanvasRedraw(draw);

  /** Index of the point within HIT_RADIUS of (px, py), or -1. Linear scan: the
   *  arrays are decimated to a display budget, so this is a few thousand
   *  distance checks — cheaper than maintaining an index. */
  const nearestPoint = (px: number, py: number): number => {
    const { x, y } = layout;
    let best = -1;
    let bestDist = HIT_RADIUS * HIT_RADIUS;
    for (let k = 0; k < series.de.length; k++) {
      const dx = toPx(x, series.de[k]) - px;
      const dy = toPx(y, series.dn[k]) - py;
      const d = dx * dx + dy * dy;
      if (d < bestDist) {
        bestDist = d;
        best = k;
      }
    }
    return best;
  };

  const onPointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const { px, py } = canvasPoint(e);
    if (drag) {
      setDrag({ ...drag, x1: px, y1: py });
      return;
    }
    const { x, y } = layout;
    if (px < x.p0 || px > x.p1 || py > y.p0 || py < y.p1) {
      setHover(null);
      if (linked.hoverIsMine) linked.setHoverT(null);
      return;
    }
    const best = nearestPoint(px, py);
    setHover(best < 0 ? null : { index: best, px, py });
    linked.setHoverT(best < 0 ? null : series.t[best]);
  };

  const onPointerDown = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (e.button !== 0) return;
    const { px, py } = canvasPoint(e);
    // See PosTimeSeriesChart: capture is a convenience, never the mechanism.
    try {
      e.currentTarget.setPointerCapture(e.pointerId);
    } catch {
      /* not capturable — the box select still works inside the canvas */
    }
    setDrag({ x0: px, y0: py, x1: px, y1: py });
    setHover(null);
  };

  const onPointerUp = () => {
    if (!drag) return;
    setDrag(null);
    if (!dragIsZoom(drag)) {
      // A click on a point selects its epoch on the shared timeline.
      if (hover) setSelectedT(series.t[hover.index]);
      else {
        const best = nearestPoint(drag.x0, drag.y0);
        if (best >= 0) setSelectedT(series.t[best]);
      }
      return;
    }
    const { x, y } = layout;
    const e0 = toData(x, Math.min(drag.x0, drag.x1));
    const e1 = toData(x, Math.max(drag.x0, drag.x1));
    // y is inverted in pixel space, so the lower pixel is the higher metre.
    const n0 = toData(y, Math.max(drag.y0, drag.y1));
    const n1 = toData(y, Math.min(drag.y0, drag.y1));
    setView({ ex0: e0, ex1: e1, ny0: n0, ny1: n1 });
  };

  const hovered = hover
    ? {
        time: formatEpoch(series.t[hover.index], timeZone),
        de: formatMillimetres(series.de[hover.index]),
        dn: formatMillimetres(series.dn[hover.index]),
        se: formatMillimetres(series.se[hover.index]),
        sn: formatMillimetres(series.sn[hover.index]),
      }
    : null;

  return (
    <div className="viz-chart" ref={wrapRef}>
      <canvas
        ref={canvasRef}
        className="viz-canvas"
        role="img"
        aria-label={`Horizontal scatter of dE against dN for ${series.name}, ${series.total_epochs} epochs. The figures are in the statistics panel.`}
        style={{ width: "100%", height: "100%", cursor: "crosshair" }}
        onPointerMove={onPointerMove}
        onPointerDown={onPointerDown}
        onPointerUp={onPointerUp}
        onPointerLeave={() => {
          setHover(null);
          if (linked.hoverIsMine) linked.setHoverT(null);
          setDrag(null);
        }}
        onDoubleClick={() => setView(null)}
      />

      {hovered && !drag && (
        <div
          className="viz-tooltip"
          style={{
            left: Math.min(hover!.px + 14, Math.max(0, size.width - 190)),
            top: Math.min(hover!.py + 12, Math.max(0, size.height - 92)),
          }}
        >
          <div className="viz-tooltip-time">{hovered.time}</div>
          <table>
            <tbody>
              <tr>
                <td>
                  <span className="viz-key" style={{ background: "var(--viz-series-1)" }} />
                  dE
                </td>
                <td className="viz-num">{hovered.de}</td>
                <td className="viz-num viz-dim">±{hovered.se}</td>
              </tr>
              <tr>
                <td>
                  <span className="viz-key" style={{ background: "var(--viz-series-1)" }} />
                  dN
                </td>
                <td className="viz-num">{hovered.dn}</td>
                <td className="viz-num viz-dim">±{hovered.sn}</td>
              </tr>
            </tbody>
          </table>
        </div>
      )}

      {view && (
        <button className="viz-reset" onClick={() => setView(null)} title="Show the whole cloud">
          Reset zoom
        </button>
      )}
      {!view && <span className="viz-hint">Click a point to select · drag a box to zoom</span>}

      {/* Floats over the plot, so a small tile drops it rather than hide the
          very points it describes. */}
      {size.width >= 360 && size.height >= 240 && (
      <div className="viz-map-legend">
        <span>Oldest</span>
        <span className="viz-map-legend-bar" />
        <span>Newest</span>
      </div>
      )}
    </div>
  );
}

// ── drawing ──────────────────────────────────────────────────────────────────

function drawVerticalGrid(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  x: Scale,
  y: Scale,
  ticks: number[],
) {
  ctx.save();
  ctx.strokeStyle = theme.grid;
  ctx.lineWidth = 1;
  ctx.beginPath();
  for (const t of ticks) {
    const px = Math.round(toPx(x, t)) + 0.5;
    ctx.moveTo(px, y.p1);
    ctx.lineTo(px, y.p0);
  }
  ctx.stroke();
  ctx.restore();
}

function drawXTickLabels(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  x: Scale,
  ticks: number[],
  precision: number,
  yBottom: number,
  canvasWidth: number,
) {
  ctx.save();
  ctx.fillStyle = theme.textDim;
  ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
  ctx.textBaseline = "top";
  for (const t of ticks) {
    const px = toPx(x, t);
    if (px < x.p0 - 1 || px > x.p1 + 1) continue;
    drawEdgeSafeLabel(ctx, formatMetres(t, precision), px, yBottom + 6, canvasWidth);
  }
  ctx.restore();
}

function drawAxisTitles(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  x: Scale,
  y: Scale,
  height: number,
) {
  ctx.save();
  ctx.fillStyle = theme.textMuted;
  ctx.font = "600 11px ui-sans-serif, system-ui, sans-serif";
  ctx.textAlign = "right";
  ctx.textBaseline = "bottom";
  ctx.fillText("dE · East (m)", x.p1, height - 2);
  ctx.textAlign = "left";
  ctx.textBaseline = "top";
  ctx.fillText("dN · North (m)", x.p0 + 8, y.p1 + 4);
  ctx.restore();
}

/**
 * The weighted mean of the cloud, plus 1σ and 2σ circles. Circles rather than
 * an error ellipse: the per-component standard deviations are what the parser
 * reports, and drawing a tilted ellipse would imply a correlation this figure
 * has not computed.
 */
function drawMeanAndSpread(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  x: Scale,
  y: Scale,
  series: PosSeries,
) {
  const mE = series.stats.de.weighted_mean;
  const mN = series.stats.dn.weighted_mean;
  const cx = toPx(x, mE);
  const cy = toPx(y, mN);
  // One scale for both axes, so a metre is the same number of pixels either way.
  const pxPerMetre = (x.p1 - x.p0) / (x.d1 - x.d0);
  const spread = Math.hypot(series.stats.de.std_dev, series.stats.dn.std_dev) / Math.SQRT2;

  ctx.save();
  ctx.strokeStyle = withAlpha(theme.text, 0.32);
  ctx.lineWidth = 1;
  for (const k of [1, 2]) {
    ctx.beginPath();
    ctx.arc(cx, cy, spread * k * pxPerMetre, 0, Math.PI * 2);
    ctx.stroke();
  }

  // Cross marker: ink core over a surface-coloured backing so it stays visible
  // wherever in the cloud the mean falls.
  const r = 6;
  ctx.lineWidth = 4;
  ctx.strokeStyle = theme.surface;
  crossPath(ctx, cx, cy, r);
  ctx.lineWidth = 2;
  ctx.strokeStyle = theme.text;
  crossPath(ctx, cx, cy, r);

  // The label lands inside the cloud, so it gets a surface plate to sit on —
  // otherwise dots read through the glyphs.
  ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
  const label = "weighted mean";
  const tx = cx + r + 6;
  const tw = ctx.measureText(label).width;
  ctx.fillStyle = withAlpha(theme.surface, 0.85);
  ctx.fillRect(tx - 3, cy - 7, tw + 6, 14);
  ctx.fillStyle = theme.textMuted;
  ctx.textAlign = "left";
  ctx.textBaseline = "middle";
  ctx.fillText(label, tx, cy);
  ctx.restore();
}

function crossPath(ctx: CanvasRenderingContext2D, cx: number, cy: number, r: number) {
  ctx.beginPath();
  ctx.moveTo(cx - r, cy - r);
  ctx.lineTo(cx + r, cy + r);
  ctx.moveTo(cx + r, cy - r);
  ctx.lineTo(cx - r, cy + r);
  ctx.stroke();
}

function dragIsZoom(d: { x0: number; y0: number; x1: number; y1: number }): boolean {
  return Math.abs(d.x1 - d.x0) >= MIN_DRAG_PX && Math.abs(d.y1 - d.y0) >= MIN_DRAG_PX;
}

function drawSelection(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  d: { x0: number; y0: number; x1: number; y1: number },
) {
  const x = Math.min(d.x0, d.x1);
  const y = Math.min(d.y0, d.y1);
  const w = Math.abs(d.x1 - d.x0);
  const h = Math.abs(d.y1 - d.y0);
  ctx.save();
  ctx.fillStyle = withAlpha(theme.text, 0.1);
  ctx.fillRect(x, y, w, h);
  ctx.strokeStyle = withAlpha(theme.text, 0.35);
  ctx.lineWidth = 1;
  ctx.strokeRect(Math.round(x) + 0.5, Math.round(y) + 0.5, Math.round(w), Math.round(h));
  ctx.restore();
}
