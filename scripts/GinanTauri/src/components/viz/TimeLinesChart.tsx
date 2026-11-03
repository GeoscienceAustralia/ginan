import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import type { PosSeries } from "../../types";
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
  formatEpoch,
  formatMetres,
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
  type LegendEntry,
  type Scale,
  type VizTheme,
  useCanvasRedraw,
  canvasPoint,
} from "../chartUtils";
import { useSelectedIndex, useSelectedTime, useTimeLink } from "./VizContext";
import { useTimeZone } from "../../hooks/useTimeZone";

/**
 * Several quantities against time on one pair of axes — the shape shared by
 * the error-magnitude and formal-error panels. Same interaction model as the
 * dN/dE/dU time series (drag to zoom, double-click to reset, hover readout)
 * and the same linked time axis, so a spike in one panel lines up with its
 * cause in another.
 *
 * Unlike the residual time series these quantities are all non-negative and
 * of like kind, so they share one y axis; `logY` spreads a converging filter's
 * metres-to-millimetres range over the height instead of flattening the tail.
 */

export interface TimeLine {
  key: string;
  label: string;
  values: number[];
  /** Resolved colour (not a CSS var) — the canvas needs a concrete value. */
  color: (theme: VizTheme) => string;
  /** CSS colour for the HTML tooltip key. */
  cssColor: string;
}

export interface RefLine {
  label: string;
  value: number;
  color: (theme: VizTheme) => string;
}

interface Props {
  series: PosSeries;
  lines: TimeLine[];
  refLines?: RefLine[];
  logY?: boolean;
  ariaLabel: string;
}

const MIN_DRAG_PX = 6;
/** Floor for the log axis — below a micrometre is noise, not signal. */
const LOG_FLOOR = 1e-6;

export default function TimeLinesChart({ series, lines, refLines = [], logY = false, ariaLabel }: Props) {
  const timeZone = useTimeZone();
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const { view, setView, hoverT, hoverIsMine, setHoverT } = useTimeLink(series.path);
  const [pointerY, setPointerY] = useState(0);
  const [drag, setDrag] = useState<{ from: number; to: number } | null>(null);

  useEffect(() => setDrag(null), [series.path]);

  const tx = useCallback((v: number) => (logY ? Math.log10(Math.max(v, LOG_FLOOR)) : v), [logY]);

  const full = useMemo<[number, number]>(() => [series.t[0], series.t[series.t.length - 1]], [series]);
  const span: [number, number] = view ?? full;

  const [i0, i1] = useMemo(() => {
    const lo = Math.max(0, nearestIndex(series.t, span[0]) - 1);
    const hi = Math.min(series.t.length - 1, nearestIndex(series.t, span[1]) + 1);
    return [lo, hi];
  }, [series, span[0], span[1]]);

  const x = useMemo(
    () => scale(span[0], span[1], PAD.left, Math.max(PAD.left + 1, size.width - PAD.right - legendWidth(size.width))),
    [span[0], span[1], size.width],
  );

  const y = useMemo(() => {
    let min = Infinity;
    let max = -Infinity;
    for (const line of lines) {
      for (let k = i0; k <= i1; k++) {
        const v = tx(line.values[k]);
        if (v < min) min = v;
        if (v > max) max = v;
      }
    }
    for (const r of refLines) {
      const v = tx(r.value);
      if (Number.isFinite(v)) {
        min = Math.min(min, v);
        max = Math.max(max, v);
      }
    }
    // Magnitudes start at zero; anchoring the linear axis there keeps "how
    // big is the error" honest instead of zooming into the noise band.
    if (!logY) min = Math.min(0, min);
    const [lo, hi] = padRange(min, max, 0.06);
    const bottom = Math.max(PAD.top + 1, size.height - PAD.bottom);
    return scale(logY ? lo : Math.max(0, lo), hi, bottom, PAD.top);
  }, [lines, refLines, i0, i1, tx, logY, size.height]);

  const hoverIndex = useMemo(() => {
    if (hoverT === null || series.t.length === 0) return null;
    if (hoverT < series.t[0] || hoverT > series.t[series.t.length - 1]) return null;
    return nearestIndex(series.t, hoverT);
  }, [hoverT, series]);
  const hoverPx = hoverIndex === null ? null : toPx(x, series.t[hoverIndex]);
  const { setSelectedT } = useSelectedTime();
  const selIndex = useSelectedIndex(series.t);

  const draw = useCallback(() => {
    const ctx = prepareCanvas(canvasRef.current, size.width, size.height);
    if (!ctx || y.p0 - y.p1 < 20 || x.p1 - x.p0 < 20) return;
    const theme = readVizTheme(wrapRef.current);

    const yTicks = logY ? logTicks(y.d0, y.d1) : niceTicks(y.d0, y.d1, Math.max(2, Math.floor((y.p0 - y.p1) / 36)));
    drawGrid(ctx, theme, x, y, yTicks);
    drawYLabels(ctx, theme, y, yTicks, logY, x.p0);
    drawXAxis(ctx, theme, x, timeAxisTicks(span[0], span[1], Math.max(2, Math.floor(size.width / 110)), timeZone), y.p0, size.width);

    ctx.save();
    ctx.beginPath();
    ctx.rect(x.p0, y.p1, x.p1 - x.p0, y.p0 - y.p1);
    ctx.clip();

    for (const r of refLines) {
      if (!Number.isFinite(r.value)) continue;
      const py = Math.round(toPx(y, tx(r.value))) + 0.5;
      ctx.strokeStyle = r.color(theme);
      ctx.lineWidth = 1;
      ctx.setLineDash([5, 4]);
      ctx.beginPath();
      ctx.moveTo(x.p0, py);
      ctx.lineTo(x.p1, py);
      ctx.stroke();
      ctx.setLineDash([]);
    }

    for (const line of lines) {
      ctx.strokeStyle = line.color(theme);
      ctx.lineWidth = 1.75;
      ctx.lineJoin = "round";
      ctx.beginPath();
      for (let k = i0; k <= i1; k++) {
        const px = toPx(x, series.t[k]);
        const py = toPx(y, tx(line.values[k]));
        if (k === i0) ctx.moveTo(px, py);
        else ctx.lineTo(px, py);
      }
      ctx.stroke();
    }

    if (selIndex !== null) {
      for (const line of lines) {
        drawSelectedRing(ctx, theme, toPx(x, series.t[selIndex]), toPx(y, tx(line.values[selIndex])));
      }
    }
    if (hoverIndex !== null) {
      for (const line of lines) {
        ctx.beginPath();
        ctx.arc(toPx(x, series.t[hoverIndex]), toPx(y, tx(line.values[hoverIndex])), 4, 0, Math.PI * 2);
        ctx.fillStyle = line.color(theme);
        ctx.fill();
        ctx.lineWidth = 2;
        ctx.strokeStyle = theme.surface;
        ctx.stroke();
      }
    }
    ctx.restore();

    const legend: LegendEntry[] = lines.map((l) => ({ label: l.label, unit: "m", color: l.color(theme), swatch: "line" }));
    for (const r of refLines) legend.push({ label: r.label, color: r.color(theme), swatch: "dashed" });
    drawLegend(ctx, theme, legend, x.p1 + 12, y.p1);

    if (selIndex !== null) {
      const sx = toPx(x, series.t[selIndex]);
      if (sx >= x.p0 && sx <= x.p1) drawSelectedTime(ctx, theme, sx, y.p1, y.p0);
    }
    if (hoverPx !== null) drawCrosshair(ctx, theme, hoverPx, y.p1, y.p0);
    if (drag && Math.abs(drag.to - drag.from) >= MIN_DRAG_PX) drawSelection(ctx, theme, drag, y.p1, y.p0);
  }, [size, x, y, lines, refLines, i0, i1, series, tx, logY, hoverIndex, hoverPx, selIndex, drag, span[0], span[1], timeZone]);

  useCanvasRedraw(draw);

  const onPointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const { px, py } = canvasPoint(e);
    if (drag) {
      setDrag({ ...drag, to: clamp(px, x.p0, x.p1) });
      return;
    }
    if (px < x.p0 || px > x.p1) {
      setHoverT(null);
      return;
    }
    setPointerY(py);
    setHoverT(series.t[nearestIndex(series.t, toData(x, px))]);
  };

  const onPointerDown = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (e.button !== 0) return;
    const { px } = canvasPoint(e);
    if (px < x.p0 || px > x.p1) return;
    try {
      e.currentTarget.setPointerCapture(e.pointerId);
    } catch {
      /* the drag still works while the cursor stays inside */
    }
    setDrag({ from: px, to: px });
    setHoverT(null);
  };

  const onPointerUp = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (!drag) return;
    const to = clamp(canvasPoint(e).px, x.p0, x.p1);
    setDrag(null);
    if (Math.abs(to - drag.from) < MIN_DRAG_PX) {
      setSelectedT(series.t[nearestIndex(series.t, toData(x, to))]);
      return;
    }
    const a = toData(x, Math.min(drag.from, to));
    const b = toData(x, Math.max(drag.from, to));
    if (nearestIndex(series.t, b) - nearestIndex(series.t, a) < 1) return;
    setView([a, b]);
  };

  const showTip = hoverIndex !== null && hoverIsMine && !drag;

  return (
    <div className="viz-chart" ref={wrapRef}>
      <canvas
        ref={canvasRef}
        className="viz-canvas"
        role="img"
        aria-label={ariaLabel}
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
      {showTip && (
        <div
          className="viz-tooltip"
          style={{
            left: Math.min((hoverPx ?? 0) + 14, Math.max(0, size.width - 190)),
            top: Math.min(pointerY + 12, Math.max(0, size.height - 30 - lines.length * 20)),
          }}
        >
          <div className="viz-tooltip-time">{formatEpoch(series.t[hoverIndex!], timeZone)}</div>
          <table>
            <tbody>
              {lines.map((l) => (
                <tr key={l.key}>
                  <td>
                    <span className="viz-key" style={{ background: l.cssColor }} />
                    {l.label}
                  </td>
                  <td className="viz-num">{formatMillimetres(l.values[hoverIndex!])}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
      {view && (
        <button className="viz-reset" onClick={() => setView(null)} title="Show the whole solution">
          Reset zoom
        </button>
      )}
    </div>
  );
}

/** Decade ticks (…, 0.01, 0.1, 1) in log10 space, plus 2 and 5 when the range is narrow. */
function logTicks(d0: number, d1: number): number[] {
  const out: number[] = [];
  const decades = d1 - d0;
  const mult = decades < 2.5 ? [1, 2, 5] : [1];
  for (let e = Math.floor(d0); e <= Math.ceil(d1); e++) {
    for (const m of mult) {
      const v = e + Math.log10(m);
      if (v >= d0 - 1e-9 && v <= d1 + 1e-9) out.push(v);
    }
  }
  return out;
}

function drawYLabels(ctx: CanvasRenderingContext2D, theme: VizTheme, y: Scale, ticks: number[], logY: boolean, x0: number) {
  ctx.save();
  ctx.fillStyle = theme.textDim;
  ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
  ctx.textAlign = "right";
  ctx.textBaseline = "middle";
  const step = ticks.length > 1 ? ticks[1] - ticks[0] : (y.d1 - y.d0) / 4;
  const precision = stepPrecision(step);
  for (const t of ticks) {
    const label = logY ? formatLogValue(Math.pow(10, t)) : formatMetres(t, precision);
    ctx.fillText(label, x0 - 8, toPx(y, t));
  }
  ctx.restore();
}

function formatLogValue(v: number): string {
  if (v >= 1) return v.toFixed(0);
  const digits = Math.min(6, Math.max(1, Math.ceil(-Math.log10(v))));
  return v.toFixed(digits);
}

/** Canvas-safe colour for a reference line in the neutral ink. */
export function neutralInk(alpha = 0.55) {
  return (theme: VizTheme) => withAlpha(theme.text, alpha);
}
