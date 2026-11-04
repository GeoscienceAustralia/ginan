import { useCallback, useMemo, useRef, useState } from "react";
import type { PosSeries } from "../../types";
import {
  PAD,
  legendWidth,
  drawCrosshair,
  drawEdgeSafeLabel,
  drawGrid,
  drawLegend,
  formatMetres,
  formatMillimetres,
  niceTicks,
  prepareCanvas,
  readVizTheme,
  scale,
  stepPrecision,
  toData,
  toPx,
  useElementSize,
  withAlpha,
  type LegendEntry,
  type VizTheme,
  useCanvasRedraw,
  canvasPoint,
} from "../chartUtils";
import { ERROR_METRICS, deriveErrors, percentileSorted, sortedCopy, type ErrorMetric } from "./metrics";

/**
 * Cumulative distribution of the error magnitudes — "what fraction of epochs
 * were within x". Reads the numbers accuracy is usually quoted in (CEP50,
 * 95%) straight off the curve, and compares horizontal, vertical and 3D on
 * one axis without a time dimension in the way.
 */

export function metricColor(m: ErrorMetric, theme: VizTheme): string {
  return m === "h" ? theme.series[0] : m === "v" ? theme.series[2] : theme.text;
}

export function metricCssColor(m: ErrorMetric): string {
  return m === "h" ? "var(--viz-series-1)" : m === "v" ? "var(--viz-series-3)" : "var(--text)";
}

const MARKS = [50, 95];

interface Props {
  series: PosSeries;
  metrics: ErrorMetric[];
  /** Cap the x axis at the 99th percentile so one outlier can't flatten the curve. */
  clipTail: boolean;
}

export default function CdfChart({ series, metrics, clipTail }: Props) {
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [hover, setHover] = useState<{ v: number; px: number; py: number } | null>(null);

  const curves = useMemo(() => {
    const d = deriveErrors(series);
    return metrics.map((m) => ({ key: m, sorted: sortedCopy(d[m]) }));
  }, [series, metrics]);

  const layout = useMemo(() => {
    let max = 0;
    for (const c of curves) {
      if (c.sorted.length === 0) continue;
      max = Math.max(max, clipTail ? percentileSorted(c.sorted, 99) : c.sorted[c.sorted.length - 1]);
    }
    const x = scale(0, max > 0 ? max * 1.04 : 1, PAD.left, Math.max(PAD.left + 1, size.width - PAD.right - legendWidth(size.width)));
    const y = scale(0, 100, Math.max(PAD.top + 1, size.height - PAD.bottom - 14), PAD.top);
    return { x, y };
  }, [curves, clipTail, size.width, size.height]);

  // The canvas only needs the crosshair column; the pointer height only moves
  // the tooltip.
  const hoverPx = hover?.px ?? null;

  const draw = useCallback(() => {
    const ctx = prepareCanvas(canvasRef.current, size.width, size.height);
    const { x, y } = layout;
    if (!ctx || x.p1 - x.p0 < 20 || y.p0 - y.p1 < 20) return;
    const theme = readVizTheme(wrapRef.current);

    const yTicks = [0, 25, 50, 75, 95, 100].filter((t) => (y.p0 - y.p1) > 120 || t % 50 === 0 || t === 95);
    drawGrid(ctx, theme, x, y, yTicks);

    ctx.save();
    ctx.fillStyle = theme.textDim;
    ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    for (const t of yTicks) ctx.fillText(`${t}%`, x.p0 - 8, toPx(y, t));
    const xTicks = niceTicks(x.d0, x.d1, Math.max(2, Math.floor((x.p1 - x.p0) / 80)));
    const precision = stepPrecision(xTicks.length > 1 ? xTicks[1] - xTicks[0] : (x.d1 - x.d0) / 4);
    ctx.textBaseline = "top";
    for (const t of xTicks) drawEdgeSafeLabel(ctx, formatMetres(t, precision), toPx(x, t), y.p0 + 6, size.width);
    ctx.textAlign = "center";
    ctx.fillText("Error magnitude (m)", (x.p0 + x.p1) / 2, y.p0 + 20);
    ctx.restore();

    ctx.save();
    ctx.beginPath();
    ctx.rect(x.p0, y.p1, x.p1 - x.p0, y.p0 - y.p1);
    ctx.clip();

    for (const c of curves) {
      const n = c.sorted.length;
      if (n === 0) continue;
      const color = metricColor(c.key, theme);
      ctx.strokeStyle = color;
      ctx.lineWidth = 2;
      ctx.beginPath();
      // A step function; thinned to about one vertex per pixel column.
      const stride = Math.max(1, Math.floor(n / Math.max(1, x.p1 - x.p0)));
      ctx.moveTo(toPx(x, 0), toPx(y, 0));
      for (let i = 0; i < n; i += stride) {
        ctx.lineTo(toPx(x, c.sorted[i]), toPx(y, (100 * (i + 1)) / n));
      }
      ctx.lineTo(toPx(x, c.sorted[n - 1]), toPx(y, 100));
      ctx.lineTo(x.p1, toPx(y, 100));
      ctx.stroke();

      // Percentile drop-lines, labelled at the foot so they read as a value.
      for (const p of MARKS) {
        const v = percentileSorted(c.sorted, p);
        const px = toPx(x, v);
        const py = toPx(y, p);
        ctx.strokeStyle = withAlpha(color, 0.55);
        ctx.lineWidth = 1;
        ctx.setLineDash([3, 3]);
        ctx.beginPath();
        ctx.moveTo(px, py);
        ctx.lineTo(px, y.p0);
        ctx.stroke();
        ctx.setLineDash([]);
        ctx.beginPath();
        ctx.arc(px, py, 3.5, 0, Math.PI * 2);
        ctx.fillStyle = color;
        ctx.fill();
      }
    }
    ctx.restore();

    if (hoverPx !== null) drawCrosshair(ctx, theme, hoverPx, y.p1, y.p0);

    const legend: LegendEntry[] = curves.map((c) => ({
      label: ERROR_METRICS.find((m) => m.key === c.key)!.label,
      unit: "m",
      color: metricColor(c.key, theme),
      swatch: "line",
    }));
    legend.push({ label: "50% / 95%", color: withAlpha(theme.text, 0.55), swatch: "dashed" });
    drawLegend(ctx, theme, legend, x.p1 + 12, y.p1);
  }, [size, layout, curves, hoverPx]);

  useCanvasRedraw(draw);

  const onPointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const { px, py } = canvasPoint(e);
    const { x } = layout;
    if (px < x.p0 || px > x.p1) {
      setHover(null);
      return;
    }
    setHover({ v: toData(x, px), px, py });
  };

  return (
    <div className="viz-chart" ref={wrapRef}>
      <canvas
        ref={canvasRef}
        className="viz-canvas"
        role="img"
        aria-label={`Cumulative distribution of ${metrics.join(", ")} error for ${series.name}. ${curves
          .map((c) => `${c.key} 95%: ${formatMillimetres(percentileSorted(c.sorted, 95))}`)
          .join("; ")}.`}
        style={{ width: "100%", height: "100%", cursor: "crosshair" }}
        onPointerMove={onPointerMove}
        onPointerLeave={() => setHover(null)}
      />
      {hover && (
        <div
          className="viz-tooltip"
          style={{
            left: Math.min(hover.px + 14, Math.max(0, size.width - 190)),
            top: Math.min(hover.py + 12, Math.max(0, size.height - 30 - curves.length * 20)),
          }}
        >
          <div className="viz-tooltip-time">Within {formatMillimetres(hover.v)}</div>
          <table>
            <tbody>
              {curves.map((c) => (
                <tr key={c.key}>
                  <td>
                    <span className="viz-key" style={{ background: metricCssColor(c.key) }} />
                    {ERROR_METRICS.find((m) => m.key === c.key)!.label}
                  </td>
                  <td className="viz-num">{fractionBelow(c.sorted, hover.v).toFixed(1)}%</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

/** Percentage of a sorted array at or below `v`. */
function fractionBelow(sorted: number[], v: number): number {
  let lo = 0;
  let hi = sorted.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (sorted[mid] <= v) lo = mid + 1;
    else hi = mid;
  }
  return sorted.length ? (100 * lo) / sorted.length : 0;
}
