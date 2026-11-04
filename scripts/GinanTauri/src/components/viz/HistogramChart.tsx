import { useCallback, useMemo, useRef, useState } from "react";
import type { PosSeries } from "../../types";
import {
  PAD,
  legendWidth,
  LEGEND_TOP_GAP,
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
  toPx,
  useElementSize,
  withAlpha,
  type LegendEntry,
  type VizTheme,
  useCanvasRedraw,
  canvasPoint,
} from "../chartUtils";
import { deriveErrors, histogram, meanStd } from "./metrics";

/**
 * Distribution of one quantity over the run. A residual that is centred and
 * bell-shaped is noise; a skew, a second hump or a fat tail is a systematic —
 * multipath, a bad ambiguity fix, a mismodelled troposphere — and none of
 * those show up in a mean and a standard deviation alone.
 */

export type HistQuantity = "dn" | "de" | "du" | "h" | "v" | "3d";

export const HIST_QUANTITIES: { key: HistQuantity; label: string; signed: boolean }[] = [
  { key: "dn", label: "dN · North", signed: true },
  { key: "de", label: "dE · East", signed: true },
  { key: "du", label: "dU · Up", signed: true },
  { key: "h", label: "Horizontal error", signed: false },
  { key: "v", label: "Vertical error", signed: false },
  { key: "3d", label: "3D error", signed: false },
];

function quantityColor(q: HistQuantity, theme: VizTheme): string {
  switch (q) {
    case "dn":
    case "h":
      return theme.series[0];
    case "de":
      return theme.series[1];
    case "du":
    case "v":
      return theme.series[2];
    default:
      return theme.text;
  }
}

function quantityValues(series: PosSeries, q: HistQuantity): number[] {
  if (q === "dn" || q === "de" || q === "du") return series[q];
  return deriveErrors(series)[q];
}

interface Props {
  series: PosSeries;
  quantity: HistQuantity;
  showNormal: boolean;
  /** Bin the central 1–99% only, so the convergence tail can't swamp the shape. */
  clipTail: boolean;
}

/** Percentile trimmed from each end when `clipTail` is on. */
const CLIP_PERCENT = 1;
/**
 * Fewest samples the trim applies to. On a short run every sample is more than
 * 1% of the whole, so trimming "the outer 1%" would throw most of them away and
 * leave a single bar.
 */
const CLIP_MIN_SAMPLES = 200;

export default function HistogramChart({ series, quantity, showNormal, clipTail }: Props) {
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [hover, setHover] = useState<{ bin: number; px: number; py: number } | null>(null);

  const meta = HIST_QUANTITIES.find((q) => q.key === quantity) ?? HIST_QUANTITIES[0];
  const values = useMemo(() => quantityValues(series, quantity), [series, quantity]);
  const clip = clipTail && values.length >= CLIP_MIN_SAMPLES;
  const hist = useMemo(() => histogram(values, clip ? CLIP_PERCENT : 0), [values, clip]);
  // Moments over the binned samples, so the fitted curve describes the bars
  // it is drawn over rather than a width inflated by the excluded tail.
  const binned = useMemo(() => {
    if (!clip || hist.edges.length === 0) return values;
    const lo = hist.edges[0];
    const hi = hist.edges[hist.edges.length - 1];
    return values.filter((v) => v >= lo && v <= hi);
  }, [values, hist, clip]);
  const moments = useMemo(() => meanStd(binned), [binned]);

  const layout = useMemo(() => {
    const lo = hist.edges[0] ?? 0;
    const hi = hist.edges[hist.edges.length - 1] ?? 1;
    const x = scale(lo, hi, PAD.left, Math.max(PAD.left + 1, size.width - PAD.right - legendWidth(size.width)));
    const y = scale(0, Math.max(1, hist.max) * 1.08, Math.max(PAD.top + 1, size.height - PAD.bottom - 14), PAD.top);
    return { x, y };
  }, [hist, size.width, size.height]);

  const draw = useCallback(() => {
    const ctx = prepareCanvas(canvasRef.current, size.width, size.height);
    const { x, y } = layout;
    if (!ctx || hist.counts.length === 0 || x.p1 - x.p0 < 20 || y.p0 - y.p1 < 20) return;
    const theme = readVizTheme(wrapRef.current);
    const color = quantityColor(quantity, theme);

    const yTicks = niceTicks(0, y.d1, Math.max(2, Math.floor((y.p0 - y.p1) / 36))).filter((t) => Number.isInteger(t));
    drawGrid(ctx, theme, x, y, yTicks);

    ctx.save();
    ctx.fillStyle = theme.textDim;
    ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    for (const t of yTicks) ctx.fillText(String(t), x.p0 - 8, toPx(y, t));

    const xTicks = niceTicks(x.d0, x.d1, Math.max(2, Math.floor((x.p1 - x.p0) / 80)));
    const precision = stepPrecision(xTicks.length > 1 ? xTicks[1] - xTicks[0] : (x.d1 - x.d0) / 4);
    ctx.textBaseline = "top";
    for (const t of xTicks) drawEdgeSafeLabel(ctx, formatMetres(t, precision), toPx(x, t), y.p0 + 6, size.width);
    ctx.textAlign = "center";
    ctx.fillText(`${meta.label} (m)`, (x.p0 + x.p1) / 2, y.p0 + 20);
    ctx.restore();

    // Bars, with a one-pixel gap so neighbouring bins stay countable.
    for (let i = 0; i < hist.counts.length; i++) {
      const a = toPx(x, hist.edges[i]);
      const b = toPx(x, hist.edges[i + 1]);
      const top = toPx(y, hist.counts[i]);
      ctx.fillStyle = withAlpha(color, hover?.bin === i ? 0.95 : 0.7);
      ctx.fillRect(a + 0.5, top, Math.max(1, b - a - 1), y.p0 - top);
    }

    // The fitted normal, scaled to counts: area under the curve equals the
    // sample count, so it overlays the bars directly.
    const { mean, std } = moments;
    const width = hist.edges[1] - hist.edges[0];
    if (showNormal && meta.signed && std > 0) {
      ctx.save();
      ctx.strokeStyle = theme.text;
      ctx.lineWidth = 1.5;
      ctx.beginPath();
      const steps = 120;
      for (let k = 0; k <= steps; k++) {
        const v = x.d0 + ((x.d1 - x.d0) * k) / steps;
        const pdf = Math.exp(-0.5 * ((v - mean) / std) ** 2) / (std * Math.sqrt(2 * Math.PI));
        const px = toPx(x, v);
        const py = toPx(y, pdf * binned.length * width);
        if (k === 0) ctx.moveTo(px, py);
        else ctx.lineTo(px, py);
      }
      ctx.stroke();
      ctx.restore();
    }

    // Mean marker.
    if (Number.isFinite(mean)) {
      const px = Math.round(toPx(x, mean)) + 0.5;
      ctx.save();
      ctx.strokeStyle = withAlpha(theme.text, 0.7);
      ctx.setLineDash([4, 3]);
      ctx.beginPath();
      ctx.moveTo(px, y.p1);
      ctx.lineTo(px, y.p0);
      ctx.stroke();
      ctx.restore();
    }

    const legend: LegendEntry[] = [
      { label: "Epochs per bin", color, swatch: "band", alpha: 0.7 },
      { label: "Mean", unit: "m", color: withAlpha(theme.text, 0.7), swatch: "dashed" },
    ];
    if (showNormal && meta.signed) legend.push({ label: "Normal fit", color: theme.text, swatch: "line" });
    drawLegend(ctx, theme, legend, x.p1 + 12, y.p1);

    ctx.save();
    ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
    ctx.fillStyle = theme.textDim;
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    const lines = [`n = ${binned.length.toLocaleString()}`, `mean ${formatMillimetres(mean)}`, `σ ${formatMillimetres(std)}`];
    if (hist.outside > 0) lines.push(`${hist.outside.toLocaleString()} outside ${CLIP_PERCENT}–${100 - CLIP_PERCENT}%`);
    lines.forEach((l, i) => ctx.fillText(l, x.p1 + 12, y.p1 + LEGEND_TOP_GAP + 16 * legend.length + 20 + i * 15));
    ctx.restore();
  }, [size, layout, hist, moments, quantity, meta, showNormal, binned.length, hover?.bin]);

  useCanvasRedraw(draw);

  const onPointerMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const { px, py } = canvasPoint(e);
    const { x } = layout;
    if (px < x.p0 || px > x.p1 || hist.counts.length === 0) {
      setHover(null);
      return;
    }
    const w = (x.p1 - x.p0) / hist.counts.length;
    const bin = Math.min(hist.counts.length - 1, Math.max(0, Math.floor((px - x.p0) / w)));
    setHover({ bin, px, py });
  };

  return (
    <div className="viz-chart" ref={wrapRef}>
      <canvas
        ref={canvasRef}
        className="viz-canvas"
        role="img"
        aria-label={`Histogram of ${meta.label} for ${series.name}: mean ${formatMillimetres(moments.mean)}, standard deviation ${formatMillimetres(moments.std)}.`}
        style={{ width: "100%", height: "100%", cursor: "default" }}
        onPointerMove={onPointerMove}
        onPointerLeave={() => setHover(null)}
      />
      {hover && (
        <div
          className="viz-tooltip"
          style={{
            left: Math.min(hover.px + 14, Math.max(0, size.width - 190)),
            top: Math.min(hover.py + 12, Math.max(0, size.height - 70)),
          }}
        >
          <div className="viz-tooltip-time">
            {formatMillimetres(hist.edges[hover.bin])} ... {formatMillimetres(hist.edges[hover.bin + 1])}
          </div>
          <table>
            <tbody>
              <tr>
                <td>Epochs</td>
                <td className="viz-num">{hist.counts[hover.bin].toLocaleString()}</td>
                <td className="viz-num viz-dim">
                  {((100 * hist.counts[hover.bin]) / Math.max(1, values.length)).toFixed(1)}% of all
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}
