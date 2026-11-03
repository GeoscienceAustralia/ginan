/**
 * Grid and axis painting, split out of chartUtils to keep each file under
 * 500 lines. These are the functions that put ticks, labels, gridlines and
 * time-axis markers (hover crosshair, selected epoch, drag band) on a canvas;
 * chartUtils keeps the scales, theme and formatting they use.
 */

import { type Scale, type VizTheme, toPx, formatMetres, withAlpha } from "./chartUtils";

/** Hairline frame and horizontal gridlines. Recessive by design. */
export function drawGrid(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  x: Scale,
  y: Scale,
  yTicks: number[],
) {
  ctx.save();
  ctx.strokeStyle = theme.grid;
  ctx.lineWidth = 1;
  ctx.beginPath();
  for (const t of yTicks) {
    // Half-pixel offset keeps a 1px line on one row of pixels instead of
    // smearing it across two.
    const py = Math.round(toPx(y, t)) + 0.5;
    ctx.moveTo(x.p0, py);
    ctx.lineTo(x.p1, py);
  }
  ctx.stroke();
  ctx.strokeRect(
    Math.round(x.p0) + 0.5,
    Math.round(y.p1) + 0.5,
    Math.round(x.p1 - x.p0),
    Math.round(y.p0 - y.p1),
  );
  ctx.restore();
}

/** Y tick labels, right-aligned into the left gutter. */
export function drawYAxis(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  y: Scale,
  ticks: number[],
  precision: number,
  x0: number,
) {
  ctx.save();
  ctx.fillStyle = theme.textDim;
  ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
  ctx.textAlign = "right";
  ctx.textBaseline = "middle";
  for (const t of ticks) {
    ctx.fillText(formatMetres(t, precision), x0 - 8, toPx(y, t));
  }
  ctx.restore();
}

/** X tick labels under the bottom facet; two-line labels carry the date. */
export function drawXAxis(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  x: Scale,
  ticks: { value: number; label: string }[],
  yBottom: number,
  canvasWidth: number,
) {
  ctx.save();
  ctx.fillStyle = theme.textDim;
  ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
  ctx.textBaseline = "top";
  for (const t of ticks) {
    const px = toPx(x, t.value);
    if (px < x.p0 - 1 || px > x.p1 + 1) continue;
    t.label.split("\n").forEach((line, i) => {
      drawEdgeSafeLabel(ctx, line, px, yBottom + 6 + i * 12, canvasWidth);
    });
  }
  ctx.restore();
}

/**
 * Centres a label on `px`, but flips to left- or right-aligned when centring
 * would push it off the canvas. A tick sitting on the last pixel of the axis is
 * the normal case, not an edge case, and half a clipped timestamp is worse than
 * a nudged one.
 */
export function drawEdgeSafeLabel(
  ctx: CanvasRenderingContext2D,
  text: string,
  px: number,
  py: number,
  canvasWidth: number,
) {
  const half = ctx.measureText(text).width / 2;
  ctx.textAlign = px + half > canvasWidth - 2 ? "right" : px - half < 2 ? "left" : "center";
  ctx.fillText(text, px, py);
}

/** Nearest sample to a pixel position on a sorted, monotonic x array. */
export function nearestIndex(xs: number[], target: number): number {
  if (xs.length === 0) return -1;
  let lo = 0;
  let hi = xs.length - 1;
  while (hi - lo > 1) {
    const mid = (lo + hi) >> 1;
    if (xs[mid] < target) lo = mid;
    else hi = mid;
  }
  return target - xs[lo] <= xs[hi] - target ? lo : hi;
}

/** Vertical hairline at the hovered time, spanning the plot height. */
export function drawCrosshair(
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

/**
 * The selected epoch on a time axis: a solid accent line with a small flag at
 * the top, distinct from the dim hover crosshair so the two can coexist.
 */
export function drawSelectedTime(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  px: number,
  yTop: number,
  yBottom: number,
) {
  const x = Math.round(px) + 0.5;
  ctx.save();
  ctx.strokeStyle = theme.accent;
  ctx.lineWidth = 1.5;
  ctx.beginPath();
  ctx.moveTo(x, yTop);
  ctx.lineTo(x, yBottom);
  ctx.stroke();
  ctx.fillStyle = theme.accent;
  ctx.beginPath();
  ctx.moveTo(x - 5, yTop);
  ctx.lineTo(x + 5, yTop);
  ctx.lineTo(x, yTop + 6);
  ctx.closePath();
  ctx.fill();
  ctx.restore();
}

/** Accent ring around the selected epoch's point. */
export function drawSelectedRing(ctx: CanvasRenderingContext2D, theme: VizTheme, px: number, py: number) {
  ctx.save();
  ctx.lineWidth = 2.5;
  ctx.strokeStyle = theme.accent;
  ctx.beginPath();
  ctx.arc(px, py, 7, 0, Math.PI * 2);
  ctx.stroke();
  ctx.restore();
}

/** Shaded band for an in-progress drag-to-zoom over a time axis. */
export function drawSelection(
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

export function clamp(v: number, lo: number, hi: number): number {
  return Math.min(hi, Math.max(lo, v));
}
