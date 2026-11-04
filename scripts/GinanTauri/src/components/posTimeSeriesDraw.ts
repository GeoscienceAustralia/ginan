import { MARKER_MIN_SPACING, type Facet } from "./PosTimeSeriesChart";
/**
 * Canvas drawing for PosTimeSeriesChart, split out to keep each file under
 * 500 lines. Pure rendering: every function takes a context and data and draws,
 * holding no state of its own.
 */

import {
  formatMillimetres,
  toPx,
  withAlpha,
  type LegendEntry,
  type Scale,
  type VizTheme,
} from "./chartUtils";

export function drawSigmaEnvelope(
  ctx: CanvasRenderingContext2D,
  x: Scale,
  y: Scale,
  t: number[],
  values: number[],
  sigmas: number[],
  i0: number,
  i1: number,
  color: string,
) {
  ctx.save();
  ctx.fillStyle = withAlpha(color, 0.18);
  ctx.beginPath();
  for (let k = i0; k <= i1; k++) {
    const px = toPx(x, t[k]);
    const py = toPx(y, values[k] + sigmas[k]);
    if (k === i0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  }
  for (let k = i1; k >= i0; k--) {
    ctx.lineTo(toPx(x, t[k]), toPx(y, values[k] - sigmas[k]));
  }
  ctx.closePath();
  ctx.fill();
  ctx.restore();
}

export function drawSeriesLine(
  ctx: CanvasRenderingContext2D,
  x: Scale,
  y: Scale,
  t: number[],
  values: number[],
  i0: number,
  i1: number,
  color: string,
  theme: VizTheme,
) {
  ctx.save();
  ctx.strokeStyle = color;
  ctx.lineWidth = 2;
  ctx.lineJoin = "round";
  ctx.lineCap = "round";
  ctx.beginPath();
  for (let k = i0; k <= i1; k++) {
    const px = toPx(x, t[k]);
    const py = toPx(y, values[k]);
    if (k === i0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  }
  ctx.stroke();

  // Markers only once they can be told apart; otherwise they thicken the line
  // into a blob and say nothing the line hasn't.
  const count = i1 - i0 + 1;
  const spacing = count > 1 ? (x.p1 - x.p0) / (count - 1) : Infinity;
  if (spacing >= MARKER_MIN_SPACING) {
    for (let k = i0; k <= i1; k++) {
      const px = toPx(x, t[k]);
      const py = toPx(y, values[k]);
      ctx.beginPath();
      ctx.arc(px, py, 4, 0, Math.PI * 2);
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

export function drawHoverMarker(
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

/**
 * Legend rows for one facet, naming every series the panel actually draws.
 *
 * Units are metres to match the y axis. The hover readout uses millimetres,
 * but the legend describes the plot, and a legend that disagreed with the axis
 * beside it would be worse than no legend.
 */
export function legendFor(
  facet: Facet,
  color: string,
  theme: VizTheme,
  showSigma: boolean,
  showCi: boolean,
): LegendEntry[] {
  const entries: LegendEntry[] = [
    { label: facet.label, unit: "m", color, swatch: "line" },
  ];
  if (showSigma) {
    entries.push({
      label: "±1σ formal error",
      unit: "m",
      color,
      swatch: "band",
      alpha: 0.22,
    });
  }
  if (showCi) {
    entries.push({ label: "Weighted mean", unit: "m", color, swatch: "line" });
    entries.push({
      label: "±2σ scatter",
      unit: "m",
      color: theme.text,
      swatch: "band",
      alpha: 0.12,
    });
  }
  return entries;
}

/** Component name and RMS, inside the panel's top-left, with a colour key. */
export function drawFacetCaption(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  color: string,
  facet: Facet,
  x0: number,
  yTop: number,
  rms: number,
) {
  ctx.save();
  const y = yTop + 12;
  ctx.fillStyle = color;
  ctx.fillRect(x0 + 8, y - 1.5, 10, 3);
  ctx.font = "600 11px ui-sans-serif, system-ui, sans-serif";
  ctx.fillStyle = theme.text;
  ctx.textAlign = "left";
  ctx.textBaseline = "middle";
  ctx.fillText(`${facet.label} · ${facet.axis}`, x0 + 24, y);
  const w = ctx.measureText(`${facet.label} · ${facet.axis}`).width;
  ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
  ctx.fillStyle = theme.textDim;
  ctx.fillText(`RMS ${formatMillimetres(rms)}`, x0 + 32 + w, y);
  ctx.restore();
}
