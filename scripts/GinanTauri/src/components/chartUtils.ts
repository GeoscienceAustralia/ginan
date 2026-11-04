/**
 * Canvas plotting primitives shared by the Visualization tab.
 *
 * The charts are drawn on a 2D canvas rather than assembled from SVG nodes: a
 * day of 1 Hz output is tens of thousands of points per component, which is
 * fine as canvas paths and ruinous as DOM. Everything data-bearing is painted
 * here; only the legend, tooltip and controls are HTML.
 *
 * Colours come from CSS custom properties (see the "Visualization" block in
 * App.css) so the light and dark palettes stay declared in one place and a
 * theme switch is picked up on the next draw.
 */

import { useEffect, useRef, useState, type RefObject } from "react";
import { formatDateTime, formatDayMonth, formatTime, timeZoneOffsetMs } from "../utils/time";

/** Plot area insets. The left gutter has to hold a signed metre value. */
export const PAD = { left: 62, right: 14, top: 10, bottom: 26 };

/**
 * Horizontal gutter a chart reserves for its legend, to the right of the plot.
 *
 * Charts subtract this from their right edge rather than growing `PAD.right`,
 * which is shared with charts that draw no legend. Sitting outside the plot is
 * the point: a legend floated over the data covers the very samples the user is
 * reading.
 */
export const LEGEND_WIDTH = 150;

/** Narrowest chart that still gets a legend gutter; see {@link legendWidth}. */
const MIN_WIDTH_FOR_LEGEND = 460;

/**
 * The legend gutter for a chart this wide. A narrow tile can't spare 150 px
 * without squeezing the plot to a sliver, so below a threshold the legend is
 * dropped — the facet captions, tooltip and statistics still name every series.
 */
export function legendWidth(chartWidth: number): number {
  return chartWidth >= MIN_WIDTH_FOR_LEGEND ? LEGEND_WIDTH : 0;
}

/**
 * Vertical space the legend leaves at the top of its gutter for the zoom hint
 * and "Reset zoom" button, which share that corner.
 */
export const LEGEND_TOP_GAP = 16;

/** How a legend entry's swatch is drawn — matches how the series is plotted. */
type LegendSwatch = "line" | "dashed" | "band" | "marker";

export interface LegendEntry {
  label: string;
  /** Unit shown after the label, e.g. "m". Omit for unitless entries. */
  unit?: string;
  color: string;
  swatch: LegendSwatch;
  /** Fill alpha for "band"; ignored otherwise. */
  alpha?: number;
}

/**
 * Legend block, drawn top-down from (x, yTop) in the reserved right gutter.
 *
 * Colours come from the caller so a swatch is always the colour the series is
 * actually drawn in, and text uses the theme's own foreground tokens, which is
 * what keeps it legible in both light and dark mode.
 *
 * `yBottom` is how far down the block may go — a faceted chart passes its
 * facet's bottom edge. Rows past it are left off rather than drawn over the
 * next facet's legend; entries are ordered most important first, so what is
 * kept is the line itself. Nothing is drawn when the chart has no gutter
 * (see {@link legendWidth}).
 */
export function drawLegend(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  entries: LegendEntry[],
  x: number,
  yTop: number,
  yBottom = Infinity,
): void {
  if (entries.length === 0 || x + 40 > ctx.canvas.clientWidth) return;
  ctx.save();
  ctx.textAlign = "left";
  ctx.textBaseline = "middle";

  const rowH = 16;
  const swatchW = 14;
  const textX = x + swatchW + 7;
  const first = yTop + LEGEND_TOP_GAP + 8;
  const fits = Math.max(1, Math.floor((yBottom - first) / rowH) + 1);

  entries.slice(0, fits).forEach((e, i) => {
    const y = yTop + LEGEND_TOP_GAP + 8 + i * rowH;

    ctx.strokeStyle = e.color;
    ctx.fillStyle = e.color;
    ctx.lineWidth = 2;

    if (e.swatch === "band") {
      ctx.globalAlpha = e.alpha ?? 0.22;
      ctx.fillRect(x, y - 4, swatchW, 8);
      ctx.globalAlpha = 1;
    } else if (e.swatch === "marker") {
      ctx.beginPath();
      ctx.arc(x + swatchW / 2, y, 3, 0, Math.PI * 2);
      ctx.fill();
    } else {
      ctx.beginPath();
      if (e.swatch === "dashed") ctx.setLineDash([4, 3]);
      ctx.moveTo(x, y);
      ctx.lineTo(x + swatchW, y);
      ctx.stroke();
      ctx.setLineDash([]);
    }

    ctx.font = "11px ui-sans-serif, system-ui, sans-serif";
    ctx.fillStyle = theme.text;
    ctx.fillText(e.label, textX, y);

    if (e.unit) {
      const w = ctx.measureText(e.label).width;
      ctx.fillStyle = theme.textDim;
      ctx.fillText(`(${e.unit})`, textX + w + 4, y);
    }
  });

  ctx.restore();
}

/** Series slots 1–3 of the validated categorical palette, in dN/dE/dU order. */
export interface VizTheme {
  surface: string;
  text: string;
  textMuted: string;
  textDim: string;
  grid: string;
  /** The selected-epoch marker — the app accent, so it reads as "chosen". */
  accent: string;
  /** Slots 1–3 are the validated palette every chart uses; 4–6 only serve
   *  charts that can have more series than that, such as a satellite's signals. */
  series: [string, string, string, string, string, string];
}

const FALLBACK: VizTheme = {
  surface: "#2c2c2c",
  text: "#f0f0f0",
  textMuted: "#b3b3b3",
  textDim: "#8c8c8c",
  grid: "#444444",
  accent: "#0d99ff",
  series: ["#6fa6db", "#e39a55", "#4fbf9a", "#a77bdb", "#d6a419", "#d9578c"],
};

/**
 * Reads the palette off `el`'s computed style. Called once per draw — cheap
 * enough at that rate, and it means the charts follow the theme toggle without
 * threading the theme value through every component.
 */
export function readVizTheme(el: HTMLElement | null): VizTheme {
  if (!el) return FALLBACK;
  const s = getComputedStyle(el);
  const read = (name: string, fallback: string) => {
    const v = s.getPropertyValue(name).trim();
    return v || fallback;
  };
  return {
    surface: read("--viz-surface", FALLBACK.surface),
    text: read("--text", FALLBACK.text),
    textMuted: read("--text-muted", FALLBACK.textMuted),
    textDim: read("--text-dim", FALLBACK.textDim),
    grid: read("--viz-grid", FALLBACK.grid),
    accent: read("--accent", FALLBACK.accent),
    series: [
      read("--viz-series-1", FALLBACK.series[0]),
      read("--viz-series-2", FALLBACK.series[1]),
      read("--viz-series-3", FALLBACK.series[2]),
      read("--viz-series-4", FALLBACK.series[3]),
      read("--viz-series-5", FALLBACK.series[4]),
      read("--viz-series-6", FALLBACK.series[5]),
    ],
  };
}

/** `#rrggbb` (or `#rgb`) at the given alpha. Non-hex input is returned as-is. */
export function withAlpha(color: string, alpha: number): string {
  let hex = color.trim();
  if (!hex.startsWith("#")) return color;
  hex = hex.slice(1);
  if (hex.length === 3) hex = hex.split("").map((c) => c + c).join("");
  if (hex.length !== 6) return color;
  const n = parseInt(hex, 16);
  if (Number.isNaN(n)) return color;
  return `rgba(${(n >> 16) & 255}, ${(n >> 8) & 255}, ${n & 255}, ${alpha})`;
}

/**
 * Cool-to-warm gradient shared by every chart that colors epochs by recency
 * (the map, the horizontal scatter): oldest reads as "settled", newest as
 * "hot", so a run's direction of travel is visible without a time axis.
 * `t` is 0 (oldest) to 1 (newest).
 */
const RECENCY_OLD: [number, number, number] = [58, 110, 224]; // blue
const RECENCY_NEW: [number, number, number] = [235, 83, 62]; // red-orange

export function recencyColor(t: number, alpha = 1): string {
  const [r0, g0, b0] = RECENCY_OLD;
  const [r1, g1, b1] = RECENCY_NEW;
  const r = Math.round(r0 + (r1 - r0) * t);
  const g = Math.round(g0 + (g1 - g0) * t);
  const b = Math.round(b0 + (b1 - b0) * t);
  return alpha >= 1 ? `rgb(${r}, ${g}, ${b})` : `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

// ── Sizing ───────────────────────────────────────────────────────────────────

/**
 * Tracks an element's content box. Charts need a pixel size before they can
 * lay out, and the workspace is resizable, so every chart observes its own box.
 */
export function useElementSize<T extends HTMLElement>(): [
  RefObject<T>,
  { width: number; height: number },
] {
  const ref = useRef<T>(null);
  const [size, setSize] = useState({ width: 0, height: 0 });

  useEffect(() => {
    const el = ref.current;
    if (!el) return;

    // Round to whole pixels: sub-pixel churn from a flex layout would otherwise
    // trigger a redraw on every frame of a sidebar drag.
    const apply = (width: number, height: number) =>
      setSize((prev) => {
        const w = Math.round(width);
        const h = Math.round(height);
        return prev.width === w && prev.height === h ? prev : { width: w, height: h };
      });

    // Measure now rather than waiting on the observer's first callback. A chart
    // mounted into a box that is already its final size — switching to this tab,
    // or flipping between figures — may never get another resize notification,
    // and a chart that only draws on resize is a chart that stays blank.
    const box = el.getBoundingClientRect();
    apply(box.width, box.height);

    const observer = new ResizeObserver((entries) => {
      const rect = entries[0]?.contentRect;
      if (rect) apply(rect.width, rect.height);
    });
    observer.observe(el);
    return () => observer.disconnect();
  }, []);

  return [ref, size];
}

/**
 * Runs `draw` whenever it changes, and again on a theme flip. The palette lives
 * in CSS custom properties, so a theme switch changes no React state — the
 * chart has to watch the attribute the theme is stamped on instead.
 */
export function useCanvasRedraw(draw: () => void): void {
  const latest = useRef(draw);
  latest.current = draw;

  useEffect(() => {
    draw();
  }, [draw]);

  useEffect(() => {
    const observer = new MutationObserver(() => latest.current());
    observer.observe(document.documentElement, { attributes: true, attributeFilter: ["data-theme"] });
    return () => observer.disconnect();
  }, []);
}

/** Pointer position relative to the element the handler is attached to. */
export function canvasPoint(e: { clientX: number; clientY: number; currentTarget: Element }): {
  px: number;
  py: number;
} {
  const rect = e.currentTarget.getBoundingClientRect();
  return { px: e.clientX - rect.left, py: e.clientY - rect.top };
}

/**
 * Sizes the backing store for the device pixel ratio and returns a context
 * already scaled to CSS pixels, so all drawing code works in layout units.
 */
export function prepareCanvas(
  canvas: HTMLCanvasElement | null,
  width: number,
  height: number,
): CanvasRenderingContext2D | null {
  if (!canvas || width <= 0 || height <= 0) return null;
  const dpr = window.devicePixelRatio || 1;
  const w = Math.round(width * dpr);
  const h = Math.round(height * dpr);
  if (canvas.width !== w || canvas.height !== h) {
    canvas.width = w;
    canvas.height = h;
  }
  const ctx = canvas.getContext("2d");
  if (!ctx) return null;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, width, height);
  return ctx;
}

// ── Scales ───────────────────────────────────────────────────────────────────

export interface Scale {
  /** Data-space bounds. */
  d0: number;
  d1: number;
  /** Pixel-space bounds; `p1 < p0` for the y direction. */
  p0: number;
  p1: number;
}

export function scale(d0: number, d1: number, p0: number, p1: number): Scale {
  // A zero-width domain (one epoch, or a dead-flat series) would divide by
  // zero; nudge it apart so the single value lands mid-axis.
  if (!(d1 > d0)) {
    const pad = Math.abs(d0) > 0 ? Math.abs(d0) * 0.05 : 0.5;
    return { d0: d0 - pad, d1: d0 + pad, p0, p1 };
  }
  return { d0, d1, p0, p1 };
}

export function toPx(s: Scale, v: number): number {
  return s.p0 + ((v - s.d0) / (s.d1 - s.d0)) * (s.p1 - s.p0);
}

export function toData(s: Scale, px: number): number {
  return s.d0 + ((px - s.p0) / (s.p1 - s.p0)) * (s.d1 - s.d0);
}

/**
 * Pads a data range outward to a multiple of the tick step, so the series
 * never touches the frame and the top and bottom land on labelled ticks.
 */
export function padRange(min: number, max: number, fraction = 0.08): [number, number] {
  if (!Number.isFinite(min) || !Number.isFinite(max)) return [-1, 1];
  if (min === max) {
    const pad = Math.abs(min) > 0 ? Math.abs(min) * 0.1 : 0.5;
    return [min - pad, max + pad];
  }
  const pad = (max - min) * fraction;
  return [min - pad, max + pad];
}

// ── Ticks & formatting ───────────────────────────────────────────────────────

/** Ticks on a 1/2/5×10ⁿ step covering [min, max]. */
export function niceTicks(min: number, max: number, target = 5): number[] {
  if (!(max > min) || !Number.isFinite(min) || !Number.isFinite(max)) return [];
  const raw = (max - min) / Math.max(1, target);
  const mag = Math.pow(10, Math.floor(Math.log10(raw)));
  const norm = raw / mag;
  const step = (norm >= 5 ? 10 : norm >= 2 ? 5 : norm >= 1 ? 2 : 1) * mag;
  const first = Math.ceil(min / step) * step;
  const ticks: number[] = [];
  // The slack is float-error sized and nothing more. Anything looser emits a
  // tick beyond the domain, whose gridline and label then land outside the plot
  // frame — in a faceted chart, on top of the neighbouring panel's labels.
  const eps = step * 1e-9;
  for (let v = first; v <= max + eps; v += step) {
    if (v >= min - eps) ticks.push(Math.abs(v) < eps ? 0 : v);
  }
  return ticks;
}

/** Decimal places needed to tell neighbouring ticks apart. */
export function stepPrecision(step: number): number {
  if (!(step > 0)) return 3;
  return Math.min(9, Math.max(0, Math.ceil(-Math.log10(step)) + 1));
}

/** Metres, fixed to `precision`, with a real minus sign and no "-0". */
export function formatMetres(v: number, precision: number): string {
  const s = v.toFixed(precision);
  return s === (0).toFixed(precision) || Number(s) === 0 ? (0).toFixed(precision) : s;
}

/**
 * Millimetres for the readouts. Position residuals live at the millimetre to
 * centimetre scale, where "0.0123 m" is harder to compare at a glance than
 * "12.3 mm".
 */
export function formatMillimetres(v: number): string {
  const mm = v * 1000;
  const abs = Math.abs(mm);
  const digits = abs >= 100 ? 0 : abs >= 10 ? 1 : 2;
  return `${mm.toFixed(digits)} mm`;
}

/**
 * Millimetres, but metres once the value stops being a millimetre-scale
 * quantity. Position residuals are always small enough for
 * {@link formatMillimetres}; a `.TRACE` prefit residual is the raw
 * measurement misclosure and routinely runs to metres, where "3421.7 mm" is
 * three digits of noise pretending to be precision.
 */
export function formatResidual(v: number): string {
  if (Math.abs(v) >= 1) return `${v.toFixed(3)} m`;
  return formatMillimetres(v);
}

const TIME_STEPS_MS = [
  1000, 2000, 5000, 10_000, 15_000, 30_000,
  60_000, 2 * 60_000, 5 * 60_000, 10 * 60_000, 15 * 60_000, 30 * 60_000,
  3_600_000, 2 * 3_600_000, 3 * 3_600_000, 6 * 3_600_000, 12 * 3_600_000,
  86_400_000, 2 * 86_400_000, 7 * 86_400_000,
];

/**
 * Axis ticks for an epoch-millisecond range, labelled in `timeZone`.
 *
 * The epochs are PEA's GPS-time digits carried through as UTC (see
 * utils/time), so with UTC selected the labels read exactly like the .pos
 * file. Steps are aligned to the zone's wall clock, so labels land on 00:00,
 * 00:30, … there even in a zone with a half-hour offset.
 */
export function timeAxisTicks(
  minMs: number,
  maxMs: number,
  target: number,
  timeZone: string,
): { value: number; label: string }[] {
  if (!(maxMs > minMs)) return [];
  const span = maxMs - minMs;
  const raw = span / Math.max(1, target);
  const step = TIME_STEPS_MS.find((s) => s >= raw) ?? TIME_STEPS_MS[TIME_STEPS_MS.length - 1];
  const showDate = step >= 86_400_000 || spansMoreThanOneDay(minMs, maxMs, timeZone);
  const seconds = step < 60_000;

  const ticks: { value: number; label: string }[] = [];
  // Step from a boundary-aligned start so labels read 00:00, 00:30, … rather
  // than from wherever the first epoch happened to fall. The offset is taken
  // once, at the start of the range; a DST change inside it only nudges the
  // ticks off the round number, the labels themselves stay correct.
  const offset = timeZoneOffsetMs(minMs, timeZone);
  for (let v = Math.ceil((minMs + offset) / step) * step - offset; v <= maxMs; v += step) {
    const time = formatTime(v, timeZone, { seconds });
    ticks.push({
      value: v,
      label: showDate ? `${formatDayMonth(v, timeZone)}
${time}` : time,
    });
  }
  return ticks;
}

function spansMoreThanOneDay(minMs: number, maxMs: number, timeZone: string): boolean {
  return (
    formatDayMonth(minMs, timeZone) !== formatDayMonth(maxMs, timeZone) ||
    maxMs - minMs > 86_400_000
  );
}

/**
 * Full timestamp for tooltips and readouts, in the .pos file's own layout
 * ("2026-09-22 08:59:09") and, unless `zone` is false, the zone's abbreviation.
 */
export function formatEpoch(ms: number, timeZone: string, opts: { zone?: boolean } = {}): string {
  return formatDateTime(ms, timeZone, { zone: opts.zone ?? true });
}

/** "3 h 20 m", "45 s" — the duration shown beside an epoch count. */
export function formatDuration(ms: number): string {
  if (!Number.isFinite(ms) || ms <= 0) return "-";
  const s = Math.round(ms / 1000);
  if (s < 60) return `${s} s`;
  const m = Math.floor(s / 60);
  if (m < 60) return `${m} m`;
  const h = Math.floor(m / 60);
  const rem = m % 60;
  if (h < 24) return rem ? `${h} h ${rem} m` : `${h} h`;
  const d = Math.floor(h / 24);
  return `${d} d ${h % 24} h`;
}

// ── Drawing helpers ──────────────────────────────────────────────────────────

// Grid, axis and time-marker painters live in chartAxes; re-exported so the
// charts that import them from here keep working.
export {
  drawGrid,
  drawYAxis,
  drawXAxis,
  drawEdgeSafeLabel,
  nearestIndex,
  drawCrosshair,
  drawSelectedTime,
  drawSelectedRing,
  drawSelection,
  clamp,
} from "./chartAxes";
