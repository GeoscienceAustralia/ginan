import type { PosSeries } from "../../types";

/**
 * Quantities derived from a loaded `.pos` series for the extra panels.
 *
 * These are computed from the arrays the backend returned, which are
 * decimated to a display budget on long runs. The per-component statistics in
 * `series.stats` are exact (the backend computes them over every epoch); the
 * percentiles here are over the drawn sample, and the panels say so when the
 * series is decimated.
 */

export type ErrorMetric = "h" | "v" | "3d";

export const ERROR_METRICS: { key: ErrorMetric; label: string; long: string }[] = [
  { key: "h", label: "Horizontal", long: "√(dN² + dE²)" },
  { key: "v", label: "Vertical", long: "|dU|" },
  { key: "3d", label: "3D", long: "√(dN² + dE² + dU²)" },
];

export interface DerivedErrors {
  h: number[];
  v: number[];
  "3d": number[];
  /** Horizontal formal error √(σN² + σE²), per epoch. */
  sh: number[];
}

const cache = new WeakMap<PosSeries, DerivedErrors>();

/** Error magnitudes per epoch, memoised per series object. */
export function deriveErrors(series: PosSeries): DerivedErrors {
  const hit = cache.get(series);
  if (hit) return hit;
  const n = series.t.length;
  const h = new Array<number>(n);
  const v = new Array<number>(n);
  const d3 = new Array<number>(n);
  const sh = new Array<number>(n);
  for (let i = 0; i < n; i++) {
    const dn = series.dn[i];
    const de = series.de[i];
    const du = series.du[i];
    h[i] = Math.hypot(dn, de);
    v[i] = Math.abs(du);
    d3[i] = Math.hypot(dn, de, du);
    sh[i] = Math.hypot(series.sn[i], series.se[i]);
  }
  const out = { h, v, "3d": d3, sh };
  cache.set(series, out);
  return out;
}

/** Linear-interpolated percentile (0–100) of an already-sorted array. */
export function percentileSorted(sorted: number[], p: number): number {
  if (sorted.length === 0) return NaN;
  if (sorted.length === 1) return sorted[0];
  const rank = (p / 100) * (sorted.length - 1);
  const lo = Math.floor(rank);
  const hi = Math.ceil(rank);
  return sorted[lo] + (sorted[hi] - sorted[lo]) * (rank - lo);
}

export function sortedCopy(values: number[]): number[] {
  return values.slice().sort((a, b) => a - b);
}

export function rms(values: number[]): number {
  if (values.length === 0) return NaN;
  let s = 0;
  for (const v of values) s += v * v;
  return Math.sqrt(s / values.length);
}

export function meanStd(values: number[]): { mean: number; std: number } {
  const n = values.length;
  if (n === 0) return { mean: NaN, std: NaN };
  let sum = 0;
  for (const v of values) sum += v;
  const mean = sum / n;
  let ss = 0;
  for (const v of values) ss += (v - mean) * (v - mean);
  return { mean, std: n > 1 ? Math.sqrt(ss / (n - 1)) : 0 };
}

/**
 * First epoch after which the horizontal formal error stays at or below
 * `threshold` metres for the rest of the run — the usual "time to converge"
 * for a PPP filter. Null if it never settles.
 */
export function convergenceTime(series: PosSeries, threshold: number): number | null {
  const { sh } = deriveErrors(series);
  const n = sh.length;
  if (n === 0 || sh[n - 1] > threshold) return null;
  let i = n - 1;
  while (i > 0 && sh[i - 1] <= threshold) i--;
  return series.t[i] - series.t[0];
}

/** Most common spacing between consecutive epochs, in ms. */
export function typicalInterval(t: number[]): number | null {
  if (t.length < 2) return null;
  const counts = new Map<number, number>();
  const limit = Math.min(t.length, 2000);
  for (let i = 1; i < limit; i++) {
    const d = Math.round(t[i] - t[i - 1]);
    if (d > 0) counts.set(d, (counts.get(d) ?? 0) + 1);
  }
  let best: number | null = null;
  let bestCount = 0;
  for (const [d, c] of counts) {
    if (c > bestCount) {
      best = d;
      bestCount = c;
    }
  }
  return best;
}

export interface Histogram {
  edges: number[];
  counts: number[];
  max: number;
  /** Samples outside the binned range (only when a clip was asked for). */
  outside: number;
}

/**
 * Freedman–Diaconis binning, clamped to a sensible count so a panel neither
 * shows a comb of single-sample bins nor a handful of slabs.
 *
 * `clip` bins only the central `clip`–`100 - clip` percentile range. A PPP
 * run's first minutes sit metres out while the rest is millimetres; binned
 * over the full range, the converged solution collapses into one bar.
 */
export function histogram(values: number[], clip = 0, minBins = 8, maxBins = 60): Histogram {
  const all = sortedCopy(values);
  const sorted = clip > 0 ? all.filter((v) => v >= percentileSorted(all, clip) && v <= percentileSorted(all, 100 - clip)) : all;
  const outside = all.length - sorted.length;
  const n = sorted.length;
  if (n === 0) return { edges: [], counts: [], max: 0, outside };
  const lo = sorted[0];
  const hi = sorted[n - 1];
  if (!(hi > lo)) return { edges: [lo - 0.0005, hi + 0.0005], counts: [n], max: n, outside };

  const iqr = percentileSorted(sorted, 75) - percentileSorted(sorted, 25);
  const fd = iqr > 0 ? 2 * iqr * Math.cbrt(1 / n) : (hi - lo) / Math.sqrt(n);
  const bins = Math.min(maxBins, Math.max(minBins, Math.round((hi - lo) / fd)));
  const width = (hi - lo) / bins;

  const edges = Array.from({ length: bins + 1 }, (_, i) => lo + i * width);
  const counts = new Array<number>(bins).fill(0);
  for (const v of sorted) {
    const k = Math.min(bins - 1, Math.floor((v - lo) / width));
    counts[k] += 1;
  }
  return { edges, counts, max: Math.max(...counts), outside };
}

/**
 * The stretch of the run where most of the "action" is — the part worth
 * zooming the timeline into when a file opens.
 *
 * Activity is how far each epoch's horizontal error sits from the run's
 * typical level, in decades, less the spread ordinary noise already produces
 * (the 90th percentile of that distance). A converging filter, a reset or a
 * jump scores; the settled solution's own scatter doesn't. The result is the
 * shortest time window holding `share` of the total activity, padded a
 * little, or null when the activity is spread across most of the run (or
 * there is none), in which case the whole run is the right view.
 */
export function activityWindow(series: PosSeries, share = 0.8, maxFraction = 0.6): [number, number] | null {
  const t = series.t;
  const n = t.length;
  if (n < 20) return null;
  const { h } = deriveErrors(series);
  const logs = h.map((v) => Math.log10(Math.max(v, 1e-4)));
  const med = percentileSorted(sortedCopy(logs), 50);
  const dev = logs.map((v) => Math.abs(v - med));
  const noise = percentileSorted(sortedCopy(dev), 90);
  const act = dev.map((d) => Math.max(0, d - noise));
  const total = act.reduce((a, b) => a + b, 0);
  if (!(total > 0)) return null;

  // Shortest window (by time) whose activity reaches share * total.
  const need = share * total;
  let best: [number, number] | null = null;
  let sum = 0;
  let lo = 0;
  for (let hi = 0; hi < n; hi++) {
    sum += act[hi];
    while (lo < hi && sum - act[lo] >= need) {
      sum -= act[lo];
      lo++;
    }
    if (sum >= need && (!best || t[hi] - t[lo] < best[1] - best[0])) best = [t[lo], t[hi]];
  }
  if (!best) return null;

  const full = t[n - 1] - t[0];
  const pad = Math.max((best[1] - best[0]) * 0.15, (full / n) * 5);
  const a = Math.max(t[0], best[0] - pad);
  const b = Math.min(t[n - 1], best[1] + pad);
  return b - a > maxFraction * full ? null : [a, b];
}
