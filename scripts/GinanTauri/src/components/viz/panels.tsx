import { useMemo, type ReactNode } from "react";
import type { PosComponentKey, PosSeries } from "../../types";
import { AlertIcon } from "../icons";
import PosTimeSeriesChart from "../PosTimeSeriesChart";
import PosScatterChart from "../PosScatterChart";
import PosMapChart from "../PosMapChart";
import { formatDuration, formatEpoch, formatMillimetres, withAlpha } from "../chartUtils";
import TimeLinesChart, { neutralInk, type RefLine, type TimeLine } from "./TimeLinesChart";
import HistogramChart, { HIST_QUANTITIES, type HistQuantity } from "./HistogramChart";
import CdfChart, { metricColor, metricCssColor } from "./CdfChart";
import {
  ERROR_METRICS,
  convergenceTime,
  deriveErrors,
  percentileSorted,
  sortedCopy,
  typicalInterval,
  type ErrorMetric,
} from "./metrics";
import type { PanelKind, PanelOptions } from "./layout";
import { useSelectedIndex } from "./VizContext";
import { useTimeZone } from "../../hooks/useTimeZone";

/**
 * The figures a tile can hold. Each entry says what the panel is called, how
 * to draw it from a loaded series, and which settings it offers in the tile's
 * settings popover. Adding a figure to the tab is adding an entry here.
 */

export interface PanelDef {
  kind: PanelKind;
  title: string;
  description: string;
  /** Fewest epochs the figure can draw anything meaningful from: a line
   *  needs two points, a distribution three. Below it the figure is
   *  disabled in the pickers and the tile says why. */
  minEpochs: number;
  /** Data only some solutions have: formal errors (`.pos`, not SPP) or
   *  protection levels (an SBAS `.SPP`). Without it the figure is disabled. */
  needs?: "sigma" | "levels";
  Body: (p: { series: PosSeries; options: PanelOptions }) => ReactNode;
  Options?: (p: { options: PanelOptions; set: (patch: PanelOptions) => void }) => ReactNode;
}

// ── Option helpers ───────────────────────────────────────────────────────────

const bool = (o: PanelOptions, key: string, fallback: boolean) =>
  typeof o[key] === "boolean" ? (o[key] as boolean) : fallback;

function list<T extends string>(o: PanelOptions, key: string, allowed: readonly T[], fallback: T[]): T[] {
  const v = o[key];
  if (!Array.isArray(v)) return fallback;
  const picked = v.filter((x): x is T => allowed.includes(x as T));
  return picked.length ? picked : fallback;
}

function one<T extends string>(o: PanelOptions, key: string, allowed: readonly T[], fallback: T): T {
  return allowed.includes(o[key] as T) ? (o[key] as T) : fallback;
}

function Toggle({ label, checked, onChange, title }: { label: string; checked: boolean; onChange: (v: boolean) => void; title?: string }) {
  return (
    <label className="checkbox-row viz-opt-toggle" title={title}>
      <input type="checkbox" checked={checked} onChange={(e) => onChange(e.target.checked)} />
      {label}
    </label>
  );
}

/** Multi-select as a row of toggle chips; never lets the last one go. */
function Chips<T extends string>({
  label,
  items,
  value,
  onChange,
}: {
  label: string;
  items: { key: T; label: string; color?: string }[];
  value: T[];
  onChange: (v: T[]) => void;
}) {
  return (
    <div className="viz-opt-row">
      <span className="viz-opt-label">{label}</span>
      <div className="viz-chips" role="group" aria-label={label}>
        {items.map((it) => {
          const on = value.includes(it.key);
          return (
            <button
              key={it.key}
              className={`viz-chip${on ? " is-on" : ""}`}
              aria-pressed={on}
              disabled={on && value.length === 1}
              onClick={() => onChange(on ? value.filter((v) => v !== it.key) : [...value, it.key])}
            >
              {it.color && <span className="viz-key" style={{ background: it.color }} />}
              {it.label}
            </button>
          );
        })}
      </div>
    </div>
  );
}

function Choice<T extends string>({
  label,
  items,
  value,
  onChange,
}: {
  label: string;
  items: { key: T; label: string }[];
  value: T;
  onChange: (v: T) => void;
}) {
  return (
    <label className="viz-opt-row">
      <span className="viz-opt-label">{label}</span>
      <select className="viz-select" value={value} onChange={(e) => onChange(e.target.value as T)}>
        {items.map((it) => (
          <option key={it.key} value={it.key}>
            {it.label}
          </option>
        ))}
      </select>
    </label>
  );
}

const COMPONENTS: { key: PosComponentKey; label: string; color: string }[] = [
  { key: "dn", label: "dN", color: "var(--viz-series-1)" },
  { key: "de", label: "dE", color: "var(--viz-series-2)" },
  { key: "du", label: "dU", color: "var(--viz-series-3)" },
];
const COMPONENT_KEYS = COMPONENTS.map((c) => c.key);
const METRIC_KEYS = ERROR_METRICS.map((m) => m.key);
const HIST_KEYS = HIST_QUANTITIES.map((q) => q.key);

const THRESHOLDS = ["0.05", "0.1", "0.2", "0.5"] as const;

// ── Panels built on the generic line chart ───────────────────────────────────

function ErrorPanel({ series, options }: { series: PosSeries; options: PanelOptions }) {
  const metrics = list<ErrorMetric>(options, "metrics", METRIC_KEYS, ["h", "v"]);
  const showP95 = bool(options, "showP95", true);
  const logY = bool(options, "logY", false);
  const d = deriveErrors(series);

  const lines = useMemo<TimeLine[]>(
    () =>
      metrics.map((m) => ({
        key: m,
        label: ERROR_METRICS.find((x) => x.key === m)!.label,
        values: d[m],
        color: (theme) => metricColor(m, theme),
        cssColor: metricCssColor(m),
      })),
    [d, metrics.join(",")],
  );
  const refLines = useMemo<RefLine[]>(
    () =>
      showP95
        ? metrics.map((m) => ({
            label: `${ERROR_METRICS.find((x) => x.key === m)!.label} 95%`,
            value: percentileSorted(sortedCopy(d[m]), 95),
            color: (theme) => withAlpha(metricColor(m, theme), 0.6),
          }))
        : [],
    [d, metrics.join(","), showP95],
  );

  return (
    <TimeLinesChart
      series={series}
      lines={lines}
      refLines={refLines}
      logY={logY}
      ariaLabel={`Position error magnitude over time for ${series.name}.`}
    />
  );
}

function SigmaPanel({ series, options }: { series: PosSeries; options: PanelOptions }) {
  const comps = list<PosComponentKey>(options, "components", COMPONENT_KEYS, COMPONENT_KEYS);
  const logY = bool(options, "logY", true);
  const threshold = Number(one(options, "threshold", THRESHOLDS, "0.1"));
  const showThreshold = bool(options, "showThreshold", true);

  const lines = useMemo<TimeLine[]>(
    () =>
      COMPONENTS.map((c, slot) => ({ c, slot }))
        .filter(({ c }) => comps.includes(c.key))
        .map(({ c, slot }) => ({
          key: c.key,
          label: `σ${c.label.slice(1)}`,
          values: series[c.key === "dn" ? "sn" : c.key === "de" ? "se" : "su"],
          color: (theme) => theme.series[slot],
          cssColor: c.color,
        })),
    [series, comps.join(",")],
  );
  const refLines = useMemo<RefLine[]>(
    () => (showThreshold ? [{ label: `${threshold * 100} cm threshold`, value: threshold, color: neutralInk() }] : []),
    [threshold, showThreshold],
  );

  return (
    <TimeLinesChart
      series={series}
      lines={lines}
      refLines={refLines}
      logY={logY}
      ariaLabel={`Formal error (sigma) per component over time for ${series.name}.`}
    />
  );
}

/** HPL and VPL from an SBAS `.SPP`: how far the solution may be off, at
 *  integrity-level confidence, as PEA computed them each epoch. */
function LevelsPanel({ series, options }: { series: PosSeries; options: PanelOptions }) {
  const logY = bool(options, "logY", false);
  const showP95 = bool(options, "showP95", true);
  const lines = useMemo<TimeLine[]>(
    () => [
      { key: "hpl", label: "HPL", values: series.hpl, color: (theme) => theme.series[0], cssColor: "var(--viz-series-1)" },
      { key: "vpl", label: "VPL", values: series.vpl, color: (theme) => theme.series[2], cssColor: "var(--viz-series-3)" },
    ],
    [series],
  );
  const refLines = useMemo<RefLine[]>(
    () =>
      showP95
        ? lines.map((l) => ({
            label: `${l.label} 95%`,
            value: percentileSorted(sortedCopy(l.values), 95),
            color: (theme) => withAlpha(l.color(theme), 0.6),
          }))
        : [],
    [lines, showP95],
  );
  return (
    <TimeLinesChart
      series={series}
      lines={lines}
      refLines={refLines}
      logY={logY}
      ariaLabel={`SBAS protection levels over time for ${series.name}.`}
    />
  );
}

// ── Summary & statistics ─────────────────────────────────────────────────────

function SummaryPanel({ series, options }: { series: PosSeries; options: PanelOptions }) {
  const timeZone = useTimeZone();
  const threshold = Number(one(options, "threshold", THRESHOLDS, "0.1"));
  const selIndex = useSelectedIndex(series.t);
  const spp = isSpp(series);
  const figures = useMemo(() => {
    const d = deriveErrors(series);
    const h = sortedCopy(d.h);
    const v = sortedCopy(d.v);
    const d3 = sortedCopy(d["3d"]);
    const { dn, de, du } = series.stats;
    const n = series.t.length;
    return {
      duration: formatDuration(series.t[n - 1] - series.t[0]),
      interval: typicalInterval(series.t),
      hRms: Math.hypot(dn.rms, de.rms),
      vRms: du.rms,
      rms3d: Math.hypot(dn.rms, de.rms, du.rms),
      cep50: percentileSorted(h, 50),
      h95: percentileSorted(h, 95),
      v95: percentileSorted(v, 95),
      d95: percentileSorted(d3, 95),
      conv: convergenceTime(series, threshold),
      last: n - 1,
    };
  }, [series, threshold]);

  const tiles: { label: string; value: string; hint?: string }[] = [
    { label: "Epochs", value: series.total_epochs.toLocaleString(), hint: figures.interval ? `every ${formatDuration(figures.interval)}` : undefined },
    { label: "Duration", value: figures.duration },
    { label: "Horizontal RMS", value: formatMillimetres(figures.hRms), hint: "√(RMS N² + RMS E²)" },
    { label: "Vertical RMS", value: formatMillimetres(figures.vRms) },
    { label: "3D RMS", value: formatMillimetres(figures.rms3d) },
    { label: "CEP50", value: formatMillimetres(figures.cep50), hint: "half of epochs within" },
    { label: "Horizontal 95%", value: formatMillimetres(figures.h95) },
    { label: "Vertical 95%", value: formatMillimetres(figures.v95) },
    { label: "3D 95%", value: formatMillimetres(figures.d95) },
    // SPP has no formal errors to converge, but SBAS gives protection levels.
    spp
      ? {
          label: "Protection levels",
          value: series.hpl.length ? `${series.hpl[series.hpl.length - 1].toFixed(1)} / ${series.vpl[series.vpl.length - 1].toFixed(1)} m` : "-",
          hint: "latest HPL / VPL",
        }
      : {
          label: "Convergence",
          value: figures.conv === null ? "Not reached" : figures.conv === 0 ? "From start" : formatDuration(figures.conv),
          hint: `horizontal σ ≤ ${threshold * 100} cm`,
        },
  ];

  return (
    <div className="viz-summary">
      <div className="viz-summary-head">
        <span className="viz-summary-station">{series.station ?? series.name}</span>
        {series.smoothed && <span className="viz-badge">Smoothed</span>}
        {spp && <span className="viz-badge" title="Single-point solution with SBAS corrections">SBAS SPP</span>}
        {series.decimated && (
          <span className="viz-badge" title="Percentiles are over the drawn sample; RMS values are over every epoch">
            Sampled
          </span>
        )}
      </div>
      <div className="viz-kpis">
        {tiles.map((t) => (
          <div className="viz-kpi" key={t.label}>
            <div className="viz-kpi-label">{t.label}</div>
            <div className="viz-kpi-value">{t.value}</div>
            {t.hint && <div className="viz-kpi-hint">{t.hint}</div>}
          </div>
        ))}
      </div>
      {(() => {
        // The timeline's selected epoch when there is one, else the newest.
        const i = selIndex ?? figures.last;
        const sigma = { dn: series.sn[i], de: series.se[i], du: series.su[i] };
        return (
          <div className={`viz-summary-latest${selIndex !== null ? " is-selected" : ""}`}>
            <span className="viz-dim">{selIndex !== null ? "Selected epoch" : "Latest epoch"}</span> {formatEpoch(series.t[i], timeZone)}
            <span className="viz-summary-latest-vals">
              {COMPONENTS.map((c) => (
                <span key={c.key}>
                  <span className="viz-key" style={{ background: c.color }} />
                  {c.label} {formatMillimetres(series[c.key][i])}
                  {!spp && <span className="viz-dim"> ±{formatMillimetres(sigma[c.key])}</span>}
                </span>
              ))}
            </span>
          </div>
        );
      })()}
    </div>
  );
}

function StatsPanel({ series }: { series: PosSeries }) {
  return (
    <div className="viz-stats-panel">
      {/* The numbers behind the figures, and the accessible fallback: every
          value a plot encodes with colour is readable here as text. */}
      <table className="viz-stats">
        <thead>
          <tr>
            <th scope="col">Component</th>
            <th scope="col">Weighted mean</th>
            <th scope="col">Std dev</th>
            <th scope="col">RMS</th>
            <th scope="col">Mean σ</th>
            <th scope="col">Range</th>
          </tr>
        </thead>
        <tbody>
          {(
            [
              ["dN · North", series.stats.dn, 1],
              ["dE · East", series.stats.de, 2],
              ["dU · Up", series.stats.du, 3],
            ] as const
          ).map(([label, st, slot]) => (
            <tr key={label}>
              <th scope="row">
                <span className="viz-key" style={{ background: `var(--viz-series-${slot})` }} />
                {label}
              </th>
              <td>{formatMillimetres(st.weighted_mean)}</td>
              <td>{formatMillimetres(st.std_dev)}</td>
              <td>{formatMillimetres(st.rms)}</td>
              <td>{isSpp(series) ? "-" : formatMillimetres(st.mean_sigma)}</td>
              <td>
                {formatMillimetres(st.min)} ... {formatMillimetres(st.max)}
              </td>
            </tr>
          ))}
        </tbody>
      </table>

      <div className="viz-meta">
        <span>
          {series.station ? `${series.station} · ` : ""}
          {series.total_epochs.toLocaleString()} epochs over {formatDuration(series.t[series.t.length - 1] - series.t[0])}
        </span>
        {series.smoothed && <span>Backward-smoothed solution</span>}
        {isSpp(series) && <span>SBAS single-point solution: no formal errors, so means are unweighted</span>}
        {series.decimated && (
          <span title="Statistics above are computed over every epoch, not the drawn sample">
            Drawn from {series.returned_epochs.toLocaleString()} sampled epochs
          </span>
        )}
        {series.skipped_lines > 0 && (
          <span className="viz-meta--warn">
            <AlertIcon size={12} /> {series.skipped_lines} unreadable line
            {series.skipped_lines === 1 ? "" : "s"} skipped
          </span>
        )}
      </div>
    </div>
  );
}

// ── Registry ─────────────────────────────────────────────────────────────────

/** Whether a series is an SBAS `.SPP` solution: no formal errors, protection levels instead. */
export const isSpp = (s: Pick<PosSeries, "format"> | null | undefined) => s?.format === "spp";

/**
 * Why `def` can't draw `series`, or null when it can (or nothing is loaded yet).
 * Short enough to follow the figure's title in a menu.
 */
export function unavailableReason(
  def: PanelDef,
  series: Pick<PosSeries, "t" | "format"> | null | undefined,
): string | null {
  if (!series) return null;
  if (def.needs === "sigma" && isSpp(series)) return "not in SPP: no formal errors";
  if (def.needs === "levels" && !isSpp(series)) return "SBAS .SPP only";
  if (series.t.length >= def.minEpochs) return null;
  return `needs ${def.minEpochs}+ epochs`;
}

export const PANELS: Record<PanelKind, PanelDef> = {
  timeseries: {
    kind: "timeseries",
    title: "Time series",
    description: "dN, dE and dU against time, one panel each",
    minEpochs: 2,
    Body: ({ series, options }) => (
      <PosTimeSeriesChart
        series={series}
        showSigma={!isSpp(series) && bool(options, "showSigma", true)}
        showCi={bool(options, "showCi", true)}
        components={list(options, "components", COMPONENT_KEYS, COMPONENT_KEYS)}
      />
    ),
    Options: ({ options, set }) => (
      <>
        <Chips
          label="Components"
          items={COMPONENTS}
          value={list(options, "components", COMPONENT_KEYS, COMPONENT_KEYS)}
          onChange={(v) => set({ components: v })}
        />
        <Toggle label="Formal error ±1σ" checked={bool(options, "showSigma", true)} onChange={(v) => set({ showSigma: v })} />
        <Toggle label="Mean ±2σ" checked={bool(options, "showCi", true)} onChange={(v) => set({ showCi: v })} />
      </>
    ),
  },
  scatter: {
    kind: "scatter",
    title: "Horizontal scatter",
    description: "dE against dN on equal axes, coloured by time",
    minEpochs: 1,
    Body: ({ series, options }) => <PosScatterChart series={series} showCi={bool(options, "showCi", true)} />,
    Options: ({ options, set }) => (
      <Toggle label="Mean and ±1σ/±2σ rings" checked={bool(options, "showCi", true)} onChange={(v) => set({ showCi: v })} />
    ),
  },
  map: {
    kind: "map",
    title: "Map",
    description: "Epoch positions on an OpenStreetMap basemap",
    minEpochs: 1,
    Body: ({ series }) => <PosMapChart series={series} />,
  },
  error: {
    kind: "error",
    title: "Error magnitude",
    description: "Horizontal, vertical and 3D error over time",
    minEpochs: 2,
    Body: ErrorPanel,
    Options: ({ options, set }) => (
      <>
        <Chips
          label="Show"
          items={ERROR_METRICS.map((m) => ({ key: m.key, label: m.label, color: metricCssColor(m.key) }))}
          value={list<ErrorMetric>(options, "metrics", METRIC_KEYS, ["h", "v"])}
          onChange={(v) => set({ metrics: v })}
        />
        <Toggle label="95% lines" checked={bool(options, "showP95", true)} onChange={(v) => set({ showP95: v })} />
        <Toggle label="Log scale" checked={bool(options, "logY", false)} onChange={(v) => set({ logY: v })} />
      </>
    ),
  },
  sigma: {
    kind: "sigma",
    title: "Formal error",
    description: "σN, σE, σU over time, showing how the filter converges",
    minEpochs: 2,
    needs: "sigma",
    Body: SigmaPanel,
    Options: ({ options, set }) => (
      <>
        <Chips
          label="Components"
          items={COMPONENTS.map((c) => ({ ...c, label: `σ${c.label.slice(1)}` }))}
          value={list(options, "components", COMPONENT_KEYS, COMPONENT_KEYS)}
          onChange={(v) => set({ components: v })}
        />
        <Toggle label="Log scale" checked={bool(options, "logY", true)} onChange={(v) => set({ logY: v })} />
        <Toggle
          label="Convergence threshold"
          checked={bool(options, "showThreshold", true)}
          onChange={(v) => set({ showThreshold: v })}
        />
        <Choice
          label="Threshold"
          items={THRESHOLDS.map((t) => ({ key: t, label: `${Number(t) * 100} cm` }))}
          value={one(options, "threshold", THRESHOLDS, "0.1")}
          onChange={(v) => set({ threshold: v })}
        />
      </>
    ),
  },
  levels: {
    kind: "levels",
    title: "Protection levels",
    description: "SBAS horizontal and vertical protection levels (HPL, VPL) over time",
    minEpochs: 2,
    needs: "levels",
    Body: LevelsPanel,
    Options: ({ options, set }) => (
      <>
        <Toggle label="Log scale" checked={bool(options, "logY", false)} onChange={(v) => set({ logY: v })} />
        <Toggle label="95% lines" checked={bool(options, "showP95", true)} onChange={(v) => set({ showP95: v })} />
      </>
    ),
  },
  histogram: {
    kind: "histogram",
    title: "Histogram",
    description: "Distribution of a residual or error, with a normal fit",
    minEpochs: 3,
    Body: ({ series, options }) => (
      <HistogramChart
        series={series}
        quantity={one<HistQuantity>(options, "component", HIST_KEYS, "du")}
        showNormal={bool(options, "showNormal", true)}
        clipTail={bool(options, "clipTail", true)}
      />
    ),
    Options: ({ options, set }) => (
      <>
        <Choice
          label="Quantity"
          items={HIST_QUANTITIES.map((q) => ({ key: q.key, label: q.label }))}
          value={one<HistQuantity>(options, "component", HIST_KEYS, "du")}
          onChange={(v) => set({ component: v })}
        />
        <Toggle label="Normal fit" checked={bool(options, "showNormal", true)} onChange={(v) => set({ showNormal: v })} />
        <Toggle
          label="Trim outer 1% each side"
          title="Bin the central 98% so the convergence period doesn't squash the converged solution into one bar"
          checked={bool(options, "clipTail", true)}
          onChange={(v) => set({ clipTail: v })}
        />
      </>
    ),
  },
  cdf: {
    kind: "cdf",
    title: "Error CDF",
    description: "Share of epochs within a given error, with 50% and 95%",
    minEpochs: 3,
    Body: ({ series, options }) => (
      <CdfChart
        series={series}
        metrics={list<ErrorMetric>(options, "metrics", METRIC_KEYS, ["h", "v", "3d"])}
        clipTail={bool(options, "clipTail", true)}
      />
    ),
    Options: ({ options, set }) => (
      <>
        <Chips
          label="Show"
          items={ERROR_METRICS.map((m) => ({ key: m.key, label: m.label, color: metricCssColor(m.key) }))}
          value={list<ErrorMetric>(options, "metrics", METRIC_KEYS, ["h", "v", "3d"])}
          onChange={(v) => set({ metrics: v })}
        />
        <Toggle
          label="Clip at 99th percentile"
          title="Keep one outlier from squashing the rest of the curve"
          checked={bool(options, "clipTail", true)}
          onChange={(v) => set({ clipTail: v })}
        />
      </>
    ),
  },
  summary: {
    kind: "summary",
    title: "Summary",
    description: "Headline accuracy numbers and convergence time",
    minEpochs: 1,
    Body: SummaryPanel,
    Options: ({ options, set }) => (
      <Choice
        label="Convergence at"
        items={THRESHOLDS.map((t) => ({ key: t, label: `σH ≤ ${Number(t) * 100} cm` }))}
        value={one(options, "threshold", THRESHOLDS, "0.1")}
        onChange={(v) => set({ threshold: v })}
      />
    ),
  },
  stats: {
    kind: "stats",
    title: "Statistics",
    description: "Per-component mean, spread, RMS and range",
    minEpochs: 1,
    Body: ({ series }) => <StatsPanel series={series} />,
  },
};
