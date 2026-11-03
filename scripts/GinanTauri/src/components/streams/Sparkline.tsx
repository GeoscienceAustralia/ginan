export type SparkTone = "ok" | "warn" | "bad";

/** Throughput over the last 5 min (≤ 61 samples). Static: no animation to opt out of beyond the CSS transition. */
export default function Sparkline({ values, tone, label }: { values: number[]; tone: SparkTone; label: string }) {
  const flat = values.length < 2;
  const max = Math.max(1, ...values);
  const step = 300 / Math.max(1, values.length - 1);
  const pts = values.map((v, i) => `${(i * step).toFixed(1)},${(50 - (v / max) * 40).toFixed(1)}`);
  return (
    <div className={`sparkline is-${tone}${flat ? " is-flat" : ""}`}>
      {flat ? null : (
        <svg role="img" aria-label={label} viewBox="0 0 300 52" preserveAspectRatio="none">
          <path className="sparkline-area" d={`M0,52 L${pts.join(" L")} L300,52 Z`} />
          <polyline className="sparkline-line" points={pts.join(" ")} vectorEffect="non-scaling-stroke" />
        </svg>
      )}
      {flat ? <span className="sr-only">{label}</span> : <span className="sparkline-corner mono">5 min</span>}
    </div>
  );
}
