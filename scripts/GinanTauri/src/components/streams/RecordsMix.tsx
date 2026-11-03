import { useState } from "react";
import type { StreamsModel } from "../../types/streams";
import { fmtCount, fmtPerSec } from "./format";

export default function RecordsMix({ model }: { model: StreamsModel }) {
  const { mix, kpis } = model;
  const total = mix.reduce((n, m) => n + m.count, 0);
  const [hover, setHover] = useState<number | null>(null);
  // Centre of the hovered segment, as a share of the bar, to anchor the tooltip.
  const tipAt = hover == null || !total
    ? 0
    : (mix.slice(0, hover).reduce((n, m) => n + m.count, 0) + mix[hover].count / 2) / total;
  const label = `Decoded messages in the last 60 seconds: ${mix.map((m) => `${m.label} ${m.count}`).join(", ")}`;
  return (
    <section className="streams-panel" aria-labelledby="h-mix">
      <div className="streams-panel-head">
        <h2 id="h-mix">Decoded messages</h2>
        <span className="streams-muted">last 60 s · all streams</span>
        <span style={{ flex: 1 }} />
        <span className="mono streams-muted">{fmtCount(total)} total</span>
      </div>
      {model.recordDecodedOff ? (
        <p className="streams-note">
          Turn on Recordings › Decoded RTCM (JSON) and restart the run to see message types, counts and C/N₀.
        </p>
      ) : mix.length === 0 ? (
        <p className="streams-note">No decoded messages yet.</p>
      ) : (
        <>
          <div className="mix-bar-wrap">
            <div className="mix-bar" role="img" aria-label={label} onMouseLeave={() => setHover(null)}>
              {mix.map((m, i) => (
                <span
                  key={m.family + m.label}
                  className={`mix-${m.family}${hover === i ? " is-hover" : ""}`}
                  style={{ flex: `${m.count} 1 0` }}
                  onMouseEnter={() => setHover(i)}
                />
              ))}
            </div>
            {hover != null && (
              <div className="mix-tip" aria-hidden="true" style={{ left: `${(Math.min(0.88, Math.max(0.12, tipAt)) * 100).toFixed(2)}%` }}>
                <span>{mix[hover].label}</span>
                <span className="mono">{fmtCount(mix[hover].count)}</span>
                <span className="mono streams-muted">{fmtPerSec(mix[hover].ratePerS)}/s</span>
              </div>
            )}
          </div>
          <ul className="mix-legend">
            {mix.map((m) => (
              <li key={m.family + m.label}>
                <span className={`mix-swatch mix-${m.family}`} aria-hidden="true" />
                <span>{m.label}</span>
                <span style={{ flex: 1 }} />
                <span className="mono">{fmtCount(m.count)}</span>
                <span className="mono rate">{fmtPerSec(m.ratePerS)}/s</span>
              </li>
            ))}
          </ul>
          <span className="streams-note">
            One RTCM message = one count, whatever its satellites and signals · {kpis.malformed} malformed · {kpis.unknown} unknown
          </span>
        </>
      )}
    </section>
  );
}
