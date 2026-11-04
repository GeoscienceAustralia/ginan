import type { StreamRole, StreamStatus, StreamView } from "../../types/streams";
import { AlertIcon, CloseIcon, SatelliteIcon } from "../icons";
import Sparkline, { type SparkTone } from "./Sparkline";
import { DASH, fmtAgo, fmtBytes, fmtPerSec, fmtRate, joinUnit } from "./format";

type Tone = "ok" | "warn" | "bad" | "neutral";

export const STATUS: Record<StreamStatus, { label: string; tone: Tone }> = {
  streaming: { label: "Connected", tone: "ok" },
  receiving: { label: "Receiving", tone: "ok" },
  connecting: { label: "Connecting...", tone: "neutral" },
  reconnecting: { label: "Reconnecting", tone: "warn" },
  stale: { label: "Stale", tone: "warn" },
  failed: { label: "Failed", tone: "bad" },
  idle: { label: "Idle", tone: "neutral" },
};

export const ROLE_NAME: Record<StreamRole, string> = {
  OBS: "Observations",
  CORR: "Corrections",
  NAV: "Broadcast navigation",
};

/** Dot for fine states, a glyph for the ones that need more than colour. */
export function StatusMark({ tone }: { tone: Tone }) {
  if (tone === "warn") return <AlertIcon size={13} />;
  if (tone === "bad") return <CloseIcon size={13} />;
  const bg = tone === "ok" ? "var(--accent)" : "var(--text-dim)";
  return <span aria-hidden="true" style={{ width: 8, height: 8, borderRadius: "50%", background: bg }} />;
}

interface Stat { label: string; value: string; tone?: "warn" | "bad" }

function statsFor(s: StreamView, tone: Tone): Stat[] {
  const bad = tone === "warn" || tone === "bad" ? tone : undefined;
  const hasData = s.bytesTotal != null || s.records > 0;
  const base: Stat[] = [
    { label: "Throughput", value: hasData ? joinUnit(fmtRate(s.bytesPerSec)) : DASH, tone: bad },
    { label: "Received", value: joinUnit(fmtBytes(s.bytesTotal)) },
    { label: "Last received", value: fmtAgo(s.lastEventAgeMs), tone: bad },
  ];
  const invalid: Stat = { label: "Malformed", value: String(s.invalid) };
  if (s.role === "CORR") {
    const udi = Math.max(0, ...s.messages.filter((m) => m.group === "ssr").map((m) => m.periodicityS ?? 0));
    return [...base,
      { label: "SSR messages/s", value: fmtPerSec(s.windowRecords / 60) },
      { label: "Update interval", value: udi ? `${udi} s` : DASH },
      invalid];
  }
  if (s.role === "NAV") {
    return [...base,
      bad && s.lastError ? { label: "Last error", value: s.lastError, tone: bad } : { label: "State", value: s.retryCount ? `retry ${s.retryCount}` : STATUS[s.status].label },
      { label: "Ephemeris sats", value: String(s.satCount) },
      invalid];
  }
  return [...base,
    { label: "Epochs/s", value: s.epochsPerSec == null ? DASH : s.epochsPerSec.toFixed(1) },
    { label: "Avg C/N₀", value: s.avgSnr == null ? DASH : `${s.avgSnr.toFixed(1)} dB-Hz` },
    invalid];
}

const CHIP_LABEL: Record<StreamRole, string> = { OBS: "Satellites", CORR: "Satellites covered", NAV: "Ephemeris" };

export default function StreamCard({
  stream: s,
  waiting,
  onOpen,
}: {
  stream: StreamView;
  /** Waiting screen: show skeleton stats while no data has arrived. */
  waiting?: boolean;
  onOpen?: (sub: "messages" | "satellites" | "station") => void;
}) {
  const st = STATUS[s.status];
  const hid = `mp-${s.mount}`;
  const spark: SparkTone = st.tone === "warn" ? "warn" : st.tone === "bad" ? "bad" : "ok";
  const note = s.status === "reconnecting" ? `attempt ${s.retryCount}` : s.status === "failed" ? (s.lastError ?? "") : "";
  const hist = s.throughputHistory;
  const sparkLabel = hist.length
    ? `Throughput, last 5 minutes: now ${joinUnit(fmtRate(hist[hist.length - 1]))}`
    : "No throughput samples yet";
  const chips = Object.entries(s.satsByConstellation);

  return (
    <article className={`stream-card${st.tone === "warn" ? " is-warn" : st.tone === "bad" ? " is-failed" : ""}`} aria-labelledby={hid}>
      <div className="stream-card-top">
        <div className="stream-card-title">
          {s.role && <span className={`role-chip is-${s.role.toLowerCase()}`} title={ROLE_NAME[s.role]}>{s.role}</span>}
          <h3 id={hid} className="mono">{s.mount}</h3>
          <span className="stream-card-spacer" />
          <span className={`stream-status${st.tone === "neutral" ? "" : ` is-${st.tone}`}`}>
            <StatusMark tone={st.tone} />{st.label}
          </span>
        </div>
        <div className="stream-card-meta">
          <span><span className="stream-meta-label">Caster</span><span className="mono">{s.casterHost ?? DASH}</span></span>
          <span className={st.tone === "warn" ? "is-warn" : st.tone === "bad" ? "is-bad" : undefined}>
            {waiting ? (s.status === "connecting" ? "Opening connection" : "Connected · waiting for first data") : note}
          </span>
        </div>
        <Sparkline values={hist} tone={spark} label={sparkLabel} />
      </div>
      {waiting ? (
        <div className="stream-stats" aria-hidden="true">
          {[60, 50, 55].map((w) => (
            <span key={w} className="stream-stat"><span className="stream-skel" style={{ width: `${w}%` }} /><span className="stream-skel" style={{ width: "75%", height: 14 }} /></span>
          ))}
        </div>
      ) : (
        <>
          <dl className="stream-stats">
            {statsFor(s, st.tone).map((x) => (
              <div className="stream-stat" key={x.label}>
                <dt>{x.label}</dt>
                <dd className={`mono${x.tone ? ` is-${x.tone}` : ""}`} title={x.value}>{x.value}</dd>
              </div>
            ))}
          </dl>
          {chips.length > 0 && s.role && (
            <div className="stream-chips">
              <span className="stream-chip-label stream-chips-label">{CHIP_LABEL[s.role]}</span>
              {chips.map(([c, n]) => (
                <span className="stream-chip" key={c}>
                  <span aria-hidden="true" className={`stream-chip-swatch${s.role === "CORR" ? "" : ` ${c.toLowerCase()}`}`} />
                  {c}<span className="mono count">{n}</span>
                </span>
              ))}
            </div>
          )}
          {onOpen && (
            <div className="stream-actions">
              <button className="btn-secondary" onClick={() => onOpen("messages")} aria-label={`${s.mount} messages, ${s.messages.length} types`}>
                <svg aria-hidden="true" width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round"><path d="M2 4h2M6 4h8M2 8h2M6 8h8M2 12h2M6 12h8" /></svg>
                Messages<span className="mono count">{s.messages.length}</span>
              </button>
              <button className="btn-secondary" onClick={() => onOpen("satellites")} aria-label={`${s.mount} satellites, ${s.satCount}`}>
                <SatelliteIcon size={16} />Satellites<span className="mono count">{s.satCount}</span>
              </button>
              <button className="btn-secondary btn-station" onClick={() => onOpen("station")} aria-label={`${s.mount} station`}>Station</button>
            </div>
          )}
        </>
      )}
    </article>
  );
}
