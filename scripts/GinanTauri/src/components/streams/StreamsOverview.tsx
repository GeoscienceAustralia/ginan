import { useEffect, useState, type ReactNode } from "react";
import type { StreamsModel } from "../../types/streams";
import { ResetIcon } from "../icons";
import KpiTiles from "./KpiTiles";
import RecordsMix from "./RecordsMix";
import StreamCard, { STATUS } from "./StreamCard";
import StreamTestCard from "./StreamTestCard";
import SbasPanel from "./SbasPanel";
import { fmtClock } from "./format";

export type SubTab = "messages" | "satellites" | "station";

/** Seconds since `baseS` was received, ticking locally every second while `active`. */
export function useNow(active: boolean): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!active) return;
    const id = setInterval(() => setNow(Date.now()), 1000);
    return () => clearInterval(id);
  }, [active]);
  return now;
}

export function useTicking(baseS: number | null, active: boolean): number | null {
  const now = useNow(active);
  const [stamp, setStamp] = useState(() => Date.now());
  useEffect(() => setStamp(Date.now()), [baseS]);
  if (baseS == null) return null;
  return active ? baseS + Math.max(0, now - stamp) / 1000 : baseS;
}

export function casterSubtitle(model: StreamsModel): ReactNode {
  const hosts = model.casterHosts;
  const n = model.streams.length;
  if (hosts.length > 1) return <>{n} mountpoints on {hosts.length} casters</>;
  const host = hosts[0];
  return (
    <>
      {n} mountpoints{host && <> on <span className="mono" style={{ color: "var(--text)" }}>{host}</span></>}
    </>
  );
}

export function StreamsHeader({
  title = "Streams",
  pill,
  live,
  subtitle,
  children,
}: {
  title?: string;
  pill: ReactNode;
  live?: boolean;
  subtitle: ReactNode;
  children?: ReactNode;
}) {
  return (
    <div className="streams-head">
      <div className="streams-head-text">
        <div className="streams-title-row">
          <h1>{title}</h1>
          <span className={`streams-pill${live ? " is-live" : ""}`}>
            {live && <span className="streams-pill-dot" aria-hidden="true" />}
            {pill}
          </span>
        </div>
        <span className="streams-muted">{subtitle}</span>
      </div>
      {children}
    </div>
  );
}

/** "Monitor stream" button; handles running (busy, seconds) and failed ("Monitor again") labels. */
export function TestButton({ model, secondsRun }: { model: StreamsModel; secondsRun?: number | null }) {
  const running = model.test.status === "running";
  const label = running ? "Monitoring..." : model.test.status === "failed" ? "Monitor again" : "Monitor stream";
  return (
    <button
      className="btn-primary"
      onClick={() => void model.runStreamTest()}
      disabled={running || model.phase === "file-mode" || (model.isProcessing && !running)}
      aria-busy={running || undefined}
      title={model.phase === "file-mode" ? "Only for realtime runs" : "Watch each mountpoint for about 10 s"}
    >
      {label}
      {running && secondsRun != null && <span className="mono"> {Math.floor(secondsRun)} s</span>}
    </button>
  );
}

export default function StreamsOverview({
  model,
  onOpen,
  onReset,
}: {
  model: StreamsModel;
  onOpen: (mount: string, sub: SubTab) => void;
  onReset: () => void;
}) {
  const stopped = model.phase === "stopped";
  const uptime = useTicking(model.runtimeS, !stopped);
  const counts = model.streams.reduce<Record<string, number>>((acc, s) => {
    const t = STATUS[s.status].label.replace("...", "").toLowerCase();
    acc[t] = (acc[t] ?? 0) + 1;
    return acc;
  }, {});
  const summary = Object.entries(counts).map(([k, v]) => `${v} ${k}`).join(" · ");

  return (
    <>
      <StreamsHeader
        live={!stopped}
        pill={stopped ? "STOPPED" : <>LIVE · <span className="mono">{fmtClock(uptime)}</span></>}
        subtitle={<>{casterSubtitle(model)} · stats every epoch · times in GPST</>}
      >
        <button className="btn-secondary" onClick={onReset} disabled={stopped}>
          <ResetIcon size={14} />Reset counters
        </button>
        <TestButton model={model} />
      </StreamsHeader>

      {stopped && (
        <div className="streams-banner is-warn" role="status">
          <span className="streams-banner-stopped">The run has stopped. Values are frozen at the last update.</span>
        </div>
      )}

      <KpiTiles model={model} />

      <section className="streams-section" aria-labelledby="h-conn">
        <div className="streams-section-head">
          <h2 id="h-conn">Connections</h2>
          <span className="streams-muted">{summary}</span>
        </div>
        <div className="streams-cards">
          {model.streams.map((s) => (
            <StreamCard key={s.mount} stream={s} onOpen={(sub) => onOpen(s.mount, sub)} />
          ))}
        </div>
      </section>

      {model.sbas && <SbasPanel sbas={model.sbas} />}

      <div className="streams-bottom">
        <RecordsMix model={model} />
        <StreamTestCard test={model.test} />
      </div>
    </>
  );
}
