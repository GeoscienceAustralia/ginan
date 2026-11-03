import type { ReactNode } from "react";
import type { StreamsModel } from "../../types/streams";
import { CheckIcon, CloseIcon, InfoIcon, ResetIcon } from "../icons";
import KpiTiles from "./KpiTiles";
import RecordsMix from "./RecordsMix";
import StreamCard, { ROLE_NAME, STATUS, StatusMark } from "./StreamCard";
import { TestTiles } from "./StreamTestCard";
import SbasPanel from "./SbasPanel";
import { StreamsHeader, TestButton, casterSubtitle, useNow, useTicking } from "./StreamsOverview";
import { fmtClock, fmtTimeOfDay } from "./format";

export type NavTarget = "workflows" | "visualization";
interface StateProps { model: StreamsModel; onNavigate?: (tab: NavTarget) => void }

/** file-mode and idle. */
export function EmptyState({ model, onNavigate }: StateProps) {
  const file = model.phase === "file-mode";
  return (
    <>
      <StreamsHeader
        pill={file ? "FILE RUN" : "IDLE"}
        subtitle={file ? "Workflow input is observation files" : "No mountpoints connected"}
      >
        <TestButton model={model} />
      </StreamsHeader>
      <section className="streams-empty" aria-labelledby="h-empty">
        <InfoIcon size={40} />
        <h2 id="h-empty">{file ? "No streams to monitor" : "No live run yet"}</h2>
        <p>
          {file
            ? "This run reads observation files, so there are no NTRIP streams. Satellites in the solution are still shown from the .TRACE."
            : "Press Process to start a real-time run. Monitor stream, above, checks the mountpoints for about 10 s first."}
        </p>
        {file && (
          <button className="btn-secondary" onClick={() => onNavigate?.("visualization")}>
            See satellites in solution
          </button>
        )}
      </section>
      {!file && (
        <section className="streams-explain" aria-labelledby="h-what">
          <h2 id="h-what" className="streams-eyebrow">What you'll see here during a live run</h2>
          <div><b>Connections</b><span className="streams-muted">Status, throughput and the last 5 minutes for every mountpoint.</span></div>
          <div><b>Messages</b><span className="streams-muted">Each RTCM message type, how often it arrives and when it is late.</span></div>
          <div><b>Satellites</b><span className="streams-muted">C/N₀ per satellite and which ones made it into the solution.</span></div>
        </section>
      )}
      {!file && model.sbas && <SbasPanel sbas={model.sbas} />}
    </>
  );
}

/** Live run with no decoded data yet (connecting, or decoded recording off). */
export function WaitingState({ model, onNavigate }: StateProps) {
  const uptime = useTicking(model.runtimeS, true);
  const connected = model.streams.filter((s) => s.status === "streaming" || s.status === "receiving").length;
  const connecting = model.streams.filter((s) => s.status === "connecting").length;
  return (
    <>
      <StreamsHeader
        live
        pill={<>LIVE · <span className="mono">{fmtClock(uptime)}</span></>}
        subtitle={<>Waiting for PEA to connect to <span className="mono" style={{ color: "var(--text)" }}>{model.casterHosts[0] ?? "the caster"}</span>...</>}
      >
        <button className="btn-secondary" disabled><ResetIcon size={14} />Reset counters</button>
      </StreamsHeader>
      {model.recordDecodedOff && (
        <div className="streams-banner" role="status">
          <InfoIcon size={16} />
          <span>Decoded records are off, so message types, counts and C/N₀ aren't available. Connection stats still work.</span>
          <button className="btn-ghost" onClick={() => onNavigate?.("workflows")}>Recordings › Decoded RTCM (JSON)</button>
        </div>
      )}
      <KpiTiles model={model} />
      <section className="streams-section" aria-labelledby="h-conn">
        <div className="streams-section-head">
          <h2 id="h-conn">Connections</h2>
          <span className="streams-muted">{connected} connected · {connecting} connecting</span>
        </div>
        <div className="streams-cards">
          {model.streams.map((s) => <StreamCard key={s.mount} stream={s} waiting />)}
        </div>
      </section>
      {model.sbas && <SbasPanel sbas={model.sbas} />}
      <RecordsMix model={model} />
    </>
  );
}

function TestRow({ role, mount, state, ink, reason, count, dot }: {
  role: string | null; mount: string; state: ReactNode; ink: "ok" | "bad" | "neutral"; reason?: string; count: string; dot?: ReactNode;
}) {
  return (
    <li className="test-row">
      <span className="role-chip" title={role ? ROLE_NAME[role as keyof typeof ROLE_NAME] : undefined}>{role ?? "?"}</span>
      <span className="mono" style={{ fontWeight: 600 }}>{mount}</span>
      <span className="test-row-state">
        <span className={`state${ink === "neutral" ? "" : ` is-${ink}`}`}>{dot}{state}</span>
        {reason && <span className="streams-muted">{reason}</span>}
      </span>
      <span className="mono count">{count}</span>
    </li>
  );
}

export function TestRunningState({ model }: StateProps) {
  const now = useNow(true);
  const elapsed = model.test.atMs ? Math.min(10, Math.max(0, (now - model.test.atMs) / 1000)) : 0;
  const n = model.streams.length;
  return (
    <>
      <StreamsHeader pill="TESTING" subtitle={casterSubtitle(model)}>
        <TestButton model={model} secondsRun={elapsed} />
      </StreamsHeader>
      <section className="streams-panel" aria-labelledby="h-test">
        <div className="streams-panel-head">
          <h2 id="h-test">Stream test</h2>
          <span role="status" className="streams-muted">Watching {n} mountpoints for about 10 s...</span>
          <span style={{ flex: 1 }} />
          <span className="mono streams-muted">{Math.floor(elapsed)} / 10 s</span>
        </div>
        <div className="test-progress" role="progressbar" aria-label="Stream test progress" aria-valuemin={0} aria-valuemax={10} aria-valuenow={Math.floor(elapsed)}>
          <span style={{ width: `${elapsed * 10}%` }} />
        </div>
        <ul className="test-rows">
          {model.streams.map((s) => {
            const ok = s.status === "streaming" || s.status === "receiving";
            return (
              <TestRow key={s.mount} role={s.role} mount={s.mount}
                ink={ok ? "ok" : "neutral"} state={ok ? "Receiving" : "Connecting..."}
                dot={<StatusMark tone={ok ? "ok" : "neutral"} />}
                count={s.records ? `${s.records} messages` : "–"} />
            );
          })}
        </ul>
        <p className="streams-note">Start and Reset are paused until the test finishes. The result fills the constellations, codes and epoch interval in Workflow › Constellations.</p>
      </section>
    </>
  );
}

export function TestFailedState({ model }: StateProps) {
  const { test, streams } = model;
  const bad = streams.filter((s) => s.status === "failed").length;
  return (
    <>
      <StreamsHeader pill="IDLE" subtitle={casterSubtitle(model)}>
        <TestButton model={model} />
      </StreamsHeader>
      <section className="streams-panel is-bad" role="alert" aria-labelledby="h-test">
        <div className="streams-panel-head">
          <h2 id="h-test">Stream test failed</h2>
          {test.atMs != null && <span className="streams-muted">{fmtTimeOfDay(test.atMs)} · 10 s watch</span>}
          <span style={{ flex: 1 }} />
          <span className="result-badge is-bad"><CloseIcon size={12} />Failed</span>
        </div>
        <p style={{ margin: 0, fontSize: 14 }}>
          {bad > 0 && `${bad} of ${streams.length} mountpoints didn't deliver data. `}
          {test.error && `${test.error} `}
          The run can't start until every mountpoint passes, or you remove the failing ones in Workflow.
        </p>
        <ul className="test-rows">
          {streams.map((s) => {
            const ok = s.status !== "failed";
            return (
              <TestRow key={s.mount} role={s.role} mount={s.mount} ink={ok ? "ok" : "bad"}
                dot={ok ? <CheckIcon size={13} /> : <StatusMark tone="bad" />}
                state={ok ? "Passed" : s.lastError ?? "No data"}
                reason={ok ? s.protocol ?? undefined : s.httpStatus === 401 ? "Caster rejected the username or password. Check the caster credentials in Settings." : "Connected but no RTCM arrived in 10 s."}
                count={s.records ? `${s.records} messages` : "–"} />
            );
          })}
        </ul>
      </section>
      {test.result && (
        <section className="streams-panel" aria-labelledby="h-res">
          <h2 id="h-res">What the test found</h2>
          <TestTiles result={test.result} />
          <p className="streams-note">Workflow › Constellations is not updated from a failed test.</p>
        </section>
      )}
    </>
  );
}

export { STATUS };
