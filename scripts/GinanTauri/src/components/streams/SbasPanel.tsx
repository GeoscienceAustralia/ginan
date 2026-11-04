import type { SbasProbe } from "../../types";
import type { SbasView } from "../../types/streams";
import { CheckIcon, CloseIcon } from "../icons";
import Sparkline from "./Sparkline";
import { fmtAgo, fmtCount, fmtPerSec } from "./format";

/**
 * SouthPAN over SISNet. PEA reports this stream nowhere the other cards read
 * from, so the card has three sources of its own (see `sbas_monitor.rs`):
 * the stream test's own SISNet login, PEA's SISNet log lines during the run,
 * and the protection levels in the run's `.SPP`.
 */

/** SBAS message types by number: L1 (RTCA DO-229) and L5 DFMC (ED-259). */
const MESSAGE_NAMES: Record<number, string> = {
  0: "Do not use",
  1: "PRN mask",
  2: "Fast corrections",
  3: "Fast corrections",
  4: "Fast corrections",
  5: "Fast corrections",
  6: "Integrity",
  7: "Fast degradation",
  9: "GEO navigation",
  10: "Degradation",
  12: "SBAS time",
  17: "GEO almanac",
  18: "Iono grid mask",
  24: "Mixed corrections",
  25: "Long-term corrections",
  26: "Iono delays",
  27: "Service message",
  28: "Clock-ephemeris covariance",
  31: "Satellite mask",
  32: "Clock-ephemeris corrections",
  34: "Integrity",
  35: "Integrity",
  36: "Integrity",
  37: "OBAD and DFRE",
  39: "SBAS satellite ephemeris",
  40: "SBAS satellite ephemeris",
  42: "Time offsets",
  47: "SBAS almanac",
  62: "Test",
  63: "Null",
};

const PROBE_STATE: Record<SbasProbe["state"], { label: string; ok: boolean }> = {
  ok: { label: "Receiving", ok: true },
  no_login: { label: "No login", ok: false },
  unreachable: { label: "Unreachable", ok: false },
  auth_failed: { label: "Login rejected", ok: false },
  start_failed: { label: "Did not start", ok: false },
  no_data: { label: "No messages", ok: false },
};

function ProbeResult({ probe }: { probe: SbasProbe }) {
  const st = PROBE_STATE[probe.state];
  const types = Object.entries(probe.by_type).sort(([a], [b]) => Number(a) - Number(b));
  return (
    <div className="sbas-block" aria-label="Last stream test">
      <div className="streams-panel-head">
        <h3>Last stream test</h3>
        <span className={`result-badge ${st.ok ? "is-ok" : "is-bad"}`}>
          {st.ok ? <CheckIcon size={12} /> : <CloseIcon size={12} />}
          {st.label}
        </span>
      </div>
      {probe.error && <p className="streams-note">{probe.error}</p>}
      <div className="result-tiles">
        <div className="result-tile"><span>Messages/s</span><span className="mono">{fmtPerSec(probe.messages_per_s)}</span></div>
        <div className="result-tile"><span>Messages</span><span className="mono">{fmtCount(probe.messages)} in {Math.round(probe.seconds)} s</span></div>
        <div className="result-tile"><span>CRC errors</span><span className="mono">{fmtCount(probe.crc_errors)}</span></div>
        <div className="result-tile"><span>Unreadable lines</span><span className="mono">{fmtCount(probe.invalid_lines)}</span></div>
      </div>
      {types.length > 0 && (
        <div className="stream-chips" aria-label="Message types received">
          <span className="stream-chips-label">Message types</span>
          {types.map(([t, n]) => (
            <span className="stream-chip" key={t} title={MESSAGE_NAMES[Number(t)] ?? "Unknown type"}>
              MT{t} {MESSAGE_NAMES[Number(t)] ? <span>{MESSAGE_NAMES[Number(t)]}</span> : null}
              <span className="count">×{n}</span>
            </span>
          ))}
        </div>
      )}
    </div>
  );
}

function RunResult({ live, nowMs }: { live: NonNullable<SbasView["live"]>; nowMs: number }) {
  const c = live.console;
  const points = live.levels?.points ?? [];
  const last = points[points.length - 1];
  return (
    <div className="sbas-block" aria-label="This run">
      <div className="streams-panel-head">
        <h3>This run</h3>
        <span className="streams-muted">from PEA's SISNet log{live.levels ? " and its .SPP" : ""}</span>
      </div>
      <div className="result-tiles">
        <div className="result-tile"><span>Login failures</span><span className={`mono${c.auth_failures ? " is-bad" : ""}`}>{fmtCount(c.auth_failures)}</span></div>
        <div className="result-tile"><span>CRC errors</span><span className={`mono${c.crc_errors ? " is-warn" : ""}`}>{fmtCount(c.crc_errors)}</span></div>
        <div className="result-tile"><span>Unreadable lines</span><span className={`mono${c.invalid_lines ? " is-warn" : ""}`}>{fmtCount(c.invalid_lines)}</span></div>
        <div className="result-tile">
          <span>Last problem</span>
          <span>{c.last_error ? `${c.last_error}, ${fmtAgo(nowMs - (c.last_error_at_ms ?? nowMs))}` : "None"}</span>
        </div>
      </div>
      {live.levels && points.length > 0 ? (
        <div className="sbas-levels">
          <div className="result-tiles">
            <div className="result-tile"><span>HPL</span><span className="mono">{last[1].toFixed(2)} m</span></div>
            <div className="result-tile"><span>VPL</span><span className="mono">{last[2].toFixed(2)} m</span></div>
          </div>
          <Sparkline
            values={points.map((p) => p[1])}
            tone="ok"
            label={`Horizontal protection level, last ${points.length} epochs: now ${last[1].toFixed(2)} m`}
          />
        </div>
      ) : (
        <p className="streams-note">
          {live.mode === "PVS"
            ? "PVS mode writes a .POS, not an .SPP, so PEA reports no protection levels here."
            : "Protection levels appear once PEA writes its first .SPP epoch."}
        </p>
      )}
    </div>
  );
}

export default function SbasPanel({ sbas, nowMs = Date.now() }: { sbas: SbasView; nowMs?: number }) {
  return (
    <section className="streams-panel sbas-panel" aria-labelledby="h-sbas">
      <div className="streams-panel-head">
        <h2 id="h-sbas">SBAS</h2>
        <span className="streams-muted">SouthPAN over SISNet · PRN 122 · {sbas.mode}</span>
      </div>
      {sbas.probe ? (
        <ProbeResult probe={sbas.probe} />
      ) : (
        <p className="streams-note">Press Monitor stream to log in to SISNet and count what it sends for about 10 s.</p>
      )}
      {sbas.live && <RunResult live={sbas.live} nowMs={nowMs} />}
      <p className="streams-note">
        PEA does not report SISNet like the NTRIP streams, so the test logs in on its own, and a run is followed from
        PEA's log and protection levels.
      </p>
    </section>
  );
}
