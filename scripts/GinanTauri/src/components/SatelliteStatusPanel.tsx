import { useCallback, useEffect, useMemo, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { resultsDir, useAppStore } from "../store/appStore";
import type { SatelliteStatus, SatelliteSummary } from "../types";
import { formatEpoch, formatResidual } from "./chartUtils";
import { ResetIcon } from "./icons";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * Which satellites the solution is actually using, grouped by constellation.
 *
 * Derived from the `.TRACE` rather than the NTRIP stats file: that file only
 * has byte counts and connect/disconnect events, so it can say a stream is
 * alive but not which spacecraft the filter took an observation from. The `.TRACE` can: a residual row is a measurement the
 * filter used, tagged with its satellite, so the set of satellites at the
 * latest epoch is the set currently contributing.
 *
 * Sits under the stream table because that is where a live run is watched, and
 * it answers the question the byte counters can't: the caster is delivering,
 * but is anything usable coming out of it?
 *
 * Nothing here is an error state. Trace output is a config toggle, and a run
 * that has just started has no `.TRACE` yet, so the backend reports "nothing
 * to show" with a reason and the panel prints it.
 */

/**
 * Poll cadence while a run is going.
 *
 * Re-reading the trace is the only way to notice a satellite that has dropped
 * out — nothing is emitted when one stops appearing — but each poll parses the
 * whole file, so this matches the residual view's 5 s rather than the position
 * view's 2 s. A satellite list is not a number that needs to tick.
 */
const POLL_MS = 5000;

export default function SatelliteStatusPanel() {
  const timeZone = useTimeZone();
  const outputDir = useAppStore(resultsDir);
  // Bumped when a run finishes, so the panel settles on the final state
  // without the user pressing anything.
  const posEpoch = useAppStore((s) => s.posEpoch);
  const isProcessing = useAppStore((s) => s.isProcessing);

  const [status, setStatus] = useState<SatelliteStatus | null>(null);
  const [error, setError] = useState<string | null>(null);
  /** Bumped by the poll timer and the Refresh button. */
  const [tick, setTick] = useState(0);

  const refresh = useCallback(() => setTick((n) => n + 1), []);

  useEffect(() => {
    let cancelled = false;
    invoke<SatelliteStatus>("load_satellite_status", { outputDir: outputDir ?? "", path: null })
      .then((s) => {
        if (cancelled) return;
        setStatus(s);
        setError(null);
      })
      .catch((e) => {
        if (cancelled) return;
        // Only a genuinely unreadable file lands here; "no trace yet" comes
        // back as a successful result with a message.
        setError(String(e));
      });
    return () => {
      cancelled = true;
    };
  }, [outputDir, posEpoch, tick]);

  useEffect(() => {
    if (!isProcessing) return;
    const id = setInterval(refresh, POLL_MS);
    return () => clearInterval(id);
  }, [isProcessing, refresh]);

  const totals = useMemo(() => {
    if (!status?.available) return null;
    const active = status.constellations.reduce(
      (n, c) => n + c.satellites.filter((s) => isActive(s, status)).length,
      0,
    );
    return { active, total: status.satellite_count };
  }, [status]);

  return (
    <div className="sat-status">
      <div className="sat-status-head">
        <h4>Satellites</h4>
        {totals && (
          <span className="sat-status-count">
            {totals.active} contributing of {totals.total} seen
          </span>
        )}
        <button className="btn-ghost sat-status-refresh" onClick={refresh} title="Re-read the trace file">
          <ResetIcon size={13} /> Refresh
        </button>
      </div>

      {error ? (
        <p className="sat-status-empty">{error}</p>
      ) : !status ? (
        <p className="sat-status-empty">Reading the trace file...</p>
      ) : !status.available ? (
        <p className="sat-status-empty">{status.message}</p>
      ) : (
        <>
          {status.constellations.map((c) => {
            const active = c.satellites.filter((s) => isActive(s, status)).length;
            return (
              <section key={c.constellation} className="sat-group">
                <h5>
                  {CONSTELLATION_NAMES[c.constellation] ?? c.constellation}
                  <span className="sat-group-count">
                    {active}/{c.satellites.length}
                  </span>
                </h5>
                <table className="sat-table">
                  <thead>
                    <tr>
                      <th scope="col">Satellite</th>
                      <th scope="col">Signals</th>
                      <th scope="col" className="sat-num">
                        Postfit RMS
                      </th>
                      <th scope="col" className="sat-num">
                        Samples
                      </th>
                      <th scope="col">Last epoch</th>
                    </tr>
                  </thead>
                  <tbody>
                    {c.satellites.map((s) => {
                      const contributing = isActive(s, status);
                      return (
                        <tr key={s.sat} className={contributing ? "" : "sat-row--dropped"}>
                          <th scope="row">
                            {/* State is spelled out in the row's title and in
                                the "Last epoch" column; the dot is a shortcut,
                                not the only way to read it. */}
                            <span
                              className={`sat-dot${contributing ? " is-active" : ""}`}
                              aria-hidden="true"
                            />
                            {s.sat}
                          </th>
                          <td>{s.signals.join(", ") || "-"}</td>
                          <td className="sat-num">{formatResidual(s.postfit_rms)}</td>
                          <td className="sat-num">{s.samples.toLocaleString()}</td>
                          <td title={contributing ? "Still contributing" : "Dropped out"}>
                            {formatEpoch(s.last_ms, timeZone, { zone: false })}
                            {contributing ? "" : " · dropped"}
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </section>
            );
          })}

          <p className="sat-status-meta">
            {status.name} · {status.epochs.toLocaleString()} epochs to{" "}
            {formatEpoch(status.t_end, timeZone)}
            {status.receivers.length > 0 ? ` · ${status.receivers.join(", ")}` : ""}
          </p>
        </>
      )}
    </div>
  );
}

/**
 * A satellite is contributing if it has a residual at the most recent epoch in
 * the file. Anything earlier means the filter stopped using it — set below the
 * elevation mask, lost lock, or rejected — which is worth showing as a state
 * rather than hiding by dropping the row.
 */
function isActive(sat: SatelliteSummary, status: SatelliteStatus): boolean {
  return sat.last_ms >= status.t_end;
}

/** Full names for the constellation headings, matching ConstellationFilter's
 *  wording. `.TRACE` can also carry systems that the sidebar doesn't offer. */
const CONSTELLATION_NAMES: Record<string, string> = {
  GPS: "GPS",
  GAL: "Galileo",
  GLO: "GLONASS",
  BDS: "BeiDou",
  QZS: "QZSS",
  IRN: "NavIC",
  SBS: "SBAS",
  LEO: "LEO",
  Other: "Other",
};
