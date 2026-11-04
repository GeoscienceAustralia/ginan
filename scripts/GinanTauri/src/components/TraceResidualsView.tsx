import { useCallback, useEffect, useMemo, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { resultsDir, useAppStore } from "../store/appStore";
import type { TraceFileInfo, TraceIndex, TraceSeries } from "../types";
import { AlertIcon, ChevronLeftIcon, ChevronRightIcon, ResetIcon } from "./icons";
import TraceResidualChart from "./TraceResidualChart";
import VizEmpty, { shortName } from "./VizEmpty";
import { formatDuration, formatResidual } from "./chartUtils";
import { TabBarActions } from "./TabBarSlot";
import { traceFileLabel } from "./viz/files";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * The `.TRACE` half of the Visualization tab.
 *
 * The `.pos` view answers "where did the receiver end up"; this one answers
 * "on what". PEA writes one prefit/postfit residual per measurement per epoch
 * into the `.TRACE`, which is where a bad satellite, a bad signal or a
 * cycle slip shows up long before it is visible in the position.
 *
 * A run's `.TRACE` holds every satellite at once, which is far more than one
 * set of axes can carry legibly, so the view is one satellite at a time. The
 * first request for a file asks for its index only — which satellites,
 * receivers and epochs it holds — and the satellite with the most data is
 * drawn from there. The backend keeps the parsed file, so every later request
 * (another satellite, or a poll while PEA is still writing) only costs the
 * bytes appended since.
 */

/**
 * How often to re-read the trace while a realtime run is active.
 *
 * Slower than the position view's 2 s on purpose: a `.TRACE` holds every
 * measurement rather than one solution per epoch, so it grows an order of
 * magnitude faster.
 */
const REALTIME_POLL_MS = 5000;

/** Above this, the first read is slow enough to say so up front. */
const LARGE_TRACE_BYTES = 50 * 1024 * 1024;

export default function TraceResidualsView() {
  const outputDir = useAppStore(resultsDir);
  // File write times in the picker follow the time zone setting.
  const timeZone = useTimeZone();
  // Bumped when PEA finishes, so a run that has just written a new .TRACE
  // refreshes the list without the user having to press anything.
  const posEpoch = useAppStore((s) => s.posEpoch);
  const bumpPosEpoch = useAppStore((s) => s.bumpPosEpoch);
  const inputMode = useAppStore((s) => s.inputMode);
  const isProcessing = useAppStore((s) => s.isProcessing);

  const [files, setFiles] = useState<TraceFileInfo[] | null>(null);
  const [selected, setSelected] = useState<string | null>(null);
  const [index, setIndex] = useState<TraceIndex | null>(null);
  const [data, setData] = useState<TraceSeries | null>(null);
  const [sat, setSat] = useState<string | null>(null);
  const [receiver, setReceiver] = useState<string | null>(null);
  const [listError, setListError] = useState<string | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  // Off by default: a phase prefit carries the float ambiguity, metres against
  // the postfit's millimetres, so drawing it squashes the postfit flat.
  const [showPrefit, setShowPrefit] = useState(false);

  /** Files that have residual rows; the rest are only PEA's config header. */
  const usable = useMemo(() => (files ?? []).filter((f) => f.has_residuals), [files]);

  const refresh = useCallback(async () => {
    if (!outputDir) {
      setFiles(null);
      setListError(null);
      return;
    }
    try {
      const found = await invoke<TraceFileInfo[]>("list_trace_files", { outputDir });
      setFiles(found);
      setListError(null);
      // Keep the current selection across a refresh when it is still there —
      // re-running the same config rewrites the same filenames. The list is
      // newest first, so the fallback is the run just produced.
      setSelected((prev) => {
        const withRows = found.filter((f) => f.has_residuals);
        if (prev && withRows.some((f) => f.path === prev)) return prev;
        return withRows[0]?.path ?? null;
      });
    } catch (e) {
      setFiles(null);
      setListError(String(e));
    }
  }, [outputDir]);

  useEffect(() => {
    refresh();
  }, [refresh, posEpoch]);

  // Realtime streams indefinitely and never fires pea-finished until the user
  // stops it, so nothing else nudges posEpoch while it's running.
  useEffect(() => {
    if (inputMode !== "realtime" || !isProcessing) return;
    const id = setInterval(bumpPosEpoch, REALTIME_POLL_MS);
    return () => clearInterval(id);
  }, [inputMode, isProcessing, bumpPosEpoch]);

  // A different file is a different run; drop everything read from the last
  // one rather than asking for a satellite the new file may not carry.
  useEffect(() => {
    setSat(null);
    setReceiver(null);
    setIndex(null);
    setData(null);
    setLoadError(null);
  }, [selected]);

  // No satellite yet: read the index alone and pick one from it. On a large
  // file this is the slow read; everything after it is served from the
  // backend's copy.
  const needIndex = !!selected && sat === null;
  useEffect(() => {
    if (!needIndex || !selected) return;
    let cancelled = false;
    setLoading(true);
    invoke<TraceSeries>("load_trace_series", { path: selected, sats: [], receiver: null, maxPoints: null })
      .then((s) => {
        if (cancelled) return;
        setIndex(s.index);
        setLoadError(null);
        setSat(defaultSatellite(s.index));
      })
      .catch((e) => {
        if (cancelled) return;
        setLoadError(String(e));
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => {
      cancelled = true;
    };
    // posEpoch: a file that had no satellites yet is asked again as it grows.
  }, [needIndex, selected, posEpoch]);

  useEffect(() => {
    if (!selected || !sat) return;
    let cancelled = false;
    setLoading(true);
    invoke<TraceSeries>("load_trace_series", { path: selected, sats: [sat], receiver, maxPoints: null })
      .then((s) => {
        if (cancelled) return;
        setData(s);
        setIndex(s.index);
        setLoadError(null);
      })
      .catch((e) => {
        if (cancelled) return;
        setData(null);
        setLoadError(String(e));
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => {
      cancelled = true;
    };
    // posEpoch is a dependency so a re-run of the same file reloads its contents.
  }, [selected, sat, receiver, posEpoch]);

  /** Only the chosen satellite's series. The backend already filtered, but a
   *  response from before a quick satellite change may still be in hand. */
  const drawn = useMemo(() => (data && sat ? data.series.filter((s) => s.sat === sat) : []), [data, sat]);

  const satIds = useMemo(() => (index ? index.satellites.map((s) => s.sat) : []), [index]);
  const satPos = sat ? satIds.indexOf(sat) : -1;
  const stepSat = useCallback(
    (by: number) => {
      if (satIds.length === 0) return;
      const next = satPos < 0 ? 0 : (satPos + by + satIds.length) % satIds.length;
      setSat(satIds[next]);
    },
    [satIds, satPos],
  );

  if (listError) {
    return <VizEmpty title="Could not read the output directory" body={listError} onRetry={refresh} />;
  }

  if (files && usable.length === 0) {
    return (
      <VizEmpty
        title={files.length === 0 ? "No .TRACE files yet" : "No residuals to plot yet"}
        body={
          files.length === 0
            ? `Nothing to plot in ${outputDir}. Turn on TRACE output in the sidebar and run a solution - the residual plots appear here as soon as it finishes.`
            : `The .TRACE ${files.length === 1 ? "file" : "files"} in ${outputDir} (${files
                .map((f) => f.name)
                .join(", ")}) ${files.length === 1 ? "holds" : "hold"} only PEA's configuration header, no residual rows. A run that is still starting, or one with residual output turned off, looks like this.`
        }
        onRetry={refresh}
      />
    );
  }

  const selectedInfo = usable.find((f) => f.path === selected);
  const satSummary = index?.satellites.find((s) => s.sat === sat);

  return (
    <>
      {/* On the tab bar's line, right of the tabs, like the position view's. */}
      <TabBarActions>
        <div className="viz-toolbar">
          <label className="viz-file" title={selected ?? "Trace file"}>
            <select
              aria-label="Trace file"
              value={selected ?? ""}
              onChange={(e) => setSelected(e.target.value)}
              disabled={usable.length === 0}
            >
              {usable.map((f) => (
                <option key={f.path} value={f.path}>
                  {traceFileLabel(f, timeZone)}
                </option>
              ))}
            </select>
          </label>

          <div className="viz-sat-picker" role="group" aria-label="Satellite">
            <button
              className="viz-tool-btn viz-icon-btn"
              onClick={() => stepSat(-1)}
              disabled={satIds.length < 2}
              title="Previous satellite"
              aria-label="Previous satellite"
            >
              <ChevronLeftIcon size={13} />
            </button>
            <label className="viz-file">
              <select
                aria-label="Satellite"
                value={sat ?? ""}
                onChange={(e) => setSat(e.target.value)}
                disabled={!index || index.satellites.length === 0}
              >
                {!sat && <option value="">Satellite...</option>}
                {groupByConstellation(index).map(([constellation, sats]) => (
                  <optgroup key={constellation} label={constellation}>
                    {sats.map((s) => (
                      <option key={s.sat} value={s.sat}>
                        {s.sat} · {s.signals.length} signal{s.signals.length === 1 ? "" : "s"}
                      </option>
                    ))}
                  </optgroup>
                ))}
              </select>
            </label>
            <button
              className="viz-tool-btn viz-icon-btn"
              onClick={() => stepSat(1)}
              disabled={satIds.length < 2}
              title="Next satellite"
              aria-label="Next satellite"
            >
              <ChevronRightIcon size={13} />
            </button>
          </div>

          {/* One receiver is the normal case for a user run; the selector only
              earns its space on a network solution that has several. */}
          {index && index.receivers.length > 1 && (
            <label className="viz-file">
              <select
                aria-label="Receiver"
                value={receiver ?? ""}
                onChange={(e) => setReceiver(e.target.value || null)}
              >
                <option value="">All receivers</option>
                {index.receivers.map((r) => (
                  <option key={r} value={r}>
                    {r}
                  </option>
                ))}
              </select>
            </label>
          )}

          <button
            className={`viz-tool-btn${showPrefit ? " is-on" : ""}`}
            aria-pressed={showPrefit}
            onClick={() => setShowPrefit((v) => !v)}
            title="Also draw the residual before the filter update, dashed. Phase prefits include the unresolved ambiguity, so they can be metres away from the postfit."
          >
            Prefit
          </button>

          <div className="viz-toolbar-right">
            <button
              className="viz-tool-btn viz-icon-btn"
              onClick={refresh}
              title="Re-scan the output directory"
              aria-label="Refresh"
            >
              <ResetIcon size={14} />
            </button>
          </div>
        </div>
      </TabBarActions>

      <div className="viz-body">
        {loadError ? (
          <div className="viz-state viz-state--error">
            <AlertIcon size={15} /> {loadError}
          </div>
        ) : drawn.length > 0 && data ? (
          <TraceResidualChart series={drawn} name={data.name} showPrefit={showPrefit} />
        ) : loading || (selected && !index) ? (
          <div className="viz-state">
            {index ? `Reading ${sat ?? "satellite"}...` : `Reading ${shortName(selected)}...`}
            {!index && selectedInfo && selectedInfo.size > LARGE_TRACE_BYTES && (
              <span className="viz-state-hint">
                {formatBytes(selectedInfo.size)} of residuals: the first read of a file this size takes a while.
                Switching satellites afterwards is quick.
              </span>
            )}
          </div>
        ) : index && index.satellites.length === 0 ? (
          <div className="viz-state">No satellite measurements in {shortName(selected)} yet.</div>
        ) : sat && data ? (
          <div className="viz-state">
            {sat} has no residuals{receiver ? ` at ${receiver}` : ""} in this file.
          </div>
        ) : (
          <div className="viz-state">Select a trace file.</div>
        )}
      </div>

      {data && drawn.length > 0 && (
        <>
          {/* The numbers behind the figure. Also the accessible fallback:
              every value the plot encodes with colour is readable here.
              Scrolls on its own so a satellite with many signals doesn't
              squeeze the chart to a strip. */}
          <div className="viz-trace-table">
          <table className="viz-stats">
            <thead>
              <tr>
                <th scope="col">Series</th>
                <th scope="col">Postfit mean</th>
                <th scope="col">Postfit RMS</th>
                <th scope="col">Prefit RMS</th>
                <th scope="col">Mean σ</th>
                <th scope="col">Samples</th>
              </tr>
            </thead>
            <tbody>
              {drawn.map((s) => (
                <tr key={s.key}>
                  <th scope="row">
                    {s.sat} · {s.signal || "-"} · {s.kind === "other" ? s.meas_type : s.kind}
                    {receiver === null && index && index.receivers.length > 1 ? ` · ${s.receiver}` : ""}
                  </th>
                  <td>{formatResidual(s.postfit_stats.mean)}</td>
                  <td>{formatResidual(s.postfit_stats.rms)}</td>
                  <td>{formatResidual(s.prefit_stats.rms)}</td>
                  <td>{formatResidual(s.postfit_stats.mean_sigma)}</td>
                  <td>{s.total_samples.toLocaleString()}</td>
                </tr>
              ))}
            </tbody>
          </table>
          </div>

          <div className="viz-meta">
            {index && (
              <span>
                {index.epochs.toLocaleString()} epochs over {formatDuration(index.t_end - index.t_start)} ·{" "}
                {index.satellites.length} satellites in file
              </span>
            )}
            {satSummary && satSummary.last_ms - satSummary.first_ms > 0 && (
              <span>
                {sat} tracked for {formatDuration(satSummary.last_ms - satSummary.first_ms)}
              </span>
            )}
            {data.decimated && (
              <span title="Statistics above are computed over every sample, not the drawn ones">
                Drawn from {data.returned_samples.toLocaleString()} sampled residuals
              </span>
            )}
            {data.skipped_lines > 0 && (
              <span className="viz-meta--warn">
                <AlertIcon size={12} /> {data.skipped_lines} unreadable line
                {data.skipped_lines === 1 ? "" : "s"} skipped
              </span>
            )}
          </div>
        </>
      )}
    </>
  );
}

/** The satellite to show first: the one with the most residuals, so the
 *  opening figure is a full-length track rather than a short pass. */
function defaultSatellite(index: TraceIndex): string | null {
  let best: TraceIndex["satellites"][number] | null = null;
  for (const s of index.satellites) if (!best || s.samples > best.samples) best = s;
  return best?.sat ?? null;
}

/** Satellites in `<optgroup>` order — the index already sorts by constellation
 *  then id, so this only has to break the run into groups. */
function groupByConstellation(index: TraceIndex | null): [string, TraceIndex["satellites"]][] {
  if (!index) return [];
  const groups: [string, TraceIndex["satellites"]][] = [];
  for (const s of index.satellites) {
    const last = groups[groups.length - 1];
    if (last && last[0] === s.constellation) last[1].push(s);
    else groups.push([s.constellation, [s]]);
  }
  return groups;
}

function formatBytes(n: number): string {
  if (n < 1024 * 1024) return `${Math.round(n / 1024)} KB`;
  return `${Math.round(n / (1024 * 1024))} MB`;
}
