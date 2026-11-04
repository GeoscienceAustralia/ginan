import { MODES } from "./ConfigPanel";
/**
 * The sidebar's per-section panels, split out of ConfigPanel to keep each
 * file under 500 lines. ConfigPanel owns the rail, resizing and which section
 * is open; these render what is inside one.
 */

import { useCallback, useEffect, useMemo } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useShallow } from "zustand/react/shallow";
import {
  defaultRunName,
  effectiveMode,
  sbasForcesDynamic,
  useAppStore,
  } from "../store/appStore";
import type { ProviderHealthStatus } from "../store/appStore";
import type { ProcessingMode } from "../types";
import { PPP_PROVIDERS, PPP_SERIES_LABELS, PPP_PROJECT_LABELS } from "../types";
import ConstellationFilter from "./ConstellationFilter";
import InfoTip from "./InfoTip";
import EpochPicker from "./EpochPicker";
import { TimeWindowNotes, TimeWindowTip, useFileWindow } from "./TimeWindowNotes";

export function GeneralTab() {
  const realtime = useAppStore((s) => s.inputMode === "realtime");
  const {
    mode, sbasOnly, setMode,
    startEpoch, setStartEpoch,
    endEpoch, setEndEpoch,
    epochInterval, setEpochInterval,
  } = useAppStore(
    useShallow((s) => ({
      mode: effectiveMode(s),
      sbasOnly: sbasForcesDynamic(s),
      setMode: s.setMode,
      startEpoch: s.startEpoch,
      setStartEpoch: s.setStartEpoch,
      endEpoch: s.endEpoch,
      setEndEpoch: s.setEndEpoch,
      epochInterval: s.epochInterval,
      setEpochInterval: s.setEpochInterval,
    })),
  );
  const fileWindow = useFileWindow();

  return (
    <div className="config-tab-content">
      {/* Processing Mode */}
      <div className="config-section">
        <h3>Processing Mode</h3>
        <div className="field-row">
          <select
            aria-label="Processing mode"
            value={mode}
            onChange={(e) => setMode(e.target.value as ProcessingMode)}
            title={sbasOnly ? "SBAS processes in Dynamic mode only. Remove the SBAS correction to choose another mode." : undefined}
          >
            {(sbasOnly ? (["Dynamic"] as const) : MODES).map((m) => (
              <option key={m} value={m}>{m}</option>
            ))}
          </select>
        </div>
      </div>

      {/* Time Window, in GPS time: PEA reads epoch_control as GPST, and the
          RINEX parser converts any other time system before seeding these.
          Unlike displayed times they don't follow the time zone setting, so
          say so. */}
      {/* A live stream has no window: the backend drops start and end epochs
          for a real-time run (config/generate.rs), so only the interval shows. */}
      {realtime ? (
        <div className="config-section">
          <h3>Epochs</h3>
          <div className="field-row">
            <label>Interval (seconds)</label>
            <input
              type="number"
              min={1}
              max={3600}
              value={epochInterval}
              onChange={(e) => setEpochInterval(Number(e.target.value))}
            />
          </div>
        </div>
      ) : (
      <div className="config-section">
        <h3 title="Epochs are in GPS time, as written to the PEA config; the time zone setting does not apply">Time Window (GPST)<TimeWindowTip /></h3>
        <EpochPicker
          id="epoch-start"
          label="Start Epoch"
          value={startEpoch}
          onChange={setStartEpoch}
          fileWindow={fileWindow}
        />
        <EpochPicker
          id="epoch-end"
          label="End Epoch"
          value={endEpoch}
          onChange={setEndEpoch}
          fileWindow={fileWindow}
        />
        <TimeWindowNotes fileWindow={fileWindow} />
        <div className="field-row">
          <label>Interval (seconds)</label>
          <input
            type="number"
            min={1}
            max={3600}
            value={epochInterval}
            onChange={(e) => setEpochInterval(Number(e.target.value))}
          />
        </div>
      </div>
      )}

      {/* RINEX metadata — editable */}
      <RinexMetaSummary />
    </div>
  );
}

export function ConstellationsTab() {
  return (
    <div className="config-tab-content">
      <ConstellationFilter />
    </div>
  );
}

/**
 * The PPP product source controls (provider, series, project and their help
 * text). Rendered inside the Corrections card (ConfigInputSource) in a file run.
 */
export function PppProductSource() {
  const pppSelection = useAppStore((s) => s.pppSelection);
  const setPppProvider = useAppStore((s) => s.setPppProvider);
  const setPppSeries = useAppStore((s) => s.setPppSeries);
  const setPppProject = useAppStore((s) => s.setPppProject);
  const pppCatalogue = useAppStore((s) => s.pppCatalogue);
  const rinexPath = useAppStore((s) => s.rinexPath);
  const getAvailableProviders = useAppStore((s) => s.getAvailableProviders);
  const getAvailableSeries = useAppStore((s) => s.getAvailableSeries);
  const getAvailableSeriesForProject = useAppStore((s) => s.getAvailableSeriesForProject);
  const getAvailableProjects = useAppStore((s) => s.getAvailableProjects);
  const getAvailableProjectsForProvider = useAppStore((s) => s.getAvailableProjectsForProvider);
  const providerHealth = useAppStore((s) => s.providerHealth);
  const setProviderHealth = useAppStore((s) => s.setProviderHealth);

  const HEALTH_TTL_SECONDS = 60;

  const refreshHealth = useCallback(async () => {
    const now = Math.floor(Date.now() / 1000);
    const stale = [...PPP_PROVIDERS].filter((p) => {
      const entry = providerHealth[p];
      return !entry || now - entry.checkedAt > HEALTH_TTL_SECONDS;
    });
    if (stale.length === 0) return;
    await Promise.all(
      stale.map(async (provider) => {
        try {
          const result = await invoke<{ status: "up" | "down" | "unknown"; checked_at: number }>(
            "check_ppp_provider_health",
            { provider },
          );
          setProviderHealth(provider, result.status, result.checked_at);
        } catch {
          setProviderHealth(provider, "down");
        }
      }),
    );
  }, [providerHealth, setProviderHealth]);

  useEffect(() => { refreshHealth(); }, []);

  const dotColor = (status: ProviderHealthStatus | undefined): string => {
    if (status === "up") return "var(--accent)";
    if (status === "down") return "var(--danger)";
    return "var(--text-dim)";
  };

  // Cascade: provider → all valid series + all valid projects for that provider.
  // Series change narrows projects; project change narrows series (matches Python UI).
  const providers = useMemo(() => getAvailableProviders(), [pppCatalogue, getAvailableProviders]);
  const seriesList = useMemo(
    () => getAvailableSeries(pppSelection.provider),
    [pppCatalogue, pppSelection.provider, getAvailableSeries],
  );
  const projectList = useMemo(
    () => getAvailableProjectsForProvider(pppSelection.provider),
    [pppCatalogue, pppSelection.provider, getAvailableProjectsForProvider],
  );
  // Narrow project list when a series is selected (matches Python _on_ppp_series_changed)
  const filteredProjects = useMemo(
    () => getAvailableProjects(pppSelection.provider, pppSelection.series),
    [pppCatalogue, pppSelection.provider, pppSelection.series, getAvailableProjects],
  );
  // Narrow series list when a project is selected (matches Python _on_ppp_project_changed)
  const filteredSeries = useMemo(
    () => getAvailableSeriesForProject(pppSelection.provider, pppSelection.project),
    [pppCatalogue, pppSelection.provider, pppSelection.project, getAvailableSeriesForProject],
  );

  const scanning = pppCatalogue === null && rinexPath !== null;

  return (
    <>
      {pppCatalogue === null && !rinexPath ? (
        <p className="cf-placeholder">Load a RINEX file to see available providers.</p>
      ) : scanning ? (
        <p className="cf-placeholder">Scanning CDDIS for available products...</p>
      ) : providers.length === 0 ? (
        <p className="cf-placeholder">No PPP products available for this observation window.</p>
      ) : (
        <>
          <div className="field-row">
            {/* The reachability note sits beside the label, not under the
                field, so all three rows are the same: a label, one dropdown. */}
            <div className="field-row-head">
              <label>Provider</label>
              {(() => {
                const status = providerHealth[pppSelection.provider]?.status;
                const label =
                  status === "up"
                    ? `${pppSelection.provider} is reachable`
                    : status === "down"
                      ? `${pppSelection.provider} is unreachable`
                      : `Checking ${pppSelection.provider}...`;
                return (
                  <span className="provider-health">
                    <span className="dot" style={{ color: dotColor(status) }} />
                    <span>{label}</span>
                  </span>
                );
              })()}
            </div>
            <select
              value={pppSelection.provider}
              onChange={(e) => setPppProvider(e.target.value)}
              onMouseDown={() => refreshHealth()}
              onFocus={() => refreshHealth()}
            >
              {providers.map((p) => {
                const status = providerHealth[p]?.status;
                return (
                  <option key={p} value={p} style={{ color: dotColor(status) }}>
                    {`● ${p}`}
                  </option>
                );
              })}
            </select>
          </div>
          <div className="field-row">
            <label>Series</label>
            <select value={pppSelection.series} onChange={(e) => setPppSeries(e.target.value)}>
              {(filteredSeries.length > 0 ? filteredSeries : seriesList).map((value) => (
                <option key={value} value={value}>
                  {PPP_SERIES_LABELS[value] ? `${value} · ${PPP_SERIES_LABELS[value]}` : value}
                </option>
              ))}
            </select>
          </div>
          <div className="field-row">
            <label>Project</label>
            <select value={pppSelection.project} onChange={(e) => setPppProject(e.target.value)}>
              {(filteredProjects.length > 0 ? filteredProjects : projectList).map((value) => (
                <option key={value} value={value}>
                  {PPP_PROJECT_LABELS[value] ? `${value} · ${PPP_PROJECT_LABELS[value]}` : value}
                </option>
              ))}
            </select>
          </div>
        </>
      )}
    </>
  );
}

export function OutputTab() {
  const {
    outputPos, outputGpx, outputTrace, outputSnx, outputSpp, sbas, setOutputFormat,
    runName, setRunName, inputMode,
  } = useAppStore(
    useShallow((s) => ({
      outputPos: s.outputPos,
      outputGpx: s.outputGpx,
      outputTrace: s.outputTrace,
      outputSnx: s.outputSnx,
      outputSpp: s.outputSpp,
      sbas: sbasForcesDynamic(s),
      setOutputFormat: s.setOutputFormat,
      runName: s.runName,
      setRunName: s.setRunName,
      inputMode: s.inputMode,
    })),
  );
  const realtime = useAppStore((s) => s.realtime);
  const updateRealtime = useAppStore((s) => s.updateRealtime);

  return (
    <div className="config-tab-content">
      <div className="config-section">
        <h3>Run</h3>
        {/* Each run writes into its own subdirectory of the one above. Left
            empty, Process names it from the time and the input source. */}
        <div className="field-row">
          <label htmlFor="run-name">Run name</label>
          <input
            id="run-name"
            type="text"
            value={runName}
            placeholder={defaultRunName(inputMode)}
            onChange={(e) => setRunName(e.target.value)}
          />
        </div>
      </div>

      <div className="config-section">
        <h3>Output Formats</h3>
        <div className="output-grid">
          {[
            { key: "pos" as const, label: ".POS", checked: outputPos },
            { key: "gpx" as const, label: ".GPX", checked: outputGpx },
            { key: "trace" as const, label: ".TRACE", checked: outputTrace },
            { key: "snx" as const, label: ".SNX", checked: outputSnx },
            // The SBAS solution and its protection levels: PEA writes none
            // without SBAS, so it is offered only while a live run uses it.
            {
              key: "spp" as const,
              label: ".SPP",
              checked: sbas && outputSpp,
              disabled: !sbas,
              title: sbas
                ? "The SBAS solution with its protection levels (HPL, VPL)"
                : "Only with SBAS: add it under Corrections › SSR corrections in a live run",
            },
          ].map(({ key, label, checked, disabled, title }) => (
            <label key={key} className={`checkbox-row${disabled ? " is-disabled" : ""}`} title={title}>
              <input
                type="checkbox"
                checked={checked}
                disabled={disabled}
                onChange={(e) => setOutputFormat(key, e.target.checked)}
              />
              {label}
            </label>
          ))}
        </div>
      </div>

      {inputMode === "realtime" && (
        <div className="config-section" data-tour="rt-recording">
          <h3>Recordings</h3>
          <label className="rt-checkbox">
            <input
              type="checkbox"
              checked={realtime.record_rtcm}
              onChange={(e) => updateRealtime({ record_rtcm: e.target.checked })}
            />
            Raw RTCM bytes
          </label>
          <label className="rt-checkbox">
            <input
              type="checkbox"
              checked={realtime.record_decoded}
              onChange={(e) => updateRealtime({ record_decoded: e.target.checked })}
            />
            Decoded RTCM (JSON)
            <InfoTip text="Needed for the real-time data stream stats in the Streams tab" />
          </label>
          <label className="rt-checkbox">
            <input
              type="checkbox"
              checked={realtime.record_rinex}
              onChange={(e) => updateRealtime({ record_rinex: e.target.checked })}
            />
            Observations as RINEX
          </label>
          <label className="rt-checkbox">
            <input
              type="checkbox"
              checked={realtime.require_obs}
              onChange={(e) => updateRealtime({ require_obs: e.target.checked })}
            />
            Exit if no observation source is available
          </label>
        </div>
      )}
    </div>
  );
}

export function RinexMetaSummary() {
  const {
    rinexData,
    markerName,
    receiverType, setReceiverType,
    antennaType, setAntennaType,
    antennaOffset, setAntennaOffset,
  } = useAppStore(
    useShallow((s) => ({
      rinexData: s.rinexData,
      markerName: s.markerName,
      receiverType: s.receiverType,
      setReceiverType: s.setReceiverType,
      antennaType: s.antennaType,
      setAntennaType: s.setAntennaType,
      antennaOffset: s.antennaOffset,
      setAntennaOffset: s.setAntennaOffset,
    })),
  );

  if (!rinexData) return null;

  return (
    <div className="config-section">
      <h3>RINEX Metadata</h3>

      {/* Marker stays read-only — parsed (and clipped for v3/v4) from header. */}
      {markerName && (
        <div className="field-row">
          <label>Marker</label>
          <input readOnly value={markerName} style={{ cursor: "default" }} />
        </div>
      )}

      {/* Receiver — editable. CRITICAL: do NOT trim; whitespace is meaningful. */}
      <div className="field-row">
        <label>Receiver</label>
        <input
          type="text"
          value={receiverType}
          onChange={(e) => setReceiverType(e.target.value)}
        />
      </div>

      {/* Antenna — editable. CRITICAL: do NOT trim; whitespace is meaningful. */}
      <div className="field-row">
        <label>Antenna</label>
        <input
          type="text"
          value={antennaType}
          onChange={(e) => setAntennaType(e.target.value)}
        />
      </div>

      {/* Antenna offset — H, E, N (metres). Editable numeric fields. */}
      <div className="field-row">
        <label>Antenna Offset (m)</label>
        <div className="offset-grid">
          <OffsetCell
            axis={0}
            label="H"
            value={antennaOffset[0]}
            onChange={(v) => setAntennaOffset(0, v)}
          />
          <OffsetCell
            axis={1}
            label="E"
            value={antennaOffset[1]}
            onChange={(v) => setAntennaOffset(1, v)}
          />
          <OffsetCell
            axis={2}
            label="N"
            value={antennaOffset[2]}
            onChange={(v) => setAntennaOffset(2, v)}
          />
        </div>
      </div>

      {/* Apriori Position (ARP) — read-only, from the header. */}
      {rinexData.approx_position && (
        <div className="field-row">
          <label>Apriori Position (ARP)</label>
          <input
            readOnly
            value={rinexData.approx_position.map((v) => v.toFixed(3)).join(", ")}
            style={{ cursor: "default" }}
          />
        </div>
      )}
    </div>
  );
}

/** One H/E/N cell with its axis letter shown above the input. */
export function OffsetCell({
  axis,
  label,
  value,
  onChange,
}: {
  axis: 0 | 1 | 2;
  label: string;
  value: number;
  onChange: (v: number) => void;
}) {
  return (
    <div className="offset-cell">
      <span aria-label={`axis-${axis}`}>{label}</span>
      <input
        type="number"
        step="any"
        value={Number.isFinite(value) ? value : 0}
        onChange={(e) => {
          const n = parseFloat(e.target.value);
          onChange(Number.isFinite(n) ? n : 0);
        }}
      />
    </div>
  );
}
