import { useMemo, useState } from "react";
import type { SatelliteView, StreamsModel, StreamView } from "../../types/streams";
import { fmtClock } from "./detailUtils";
import "./streams-detail.css";

const CONS = [
  { id: "GPS", name: "GPS", code: "G" },
  { id: "GAL", name: "Galileo", code: "E" },
  { id: "GLO", name: "GLONASS", code: "R" },
  { id: "BDS", name: "BeiDou", code: "C" },
  { id: "QZS", name: "QZSS", code: "J" },
] as const;

const WEAK = 35;
const bar = (cn0: number) => Math.round(100 * Math.min(Math.max((cn0 - 20) / 35, 0), 1));
const inSol = (s: SatelliteView) => s.inSolution === true;

/** "G5" must find "G05": compare the typed text with the PRN and with the PRN minus its zero pad. */
export function prnMatches(sat: string, query: string): boolean {
  const q = query.trim().toUpperCase();
  if (!q) return true;
  return sat.startsWith(q) || sat.replace(/^([A-Z])0/, "$1").startsWith(q);
}

export default function SatellitesView({ stream, model }: { stream: StreamView; model: StreamsModel }) {
  const [sel, setSel] = useState<string>("ALL");
  const [query, setQuery] = useState("");
  const [droppedOnly, setDroppedOnly] = useState(false);

  const sats = useMemo(
    () => [...model.satellitesFor(stream.mount)].sort((a, b) => a.sat.localeCompare(b.sat)),
    [model, stream.mount],
  );
  const status = model.satelliteStatus;
  const trace = !!status?.available;

  const byCode = (code: string) => sats.filter((s) => s.sat[0] === code);
  const eff = sel !== "ALL" && byCode(CONS.find((c) => c.id === sel)?.code ?? "").length === 0 ? "ALL" : sel;
  const cur = CONS.find((c) => c.id === eff);
  const scoped = cur ? byCode(cur.code) : sats;
  const rows = scoped.filter((s) => prnMatches(s.sat, query) && (!droppedOnly || s.dropped));
  const nIn = scoped.filter(inSol).length;
  const nDrop = scoped.filter((s) => s.dropped).length;
  const sigs = [...new Set(scoped.flatMap((s) => s.signals))].sort().join(" ");

  const count = (n: number, tot: number) => (trace ? `${n}` : `${tot}`);

  return (
    <div className="sd-split sd-sats">
      <div role="radiogroup" aria-label="Constellation" className="sd-cons-list">
        <label className={`sd-cons${eff === "ALL" ? " is-sel" : ""}`}>
          <input type="radio" name="sd-cons" checked={eff === "ALL"} onChange={() => setSel("ALL")} />
          <span className="sd-cons-name">All</span>
          <span className="sd-grow" />
          <span className="sd-mono sd-muted">{trace ? `${sats.filter(inSol).length} / ${sats.length}` : sats.length}</span>
        </label>
        {CONS.map((c) => {
          const list = byCode(c.code);
          const n = list.filter(inSol).length;
          const off = list.length === 0;
          return (
            <label key={c.id} className={`sd-cons${eff === c.id ? " is-sel" : ""}${off ? " is-off" : ""}`}>
              <input type="radio" name="sd-cons" checked={eff === c.id} disabled={off} onChange={() => setSel(c.id)} />
              <span className="sd-swatch" aria-hidden="true" style={{ background: `var(--sd-bg-msm-${c.id})` }} />
              <span className="sd-cons-name">{c.name}</span>
              <span className="sd-grow" />
              {off ? (
                <span className="sd-mono sd-muted" title="Not tracked by this stream">
                  –<span className="sd-sr">Not tracked by this stream</span>
                </span>
              ) : (
                <span className="sd-mono sd-muted">{trace ? `${count(n, list.length)} / ${list.length}` : list.length}</span>
              )}
            </label>
          );
        })}
      </div>

      <section className="sd-card sd-list-card" aria-labelledby="sd-h-sats">
        <div className="sd-toolbar">
          <div className="sd-title">
            <h2 id="sd-h-sats">{cur?.name ?? "All constellations"}</h2>
            <span className="sd-muted sd-small">
              {trace ? `${nIn} in solution · ${nDrop} dropped · ` : `${scoped.length} tracked · `}
              signals {sigs || "–"}
            </span>
          </div>
          <label className="sd-search">
            <span className="sd-sr">Search satellites</span>
            <input type="search" placeholder="PRN, e.g. G5" value={query} onChange={(e) => setQuery(e.target.value)} />
          </label>
          <label className="sd-check">
            <input type="checkbox" checked={droppedOnly} onChange={(e) => setDroppedOnly(e.target.checked)} /> Dropped only
          </label>
        </div>

        {status && !status.available && (
          <p className="sd-note sd-warn-note" role="status">
            {status.message ?? "No .TRACE file yet."} Solution columns are blank until the .TRACE is written.
          </p>
        )}

        <div className="sd-scroll">
          <table className="sd-table sd-satt">
            <thead>
              <tr>
                <th scope="col">Satellite</th>
                <th scope="col">Signals</th>
                <th scope="col">C/N₀ (avg, dB-Hz)</th>
                <th scope="col" className="sd-num">Postfit RMS</th>
                <th scope="col" className="sd-num">Samples</th>
                <th scope="col">Last epoch</th>
              </tr>
            </thead>
            <tbody>
              {rows.map((s) => (
                <tr key={s.sat}>
                  <th scope="row" data-label="Satellite">
                    <span className="sd-prn">
                      <span className={`sd-dot${s.dropped ? " is-off" : ""}`} aria-hidden="true" />
                      <span className="sd-mono">{s.sat}</span>
                    </span>
                  </th>
                  <td data-label="Signals">
                    <span className="sd-chips">
                      {s.signals.map((g) => (
                        <span key={g} className="sd-chip sd-mono">{g}</span>
                      ))}
                    </span>
                  </td>
                  <td data-label="C/N₀">
                    {s.cn0 == null ? (
                      <span className="sd-muted">–</span>
                    ) : (
                      <span className="sd-cn0">
                        <span className="sd-cn0-bar" aria-hidden="true">
                          <span className={s.cn0 < WEAK ? "is-weak" : ""} style={{ width: `${bar(s.cn0)}%` }} />
                        </span>
                        <span className="sd-mono">{s.cn0.toFixed(1)}</span>
                        {s.cn0 < WEAK && <span className="sd-sr">weak</span>}
                      </span>
                    )}
                  </td>
                  <td data-label="Postfit RMS" className="sd-mono sd-num">{s.postfitRms == null ? "–" : `${s.postfitRms.toFixed(2)} m`}</td>
                  <td data-label="Samples" className="sd-mono sd-num">{s.samples == null ? "–" : s.samples.toLocaleString()}</td>
                  <td data-label="Last epoch">
                    <span className="sd-prn">
                      <span className="sd-mono">{fmtClock(s.lastEpochMs)}</span>
                      {s.dropped && <span className="sd-badge">Dropped</span>}
                    </span>
                  </td>
                </tr>
              ))}
              {rows.length === 0 && (
                <tr className="sd-nomatch">
                  <td colSpan={6}>
                    {sats.length === 0 ? "No satellites decoded on this stream yet." : "No satellites match."}
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
        <p className="sd-muted sd-small">
          In solution = has a residual at the latest .TRACE epoch. C/N₀ bar turns amber under {WEAK} dB-Hz. C/N₀ needs decoded RTCM recording on.
        </p>
      </section>
    </div>
  );
}
