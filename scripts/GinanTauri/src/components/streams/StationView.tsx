import { useStationInfo } from "../../hooks/useStationInfo";
import type { StreamView } from "../../types/streams";
import { STATUS_LABEL, fmtAgo, fmtBytes } from "./detailUtils";
import "./streams-detail.css";

const A = 6378137;
const F = 1 / 298.257223563;
const E2 = F * (2 - F);

/** ECEF metres to WGS84 geodetic (degrees, ellipsoidal height in metres). */
export function ecefToGeodetic(x: number, y: number, z: number) {
  const p = Math.hypot(x, y);
  const lon = Math.atan2(y, x);
  let lat = Math.atan2(z, p * (1 - E2));
  let h = 0;
  for (let i = 0; i < 6; i++) {
    const sin = Math.sin(lat);
    const n = A / Math.sqrt(1 - E2 * sin * sin);
    h = Math.abs(lat) < Math.PI / 4 ? p / Math.cos(lat) - n : z / sin - n * (1 - E2);
    lat = Math.atan2(z, p * (1 - (E2 * n) / (n + h)));
  }
  const deg = 180 / Math.PI;
  return { lat: lat * deg, lon: lon * deg, h };
}

const num = (v: number | null | undefined, dp: number, unit = "") =>
  v == null ? "–" : `${v.toFixed(dp)}${unit}`;

function KV({ rows }: { rows: [string, React.ReactNode][] }) {
  return (
    <dl className="sd-kv">
      {rows.map(([k, v]) => (
        <div key={k} className="sd-kv-row">
          <dt>{k}</dt>
          <dd>{v ?? "–"}</dd>
        </div>
      ))}
    </dl>
  );
}

function Panel({ id, title, hint, children }: { id: string; title: string; hint?: string; children: React.ReactNode }) {
  return (
    <section className="sd-card" aria-labelledby={id}>
      <div className="sd-card-head">
        <h2 id={id}>{title}</h2>
        {hint && <span className="sd-muted sd-small sd-mono">{hint}</span>}
      </div>
      {children}
    </section>
  );
}

export default function StationView({ stream }: { stream: StreamView }) {
  const { info, loading, error, refresh } = useStationInfo(stream.mount);
  const hasPos = info?.ecef_x != null && info.ecef_y != null && info.ecef_z != null;
  const geo = hasPos ? ecefToGeodetic(info!.ecef_x!, info!.ecef_y!, info!.ecef_z!) : null;
  const empty = !info || (!hasPos && !info.antenna_desc && !info.receiver_type);

  return (
    <div className="sd-grid">
      {empty ? (
        <section className="sd-card sd-span" aria-labelledby="sd-h-st-empty">
          <h2 id="sd-h-st-empty">Reference station</h2>
          {error ? (
            <p className="sd-empty" role="alert">
              Couldn't read station messages: {error}{" "}
              <button type="button" className="sd-btn" onClick={refresh}>Retry</button>
            </p>
          ) : loading ? (
            <p className="sd-empty">Reading station messages...</p>
          ) : (
            <p className="sd-empty">
              No station messages yet. The position (1005/1006) and receiver and antenna (1033) come from the recorded
              RTCM, so this needs Recordings › RTCM on and a mountpoint that sends them. Correction and navigation
              mountpoints usually don't.
            </p>
          )}
        </section>
      ) : (
        <>
          <Panel id="sd-h-pos" title="Reference position" hint={info.messages.filter((m) => m === 1005 || m === 1006).join("/") || undefined}>
            <KV
              rows={[
                ["Station ID", info.station_id == null ? null : <span className="sd-mono">{info.station_id}</span>],
                ["ECEF X", <span className="sd-mono">{num(info.ecef_x, 4, " m")}</span>],
                ["ECEF Y", <span className="sd-mono">{num(info.ecef_y, 4, " m")}</span>],
                ["ECEF Z", <span className="sd-mono">{num(info.ecef_z, 4, " m")}</span>],
                ["Latitude", <span className="sd-mono">{num(geo?.lat, 8, "°")}</span>],
                ["Longitude", <span className="sd-mono">{num(geo?.lon, 8, "°")}</span>],
                ["Ellipsoidal height", <span className="sd-mono">{num(geo?.h, 3, " m")}</span>],
                ["Antenna height", <span className="sd-mono">{num(info.antenna_height, 4, " m")}</span>],
                ["Updated", <span className="sd-mono">{info.updated_ms == null ? "–" : new Date(info.updated_ms).toLocaleTimeString([], { hour12: false })}</span>],
              ]}
            />
            {hasPos && (
              <button
                type="button"
                className="sd-btn sd-btn-start"
                onClick={() =>
                  void navigator.clipboard?.writeText(`${info.ecef_x} ${info.ecef_y} ${info.ecef_z}`)
                }
              >
                Copy XYZ
              </button>
            )}
          </Panel>
          <Panel id="sd-h-rx" title="Receiver and antenna" hint={info.messages.includes(1033) ? "1033" : undefined}>
            <KV
              rows={[
                ["Antenna", info.antenna_desc ? <span className="sd-mono">{info.antenna_desc}{info.antenna_setup_id != null && ` · setup ${info.antenna_setup_id}`}</span> : null],
                ["Antenna serial", info.antenna_serial && <span className="sd-mono">{info.antenna_serial}</span>],
                ["Receiver", info.receiver_type && <span className="sd-mono">{info.receiver_type}</span>],
                ["Firmware", info.receiver_firmware && <span className="sd-mono">{info.receiver_firmware}</span>],
                ["Receiver serial", info.receiver_serial && <span className="sd-mono">{info.receiver_serial}</span>],
              ]}
            />
          </Panel>
        </>
      )}

      <Panel id="sd-h-cas" title="Caster and mountpoint" hint="source table">
        <KV
          rows={[
            ["Caster", stream.casterHost && <span className="sd-mono">{stream.casterHost}</span>],
            ["Protocol", stream.protocol],
            ["Format", stream.formatDetails],
            ["Identifier", stream.location],
            ["Role", stream.role],
          ]}
        />
      </Panel>

      <Panel id="sd-h-conn" title="Connection">
        <KV
          rows={[
            ["Status", STATUS_LABEL[stream.status]],
            ["Reconnects", <span className="sd-mono">{stream.retryCount}</span>],
            ["HTTP status", stream.httpStatus == null ? null : <span className="sd-mono">{stream.httpStatus}</span>],
            ["Last error", stream.lastError ?? <span className="sd-muted">None</span>],
            ["Last event", stream.lastEventAgeMs == null ? null : fmtAgo(stream.lastEventAgeMs / 1000)],
            ["Received", <span className="sd-mono">{fmtBytes(stream.bytesTotal)}</span>],
          ]}
        />
      </Panel>
    </div>
  );
}
