import type { StreamsModel, StreamView } from "../../types/streams";
import MessagesView from "./MessagesView";
import SatellitesView from "./SatellitesView";
import StationView from "./StationView";
import { ROLE_TITLE, STATUS_LABEL, fmtBytes, fmtRate, statusTone } from "./detailUtils";
import "./streams-detail.css";

export type StreamSub = "messages" | "satellites" | "station";

export interface StreamDetailProps {
  stream: StreamView;
  model: StreamsModel;
  sub: StreamSub;
  onSubChange: (sub: StreamSub) => void;
  onBack: () => void;
  onOpenMessage: (msgNo: string) => void;
}

export default function StreamDetail({ stream, model, sub, onSubChange, onBack, onOpenMessage }: StreamDetailProps) {
  const tone = statusTone(stream.status);
  const tabs: { id: StreamSub; label: string; n?: number }[] = [
    { id: "messages", label: "Messages", n: stream.messages.length },
    { id: "satellites", label: "Satellites", n: stream.satCount },
    { id: "station", label: "Station" },
  ];
  const where = [stream.location, stream.casterHost].filter(Boolean);

  return (
    <div className="sd sd-page">
      <button type="button" className="sd-back" onClick={onBack}>
        <svg aria-hidden="true" width="14" height="14" viewBox="0 0 14 14" fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round" strokeLinejoin="round">
          <path d="M8.5 3L4.5 7l4 4" />
        </svg>
        All streams
      </button>

      <header className="sd-head">
        <div className="sd-head-id">
          <div className="sd-head-title">
            {stream.role && (
              <span className={`sd-role is-${stream.role.toLowerCase()}`} title={ROLE_TITLE[stream.role]}>{stream.role}</span>
            )}
            <h1 className="sd-mono">{stream.mount}</h1>
          </div>
          <span className="sd-muted">
            {stream.location}
            {stream.location && stream.casterHost && " · "}
            {stream.casterHost && (
              <>
                <span className="stream-meta-label">Caster</span>
                <span className="sd-mono">{stream.casterHost}</span>
              </>
            )}
            {where.length === 0 && "Caster unknown"}
          </span>
        </div>
        <div className="sd-stat">
          <span className="sd-muted sd-small">Status</span>
          <span className={`sd-status is-${tone}`}>
            <span className="sd-dot" aria-hidden="true" />
            {STATUS_LABEL[stream.status]}
          </span>
        </div>
        <div className="sd-stat">
          <span className="sd-muted sd-small">Received</span>
          <span className="sd-stat-v sd-mono">{fmtBytes(stream.bytesTotal)}</span>
        </div>
        <div className="sd-stat">
          <span className="sd-muted sd-small">Protocol</span>
          <span className="sd-stat-v">
            {stream.protocol ?? "–"}
          </span>
        </div>
        <div className="sd-stat">
          <span className="sd-muted sd-small">Throughput</span>
          <span className="sd-stat-v sd-mono">{fmtRate(stream.bytesPerSec)}</span>
        </div>
      </header>

      <nav aria-label="Stream views" className="sd-subnav">
        {tabs.map((t) => (
          <button
            key={t.id}
            type="button"
            className="sd-subtab"
            aria-current={sub === t.id ? "page" : undefined}
            onClick={() => onSubChange(t.id)}
          >
            {t.label}
            {t.n != null && <span className="sd-mono sd-muted">{t.n}</span>}
          </button>
        ))}
      </nav>

      {sub === "messages" && <MessagesView stream={stream} model={model} onOpenMessage={onOpenMessage} />}
      {sub === "satellites" && <SatellitesView stream={stream} model={model} />}
      {sub === "station" && <StationView stream={stream} />}
    </div>
  );
}
