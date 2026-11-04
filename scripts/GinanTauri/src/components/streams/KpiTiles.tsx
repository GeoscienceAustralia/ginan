import type { StreamsModel } from "../../types/streams";
import { DASH, fmtBytes, fmtClock, fmtPerSec, fmtRate } from "./format";

interface Tile { label: string; value: string; unit: string; sub: string }

export function kpiTiles(model: StreamsModel): Tile[] {
  const k = model.kpis;
  const total = fmtBytes(k.bytesTotal);
  const rate = fmtRate(k.bytesPerSec);
  const waiting = model.phase === "waiting";
  const since = model.resetAtMs ? ` · since reset` : "";
  return [
    { label: "Data received", ...total, sub: waiting ? "waiting for first data" : `since start · ${fmtClock(model.runtimeS)}` },
    { label: "Throughput", ...rate, sub: waiting ? "waiting for first data" : `sum of ${model.streams.length} streams` },
    {
      label: "Decoded messages",
      value: model.recordDecodedOff ? DASH : fmtPerSec(k.recordsPerSec),
      unit: model.recordDecodedOff || k.recordsPerSec == null ? "" : "/s",
      sub: model.recordDecodedOff ? "decoded records off" : `last 60 s · ${k.malformed} malformed${since}`,
    },
    {
      label: "Satellites in solution",
      value: k.satsInSolution == null ? DASH : String(k.satsInSolution),
      unit: k.satsSeen == null ? "" : `/ ${k.satsSeen} seen`,
      sub: k.satsInSolution == null ? "waiting for the first .TRACE epoch" : `latest .TRACE epoch · ${Math.max(0, (k.satsSeen ?? 0) - k.satsInSolution)} dropped`,
    },
  ];
}

export default function KpiTiles({ model }: { model: StreamsModel }) {
  return (
    <dl className="streams-kpis">
      {kpiTiles(model).map((t) => (
        <div className="streams-kpi" key={t.label} title={t.sub}>
          <dt className="streams-eyebrow">{t.label}</dt>
          <dd className={`streams-kpi-value${t.value === DASH ? " is-empty" : ""}`}>
            <span className="mono">{t.value}</span>
            {t.unit && <span className="streams-kpi-unit">{t.unit}</span>}
          </dd>
          <dd className="streams-kpi-sub">{t.sub}</dd>
        </div>
      ))}
    </dl>
  );
}
