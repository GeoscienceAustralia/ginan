import type { StreamsModel } from "../../types/streams";
import { CheckIcon, CloseIcon } from "../icons";
import { DASH, fmtTimeOfDay } from "./format";

/** Result tiles shared by the overview card and the failed-test screen. */
export function TestTiles({ result }: { result: NonNullable<StreamsModel["test"]["result"]> }) {
  const counts = Object.entries(result.snapshot.total_by_type).map(([k, v]) => `${k.toUpperCase()} ${v}`).join(" · ");
  return (
    <div className="result-tiles">
      <div className="result-tile"><span>Constellations</span><span>{result.constellations.join(", ") || DASH}</span></div>
      <div className="result-tile"><span>Epoch interval</span><span className="mono">{result.interval_s != null ? `${result.interval_s.toFixed(2)} s` : DASH}</span></div>
      <div className="result-tile"><span>Records</span><span className="mono">{counts || DASH}</span></div>
    </div>
  );
}

export default function StreamTestCard({ test }: { test: StreamsModel["test"] }) {
  const { status, result, atMs } = test;
  return (
    <section className="streams-panel" aria-labelledby="h-test">
      <div className="streams-panel-head">
        <h2 id="h-test">Last stream test</h2>
        {atMs != null && <span className="streams-muted">{fmtTimeOfDay(atMs)} · 10 s watch</span>}
        <span style={{ flex: 1 }} />
        {status === "passed" && <span className="result-badge is-ok"><CheckIcon size={12} />Passed</span>}
        {status === "failed" && <span className="result-badge is-bad"><CloseIcon size={12} />Failed</span>}
      </div>
      {result ? (
        <TestTiles result={result} />
      ) : (
        <p className="streams-note">No test yet. Press Monitor stream (Process needs a passed run).</p>
      )}
      <p className="streams-note">
        Process runs the same check before starting PEA. It fills the constellations, codes and epoch interval in Workflow › Constellations.
      </p>
    </section>
  );
}
