import { useEffect, useState } from "react";
import { pullLiveStreamStats } from "../../hooks/useTauriEvents";
import { useStreamsModel } from "../../hooks/useStreamsModel";
import MessageDrawer from "./MessageDrawer";
import StreamDetail from "./StreamDetail";
import StreamsOverview from "./StreamsOverview";
import { EmptyState, TestFailedState, TestRunningState, WaitingState, type NavTarget } from "./StreamsStates";
import { fmtTimeOfDay } from "./format";
import "./streams.css";

type View = { kind: "overview" } | { kind: "stream"; mount: string; sub: "messages" | "satellites" | "station" };

export default function StreamsTab({ onNavigate }: { onNavigate?: (tab: NavTarget) => void }) {
  const model = useStreamsModel();
  const [view, setView] = useState<View>({ kind: "overview" });
  const [drawerMsg, setDrawerMsg] = useState<string | null>(null);
  const [toast, setToast] = useState<string | null>(null);

  useEffect(() => {
    void pullLiveStreamStats();
  }, []);
  useEffect(() => {
    if (!toast) return;
    const id = setTimeout(() => setToast(null), 6000);
    return () => clearTimeout(id);
  }, [toast]);

  const stream = view.kind === "stream" ? model.streams.find((s) => s.mount === view.mount) : undefined;
  const live = model.phase === "live" || model.phase === "stopped";
  // A stream that vanished (run ended and reset) drops back to the overview.
  const detail = live && view.kind === "stream" && stream;

  const reset = async () => {
    await model.resetCounters();
    setToast(`Counters reset at ${fmtTimeOfDay(Date.now())}`);
  };

  let body;
  if (detail && view.kind === "stream") {
    body = (
      <StreamDetail
        stream={detail}
        model={model}
        sub={view.sub}
        onSubChange={(sub) => setView({ ...view, sub })}
        onBack={() => { setView({ kind: "overview" }); setDrawerMsg(null); }}
        onOpenMessage={setDrawerMsg}
      />
    );
  } else if (live) {
    body = <StreamsOverview model={model} onReset={reset} onOpen={(mount, sub) => setView({ kind: "stream", mount, sub })} />;
  } else if (model.phase === "waiting") body = <WaitingState model={model} onNavigate={onNavigate} />;
  else if (model.phase === "test-running") body = <TestRunningState model={model} />;
  else if (model.phase === "test-failed") body = <TestFailedState model={model} />;
  else body = <EmptyState model={model} onNavigate={onNavigate} />;

  return (
    <div className="streams-scroll">
    <div className="streams-tab" data-phase={model.phase}>
      {body}
      {detail && drawerMsg && (
        <MessageDrawer
          stream={detail}
          msgNo={drawerMsg}
          order={detail.messages.map((m) => m.msgNo)}
          onNavigate={setDrawerMsg}
          onClose={() => setDrawerMsg(null)}
        />
      )}
      {toast && (
        <div className="streams-toast" role="status">
          <span>{toast}</span>
          <button className="btn-ghost" onClick={() => setToast(null)}>Dismiss</button>
        </div>
      )}
    </div>
    </div>
  );
}
