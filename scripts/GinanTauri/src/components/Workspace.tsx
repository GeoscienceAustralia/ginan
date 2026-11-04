import { Profiler, useEffect, useState } from "react";
import ConsoleLog from "./ConsoleLog";
import WorkflowPanel from "./WorkflowPanel";
import StreamsTab from "./streams/StreamsTab";
import VisualizationPanel from "./VisualizationPanel";
import WelcomeScreen from "./WelcomeScreen";
import Header from "./Header";
import ConnectionWarning from "./ConnectionWarning";
import { TabBarSlotContext } from "./TabBarSlot";
import { useAppStore, type WorkspaceTabKey } from "../store/appStore";
import { onRender } from "../profiling/onRender";

/**
 * Right-hand column: the Header, with the tabs on it, over the tab body. The
 * offline-mode warning and CDDIS failures surface in the ConnectionWarning
 * directly under the Header and in the status bar.
 *
 * Workflow presents the high-level status as a semantic timeline, while
 * Console carries raw PEA output. Both survive a tab switch because their lines
 * live in the store, not in either panel; only the scroll position resets.
 * Visualization re-reads the output directory each time it mounts, so its state
 * is cheap to throw away.
 */

type TabKey = WorkspaceTabKey;

const TABS: { key: TabKey; label: string }[] = [
  { key: "workflows", label: "Workflow" },
  { key: "console", label: "Console" },
  { key: "streams", label: "Streams" },
  { key: "visualization", label: "Visualisation" },
];

export default function Workspace() {
  // Always open on Workflow; the Streams tab is one click away in realtime.
  const [selected, setTab] = useState<TabKey>("workflows");
  // Another part of the app asking for a tab (e.g. a run blocker's fix).
  const tabRequest = useAppStore((s) => s.workspaceTabRequest);
  const requestWorkspaceTab = useAppStore((s) => s.requestWorkspaceTab);
  useEffect(() => {
    if (!tabRequest) return;
    setTab(tabRequest);
    requestWorkspaceTab(null);
  }, [tabRequest, requestWorkspaceTab]);
  // Row under the Header; the active tab may put its controls here.
  const [actionsSlot, setActionsSlot] = useState<HTMLDivElement | null>(null);
  const rinexData = useAppStore((s) => s.rinexData);
  const inputMode = useAppStore((s) => s.inputMode);
  // Set once a file is loaded or realtime is entered, so switching back to
  // file mode keeps the tabs rather than returning to the welcome screen.
  const hasEnteredWorkspace = useAppStore((s) => s.hasEnteredWorkspace);
  // Streams only means something for a live run, so a file run hides it and
  // falls back to Workflow if it was open when the mode switched.
  const tabs = inputMode === "file" ? TABS.filter((t) => t.key !== "streams") : TABS;
  const tab = inputMode === "file" && selected === "streams" ? "workflows" : selected;

  // Every launch starts here: file mode with nothing loaded yet, so the
  // welcome screen takes the tabs' place until a file or realtime is chosen.
  if (inputMode === "file" && !rinexData && !hasEnteredWorkspace) {
    return (
      <div className="workspace">
        <Header />
        <ConnectionWarning />
        <WelcomeScreen />
      </div>
    );
  }

  return (
    <div className="workspace">
      <Header>
        <div className="tabbar" data-tour="tabbar" role="tablist" aria-label="Workspace">
          {tabs.map((t) => (
            <button
              key={t.key}
              role="tab"
              id={`tab-${t.key}`}
              // Per-tab, not just the bar: the streaming tour points at Streams
              // specifically, which is the tab a live run is actually watched on.
              data-tour={`tab-${t.key}`}
              aria-selected={tab === t.key}
              aria-controls={`tabpanel-${t.key}`}
              className={`tab${tab === t.key ? " is-active" : ""}`}
              onClick={() => setTab(t.key)}
            >
              {t.label}
            </button>
          ))}
        </div>
      </Header>
      <ConnectionWarning />
      {/* Controls the active tab places under the Header (see TabBarSlot);
          the row collapses when the tab has none. */}
      <div className="tabbar-actions" ref={setActionsSlot} />

      <TabBarSlotContext.Provider value={actionsSlot}>
      <div
        className="workspace-main"
        role="tabpanel"
        id={`tabpanel-${tab}`}
        aria-labelledby={`tab-${tab}`}
      >
        {/* Render cost per tab, read by the profiling HUD. Free otherwise. */}
        <Profiler id={`tab:${tab}`} onRender={onRender}>
        {tab === "workflows" ? (
          <WorkflowPanel />
        ) : tab === "console" ? (
          <div className="console-frame">
            <ConsoleLog channel="console" />
          </div>
        ) : tab === "streams" ? (
          <StreamsTab onNavigate={setTab} />
        ) : tab === "visualization" ? (
          <VisualizationPanel />
        ) : (
          <div className="tab-placeholder">Hello world</div>
        )}
        </Profiler>
      </div>
      </TabBarSlotContext.Provider>
    </div>
  );
}
