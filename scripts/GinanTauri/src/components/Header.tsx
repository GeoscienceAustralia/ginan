import type { ReactNode } from "react";
import { useAppStore } from "../store/appStore";
import { tourForCurrentShell } from "./tourSteps";
import { SettingsIcon, PanelLeftIcon, HelpIcon } from "./icons";
import RunControls from "./RunControls";
import { useAppVersion } from "../hooks/useAppVersion";
import ginanLogo from "../assets/ginan-logo.png";

/**
 * Top bar of the workspace column, to the right of the sidebar. The workspace
 * tabs (passed in as `children`) sit on the left, the tour, Settings and the
 * run controls on the right.
 *
 * The app mark lives at the top of the sidebar (see SidebarBrand). Once the
 * sidebar is hidden the mark moves here, together with the button that brings
 * the sidebar back, so the window always shows the mark and a way to reopen
 * the rail.
 *
 * The run pair only shows once a file is loaded or realtime is entered. Settings
 * stays throughout, since it is both the machine setup and the second way to
 * open a file, via Configuration → Import; so does the tour button, which is
 * the only permanent way back into the guided tour.
 *
 * It is also the window's titlebar (see useWindowChrome): the bar and its
 * non-interactive parts carry `data-tauri-drag-region`, so they drag the window
 * and double-click to maximise. The attribute only acts on the element that
 * carries it, which is why it is repeated down to the logo and name, and why
 * the buttons, which lack it, still just click.
 */
export default function Header({ children }: { children?: ReactNode }) {
  const sidebarCollapsed = useAppStore((s) => s.sidebarCollapsed);
  const openSettings = useAppStore((s) => s.openSettings);
  const rinexData = useAppStore((s) => s.rinexData);
  const inputMode = useAppStore((s) => s.inputMode);
  const hasEnteredWorkspace = useAppStore((s) => s.hasEnteredWorkspace);
  const startTour = useAppStore((s) => s.startTour);

  return (
    <div className={`header${sidebarCollapsed ? " has-brand" : ""}`} data-tauri-drag-region>
      {sidebarCollapsed ? <AppMark /> : null}
      <div className="header-nav" data-tauri-drag-region>
        {children}
      </div>
      <div className="header-actions" data-tauri-drag-region>
        {/* Which tour depends on what is on screen: with no file open most of
            the workspace tour's targets do not exist yet, and a live stream
            puts up the same shell around entirely different panels. */}
        <button
          className="btn-icon"
          data-tour="help"
          onClick={() => startTour(tourForCurrentShell(inputMode, rinexData !== null))}
          title="Guided tour of the app"
          aria-label="Guided tour of the app"
        >
          <HelpIcon />
        </button>
        <button
          className="btn-ghost"
          data-tour="settings"
          onClick={() => openSettings()}
          title="Open settings for config files, directories, the pea binary and credentials"
        >
          <SettingsIcon /> Settings
        </button>
        {rinexData || inputMode === "realtime" || hasEnteredWorkspace ? (
          <>
            <div className="header-divider" />
            <div className="header-run" data-tour="run-controls">
              <RunControls />
            </div>
          </>
        ) : null}
      </div>
    </div>
  );
}

/**
 * The sidebar toggle, logo and name. Rendered at the top of the sidebar while
 * it is open and in the Header while it is hidden, so the toggle always sits
 * next to the mark.
 *
 * The version beside the name is the installed build's, so a user can tell
 * which release they are on without opening Settings.
 */
export function AppMark() {
  const sidebarCollapsed = useAppStore((s) => s.sidebarCollapsed);
  const toggleSidebar = useAppStore((s) => s.toggleSidebar);
  const version = useAppVersion();

  return (
    <div className="header-title" data-tauri-drag-region>
      <button
        className="btn-icon"
        onClick={toggleSidebar}
        aria-expanded={!sidebarCollapsed}
        aria-controls="config-panel"
        aria-label={sidebarCollapsed ? "Show sidebar" : "Hide sidebar"}
        title={sidebarCollapsed ? "Show sidebar" : "Hide sidebar"}
      >
        <PanelLeftIcon />
      </button>
      <img className="header-logo" src={ginanLogo} alt="" draggable={false} data-tauri-drag-region />
      <h1 data-tauri-drag-region>Ginan UI</h1>
      <span className="header-version" data-tauri-drag-region title={`Ginan UI version ${version}`}>
        v{version}
      </span>
    </div>
  );
}
