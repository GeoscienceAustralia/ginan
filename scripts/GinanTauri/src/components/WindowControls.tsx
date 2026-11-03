import { useEffect } from "react";
import { getCurrentWindow, type Window } from "@tauri-apps/api/window";
import { titlebarStyle, useWindowState } from "../hooks/useWindowChrome";

/**
 * Minimise / maximise-restore / close for the undecorated window on Windows
 * and Linux (see useWindowChrome).
 *
 * Mounted at the root rather than inside the Header, pinned to the top-right
 * above every layer: Settings and onboarding are full-window surfaces that
 * cover the header, and a native titlebar would still be there over them, so
 * these have to be too. The header and those surfaces pad themselves by
 * `--titlebar-inset-right` so nothing sits underneath.
 *
 * Renders nothing on macOS (the native traffic lights stay), in a plain
 * browser, and in fullscreen, where the OS hides its own caption buttons.
 */
export default function WindowControls() {
  const { maximized, fullscreen, focused } = useWindowState();

  // Fullscreen hides the traffic lights too, so the macOS inset is dropped as
  // well — hence on <html>, not just here.
  useEffect(() => {
    document.documentElement.toggleAttribute("data-window-fullscreen", fullscreen);
  }, [fullscreen]);

  if (titlebarStyle !== "custom" || fullscreen) return null;

  const act = (fn: (w: Window) => Promise<void>) => () => {
    fn(getCurrentWindow()).catch((e) => console.error("window control failed:", e));
  };

  // Out of the tab order, like the native caption buttons: the keyboard route
  // is the OS one (Alt+F4, Win+arrows), which still works undecorated.
  return (
    <div
      className={"window-controls" + (focused ? "" : " is-inactive")}
      role="group"
      aria-label="Window controls"
    >
      <button
        className="window-control"
        tabIndex={-1}
        onClick={act((w) => w.minimize())}
        aria-label="Minimize"
        title="Minimize"
      >
        <CaptionGlyph>
          <path d="M0 5.5h10" shapeRendering="crispEdges" />
        </CaptionGlyph>
      </button>
      <button
        className="window-control"
        tabIndex={-1}
        onClick={act((w) => w.toggleMaximize())}
        aria-label={maximized ? "Restore" : "Maximize"}
        title={maximized ? "Restore" : "Maximize"}
      >
        {maximized ? (
          <CaptionGlyph>
            <rect x="0.5" y="2.5" width="7" height="7" shapeRendering="crispEdges" />
            <path d="M2.5 2.5v-2h7v7h-2" shapeRendering="crispEdges" />
          </CaptionGlyph>
        ) : (
          <CaptionGlyph>
            <rect x="0.5" y="0.5" width="9" height="9" shapeRendering="crispEdges" />
          </CaptionGlyph>
        )}
      </button>
      <button
        className="window-control window-control--close"
        tabIndex={-1}
        onClick={act((w) => w.close())}
        aria-label="Close"
        title="Close"
      >
        <CaptionGlyph>
          <path d="M0.5 0.5l9 9M9.5 0.5l-9 9" />
        </CaptionGlyph>
      </button>
    </div>
  );
}

/**
 * Caption glyphs are drawn on a 10px grid with a 1px hairline, as Windows draws
 * them, rather than with the app's 24px line icons — those read too heavy and
 * too large at this size. Straight edges snap to the pixel grid; the close
 * cross is left anti-aliased, since its diagonals would stair-step.
 */
function CaptionGlyph({ children }: { children: React.ReactNode }) {
  return (
    <svg
      width="10"
      height="10"
      viewBox="0 0 10 10"
      fill="none"
      stroke="currentColor"
      strokeWidth="1"
      aria-hidden="true"
      focusable="false"
    >
      {children}
    </svg>
  );
}
