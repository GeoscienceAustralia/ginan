import { useEffect } from "react";
import { getCurrentWindow } from "@tauri-apps/api/window";
import { useAppStore } from "../store/appStore";

/**
 * Base app title, matching the static title in index.html and the Tauri window
 * config (tauri.conf.json).
 */
export const BASE_TITLE = "Ginan UI";

/**
 * Set both the document title (browser/HTML tab) and the native Tauri window
 * title. Tauri does NOT mirror document.title onto the OS window title, so it
 * only updates via window.setTitle(). The window draws its own titlebar (see
 * useWindowChrome), but the OS title still names the window in the taskbar,
 * dock, Alt-Tab and the Window menu. The native
 * call is best-effort: in a non-Tauri context (e.g. plain browser) or if the
 * set-title permission is missing it is swallowed so the flow never logs a
 * console error — the HTML title is already updated by then.
 */
export async function setAppTitle(title: string): Promise<void> {
  document.title = title;
  try {
    await getCurrentWindow().setTitle(title);
  } catch {
    /* non-Tauri context or missing permission — HTML title already set */
  }
}

/**
 * Keep the app/window title in sync with the loaded RINEX file.
 *
 * Reacting to rinexData covers every way a file arrives — the welcome screen
 * and the sidebar Input picker both land here. rinexPath is read imperatively
 * rather than as a dependency because it is set before parsing — reading it on
 * the rinexData change guarantees we use the path that actually produced this
 * data.
 *
 * Mount exactly once (App). The picker resets the title itself on a failed
 * parse, where rinexData is left stale and this effect does not fire.
 */
export function useAppTitle() {
  const rinexData = useAppStore((s) => s.rinexData);

  useEffect(() => {
    if (!rinexData) return;
    const path = useAppStore.getState().rinexPath;
    const fileName = path ? path.split(/[\\/]/).pop() || path : rinexData.marker_name;
    setAppTitle(`${BASE_TITLE} - ${fileName}`);
  }, [rinexData]);
}
