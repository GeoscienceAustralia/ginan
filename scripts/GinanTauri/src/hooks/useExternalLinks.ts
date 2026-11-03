import { useEffect } from "react";
import { invoke } from "@tauri-apps/api/core";

/**
 * Keep the app window on the app.
 *
 * The webview *is* the application window — it has no address bar and no Back
 * button, so a link that navigates it replaces the entire UI with a web page
 * and strands the user there. Leaflet's attribution links are one example, but
 * any external anchor anywhere in the app has the same effect.
 *
 * Rather than fixing individual links, this catches them at the document:
 * clicks on outbound anchors are cancelled and handed to the system browser.
 * Anything added later is covered without having to remember this exists.
 *
 * Capture phase so it runs before a handler that might stop propagation, and
 * `beforeunload` as the backstop for a navigation started some other way —
 * `window.location`, a form post, a `target` we did not anticipate.
 */
export function useExternalLinks() {
  useEffect(() => {
    const onClick = (e: MouseEvent) => {
      // Let modified clicks alone; they are the user asking their OS for
      // something specific, and none of them navigate this window.
      if (e.defaultPrevented || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) {
        return;
      }

      const anchor = (e.target as Element | null)?.closest?.("a");
      const href = anchor?.getAttribute("href");
      if (!href) return;

      // In-page anchors and same-document links are not navigation away.
      if (href.startsWith("#")) return;

      const isExternal = /^https?:\/\//i.test(href);
      if (!isExternal) return;

      e.preventDefault();
      void invoke("open_external", { url: href }).catch((err) => {
        console.error("open_external failed:", err);
      });
    };

    document.addEventListener("click", onClick, true);
    return () => document.removeEventListener("click", onClick, true);
  }, []);
}
