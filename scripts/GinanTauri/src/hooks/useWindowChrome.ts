import { useEffect, useState } from "react";
import { isTauri } from "@tauri-apps/api/core";
import { getCurrentWindow } from "@tauri-apps/api/window";

/**
 * Window chrome — the app draws its own titlebar (the Header) instead of the
 * OS one, so the window reads as one surface rather than a grey strip over it.
 *
 * How much of the titlebar is ours depends on the platform, and is fixed by
 * the Tauri config:
 *   • Windows / Linux — `decorations: false` (tauri.conf.json). There is no
 *     native titlebar at all, so the app draws minimise / maximise / close on
 *     the right (WindowControls).
 *   • macOS — `titleBarStyle: "Overlay"` with a hidden title
 *     (tauri.macos.conf.json). The native traffic lights stay, floating over
 *     the top-left of the webview, so the header content shifts right to clear
 *     them.
 *   • Plain browser (vite dev without Tauri) — the browser is the chrome;
 *     nothing is drawn and nothing is inset.
 *
 * Dragging and double-click-to-maximise come from `data-tauri-drag-region` on
 * the header; Tauri's injected script handles both, with the right
 * per-platform double-click timing.
 */

export type ChromePlatform = "macos" | "windows" | "linux";

/**
 * - "custom":  undecorated window, the app draws the caption buttons.
 * - "overlay": native traffic lights over the webview (macOS).
 * - "native":  not in Tauri, so no window chrome is ours to draw.
 */
export type TitlebarStyle = "custom" | "overlay" | "native";

/**
 * Best-effort OS sniff from the webview. Each platform's webview reports its
 * host OS in the user agent (WKWebView "Macintosh", WebView2 "Windows",
 * WebKitGTK "Linux"/"X11"), which is all the titlebar needs — not worth the
 * os plugin for one branch.
 */
export function detectPlatform(
  nav: Pick<Navigator, "userAgent" | "platform"> = navigator,
): ChromePlatform {
  const platform = nav.platform ?? "";
  const ua = nav.userAgent ?? "";
  if (/mac/i.test(platform) || /Macintosh|Mac OS X/.test(ua)) return "macos";
  if (/win/i.test(platform) || /Windows/.test(ua)) return "windows";
  return "linux";
}

export function titlebarStyleFor(platform: ChromePlatform, inTauri: boolean): TitlebarStyle {
  if (!inTauri) return "native";
  return platform === "macos" ? "overlay" : "custom";
}

/** The style for this window. Fixed for the life of the page. */
export const titlebarStyle: TitlebarStyle = titlebarStyleFor(detectPlatform(), isTauri());

/**
 * Tag <html> with the titlebar style so the CSS can reserve room for the
 * window controls (see `--titlebar-inset-*` in App.css). Called once from
 * main.tsx, before the first render, so the header never paints under them.
 */
export function applyTitlebarStyle(root: HTMLElement = document.documentElement) {
  root.setAttribute("data-titlebar", titlebarStyle);
}

export interface WindowState {
  maximized: boolean;
  fullscreen: boolean;
  /** Native caption buttons dim while the window is in the background. */
  focused: boolean;
}

/**
 * Live maximised / fullscreen / focus state of the current window. Outside
 * Tauri it stays at the defaults, since there is no window to ask.
 */
export function useWindowState(): WindowState {
  const [size, setSize] = useState({ maximized: false, fullscreen: false });
  const [focused, setFocused] = useState(() => document.hasFocus());

  useEffect(() => {
    const onFocus = () => setFocused(true);
    const onBlur = () => setFocused(false);
    window.addEventListener("focus", onFocus);
    window.addEventListener("blur", onBlur);
    return () => {
      window.removeEventListener("focus", onFocus);
      window.removeEventListener("blur", onBlur);
    };
  }, []);

  useEffect(() => {
    if (!isTauri()) return;
    const win = getCurrentWindow();
    let cancelled = false;
    let unlisten: (() => void) | undefined;

    // Maximising, restoring and entering fullscreen all resize the window, and
    // so does an OS snap, so one listener covers every way the state changes.
    const sync = async () => {
      try {
        const [maximized, fullscreen] = await Promise.all([win.isMaximized(), win.isFullscreen()]);
        if (!cancelled) setSize({ maximized, fullscreen });
      } catch {
        /* missing permission — keep the last known state */
      }
    };

    void sync();
    win
      .onResized(() => void sync())
      .then((fn) => {
        if (cancelled) fn();
        else unlisten = fn;
      })
      .catch(() => {});

    return () => {
      cancelled = true;
      unlisten?.();
    };
  }, []);

  return { ...size, focused };
}
