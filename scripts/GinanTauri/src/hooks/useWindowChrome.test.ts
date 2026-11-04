import { describe, expect, it } from "vitest";
import { applyTitlebarStyle, detectPlatform, titlebarStyleFor } from "./useWindowChrome";

const nav = (platform: string, userAgent = "") => ({ platform, userAgent });

describe("detectPlatform", () => {
  it("reads macOS, Windows and Linux from the platform string", () => {
    expect(detectPlatform(nav("MacIntel"))).toBe("macos");
    expect(detectPlatform(nav("Win32"))).toBe("windows");
    expect(detectPlatform(nav("Linux x86_64"))).toBe("linux");
  });

  it("falls back to the user agent when the platform is blank", () => {
    expect(detectPlatform(nav("", "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0)"))).toBe("macos");
    expect(detectPlatform(nav("", "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))).toBe("windows");
    expect(detectPlatform(nav("", "Mozilla/5.0 (X11; Linux x86_64)"))).toBe("linux");
  });
});

describe("titlebarStyleFor", () => {
  it("draws its own caption buttons on Windows and Linux", () => {
    expect(titlebarStyleFor("windows", true)).toBe("custom");
    expect(titlebarStyleFor("linux", true)).toBe("custom");
  });

  it("overlays the native traffic lights on macOS", () => {
    expect(titlebarStyleFor("macos", true)).toBe("overlay");
  });

  it("leaves the chrome to the browser outside Tauri", () => {
    for (const p of ["windows", "linux", "macos"] as const) {
      expect(titlebarStyleFor(p, false)).toBe("native");
    }
  });
});

describe("applyTitlebarStyle", () => {
  it("tags the root so the CSS can reserve room for the controls", () => {
    const root = document.createElement("html");
    applyTitlebarStyle(root);
    // jsdom is not Tauri, so the module-level style is "native".
    expect(root).toHaveAttribute("data-titlebar", "native");
  });
});
