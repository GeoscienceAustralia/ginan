/**
 * Global test setup: browser polyfills first, then the Tauri module mocks.
 *
 * Order matters. `appStore.ts` reads `window.matchMedia` and `localStorage` at
 * module-evaluation time, so the polyfills below have to be installed before
 * any application module is imported — which is why they are statements in this
 * file rather than something a test opts into.
 */
import "@testing-library/jest-dom/vitest";
import { afterEach, beforeEach, vi } from "vitest";
import { cleanup } from "@testing-library/react";
import { resetTauriMocks } from "./tauriMocks";

/* ── browser APIs jsdom does not implement ──────────────────────────────── */

if (!window.matchMedia) {
  Object.defineProperty(window, "matchMedia", {
    writable: true,
    configurable: true,
    value: vi.fn((query: string) => ({
      matches: false,
      media: query,
      onchange: null,
      addListener: vi.fn(),
      removeListener: vi.fn(),
      addEventListener: vi.fn(),
      removeEventListener: vi.fn(),
      dispatchEvent: vi.fn(),
    })),
  });
}

// The charts observe their own box (chartUtils.useElementSize). jsdom lays
// nothing out, so the stub simply never fires — components fall back to the
// synchronous getBoundingClientRect measurement, which tests can stub.
if (!("ResizeObserver" in window)) {
  class ResizeObserverStub {
    observe() {}
    unobserve() {}
    disconnect() {}
  }
  Object.defineProperty(window, "ResizeObserver", {
    writable: true,
    configurable: true,
    value: ResizeObserverStub,
  });
}

// jsdom's rAF is tied to a frame clock that never advances under fake timers.
// A setTimeout(0) shim makes the buffered console/telemetry flushes observable
// by awaiting a macrotask, which is what the event tests do.
Object.defineProperty(window, "requestAnimationFrame", {
  writable: true,
  configurable: true,
  value: (cb: FrameRequestCallback) =>
    setTimeout(() => cb(performance.now()), 0) as unknown as number,
});
Object.defineProperty(window, "cancelAnimationFrame", {
  writable: true,
  configurable: true,
  value: (id: number) => clearTimeout(id as unknown as ReturnType<typeof setTimeout>),
});

// jsdom has no canvas backend: getContext() returns null and logs a "not
// implemented" error. The charts and Leaflet's canvas renderer only draw into
// it, so a context whose every method is a no-op is enough for them to mount.
// Tests that care what was drawn can spy on HTMLCanvasElement.prototype.getContext.
function makeCanvasContextStub(canvas: HTMLCanvasElement): CanvasRenderingContext2D {
  const fields: Record<string | symbol, unknown> = {
    canvas,
    measureText: (text: string) => ({ width: String(text).length * 6 }),
    getImageData: (_x: number, _y: number, w: number, h: number) => ({
      width: w,
      height: h,
      data: new Uint8ClampedArray(Math.max(0, w * h * 4)),
    }),
    createLinearGradient: () => ({ addColorStop: () => {} }),
    createRadialGradient: () => ({ addColorStop: () => {} }),
    createPattern: () => null,
    getLineDash: () => [],
    getTransform: () => ({ a: 1, b: 0, c: 0, d: 1, e: 0, f: 0 }),
    isPointInPath: () => false,
    isPointInStroke: () => false,
  };
  return new Proxy(fields, {
    get(target, prop) {
      if (prop in target) return target[prop];
      return () => {};
    },
    set(target, prop, value) {
      target[prop] = value;
      return true;
    },
  }) as unknown as CanvasRenderingContext2D;
}
Object.defineProperty(HTMLCanvasElement.prototype, "getContext", {
  writable: true,
  configurable: true,
  value: function getContext(this: HTMLCanvasElement) {
    return makeCanvasContextStub(this);
  },
});
Object.defineProperty(HTMLCanvasElement.prototype, "toDataURL", {
  writable: true,
  configurable: true,
  value: () => "data:image/png;base64,",
});

// Used by the tour and the console to bring an element into view.
if (!Element.prototype.scrollIntoView) {
  Element.prototype.scrollIntoView = function scrollIntoView() {};
}

// Pointer capture is used by the resizable/draggable viz controls.
for (const name of ["setPointerCapture", "releasePointerCapture"] as const) {
  if (!(name in Element.prototype)) {
    Object.defineProperty(Element.prototype, name, {
      writable: true,
      configurable: true,
      value: () => {},
    });
  }
}
if (!("hasPointerCapture" in Element.prototype)) {
  Object.defineProperty(Element.prototype, "hasPointerCapture", {
    writable: true,
    configurable: true,
    value: () => false,
  });
}

if (!navigator.clipboard) {
  Object.defineProperty(navigator, "clipboard", {
    writable: true,
    configurable: true,
    value: { writeText: vi.fn(async () => undefined) },
  });
}

/* ── Tauri modules ──────────────────────────────────────────────────────── */

vi.mock("@tauri-apps/api/core", async () => {
  const m = await import("./tauriMocks");
  return { invoke: m.invoke, isTauri: m.isTauri };
});
vi.mock("@tauri-apps/api/app", async () => {
  const m = await import("./tauriMocks");
  return { getVersion: m.getVersion, getTauriVersion: m.getTauriVersion };
});
vi.mock("@tauri-apps/api/event", async () => {
  const m = await import("./tauriMocks");
  return { listen: m.listen, emit: m.emit };
});
vi.mock("@tauri-apps/api/window", async () => {
  const m = await import("./tauriMocks");
  return { getCurrentWindow: m.getCurrentWindow };
});
vi.mock("@tauri-apps/api/webview", async () => {
  const m = await import("./tauriMocks");
  return { getCurrentWebview: m.getCurrentWebview };
});
vi.mock("@tauri-apps/plugin-dialog", async () => {
  const m = await import("./tauriMocks");
  return { open: m.open, save: m.save };
});
vi.mock("@tauri-apps/plugin-store", async () => {
  const m = await import("./tauriMocks");
  return { load: m.storeLoad, Store: class {} };
});

beforeEach(() => {
  localStorage.clear();
  document.documentElement.removeAttribute("data-theme");
  resetTauriMocks();
});

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
