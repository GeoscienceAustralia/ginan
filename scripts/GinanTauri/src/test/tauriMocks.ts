/**
 * In-memory stand-ins for every Tauri surface the frontend touches.
 *
 * The app never runs against a real backend in tests, so each module is mocked
 * once in `setupTests.ts` and steered from individual tests through the helpers
 * here. Everything is reset between tests by `resetTauriMocks`, which is what
 * lets a suite assume a clean store file and no registered listeners.
 */
import { vi } from "vitest";

/* ── invoke ─────────────────────────────────────────────────────────────── */

export type InvokeHandler = (
  cmd: string,
  args: Record<string, unknown> | undefined,
) => unknown | Promise<unknown>;

let invokeHandler: InvokeHandler = (cmd) => {
  throw new Error(
    `invoke("${cmd}") called without a handler — set one with setInvokeHandler() or mockInvoke()`,
  );
};

export function setInvokeHandler(handler: InvokeHandler) {
  invokeHandler = handler;
}

/**
 * What a mocked command returns: either a fixed value, or a function of the
 * arguments. Spelled out as a union rather than `unknown` so an inline handler
 * gets its `args` typed instead of falling back to implicit `any`.
 */
export type CommandStub =
  | ((args: Record<string, unknown>) => unknown)
  | string
  | number
  | boolean
  | null
  | undefined
  | object;

/**
 * Route commands by name. Unlisted commands reject, which keeps a test honest
 * about which backend calls its subject actually makes.
 */
export function mockInvoke(handlers: Record<string, CommandStub>) {
  setInvokeHandler((cmd, args) => {
    if (!(cmd in handlers)) return Promise.reject(new Error(`unmocked command: ${cmd}`));
    const entry = handlers[cmd];
    return typeof entry === "function" ? entry(args ?? {}) : entry;
  });
}

export const invoke = vi.fn(async (cmd: string, args?: Record<string, unknown>) =>
  invokeHandler(cmd, args),
);

/**
 * `isTauri()` — false by default, since jsdom is a plain browser. Tests that
 * need the in-Tauri branch flip it with `isTauri.mockReturnValue(true)`; note
 * that `useWindowChrome` reads it once at module load, so those tests also have
 * to re-import the module (`vi.resetModules()`).
 */
export const isTauri = vi.fn(() => false);

/** Arguments of the last `invoke` call for `cmd`, or undefined if never called. */
export function lastInvokeArgs(cmd: string): Record<string, unknown> | undefined {
  for (let i = invoke.mock.calls.length - 1; i >= 0; i--) {
    if (invoke.mock.calls[i][0] === cmd) return invoke.mock.calls[i][1];
  }
  return undefined;
}

export function invokeCount(cmd: string): number {
  return invoke.mock.calls.filter((c) => c[0] === cmd).length;
}

/* ── events ─────────────────────────────────────────────────────────────── */

type Listener<T> = (event: { payload: T }) => void;
const listeners = new Map<string, Set<Listener<unknown>>>();

export const listen = vi.fn(async <T,>(event: string, cb: Listener<T>) => {
  if (!listeners.has(event)) listeners.set(event, new Set());
  listeners.get(event)!.add(cb as Listener<unknown>);
  return () => {
    listeners.get(event)?.delete(cb as Listener<unknown>);
  };
});

/** Deliver a backend event to everything currently subscribed. */
export function emit<T>(event: string, payload: T) {
  for (const l of listeners.get(event) ?? []) (l as Listener<T>)({ payload });
}

export function listenerCount(event: string): number {
  return listeners.get(event)?.size ?? 0;
}

/* ── window / webview ───────────────────────────────────────────────────── */

export const setTitle = vi.fn(async (_title: string) => undefined);
export const minimize = vi.fn(async () => undefined);
export const toggleMaximize = vi.fn(async () => undefined);
export const close = vi.fn(async () => undefined);
export const startDragging = vi.fn(async () => undefined);
export const isMaximized = vi.fn(async () => false);
export const isFullscreen = vi.fn(async () => false);

let resizedListener: (() => void) | null = null;
export const onResized = vi.fn(async (cb: () => void) => {
  resizedListener = cb;
  return () => {
    if (resizedListener === cb) resizedListener = null;
  };
});

/** Fire a window resize at whatever `useWindowState` subscribed. */
export function emitResized() {
  resizedListener?.();
}

export const getCurrentWindow = vi.fn(() => ({
  setTitle,
  minimize,
  toggleMaximize,
  close,
  startDragging,
  isMaximized,
  isFullscreen,
  onResized,
}));

/* ── app (@tauri-apps/api/app) ──────────────────────────────────────────── */

export const getVersion = vi.fn(async () => "0.0.0-test");
export const getTauriVersion = vi.fn(async () => "2.0.0-test");

type DragDropPayload =
  | { type: "enter" | "over"; paths: string[]; position: { x: number; y: number } }
  | { type: "leave" }
  | { type: "drop"; paths: string[]; position: { x: number; y: number } };

let dragDropListener: ((e: { payload: DragDropPayload }) => void) | null = null;
export const onDragDropEvent = vi.fn(async (cb: (e: { payload: DragDropPayload }) => void) => {
  dragDropListener = cb;
  return () => {
    if (dragDropListener === cb) dragDropListener = null;
  };
});
export const getCurrentWebview = vi.fn(() => ({ onDragDropEvent }));

/** Fire a Tauri drag-drop event at whatever `useRinexDrop` subscribed. */
export function emitDragDrop(payload: DragDropPayload) {
  dragDropListener?.({ payload });
}

export function hasDragDropListener(): boolean {
  return dragDropListener !== null;
}

/* ── dialogs ────────────────────────────────────────────────────────────── */

export const open = vi.fn(async (_opts?: unknown) => null as string | string[] | null);
export const save = vi.fn(async (_opts?: unknown) => null as string | null);

/* ── persistent store (@tauri-apps/plugin-store) ────────────────────────── */

const storeData = new Map<string, unknown>();

export const storeLoad = vi.fn(async (_file?: string, _opts?: unknown) => ({
  get: vi.fn(async (key: string) => storeData.get(key)),
  set: vi.fn(async (key: string, value: unknown) => {
    storeData.set(key, value);
  }),
  delete: vi.fn(async (key: string) => storeData.delete(key)),
  save: vi.fn(async () => undefined),
  entries: vi.fn(async () => [...storeData.entries()]),
}));

/** Pre-populate `app-settings.json` as if a previous session had written it. */
export function seedStore(values: Record<string, unknown>) {
  for (const [k, v] of Object.entries(values)) storeData.set(k, v);
}

export function readStore(key: string): unknown {
  return storeData.get(key);
}

export function storeHas(key: string): boolean {
  return storeData.has(key);
}

export function clearStore() {
  storeData.clear();
}

/* ── reset ──────────────────────────────────────────────────────────────── */

export function resetTauriMocks() {
  invokeHandler = (cmd) => {
    throw new Error(`invoke("${cmd}") called without a handler`);
  };
  invoke.mockClear();
  listen.mockClear();
  open.mockClear();
  save.mockClear();
  storeLoad.mockClear();
  isTauri.mockReset();
  isTauri.mockImplementation(() => false);
  setTitle.mockClear();
  for (const fn of [minimize, toggleMaximize, close, startDragging, onResized]) fn.mockClear();
  isMaximized.mockReset();
  isMaximized.mockImplementation(async () => false);
  isFullscreen.mockReset();
  isFullscreen.mockImplementation(async () => false);
  resizedListener = null;
  getVersion.mockReset();
  getVersion.mockImplementation(async () => "0.0.0-test");
  getTauriVersion.mockReset();
  getTauriVersion.mockImplementation(async () => "2.0.0-test");
  getCurrentWindow.mockClear();
  getCurrentWebview.mockClear();
  onDragDropEvent.mockClear();
  dragDropListener = null;
  listeners.clear();
  clearStore();
}
