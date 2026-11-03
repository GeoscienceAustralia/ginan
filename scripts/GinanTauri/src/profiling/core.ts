/**
 * Stands in for `@tauri-apps/api/core` in a `--mode profiling` build (aliased
 * in vite.config.ts), so every invoke() in the app is timed without touching
 * a call site. Tauri's own invoke is read-only and cannot be wrapped at run
 * time.
 *
 * Measured from the page: argument serialisation, the backend's work, and the
 * response coming back.
 */
import { invoke as rawInvoke } from "../../node_modules/@tauri-apps/api/core.js";

export * from "../../node_modules/@tauri-apps/api/core.js";

export const invokes = new Map<string, { n: number; ms: number; max: number }>();

export function invoke<T>(...call: Parameters<typeof rawInvoke>): Promise<T> {
  const cmd = call[0];
  // The plugins' own traffic (log lines, profiler control) is not the app's.
  if (cmd.startsWith("plugin:")) return rawInvoke<T>(...call);
  const t0 = performance.now();
  return rawInvoke<T>(...call).finally(() => {
    const dt = performance.now() - t0;
    const s = invokes.get(cmd) ?? { n: 0, ms: 0, max: 0 };
    s.n += 1;
    s.ms += dt;
    s.max = Math.max(s.max, dt);
    invokes.set(cmd, s);
  });
}
