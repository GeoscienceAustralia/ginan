import type { ProfilerOnRenderCallback } from "react";

/**
 * Render cost per <Profiler id>, accumulated until the profiling HUD reads and
 * clears it. React only calls this in dev and profiling builds; a normal
 * production build strips Profiler timing, so this never runs there.
 */
export const renders = new Map<string, { n: number; ms: number; max: number }>();

export const onRender: ProfilerOnRenderCallback = (id, _phase, actualDuration) => {
  const r = renders.get(id) ?? { n: 0, ms: 0, max: 0 };
  r.n += 1;
  r.ms += actualDuration;
  r.max = Math.max(r.max, actualDuration);
  renders.set(id, r);
};
