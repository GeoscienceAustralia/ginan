import type { PosFileInfo, TraceFileInfo } from "../../types";
import { formatShortDateTime } from "../../utils/time";

/**
 * The file to show when nothing is selected yet: the most recently written
 * forward solution, then any solution. Only files with epochs are candidates —
 * the caller lists nothing else. The backend lists files by name, so its first
 * entry is often an old run rather than the one just produced.
 */
export function pickDefaultPos(files: PosFileInfo[]): string | null {
  const newest = (list: PosFileInfo[]) =>
    list.slice().sort((a, b) => (b.modified_ms ?? 0) - (a.modified_ms ?? 0))[0];
  const pick = newest(files.filter((f) => !f.smoothed)) ?? newest(files);
  return pick?.path ?? null;
}

/**
 * Dropdown label. The toolbar's select is narrow and every file of a run
 * shares a long name prefix, so the parts that tell files apart — forward,
 * smoothed or SBAS SPP, and when it was written — come first, and the name last. The
 * write time is a wall-clock "when did I run this", so it follows the time
 * zone setting.
 */
export function posFileLabel(f: PosFileInfo, timeZone: string): string {
  const kind = f.format === "spp" ? "SBAS SPP" : f.smoothed ? "Smoothed" : "Forward";
  return [kind, f.modified_ms ? formatShortDateTime(f.modified_ms, timeZone) : null, stem(f.name), formatBytes(f.size)]
    .filter(Boolean)
    .join(" · ");
}

export function traceFileLabel(f: TraceFileInfo, timeZone: string): string {
  const smoothed = /_smoothed/i.test(f.name);
  return [smoothed ? "Smoothed" : null, f.modified_ms ? formatShortDateTime(f.modified_ms, timeZone) : null, stem(f.name), formatBytes(f.size)]
    .filter(Boolean)
    .join(" · ");
}

/** The file name without its extension or `_smoothed` marker, which the label already says. */
function stem(name: string): string {
  return name.replace(/\.[^.]+$/, "").replace(/_smoothed$/i, "");
}

function formatBytes(n: number): string {
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${Math.round(n / 1024)} KB`;
  return `${(n / (1024 * 1024)).toFixed(1)} MB`;
}
