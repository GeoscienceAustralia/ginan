/** Number/time formatting shared by the Streams views. "–" stands for no data. */
export const DASH = "–";

export function fmtBytes(n: number | null | undefined): { value: string; unit: string } {
  if (n == null) return { value: DASH, unit: "" };
  if (n < 1024) return { value: String(Math.round(n)), unit: "B" };
  if (n < 1024 ** 2) return { value: (n / 1024).toFixed(1), unit: "kB" };
  if (n < 1024 ** 3) return { value: (n / 1024 ** 2).toFixed(2), unit: "MB" };
  return { value: (n / 1024 ** 3).toFixed(2), unit: "GB" };
}

export function fmtRate(n: number | null | undefined): { value: string; unit: string } {
  const b = fmtBytes(n);
  return { value: b.value, unit: b.unit ? `${b.unit}/s` : "" };
}

export const joinUnit = (x: { value: string; unit: string }) => (x.unit ? `${x.value} ${x.unit}` : x.value);

export function fmtClock(totalS: number | null | undefined): string {
  if (totalS == null) return "--:--:--";
  const s = Math.max(0, Math.floor(totalS));
  const p = (n: number) => String(n).padStart(2, "0");
  return `${p(Math.floor(s / 3600))}:${p(Math.floor((s % 3600) / 60))}:${p(s % 60)}`;
}

/** Unix ms to hh:mm:ss, read as UTC-like GPST. */
export function fmtTimeOfDay(ms: number | null | undefined): string {
  return ms == null ? DASH : new Date(ms).toISOString().slice(11, 19);
}

export function fmtAgo(ms: number | null | undefined): string {
  if (ms == null) return DASH;
  if (ms < 60_000) return `${(ms / 1000).toFixed(1)} s ago`;
  const m = Math.floor(ms / 60_000);
  return m < 60 ? `${m} min ago` : `${Math.floor(m / 60)} h ago`;
}

export const fmtCount = (n: number | null | undefined) => (n == null ? DASH : n.toLocaleString("en-US"));

export const fmtPerSec = (n: number | null | undefined) =>
  n == null ? DASH : n >= 10 ? n.toFixed(0) : n >= 1 ? n.toFixed(1) : n.toFixed(2);
