/** Small formatters and colour lookups shared by the per-stream views. */
import type { MessageView, StreamRole, StreamStatus } from "../../types/streams";

const pad = (n: number, w = 2) => String(Math.trunc(n)).padStart(w, "0");

/** hh:mm:ss (+ centiseconds) of an epoch in ms, read as UTC-like GPST. */
export function fmtClock(ms: number | null | undefined, frac = false): string {
  if (ms == null) return "–";
  const d = new Date(ms);
  const base = `${pad(d.getUTCHours())}:${pad(d.getUTCMinutes())}:${pad(d.getUTCSeconds())}`;
  return frac ? `${base}.${pad(d.getUTCMilliseconds() / 10)}` : base;
}

export function fmtAgo(s: number | null | undefined): string {
  if (s == null) return "–";
  if (s < 1) return "just now";
  if (s < 90) return `${Math.round(s)} s ago`;
  if (s < 5400) return `${Math.round(s / 60)} min ago`;
  return `${Math.round(s / 3600)} h ago`;
}

export function fmtPeriod(s: number | null | undefined): string {
  return s == null ? "–" : `${s.toFixed(s < 10 ? 2 : 1)} s`;
}

export function fmtBytes(n: number | null | undefined): string {
  if (n == null) return "–";
  const u = ["B", "kB", "MB", "GB"];
  let i = 0;
  while (n >= 1000 && i < u.length - 1) {
    n /= 1000;
    i++;
  }
  return `${n.toFixed(i === 0 ? 0 : i === 1 ? 1 : 2)} ${u[i]}`;
}

export const fmtRate = (n: number | null | undefined) =>
  n == null ? "–" : `${fmtBytes(n)}/s`;

export const fmtPct = (x: number) => `${(100 * x).toFixed(1)}%`;

export const ROLE_TITLE: Record<StreamRole, string> = {
  OBS: "Observations",
  CORR: "SSR corrections",
  NAV: "Broadcast ephemeris",
};

export const STATUS_LABEL: Record<StreamStatus, string> = {
  connecting: "Connecting",
  streaming: "Connected",
  receiving: "Receiving",
  reconnecting: "Reconnecting",
  stale: "Stale",
  failed: "Failed",
  idle: "Idle",
};

/** Tone drives the colour of status text and dot (see streams-detail.css). */
export function statusTone(s: StreamStatus): "ok" | "warn" | "bad" | "idle" {
  if (s === "streaming" || s === "receiving") return "ok";
  if (s === "failed") return "bad";
  if (s === "idle") return "idle";
  return "warn";
}

export const GROUP_LABEL: Record<MessageView["group"], string> = {
  msm: "Observations · MSM",
  ephemeris: "Broadcast ephemeris",
  ssr: "SSR corrections",
  station: "Station + antenna",
  bias: "Biases",
  other: "Other",
};

/** CSS class suffix selecting the colour pair of a message (bg/ink vars in streams-detail.css). */
export function msgTone(m: Pick<MessageView, "group" | "constellation">): string {
  const c = m.constellation ?? "X";
  if (m.group === "msm" || m.group === "ephemeris") return `${m.group}-${c}`;
  return m.group;
}
