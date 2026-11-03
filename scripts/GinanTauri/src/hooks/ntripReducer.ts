import type { NtripConnection } from "../types/streams";

/** Mountpoint key: PEA writes `/ALIC00AUS0` in `StreamName`, decoded JSON has no slash. */
const mountOf = (name: string) => name.replace(/^\/+/, "");

/**
 * Fold one `ntrip_stats.jsonl` line (PEA's real schema) into the per-mount
 * connection table. Pure; returns the same object when the line is not about a
 * stream we can name.
 *
 *   connecting -(serverResponse 200)-> streaming -(Error|Disconnected)-> reconnecting(n)
 *   serverResponse != 200 -> failed (a later 200 recovers it)
 *
 * `Connected` fires at TCP connect, before the HTTP status, so it never means
 * "streaming". `retryCount` resets on 200.
 */
export function reduceNtripEvent(
  state: Record<string, NtripConnection>,
  payload: unknown,
  now: number,
): Record<string, NtripConnection> {
  if (!payload || typeof payload !== "object") return state;
  const p = payload as Record<string, unknown>;
  const rawName = typeof p.StreamName === "string" ? p.StreamName : typeof p.Stream === "string" ? p.Stream : null;
  if (!rawName) return state;
  const mount = mountOf(rawName);
  if (!mount) return state;

  const prev: NtripConnection = state[mount] ?? {
    mount,
    state: "idle",
    retryCount: 0,
    lastHttpStatus: null,
    lastError: null,
    connectedSince: null,
    lastEventAt: now,
    chunksReceived: null,
  };
  const next: NtripConnection = { ...prev, lastEventAt: now };

  if (p.label === "serverResponse" && typeof p.ServerStatus === "number") {
    next.lastHttpStatus = p.ServerStatus;
    if (p.ServerStatus === 200) {
      next.state = "streaming";
      next.retryCount = 0;
      next.lastError = null;
      next.connectedSince = now;
    } else {
      next.state = "failed";
      next.lastError = `HTTP ${p.ServerStatus}`;
    }
    return { ...state, [mount]: next };
  }

  switch (p.MessageType) {
    case "Connected":
      // Keep reconnecting/failed until the HTTP status says otherwise.
      if (prev.state === "idle" || prev.state === "streaming") next.state = "connecting";
      break;
    case "Error":
    case "Disconnected": {
      if (p.MessageType === "Error" && typeof p.Error === "string") next.lastError = p.Error;
      // PEA usually writes Error then Disconnected for one drop: count once.
      const sameDrop = prev.state === "reconnecting" && now - prev.lastEventAt < 1000;
      if (!sameDrop) next.retryCount = prev.retryCount + 1;
      if (prev.state !== "failed") next.state = "reconnecting";
      break;
    }
    case "ChunkReceived":
      next.chunksReceived = typeof p.ChunksReceived === "number" ? p.ChunksReceived : (prev.chunksReceived ?? 0) + 1;
      break;
    default:
      return state;
  }
  return { ...state, [mount]: next };
}
