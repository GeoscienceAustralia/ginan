import { beforeEach, describe, expect, it } from "vitest";
import { act, renderHook } from "@testing-library/react";
import { reduceNtripEvent, useNtripTelemetry } from "./useNtripTelemetry";
import { resetAppStore, store } from "../test/storeHelpers";
import type { NtripConnection } from "../types/streams";

beforeEach(() => resetAppStore());

// Real ntrip_stats.jsonl lines from PEA (ntripTrace.cpp / tcpSocket.cpp).
const connected = { StreamName: "/ALIC00AUS0", MessageType: "Connected", Time: "2026-01-01 00:00:00", DisonnectCount: 0 };
const ok = { label: "serverResponse", Stream: "ALIC00AUS0", Time: "t", ServerStatus: 200, VersionHTTP: "HTTP/1.1" };
const unauth = { ...ok, ServerStatus: 401 };
const disconnected = { StreamName: "/ALIC00AUS0", MessageType: "Disconnected", Time: "t", DisonnectCount: 1 };
const error = { StreamName: "/ALIC00AUS0", MessageType: "Error", Error: "Connection reset", Time: "t" };
const chunk = { StreamName: "/ALIC00AUS0", MessageType: "ChunkReceived", Time: "t", ChunksReceived: 7 };

function run(events: unknown[], t0 = 1000, step = 5000) {
  let s: Record<string, NtripConnection> = {};
  events.forEach((e, i) => (s = reduceNtripEvent(s, e, t0 + i * step)));
  return s.ALIC00AUS0;
}

describe("reduceNtripEvent", () => {
  it("connects, then streams only after HTTP 200 and strips the leading slash", () => {
    expect(run([connected]).state).toBe("connecting");
    const c = run([connected, ok]);
    expect(c).toMatchObject({ mount: "ALIC00AUS0", state: "streaming", lastHttpStatus: 200, retryCount: 0, connectedSince: 6000 });
  });

  it("goes reconnecting with a retry count on Disconnected, and resets on 200", () => {
    const c = run([connected, ok, disconnected, connected, disconnected]);
    expect(c.state).toBe("reconnecting");
    expect(c.retryCount).toBe(2);
    expect(run([connected, ok, disconnected, connected, ok]).retryCount).toBe(0);
  });

  it("counts an Error+Disconnected pair from one drop once and keeps the error text", () => {
    let s = reduceNtripEvent({}, ok, 0);
    s = reduceNtripEvent(s, error, 10_000);
    s = reduceNtripEvent(s, disconnected, 10_100);
    expect(s.ALIC00AUS0).toMatchObject({ state: "reconnecting", retryCount: 1, lastError: "Connection reset" });
  });

  it("fails on a non-200 response and stays failed through the retry loop", () => {
    const c = run([connected, unauth, disconnected, connected, unauth]);
    expect(c).toMatchObject({ state: "failed", lastHttpStatus: 401, lastError: "HTTP 401" });
  });

  it("recovers from failed on a later 200", () => {
    expect(run([connected, unauth, disconnected, connected, ok]).state).toBe("streaming");
  });

  it("tracks chunks without changing state and ignores unnamed or unknown lines", () => {
    expect(run([connected, ok, chunk]).chunksReceived).toBe(7);
    expect(run([connected, ok, chunk]).state).toBe("streaming");
    const s = {};
    expect(reduceNtripEvent(s, { MessageType: "Connected" }, 1)).toBe(s);
    expect(reduceNtripEvent(s, null, 1)).toBe(s);
    expect(reduceNtripEvent(s, { StreamName: "/X", MessageType: "ChunkSent" }, 1)).toBe(s);
  });
});

describe("store wiring", () => {
  it("applyNtripEvent feeds the hook and resetStreamCounters zeroes retries", async () => {
    const { result } = renderHook(() => useNtripTelemetry());
    expect(result.current).toEqual([]);
    act(() => {
      for (const e of [ok, disconnected]) store().applyNtripEvent(e);
    });
    expect(result.current[0]).toMatchObject({ mount: "ALIC00AUS0", state: "reconnecting", retryCount: 1 });
  });
});
