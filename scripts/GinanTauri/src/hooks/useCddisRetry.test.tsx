import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { act, renderHook } from "@testing-library/react";
import { useCddisRetry, useIsOnline } from "./useCddisRetry";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, mockInvoke } from "../test/tauriMocks";

const POLL_MS = 10_000;

beforeEach(() => {
  resetAppStore();
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

function disconnected() {
  useAppStore.setState({ cddisStatus: "error", cddisStatusMessage: "CDDIS unreachable: timeout" });
}

/** Let the awaited invoke settle without advancing the clock. */
const settle = () => act(async () => {});

describe("useCddisRetry", () => {
  it("does not poll while CDDIS is reachable", async () => {
    mockInvoke({ check_cddis_connectivity: "ok" });
    renderHook(() => useCddisRetry());

    await act(async () => vi.advanceTimersByTime(POLL_MS * 3));

    expect(invokeCount("check_cddis_connectivity")).toBe(0);
  });

  it("keeps polling while the check fails, and stays down", async () => {
    mockInvoke({ check_cddis_connectivity: () => Promise.reject("CDDIS unreachable") });
    disconnected();
    const epoch = store().cddisEpoch;
    renderHook(() => useCddisRetry());

    await act(async () => vi.advanceTimersByTime(POLL_MS));
    await act(async () => vi.advanceTimersByTime(POLL_MS));

    expect(invokeCount("check_cddis_connectivity")).toBe(2);
    expect(store().cddisEpoch).toBe(epoch);
  });

  it("re-runs the product chain once connectivity returns", async () => {
    mockInvoke({ check_cddis_connectivity: "ok" });
    disconnected();
    const epoch = store().cddisEpoch;
    renderHook(() => useCddisRetry());

    await act(async () => vi.advanceTimersByTime(POLL_MS));

    expect(store().cddisEpoch).toBe(epoch + 1);
  });

  it("retries straight away when the OS says the network is back", async () => {
    mockInvoke({ check_cddis_connectivity: "ok" });
    disconnected();
    renderHook(() => useCddisRetry());

    act(() => void window.dispatchEvent(new Event("online")));
    await settle();

    expect(invokeCount("check_cddis_connectivity")).toBe(1);
    expect(store().cddisEpoch).toBe(1);
  });

  it("stops polling once the status recovers", async () => {
    mockInvoke({ check_cddis_connectivity: () => Promise.reject("down") });
    disconnected();
    renderHook(() => useCddisRetry());

    act(() => useAppStore.setState({ cddisStatus: "ok" }));
    await act(async () => vi.advanceTimersByTime(POLL_MS * 3));

    expect(invokeCount("check_cddis_connectivity")).toBe(0);
  });

  it("reports a manual retry as in flight until it settles", async () => {
    let resolve!: () => void;
    mockInvoke({
      check_cddis_connectivity: () => new Promise<void>((r) => (resolve = r)),
    });
    disconnected();
    const { result } = renderHook(() => useCddisRetry());

    act(() => void result.current.retry());
    expect(result.current.retrying).toBe(true);

    await act(async () => resolve());
    expect(result.current.retrying).toBe(false);
  });

  it("does not double up on a check that is already running", async () => {
    mockInvoke({ check_cddis_connectivity: "ok" });
    useAppStore.setState({ cddisStatus: "checking" });
    const { result } = renderHook(() => useCddisRetry());

    await act(async () => result.current.retry());

    expect(invokeCount("check_cddis_connectivity")).toBe(0);
  });
});

describe("useIsOnline", () => {
  it("tracks the browser's online and offline events", () => {
    const { result } = renderHook(() => useIsOnline());
    expect(result.current).toBe(navigator.onLine);

    act(() => void window.dispatchEvent(new Event("offline")));
    expect(result.current).toBe(false);
    act(() => void window.dispatchEvent(new Event("online")));
    expect(result.current).toBe(true);
  });
});
