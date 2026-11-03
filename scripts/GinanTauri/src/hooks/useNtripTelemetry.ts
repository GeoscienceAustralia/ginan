import { useMemo } from "react";
import { useAppStore } from "../store/appStore";
import type { NtripConnection } from "../types/streams";

export { reduceNtripEvent } from "./ntripReducer";

/**
 * Per-mount NTRIP connection state, sorted by mount. The `ntrip-stats`
 * listener lives at app level (useTauriEvents) and folds events into the store,
 * so this is a plain selector and survives tab switches.
 */
export function useNtripTelemetry(): NtripConnection[] {
  const conns = useAppStore((s) => s.ntripConnections);
  return useMemo(() => Object.values(conns).sort((a, b) => a.mount.localeCompare(b.mount)), [conns]);
}
