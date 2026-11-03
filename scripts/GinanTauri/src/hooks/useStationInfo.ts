import { useCallback, useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { resultsDir, useAppStore } from "../store/appStore";
import type { StationInfo } from "../types/streams";

const REFRESH_MS = 30_000;

/** Reference-station facts decoded from the recorded RTCM (1005-1008, 1033) for one mount. */
export function useStationInfo(mount: string) {
  const outputDir = useAppStore(resultsDir);
  const isProcessing = useAppStore((s) => s.isProcessing);
  const [info, setInfo] = useState<StationInfo | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [tick, setTick] = useState(0);
  const refresh = useCallback(() => setTick((n) => n + 1), []);

  useEffect(() => {
    if (!outputDir || !mount) {
      setInfo(null);
      return;
    }
    let cancelled = false;
    setLoading(true);
    invoke<StationInfo | null>("get_station_info", { outputDir, mount })
      .then((r) => {
        if (cancelled) return;
        setInfo(r);
        setError(null);
      })
      .catch((e) => !cancelled && setError(String(e)))
      .finally(() => !cancelled && setLoading(false));
    return () => {
      cancelled = true;
    };
  }, [outputDir, mount, tick]);

  useEffect(() => {
    if (!isProcessing) return;
    const id = setInterval(refresh, REFRESH_MS);
    return () => clearInterval(id);
  }, [isProcessing, refresh]);

  return { info, loading, error, refresh };
}
