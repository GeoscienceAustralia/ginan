import { useCallback, useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";

/**
 * How often to re-check CDDIS while it is unreachable.
 *
 * The `online` event below already covers the common case — a laptop rejoining
 * wifi reconnects within a moment of the OS noticing. This poll is the backstop
 * for the cases that event does not fire for: the machine is online but CDDIS
 * itself is down, a captive portal, or a VPN coming up. Short enough to feel
 * automatic, long enough not to hammer a NASA server while someone is at lunch.
 */
const POLL_MS = 10_000;

/**
 * Keeps trying CDDIS after a failed connectivity check, and exposes a manual
 * retry.
 *
 * The check in App runs once per products directory, so without this a drop
 * in connectivity would leave `cddisStatus` stuck at "error" for the rest of
 * the session: the internet could come back and the app would never notice,
 * and there would be no way to ask it to look again short of a restart.
 *
 * On success this bumps `cddisEpoch` rather than setting status directly, which
 * re-runs the whole startup chain in App — connectivity, static products, ANTEX
 * — so a recovered connection picks up the work it missed instead of only
 * clearing the banner.
 */
export function useCddisRetry() {
  const cddisStatus = useAppStore((s) => s.cddisStatus);
  const bumpCddisEpoch = useAppStore((s) => s.bumpCddisEpoch);
  const [retrying, setRetrying] = useState(false);

  const retry = useCallback(async () => {
    // A poll firing while the user's own click is still in flight would double
    // up the request and flicker the button.
    if (useAppStore.getState().cddisStatus === "checking") return;
    setRetrying(true);
    try {
      await invoke<string>("check_cddis_connectivity");
      bumpCddisEpoch();
    } catch {
      // Still down. The banner stays up and the next poll tries again; there
      // is nothing useful to say that it is not already saying.
    } finally {
      setRetrying(false);
    }
  }, [bumpCddisEpoch]);

  // Poll only while disconnected. Clearing the interval on success is what
  // stops this running for the rest of the session.
  useEffect(() => {
    if (cddisStatus !== "error") return;
    const id = setInterval(() => void retry(), POLL_MS);
    return () => clearInterval(id);
  }, [cddisStatus, retry]);

  // The OS telling us the network is back is both faster and more reliable
  // than waiting out the poll interval.
  useEffect(() => {
    if (cddisStatus !== "error") return;
    const onOnline = () => void retry();
    window.addEventListener("online", onOnline);
    return () => window.removeEventListener("online", onOnline);
  }, [cddisStatus, retry]);

  return { retry, retrying };
}

/**
 * Whether the machine itself has a network, tracked live.
 *
 * Lets the banner say "no internet" when that is the actual problem, rather
 * than blaming CDDIS for the user's wifi.
 */
export function useIsOnline(): boolean {
  const [online, setOnline] = useState(() => navigator.onLine);
  useEffect(() => {
    const up = () => setOnline(true);
    const down = () => setOnline(false);
    window.addEventListener("online", up);
    window.addEventListener("offline", down);
    return () => {
      window.removeEventListener("online", up);
      window.removeEventListener("offline", down);
    };
  }, []);
  return online;
}
