import { useEffect, useRef } from "react";
import { useTauriEvents } from "./hooks/useTauriEvents";
import { useBiaValidation } from "./hooks/useBiaValidation";
import { useCatalogueRescan } from "./hooks/usePickers";
import { useAppTitle } from "./hooks/useAppTitle";
import { useAppStore, loadPersistedConfig } from "./store/appStore";
import type { TourKey } from "./store/appStore";
import { invoke } from "@tauri-apps/api/core";
import type { PeaBinaryStatus } from "./types";
import ConfigPanel from "./components/ConfigPanel";
import Workspace from "./components/Workspace";
import StatusBar from "./components/StatusBar";
import CddisCredentialsModal from "./components/CddisCredentialsModal";
import PeaUpdateModal from "./components/PeaUpdateModal";
import CustomPeaModal from "./components/CustomPeaModal";
import SettingsView from "./components/SettingsView";
import { useExternalLinks } from "./hooks/useExternalLinks";
import ActivityOverlay from "./components/ActivityOverlay";
import Onboarding from "./components/Onboarding";
import Tour from "./components/Tour";
import WindowControls from "./components/WindowControls";
import { tourForCurrentShell } from "./components/tourSteps";
import { WF } from "./config/workflowSteps";


/** Last path segment, for the filename-only ANTEX workflow lines. */
function basename(path: string): string {
  return path.split(/[\\/]/).pop() ?? path;
}

export default function App() {
  const hydrate = useAppStore((s) => s.hydrate);
  const setStatus = useAppStore((s) => s.setStatus);
  const setOfflineMode = useAppStore((s) => s.setOfflineMode);
  const setCredentialsModalOpen = useAppStore((s) => s.setCredentialsModalOpen);
  const setCddisStatus = useAppStore((s) => s.setCddisStatus);
  const setAtxPath = useAppStore((s) => s.setAtxPath);
  const appendWorkflowLine = useAppStore((s) => s.appendWorkflowLine);
  const productsDir = useAppStore((s) => s.productsDir);
  const cddisEpoch = useAppStore((s) => s.cddisEpoch);
  const setPeaBinaryPath = useAppStore((s) => s.setPeaBinaryPath);
  const setPeaVersion = useAppStore((s) => s.setPeaVersion);
  const setPeaUpdateAvailable = useAppStore((s) => s.setPeaUpdateAvailable);
  // Nothing restores the RINEX on startup, so every launch begins with the
  // welcome screen in the workspace column (see Workspace).
  const rinexData = useAppStore((s) => s.rinexData);
  const inputMode = useAppStore((s) => s.inputMode);
  const onboardingStatus = useAppStore((s) => s.onboardingStatus);
  const completedTours = useAppStore((s) => s.completedTours);
  const activeTour = useAppStore((s) => s.activeTour);
  const activity = useAppStore((s) => s.activity);
  const startTour = useAppStore((s) => s.startTour);
  useTauriEvents();
  // Outbound links go to the system browser, never this window.
  useExternalLinks();
  // BIA fetch / RINEX∩BIA validation. Mount exactly once.
  useBiaValidation();
  // Re-scan the PPP catalogue when credentials become valid. Mount exactly once.
  useCatalogueRescan();
  // Keep the window title in sync with the loaded RINEX. Mount exactly once.
  useAppTitle();
  const theme = useAppStore((s) => s.theme);

  useEffect(() => {
    document.documentElement.setAttribute("data-theme", theme);
  }, [theme]);

  // Validate Earthdata/CDDIS credentials once on startup. If absent or invalid,
  // open the credentials modal so the user can sign in or create a free
  // Earthdata account, and drop into offline mode until they do. CDDIS is the
  // only product source, so without credentials no downloads are possible.
  //
  // Held until the persisted onboarding flag has been read ("unknown"), and
  // skipped entirely on a first run: onboarding asks for the same credentials
  // itself, and throwing this modal over that screen — or straight after the
  // user has just chosen to skip it — would be asking twice.
  const onboardingOwnsCredentials = useRef(false);
  useEffect(() => {
    if (onboardingStatus === "active") onboardingOwnsCredentials.current = true;
    if (onboardingStatus !== "done") return;
    if (onboardingOwnsCredentials.current) return;

    invoke<{ ok: boolean; where: string | null; why: string | null }>(
      "validate_cddis_credentials",
    )
      .then((status) => {
        // The same pair of lines GinanUI opens with, so the Workflow tab
        // starts with a record of whether Earthdata credentials were found.
        if (status.ok) {
          appendWorkflowLine(WF.credentialsFound(status.where ?? "netrc"));
          return;
        }
        appendWorkflowLine(WF.credentialsInvalid(status.why ?? "unknown"));
        appendWorkflowLine(WF.noCredentials());
        setOfflineMode(true);
        setCredentialsModalOpen(true);
      })
      .catch((e) => {
        // If the check itself cannot run, assume the worst rather than leaving
        // the app silently presenting as online.
        console.error("validate_cddis_credentials failed:", e);
        appendWorkflowLine(WF.credentialsInvalid(String(e)));
        setOfflineMode(true);
      });
  }, [onboardingStatus]);

  // Second half of the tour. Most of the window — sidebar, tabs, run controls,
  // status bar — does not exist until the shell is up, so the walk through it
  // waits for that, and for the opening activity panel to clear so the
  // spotlight isn't fighting a scrim. Offered once; `completedTours` remembers.
  //
  // The shell has two ways in and each gets its own tour: a parsed RINEX, or
  // realtime mode picked with no file at all. It cannot gate on `rinexData`
  // alone — switching to realtime clears the parsed file (see setInputMode).
  // `completedTours` tracks them separately, so someone who has seen the
  // file tour is still offered the streaming one the first time they stream.
  //
  // Walking out of a tour sticks on its own: endTour records it as seen
  // whether it was completed or dismissed, so the gate below skips it for good
  // afterwards. `autoStartedTours` stays as a same-render safety net, keyed by
  // tour so a just-started tour cannot re-trigger before its state settles,
  // without suppressing the other mode's tour.
  const autoStartedTours = useRef(new Set<TourKey>());
  useEffect(() => {
    if (!rinexData && inputMode !== "realtime") return;
    if (onboardingStatus !== "done") return;
    if (activeTour || activity !== null) return;
    // Never "welcome" here: the guard above only lets a live shell through.
    const tour = tourForCurrentShell(inputMode, rinexData !== null);
    if (autoStartedTours.current.has(tour)) return;
    if (completedTours.includes(tour)) return;
    autoStartedTours.current.add(tour);
    startTour(tour);
  }, [
    rinexData,
    inputMode,
    onboardingStatus,
    activeTour,
    activity,
    completedTours,
    startTour,
  ]);

  // Startup: CDDIS connectivity check → static products → load ATX.
  // Gated on `productsDir` so we wait for the persisted config to hydrate
  // (productsDir defaults to "" pre-hydrate). We run this once per productsDir
  // change so a user picking a new directory re-triggers the chain.
  useEffect(() => {
    if (!productsDir) return;
    // A credential-change epoch can mean explicit sign-out as well as
    // sign-in. Do not start another authenticated probe after removal.
    if (useAppStore.getState().cddisChecksPaused) return;
    let cancelled = false;

    (async () => {
      setCddisStatus("checking", null);
      // GinanUI reports how long the check took, so time it the same way.
      const startedAt = Date.now();
      try {
        await invoke<string>("check_cddis_connectivity");
        if (cancelled) return;
        setCddisStatus("ok", null);
        // CDDIS is reachable and authenticated, so we are definitively not
        // offline — clears the flag even if the user dismissed the modal.
        setOfflineMode(false);
        appendWorkflowLine(
          WF.connectivityPassed(((Date.now() - startedAt) / 1000).toFixed(2)),
        );
      } catch (e) {
        if (cancelled) return;
        const msg = String(e);
        setCddisStatus("error", msg);
        // A failed connectivity check means no product downloads at all —
        // CDDIS is the only source, so there is no mirror to fall back to.
        setOfflineMode(true);
        appendWorkflowLine(WF.connectivityFailed(msg));
        // Do not auto-download static products when CDDIS auth is broken —
        // the user must fix creds first via the CredentialsModal.
        appendWorkflowLine(WF.metadataSkippedOffline());
        return;
      }

      // GinanUI calls this set "metadata". Its per-file detail belongs on the
      // Console channel; only the outcome is a Workflow step.
      try {
        await invoke("ensure_products", { productsDir });
        if (cancelled) return;
        appendWorkflowLine(WF.metadataDownloaded());
        const state = useAppStore.getState();
        // Progress events leave the last file at 100%. Only replace our idle
        // download message, never an opening activity or a running PEA status.
        if (!state.isProcessing && !state.activity && state.status.startsWith("Downloading ")) {
          setStatus(state.peaBinaryPath
            ? `pea ${state.peaVersion ?? ""} ready`.replace("  ", " ")
            : "Support files ready - select a PEA binary in Settings");
        }
      } catch (e) {
        if (cancelled) return;
        appendWorkflowLine(WF.metadataFailed(String(e)));
        const state = useAppStore.getState();
        if (!state.isProcessing && !state.activity) setStatus(`Support files unavailable: ${String(e)}`);
        return;
      }

      // There is no manual ANTEX override — Ginan resolves the antenna model
      // itself, so this always re-detects from the products directory. The
      // path is still used to validate the RINEX antenna type.
      try {
        const path = await invoke<string | null>("find_antex_file", { productsDir });
        if (cancelled) return;
        setAtxPath(path);
        if (path) {
          appendWorkflowLine(WF.atxSelected(basename(path), "priority"));
        } else {
          appendWorkflowLine(WF.atxMissing());
        }
      } catch (e) {
        if (cancelled) return;
        appendWorkflowLine(WF.warning(String(e)));
      }
    })();

    return () => { cancelled = true; };
  }, [productsDir, cddisEpoch]);

  useEffect(() => {
    // `active` prevents React StrictMode's double-invocation from running this
    // startup logic twice (which would trigger two scans, two set_pea_binary
    // calls, and duplicate console output).
    let active = true;

    // The observation file is deliberately not restored — every launch starts
    // on the welcome screen and the user opens a file for this session. Only
    // machine-level setup (directories, product selection, pea binary) is
    // carried over.
    loadPersistedConfig().then(async (config) => {
      if (!active) return;
      hydrate(config);

      // Read the settable parameters out of the PEA config template so the
      // settings page reflects the engine actually installed, rather than a
      // list hardcoded when this was written. Non-fatal — the curated table
      // stands in if it fails.
      void useAppStore.getState().loadDiscoveredParams();

      // Reapply a previously chosen binary (a custom pick or a prior download)
      // so it stays authoritative across restarts. If the path no longer exists,
      // set_pea_binary rejects it and we fall through to the managed download.
      if (config.peaBinaryPath) {
        try {
          await invoke("set_pea_binary", { path: config.peaBinaryPath });
        } catch {
          // Stale/invalid saved path — ignore and let ensure resolve a default.
        }
      }

      // Resolve the pea binary. An existing binary is reused as-is — the app
      // never auto-updates — but if a newer release exists the user is prompted
      // (status bar + Settings) to switch. Only a first run with no binary at
      // all triggers an automatic download.
      setStatus("Checking pea binary...");
      try {
        const info = await invoke<PeaBinaryStatus>("ensure_default_pea_binary");
        setPeaBinaryPath(info.path);
        setPeaVersion(info.version);
        setPeaUpdateAvailable(info.update_available);
        if (info.update_available) {
          // Prompt the user with a popup (the app never auto-updates). The
          // status-bar pill reopens it if they dismiss it.
          useAppStore.getState().setPeaUpdateModalOpen(true);
          setStatus(
            `pea ${info.version ?? "installed"} ready - ${info.update_available} available`,
          );
        } else if (info.version) {
          setStatus(`pea ${info.version} ready`);
        } else {
          setStatus("Ready");
        }
        if (info.source === "override") {
          // A binary the app doesn't manage, so its version is unknown and the
          // user would otherwise assume they're on the latest release. Told
          // once per path, so an existing local install isn't raised every launch.
          const s = useAppStore.getState();
          s.setLastCustomPeaPath(info.path);
          s.setManagedPeaVersion(info.managed_version ?? null);
          if (s.customPeaNoticeSeen !== info.path) s.setCustomPeaModalOpen(true);
        }
      } catch (e) {
        console.error("ensure_default_pea_binary failed:", e);
        setStatus(`Pea binary unavailable: ${e}`);
      }

      // Libraries PEA links but a machine may lack (libomp on macOS): fetched
      // now so the first run or stream test does not die before it starts.
      // The backend reports progress and failure in the Console, and retries
      // before each run.
      void invoke("ensure_pea_runtime").catch((e) => console.error("ensure_pea_runtime failed:", e));
    }).catch((e) => {
      console.error("startup failed:", e);
      setStatus(`Could not read saved settings: ${e}`);
    });

    return () => { active = false; };
  }, []);

  return (
    <div className="app">
      {/* Sidebar runs the full height of the window, app mark at its top; the
          workspace column to its right carries the Header with the tabs, or
          the welcome screen until a file is open. */}
      <div className="app-body">
        <ConfigPanel />
        <Workspace />
      </div>
      <StatusBar />
      <CddisCredentialsModal />
      <PeaUpdateModal />
      <CustomPeaModal />
      <SettingsView />
      {/* Above the whole shell: opening a file starts on the welcome screen and
          finishes on the tabs. */}
      <ActivityOverlay />
      {/* The tour points at the live UI, so it goes over everything; first-run
          setup covers the lot, tour included, until it is finished. */}
      <Tour />
      <Onboarding />
      {/* The window's own caption buttons, over every layer above — see the
          component for why they are not inside the Header. */}
      <WindowControls />
    </div>
  );
}
