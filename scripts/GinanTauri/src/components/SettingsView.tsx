import { useEffect, useMemo, useState } from "react";
import { useAppStore } from "../store/appStore";
import type { SettingsPageKey } from "../store/appStore";
import PathControl from "./PathControl";
import ConfigYamlInspector from "./ConfigYamlInspector";
import CodePrioritiesSummary from "./CodePrioritiesSummary";
import NtripCasterSettings from "./NtripCasterSettings";
import CddisSignOut from "./CddisSignOut";
import SisnetLoginSettings from "./SisnetLoginSettings";
import { tourForCurrentShell } from "./tourSteps";
import { ConfirmDialog } from "./ModalDialog";
import { switchToFile, switchToLive } from "../hooks/useProductAdd";
import { isForeignPath } from "./runGate";
import ProcessingParamControl from "./ProcessingParamControl";
import PeaBinaryCards from "./PeaBinaryCards";
import TimeZoneControl from "./TimeZoneControl";
import { mergeParamGroups, defsByPath, changedParams } from "../config/processingParams";
import { usePickers } from "../hooks/usePickers";
import { useConfigIo } from "../hooks/useConfigIo";
import { deviceTimeZone, timeZoneAbbr, timeZoneOffsetLabel } from "../utils/time";
import {
  ABOUT_LINKS,
  PROJECT_COPYRIGHT,
  PROJECT_LICENSE,
  THIRD_PARTY_NOTICES,
  LICENSE_TEXTS,
} from "../config/about";
import type { ConfigIoFeedback, ConfigIoRow } from "../hooks/useConfigIo";
import {
  PackageIcon,
  ChipIcon,
  KeyIcon,
  SunIcon,
  SearchIcon,
  ChevronLeftIcon,
  ChevronRightIcon,
  ShieldIcon,
  AlertIcon,
  CheckIcon,
  FileIcon,
  OutputIcon,
  ImportIcon,
  ExportIcon,
  ResetIcon,
  HelpIcon,
  InfoIcon,
  LinkIcon,
} from "./icons";

/**
 * Full-window settings surface: page list on the left, the selected page's
 * content on the right.
 *
 * This owns the machine-level setup: the products cache, the output directory,
 * the pea binary, the CDDIS credentials and appearance. Anything tied to a
 * particular run — the RINEX input and the choice of output formats — lives in
 * the sidebar next to the options it affects.
 *
 * Pages are built as data (`Page[] → Group[] → Row[]`) rather than bespoke JSX
 * per page so the search box can filter rows across every page uniformly.
 */

type Row = {
  /** Stable id — also the search key alongside title/desc. */
  id: string;
  title: string;
  /** Small element rendered inline just after the title (e.g. a version tag). */
  badge?: React.ReactNode;
  desc?: string;
  control: React.ReactNode;
  /**
   * Render the control on its own line below the text, at full width, instead
   * of beside it. For controls that are panels rather than single widgets —
   * the YAML inspector would be unreadable squeezed into the control column.
   */
  wide?: boolean;
  /**
   * Give the control column more of the row than the usual cap allows, and let
   * it shrink with the window instead of overflowing. For controls that are a
   * single widget but a wide one — a filename template is 41 characters.
   */
  roomy?: boolean;
};

type Group = {
  label?: string;
  rows: Row[];
  /** Fold the group's rows away behind its label, collapsed to start with.
   *  For the long run of parameter groups on Configuration, which are
   *  cumbersome to scroll past. Needs a label to hang the control on. */
  collapsible?: boolean;
  /** Render each row's control alone, outside the usual card, for pages that
   *  lay themselves out (PEA Binary's choice cards). */
  bare?: boolean;
};

/**
 * Outcome of the last import / export / reset, shown against the row whose
 * button was pressed. Renders nothing until there is something to say.
 */
function ConfigIoResult({
  row,
  feedback,
}: {
  row: ConfigIoRow;
  feedback: ConfigIoFeedback | null;
}) {
  if (!feedback || feedback.row !== row) return null;
  return (
    <span
      className={`status-pill status-pill--${feedback.tone}`}
      role="status"
      title={feedback.detail}
    >
      {feedback.tone === "ok" ? <CheckIcon size={14} /> : <AlertIcon size={14} />}
      {feedback.text}
    </span>
  );
}

/**
 * An outbound link. A plain anchor is enough: `useExternalLinks` catches every
 * http(s) click at the document and hands it to the system browser, so the
 * webview never navigates away from the app.
 */
function ExternalLink({ href, children }: { href: string; children: React.ReactNode }) {
  return (
    <a className="settings-link" href={href} target="_blank" rel="noreferrer noopener">
      <LinkIcon size={13} />
      {children}
    </a>
  );
}

/**
 * Full licence texts for the bundled third-party code, collapsed by default —
 * they have to be present in the distributed app, not read on every visit.
 */
function LicenseTexts() {
  return (
    <details className="about-licenses">
      <summary>Show licence texts</summary>
      <pre>
        {THIRD_PARTY_NOTICES.map((n) =>
          [`${n.name} (${n.license})`, n.copyright, "", LICENSE_TEXTS[n.text]].join("\n"),
        ).join(`\n\n${"-".repeat(72)}\n\n`)}
      </pre>
    </details>
  );
}

type Page = {
  key: SettingsPageKey;
  label: string;
  title: string;
  /** Small element rendered just after the page heading (e.g. a status tag). */
  badge?: React.ReactNode;
  icon: React.ReactNode;
  /** Renders a hairline above this nav item, splitting the list into blocks. */
  startsBlock?: boolean;
  groups: Group[];
};

export default function SettingsView() {
  const settingsOpen = useAppStore((s) => s.settingsOpen);
  const settingsPage = useAppStore((s) => s.settingsPage);
  const setSettingsOpen = useAppStore((s) => s.setSettingsOpen);
  const setSettingsPage = useAppStore((s) => s.setSettingsPage);

  const productsDir = useAppStore((s) => s.productsDir);
  const outputDir = useAppStore((s) => s.outputDir);
  const rinexData = useAppStore((s) => s.rinexData);
  const inputMode = useAppStore((s) => s.inputMode);
  const isProcessing = useAppStore((s) => s.isProcessing);
  // The run mode the user asked to switch to, waiting on the confirm dialog.
  const [pendingMode, setPendingMode] = useState<"file" | "realtime" | null>(null);
  const peaBinaryPath = useAppStore((s) => s.peaBinaryPath);
  const peaVersion = useAppStore((s) => s.peaVersion);
  // Only managed downloads carry a release tag, so no tag means custom.
  const customPea = !!peaBinaryPath && !peaVersion;
  const theme = useAppStore((s) => s.theme);
  const timeZone = useAppStore((s) => s.timeZone);
  const offlineMode = useAppStore((s) => s.offlineMode);
  const cddisStatus = useAppStore((s) => s.cddisStatus);
  const cddisStatusMessage = useAppStore((s) => s.cddisStatusMessage);

  const processingParams = useAppStore((s) => s.processingParams);
  const discoveredParams = useAppStore((s) => s.discoveredParams);
  const resetAllProcessingParams = useAppStore((s) => s.resetAllProcessingParams);

  // Curated definitions plus anything else found in the PEA config template, so
  // a parameter added by a newer PEA appears here without a frontend change.
  const paramGroups = useMemo(
    () => mergeParamGroups(discoveredParams),
    [discoveredParams],
  );

  const setTheme = useAppStore((s) => s.setTheme);
  const setCredentialsModalOpen = useAppStore((s) => s.setCredentialsModalOpen);
  const startTour = useAppStore((s) => s.startTour);
  const restartOnboarding = useAppStore((s) => s.restartOnboarding);

  // Count only values that actually differ from the template default — a stored
  // key set back to its default is not a modification.
  const paramDefs = useMemo(() => defsByPath(paramGroups), [paramGroups]);
  const modifiedParamCount = Object.keys(
    changedParams(processingParams, paramDefs),
  ).length;
  const totalParamCount = Object.keys(paramDefs).length;

  const { pickProductsDir, pickOutputDir } =
    usePickers();
  const { importConfig, exportConfig, resetConfig, feedback, clearFeedback } = useConfigIo();

  const [query, setQuery] = useState("");

  // Which collapsible groups the user has opened, keyed by page and label.
  // Component state, so it survives closing and reopening Settings within the
  // session but not a restart.
  const [openGroups, setOpenGroups] = useState<Record<string, boolean>>({});
  const toggleGroup = (key: string) =>
    setOpenGroups((o) => ({ ...o, [key]: !o[key] }));

  // Escape closes the settings surface — but not while the credentials modal is
  // open over it, or Escape would close the layer the user isn't looking at.
  useEffect(() => {
    if (!settingsOpen) return;
    const onKey = (e: KeyboardEvent) => {
      if (e.key !== "Escape") return;
      if (useAppStore.getState().credentialsModalOpen) return;
      setSettingsOpen(false);
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [settingsOpen, setSettingsOpen]);

  // Clear a stale query — and a stale result message — when reopening, so
  // settings always opens on a full list and never on the outcome of something
  // the user did before they last closed it. Keyed on `settingsOpen` alone on
  // purpose: it should run on open/close, not whenever the clear helpers change.
  useEffect(() => {
    if (settingsOpen) {
      setQuery("");
      clearFeedback();
    }
  }, [settingsOpen]);

  // ── Page definitions ─────────────────────────────────────────────────────

  const pages: Page[] = useMemo(() => {
    const credentialState = offlineMode
      ? { icon: <AlertIcon size={14} />, tone: "warn", text: cddisStatusMessage ?? "Earthdata sign-in required - downloads unavailable" }
      : cddisStatus === "ok"
      ? { icon: <CheckIcon size={14} />, tone: "ok", text: "Connected to CDDIS" }
      : cddisStatus === "checking"
      ? { icon: <ShieldIcon size={14} />, tone: "", text: "Checking CDDIS..." }
      : cddisStatus === "idle"
      ? { icon: <ShieldIcon size={14} />, tone: "", text: "Not checked yet, set a products directory" }
      : { icon: <AlertIcon size={14} />, tone: "warn", text: cddisStatusMessage ?? "Credentials missing or invalid" };

    return [
      {
        key: "config",
        label: "Configuration",
        title: "Configuration",
        icon: <FileIcon />,
        groups: [
          {
            rows: [
              {
                id: "run-mode",
                title: "Run mode",
                desc:
                  "A file run processes RINEX observation files; a real-time run reads " +
                  "live data streams. Adding a file or a stream from the sidebar's + " +
                  "switches too. Switching clears the other mode's inputs.",
                control: (
                  <div className="segmented" role="group" aria-label="Run mode">
                    {(["file", "realtime"] as const).map((m) => (
                      <button
                        key={m}
                        className={inputMode === m ? "is-active" : ""}
                        aria-pressed={inputMode === m}
                        disabled={isProcessing && inputMode !== m}
                        title={
                          isProcessing && inputMode !== m
                            ? "Stop processing to switch between a file run and a real-time run"
                            : undefined
                        }
                        onClick={() => inputMode !== m && setPendingMode(m)}
                      >
                        {m === "file" ? "File" : "Real-time"}
                      </button>
                    ))}
                  </div>
                ),
              },
            ],
          },
          {
            rows: [
              {
                id: "import-config",
                title: "Import config",
                desc:
                  "Load settings from a PEA YAML file. Directories are left as they are " +
                  "here; the RINEX is reloaded if its path still resolves on this machine.",
                control: (
                  <div className="settings-inline">
                    <ConfigIoResult row="import-config" feedback={feedback} />
                    <button className="btn-secondary" onClick={importConfig}>
                      <ImportIcon /> Import...
                    </button>
                  </div>
                ),
              },
              {
                id: "export-config",
                title: "Export config",
                desc:
                  "Write the config a run on this machine would use, paths included. " +
                  "Caster logins are left as <USER>:<PASS>.",
                control: (
                  <div className="settings-inline">
                    <ConfigIoResult row="export-config" feedback={feedback} />
                    <button className="btn-secondary" onClick={exportConfig}>
                      <ExportIcon /> Export...
                    </button>
                  </div>
                ),
              },
              {
                id: "reset-config",
                title: "Reset config",
                desc: "Put processing mode, time window and output formats back to their defaults. Directories, credentials and the loaded RINEX are left alone.",
                control: (
                  <div className="settings-inline">
                    <ConfigIoResult row="reset-config" feedback={feedback} />
                    <button className="btn-danger" onClick={resetConfig}>
                      <ResetIcon /> Reset
                    </button>
                  </div>
                ),
              },
            ],
          },
          {
            rows: [
              {
                id: "processing-params-reset",
                title: "Processing parameters",
                desc: `${modifiedParamCount} of ${totalParamCount} changed from the template defaults. Anything not listed below stays at the template value and can still be hand-edited in the config YAML.`,
                control: (
                  <button
                    className="btn-secondary"
                    onClick={resetAllProcessingParams}
                    disabled={modifiedParamCount === 0}
                  >
                    <ResetIcon /> Reset all
                  </button>
                ),
              },
            ],
          },
          // Ordered to match the generated YAML, so the page reads top to
          // bottom like the file it produces — and lands on the inspector at
          // the end, showing the result of everything above it.
          ...paramGroups.map((group) => ({
            label: group.label,
            collapsible: true,
            rows: group.params.map((def) => ({
              id: def.path,
              title: def.label,
              desc: def.desc,
              // Filename templates are the long ones; a 170px box showed about
              // half of `<RECEIVER>_<CONFIG>_<YYYY><DDD><HH>.GPX`.
              roomy: def.type === "string",
              control: <ProcessingParamControl def={def} />,
            })),
          })),
          {
            label: "Generated config",
            collapsible: true,
            rows: [
              {
                id: "code-priorities",
                title: "Observation code priorities",
                desc:
                  "The signal list this config writes for each constellation, in priority " +
                  "order. Phase (L) signals only, which is what PEA's code_priorities takes. " +
                  "Change it under Constellations in the sidebar.",
                wide: true,
                control: <CodePrioritiesSummary />,
              },
              {
                id: "yaml-inspector",
                title: "PEA config YAML",
                desc: "The exact config a run would write. Turn off overwrite to hand-edit it for PEA options this UI does not expose, and keep those edits across runs.",
                wide: true,
                control: <ConfigYamlInspector />,
              },
            ],
          },
        ],
      },
      {
        key: "products",
        label: "Products Cache",
        title: "Products Cache",
        icon: <PackageIcon />,
        startsBlock: true,
        groups: [
          {
            rows: [
              {
                id: "products-dir",
                title: "Products directory",
                desc:
                  productsDir && isForeignPath(productsDir)
                    ? "This is not a path on this computer, so runs cannot use it. Choose a folder with Browse."
                    : "Local cache for SP3, CLK, BRDC, BIA and ANTEX support files.",
                control: (
                  <PathControl value={productsDir || null} empty="Not set" onBrowse={pickProductsDir} />
                ),
              },
            ],
          },
        ],
      },
      {
        key: "output",
        label: "Output",
        title: "Output",
        icon: <OutputIcon />,
        groups: [
          {
            rows: [
              {
                id: "output-dir",
                title: "Output directory",
                desc:
                  outputDir && isForeignPath(outputDir)
                    ? "This is not a path on this computer, probably from an imported config, so runs cannot write to it. Choose a folder with Browse."
                    : "Where PEA writes its results, and where the config for each run is " +
                      "saved. Which formats are written is set per run, in the sidebar.",
                control: (
                  <PathControl value={outputDir} empty="Not set" onBrowse={pickOutputDir} />
                ),
              },
            ],
          },
        ],
      },
      {
        key: "binary",
        label: "PEA Binary",
        title: "PEA Binary",
        // Managed or custom at a glance, before reading any row.
        badge: peaBinaryPath ? (
          <span className="settings-tag">{customPea ? "Custom" : "Managed"}</span>
        ) : undefined,
        icon: <ChipIcon />,
        groups: [
          {
            bare: true,
            rows: [
              {
                id: "pea-binary",
                // Never shown: the cards carry their own headings. Title and
                // description are kept for the settings search.
                title: "PEA binary version and location",
                desc: "Managed release or custom binary. Download a version, or override with your own pea build.",
                control: <PeaBinaryCards />,
              },
            ],
          },
        ],
      },
      {
        key: "credentials",
        label: "Credentials",
        title: "Credentials",
        icon: <KeyIcon />,
        groups: [
          {
            label: "NASA Earthdata",
            rows: [
              {
                id: "cddis-status",
                title: "CDDIS connection",
                desc: "CDDIS hosts the preferred PPP products. Credentials are stored locally in your netrc file.",
                control: (
                  <div className="settings-inline">
                    <span className={`status-pill${credentialState.tone ? ` status-pill--${credentialState.tone}` : ""}`}>
                      {credentialState.icon}
                      {credentialState.text}
                    </span>
                    <button className="btn-secondary" onClick={() => setCredentialsModalOpen(true)}>
                      Set credentials...
                    </button>
                    <CddisSignOut />
                  </div>
                ),
              },
            ],
          },
          // The caster login is a credential like the Earthdata one, so it
          // sits on the same page.
          {
            label: "NTRIP caster (real-time data streams)",
            rows: [
              {
                id: "ntrip-caster",
                title: "Caster",
                desc:
                  "The server your mountpoints live on, and the login for it. The " +
                  "username and password are stored locally in your netrc file and " +
                  "spliced into the connection only when a config is written - they " +
                  "never appear in an exported config. Browse streams lists the " +
                  "caster's mountpoints so the stream fields in the sidebar can " +
                  "suggest them.",
                wide: true,
                control: <NtripCasterSettings />,
              },
            ],
          },
          {
            label: "SISNet (SBAS corrections)",
            rows: [
              {
                id: "sisnet",
                title: "SouthPAN SISNet",
                desc:
                  "The login for SouthPAN's SBAS corrections, used when Corrections has SBAS. " +
                  "Stored locally in your netrc file and spliced in only when a config is " +
                  "written; it never appears in an exported config.",
                control: <SisnetLoginSettings />,
              },
            ],
          },
        ],
      },
      {
        key: "appearance",
        label: "Appearance",
        title: "Appearance",
        icon: <SunIcon />,
        startsBlock: true,
        groups: [
          {
            rows: [
              {
                id: "theme",
                title: "Theme",
                desc: "Applies immediately and is remembered between sessions.",
                control: (
                  <div className="segmented" role="group" aria-label="Theme">
                    <button
                      className={theme === "light" ? "is-active" : ""}
                      onClick={() => setTheme("light", true)}
                    >
                      Light
                    </button>
                    <button
                      className={theme === "dark" ? "is-active" : ""}
                      onClick={() => setTheme("dark", true)}
                    >
                      Dark
                    </button>
                  </div>
                ),
              },
            ],
          },
          {
            label: "Date & time",
            rows: [
              {
                id: "timezone",
                title: "Time zone",
                badge: (
                  <span className="settings-tag">
                    {timeZoneOffsetLabel(timeZone)}
                    {timeZone !== "UTC" ? ` · ${timeZoneAbbr(Date.now(), timeZone)}` : ""}
                  </span>
                ),
                desc:
                  "Every date, time and epoch in the app is shown in this zone - chart axes, " +
                  "tooltips and file times. This is for display only, and configs and output files stay in " +
                  "UTC/GPS time. Pick UTC to read epochs exactly as PEA writes them, or the " +
                  `locate button to use this device's timezone (${deviceTimeZone()}).`,
                control: <TimeZoneControl />,
              },
            ],
          },
        ],
      },
      {
        key: "help",
        label: "Help",
        title: "Help",
        icon: <HelpIcon />,
        groups: [
          {
            rows: [
              {
                id: "guided-tour",
                title: "Guided tour",
                desc:
                  "Walk through the window one section at a time. Which tour you get depends on what is on screen: " +
                  "with a file open it covers the sidebar, tabs and run controls, and a real-time data stream gets its own " +
                  "walk through the observation and correction streams and what a running stream writes.",
                control: (
                  <button
                    className="btn-secondary"
                    onClick={() =>
                      startTour(tourForCurrentShell(inputMode, rinexData !== null))
                    }
                  >
                    <HelpIcon /> Start tour
                  </button>
                ),
              },
              {
                id: "rerun-setup",
                title: "First-run setup",
                desc:
                  "Run the welcome, credentials and products-folder steps again. Nothing already configured is cleared, " +
                  "so each step starts from what you have now.",
                control: (
                  <button className="btn-secondary" onClick={restartOnboarding}>
                    <ResetIcon /> Run setup again
                  </button>
                ),
              },
            ],
          },
        ],
      },
      {
        key: "about",
        label: "About",
        title: "About",
        icon: <InfoIcon />,
        groups: [
          {
            rows: [
              {
                id: "about-ginan",
                title: "Ginan",
                desc:
                  "An open-source toolkit from Geoscience Australia for processing GNSS " +
                  "observations for geodetic applications - precise point positioning (PPP), " +
                  "orbit determination and atmospheric modelling. PEA, the Parameter " +
                  "Estimation Algorithm, is its processing engine; this app downloads and " +
                  "runs a released PEA binary rather than bundling one. This interface is part " +
                  "of Ginan and released with it.",
                control: (
                  <div className="settings-inline">
                    <ExternalLink href={ABOUT_LINKS.repo}>GitHub repository</ExternalLink>
                    <ExternalLink href={ABOUT_LINKS.docs}>Documentation</ExternalLink>
                  </div>
                ),
              },
              {
                id: "about-license",
                title: "Licence",
                badge: <span className="settings-tag">Apache-2.0</span>,
                desc: `${PROJECT_COPYRIGHT}. Licensed under the ${PROJECT_LICENSE}; distributed on an "as is" basis, without warranties or conditions of any kind.`,
                control: <ExternalLink href={ABOUT_LINKS.license}>Licence text</ExternalLink>,
              },
              {
                id: "about-issues",
                title: "Report an issue",
                desc: "Bugs in Ginan or in this interface go to Ginan's issue tracker.",
                control: <ExternalLink href={ABOUT_LINKS.issues}>GitHub issues</ExternalLink>,
              },
            ],
          },
          {
            label: "Third-party notices",
            rows: [
              ...THIRD_PARTY_NOTICES.map((n) => ({
                id: `notice-${n.name}`,
                title: n.name,
                badge: <span className="settings-tag">{n.license}</span>,
                desc: n.copyright.replace(/\n/g, " · "),
                control: <ExternalLink href={n.url}>Source</ExternalLink>,
              })),
              {
                id: "notice-osm",
                title: "OpenStreetMap",
                badge: <span className="settings-tag">ODbL</span>,
                desc:
                  "Map tiles and map data © OpenStreetMap contributors, available under the " +
                  "Open Database License. Credited on the map itself as well.",
                control: (
                  <ExternalLink href="https://www.openstreetmap.org/copyright">Copyright</ExternalLink>
                ),
              },
              {
                id: "notice-texts",
                title: "Licence texts",
                desc: "The licences above require their notices to travel with the app; here they are in full.",
                wide: true,
                control: <LicenseTexts />,
              },
            ],
          },
        ],
      },
    ];
    // Deliberately lists only the values the rows display; the handlers the
    // rows call are not tracked.
  }, [
    productsDir, outputDir, peaBinaryPath, peaVersion,
    rinexData, inputMode, isProcessing,
    theme, timeZone, offlineMode, cddisStatus, cddisStatusMessage,
    // The three config rows render it, so the pages have to be rebuilt when a
    // result arrives or expires.
    feedback,
  ]);

  if (!settingsOpen) return null;

  const normalise = (text: string) => text.toLowerCase().replace(/[-\u2010-\u2014\s]+/g, " ").trim();
  const q = normalise(query);
  const matchingRows = (g: Group) => g.rows.filter((r) =>
    !q || normalise(`${g.label ?? ""} ${r.title} ${r.desc ?? ""}`).includes(q));

  const pageMatchCount = (p: Page) =>
    p.groups.reduce((n, g) => n + matchingRows(g).length, 0);

  const active = pages.find((p) => p.key === settingsPage) ?? pages[0];

  // Searching flattens the nav: every page with a hit is rendered in one
  // scroll, each under its own page name, so results are never hidden behind
  // a page the user hasn't clicked.
  const shown: Page[] = q ? pages.filter((p) => pageMatchCount(p) > 0) : [active];

  return (
    <div className="settings-view" role="dialog" aria-modal="true" aria-label="Settings">
      {pendingMode && (
        <ConfirmDialog
          title={pendingMode === "file" ? "Switch to a file run?" : "Switch to a real-time run?"}
          message={
            pendingMode === "file"
              ? "A file run reads RINEX files instead of real-time data streams. Switching clears your stream setup."
              : "A real-time run reads data streams instead of a RINEX file. Switching closes the loaded file and clears its settings."
          }
          confirmLabel={pendingMode === "file" ? "Switch to file run" : "Switch to real-time run"}
          danger
          onConfirm={() => {
            if (pendingMode === "file") switchToFile();
            else switchToLive();
            setPendingMode(null);
          }}
          onCancel={() => setPendingMode(null)}
        />
      )}
      <aside className="settings-nav">
        {/* Clears the macOS traffic lights, which float over this corner; sized
            to nothing on the other platforms (see App.css). */}
        <div className="settings-titlebar" data-tauri-drag-region />
        <div className="settings-search-row">
          <button
            type="button"
            className="btn-icon settings-back"
            onClick={() => setSettingsOpen(false)}
            aria-label="Back"
            title="Back (Esc)"
          >
            <ChevronLeftIcon size={16} />
          </button>
          <div className="settings-search">
            <SearchIcon size={14} />
            <input
              type="text"
              value={query}
              placeholder="Search settings"
              aria-label="Search settings"
              onChange={(e) => setQuery(e.target.value)}
            />
          </div>
        </div>

        <nav className="settings-nav-list">
          {pages.map((p) => {
            const dimmed = q.length > 0 && pageMatchCount(p) === 0;
            return (
              <button
                key={p.key}
                className={
                  "settings-nav-item" +
                  (!q && p.key === active.key ? " is-active" : "") +
                  (dimmed ? " is-dimmed" : "") +
                  (p.startsBlock ? " starts-block" : "")
                }
                onClick={() => {
                  setQuery("");
                  setSettingsPage(p.key);
                }}
              >
                {p.icon}
                {p.label}
              </button>
            );
          })}
        </nav>
      </aside>

      <div className="settings-main">
        {/* Settings covers the app header, so this stands in as the titlebar. */}
        <header className="settings-header" data-tauri-drag-region>
          <h1 data-tauri-drag-region>
            {q ? `Results for “${query.trim()}”` : active.title}
            {!q && active.badge}
          </h1>
        </header>

        <div className="settings-scroll">
          {shown.length === 0 || shown.every((p) => pageMatchCount(p) === 0) ? (
            <p className="settings-empty">No settings match “{query.trim()}”.</p>
          ) : (
            shown.map((page) => (
              <div key={page.key}>
                {q && <div className="settings-page-label">{page.label}</div>}
                {page.groups.map((group, gi) => {
                  const rows = matchingRows(group);
                  if (rows.length === 0) return null;
                  // A search result is never hidden behind a collapsed
                  // heading: a group with a hit is shown open.
                  const groupKey = `${page.key}:${group.label ?? gi}`;
                  const collapsible = !!group.collapsible && !!group.label;
                  const open = !collapsible || !!q || !!openGroups[groupKey];
                  const bodyId = `settings-group-${page.key}-${gi}`;
                  return (
                    <section key={gi} className="settings-group">
                      {group.label && !collapsible && (
                        <div className="settings-group-label">{group.label}</div>
                      )}
                      {collapsible && (
                        <button
                          type="button"
                          className={`settings-group-label settings-group-toggle${open ? " is-open" : ""}`}
                          aria-expanded={open}
                          aria-controls={bodyId}
                          onClick={() => toggleGroup(groupKey)}
                        >
                          <ChevronRightIcon size={13} />
                          {group.label}
                          <span className="settings-group-count">
                            {rows.length}
                          </span>
                        </button>
                      )}
                      {!open ? null : group.bare ? (
                        rows.map((row) => <div key={row.id}>{row.control}</div>)
                      ) : (
                      <div className="settings-card" id={bodyId}>
                        {rows.map((row) => (
                          <div
                            key={row.id}
                            className={`settings-row${row.wide ? " is-wide" : ""}`}
                          >
                            <div className="settings-row-text">
                              <div className="settings-row-title">
                                {row.title}
                                {row.badge}
                              </div>
                              {row.desc && <p className="settings-row-desc">{row.desc}</p>}
                            </div>
                            <div
                              className={`settings-row-control${row.roomy ? " is-roomy" : ""}`}
                            >
                              {row.control}
                            </div>
                          </div>
                        ))}
                      </div>
                      )}
                    </section>
                  );
                })}
              </div>
            ))
          )}
        </div>
      </div>
    </div>
  );
}
