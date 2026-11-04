/**
 * The Workflow-tab vocabulary, kept deliberately identical to the steps the
 * original Python UI writes to its Workflow pane (`scripts/GinanUI`).
 *
 * GinanUI splits its logging in two (see `app/utils/logger.py`):
 *
 *   Logger.workflow(...)  → the "Workflow" pane: the handful of high-level
 *                           steps a user follows through a run.
 *   Logger.console(...)   → the "Console" pane: per-file, per-host and
 *                           per-attempt detail.
 *
 * This app keeps the same split: mirror listings, per-product enumerations and
 * per-host failures go to the Console, so the Workflow step list matches the
 * Python one. Every builder below carries the GinanUI call site it mirrors, so
 * the two can be diffed.
 *
 * Steps GinanUI logs that have no counterpart here are deliberately absent
 * rather than faked: SINEX marker validation, product/output archiving and the
 * repro3 directory fallback are not implemented in this app.
 */

/** Trailing "..." is written as three dots, exactly as GinanUI writes it. */

export const WF = {
  // ── Credentials & connectivity ──────────────────────────────────────────
  // GinanUI: app/main_window.py::_validate_cddis_credentials_once
  noCredentials: () =>
    "⚠️ No Earthdata credentials. Opening CDDIS Credentials dialog...",
  credentialsFound: (where: string) =>
    `✅ Earthdata Credentials found: ${where}`,
  credentialsInvalid: (where: string) => `❌ Credentials invalid: ${where}`,
  casterRejected: (detail: string) =>
    `❌ The caster rejected the saved login, so the run was not started. ${detail}`,
  casterUncheckable: (detail: string) =>
    `⚠️ Could not check the caster login before starting: ${detail}`,
  connectivityPassed: (seconds: string) =>
    `✅ CDDIS connectivity check passed in ${seconds} seconds`,
  connectivityFailed: (why: string) =>
    `❌ CDDIS connectivity check failed: ${why}. Please verify Earthdata credentials via the CDDIS Credentials dialog`,
  noInternet: (error: string) => `⚠️ No internet connection detected: ${error}`,

  // ── Support files / metadata ────────────────────────────────────────────
  // GinanUI: app/utils/workers.py::MetadataDownloadWorker (finished signal),
  // app/main_window.py::_on_metadata_download_finished / _on_atx_downloaded.
  // "Metadata" there is the same static support-file set `ensure_products`
  // fetches here.
  metadataSkippedOffline: () =>
    "⚠️ Skipping metadata download - running in offline mode",
  metadataDownloaded: () => "📦 Downloaded metadata successfully.",
  metadataFailed: (error: string) =>
    `⚠️ Error whilst downloading metadata:\n${error}`,

  /**
   * GinanUI's catch-all worker warning (`Logger.workflow(f"⚠️ {e}")`, see
   * app/utils/workers.py) - for failures with no more specific step.
   */
  warning: (error: string) => `⚠️ ${error}`,

  // ── ANTEX selection & antenna check ─────────────────────────────────────
  // GinanUI: app/controllers/general_config_controller.py
  // GinanUI logs "priority" or "fallback" depending on which of its two
  // .atx-choosing rules fired; a hand-picked file is this app's own third case.
  atxSelected: (
    filename: string,
    basis: "priority" | "fallback" | "manual selection",
  ) => `📁 Selected .atx file: ${filename} based on ${basis}`,
  atxMissing: () => "⚠️ No igs*.atx ANTEX file found in products directory.",
  antennaVerified: () => "✅ Antenna type verified from .atx file",
  antennaUnverified: (atxPath: string) =>
    `⚠️ Antenna type failed to verify from .atx file: ${atxPath}`,
  antennaCheckSkipped: () =>
    "⚠️ ANTEX (.atx) file not installed yet. Antenna type verification will be skipped.",

  // ── Input selection ─────────────────────────────────────────────────────
  // GinanUI: app/controllers/input_controller.py
  rinexSelected: (path: string) => `📄 RINEX file selected: ${path}`,
  rinexMetadataExtracted: () =>
    "⚒️ RINEX file metadata extracted and applied to UI fields",
  rinexMetadataError: (error: string) =>
    `Error extracting RNX metadata: ${error}`,
  outputDirSelected: (dir: string) => `📂 Output directory selected: ${dir}`,

  // ── PPP catalogue scan ──────────────────────────────────────────────────
  // GinanUI: input_controller.py + app/utils/workers.py::AnalysisCentreWorker
  // + app/models/dl_products.py::get_valid_analysis_centers
  scanningArchive: () =>
    "🔍 Scanning CDDIS archive for PPP products. Please wait...",
  retrievingValidProducts: () => "📦 Retrieving valid products",
  fetchingConstellations: () =>
    "📡 Fetching constellation information from SP3 headers...",
  foundProviders: (providers: string[]) =>
    `✅ Found valid PPP providers in main directory: ${providers.join(", ")}`,
  noValidProviders: () => "⚠️ No valid PPP providers found",
  scanCancelled: () => "📦 PPP provider scan was cancelled",
  scanFailed: (error: string) => `Error loading CDDIS data: ${error}`,
  noDateRange: () =>
    "📦 No start and/or end date, can't check valid analysis centres",

  // ── Observation codes & BIA validation ──────────────────────────────────
  // GinanUI: app/controllers/constellation_config_controller.py
  obsCodesPopulated: (constellations: string[]) =>
    `✅ Populated observation codes for ${constellations.join(", ")}`,
  noObsCodes: () => "⚠️ No observation codes found in RINEX",
  validatingBia: (provider: string, series: string, project: string) =>
    `📥 Validating constellation signal frequencies against BIA file for ${provider}/${series}/${project}...`,
  biaCached: (provider: string, series: string, project: string) =>
    `✅ BIA code priorities cached for ${provider}/${series}/${project}`,
  biaValidated: (provider: string, series: string, project: string) =>
    `✅ Validated constellation codes against BIA for ${provider}/${series}/${project}`,
  biaFetchFailed: () =>
    "⚠️ Failed to fetch BIA file for selected PPP products - unable to validate codes",

  // ── Run ─────────────────────────────────────────────────────────────────
  // GinanUI: app/main_window.py::_on_process_clicked and friends
  selectInputsFirst: () =>
    "⚠️ Please select RINEX and output directory first",
  cannotProcessOffline: () =>
    "⚠️ Cannot process: Ginan UI is running in offline mode (no internet connection)",
  alreadyProcessing: () => "⚠️ Processing already in progress. Please wait...",
  startingDownloads: () => "📡 Starting PPP product downloads...",
  downloadsFinished: () => "📦 Downloaded all products successfully.",
  downloadsIncomplete: (missing: number) =>
    `⚠️ Products downloaded, but ${missing} could not be fetched. PEA may fail or skip those days.`,
  downloadError: (message: string) => `⚠️ PPP download error: ${message}`,
  startingPea: () => "⚙️ Starting PEA execution in background...",
  peaCompleted: () => "✅ PEA processing completed",
  peaFailed: (message: string) => `⚠️ PEA execution failed: ${message}`,

  // ── Stop ────────────────────────────────────────────────────────────────
  // GinanUI: app/main_window.py::on_stopAllClicked, app/utils/workers.py
  stopRequested: () => "🛑 Stop requested - stopping all running tasks...",
  stoppingPea: () => "🛑 Stop requested - terminating PEA...",
  stopped: () => "🛑 Stopped by you",

  // ── Visualisation ───────────────────────────────────────────────────────
  // GinanUI: app/main_window.py::_run_visualisation,
  // app/models/execution.py::build_pos_plots
  generatingPosPlots: () => "📊 Generating position plots from PEA output...",
  foundPosFiles: (count: number, root: string) =>
    `📂 Found ${count} .pos files in ${root}:`,
  posFileEntry: (name: string) => `   • ${name}`,
  noPosFiles: (root: string) => `⚠️ No .pos files found in ${root}`,
  posPlotsGenerated: (count: number) => `✅ Generated ${count} plot(s)`,
  noPositionPlots: () => "⚠️ No position plots found",
  noVisualisations: () => "📊 No visualisations generated - selector disabled",
} as const;
