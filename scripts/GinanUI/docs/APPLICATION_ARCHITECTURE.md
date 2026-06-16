# Ginan-UI
## Application Architecture
### This document describes the software design choices and architecture framework of Ginan-UI
### Version: Release v4.1.2
#### Written by: Sam Greenwood
#### Last Updated: 16th June 2026

## 0. Table of Contents
- [1. Overview & Purpose](#1-overview--purpose)
- [2. System Context](#2-system-context)
- [3. Tech Stack](#3-tech-stack)
- [4. Application Structure](#4-application-structure)
- [5. Key Components & Modules](#5-key-components--modules)
- [6. Data Flow](#6-data-flow)
- [7. Authentication & Authorisation](#7-authentication--authorisation)
- [8. Configuration & Environment](#8-configuration--environment)
- [9. Build, Run & Deployment](#9-build-run--deployment)
- [10. Testing](#10-testing)
- [11. Known Issues & Technical Debt](#11-known-issues--technical-debt)
- [12. Decision Log](#12-decision-log)
- [13. Glossary](#13-glossary)

---
## 1. Overview & Purpose

Ginan-UI is a graphical user interface for the Ginan software developed by Geoscience Australia. It aims to lower the barrier of entry for users trying to use Ginan by simplifying the user's interaction with the software away from a command-line interface. On top of this, it automatically populates a `.yaml` configuration file based on a user-provided `.rnx` RINEX observation file, automatically downloads all static and dynamic GNSS products required for processing from NASA's CDDIS Earthdata archives, executes Ginan's Parameter Estimation Algorithm (PEA), and then visualises its output in an interactive HTML format embedded directly within the UI.

Ginan-UI lives inside the broader Ginan repository at `ginan/scripts/GinanUI/` and is designed as a companion tool - it does not replace or modify Ginan itself, but rather wraps around it to make Ginan as easy as drag-and-drop. It was developed as part of the ANU TechLauncher program in collaboration with Geoscience Australia.

---
## 2. System Context

Ginan-UI interacts with the following external systems:

**NASA CDDIS Earthdata Archives (`cddis.nasa.gov`):** The primary external dependency. Ginan-UI downloads all GNSS products (CLK, BIA, SP3, BRDC, SNX, and various other static metadata files) directly from the CDDIS HTTP archive. Authentication is via NASA Earthdata credentials stored in the user's `.netrc` / `_netrc` file. The `requests` library is used for all HTTP communication. Connectivity and credential validity are tested at startup.

**The Ginan PEA binary (`ginan/bin/pea`):** The core processing engine that Ginan-UI wraps. Ginan-UI locates the PEA binary (either bundled in a PyInstaller release or found on the system PATH), instantiates it as a subprocess, and then streams its `stdout` / `stderr` into the Console log panel in real time.

**`scripts/plot_pos.py` and `scripts/plot_trace_res.py`:** External plotting scripts located outside of Ginan-UI and within the broader Ginan repository. These are called as Python functions after PEA finishes processing and generates interactive HTML visualisations from the resultant `.POS` and `.TRACE` output files.

**The Local Filesystem:** Ginan-UI reads user-supplied RINEX observation files, reads and writes the generated `ppp_generated.yaml` config, manages product and output directories, and archives files from previous processing runs.

Ginan-UI does not expose any ports, APIs, or services. It is a standalone desktop application with no inbound network communication.

```
┌─────────────────────────────────────────────────────────────────┐
│                         User's Machine                          │
│                                                                 │
│  ┌──────────────┐     subprocess      ┌──────────────────────┐  │
│  │   Ginan-UI   │ ─────────────────── │   Ginan PEA Binary   │  │
│  │  (PySide6)   │                     │   (ginan/bin/pea)    │  │
│  └──────┬───────┘                     └──────────────────────┘  │
│         │ calls                                                 │
│         ▼                                                       │
│  ┌──────────────────────────────────┐                           │
│  │  plot_pos.py / plot_trace_res.py │                           │
│  │  (from ginan/scripts/)           │                           │
│  └──────────────────────────────────┘                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
         │ HTTPS (requests + .netrc auth)
         ▼
┌─────────────────────────┐
│  NASA CDDIS Earthdata   │
│  cddis.nasa.gov         │
│  (SP3, CLK, BIA, etc.)  │
└─────────────────────────┘
```

---
## 3. Tech Stack

**Python 3.9+:** The implementation language. The minimum version of 3.9 was chosen for compatibility with the broader Ginan repository's Python tooling.

**PySide6 ~6.10.0:** Qt6 Python bindings, used for the entire GUI (widgets, layouts, signals / slots, threading, web engine). PySide6 was chosen over PyQt6 for its more permissive LGPL licensing, which is compatible with Ginan being open-source. Qt's signal / slot mechanism is the primary pattern for cross-thread communication (e.g., logging from worker threads back to the UI). The `.ui` file (`main_window.ui`) is designed in Qt Designer and compiled to Python via `pyside6-uic`.

**ruamel.yaml ~0.18.15:** Used for all YAML reading and writing. Chosen over PyYAML because it preserves comments and formatting in the config file, which is important so that manual user edits to `ppp_generated.yaml` are not silently destroyed when Ginan-UI writes back specific fields. Breaking changes in ruamel.yaml's API are common across minor versions, and the pinned version in `requirements.txt` should be respected.

**pandas ~2.3.3:** Used for managing the product availability dataframe returned by the CDDIS product query logic in `dl_products.py`. Provides a convenient structure for filtering available PPP providers, series, and projects by date range.

**plotly ~6.3.1:** Used by the external `plot_pos.py` and `plot_trace_res.py` scripts to generate the interactive HTML visualisations.

**numpy ~2.3.3:** Used internally by the plotting scripts and data processing pipeline.

**statsmodels ~0.14.5:** Used by the plotting scripts for statistical analysis of position solutions.

**requests ~2.32.5:** Used for all HTTP communication with the CDDIS archive.

**hatanaka ~2.8.1:** Used to decompress Hatanaka-compressed RINEX files (`.crx` / `.rnx.gz`).

**unlzw3 ~0.2.3:** Used to decompress Unix `.Z` compressed product files (a legacy format still common in the CDDIS archive).

**beautifulsoup4 ~4.14.2:** Used for parsing CDDIS HTML directory listings when scanning for available product files.

> **Note on requirements.txt accuracy:** `requirements.txt` lists the direct Python dependencies used by Ginan-UI. `netrc` is part of the Python standard library, and runtime dependencies such as `numpy` are listed explicitly even when another package also requires them. If `pip install -r requirements.txt` fails, check whether an upstream dependency has changed its supported Python or platform constraints.

---
## 4. Application Structure

All Ginan-UI code lives within `ginan/scripts/GinanUI/`. The structure follows a loose Model-View-Controller (MVC) architecture pattern with three clearly separated layers.

```
GinanUI/
├── main.py                         # Entry point - instantiates QApplication and MainWindow
├── README.md
├── requirements.txt
├── app/
│   ├── main_window.py              # Top-level controller / application shell
│   ├── controllers/                # UI controllers (C in MVC)
│   │   ├── input_controller.py     # Parent controller - owns shared state & top-level buttons
│   │   ├── general_config_controller.py
│   │   ├── constellation_config_controller.py
│   │   ├── output_config_controller.py
│   │   ├── yaml_config_controller.py
│   │   └── visualisation_controller.py
│   ├── models/                     # Core logic & data (M in MVC)
│   │   ├── execution.py            # PEA subprocess lifecycle & YAML config management
│   │   ├── dl_products.py          # CDDIS product discovery & downloading
│   │   ├── rinex_extractor.py      # RINEX file parsing
│   │   ├── archive_manager.py      # Product and output file archival
│   │   └── inspector.py            # GinanYAMLInspector integration & YAML merging
│   ├── views/                      # UI definitions (V in MVC)
│   │   ├── main_window.ui          # Qt Designer file - edit this, not main_window_ui.py
│   │   └── main_window_ui.py       # Auto-generated - DO NOT EDIT MANUALLY
│   ├── resources/
│   │   ├── assets/                 # Icons, logos, Qt resource files (.qrc, _rc.py)
│   │   ├── Yaml/
│   │   │   ├── default_config.yaml       # Template YAML that is used to generate ppp_generated.yaml
│   │   │   └── GinanYAMLInspector.html   # Inspector HTML (auto-generated by "pea -Y 4")
│   │   ├── ppp_generated.yaml      # Generated at runtime from default_config.yaml
│   │   └── inputData/products/     # Downloaded GNSS products live here at runtime
│   └── utils/
│       ├── logger.py               # Centralised thread-safe logging via Qt signals
│       ├── toast.py                # Non-blocking toast notification widget
│       ├── common_dirs.py          # Centralised path constants (dev & PyInstaller aware)
│       ├── yaml.py                 # ruamel.yaml wrappers (load, write, update in-place)
│       ├── workers.py              # QObject workers for background threads
│       ├── cddis_credentials.py    # .netrc credential save / validate utilities
│       ├── cddis_connection.py     # CDDIS connectivity & authentication testing
│       └── ui_compilation.py       # Compiles main_window.ui -> main_window_ui.py
├── docs/
│   ├── USER_MANUAL.md
│   ├── APPLICATION_ARCHITECTURE.md
│   └── images/
└── tests/
    ├── test_checksum.py
    ├── test_executable.py
    ├── test_execution.py
    └── test_ui_compilation.py
```

**Key conventions:**
- Many complex code files use ``#region`` blocks to segment and organise sections of code. These drastically improve readability and make it easier to find the relevant code when debugging.
- Controllers are instantiated by `main_window.py` and do not instantiate each other except through the parent `InputController`, which owns and instantiates its four config tab sub-controllers (`GeneralConfigController`, `ConstellationConfigController`, `OutputConfigController`, `YAMLConfigController`).
- All background work (i.e. product downloads, PEA execution, and loading BLQ generation) is done in `QThread` workers defined in `workers.py`. Qt signals are used exclusively for communicating results and progress back to the UI thread - no direct cross-thread widget access.
- All filepath constants are defined in `common_dirs.py` and resolve correctly in both development mode and PyInstaller bundle mode. **DO NOT** hardcode paths elsewhere.
- Logging throughout the app is done exclusively via the static `Logger` class (`utils/logger.py`). **DO NOT** use `print()` in production code, only temporary testing.
- The `main_window.ui` file is the single source of truth for the UI layout. `main_window_ui.py` is auto-generated every time ``main.py`` is run (it calls ``utils/ui_compilation.py`` automatically) and should never be edited directly - regenerate it using `ui_compilation.py` after any `.ui` changes.

---
## 5. Key Components & Modules

### `main_window.py` - Application Shell
The top-level class that initialises and owns all controllers. It creates the `Execution` model instance, instantiates `InputController` and `VisualisationController`, and wires together the high-level workflow signals (e.g., `pea_ready` -> start download + PEA). It also contains the `log_message()` method that the `Logger` signals connect to, routing messages to either the "Workflow" or "Console" `QTextEdit` widgets. `MainWindow` is also responsible for spinning up and tearing down `QThread` / worker pairs and connecting their signals.

### `input_controller.py` - Parent UI Controller
Owns all shared UI state. This includes the selected RINEX file path (`rnx_file`), the output directory (`output_dir`), the products dataframe (`products_df`), and the `Execution` instance. It wires the top-level action buttons ("Observations", "Output", "Process", "Stop", "CDDIS Credentials", "Open User Manual") and delegates configuration tab-specific behaviour to four sub-controllers (`GeneralConfigController`, `ConstellationConfigController`, `OutputConfigController`, `YAMLConfigController`). The `ExtractedInputs` dataclass, defined inside this module, is the data transfer object that packages all UI values before they are passed to `Execution.apply_ui_config()` for config generation.

### `execution.py` - PEA Lifecycle & Config Manager
The primary model class. On instantiation it locates the PEA binary (checking the PyInstaller bundle, then system PATH, then relative path from the source tree), and loads the `ppp_generated.yaml` config using ruamel.yaml. Its key responsibilities include: `edit_config()` applies individual YAML field changes in-place without destroying comments; `apply_ui_config()` orchestrates writing all UI-derived values into the YAML before a run; `ensure_loading_blq()` verifies and generates ocean / atmospheric tide loading BLQ files for the station; `execute_config()` spawns PEA as a subprocess and streams its output; `build_pos_plots()` and `build_trace_plots()` call the external plotting scripts post-run. The `stop_all()` method uses `os.killpg` (process group kill) to reliably terminate PEA and any child processes on Unix.

**Ocean and Atmospheric Loading:** The `ensure_loading_blq()` method checks whether the station has entries in the configured BLQ files. If not, it downloads loading grid netCDF files (`oceantide.nc` and `atmtide.nc`) and runs the `interpolate_loading` binary to compute station-specific loading coefficients. The generated BLQ files are automatically added to the YAML configuration. This workflow is triggered before PEA execution and operates even when the "Overwrite Config with UI Values" toggle is disabled.

### `dl_products.py` - Product Discovery & Downloading
Handles all interaction with the CDDIS archive for downloading GNSS products. Scans the CDDIS HTTP directory listing (using `beautifulsoup4`) to find available SP3, CLK, BIA, and BRDC files for a given date range and PPP provider selection. Includes a REPRO3 fallback for older RINEX data where standard products are no longer available. Also responsible for downloading static metadata products (ATX, ALOAD, OLOAD, etc.) on first launch and every seven (7) days after that. The `download_loading_grids()` function handles downloading the ocean and atmospheric tide loading grid files (`oceantide.nc` and `atmtide.nc`) required for BLQ generation.

### `rinex_extractor.py` - RINEX Parser
Parses RINEX v2 and v3 / v4 observation file headers to extract: marker name, receiver type, antenna type, antenna offset (ENU), approximate position (referred to as "apriori position"), time window (first / last epoch), data interval, and per-constellation observation codes. The extracted data is used to pre-populate the UI fields and to construct the `ExtractedInputs` object. A non-obvious behaviour: for RINEX v3 / v4 files, extracted observation codes are culled to carrier phase (L) codes only, then reordered against the template config's default priority list. For v2 files, codes are converted and already in priority order so this reordering step is skipped.

### `archive_manager.py` - File Archival
Manages moving old product and output files into timestamped archive subdirectories. Archival is triggered in three scenarios: (1) on app startup for static products older than seven (7) days; (2) when the RINEX file changes, to prevent products from mismatched time windows being mixed; (3) when the PPP provider / series / project selection changes. Also provides `restore_from_archive()` which checks the archive directory before downloading a product from CDDIS, avoiding redundant network requests.

### `workers.py` - Background Thread Workers
Defines five `QObject`-based workers intended to run in `QThread`: `PeaExecutionWorker`, `DownloadWorker`, `BiasProductWorker`, `SinexValidationWorker`, and `LoadingBlqWorker`. All workers expose a `stop()` slot and check a `_stop` flag at regular intervals to ensure they do not cause a segmentation fault on cancellation. Results are communicated back via Qt signals (`finished`, `error`, `progress`). **A non-obvious behaviour:** `DownloadWorker` handles three distinct modes depending on which constructor arguments are provided - analysis centre discovery, metadata installation, or product downloading - and branches its `run()` logic accordingly. The `LoadingBlqWorker` delegates to `Execution.ensure_loading_blq()` and runs in a background thread to prevent UI blocking during BLQ generation.

### `logger.py` - Centralised Logging
A static class that must be initialised with the `MainWindow` instance before use. Exposes `Logger.workflow()`, `Logger.console()`, and `Logger.both()`. Uses Qt signals internally so that log calls from background worker threads are safely passed to the UI thread before updating the `QTextEdit` widgets. Falls back to `print()` if called before initialisation.

### `utils/yaml.py` - YAML Utilities
Wraps ruamel.yaml with safe read / write helpers that preserve comments and formatting. **The key non-obvious behaviour** is that all values written to the YAML are passed through `normalise_yaml_value()` first, which converts Python `Path` objects and plain strings to `PlainScalarString` and lists to block-style `CommentedSeq`. This is necessary because ruamel.yaml will raise `RepresenterError` on bare Python types in certain contexts.

### `utils/common_dirs.py` - Path Constants
Defines all important path constants (`TEMPLATE_PATH`, `GENERATED_YAML`, `INPUT_PRODUCTS_PATH`, `USER_MANUAL_PATH`) in a way that resolves correctly both in development mode and when running from a release build that is bundled with PyInstaller (where `sys._MEIPASS` is set). Any new path constant should be added here rather than hardcoded in individual modules.

### `views/main_window.ui` - Qt Designer UI Definition
The XML file that defines all UI widget geometry, layout, and properties. It is highly recommended that you edit this file in Qt Designer by running the ``pyside6-designer`` file from within your Python virtual environment (however you can still edit it by hand), then regenerate `main_window_ui.py` by running ``utils/ui_compilation.py``. The generated file patches two import lines for Qt resource files - this patching is done automatically by `ui_compilation.py`.

### `controllers/constellation_config_controller.py`
Manages the Constellations tab, which displays drag-and-drop lists of observation code priorities for each enabled GNSS constellation. Triggers a `BiasProductWorker` background download whenever the PPP provider / series / project selection changes, to fetch and parse the corresponding `.BIA` file and update the available code priorities. Cross-validates RINEX constellations against the SP3 file and highlights any unsupported constellations in red with strikethrough styling.

### `controllers/yaml_config_controller.py`
Manages the YAML Config tab, which provides controls for configuration file management and advanced editing. Wires the "Overwrite Config with UI Values" toggle, "Show Config" button, "Reset Config" button, and "Edit Config in Inspector" button. The inspector integration uses `QWebEngineView` to embed the GinanYAMLInspector HTML within a Qt dialog which automatically imports the current configuration and intercepts the save action to merge changes back via `QWebChannel`. Delegates all inspector I/O and YAML merging logic to the `Inspector` model.

### `models/inspector.py`
The model layer for GinanYAMLInspector integration. Handles: (1) ensuring the inspector HTML exists by auto-generating it via `pea -Y 4` when missing; (2) building the JavaScript that wires auto-import and save interception when the inspector is opened from Ginan-UI; (3) sanitising the inspector's YAML output (quoting wildcard patterns, stripping trailing whitespace); (4) deep-merging the inspector export onto the existing `ppp_generated.yaml` so keys the inspector doesn't know about are preserved; (5) repairing known ruamel.yaml output edge cases. The `merge_and_save()` method includes fallback validation - if the comment-preserving write produces unparseable YAML, it automatically falls back to a clean write without comment preservation.

### `controllers/visualisation_controller.py`
Manages the visualisation panel and plot display. Embeds generated HTML plots into a `QWebEngineView` widget, maintains an indexed list of available visualisations, and provides controls for plot selection and external viewing. The `bind_enlarge_button()` method wires the "Enlarge" button to open the current visualisation in a separate resizable pop-out window (implemented as a `QDialog` containing a `QWebEngineView`). The `bind_open_button()` method wires the "Open in Browser" button to launch the system's default browser.

---
## 6. Data Flow

The primary processing workflow follows this sequence:

```
User selects RINEX file
        │
        ▼
RinexExtractor.extract_rinex_data()
  → Extracts metadata (marker name, time window, constellations, codes, antenna info, etc.)
  → UI fields pre-populated via GeneralConfigController / ConstellationConfigController
        │
        ▼
DownloadWorker (QThread) - analysis centre discovery
  → Scans CDDIS for valid PPP providers for the RINEX time window
  → Populates PPP Provider / Series / Project dropdowns in UI
        │
        ▼
User sets Mode, and reviews / adjusts fields before clicking "Process"
        │
        ▼
InputController.on_run_pea()
  → Calls archive_old_outputs() to move previous run outputs
  → Calls archive_products_if_selection_changed() if PPP selection changed
  → Calls InputController.extract_ui_values() → produces ExtractedInputs dataclass
  → Calls Execution.apply_ui_config(ExtractedInputs) → writes all values into ppp_generated.yaml
        │
        ▼
DownloadWorker (QThread) - product downloading
  → Downloads missing dynamic products (SP3, CLK, BIA, BRDC) from CDDIS
  → Checks archive first via restore_from_archive() before downloading
  → Progress streamed to "Workflow" log via Qt signals
        │
        ▼
LoadingBlqWorker (QThread) - ocean and atmospheric loading generation
  → Checks configured BLQ files for station entry
  → Downloads loading grid files (oceantide.nc, atmtide.nc) if needed
  → Runs interpolate_loading binary to generate station-specific BLQ files
  → Updates YAML config with new BLQ file paths
  → Progress streamed to "Workflow" log via Qt signals
        │
        ▼
PeaExecutionWorker (QThread)
  → Spawns PEA binary as subprocess with ppp_generated.yaml
  → Streams stdout / stderr to "Console" log via Qt signals
        │
        ▼
Execution.build_pos_plots() / build_trace_plots()
  → Calls plot_pos_files() / plot_trace_res_files() from ginan/scripts/
  → Generates HTML files in output_dir/visual/
        │
        ▼
VisualisationController
  → Registers HTML files, embeds first plot in QWebEngineView
  → Enables plot selector ComboBox, "Enlarge" button, and "Open in Browser" button
```

---
## 7. Authentication & Authorisation

Ginan-UI has a single authentication concern: NASA Earthdata credentials for accessing the CDDIS archive. There is no user login system, sessions, or role-based access control within the application itself.

**Credential Storage:** NASA Earthdata credentials (username and password) are saved to the user's `.netrc` file (Unix / MacOS: `~/.netrc`; Windows: `%USERPROFILE%\.netrc` and `%USERPROFILE%\_netrc`). Both `urs.earthdata.nasa.gov` and `cddis.nasa.gov` entries are written simultaneously. The credential file is managed by `cddis_credentials.py`.

**Credential Entry:** On first launch, if valid credentials are not detected in `.netrc`, a separate dialog window `CredentialsDialog` is displayed prompting the user for their username and password. This dialog can also be opened at any time via the "CDDIS Credentials" button in the top-right. Credentials are saved via `save_earthdata_credentials()`.

**Credential Usage:** The `requests` library uses the `.netrc` file automatically for HTTP Basic authentication when downloading from CDDIS. The `cddis_connection.py` module provides `test_cddis_connection()` for verifying both connectivity and authentication validity, and `get_netrc_auth()` for explicitly reading credentials when needed.

**Email / Username Persistence:** A secondary `CDDIS.env` file (`app/utils/CDDIS.env`) stores the email / username separately. This is read by `read_email()` and used in some CDDIS API contexts. If not present in the env file, it is derived from the `.netrc` username.

---
## 8. Configuration & Environment

Configuration is handled through two mechanisms: a template YAML for PEA configuration, and path resolution logic for development versus bundled distribution.

**YAML Configuration:**
- `app/resources/Yaml/default_config.yaml` - The template config. This is the committed, default PEA configuration that ships with Ginan-UI. It contains default values for all PEA processing parameters.
- `app/resources/ppp_generated.yaml` - The generated config, created at runtime by copying the template and then overwriting specific fields with user-supplied values. This file is ignored by git and should not be committed. It persists between sessions and preserves manual edits made via the "Show Config" button.

**Path Resolution:**
The `common_dirs.py` module detects whether Ginan-UI is running in development mode or a PyInstaller bundle by checking `sys.frozen`. In bundle mode, `sys._MEIPASS` points to the `_internal/` directory and paths are constructed accordingly. All path constants (`TEMPLATE_PATH`, `GENERATED_YAML`, `INPUT_PRODUCTS_PATH`, `USER_MANUAL_PATH`) are derived from `get_base_path()` and `get_user_manual_path()` in this module.

**Product Storage:**
Downloaded products are stored in `app/resources/inputData/products/` (or `_internal/scripts/GinanUI/app/resources/inputData/products/` in the bundled distribution). Archived products live in subdirectories under `products/archive/`.

---
## 9. Build, Run & Deployment

### Running from Source

Ginan-UI runs as a Python module from within the Ginan repository. The following assumes you have Python 3.9+ and have cloned the Ginan repository.

```bash
# Navigate to the Ginan repository root
cd /path/to/ginan

# Install dependencies
pip install -r scripts/GinanUI/requirements.txt

# Run Ginan-UI
python -m scripts.GinanUI.main
```

You will also need a built `pea` binary available. The binary is expected at `ginan/bin/pea`. If it is not present, `execution.py` will also check the system PATH for a `pea` executable.

### Running Qt Designer

Ensure that you have a python virtual environment (sometimes referred to as ``venv``) activated, and run the ``pyside6-designer`` file like so:

```bash
pyside6-designer
```

If the command is not on your `PATH`, run the executable from your virtual environment's `bin/` directory on Linux/macOS or `Scripts\` directory on Windows. This will open a GUI editing software window that makes modifying the front-end of Ginan-UI much easier than editing the XML file ``main_window.ui`` directly. Do this by selecting to open the `app/views/main_window.ui` file.

### Adding Images / Icons to the UI - Qt Resource Files

Qt has its own asset pipeline for bundling images and icons into the application. Raw image files cannot simply be referenced by filesystem path in a `.ui` file - they must be compiled into a Qt resource module first. The workflow is:

1. **Create a `.qrc` file** - this is an XML file that lists the image assets to include. For example, `ginan_logo.qrc` references `ginan-logo.png`. These files live in `app/resources/assets/`.

2. **Compile the `.qrc` file into a Python module** using `pyside6-rcc`:
```bash
   pyside6-rcc app/resources/assets/ginan_logo.qrc -o app/resources/assets/ginan_logo_rc.py
```
   This produces `ginan_logo_rc.py` - a Python module containing the image data as base64-encoded bytes. The `_rc.py` suffix is the convention.

3. **Import the `_rc.py` module** somewhere in the application before the `.ui` file is loaded. The import registers the resources with Qt's internal resource system. In Ginan-UI this is handled by the auto-generated `main_window_ui.py`, which imports `ginan_logo_rc` and `icons_rc`. When `ui_compilation.py` regenerates this file, it patches the raw `import ginan_logo_rc` lines to the correct package path (`from scripts.GinanUI.app.resources.assets import ginan_logo_rc`). **However this must be updated for all new images added to the UI**.

4. **Reference the asset in the `.ui` file** using the Qt resource path syntax: `:/prefix/filename` (e.g., `:/icons/help.png`). Qt Designer uses this same syntax when you add images through its built-in resource browser.

The short version: if you add a new image to the UI, add it to the relevant `.qrc` file, recompile with `pyside6-rcc`, and ensure the resulting `_rc.py` is imported before the UI loads. Do not reference image files by raw filesystem path in the `.ui` file.

### Recompiling the UI After Qt Designer Changes

If you modify `app/views/main_window.ui` in Qt Designer, you must regenerate the Python file using ``utils/ui_compilation.py``, or command-line:

```bash
python -m scripts.GinanUI.app.utils.ui_compilation
```

This will recompile `main_window_ui.py` and automatically patch the resource import lines.

### Building a Distributable Executable

Ginan-UI is distributed as a standalone executable via PyInstaller.

Releases are published to the [Ginan GitHub Releases page](https://github.com/GeoscienceAustralia/ginan/releases) for Windows, MacOS, and Linux. See the User Manual (Section 2.2) for platform-specific installation instructions for end users.

---
## 10. Testing

Tests live in `GinanUI/tests/` and are run with pytest.

```bash
cd /path/to/ginan
pytest scripts/GinanUI/tests/
```

Four test modules exist:

- `test_ui_compilation.py` - Verifies that `pyside6-uic` is available and that `main_window.ui` compiles without errors.
- `test_executable.py` - Checks that the PEA binary can be located from the development directory structure.
- `test_execution.py` - Tests the `Execution` model class, including config loading, YAML editing, and the `apply_ui_config()` workflow.
- `test_checksum.py` - Validates the SHA512 checksum verification logic used during product downloading.

Test resources (a sample RINEX file, example YAML config, product lists) live within ``ginan/exampleConfig/`` and ``ginan/inputData/``. The shell scripts `getFiles.sh` and `setFiles.sh` are used to fetch or set up the test input data.

**Known Gaps:** There is no automated UI testing (no widget interaction tests). The product downloading logic in `dl_products.py` is not unit tested due to its dependency on live CDDIS connectivity. The `RinexExtractor` parsing logic has no dedicated test coverage.

---
## 11. Known Issues & Technical Debt

**Race Condition on First Launch (Qt Segmentation Fault):** On rare occasions, Ginan-UI will launch to a black screen and then crash with a segmentation fault. This is a known Qt initialisation race condition and resolves itself on a second launch attempt. The User Manual documents this for end users.

**Stop Button Race Condition:** If the user clicks "Stop" before the first download thread has started, and then clicks "Process" again immediately, a core dump can occur because the thread has not yet had a chance to exit. The workaround is to wait for the "stopped thread" message in the "Console" terminal before clicking "Process" again. This is documented in the troubleshooting table of the User Manual.

**GinanYAMLInspector Wildcard Pattern Handling:** The inspector copies values directly from HTML form fields without adding YAML quotes. Wildcard patterns (e.g., `*.CLK`, `*_ocean.BLQ`) are treated as YAML aliases by ruamel.yaml unless quoted. The `Inspector.fix_inspector_yaml()` method detects and quotes these patterns before parsing, but edge cases with unusual patterns may still cause issues. If you encounter "found undefined alias" errors when saving from the inspector, check for unquoted wildcards in the generated YAML.

**YAML Artifacts Persisting Between Sessions:** When the RINEX file is changed, Ginan-UI updates specific YAML fields but does not regenerate the entire config from scratch. This means some stale values (e.g., old marker names listed under `receiver_options` ) can persist across sessions. This is usually harmless but can occasionally cause unexpected PEA behaviour. The "Reset Config" button is the remedy as it deletes and fully regenerates the config file from ``default_config.yaml``.

**CDDIS Server Reliability:** The CDDIS servers have experienced significant reliability issues (notably during the 2025 US government shutdown). The application handles connection timeouts with retry logic, but extended outages will result in failed downloads. This is an external dependency that cannot be resolved within the codebase.

---
## 12. Decision Log

### UI Framework - PySide6

Several Python GUI frameworks were evaluated: Tkinter / CustomTkinter, PyQt6, PySide6, and Kivy.

Tkinter and CustomTkinter were ruled out for lacking the complex UI elements the project required - particularly interactive graph plotting and an embedded web view for HTML visualisations. While third-party themes can modernise Tkinter's appearance, that introduces additional dependencies without solving the underlying capability gap.

PyQt6 provides the same feature set as PySide6 and is based on the same underlying Qt C++ library, but is distributed under a commercial licence that would require a paid licence for non-personal use. PySide6 is functionally near-identical to PyQt6 but is officially maintained by The Qt Company under an LGPL licence, making it compatible with Ginan being open-source.

**PySide6 was chosen** for the following reasons: it provides the full Qt feature set (modern widgets, Qt Designer drag-and-drop layout, signals / slots, multi-threading, `QWebEngineView` for embedded HTML, Matplotlib / Plotly integration); it has a modern look that will hold up over time; and its LGPL licence is appropriate for an open-source project. The steeper learning curve compared to Tkinter was considered an acceptable trade-off given that the features requiring that curve - graph plotting, complex layouts, thread-safe UI updates - were hard requirements of the product.

> **Note for future maintainers:** PyQt6 and PySide6 APIs are nearly identical. If there is ever a reason to switch, the `qtpy` abstraction library (`pip install qtpy`) provides compatibility that allows both to be used interchangeably from a single codebase.

---

### Architecture Pattern - MVC

A Model-View-Controller pattern was chosen to structure the codebase. This provided a clear separation between the UI layer (`views/`, `controllers/`), the central logic and data layer (`models/`), and supporting utilities (`utils/`). MVC was a natural fit for a desktop application with a clear front-end (Qt widgets) and back-end (PEA execution, file downloading, YAML management), and the original TechLauncher team had prior familiarity with the pattern.

---

### YAML Library - ruamel.yaml over PyYAML

PyYAML is the more commonly known YAML library, but it does not preserve comments or formatting when writing back to a file. Because Ginan-UI writes specific fields into `ppp_generated.yaml` while leaving the rest untouched - and because advanced users are expected to make manual edits to this file - silently destroying comments and formatting on every write would be a poor experience. `ruamel.yaml` was chosen specifically for its ability to perform targeted in-place updates while preserving all existing comments, indentation, and structure.

---
## 13. Glossary

**ATX** - Antenna Exchange Format. A file format describing antenna phase centre corrections for GNSS receivers and satellites.

**BDS** - BeiDou Navigation Satellite System. China's GNSS constellation.

**BIA** - Bias product file. Contains code and phase biases for multi-GNSS processing. Used to populate the Constellations tab code priorities.

**BLQ** - Ocean and atmospheric tide loading coefficient file. Contains corrections that account for the deformation of the Earth's crust due to ocean and atmospheric tides. Ginan-UI automatically generates station-specific BLQ files using the `interpolate_loading` tool when they are not present in the configured files.

**BRDC / NAV** - Broadcast navigation message. The navigation data broadcast by GNSS satellites, used as a fallback or supplement to precise ephemeris products.

**CDDIS** - Crustal Dynamics Data Information System. NASA's geodetic data archive, from which Ginan-UI downloads all GNSS products. Hosted at `cddis.nasa.gov`.

**CLK** - Clock product file. Contains precise satellite and station clock corrections.

**ECEF** - Earth-Centred, Earth-Fixed coordinate system. The Cartesian coordinate system used for apriori position values (X, Y, Z in metres).

**ENU** - East-North-Up coordinate system. Used for antenna offset values.

**GAL** - Galileo. The European GNSS constellation.

**GLO** - GLONASS. Russia's GNSS constellation.

**GNSS** - Global Navigation Satellite System. The generic term for satellite navigation systems (GPS, GLONASS, Galileo, BeiDou, etc.).

**GPS** - Global Positioning System. The United States' GNSS constellation.

**GPT2** - Global Pressure and Temperature 2. A tropospheric model used in GNSS processing.

**IGS** - International GNSS Service. A global collaborative network of GNSS tracking stations and analysis centres that produces the reference products (SP3, CLK, BIA, etc.) used by Ginan.

**interpolate_loading** - A Ginan binary tool that computes station-specific ocean and atmospheric tide loading coefficients from grid files. Ginan-UI automatically runs this tool to generate BLQ files when a station is not found in the configured loading files.

**ION** - Ionospheric product file. Contains ionospheric delay corrections.

**MVC** - Model-View-Controller. The broad architectural pattern used to organise Ginan-UI's codebase: Models (`models/`), Views (`views/`), Controllers (`controllers/`).

**.netrc / _netrc** - A plain text file storing credentials for network services. Used by Ginan-UI and the `requests` library for authenticating with the CDDIS archive. Located at `~/.netrc` on Unix / MacOS, or `%USERPROFILE%\.netrc` / `%USERPROFILE%\_netrc` on Windows.

**PEA** - Parameter Estimation Algorithm. Ginan's core GNSS processing engine, compiled as a native binary (`ginan/bin/pea`). Ginan-UI wraps and automates the use of PEA.

**PPP** - Precise Point Positioning. A GNSS processing technique that uses precise satellite orbit and clock products (as opposed to differential positioning) to compute highly accurate positions from a single receiver.

**PPP Provider / AC** - Analysis Centre. An organisation that produces and publishes precise GNSS products (e.g., IGS, COD, GFZ, JPL). Referred to interchangeably as "PPP Provider" in the UI and "analysis centre" in the code.

**QWebChannel** - Qt's JavaScript integration mechanism for bidirectional communication between C++/Python code and web content loaded in `QWebEngineView`. Used by the GinanYAMLInspector integration to intercept the save action and route the generated YAML back to Python for merging.

**QZS / QZSS** - Quasi-Zenith Satellite System. Japan's regional GNSS constellation.

**REPRO3** - The third IGS reprocessing campaign. A set of reprocessed historical GNSS products available on CDDIS for older observation data where standard products may no longer be published. REPRO3 is prioritised between the GPS weeks 730 to 2138. Outside of this, the regular products are prioritised.

**RINEX** - Receiver INdependent EXchange format. The standard file format for raw GNSS observation data. Ginan-UI supports RINEX v2, v3, and v4.

**RNX** - Common file extension for RINEX observation files.

**SNX / SINEX** - Solution INdependent EXchange format. Used for station coordinate and metadata exchange. Ginan-UI downloads and validates IGS CRD SINEX files to cross-check receiver metadata extracted from the RINEX file.

**SP3** - Standard Product 3, also known as the "Orbit" file. A file format containing precise satellite orbit positions (ephemeris). One of the primary dynamic products downloaded for each processing run.

**TRO** - Tropospheric product file. Contains tropospheric delay corrections.

**URS** - Earthdata User Registration System. NASA's authentication system for CDDIS access. Credentials registered at `urs.earthdata.nasa.gov`.

**YAML** - YAML Ain't Markup Language (Yes that is the acronym). The configuration file format used by Ginan's PEA. Ginan-UI reads and writes `ppp_generated.yaml` using `ruamel.yaml` to preserve comments and formatting.

**GinanYAMLInspector** - An HTML-based configuration editor generated by Ginan's PEA executable (`pea -Y 4`). Provides a comprehensive form interface for editing nearly all Ginan configuration options. When opened from Ginan-UI, the inspector auto-imports the current configuration and intelligently merges saved changes back to `ppp_generated.yaml` whilst preserving keys the inspector doesn't manage.
