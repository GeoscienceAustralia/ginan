"""
Top-level UI input controller for the Ginan-UI.

This is the parent controller that owns the top-level action buttons
(Observations, Output, Show Config, Process, Stop All, CDDIS Credentials,
User Manual, Reset Config) and coordinates three tab-specific sub-controllers:

  - GeneralConfigController         - General config tab
  - ConstellationConfigController   - Constellations config tab
  - OutputConfigController          - Output config tab
  - YAMLConfigController            - YAML config tab

It also holds shared state (rnx_file, output_dir, products_df, execution)
and the ExtractedInputs dataclass used by the Execution model.
"""

from __future__ import annotations
import os
import re
import subprocess
import webbrowser
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import List
import pandas as pd
from PySide6.QtCore import QObject, Qt, Signal
from PySide6.QtWidgets import (
    QDialog,
    QFileDialog,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPushButton,
    QVBoxLayout,
)
from scripts.GinanUI.app.models.archive_manager import archive_old_outputs, archive_products_if_rinex_changed
from scripts.GinanUI.app.models.execution import GENERATED_YAML, INPUT_PRODUCTS_PATH, Execution
from scripts.GinanUI.app.models.rinex_extractor import RinexExtractor
from scripts.GinanUI.app.controllers.constellation_config_controller import ConstellationConfigController
from scripts.GinanUI.app.controllers.general_config_controller import GeneralConfigController
from scripts.GinanUI.app.controllers.output_config_controller import OutputConfigController
from scripts.GinanUI.app.controllers.yaml_config_controller import YAMLConfigController
from scripts.GinanUI.app.utils.cddis_credentials import save_earthdata_credentials
from scripts.GinanUI.app.utils.common_dirs import USER_MANUAL_PATH
from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.utils.toast import show_toast

class InputController(QObject):
    """
    Parent UI controller that coordinates file selection, configuration,
    and processing workflows across the Ginan-UI input panel.

    Delegates detailed responsibilities to three tab-specific sub-controllers:
      - self.general_tab        (GeneralConfigController)
      - self.constellations_tab (ConstellationConfigController)
      - self.output_tab         (OutputConfigController)
      - self.yaml_tab           (YAMLConfigController)

    Signals:
      ready(str, str): Emitted when both RINEX path and output directory are set.
      pea_ready():     Emitted when PEA processing should start.
    """

    ready = Signal(str, str)  # rnx_path, output_path
    pea_ready = Signal()  # emitted when PEA processing should start

    def __init__(self, ui, parent_window, execution: Execution):
        """
        Initialise the top-level input controller and its sub-controllers.

        Arguments:
          ui: Main window UI instance (generated from Qt .ui).
          parent_window: Parent widget/window to anchor dialogs.
          execution (Execution): Backend execution bridge used to read/apply UI config.
        """
        super().__init__()
        self.ui = ui
        self.parent = parent_window
        self.execution = execution

        # Shared state
        self.rnx_file: Path = None
        self.output_dir: Path = None
        self.products_df: pd.DataFrame = pd.DataFrame()  # CDDIS replaces with a populated dataframe
        self.config_path = GENERATED_YAML

        # Time window (set during on_run_pea, used by MainWindow for downloads)
        self.start_time: datetime = None
        self.end_time: datetime = None

        # Track the last loaded RINEX path (for change detection / archiving)
        self.last_rinex_path: Path = None

        # BIA code priorities cache: provider -> series -> project -> {'GPS': set(), ...}
        self.bia_code_priorities = {}

        # SINEX validation result
        self._sinex_filename = None  # Stored until apply_ui_config() is called

        # Valid analysis centres from CDDIS scan
        self.valid_analysis_centers = []

        # Instantiate sub-controllers (one per config tab)
        self.general_tab = GeneralConfigController(ui, self)
        self.constellations_tab = ConstellationConfigController(ui, self)
        self.output_tab = OutputConfigController(ui, self)
        self.yaml_tab = YAMLConfigController(ui, self)

        # Top-level button wiring
        self.ui.observationsButton.clicked.connect(self.load_rnx_file)
        self.ui.outputButton.clicked.connect(self.load_output_dir)
        self.ui.processButton.clicked.connect(self.on_run_pea)
        self.ui.cddisCredentialsButton.clicked.connect(self._open_cddis_credentials_dialog)
        self.ui.userManualButton.clicked.connect(self._open_user_manual)

        # Initial button states
        self.ui.outputButton.setEnabled(False)
        self.ui.processButton.setEnabled(False)
        self.ui.stopAllButton.setEnabled(False)

        self._setup_top_level_tooltips()
        self.general_tab.setup_tooltips()
        self.constellations_tab.setup_tooltips()
        self.output_tab.setup_tooltips()
        self.yaml_tab.setup_tooltips()

    #region UI Tooltips

    def _setup_top_level_tooltips(self):
        """
        Set up tooltips and visual style for top-level action buttons.
        Sub-controller tooltips are set up in their own setup_tooltips() methods.
        """
        tooltip_style = """
                QToolTip {
                    background-color: #2c5d7c;
                    color: #ffffff;
                    border: 1px solid #999999;
                    padding: 4px;
                    border-radius: 3px;
                    font:13pt "Segoe UI";
                }
                """

        self.parent.setStyleSheet(self.parent.styleSheet() + tooltip_style)

        for btn in [self.ui.observationsButton, self.ui.outputButton,
                    self.ui.processButton, self.ui.stopAllButton,
                    self.ui.cddisCredentialsButton]:
            btn.setStyleSheet(btn.styleSheet() + tooltip_style)

        self.ui.observationsButton.setToolTip(
            "Select a RINEX observation file (.rnx or .rnx.gz).\n"
            "This will automatically extract metadata and populate the UI fields."
        )
        self.ui.outputButton.setToolTip(
            "Choose the directory where processing results will be saved.\n"
            "Existing .POS, .GPX, .TRACE, and .SNX output in this directory will be saved in the archived subdirectory."
        )
        self.ui.processButton.setToolTip(
            "Start the Ginan (PEA) PPP processing using the configured parameters.\n"
            "Ensure all required fields are filled before processing."
        )
        self.ui.stopAllButton.setToolTip(
            "Stop the Ginan (PEA) PPP processing.\n"
            "Will terminate all download threads and unlock the UI again."
        )
        self.ui.userManualButton.setToolTip(
            "Open the Ginan-UI User Manual\n"
            "Located in docs/USER_MANUAL.md"
        )
        self.ui.cddisCredentialsButton.setToolTip(
            "Set your NASA Earthdata credentials for downloading PPP products\n"
            "Required for accessing the CDDIS archive data"
        )

    #endregion

    #region File Selection

    def load_rnx_file(self):
        """
        UI handler: choose a RINEX file, extract metadata, update UI, and start PPP products query.
        """
        path = self._select_rnx_file(self.parent)
        if not path:
            return None

        current_rinex_path = Path(path).resolve()
        archive_products_if_rinex_changed(
            current_rinex=current_rinex_path,
            last_rinex=self.last_rinex_path,
            products_dir=INPUT_PRODUCTS_PATH
        )

        # Disable until new providers found
        if current_rinex_path != self.last_rinex_path:
            self.ui.processButton.setEnabled(False)
            self.ui.stopAllButton.setEnabled(False)
            self.general_tab.on_cddis_ready(pd.DataFrame(), False)  # Clears providers until worker completes

            # Stop any running BIA worker before clearing cache
            self.constellations_tab.stop_bia_worker()
            self.bia_code_priorities = {}
            self.constellations_tab.reset_list_styling()

        self.last_rinex_path = current_rinex_path
        self.rnx_file = str(current_rinex_path)

        Logger.workflow(f"📄 RINEX file selected: {self.rnx_file}")

        try:
            extractor = RinexExtractor(self.rnx_file)
            result = extractor.extract_rinex_data(self.rnx_file)

            # Verify antenna_type against .atx file
            if not self.parent.atx_required_for_rnx_extraction:
                Logger.workflow(
                    "⚠️ ANTEX (.atx) file not installed yet. Antenna type verification will be skipped.")
            else:
                self.general_tab.verify_antenna_type(result)

            Logger.workflow("🔍 Scanning CDDIS archive for PPP products. Please wait...")

            # Show toast notification
            show_toast(self.parent, "🔍 Scanning CDDIS archive for PPP products...", duration=15000)

            # Show waiting cursor during CDDIS scan
            self.parent.setCursor(Qt.CursorShape.WaitCursor)

            # Start CDDIS scan in background
            from scripts.GinanUI.app.models.dl_products import str_to_datetime
            start_epoch = str_to_datetime(result['start_epoch'])
            end_epoch = str_to_datetime(result['end_epoch'])
            self.general_tab.start_analysis_centre_scan(start_epoch, end_epoch)

            # Populate extracted metadata into the config panel immediately
            self.general_tab.populate_from_rinex(result)

            # Populate observation code list widgets
            self.constellations_tab.populate_observation_codes(result)

            self.ui.outputButton.setEnabled(True)
            self.ui.showConfigButton.setEnabled(True)

            Logger.workflow("⚒️ RINEX file metadata extracted and applied to UI fields")

            # Start SINEX validation in background
            self.general_tab.start_sinex_validation(
                target_date=start_epoch,
                marker_name=result.get("marker_name", ""),
                receiver_type=result.get("receiver_type", ""),
                antenna_type=result.get("antenna_type", ""),
                antenna_offset=result.get("antenna_offset", [0.0, 0.0, 0.0]),
                apriori_position=result.get("apriori_position"),
            )

            # Store marker number (DOMES number) for loading BLQ generation
            self._marker_number = result.get("marker_number")

        except Exception as e:
            Logger.workflow(f"Error extracting RNX metadata: {e}")
            return None

        # Always update MainWindow's state
        self.parent.rnx_file = self.rnx_file

        if self.output_dir:
            self.ready.emit(str(self.rnx_file), str(self.output_dir))

        return result

    def load_output_dir(self):
        """
        UI handler: choose the output directory and (if RNX is set) emit ready.
        """
        path = self._select_output_dir(self.parent)
        if not path:
            return

        # Ensure output_dir is a Path object
        self.output_dir = Path(path).resolve()
        Logger.workflow(f"📂 Output directory selected: {self.output_dir}")

        # Archive existing/old outputs
        visual_dir = self.output_dir / "visual"
        archive_old_outputs(self.output_dir, visual_dir)

        # Enable process button
        # MainWindow owns when to enable processButton. This controller exposes a helper if needed.
        self.try_enable_process_button()

        # Always update MainWindow's state
        self.parent.output_dir = self.output_dir

        if self.rnx_file:
            self.ready.emit(str(self.rnx_file), str(self.output_dir))

    #endregion

    #region PEA Processing

    def on_run_pea(self):
        """
        UI handler: validate time window and config, apply UI, then emit pea_ready.
        If YAML overwrite is disabled, skip applying UI values to the config file
        but still store loading BLQ parameters for ensure_loading_blq().
        """
        raw = self.ui.timeWindowValue.text()

        try:
            start_str, end_str = raw.split("to")
            start_time = datetime.strptime(start_str.strip(), "%Y-%m-%d_%H:%M:%S")
            end_time = datetime.strptime(end_str.strip(), "%Y-%m-%d_%H:%M:%S")
        except ValueError:
            QMessageBox.warning(
                None,
                "Format error",
                "Time window must be in the format:\n"
                "YYYY-MM-DD_HH:MM:SS to YYYY-MM-DD_HH:MM:SS"
            )
            return

        if start_time > end_time:
            QMessageBox.warning(None, "Time error", "Start time cannot be later than end time.")
            return

        if not getattr(self, "config_path", None):
            QMessageBox.warning(
                None,
                "No config file",
                "Please click Show config and select a YAML file first."
            )
            return

        self.start_time = start_time
        self.end_time = end_time

        yaml_overwrite = self.yaml_tab.get_yaml_toggles()

        # Warn the user when YAML overwrite is disabled so they are aware
        # that UI changes will not be applied to the config file
        if not yaml_overwrite:
            reply = QMessageBox.warning(
                self.parent,
                "YAML Overwrite Disabled",
                "YAML config overwrite is disabled. The config file on disk will be "
                "used as-is, and any UI changes (RINEX file, constellations, output "
                "settings, etc.) will NOT be applied.\n\n"
                "Are you sure you want to continue?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No
            )
            if reply != QMessageBox.StandardButton.Yes:
                return

        try:
            self.execution.reload_config()

            if yaml_overwrite:
                inputs = self.extract_ui_values(self.rnx_file)
                self.execution.apply_ui_config(inputs)
                self.execution.write_cached_changes()
            else:
                # Still need to store loading BLQ parameters even when not overwriting,
                # so that ensure_loading_blq() can generate BLQ files if needed
                self.execution.set_loading_params(
                    marker_name=self.extract_marker_name(self.rnx_file),
                    marker_number=getattr(self, '_marker_number', None),
                    apriori_position=self.parse_apriori_position(self.ui.aprioriPositionButton.text()),
                )
                self.execution.yaml_overwrite = False
        except Exception as e:
            Logger.workflow(f"⚠️ Failed to apply config: {e}")
            return

        self.pea_ready.emit()

    def _reset_ui_to_defaults(self):
        """
        Reset all UI fields to their default/initial states.
        This is the "start from scratch" reset that clears all user inputs.
        """
        self.rnx_file = None
        self.output_dir = None
        self.products_df = pd.DataFrame()
        self.last_rinex_path = None
        self.valid_analysis_centers = []
        self.start_time = None
        self.end_time = None
        for attr in ['_valid_project_series_df', '_valid_series_for_provider']:
            if hasattr(self, attr):
                delattr(self, attr)

        self._marker_number = None

        self.parent.rnx_file = None
        self.parent.output_dir = None

        # Delegate to each tab controller
        self.general_tab.reset_to_defaults()
        self.constellations_tab.reset_to_defaults()
        self.output_tab.reset_to_defaults()
        self.yaml_tab.reset_to_defaults()

        # Reset button states
        self.ui.outputButton.setEnabled(False)
        self.ui.showConfigButton.setEnabled(False)
        self.ui.processButton.setEnabled(False)
        self.ui.stopAllButton.setEnabled(False)
        self.ui.observationsButton.setEnabled(True)
        self.ui.cddisCredentialsButton.setEnabled(True)

        # Reset visualisation panel
        if hasattr(self.parent, 'visCtrl'):
            self.parent.visCtrl.set_html_files([])
            if hasattr(self.ui, 'webEngineView'):
                self.ui.webEngineView.setHtml("")

        if hasattr(self.ui, 'configTabWidget'):
            self.ui.configTabWidget.setCurrentIndex(0)

    def try_enable_process_button(self):
        """
        Enable the Process button when RNX, output path, and metadata are ready.
        """
        if not self.parent.metadata_downloaded:
            return
        if not self.output_dir:
            return
        if not self.rnx_file:
            return
        if len(self.valid_analysis_centers) < 1:
            return
        self.ui.processButton.setEnabled(True)

    def extract_ui_values(self, rnx_path):
        """
        Extract current UI values, parse/normalise them, and return as dataclass.

        Arguments:
          rnx_path (str): Selected RINEX observation file path.

        Returns:
          ExtractedInputs: Dataclass containing parsed fields and raw strings.
        """
        mode_raw = self.ui.modeCombo.currentText() if self.ui.modeCombo.currentText() != "Select one" else "Static"
        constellations_raw = self.general_tab.get_selected_constellations_text()
        time_window_raw = self.ui.timeWindowValue.text()
        epoch_interval_raw = self.ui.dataIntervalButton.text()
        receiver_type = self.ui.receiverTypeValue.text()
        antenna_type = self.ui.antennaTypeValue.text()
        antenna_offset_raw = self.ui.antennaOffsetButton.text()
        apriori_position_raw = self.ui.aprioriPositionButton.text()
        ppp_provider = self.ui.pppProviderCombo.currentText() if self.ui.pppProviderCombo.currentText() != "Select one" else ""
        ppp_series = self.ui.pppSeriesCombo.currentText() if self.ui.pppSeriesCombo.currentText() != "Select one" else ""
        ppp_project = self.ui.pppProjectCombo.currentText() if self.ui.pppProjectCombo.currentText() != "Select one" else ""

        obs_codes = self.constellations_tab.extract_observation_codes()
        gpx_output, pos_output, trace_output_network, snx_output = self.output_tab.get_output_toggles()

        start_epoch, end_epoch = self.parse_time_window(time_window_raw)
        antenna_offset = self.parse_antenna_offset(antenna_offset_raw)
        apriori_position = self.parse_apriori_position(apriori_position_raw)
        epoch_interval = float(epoch_interval_raw.replace("s", "").strip())
        marker_name = self.extract_marker_name(rnx_path)
        mode = self.determine_mode_value(mode_raw)

        return self.ExtractedInputs(
            marker_name=marker_name,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            epoch_interval=epoch_interval,
            rinex_epoch_interval=getattr(self, 'rinex_epoch_interval', epoch_interval),
            antenna_offset=antenna_offset,
            apriori_position=apriori_position,
            mode=mode,
            constellations_raw=constellations_raw,
            receiver_type=receiver_type,
            antenna_type=antenna_type,
            ppp_provider=ppp_provider,
            ppp_series=ppp_series,
            ppp_project=ppp_project,
            rnx_path=rnx_path,
            output_path=str(self.output_dir),
            gps_codes=obs_codes.get('gps', []),
            gal_codes=obs_codes.get('gal', []),
            glo_codes=obs_codes.get('glo', []),
            bds_codes=obs_codes.get('bds', []),
            qzs_codes=obs_codes.get('qzs', []),
            gpx_output=gpx_output,
            pos_output=pos_output,
            trace_output_network=trace_output_network,
            snx_output=snx_output,
            sinex_filename=self._sinex_filename,
            marker_number=getattr(self, '_marker_number', None),
        )

    #endregion

    #region User Manual

    def _open_user_manual(self):
        """
        Open the USER_MANUAL.md file
        Attempts to open the file in the system's default markdown viewer / browser
        """
        try:
            # Get the path from common_dirs
            manual_path = USER_MANUAL_PATH

            if not manual_path.exists():
                Logger.workflow(f"⚠️ User manual not found at: {manual_path}")
                QMessageBox.warning(
                    self.parent,
                    "User Manual Not Found",
                    f"Could not find the user manual at:\n{manual_path}\n\n"
                    "Please ensure the file exists at /docs/USER_MANUAL.md"
                )
                return

            Logger.workflow(f"📖 Opening user manual: {manual_path}")

            # Try to open the file with the default application
            if os.name == 'nt':  # Windows
                os.startfile(manual_path)
            elif os.name == 'posix':  # macOS and Linux
                if subprocess.call(['which', 'xdg-open'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL) == 0:
                    subprocess.Popen(['xdg-open', str(manual_path)])
                elif subprocess.call(['which', 'open'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL) == 0:
                    subprocess.Popen(['open', str(manual_path)])
                else:
                    # Fall back to browser
                    webbrowser.open(f'file://{manual_path.absolute()}')
            else:
                # Fall back to browser for other platforms
                webbrowser.open(f'file://{manual_path.absolute()}')

        except Exception as e:
            Logger.workflow(f"⚠️ Failed to open user manual: {e}")
            QMessageBox.critical(self.parent, "Error Opening Manual", f"Failed to open the user manual:\n{e}")

    #endregion

    # region Thread Management

    def stop_all(self):
        """
        Best-effort stop for all background workers managed by the controller and sub-controllers.
        """
        try:
            self.general_tab.stop_all_workers()
            self.constellations_tab.stop_bia_worker()
            if hasattr(self, "parent"):
                self.parent.setCursor(Qt.CursorShape.ArrowCursor)
        except Exception:
            pass

    # endregion

    #region Static Helpers

    @staticmethod
    def _select_rnx_file(parent) -> str:
        """
        Open a file dialog to select a RINEX observation file.

        Arguments:
          parent: Parent widget.

        Returns:
          str: Selected file path or empty string.
        """
        path, _ = QFileDialog.getOpenFileName(
            parent,
            "Select RINEX Observation File",
            "",
            "RINEX Observation Files (*.rnx *.rnx.gz *.[0-9][0-9]o *.[0-9][0-9]o.gz *.obs *.obs.gz);;All Files (*.*)"
        )
        return path or ""

    @staticmethod
    def _select_output_dir(parent) -> str:
        """
        Open a directory dialog to select the output folder.

        Arguments:
          parent: Parent widget.

        Returns:
          str: Selected directory path or empty string.
        """
        path = QFileDialog.getExistingDirectory(parent, "Select Output Directory")
        return path or ""

    @staticmethod
    def determine_mode_value(mode_raw: str) -> int:
        """
        Map a mode label to its numeric value used by backend.

        Arguments:
          mode_raw (str): One of 'Static', 'Kinematic', 'Dynamic'.

        Returns:
          int: 0 for Static, 30 for Kinematic, 100 for Dynamic.

        Example:
          >>> determine_mode_value("Static")
          0
        """
        if mode_raw == "Static":
            return 0
        elif mode_raw == "Kinematic":
            return 30
        elif mode_raw == "Dynamic":
            return 100
        else:
            raise ValueError(f"Unknown mode: {mode_raw!r}")

    @staticmethod
    def extract_marker_name(rnx_path: str) -> str:
        """
        Extract a 4-char site code (marker) from a RINEX filename.

        Arguments:
          rnx_path (str): RNX file path. If empty/invalid, returns 'TEST'.

        Returns:
          str: Upper-cased 4-char marker or 'TEST' when not found.

        Example:
          >>> extract_marker_name("ALIC00AUS_R_20250190000_01D_30S_MO.rnx.gz")
          'ALIC'
        """
        if not rnx_path:
            return "TEST"
        stem = Path(rnx_path).stem  # drops .gz/.rnx
        m = re.match(r"([A-Za-z0-9]{4})", stem)
        return m.group(1).upper() if m else "TEST"

    @staticmethod
    def parse_time_window(time_window_raw: str):
        """
        Convert 'start_time to end_time' into (start_epoch, end_epoch) strings.

        Arguments:
          time_window_raw (str): e.g., 'YYYY-MM-DD_HH:MM:SS to YYYY-MM-DD_HH:MM:SS'.

        Returns:
          tuple[str, str]: (start_epoch, end_epoch) with underscores replaced by spaces.

        Example:
          >>> parse_time_window("2025-01-01_00:00:00 to 2025-01-02_00:00:00")
          ('2025-01-01 00:00:00', '2025-01-02 00:00:00')
        """
        try:
            start, end = map(str.strip, time_window_raw.split("to"))

            # Replace underscores with spaces in datetime strings
            start = start.replace("_", " ")
            end = end.replace("_", " ")
            return start, end
        except ValueError:
            raise ValueError("Invalid time_window format. Expected: 'start_time to end_time'")

    @staticmethod
    def parse_antenna_offset(antenna_offset_raw: str):
        """
        Convert 'e, n, u' string into [e, n, u] floats.

        Arguments:
          antenna_offset_raw (str): e.g., '0.0, 0.0, 1.234'.

        Returns:
          list[float]: [e, n, u] in metres.

        Example:
          >>> parse_antenna_offset("0.1, -0.2, 1.0")
          [0.1, -0.2, 1.0]
        """
        try:
            e, n, u = map(str.strip, antenna_offset_raw.split(","))
            return [float(e), float(n), float(u)]
        except ValueError:
            raise ValueError("Invalid antenna offset format. Expected: 'e, n, u'")

    @staticmethod
    def parse_apriori_position(apriori_position_raw: str):
        """
        Convert 'x, y, z' string into [x, y, z] floats.

        Arguments:
          apriori_position_raw (str): e.g., '2765120.6553, -4449249.8563, -3626405.2770'.

        Returns:
          list[float]: [x, y, z] in metres (ECEF coordinates).

        Example:
          >>> parse_apriori_position("2765120.6553, -4449249.8563, -3626405.2770")
          [2765120.6553, -4449249.8563, -3626405.2770]
        """
        try:
            x, y, z = map(str.strip, apriori_position_raw.split(","))
            return [float(x), float(y), float(z)]
        except ValueError:
            raise ValueError("Invalid apriori position format. Expected: 'x, y, z'")

    @staticmethod
    def _get_mode_items() -> List[str]:
        """
         Provide available processing modes for the UI combo.

         Returns:
           list[str]: ['Static', 'Kinematic', 'Dynamic']

         Example:
           >>> InputController._get_mode_items()
           ['Static', 'Kinematic', 'Dynamic']
         """
        return ["Static", "Kinematic", "Dynamic"]

    @staticmethod
    def _get_constellations_items() -> List[str]:
        """
        Provide available GNSS constellations for the UI combo.

        Returns:
          list[str]: ['GPS', 'GAL', 'GLO', 'BDS', 'QZS']

        Example:
          >>> InputController._get_constellations_items()
          ['GPS', 'GAL', 'GLO', 'BDS', 'QZS']
        """
        return ["GPS", "GAL", "GLO", "BDS", "QZS"]

    @staticmethod
    def _get_ppp_series_items() -> List[str]:
        """
         Provide available PPP series codes for the UI combo.

         Returns:
           list[str]: ['ULT', 'RAP', 'FIN']

         Example:
           >>> InputController._get_ppp_series_items()
           ['ULT', 'RAP', 'FIN']
         """
        return ["ULT", "RAP", "FIN"]

    #endregion

    #region ExtractedInputs Dataclass

    @dataclass
    class ExtractedInputs:
        """
        Dataclass container for parsed UI values and raw strings.
        Produced by extract_ui_values() and consumed by Execution.apply_ui_config().
        """
        # Parsed / derived values
        marker_name: str
        start_epoch: str
        end_epoch: str
        epoch_interval: float
        rinex_epoch_interval: float
        antenna_offset: list[float]
        apriori_position: list[float]
        mode: int

        # Raw strings / controls that are needed downstream
        constellations_raw: str
        receiver_type: str
        antenna_type: str
        ppp_provider: str
        ppp_series: str
        ppp_project: str

        # File paths associated to this run
        rnx_path: str
        output_path: str

        # Observation codes for each constellation
        gps_codes: list[str] = None
        gal_codes: list[str] = None
        glo_codes: list[str] = None
        bds_codes: list[str] = None
        qzs_codes: list[str] = None

        # Output toggles
        gpx_output: bool = True
        pos_output: bool = True
        trace_output_network: bool = False
        snx_output: bool = False

        sinex_filename: str = None
        marker_number: str = None

    #endregion

    #region CDDIS Credentials Dialog

    def _open_cddis_credentials_dialog(self):
        """
        UI handler: open the CDDIS credentials dialog for Earthdata login.
        """
        dialog = CredentialsDialog(self.parent)
        dialog.exec()

    #endregion

#region CDDIS Credentials Dialog Class

class CredentialsDialog(QDialog):
    """
    Modal dialog for entering NASA Earthdata credentials (username/password).
    Saves credentials to .netrc for CDDIS access.
    """

    def __init__(self, parent=None):
        """
        Initialise credential input widgets and layout.

        Arguments:
          parent: Optional parent widget.
        """
        super().__init__(parent)
        self.setWindowTitle("CDDIS Credentials")

        layout = QVBoxLayout()

        # Username
        layout.addWidget(QLabel("Username:"))
        self.username_input = QLineEdit()
        layout.addWidget(self.username_input)

        # Password
        layout.addWidget(QLabel("Password:"))
        self.password_input = QLineEdit()
        self.password_input.setEchoMode(QLineEdit.Password)
        layout.addWidget(self.password_input)

        # Confirm button
        self.confirm_button = QPushButton("Save")
        self.confirm_button.clicked.connect(self.save_credentials)
        layout.addWidget(self.confirm_button)

        self.setLayout(layout)

    def save_credentials(self):
        """
        Validate username/password, save to netrc, and close dialog.
        """
        username = self.username_input.text().strip()
        password = self.password_input.text().strip()

        if not username or not password:
            QMessageBox.warning(self, "Error", "Username and password cannot be empty")
            return

        # ✅ Save correctly in one go (Windows will write both %USERPROFILE%\\.netrc and %USERPROFILE%\\_netrc;
        #    macOS/Linux will write ~/.netrc and automatically chmod 600; both URS and CDDIS entries are written)
        try:
            paths = save_earthdata_credentials(username, password)
        except Exception as e:
            QMessageBox.critical(self, "Save failed", f"❌ Failed to save credentials:\n{e}")
            return

        QMessageBox.information(self, "Success",
                                "✅ Credentials saved to:\n" + "\n".join(str(p) for p in paths))
        self.accept()

#endregion