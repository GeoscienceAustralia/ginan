# app/controllers/input_controller.py
"""
UI input flow controller for the Ginan-UI.
"""
from __future__ import annotations

import os
import re
import subprocess
import webbrowser
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Callable, List
from decimal import Decimal, InvalidOperation

import pandas as pd

from scripts.GinanUI.app.utils.logger import Logger

from scripts.GinanUI.app.models.dl_products import (
    get_valid_analysis_centers,
    get_valid_series_for_provider,
    get_valid_providers_with_series,
    str_to_datetime
)
from PySide6.QtCore import QObject, Signal, Qt, QDateTime, QThread
from PySide6.QtGui import QStandardItemModel, QStandardItem, QColor, QBrush
from PySide6.QtWidgets import (
    QFileDialog,
    QDialog,
    QFormLayout,
    QDoubleSpinBox,
    QHBoxLayout,
    QVBoxLayout,
    QDateTimeEdit,
    QInputDialog,
    QMessageBox,
    QComboBox,
    QLineEdit,
    QPushButton,
    QLabel,
    QListWidget,
    QListWidgetItem,
    QAbstractItemView,
    QSizePolicy,
    QWidget
)

from scripts.GinanUI.app.models.execution import Execution, GENERATED_YAML, INPUT_PRODUCTS_PATH
from scripts.GinanUI.app.utils.common_dirs import USER_MANUAL_PATH
from scripts.GinanUI.app.models.rinex_extractor import RinexExtractor
from scripts.GinanUI.app.utils.cddis_credentials import save_earthdata_credentials
from scripts.GinanUI.app.models.archive_manager import (archive_products_if_rinex_changed)
from scripts.GinanUI.app.models.archive_manager import archive_old_outputs
from scripts.GinanUI.app.utils.workers import DownloadWorker, BiasProductWorker
from scripts.GinanUI.app.utils.toast import show_toast


class InputController(QObject):
    """
    UI controller class InputController.
    """

    ready = Signal(str, str)  # rnx_path, output_path
    pea_ready = Signal()  # emitted when PEA processing should start

    def __init__(self, ui, parent_window, execution: Execution):
        """
        UI handler: init.

        Arguments:
          ui (Any): Main window UI instance (generated from Qt .ui).
          parent_window (Any): Parent widget/window to anchor dialogs.
          execution (Execution): Backend execution bridge used to read/apply UI config.
        """
        super().__init__()
        self.ui = ui
        self.parent = parent_window
        self.execution = execution

        self.rnx_file: Path = None
        self.output_dir: Path = None
        self.products_df: pd.DataFrame = pd.DataFrame()  # CDDIS replaces with a populated dataframe

        # Config file path
        self.config_path = GENERATED_YAML

        ### Wire: file selection buttons ###
        self.ui.observationsButton.clicked.connect(self.load_rnx_file)
        self.ui.outputButton.clicked.connect(self.load_output_dir)

        # Initial states
        self.ui.outputButton.setEnabled(False)
        self.ui.showConfigButton.setEnabled(False)
        self.ui.processButton.setEnabled(False)
        self.ui.stopAllButton.setEnabled(False)

        ### Bind: configuration drop-downs / UIs ###

        self._bind_combo(self.ui.modeCombo, self._get_mode_items)

        # PPP provider, project and series
        self.ui.pppProviderCombo.currentTextChanged.connect(self._on_ppp_provider_changed)
        self.ui.pppProjectCombo.currentTextChanged.connect(self._on_ppp_project_changed)
        self.ui.pppSeriesCombo.currentTextChanged.connect(self._on_ppp_series_changed)

        # Constellations
        self._bind_multiselect_combo(
            self.ui.constellationsCombo,
            self._get_constellations_items,
            self.ui.constellationsValue,
            placeholder="Select one or more",
        )

        # Receiver/Antenna types: free-text input
        self._enable_free_text_for_receiver_and_antenna()

        # Antenna offset
        self.ui.antennaOffsetButton.clicked.connect(self._open_antenna_offset_dialog)
        self.ui.antennaOffsetButton.setCursor(Qt.CursorShape.PointingHandCursor)
        self.ui.antennaOffsetValue.setText("0.0, 0.0, 0.0")

        # Time window and data interval
        self.ui.timeWindowButton.clicked.connect(self._open_time_window_dialog)
        self.ui.timeWindowButton.setCursor(Qt.CursorShape.PointingHandCursor)
        self.ui.dataIntervalButton.clicked.connect(self._open_data_interval_dialog)
        self.ui.dataIntervalButton.setCursor(Qt.CursorShape.PointingHandCursor)

        # Run buttons
        self.ui.showConfigButton.clicked.connect(self.on_show_config)
        self.ui.showConfigButton.setCursor(Qt.CursorShape.PointingHandCursor)
        self.ui.processButton.clicked.connect(self.on_run_pea)

        # CDDIS credentials dialog
        self.ui.cddisCredentialsButton.clicked.connect(self._open_cddis_credentials_dialog)

        # Reset config button
        self.ui.resetConfigButton.clicked.connect(self._on_reset_config_clicked)

        # User manual button
        self.ui.userManualButton.clicked.connect(self._open_user_manual)

        self.setup_tooltips()

        # Initialise "Constellations" placeholder
        self._setup_constellation_placeholder()
        self._hide_all_constellation_widgets()

        # Track threads that are pending cleanup (threads that are cancelled but not yet finished)
        self._pending_threads = []

        # BIA code priorities cache: provider -> series -> project -> {'GPS': set(), ...}
        self.bia_code_priorities = {}
        self._bia_loading = False
        self._bia_worker = None
        self._bia_thread = None

        # Connect tab change signal to trigger BIA fetch when switching to Constellations tab
        self.ui.configTabWidget.currentChanged.connect(self._on_config_tab_changed)

    def setup_tooltips(self):
        """
        UI handler: setup tooltips and visual style for key controls.
        """

        # Consistent tooltip style for all elements
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

        # Apply to parent window
        self.parent.setStyleSheet(self.parent.styleSheet() + tooltip_style)

        # Add tooltip styling to buttons without changing their appearance
        # Just append the tooltip style to their existing styles

        # Get current styles and append tooltip styling
        obs_style = self.ui.observationsButton.styleSheet() + tooltip_style
        out_style = self.ui.outputButton.styleSheet() + tooltip_style
        proc_style = self.ui.processButton.styleSheet() + tooltip_style
        stop_style = self.ui.stopAllButton.styleSheet() + tooltip_style
        cddis_style = self.ui.cddisCredentialsButton.styleSheet() + tooltip_style

        self.ui.observationsButton.setStyleSheet(obs_style)
        self.ui.outputButton.setStyleSheet(out_style)
        self.ui.processButton.setStyleSheet(proc_style)
        self.ui.stopAllButton.setStyleSheet(stop_style)
        self.ui.cddisCredentialsButton.setStyleSheet(cddis_style)

        # File selection buttons
        self.ui.observationsButton.setToolTip(
            "Select a RINEX observation file (.rnx or .rnx.gz).\n"
            "This will automatically extract metadata and populate the UI fields."
        )

        self.ui.outputButton.setToolTip(
            "Choose the directory where processing results will be saved.\n"
            "Existing .POS or .GPX output in this directory will be saved in the archived subdirectory."
        )

        self.ui.processButton.setToolTip(
            "Start the Ginan (PEA) PPP processing using the configured parameters.\n"
            "Ensure all required fields are filled before processing."
        )

        self.ui.stopAllButton.setToolTip(
            "Stop the Ginan (PEA) PPP processing.\n"
            "Will terminate all download threads and unlock the UI again."
        )

        # Configuration buttons
        self.ui.showConfigButton.setToolTip(
            "Generate and open the YAML configuration file.\n"
            "You can review and modify advanced settings before processing.\n"
            "Note: UI defined parameters will ALWAYS override manual config edits."
        )

        self.ui.resetConfigButton.setToolTip(
            "Delete and regenerate the YAML configuration file and start from a clean slate.\n"
            "Note: Will delete all modifications to the existing file!"
        )

        self.ui.userManualButton.setToolTip(
            "Open the Ginan-UI User Manual\n"
            "Located in docs/USER_MANUAL.md"
        )

        self.ui.cddisCredentialsButton.setToolTip(
            "Set your NASA Earthdata credentials for downloading PPP products\n"
            "Required for accessing the CDDIS archive data"
        )

        # Input fields and combos
        self.ui.modeCombo.setToolTip(
            "Processing mode:\n"
            "• Static: For stationary receivers\n"
            "• Kinematic: For moving receivers\n"
            "• Dynamic: For high-dynamic applications"
        )

        self.ui.constellationsCombo.setToolTip(
            "Select which GNSS constellations to use:\n"
            "GPS, Galileo (GAL), GLONASS (GLO), BeiDou (BDS), QZSS (QZS)\n"
            "More constellations generally improve accuracy"
        )

        self.ui.pppProviderCombo.setToolTip(
            "Analysis centre that provides PPP products\n"
            "Options populated based on your observation time window"
        )

        self.ui.pppProjectCombo.setToolTip(
            "PPP product project type.\n"
            "Different projects types offer varying GNSS constellation PPP products."
        )

        self.ui.pppSeriesCombo.setToolTip(
            "PPP product series:\n"
            "• ULT: Ultra-rapid (lower latency)\n"
            "• RAP: Rapid \n"
            "• FIN: Final (highest accuracy)"
        )

        # Receiver/Antenna fields
        self.ui.receiverTypeCombo.setToolTip(
            "Receiver model extracted from RINEX header\n"
            "Click to manually edit if needed"
        )

        self.ui.antennaTypeCombo.setToolTip(
            "Antenna model extracted from RINEX header\n"
            "Must match entries in the ANTEX (.atx) calibration file\n"
            "Click to manually edit if needed"
        )

        # Time and offset buttons
        self.ui.timeWindowButton.setToolTip(
            "Observation time window extracted from RINEX file\n"
            "Click to adjust start and end times for processing"
        )

        self.ui.dataIntervalButton.setToolTip(
            "Data sampling interval in seconds\n"
            "Click to change the processing interval"
        )

        self.ui.antennaOffsetButton.setToolTip(
            "Antenna reference point offset in metres (East, North, Up)\n"
            "Typically extracted from RINEX header\n"
            "Click to modify if needed"
        )

        # Value display labels
        self.ui.receiverTypeValue.setToolTip("Receiver type from RINEX header")
        self.ui.antennaTypeValue.setToolTip("Antenna type from RINEX header")
        self.ui.constellationsValue.setToolTip("Available constellations in RINEX data")
        self.ui.timeWindowValue.setToolTip("Observation time span")
        self.ui.dataIntervalValue.setToolTip("Data sampling interval")
        self.ui.antennaOffsetValue.setToolTip("Antenna offset: East, North, Up (metres)")

        # Observation code list widget tooltips
        if hasattr(self.ui, 'gpsListWidget'):
            self.ui.gpsListWidget.setToolTip(
                "GPS observation codes\n"
                "✓ Check / uncheck to enable / disable codes\n"
                "↕ Drag and drop to set priority order (top = highest priority)"
            )
        if hasattr(self.ui, 'galListWidget'):
            self.ui.galListWidget.setToolTip(
                "Galileo observation codes\n"
                "✓ Check / uncheck to enable / disable codes\n"
                "↕ Drag and drop to set priority order (top = highest priority)"
            )
        if hasattr(self.ui, 'gloListWidget'):
            self.ui.gloListWidget.setToolTip(
                "GLONASS observation codes\n"
                "✓ Check / uncheck to enable / disable codes\n"
                "↕ Drag and drop to set priority order (top = highest priority)"
            )
        if hasattr(self.ui, 'bdsListWidget'):
            self.ui.bdsListWidget.setToolTip(
                "BeiDou observation codes\n"
                "✓ Check / uncheck to enable / disable codes\n"
                "↕ Drag and drop to set priority order (top = highest priority)"
            )
        if hasattr(self.ui, 'qzsListWidget'):
            self.ui.qzsListWidget.setToolTip(
                "QZSS observation codes\n"
                "✓ Check / uncheck to enable / disable codes\n"
                "↕ Drag and drop to set priority order (top = highest priority)"
            )

        self.ui.posCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a Positioning Solution (.POS) file"
        )

        self.ui.gpxCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a GPS Exchange Format (.GPX) file"
        )

        self.ui.traceCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a trace log (.TRACE) file"
        )

    def _hide_all_constellation_widgets(self):
        """
        Hide all constellation labels and list widgets on startup.
        They will be shown when a RINEX file is loaded and constellations are selected.
        """
        widget_names = [
            'gpsLabel', 'gpsListWidget',
            'galLabel', 'galListWidget',
            'gloLabel', 'gloListWidget',
            'bdsLabel', 'bdsListWidget',
            'qzsLabel', 'qzsListWidget',
        ]

        for widget_name in widget_names:
            if hasattr(self.ui, widget_name):
                widget = getattr(self.ui, widget_name)
                widget.setVisible(False)

    def _open_cddis_credentials_dialog(self):
        """
        UI handler: open the CDDIS credentials dialog for Earthdata login.
        """
        dialog = CredentialsDialog(self.parent)
        dialog.exec()

    # region File Selection + Metadata Extraction + PPP product selection
    def load_rnx_file(self) -> ExtractedInputs | None:
        """
        UI handler: choose a RINEX file, extract metadata, update UI, and start PPP products query.
        """
        path = self._select_rnx_file(self.parent)
        if not path:
            return None

        current_rinex_path = Path(path).resolve()
        archive_products_if_rinex_changed(
            current_rinex=current_rinex_path,
            last_rinex=getattr(self, "last_rinex_path", None),
            products_dir=INPUT_PRODUCTS_PATH
        )
        # Disable until new providers found
        if current_rinex_path != getattr(self, "last_rinex_path", None):
            self.ui.processButton.setEnabled(False)
            self.ui.stopAllButton.setEnabled(False)
            self._on_cddis_ready(pd.DataFrame(), False)  # Clears providers until worker completes

            # Stop any running BIA worker before clearing cache
            self._stop_bia_worker()
            # Clear BIA code priorities cache when RINEX file changes
            self.bia_code_priorities = {}
            self._reset_constellation_list_styling()

        self.last_rinex_path = current_rinex_path
        self.rnx_file = str(current_rinex_path)

        Logger.terminal(f"📄 RINEX file selected: {self.rnx_file}")

        try:
            extractor = RinexExtractor(self.rnx_file)
            result = extractor.extract_rinex_data(self.rnx_file)

            # Verify antenna_type against .atx file
            if not self.parent.atx_required_for_rnx_extraction:
                Logger.terminal(
                    "⚠️ ANTEX (.atx) file not installed yet. Antenna type verification will be skipped.")
            else:
                self.verify_antenna_type(result)

            Logger.terminal("🔍 Scanning CDDIS archive for PPP products. Please wait...")

            # Show toast notification
            show_toast(self.parent, "🔍 Scanning CDDIS archive for PPP products...", duration=15000)

            # Show waiting cursor during CDDIS scan
            self.parent.setCursor(Qt.CursorShape.WaitCursor)

            # Retrieve valid analysis centers
            start_epoch = str_to_datetime(result['start_epoch'])
            end_epoch = str_to_datetime(result['end_epoch'])

            # Clean up any existing analysis centre threads before starting a new one
            self._cleanup_analysis_thread()

            self.worker = DownloadWorker(start_epoch=start_epoch, end_epoch=end_epoch, analysis_centers=True)
            self.metadata_thread = QThread()
            self.worker.moveToThread(self.metadata_thread)

            self.worker.finished.connect(self._on_cddis_ready)
            self.worker.finished.connect(self._restore_cursor)
            self.worker.cancelled.connect(self._on_cddis_cancelled)
            self.worker.cancelled.connect(self._restore_cursor)
            self.worker.constellation_info.connect(self._on_constellation_info_received)

            # Connect both finished and cancelled to thread quit
            self.worker.finished.connect(self.metadata_thread.quit)
            self.worker.cancelled.connect(self.metadata_thread.quit)
            self.metadata_thread.finished.connect(self._on_analysis_thread_finished)
            self.metadata_thread.started.connect(self.worker.run)
            self.metadata_thread.start()

            # Populate extracted metadata immediately
            self.ui.constellationsValue.setText(result["constellations"])
            self.ui.timeWindowValue.setText(f"{result['start_epoch']} to {result['end_epoch']}")
            self.ui.timeWindowButton.setText(f"{result['start_epoch']} to {result['end_epoch']}")
            self.ui.dataIntervalButton.setText(f"{result['epoch_interval']} s")
            self.ui.receiverTypeValue.setText(result["receiver_type"])
            self.ui.antennaTypeValue.setText(result["antenna_type"])
            self.ui.antennaOffsetValue.setText(", ".join(map(str, result["antenna_offset"])))
            self.ui.antennaOffsetButton.setText(", ".join(map(str, result["antenna_offset"])))

            self.ui.receiverTypeCombo.clear()
            self.ui.receiverTypeCombo.addItem(result["receiver_type"])
            self.ui.receiverTypeCombo.setCurrentIndex(0)
            self.ui.receiverTypeCombo.lineEdit().setText(result["receiver_type"])

            self.ui.antennaTypeCombo.clear()
            self.ui.antennaTypeCombo.addItem(result["antenna_type"])
            self.ui.antennaTypeCombo.setCurrentIndex(0)
            self.ui.antennaTypeCombo.lineEdit().setText(result["antenna_type"])

            self._update_constellations_multiselect(result["constellations"])

            # Populate observation code combos if available
            self._populate_observation_code_combos(result)

            self.ui.outputButton.setEnabled(True)
            self.ui.showConfigButton.setEnabled(True)

            Logger.terminal("⚒️ RINEX file metadata extracted and applied to UI fields")
            self.ui.outputButton.setEnabled(True)
            self.ui.showConfigButton.setEnabled(True)

        except Exception as e:
            Logger.terminal(f"Error extracting RNX metadata: {e}")
            return None

        # Always update MainWindow's state
        self.parent.rnx_file = self.rnx_file

        if self.output_dir:
            self.ready.emit(str(self.rnx_file), str(self.output_dir))

        return result

    def _populate_observation_code_combos(self, result: dict):
        """
        Populate the observation code list widgets with available codes from RINEX.

        Arguments:
          result (dict): Dictionary containing observation code lists for each constellation
        """
        # Map constellation names to list widgets and result keys
        list_widget_mapping = {
            'GPS': ('obs_types_gps', 'enabled_gps', 'gpsListWidget'),
            'GAL': ('obs_types_gal', 'enabled_gal', 'galListWidget'),
            'GLO': ('obs_types_glo', 'enabled_glo', 'gloListWidget'),
            'BDS': ('obs_types_bds', 'enabled_bds', 'bdsListWidget'),
            'QZS': ('obs_types_qzs', 'enabled_qzs', 'qzsListWidget')
        }

        populated_constellations = []

        for const_name, (result_key, enabled_key, widget_name) in list_widget_mapping.items():
            if not hasattr(self.ui, widget_name):
                continue

            list_widget = getattr(self.ui, widget_name)
            codes = result.get(result_key, [])
            enabled_codes = result.get(enabled_key, set())

            if codes and len(codes) > 0:
                self._setup_observation_code_list_widget(list_widget, codes, enabled_codes)
                populated_constellations.append(const_name)
            else:
                # Clear and disable list widget if no codes available
                list_widget.clear()
                list_widget.setEnabled(False)

        # Log summary message
        if populated_constellations:
            Logger.terminal(f"✅ Populated observation codes for {', '.join(populated_constellations)}")
        else:
            Logger.terminal("⚠️ No observation codes found in RINEX")

    def _setup_observation_code_list_widget(self, list_widget: QListWidget, codes: List[str], enabled_codes: set):
        """
        Set up a list widget with drag-drop reordering and checkboxes for observation codes.

        Arguments:
          list_widget (QListWidget): The list widget to set up
          codes (List[str]): List of observation codes to populate (in priority order)
          enabled_codes (set): Set of codes that should be checked by default
        """
        list_widget.setEnabled(True)
        list_widget.clear()

        # Enable drag and drop for reordering
        list_widget.setDragDropMode(QAbstractItemView.DragDropMode.InternalMove)
        list_widget.setDefaultDropAction(Qt.DropAction.MoveAction)
        list_widget.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)

        # Add items with checkboxes
        for code in codes:
            item = QListWidgetItem(code)
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable | Qt.ItemFlag.ItemIsEnabled)

            # Check if this code is in the enabled set (from template priorities)
            if code in enabled_codes:
                item.setCheckState(Qt.CheckState.Checked)  # Priority codes: checked
            else:
                item.setCheckState(Qt.CheckState.Unchecked)  # Extra codes: unchecked

            list_widget.addItem(item)

    def verify_antenna_type(self, result: List[str]):
        """
        UI handler: verify that the RINEX antenna_type exists in the selected ANTEX (.atx) file.
        """
        # Verify antenna_type is present within the .atx file
        # Return warning if not
        atx_path = self.get_best_atx_path()

        with open(atx_path, "r") as file:
            for line in file:
                label = line[60:].strip()

                # Read and find antenna_type tag
                if label == "TYPE / SERIAL NO" and line[20:24].strip() == "":
                    valid_antenna_type = line[0:20]

                    if len(valid_antenna_type.strip()) < 16 or not valid_antenna_type[16:].strip():
                        # Just the antenna part is included, need to add radome (cover)
                        antenna_part = valid_antenna_type[:15].strip()
                        valid_antenna_type = f"{antenna_part:<15} NONE"

                    # Do same normalisation for result["antenna_type"]
                    result_antenna = result["antenna_type"]

                    if len(result_antenna.strip()) < 16 or (
                            len(result_antenna) > 16 and not result_antenna[16:].strip()):
                        antenna_part = result_antenna[:15].strip()
                        result_antenna = f"{antenna_part:<15} NONE"

                    # Compare strings
                    if result_antenna.strip() == valid_antenna_type.strip():
                        Logger.terminal("✅ Antenna type verified from .atx file")
                        return

        # Not found! Return warning to user
        QMessageBox.warning(
            None,
            "Provided Antenna Type Invalid",
            f'Provided antenna type in .rnx file: "{result["antenna_type"]}"\n'
            f'not found in .atx file: "{atx_path}"'
        )
        Logger.terminal(f"⚠️ Antenna type failed to verify from .atx file: {atx_path}")
        return

    def get_best_atx_path(self):
        """
        Select the best available ANTEX (.atx) file with a priority order.
        """
        # Find all .atx files present and prioritise the newest ones
        # Return filepath string to best .atx file
        atx_files = list(INPUT_PRODUCTS_PATH.glob("*.atx"))
        if len(atx_files) == 0:
            raise FileNotFoundError("No .atx file found")
        elif len(atx_files) > 1:
            # Priority order: igs20 > igs14 > igs13 > igs08 > igs05 > any other .atx file
            priority_order = ['igs20.atx', 'igs14.atx', 'igs13.atx', 'igs08.atx', 'igs05.atx']
            atx_path = None
            for best_atx in priority_order:
                matching_files = [f for f in atx_files if f.name == best_atx]
                if matching_files:
                    atx_path = matching_files[0]
                    Logger.terminal(f"📁 Selected .atx file: {atx_path.name} based on priority")
                    break

            # If none of the preferred files found, use the first available
            if atx_path is None:
                atx_path = atx_files[0]
                Logger.terminal(f"📁 Selected .atx file: {atx_path.name} based on fallback")
        else:
            atx_path = atx_files[0]
        return atx_path

    def _update_constellations_multiselect(self, constellation_str: str):
        """
        Populate and mirror a multi-select constellation combo with checkboxes.

        Arguments:
          constellation_str (str): Comma-separated constellations (e.g., "GPS, GAL, GLO").

        """
        from PySide6.QtGui import QStandardItemModel, QStandardItem

        constellations = [c.strip() for c in constellation_str.split(",") if c.strip()]
        combo = self.ui.constellationsCombo

        # Remove previous bindings
        if hasattr(combo, '_old_showPopup'):
            delattr(combo, '_old_showPopup')

        combo.clear()
        combo.setEditable(True)
        combo.lineEdit().setReadOnly(True)
        combo.setInsertPolicy(QComboBox.NoInsert)

        # Build the item model
        model = QStandardItemModel(combo)
        for txt in constellations:
            item = QStandardItem(txt)
            item.setFlags(Qt.ItemIsEnabled | Qt.ItemIsUserCheckable)
            item.setCheckState(Qt.Checked)
            model.appendRow(item)

        def on_item_changed(_item):
            selected = [
                model.item(i).text()
                for i in range(model.rowCount())
                if model.item(i).checkState() == Qt.Checked
            ]
            label = ", ".join(selected) if selected else "Select one or more"
            combo.lineEdit().setText(label)
            self.ui.constellationsValue.setText(label)
            self._sync_constellation_list_widgets_to_selection()

        model.itemChanged.connect(on_item_changed)
        combo.setModel(model)
        combo.setCurrentIndex(-1)

        # Custom showPopup function to keep things reset
        def show_popup_constellation():
            if combo.model() != model:
                combo.setModel(model)
            combo.setCurrentIndex(-1)
            QComboBox.showPopup(combo)

        combo.showPopup = show_popup_constellation

        # Store for access and event consistency
        combo._constellation_model = model
        combo._constellation_on_item_changed = on_item_changed

        # Set initial label text
        combo.lineEdit().setText(", ".join(constellations))
        self.ui.constellationsValue.setText(", ".join(constellations))

        # Initial sync of list widgets
        self._sync_constellation_list_widgets_to_selection()

    def _sync_constellation_list_widgets_to_selection(self):
        """
        Show / hide constellation list widgets and labels based on the "General" tab's
        constellation multi-select. Called when constellation selection changes.
        Shows a placeholder message when no constellations are selected.
        """
        # Get currently selected constellations from the General tab combo
        selected_constellations = set()
        combo = self.ui.constellationsCombo
        if hasattr(combo, '_constellation_model') and combo._constellation_model:
            model = combo._constellation_model
            for i in range(model.rowCount()):
                if model.item(i).checkState() == Qt.Checked:
                    selected_constellations.add(model.item(i).text().upper())

        # Map constellation names to their UI widgets
        widget_mapping = {
            'GPS': ('gpsLabel', 'gpsListWidget'),
            'GAL': ('galLabel', 'galListWidget'),
            'GLO': ('gloLabel', 'gloListWidget'),
            'BDS': ('bdsLabel', 'bdsListWidget'),
            'QZS': ('qzsLabel', 'qzsListWidget'),
        }

        for const_name, (label_name, list_widget_name) in widget_mapping.items():
            is_enabled = const_name in selected_constellations

            # Show / hide label
            if hasattr(self.ui, label_name):
                label = getattr(self.ui, label_name)
                label.setVisible(is_enabled)

            # Show / hide list widget
            if hasattr(self.ui, list_widget_name):
                list_widget = getattr(self.ui, list_widget_name)
                list_widget.setVisible(is_enabled)

        # Show / hide placeholder message
        self._update_constellation_placeholder(len(selected_constellations) == 0)

    def _setup_constellation_placeholder(self):
        """
        Create a placeholder label for the Constellations tab that shows when
        no constellations are selected or no RINEX file is loaded.
        """
        # Create the placeholder label
        self._constellation_placeholder = QLabel(
            "No constellations available!\n\n"
            "Load a RINEX observation file and select constellations\n"
            "in the General tab to configure observation codes"
        )
        self._constellation_placeholder.setAlignment(Qt.AlignCenter)
        self._constellation_placeholder.setWordWrap(True)
        self._constellation_placeholder.setMinimumWidth(250)
        self._constellation_placeholder.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._constellation_placeholder.setStyleSheet(
            "color: #bfbfbf; font-size: 13pt; margin: 15px;"
        )

        # Add to the constellations tab layout
        if hasattr(self.ui, 'constellationsGridLayout'):
            self.ui.constellationsGridLayout.addWidget(
                self._constellation_placeholder, 0, 0, 10, 1, Qt.AlignCenter
            )

        # Initially visible
        self._constellation_placeholder.setVisible(True)

        # Create explanation label for the Constellations tab
        self._constellation_explanation_label = QLabel(
            "Select observation codes and set priorities for each active constellation below.<br>"
            "These observation codes are extracted from the loaded RINEX file.<br>"
            "<span style='color:#ff6b6b; text-decoration:line-through;'>Red strikethrough</span> = missing from .BIA file"
        )
        self._constellation_explanation_label.setTextFormat(Qt.RichText)
        self._constellation_explanation_label.setWordWrap(True)
        self._constellation_explanation_label.setStyleSheet(
            "color: #bfbfbf; font-size: 11pt; font-style: italic; margin-bottom: 6x; line-height: 1.4;"
        )
        self._constellation_explanation_label.setVisible(False)

        # Create BIA warning label (shown when BIA fetch fails)
        self._bia_warning_label = QLabel(
            "⚠️ Failed to fetch BIA file for selected PPP products - unable to validate codes"
        )
        self._bia_warning_label.setWordWrap(True)
        self._bia_warning_label.setStyleSheet(
            "QLabel { background-color: #8B4513; color: white; padding: 6px 12px; "
            "border-radius: 4px; font: 10pt 'Segoe UI'; }"
        )
        self._bia_warning_label.setAlignment(Qt.AlignCenter)
        self._bia_warning_label.setVisible(False)

        # Create BIA loading label
        self._bia_loading_label = QLabel("⏳ Loading code priorities from .BIA file...")
        self._bia_loading_label.setWordWrap(True)
        self._bia_loading_label.setStyleSheet(
            "QLabel { background-color: #2c5d7c; color: white; padding: 8px 16px; "
            "border-radius: 4px; font: 12pt 'Segoe UI'; }"
        )
        self._bia_loading_label.setAlignment(Qt.AlignCenter)
        self._bia_loading_label.setVisible(False)

        # Create a container widget with vertical layout for the status labels
        self._constellation_status_container = QWidget()
        status_layout = QVBoxLayout(self._constellation_status_container)
        status_layout.setContentsMargins(0, 0, 0, 8)
        status_layout.setSpacing(4)
        status_layout.addWidget(self._constellation_explanation_label)
        status_layout.addWidget(self._bia_warning_label)
        status_layout.addWidget(self._bia_loading_label)

        # Add the status container to row 0 of the constellations grid layout
        # (existing widgets start at row 1, so row 0 is available)
        if hasattr(self.ui, 'constellationsGridLayout'):
            self.ui.constellationsGridLayout.addWidget(self._constellation_status_container, 0, 0)

    def _update_constellation_placeholder(self, show_placeholder: bool):
        """
        Show or hide the constellation placeholder message.

        Arguments:
          show_placeholder (bool): True to show placeholder, False to hide it.
        """
        if hasattr(self, '_constellation_placeholder'):
            self._constellation_placeholder.setVisible(show_placeholder)
        # Show explanation label when placeholder is hidden (i.e., constellations are visible)
        if hasattr(self, '_constellation_explanation_label'):
            self._constellation_explanation_label.setVisible(not show_placeholder)

    def _on_cddis_ready(self, data: pd.DataFrame, log_messages: bool = True):
        """
        UI handler: receive PPP products DataFrame from worker and populate provider/project/series combos.
        """
        self.products_df = data

        if data.empty:
            self.valid_analysis_centers = []
            self.ui.pppProviderCombo.clear()
            self.ui.pppProviderCombo.addItem("None")
            self.ui.pppSeriesCombo.clear()
            self.ui.pppSeriesCombo.addItem("None")
            return

        self.valid_analysis_centers = list(get_valid_analysis_centers(self.products_df))

        if len(self.valid_analysis_centers) == 0:
            self.ui.pppProviderCombo.clear()
            self.ui.pppProviderCombo.addItem("None")
            self.ui.pppSeriesCombo.clear()
            self.ui.pppSeriesCombo.addItem("None")
            return

        self.ui.pppProviderCombo.blockSignals(True)
        self.ui.pppProviderCombo.clear()
        self.ui.pppProviderCombo.addItems(self.valid_analysis_centers)
        self.ui.pppProviderCombo.setCurrentIndex(0)

        # Update PPP series based on default PPP provider
        self.ui.pppProviderCombo.blockSignals(False)
        self.try_enable_process_button()
        self._on_ppp_provider_changed(self.valid_analysis_centers[0])
        if log_messages:
            Logger.terminal(
                f"✅ CDDIS archive scan complete. Found PPP product providers: {', '.join(self.valid_analysis_centers)}")
            # Show success toast
            show_toast(self.parent, f"✅ Found {len(self.valid_analysis_centers)} PPP provider(s)", duration=3000)

    def _on_cddis_error(self, msg):
        """
        UI handler: report CDDIS worker error to the UI.
        """
        Logger.terminal(f"Error loading CDDIS data: {msg}")
        self.ui.pppProviderCombo.clear()
        self.ui.pppProviderCombo.addItem("None")
        # Restore cursor in case of error
        self.parent.setCursor(Qt.CursorShape.ArrowCursor)
        # Show error toast
        show_toast(self.parent, "⚠️ Failed to scan CDDIS archive", duration=4000)

    def _restore_cursor(self):
        """
        Restore the cursor to normal arrow after background operation completes.
        """
        self.parent.setCursor(Qt.CursorShape.ArrowCursor)

    def _cleanup_analysis_thread(self):
        """
        Request any running analysis centre threads to cancel
        Moves the thread to _pending_threads list so it isn't destroyed while running.
        """
        if hasattr(self, 'worker') and self.worker is not None:
            self.worker.stop()

        if hasattr(self, 'metadata_thread') and self.metadata_thread is not None:
            if self.metadata_thread.isRunning():
                # Disconnect old signals to prevent callbacks to stale state
                try:
                    self.worker.finished.disconnect()
                    self.worker.cancelled.disconnect()
                except (TypeError, RuntimeError):
                    pass  # Already disconnected or object deleted

                # Keep reference alive until thread actually finishes
                old_thread = self.metadata_thread

                def cleanup_old_thread():
                    if old_thread in self._pending_threads:
                        self._pending_threads.remove(old_thread)

                old_thread.finished.connect(cleanup_old_thread)
                self._pending_threads.append(old_thread)

            # Clear current references so new thread can be created
            self.worker = None
            self.metadata_thread = None

    def _on_cddis_cancelled(self):
        """
        UI handler: handle cancellation of CDDIS worker.
        """
        Logger.terminal("📦 PPP provider scan was cancelled")

    def _on_constellation_info_received(self, provider_constellations: dict):
        """
        UI handler: receive and store constellation information for each PPP provider/series/project
        This is emitted by the DownloadWorker after fetching the SP3 headers

        Arguments:
          provider_constellations (dict): Nested dictionary mapping "provider -> series -> project -> constellations"
              e.g., {
                  'COD': {
                      'FIN': {'OPS': {'GPS', 'GLO', 'GAL'}, 'MGX': {'GPS', 'GLO', 'GAL', 'BDS', 'QZS'}},
                      'RAP': {'OPS': {'GPS', 'GLO', 'GAL'}}
                  }, ...
              }
        """
        # Store for later use when filtering constellations UI based on selected provider/series/project
        self.provider_constellations = provider_constellations

        # Log the received constellation info
        Logger.console("📡 Provider constellation information received")

        # Update constellations combobox based on current PPP selection
        self._update_constellations_for_ppp_selection()

        # If already on Constellations tab, trigger BIA fetch
        if self.ui.configTabWidget.currentIndex() == 1:
            self._on_config_tab_changed(1)

    def _update_constellations_for_ppp_selection(self):
        """
        Update the constellations combobox to enable / disable items based on the
        currently selected PPP provider/series/project combination.
        Constellations supported by the selected combination are enabled and checked,
        unsupported constellations are disabled and unchecked.
        """
        combo = self.ui.constellationsCombo
        if not hasattr(combo, '_constellation_model') or combo._constellation_model is None:
            return

        model = combo._constellation_model

        # Get current PPP selection
        provider = self.ui.pppProviderCombo.currentText()
        series = self.ui.pppSeriesCombo.currentText()
        project = self.ui.pppProjectCombo.currentText()

        # Get available constellations for this combination
        available_constellations = set()
        if hasattr(self, 'provider_constellations') and self.provider_constellations:
            try:
                available_constellations = self.provider_constellations.get(provider, {}).get(series, {}).get(project, set())
            except (KeyError, AttributeError):
                available_constellations = set()

        # If no constellation info available, enable all (fallback behaviour)
        if not available_constellations:
            for i in range(model.rowCount()):
                item = model.item(i)
                item.setFlags(Qt.ItemIsEnabled | Qt.ItemIsUserCheckable)
            return

        # Block signals to prevent triggering on_item_changed multiple times
        model.blockSignals(True)

        # Update each constellation item
        for i in range(model.rowCount()):
            item = model.item(i)
            constellation_name = item.text().upper()

            if constellation_name in available_constellations:
                # Enable and check this constellation
                #item.setFlags(Qt.ItemIsEnabled | Qt.ItemIsUserCheckable) # Un-comment to also disable checkability
                item.setCheckState(Qt.Checked)
            else:
                # Disable and uncheck this constellation
                #item.setFlags(Qt.ItemIsUserCheckable)                    # Un-comment to also disable checkability
                item.setCheckState(Qt.Unchecked)

        model.blockSignals(False)

        # Update the label text to show only enabled/checked constellations
        selected = [
            model.item(i).text()
            for i in range(model.rowCount())
            if model.item(i).checkState() == Qt.Checked
        ]
        label = ", ".join(selected) if selected else "Select one or more"
        combo.lineEdit().setText(label)
        self.ui.constellationsValue.setText(label)

        # Sync the constellation list widgets
        self._sync_constellation_list_widgets_to_selection()

    def _on_config_tab_changed(self, index: int):
        """
        UI handler: triggered when the config tab widget changes tabs.
        When switching to the Constellations tab (index 1), fetch .BIA code priorities
        for the current PPP selection if not already cached

        Arguments:
          index (int): The index of the newly selected tab
        """
        # Constellations tab is index 1
        if index != 1:
            return

        # Check if we have valid PPP selection
        provider = self.ui.pppProviderCombo.currentText()
        series = self.ui.pppSeriesCombo.currentText()
        project = self.ui.pppProjectCombo.currentText()

        # Guard: Skip if any combo is empty or has placeholder values
        if not provider or not series or not project:
            return
        if provider in ("", "None", "Select one") or series in ("", "None", "Select one") or project in ("", "None", "Select one"):
            return

        # Guard: Skip if products_df is empty (happens during RINEX file change)
        if self.products_df.empty:
            return

        # Check if we already have cached BIA data for this combination
        if self._is_bia_cached(provider, series, project):
            # Already have the data, just validate
            self._validate_constellation_codes_against_bia()
            return

        # Check if we are already loading the same combination
        if self._bia_loading:
            # Check if it is for a different combination - if so, restart
            if (hasattr(self, '_bia_current_provider') and
                (self._bia_current_provider != provider or
                 self._bia_current_series != series or
                 self._bia_current_project != project)):
                # Different combination requested, stop current and start new
                Logger.console(f"🔄 BIA fetch interrupted - switching to {provider}/{series}/{project}")
            else:
                # Same combination, let it continue
                return

        # Start BIA fetch (will stop any existing worker first)
        self._fetch_bia_code_priorities(provider, series, project)

    def _is_bia_cached(self, provider: str, series: str, project: str) -> bool:
        """
        Check if BIA code priorities are cached for the given combination.

        Arguments:
          provider (str)
          series (str)
          project (str)

        Returns:
          bool: True if cached, False otherwise
        """
        try:
            return (provider in self.bia_code_priorities and
                    series in self.bia_code_priorities[provider] and
                    project in self.bia_code_priorities[provider][series])
        except (KeyError, TypeError):
            return False

    def _fetch_bia_code_priorities(self, provider: str, series: str, project: str):
        """
        Start background worker to fetch and parse BIA file for code priorities.

        Arguments:
          provider (str)
          series (str)
          project (str)
        """
        # Safety guard: don't start worker with invalid parameters
        if not provider or not series or not project:
            Logger.console(f"⚠️ BIA fetch skipped: invalid parameters provider='{provider}' series='{series}' project='{project}'")
            return
        if provider in ("", "None", "Select one") or series in ("", "None", "Select one") or project in ("", "None", "Select one"):
            Logger.console(f"⚠️ BIA fetch skipped: placeholder values in parameters")
            return
        if self.products_df.empty:
            Logger.console(f"⚠️ BIA fetch skipped: products_df is empty")
            return

        # Stop any existing BIAProductWorker before starting a new one
        self._stop_bia_worker()

        self._bia_loading = True

        # Show loading indicator
        self._show_bia_loading_indicator(True)

        # Create worker and thread
        self._bia_thread = QThread()
        self._bia_worker = BiasProductWorker(self.products_df, provider, series, project)
        self._bia_worker.moveToThread(self._bia_thread)

        # Connect signals
        self._bia_thread.started.connect(self._bia_worker.run)
        self._bia_worker.finished.connect(self._on_bia_finished)
        self._bia_worker.error.connect(self._on_bia_error)
        self._bia_worker.progress.connect(self._on_bia_progress)
        self._bia_worker.finished.connect(self._bia_thread.quit)
        self._bia_worker.error.connect(self._bia_thread.quit)
        self._bia_thread.finished.connect(self._on_bia_thread_finished)

        # Store current selection for when results come back
        self._bia_current_provider = provider
        self._bia_current_series = series
        self._bia_current_project = project

        # Start the thread
        self._bia_thread.start()

    def _stop_bia_worker(self):
        """
        Stop any running BIA worker and clean up thread resources.
        """
        if self._bia_worker is not None:
            # Disconnect signals to prevent callbacks after cleanup
            try:
                self._bia_worker.finished.disconnect()
                self._bia_worker.error.disconnect()
                self._bia_worker.progress.disconnect()
            except (RuntimeError, TypeError):
                # Signals may not be connected or already disconnected
                pass
            # Signal the worker to stop
            self._bia_worker.stop()

        if self._bia_thread is not None:
            # Disconnect thread signals
            try:
                self._bia_thread.started.disconnect()
                self._bia_thread.finished.disconnect()
            except (RuntimeError, TypeError):
                pass

            if self._bia_thread.isRunning():
                # Ask thread to quit and wait briefly
                self._bia_thread.quit()
                # Wait up to 2 seconds for thread to finish
                if not self._bia_thread.wait(2000):
                    # Force terminate if it doesn't stop gracefully
                    Logger.console("⚠️ BIA thread did not stop gracefully, forcing termination")
                    self._bia_thread.terminate()
                    self._bia_thread.wait(1000)

        # Clean up references
        self._bia_worker = None
        self._bia_thread = None
        self._bia_loading = False

    def _on_bia_progress(self, description: str, percent: int):
        """
        UI handler: update progress during BIA fetch.

        Arguments:
          description (str): Progress description
          percent (int): Progress percentage (-1 for indeterminate)
        """
        # Update the loading label if it exists
        if hasattr(self, '_bia_loading_label') and self._bia_loading_label:
            self._bia_loading_label.setText(f"⏳ {description}")

    def _on_bia_finished(self, code_priorities: dict):
        """
        UI handler: BIA fetch completed successfully.

        Arguments:
          code_priorities (dict): Dictionary mapping constellation names to sets of code priorities
                                  e.g., {'GPS': {'L1C', 'L2W'}, 'GAL': {'L1C', 'L5Q'}, ...}
        """
        self._bia_loading = False
        self._show_bia_loading_indicator(False)

        # Hide any previous BIA warning since we now have valid data
        self._show_bia_warning(False)

        # Cache the results
        provider = self._bia_current_provider
        series = self._bia_current_series
        project = self._bia_current_project

        if provider not in self.bia_code_priorities:
            self.bia_code_priorities[provider] = {}
        if series not in self.bia_code_priorities[provider]:
            self.bia_code_priorities[provider][series] = {}
        self.bia_code_priorities[provider][series][project] = code_priorities

        Logger.terminal(f"✅ BIA code priorities cached for {provider}/{series}/{project}")

        # Validate the constellation codes against BIA
        self._validate_constellation_codes_against_bia()

    def _on_bia_error(self, error_msg: str):
        """
        UI handler: BIA fetch failed.

        Arguments:
          error_msg (str): Error message describing the failure
        """
        self._bia_loading = False
        self._show_bia_loading_indicator(False)

        Logger.console(f"⚠️ BIA fetch error: {error_msg}")

        # Don't show warnings for cancelled fetches (user-initiated)
        if "cancelled" in error_msg.lower():
            return

        # Mark all codes as invalid (red strikethrough)
        self._mark_all_codes_invalid()

        # Show BIA warning label
        self._show_bia_warning(True)

        # Log to terminal (workflow tab) so user is aware BIA validation is unavailable
        Logger.terminal(f"⚠️ Failed to fetch BIA file for selected PPP products - unable to validate codes")

        show_toast(self.parent, f"⚠️ Could not fetch BIA data: {error_msg}", duration=3000)

    def _on_bia_thread_finished(self):
        """
        Slot called when the BIA thread has fully finished.
        Safe to clean up references here.
        """
        self._bia_worker = None
        self._bia_thread = None

    def _show_bia_loading_indicator(self, show: bool):
        """
        Show or hide a loading indicator on the Constellations tab.

        Arguments:
          show (bool): True to show, False to hide
        """
        if not hasattr(self, '_bia_loading_label') or self._bia_loading_label is None:
            return

        # Reset to initial text when showing (in case it was changed by progress updates)
        if show:
            self._bia_loading_label.setText("⏳ Loading code priorities from .BIA file...")

        self._bia_loading_label.setVisible(show)

    def _validate_constellation_codes_against_bia(self):
        """
        Validate the codes in each constellation list widget against the cached BIA codes.
        Codes that are NOT in the .BIA file are marked with strikethrough and a different colour.
        """
        # Get current PPP selection
        provider = self.ui.pppProviderCombo.currentText()
        series = self.ui.pppSeriesCombo.currentText()
        project = self.ui.pppProjectCombo.currentText()

        # Get cached BIA codes for this selection
        bia_codes = None
        try:
            bia_codes = self.bia_code_priorities.get(provider, {}).get(series, {}).get(project, None)
        except (KeyError, TypeError, AttributeError):
            pass

        if not bia_codes:
            # No BIA data available, reset all items to normal styling
            self._reset_constellation_list_styling()
            return

        # Map widget names to constellation keys
        widget_mapping = {
            'gpsListWidget': 'GPS',
            'galListWidget': 'GAL',
            'gloListWidget': 'GLO',
            'bdsListWidget': 'BDS',
            'qzsListWidget': 'QZS',
        }

        # Colours for codes
        valid_color = QColor('white')  # White for valid
        invalid_color = QColor('#FF6B6B')  # Red for invalid

        for widget_name, constellation in widget_mapping.items():
            if not hasattr(self.ui, widget_name):
                continue

            list_widget = getattr(self.ui, widget_name)
            constellation_bia_codes = bia_codes.get(constellation, set())

            for i in range(list_widget.count()):
                item = list_widget.item(i)
                if item is None:
                    continue

                # Get the code text (e.g., "L1C", "L2W")
                code = item.text().strip()

                # Get current font
                font = item.font()

                if code in constellation_bia_codes:
                    # Valid code - normal styling
                    font.setStrikeOut(False)
                    item.setFont(font)
                    item.setForeground(QBrush(valid_color))
                else:
                    # Invalid code - strikethrough + colour
                    font.setStrikeOut(True)
                    item.setFont(font)
                    item.setForeground(QBrush(invalid_color))

        Logger.terminal(f"✅ Validated constellation codes against BIA for {provider}/{series}/{project}")

    def _reset_constellation_list_styling(self):
        """
        Reset all constellation list widget items to normal styling (no strikethrough, white colour).
        Called when BIA data is not available.
        """
        widget_names = ['gpsListWidget', 'galListWidget', 'gloListWidget', 'bdsListWidget', 'qzsListWidget']
        normal_color = QColor('white')

        for widget_name in widget_names:
            if not hasattr(self.ui, widget_name):
                continue

            list_widget = getattr(self.ui, widget_name)

            for i in range(list_widget.count()):
                item = list_widget.item(i)
                if item is None:
                    continue

                font = item.font()
                font.setStrikeOut(False)
                item.setFont(font)
                item.setForeground(QBrush(normal_color))

        # Also hide BIA warning when resetting
        self._show_bia_warning(False)

    def _mark_all_codes_invalid(self):
        """
        Mark all constellation list widget items as invalid (red strikethrough).
        Called when BIA file fetch fails.
        """
        widget_names = ['gpsListWidget', 'galListWidget', 'gloListWidget', 'bdsListWidget', 'qzsListWidget']
        invalid_color = QColor('#ff6b6b')

        for widget_name in widget_names:
            if not hasattr(self.ui, widget_name):
                continue

            list_widget = getattr(self.ui, widget_name)

            for i in range(list_widget.count()):
                item = list_widget.item(i)
                if item is None:
                    continue

                font = item.font()
                font.setStrikeOut(True)
                item.setFont(font)
                item.setForeground(QBrush(invalid_color))

    def _show_bia_warning(self, show: bool):
        """
        Show or hide the BIA warning label on the Constellations tab.

        Arguments:
          show (bool): True to show warning, False to hide it.
        """
        if hasattr(self, '_bia_warning_label'):
            self._bia_warning_label.setVisible(show)

    def _on_analysis_thread_finished(self):
        """
        Slot called when the analysis thread has fully finished.
        Safe to clean up references here.
        """
        # Clean up current thread references if it's no longer running
        if hasattr(self, 'metadata_thread') and self.metadata_thread is not None:
            if not self.metadata_thread.isRunning():
                self.worker = None
                self.metadata_thread = None

        # Also clean any finished pending threads
        self._pending_threads = [t for t in self._pending_threads if t.isRunning()]

    def _on_ppp_provider_changed(self, provider_name: str):
        """
        UI handler: when PPP provider changes, refresh project and series options.
        Only shows series that have all required files (SP3, BIA, CLK).
        """
        if not provider_name or provider_name.strip() == "":
            return
        try:
            # Get valid series for this provider (only those with all required files)
            valid_series = get_valid_series_for_provider(self.products_df, provider_name)

            if not valid_series:
                raise ValueError(f"No valid series (with all required files) for provider: {provider_name}")

            # Get DataFrame of valid (project, series) pairs - filter for valid series only
            df = self.products_df.loc[
                (self.products_df["analysis_center"] == provider_name) &
                (self.products_df["solution_type"].isin(valid_series)),
                ["project", "solution_type"]]

            if df.empty:
                raise ValueError(f"No valid project–series combinations for provider: {provider_name}")

            # Store for future filtering if needed
            self._valid_project_series_df = df
            self._valid_series_for_provider = valid_series  # Cache valid series

            project_options = sorted(df['project'].unique())
            series_options = sorted(df['solution_type'].unique())

            # Block signals before clearing and populating to prevent any duplicates in dropdown
            self.ui.pppProjectCombo.blockSignals(True)
            self.ui.pppSeriesCombo.blockSignals(True)

            self.ui.pppProjectCombo.clear()
            self.ui.pppSeriesCombo.clear()

            self.ui.pppProjectCombo.addItems(project_options)
            self.ui.pppSeriesCombo.addItems(series_options)

            self.ui.pppProjectCombo.setCurrentIndex(0)
            self.ui.pppSeriesCombo.setCurrentIndex(0)

            # Unblock signals now that the population is complete
            self.ui.pppProjectCombo.blockSignals(False)
            self.ui.pppSeriesCombo.blockSignals(False)

            # Update constellations combobox based on new PPP selection
            self._update_constellations_for_ppp_selection()

            # If we're on the Constellations tab, trigger BIA fetch for new selection
            if self.ui.configTabWidget.currentIndex() == 1:
                self._on_config_tab_changed(1)

        except Exception as e:
            self.ui.pppSeriesCombo.clear()
            self.ui.pppSeriesCombo.addItem("None")
            self.ui.pppProjectCombo.clear()
            self.ui.pppProjectCombo.addItem("None")

    def _on_ppp_series_changed(self, selected_series: str):
        """
        UI handler: when PPP series changes, filter valid projects.

        Arguments:
          selected_series (str): Series code, e.g., 'ULT', 'RAP', 'FIN'.
        """
        if not hasattr(self, "_valid_project_series_df"):
            return

        df = self._valid_project_series_df
        filtered_df = df[df["solution_type"] == selected_series]
        valid_projects = sorted(filtered_df["project"].unique())

        self.ui.pppProjectCombo.blockSignals(True)
        self.ui.pppProjectCombo.clear()
        self.ui.pppProjectCombo.addItems(valid_projects)
        self.ui.pppProjectCombo.setCurrentIndex(0)
        self.ui.pppProjectCombo.blockSignals(False)

        # Update constellations combobox based on new PPP selection
        self._update_constellations_for_ppp_selection()

        # If we are on the Constellations tab, trigger BIA fetch for new selection
        # This may occur if the user is on this tab while PPP products are being fetched
        if self.ui.configTabWidget.currentIndex() == 1:
            self._on_config_tab_changed(1)

    def _on_ppp_project_changed(self, selected_project: str):
        """
        UI handler: when PPP project changes, filter valid series.
        Only displays series that have all required files (SP3, BIA, CLK).
        """
        if not hasattr(self, "_valid_project_series_df"):
            return

        df = self._valid_project_series_df
        filtered_df = df[df["project"] == selected_project]
        valid_series = sorted(filtered_df["solution_type"].unique())

        # Ensure only series with all required files are displayed
        if hasattr(self, "_valid_series_for_provider"):
            valid_series = [s for s in valid_series if s in self._valid_series_for_provider]

        self.ui.pppSeriesCombo.blockSignals(True)
        self.ui.pppSeriesCombo.clear()
        self.ui.pppSeriesCombo.addItems(valid_series)
        self.ui.pppSeriesCombo.setCurrentIndex(0)
        self.ui.pppSeriesCombo.blockSignals(False)

        # Update constellations combobox based on new PPP selection
        self._update_constellations_for_ppp_selection()

        Logger.terminal(f"✅ Filtered PPP series for project '{selected_project}': {valid_series}")

        # If we are on the Constellations tab, trigger BIA fetch for new selection
        # This may occur if the user is on this tab while PPP products are being fetched
        if self.ui.configTabWidget.currentIndex() == 1:
            self._on_config_tab_changed(1)

    def load_output_dir(self):
        """
        UI handler: choose the output directory and (if RNX is set) emit ready.
        """
        """Pick an output directory; if RNX is also set, emit ready."""
        path = self._select_output_dir(self.parent)
        if not path:
            return

        # Ensure output_dir is a Path object
        self.output_dir = Path(path).resolve()
        Logger.terminal(f"📂 Output directory selected: {self.output_dir}")

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

    def try_enable_process_button(self):
        """
        UI handler: enable the Process button when RNX, output path, and metadata are ready.
        """
        if not self.parent.metadata_downloaded:
            return
        if not self.output_dir:
            return
        if not self.rnx_file:
            return
        if len(self._get_ppp_provider_items()) < 1:
            return
        self.ui.processButton.setEnabled(True)

    # endregion

    # region Multi-Selectors Assigning (A.K.A. Combo Plumbing)

    def _on_select(self, combo: QComboBox, label, title: str, index: int):
        """
        UI handler: mirror a single-select combo choice to a label and reset placeholder.

        Arguments:
          combo (QComboBox): Source combo box.
          label (QLabel): Target label to mirror text.
          title (str): Placeholder title to reset in the combo.
          index (int): Selected index.
        """
        value = combo.itemText(index)
        label.setText(value)

        combo.clear()
        combo.addItem(title)

    def _bind_combo(self, combo: QComboBox, items_func: Callable[[], List[str]]):
        """
        Bind a single-choice combo to dynamically populate items on open and keep the UI clean.

        Arguments:
          combo (QComboBox): Target combo box to bind.
          items_func (Callable[[], list[str]]): Function returning the items list.
        """
        combo._old_showPopup = combo.showPopup

        def new_showPopup():
            combo.clear()
            combo.setEditable(True)
            combo.lineEdit().setAlignment(Qt.AlignCenter)
            for item in items_func():
                combo.addItem(item)
            combo.setEditable(False)
            combo._old_showPopup()

        combo.showPopup = new_showPopup

    def _bind_multiselect_combo(
            self,
            combo: QComboBox,
            items_func: Callable[[], List[str]],
            mirror_label,
            placeholder: str,
    ):
        """
        Bind a multi-select combo using checkable items and mirror checked labels as comma-separated text.

        Arguments:
          combo (QComboBox): Target combo box.
          items_func (Callable[[], list[str]]): Function returning the items list.
          mirror_label (QLabel): Label where checked values are mirrored.
          placeholder (str): Placeholder text when no item is checked.

        """
        combo.setEditable(True)
        combo.lineEdit().setReadOnly(True)
        combo.lineEdit().setPlaceholderText(placeholder)
        combo.setInsertPolicy(QComboBox.NoInsert)

        combo._old_showPopup = combo.showPopup

        def show_popup():
            model = QStandardItemModel(combo)
            for txt in items_func():
                it = QStandardItem(txt)
                it.setFlags(Qt.ItemIsEnabled | Qt.ItemIsUserCheckable)
                it.setData(Qt.Unchecked, Qt.CheckStateRole)
                model.appendRow(it)

            def on_item_changed(_item: QStandardItem):
                # Collect all checked items
                selected = [
                    model.item(r).text()
                    for r in range(model.rowCount())
                    if model.item(r).checkState() == Qt.Checked
                ]
                text = ", ".join(selected) if selected else placeholder
                combo.lineEdit().setText(text)
                mirror_label.setText(text)

            model.itemChanged.connect(on_item_changed)
            combo.setModel(model)
            combo._old_showPopup()

        combo.showPopup = show_popup
        combo.clear()
        combo.lineEdit().clear()
        combo.lineEdit().setPlaceholderText(placeholder)

        # ==========================================================

    # Receiver / Antenna free text popups
    # ==========================================================
    def _enable_free_text_for_receiver_and_antenna(self):
        """
        Allow users to enter custom receiver/antenna types via popup, mirroring to UI.
        """
        self.ui.receiverTypeCombo.setEditable(True)
        self.ui.receiverTypeCombo.lineEdit().setReadOnly(True)
        self.ui.antennaTypeCombo.setEditable(True)
        self.ui.antennaTypeCombo.lineEdit().setReadOnly(True)

        # Receiver type free text
        def _ask_receiver_type():
            current_text = self.ui.receiverTypeCombo.currentText().strip()
            text, ok = QInputDialog.getText(
                self.ui.receiverTypeCombo,
                "Receiver Type",
                "Enter receiver type:",
                text=current_text  # prefill with current
            )
            if ok and text:
                self.ui.receiverTypeCombo.clear()
                self.ui.receiverTypeCombo.addItem(text)
                self.ui.receiverTypeCombo.lineEdit().setText(text)
                self.ui.receiverTypeValue.setText(text)

        self.ui.receiverTypeCombo.showPopup = _ask_receiver_type

        # Antenna type free text
        def _ask_antenna_type():
            current_text = self.ui.antennaTypeCombo.currentText().strip()
            text, ok = QInputDialog.getText(
                self.ui.antennaTypeCombo,
                "Antenna Type",
                "Enter antenna type:",
                text=current_text  # prefill with current
            )
            if ok and text:
                self.ui.antennaTypeCombo.clear()
                self.ui.antennaTypeCombo.addItem(text)
                self.ui.antennaTypeCombo.lineEdit().setText(text)
                self.ui.antennaTypeValue.setText(text)

        self.ui.antennaTypeCombo.showPopup = _ask_antenna_type

    # ==========================================================
    # Antenna offset popup
    # ==========================================================
    def _open_antenna_offset_dialog(self):
        """
        UI handler: open antenna offset dialog (E, N, U) with high-precision spin boxes.
        """
        dlg = QDialog(self.ui.antennaOffsetButton)
        dlg.setWindowTitle("Antenna Offset")

        # Parse existing "E, N, U"
        try:
            e0, n0, u0 = [float(x.strip()) for x in self.ui.antennaOffsetValue.text().split(",")]
        except Exception:
            e0 = n0 = u0 = 0.0

        form = QFormLayout(dlg)

        class DecimalSpinBox(QDoubleSpinBox):
            def __init__(self, parent=None, top=10000, bottom=-10000, precision=15, step_size=0.1):
                super().__init__(parent)
                self.setRange(bottom, top)

                self.setDecimals(precision)  # fallback precision
                # up down arrow Step size
                # note there is some float point inaccuracy when useing steps
                self.setSingleStep(step_size)

            def textFromValue(self, value: float) -> str:
                """Format value dynamically with Decimal for more precision"""
                # Convert through Decimal to avoid scientific notation
                d = Decimal(str(value))
                return str(d.normalize())  # trims trailing zeros

            def valueFromText(self, text: str) -> float:
                """Parse text back into a float"""
                try:
                    return float(Decimal(text))
                except InvalidOperation:
                    raise ValueError(f"Failed to convert Antenna offset to float: {text}")

        sb_e = DecimalSpinBox(dlg)
        sb_e.setValue(e0)

        sb_n = DecimalSpinBox(dlg)
        sb_n.setValue(n0)

        sb_u = DecimalSpinBox(dlg)
        sb_u.setValue(u0)

        form.addRow("E:", sb_e)
        form.addRow("N:", sb_n)
        form.addRow("U:", sb_u)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("OK", dlg)
        cancel_btn = QPushButton("Cancel", dlg)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        form.addRow(btn_row)

        ok_btn.clicked.connect(lambda: self._set_antenna_offset(sb_e, sb_n, sb_u, dlg))
        cancel_btn.clicked.connect(dlg.reject)

        dlg.exec()

    def _set_antenna_offset(self, sb_e, sb_n, sb_u, dlg: QDialog):
        """
        UI handler: apply antenna offset values back to UI.

        Arguments:
          sb_e (QDoubleSpinBox): East (E) spin box.
          sb_n (QDoubleSpinBox): North (N) spin box.
          sb_u (QDoubleSpinBox): Up (U) spin box.
          dlg (QDialog): Dialog to accept/close.
        """
        e, n, u = sb_e.value(), sb_n.value(), sb_u.value()
        text = f"{e}, {n}, {u}"
        self.ui.antennaOffsetButton.setText(text)
        self.ui.antennaOffsetValue.setText(text)
        dlg.accept()

    # ==========================================================
    # Time window popup
    # ==========================================================
    def _open_time_window_dialog(self):
        """
        UI handler: open dialog to adjust observation start/end times.
        """
        dlg = QDialog(self.ui.timeWindowValue)
        dlg.setWindowTitle("Select start / end time")

        # Parse existing "yyyy-MM-dd_HH:mm:ss to yyyy-MM-dd_HH:mm:ss"
        current_text = self.ui.timeWindowButton.text()
        try:
            s_text, e_text = current_text.split(" to ")
            s_dt = QDateTime.fromString(s_text, "yyyy-MM-dd_HH:mm:ss")
            e_dt = QDateTime.fromString(e_text, "yyyy-MM-dd_HH:mm:ss")
            if not s_dt.isValid():
                s_dt = QDateTime.fromString(s_text, "yyyy-MM-dd HH:mm:ss")
            if not e_dt.isValid():
                e_dt = QDateTime.fromString(e_text, "yyyy-MM-dd HH:mm:ss")
        except Exception:
            s_dt = e_dt = QDateTime.currentDateTime()

        vbox = QVBoxLayout(dlg)
        start_edit = QDateTimeEdit(s_dt, dlg)
        end_edit = QDateTimeEdit(e_dt, dlg)

        start_edit.setCalendarPopup(True)
        end_edit.setCalendarPopup(True)
        start_edit.setDisplayFormat("yyyy-MM-dd_HH:mm:ss")
        end_edit.setDisplayFormat("yyyy-MM-dd_HH:mm:ss")

        vbox.addWidget(start_edit)
        vbox.addWidget(end_edit)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("OK", dlg)
        cancel_btn = QPushButton("Cancel", dlg)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        vbox.addLayout(btn_row)

        ok_btn.clicked.connect(lambda: self._set_time_window(start_edit, end_edit, dlg))
        cancel_btn.clicked.connect(dlg.reject)

        dlg.exec()

    def _set_time_window(self, start_edit, end_edit, dlg: QDialog):
        """
        UI handler: validate and set selected time window into UI.

        Arguments:
          start_edit (QDateTimeEdit): Start time widget.
          end_edit (QDateTimeEdit): End time widget.
          dlg (QDialog): Dialog to accept/close.
        """
        if end_edit.dateTime() < start_edit.dateTime():
            QMessageBox.warning(dlg, "Time error",
                                "End time cannot be earlier than start time.\nPlease select again.")
            return

        s = start_edit.dateTime().toString("yyyy-MM-dd_HH:mm:ss")
        e = end_edit.dateTime().toString("yyyy-MM-dd_HH:mm:ss")
        self.ui.timeWindowButton.setText(f"{s} to {e}")
        self.ui.timeWindowValue.setText(f"{s} to {e}")
        dlg.accept()

    # ==========================================================
    # Data interval popup
    # ==========================================================
    def _open_data_interval_dialog(self):
        """
        UI handler: prompt for data interval (seconds) and update UI.
        """
        # Extract current value from button text ("30 s" → 30)
        current_text = self.ui.dataIntervalButton.text().replace(" s", "").strip()
        try:
            current_val = int(current_text)
        except ValueError:
            current_val = 1  # fallback if parsing fails

        val, ok = QInputDialog.getInt(
            self.ui.dataIntervalButton,
            "Data interval",
            "Input interval (seconds):",
            current_val,  # prefill with current value
            1,
            999_999,
        )
        if ok:
            text = f"{val} s"
            self.ui.dataIntervalButton.setText(text)
            self.ui.dataIntervalValue.setText(text)

    # endregion

    # region Config and PEA Processing

    def extract_ui_values(self, rnx_path):
        """
        Extract current UI values, parse/normalize them, and return as dataclass.

        Arguments:
          rnx_path (str): Selected RINEX observation file path.

        Returns:
          ExtractedInputs: Dataclass containing parsed fields and raw strings.

        """
        # Extract user input from the UI and assign it to class variables.
        mode_raw = self.ui.modeCombo.currentText() if self.ui.modeCombo.currentText() != "Select one" else "Static"

        # Get constellations from the actual dropdown selections, not the label
        constellations_raw = ""
        combo = self.ui.constellationsCombo
        if hasattr(combo, '_constellation_model') and combo._constellation_model:
            model = combo._constellation_model
            selected = [model.item(i).text() for i in range(model.rowCount()) if
                        model.item(i).checkState() == Qt.Checked]
            constellations_raw = ", ".join(selected)
        else:
            # Fallback to the label text if no custom model exists
            constellations_raw = self.ui.constellationsValue.text()
        time_window_raw = self.ui.timeWindowValue.text()  # Get from button, not value label
        epoch_interval_raw = self.ui.dataIntervalButton.text()  # Get from button, not value label
        receiver_type = self.ui.receiverTypeValue.text()
        antenna_type = self.ui.antennaTypeValue.text()
        antenna_offset_raw = self.ui.antennaOffsetButton.text()  # Get from button, not value label
        ppp_provider = self.ui.pppProviderCombo.currentText() if self.ui.pppProviderCombo.currentText() != "Select one" else ""
        ppp_series = self.ui.pppSeriesCombo.currentText() if self.ui.pppSeriesCombo.currentText() != "Select one" else ""
        ppp_project = self.ui.pppProjectCombo.currentText() if self.ui.pppProjectCombo.currentText() != "Select one" else ""

        # Extract observation codes from combos
        obs_codes = self._extract_observation_codes()

        # Parsed values
        start_epoch, end_epoch = self.parse_time_window(time_window_raw)
        antenna_offset = self.parse_antenna_offset(antenna_offset_raw)
        epoch_interval = int(epoch_interval_raw.replace("s", "").strip())
        marker_name = self.extract_marker_name(rnx_path)
        mode = self.determine_mode_value(mode_raw)

        # Output toggles
        gpx_output = self.ui.gpxCheckbox.isChecked() if hasattr(self.ui, "gpxCheckbox") else True
        pos_output = self.ui.posCheckbox.isChecked() if hasattr(self.ui, "posCheckbox") else True
        trace_output_network = self.ui.traceCheckbox.isChecked() if hasattr(self.ui, "traceCheckbox") else False

        # Returned the values found as a dataclass for easier access
        return self.ExtractedInputs(
            marker_name=marker_name,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            epoch_interval=epoch_interval,
            antenna_offset=antenna_offset,
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
        )

    def _extract_observation_codes(self) -> dict:
        """
        Extract selected observation codes from all constellation list widgets in priority order.

        Returns:
          dict: Dictionary mapping constellation names to lists of selected codes in order
        """
        obs_codes = {}

        list_widget_mapping = {
            'gps': 'gpsListWidget',
            'gal': 'galListWidget',
            'glo': 'gloListWidget',
            'bds': 'bdsListWidget',
            'qzs': 'qzsListWidget'
        }

        for const_name, widget_name in list_widget_mapping.items():
            if not hasattr(self.ui, widget_name):
                obs_codes[const_name] = []
                continue

            list_widget = getattr(self.ui, widget_name)

            # Extract checked items in their current order (priority order)
            selected = []
            for i in range(list_widget.count()):
                item = list_widget.item(i)
                if item.checkState() == Qt.CheckState.Checked:
                    selected.append(item.text())

            obs_codes[const_name] = selected

        return obs_codes

    def on_show_config(self):
        """
        UI handler: reload config, apply UI values, write changes, then open the YAML.
        """
        Logger.terminal("📄 Opening YAML configuration file...")
        # Reload disk version before overwriting with GUI changes
        self.execution.reload_config()
        inputs = self.extract_ui_values(self.rnx_file)
        self.execution.apply_ui_config(inputs)
        self.execution.write_cached_changes()

        # Execution class will throw error when instantiated if the file doesn't exist and it can't create it
        # This code is run after Execution class is instantiated within this file, thus never will occur
        if not os.path.exists(GENERATED_YAML):
            QMessageBox.warning(
                None,
                "File not found",
                f"The file {GENERATED_YAML} does not exist."
            )
            return

        self.on_open_config_in_editor(self.config_path)

    def on_open_config_in_editor(self, file_path):
        """
        Open the config YAML file in the OS default editor/viewer.

        Arguments:
          file_path (str): Absolute or relative path to the YAML file.
        """
        import subprocess
        import platform

        try:
            abs_path = os.path.abspath(file_path)

            # Open the file with the appropriate method for the operating system
            if platform.system() == "Windows":
                os.startfile(abs_path)
                return

            if platform.system() == "Darwin":  # macOS
                subprocess.run(["open", abs_path])

            else:  # Linux and other Unix-like systems
                # When compiled with pyinstaller, LD_LIBRARY_PATH is modified which prevents external app opening
                env = os.environ.copy()
                original = env.get("LD_LIBRARY_PATH_ORIG")
                if original:
                    env["LD_LIBRARY_PATH"] = original  # Restore original value
                else:
                    env.pop("LD_LIBRARY_PATH", None)  # Clear the value to use sys defaults
                subprocess.run(["xdg-open", abs_path], env=env)

        except Exception as e:
            error_message = f"Cannot open config file:\n{file_path}\n\nError: {str(e)}"
            Logger.terminal(f"Error: {error_message}")
            QMessageBox.critical(
                None,
                "Error Opening File",
                error_message
            )

    def on_run_pea(self):
        """
        UI handler: validate time window and config, apply UI, then emit pea_ready.
        """
        raw = self.ui.timeWindowValue.text()

        # --- Parse time window ---
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

        # Store time window so MainWindow can use it later
        self.start_time = start_time
        self.end_time = end_time

        # --- Write updated config ---
        try:
            self.execution.reload_config()
            inputs = self.extract_ui_values(self.rnx_file)
            self.execution.apply_ui_config(inputs)  # config only, no product archiving here
            self.execution.write_cached_changes()
        except Exception as e:
            Logger.terminal(f"⚠️ Failed to apply config: {e}")
            return

        # --- Emit signal for MainWindow ---
        self.pea_ready.emit()

    def _on_reset_config_clicked(self):
        """
        UI handler: reset the configuration file and UI to defaults.
        Shows a confirmation dialog before proceeding.
        """
        # Show confirmation dialog
        reply = QMessageBox.question(
            self.parent,
            "Reset Configuration",
            "This will reset all settings to their defaults.\n\n"
            "• The configuration file will be regenerated from the template\n"
            "• All UI fields will be cleared\n"
            "• You will need to re-select your RINEX file and output directory\n\n"
            "Are you sure you want to continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )

        if reply != QMessageBox.StandardButton.Yes:
            return

        try:
            # Stop any running background workers first
            self.stop_all()

            # Reset the config file
            self.execution.reset_config()

            # Reset the UI to defaults
            self.reset_ui_to_defaults()

            Logger.terminal("🔄 Configuration and UI reset to defaults")
            show_toast(self.parent, "🔄 Configuration and UI reset to defaults", duration=3000)

        except Exception as e:
            Logger.terminal(f"⚠️ Failed to reset configuration: {e}")
            QMessageBox.critical(
                self.parent,
                "Reset Failed",
                f"Failed to reset configuration:\n{e}"
            )

    def _open_user_manual(self):
        """
        Open the USER_MANUAL.md file
        Attempts to open the file in the system's default markdown viewer / browser
        """
        try:
            # Get the path from common_dirs
            manual_path = USER_MANUAL_PATH

            if not manual_path.exists():
                Logger.terminal(f"⚠️ User manual not found at: {manual_path}")
                QMessageBox.warning(
                    self.parent,
                    "User Manual Not Found",
                    f"Could not find the user manual at:\n{manual_path}\n\n"
                    "Please ensure the file exists at /docs/USER_MANUAL.md"
                )
                return

            Logger.terminal(f"📖 Opening user manual: {manual_path}")

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

            Logger.terminal("✅ User manual opened successfully")

        except Exception as e:
            Logger.terminal(f"⚠️ Failed to open user manual: {e}")
            QMessageBox.critical(
                self.parent,
                "Error Opening Manual",
                f"Failed to open the user manual:\n{e}"
            )

    def reset_ui_to_defaults(self):
        """
        Reset all UI fields to their default/initial states.
        This is the "start from scratch" reset that clears all user inputs.
        """
        # Clear internal state
        self.rnx_file = None
        self.output_dir = None
        self.products_df = pd.DataFrame()
        if hasattr(self, 'last_rinex_path'):
            delattr(self, 'last_rinex_path')
        if hasattr(self, 'valid_analysis_centers'):
            self.valid_analysis_centers = []
        if hasattr(self, '_valid_project_series_df'):
            delattr(self, '_valid_project_series_df')
        if hasattr(self, '_valid_series_for_provider'):
            delattr(self, '_valid_series_for_provider')
        if hasattr(self, 'start_time'):
            delattr(self, 'start_time')
        if hasattr(self, 'end_time'):
            delattr(self, 'end_time')

        # Reset MainWindow state
        self.parent.rnx_file = None
        self.parent.output_dir = None

        # Reset General Tab

        # Mode combo - reset to placeholder
        self.ui.modeCombo.clear()
        self.ui.modeCombo.addItem("Select one")
        self.ui.modeCombo.setCurrentIndex(0)

        # Constellations combo - reset to placeholder
        self.ui.constellationsCombo.clear()
        self.ui.constellationsCombo.setEditable(True)
        self.ui.constellationsCombo.lineEdit().clear()
        self.ui.constellationsCombo.lineEdit().setPlaceholderText("Select one or more")
        self.ui.constellationsValue.setText("Constellations")
        # Clear any custom model
        if hasattr(self.ui.constellationsCombo, '_constellation_model'):
            delattr(self.ui.constellationsCombo, '_constellation_model')
        if hasattr(self.ui.constellationsCombo, '_constellation_on_item_changed'):
            delattr(self.ui.constellationsCombo, '_constellation_on_item_changed')

        # Time window - reset to placeholder text
        self.ui.timeWindowButton.setText("Start / End")
        self.ui.timeWindowValue.setText("Time Window")

        # Data interval - reset to placeholder
        self.ui.dataIntervalButton.setText("Interval (Seconds)")
        self.ui.dataIntervalValue.setText("Data interval")

        # Receiver type - reset to placeholder
        self.ui.receiverTypeCombo.clear()
        self.ui.receiverTypeCombo.addItem("Import text")
        self.ui.receiverTypeCombo.setCurrentIndex(0)
        if self.ui.receiverTypeCombo.lineEdit():
            self.ui.receiverTypeCombo.lineEdit().setText("Import text")
        self.ui.receiverTypeValue.setText("Receiver Type")

        # Antenna type - reset to placeholder
        self.ui.antennaTypeCombo.clear()
        self.ui.antennaTypeCombo.addItem("Import text")
        self.ui.antennaTypeCombo.setCurrentIndex(0)
        if self.ui.antennaTypeCombo.lineEdit():
            self.ui.antennaTypeCombo.lineEdit().setText("Import text")
        self.ui.antennaTypeValue.setText("")

        # Antenna offset - reset to default
        self.ui.antennaOffsetButton.setText("0.0, 0.0, 0.0")
        self.ui.antennaOffsetValue.setText("0.0, 0.0, 0.0")

        # PPP Provider - reset to placeholder
        self.ui.pppProviderCombo.clear()
        self.ui.pppProviderCombo.addItem("Select one")
        self.ui.pppProviderCombo.setCurrentIndex(0)

        # PPP Series - reset to placeholder
        self.ui.pppSeriesCombo.clear()
        self.ui.pppSeriesCombo.addItem("Select one")
        self.ui.pppSeriesCombo.setCurrentIndex(0)

        # PPP Project - reset to placeholder
        self.ui.pppProjectCombo.clear()
        self.ui.pppProjectCombo.addItem("Select one")
        self.ui.pppProjectCombo.setCurrentIndex(0)

        # Reset Constellations Tab

        # Clear all constellation list widgets
        list_widgets = ['gpsListWidget', 'galListWidget', 'gloListWidget', 'bdsListWidget', 'qzsListWidget']
        for widget_name in list_widgets:
            if hasattr(self.ui, widget_name):
                list_widget = getattr(self.ui, widget_name)
                list_widget.clear()
                list_widget.setEnabled(False)

        # Hide all constellation widgets and show placeholder
        self._hide_all_constellation_widgets()
        self._update_constellation_placeholder(True)

        # Reset Output Tab

        # Reset output checkboxes to defaults (POS and GPX true, TRACE false)
        if hasattr(self.ui, 'posCheckbox'):
            self.ui.posCheckbox.setChecked(True)
        if hasattr(self.ui, 'gpxCheckbox'):
            self.ui.gpxCheckbox.setChecked(True)
        if hasattr(self.ui, 'traceCheckbox'):
            self.ui.traceCheckbox.setChecked(False)

        # Reset Button States and Locks

        # Disable buttons that should be locked on startup
        self.ui.outputButton.setEnabled(False)
        self.ui.showConfigButton.setEnabled(False)
        self.ui.processButton.setEnabled(False)
        self.ui.stopAllButton.setEnabled(False)

        # Ensure launch buttons are enabled
        self.ui.observationsButton.setEnabled(True)
        self.ui.cddisCredentialsButton.setEnabled(True)

        # Reset Visualisation Panel
        # Clear the visualisation panel
        if hasattr(self.parent, 'visCtrl'):
            self.parent.visCtrl.set_html_files([])
            # Clear the web view
            if hasattr(self.ui, 'webEngineView'):
                self.ui.webEngineView.setHtml("")

        # Reset config tab to General
        # Not really needed since the "Reset Config" button is in General,
        # But just in case for the future / aesthetics
        if hasattr(self.ui, 'configTabWidget'):
            self.ui.configTabWidget.setCurrentIndex(0)

    # endregion

    # region Utility Functions

    @staticmethod
    def _set_combobox_by_value(combo: QComboBox, value: str):
        """
        Helper: find a value in a combo and set current index if present.

        Arguments:
          combo (QComboBox): Target combo box.
          value (str): Text to search.
        """
        if value is None:
            return
        idx = combo.findText(value)
        if idx != -1:
            combo.setCurrentIndex(idx)

    @staticmethod
    def _select_rnx_file(parent) -> str:
        """
        Open a file dialog to select a RINEX observation file.

        Arguments:
          parent (Any): Parent widget.

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
          parent (Any): Parent widget.

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
        m = re.match(r"([A-Za-z]{4})", stem)
        return m.group(1).upper() if m else "TEST"

    @staticmethod
    def parse_time_window(time_window_raw: str):
        """
        Convert 'start_time to end_time' into (start_epoch, end_epoch) strings.

        Arguments:
          time_window_raw (str): e.g., 'YYYY-MM-DD_HH:MM:SS to YYYY-MM-DD_HH:MM:SS'.

        Returns:
          tuple[str, str]: (start_epoch, end_epoch) with underscores preserved for UI.

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

    @dataclass
    class ExtractedInputs:
        """
        Dataclass container for parsed UI values and raw strings.
        """
        # Parsed / derived values
        marker_name: str
        start_epoch: str
        end_epoch: str
        epoch_interval: int
        antenna_offset: list[float]
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

    # endregion

    # region Statics

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

        Arguments:
          None

        Returns:
          list[str]: ['GPS', 'GAL', 'GLO', 'BDS', 'QZS']

        Example:
          >>> InputController._get_constellations_items()
          ['GPS', 'GAL', 'GLO', 'BDS', 'QZS']
        """
        return ["GPS", "GAL", "GLO", "BDS", "QZS"]

    def _get_ppp_provider_items(self) -> List[str]:
        """
        Provide available PPP providers from the cached products DataFrame.

        Returns:
          list[str]: Provider names; empty when products list is not yet available.

        Example:
          >>> ctrl._get_ppp_provider_items()
        """
        if hasattr(self, "valid_analysis_centers") and self.valid_analysis_centers:
            return self.valid_analysis_centers
        return []

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

    # endregion


class CredentialsDialog(QDialog):
    """
    UI controller class CredentialsDialog.
    """

    def __init__(self, parent=None):
        """
        UI handler: initialize credential input widgets and layout.

        Arguments:
          parent (Any): Optional parent widget.
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
        UI handler: validate username/password, save to netrc, and close dialog.
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


# Minimal unified stop entry for InputController background worker
def _safe_call_stop(obj):
    """
    Safely call .stop() on an object if present, ignoring exceptions.

    Arguments:
      obj (Any): Object that may implement stop().
    """
    try:
        if obj is not None and hasattr(obj, "stop"):
            obj.stop()
    except Exception:
        pass


def stop_all(self):
    """
    Best-effort stop for the metadata PPPWorker started by the controller.

    Arguments:
      self (InputController): Controller instance owning the worker/thread.
    """
    try:
        # Request the worker to stop - it will emit cancelled signal when done
        if hasattr(self, "worker") and self.worker is not None:
            self.worker.stop()
        # Restore cursor when stopping
        if hasattr(self, "parent"):
            self.parent.setCursor(Qt.CursorShape.ArrowCursor)
    except Exception:
        pass

# Bind without touching existing class body
setattr(InputController, "stop_all", stop_all)