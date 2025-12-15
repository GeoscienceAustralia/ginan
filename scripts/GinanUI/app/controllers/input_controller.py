# app/controllers/input_controller.py
"""
UI input flow controller for the Ginan-UI.
"""
from __future__ import annotations

import os
import re
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
from PySide6.QtGui import QStandardItemModel, QStandardItem
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
    QLabel
)

from scripts.GinanUI.app.models.execution import Execution, GENERATED_YAML, INPUT_PRODUCTS_PATH
from scripts.GinanUI.app.models.rinex_extractor import RinexExtractor
from scripts.GinanUI.app.utils.cddis_credentials import save_earthdata_credentials
from scripts.GinanUI.app.models.archive_manager import (archive_products_if_rinex_changed)
from scripts.GinanUI.app.models.archive_manager import archive_old_outputs
from scripts.GinanUI.app.utils.workers import DownloadWorker
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

        ### Bind: configuration drop-downs / UIs ###

        self._bind_combo(self.ui.Mode, self._get_mode_items)

        # PPP_provider, project and series
        self.ui.PPP_provider.currentTextChanged.connect(self._on_ppp_provider_changed)
        self.ui.PPP_project.currentTextChanged.connect(self._on_ppp_project_changed)
        self.ui.PPP_series.currentTextChanged.connect(self._on_ppp_series_changed)

        # Constellations
        self._bind_multiselect_combo(
            self.ui.Constellations_2,
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

        self.setup_tooltips()

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
        cddis_style = self.ui.cddisCredentialsButton.styleSheet() + tooltip_style

        self.ui.observationsButton.setStyleSheet(obs_style)
        self.ui.outputButton.setStyleSheet(out_style)
        self.ui.processButton.setStyleSheet(proc_style)
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
            "Start the Ginan (pea) PPP processing using the configured parameters.\n"
            "Ensure all required fields are filled before processing."
        )

        # Configuration buttons
        self.ui.showConfigButton.setToolTip(
            "Generate and open the YAML configuration file.\n"
            "You can review and modify advanced settings before processing.\n"
            "Note: UI defined parameters will ALWAYS override manual config edits."
        )

        self.ui.cddisCredentialsButton.setToolTip(
            "Set your NASA Earthdata credentials for downloading PPP products\n"
            "Required for accessing the CDDIS archive data"
        )

        # Input fields and combos
        self.ui.Mode.setToolTip(
            "Processing mode:\n"
            "• Static: For stationary receivers\n"
            "• Kinematic: For moving receivers\n"
            "• Dynamic: For high-dynamic applications"
        )

        self.ui.Constellations_2.setToolTip(
            "Select which GNSS constellations to use:\n"
            "GPS, Galileo (GAL), GLONASS (GLO), BeiDou (BDS), QZSS (QZS)\n"
            "More constellations generally improve accuracy"
        )

        self.ui.PPP_provider.setToolTip(
            "Analysis centre that provides PPP products\n"
            "Options populated based on your observation time window"
        )

        self.ui.PPP_project.setToolTip(
            "PPP product project type.\n"
            "Different projects types offer varying GNSS constellation PPP products."
        )

        self.ui.PPP_series.setToolTip(
            "PPP product series:\n"
            "• ULT: Ultra-rapid (lower latency)\n"
            "• RAP: Rapid \n"
            "• FIN: Final (highest accuracy)"
        )

        # Receiver/Antenna fields
        self.ui.Receiver_type.setToolTip(
            "Receiver model extracted from RINEX header\n"
            "Click to manually edit if needed"
        )

        self.ui.Antenna_type.setToolTip(
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
            self._on_cddis_ready(pd.DataFrame(), False)  # Clears providers until worker completes

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
            self.worker = DownloadWorker(start_epoch=start_epoch, end_epoch=end_epoch, analysis_centers=True)
            self.metadata_thread = QThread()
            self.worker.moveToThread(self.metadata_thread)

            self.worker.finished.connect(self._on_cddis_ready)
            self.worker.finished.connect(self._restore_cursor)  # Restore cursor when done
            self.worker.finished.connect(self.worker.deleteLater)
            self.worker.finished.connect(self.metadata_thread.quit)
            self.metadata_thread.finished.connect(self.metadata_thread.deleteLater)
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

            self.ui.Receiver_type.clear()
            self.ui.Receiver_type.addItem(result["receiver_type"])
            self.ui.Receiver_type.setCurrentIndex(0)
            self.ui.Receiver_type.lineEdit().setText(result["receiver_type"])

            self.ui.Antenna_type.clear()
            self.ui.Antenna_type.addItem(result["antenna_type"])
            self.ui.Antenna_type.setCurrentIndex(0)
            self.ui.Antenna_type.lineEdit().setText(result["antenna_type"])

            self._update_constellations_multiselect(result["constellations"])

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
        combo = self.ui.Constellations_2

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

    def _on_cddis_ready(self, data: pd.DataFrame, log_messages: bool = True):
        """
        UI handler: receive PPP products DataFrame from worker and populate provider/project/series combos.
        """
        self.products_df = data

        if data.empty:
            self.valid_analysis_centers = []
            self.ui.PPP_provider.clear()
            self.ui.PPP_provider.addItem("None")
            self.ui.PPP_series.clear()
            self.ui.PPP_series.addItem("None")
            return

        self.valid_analysis_centers = list(get_valid_analysis_centers(self.products_df))

        if len(self.valid_analysis_centers) == 0:
            if log_messages:
                Logger.terminal("⚠️ No valid PPP providers found.")
            self.ui.PPP_provider.clear()
            self.ui.PPP_provider.addItem("None")
            self.ui.PPP_series.clear()
            self.ui.PPP_series.addItem("None")
            return

        self.ui.PPP_provider.blockSignals(True)
        self.ui.PPP_provider.clear()
        self.ui.PPP_provider.addItems(self.valid_analysis_centers)
        self.ui.PPP_provider.setCurrentIndex(0)

        # Update PPP_series based on default PPP_provider
        self.ui.PPP_provider.blockSignals(False)
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
        self.ui.PPP_provider.clear()
        self.ui.PPP_provider.addItem("None")
        # Restore cursor in case of error
        self.parent.setCursor(Qt.CursorShape.ArrowCursor)
        # Show error toast
        show_toast(self.parent, "⚠️ Failed to scan CDDIS archive", duration=4000)

    def _restore_cursor(self):
        """
        Restore the cursor to normal arrow after background operation completes.
        """
        self.parent.setCursor(Qt.CursorShape.ArrowCursor)

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
            self.ui.PPP_project.blockSignals(True)
            self.ui.PPP_series.blockSignals(True)

            self.ui.PPP_project.clear()
            self.ui.PPP_series.clear()

            self.ui.PPP_project.addItems(project_options)
            self.ui.PPP_series.addItems(series_options)

            self.ui.PPP_project.setCurrentIndex(0)
            self.ui.PPP_series.setCurrentIndex(0)

            # Unblock signals now that the population is complete
            self.ui.PPP_project.blockSignals(False)
            self.ui.PPP_series.blockSignals(False)

        except Exception as e:
            self.ui.PPP_series.clear()
            self.ui.PPP_series.addItem("None")
            self.ui.PPP_project.clear()
            self.ui.PPP_project.addItem("None")

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

        self.ui.PPP_project.blockSignals(True)
        self.ui.PPP_project.clear()
        self.ui.PPP_project.addItems(valid_projects)
        self.ui.PPP_project.setCurrentIndex(0)
        self.ui.PPP_project.blockSignals(False)

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

        self.ui.PPP_series.blockSignals(True)
        self.ui.PPP_series.clear()
        self.ui.PPP_series.addItems(valid_series)
        self.ui.PPP_series.setCurrentIndex(0)
        self.ui.PPP_series.blockSignals(False)

        Logger.terminal(f"[UI] Filtered PPP_series for project '{selected_project}': {valid_series}")

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
        self.ui.Receiver_type.setEditable(True)
        self.ui.Receiver_type.lineEdit().setReadOnly(True)
        self.ui.Antenna_type.setEditable(True)
        self.ui.Antenna_type.lineEdit().setReadOnly(True)

        # Receiver type free text
        def _ask_receiver_type():
            current_text = self.ui.Receiver_type.currentText().strip()
            text, ok = QInputDialog.getText(
                self.ui.Receiver_type,
                "Receiver Type",
                "Enter receiver type:",
                text=current_text  # prefill with current
            )
            if ok and text:
                self.ui.Receiver_type.clear()
                self.ui.Receiver_type.addItem(text)
                self.ui.Receiver_type.lineEdit().setText(text)
                self.ui.receiverTypeValue.setText(text)

        self.ui.Receiver_type.showPopup = _ask_receiver_type

        # Antenna type free text
        def _ask_antenna_type():
            current_text = self.ui.Antenna_type.currentText().strip()
            text, ok = QInputDialog.getText(
                self.ui.Antenna_type,
                "Antenna Type",
                "Enter antenna type:",
                text=current_text  # prefill with current
            )
            if ok and text:
                self.ui.Antenna_type.clear()
                self.ui.Antenna_type.addItem(text)
                self.ui.Antenna_type.lineEdit().setText(text)
                self.ui.antennaTypeValue.setText(text)

        self.ui.Antenna_type.showPopup = _ask_antenna_type

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
        mode_raw = self.ui.Mode.currentText() if self.ui.Mode.currentText() != "Select one" else "Static"

        # Get constellations from the actual dropdown selections, not the label
        constellations_raw = ""
        combo = self.ui.Constellations_2
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
        ppp_provider = self.ui.PPP_provider.currentText() if self.ui.PPP_provider.currentText() != "Select one" else ""
        ppp_series = self.ui.PPP_series.currentText() if self.ui.PPP_series.currentText() != "Select one" else ""
        ppp_project = self.ui.PPP_project.currentText() if self.ui.PPP_project.currentText() != "Select one" else ""

        # Parsed values
        start_epoch, end_epoch = self.parse_time_window(time_window_raw)
        antenna_offset = self.parse_antenna_offset(antenna_offset_raw)
        epoch_interval = int(epoch_interval_raw.replace("s", "").strip())
        marker_name = self.extract_marker_name(rnx_path)
        mode = self.determine_mode_value(mode_raw)

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
        )

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
        if hasattr(self, "worker"):
            _safe_call_stop(self.worker)
        # Restore cursor when stopping
        if hasattr(self, "parent"):
            self.parent.setCursor(Qt.CursorShape.ArrowCursor)
    except Exception:
        pass


# Bind without touching existing class body
setattr(InputController, "stop_all", stop_all)