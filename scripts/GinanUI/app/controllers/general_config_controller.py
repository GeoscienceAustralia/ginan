"""
Controller for the General configuration tab.

Manages the following UI widgets and background workflows:
  - Mode combo (Static / Kinematic / Dynamic)
  - Constellations multi-select combo
  - PPP Provider / Project / Series combos
  - Receiver Type and Antenna Type (free-text combos)
  - Antenna Offset button / dialog
  - Apriori Position button / dialog
  - Time Window button / dialog
  - Data Interval button / dialog
  - CDDIS archive scanning for valid PPP analysis centres (DownloadWorker)
  - SINEX validation against RINEX-extracted metadata (SinexValidationWorker)
  - Constellation info retrieval from SP3 headers
"""

from __future__ import annotations
from datetime import datetime
from pathlib import Path
from typing import Callable, List
import pandas as pd
from PySide6.QtCore import QDateTime, QObject, QThread, Qt
from PySide6.QtGui import QStandardItem, QStandardItemModel
from PySide6.QtWidgets import (
    QComboBox,
    QDateTimeEdit,
    QDialog,
    QDoubleSpinBox,
    QFormLayout,
    QHBoxLayout,
    QInputDialog,
    QLineEdit,
    QMessageBox,
    QPushButton,
    QSizePolicy,
)
from scripts.GinanUI.app.models.dl_products import (
    get_valid_analysis_centers,
    get_valid_series_for_provider,
)
from scripts.GinanUI.app.models.execution import INPUT_PRODUCTS_PATH
from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.utils.toast import show_toast
from scripts.GinanUI.app.utils.workers import DownloadWorker, SinexValidationWorker

class GeneralConfigController(QObject):
    """
    Manages the General configuration tab widgets and the background workflows
    (CDDIS scanning, SINEX validation) that are triggered from this tab.

    Arguments:
      ui: The main window UI instance.
      input_ctrl: The parent InputController instance (for accessing shared state).
    """

    def __init__(self, ui, input_ctrl):
        """
        Initialise config panel bindings and background worker state.

        Arguments:
          ui: The main window UI instance.
          input_ctrl: The parent InputController that owns shared state.
        """
        super().__init__(parent=input_ctrl)
        self.ui = ui
        self.ctrl = input_ctrl  # parent InputController

        # Mode combo
        self._bind_combo(self.ui.modeCombo, lambda: ["Static", "Kinematic", "Dynamic"])

        # PPP provider, project and series
        self.ui.pppProviderCombo.currentTextChanged.connect(self._on_ppp_provider_changed)
        self.ui.pppProjectCombo.currentTextChanged.connect(self._on_ppp_project_changed)
        self.ui.pppSeriesCombo.currentTextChanged.connect(self._on_ppp_series_changed)

        # Constellations multi-select
        self._bind_multiselect_combo(
            self.ui.constellationsCombo,
            lambda: ["GPS", "GAL", "GLO", "BDS", "QZS"],
            self.ui.constellationsValue,
            placeholder="Select one or more",
        )

        # Receiver/Antenna types: free-text input
        self._enable_free_text_for_receiver_and_antenna()

        # Antenna offset
        self.ui.antennaOffsetButton.clicked.connect(self._open_antenna_offset_dialog)
        self.ui.antennaOffsetButton.setCursor(Qt.CursorShape.PointingHandCursor)
        self.ui.antennaOffsetValue.setText("0.0, 0.0, 0.0")

        # Apriori position
        self.ui.aprioriPositionButton.clicked.connect(self._open_apriori_position_dialog)
        self.ui.aprioriPositionButton.setCursor(Qt.CursorShape.PointingHandCursor)

        # Time window and data interval
        self.ui.timeWindowButton.clicked.connect(self._open_time_window_dialog)
        self.ui.timeWindowButton.setCursor(Qt.CursorShape.PointingHandCursor)
        self.ui.dataIntervalButton.clicked.connect(self._open_data_interval_dialog)
        self.ui.dataIntervalButton.setCursor(Qt.CursorShape.PointingHandCursor)

        # CDDIS analysis centre scan worker tracking
        self._worker = None
        self._metadata_thread = None
        self._pending_threads = []

        # SINEX validation worker tracking
        self._sinex_worker = None
        self._sinex_thread = None
        self._sinex_path = None

    #region UI Tooltips

    def setup_tooltips(self):
        """
        Set up tooltips for all General config tab widgets.
        """
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
        self.ui.receiverTypeCombo.setToolTip(
            "Receiver model extracted from RINEX header\n"
            "Click to manually edit if needed"
        )
        self.ui.antennaTypeCombo.setToolTip(
            "Antenna model extracted from RINEX header\n"
            "Must match entries in the ANTEX (.atx) calibration file\n"
            "Click to manually edit if needed"
        )
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
        self.ui.aprioriPositionButton.setToolTip(
            "Approximate receiver position in ECEF coordinates (X, Y, Z) in metres\n"
            "Typically extracted from RINEX header\n"
            "Click to modify if needed"
        )
        self.ui.receiverTypeValue.setToolTip("Receiver type from RINEX header")
        self.ui.antennaTypeValue.setToolTip("Antenna type from RINEX header")
        self.ui.constellationsValue.setToolTip("Available constellations in RINEX data")
        self.ui.timeWindowValue.setToolTip("Observation time span")
        self.ui.dataIntervalValue.setToolTip("Data sampling interval")
        self.ui.antennaOffsetValue.setToolTip("Antenna offset: East, North, Up (metres)")

    #endregion

    #region UI Population from RINEX Extraction

    def populate_from_rinex(self, result: dict):
        """
        Populate the General config tab fields with extracted RINEX metadata.

        Arguments:
          result (dict): Dictionary from RinexExtractor.extract_rinex_data().
        """
        self.ui.constellationsValue.setText(result["constellations"])
        self.ui.timeWindowValue.setText(f"{result['start_epoch']} to {result['end_epoch']}")
        self.ui.timeWindowButton.setText(f"{result['start_epoch']} to {result['end_epoch']}")
        self.ui.dataIntervalButton.setText(f"{result['epoch_interval']} s")
        self.ctrl.rinex_epoch_interval = float(result['epoch_interval'])
        self.ui.receiverTypeValue.setText(result["receiver_type"])
        self.ui.antennaTypeValue.setText(result["antenna_type"])
        self.ui.antennaOffsetValue.setText(", ".join(map(str, result["antenna_offset"])))
        self.ui.antennaOffsetButton.setText(", ".join(map(str, result["antenna_offset"])))

        # Populate apriori position if available
        apriori = result.get("apriori_position")
        if apriori and any(v != 0.0 for v in apriori):
            self.ui.aprioriPositionButton.setText(", ".join(map(str, apriori)))
        else:
            self.ui.aprioriPositionButton.setText("0.0, 0.0, 0.0")

        # Receiver and antenna type combos
        self.ui.receiverTypeCombo.clear()
        self.ui.receiverTypeCombo.addItem(result["receiver_type"])
        self.ui.receiverTypeCombo.setCurrentIndex(0)
        self.ui.receiverTypeCombo.lineEdit().setText(result["receiver_type"])

        self.ui.antennaTypeCombo.clear()
        self.ui.antennaTypeCombo.addItem(result["antenna_type"])
        self.ui.antennaTypeCombo.setCurrentIndex(0)
        self.ui.antennaTypeCombo.lineEdit().setText(result["antenna_type"])

        # Constellation multi-select
        self._update_constellations_multiselect(result["constellations"])

    #endregion

    #region Constellations Multi-Select

    def _update_constellations_multiselect(self, constellation_str: str):
        """
        Populate and mirror a multi-select constellation combo with checkboxes.

        Arguments:
          constellation_str (str): Comma-separated constellations (e.g., "GPS, GAL, GLO").
        """
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
            self.ctrl.constellations_tab.sync_list_widgets_to_selection()

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
        self.ctrl.constellations_tab.sync_list_widgets_to_selection()

    def get_selected_constellations_text(self) -> str:
        """
        Return comma-separated text of currently selected constellations from the General tab combo.

        Returns:
          str: e.g. "GPS, GAL, GLO" or fallback from the label.
        """
        combo = self.ui.constellationsCombo
        if hasattr(combo, '_constellation_model') and combo._constellation_model:
            model = combo._constellation_model
            selected = [model.item(i).text() for i in range(model.rowCount()) if model.item(i).checkState() == Qt.Checked]
            return ", ".join(selected)
        # Fallback to the label text if no custom model exists
        return self.ui.constellationsValue.text()

    def get_selected_constellation_set(self) -> set:
        """
        Return a set of currently selected constellation names (upper-cased).

        Returns:
          set[str]: e.g. {'GPS', 'GAL', 'GLO'}
        """
        selected = set()
        combo = self.ui.constellationsCombo
        if hasattr(combo, '_constellation_model') and combo._constellation_model:
            model = combo._constellation_model
            for i in range(model.rowCount()):
                if model.item(i).checkState() == Qt.Checked:
                    selected.add(model.item(i).text().upper())
        return selected

    def update_constellations_for_ppp_selection(self):
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
        if hasattr(self.ctrl, 'provider_constellations') and self.ctrl.provider_constellations:
            try:
                available_constellations = self.ctrl.provider_constellations.get(provider, {}).get(series, {}).get(project, set())
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
        self.ctrl.constellations_tab.sync_list_widgets_to_selection()

    #endregion

    # region Time Window Dialog

    def _open_time_window_dialog(self):
        """
        UI handler: open dialog to adjust observation start/end times.
        """
        dlg = QDialog(self.ui.timeWindowButton)
        dlg.setWindowTitle("Time Window")

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

        form = QFormLayout(dlg)
        start_edit = QDateTimeEdit(s_dt, dlg)
        end_edit = QDateTimeEdit(e_dt, dlg)
        start_edit.setCalendarPopup(True)
        end_edit.setCalendarPopup(True)
        start_edit.setDisplayFormat("yyyy-MM-dd_HH:mm:ss")
        end_edit.setDisplayFormat("yyyy-MM-dd_HH:mm:ss")
        start_edit.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        end_edit.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        form.addRow("Start:", start_edit)
        form.addRow("End:", end_edit)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("OK", dlg)
        cancel_btn = QPushButton("Cancel", dlg)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        form.addRow(btn_row)

        ok_btn.clicked.connect(lambda: self._set_time_window(start_edit, end_edit, dlg))
        cancel_btn.clicked.connect(dlg.reject)
        dlg.setMinimumWidth(300)
        dlg.setFixedHeight(dlg.sizeHint().height())
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

    # endregion

    # region Data Interval Dialog

    def _open_data_interval_dialog(self):
        """
        UI handler: open dialog to adjust data interval (seconds).
        """
        dlg = QDialog(self.ui.dataIntervalButton)
        dlg.setWindowTitle("Data Interval")

        current_text = self.ui.dataIntervalButton.text().replace(" s", "").strip()
        try:
            current_val = float(current_text)
        except ValueError:
            current_val = 1.0

        form = QFormLayout(dlg)
        interval_spin = QDoubleSpinBox(dlg)
        interval_spin.setRange(0.01, 999999.99)
        interval_spin.setDecimals(2)
        interval_spin.setValue(current_val)
        interval_spin.setSuffix(" s")
        interval_spin.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        form.addRow("Interval:", interval_spin)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("OK", dlg)
        cancel_btn = QPushButton("Cancel", dlg)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        form.addRow(btn_row)

        ok_btn.clicked.connect(lambda: self._set_data_interval(interval_spin, dlg))
        cancel_btn.clicked.connect(dlg.reject)
        dlg.setMinimumWidth(300)
        dlg.setFixedHeight(dlg.sizeHint().height())
        dlg.exec()

    def _set_data_interval(self, interval_spin, dlg: QDialog):
        """
        UI handler: apply data interval value back to UI.

        Arguments:
          interval_spin (QDoubleSpinBox): Interval spin box.
          dlg (QDialog): Dialog to accept/close.
        """
        val = interval_spin.value()
        text = f"{int(val)} s" if val == int(val) else f"{val:.2f} s"
        self.ui.dataIntervalButton.setText(text)
        self.ui.dataIntervalValue.setText(text)
        dlg.accept()

    # endregion

    # region Receiver / Antenna Type Dialog

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
                text=current_text
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
                text=current_text
            )
            if ok and text:
                self.ui.antennaTypeCombo.clear()
                self.ui.antennaTypeCombo.addItem(text)
                self.ui.antennaTypeCombo.lineEdit().setText(text)
                self.ui.antennaTypeValue.setText(text)

        self.ui.antennaTypeCombo.showPopup = _ask_antenna_type

    # endregion

    # region Antenna Type Verification

    def verify_antenna_type(self, result: dict):
        """
        UI handler: verify that the RINEX antenna_type exists in the selected ANTEX (.atx) file.

        Arguments:
          result (dict): RINEX extraction result containing 'antenna_type'.
        """
        atx_path = self._get_best_atx_path()

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
                        Logger.workflow("✅ Antenna type verified from .atx file")
                        return

        # Not found! Return warning to user
        QMessageBox.warning(
            None,
            "Provided Antenna Type Invalid",
            f'Provided antenna type in .rnx file: "{result["antenna_type"]}"\n'
            f'not found in .atx file: "{atx_path}"'
        )
        Logger.workflow(f"⚠️ Antenna type failed to verify from .atx file: {atx_path}")
        return

    def _get_best_atx_path(self):
        """
        Select the best available ANTEX (.atx) file with a priority order.

        Returns:
          Path: Path to the best available .atx file.

        Raises:
          FileNotFoundError: If no .atx file is found.
        """
        atx_files = list(INPUT_PRODUCTS_PATH.glob("*.atx"))
        if len(atx_files) == 0:
            raise FileNotFoundError("No .atx file found")
        elif len(atx_files) > 1:
            priority_order = ['igs20.atx', 'igs14.atx', 'igs13.atx', 'igs08.atx', 'igs05.atx']
            atx_path = None
            for best_atx in priority_order:
                matching_files = [f for f in atx_files if f.name == best_atx]
                if matching_files:
                    atx_path = matching_files[0]
                    Logger.workflow(f"📁 Selected .atx file: {atx_path.name} based on priority")
                    break

            if atx_path is None:
                atx_path = atx_files[0]
                Logger.workflow(f"📁 Selected .atx file: {atx_path.name} based on fallback")
        else:
            atx_path = atx_files[0]
        return atx_path

    # endregion

    # region Antenna Offset Dialog

    def _open_antenna_offset_dialog(self):
        """
        UI handler: open antenna offset dialog (E, N, U) with text input fields.
        """
        dlg = QDialog(self.ui.antennaOffsetButton)
        dlg.setWindowTitle("Antenna Offset")

        try:
            e0, n0, u0 = [x.strip() for x in self.ui.antennaOffsetValue.text().split(",")]
        except Exception:
            e0 = n0 = u0 = "0.0"

        form = QFormLayout(dlg)
        edit_e = QLineEdit(str(e0), dlg)
        edit_n = QLineEdit(str(n0), dlg)
        edit_u = QLineEdit(str(u0), dlg)
        form.addRow("E:", edit_e)
        form.addRow("N:", edit_n)
        form.addRow("U:", edit_u)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("OK", dlg)
        cancel_btn = QPushButton("Cancel", dlg)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        form.addRow(btn_row)

        ok_btn.clicked.connect(lambda: self._set_antenna_offset(edit_e, edit_n, edit_u, dlg))
        cancel_btn.clicked.connect(dlg.reject)
        dlg.setMinimumWidth(300)
        dlg.setFixedHeight(dlg.sizeHint().height())
        dlg.exec()

    def _set_antenna_offset(self, edit_e, edit_n, edit_u, dlg: QDialog):
        """
        UI handler: apply antenna offset values back to UI.

        Arguments:
          edit_e (QLineEdit): East (E) input field.
          edit_n (QLineEdit): North (N) input field.
          edit_u (QLineEdit): Up (U) input field.
          dlg (QDialog): Dialog to accept/close.
        """
        try:
            e = float(edit_e.text().strip())
            n = float(edit_n.text().strip())
            u = float(edit_u.text().strip())
        except ValueError:
            QMessageBox.warning(dlg, "Invalid input", "Please enter valid numeric values.")
            return

        text = f"{e}, {n}, {u}"
        self.ui.antennaOffsetButton.setText(text)
        self.ui.antennaOffsetValue.setText(text)
        dlg.accept()

    # endregion

    # region Apriori Position Dialog

    def _open_apriori_position_dialog(self):
        """
        UI handler: open apriori position dialog (X, Y, Z) with text input fields.
        """
        dlg = QDialog(self.ui.aprioriPositionButton)
        dlg.setWindowTitle("Apriori Position (ECEF)")

        try:
            x0, y0, z0 = [x.strip() for x in self.ui.aprioriPositionButton.text().split(",")]
        except Exception:
            x0 = y0 = z0 = "0.0"

        form = QFormLayout(dlg)
        edit_x = QLineEdit(str(x0), dlg)
        edit_y = QLineEdit(str(y0), dlg)
        edit_z = QLineEdit(str(z0), dlg)
        form.addRow("X:", edit_x)
        form.addRow("Y:", edit_y)
        form.addRow("Z:", edit_z)

        btn_row = QHBoxLayout()
        ok_btn = QPushButton("OK", dlg)
        cancel_btn = QPushButton("Cancel", dlg)
        btn_row.addWidget(ok_btn)
        btn_row.addWidget(cancel_btn)
        form.addRow(btn_row)

        ok_btn.clicked.connect(lambda: self._set_apriori_position(edit_x, edit_y, edit_z, dlg))
        cancel_btn.clicked.connect(dlg.reject)
        dlg.setMinimumWidth(300)
        dlg.setFixedHeight(dlg.sizeHint().height())
        dlg.exec()

    def _set_apriori_position(self, edit_x, edit_y, edit_z, dlg: QDialog):
        """
        UI handler: apply apriori position values back to UI.

        Arguments:
          edit_x (QLineEdit): X coordinate input field.
          edit_y (QLineEdit): Y coordinate input field.
          edit_z (QLineEdit): Z coordinate input field.
          dlg (QDialog): Dialog to accept/close.
        """
        try:
            x = float(edit_x.text().strip())
            y = float(edit_y.text().strip())
            z = float(edit_z.text().strip())
        except ValueError:
            QMessageBox.warning(dlg, "Invalid input", "Please enter valid numeric values.")
            return

        text = f"{x}, {y}, {z}"
        self.ui.aprioriPositionButton.setText(text)
        dlg.accept()

    # endregion

    # region SINEX Validation

    def start_sinex_validation(self, target_date: datetime, marker_name: str, receiver_type: str,
                               antenna_type: str, antenna_offset: list, apriori_position: list = None):
        """
        Start SINEX validation in a background thread.

        Arguments:
          target_date (datetime): Date for which to download the SINEX file.
          marker_name (str): 4-character marker name from RINEX.
          receiver_type (str): Receiver type from RINEX.
          antenna_type (str): Antenna type from RINEX.
          antenna_offset (list): Antenna offset [E, N, U] from RINEX.
          apriori_position (list): Optional apriori position [X, Y, Z] from RINEX.
        """
        if not marker_name or len(marker_name) < 4:
            Logger.workflow("⚠️ Invalid marker name - SINEX validation skipped")
            return

        # Stop any existing SINEX worker
        self._stop_sinex_worker()

        Logger.workflow(f"📋 Starting SINEX validation for marker '{marker_name[:4]}'...")

        # Create worker and thread
        self._sinex_worker = SinexValidationWorker(
            target_date=target_date,
            marker_name=marker_name[:4],  # Use first 4 characters
            receiver_type=receiver_type,
            antenna_type=antenna_type,
            antenna_offset=antenna_offset,
            apriori_position=apriori_position,
        )
        self._sinex_thread = QThread()
        self._sinex_worker.moveToThread(self._sinex_thread)

        # Connect signals
        self._sinex_worker.finished.connect(self._on_sinex_validation_finished)
        self._sinex_worker.error.connect(self._on_sinex_validation_error)
        self._sinex_worker.progress.connect(self._on_sinex_validation_progress)

        self._sinex_thread.started.connect(self._sinex_worker.run)
        self._sinex_worker.finished.connect(self._sinex_thread.quit)
        self._sinex_worker.error.connect(self._sinex_thread.quit)
        self._sinex_thread.finished.connect(self._on_sinex_thread_finished)

        self._sinex_thread.start()

    def _stop_sinex_worker(self):
        """
        Stop any running SINEX validation worker and clean up thread resources.
        """
        if self._sinex_worker is not None:
            self._sinex_worker.stop()
            try:
                self._sinex_worker.finished.disconnect()
                self._sinex_worker.error.disconnect()
                self._sinex_worker.progress.disconnect()
            except (RuntimeError, TypeError):
                pass

        if self._sinex_thread is not None:
            try:
                self._sinex_thread.started.disconnect()
                self._sinex_thread.finished.disconnect()
            except (RuntimeError, TypeError):
                pass

            if self._sinex_thread.isRunning():
                self._sinex_thread.quit()
                if not self._sinex_thread.wait(2000):
                    Logger.console("⚠️ SINEX thread did not stop gracefully, forcing termination")
                    self._sinex_thread.terminate()
                    self._sinex_thread.wait(1000)

        self._sinex_worker = None
        self._sinex_thread = None

    def _on_sinex_validation_progress(self, description: str, percent: int):
        """
        UI handler: update progress bar during SINEX download.

        Arguments:
          description (str): Progress description (filename).
          percent (int): Progress percentage (0-100).
        """
        if hasattr(self.ui, 'progressBar'):
            self.ui.progressBar.setValue(percent)
            self.ui.progressBar.setFormat(f"📥 {description}: {percent}%")

    def _on_sinex_validation_finished(self, sinex_path, validation_results: dict):
        """
        UI handler: SINEX validation completed.

        Arguments:
          sinex_path (Path): Path to the downloaded SINEX file.
          validation_results (dict): Validation results dictionary.
        """
        self._sinex_path = sinex_path

        # Store the SINEX filename for later use in apply_ui_config()
        if sinex_path is not None:
            self.ctrl._sinex_filename = sinex_path.name

        # Reset progress bar
        if hasattr(self.ui, 'progressBar'):
            self.ui.progressBar.setValue(0)
            self.ui.progressBar.setFormat("")

        # Check validation results and show appropriate toast
        if 'error' in validation_results:
            show_toast(self.ctrl.parent, f"⚠️ SINEX validation error: {validation_results['error']}", duration=5000)
            return

        if not validation_results.get('marker_found', False):
            show_toast(self.ctrl.parent, f"ℹ️ Marker not found in SINEX file", duration=3000)
            return

        # Apply SINEX apriori_position to UI if available (SINEX is more accurate than RINEX)
        apriori_result = validation_results.get('apriori_position', {})
        sinex_position = apriori_result.get('sinex_value')
        if sinex_position is not None and len(sinex_position) == 3:
            position_str = ", ".join(str(v) for v in sinex_position)
            self.ui.aprioriPositionButton.setText(position_str)

        # Check if all validations passed
        all_valid = True
        has_validations = False
        for field in ['receiver_type', 'antenna_type', 'antenna_offset', 'apriori_position']:
            field_result = validation_results.get(field, {})
            if field_result.get('valid') is True:
                has_validations = True
            elif field_result.get('valid') is False:
                all_valid = False
                has_validations = True

        if has_validations:
            if all_valid:
                show_toast(self.ctrl.parent, "✅ SINEX validation passed", duration=3000)
            else:
                show_toast(self.ctrl.parent, "⚠️ SINEX validation warnings - check workflow", duration=5000)

    def _on_sinex_validation_error(self, error_msg: str):
        """
        UI handler: SINEX validation failed.

        Arguments:
          error_msg (str): Error message describing the failure.
        """
        # Don't show a toast for cancelled operations
        if "cancelled" in error_msg.lower():
            return

        # A skipped validation (e.g. the SINEX file does not exist for this date)
        # is expected behaviour, not a failure - inform the user and move on.
        if "skipped" in error_msg.lower():
            show_toast(self.ctrl.parent, "ℹ️ SINEX unavailable - validation skipped", duration=3000)
            return

        Logger.workflow(f"⚠️ SINEX validation error: {error_msg}")
        show_toast(self.ctrl.parent, f"⚠️ SINEX validation failed: {error_msg}", duration=5000)

    def _on_sinex_thread_finished(self):
        """
        Slot called when the SINEX thread has fully finished.
        Safe to clean up references here.
        """
        self._sinex_worker = None
        self._sinex_thread = None

    # endregion

    #region PPP Provider / Series / Project Combos

    def _on_ppp_provider_changed(self, provider_name: str):
        """
        UI handler: when PPP provider changes, refresh project and series options.
        Only shows series that have all required files (SP3, BIA, CLK).
        """
        if not provider_name or provider_name.strip() == "":
            return
        try:
            # Get valid series for this provider (only those with all required files)
            valid_series = get_valid_series_for_provider(self.ctrl.products_df, provider_name)

            if not valid_series:
                raise ValueError(f"No valid series (with all required files) for provider: {provider_name}")

            # Get DataFrame of valid (project, series) pairs - filter for valid series only
            df = self.ctrl.products_df.loc[
                (self.ctrl.products_df["analysis_center"] == provider_name) &
                (self.ctrl.products_df["solution_type"].isin(valid_series)),
                ["project", "solution_type"]]

            if df.empty:
                raise ValueError(f"No valid project–series combinations for provider: {provider_name}")

            # Store for future filtering if needed
            self.ctrl._valid_project_series_df = df
            self.ctrl._valid_series_for_provider = valid_series  # Cache valid series

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
            self.update_constellations_for_ppp_selection()

            # If we're on the Constellations tab, trigger BIA fetch for new selection
            if self.ui.configTabWidget.currentIndex() == 1:
                self.ctrl.constellations_tab.on_config_tab_changed(1)

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
        if not hasattr(self.ctrl, "_valid_project_series_df"):
            return

        df = self.ctrl._valid_project_series_df
        filtered_df = df[df["solution_type"] == selected_series]
        valid_projects = sorted(filtered_df["project"].unique())

        self.ui.pppProjectCombo.blockSignals(True)
        self.ui.pppProjectCombo.clear()
        self.ui.pppProjectCombo.addItems(valid_projects)
        self.ui.pppProjectCombo.setCurrentIndex(0)
        self.ui.pppProjectCombo.blockSignals(False)

        # Update constellations combobox based on new PPP selection
        self.update_constellations_for_ppp_selection()

        # If we are on the Constellations tab, trigger BIA fetch for new selection
        if self.ui.configTabWidget.currentIndex() == 1:
            self.ctrl.constellations_tab.on_config_tab_changed(1)

    def _on_ppp_project_changed(self, selected_project: str):
        """
        UI handler: when PPP project changes, filter valid series.
        Only displays series that have all required files (SP3, BIA, CLK).
        """
        if not hasattr(self.ctrl, "_valid_project_series_df"):
            return

        df = self.ctrl._valid_project_series_df
        filtered_df = df[df["project"] == selected_project]
        valid_series = sorted(filtered_df["solution_type"].unique())

        # Ensure only series with all required files are displayed
        if hasattr(self.ctrl, "_valid_series_for_provider"):
            valid_series = [s for s in valid_series if s in self.ctrl._valid_series_for_provider]

        self.ui.pppSeriesCombo.blockSignals(True)
        self.ui.pppSeriesCombo.clear()
        self.ui.pppSeriesCombo.addItems(valid_series)
        self.ui.pppSeriesCombo.setCurrentIndex(0)
        self.ui.pppSeriesCombo.blockSignals(False)

        # Update constellations combobox based on new PPP selection
        self.update_constellations_for_ppp_selection()

        Logger.workflow(f"✅ Filtered PPP series for project '{selected_project}': {valid_series}")

        # If we are on the Constellations tab, trigger BIA fetch for new selection
        if self.ui.configTabWidget.currentIndex() == 1:
            self.ctrl.constellations_tab.on_config_tab_changed(1)

    #endregion

    #region CDDIS Analysis Centre Scanning

    def start_analysis_centre_scan(self, start_epoch: datetime, end_epoch: datetime):
        """
        Start a background worker to scan the CDDIS archive for valid PPP analysis centres.

        Arguments:
          start_epoch (datetime): Start of the observation time window.
          end_epoch (datetime): End of the observation time window.
        """
        # Clean up any existing analysis centre threads before starting a new one
        self._cleanup_analysis_thread()

        self._worker = DownloadWorker(start_epoch=start_epoch, end_epoch=end_epoch, analysis_centers=True)
        self._metadata_thread = QThread()
        self._worker.moveToThread(self._metadata_thread)

        self._worker.finished.connect(self.on_cddis_ready)
        self._worker.finished.connect(self._restore_cursor)
        self._worker.cancelled.connect(self._on_cddis_cancelled)
        self._worker.cancelled.connect(self._restore_cursor)
        self._worker.constellation_info.connect(self._on_constellation_info_received)

        # Connect both finished and cancelled to thread quit
        self._worker.finished.connect(self._metadata_thread.quit)
        self._worker.cancelled.connect(self._metadata_thread.quit)
        self._metadata_thread.finished.connect(self._on_analysis_thread_finished)
        self._metadata_thread.started.connect(self._worker.run)
        self._metadata_thread.start()

    def on_cddis_ready(self, data: pd.DataFrame, log_messages: bool = True):
        """
        UI handler: receive PPP products DataFrame from worker and populate provider/project/series combos.

        Arguments:
          data (pd.DataFrame): Products dataframe from CDDIS scan.
          log_messages (bool): Whether to log success messages (False when clearing).
        """
        self.ctrl.products_df = data

        if data.empty:
            self.ctrl.valid_analysis_centers = []
            self.ui.pppProviderCombo.clear()
            self.ui.pppProviderCombo.addItem("None")
            self.ui.pppSeriesCombo.clear()
            self.ui.pppSeriesCombo.addItem("None")
            return

        self.ctrl.valid_analysis_centers = list(get_valid_analysis_centers(self.ctrl.products_df))

        if len(self.ctrl.valid_analysis_centers) == 0:
            self.ui.pppProviderCombo.clear()
            self.ui.pppProviderCombo.addItem("None")
            self.ui.pppSeriesCombo.clear()
            self.ui.pppSeriesCombo.addItem("None")
            return

        self.ui.pppProviderCombo.blockSignals(True)
        self.ui.pppProviderCombo.clear()
        self.ui.pppProviderCombo.addItems(self.ctrl.valid_analysis_centers)
        self.ui.pppProviderCombo.setCurrentIndex(0)

        # Update PPP series based on default PPP provider
        self.ui.pppProviderCombo.blockSignals(False)
        self.ctrl.try_enable_process_button()
        self._on_ppp_provider_changed(self.ctrl.valid_analysis_centers[0])
        if log_messages:
            Logger.workflow(
                f"✅ CDDIS archive scan complete. Found PPP product providers: {', '.join(self.ctrl.valid_analysis_centers)}")
            show_toast(self.ctrl.parent, f"✅ Found {len(self.ctrl.valid_analysis_centers)} PPP provider(s)", duration=3000)

    def _on_cddis_cancelled(self):
        """
        UI handler: handle cancellation of CDDIS worker.
        """
        Logger.workflow("📦 PPP provider scan was cancelled")

    def _on_cddis_error(self, msg):
        """
        UI handler: report CDDIS worker error to the UI.

        Arguments:
          msg (str): Error message from the worker.
        """
        Logger.workflow(f"Error loading CDDIS data: {msg}")
        self.ui.pppProviderCombo.clear()
        self.ui.pppProviderCombo.addItem("None")
        self.ctrl.parent.setCursor(Qt.CursorShape.ArrowCursor)
        show_toast(self.ctrl.parent, "⚠️ Failed to scan CDDIS archive", duration=4000)

    def _restore_cursor(self):
        """
        Restore the cursor to normal arrow after background operation completes.
        """
        self.ctrl.parent.setCursor(Qt.CursorShape.ArrowCursor)

    def _on_constellation_info_received(self, provider_constellations: dict):
        """
        UI handler: receive and store constellation information for each PPP provider/series/project.
        This is emitted by the DownloadWorker after fetching the SP3 headers.

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
        self.ctrl.provider_constellations = provider_constellations

        Logger.console("📡 Provider constellation information received")

        # Update constellations combobox based on current PPP selection
        self.update_constellations_for_ppp_selection()

        # If already on Constellations tab, trigger BIA fetch
        if self.ui.configTabWidget.currentIndex() == 1:
            self.ctrl.constellations_tab.on_config_tab_changed(1)

    def _cleanup_analysis_thread(self):
        """
        Request any running analysis centre threads to cancel.
        Moves the thread to _pending_threads list so it isn't destroyed while running.
        """
        if self._worker is not None:
            self._worker.stop()

        if self._metadata_thread is not None:
            if self._metadata_thread.isRunning():
                # Disconnect old signals to prevent callbacks to stale state
                try:
                    self._worker.finished.disconnect()
                    self._worker.cancelled.disconnect()
                except (TypeError, RuntimeError):
                    pass  # Already disconnected or object deleted
                try:
                    self._worker.constellation_info.disconnect()
                    self._worker.progress.disconnect()
                except (TypeError, RuntimeError):
                    pass  # Already disconnected or object deleted

                # Keep reference alive until thread actually finishes
                old_thread = self._metadata_thread

                def cleanup_old_thread():
                    if old_thread in self._pending_threads:
                        self._pending_threads.remove(old_thread)

                old_thread.finished.connect(cleanup_old_thread)
                self._pending_threads.append(old_thread)

            # Clear current references so new thread can be created
            self._worker = None
            self._metadata_thread = None

    def _on_analysis_thread_finished(self):
        """
        Slot called when the analysis thread has fully finished.
        Safe to clean up references here.
        """
        if self._metadata_thread is not None:
            if not self._metadata_thread.isRunning():
                self._worker = None
                self._metadata_thread = None

        # Also clean any finished pending threads
        self._pending_threads = [t for t in self._pending_threads if t.isRunning()]

    #endregion

    #region Thread Management

    def stop_all_workers(self):
        """
        Best-effort stop for all background workers managed by this controller.
        """
        try:
            if self._worker is not None:
                self._worker.stop()
        except Exception:
            pass

        try:
            self._stop_sinex_worker()
        except Exception:
            pass

    #endregion

    #region Reset to Defaults

    def reset_to_defaults(self):
        """
        Reset all General config tab fields to their default/placeholder states.
        """
        # Mode combo
        self.ui.modeCombo.clear()
        self.ui.modeCombo.addItem("Select one")
        self.ui.modeCombo.setCurrentIndex(0)

        # Constellations combo
        self.ui.constellationsCombo.clear()
        self.ui.constellationsCombo.setEditable(True)
        self.ui.constellationsCombo.lineEdit().clear()
        self.ui.constellationsCombo.lineEdit().setPlaceholderText("Select one or more")
        self.ui.constellationsValue.setText("Constellations")
        if hasattr(self.ui.constellationsCombo, '_constellation_model'):
            delattr(self.ui.constellationsCombo, '_constellation_model')
        if hasattr(self.ui.constellationsCombo, '_constellation_on_item_changed'):
            delattr(self.ui.constellationsCombo, '_constellation_on_item_changed')

        # Time window
        self.ui.timeWindowButton.setText("Start / End")
        self.ui.timeWindowValue.setText("Time Window")

        # Data interval
        self.ui.dataIntervalButton.setText("Interval (Seconds)")
        self.ui.dataIntervalValue.setText("Data interval")

        # Receiver type
        self.ui.receiverTypeCombo.clear()
        self.ui.receiverTypeCombo.addItem("Import text")
        self.ui.receiverTypeCombo.setCurrentIndex(0)
        if self.ui.receiverTypeCombo.lineEdit():
            self.ui.receiverTypeCombo.lineEdit().setText("Import text")
        self.ui.receiverTypeValue.setText("Receiver Type")

        # Antenna type
        self.ui.antennaTypeCombo.clear()
        self.ui.antennaTypeCombo.addItem("Import text")
        self.ui.antennaTypeCombo.setCurrentIndex(0)
        if self.ui.antennaTypeCombo.lineEdit():
            self.ui.antennaTypeCombo.lineEdit().setText("Import text")
        self.ui.antennaTypeValue.setText("")

        # Antenna offset
        self.ui.antennaOffsetButton.setText("0.0, 0.0, 0.0")
        self.ui.antennaOffsetValue.setText("0.0, 0.0, 0.0")

        # Apriori position
        self.ui.aprioriPositionButton.setText("0.0, 0.0, 0.0")

        # PPP combos
        self.ui.pppProviderCombo.clear()
        self.ui.pppProviderCombo.addItem("Select one")
        self.ui.pppProviderCombo.setCurrentIndex(0)
        self.ui.pppSeriesCombo.clear()
        self.ui.pppSeriesCombo.addItem("Select one")
        self.ui.pppSeriesCombo.setCurrentIndex(0)
        self.ui.pppProjectCombo.clear()
        self.ui.pppProjectCombo.addItem("Select one")
        self.ui.pppProjectCombo.setCurrentIndex(0)

    #endregion

    #region Combo Plumbing Helpers

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

    def _bind_multiselect_combo(self, combo: QComboBox, items_func: Callable[[], List[str]], mirror_label, placeholder: str):
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

    #endregion