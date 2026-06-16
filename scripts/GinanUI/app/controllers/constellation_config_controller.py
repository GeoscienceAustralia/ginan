"""
Controller for the Constellations configuration tab.

Manages the following UI widgets and background workflows:
  - Per-constellation observation code QListWidgets (GPS, GAL, GLO, BDS, QZS)
  - Per-constellation labels
  - Placeholder / explanation / BIA warning / BIA loading status labels
  - BIA code priority fetching (background worker) and code validation styling
"""

from __future__ import annotations
from typing import List
from PySide6.QtCore import QObject, Qt, QThread
from PySide6.QtGui import QColor, QBrush
from PySide6.QtWidgets import (
    QLabel,
    QListWidget,
    QListWidgetItem,
    QAbstractItemView,
    QSizePolicy,
    QWidget,
    QVBoxLayout,
)
from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.utils.workers import BiasProductWorker
from scripts.GinanUI.app.utils.toast import show_toast


class ConstellationConfigController(QObject):
    """
    Manages the Constellations configuration tab: observation code list widgets,
    BIA code priority validation, and placeholder/status labels.

    Arguments:
      ui: The main window UI instance.
      input_ctrl: The parent InputController instance (for accessing shared state).
    """

    def __init__(self, ui, input_ctrl):
        """
        Initialise constellation tab bindings and state.

        Arguments:
          ui: The main window UI instance.
          input_ctrl: The parent InputController that owns shared state.
        """
        super().__init__(parent=input_ctrl)
        self.ui = ui
        self.ctrl = input_ctrl  # parent InputController

        # BIA worker tracking
        self._bia_loading = False
        self._bia_worker = None
        self._bia_thread = None
        self._bia_current_provider = None
        self._bia_current_series = None
        self._bia_current_project = None

        # Setup placeholder and status labels
        self._setup_placeholder()
        self._hide_all_widgets()

        # Connect tab change signal to trigger BIA fetch when switching to Constellations tab
        self.ui.configTabWidget.currentChanged.connect(self.on_config_tab_changed)

    #region UI Tooltips

    def setup_tooltips(self):
        """
        Set up tooltips for all constellation list widgets.
        """
        tooltip_mapping = {
            'gpsListWidget': "GPS observation codes",
            'galListWidget': "Galileo observation codes",
            'gloListWidget': "GLONASS observation codes",
            'bdsListWidget': "BeiDou observation codes",
            'qzsListWidget': "QZSS observation codes",
        }
        for widget_name, label in tooltip_mapping.items():
            if hasattr(self.ui, widget_name):
                getattr(self.ui, widget_name).setToolTip(
                    f"{label}\n"
                    "✓ Check / uncheck to enable / disable codes\n"
                    "↕ Drag and drop to set priority order (top = highest priority)"
                )

    #endregion

    #region Status Labels

    def _setup_placeholder(self):
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
        if hasattr(self.ui, 'constellationsGridLayout'):
            self.ui.constellationsGridLayout.addWidget(self._constellation_status_container, 0, 0)

    def _hide_all_widgets(self):
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
                getattr(self.ui, widget_name).setVisible(False)

    def _update_placeholder(self, show_placeholder: bool):
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

    #endregion

    #region Populate Observation Codes from RINEX

    def populate_observation_codes(self, result: dict):
        """
        Populate the observation code list widgets with available codes from RINEX.

        Arguments:
          result (dict): Dictionary containing observation code lists for each constellation.
        """
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
                self._setup_list_widget(list_widget, codes, enabled_codes)
                populated_constellations.append(const_name)
            else:
                list_widget.clear()
                list_widget.setEnabled(False)

        if populated_constellations:
            Logger.workflow(f"✅ Populated observation codes for {', '.join(populated_constellations)}")
        else:
            Logger.workflow("⚠️ No observation codes found in RINEX")

    def _setup_list_widget(self, list_widget: QListWidget, codes: List[str], enabled_codes: set):
        """
        Set up a list widget with drag-drop reordering and checkboxes for observation codes.

        Arguments:
          list_widget (QListWidget): The list widget to set up.
          codes (List[str]): List of observation codes to populate (in priority order).
          enabled_codes (set): Set of codes that should be checked by default.
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

    def extract_observation_codes(self) -> dict:
        """
        Extract selected observation codes from all constellation list widgets in priority order.

        Returns:
          dict: Dictionary mapping constellation names to lists of selected codes in order.
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

    #endregion

    #region Visibility of List Widgets

    def sync_list_widgets_to_selection(self):
        """
        Show / hide constellation list widgets and labels based on the "General" tab's
        constellation multi-select. Called when constellation selection changes.
        Shows a placeholder message when no constellations are selected.
        """
        selected_constellations = self.ctrl.general_tab.get_selected_constellation_set()

        widget_mapping = {
            'GPS': ('gpsLabel', 'gpsListWidget'),
            'GAL': ('galLabel', 'galListWidget'),
            'GLO': ('gloLabel', 'gloListWidget'),
            'BDS': ('bdsLabel', 'bdsListWidget'),
            'QZS': ('qzsLabel', 'qzsListWidget'),
        }

        for const_name, (label_name, list_widget_name) in widget_mapping.items():
            is_enabled = const_name in selected_constellations

            if hasattr(self.ui, label_name):
                getattr(self.ui, label_name).setVisible(is_enabled)
            if hasattr(self.ui, list_widget_name):
                getattr(self.ui, list_widget_name).setVisible(is_enabled)

        self._update_placeholder(len(selected_constellations) == 0)

    #endregion

    #region BIA Code Priority Fetching and Validation

    def on_config_tab_changed(self, index: int):
        """
        UI handler: triggered when the config tab widget changes tabs.
        When switching to the Constellations tab (index 1), fetch .BIA code priorities
        for the current PPP selection if not already cached.

        Arguments:
          index (int): The index of the newly selected tab.
        """
        if index != 1:
            return

        provider = self.ui.pppProviderCombo.currentText()
        series = self.ui.pppSeriesCombo.currentText()
        project = self.ui.pppProjectCombo.currentText()

        # Guard: Skip if any combo is empty or has placeholder values
        if not provider or not series or not project:
            return
        if provider in ("", "None", "Select one") or series in ("", "None", "Select one") or project in ("", "None", "Select one"):
            return

        # Guard: Skip if products_df is empty (happens during RINEX file change)
        if self.ctrl.products_df.empty:
            return

        # Check if we already have cached BIA data for this combination
        if self._is_bia_cached(provider, series, project):
            self._validate_codes_against_bia()
            return

        # Check if we are already loading the same combination
        if self._bia_loading:
            if (self._bia_current_provider != provider or
                self._bia_current_series != series or
                self._bia_current_project != project):
                Logger.console(f"🔄 BIA fetch interrupted - switching to {provider}/{series}/{project}")
            else:
                return

        # Start BIA fetch (will stop any existing worker first)
        self._fetch_bia_code_priorities(provider, series, project)

    def _is_bia_cached(self, provider: str, series: str, project: str) -> bool:
        """
        Check if BIA code priorities are cached for the given combination.

        Arguments:
          provider (str): Analysis centre code.
          series (str): Solution type code.
          project (str): Project code.

        Returns:
          bool: True if cached, False otherwise.
        """
        try:
            return (provider in self.ctrl.bia_code_priorities and
                    series in self.ctrl.bia_code_priorities[provider] and
                    project in self.ctrl.bia_code_priorities[provider][series])
        except (KeyError, TypeError):
            return False

    def _fetch_bia_code_priorities(self, provider: str, series: str, project: str):
        """
        Start background worker to fetch and parse BIA file for code priorities.

        Arguments:
          provider (str): Analysis centre code.
          series (str): Solution type code.
          project (str): Project code.
        """
        # Safety guard: don't start worker with invalid parameters
        if not provider or not series or not project:
            Logger.console(f"⚠️ BIA fetch skipped: invalid parameters provider='{provider}' series='{series}' project='{project}'")
            return
        if provider in ("", "None", "Select one") or series in ("", "None", "Select one") or project in ("", "None", "Select one"):
            Logger.console(f"⚠️ BIA fetch skipped: placeholder values in parameters")
            return
        if self.ctrl.products_df.empty:
            Logger.console(f"⚠️ BIA fetch skipped: products_df is empty")
            return

        # Stop any existing BIAProductWorker before starting a new one
        self.stop_bia_worker()

        self._bia_loading = True
        self._show_bia_loading_indicator(True)

        # Create worker and thread
        self._bia_thread = QThread()
        self._bia_worker = BiasProductWorker(self.ctrl.products_df, provider, series, project)
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

        self._bia_thread.start()

    def stop_bia_worker(self):
        """
        Stop any running BIA worker and clean up thread resources.
        """
        if self._bia_worker is not None:
            self._bia_worker.stop()
            try:
                self._bia_worker.finished.disconnect()
                self._bia_worker.error.disconnect()
                self._bia_worker.progress.disconnect()
            except (RuntimeError, TypeError):
                pass

        if self._bia_thread is not None:
            try:
                self._bia_thread.started.disconnect()
                self._bia_thread.finished.disconnect()
            except (RuntimeError, TypeError):
                pass

            if self._bia_thread.isRunning():
                self._bia_thread.quit()
                if not self._bia_thread.wait(2000):
                    Logger.console("⚠️ BIA thread did not stop gracefully, forcing termination")
                    self._bia_thread.terminate()
                    self._bia_thread.wait(1000)

        self._bia_worker = None
        self._bia_thread = None
        self._bia_loading = False

    def _on_bia_progress(self, description: str, percent: int):
        """
        UI handler: update progress during BIA fetch.

        Arguments:
          description (str): Progress description.
          percent (int): Progress percentage (-1 for indeterminate).
        """
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
        self._show_bia_warning(False)

        # Cache the results
        provider = self._bia_current_provider
        series = self._bia_current_series
        project = self._bia_current_project

        if provider not in self.ctrl.bia_code_priorities:
            self.ctrl.bia_code_priorities[provider] = {}
        if series not in self.ctrl.bia_code_priorities[provider]:
            self.ctrl.bia_code_priorities[provider][series] = {}
        self.ctrl.bia_code_priorities[provider][series][project] = code_priorities

        Logger.workflow(f"✅ BIA code priorities cached for {provider}/{series}/{project}")
        self._validate_codes_against_bia()

    def _on_bia_error(self, error_msg: str):
        """
        UI handler: BIA fetch failed.

        Arguments:
          error_msg (str): Error message describing the failure.
        """
        self._bia_loading = False
        self._show_bia_loading_indicator(False)

        Logger.console(f"⚠️ BIA fetch error: {error_msg}")

        # Don't show warnings for cancelled fetches (user-initiated)
        if "cancelled" in error_msg.lower():
            return

        self._mark_all_codes_invalid()
        self._show_bia_warning(True)
        Logger.workflow(f"⚠️ Failed to fetch BIA file for selected PPP products - unable to validate codes")
        show_toast(self.ctrl.parent, f"⚠️ Could not fetch BIA data: {error_msg}", duration=3000)

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
          show (bool): True to show, False to hide.
        """
        if not hasattr(self, '_bia_loading_label') or self._bia_loading_label is None:
            return
        if show:
            self._bia_loading_label.setText("⏳ Loading code priorities from .BIA file...")
        self._bia_loading_label.setVisible(show)

    def _show_bia_warning(self, show: bool):
        """
        Show or hide the BIA warning label on the Constellations tab.

        Arguments:
          show (bool): True to show warning, False to hide it.
        """
        if hasattr(self, '_bia_warning_label'):
            self._bia_warning_label.setVisible(show)

    #endregion

    #region Code Frequency Validation Styling

    def _validate_codes_against_bia(self):
        """
        Validate the codes in each constellation list widget against the cached BIA codes.
        Codes that are NOT in the .BIA file are marked with strikethrough and a different colour.
        """
        provider = self.ui.pppProviderCombo.currentText()
        series = self.ui.pppSeriesCombo.currentText()
        project = self.ui.pppProjectCombo.currentText()

        bia_codes = None
        try:
            bia_codes = self.ctrl.bia_code_priorities.get(provider, {}).get(series, {}).get(project, None)
        except (KeyError, TypeError, AttributeError):
            pass

        if not bia_codes:
            self.reset_list_styling()
            return

        widget_mapping = {
            'gpsListWidget': 'GPS',
            'galListWidget': 'GAL',
            'gloListWidget': 'GLO',
            'bdsListWidget': 'BDS',
            'qzsListWidget': 'QZS',
        }

        # Colours for codes
        valid_color = QColor('white')
        invalid_color = QColor('#FF6B6B')

        for widget_name, constellation in widget_mapping.items():
            if not hasattr(self.ui, widget_name):
                continue

            list_widget = getattr(self.ui, widget_name)
            constellation_bia_codes = bia_codes.get(constellation, set())

            for i in range(list_widget.count()):
                item = list_widget.item(i)
                if item is None:
                    continue

                code = item.text().strip()
                font = item.font()

                if code in constellation_bia_codes:
                    font.setStrikeOut(False)
                    item.setFont(font)
                    item.setForeground(QBrush(valid_color))
                else:
                    font.setStrikeOut(True)
                    item.setFont(font)
                    item.setForeground(QBrush(invalid_color))

        Logger.workflow(f"✅ Validated constellation codes against BIA for {provider}/{series}/{project}")

    def reset_list_styling(self):
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

    #endregion

    #region Reset to Defaults

    def reset_to_defaults(self):
        """
        Reset all Constellations tab fields to their default/initial states.
        """
        list_widgets = ['gpsListWidget', 'galListWidget', 'gloListWidget', 'bdsListWidget', 'qzsListWidget']
        for widget_name in list_widgets:
            if hasattr(self.ui, widget_name):
                list_widget = getattr(self.ui, widget_name)
                list_widget.clear()
                list_widget.setEnabled(False)

        self._hide_all_widgets()
        self._update_placeholder(True)

    #endregion