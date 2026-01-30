# app/controllers/visualisation_controller.py
"""Controller responsible for everything inside the visualisation panel.

Responsibilities
----------------
1. Embed one of the generated HTML files into the QTextEdit area.
2. Maintain a list (indexed) of available HTML visualisations.
3. Provide a double-click handler and an explicit *Open* action that open the
   current html in the user's default browser.

NOTE:  UI widgets for selecting visualisation (e.g. a ComboBox or QListWidget)
       and an *Open* button are **not** yet present in the .ui file.  This
       controller exposes stub `bind_open_button()` / `bind_selector()` helpers
       which can be called once those widgets are added.
"""
from __future__ import annotations
import os
import platform
import subprocess
import sys
from pathlib import Path
from typing import List, Sequence, Optional
from PySide6.QtCore import QRect, QUrl, QObject, QEvent
from PySide6.QtGui import QDesktopServices
from PySide6.QtWidgets import QTextEdit, QPushButton, QComboBox, QApplication
from PySide6.QtWebEngineWidgets import QWebEngineView
from scripts.GinanUI.app.utils.logger import Logger

HERE = Path(__file__).resolve()
ROOT = HERE.parents[2]
DEFAULT_OUT_DIR = ROOT / "tests" / "resources" / "outputData" / "visual"


class VisualisationController(QObject):
    """
    Manage interactions and rendering inside the visualisation panel.

    Arguments:
      ui (object): The main window UI object that exposes the visualisation widgets (e.g., `visualisationTextEdit`).
      parent_window (QObject): The parent window/controller used as the QObject parent.

    Returns:
      None: Constructor returns nothing.

    Example:
      Function itself returns None; example shows how to instantiate and inspect state.
      >>> controller = VisualisationController(ui, parent_window)
      >>> controller.html_files
      []
    """

    def __init__(self, ui, parent_window):
        """
        Initialize controller state and install required event filters.

        Arguments:
          ui: The main window UI instance.
          parent_window: The parent QMainWindow or controller.

        Example:
          Function itself returns None; example shows initial empty html_files.
          >>> ctrl = VisualisationController(ui, parent_window)
          >>> ctrl.html_files
          []
        """
        super().__init__(parent_window)
        self.ui = ui  # Ui_MainWindow instance
        self.parent = parent_window
        self.html_files: List[str] = []  # paths of available visualisations
        self.current_index: Optional[int] = None
        self.external_base_url: Optional[str] = None
        self._selector: Optional[QComboBox] = None
        self._open_button: Optional[QPushButton] = None

    # ---------------------------------------------------------------------
    # Public API (to be called from MainWindow / other controllers)
    # ---------------------------------------------------------------------
    def set_html_files(self, paths: Sequence[str]):
        """
        Register available HTML visualisation files and display the first one.

        Arguments:
          paths (Sequence[str]): List of file paths to HTML visualisations.

        Example:
          Function itself returns None; example shows state update after call.
          >>> controller.set_html_files(["plot1.html", "plot2.html"])
          >>> controller.current_index
          0
        """
        self.html_files = list(dict.fromkeys(paths))
        # Refresh selector if bound
        if self._selector:
            self._refresh_selector()
        if self.html_files:
            self.display_html(0)
            # Enable widgets once we have plots
            if self._selector:
                self._selector.setEnabled(True)
            if self._open_button:
                self._open_button.setEnabled(True)
        else:
            # Disable widgets if no plots available
            if self._selector:
                self._selector.setEnabled(False)
            if self._open_button:
                self._open_button.setEnabled(False)

    def display_html(self, index: int):
        """
        Embed the HTML file at the given index into the visualisation panel.

        Arguments:
          index (int): Zero-based index into `self.html_files`.

        Example:
          Function itself returns None; example shows updated index.
          >>> controller.display_html(0)
          >>> controller.current_index
          0
        """
        if not isinstance(index, int) or not (0 <= index < len(self.html_files)):
            return
        file_path = self.html_files[index]
        self.current_index = index
        self._embed_html(file_path)

    def open_current_external(self):
        """
        Open the currently displayed HTML in the system’s default web browser.

        Example:
          Function itself returns None; example shows that return value is None.
          >>> controller.open_current_external() is None
          True
        """
        if self.current_index is None:
            return
        path = self.html_files[self.current_index]
        try:
            url = QUrl.fromLocalFile(Path(path).resolve())

            # Open the file with the appropriate method for the operating system
            if platform.system() == "Windows":
                # sys._MEIPASS and some dll file need to be changed
                QDesktopServices.openUrl(url)

            elif platform.system() == "Darwin":
                # sys._MEIPASS but might also work without any changes
                QDesktopServices.openUrl(url)

            else:
                # When compiled with pyinstaller, LD_LIBRARY_PATH is modified which prevents external app opening
                env = os.environ.copy()
                original = env.get("LD_LIBRARY_PATH_ORIG")
                if original:
                    env["LD_LIBRARY_PATH"] = original  # Restore original value
                else:
                    env.pop("LD_LIBRARY_PATH", None)  # Clear the value to use sys defaults
                subprocess.run(["xdg-open", url.url()], env=env)
        except Exception as e:
            Logger.console(f"Error occurred trying to open in browser: {e}")

    # ------------------------------------------------------------------
    # Helpers for wiring additional UI elements
    # ------------------------------------------------------------------
    def bind_open_button(self, button: QPushButton):
        """
        Connect an *Open* button to open the current visualisation externally.

        Arguments:
          button (QPushButton): The push button to connect to the handler.

        Example:
          Function itself returns None; example shows valid binding.
          >>> controller.bind_open_button(ui.openButton) is None
          True
        """
        self._open_button = button
        button.clicked.connect(self.open_current_external)
        button.setEnabled(False)

    def bind_selector(self, combo: QComboBox):
        """
        Bind a QComboBox selector to manage and display HTML visualisations.

        Arguments:
          combo (QComboBox): The combo box used as selector.

        Example:
          Function itself returns None; example shows valid selector binding.
          >>> controller.bind_selector(ui.comboBox) is None
          True
        """
        self._selector = combo

        def safe_display():
            data = combo.currentData()
            if isinstance(data, int):  # Only proceed if it's a valid index
                self.display_html(data)

        combo.currentIndexChanged.connect(lambda _: safe_display())
        combo.setEnabled(False)
        self._refresh_selector()

    def _refresh_selector(self):
        """
        Populate the selector combo box with available HTML files.

        Example:
          # Function itself returns None; example shows refresh success.
          >>> controller._refresh_selector() is None
          True
        """
        if not self._selector:
            return
        self._selector.clear()
        for idx, path in enumerate(self.html_files):
            self._selector.addItem(f"#{idx} – {os.path.basename(path)}", userData=idx)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _embed_html(self, file_path: str):
        """
        Embed an HTML file inside the dedicated QWebEngineView in the UI.

        Arguments:
          file_path (str): Absolute or relative path to a local HTML file to display.
        """
        # Use the QWebEngineView that is defined in main_window.ui
        webview: QWebEngineView = self.ui.webEngineView

        # Resolve to an absolute path so QWebEngineView can load it reliably
        url = QUrl.fromLocalFile(str(Path(file_path).resolve()))
        webview.setUrl(url)

        # Optional zoom factor
        webview.setZoomFactor(0.8)

        # Install event filter if you still want to intercept events later
        webview.installEventFilter(self)

        # Keep a reference to avoid GC (and for later access)
        self._webview = webview

    # ------------------------------------------------------------------
    # Optional configuration
    # ------------------------------------------------------------------
    def set_external_base_url(self, url: str):
        """
        Set a base HTTP URL to prefer when opening visualisations externally.

        Arguments:
          url (str): Base URL (a trailing slash is appended if missing).

        Example:
          Function itself returns None; example shows URL assignment.
          >>> controller.set_external_base_url("http://localhost:8000/")
          >>> controller.external_base_url
          'http://localhost:8000/'
        """
        if not url.endswith('/'):
            url += '/'
        self.external_base_url = url

    def build_from_execution(self):
        """
        Generate visualisation HTML files from the execution model and load them.

        Example:
          Function itself returns None; example checks that call succeeds.
          >>> controller.build_from_execution() is None
          True
        """
        try:
            exec_obj = getattr(self.parent, "execution", None)
            if exec_obj is None:
                from PySide6.QtWidgets import QMessageBox
                QMessageBox.warning(self.ui, "Plot", "execution object is not set")
                return

            new_html_paths = exec_obj.build_pos_plots()  # default output to tests/resources/outputData/visual

            # Only use newly generated plots, not old ones from previous runs
            new_html_paths.sort(key=lambda x: os.path.basename(x))

            self.set_html_files(new_html_paths)

        except Exception as e:
            from PySide6.QtWidgets import QMessageBox
            QMessageBox.critical(self.ui, "Plot Error", str(e))

    def _find_existing_html_files(self):
        """
        Locate and return paths of existing visualisation HTML files.

        Returns:
          list[str]: A list of absolute paths to discovered HTML files.git

        Example:
          Function returns a list; example checks returned type.
          >>> isinstance(controller._find_existing_html_files(), list)
          True
        """
        existing_files = []

        default_visual_dir = DEFAULT_OUT_DIR
        if default_visual_dir.exists():
            for html_file in default_visual_dir.glob("*.html"):
                existing_files.append(str(html_file))

        if self.external_base_url:
            pass

        return existing_files