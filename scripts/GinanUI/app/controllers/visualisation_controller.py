"""
Controller for the visualisation panel.

Manages the following UI widgets and behaviours:
  - Embedding generated HTML visualisation files into a QWebEngineView
  - Maintaining an indexed list of available HTML visualisations
  - Opening the current visualisation in the system's default browser or in an attached dialog widget
  - Binding a QComboBox selector and Open button for plot navigation
"""

from __future__ import annotations
import os
import platform
import subprocess
from pathlib import Path
from typing import List, Sequence, Optional
from PySide6.QtCore import QUrl, QObject, Qt
from PySide6.QtGui import QDesktopServices
from PySide6.QtWidgets import QPushButton, QComboBox
from PySide6.QtWebEngineWidgets import QWebEngineView
from scripts.GinanUI.app.utils.logger import Logger

HERE = Path(__file__).resolve()
ROOT = HERE.parents[2]
DEFAULT_OUT_DIR = ROOT / "tests" / "resources" / "outputData" / "visual"


class VisualisationController(QObject):
    """
    Manages interactions and rendering inside the visualisation panel.

    Arguments:
      ui: The main window UI object that exposes the visualisation widgets.
      parent_window: The parent window/controller used as the QObject parent.
    """

    def __init__(self, ui, parent_window):
        """
        Initialise controller state and install required event filters.

        Arguments:
          ui: The main window UI instance.
          parent_window: The parent QMainWindow or controller.
        """
        super().__init__(parent_window)
        self.ui = ui  # Ui_MainWindow instance
        self.parent = parent_window
        self.html_files: List[str] = []  # paths of available visualisations
        self.current_index: Optional[int] = None
        self.external_base_url: Optional[str] = None
        self._selector: Optional[QComboBox] = None
        self._open_button: Optional[QPushButton] = None
        self._enlarge_button: Optional[QPushButton] = None

        self.setup_tooltips()

    # region UI Tooltips

    def setup_tooltips(self):
        """
        Set up tooltips for all Visualisation panel widgets.
        """
        self.ui.enlargeButton.setToolTip("Enlarge the plot visualisation to a pop-out window")
        self.ui.openInBrowserButton.setToolTip("Open the plot visualisation in your system's default web browser")
        self.ui.visualisationSelectorCombo.setToolTip(
            "Set the active plot visualisation being displayed\n"
            "This list is automatically generated according to the files PEA outputted in its processing"
        )

    # endregion

    #region Plotting

    def build_from_execution(self):
        """
        Generate visualisation HTML files from the execution model and load them.
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

    #endregion

    #region HTML Viewing

    def set_html_files(self, paths: Sequence[str]):
        """
        Register available HTML visualisation files and display the first one.

        Arguments:
          paths (Sequence[str]): List of file paths to HTML visualisations.
        """
        self.html_files = list(dict.fromkeys(paths))
        # Refresh selector if bound
        if self._selector:
            self._refresh_selector()
        if self.html_files:
            self.display_html(0)
            # Show widgets once we have plots
            if self._selector:
                self._selector.setVisible(True)
            if self._open_button:
                self._open_button.setVisible(True)
            if self._enlarge_button:
                self._enlarge_button.setVisible(True)
        else:
            # Hide widgets if no plots available
            if self._selector:
                self._selector.setVisible(False)
            if self._open_button:
                self._open_button.setVisible(False)
            if self._enlarge_button:
                self._enlarge_button.setVisible(False)

    def display_html(self, index: int):
        """
        Embed the HTML file at the given index into the visualisation panel.

        Arguments:
          index (int): Zero-based index into self.html_files.
        """
        if not isinstance(index, int) or not (0 <= index < len(self.html_files)):
            return
        file_path = self.html_files[index]
        self.current_index = index
        self._embed_html(file_path)

    def open_current_external(self):
        """
        Open the currently displayed HTML in the system's default web browser.
        """
        if self.current_index is None:
            return
        path = self.html_files[self.current_index]
        try:
            url = QUrl.fromLocalFile(str(Path(path).resolve()))

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

    def open_current_enlarged(self):
        """
        Open the currently displayed HTML in a resizable embedded browser window
        """
        if self.current_index is None:
            return
        path = self.html_files[self.current_index]
        url = QUrl.fromLocalFile(str(Path(path).resolve()))
        self._open_enlarged_window(url)

    def _open_enlarged_window(self, url: QUrl):
        """
        Create and display a resizable window containing a QWebEngineView

        Arguments:
          url (QUrl): The local file URL of the HTML visualisation to display
        """
        from PySide6.QtWidgets import QDialog, QVBoxLayout

        dialog = QDialog(self.parent)
        dialog.setWindowTitle("Visualisation")
        dialog.setWindowFlags(
            Qt.WindowType.Window |
            Qt.WindowType.WindowMinimizeButtonHint |
            Qt.WindowType.WindowMaximizeButtonHint |
            Qt.WindowType.WindowCloseButtonHint
        )
        dialog.resize(1200, 800)
        dialog.setMinimumSize(600, 400)

        layout = QVBoxLayout(dialog)
        layout.setContentsMargins(0, 0, 0, 0)

        webview = QWebEngineView(dialog)
        webview.setUrl(url)
        layout.addWidget(webview)

        dialog.show()

        # Keep a reference to prevent garbage collection before the dialog closes
        self._enlarged_dialogs = getattr(self, "_enlarged_dialogs", [])
        self._enlarged_dialogs.append(dialog)
        dialog.finished.connect(lambda: self._enlarged_dialogs.remove(dialog))

    #endregion

    #region Widget Binding

    def bind_open_button(self, button: QPushButton):
        """
        Connect an Open button to open the current visualisation externally.

        Arguments:
          button (QPushButton): The push button to connect to the handler.
        """
        self._open_button = button
        button.clicked.connect(self.open_current_external)
        button.setVisible(False)

    def bind_selector(self, combo: QComboBox):
        """
        Bind a QComboBox selector to manage and display HTML visualisations.

        Arguments:
          combo (QComboBox): The combo box used as selector.
        """
        self._selector = combo

        def safe_display():
            data = combo.currentData()
            if isinstance(data, int):  # Only proceed if it's a valid index
                self.display_html(data)

        combo.currentIndexChanged.connect(lambda _: safe_display())
        combo.setVisible(False)
        self._refresh_selector()

    def bind_enlarge_button(self, button: QPushButton):
        """
        Connect an Enlarge button to open the current visualisation in a new window

        Arguments:
          button (QPushButton): The push button to connect to the handler
        """
        self._enlarge_button = button
        button.clicked.connect(self.open_current_enlarged)
        button.setVisible(False)

    #endregion

    #region Configuration

    def set_external_base_url(self, url: str):
        """
        Set a base HTTP URL to prefer when opening visualisations externally.

        Arguments:
          url (str): Base URL (a trailing slash is appended if missing).
        """
        if not url.endswith('/'):
            url += '/'
        self.external_base_url = url

    #endregion

    #region Helper Functions

    def _refresh_selector(self):
        """
        Populate the selector combo box with available HTML files.
        """
        if not self._selector:
            return
        self._selector.clear()
        for idx, path in enumerate(self.html_files):
            self._selector.addItem(f"#{idx} — {os.path.basename(path)}", userData=idx)

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

    def _find_existing_html_files(self):
        """
        Locate and return paths of existing visualisation HTML files.

        Returns:
          list[str]: A list of absolute paths to discovered HTML files.
        """
        existing_files = []

        default_visual_dir = DEFAULT_OUT_DIR
        if default_visual_dir.exists():
            for html_file in default_visual_dir.glob("*.html"):
                existing_files.append(str(html_file))

        if self.external_base_url:
            pass

        return existing_files

    #endregion