from pathlib import Path
from typing import Optional

from PySide6.QtCore import QUrl, Signal, QThread, Slot, Qt, QRegularExpression, QCoreApplication
from scripts.GinanUI.app.utils.logger import Logger
from PySide6.QtWidgets import QMainWindow, QDialog, QVBoxLayout, QPushButton, QComboBox, QMessageBox
from PySide6.QtWebEngineWidgets import QWebEngineView
from PySide6.QtGui import QTextCursor, QTextDocument

from scripts.GinanUI.app.utils.cddis_credentials import validate_netrc as gui_validate_netrc
from scripts.GinanUI.app.models.execution import Execution
from scripts.GinanUI.app.utils.toast import show_toast
from scripts.GinanUI.app.utils.ui_compilation import compile_ui
from scripts.GinanUI.app.controllers.input_controller import InputController
from scripts.GinanUI.app.controllers.visualisation_controller import VisualisationController
from scripts.GinanUI.app.utils.cddis_email import get_username_from_netrc, write_email, test_cddis_connection
from scripts.GinanUI.app.utils.workers import PeaExecutionWorker, DownloadWorker
from scripts.GinanUI.app.models.archive_manager import archive_products_if_selection_changed, archive_products, archive_old_outputs
from scripts.GinanUI.app.models.execution import INPUT_PRODUCTS_PATH

# Optional toggle for development visualization testing
test_visualisation = False


def setup_main_window():
    import sys
    IS_FROZEN = getattr(sys, 'frozen', False)

    if not IS_FROZEN:
        # Only compile UI during development
        compile_ui()

    from scripts.GinanUI.app.views.main_window_ui import Ui_MainWindow
    return Ui_MainWindow()

class MainWindow(QMainWindow):
    log_signal = Signal(str)

    def __init__(self):
        super().__init__()

        # Setup UI
        self.ui = setup_main_window()
        self.ui.setupUi(self)

        # Add rounded corners to UI elements
        self.setStyleSheet("""
            QPushButton {
                border-radius: 4px;
            }
            QPushButton:disabled {
                border-radius: 4px;
            }
            QTextEdit {
                border-radius: 4px;
            }
            QComboBox {
                border-radius: 4px;
            }
            QMessageBox QPushButton {
                border-radius: 4px;
                background-color: #2c5d7c;
                color: white;
                padding: 6px 16px;
                min-width: 60px;
            }
            QMessageBox QPushButton:hover {
                background-color: #214861;
            }
            QMessageBox QPushButton:pressed {
                background-color: #1a3649;
            }
        """)

        # Fix macOS tab widget styling
        self._fix_macos_tab_styling()

        # Initialize the Logger system for easy logging throughout the app
        Logger.initialise(self)

        # Controllers
        self.execution = Execution()
        self.inputCtrl = InputController(self.ui, self, self.execution)
        self.visCtrl = VisualisationController(self.ui, self)

        # Connect ready signals
        self.inputCtrl.ready.connect(self.on_files_ready)
        self.inputCtrl.pea_ready.connect(self._on_process_clicked)

        # State
        self.rnx_file: Optional[str] = None
        self.output_dir: Optional[str] = None
        self.download_progress: dict[str, int] = {}  # track per-file progress
        self.is_processing = False
        self.atx_required_for_rnx_extraction = False # File required to extract info from RINEX
        self.metadata_downloaded = False
        self.offline_mode = False  # Track if running without internet
        self._pending_threads = []  # Track threads pending cleanup

        # Visualisation widgets

        self.visCtrl.bind_open_button(self.ui.openInBrowserButton)

        self.visCtrl.bind_selector(self.ui.visualisationSelectorCombo)

        archive_products(INPUT_PRODUCTS_PATH, "startup_archival", True)

        # Validate connection then start metadata download in a separate thread
        self._validate_cddis_credentials_once()

        # Only start metadata download if we have internet connection
        if not self.offline_mode:
            self.metadata_worker = DownloadWorker()
            self.metadata_thread, _ = self._setup_worker_thread(
                self.metadata_worker,
                self._on_metadata_download_finished,
                self._on_download_progress,
                thread_attr='metadata_thread',
                worker_attr='metadata_worker'
            )

            self.metadata_thread.setObjectName("MetadataDownloadWorker")

            # Connect additional signal specific to metadata download
            self.metadata_worker.atx_downloaded.connect(self._on_atx_downloaded)
            self.metadata_thread.start()
        else:
            Logger.terminal("⚠️ Skipping metadata download - running in offline mode")

        # Added: wire an optional stop-all button if present in the UI
        if hasattr(self.ui, "stopAllButton") and self.ui.stopAllButton:
            self.ui.stopAllButton.clicked.connect(self.on_stopAllClicked)
        elif hasattr(self.ui, "btnStopAll") and self.ui.btnStopAll:
            self.ui.btnStopAll.clicked.connect(self.on_stopAllClicked)

    def log_message(self, msg: str, channel = "terminal"):
        """Append a log line to the specified text channel"""
        if channel == "terminal":
            self.ui.terminalTextEdit.append(msg)
        elif channel == "console":
            self.ui.consoleTextEdit.append(msg)
        else:
            raise ValueError("[MainWindow] Invalid channel for log_message")

    def _set_processing_state(self, processing: bool):
        """Enable / disable UI elements during processing"""
        self.is_processing = processing
        enabled = not processing

        # Control buttons
        self.ui.processButton.setEnabled(enabled)
        self.ui.stopAllButton.setEnabled(processing)
        self.ui.cddisCredentialsButton.setEnabled(enabled)
        self.ui.observationsButton.setEnabled(enabled)
        self.ui.outputButton.setEnabled(enabled)
        self.ui.showConfigButton.setEnabled(enabled)
        self.ui.resetConfigButton.setEnabled(enabled)

        # PPP provider / series / project combos
        self.ui.pppProviderCombo.setEnabled(enabled)
        self.ui.pppProjectCombo.setEnabled(enabled)
        self.ui.pppSeriesCombo.setEnabled(enabled)

        # "General" config tab
        self.ui.modeCombo.setEnabled(enabled)
        self.ui.constellationsCombo.setEnabled(enabled)
        self.ui.receiverTypeCombo.setEnabled(enabled)
        self.ui.antennaTypeCombo.setEnabled(enabled)
        self.ui.antennaOffsetButton.setEnabled(enabled)
        self.ui.aprioriPositionButton.setEnabled(enabled)
        self.ui.timeWindowButton.setEnabled(enabled)
        self.ui.dataIntervalButton.setEnabled(enabled)

        # "Constellations" config tab
        self.ui.gpsListWidget.setEnabled(enabled)
        self.ui.galListWidget.setEnabled(enabled)
        self.ui.gloListWidget.setEnabled(enabled)
        self.ui.bdsListWidget.setEnabled(enabled)
        self.ui.qzsListWidget.setEnabled(enabled)

        # "Output" config tab
        self.ui.posCheckbox.setEnabled(enabled)
        self.ui.gpxCheckbox.setEnabled(enabled)
        self.ui.traceCheckbox.setEnabled(enabled)

        # Update button text to show processing state
        if processing:
            self.ui.processButton.setText("Processing...")
            # Set cursor to waiting cursor for visual feedback
            self.setCursor(Qt.CursorShape.WaitCursor)
        else:
            self.ui.processButton.setText("Process")
            self.setCursor(Qt.CursorShape.ArrowCursor)

    def _setup_worker_thread(self, worker, finished_callback, progress_callback=None, thread_attr=None, worker_attr=None):
        """
        Helper method to set up a worker in a QThread with standard cleanup.

        Args:
            worker: The worker object to run in the thread
            finished_callback: Callback to connect to worker.finished signal
            progress_callback: Optional callback to connect to worker.progress signal
            thread_attr: Optional attribute name to clear when thread finishes
            worker_attr: Optional attribute name to clear when thread finishes

        Returns:
            tuple: (thread, worker) for storing references
        """
        thread = QThread()
        worker.moveToThread(thread)

        # Connect started signal
        thread.started.connect(worker.run)

        # Connect finished signal
        worker.finished.connect(finished_callback)

        # Connect progress signal if provided
        if progress_callback and hasattr(worker, 'progress'):
            worker.progress.connect(progress_callback)

        # Connect cleanup signals
        worker.finished.connect(thread.quit)
        worker.finished.connect(worker.deleteLater)
        thread.finished.connect(thread.deleteLater)

        # Clear our references when thread finishes to avoid accessing deleted objects
        if thread_attr and worker_attr:
            def clear_references():
                if hasattr(self, thread_attr) and getattr(self, thread_attr) is thread:
                    setattr(self, thread_attr, None)
                if hasattr(self, worker_attr) and getattr(self, worker_attr) is worker:
                    setattr(self, worker_attr, None)

            thread.finished.connect(clear_references)

        return thread, worker

    def _cleanup_thread(self, thread_attr: str, worker_attr: str):
        """
        Request cancellation of a running thread and move it to _pending_threads

        Args:
            thread_attr: Name of the thread attribute (e.g., 'download_thread')
            worker_attr: Name of the worker attribute (e.g., 'download_worker')
        """
        worker = getattr(self, worker_attr, None)
        thread = getattr(self, thread_attr, None)

        # Try to stop the worker
        try:
            if worker is not None and hasattr(worker, 'stop'):
                worker.stop()
        except RuntimeError:
            pass  # Object already deleted

        # Check if thread is still running (with safety check for deleted objects)
        thread_running = False
        try:
            if thread is not None:
                thread_running = thread.isRunning()
        except RuntimeError:
            pass  # C++ object already deleted

        if thread_running:
            # Disconnect old signals to prevent callbacks to stale state
            try:
                worker.finished.disconnect()
                if hasattr(worker, 'cancelled'):
                    worker.cancelled.disconnect()
                if hasattr(worker, 'progress'):
                    worker.progress.disconnect()
            except (TypeError, RuntimeError):
                pass  # Already disconnected or object deleted

            # Keep reference alive until thread actually finishes
            old_thread = thread

            def cleanup_old_thread():
                if old_thread in self._pending_threads:
                    self._pending_threads.remove(old_thread)

            try:
                old_thread.finished.connect(cleanup_old_thread)
                self._pending_threads.append(old_thread)
            except RuntimeError:
                pass  # Object already deleted

        # Clear current references so new thread can be created
        setattr(self, worker_attr, None)
        setattr(self, thread_attr, None)

    def on_files_ready(self, rnx_path: str, out_path: str):
        self.rnx_file = rnx_path
        self.output_dir = out_path

    def _on_process_clicked(self):
        if not self.rnx_file or not self.output_dir:
            Logger.terminal("⚠️ Please select RINEX and output directory first")
            return

        # Check if in offline mode
        if self.offline_mode:
            Logger.terminal("⚠️ Cannot process: Ginan-UI is running in offline mode (no internet connection)")
            self._show_processing_offline_warning()
            return

        # Prevent multiple simultaneous processing
        if self.is_processing:
            Logger.terminal("⚠️ Processing already in progress. Please wait...")
            return

        # Lock the "Process" button and set processing state
        self._set_processing_state(True)

        # Get PPP params from UI
        ac = self.ui.pppProviderCombo.currentText()
        project = self.ui.pppProjectCombo.currentText()
        series = self.ui.pppSeriesCombo.currentText()

        # Archive old products if needed
        current_selection = {"ppp_provider": ac, "ppp_project": project, "ppp_series": series}
        archive_dir = archive_products_if_selection_changed(
            current_selection, getattr(self, "last_ppp_selection", None), INPUT_PRODUCTS_PATH
        )
        self.last_ppp_selection = current_selection
        if archive_dir:
            Logger.terminal(f"📦 Archived old PPP products → {archive_dir}")

        visual_dir = Path(self.output_dir) / "visual"
        output_archive = archive_old_outputs(Path(self.output_dir), visual_dir)
        if output_archive:
            Logger.terminal(f"📦 Archived old outputs → {output_archive}")

        # List products to be downloaded
        x = self.inputCtrl.products_df
        products = x.loc[(x["analysis_center"] == ac) & (x["project"] == project) & (x["solution_type"] == series)].drop_duplicates()

        # Reset progress
        self.download_progress.clear()

        # Clean up any existing download thread before starting a new one
        self._cleanup_thread('download_thread', 'download_worker')

        # Start download in background
        self.download_worker = DownloadWorker(products=products, start_epoch=self.inputCtrl.start_time, end_epoch=self.inputCtrl.end_time)
        self.download_thread, _ = self._setup_worker_thread(
            self.download_worker,
            self._on_download_finished,
            self._on_download_progress,
            thread_attr='download_thread',
            worker_attr='download_worker'
        )

        self.download_thread.setObjectName("ProductDownloadWorker")

        Logger.terminal("📡 Starting PPP product downloads...")
        self.download_thread.start()

    @Slot(str, int)
    def _on_download_progress(self, filename: str, percent: int):
        """Update progress display in-place at the bottom of the UI terminal."""
        self.download_progress[filename] = percent

        total_length = 20
        filled_length = int(percent/100 * total_length)
        bar = '[' + "█" * filled_length + "░" * (total_length - filled_length) + ']'
        output = f"{filename[:30]} {bar} {percent:3d}%"
        search_pattern = QRegularExpression(f"^{filename[:30]}.+%$")

        # Work with cursor & doc
        cursor = self.ui.terminalTextEdit.textCursor()
        cursor.movePosition(QTextCursor.End)
        flags = QTextDocument.FindFlag.FindBackward
        found_cursor = self.ui.terminalTextEdit.document().find(search_pattern, cursor, flags)

        on_latest_5_lines = self.ui.terminalTextEdit.document().blockCount() - found_cursor.blockNumber() <= 5
        if found_cursor.hasSelection() and on_latest_5_lines:
            found_cursor.movePosition(QTextCursor.EndOfLine) # Replaces final percent symbol too
            found_cursor.movePosition(QTextCursor.StartOfLine, QTextCursor.KeepAnchor)
            found_cursor.removeSelectedText()
            found_cursor.insertText(output)
        else: # Make new progress bar
            self.ui.terminalTextEdit.setTextCursor(cursor)
            cursor.insertText("\n" + output)
            cursor.movePosition(QTextCursor.End)
            self.ui.terminalTextEdit.setTextCursor(cursor)

    def _on_atx_downloaded(self, filename: str):
        self.atx_required_for_rnx_extraction = True
        Logger.terminal(f"✅ ATX file {filename} installed - ready for RINEX parsing")

    def _on_metadata_download_finished(self, message):
        Logger.terminal(message)
        self.metadata_downloaded = True
        self.inputCtrl.try_enable_process_button()

    def _on_download_finished(self, message):
        Logger.terminal(message)
        self._start_pea_execution()

    def _on_download_error(self, msg):
        Logger.terminal(f"⚠️ PPP download error: {msg}")
        self._set_processing_state(False)

    def _start_pea_execution(self):
        Logger.terminal("⚙️ Starting PEA execution in background...")

        # Clean up any existing PEA thread before starting a new one
        self._cleanup_thread('pea_thread', 'pea_worker')

        # Reset stop flag for new execution
        self.execution.reset_stop_flag()

        self.pea_worker = PeaExecutionWorker(self.execution)
        self.pea_thread, _ = self._setup_worker_thread(
            self.pea_worker,
            self._on_pea_finished,
            thread_attr='pea_thread',
            worker_attr='pea_worker'
        )

        self.pea_thread.setObjectName("PeaExecutionWorker")

        self.pea_thread.start()

    def _on_pea_finished(self):
        Logger.terminal("✅ PEA processing completed")
        show_toast(self, "✅ PEA Processing complete!", 3000)
        self._run_visualisation()
        self._set_processing_state(False)

    def _on_pea_error(self, msg: str):
        Logger.terminal(f"⚠️ PEA execution failed: {msg}")
        self._set_processing_state(False)

    def _run_visualisation(self):
        all_html_files = []

        # Check checkbox states to determine which visualisations to generate
        pos_enabled = self.ui.posCheckbox.isChecked() if hasattr(self.ui, "posCheckbox") else True
        trace_enabled = self.ui.traceCheckbox.isChecked() if hasattr(self.ui, "traceCheckbox") else False

        # Generate POS plots
        if pos_enabled:
            try:
                Logger.terminal("📊 Generating position plots from PEA output...")
                pos_html_files = self.execution.build_pos_plots()
                if pos_html_files:
                    all_html_files.extend(pos_html_files)
                else:
                    Logger.terminal("⚠️ No position plots found")
            except Exception as err:
                Logger.terminal(f"⚠️ Position plot generation failed: {err}")

        # Generate TRACE residual plots
        if trace_enabled:
            try:
                Logger.terminal("📊 Generating trace residual plots from PEA output...")
                trace_html_files = self.execution.build_trace_plots()
                if trace_html_files:
                    all_html_files.extend(trace_html_files)
                else:
                    Logger.terminal("⚠️ No trace plots found")
            except Exception as err:
                Logger.terminal(f"⚠️ Trace plot generation failed: {err}")

        # Update the visualisation panel with all generated plots (or empty list)
        self.visCtrl.set_html_files(all_html_files)

        if not all_html_files:
            Logger.terminal("📊 No visualisations generated - selector disabled")
            # Clear the web view display
            self.ui.webEngineView.setHtml("")

    def _validate_cddis_credentials_once(self):
        """
        Validate CDDIS credentials and connectivity.
        If no internet, app continues in offline mode with warning.
        """
        ok, where = gui_validate_netrc()
        if not ok and hasattr(self.ui, "cddisCredentialsButton"):
            Logger.terminal("⚠️ No Earthdata credentials. Opening CDDIS Credentials dialog...")
            self.ui.cddisCredentialsButton.click()
            ok, where = gui_validate_netrc()
        if not ok:
            Logger.terminal(f"❌ Credentials invalid: {where}")
            return
        Logger.terminal(f"✅ Earthdata Credentials found: {where}")

        ok_user, email_candidate = get_username_from_netrc()
        if not ok_user:
            Logger.terminal(f"❌ Cannot read username from .netrc: {email_candidate}")
            return

        # Wrap connection test in try-except to handle network errors gracefully
        try:
            ok_conn, why = test_cddis_connection()
            if not ok_conn:
                Logger.terminal(
                    f"❌ CDDIS connectivity check failed: {why}. Please verify Earthdata credentials via the CDDIS Credentials dialog"
                )
                self._show_offline_warning("Connection test failed", why)
                return
            Logger.terminal(f"✅ CDDIS connectivity check passed in {why.split(' ')[-2]} seconds")

            # Connection successful - set email
            write_email(email_candidate)
            Logger.terminal(f"✉️ EMAIL set to: {email_candidate}")

        except Exception as e:
            # Network error (no internet, DNS failure, timeout, etc.)
            error_msg = str(e)
            Logger.terminal(f"⚠️ No internet connection detected: {error_msg}")
            self._show_offline_warning("No internet connection", error_msg)
            return

    def _show_processing_offline_warning(self):
        """
        Show a warning when attempting to process while in offline mode.
        """
        show_toast(self, "⚠️ Processing requires internet connection", 4000)

        msg = QMessageBox(self)
        msg.setIcon(QMessageBox.Icon.Warning)
        msg.setWindowTitle("Offline Mode")
        msg.setText("Processing requires an internet connection to download PPP products from CDDIS")
        msg.setInformativeText("Please check your internet connection and restart Ginan-UI")
        msg.setStandardButtons(QMessageBox.StandardButton.Ok)
        msg.exec()

    def _show_offline_warning(self, title: str, details: str):
        """
        Show a warning dialog when Ginan-UI starts without internet.
        The app can continue to run, but very limited (some features are unavailable)
        """
        # Mark as offline mode
        self.offline_mode = True

        msg = QMessageBox(self)
        msg.setIcon(QMessageBox.Icon.Warning)
        msg.setWindowTitle("Ginan-UI - No Internet Connection")
        msg.setText(
            "<b>Ginan-UI requires internet access to function properly</b><br><br>"
            "The following features will be unavailable:"
        )
        msg.setInformativeText(
            "- Downloading PPP products from CDDIS<br>"
            "- Scanning for available analysis centers<br>"
            "- Retrieving GNSS data products<br><br>"
            "<b>The application will continue to run in offline mode.</b><br>"
            "You can still view configurations and access local files"
        )
        msg.setDetailedText(f"Error details:\n{title}: {details}")
        msg.setStandardButtons(QMessageBox.StandardButton.Ok)
        msg.setDefaultButton(QMessageBox.StandardButton.Ok)

        # Show the dialog
        msg.exec()

        # Also show a toast notification
        show_toast(self, "⚠️ Running in offline mode - limited functionality", 8000)

    # Added: unified stop entry, wired to an optional UI button
    @Slot()
    def on_stopAllClicked(self):
        Logger.terminal("🛑 Stop requested - stopping all running tasks...")

        # Stop the metadata worker in InputController, if present
        try:
            if hasattr(self, "inputCtrl") and hasattr(self.inputCtrl, "stop_all"):
                self.inputCtrl.stop_all()
        except Exception:
            pass

        # Stop PPP downloads, if running
        try:
            self._cleanup_thread('download_thread', 'download_worker')
        except Exception:
            pass

        # Stop PEA execution, if running
        try:
            self._cleanup_thread('pea_thread', 'pea_worker')
        except Exception:
            pass

        # Best-effort: ask Execution to stop any external process if supported
        try:
            if hasattr(self, "execution") and self.execution is not None and hasattr(self.execution, "stop_all"):
                self.execution.stop_all()
        except Exception:
            pass

        # Restore UI state immediately
        try:
            if self.is_processing:
                self._set_processing_state(False)
        except Exception:
            pass

    def closeEvent(self, event):
        """
        Handle window close event - makes sure that all threads are terminated cleanly
        """
        # Stop all running threads
        self.on_stopAllClicked()

        # Also stop InputController threads
        if hasattr(self, 'inputCtrl'):
            self.inputCtrl.stop_all()

        # Give threads a moment to clean up
        QCoreApplication.processEvents()

        # Accept the close event
        event.accept()

    def _fix_macos_tab_styling(self):
        """
        Fix tab widget styling on macOS where native styling overrides custom stylesheets.
        This method applies a comprehensive stylesheet directly to the QTabBar to ensure
        consistent appearance across all platforms.
        """
        import platform

        # On macOS, we need to be more aggressive with styling to override native appearance
        if platform.system() == "Darwin":
            # Import QStyleFactory to optionally force Fusion style
            from PySide6.QtWidgets import QStyleFactory

            # Force Fusion style on the tab widget to disable native macOS rendering
            fusion_style = QStyleFactory.create("Fusion")
            if fusion_style:
                self.ui.logTabWidget.setStyle(fusion_style)
                self.ui.configTabWidget.setStyle(fusion_style)

        # Apply comprehensive stylesheet to ensure consistent appearance
        tab_bar_stylesheet = """
        QTabWidget::pane {
            border: none;
            background-color: #2c5d7c;
        }

        QTabBar {
            background-color: transparent;
            alignment: left;
        }

        QTabBar::tab {
            background-color: #1a3a4d;
            color: white;
            padding: 8px 16px;
            margin-right: 2px;
            border: none;
            border-top-left-radius: 4px;
            border-top-right-radius: 4px;
            min-width: 60px;
        }

        QTabBar::tab:selected {
            background-color: #2c5d7c;
            color: white;
            font-weight: bold;
        }

        QTabBar::tab:hover:!selected {
            background-color: #234a5f;
        }

        QTabBar::tab:!selected {
            margin-top: 2px;
        }
        """

        # Apply the stylesheet to the tab widget
        self.ui.logTabWidget.setStyleSheet(tab_bar_stylesheet)
        self.ui.configTabWidget.setStyleSheet(tab_bar_stylesheet)