# app/utils/workers.py
import traceback
from datetime import datetime
from pathlib import Path
from typing import Optional

import pandas as pd
from PySide6.QtCore import QObject, Signal, Slot

from scripts.GinanUI.app.models.dl_products import get_product_dataframe_with_repro3_fallback, download_products, get_brdc_urls, download_metadata, get_provider_constellations, get_bia_code_priorities_for_selection
from scripts.GinanUI.app.utils.common_dirs import INPUT_PRODUCTS_PATH

from scripts.GinanUI.app.utils.logger import Logger

class PeaExecutionWorker(QObject):
    """
    Executes execute_config() method of a given PEAExecution instance.
    The 'execution' object is expected to implement:
      - execute_config()
      - stop_all()  (optional but recommended: terminate underlying process)
    """
    finished = Signal(object)

    def __init__(self, execution):
        super().__init__()
        self.execution = execution

    @Slot()
    def stop(self):
        try:
            Logger.terminal("🛑 Stop requested - terminating PEA...")
            # recommended to implement stop_all() in Execution to terminate child processes
            if hasattr(self.execution, "stop_all"):
                self.execution.stop_all()
                Logger.terminal("🛑 Stopped")
        except Exception:
            tb = traceback.format_exc()
            Logger.terminal(f"⚠️ Exception during stop:\n{tb}")

    @Slot()
    def run(self):
        try:
            self.execution.execute_config()
            self.finished.emit("✅ Execution finished successfully.")
        except Exception:
            tb = traceback.format_exc()
            Logger.terminal(f"⚠️ Error launching Execution! Exception:\n{tb}")


class DownloadWorker(QObject):
    """
    Downloads PPP and BRDC products for a specified date range or retrieves valid analysis centers.

    :param products: DataFrame of products to download. (See get_product_dataframe())
    :param download_dir: Directory to save downloaded products.
    :param start_epoch: Start datetime for BRDC files.
    :param end_epoch: End datetime for BRDC files.
    :param analysis_centers: Set to true to retrieve valid analysis centers, ensure start and end date specified
    """
    finished = Signal(object)
    cancelled = Signal()
    progress = Signal(str, int)
    atx_downloaded = Signal(str)
    constellation_info = Signal(dict)  # Emits provider to constellations mapping

    def __init__(self, start_epoch: Optional[datetime]=None, end_epoch: Optional[datetime]=None,
                 download_dir: Path=INPUT_PRODUCTS_PATH, products: pd.DataFrame=pd.DataFrame(), analysis_centers=False):
        super().__init__()
        self.products = products
        self.download_dir = download_dir
        self.start_epoch = start_epoch
        self.end_epoch = end_epoch
        self.analysis_centers = analysis_centers
        self._stop = False

    @Slot()
    def stop(self):
        self._stop = True

    @Slot()
    def run(self):

        # 1. Get valid products
        if self.analysis_centers:
            if not self.start_epoch and not self.end_epoch:
                Logger.terminal(f"📦 No start and/or end date, can't check valid analysis centers")
                self.cancelled.emit()
                return
            Logger.terminal(f"📦 Retrieving valid products")
            try:
                # Check if stop was requested before starting
                if self._stop:
                    Logger.terminal(f"📦 Analysis centres retrieval cancelled")
                    self.cancelled.emit()
                    return
                # Use the repro3 fallback function which automatically checks repro3
                # if no valid PPP providers are found in the main directory
                valid_products = get_product_dataframe_with_repro3_fallback(self.start_epoch, self.end_epoch)
                # Check again before emitting result - don't emit finished if cancelled
                if self._stop:
                    Logger.terminal(f"📦 Analysis centres retrieval cancelled")
                    self.cancelled.emit()
                    return

                # Fetch constellation information for each provider
                if not valid_products.empty:
                    Logger.terminal(f"📡 Fetching constellation information from SP3 headers...")

                    def check_stop():
                        return self._stop

                    provider_constellations = get_provider_constellations(
                        valid_products,
                        progress_callback=self.progress.emit,
                        stop_requested=check_stop
                    )

                    if self._stop:
                        Logger.terminal(f"📦 Analysis centres retrieval cancelled")
                        self.cancelled.emit()
                        return

                    # Emit constellation info before finished
                    if provider_constellations:
                        self.constellation_info.emit(provider_constellations)

                self.finished.emit(valid_products)
            except Exception as e:
                if self._stop:
                    Logger.terminal(f"📦 Analysis centres retrieval cancelled")
                    self.cancelled.emit()
                    return
                tb = traceback.format_exc()
                Logger.terminal(f"⚠️ Error whilst retrieving valid products:\n{tb}")
                Logger.terminal(f"⚠️ {e}")
                self.cancelled.emit()
            return

        # 2. Install metadata
        elif self.products.empty:
            try:
                download_metadata(self.download_dir, self.progress.emit, self.atx_downloaded.emit)
            except Exception as e:
                tb = traceback.format_exc()
                Logger.terminal(f"⚠️ Error whilst downloading metadata:\n{tb}")
                Logger.terminal(f"⚠️ {e}")
                self.cancelled.emit()
                return

            self.finished.emit("📦 Downloaded metadata successfully.")


        # 3. Install products
        else:
            try:
                def check_stop():
                    return self._stop
                # Disregard generator output
                for _ in download_products(self.products, download_dir=self.download_dir,
                                  dl_urls=get_brdc_urls(self.start_epoch, self.end_epoch),
                                  progress_callback=self.progress.emit, stop_requested=check_stop):
                    pass
            except RuntimeError as e:
                Logger.terminal(f"⚠️ {e}")
                self.cancelled.emit()
                return
            except Exception as e:
                tb = traceback.format_exc()
                Logger.terminal(f"⚠️ Error whilst downloading products:\n{tb}")
                Logger.terminal(f"⚠️ {e}")
                self.cancelled.emit()
                return

        if self._stop:
            self.cancelled.emit()
            return

        self.finished.emit("📦 Downloaded all products successfully.")

class BiasProductWorker(QObject):
    """
    Downloads and parses .BIA file for a specific PPP provider / series / project combination
    to extract available code priorities per constellation.

    :param products_df: Products dataframe from get_product_dataframe_with_repro3_fallback()
    :param provider: Analysis centre (e.g., 'COD', 'GRG')
    :param series: Solution type (e.g., 'FIN', 'RAP')
    :param project: Project code (e.g., 'OPS', 'MGX')
    """
    finished = Signal(dict)  # Emits code priorities dict: {'GPS': {'L1C', ...}, ...}
    error = Signal(str)  # Emits error message string
    progress = Signal(str, int)  # Emits (description, percent) for progress updates

    def __init__(self, products_df: pd.DataFrame, provider: str, series: str, project: str):
        super().__init__()
        self.products_df = products_df
        self.provider = provider
        self.series = series
        self.project = project
        self._stop = False

    @Slot()
    def stop(self):
        self._stop = True

    @Slot()
    def run(self):
        try:
            # Check if stop was requested before starting
            if self._stop:
                Logger.console(f"📦 BIA fetch cancelled")
                self.error.emit("BIA fetch cancelled")
                return

            Logger.console(f"📦 Fetching BIA code priorities for {self.provider}/{self.series}/{self.project}...")

            def check_stop():
                return self._stop

            # Download and parse BIA file
            code_priorities = get_bia_code_priorities_for_selection(
                self.products_df,
                self.provider,
                self.series,
                self.project,
                progress_callback=self.progress.emit,
                stop_requested=check_stop
            )

            # Check again after download
            if self._stop:
                Logger.console(f"📦 BIA fetch cancelled")
                self.error.emit("BIA fetch cancelled")
                return

            if code_priorities is None:
                self.error.emit(f"Failed to fetch BIA data for {self.provider}/{self.series}/{self.project}")
                return

            # Emit successful result
            self.finished.emit(code_priorities)

        except Exception as e:
            if self._stop:
                self.error.emit("BIA fetch cancelled")
                return
            tb = traceback.format_exc()
            Logger.console(f"⚠️ Error fetching BIA code priorities:\n{tb}")
            self.error.emit(f"Error fetching BIA: {e}")