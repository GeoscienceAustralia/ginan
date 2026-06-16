"""
QObject worker classes for running background tasks in Ginan-UI.

Provides thread-safe workers for:
  - PeaExecutionWorker: runs a PEA processing execution
  - DownloadWorker: downloads PPP / BRDC products or retrieves valid analysis centres
  - BiasProductWorker: fetches and parses BIA code priorities for a given provider
  - SinexValidationWorker: downloads and validates an IGS CRD SINEX file against RINEX metadata

All workers communicate results back to the UI via Qt signals.
"""

import traceback
from datetime import datetime
from pathlib import Path
from typing import Optional, List
import pandas as pd
from PySide6.QtCore import QObject, Signal, Slot
from scripts.GinanUI.app.models.dl_products import (
    get_product_dataframe_with_repro3_fallback,
    download_products,
    get_brdc_urls,
    download_metadata,
    get_provider_constellations,
    get_bia_code_priorities_for_selection,
    download_and_validate_sinex,
    log_sinex_validation_results,
)
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
            Logger.workflow("🛑 Stop requested - terminating PEA...")
            # recommended to implement stop_all() in Execution to terminate child processes
            if hasattr(self.execution, "stop_all"):
                self.execution.stop_all()
                Logger.workflow("🛑 Stopped")
        except Exception:
            tb = traceback.format_exc()
            Logger.workflow(f"⚠️ Exception during stop:\n{tb}")

    @Slot()
    def run(self):
        try:
            self.execution.execute_config()
            self.finished.emit("✅ Execution finished successfully.")
        except Exception:
            tb = traceback.format_exc()
            Logger.workflow(f"⚠️ Error launching Execution! Exception:\n{tb}")

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
                Logger.workflow(f"📦 No start and/or end date, can't check valid analysis centers")
                self.cancelled.emit()
                return
            Logger.workflow(f"📦 Retrieving valid products")
            try:
                # Check if stop was requested before starting
                if self._stop:
                    Logger.workflow(f"📦 Analysis centres retrieval cancelled")
                    self.cancelled.emit()
                    return
                # Use the repro3 fallback function which automatically checks repro3
                # if no valid PPP providers are found in the main directory
                valid_products = get_product_dataframe_with_repro3_fallback(self.start_epoch, self.end_epoch)
                # Check again before emitting result - don't emit finished if cancelled
                if self._stop:
                    Logger.workflow(f"📦 Analysis centres retrieval cancelled")
                    self.cancelled.emit()
                    return

                # Fetch constellation information for each provider
                if not valid_products.empty:
                    Logger.workflow(f"📡 Fetching constellation information from SP3 headers...")

                    def check_stop():
                        return self._stop

                    provider_constellations = get_provider_constellations(
                        valid_products,
                        progress_callback=self.progress.emit,
                        stop_requested=check_stop
                    )

                    if self._stop:
                        Logger.workflow(f"📦 Analysis centres retrieval cancelled")
                        self.cancelled.emit()
                        return

                    # Emit constellation info before finished
                    if provider_constellations:
                        self.constellation_info.emit(provider_constellations)

                self.finished.emit(valid_products)
            except Exception as e:
                if self._stop:
                    Logger.workflow(f"📦 Analysis centres retrieval cancelled")
                    self.cancelled.emit()
                    return
                tb = traceback.format_exc()
                Logger.workflow(f"⚠️ Error whilst retrieving valid products:\n{tb}")
                Logger.workflow(f"⚠️ {e}")
                self.cancelled.emit()
            return

        # 2. Install metadata
        elif self.products.empty:
            try:
                download_metadata(self.download_dir, self.progress.emit, self.atx_downloaded.emit)
            except Exception as e:
                tb = traceback.format_exc()
                Logger.workflow(f"⚠️ Error whilst downloading metadata:\n{tb}")
                Logger.workflow(f"⚠️ {e}")
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
                Logger.workflow(f"⚠️ {e}")
                self.cancelled.emit()
                return
            except Exception as e:
                tb = traceback.format_exc()
                Logger.workflow(f"⚠️ Error whilst downloading products:\n{tb}")
                Logger.workflow(f"⚠️ {e}")
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

class SinexValidationWorker(QObject):
    """
    Downloads the IGS CRD SINEX file and validates RINEX-extracted values against it.

    :param target_date: The date for which to download the SINEX file
    :param marker_name: 4-character marker name from RINEX
    :param receiver_type: Receiver type from RINEX
    :param antenna_type: Antenna type from RINEX
    :param antenna_offset: Antenna offset [E, N, U] from RINEX
    :param apriori_position: Optional apriori position [X, Y, Z] from RINEX
    :param download_dir: Directory to save the downloaded file
    """
    finished = Signal(Path, dict)  # Emits (sinex_path, validation_results)
    error = Signal(str)                  # Emits error message string
    progress = Signal(str, int)    # Emits (description, percent) for progress updates

    def __init__(self, target_date: datetime, marker_name: str, receiver_type: str, antenna_type: str,antenna_offset: List[float],
                 apriori_position: Optional[List[float]] = None, download_dir: Path = INPUT_PRODUCTS_PATH):
        super().__init__()
        self.target_date = target_date
        self.marker_name = marker_name
        self.receiver_type = receiver_type
        self.antenna_type = antenna_type
        self.antenna_offset = antenna_offset
        self.apriori_position = apriori_position
        self.download_dir = download_dir
        self._stop = False

    @Slot()
    def stop(self):
        self._stop = True

    @Slot()
    def run(self):
        try:
            # Check if stop was requested before starting
            if self._stop:
                Logger.workflow(f"📦 SINEX validation cancelled")
                self.error.emit("SINEX validation cancelled")
                return

            def check_stop():
                return self._stop

            # Download and validate SINEX file
            sinex_path, results = download_and_validate_sinex(
                self.target_date,
                self.marker_name,
                self.receiver_type,
                self.antenna_type,
                self.antenna_offset,
                self.apriori_position,
                self.download_dir,
                progress_callback=self.progress.emit,
                stop_requested=check_stop
            )

            # Check again after download
            if self._stop:
                Logger.workflow(f"📦 SINEX validation cancelled")
                self.error.emit("SINEX validation cancelled")
                return

            if sinex_path is None:
                # The SINEX file could not be downloaded - it most likely does not
                # exist for this date (common for recent ultra-rapid / rapid products).
                # Skip validation gracefully rather than reporting a hard failure.
                Logger.workflow("ℹ️ SINEX file unavailable for this date - skipping SINEX validation")
                self.error.emit("SINEX validation skipped: file unavailable")
                return

            # Log the validation results
            log_sinex_validation_results(results, self.marker_name)

            # Emit successful result
            self.finished.emit(sinex_path, results)

        except Exception as e:
            if self._stop:
                self.error.emit("SINEX validation cancelled")
                return
            tb = traceback.format_exc()
            Logger.workflow(f"⚠️ Error during SINEX validation:\n{tb}")
            self.error.emit(f"Error during SINEX validation: {e}")

class LoadingWorker(QObject):
    """
    Ensures ocean and atmospheric tide loading BLQ files exist for a given station.

    Downloads the loading grid netCDF files if needed, checks whether the station
    already has entries in the existing BLQ files, and runs interpolate_loading
    to generate station-specific BLQ files when required.

    :param execution: The Execution instance (used for ensure_loading_blq)
    :param marker_name: 4-character station marker name (e.g. 'ALIC')
    :param marker_number: DOMES marker number (e.g. '50137M0014') or None
    :param apriori_position: [X, Y, Z] ECEF coordinates in metres
    """
    finished = Signal()           # Emitted when loading BLQ processing completes
    error = Signal(str)           # Emits error message string
    progress = Signal(str, int)   # Emits (description, percent) for progress updates

    def __init__(self, execution, marker_name: str, marker_number: Optional[str],
                 apriori_position: List[float]):
        super().__init__()
        self.execution = execution
        self.marker_name = marker_name
        self.marker_number = marker_number
        self.apriori_position = apriori_position
        self._stop = False

    @Slot()
    def stop(self):
        self._stop = True

    @Slot()
    def run(self):
        try:
            if self._stop:
                Logger.workflow("📦 Loading BLQ generation cancelled")
                self.error.emit("Loading BLQ generation cancelled")
                return

            def check_stop():
                return self._stop

            self.execution.ensure_loading_blq(
                marker_name=self.marker_name,
                marker_number=self.marker_number,
                apriori_position=self.apriori_position,
                progress_callback=self.progress.emit,
                stop_requested=check_stop,
            )

            if self._stop:
                Logger.workflow("📦 Loading BLQ generation cancelled")
                self.error.emit("Loading BLQ generation cancelled")
                return

            self.finished.emit()

        except Exception as e:
            if self._stop:
                self.error.emit("Loading BLQ generation cancelled")
                return
            tb = traceback.format_exc()
            Logger.workflow(f"⚠️ Error during loading BLQ generation:\n{tb}")
            self.error.emit(f"Error during loading BLQ generation: {e}")