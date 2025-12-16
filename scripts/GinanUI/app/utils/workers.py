# app/utils/workers.py
import traceback
from datetime import datetime
from pathlib import Path
from typing import Optional

import pandas as pd
from PySide6.QtCore import QObject, Signal, Slot

from scripts.GinanUI.app.models.dl_products import get_product_dataframe, download_products, get_brdc_urls, METADATA, download_metadata
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
            Logger.terminal("🛑 Stop requested — terminating PEA...")
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
    progress = Signal(str, int)
    atx_downloaded = Signal(str)

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
                return
            Logger.terminal(f"📦 Retrieving valid products")
            try:
                valid_products = get_product_dataframe(self.start_epoch, self.end_epoch)
                self.finished.emit(valid_products)
            except Exception as e:
                tb = traceback.format_exc()
                Logger.terminal(f"⚠️ Error whilst retrieving valid products:\n{tb}")
                Logger.terminal(f"⚠️ {e}")
            return

        # 2. Install metadata
        elif self.products.empty:
            try:
                download_metadata(self.download_dir, self.progress.emit, self.atx_downloaded.emit)
            except Exception as e:
                tb = traceback.format_exc()
                Logger.terminal(f"⚠️ Error whilst downloading metadata:\n{tb}")
                Logger.terminal(f"⚠️ {e}")
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
                return
            except Exception as e:
                tb = traceback.format_exc()
                Logger.terminal(f"⚠️ Error whilst downloading products:\n{tb}")
                Logger.terminal(f"⚠️ {e}")
                return

        self.finished.emit("📦 Downloaded all products successfully.")