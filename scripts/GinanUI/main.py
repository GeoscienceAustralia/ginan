import os
import sys
import logging

from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QApplication
from scripts.GinanUI.app.main_window import MainWindow

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

if __name__ == "__main__":
    # Disable GPU acceleration (can cause segmentation faults on launch if enabled)
    os.environ['QTWEBENGINE_CHROMIUM_FLAGS'] = '--disable-gpu --disable-software-rasterizer --no-sandbox'

    app = QApplication(sys.argv)
    app.setWindowIcon(QIcon("app/resources/ginan-logo.png"))
    window = MainWindow()
    window.setWindowIcon(QIcon("app/resources/ginan-logo.png"))
    window.show()
    sys.exit(app.exec())
