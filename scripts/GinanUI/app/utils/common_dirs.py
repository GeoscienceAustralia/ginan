import sys
from pathlib import Path

def get_base_path():
    """Get the base path for resources, handling both development and PyInstaller bundled modes."""
    if getattr(sys, 'frozen', False):
        # Running in PyInstaller bundle - sys._MEIPASS is _internal/
        # and app folder is at _internal/app/
        return Path(sys._MEIPASS) / "app"
    else:
        # Running in development mode - __file__ is in app/utils/
        return Path(__file__).parent.parent

BASE_PATH = get_base_path()
TEMPLATE_PATH = BASE_PATH / "resources" / "Yaml" / "default_config.yaml"
GENERATED_YAML = BASE_PATH / "resources" / "ppp_generated.yaml"
INPUT_PRODUCTS_PATH = BASE_PATH / "resources" / "inputData" / "products"
