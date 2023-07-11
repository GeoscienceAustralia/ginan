import logging
import sys

# __version__ = _get_version_string(abbrev=10)

logging.captureWarnings(True)


logger = logging.getLogger(__name__)
formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s> %(message)s", "%Y-%m-%dT%H:%M:%S")
logger.setLevel(logging.INFO)
stdout_handler = logging.StreamHandler(sys.stdout)
stdout_handler.setFormatter(formatter)

# file_handler = logging.FileHandler("log.log")
# file_handler.setLevel(logging.DEBUG)
# file_handler.setFormatter(formatter)

# logger.addHandler(file_handler)
logger.addHandler(stdout_handler)

from . import _version

__version__ = _version.get_versions()["version"]
