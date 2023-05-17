"""_summary_
"""
import logging
import os
import sys

logger = logging.getLogger(__name__)
# logger.setLevel(logging.DEBUG)
formatter = logging.Formatter(
    "%(asctime)s - %(name)s - %(levelname)s> %(message)s", "%Y-%m-%dT%H:%M:%S"
)

stdout_handler = logging.StreamHandler(sys.stdout)
# stdout_handler.setLevel(logging.DEBUG)
stdout_handler.setFormatter(formatter)

file_handler = logging.FileHandler("log.log")
# file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(formatter)

logger.addHandler(file_handler)
logger.addHandler(stdout_handler)


__version__ = "TBD"
