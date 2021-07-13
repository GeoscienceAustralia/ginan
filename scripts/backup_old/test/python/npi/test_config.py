import logging
import json
from datetime import datetime
from croniter import croniter

import yaml

from npi import EpochDuration
from npi.config import Config

logging.basicConfig(level=logging.INFO)

_log = logging.getLogger(__name__)


# def test():
#     config = {
#         "streams": [{
#             "type": "NTRIP",
#             "protocol": "http",
#             "host": "auscors.ga.gov.au",
#             "port": 2101,
#             "username": "TaoLi1234",
#             "password": "LGYdOWDnVC",
#             "stations": [{"id": "COCO7", "format": "RTCM3"},
#                          {"id": "CUT07", "format": "RTCM3"}]
#         }]
#     }
#
#     _log.info("JSON: [%s]", json.dumps(config))
#
#     _log.info("YAML: [%s]", yaml.dump(config))
#
#     with open("config.yaml", "wt") as f:
#         yaml.dump(config, f)


def test_parse_default():
    _log.info("Testing parsing the config")

    config = Config()

    for stream in config.get_streams():
        _log.info("Found stream [%s]", stream)


def test_parse_file():

    _log.info("Testing parsing the config file")

    config = Config("config/config.yaml")

    for stream in config.get_streams():
        _log.info("Found stream [%s]", stream)


def test_epoch():

    epoch = EpochDuration.MINUTE_15

    now = datetime.utcnow()

    _log.info("Epoch duration is [%s] which is [%s] time is [%s]", epoch.name, epoch.value, now)

    _log.info("Next is [%s]", croniter('*/5 * * * *', now).get_next(datetime))
