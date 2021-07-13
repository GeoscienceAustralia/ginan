import logging

import yaml

from npi import InputType, InputProtocol, InputFormat

_log = logging.getLogger(__name__)

DEFAULT = """
streams:
- type: NTRIP
  protocol: http
  host: auscors.ga.gov.au
  port: 2101
  username: TaoLi1234
  password: LGYdOWDnVC
  stations:
  - {id: COCO7, format: RTCM3}
  - {id: CUT07, format: RTCM3}
"""


class Config:

    def __init__(self, filename=None):

        if filename:
            with open(filename, "r") as f:
                self.config = yaml.load(f)

        else:
            self.config = yaml.load(DEFAULT)

    def get_streams(self):

        _log.debug("Getting streams...")

        for stream in self.config["streams"]:

            type = InputType.NTRIP

            if "type" in stream:
                type = InputType[stream["type"].upper()]

            protocol = InputProtocol.HTTP

            if "protocol" in stream:
                protocol = InputProtocol[stream["protocol"].upper()]

            host = None

            if "host" in stream:
                host = stream["host"]

            port = 80

            if "port" in stream:
                port = int(stream["port"])

            username = None

            if "username" in stream:
                username = stream["username"]

            password = None

            if "password" in stream:
                password = stream["password"]

            count = None

            if "count" in stream:
                count = int(stream["count"])

            format = InputFormat.RTCM3

            if "format" in stream:
                format = InputFormat[stream["format"].upper()]

            # epoch =

            for station in stream["stations"]:

                if "count" in station:
                    count = int(station["count"])

                if "format" in station:
                    format = InputFormat[station["format"].upper()]

                yield type, format, station["id"], "{protocol}://{host}:{port}/{station}".format(protocol=protocol.value, host=host, port=port, station=station["id"]), username, password, count
