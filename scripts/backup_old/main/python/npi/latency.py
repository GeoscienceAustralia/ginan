import argparse
import logging
import os
import sys
import time

import requests
import threading

from bitstring import BitArray

from datetime import datetime, timedelta

from npi import get_correct_utc_time
from npi.config import Config
from npi.rtcm import RTCM3

MESSAGE = 100

logging.addLevelName(MESSAGE, "MESSAGE")

logging.basicConfig(level=logging.INFO, format="%(asctime)s | %(levelname)s | (%(threadName)-10s) | %(message)s")

_log = logging.getLogger(__name__)


def main():

    _log.info("Latency tool starting...")

    args = process_arguments()

    # TODO
    #  - command line arguments for config file, output directory, ???

    _log.info("Reading configuration from [%s]", "config/config.yaml")

    config = Config("config/config.yaml")

    threads = []

    _log.info("Creating handler thread for each stream...")

    for type, format, id, url, username, password, count in config.get_streams():

        _log.debug("Adding thread for stream url=[%s] username=[%s] id=[%s] count=[%3d]", url, username, id, count)

        t = threading.Thread(target=worker, args=(args, username, password, url, id, count))

        threads.append(t)

        t.start()


def worker(args, username, password, url, id, num_messages):

    from logging.handlers import TimedRotatingFileHandler

    filename = os.path.join("data", "latency", "latency.{id}.dat".format(id=id))

    # TODO use atTime to get the interval start on the epoch boundary rather than whenever it starts

    # handler = TimedRotatingFileHandler(filename=filename, when="S", interval=30, delay=True, utc=True)

    # at = datetime.now().time()
    #
    # at.replace(hour=at.hour + 1, minute=0, second=0)
    #
    # handler = TimedRotatingFileHandler(filename=filename, when="S", interval=30, delay=True, utc=True, atTime=at)

    # at = datetime.utcnow().time()
    #
    # at.replace(minute=at.minute + 1, second=0)
    #
    # handler = TimedRotatingFileHandler(filename=filename, when="H", interval=1, delay=True, utc=True, atTime=at)

    handler = TimedRotatingFileHandler(filename=filename, when="H", interval=1, delay=True, utc=True)

    handler.setLevel(MESSAGE)

    formatter = logging.Formatter("%(asctime)s.%(msecs)d,%(message)s", datefmt="%Y-%m-%d.%H:%M:%S")

    formatter.converter = time.gmtime

    handler.setFormatter(formatter)

    _log.addHandler(handler)

    if num_messages:
        _log.info("Starting processing username=[%s] url=[%s] filename=[%s] messages=[% 4d]", username, url, filename, num_messages)

    else:
        _log.info("Starting processing username=[%s] url=[%s] filename=[%s]", username, url, filename)

    auth = (username, password)
    headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}

    proxies = None

    if args.noproxy:
        proxies = {"http": None, "https": None}

    done = False

    count = 0
    count_skipped = 0

    while not done:

        try:

            _log.info("Connecting...")

            with requests.get(url, auth=auth, headers=headers, proxies=proxies, stream=True, timeout=300) as response:

                _log.info("Connected - starting to read messages...")

                while True:

                    length, message_number, crc, reference_station_id, epoch, epoch_dtm, data = RTCM3.get_next_message(response.raw)

                    current_dtm = get_correct_utc_time()

                    _log.debug("frame length=[%s] message number=[%s]", length, message_number)

                    if length and message_number and crc:

                        if RTCM3.is_msm_message(message_number):

                            _log.debug("message number=[%d] epoch=[%d] epoch_dtm=[%s] current_dtm=[%s] latency=[%s] bits=[%s]", message_number, epoch, epoch_dtm, current_dtm, current_dtm - epoch_dtm, " ".join(["{0:08b}".format(x) for x in data[:12]]))

                            _log.log(MESSAGE, "{},{},{},{},{},{},{}".format(message_number, epoch, epoch_dtm, current_dtm, current_dtm - epoch_dtm, length, " ".join(["{0:08b}".format(x) for x in data[:12]])))

                            count += 1

                    else:
                        count_skipped += 1

                    if num_messages and count >= num_messages:
                        done = True

                    _log.info("Processed [%d] messages skipped [%d] ----> done = [%s]", count, count_skipped, done)

                    if done:
                        break

        except Exception as e:

            _log.info("Caught exception...reconnecting...\n%s", e)

    _log.info("count=[%d] skipped=[%d]", count, count_skipped)


def process_arguments():

    parser = argparse.ArgumentParser(prog=sys.argv[0], description=__name__)

    group = parser.add_mutually_exclusive_group()

    group.add_argument("--quiet", help="Less output (WARNINGS and higher)", action="store_const", dest="log_level", const=logging.WARN)
    group.add_argument("--verbose", help="More output (DEBUG and higher)", action="store_const", dest="log_level", const=logging.DEBUG)

    parser.set_defaults(log_level=logging.INFO)

    parser.add_argument("--no-proxy", help="Don't use proxy", action="store_true", dest="noproxy", default=False)

    args = parser.parse_args()

    _log.setLevel(args.log_level)

    _log.debug("Logging level = [%s]", logging.getLevelName(args.log_level))

    _log.debug("Ignore proxy = [%s]", args.noproxy)

    return args


if __name__ == "__main__":
    main()
