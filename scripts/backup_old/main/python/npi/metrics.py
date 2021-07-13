import logging
import os

import requests

from npi.config import Config

logging.basicConfig(level=logging.INFO)

_log = logging.getLogger(__name__)


def main():

    _log.debug("ACS metrics starting...")

    _log.debug("Current working directory is [%s]", os.getcwd())

    config = Config()

    input_type, input_format, input_url = (config.get_input_type(), config.get_input_format(), config.get_input_url())

    _log.debug("Reading data from type=[%s] format=[%s] url=[%s]", input_type, input_format, input_url)

    go()


def go():
    username = "TaoLi1234"
    password = "LGYdOWDnVC"
    url = "http://auscors.ga.gov.au:2101/COCO7"

    # proxies = {"http": "http://localhost:8888"}
    proxies = {}
    proxies = {"http": "http://proxy.ga.gov.au:8080"}

    auth = (username, password)
    headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}

    with requests.get(url, auth=auth, headers=headers, proxies=proxies, stream=True) as response:

        _log.debug("Connected - starting to read messages...")

        count = 0
        count_skipped = 0

        with open("data/COCO7.dat", "wb") as f:

            for data in response.iter_content(chunk_size=1024):

                if data:
                    _log.debug("count=[%d] size=[%d] data=[%s]", count, len(data), data)
                    f.write(data)
                    count += 1

                if count > 100:
                    break

    _log.debug("done")

    _log.debug("response is [%s]", response)


if __name__ == "__main__":
    main()
