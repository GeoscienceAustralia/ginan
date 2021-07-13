import logging
from datetime import datetime

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

_log = logging.getLogger(__name__)


def main():
    import requests
    import pickle

    username = "TaoLi1234"
    password = "LGYdOWDnVC"
    url = "http://auscors.ga.gov.au:2101/COCO7"

    proxies = {}
    proxies = {"http": "http://proxy.ga.gov.au:8080"}
    proxies = {"http": "http://proxy.inno.lan:3128"}

    auth = (username, password)
    headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}

    _log.debug("About to connect")

    with requests.get(url, auth=auth, headers=headers, proxies=proxies, stream=True) as response:

        _log.debug("Connected - starting to read messages...")

        count = 0

        with open("data/dumper.COCO7.1000.laptop.dat", "wb") as f:

            for data in response.iter_content(chunk_size=1024):

                tstamp = datetime.utcnow()

                _log.debug("count=[%d] size=[%d]", count, len(data))

                f.write(pickle.dumps((tstamp, data)))

                count += 1

                if count > 1000:
                    break

    _log.debug("done")


if __name__ == "__main__":
    main()
