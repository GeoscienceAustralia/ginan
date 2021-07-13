import binascii
import logging
import sys
from datetime import datetime, timedelta

from npi.rtcm import RTCM3

logging.basicConfig(level=logging.INFO)

_log = logging.getLogger(__name__)


def get_correct_utc_time():

    # On Simon's laptop the UTC time is pretty close to 10 seconds behind UTC time

    return datetime.utcnow() + timedelta(seconds=10)


def test_rtcm3():

    _log.info("Parsing data/COCO7.dat...")

    with open("data/COCO7.dat", "rb") as f:

        count = 0

        b = None

        while b != b'\xd3':

            b = f.read(1)

            # _log.info("byte [%d] is [%s] which is [%s]", count, binascii.hexlify(b), b)

            count += 1

        _log.info("Found header at byte [%d]", count)

        d = f.read(2)

        _log.info("byte order is [%s]", sys.byteorder)

        _log.info("d is [%s] which is [%d]", binascii.hexlify(d), int.from_bytes(d, "big"))

        len1 = int.from_bytes(d, "big")

        mask = 0x3ff

        len2 = len1 & mask

        _log.info("len1=[%d] len2=[%d]", len1, len2)

        # _log.info("modified d is [%s]", binascii.hexlify(d & b'\x3ff'))

        # _log.info("length is [%d] bits are [%s]", int.from_bytes(d, byteorder=sys.byteorder), bin(d))

        # _log.info("length is [%d]", int.from_bytes(d, byteorder=sys.byteorder))

        # _log.info("bits are [%s]", bin(d))


def test_rtcm3_class_from_file():

    _log.info("Testing RTCM v3 class - reading from captured file...")

    try:

        messages = {}

        _log.info("Starting to read messages...")

        count = 0
        count_skipped = 0

        with open("data/COCO7.2018-07-23.04-12-27.dat", "rb") as f:

            while True:

                length, message_number, crc = RTCM3.get_next_message(f)

                _log.info("frame length=[%s] message number=[%s]", length, message_number)

                if length and message_number and crc:

                    if message_number not in messages:
                        messages[message_number] = 0

                    messages[message_number] += 1
                    count += 1

                else:
                    count_skipped += 1

    except EOFError:
        _log.info("Reached EOF")

    finally:
        _log.info("count=[%d] skipped=[%d]", count, count_skipped)

        _log.info("The message numbers are [%s]", " ".join(str(x) for x in sorted(messages.keys())))

        for msg in sorted(messages.keys()):
            _log.info("\t[% 4d] -> [% 5d]", msg, messages[msg])


def test_rtcm3_class_from_ntrip():

    import requests

    _log.info("Testing RTCM v3 class - reading from NTRIP server...")

    username = "TaoLi1234"
    password = "LGYdOWDnVC"
    url = "http://auscors.ga.gov.au:2101/COCO7"

    # proxies = {"http": "http://localhost:8888"}
    proxies = {}
    proxies = {"http": "http://proxy.ga.gov.au:8080"}

    auth = (username, password)
    headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}

    messages = {}

    with requests.get(url, auth=auth, headers=headers, proxies=proxies, stream=True) as response:

        _log.info("Connected - starting to read messages...")
        
        count = 0
        count_skipped = 0

        while True:

            length, message_number, crc = RTCM3.get_next_message(response.raw)

            _log.info("frame length=[%s] message number=[%s]", length, message_number)

            if length and message_number and crc:

                if message_number not in messages:
                    messages[message_number] = 0

                messages[message_number] += 1
                count += 1

            else:
                count_skipped += 1

            if count >= 10000:
                break

    _log.info("count=[%d] skipped=[%d]", count, count_skipped)

    _log.info("The message numbers are [%s]", " ".join(str(x) for x in sorted(messages.keys())))

    for msg in sorted(messages.keys()):
        _log.info("\t[% 4d] -> [% 5d]", msg, messages[msg])


def test_rtcm3_class_decode_msm_from_file():

    _log.info("Testing RTCM v3 class - decoding MSM messages from captured file...")

    try:

        messages = {}

        epochs = {}
        epochs_dtm = {}

        _log.info("Starting to read messages...")

        count = 0
        count_skipped = 0

        with open("data/COCO7.dat", "rb") as f:

            while True:

                length, message_number, crc, reference_station_id, epoch, epoch_dtm = RTCM3.get_next_message(f)

                current_dtm = get_correct_utc_time()

                _log.info("frame length=[%s] message number=[%s]", length, message_number)

                if length and message_number and crc:

                    if message_number not in messages:
                        messages[message_number] = 0

                    messages[message_number] += 1
                    count += 1

                    if RTCM3.is_msm_message(message_number):

                        _log.info("message number=[%d] epoch=[%d] epoch_dtm=[%s] current_dtm=[%s] latency=[%s]",
                                  message_number, epoch, epoch_dtm, current_dtm, epoch_dtm - current_dtm)

                        if message_number not in epochs:
                            epochs[message_number] = list()
                            epochs_dtm[message_number] = list()

                        epochs[message_number].append(epoch)
                        epochs_dtm[message_number].append(epoch_dtm)
                else:
                    count_skipped += 1

    except EOFError:
            _log.info("Reached EOF")

    finally:

        _log.info("count=[%d] skipped=[%d]", count, count_skipped)

        _log.info("The message numbers are [%s]", " ".join(str(x) for x in sorted(messages.keys())))

        for msg in sorted(messages.keys()):
            _log.info("\t[% 4d] -> [% 5d]", msg, messages[msg])

        for msg in sorted(epochs.keys()):
            _log.info("Message [%d] [%s]", msg, " ".join([str(x) for x in sorted(epochs[msg])]))
            _log.info("Message [%d] [%s]", msg, " ".join([x.strftime("%H:%M:%S") for x in sorted(epochs_dtm[msg])]))


def test_rtcm3_class_decode_msm_from_ntrip():

    import requests

    _log.info("Testing RTCM v3 class - decoding MSM messages from NTRIP stream...")

    username = "TaoLi1234"
    password = "LGYdOWDnVC"
    url = "http://auscors.ga.gov.au:2101/COCO7"

    # proxies = {"http": "http://localhost:8888"}
    proxies = {}
    # proxies = {"http": "http://proxy.ga.gov.au:8080"}
    proxies = {"http": "http://proxy.inno.lan:3128"}

    auth = (username, password)
    headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}

    messages = {}

    epochs = {}
    epochs_dtm = {}

    with requests.get(url, auth=auth, headers=headers, proxies=proxies, stream=True) as response:

        _log.info("Connected - starting to read messages...")

        count = 0
        count_skipped = 0

        while True:

            length, message_number, crc, reference_station_id, epoch, epoch_dtm = RTCM3.get_next_message(response.raw)

            current_dtm = get_correct_utc_time()

            _log.info("frame length=[%s] message number=[%s]", length, message_number)

            if length and message_number and crc:

                if message_number not in messages:
                    messages[message_number] = 0

                messages[message_number] += 1
                count += 1

                if RTCM3.is_msm_message(message_number):

                    _log.info("message number=[%d] epoch=[%d] epoch_dtm=[%s] current_dtm=[%s] latency=[%s]",
                              message_number, epoch, epoch_dtm, current_dtm, current_dtm - epoch_dtm)

                    if message_number not in epochs:
                        epochs[message_number] = list()
                        epochs_dtm[message_number] = list()

                    epochs[message_number].append(epoch)
                    epochs_dtm[message_number].append(epoch_dtm)
            else:
                count_skipped += 1

            if count > 50:
                break

    _log.info("count=[%d] skipped=[%d]", count, count_skipped)

    _log.info("The message numbers are [%s]", " ".join(str(x) for x in sorted(messages.keys())))

    for msg in sorted(messages.keys()):
        _log.info("\t[% 4d] -> [% 5d]", msg, messages[msg])

    for msg in sorted(epochs.keys()):
        _log.info("Message [%d] [%s]", msg, " ".join([str(x) for x in sorted(epochs[msg])]))
        _log.info("Message [%d] [%s]", msg, " ".join([x.strftime("%H:%M:%S") for x in sorted(epochs_dtm[msg])]))


# def test_rtcm3_class_decode_msm_from_ntrip_urllib3():
#
#     import urllib3
#     from urllib3.util import make_headers
#
#     _log.info("Testing RTCM v3 class - decoding MSM messages from NTRIP stream...")
#
#     username = "TaoLi1234"
#     password = "LGYdOWDnVC"
#     url = "http://auscors.ga.gov.au:2101/COCO7"
#
#     proxy = urllib3.ProxyManager("http://proxy.inno.lan:3128")
#
#     auth = (username, password)
#     headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}
#     headers.update(make_headers(basic_auth="{username}:{password}".format(username=username, password=password)))
#
#     messages = {}
#
#     epochs = {}
#     epochs_dtm = {}
#
#     # urllib3.response.HTTPResponse
#
#     with proxy.request(method="GET", url=url, headers=headers, preload_content=False) as response:
#
#         _log.info("status code is [%d]", response.status)
#
#         _log.info("Connected - starting to read messages...")
#
#         count = 0
#         count_skipped = 0
#
#         while True:
#
#             length, message_number, crc, reference_station_id, epoch, epoch_dtm = RTCM3.get_next_message(response)
#
#             current_dtm = get_correct_utc_time()
#
#             _log.info("frame length=[%s] message number=[%s]", length, message_number)
#
#             if length and message_number and crc:
#
#                 if message_number not in messages:
#                     messages[message_number] = 0
#
#                 messages[message_number] += 1
#                 count += 1
#
#                 if RTCM3.is_msm_message(message_number):
#
#                     _log.info("message number=[%d] epoch=[%d] epoch_dtm=[%s] current_dtm=[%s] latency=[%s]",
#                               message_number, epoch, epoch_dtm, current_dtm, current_dtm - epoch_dtm)
#
#                     if message_number not in epochs:
#                         epochs[message_number] = list()
#                         epochs_dtm[message_number] = list()
#
#                     epochs[message_number].append(epoch)
#                     epochs_dtm[message_number].append(epoch_dtm)
#             else:
#                 count_skipped += 1
#
#             if count > 50:
#                 break
#
#     _log.info("count=[%d] skipped=[%d]", count, count_skipped)
#
#     _log.info("The message numbers are [%s]", " ".join(str(x) for x in sorted(messages.keys())))
#
#     for msg in sorted(messages.keys()):
#         _log.info("\t[% 4d] -> [% 5d]", msg, messages[msg])
#
#     for msg in sorted(epochs.keys()):
#         _log.info("Message [%d] [%s]", msg, " ".join([str(x) for x in sorted(epochs[msg])]))
#         _log.info("Message [%d] [%s]", msg, " ".join([x.strftime("%H:%M:%S") for x in sorted(epochs_dtm[msg])]))
#

def test_dumping():

    import requests
    import pickle

    username = "TaoLi1234"
    password = "LGYdOWDnVC"
    url = "http://auscors.ga.gov.au:2101/COCO7"

    # proxies = {"http": "http://localhost:8888"}
    proxies = {}
    # proxies = {"http": "http://proxy.ga.gov.au:8080"}
    proxies = {"http": "http://proxy.inno.lan:3128"}

    auth = (username, password)
    headers = {"User-Agent": "NTRIP ACS_PYTHON/0.1", "Ntrip-Version": "Ntrip/2.0"}

    with requests.get(url, auth=auth, headers=headers, proxies=proxies, stream=True) as response:

        _log.info("Connected - starting to read messages...")

        count = 0
        count_skipped = 0

        with open("data/dump.dat", "wb") as f:

            for data in response.iter_content(chunk_size=1024):

                tstamp = datetime.utcnow()

                _log.info("count=[%d] size=[%d] data=[%s]", count, len(data), data)

                # _log.info("timestamp=[%s] data=[%s] will write=[%s]", tstamp, data, pickle.dumps((tstamp, data)))

                f.write(pickle.dumps((tstamp, data)))

                count += 1

                # if data:
                #     _log.info("count=[%d] size=[%d] data=[%s]", count, len(data), data)
                #     _log.info("timestamp=[%s] data=[%s] will write=[%s]", datetime.utcnow(), data, pickle.dumps(()))
                #     f.write(pickle.dumps((tstamp, data)))
                #     count += 1

                if count > 1000:
                    break

    _log.info("done")

    _log.info("response is [%s]", response)


# dump_packet("D3 00 13 3E D7 D3 02 02 98 0E DE EF 34 B4 BD 62 AC 09 41 98 6F 33 36 0B 98")
#
# dump_packet("d3 74 63 18 d1 94 65 0d 43 4c 53 14 b1 2c 4f")
#
# dump_packet("d3 f4 73 1c c6 d4 7d 1f 0e 43 74 2d 0b 37 cd c0 00 00 00 31 7b 67 13 d1 eb 78 9e c1 bc aa 96 76 73 63 66 ea cf 42 c0 71 5c ff 07 ad 2d 78 ae 9f b9 a6 59 29 4b a9 de dc 7a c5 fb 87 f6 f7 f0 97 e0 ee aa fd 55 7a be f4 ce ed e1 db 98 34 a0 68 61 05 a1 a9 44 d4 09 a9 13 54 25 ff d7 1f ae b1 db a3 b5 c7 f0 8b c2 17 92 30 94 63 59 19 d2 34 43 b0 87 5f 80 9f 4c 3a")
#
# dump_packet("d3 01 00 43 f0 00 24 91 40 22 00 28 60 90 03 80 00 00 00 00 20 c0 00 00 7f ff f9 1d 29 35 31 24 fd 16 0f 5d 2a 54 a5 ff 15 3c ae 3c 7b 8b 04 af fa 10 6f be 83 0b d8 05 cf 71 36 30 7f 6d c6 f6 ea 6c 10 0e 41 96 f4 19 2d 8b 41 64 b9 6d 0b 92 db f4 88 00 07 58 00 1e 81 8d 5c 24 d6 82 3c 4f cf 05 3d 97 6b da 54 b3 f0 b3 44 bc bd 74 1a 7d 8a 8b 3d 96 50 80 5d 65 40 7b 60 40 5d 08 42 aa 3d 42 bd ce 82 b8 c6 c0 1c f3 40 2e 22 00 27 63 80 34 2a 40 4d d6 c0 42 0c 7f 33 36 7f 44 58 7f 42 ab 3c f1 4b fd 33 28 66 d9 b6 6d 91 a4 69 0a 81 a0 68 18 9a 25 88 e7 69 be 6f 99 e6 59 92 3f 8b 40 00 02 c0 a7 a9 ab 62 a9 ab 27 ca 3e 93 ac 29 2a 36 e6 b4 ed dc ee e8 b9 6c 08 f0 49 f8 97 71 07 5b 3f 36 5e 6d 37 00 4c 04 f8 09 fe d2 2d b0 1b 59 c2 50 84 d2 08 e1 e9 7b d1 0f ad 6f 06 1e c0 40 2a 9d 26")
#
#def dump_packet(s):
#
#    length, message_number, crc = RTCM3.get_next_message(io.BytesIO(bytearray.fromhex(s)))
#
#    _log.info("frame length=[%s] message number=[%s]", length, message_number)

