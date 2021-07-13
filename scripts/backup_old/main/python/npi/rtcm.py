import logging
from datetime import datetime, timedelta

from bitstring import BitArray

from npi import get_correct_utc_time
from npi.crc24q import crc24q

_log = logging.getLogger(__name__)


class RTCM3:

    @staticmethod
    def skip_to_frame(s):

        b = None

        while b != b'\xd3':

            b = s.read(1)

            if not b:
                _log.debug("Reached EOF")
                raise EOFError

        return True

    @staticmethod
    def read_frame_length(s):

        # Read 2 bytes

        d = s.read(2)

        if len(d) < 2:
            return None

        # Length is 10-bits from the 2-bytes

        return int.from_bytes(d, "big") & 0x3ff

    @staticmethod
    def read_frame_data(s, length):

        return s.read(length)

    @staticmethod
    def read_frame_crc(s):

        return s.read(3)

    @staticmethod
    def extract_message_number(data):

        bits = BitArray(data[:2])

        return bits[:12].uint

    # @staticmethod
    # def extract_1005(data):
    #
    #     bits = BitArray(data[:2])
    #
    #     return bits[:12].uint

    @staticmethod
    def get_next_message(s):

        length = message_number = crc = None

        bad = None, None, None, None, None, None, None

        if not RTCM3.skip_to_frame(s):

            return bad

        length = RTCM3.read_frame_length(s)

        if not length:
            return bad

        data = RTCM3.read_frame_data(s, length)

        if not data or len(data) < length:
            return bad

        crc = RTCM3.read_frame_crc(s)

        if not crc:
            return bad

        # check CRC

        if not RTCM3.valid_crc(length, data, crc):
            return bad

        # extract message type from data

        message_number = RTCM3.extract_message_number(data)

        reference_station_id = epoch = epoch_dtm = None

        if RTCM3.is_msm_message(message_number):

            _log.debug("Decoding MSM message [%d]", message_number)

            reference_station_id = RTCM3.extract_msm_reference_station_id(data)

            epoch, epoch_dtm = RTCM3.extract_msm_gnss_epoch_time(message_number, data)

        return length, message_number, crc, reference_station_id, epoch, epoch_dtm, data

    @staticmethod
    def valid_crc(length, data, crc):

        return RTCM3.generate_crc(bytearray.fromhex("D3") + bytearray((length).to_bytes(2, "big", signed=False)) + bytearray(data)) == crc

    @staticmethod
    def generate_crc(data):

        return bytearray(crc24q(data).to_bytes(3, "big", signed=False))

    @staticmethod
    def is_msm_message(message_number):

        return message_number in [1077, 1087, 1097, 1117, 1127]

    @staticmethod
    def extract_msm_reference_station_id(data):

        bits = BitArray(data[1:3])

        reference_station_id = bits[4:].uint

        _log.debug("Reference station id=[%d]", reference_station_id)

        return reference_station_id

    @staticmethod
    def extract_msm_gnss_epoch_time(message_number, data):

        def get_midnight_saturday_sunday():

            d = get_midnight()

            # Map back to previous Sunday

            d -= timedelta(days=d.isoweekday())

            return d

        def get_midnight():

            # NOW (UTC)

            d = get_correct_utc_time()

            # Set to midnight

            d = d.replace(hour=0, minute=0, second=0, microsecond=0)

            return d

        # GPS (1077)

        def extract_msm_gnss_epoch_time_1077(data):

            bits = BitArray(data[3:7])

            tow = bits[:30].uint

            d = get_midnight_saturday_sunday()

            _log.debug("\t*** start of week=[%s]", d)

            d += timedelta(milliseconds=tow)

            _log.debug("\t*** adding tow=[%s]", d)

            # GPS time is currently 18 seconds different due to leap seconds

            d -= timedelta(seconds=18)

            _log.debug("\t*** taking off leap seconds=[%s]", d)

            return tow, d

        # GLONASS (1087)

        def extract_msm_gnss_epoch_time_1087(data):

            bits = BitArray(data[3:7])

            dow = bits[:3].uint

            epoch = bits[3:30].uint

            d = get_midnight()

            # NOTE  : dow from data is  0=Sunday, 1=Monday,  ..., 6=Saturday, 7=UNKNOWN
            #       : d.weekday() is    0=Monday, 1=Tuesday, ..., 6=Sunday

            current_glonass_dow = d.weekday() + 1

            _log.info("\tbits=[%s]=[%d] dow=[%d] epoch=[%d] current_glonass_dow=[%d]", bits, bits.uint, dow, epoch, current_glonass_dow)

            # TODO edge cases and weird shit - and note this only works for real-time reception

            # Do I need to adjust the day???

            if dow == 7:
                _log.warning("Unknown day-of-week received in epoch field (epoch bits=[%s] dow=[%d] tod=[%d]", bits, dow, epoch)

            elif dow != current_glonass_dow:
                _log.info("Need to adjust the day - dow=[%d] current glonass dow=[%d]", dow, current_glonass_dow)
                d += timedelta(days=dow-current_glonass_dow)

            d += timedelta(milliseconds=epoch)

            # GLONASS time is 3 hours different

            d -= timedelta(hours=3)

            return epoch, d

        # GALILEO (1097)

        def extract_msm_gnss_epoch_time_1097(data):

            bits = BitArray(data[3:7])

            tow = bits[:30].uint

            d = get_midnight_saturday_sunday()

            d += timedelta(milliseconds=tow)

            # GST time is currently 18 seconds different due to leap seconds

            d -= timedelta(seconds=18)

            return tow, d

        # QZSS (1117)

        def extract_msm_gnss_epoch_time_1117(data):

            bits = BitArray(data[3:7])

            tow = bits[:30].uint

            d = get_midnight_saturday_sunday()

            d += timedelta(milliseconds=tow)

            # QZSS time is currently 37-19 seconds different due to leap seconds

            d -= timedelta(seconds=18)

            return tow, d

        # BEIDOU (BDS) (1127)

        def extract_msm_gnss_epoch_time_1127(data):

            bits = BitArray(data[3:7])

            tow = bits[:30].uint

            d = get_midnight_saturday_sunday()

            d += timedelta(milliseconds=tow)

            # BDS time is currently 4 seconds different due to leap seconds

            d -= timedelta(seconds=4)

            return tow, d

        METHODS = {1077: extract_msm_gnss_epoch_time_1077, 1087: extract_msm_gnss_epoch_time_1087, 1097: extract_msm_gnss_epoch_time_1097, 1117: extract_msm_gnss_epoch_time_1117, 1127: extract_msm_gnss_epoch_time_1127}

        if message_number in METHODS:
            return METHODS[message_number](data)

        _log.error("Unsupported MSM message")

        return None
