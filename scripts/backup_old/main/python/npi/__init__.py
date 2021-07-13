from datetime import timedelta, datetime
from enum import Enum


def get_correct_utc_time():

    return datetime.utcnow()


# TODO - rename this?

class InputType(Enum):
    NTRIP = "NTRIP"


# TODO - rename this?

class InputFormat(Enum):
    RTCM3 = "RTCM3"


class InputProtocol(Enum):
    HTTP = "HTTP"


class EpochDuration(Enum):
    MINUTE = timedelta(minutes=1)
    MINUTE_15 = timedelta(minutes=15)
    HOUR = timedelta(hours=1)
