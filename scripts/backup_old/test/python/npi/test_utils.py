import logging

from datetime import datetime, timedelta

logging.basicConfig(level=logging.INFO)

_log = logging.getLogger(__name__)


def test():

    _log.info("Test starting...")

    d = datetime.utcnow()

    for day in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:

        d2 = d + timedelta(days=day)

        _log.info("date=[%s] dow=[%d] iso dow=[%d] ... [%d]", d2.strftime("%A %Y-%m-%d %H:%M:%S.%f"), d2.weekday(), d2.isoweekday(), d2.isoweekday())

        d2 -= timedelta(days=d2.isoweekday())

        d2 = d2.replace(hour=0, minute=0, second=0, microsecond=0)

        _log.info("\t[%s]", d2.strftime("%A %Y-%m-%d %H:%M:%S.%f"))