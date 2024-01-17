""" This script determines the dependency files that are required
    to post-process a given static RINEX obs file with Ginan, and
    downloads them from an appropriate repository.

    These include the Earth Rotation Parameter file (ERP), the orbit
    file (SP3) and the clock file (CLK).
"""
from datetime import date, timedelta
from pathlib import Path
from parse_rinex_header import RinexHeader

import logging

from gnssanalysis.gn_download import ftp_tls_cddis, download_prod
from gnssanalysis.gn_datetime import dt2gpswk


logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
logging.getLogger().setLevel(logging.DEBUG)


def download(header: RinexHeader, target_dir: Path) -> dict:
    start_epoch = header.first_obs_time
    last_epoch = header.last_obs_time
    start_date = date(start_epoch.year, start_epoch.month, start_epoch.day)

    # TODO: CORS files won't have an end date - need to handle this eventually
    end_date = date(last_epoch.year, last_epoch.month, last_epoch.day)

    dates = list(daterange(start_date, end_date))

    # TODO: Handle rinex files that are split over two gps weeks
    gps_week = dt2gpswk(header.first_obs_time)

    # gnssanalysis expects strings for directories
    dwn_dir = str(target_dir)

    with ftp_tls_cddis() as ftps:
        daily_files = download_prod(
            dates, dwn_dir, ac="igs", f_type=["sp3", "clk"], dwn_src="cddis", f_dict=True, ftps=ftps
        )
        weekly_files = download_prod(
            dates, dwn_dir, ac="igs", f_type=["erp"], wkly_file=True, dwn_src="cddis", f_dict=True, ftps=ftps
        )

    return daily_files, weekly_files


def daterange(start_date, end_date):
    for i in range(int((end_date - start_date).days) + 1):
        yield start_date + timedelta(i)
