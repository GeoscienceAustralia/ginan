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

from gnssanalysis.filenames import generate_IGS_long_filename
from gnssanalysis.gn_download import download_multiple_files_from_cddis
from gnssanalysis.gn_datetime import dt2gpswk


logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
logging.getLogger().setLevel(logging.DEBUG)


def download(header: RinexHeader, target_dir: Path) -> dict:
    start_epoch = header.first_obs_time
    last_epoch = header.last_obs_time
    start_date = date(start_epoch.year, start_epoch.month, start_epoch.day)

    # TODO: CORS files won't have an end date - need to handle this eventually
    end_date = date(last_epoch.year, last_epoch.month, last_epoch.day)

    _download_static_dependencies(start_date, end_date, target_dir)


def _download_static_dependencies(start_date: date, end_date: date, target_dir: Path):
    for date in daterange(start_date, end_date):
        files = _get_static_long_filenames(date)

        gps_week = dt2gpswk(date)
        ftp_folder = f"gnss/products/{gps_week}"

        download_multiple_files_from_cddis(files, ftp_folder, target_dir)


def _get_static_long_filenames(date: date) -> [str]:
    # TODO: Support more than just IGS FIN products

    # Weekly files
    week = timedelta(7)

    def _first_day_of_week(date: date):
        return date + timedelta(days=-(date.weekday() + 1))

    first_day_of_week = _first_day_of_week(date)
    erp_filename = generate_IGS_long_filename(
        analysis_center="IGS",
        content_type="ERP",
        format_type="ERP",
        start_epoch=first_day_of_week,
        timespan=week,
        solution_type="FIN",
        sampling_rate="01D",
        project="OPS",
    )

    # Daily files
    day = timedelta(1)
    sp3_filename = generate_IGS_long_filename(
        analysis_center="IGS",
        content_type="ORB",
        format_type="SP3",
        start_epoch=date,
        timespan=day,
        solution_type="FIN",
        sampling_rate="15M",
        project="OPS",
    )

    clk_filename = generate_IGS_long_filename(
        analysis_center="IGS",
        content_type="CLK",
        format_type="CLK",
        start_epoch=date,
        timespan=day,
        solution_type="FIN",
        sampling_rate="05M",
        project="OPS",
    )

    files = [erp_filename, sp3_filename, clk_filename]
    compressed_files = [f + ".gz" for f in files]
    return compressed_files


def daterange(start_date: date, end_date: date = None):
    for i in range(int((end_date - start_date).days) + 1):
        yield start_date + timedelta(i)
