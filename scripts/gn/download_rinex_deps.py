""" This script determines the dependency files that are required
    to post-process a given static RINEX obs file with Ginan, and
    downloads them from an appropriate repository.

    These include the Earth Rotation Parameter file (ERP), the orbit
    file (SP3) and the clock file (CLK).
"""

from datetime import date as _date
from datetime import timedelta
from pathlib import Path
from .parse_rinex_header import RinexHeader
import pandas as pd

from gnssanalysis.filenames import generate_IGS_long_filename
from gnssanalysis.gn_download import download_multiple_files_from_cddis
from gnssanalysis.gn_datetime import dt2gpswk


def download(header: RinexHeader, target_dir: Path) -> None:
    """Using information from the given header, downloads the required
    files for Ginan to be able to run in PPP static mode.

    :param header: Parsed header from the input rinex file
    :param target_dir: Path to the target directory
    """
    start_date = pd.to_datetime(header.first_obs_time).date()
    end_date = pd.to_datetime(header.last_obs_time).date()
    _download_static_dependencies(start_date, end_date, target_dir)


def _download_static_dependencies(start_date: _date, end_date: _date, target_dir: Path):
    """Downloads dependencies from IGS for static ppp between start date and end date.

    :param start_date: First date to download for
    :param end_date: Last date to download for
    :param target_dir: Target directory to download files to
    """
    for date in daterange(start_date, end_date):
        files = _get_static_long_filenames(date)

        gps_week = dt2gpswk(date)
        ftp_folder = f"gnss/products/{gps_week}"

        download_multiple_files_from_cddis(files, ftp_folder, target_dir)


def _get_static_long_filenames(date: _date) -> [str]:
    """Deterimines the long filenames for sp3, erp and clk files for a given date,
    which will allow ginan to run ppp static.

    :param date: Date to determine filenames for
    :returns [str] of filenames
    """
    # TODO: Support more than just IGS FIN products

    # Weekly files
    week = timedelta(7)

    def _first_day_of_week(date: _date):
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


def daterange(start_date: _date, end_date: _date = None):
    """Generator for dates between start and end"""
    for i in range(int((end_date - start_date).days) + 1):
        yield start_date + timedelta(i)
