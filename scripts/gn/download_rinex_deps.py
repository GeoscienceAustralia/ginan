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

# This import is a hack to allow the import of auto_download_PPP from the
# parent directory. Some options to refactor:
# 1. Move the code from this module into the parent directory and make it like any other script
# 2. Move the auto_download code that this module depends on into gnssanalysis
# 3. Leave it like this for now
import sys
sys.path.append("..")

from auto_download_PPP import auto_download


def download(header: RinexHeader, target_dir: Path) -> None:
    """Using information from the given header, downloads the required
    files for Ginan to be able to run in PPP static mode.

    :param header: Parsed header from the input rinex file
    :param target_dir: Path to the target directory
    """
    start_date = pd.to_datetime(header.first_obs_time).date()
    # Add one day because auto_download does not allow the same date to be given as start and end
    end_date = pd.to_datetime(header.last_obs_time).date() + pd.Timedelta("1 day")
    _download_static_dependencies(start_date, end_date, target_dir)


def _download_static_dependencies(start_date: _date, end_date: _date, target_dir: Path):
    """Downloads dependencies from IGS for static ppp between start date and end date.

    :param start_date: First date to download for
    :param end_date: Last date to download for
    :param target_dir: Target directory to download files to
    """
    auto_download(
        target_dir,
        preset=None,
        station_list=None,
        start_datetime=start_date.strftime("%Y-%m-%d"),
        end_datetime=end_date.strftime("%Y-%m-%d"),
        replace=True,
        dont_replace=False,
        most_recent=False,
        analysis_center="IGS",
        atx=False,
        aload=False,
        igrf=False,
        egm=False,
        oload=False,
        opole=False,
        fes=False,
        planet=False,
        sat_meta=False,
        yaw=False,
        snx=False,
        nav=False,
        sp3=True,
        erp=True,
        clk=True,
        bia=False,
        gpt2=False,
        rinex_data_dir=None,
        trop_dir=None,
        model_dir=None,
        solution_type="FIN",
        project_type="OPS",
        rinex_file_period=None,
        bia_ac=None,
        iau2000=False,
        datetime_format="%Y-%m-%d",
        data_source=None,
        verbose=True,
    )
