import gzip, hashlib, os, shutil, unlzw3, requests
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup, SoupStrainer
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Callable, Dict, Generator, List

from scripts.GinanUI.app.utils.cddis_email import get_netrc_auth
from scripts.GinanUI.app.utils.common_dirs import INPUT_PRODUCTS_PATH
from scripts.GinanUI.app.utils.gn_functions import GPSDate
from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.models.archive_manager import restore_from_archive

BASE_URL = "https://cddis.nasa.gov/archive"
GPS_ORIGIN = np.datetime64("1980-01-06 00:00:00")  # Magic date from gn_functions
MAX_RETRIES = 3  # download attempts
CHUNK_SIZE = 8192  # 8 KiB
COMPRESSED_FILETYPE = (".gz", ".gzip", ".Z")  # ignore any others (maybe add crx2rnx using hatanaka package)

# repro3 fallback constants
REPRO3_PROJECT = "R03"  # Project code for reproduction products
REPRO3_4TH_CHAR_RANGE = range(10)  # 4th character can be 0-9, prioritise lower numbers
REPRO3_PRIORITY_GPS_WEEK_START = 730  # Start of GPS week range where repro3 products are better quality
REPRO3_PRIORITY_GPS_WEEK_END = 2138  # End of GPS week range where repro3 products are better quality

CONSTELLATION_MAP = {
    'G': 'GPS',
    'R': 'GLO',
    'E': 'GAL',
    'C': 'BDS',
    'J': 'QZS',
}

METADATA = [
    "https://files.igs.org/pub/station/general/igs_satellite_metadata.snx",
    "https://files.igs.org/pub/station/general/igs20.atx",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/OLOAD_GO.BLQ.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/ALOAD_GO.BLQ.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/igrf14coeffs.txt.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/opoleloadcoefcmcor.txt.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/fes2014b_Cnm-Snm.dat.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/DE436.1950.2050.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/gpt_25.grd.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/bds_yaw_modes.snx.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/qzss_yaw_modes.snx.gz",
    "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/sat_yaw_bias_rate.snx.gz",
    "https://datacenter.iers.org/data/latestVersion/finals.data.iau2000.txt"
]


CHECKSUM_FILENAME = "SHA512SUMS"
# File types that should be validated against SHA512SUMS
CHECKSUM_VALIDATED_FORMATS = {"SP3", "BIA", "CLK", "SNX"}


def date_to_gpswk(date: datetime) -> int:
    return int(GPSDate(np.datetime64(date)).gpswk)


def gpswk_to_date(gps_week: int, gps_day: int = 0) -> datetime:
    return GPSDate(GPS_ORIGIN + np.timedelta64(gps_week, "W") + np.timedelta64(gps_day, "D")).as_datetime

def check_repro3_exists(gps_week: int, session: requests.Session = None) -> bool:
    """
    Check if the /repro3/ directory exists for a given GPS week.

    :param gps_week: GPS week number
    :param session: Authenticated requests session for CDDIS access
    :returns: True if /repro3/ directory exists, False otherwise
    """
    url = f"https://cddis.nasa.gov/archive/gnss/products/{gps_week}/repro3/"
    try:
        if session is None:
            # Create authenticated session if not provided
            session = requests.Session()
            session.auth = get_netrc_auth()
        resp = session.get(url, timeout=10, allow_redirects=True)
        # CDDIS returns 200 for valid directories, check for content indicating it's a directory listing
        return resp.status_code == 200
    except requests.RequestException:
        return False

def _is_in_repro3_priority_range(start_time: datetime, end_time: datetime) -> bool:
    """
    Check if the time range falls within the GPS week range where repro3 products
    Prioritise if so (GPSWeeks 730 - 2138 inclusive).

    :param start_time: the start of the time window
    :param end_time: the end of the time window
    :returns: True if ALL GPS weeks in the range are within 730-2138 inclusive
    """
    start_week = date_to_gpswk(start_time)
    end_week = date_to_gpswk(end_time)

    # Check if entire range is within the priority range
    return (REPRO3_PRIORITY_GPS_WEEK_START <= start_week <= REPRO3_PRIORITY_GPS_WEEK_END and
            REPRO3_PRIORITY_GPS_WEEK_START <= end_week <= REPRO3_PRIORITY_GPS_WEEK_END)


def get_repro3_product_dataframe(start_time: datetime, end_time: datetime, target_files: List[str] = None) -> pd.DataFrame:
    """
    Retrieves a DataFrame of available products from the /repro3/ directory for given time window.
    This is used as a fallback when no valid PPP providers are found in the main directory.

    Note: The 4th character in /repro3/ filenames can be 0-9 (not just 0).
    We prioritise lower numbers (0, then 1, then 2, etc.) for each provider
    as they are provider-defined and completely arbitrary

    :param start_time: the start of the time window (start_epoch)
    :param end_time: the start of the time window (end_epoch)
    :param target_files: list of target files to filter for, defaulted to ["CLK","BIA","SP3"]
    :returns: dataframe of products, columns: "analysis_center", "project", "date", "solution_type", "period",
    "resolution", "content", "format"
    """
    if target_files is None:
        target_files = ["CLK", "BIA", "SP3"]
    else:
        target_files = [file.upper() for file in target_files]

    products = pd.DataFrame(
        columns=["analysis_center", "project", "date", "solution_type", "period", "resolution", "content", "format", "_4th_char"])

    gps_weeks = range(date_to_gpswk(start_time), date_to_gpswk(end_time) + 1)

    # Create authenticated session for CDDIS access
    session = requests.Session()
    session.auth = get_netrc_auth()

    for gps_week in gps_weeks:
        # Check if repro3 directory exists for this week
        if not check_repro3_exists(gps_week, session):
            Logger.console(f"/repro3/ directory does not exist for GPS week {gps_week}, skipping")
            continue

        url = f"https://cddis.nasa.gov/archive/gnss/products/{gps_week}/repro3/"
        try:
            week_files = session.get(url, timeout=10)
            week_files.raise_for_status()
        except requests.RequestException as e:
            Logger.console(f"Failed to fetch /repro3/ files for GPS week {gps_week}: {e}")
            continue

        soup = BeautifulSoup(week_files.content, "html.parser", parse_only=SoupStrainer("div", class_="archiveItemTextContainer"))

        for div in soup:
            filename = div.get_text().split(" ")[0]
            try:
                # repro3 uses the modern format (post week 2237 style)
                # e.g. COD0R03FIN_20232620000_01D_01D_OSB.BIA.gz
                # AAA#PPPSNX_YYYYDDDHHMM_LEN_SMP_CNT.FMT.gz
                # where # is the 4th character (can be 0-9)
                center = filename[0:3]  # e.g. "COD"
                fourth_char = filename[3]  # e.g. "0", "1", "2", etc.
                project = filename[4:7]  # e.g. "R03"
                _type = filename[7:10]  # e.g. "FIN"
                year = int(filename[11:15])  # e.g. "2023"
                day_of_year = int(filename[15:18])  # e.g. "262"
                hour = int(filename[18:20])  # e.g. "00"
                minute = int(filename[20:22])  # e.g. "00"
                intended_period = filename[23:26]  # eg "01D"
                sampling_resolution = filename[27:30]  # eg "01D"
                content = filename[31:34]  # e.g. "OSB"
                _format = filename[35:38]  # e.g. "BIA"

                date = datetime(year, 1, 1, hour, minute) + timedelta(day_of_year - 1)
                period = timedelta(days=int(intended_period[:-1]))  # Assuming all periods are in days

                # Check if product's coverage period overlaps with the requested time window
                product_end = date + period
                if _format in target_files and date < end_time and product_end > start_time:
                    products.loc[len(products)] = {
                        "analysis_center": center,
                        "project": project,
                        "date": date,
                        "solution_type": _type,
                        "period": period,
                        "resolution": sampling_resolution,
                        "content": content,
                        "format": _format,
                        "_4th_char": fourth_char
                    }
            except (ValueError, IndexError):
                # Skips md5 sums and other non-conforming files
                continue

    if products.empty:
        return products

    # Prioritise lower values 4th character values for each (center, project, date, solution_type, format) combination
    products = products.sort_values(by=["analysis_center", "project", "date", "solution_type", "format", "_4th_char"])
    products = products.drop_duplicates(subset=["analysis_center", "project", "date", "solution_type", "format"], keep="first")
    products = products.drop(columns=["_4th_char"])
    products = products.reset_index(drop=True)

    return products


def str_to_datetime(date_time_str):
    """
    :param date_time_str: YYYY-MM-DD_HH:mm:ss
    :returns datetime: datetime.strptime()
    """
    try:
        # YYYY-dddHHmm format through datetime.strptime(date_time,"%Y%j%H%M")
        return datetime.strptime(date_time_str, "%Y-%m-%d_%H:%M:%S")
    except ValueError:
        raise ValueError("Invalid datetime format. Use YYYY-MM-DDTHH:MM (e.g. 2025-05-01_00:00:00)")


def get_product_dataframe(start_time: datetime, end_time: datetime, target_files: List[str] = None) -> pd.DataFrame:
    """
    Retrieves a DataFrame of available products for given time window and target files from CDDIS archive.
    Filter the DataFrame (i.e. for a specific ppp provider) then use download_products() to download the files.

    :param start_time: the start of the time window (start_epoch)
    :param end_time: the start of the time window (end_epoch)
    :param target_files: list of target files to filter for, defaulted to ["CLK","BIA","SP3"]
    :returns: dataframe of products, columns: "analysis_center", "project", "date", "solution_type", "period",
    "resolution", "content", "format"
    """
    if target_files is None:
        target_files = ["CLK", "BIA", "SP3"]
    else:
        target_files = [file.upper() for file in target_files]

    products = pd.DataFrame(
        columns=["analysis_center", "project", "date", "solution_type", "period", "resolution", "content", "format", "_4th_char"])

    # 1. Retrieve available options
    gps_weeks = range(date_to_gpswk(start_time), date_to_gpswk(end_time) + 1)
    for gps_week in gps_weeks:
        url = f"https://cddis.nasa.gov/archive/gnss/products/{gps_week}/"
        try:
            week_files = requests.get(url, timeout=10)
            week_files.raise_for_status()
        except requests.RequestException as e:
            raise requests.RequestException(f"Failed to fetch files for GPS week {gps_week}: {e}")

        # 2. Extract data from available options
        soup = BeautifulSoup(week_files.content, "html.parser",
                             parse_only=SoupStrainer("div", class_="archiveItemTextContainer"))
        # Above SoupStrainer makes it that only relevant items are stored in memory
        for div in soup:
            filename = div.get_text().split(" ")[0]
            try:
                if gps_week < 2237:
                    # Format convention changed in week 2237
                    # AAAWWWWD.TYP.Z
                    center = filename[0:3].upper()  # e.g. "COD"
                    _type = "FIN"  # pre-2237 were probably always final solutions :shrug:
                    day = int(filename[7])  # e.g. "0", 0-indexed, 7 indicates weekly
                    _format = filename[9:12].upper()  # e.g. "snx", "ssc", "sum", "erp"
                    project = "OPS"
                    sampling_resolution = None
                    content = None
                    fourth_char = None
                    date = gpswk_to_date(gps_week)
                    if 0 < day < 7:
                        date += timedelta(days=day)
                        period = timedelta(days=1)
                    else:
                        period = timedelta(days=7)

                else:
                    # e.g. GRG0OPSFIN_20232620000_01D_01D_SOL.SNX.gz
                    # AAA#PPPSNX_YYYYDDDHHMM_LEN_SMP_CNT.FMT.gz
                    # where # is the 4th character (can be 0-9)
                    center = filename[0:3]  # e.g. "COD"
                    fourth_char = filename[3]  # e.g. "0", "1", "2", etc.
                    project = filename[4:7]  # e.g. "OPS" or "RNN" unused
                    _type = filename[7:10]  # e.g. "FIN"
                    year = int(filename[11:15])  # e.g. "2023"
                    day_of_year = int(filename[15:18])  # e.g. "262"
                    hour = int(filename[18:20])  # e.g. "00"
                    minute = int(filename[20:22])  # e.g. "00"
                    intended_period = filename[23:26]  # eg "01D"
                    sampling_resolution = filename[27:30]  # eg "01D"
                    content = filename[31:34]  # e.g. "SOL"
                    _format = filename[35:38]  # e.g. "SNX"

                    date = datetime(year, 1, 1, hour, minute) + timedelta(day_of_year - 1)
                    period = timedelta(days=int(intended_period[:-1]))  # Assuming all periods are in days :shrug:

                # Check if product's coverage period overlaps with the requested time window
                product_end = date + period
                if _format in target_files and date < end_time and product_end > start_time:
                    products.loc[len(products)] = {
                        "analysis_center": center,
                        "project": project,
                        "date": date,
                        "solution_type": _type,
                        "period": period,
                        "resolution": sampling_resolution,
                        "content": content,
                        "format": _format,
                        "_4th_char": fourth_char
                    }
            except (ValueError, IndexError):
                # Skips md5 sums and other non-conforming files
                continue

    if products.empty:
        return products

    # Prioritise lower 4th character values for each (center, project, date, solution_type, format) combination
    # This is consistent with the repro3 logic where lower numbers are preferred
    products = products.sort_values(by=["analysis_center", "project", "date", "solution_type", "format", "_4th_char"])
    products = products.drop_duplicates(subset=["analysis_center", "project", "date", "solution_type", "format"], keep="first")
    products = products.reset_index(drop=True)

    return products


def get_valid_analysis_centers(data: pd.DataFrame) -> set[str]:
    """
    Analyzes dataframe for valid analysis centers (those that provide contiguous coverage)
    AND have all required file types (SP3, BIA, CLK) for at least one series.

    :param data: products dataframe, see: get_product_dataframe(), requires columns: "analysis_center", "project",
    "date", "solution_type", "period", "resolution", "content", "format"
    :returns: set of valid analysis centers that have all required files for at least one series
    """
    # Required file types for PPP processing
    REQUIRED_FILES = {"SP3", "BIA", "CLK"}

    # 1. Check for any gaps in coverage and remove
    for (center, _type, _format), group in data.groupby(["analysis_center", "solution_type", "format"]):
        # Time window is filtered for in get_product_dataframe; only need to check they're contiguous
        group = group.sort_values("date").reset_index(drop=True)
        for i in range(len(group) - 1):
            if group.loc[i]["date"] + group.loc[i]["period"] < group.loc[i + 1]["date"]:
                Logger.console(
                    f"Gap detected for {center} {_type} {_format} between {group.loc[i, 'date']} and {group.loc[i + 1, 'date']}")
                data = data[
                    ~((data["analysis_center"] == center) and
                      (data["solution_type"] == _type) and
                      (data["format"] == _format))]

    # 2. Filter for centers that have all required files for at least one series
    valid_centers = set()
    for analysis_center in data["analysis_center"].unique():
        center_data = data[data["analysis_center"] == analysis_center]

        # Check each series to see if it has all required files
        for series in center_data["solution_type"].unique():
            series_data = center_data[center_data["solution_type"] == series]
            available_files = set(series_data["format"].unique())

            # If this series has all required files, the center is valid
            if REQUIRED_FILES.issubset(available_files):
                valid_centers.add(analysis_center)
                break  # No need to check other series for this center

    # 3. Only show series with all required files
    centers = set()
    for analysis_center in sorted(valid_centers):
        centers.add(analysis_center)
        center_products = data.loc[data["analysis_center"] == analysis_center]

        # Build a dict of file types to their available series
        file_series_map = {}
        for _format in center_products["format"].unique():
            series_list = center_products.loc[center_products["format"] == _format, "solution_type"].unique()
            file_series_map[_format] = set(series_list)

        # Find series that have all required files
        valid_series_for_center = set()
        for series in center_products["solution_type"].unique():
            series_data = center_products[center_products["solution_type"] == series]
            available_files = set(series_data["format"].unique())
            if REQUIRED_FILES.issubset(available_files):
                valid_series_for_center.add(series)

        # Build output string showing only complete series
        offerings = ""
        for _format in sorted(REQUIRED_FILES):
            if _format in file_series_map:
                # Only show series that have all three file types
                complete_series = sorted(file_series_map[_format].intersection(valid_series_for_center))
                if complete_series:
                    offerings += f"{_format}:({'/'.join(complete_series)}) "

        Logger.console(f"Analysis centre {analysis_center} offers: {offerings.strip()}")

    return centers


def get_valid_series_for_provider(data: pd.DataFrame, provider: str) -> List[str]:
    """
    Get list of valid series (with all required files) for a specific provider.

    :param data: products dataframe from get_product_dataframe()
    :param provider: analysis center name (e.g., "COD", "GRG")
    :returns: sorted list of valid series codes (e.g., ["FIN", "RAP"])
    """
    REQUIRED_FILES = {"SP3", "BIA", "CLK"}

    # Filter for this provider
    provider_data = data[data["analysis_center"] == provider]

    # Find series that have all required files
    valid_series = []
    for series in provider_data["solution_type"].unique():
        series_data = provider_data[provider_data["solution_type"] == series]
        available_files = set(series_data["format"].unique())

        if REQUIRED_FILES.issubset(available_files):
            valid_series.append(series)

    return sorted(valid_series)

def get_valid_providers_with_series(data: pd.DataFrame) -> dict:
    """
    Get a mapping of providers to their valid series (with all required files).

    :param data: products dataframe from get_product_dataframe()
    :returns: dict mapping provider names to lists of valid series
    """
    provider_series_map = {}

    for provider in data["analysis_center"].unique():
        valid_series = get_valid_series_for_provider(data, provider)
        if valid_series:  # Only include providers with at least one valid series
            provider_series_map[provider] = valid_series

    return provider_series_map

def get_product_dataframe_with_repro3_fallback(start_time: datetime, end_time: datetime, target_files: List[str] = None) -> pd.DataFrame:
    """
    Retrieves a DataFrame of available products, with intelligent handling of repro3 directory
    based on GPS week range:
    - GPS week < 730: Use main directory first, fallback to repro3 if no valid providers
    - 730 <= GPS week <= 2138: Prioritise repro3 (better quality), fallback to main if repro3 unavailable
    - GPS week > 2138: Use main directory first, fallback to repro3 if no valid providers

    :param start_time: the start of the time window (start_epoch)
    :param end_time: the start of the time window (end_epoch)
    :param target_files: list of target files to filter for, defaulted to ["CLK","BIA","SP3"]
    :returns: dataframe of products (from main directory or repro3 depending on GPS week range)
    """
    # Check if time range falls within the repro3 priority range
    if _is_in_repro3_priority_range(start_time, end_time):
        return _try_repro3_first(start_time, end_time, target_files)
    else:
        # Outside priority range: use main directory first, fallback to repro3 if needed
        return _try_main_first(start_time, end_time, target_files)


def _try_main_first(start_time: datetime, end_time: datetime, target_files: List[str] = None) -> pd.DataFrame:
    """
    Try main directory first, fallback to repro3 if no valid PPP providers found.
    Used for GPS weeks outside the repro3 priority range.

    :param start_time: the start of the time window
    :param end_time: the end of the time window
    :param target_files: list of target files to filter for
    :returns: dataframe of products
    """
    # First try the main directory
    products = get_product_dataframe(start_time, end_time, target_files)

    if products.empty:
        Logger.terminal("📦 No products found in main directory, checking /repro3/...")
        return _try_repro3_fallback(start_time, end_time, target_files)

    # Check if we have valid PPP providers
    valid_centers = get_valid_analysis_centers(products)

    if valid_centers:
        # Valid providers found in main directory, no fallback needed
        return products

    # No valid PPP providers found, try repro3 fallback
    Logger.terminal("📦 No valid PPP providers in main directory, checking /repro3/...")
    return _try_repro3_fallback(start_time, end_time, target_files, main_products=products)


def _try_repro3_first(start_time: datetime, end_time: datetime, target_files: List[str] = None) -> pd.DataFrame:
    """
    Try /repro3/ directory first
    fallback to main directory if repro3 is unavailable or has no valid providers.

    :param start_time: the start of the time window
    :param end_time: the end of the time window
    :param target_files: list of target files to filter for
    :returns: dataframe of products
    """
    # Create authenticated session for CDDIS access
    session = requests.Session()
    session.auth = get_netrc_auth()

    # Check if repro3 exists for any of the GPS weeks in the range
    gps_weeks = range(date_to_gpswk(start_time), date_to_gpswk(end_time) + 1)
    repro3_exists_for_any_week = False

    for gps_week in gps_weeks:
        if check_repro3_exists(gps_week, session):
            repro3_exists_for_any_week = True
            break

    if not repro3_exists_for_any_week:
        Logger.terminal("📦 /repro3/ directory does not exist, falling back to main directory...")
        return _try_main_directory_fallback(start_time, end_time, target_files)

    # Fetch products from repro3
    repro3_products = get_repro3_product_dataframe(start_time, end_time, target_files)

    if repro3_products.empty:
        Logger.terminal("📦 No products found in /repro3/ directory, falling back to main directory...")
        return _try_main_directory_fallback(start_time, end_time, target_files)

    # Check if repro3 has valid PPP providers
    repro3_valid_centers = get_valid_analysis_centers(repro3_products)

    if repro3_valid_centers:
        return repro3_products

    # No valid providers in repro3, try main directory as fallback
    Logger.terminal("📦 No valid PPP providers in /repro3/, falling back to main directory...")
    return _try_main_directory_fallback(start_time, end_time, target_files, repro3_products=repro3_products)


def _try_main_directory_fallback(start_time: datetime, end_time: datetime, target_files: List[str] = None, repro3_products: pd.DataFrame = None) -> pd.DataFrame:
    """
    Internal helper to attempt fetching products from main directory as a fallback
    when /repro3/ is prioritised but unavailable.

    :param start_time: the start of the time window
    :param end_time: the end of the time window
    :param target_files: list of target files to filter for
    :param repro3_products: optional existing products from repro3 to return if main fails
    :returns: main products if valid providers found, otherwise repro3_products or empty DataFrame
    """
    products = get_product_dataframe(start_time, end_time, target_files)

    if products.empty:
        if repro3_products is not None and not repro3_products.empty:
            Logger.terminal("⚠️ No valid PPP providers found")
            return repro3_products
        Logger.terminal("⚠️ No valid PPP providers found")
        return pd.DataFrame()

    # Check if main directory has valid PPP providers
    valid_centers = get_valid_analysis_centers(products)

    if valid_centers:
        Logger.terminal(f"✅ Found valid PPP providers in main directory: {', '.join(sorted(valid_centers))}")
        return products

    # No valid providers in main either
    Logger.terminal("⚠️ No valid PPP providers found")
    if repro3_products is not None and not repro3_products.empty:
        return repro3_products
    return products


def _try_repro3_fallback(start_time: datetime, end_time: datetime, target_files: List[str] = None, main_products: pd.DataFrame = None) -> pd.DataFrame:
    """
    Internal helper to attempt fetching products from repro3 directory.

    :param start_time: the start of the time window
    :param end_time: the end of the time window
    :param target_files: list of target files to filter for
    :param main_products: optional existing products from main directory to return if repro3 fails
    :returns: repro3 products if valid providers found, otherwise main_products or empty DataFrame
    """
    # Create authenticated session for CDDIS access
    session = requests.Session()
    session.auth = get_netrc_auth()

    # Check if repro3 exists for any of the GPS weeks in the range
    gps_weeks = range(date_to_gpswk(start_time), date_to_gpswk(end_time) + 1)
    repro3_exists_for_any_week = False

    for gps_week in gps_weeks:
        if check_repro3_exists(gps_week, session):
            repro3_exists_for_any_week = True
            break

    if not repro3_exists_for_any_week:
        Logger.terminal("📦 repro3 directory does not exist for this time range")
        if main_products is not None and not main_products.empty:
            Logger.terminal("⚠️ No valid PPP providers found")
            return main_products
        return pd.DataFrame()

    # Fetch products from repro3
    repro3_products = get_repro3_product_dataframe(start_time, end_time, target_files)

    if repro3_products.empty:
        Logger.terminal("📦 No products found in repro3 directory")
        if main_products is not None and not main_products.empty:
            Logger.terminal("⚠️ No valid PPP providers found")
            return main_products
        return pd.DataFrame()

    # Check if repro3 has valid PPP providers
    repro3_valid_centers = get_valid_analysis_centers(repro3_products)

    if repro3_valid_centers:
        return repro3_products
    else:
        Logger.terminal("⚠️ No valid PPP providers found")
        if main_products is not None and not main_products.empty:
            return main_products
        return repro3_products

def extract_file(filepath: Path, keep_compressed: bool = True) -> Path:
    """
    Extracts [".gz", ".gzip", ".Z"] files with gzip and unlzw3 respectively.
    By default, the compressed file is retained alongside the extracted version
    so that it can be archived and later validated against SHA-512 checksums.

    :param filepath: compressed file path
    :param keep_compressed: if True, retain the compressed file after extraction
    :return: path to extracted file
    """
    finalpath = ".".join(str(filepath).split(".")[:-1])
    if str(filepath.name).endswith((".gz", ".gzip")):
        with gzip.open(filepath, "rb") as f_in, open(finalpath, "wb") as f_out:
            shutil.copyfileobj(f_in, f_out)
    elif str(filepath.name).endswith(".Z"):
        decompressed_data = unlzw3.unlzw(filepath)
        with open(finalpath, "wb") as f_out:
            f_out.write(decompressed_data)
    if not keep_compressed:
        filepath.unlink()
    return Path(finalpath)


# region SHA-512 Checksum Validation

def get_checksum_url(gps_week: int, use_repro3: bool = False) -> str:
    """
    Generate the URL for the SHA512SUMS file for a given GPS week.

    :param gps_week: GPS week number
    :param use_repro3: If True, generate URL for the repro3 subdirectory
    :returns: URL to the SHA512SUMS file
    """
    if use_repro3:
        return f"{BASE_URL}/gnss/products/{gps_week}/repro3/{CHECKSUM_FILENAME}"
    return f"{BASE_URL}/gnss/products/{gps_week}/{CHECKSUM_FILENAME}"

def download_checksum_file(gps_week: int, session: requests.Session, download_dir: Path = INPUT_PRODUCTS_PATH,
                           use_repro3: bool = False, progress_callback: Optional[Callable] = None,
                           stop_requested: Optional[Callable] = None) -> Optional[Path]:
    """
    Download the SHA512SUMS file for a given GPS week if it hasn't been downloaded already.
    The file is saved as SHA512SUMS_{gps_week} (or SHA512SUMS_{gps_week}_repro3) to avoid
    collisions between different weeks.

    :param gps_week: GPS week number
    :param session: Authenticated requests session for CDDIS access
    :param download_dir: Directory to save the checksum file
    :param use_repro3: If True, download from the repro3 subdirectory
    :param progress_callback: Optional callback for progress updates (filename, percent)
    :param stop_requested: Optional callback to check if operation should stop
    :returns: Path to the downloaded checksum file, or None if download failed
    """
    suffix = "_repro3" if use_repro3 else ""
    local_filename = f"{CHECKSUM_FILENAME}_{gps_week}{suffix}"
    local_path = download_dir / local_filename

    # Return cached file if it already exists
    if local_path.exists():
        return local_path

    # Check if file can be restored from the archive
    restored = restore_from_archive(local_filename, download_dir)
    if restored is not None and restored.exists():
        return restored

    url = get_checksum_url(gps_week, use_repro3)
    Logger.terminal(f"📥 Downloading checksum file {CHECKSUM_FILENAME} for GPS week {gps_week}{' (repro3)' if use_repro3 else ''}...")

    for attempt in range(MAX_RETRIES):
        if stop_requested and stop_requested():
            return None

        try:
            resp = session.get(url, stream=True, timeout=30)
            resp.raise_for_status()

            total_size = int(resp.headers.get("content-length", 0))
            downloaded = 0

            os.makedirs(download_dir, exist_ok=True)
            partial_path = local_path.with_suffix(".part")

            with open(partial_path, "wb") as f:
                for chunk in resp.iter_content(chunk_size=CHUNK_SIZE):
                    if stop_requested and stop_requested():
                        partial_path.unlink(missing_ok=True)
                        return None
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
                        if progress_callback and total_size > 0:
                            percent = int(downloaded / total_size * 100)
                            progress_callback(local_filename, percent)

            os.rename(partial_path, local_path)
            Logger.terminal(f"✅ Downloaded checksum file {local_filename}")
            return local_path

        except requests.RequestException as e:
            if attempt < MAX_RETRIES - 1:
                Logger.terminal(f"⚠️ Checksum download attempt {attempt + 1}/{MAX_RETRIES} failed: {e}")
            else:
                Logger.terminal(f"⚠️ Failed to download {CHECKSUM_FILENAME} for GPS week {gps_week} after {MAX_RETRIES} attempts: {e}")

    return None

def parse_checksum_file(checksum_path: Path) -> dict:
    """
    Parse a SHA512SUMS file into a dictionary mapping filenames to their expected SHA-512 hashes.

    The file format is: {128-char hex hash} {filename} (with one or two spaces as separator).

    :param checksum_path: Path to the SHA512SUMS file
    :returns: Dictionary mapping filename -> expected SHA-512 hex digest
    """
    checksums = {}
    try:
        with open(checksum_path, "r", encoding="utf-8", errors="replace") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                # SHA-512 hex digest is 128 characters, followed by space(s) and the filename
                parts = line.split(None, 1)
                if len(parts) == 2 and len(parts[0]) == 128:
                    hex_hash = parts[0].lower()
                    # Validate that the hash is actually valid hexadecimal
                    try:
                        int(hex_hash, 16)
                    except ValueError:
                        Logger.terminal(f"⚠️ Invalid hex hash in SHA512SUMS for {parts[1].strip()}, skipping entry")
                        continue
                    checksums[parts[1].strip()] = hex_hash
    except Exception as e:
        Logger.terminal(f"⚠️ Failed to parse checksum file {checksum_path}: {e}")
    if not checksums:
        Logger.terminal(f"⚠️ No valid checksum entries found in {checksum_path.name}")
    return checksums

def compute_sha512(filepath: Path) -> str:
    """
    Compute the SHA-512 hash of a file.

    :param filepath: Path to the file to hash
    :returns: Hex digest string of the SHA-512 hash
    """
    sha512 = hashlib.sha512()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(CHUNK_SIZE), b""):
            sha512.update(chunk)
    return sha512.hexdigest()

def validate_checksum(filepath: Path, checksums: dict) -> Optional[bool]:
    """
    Validate a downloaded file against its expected SHA-512 checksum.

    :param filepath: Path to the compressed file to validate
    :param checksums: Dictionary from parse_checksum_file() mapping filename -> expected hash
    :returns: True if checksum matches, False if mismatch, None if filename not found in checksums
    """
    filename = filepath.name
    expected_hash = checksums.get(filename)

    if expected_hash is None:
        Logger.terminal(f"⚠️ No checksum entry found for {filename} in SHA512SUMS (file may be corrupted or incomplete)")
        return None

    # Verify the expected hash is valid hex before comparing
    try:
        int(expected_hash, 16)
    except ValueError:
        Logger.terminal(f"⚠️ Invalid checksum hash in SHA512SUMS for {filename}, skipping validation")
        return None

    actual_hash = compute_sha512(filepath)

    if actual_hash == expected_hash:
        Logger.console(f"✅ Checksum ok: {filename}")
        return True
    else:
        Logger.console(f"❌ Checksum mismatch: {filename} | Expected: {expected_hash[:16]}... Got: {actual_hash[:16]}...")
        return False

# Cache of downloaded checksum files: (gps_week, use_repro3) -> parsed checksums dict
_checksum_cache: Dict[tuple, dict] = {}

def get_checksums_for_week(gps_week: int, session: requests.Session, download_dir: Path = INPUT_PRODUCTS_PATH,
                           use_repro3: bool = False, progress_callback: Optional[Callable] = None,
                           stop_requested: Optional[Callable] = None) -> Optional[dict]:
    """
    Get the parsed checksum dictionary for a GPS week, downloading the SHA512SUMS file if needed.
    Uses an in-memory cache so the file is only downloaded and parsed once per session.

    :param gps_week: GPS week number
    :param session: Authenticated requests session for CDDIS access
    :param download_dir: Directory to save/find the checksum file
    :param use_repro3: If True, use the repro3 subdirectory
    :param progress_callback: Optional callback for progress updates (filename, percent)
    :param stop_requested: Optional callback to check if operation should stop
    :returns: Dictionary mapping filename -> expected SHA-512 hex digest, or None if unavailable
    """
    cache_key = (gps_week, use_repro3)

    if cache_key in _checksum_cache:
        return _checksum_cache[cache_key]

    checksum_path = download_checksum_file(gps_week, session, download_dir, use_repro3,
                                           progress_callback, stop_requested)
    if checksum_path is None:
        return None

    checksums = parse_checksum_file(checksum_path)
    _checksum_cache[cache_key] = checksums
    return checksums

# endregion

def download_file(url: str, session: requests.Session, download_dir: Path = INPUT_PRODUCTS_PATH,
                  progress_callback: Optional[Callable] = None,
                  stop_requested: Callable = None, checksums: Optional[dict] = None,
                  keep_compressed: bool = True) -> Path:
    """
    Checks if file already exists (additionally in compressed or .part forms).
    Uses provided session for CDDIS files (session made during startup).
    Downloads in chunks to a file with the same file path suffixed by .part.
    Deletes .part file after download.
    Automatically calls extract_file() on compressed files.

    :param url: download url
    :param session: requests Session preloaded with users CDDIS credentials
    :param download_dir: dir to download to
    :param progress_callback: reports, on every chunk, an int percentage of total download
    :param stop_requested: bool callback. Raises a RuntimeError if occurred during download
    :param checksums: Optional dict of filename -> expected SHA-512 hex digest for validation
    :param keep_compressed: if True, retain compressed file alongside extracted version for archival
    :raises RuntimeError: Stop requested during download
    :raises Exception: Max retries reached
    :return:
    """

    filepath = Path(download_dir / url.split("/")[-1])  # Download dir + filename
    # 1. When file already exists, extract if possible, then return
    if filepath.exists():
        # Validate checksum on the existing compressed file before extracting
        if checksums is not None:
            result = validate_checksum(filepath, checksums)
            if result is False:
                Logger.terminal(f"⚠️ Existing file {filepath.name} failed checksum, re-downloading...")
                filepath.unlink(missing_ok=True)
                # Fall through to download below
            else:
                if filepath.suffix in COMPRESSED_FILETYPE:
                    return extract_file(filepath, keep_compressed=keep_compressed)
                else:
                    return filepath
        else:
            if filepath.suffix in COMPRESSED_FILETYPE:
                return extract_file(filepath, keep_compressed=keep_compressed)
            else:
                return filepath

    # 2. Check if an extracted version of this file already exists
    if filepath.suffix in COMPRESSED_FILETYPE:
        potential_decompressed = filepath.with_suffix('')  # Remove one suffix
        if potential_decompressed.exists():
            # If the compressed file is still alongside it, validate checksum against it
            if checksums is not None and filepath.name in checksums:
                if filepath.exists():
                    result = validate_checksum(filepath, checksums)
                    if result is False:
                        Logger.terminal(f"⚠️ Compressed file {filepath.name} failed checksum, re-downloading...")
                        filepath.unlink(missing_ok=True)
                        potential_decompressed.unlink(missing_ok=True)
                        # Fall through to download below
                    else:
                        return potential_decompressed
                else:
                    Logger.terminal(f"⚠️ Cannot verify checksum for {filepath.name} (compressed file missing), re-downloading to validate...")
                    potential_decompressed.unlink(missing_ok=True)
                    # Fall through to download below
            else:
                return potential_decompressed

    # 3. Check if the file can be restored from the archive (avoids re-downloading from CDDIS)
    restored = restore_from_archive(filepath.name, download_dir)
    if restored is not None and restored.exists():
        # Restored file is compressed - validate checksum then extract
        if checksums is not None and filepath.name in checksums:
            result = validate_checksum(restored, checksums)
            if result is False:
                Logger.terminal(f"⚠️ Archived file {restored.name} failed checksum validation, re-downloading...")
                restored.unlink(missing_ok=True)
                # Fall through to download below
            else:
                return extract_file(restored, keep_compressed=keep_compressed)
        else:
            if restored.suffix in COMPRESSED_FILETYPE:
                return extract_file(restored, keep_compressed=keep_compressed)
            else:
                return restored

    # 4. Download the file in chunks (.part)
    for i in range(MAX_RETRIES):
        _partial = filepath.with_suffix(filepath.suffix + ".part")

        if _partial.exists():
            # Resume partial downloads
            headers = {"Range": f"bytes={_partial.stat().st_size}-"}
            Logger.terminal(f"Resuming download of {filepath.name} from byte {_partial.stat().st_size}")
        else:
            # Download whole file
            headers = {"Range": "bytes=0-"}
            Logger.terminal(f"Starting new download of {filepath.name}")
            os.makedirs(_partial.parent, exist_ok=True)

            # Hack?! for windows error when open(_partial, "wb") not creating new files
            ensure_file_exists = open(_partial, "w")
            ensure_file_exists.close()

        try:
            if url.startswith(BASE_URL):
                # Download files from CDDIS with authorized session
                resp = session.get(url, headers=headers, stream=True, timeout=30)
            else:
                resp = requests.get(url, headers=headers, stream=True, timeout=30)
            resp.raise_for_status()

            if resp.status_code == 206:
                # Received partial content as expected
                mode = 'ab' if _partial.exists() else 'wb'
                total_size = int(resp.headers.get("content-length")) + _partial.stat().st_size
            else:
                # likely 200 OK, server is sending the entire file again
                mode = 'wb'
                total_size = int(resp.headers.get("content-length"))

            with open(_partial, mode) as partial_out:
                downloaded = _partial.stat().st_size
                for _chunk in resp.iter_content(chunk_size=CHUNK_SIZE):
                    if stop_requested and stop_requested():
                        raise RuntimeError("Stop requested during download")

                    if _chunk:  # Filters keep-alives
                        partial_out.write(_chunk)
                        downloaded += len(_chunk)

                        if progress_callback:
                            percent = int(downloaded / total_size * 100)
                            progress_callback(filepath.name, percent)

            os.rename(_partial, filepath)

            # Validate checksum on the compressed file before extraction
            if checksums is not None:
                result = validate_checksum(filepath, checksums)
                if result is False:
                    Logger.terminal(f"⚠️ Deleting corrupted file {filepath.name} and retrying...")
                    filepath.unlink(missing_ok=True)
                    continue

            if filepath.suffix in COMPRESSED_FILETYPE:
                return extract_file(filepath, keep_compressed=keep_compressed)
            else:
                return filepath
        except requests.RequestException as e:
            Logger.terminal(f"Failed attempt {i} to download {filepath.name}: {e}")

    raise (Exception(f"Failed to download {filepath.name} after {MAX_RETRIES} attempts"))


def get_brdc_urls(start_time: datetime, end_time: datetime) -> list[str]:
    """
    Generates a list of BRDC file URLs for the specified date range.

    :param start_time: Start of the date range
    :param end_time: End of the date range
    :returns: List URLs to download BRDC files
    """
    urls = []
    reference_dt = start_time
    while int((end_time - reference_dt).total_seconds()) > 0:
        day = reference_dt.strftime("%j")
        filename = f"BRDC00IGS_R_{reference_dt.year}{day}0000_01D_MN.rnx.gz"
        url = f"{BASE_URL}/gnss/data/daily/{reference_dt.year}/brdc/{filename}"
        urls.append(url)
        reference_dt += timedelta(days=1)
    return urls


def download_metadata(download_dir: Path = INPUT_PRODUCTS_PATH,
                      progress_callback: Optional[Callable] = None, atx_callback: Optional[Callable] = None):
    """
    Calls download_products() with args to download standard metadata files. Calls atx_callback("igs20.atx")
    once "igs20.atx" is downloaded. Won't install duplicate files.

    :param download_dir: dir to download to
    :param progress_callback: reports, on every chunk, an int percentage of total download
    :param atx_callback: Optional callback function when igs20.atx is downloaded (downloaded_file)
    :raises Exception: Max retries reached
    """
    for download in download_products(products=pd.DataFrame(), download_dir=download_dir,
                                      progress_callback=progress_callback, dl_urls=METADATA):
        if atx_callback and download.name == "igs20.atx":
            atx_callback(download.name)


def download_products(products: pd.DataFrame, download_dir: Path = INPUT_PRODUCTS_PATH,
                      dl_urls: list = None, progress_callback: Optional[Callable] = None,
                      stop_requested: Optional[Callable] = None) -> Generator[Path, None, None]:
    """
    Creates download URLs for products and subsequently calls download_file() on them. Won't install duplicate files.

    :param pd.DataFrame products: (from get_product_dataframe) of all products to download
    :param download_dir: dir to download to
    :param dl_urls: Optional list of additional URLs to download (e.g. BRDC files)
    :param progress_callback: reports, on every chunk, an int percentage of total download
    :param stop_requested: bool callback. Raises a RuntimeError if occurred during download
    :returns: Generator with paths to downloaded files
    :raises RuntimeError: Stop requested during download
    :raises Exception: Max retries reached
    """

    # Create authenticated session for CDDIS access early so it can be used for URL generation
    _sesh = requests.Session()
    _sesh.auth = get_netrc_auth()

    # 1. Generate filenames from the DataFrame, tracking which URLs need checksum validation
    downloads = []
    # Maps URL -> (gps_week, is_repro3) for CDDIS product files that need checksum validation
    url_checksum_info = {}

    for _, row in products.iterrows():
        gps_week = date_to_gpswk(row.date)
        # Check if this is a repro3 product (R03 project) FIRST
        # repro3 always uses the modern naming convention regardless of GPS week
        is_repro3 = row.project == REPRO3_PROJECT

        if is_repro3:
            # For repro3, always use modern naming convention and find the correct 4th character
            # Try each character 0 - 9, prioritising lower numbers
            filename, url = _get_repro3_filename_and_url(row, gps_week, _sesh)
        elif gps_week < 2237:
            # Old naming convention for non-repro3 products before week 2237
            # AAAWWWWD.TYP.Z
            # e.g. COD22360.FIN.SNX.gz
            if row.period == timedelta(days=7):
                day = 7
            else:
                day = int((row.date - gpswk_to_date(gps_week)).days)
            filename = f"{row.analysis_center.lower()}{gps_week}{day}.{row.format.lower()}.Z"
            url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"
        else:
            # Modern naming convention for non-repro3 products from week 2237 onwards
            # e.g. GRG0OPSFIN_20232620000_01D_01D_SOL.SNX.gz
            # AAA#PPPSNX_YYYYDDDHHMM_LEN_SMP_CNT.FMT.gz
            # where # is the 4th character (can be 0-9, stored in _4th_char column)
            fourth_char = row._4th_char if hasattr(row, '_4th_char') and row._4th_char is not None else "0"
            filename = f"{row.analysis_center}{fourth_char}{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"
            url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"

        downloads.append(url)

        # Track checksum info for validated file types
        if row.format.upper() in CHECKSUM_VALIDATED_FORMATS:
            url_checksum_info[url] = (gps_week, is_repro3)

    if dl_urls:
        downloads.extend(dl_urls)

    Logger.terminal(f"📦 {len(downloads)} files to check or download")
    download_dir.mkdir(parents=True, exist_ok=True)
    (download_dir / "tables").mkdir(parents=True, exist_ok=True)
    for url in downloads:
        _x = url.split("/")
        if len(_x) < 2:
            fin_dir = download_dir
        else:
            fin_dir = download_dir / "tables" if _x[-2] == "tables" else download_dir

        # Fetch checksums for this URL if it needs validation
        checksums = None
        if url in url_checksum_info:
            gps_week, is_repro3 = url_checksum_info[url]
            checksums = get_checksums_for_week(gps_week, _sesh, download_dir, is_repro3,
                                               progress_callback, stop_requested)

        # Don't keep compressed files for tables/metadata - only for CDDIS product files
        is_tables = (fin_dir != download_dir)
        yield download_file(url, _sesh, fin_dir, progress_callback, stop_requested, checksums, keep_compressed=not is_tables)

def _get_repro3_filename_and_url(row: pd.Series, gps_week: int, session: requests.Session = None) -> tuple:
    """
    Determine the correct filename and URL for a repro3 product.
    Tries 4th characters 0-9 and returns the first one that exists.

    :param row: DataFrame row with product info
    :param gps_week: GPS week number
    :param session: Optional authenticated requests session for CDDIS access
    :returns: tuple of (filename, url)
    """
    if session is None:
        session = requests.Session()
        session.auth = get_netrc_auth()

    for fourth_char in REPRO3_4TH_CHAR_RANGE:
        filename = f"{row.analysis_center}{fourth_char}{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"
        url = f"{BASE_URL}/gnss/products/{gps_week}/repro3/{filename}"

        # Check if this file exists using authenticated session
        try:
            resp = session.head(url, timeout=5, allow_redirects=True)
            if resp.status_code == 200:
                return filename, url
        except requests.RequestException:
            continue

    # Fallback: return with 4th char = 0 (will likely fail download, but that's expected behaviour)
    filename = f"{row.analysis_center}0{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"
    url = f"{BASE_URL}/gnss/products/{gps_week}/repro3/{filename}"
    return filename, url


#region SP3 Product Validation

# Size of partial download for SP3 header (2KB should be enough)
SP3_HEADER_BYTES = 2048

def parse_sp3_header_constellations(header_content: str) -> set:
    """
    Parse SP3 file header content and extract constellation prefixes.

    The SP3 header contains satellite list lines starting with '+' that look like:
    +  122   G01G02G03G04G05G06G07G08G09G10G11G12G13G14G15G16G17
    +        G18G19G20G21G22G23G24...R01R02...E02E03...C06C07...J02J03

    :param header_content: String content of the SP3 header
    :returns: Set of constellation codes found (e.g., {'GPS', 'GLO', 'GAL', 'BDS', 'QZS'})
    """
    constellations = set()

    for line in header_content.split('\n'):
        # Look for satellite list lines (start with '+')
        # Skip the first '+' line which contains the satellite count
        if line.startswith('+') and len(line) > 10:
            # Extract satellite IDs from the line (format: G01, R02, E03, etc.)
            # The satellite data starts after the initial '+' and whitespace/numbers
            content = line[1:].strip()

            # Skip if this is the count line (contains only numbers and spaces at start)
            if content and content.split()[0].isdigit():
                # This line has count info, but satellite IDs follow
                # Find where satellite IDs start (first letter)
                for i, char in enumerate(content):
                    if char.isalpha():
                        content = content[i:]
                        break
                else:
                    continue

            # Parse satellite IDs (3 characters each: letter + 2 digits)
            # They are packed together without separators: "G01G02G03R01R02E01..."
            i = 0
            while i < len(content):
                if content[i].isalpha():
                    sat_prefix = content[i]
                    if sat_prefix in CONSTELLATION_MAP:
                        constellations.add(CONSTELLATION_MAP[sat_prefix])
                    # Move to next potential satellite ID (3 chars)
                    i += 3
                elif content[i] == '0' and i + 1 < len(content) and content[i:i + 2] == '  ':
                    break
                else:
                    i += 1

    return constellations

def download_sp3_header(url: str, session: requests.Session, max_retries: int = 3) -> Optional[str]:
    """
    Download only the first portion of an SP3 file (compressed) to get the header.
    Uses HTTP Range request to download partial content.

    :param url: URL of the SP3 file (may be .gz compressed)
    :param session: Authenticated requests session
    :param max_retries: Maximum number of retry attempts (default 3)
    :returns: Decompressed header content as string, or None if download fails
    """
    import time

    for attempt in range(max_retries):
        try:
            # Request only the first N bytes using Range header
            headers = {"Range": f"bytes=0-{SP3_HEADER_BYTES - 1}"}
            resp = session.get(url, headers=headers, timeout=15, stream=True)

            # Accept both 200 (full content) and 206 (partial content)
            if resp.status_code not in (200, 206):
                if attempt < max_retries - 1:
                    Logger.console(f"SP3 download attempt {attempt + 1} failed (status {resp.status_code}), retrying...")
                    time.sleep(1 * (attempt + 1))  # Exponential backoff
                    continue
                return None

            content = resp.content

            # Decompress if gzip compressed
            if url.endswith('.gz') or url.endswith('.gzip'):
                try:
                    # For partial gzip, we may get truncation errors - that's OK
                    # We just need enough to parse the header
                    import zlib
                    try:
                        # Try raw deflate with gzip header handling
                        decompressor = zlib.decompressobj(16 + zlib.MAX_WBITS)
                        decompressed = decompressor.decompress(content)
                    except zlib.error:
                        # Fallback: try standard gzip decompress
                        try:
                            decompressed = gzip.decompress(content)
                        except (gzip.BadGzipFile, OSError):
                            if attempt < max_retries - 1:
                                time.sleep(1 * (attempt + 1))
                                continue
                            return None
                    content = decompressed
                except Exception:
                    if attempt < max_retries - 1:
                        time.sleep(1 * (attempt + 1))
                        continue
                    return None
            elif url.endswith('.Z'):
                try:
                    decompressed = unlzw3.unlzw(content)
                    content = decompressed
                except Exception:
                    if attempt < max_retries - 1:
                        time.sleep(1 * (attempt + 1))
                        continue
                    return None

            # Decode to string
            return content.decode('utf-8', errors='ignore')

        except requests.RequestException as e:
            if attempt < max_retries - 1:
                Logger.console(f"SP3 download attempt {attempt + 1} failed ({e}), retrying...")
                time.sleep(1 * (attempt + 1))
                continue
            Logger.console(f"Failed to download SP3 header from {url} after {max_retries} attempts: {e}")
            return None
        except Exception as e:
            if attempt < max_retries - 1:
                time.sleep(1 * (attempt + 1))
                continue
            Logger.console(f"Error processing SP3 header from {url}: {e}")
            return None

    return None

def get_sp3_url_for_product(row: pd.Series, session: requests.Session = None) -> Optional[str]:
    """
    Get the URL for an SP3 file for a specific product row.

    :param row: DataFrame row with product info (must have format == 'SP3')
    :param session: Optional authenticated session for CDDIS
    :returns: URL string or None if not found
    """
    gps_week = date_to_gpswk(row.date)

    # Check if this is a repro3 product
    is_repro3 = row.project == REPRO3_PROJECT

    if session is None:
        session = requests.Session()
        session.auth = get_netrc_auth()

    if is_repro3:
        # Use repro3 naming and URL
        filename, url = _get_repro3_filename_and_url(row, gps_week, session)
    elif gps_week < 2237:
        # Old naming convention
        if row.period == timedelta(days=7):
            day = 7
        else:
            day = int((row.date - gpswk_to_date(gps_week)).days)
        filename = f"{row.analysis_center.lower()}{gps_week}{day}.{row.format.lower()}.Z"
        url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"
    else:
        # Modern naming convention
        fourth_char = row._4th_char if hasattr(row, '_4th_char') and row._4th_char is not None else "0"
        filename = f"{row.analysis_center}{fourth_char}{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"
        url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"

    return url

def get_provider_constellations(products_df: pd.DataFrame, progress_callback: Optional[Callable] = None, stop_requested: Optional[Callable] = None) -> dict:
    """
    Download partial SP3 headers for valid provider/series/project combinations
    (those that have all required files: SP3, BIA, CLK) and extract constellation information.

    :param products_df: Products dataframe from get_product_dataframe_with_repro3_fallback()
    :param progress_callback: Optional callback for progress updates (description, percent)
    :param stop_requested: Optional callback to check if operation should stop
    :returns: Nested dictionary mapping provider -> series -> project -> set of constellations
          e.g., {
              'COD': {
                  'FIN': {'OPS': {'GPS', 'GLO', 'GAL'}, 'MGX': {'GPS', 'GLO', 'GAL', 'BDS', 'QZS'}},
                  'RAP': {'OPS': {'GPS', 'GLO', 'GAL'}}
              },
              'GRG': {
                  'FIN': {'OPS': {'GPS', 'GLO', 'GAL'}},
                  'RAP': {'OPS': {'GPS', 'GLO', 'GAL'}}
              }
          }
    """
    provider_constellations = {}
    REQUIRED_FILES = {"SP3", "BIA", "CLK"}

    # Get valid analysis centers first (those with all required files)
    valid_centers = get_valid_analysis_centers(products_df)

    if not valid_centers:
        Logger.console("No valid analysis centers found")
        return provider_constellations

    # Build list of valid (provider, series, project) combinations
    # These are the ones that have all three of the .SP3, .BIA, and .CLK files
    valid_combinations = []

    for provider in valid_centers:
        provider_data = products_df[products_df['analysis_center'] == provider]

        # Get valid series for this provider (those with all required files)
        for series in provider_data['solution_type'].unique():
            series_data = provider_data[provider_data['solution_type'] == series]
            available_files = set(series_data['format'].unique())

            if REQUIRED_FILES.issubset(available_files):
                # This series has all required files, get the project(s)
                for project in series_data['project'].unique():
                    valid_combinations.append((provider, series, project))

    if not valid_combinations:
        Logger.console("No valid provider/series/project combinations found")
        return provider_constellations

    total_combinations = len(valid_combinations)
    Logger.console(f"📡 Fetching constellation info for {total_combinations} valid combinations...")

    # Create authenticated session
    session = requests.Session()
    session.auth = get_netrc_auth()

    for i, (provider, series, project) in enumerate(valid_combinations):
        # Check for stop request
        if stop_requested and stop_requested():
            Logger.console("Constellation fetch cancelled")
            break

        # Progress callback
        if progress_callback:
            percent = int((i + 1) / total_combinations * 100)
            progress_callback(f"Fetching {provider}/{series}/{project} SP3", percent)

        # Find the SP3 product row for this combination
        sp3_row = products_df[
            (products_df['analysis_center'] == provider) &
            (products_df['project'] == project) &
            (products_df['solution_type'] == series) &
            (products_df['format'] == 'SP3')
            ]

        if sp3_row.empty:
            Logger.console(f"  {provider}/{series}/{project}: No SP3 product found")
            continue

        # Get SP3 URL for this combination
        url = get_sp3_url_for_product(sp3_row.iloc[0], session)

        if not url:
            Logger.console(f"  {provider}/{series}/{project}: No SP3 URL found")
            continue

        # Download partial header
        header_content = download_sp3_header(url, session)

        if not header_content:
            Logger.console(f"  {provider}/{series}/{project}: Failed to download SP3 header")
            continue

        # Parse constellations from header
        constellations = parse_sp3_header_constellations(header_content)

        if constellations:
            # Build nested dictionary structure: provider -> series -> project -> constellations
            if provider not in provider_constellations:
                provider_constellations[provider] = {}
            if series not in provider_constellations[provider]:
                provider_constellations[provider][series] = {}
            provider_constellations[provider][series][project] = constellations

            Logger.console(f"  {provider}/{series}/{project}: {', '.join(sorted(constellations))}")
        else:
            Logger.console(f"  {provider}/{series}/{project}: No constellations found in header")

    return provider_constellations

#endregion

#region BIA Product Validation

# Chunk size for BIA file downloads (100 KB)
BIA_CHUNK_SIZE = 102400

def get_bia_url_for_product(row: pd.Series, session: requests.Session = None) -> Optional[str]:
    """
    Get the URL for a BIA file for a specific product row.

    :param row: DataFrame row with product info (must have format == 'BIA')
    :param session: Optional authenticated session for CDDIS
    :returns: URL string or None if not found
    """
    gps_week = date_to_gpswk(row.date)

    # Check if this is a repro3 product
    is_repro3 = row.project == REPRO3_PROJECT

    if session is None:
        session = requests.Session()
        session.auth = get_netrc_auth()

    if is_repro3:
        # Use repro3 naming and URL
        filename, url = _get_repro3_filename_and_url(row, gps_week, session)
    elif gps_week < 2237:
        # Old naming convention - BIA files may not exist in old format
        # Try the modern format anyway as BIA is a newer product type
        fourth_char = row._4th_char if hasattr(row, '_4th_char') and row._4th_char is not None else "0"
        filename = f"{row.analysis_center}{fourth_char}{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"
        url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"
    else:
        # Modern naming convention
        fourth_char = row._4th_char if hasattr(row, '_4th_char') and row._4th_char is not None else "0"
        filename = f"{row.analysis_center}{fourth_char}{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"
        url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"

    return url

def download_bia_satellite_section(url: str, session: requests.Session, progress_callback: Optional[Callable] = None, stop_requested: Optional[Callable] = None, max_retries: int = 3) -> Optional[str]:
    """
    Download the satellite bias section of a BIA file in chunks.
    Stops downloading when:
    - A station marker is detected (4-char STATION field populated)
    - The first PRN cycles back with a different time (full satellite cycle complete)
    - The -BIAS/SOLUTION end marker is reached
    - The file ends

    :param url: URL of the BIA file (may be .gz compressed)
    :param session: Authenticated requests session
    :param progress_callback: Optional callback for progress updates (description, percent)
    :param stop_requested: Optional callback to check if operation should stop
    :param max_retries: Maximum number of retry attempts for failed requests (default 3)
    :returns: Decompressed satellite bias section as string, or None if download fails
    """
    import zlib
    import time

    for attempt in range(max_retries):
        accumulated_content = b""
        accumulated_text = ""
        chunk_num = 0
        max_chunks = 100  # Safety limit: 100 * 100KB = 1MB max
        chunk_retry_count = 0
        max_chunk_retries = 3

        # Set up decompressor for streaming gzip
        is_gzip = url.endswith('.gz') or url.endswith('.gzip')
        is_lzw = url.endswith('.Z')

        try:
            while chunk_num < max_chunks:
                # Check for stop request
                if stop_requested and stop_requested():
                    return None

                # Calculate byte range for this chunk
                start_byte = chunk_num * BIA_CHUNK_SIZE
                end_byte = start_byte + BIA_CHUNK_SIZE - 1

                # Request chunk using Range header with retry logic
                try:
                    headers = {"Range": f"bytes={start_byte}-{end_byte}"}
                    resp = session.get(url, headers=headers, timeout=30, stream=True)
                except requests.RequestException as chunk_error:
                    chunk_retry_count += 1
                    if chunk_retry_count < max_chunk_retries:
                        Logger.console(f"BIA chunk {chunk_num} download failed, retrying ({chunk_retry_count}/{max_chunk_retries})...")
                        time.sleep(1 * chunk_retry_count)
                        continue
                    raise chunk_error

                # Check response status
                if resp.status_code == 416:
                    # Range not satisfiable - we've reached the end of the file
                    break
                elif resp.status_code not in (200, 206):
                    if chunk_num == 0:
                        # First chunk failed, retry the whole download
                        if attempt < max_retries - 1:
                            Logger.console(f"BIA download attempt {attempt + 1} failed (status {resp.status_code}), retrying...")
                            time.sleep(1 * (attempt + 1))
                            break  # Break inner loop to retry from start
                        Logger.console(f"BIA download failed with status {resp.status_code}")
                        return None
                    break

                chunk_data = resp.content
                if not chunk_data:
                    break

                accumulated_content += chunk_data
                chunk_retry_count = 0  # Reset retry count on success

                # Decompress accumulated content
                try:
                    if is_gzip:
                        # Try to decompress what we have so far
                        try:
                            temp_decompressor = zlib.decompressobj(16 + zlib.MAX_WBITS)
                            decompressed = temp_decompressor.decompress(accumulated_content)
                            accumulated_text = decompressed.decode('utf-8', errors='ignore')
                        except zlib.error:
                            # Incomplete gzip stream, continue accumulating
                            chunk_num += 1
                            continue
                    elif is_lzw:
                        try:
                            decompressed = unlzw3.unlzw(accumulated_content)
                            accumulated_text = decompressed.decode('utf-8', errors='ignore')
                        except Exception:
                            chunk_num += 1
                            continue
                    else:
                        accumulated_text = accumulated_content.decode('utf-8', errors='ignore')
                except Exception:
                    chunk_num += 1
                    continue

                # Progress callback
                if progress_callback:
                    progress_callback(f"Downloading BIA ({(chunk_num + 1) * BIA_CHUNK_SIZE // 1024} KB)", -1)

                # Check if we've found a termination condition (station marker, cycle complete, or end marker)
                should_stop, satellite_section = _check_bia_termination(accumulated_text)

                if should_stop:
                    Logger.console(f"📥 BIA download finished after {(chunk_num + 1) * BIA_CHUNK_SIZE // 1024} KB")
                    return satellite_section

                chunk_num += 1

            # If we have accumulated content, process it
            if accumulated_text:
                # If we exited the loop without finding termination, return what we have
                Logger.console(f"📥 BIA download completed (no early termination), processing {len(accumulated_text)} chars")
                _, satellite_section = _check_bia_termination(accumulated_text, force_return=True)
                return satellite_section if satellite_section else accumulated_text

            # If we got here with no content on first chunk failure, continue to next attempt
            if attempt < max_retries - 1:
                continue

        except requests.RequestException as e:
            if attempt < max_retries - 1:
                Logger.console(f"BIA download attempt {attempt + 1} failed ({e}), retrying...")
                time.sleep(1 * (attempt + 1))
                continue
            Logger.console(f"Failed to download BIA file from {url} after {max_retries} attempts: {e}")
            return None
        except Exception as e:
            if attempt < max_retries - 1:
                time.sleep(1 * (attempt + 1))
                continue
            Logger.console(f"Error processing BIA file from {url}: {e}")
            return None

    return None


def _check_bia_termination(content: str, force_return: bool = False) -> tuple[bool, Optional[str]]:
    """
    Check if we should stop downloading and extract the satellite bias section.

    Termination conditions:
    1. Station marker detected (4-char STATION field at columns 15-18)
    2. We see a (constellation, OBS) combination that we've already seen, but with a
       DIFFERENT BIAS_START - this means we've collected all unique codes for that
       constellation and are now seeing repeats
    3. -BIAS/SOLUTION end marker reached

    This handles:
    - COD/GRG format: GPS → GAL → BDS → station markers (terminates on station)
    - IGS format: GPS(time1) → GPS(time2) → ... → GLO(time2) → GLO(time3) → ...
      (terminates when a constellation+OBS repeats with new time)
    - TUG format: G01(time1,time2,time3) → G02(time1,time2) → ...
      (terminates when constellation+OBS repeats with new time after seeing other constellations)

    BIA format column positions (0-indexed):
    - Cols 0-3: BIAS type (e.g., " OSB")
    - Cols 6-9: SVN (e.g., "G080")
    - Cols 11-13: PRN (e.g., "G01")
    - Cols 15-18: STATION (4 spaces for satellite biases, 4-char marker for station biases)
    - Cols 25-27: OBS1 (e.g., "C1C")
    - Cols 35-48: BIAS_START (e.g., "2025:180:00000")

    :param content: Accumulated BIA file content
    :param force_return: If True, return whatever satellite section we have
    :returns: Tuple of (should_stop, satellite_section_content)
    """
    lines = content.split('\n')
    satellite_lines = []
    in_bias_solution = False
    found_termination = False

    # Track seen (constellation, OBS) -> first BIAS_START for that combination
    # This allows us to detect when we've cycled through all unique codes
    seen_constellation_obs = {}

    # Track which constellations we've seen to know when we have a complete set
    seen_constellations = set()

    # Map PRN prefix to constellation
    prn_to_constellation = {'G': 'GPS', 'R': 'GLO', 'E': 'GAL', 'C': 'BDS', 'J': 'QZS'}

    for line in lines:
        # Check for BIAS/SOLUTION block start
        if '+BIAS/SOLUTION' in line:
            in_bias_solution = True
            continue

        # Check for BIAS/SOLUTION block end
        if '-BIAS/SOLUTION' in line:
            found_termination = True
            break

        # Skip if not in BIAS/SOLUTION block
        if not in_bias_solution:
            continue

        # Skip comment/header lines
        if line.startswith('*') or not line.strip():
            continue

        # Check if this is a bias data line
        if len(line) >= 28 and line[0] == ' ' and line[1:4] in ('OSB', 'DSB', 'DCB', 'ISB', 'LCB'):
            # Check for station marker (columns 15-18, 0-indexed)
            station_field = line[15:19] if len(line) > 19 else "    "

            if station_field.strip():
                # Found a station marker - we have reached station biases
                found_termination = True
                break

            # Extract PRN, OBS, and BIAS_START
            prn = line[11:14].strip() if len(line) > 14 else ""
            obs = line[25:28].strip() if len(line) > 28 else ""
            bias_start = line[35:49].strip() if len(line) > 49 else ""

            if not prn or not obs or len(prn) < 1:
                continue

            # Get constellation from PRN prefix
            constellation = prn_to_constellation.get(prn[0], None)
            if not constellation:
                continue

            seen_constellations.add(constellation)

            # Check if we've seen this (constellation, OBS) before
            key = (constellation, obs)
            if key in seen_constellation_obs:
                first_time = seen_constellation_obs[key]
                if bias_start and bias_start != first_time:
                    # We've seen this constellation+OBS before with a different time
                    # This means we've completed collecting unique codes for this constellation
                    # But only terminate if we've seen at least 2 constellations (to handle IGS format)
                    # OR if we've seen enough entries (safety check)
                    if len(seen_constellations) >= 2 or len(satellite_lines) > 500:
                        found_termination = True
                        break
                    # Otherwise, just skip this duplicate line
                    continue
            else:
                # First time seeing this constellation+OBS, record its BIAS_START
                seen_constellation_obs[key] = bias_start
                satellite_lines.append(line)

    if found_termination:
        satellite_section = '\n'.join(satellite_lines) if satellite_lines else None
        return True, satellite_section

    if force_return:
        satellite_section = '\n'.join(satellite_lines) if satellite_lines else None
        return True, satellite_section

    return False, None

def parse_bia_code_priorities(bia_content: str) -> dict:
    """
    Parse BIA file content and extract code priorities per constellation.
    Transforms code observations (C*) to phase observations (L*).

    BIA format column positions (0-indexed):
    - Cols 0-3: BIAS type (e.g., " OSB")
    - Cols 6-9: SVN (e.g., "G080")
    - Cols 11-13: PRN (e.g., "G01")
    - Cols 15-18: STATION (4 spaces for satellite biases, 4-char marker for station biases)
    - Cols 25-27: OBS1 (e.g., "C1C")
    - Cols 35-48: BIAS_START (e.g., "2025:180:00000")

    :param bia_content: String content of the satellite bias section
    :returns: Dictionary mapping constellation names to sets of code priorities
          e.g., {'GPS': {'L1C', 'L2W', 'L1W'}, 'GAL': {'L1C', 'L5Q'}, ...}
    """
    code_priorities = {
        'GPS': set(),
        'GLO': set(),
        'GAL': set(),
        'BDS': set(),
        'QZS': set(),
    }

    if not bia_content:
        return code_priorities

    # Debug: show first 20 lines being parsed
    debug_lines = []
    line_count = 0

    for line in bia_content.split('\n'):
        # Skip empty lines
        if not line.strip():
            continue

        # Check if this is a bias data line (need at least enough chars for OBS1)
        if len(line) < 28:
            continue

        # Bias data lines start with space + bias type
        if line[0] != ' ' or line[1:4] not in ('OSB', 'DSB', 'DCB', 'ISB', 'LCB'):
            continue

        # Extract fields using correct column positions (verified against real files)
        svn = line[6:10].strip() if len(line) > 10 else ""
        prn = line[11:14].strip() if len(line) > 14 else ""
        station = line[15:19] if len(line) > 19 else "    "  # Don't strip - check for spaces
        obs1 = line[25:28].strip() if len(line) > 28 else ""

        # Skip station bias lines - station field should be all spaces for satellite biases
        if station.strip():
            # This is a station bias line, skip it
            continue

        # Skip if PRN is invalid
        if not prn or len(prn) < 2:
            continue

        # Get constellation from first character of PRN
        constellation_char = prn[0]
        if constellation_char not in CONSTELLATION_MAP:
            continue

        constellation = CONSTELLATION_MAP[constellation_char]

        # Skip if OBS1 is invalid
        if not obs1 or len(obs1) < 3:
            continue

        # Transform C* codes to L* codes
        if obs1.startswith('C'):
            obs1 = 'L' + obs1[1:]

        # Add to the constellation's set
        code_priorities[constellation].add(obs1)

    return code_priorities

def get_bia_code_priorities_for_selection(products_df: pd.DataFrame,
                                          provider: str, series: str, project: str,
                                          progress_callback: Optional[Callable] = None,
                                          stop_requested: Optional[Callable] = None) -> Optional[dict]:
    """
    Download and parse BIA file for a specific provider/series/project combination
    to extract available code priorities per constellation.

    If the BIA file already exists locally (from a previous download) or can be restored
    from the archive, it is read directly without contacting CDDIS.

    :param products_df: Products dataframe from get_product_dataframe_with_repro3_fallback()
    :param provider: Analysis center (e.g., 'COD', 'GRG')
    :param series: Solution type (e.g., 'FIN', 'RAP')
    :param project: Project code (e.g., 'OPS', 'MGX')
    :param progress_callback: Optional callback for progress updates (description, percent)
    :param stop_requested: Optional callback to check if operation should stop
    :returns: Dictionary mapping constellation names to sets of code priorities,
              or None if download/parse fails
    """
    # Find the BIA product row for this combination
    bia_row = products_df[
        (products_df['analysis_center'] == provider) &
        (products_df['project'] == project) &
        (products_df['solution_type'] == series) &
        (products_df['format'] == 'BIA')
        ]

    if bia_row.empty:
        Logger.console(f"No BIA product found for {provider}/{series}/{project}")
        return None

    # Create authenticated session
    session = requests.Session()
    session.auth = get_netrc_auth()

    # Get BIA URL
    url = get_bia_url_for_product(bia_row.iloc[0], session)
    if not url:
        Logger.console(f"Could not generate BIA URL for {provider}/{series}/{project}")
        return None

    # Check if the BIA file already exists locally (uncompressed from a previous download)
    compressed_filename = url.split("/")[-1]
    uncompressed_filename = compressed_filename.removesuffix(".gz").removesuffix(".Z").removesuffix(".gzip")
    local_uncompressed = INPUT_PRODUCTS_PATH / uncompressed_filename
    local_compressed = INPUT_PRODUCTS_PATH / compressed_filename

    bia_content = _try_read_local_bia(local_uncompressed, local_compressed, compressed_filename, provider, series, project)
    if bia_content is not None:
        code_priorities = parse_bia_code_priorities(bia_content)
        _log_bia_code_priorities(code_priorities, provider, series, project)
        return code_priorities

    Logger.terminal(f"📥 Validating constellation signal frequencies against BIA file for {provider}/{series}/{project}...")

    # Download satellite bias section
    bia_content = download_bia_satellite_section(url, session, progress_callback=progress_callback, stop_requested=stop_requested)

    if not bia_content:
        Logger.console(f"Failed to download BIA content for {provider}/{series}/{project}")
        return None

    # Parse code priorities
    code_priorities = parse_bia_code_priorities(bia_content)
    _log_bia_code_priorities(code_priorities, provider, series, project)
    return code_priorities


def _try_read_local_bia(local_uncompressed: Path, local_compressed: Path,
                        compressed_filename: str, provider: str, series: str, project: str) -> Optional[str]:
    """
    Try to read BIA content from a local file or by restoring from the archive.
    Returns the satellite bias section content, or None if not available locally.

    :param local_uncompressed: Path to the expected uncompressed BIA file
    :param local_compressed: Path to the expected compressed BIA file
    :param compressed_filename: The compressed filename for archive restoration
    :param provider: Analysis centre for logging
    :param series: Solution type for logging
    :param project: Project code for logging
    :return: BIA satellite section content string, or None
    """
    # 1. Check if uncompressed BIA file exists locally
    if local_uncompressed.exists():
        try:
            Logger.console(f"📂 Using local BIA file for {provider}/{series}/{project}: {local_uncompressed.name}")
            with open(local_uncompressed, 'r', encoding='utf-8', errors='replace') as f:
                content = f.read()
            _, satellite_section = _check_bia_termination(content, force_return=True)
            if satellite_section:
                return satellite_section
        except Exception as e:
            Logger.console(f"Failed to read local BIA file {local_uncompressed.name}: {e}")

    # 2. Check if compressed BIA file exists locally (extract and read)
    if local_compressed.exists():
        try:
            Logger.console(f"📂 Using local compressed BIA file for {provider}/{series}/{project}: {local_compressed.name}")
            content = _read_compressed_bia(local_compressed)
            if content:
                _, satellite_section = _check_bia_termination(content, force_return=True)
                if satellite_section:
                    return satellite_section
        except Exception as e:
            Logger.console(f"Failed to read compressed BIA file {local_compressed.name}: {e}")

    # 3. Try restoring from archive
    restored = restore_from_archive(compressed_filename, INPUT_PRODUCTS_PATH)
    if restored is not None and restored.exists():
        try:
            Logger.console(f"📂 Using archived BIA file for {provider}/{series}/{project}: {restored.name}")
            content = _read_compressed_bia(restored) if restored.suffix in COMPRESSED_FILETYPE else None
            if content is None:
                with open(restored, 'r', encoding='utf-8', errors='replace') as f:
                    content = f.read()
            if content:
                _, satellite_section = _check_bia_termination(content, force_return=True)
                if satellite_section:
                    return satellite_section
        except Exception as e:
            Logger.console(f"Failed to read archived BIA file {restored.name}: {e}")

    return None


def _read_compressed_bia(filepath: Path) -> Optional[str]:
    """
    Read and decompress a .gz or .Z compressed BIA file.

    :param filepath: Path to the compressed file
    :return: Decompressed content as string, or None on failure
    """
    try:
        if str(filepath).endswith((".gz", ".gzip")):
            with gzip.open(filepath, "rb") as f:
                return f.read().decode('utf-8', errors='replace')
        elif str(filepath).endswith(".Z"):
            data = unlzw3.unlzw(filepath)
            return data.decode('utf-8', errors='replace')
    except Exception as e:
        Logger.console(f"Failed to decompress {filepath.name}: {e}")
    return None


def _log_bia_code_priorities(code_priorities: dict, provider: str, series: str, project: str):
    """Log extracted BIA code priorities."""
    Logger.console(f"✅ Extracted code priorities for {provider}/{series}/{project}:")
    for constellation, codes in sorted(code_priorities.items()):
        if codes:
            Logger.console(f"    {constellation}: {', '.join(sorted(codes))}")

#endregion

#region SINEX Product Validation

def get_sinex_url(target_date: datetime, use_repro3: bool = False) -> str:
    """
    Generate the URL for the IGS CRD SINEX file for the given date.
    Downloads the 1-day (daily) IGS CRD file

    Main directory format: IGS0OPSSNX_YYYYDDD0000_01D_01D_CRD.SNX.gz
    Repro3 format: IGS0R03SNX_YYYYDDD0000_01D_01D_CRD.SNX.gz (in /repro3/ subdirectory)

    :param target_date: The date for which to download the SINEX file
    :param use_repro3: If True, generate URL for repro3 subdirectory
    :returns: URL to the IGS CRD SNX file
    """
    gps_week = date_to_gpswk(target_date)
    year = target_date.year
    doy = target_date.timetuple().tm_yday

    if use_repro3:
        filename = f"IGS0R03SNX_{year}{doy:03d}0000_01D_01D_CRD.SNX.gz"
        url = f"{BASE_URL}/gnss/products/{gps_week}/repro3/{filename}"
    else:
        filename = f"IGS0OPSSNX_{year}{doy:03d}0000_01D_01D_CRD.SNX.gz"
        url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"
    return url

def download_sinex_file(target_date: datetime, download_dir: Path = INPUT_PRODUCTS_PATH, progress_callback: Optional[Callable] = None,
                        stop_requested: Optional[Callable] = None, max_retries: int = 3) -> Optional[Path]:
    """
    Download the IGS CRD SINEX file for the given date with retry logic.
    Determines whether to use repro3 or main directory based on GPS week range
    (same logic as BIA, CLK, SP3 products).

    - GPS week 730-2138: Use repro3 directory
    - Outside that range: Use main directory

    :param target_date: The date for which to download the SINEX file
    :param download_dir: Directory to save the downloaded file
    :param progress_callback: Optional callback for progress updates (filename, percent)
    :param stop_requested: Optional callback to check if operation should stop
    :param max_retries: Number of retry attempts (default 3)
    :returns: Path to the downloaded (and extracted) SINEX file, or None if failed
    """
    session = requests.Session()
    session.auth = get_netrc_auth()

    # Check if we're in the repro3 priority range (same logic as other products)
    gps_week = date_to_gpswk(target_date)
    use_repro3 = REPRO3_PRIORITY_GPS_WEEK_START <= gps_week <= REPRO3_PRIORITY_GPS_WEEK_END

    url = get_sinex_url(target_date, use_repro3=use_repro3)

    # Fetch checksums for the SINEX file's GPS week
    checksums = get_checksums_for_week(gps_week, session, download_dir, use_repro3, progress_callback, stop_requested)

    for attempt in range(max_retries):
        if stop_requested and stop_requested():
            Logger.terminal("🛑 SINEX download cancelled")
            return None

        try:
            filepath = download_file(url, session, download_dir, progress_callback, stop_requested, checksums)
            Logger.terminal(f"✅ SINEX file downloaded: {filepath.name}")
            return filepath
        except Exception as e:
            if attempt < max_retries - 1:
                Logger.terminal(f"⚠️ SINEX download attempt {attempt + 1}/{max_retries} failed: {e}")
            else:
                Logger.terminal(f"❌ Failed to download SINEX file after {max_retries} attempts: {e}")

    return None

def parse_sinex_section(content: str, section_name: str) -> List[str]:
    """
    Extract lines from a specific SINEX section.

    :param content: Full SINEX file content
    :param section_name: Name of the section (e.g., "SITE/RECEIVER", "SITE/ANTENNA")
    :returns: List of data lines from the section (excluding header/comment lines)
    """
    lines = []
    in_section = False
    start_marker = f"+{section_name}"
    end_marker = f"-{section_name}"

    for line in content.split('\n'):
        if line.startswith(start_marker):
            in_section = True
            continue
        elif line.startswith(end_marker):
            break
        elif in_section and not line.startswith('*') and line.strip():
            lines.append(line)

    return lines

def parse_sinex_receiver(sinex_content: str, marker_name: str) -> Optional[str]:
    """
    Extract the receiver type for a given marker from SINEX SITE/RECEIVER section.

    SINEX format (columns are fixed-width):
    *CODE PT SOLN T _DATA START_ __DATA_END__ ___RECEIVER_TYPE____ _S/N_ _FIRMWARE__
     AB51  A ---- P 17:156:72000 00:000:00000 TRIMBLE NETRS        45032 1.3-2

    Receiver type is 20 characters starting at column 42.

    :param sinex_content: Full SINEX file content
    :param marker_name: 4-character marker name to look up
    :returns: Receiver type string (20 chars) or None if not found
    """
    lines = parse_sinex_section(sinex_content, "SITE/RECEIVER")

    for line in lines:
        if len(line) < 62:
            continue
        code = line[1:5].strip()
        if code.upper() == marker_name.upper():
            # Receiver type is columns 42-61 (20 characters)
            receiver_type = line[42:62]
            return receiver_type

    return None

def parse_sinex_antenna(sinex_content: str, marker_name: str) -> Optional[str]:
    """
    Extract the antenna type for a given marker from SINEX SITE/ANTENNA section.

    SINEX format (columns are fixed-width):
    *CODE PT SOLN T _DATA START_ __DATA_END__ ____ANTENNA_TYPE____ _S/N_ _DAZ
     AB51  A ---- P 05:273:00000 00:000:00000 TRM29659.00     SCIT 02203    0

    Antenna type is 20 characters starting at column 42.

    :param sinex_content: Full SINEX file content
    :param marker_name: 4-character marker name to look up
    :returns: Antenna type string (20 chars) or None if not found
    """
    lines = parse_sinex_section(sinex_content, "SITE/ANTENNA")

    for line in lines:
        if len(line) < 62:
            continue
        code = line[1:5].strip()
        if code.upper() == marker_name.upper():
            # Antenna type is columns 42-61 (20 characters)
            antenna_type = line[42:62]
            return antenna_type

    return None

def parse_sinex_eccentricity(sinex_content: str, marker_name: str) -> Optional[List[float]]:
    """
    Extract the antenna eccentricity (offset) for a given marker from SINEX SITE/ECCENTRICITY section.

    SINEX format (columns are fixed-width):
    *CODE PT SOLN T _DATA START_ __DATA_END__ REF __DX_U__ __DX_N__ __DX_E__
     AB51  A ---- P 05:273:00000 00:000:00000 UNE   0.0083   0.0000   0.0000

    DX_U (Up) starts at column 46, DX_N (North) at 55, DX_E (East) at 64.

    :param sinex_content: Full SINEX file content
    :param marker_name: 4-character marker name to look up
    :returns: List of [East, North, Up] offsets in metres, or None if not found
    """
    lines = parse_sinex_section(sinex_content, "SITE/ECCENTRICITY")

    for line in lines:
        if len(line) < 72:
            continue
        code = line[1:5].strip()
        if code.upper() == marker_name.upper():
            try:
                # Extract UNE values - note SINEX stores as U, N, E but we return as E, N, U
                dx_u = float(line[46:55].strip())
                dx_n = float(line[55:64].strip())
                dx_e = float(line[64:].strip())  # Read to end of line for last field
                return [dx_e, dx_n, dx_u]
            except ValueError:
                return None

    return None

def parse_sinex_apriori_position(sinex_content: str, marker_name: str) -> Optional[List[float]]:
    """
    Extract the apriori position (X, Y, Z) for a given marker from SINEX SOLUTION/APRIORI section.

    SINEX format (columns are fixed-width):
    *INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S ____APRIORI_VALUE____ __STD_DEV__
         1 STAX   AB51  A    3 23:260:43200 m    2 -2.38374988824415e+06 0.00000e+00

    We need to find STAX, STAY, STAZ entries for the marker.

    :param sinex_content: Full SINEX file content
    :param marker_name: 4-character marker name to look up
    :returns: List of [X, Y, Z] coordinates in metres, or None if not found
    """
    lines = parse_sinex_section(sinex_content, "SOLUTION/APRIORI")

    position = {'STAX': None, 'STAY': None, 'STAZ': None}

    for line in lines:
        if len(line) < 68:
            continue

        # Parse the line - fields are space-separated but with fixed positions
        parts = line.split()
        if len(parts) < 9:
            continue

        try:
            # _TYPE_ is at position 1, CODE at position 2, value at position 8
            sta_type = parts[1]
            code = parts[2]

            if code.upper() != marker_name.upper():
                continue

            if sta_type in position:
                # Value is in scientific notation
                value = float(parts[8])
                position[sta_type] = value
        except (IndexError, ValueError):
            continue

    # Check if we found all three coordinates
    if all(v is not None for v in position.values()):
        return [position['STAX'], position['STAY'], position['STAZ']]

    return None

def validate_sinex_values(sinex_content: str, marker_name: str, receiver_type: str, antenna_type: str, antenna_offset: List[float],
                          apriori_position: Optional[List[float]] = None, tolerance: float = 0.001) -> dict:
    """
    Validate extracted RINEX values against SINEX file data.

    :param sinex_content: Full SINEX file content
    :param marker_name: 4-character marker name
    :param receiver_type: Receiver type from RINEX (20 chars, space-padded)
    :param antenna_type: Antenna type from RINEX (20 chars, space-padded)
    :param antenna_offset: Antenna offset [E, N, U] from RINEX
    :param apriori_position: Optional apriori position [X, Y, Z] from RINEX
    :param tolerance: Tolerance for floating point comparisons (default 1mm)
    :returns: Dictionary with validation results for each field
    """
    results = {
        'marker_found': False,
        'receiver_type': {'valid': None, 'sinex_value': None, 'rinex_value': receiver_type, 'message': ''},
        'antenna_type': {'valid': None, 'sinex_value': None, 'rinex_value': antenna_type, 'message': ''},
        'antenna_offset': {'valid': None, 'sinex_value': None, 'rinex_value': antenna_offset, 'message': ''},
        'apriori_position': {'valid': None, 'sinex_value': None, 'rinex_value': apriori_position, 'message': ''},
    }

    # Validate receiver type
    sinex_receiver = parse_sinex_receiver(sinex_content, marker_name)
    if sinex_receiver is not None:
        results['marker_found'] = True
        results['receiver_type']['sinex_value'] = sinex_receiver

        sinex_recv_norm = sinex_receiver.rstrip()
        rinex_recv_norm = receiver_type.rstrip() if receiver_type else ''

        if sinex_recv_norm == rinex_recv_norm:
            results['receiver_type']['valid'] = True
            results['receiver_type']['message'] = 'Receiver type matches SINEX'
        else:
            results['receiver_type']['valid'] = False
            results['receiver_type']['message'] = f'Receiver type mismatch: RINEX="{rinex_recv_norm}", SINEX="{sinex_recv_norm}"'

    # Validate antenna type
    sinex_antenna = parse_sinex_antenna(sinex_content, marker_name)
    if sinex_antenna is not None:
        results['marker_found'] = True
        results['antenna_type']['sinex_value'] = sinex_antenna

        sinex_ant_norm = sinex_antenna.rstrip()
        rinex_ant_norm = antenna_type.rstrip() if antenna_type else ''

        if sinex_ant_norm == rinex_ant_norm:
            results['antenna_type']['valid'] = True
            results['antenna_type']['message'] = 'Antenna type matches SINEX'
        else:
            results['antenna_type']['valid'] = False
            results['antenna_type']['message'] = f'Antenna type mismatch: RINEX="{rinex_ant_norm}", SINEX="{sinex_ant_norm}"'

    # Validate antenna offset (eccentricity)
    sinex_offset = parse_sinex_eccentricity(sinex_content, marker_name)
    if sinex_offset is not None:
        results['marker_found'] = True
        results['antenna_offset']['sinex_value'] = sinex_offset

        if antenna_offset is not None and len(antenna_offset) == 3:
            # Compare E, N, U values with tolerance
            matches = all(abs(sinex_offset[i] - antenna_offset[i]) <= tolerance for i in range(3))
            if matches:
                results['antenna_offset']['valid'] = True
                results['antenna_offset']['message'] = f'Antenna offset matches SINEX [E={sinex_offset[0]:.4f}, N={sinex_offset[1]:.4f}, U={sinex_offset[2]:.4f}]'
            else:
                results['antenna_offset']['valid'] = False
                results['antenna_offset']['message'] = (
                    f'Antenna offset mismatch: RINEX=[E={antenna_offset[0]:.4f}, N={antenna_offset[1]:.4f}, U={antenna_offset[2]:.4f}], '
                    f'SINEX=[E={sinex_offset[0]:.4f}, N={sinex_offset[1]:.4f}, U={sinex_offset[2]:.4f}]'
                )
        else:
            # RINEX didn't provide offset, just log what SINEX has
            results['antenna_offset']['valid'] = None
            results['antenna_offset']['message'] = f'SINEX offset: [E={sinex_offset[0]:.4f}, N={sinex_offset[1]:.4f}, U={sinex_offset[2]:.4f}] (no RINEX value to compare)'
    else:
        # Marker not found in SITE/ECCENTRICITY section
        if antenna_offset is not None and len(antenna_offset) == 3:
            results['antenna_offset']['message'] = f'Marker not found in SINEX SITE/ECCENTRICITY section (RINEX: [E={antenna_offset[0]:.4f}, N={antenna_offset[1]:.4f}, U={antenna_offset[2]:.4f}])'

    # Validate apriori position
    sinex_position = parse_sinex_apriori_position(sinex_content, marker_name)
    if sinex_position is not None:
        results['marker_found'] = True
        results['apriori_position']['sinex_value'] = sinex_position

        if apriori_position is not None and len(apriori_position) == 3:
            # Calculate 3D distance between RINEX and SINEX positions
            import math
            distance = math.sqrt(sum((sinex_position[i] - apriori_position[i]) ** 2 for i in range(3)))

            # Always use SINEX, but warn if large discrepancy
            results['apriori_position']['valid'] = True

            if distance <= 1.0:
                results['apriori_position']['message'] = (
                    f'Using SINEX coordinates (matches RINEX within {distance:.2f}m): '
                    f'[{sinex_position[0]}, {sinex_position[1]}, {sinex_position[2]}]'
                )
            elif distance <= 10.0:
                # Small discrepancy - just informational
                results['apriori_position']['message'] = (
                    f'Using SINEX coordinates (RINEX differs by {distance:.2f}m): '
                    f'[{sinex_position[0]}, {sinex_position[1]}, {sinex_position[2]}]'
                )
            else:
                # Large discrepancy - warn user but still use SINEX
                results['apriori_position']['valid'] = False  # Mark as warning
                results['apriori_position']['message'] = (
                    f'⚠️ Large position discrepancy ({distance:.2f}m) - verify correct station. '
                    f'Using SINEX: [{sinex_position[0]}, {sinex_position[1]}, {sinex_position[2]}], '
                    f'RINEX: [{apriori_position[0]}, {apriori_position[1]}, {apriori_position[2]}]'
                )
        else:
            # RINEX didn't provide position, use SINEX
            results['apriori_position']['valid'] = True
            results['apriori_position']['message'] = f'Using SINEX coordinates: [{sinex_position[0]}, {sinex_position[1]}, {sinex_position[2]}]'

    return results

def download_and_validate_sinex(target_date: datetime, marker_name: str, receiver_type: str, antenna_type: str, antenna_offset: List[float],
                                apriori_position: Optional[List[float]] = None, download_dir: Path = INPUT_PRODUCTS_PATH,
                                progress_callback: Optional[Callable] = None, stop_requested: Optional[Callable] = None) -> tuple[Optional[Path], dict]:
    """
    Download SINEX file and validate RINEX-extracted values against it.

    :param target_date: The date for which to download the SINEX file
    :param marker_name: 4-character marker name
    :param receiver_type: Receiver type from RINEX
    :param antenna_type: Antenna type from RINEX
    :param antenna_offset: Antenna offset [E, N, U] from RINEX
    :param apriori_position: Optional apriori position [X, Y, Z] from RINEX
    :param download_dir: Directory to save the downloaded file
    :param progress_callback: Optional callback for progress updates
    :param stop_requested: Optional callback to check if operation should stop
    :returns: Tuple of (Path to SINEX file or None, validation results dict)
    """
    # Download the SINEX file
    sinex_path = download_sinex_file(target_date, download_dir, progress_callback, stop_requested)

    if sinex_path is None:
        return None, {'error': 'Failed to download SINEX file'}

    # Read and validate
    try:
        with open(sinex_path, 'r', encoding='utf-8', errors='replace') as f:
            sinex_content = f.read()

        results = validate_sinex_values(
            sinex_content, marker_name,
            receiver_type, antenna_type,
            antenna_offset, apriori_position
        )

        return sinex_path, results

    except Exception as e:
        Logger.terminal(f"❌ Error reading SINEX file: {e}")
        return sinex_path, {'error': f'Failed to read SINEX file: {e}'}

def log_sinex_validation_results(results: dict, marker_name: str):
    """
    Log SINEX validation results to the terminal.

    :param results: Validation results dictionary from validate_sinex_values()
    :param marker_name: Marker name for logging context
    """
    if 'error' in results:
        Logger.terminal(f"❌ SINEX validation error: {results['error']}")
        return

    if not results['marker_found']:
        Logger.terminal(f"⚠️ Marker '{marker_name}' not found in SINEX file - validation skipped")
        return

    all_valid = True
    has_validations = False
    Logger.terminal(f"📋 SINEX validation results for marker '{marker_name}':")

    for field in ['receiver_type', 'antenna_type', 'antenna_offset', 'apriori_position']:
        field_result = results.get(field, {})
        message = field_result.get('message', '')

        if field_result.get('valid') is True:
            Logger.terminal(f"   ✅ {field.replace('_', ' ').title()}: {message}")
            has_validations = True
        elif field_result.get('valid') is False:
            Logger.terminal(f"   ⚠️ {field.replace('_', ' ').title()}: {message}")
            all_valid = False
            has_validations = True
        elif message:
            # valid is None but there's a message (info only, no comparison made)
            Logger.terminal(f"   ℹ️ {field.replace('_', ' ').title()}: {message}")

    if has_validations:
        if all_valid:
            Logger.terminal(f"✅ All SINEX validations passed for marker '{marker_name}'")
        else:
            Logger.terminal(f"⚠️ Some SINEX validations failed for marker '{marker_name}' - please review the above warnings")
    else:
        Logger.terminal(f"ℹ️ SINEX data found for marker '{marker_name}' but no comparisons were made (RINEX values may be missing)")

# endregion

if __name__ == "__main__":
    # Test whole file download
    sesh = requests.Session()
    sesh.auth = get_netrc_auth()
    x = Path(f"{INPUT_PRODUCTS_PATH}/COD0MGXFIN_20191950000_01D_01D_OSB.BIA.gz")
    if x.exists():
        x.unlink()
    if x.with_suffix('').exists():
        x.with_suffix('').unlink()
    if x.with_suffix(x.suffix + ".part").exists():
        x.with_suffix(x.suffix + ".part").unlink()

    download_file(f"{BASE_URL}/gnss/products/2062/{x.name}", sesh, INPUT_PRODUCTS_PATH)

    # Test resuming a partial download
    os.remove(x.with_suffix(''))  # should extract file
    y = x.with_suffix(x.suffix + ".part")
    req = sesh.get(f"{BASE_URL}/gnss/products/2062/{x.name}", headers={"Range": f"bytes=0-{CHUNK_SIZE}"}, stream=True)
    with open(y, "wb") as z:
        for chunk in req.iter_content(chunk_size=CHUNK_SIZE):
            if chunk:  # Filters keep-alives
                z.write(chunk)
    Logger.console(f"Downloaded {y.stat().st_size} bytes to {y}.\nAttempting to resume full download...")
    download_file(f"{BASE_URL}/gnss/products/2062/{x.name}", sesh, INPUT_PRODUCTS_PATH)
    Logger.console(f"Success!")
    x.unlink()