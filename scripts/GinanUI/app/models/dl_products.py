import gzip, os, shutil, unlzw3, requests
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup, SoupStrainer
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Callable, Generator, List

from scripts.GinanUI.app.utils.cddis_email import get_netrc_auth
from scripts.GinanUI.app.utils.common_dirs import INPUT_PRODUCTS_PATH
from scripts.GinanUI.app.utils.gn_functions import GPSDate
from scripts.GinanUI.app.utils.logger import Logger

BASE_URL = "https://cddis.nasa.gov/archive"
GPS_ORIGIN = np.datetime64("1980-01-06 00:00:00")  # Magic date from gn_functions
MAX_RETRIES = 3  # download attempts
CHUNK_SIZE = 8192  # 8 KiB
COMPRESSED_FILETYPE = (".gz", ".gzip", ".Z")  # ignore any others (maybe add crx2rnx using hatanaka package)

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


def date_to_gpswk(date: datetime) -> int:
    return int(GPSDate(np.datetime64(date)).gpswk)


def gpswk_to_date(gps_week: int, gps_day: int = 0) -> datetime:
    return GPSDate(GPS_ORIGIN + np.timedelta64(gps_week, "W") + np.timedelta64(gps_day, "D")).as_datetime


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
        columns=["analysis_center", "project", "date", "solution_type", "period", "resolution", "content", "format"])

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
                    date = gpswk_to_date(gps_week)
                    if 0 < day < 7:
                        date += timedelta(days=day)
                        period = timedelta(days=1)
                    else:
                        period = timedelta(days=7)

                else:
                    # e.g. GRG0OPSFIN_20232620000_01D_01D_SOL.SNX.gz
                    # AAA0OPSSNX_YYYYDDDHHMM_LEN_SMP_CNT.FMT.gz
                    center = filename[0:3]  # e.g. "COD"
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

                if _format in target_files and start_time <= date <= end_time:
                    products.loc[len(products)] = {
                        "analysis_center": center,
                        "project": project,
                        "date": date,
                        "solution_type": _type,
                        "period": period,
                        "resolution": sampling_resolution,
                        "content": content,
                        "format": _format
                    }
            except (ValueError, IndexError):
                # Skips md5 sums and other non-conforming files
                continue
    products = products.drop_duplicates(inplace=False)  # resets indexes too
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

def extract_file(filepath: Path) -> Path:
    """
    Extracts [".gz", ".gzip", ".Z"] files with gzip and unlzw3 respectively.
    Deletes compressed file after extraction.

    :param filepath: compressed file path
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
    filepath.unlink()
    return Path(finalpath)


def download_file(url: str, session: requests.Session, download_dir: Path = INPUT_PRODUCTS_PATH,
                  progress_callback: Optional[Callable] = None,
                  stop_requested: Callable = None) -> Path:
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
    :raises RuntimeError: Stop requested during download
    :raises Exception: Max retries reached
    :return:
    """

    filepath = Path(download_dir / url.split("/")[-1])  # Download dir + filename
    # 1. When file already exists, extract if possible, then return
    if filepath.exists():
        if filepath.suffix in COMPRESSED_FILETYPE:
            return extract_file(filepath)
        else:
            return filepath

    # 2. Check if an extracted version of this file already exists
    if filepath.suffix in COMPRESSED_FILETYPE:
        potential_decompressed = filepath.with_suffix('')  # Remove one suffix
        if potential_decompressed.exists():
            return potential_decompressed

    # 3. Download the file in chunks (.part)
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
                        raise RuntimeError("Stop requested during download.")

                    if _chunk:  # Filters keep-alives
                        partial_out.write(_chunk)
                        downloaded += len(_chunk)

                        if progress_callback:
                            percent = int(downloaded / total_size * 100)
                            progress_callback(filepath.name, percent)

            os.rename(_partial, filepath)

            if filepath.suffix in COMPRESSED_FILETYPE:
                return extract_file(filepath)
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

    # 1. Generate filenames from the DataFrame
    downloads = []
    for _, row in products.iterrows():
        gps_week = date_to_gpswk(row.date)
        if gps_week < 2237:
            # AAAWWWWD.TYP.Z
            # e.g. COD22360.FIN.SNX.gz
            if row.period == timedelta(days=7):
                day = 7
            else:
                day = int((row.date - gpswk_to_date(gps_week)).days)
            filename = f"{row.analysis_center.lower()}{gps_week}{day}.{row.format.lower()}.Z"
        else:
            # e.g. GRG0OPSFIN_20232620000_01D_01D_SOL.SNX.gz
            # AAA0OPSSNX_YYYYDDDHHMM_LEN_SMP_CNT.FMT.gz
            filename = f"{row.analysis_center}0{row.project}{row.solution_type}_{row.date.strftime('%Y%j%H%M')}_{row.period.days:02d}D_{row.resolution}_{row.content}.{row.format}.gz"

        url = f"{BASE_URL}/gnss/products/{gps_week}/{filename}"
        downloads.append(url)

    if dl_urls:
        downloads.extend(dl_urls)

    Logger.terminal(f"📦 {len(downloads)} files to check or download")
    download_dir.mkdir(parents=True, exist_ok=True)
    (download_dir / "tables").mkdir(parents=True, exist_ok=True)
    _sesh = requests.Session()
    _sesh.auth = get_netrc_auth()
    for url in downloads:
        _x = url.split("/")
        if len(_x) < 2:
            fin_dir = download_dir
        else:
            fin_dir = download_dir / "tables" if _x[-2] == "tables" else download_dir
        yield download_file(url, _sesh, fin_dir, progress_callback, stop_requested)


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