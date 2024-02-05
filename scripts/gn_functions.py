"""Base time conversion functions"""

from datetime import datetime as _datetime
from pathlib import Path as _Path
import logging
import shutil
import os as _os
import time as _time
import gzip as _gzip
import tarfile as _tarfile

from pathlib import Path as _Path
from typing import Optional as _Optional, Union as _Union
from urllib import request as _request
from urllib.error import HTTPError as _HTTPError

import numpy as _np
import hatanaka as _hatanaka

GPS_ORIGIN = _np.datetime64("1980-01-06 00:00:00")


def gpsweekD(yr, doy, wkday_suff=False):
    """
    Convert year, day-of-year to GPS week format: WWWWD or WWWW
    Based on code from Kristine Larson's gps.py
    https://github.com/kristinemlarson/gnssIR_python/gps.py

    Input:
    yr - year (int)
    doy - day-of-year (int)

    Output:
    GPS Week in WWWWD format - weeks since 7 Jan 1980 + day of week number (str)
    """

    # Set up the date and time variables
    yr = int(yr)
    doy = int(doy)
    dt = _datetime.strptime(f"{yr}-{doy:03d} 01", "%Y-%j %H")

    wkday = dt.weekday() + 1

    if wkday == 7:
        wkday = 0

    mn, dy, hr = dt.month, dt.day, dt.hour

    if mn <= 2:
        yr = yr - 1
        mn = mn + 12

    JD = _np.floor(365.25 * yr) + _np.floor(30.6001 * (mn + 1)) + dy + hr / 24.0 + 1720981.5
    GPS_wk = int(_np.floor((JD - 2444244.5) / 7.0))

    if wkday_suff:
        return str(GPS_wk) + str(wkday)
    else:
        return str(GPS_wk)


class GPSDate:
    """
    Representation of datetime that provides easy access to
    useful properties.

    Usage:
    today = GPSDate("today")
    tomorrow = today.next
    print(f"today year: {today.year}, doy: {today.dy}, GPS week and weekday: {today.gpswkD}")
    print(f"tomorrow year: {tomorrow.year}, doy: {tomorrow.dy}, GPS week and weekday: {tomorrow.gpswkD}")
    """

    def __init__(self, ts: _np.datetime64):
        if isinstance(ts, str):
            ts = _np.datetime64(ts)

        self.ts = ts

    @property
    def as_datetime(self):
        """Convert to Python `datetime` object."""
        return self.ts.astype(_datetime)

    @property
    def yr(self):
        """Year"""
        return self.as_datetime.strftime("%Y")

    @property
    def dy(self):
        """Day of year"""
        return self.as_datetime.strftime("%j")

    @property
    def gpswk(self):
        """GPS week"""
        return gpsweekD(self.yr, self.dy, wkday_suff=False)

    @property
    def gpswkD(self):
        """GPS week with weekday suffix"""
        return gpsweekD(self.yr, self.dy, wkday_suff=True)

    @property
    def next(self):
        """The following day"""
        return GPSDate(self.ts + 1)

    @property
    def prev(self):
        """The previous day"""
        return GPSDate(self.ts - 1)

    def __str__(self):
        """Same string representation as the underlying numpy datetime64 object"""
        return str(self.ts)


def dt2gpswk(dt, wkday_suff=False, both=False):
    """
    Convert the given datetime object to a GPS week (option to include day suffix)
    """
    yr = dt.strftime("%Y")
    doy = dt.strftime("%j")
    if not both:
        return gpsweekD(yr, doy, wkday_suff=wkday_suff)
    else:
        return gpsweekD(yr, doy, wkday_suff=False), gpsweekD(yr, doy, wkday_suff=True)


def gpswkD2dt(gpswkD):
    """
    Convert from GPS-Week-Day (WWWWDD) format to datetime object
    """
    if type(gpswkD) != str:
        gpswkD = str(gpswkD)
    dt_64 = GPS_ORIGIN + _np.timedelta64(int(gpswkD[:-1]), "W") + _np.timedelta64(int(gpswkD[-1]), "D")
    return dt_64.astype(_datetime)


def check_file_present(comp_filename: str, dwndir: str) -> bool:
    """Check if file comp_filename already present in directory dwndir"""

    if dwndir[-1] != "/":
        dwndir += "/"

    uncomp_filename = gen_uncomp_filename(comp_filename)
    uncomp_file = _Path(dwndir + uncomp_filename)

    if uncomp_file.is_file():
        logging.debug(f"File {uncomp_file.name} already present in {dwndir}")
        present = True
    else:
        present = False

    return present


def gen_uncomp_filename(comp_filename: str) -> str:
    """Name of uncompressed filename given the compressed name"""
    if comp_filename.endswith(".crx.gz"):
        return comp_filename[:-6] + "rnx"
    elif comp_filename.endswith(".gz"):
        return comp_filename[:-3]
    elif comp_filename.endswith(".Z"):
        return comp_filename[:-2]
    elif comp_filename.endswith(".bz2"):
        return comp_filename[:-4]
    else:
        return comp_filename


def decompress_file(input_filepath: _Path, delete_after_decompression: bool = False) -> _Path:
    """
    Given the file path to a compressed file, decompress it in-place
    Assumption is that filename of final file is the stem of the compressed filename for .gz files
    Option to delete original compressed file after decompression (Default: False)
    """
    # Get absolulte path
    input_file = input_filepath.resolve()
    # Determine extension
    extension = input_file.suffix
    # Check if file is a .tar.gz file (if so, assign new extension)
    if extension == ".gz" and input_file.stem[-4:] == ".tar":
        extension = ".tar.gz"
    if extension not in [".gz", ".tar", ".tar.gz", ".Z"]:
        logging.info(f"Input file extension [{extension}] not supported - must be .gz, .tar.gz or .tar to decompress")
        return None
    if extension == ".gz":
        # Special case for the extraction of RNX / CRX files (uses hatanaka module)
        if input_file.stem[-4:] in [".rnx", ".crx"]:
            output_file = _hatanaka.decompress_on_disk(path=input_file, delete=delete_after_decompression).resolve()
            return output_file
        # Output file definition:
        output_file = input_file.with_suffix("")
        # Open the input gzip file and the output file
        with _gzip.open(input_file, "rb") as f_in, output_file.open("wb") as f_out:
            # Copy the decompressed content from input to output
            f_out.write(f_in.read())
            logging.info(f"Decompression of {input_file.name} to {output_file.name} in {output_file.parent} complete")
        if delete_after_decompression:
            logging.info(f"Deleting {input_file.name}")
            input_file.unlink()
        return output_file
    elif extension == ".tar" or extension == ".tar.gz":
        # Open the input tar file and the output file
        with _tarfile.open(input_file, "r") as tar:
            # Get name of file inside tar.gz file (assuming only one file)
            filename = tar.getmembers()[0].name
            output_file = input_file.parent / filename
            # Extract contents
            tar.extractall(input_file.parent)
            logging.info(f"Decompression of {input_file.name} to {output_file.name} in {output_file.parent} complete")
        if delete_after_decompression:
            logging.info(f"Deleting {input_file.name}")
            input_file.unlink()
        return output_file
    elif extension == ".Z":
        # At the moment, we assume that the .Z file is from RINEX
        if input_file.stem[-1] not in ["d", "n"]:  # RINEX 2 files: "d" observation data, "n" broadcast ephemerides
            logging.info(f"Only decompression of RINEX files currently supported for .Z decompression")
            return None
        output_file = _hatanaka.decompress_on_disk(path=input_file, delete=delete_after_decompression).resolve()
        logging.debug(f"Decompression of {input_file.name} to {output_file.name} in {output_file.parent} complete")
        return output_file


def download_url(url: str, destfile: _Union[str, _os.PathLike], max_retries: int = 5) -> _Optional[_Path]:
    logging.info(f'requesting "{url}"')
    for retry in range(1, max_retries + 1):
        try:
            with _request.urlopen(url) as response:
                if response.status == 200:
                    logging.info(f"downloading from {url} to {destfile}")
                    with open(destfile, "wb") as out_file:
                        shutil.copyfileobj(response, out_file)
            return _Path(destfile)
        except _HTTPError as err:
            logging.error(f" HTTP Error {err.code}: {err.reason}")
            if err.code == 404:
                return None  # File Not Found on the server so no point in retrying
            t_seconds = 2**retry
            logging.error(f"Retry No. {retry} in {t_seconds} seconds")
            _time.sleep(t_seconds)
            if retry >= max_retries:
                logging.error(f"Maximum number of retries reached: {max_retries}. File not downloaded")
                return None
    logging.error("Maximum retries exceeded in download_url with no clear outcome, returning None")
    return None


def check_n_download_url(url, dwndir, filename=False):
    """
    Download single file given URL to download from.
    Optionally provide filename if different from url name
    """
    if dwndir[-1] != "/":
        dwndir += "/"

    if not filename:
        filename = url[url.rfind("/") + 1 :]

    if not check_file_present(filename, dwndir):
        logging.debug(f"Downloading {_Path(url).name}")
        out_f = _Path(dwndir) / filename
        download_url(url, out_f)


def check_n_download(comp_filename, dwndir, ftps, uncomp=True, remove_comp_file=False, no_check=False):
    """Download compressed file to dwndir if not already present and optionally uncompress"""

    success = False

    comp_file = _Path(dwndir + comp_filename)

    if dwndir[-1] != "/":
        dwndir += "/"

    if no_check or (not check_file_present(comp_filename, dwndir)):
        logging.debug(f"Downloading {comp_filename}")

        with open(comp_file, "wb") as local_f:
            ftps.retrbinary(f"RETR {comp_filename}", local_f.write)
        if uncomp:
            decompress_file(comp_file, delete_after_decompression=remove_comp_file)
            logging.debug(f"Downloaded and uncompressed {comp_filename}")
        else:
            logging.debug(f"Downloaded {comp_filename}")

        success = True

    else:
        success = True

    return success
