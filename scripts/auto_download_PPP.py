# Script for auto-downloading the necessary files to run PPP solutions in Ginan
# import boto3
import json
import click
import random
import ftplib
import logging
import requests
import numpy as np
from time import sleep
from pathlib import Path
from typing import Tuple
from urllib.parse import urlparse
from contextlib import contextmanager
from datetime import datetime, timedelta

from gnssanalysis.gn_datetime import GPSDate
from gnssanalysis.gn_download import (
    download_product_from_cddis,
    decompress_file,
    generate_content_type,
    generate_sampling_rate,
    generate_product_filename,
    download_file_from_cddis,
    check_whether_to_download,
    attempt_url_download,
    long_filename_cddis_cutoff,
)
from gnssanalysis.gn_utils import configure_logging, ensure_folders

API_URL = "https://data.gnss.ga.gov.au/api"


@contextmanager
def ftp_tls(url: str, **kwargs) -> None:
    kwargs.setdefault("timeout", 30)
    with ftplib.FTP_TLS(url, **kwargs) as ftps:
        ftps.login()
        ftps.prot_p()
        yield ftps
        ftps.quit()


def download_atx(download_dir: Path, long_filename: bool = False, if_file_present: str = "prompt_user") -> None:
    """
    Download the ATX file necessary for running the PEA provided the download directory (download_dir)
    """

    if long_filename:
        atx_filename = "igs20.atx"
    else:
        atx_filename = "igs14.atx"
    ensure_folders([download_dir])
    url = f"https://files.igs.org/pub/station/general/{atx_filename}"
    attempt_url_download(
        download_dir=download_dir, url=url, filename=atx_filename, type_of_file="ATX", if_file_present=if_file_present
    )


def download_atmosphere_loading_model(download_dir: Path, if_file_present: str = "prompt_user") -> Path:
    """
    Download the Atmospheric loading BLQ file necessary for running the PPP example
    provided the download directory (download_dir) and what to do if file already present (if_file_present)
    """
    ensure_folders([download_dir])
    download_filepath = attempt_url_download(
        download_dir=download_dir,
        url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/ALOAD_GO.BLQ.gz",
        filename="ALOAD_GO.BLQ.gz",
        type_of_file="Atmospheric Tide Loading Model",
        if_file_present=if_file_present,
    )

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_brdc(
    download_dir: Path,
    start_epoch: datetime,
    end_epoch: datetime,
    source: str = "cddis",
    rinexVersion: str = "3",
    filePeriod: str = "01D",
    if_file_present: str = "prompt_user",
) -> None:
    """
    Download the most recent BRDC file/s from CDDIS
    provided the download directory (download_dir)
    """
    # Download Broadcast file/s
    logging.info("Downloading Broadcast files")
    if source.lower() == "gnss-data":
        download_files_from_gnss_data(
            station_list=["BRDC"],
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            data_dir=download_dir,
            file_period=filePeriod,
            file_type="nav",
            rinex_version=rinexVersion,
            if_file_present=if_file_present,
        )
    elif source.lower() == "cddis":
        reference_dt = start_epoch - timedelta(days=1)
        while (end_epoch - reference_dt).total_seconds() > 0:
            doy = reference_dt.strftime("%j")
            brdc_compfile = f"BRDC00IGS_R_{reference_dt.year}{doy}0000_01D_MN.rnx.gz"  # DZ: download MN file
            if check_whether_to_download(
                filename=brdc_compfile, download_dir=download_dir, if_file_present=if_file_present
            ):

                download_file_from_cddis(
                    filename=brdc_compfile,
                    ftp_folder=f"gnss/data/daily/{reference_dt.year}/brdc/",
                    output_folder=download_dir,
                    if_file_present=if_file_present,
                )
            reference_dt += timedelta(days=1)


def download_geomagnetic_model(download_dir: Path, model: str = "igrf14", if_file_present: str = "prompt_user") -> Path:
    """
    Download the International Geomagnetic Reference Field model file necessary for running the PPP example
    provided the download directory (download_dir)
    Default: IGRF14 coefficients
    """
    if model == "igrf14":
        ensure_folders([download_dir])
        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/igrf14coeffs.txt.gz",
            filename="igrf14coeffs.txt.gz",
            type_of_file="Geomagnetic Field coefficients - IGRF14",
            if_file_present=if_file_present,
        )
    else:
        logging.info(f"Unsupported Geomagnetic Field coefficients model type - {model}")
        download_filepath = None

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_geopotential_model(
    download_dir: Path, model: str = "egm2008", if_file_present: str = "prompt_user"
) -> Path:
    """
    Download the Geopotential model file/s necessary for running the PPP example
    provided the download directory (download_dir)
    Default: EGM2008
    """
    if model == "egm2008":
        ensure_folders([download_dir])
        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/EGM2008.gfc.gz",
            filename="EGM2008.gfc.gz",
            type_of_file="Geopotential Model - EGM2008",
            if_file_present=if_file_present,
        )
    else:
        logging.info(f"Unsupported Geopotential model type - {model}")
        download_filepath = None

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_ocean_loading_model(download_dir: Path, if_file_present: str = "prompt_user") -> Path:
    """
    Download the Ocean Loading BLQ file necessary for running the PPP example
    provided the download directory (download_dir) and what to do if file already present (if_file_present)
    """
    ensure_folders([download_dir])
    download_filepath = attempt_url_download(
        download_dir=download_dir,
        url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/OLOAD_GO.BLQ.gz",
        filename="OLOAD_GO.BLQ.gz",
        type_of_file="Ocean Tide Loading Model",
        if_file_present=if_file_present,
    )

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_ocean_pole_tide_file(download_dir: Path, if_file_present: str = "prompt_user") -> Path:
    """
    Download the Ocean Pole Tide Loading coefficients file necessary for running the PPP example
    provided the download directory (download_dir) and what to do if file already present (if_file_present)
    """
    ensure_folders([download_dir])
    download_filepath = attempt_url_download(
        download_dir=download_dir,
        url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/opoleloadcoefcmcor.txt.gz",
        filename="opoleloadcoefcmcor.txt.gz",
        type_of_file="Ocean Pole Tide Loading Coefficients",
        if_file_present=if_file_present,
    )

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_ocean_tide_potential_model(
    download_dir: Path, if_file_present: str = "prompt_user", model: str = "fes2014b"
) -> Path:
    """
    Download the Ocean Tide Potential Model file necessary for running the PPP example
    provided the download directory (download_dir) and what to do if file already present (if_file_present)
    """
    if model == "fes2014b":
        ensure_folders([download_dir])
        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/fes2014b_Cnm-Snm.dat.gz",
            filename="fes2014b_Cnm-Snm.dat.gz",
            type_of_file="Ocean Tide Potential Model - fes2014b",
            if_file_present=if_file_present,
        )
    else:
        logging.info(f"Unsupported Ocean Tide Potential Model type - {model}")
        download_filepath = None

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_planetary_ephemerides_file(
    download_dir: Path, if_file_present: str = "prompt_user", ephem_file: str = "DE436.1950.2050"
) -> Path:
    """
    Download the Planetary Ephemerides file necessary for running the PPP example
    provided the download directory (download_dir) and what to do if file already present (if_file_present)
    Default: DE436.1950.2050
    """
    if ephem_file == "DE436.1950.2050":
        ensure_folders([download_dir])
        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/DE436.1950.2050.gz",
            filename="DE436.1950.2050.gz",
            type_of_file="Planetary Ephemerides - DE436.1950.2050",
            if_file_present=if_file_present,
        )
    else:
        logging.info(f"Unsupported Planetary Ephemerides type - {ephem_file}")
        download_filepath = None

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_trop_model(download_dir: Path, if_file_present: str = "prompt_user", model: str = "gpt2") -> Path:
    """
    Download the relevant troposphere model file/s necessary for running the PEA
    provided the download directory (download_dir) and model
    Default is GPT 2.5
    """
    if model == "gpt2":
        ensure_folders([download_dir])
        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url="https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/gpt_25.grd.gz",
            filename="gpt_25.grd.gz",
            type_of_file="Troposphere Model",
            if_file_present=if_file_present,
        )
    else:
        logging.info(f"Unsupported Troposphere model type - {model}")
        download_filepath = None

    if download_filepath:
        download_filepath = decompress_file(input_filepath=download_filepath, delete_after_decompression=True)

    return download_filepath


def download_iau2000_file(download_dir: Path, start_epoch: datetime, if_file_present: str = "prompt_user"):
    """
    Download relevant IAU2000 file from CDDIS or IERS based on start_epoch of data
    """
    ensure_folders([download_dir])
    # Download most recent daily IAU2000 file if running for a session within the past week (data is within 3 months)
    if datetime.now() - start_epoch < timedelta(weeks=1):
        url_dir = "daily/"
        iau2000_filename = "finals2000A.daily"
        logging.info("Attempting Download of finals2000A.daily file")
    # Otherwise download the IAU2000 file dating back to 1992
    else:
        url_dir = "standard/"
        iau2000_filename = "finals2000A.data"
        logging.info("Attempting Download of finals2000A.data file")
    # Attempt download from CDDIS first, if that fails try the IERS website
    try:
        logging.info("Downloading IAU2000 file from CDDIS")
        if check_whether_to_download(
            filename="finals.data.iau2000.txt", download_dir=download_dir, if_file_present=if_file_present
        ):
            download_filepath = download_file_from_cddis(
                filename=iau2000_filename,
                ftp_folder="products/iers",
                output_folder=download_dir,
                decompress=False,
                if_file_present=if_file_present,
            )
            (download_dir / iau2000_filename).rename(download_dir / "finals.data.iau2000.txt")
        else:
            return None
    except:
        logging.info("Failed CDDIS download - Downloading IAU2000 file from IERS")
        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url="https://datacenter.iers.org/products/eop/rapid/" + url_dir + iau2000_filename,
            filename="finals.data.iau2000.txt",
            type_of_file="EOP IAU2000",
            if_file_present=if_file_present,
        )
    return download_filepath


def download_satellite_metadata_snx(download_dir: Path, if_file_present: str = "prompt_user") -> Path:
    """
    Download the most recent IGS satellite metadata file
    """
    ensure_folders([download_dir])
    download_filepath = attempt_url_download(
        download_dir=download_dir,
        url="https://files.igs.org/pub/station/general/igs_satellite_metadata.snx",
        filename="igs_satellite_metadata.snx",
        type_of_file="IGS satellite metadata",
        if_file_present=if_file_present,
    )
    return download_filepath


def download_yaw_files(download_dir: Path, if_file_present: str = "prompt_user"):
    """
    Download yaw rate / bias files
    """
    ensure_folders([download_dir])
    urls = [
        "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/bds_yaw_modes.snx.gz",
        "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/qzss_yaw_modes.snx.gz",
        "https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/products/tables/sat_yaw_bias_rate.snx.gz",
    ]
    download_filepaths = []
    targets = ["bds_yaw_modes.snx.gz", "qzss_yaw_modes.snx.gz", "sat_yaw_bias_rate.snx.gz"]

    for url, target in zip(urls, targets):

        download_filepath = attempt_url_download(
            download_dir=download_dir,
            url=url,
            filename=target,
            type_of_file="Yaw Model SNX",
            if_file_present=if_file_present,
        )
        if download_filepath:
            download_filepaths.append(decompress_file(download_filepath, delete_after_decompression=True))

    return download_filepaths


def download_most_recent_cddis_file(
    download_dir: Path,
    pointer_date: GPSDate,
    file_type: str = "SNX",
    long_filename: bool = False,
    analysis_center: str = "IGS",
    if_file_present: str = "prompt_user",
) -> None:
    """
    Download the most recent files from IGS provided the download directory (download_dir) session, and file type
    """
    with ftp_tls("gdc.cddis.eosdis.nasa.gov") as ftps:
        # Move to directory of current week:
        ftps.cwd(f"gnss/products/")
        if pointer_date.gpswk not in ftps.nlst():
            GPS_day = int(pointer_date.gpswkD[-1])
            pointer_date = GPSDate(pointer_date.ts - np.timedelta64(7 + GPS_day, "D"))
        ftps.cwd(f"{pointer_date.gpswk}")
        # Search for most recent file
        logging.info(f"Searching for most recent {file_type} files - Long Filename set to: {long_filename}")
        target_filename, pointer_date, ftps = search_for_most_recent_file(
            pointer_date=pointer_date,
            ftps=ftps,
            long_filename=long_filename,
            file_type="SNX",
            analysis_center=analysis_center,
            timespan=timedelta(days=1),
            solution_type="SNX",
            sampling_rate="01D",
            content_type="CRD",
        )
    # Download recent file:
    download_file_from_cddis(
        filename=target_filename,
        ftp_folder=f"gnss/products/{pointer_date.gpswk}",
        output_folder=download_dir,
        if_file_present=if_file_present,
    )


def search_for_most_recent_file(
    pointer_date: GPSDate,
    ftps: ftplib.FTP_TLS,
    long_filename: bool = False,
    file_type: str = "SNX",
    analysis_center: str = "IGS",
    timespan: timedelta = timedelta(days=7),
    solution_type: str = "SNX",
    sampling_rate: str = "07D",
    content_type: str = None,
    project_type: str = "OPS",
) -> Tuple[str, GPSDate, ftplib.FTP_TLS]:
    """
    Find the most recent file on CDDIS of file type: file_type
    """
    if content_type == None:
        content_type = generate_content_type(file_type, analysis_center)
    # Get list of available files and those of file type: file_type
    most_recent_files_in_dir = ftps.nlst()
    target_filename, pointer_date, _ = generate_product_filename(
        reference_start=pointer_date.as_datetime,
        file_ext=file_type,
        long_filename=long_filename,
        analysis_center=analysis_center,
        timespan=timespan,
        solution_type=solution_type,
        sampling_rate=sampling_rate,
        content_type=content_type,
        project=project_type,
    )
    file_list = [f for f in most_recent_files_in_dir if f == target_filename]
    # If no files of file_type available, keep looking back week by week:
    while file_list == []:
        logging.info(f"GPS week {pointer_date.gpswk} too recent")
        logging.info(f"No IGS {file_type} files found in GPS week {pointer_date.gpswk}")
        logging.info(f"Moving to GPS week {int(pointer_date.gpswk) - 1}")
        # Move pointer_date back and search
        GPS_day = int(pointer_date.gpswkD[-1])
        pointer_date = GPSDate(pointer_date.ts - np.timedelta64(7 + GPS_day, "D"))
        target_filename, _, _ = generate_product_filename(
            reference_start=pointer_date.as_datetime,
            file_ext=file_type,
            long_filename=long_filename,
            analysis_center=analysis_center,
            timespan=timespan,
            solution_type=solution_type,
            sampling_rate=sampling_rate,
            content_type=content_type,
            project=project_type,
        )
        # pointer_date = GPSDate(np.datetime64(reference_epoch))
        logging.info(f"Searching for file: {target_filename}")
        ftps.cwd("../" + pointer_date.gpswk)
        most_recent_files_in_dir = ftps.nlst()
        file_list = [f for f in most_recent_files_in_dir if f == target_filename]
    logging.info("Found IGS file")
    return target_filename, pointer_date, ftps


def download_gnss_data_entry(
    entry: dict, output_dir: Path, max_retries: int = 3, if_file_present: str = "prompt_user"
) -> str:
    file_url = entry["fileLocation"]
    file_type = entry["fileType"]
    retries = 0
    download_done = False
    if file_type == "obs":
        filename = urlparse(file_url).path.split("/")[-1].split(".")[0] + ".rnx"
    elif file_type == "nav":
        filename = ".".join(urlparse(file_url).path.split("/")[-1].split(".")[:2])

    while not download_done or retries <= max_retries:
        try:
            # logging.info(f"Downloading {filename} to {out_path}")
            download_filepath = attempt_url_download(
                download_dir=output_dir,
                url=file_url,
                filename=filename,
                type_of_file="RNX",
                if_file_present=if_file_present,
            )
            download_done = True
            # logging.info(f"Downloaded {filename}")
            return download_filepath
        except:
            retries += 1
            if retries > max_retries:
                logging.warning(f"Failed to download {file_url} and reached maximum retry count ({max_retries}).")

            logging.debug(f"Received an error while try to download {file_url}, retrying({retries}).")
            # Add some backoff time (exponential random as it appears to be contention based?)
            sleep(random.uniform(0.0, 2.0**retries))


def download_files_from_gnss_data(
    station_list: list,
    start_epoch: datetime,
    end_epoch: datetime,
    data_dir: Path,
    file_period: str = "01D",
    rinex_version: int = 3,
    file_type: str = "obs",
    decompress: str = "true",
    if_file_present: str = "prompt_user",
) -> None:
    "Given a list of stations, start and end datetime, and a path to place files, download RINEX data from gnss-data"
    # Parameters for the Query:
    QUERY_PARAMS = {
        "metadataStatus": "valid",
        "stationId": ",".join(station_list),
        "fileType": file_type,
        "rinexVersion": rinex_version,
        "filePeriod": file_period,
        "decompress": decompress,
        "startDate": start_epoch.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "endDate": end_epoch.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "tenantId": "default",
    }
    # Submit Request:
    request = requests.get(API_URL + "/rinexFiles", params=QUERY_PARAMS, headers={})
    request.raise_for_status()
    # Download results:
    files_downloaded = []
    for query_response_item in json.loads(request.content):
        filename = download_gnss_data_entry(
            entry=query_response_item, output_dir=data_dir, max_retries=3, if_file_present=if_file_present
        )
        try:
            files_downloaded.append(filename.name.upper())
        except AttributeError:
            files_downloaded.append(filename)
    if files_downloaded == []:
        logging.info("No files downloaded")
    else:
        stations_downloaded = set([filename[:4] for filename in files_downloaded if filename != None])
        missing_stations = set(station_list) - stations_downloaded
        logging.info(f"Not downloaded / missing: {list(missing_stations)}")


def most_recent_6_hour():
    """
    Returns a datetime object set to the most recent hour divisible by 6: (0000, 0600, 1200 or 1800)
    """
    now_time = datetime.now()
    now_hour = now_time.hour
    # Subtract the remainder of division by 6, to get the most recent hour evenly divisible by 6.
    # E.g. for hour 17: 17 % 6 = 5. 17-5=12.
    latest_hour_interval = now_hour - (now_hour % 6)
    return now_time.replace(hour=latest_hour_interval, minute=0, second=0, microsecond=0)


def auto_download(
    target_dir: Path,
    preset: str,
    station_list: str,
    start_datetime: str,
    end_datetime: str,
    replace: bool,
    dont_replace: bool,
    most_recent: bool,
    analysis_center: str,
    atx: bool,
    aload: bool,
    igrf: bool,
    egm: bool,
    oload: bool,
    opole: bool,
    fes: bool,
    planet: bool,
    sat_meta: bool,
    yaw: bool,
    snx: bool,
    nav: bool,
    sp3: bool,
    erp: bool,
    clk: bool,
    bia: bool,
    gpt2: bool,
    rinex_data_dir: Path,
    trop_dir: Path,
    model_dir: Path,
    solution_type: str,
    project_type: str,
    rinex_file_period: str,
    bia_ac: str,
    iau2000: bool,
    datetime_format: str,
    data_source: str,
    verbose: bool,
) -> None:
    configure_logging(verbose)

    # Assign flags for preset selection
    if preset == "real-time":
        most_recent = True
        atx = True
        oload = True
        gpt2 = True
        snx = True
        sat_meta = True
        yaw = True

    if preset == "igs-station":
        atx = True
        oload = True
        aload = True
        igrf = True
        opole = True
        planet = True
        gpt2 = True
        snx = True
        sat_meta = True
        yaw = True
        nav = True
        sp3 = True
        erp = True
        clk = True
        bia = True

    if solution_type in ["FIN", "RAP"]:
        timespan = timedelta(days=1)
    elif solution_type == "ULT":
        timespan = timedelta(days=2)

    # Assign start and end datetimes (if provided)
    if start_datetime:
        start_epoch = datetime.strptime(start_datetime, datetime_format)
        start_gpsdate = GPSDate(start_epoch.strftime("%Y-%m-%dT%H:%M"))
        long_filename = long_filename_cddis_cutoff(epoch=start_epoch)
    else:
        start_epoch = datetime.now()
        start_gpsdate = GPSDate("today")
        long_filename = long_filename_cddis_cutoff(epoch=datetime.today())

    if end_datetime:
        end_epoch = datetime.strptime(end_datetime, datetime_format)

    # If directories haven't been assigned use default: target-dir
    if not rinex_data_dir:
        rinex_data_dir = target_dir
    if not trop_dir:
        trop_dir = target_dir / "tables"
    if not model_dir:
        model_dir = target_dir / "tables"
    if not rinex_file_period:
        rinex_file_period = "01D"

    # Determine what to do when file already present in target-dir:
    if replace and dont_replace:
        raise Exception("Cannot set both --replace and --dont-replace flags")
    elif replace:
        if_file_present = "replace"
    elif dont_replace:
        if_file_present = "dont_replace"
    else:
        if_file_present = "prompt_user"

    # Ensure the directories exist:
    dir_list = [target_dir, rinex_data_dir, trop_dir, model_dir]
    ensure_folders(dir_list)

    # Assign variables based on flags
    if gpt2:
        trop_model = "gpt2"
    else:
        trop_model = None
    if most_recent:
        start_gpsdate = GPSDate("today")
        start_epoch = most_recent_6_hour()
        end_epoch = datetime.now()
        long_filename = long_filename_cddis_cutoff(epoch=datetime.today())

    # Download products based on flags
    if atx:
        download_atx(download_dir=target_dir, long_filename=long_filename, if_file_present=if_file_present)
    if oload:
        download_ocean_loading_model(download_dir=model_dir, if_file_present=if_file_present)
    if aload:
        download_atmosphere_loading_model(download_dir=model_dir, if_file_present=if_file_present)
    if igrf:
        download_geomagnetic_model(download_dir=model_dir, if_file_present=if_file_present)
    if egm:
        download_geopotential_model(download_dir=model_dir, if_file_present=if_file_present)
    if opole:
        download_ocean_pole_tide_file(download_dir=model_dir, if_file_present=if_file_present)
    if fes:
        download_ocean_tide_potential_model(download_dir=model_dir, if_file_present=if_file_present)
    if planet:
        download_planetary_ephemerides_file(download_dir=model_dir, if_file_present=if_file_present)
    if trop_model:
        download_trop_model(download_dir=trop_dir, model=trop_model, if_file_present=if_file_present)
    if sat_meta:
        download_satellite_metadata_snx(download_dir=target_dir, if_file_present=if_file_present)
    if yaw:
        download_yaw_files(download_dir=model_dir, if_file_present=if_file_present)
    if nav:
        download_brdc(
            download_dir=target_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            source=data_source,
            if_file_present=if_file_present,
        )
    if snx and most_recent:
        download_most_recent_cddis_file(
            download_dir=target_dir,
            pointer_date=start_gpsdate,
            file_type="SNX",
            long_filename=long_filename,
            if_file_present=if_file_present,
        )
    if snx and not most_recent:
        try:
            download_product_from_cddis(
                download_dir=target_dir,
                start_epoch=start_epoch,
                end_epoch=end_epoch,
                file_ext="SNX",
                limit=None,  # DZ: removed limit for downloading files of multiple days
                long_filename=long_filename,
                analysis_center="IGS",
                solution_type="SNX",
                sampling_rate=generate_sampling_rate(file_ext="SNX", analysis_center="IGS", solution_type="SNX"),
                timespan=timedelta(days=1),
                if_file_present=if_file_present,
            )
        except ftplib.all_errors as e:
            logging.info(f"Received an error ({e}) while try to download - date too recent.")
            logging.info(f"Downloading most recent SNX file available.")
            download_most_recent_cddis_file(
                download_dir=target_dir,
                pointer_date=start_gpsdate,
                file_type="SNX",
                long_filename=long_filename,
                if_file_present=if_file_present,
            )
    if sp3:
        download_product_from_cddis(
            download_dir=target_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="SP3",
            limit=None,
            long_filename=long_filename,
            analysis_center=analysis_center,
            solution_type=solution_type,
            project_type=project_type,
            sampling_rate=generate_sampling_rate(
                file_ext="SP3", analysis_center=analysis_center, solution_type=solution_type
            ),
            timespan=timespan,
            if_file_present=if_file_present,
        )
    if erp:
        if iau2000:
            # If IAU2000 option is chosen, download IAU2000 data file for Earth rotation parameters
            download_iau2000_file(download_dir=target_dir, start_epoch=start_epoch, if_file_present=if_file_present)
        else:
            # Otherwise, download the ERP file
            download_product_from_cddis(
                download_dir=target_dir,
                start_epoch=start_epoch,
                end_epoch=end_epoch,
                file_ext="ERP",
                limit=None,
                long_filename=long_filename,
                analysis_center=analysis_center,
                solution_type=solution_type,
                project_type=project_type,
                sampling_rate=generate_sampling_rate(
                    file_ext="ERP", analysis_center=analysis_center, solution_type=solution_type
                ),
                timespan=timespan,
                if_file_present=if_file_present,
            )
    if clk:
        download_product_from_cddis(
            download_dir=target_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="CLK",
            limit=None,
            long_filename=long_filename,
            analysis_center=analysis_center,
            solution_type=solution_type,
            project_type=project_type,
            sampling_rate=generate_sampling_rate(
                file_ext="CLK", analysis_center=analysis_center, solution_type=solution_type
            ),
            timespan=timespan,
            if_file_present=if_file_present,
        )
    if bia:
        download_product_from_cddis(
            download_dir=target_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="BIA",
            limit=None,
            long_filename=long_filename,
            analysis_center=bia_ac,
            solution_type=solution_type,
            sampling_rate=generate_sampling_rate(
                file_ext="BIA", analysis_center=analysis_center, solution_type=solution_type
            ),
            timespan=timespan,
            if_file_present=if_file_present,
        )
    if station_list:
        # Download RINEX files from gnss-data (if station_list provided)
        download_files_from_gnss_data(
            station_list=station_list,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            data_dir=rinex_data_dir,
            file_period=rinex_file_period,
            file_type="obs",
            if_file_present=if_file_present,
        )


@click.command()
@click.option("--target-dir", required=True, help="Directory to place file downloads", type=Path)
@click.option(
    "--preset",
    help="""Choose from:
    \n\n-'manual' (choose individual flags to download desired files),
    \n\n-'real-time' (download files for processing real-time streams),
    \n\n-'igs-station' (download files for processing IGS CORS stations - include start/end-datetime)
    \n\nDefault: manual""",
    default="manual",
    type=str,
)
@click.option(
    "--station-list",
    help="Provide comma-separated list of IGS stations to download - daily observation RNX files",
    type=str,
)
@click.option(
    "--station-list-file",
    help="Read file of newline separated list of IGS stations to download - takes precedence over option --station-list",
    type=click.File("r"),
)
@click.option("--start-datetime", help="Start of date-time period to download files for", type=str)
@click.option("--end-datetime", help="End of date-time period to download files for", type=str)
@click.option("--replace", help=" Re-download all files already present in target-dir", default=False, is_flag=True)
@click.option("--dont-replace", help="Skip all files already present in target-dir", default=False, is_flag=True)
@click.option("--most-recent", help="Set to download latest version of files", default=False, is_flag=True)
@click.option("--analysis-center", help="Analysis center of files to download", default="IGS", type=str)
@click.option("--atx", help="Flag to Download ATX file", default=False, is_flag=True)
@click.option("--aload", help="Flag to Download Atmospheric Loading file", default=False, is_flag=True)
@click.option("--igrf", help="Flag to Download IGRF13 file", default=False, is_flag=True)
@click.option("--egm", help="Flag to Download EGM2008 file", default=False, is_flag=True)
@click.option("--oload", help="Flag to Download Ocean Tide Loading file", default=False, is_flag=True)
@click.option("--opole", help="Flag to Download Ocean Pole Tide Coefficients", default=False, is_flag=True)
@click.option("--fes", help="Flag to Download FES2014b Ocean Potential File", default=False, is_flag=True)
@click.option("--planet", help="Flag to Download Planetary Ephemerides: DE436.1950.2050", default=False, is_flag=True)
@click.option("--sat-meta", help="Flag to Download Satellite Metadata SNX", default=False, is_flag=True)
@click.option("--yaw", help="Flag to Download Satellite Yaw files", default=False, is_flag=True)
@click.option("--snx", help="Flag to Download Station Position SNX / SSC file", default=False, is_flag=True)
@click.option("--nav", help="Flag to Download navigation / broadcast file/s", default=False, is_flag=True)
@click.option("--sp3", help="Flag to Download SP3 file/s", default=False, is_flag=True)
@click.option("--erp", help="Flag to Download ERP file/s", default=False, is_flag=True)
@click.option("--clk", help="Flag to Download CLK file/s", default=False, is_flag=True)
@click.option("--bia", help="Flag to Download BIA bias file", default=False, is_flag=True)
@click.option("--gpt2", help="Flag to Download GPT 2.5 file", default=False, is_flag=True)
@click.option("--rinex-data-dir", help="Directory to Download RINEX data file/s. Default: target-dir", type=Path)
@click.option("--trop-dir", help="Directory to Download troposphere model file/s. Default: target-dir", type=Path)
@click.option("--model-dir", help="Directory to Download static model files. Default: target-dir / tables", type=Path)
@click.option(
    "--solution-type",
    help="The solution type of products to download from CDDIS. 'FIN': final, or 'RAP': rapid, or 'ULT': ultra-rapid. Default: RAP",
    default="RAP",
    type=str,
)
@click.option(
    "--project-type",
    help="The project type of products to download from CDDIS. 'OPS', 'MGX', 'EXP'. Default: OPS",
    default="OPS",
    type=str,
)
@click.option(
    "--rinex-file-period",
    help="File period of RINEX files to download, e.g. 01D, 01H, 15M. Default: 01D",
    default="01D",
    type=str,
)
@click.option("--bia-ac", help="Analysis center of BIA files to download. Default: COD", default="COD", type=str)
@click.option(
    "--datetime-format",
    help="Format of input datetime string. Default: %Y-%m-%d_%H:%M:%S",
    default="%Y-%m-%d_%H:%M:%S",
    type=str,
)
@click.option("--iau2000", help="Flag to download IAU2000 file for ERP. Default: True", default=True, type=bool)
@click.option(
    "--datetime-format",
    help="Format of input datetime string. Default: %Y-%m-%d_%H:%M:%S",
    default="%Y-%m-%d_%H:%M:%S",
    type=str,
)
@click.option(
    "--data-source",
    help="Source of data for Broadcast files: CDDIS or gnss-data. Default: gnss-data",
    default="gnss-data",
    type=str,
)
@click.option("--verbose", is_flag=True)
def auto_download_main(
    target_dir,
    preset,
    station_list,
    station_list_file,
    start_datetime,
    end_datetime,
    replace,
    dont_replace,
    most_recent,
    analysis_center,
    atx,
    aload,
    igrf,
    egm,
    oload,
    opole,
    fes,
    planet,
    sat_meta,
    yaw,
    snx,
    nav,
    sp3,
    erp,
    clk,
    bia,
    gpt2,
    rinex_data_dir,
    trop_dir,
    model_dir,
    solution_type,
    project_type,
    rinex_file_period,
    bia_ac,
    iau2000,
    datetime_format,
    data_source,
    verbose,
):
    try:
        station_list
    except NameError:
        station_list = None
    if not station_list == None:
        station_list = station_list.split(",")
    if station_list_file:
        content = station_list_file.read()
        station_list = content.split("\n")
    auto_download(
        target_dir,
        preset,
        station_list,
        start_datetime,
        end_datetime,
        replace,
        dont_replace,
        most_recent,
        analysis_center,
        atx,
        aload,
        igrf,
        egm,
        oload,
        opole,
        fes,
        planet,
        sat_meta,
        yaw,
        snx,
        nav,
        sp3,
        erp,
        clk,
        bia,
        gpt2,
        rinex_data_dir,
        trop_dir,
        model_dir,
        solution_type,
        project_type,  # DZ: add project_type option
        rinex_file_period,
        bia_ac,
        iau2000,
        datetime_format,
        data_source,
        verbose,
    )


if __name__ == "__main__":
    auto_download_main()
