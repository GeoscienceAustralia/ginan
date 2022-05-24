# Script for auto-downloading the necessary files to run PPP solutions in Ginan
import click
import random
import ftplib
import logging
import requests
import numpy as np
from time import sleep
from pathlib import Path
from copy import deepcopy
import concurrent.futures
from itertools import repeat
from collections import Counter
import urllib.request as request
from urllib.parse import urlparse
from contextlib import contextmanager
from datetime import datetime, timedelta
from requests_oauthlib import OAuth2Session
from oauthlib.oauth2 import LegacyApplicationClient


from gn_lib.gn_datetime import GPSDate
from gn_lib.gn_download import check_n_download, check_n_download_url, check_file_present, gen_uncomp_filename

API_URL = "https://data.gnss.ga.gov.au/api"
OAUTH_URL = "https://prodgeodesy-openam.geodesy.ga.gov.au/openam/oauth2"


@contextmanager
def ftp_tls(url, **kwargs):
    kwargs.setdefault("timeout", 30)
    with ftplib.FTP_TLS(url, **kwargs) as ftps:
        ftps.login()
        ftps.prot_p()
        yield ftps
        ftps.quit()


def configure_logging(verbose):
    if verbose:
        logging_level = logging.DEBUG
    else:
        logging_level = logging.INFO
    logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
    logging.getLogger().setLevel(logging_level)


def ensure_folders(paths):
    """
    Ensures the list of folders exist in the file system.
    """
    for path in paths:
        if not isinstance(path, Path):
            path = Path(path)
        if not path.is_dir():
            path.mkdir(parents=True)


def generate_long_filename(
    analysis_center: str,  # AAA
    content_type: str,  # CNT
    format_type: str,  # FMT
    initial_epoch: str,  # %Y%j%H%M
    timespan: str,  #
    solution_type: str = "",  # TTT
    sampling_rate: str = "15M",  # SMP
    version: str = "0",  # V
    project: str = "EXP",  # PPP, e.g. EXP, OPS
):
    """
    Function to generate filename with IGS Long Product Filename convention (v1.0) as outlined in
    http://acc.igs.org/repro3/Long_Product_Filenames_v1.0.pdf

    AAAVPPPTTT_YYYYDDDHHMM_LEN_SMP_CNT.FMT[.gz]
    """

    result = (
        f"{analysis_center}{version}{project}"
        f"{solution_type}_"
        f"{initial_epoch}_{timespan}_{sampling_rate}_"
        f"{content_type}.{format_type}"
    )
    return result


def gen_content_specifier(file_ext):
    """
    IGS files following the long filename convention require a content specifier
    Given the file extension, generate the content specifier
    """
    file_ext = file_ext.upper()
    if file_ext == "ERP":
        content = "ERP"
    elif file_ext == "BIA":
        content = "OSB"
    elif file_ext == "SP3":
        content = "ORB"
    elif file_ext == "CLK":
        content = "CLK"
    elif file_ext == "OBX":
        content = "ATT"
    elif file_ext == "TRO":
        content = "TRO"
    return content


def gen_cddis_repro_specifiers(analysis_centre, version, project, solution):
    """
    IGS files following the long filename convention require specifiers
    Some common specifiers are used by the analysis centres that submit to CDDIS
    Given the analysis centre, generate the version, project and solution specifiers (where appropriate)
    """
    if analysis_centre == "ESA":
        version = "3"
    elif analysis_centre == "GFZ":
        version = "2"
    elif analysis_centre == "GRG":
        version = "6"
        project = "RE3"
    elif analysis_centre == "IGS":
        solution = "SNX"
    elif analysis_centre == "JPL":
        project = "R3T"
    elif analysis_centre == "MIT":
        if solution == "FIN":
            solution = "GE-"
    elif analysis_centre == "ULR":
        project = "TST"
    elif analysis_centre == "WHU":
        version = "2"
    return version, project, solution


def gen_prod_filename(
    reference_dt,
    file_ext,
    shift=0,
    AC="igr",
    repro3=False,
    span="01D",
    ver="0",
    proj="R03",
    solution="FIN",
    sampling="05M",
    content="CRD",
):
    """
    Generate filename, GPSDate obj from datetime
    """
    reference_dt -= timedelta(hours=shift)
    gps_date = GPSDate(str(reference_dt.date()))

    if repro3:
        content = gen_content_specifier(file_ext=file_ext)
        ver, proj, solution = gen_cddis_repro_specifiers(
            analysis_centre=AC, version=ver, project=proj, solution=solution
        )
        IC = reference_dt.strftime("%Y%j%H%M")

        prod_file = (
            generate_long_filename(
                analysis_center=AC,
                content_type=content,
                format_type=file_ext,
                initial_epoch=IC,
                timespan=span,
                solution_type=solution,
                sampling_rate=sampling,
                version=ver,
                project=proj,
            )
            + ".gz"
        )
    elif (file_ext == "bia") and (repro3 == False):
        if (datetime.now() - reference_dt).days > 25:
            prod_file = "code_monthly.bia"
        else:
            prod_file = "code.bia"
    elif (file_ext == "snx") and (repro3 == False):
        prod_file = f"igs{gps_date.yr[2:]}P{gps_date.gpswkD}.ssc.Z"
    else:
        prod_file = f"{AC}{gps_date.gpswkD}.{file_ext}.Z"
    # hour = f"{reference_dt.hour:02}"
    return prod_file, gps_date, reference_dt


def gen_prod_basepath(repro3=False, file_ext=None):
    """
    Generate filename, GPSDate obj from datetime
    Optionally, move reference_dt back by "shift" hours
    """
    if repro3:
        basepath = "gnss/products/repro3/"
    elif (file_ext == "bia") & (repro3 == False):
        basepath = "gnss/products/bias/"
    else:
        basepath = "gnss/products/"
    return basepath


def rinex_files_api(params, headers=None):
    """
    Query the GNSS data API for a station.
    """
    if headers is None:
        headers = {}
    response = requests.get(API_URL + "/rinexFiles", params=params, headers=headers)

    if response.status_code == 404:
        # no data available for query
        return []

    response.raise_for_status()
    return response.json()


def authentication_token(client_id, client_secret, username, password):
    oauth = OAuth2Session(client=LegacyApplicationClient(client_id=client_id))

    token = oauth.fetch_token(
        token_url=OAUTH_URL + "/access_token?realm=/",
        scope=["openid", "profile"],
        username=username,
        password=password,
        client_id=client_id,
        client_secret=client_secret,
    )

    return token


def download_gnss_data_entry(entry, output_dir: Path, max_retries=5):
    file_url = entry["fileLocation"]
    retries = 0
    download_done = False
    filename = urlparse(file_url).path.split("/")[-1].split(".")[0] + ".rnx"
    out_path = output_dir / filename
    if out_path.is_file():
        logging.info(f"File {filename} already present in {output_dir}")
    else:
        while not download_done and retries <= max_retries:
            try:
                logging.info(f"Downloading {filename} to {out_path}")
                out = request.urlretrieve(file_url, out_path)
                download_done = True
                logging.info(f"Downloaded {filename}")
            except:
                retries += 1
                if retries > max_retries:
                    logging.warning(f"Failed to download {file_url} and reached maximum retry count ({max_retries}).")

                logging.debug(f"Received an error while try to download {file_url}, retrying({retries}).")
                # Add some backoff time (exponential random as it appears to be contention based?)
                sleep(random.uniform(0.0, 2.0 ** retries))


def available_stations_at_gnss_data_repo(
    start_epoch: datetime,
    end_epoch: datetime,
    stations_list: list,
    file_period="01D",
    fileType="obs",
    rinexVersion="3",
    coverage_fraction=1,
    max_retries=5,
):
    if start_epoch > end_epoch:
        start_epoch, end_epoch = end_epoch, start_epoch

    if file_period == "01D":
        stride = timedelta(days=1)
    else:
        stride = timedelta(hours=1)

    def site_id(entry):
        return entry["siteId"].upper()

    entries = []
    current_epoch = start_epoch
    epoch_count = 0

    while current_epoch + stride <= end_epoch:
        entry_success = False
        retries = 0
        while not entry_success and retries <= max_retries:
            try:
                new_entries = available_stations_at_gnss_data_repo_single_period(
                    current_epoch,
                    current_epoch + stride,
                    stations_list,
                    file_period=file_period,
                    fileType=fileType,
                    rinexVersion=rinexVersion,
                )
                entry_success = True
            except:
                retries += 1
                if retries > max_retries:
                    logging.warning(f"Failed to receive API response and reached maximum retry count ({max_retries}).")
                logging.debug(f"Received an error while attempting API call, retrying({retries}).")
                # Add some backoff time (exponential random as it appears to be contention based?)
                sleep(random.uniform(0.0, 2.0 ** retries))

        entries += new_entries

        current_epoch += stride
        epoch_count += 1

    station_count = Counter(site_id(entry) for entry in entries)
    required_count = int(coverage_fraction * epoch_count)

    return [entry for entry in entries if station_count[site_id(entry)] >= required_count]


def available_stations_at_gnss_data_repo_single_period(
    start_epoch: datetime, end_epoch: datetime, stations_list: list, file_period="01D", fileType="obs", rinexVersion="3"
):
    params = dict(
        stationId=",".join(stations_list),
        startDate=start_epoch.isoformat(),
        endDate=(end_epoch - timedelta(seconds=30)).isoformat(),
        filePeriod=file_period,
        fileType=fileType,
        rinexVersion=rinexVersion,
        decompress=True,
    )
    return rinex_files_api(params)


def download_file_from_cddis(filename, ftp_folder, output_folder: Path, max_retries=5, uncomp=True):
    with ftp_tls("gdc.cddis.eosdis.nasa.gov") as ftps:
        ftps.cwd(ftp_folder)
        retries = 0
        download_done = False
        while not download_done and retries <= max_retries:
            try:
                check_n_download(filename, str(output_folder) + "/", ftps, uncomp=uncomp)
                download_done = True
                logging.info(f"Downloaded {filename}")
            except ftplib.all_errors as e:
                retries += 1
                if retries > max_retries:
                    logging.warning(f"Failed to download {filename} and reached maximum retry count ({max_retries}).")
                    raise e

                logging.debug(f"Received an error ({e}) while try to download {filename}, retrying({retries}).")
                # Add some backoff time (exponential random as it appears to be contention based?)
                sleep(random.uniform(0.0, 2.0 ** retries))


def download_prod_cddis(
    dwndir: Path,
    start_epoch,
    end_epoch,
    file_ext,
    limit=None,
    AC="igr",
    repro3=False,
    span="01D",
    ver="0",
    proj="R03",
    solution="FIN",
    sampling="05M",
    content="CRD",
):
    """
    Download the file/s from CDDIS based on start and end epoch, to the
    provided the download directory (dwndir)
    """
    reference_dt = deepcopy(end_epoch)
    prod_file, gps_date, reference_dt = gen_prod_filename(reference_dt, file_ext, AC=AC, repro3=repro3)
    basepath = gen_prod_basepath(repro3, file_ext)
    if file_ext == "snx":
        shift = 168
        limit = 1
    else:
        shift = 24

    if check_file_present(comp_filename=prod_file, dwndir=str(dwndir)):
        logging.info(f"File {prod_file} already present in {dwndir}")
    else:
        with ftp_tls("gdc.cddis.eosdis.nasa.gov") as ftps:
            if (file_ext == "bia") & (repro3 == False):
                # Download File:
                download_file_from_cddis(
                    filename=prod_file,
                    ftp_folder=f"{basepath}",
                    output_folder=dwndir,
                )
            else:
                success = False
                while not success:
                    try:
                        ftps.cwd(f"{basepath}{gps_date.gpswk}")
                        all_files = ftps.nlst()
                        if not (prod_file in all_files):
                            logging.info(
                                f"{reference_dt} too recent for {prod_file} - Not in {basepath}{gps_date.gpswk}"
                            )
                            prod_file, gps_date, reference_dt = gen_prod_filename(reference_dt, file_ext, shift=shift)
                            ftps.cwd(f"/")
                        else:
                            success = True
                    except ftplib.all_errors as e:
                        logging.info(f"{reference_dt} too recent")
                        logging.info(f"ftp_lib error: {e}")
                        # Shift back one week - 168 hours in a week
                        prod_file, gps_date, reference_dt = gen_prod_filename(
                            reference_dt, file_ext, shift=shift, AC=AC, repro3=repro3
                        )

                # Download File:
                download_file_from_cddis(
                    filename=prod_file,
                    ftp_folder=f"{basepath}{gps_date.gpswk}",
                    output_folder=dwndir,
                )
                count = 1

                remain = reference_dt - start_epoch
                while remain.total_seconds() > 0:
                    if count == limit:
                        remain = timedelta(days=0)
                    else:
                        prod_file, gps_date, reference_dt = gen_prod_filename(reference_dt, file_ext, shift=shift)

                        # Download File:
                        download_file_from_cddis(
                            filename=prod_file,
                            ftp_folder=f"{basepath}{gps_date.gpswk}",
                            output_folder=dwndir,
                        )
                        count += 1
                        remain = reference_dt - start_epoch


def download_atx(dwndir: Path):
    """
    Download the ATX file necessary for running the PEA
    provided the download directory (dwndir)
    """
    url = "https://files.igs.org/pub/station/general/igs14.atx"
    logging.info(f"Downloading ATX file")
    check_n_download_url(url, str(dwndir))
    logging.info(f"ATX file present in {dwndir}")


def download_blq(dwndir: Path):
    """
    Download the BLQ file necessary for running the PEA
    provided the download directory (dwndir)
    """
    url = "https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/OLOAD_GO.BLQ"
    logging.info(f"Downloading BLQ file")
    check_n_download_url(url, str(dwndir))
    logging.info(f"BLQ file present in {dwndir}")


def download_brdc(dwndir: Path, start_epoch, end_epoch, source, rinexVersion="2"):
    """
    Download the most recent BRDC file/s from CDDIS
    provided the download directory (dwndir)
    """
    # Download Broadcast file/s
    if source == "gnss-data":
        api_result = available_stations_at_gnss_data_repo(
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            stations_list=["brdc"],
            fileType="nav",
            rinexVersion=rinexVersion,
        )
        for result in api_result:
            download_gnss_data_entry(result, dwndir)
    elif source == "cddis":
        reference_dt = start_epoch - timedelta(days=1)
        while (end_epoch - reference_dt).total_seconds() > 0:
            doy = reference_dt.strftime("%j")
            yr = reference_dt.strftime("%y")
            brdc_compfile = f"brdc{doy}0.{yr}n.gz"
            if check_file_present(brdc_compfile, dwndir=str(dwndir)):
                logging.info(f"File {brdc_compfile} already present in {dwndir}")
            else:
                download_file_from_cddis(
                    filename=brdc_compfile,
                    ftp_folder=f"gnss/data/daily/{reference_dt.year}/brdc/",
                    output_folder=dwndir,
                )
            reference_dt += timedelta(days=1)


def download_trop_model(dwndir: Path, model="gpt2"):
    """
    Download the relevant troposphere model file/s necessary for running the PEA
    provided the download directory (dwndir) and model
    Default is GPT 2.5
    """
    if model == "gpt2":
        url = "https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/gpt_25.grd"
        check_n_download_url(url, str(dwndir))
        logging.info(f"Troposphere Model files - GPT 2.5 - present in {dwndir}")


def download_mr_sinex_cddis(dwndir: Path, target_date: GPSDate = GPSDate("today")):
    """
    Download the most recent sinex files from SIO, JPL, GFZ, MIT and CODE
    provided the download directory (dwndir) and FTP_TLS client object (ftps)
    """
    with ftp_tls("gdc.cddis.eosdis.nasa.gov") as ftps:
        # Move to directory of current week, get list of available SNX files:
        ftps.cwd(f"gnss/products/")
        if target_date.gpswk not in ftps.nlst():
            GPS_day = int(target_date.gpswkD[-1])
            target_date = GPSDate(target_date.ts - np.timedelta64(7 + GPS_day, "D"))
        ftps.cwd(f"{target_date.gpswk}")
        mr_files = ftps.nlst()
        mr_snx_files = [f for f in mr_files if f == f"igs{target_date.yr[2:]}P{target_date.gpswkD}.ssc.Z"]
        logging.info(f"Searching for file: igs{target_date.yr[2:]}P{target_date.gpswkD}.ssc.Z at CDDIS")
        if mr_snx_files == []:
            while mr_snx_files == []:
                logging.info(f"GPS week {target_date.gpswk} too recent")
                logging.info(f"No IGS snx files found in GPS week {target_date.gpswk}")
                logging.info(f"Moving to GPS week {int(target_date.gpswk) - 1}")
                #    rapid_date -= np.timedelta64(1,'D')
                GPS_day = int(target_date.gpswkD[-1])
                target_date = GPSDate(target_date.ts - np.timedelta64(7 + GPS_day, "D"))

                z_file_snx = f"igs{target_date.yr[2:]}P{target_date.gpswkD}.ssc.Z"
                logging.info(f"Searching for file: {z_file_snx}")

                ftps.cwd("../" + target_date.gpswk)
                mr_files = ftps.nlst()
                mr_snx_files = [f for f in mr_files if f == f"igs{target_date.yr[2:]}P{target_date.gpswkD}.ssc.Z"]
    logging.info("Found IGS file")

    # Download IGS Weekly SNX:
    Z_file = f"igs{target_date.yr[2:]}P{target_date.gpswk}.ssc.Z"
    if check_file_present(Z_file, dwndir=str(dwndir)):
        logging.info(f"File {Z_file} already present in {dwndir}")
    else:
        download_file_from_cddis(
            filename=Z_file,
            ftp_folder=f"gnss/products/{target_date.gpswk}",
            output_folder=dwndir,
        )


def auto_download(
    target_dir,
    preset,
    station_list,
    start_datetime,
    end_datetime,
    most_recent,
    ac,
    atx,
    blq,
    snx,
    nav,
    sp3,
    erp,
    clk,
    bia,
    repro3,
    gpt2,
    product_dir,
    rinex_data_dir,
    trop_dir,
    rinex_file_period,
    datetime_format,
    verbose,
):
    configure_logging(verbose)
    target_dir = Path(target_dir)

    # Assign flags for preset selection
    if preset == "real-time":
        most_recent = True
        atx = True
        blq = True
        trop_model = "gpt2"
        snx = True

    if preset == "igs-station":
        station_list = station_list.split(",")
        atx = True
        blq = True
        trop_model = "gpt2"
        snx = True
        nav = True
        sp3 = True
        erp = True
        clk = True
        bia = True

    # Assign start and end datetimes (if provided)
    if start_datetime:
        start_epoch = datetime.strptime(start_datetime, datetime_format)
        target_date = GPSDate(start_epoch.strftime("%Y-%m-%dT%H:%M"))
    if end_datetime:
        end_epoch = datetime.strptime(end_datetime, datetime_format)

    # If directories haven't been assigned use default: target-dir
    target_dir = Path(target_dir)

    if not product_dir:
        product_dir = target_dir
    else:
        product_dir = Path(product_dir)

    if not rinex_data_dir:
        rinex_data_dir = target_dir
    else:
        rinex_data_dir = Path(rinex_data_dir)

    if not trop_dir:
        trop_dir = target_dir
    else:
        trop_dir = Path(trop_dir)

    # Ensure the directories exist:
    dir_list = [product_dir, rinex_data_dir, trop_dir, target_dir]
    ensure_folders(dir_list)

    # Assign variables based on flags
    if gpt2:
        trop_model = "gpt2"
    if most_recent:
        target_date = GPSDate("today")

    # Download products based on flags
    sampling = ""
    if atx:
        download_atx(dwndir=product_dir)
    if blq:
        download_blq(dwndir=product_dir)
    if trop_model:
        download_trop_model(dwndir=trop_dir, model=trop_model)
    if nav:
        download_brdc(dwndir=product_dir, start_epoch=start_epoch, end_epoch=end_epoch, source="cddis")
    if snx and most_recent:
        download_mr_sinex_cddis(dwndir=target_dir, target_date=target_date)
    if snx and not most_recent:
        if repro3:
            sampling = "01D"
        download_prod_cddis(
            dwndir=product_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="snx",
            repro3=repro3,
            sampling=sampling,
            AC=ac,
        )
    if sp3:
        if repro3:
            sampling = "05M"
        download_prod_cddis(
            dwndir=product_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="sp3",
            repro3=repro3,
            sampling=sampling,
            AC=ac,
        )
    if erp:
        if repro3:
            sampling = "01D"
        download_prod_cddis(
            dwndir=product_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="erp",
            repro3=repro3,
            sampling=sampling,
            AC=ac,
        )
    if clk:
        if repro3:
            sampling = "30S"
        download_prod_cddis(
            dwndir=product_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="clk",
            repro3=repro3,
            sampling=sampling,
            AC=ac,
        )
    if bia:
        if repro3:
            sampling = "01D"
        download_prod_cddis(
            dwndir=product_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext="bia",
            repro3=repro3,
            sampling=sampling,
            AC=ac,
        )

    # Download RINEX files (if station_list provided)
    if station_list:
        api_result = available_stations_at_gnss_data_repo(
            start_epoch=start_epoch, end_epoch=end_epoch, stations_list=station_list, file_period=rinex_file_period
        )
        download_sites = (entry for entry in api_result)
        with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
            list(
                executor.map(
                    download_gnss_data_entry,
                    download_sites,
                    repeat(rinex_data_dir),
                )
            )
        # for result in api_result:
        #     download_gnss_data_entry(result, rinex_data_dir)
        download_list = sorted(list({entry["siteId"].upper() for entry in api_result}))
        missing_stations = list(set(station_list) - set(download_list))
        logging.info(f"Stations not downloaded (missing): {missing_stations}")


@click.command()
@click.option("--target-dir", required=True, help="Directory to place file downloads")
@click.option("--preset", help="Choose from: manual, real-time, igs-station. Default: real-time", default="real-time")
@click.option("--station-list", help="If igs-station option chosen, provide comma-separated list of IGS stations")
@click.option("--start-datetime", help="Start of date-time period to download files for")
@click.option("--end-datetime", help="End of date-time period to download files for")
@click.option("--most-recent", help="Set to download latest version of files", default=False, is_flag=True)
@click.option("--ac", help="Analysis centre of files to download", default="igr")
@click.option("--atx", help="Flag to Download ATX file", default=False, is_flag=True)
@click.option("--blq", help="Flag to Download BLQ file", default=False, is_flag=True)
@click.option("--snx", help="Flag to Download SNX / SSC file", default=False, is_flag=True)
@click.option("--nav", help="Flag to Download navigation / broadcast file/s", default=False, is_flag=True)
@click.option("--sp3", help="Flag to Download SP3 file/s", default=False, is_flag=True)
@click.option("--erp", help="Flag to Download ERP file/s", default=False, is_flag=True)
@click.option("--clk", help="Flag to Download CLK file/s", default=False, is_flag=True)
@click.option("--bia", help="Flag to Download BIA bias file", default=False, is_flag=True)
@click.option("--repro3", help="Flag to use repro3 for all products", default=False, is_flag=True)
@click.option("--gpt2", help="Flag to Download GPT 2.5 file", default=False, is_flag=True)
@click.option("--product-dir", help="Directory to Download product files. Default: target-dir")
@click.option("--rinex-data-dir", help="Directory to Download RINEX data file/s. Default: target-dir")
@click.option("--trop-dir", help="Directory to Download troposphere model file/s. Default: target-dir")
@click.option(
    "--rinex-file-period",
    help="File period of RINEX files to download, e.g. 01D, 01H, 15M. Default: 01D",
    default="01D",
)
@click.option(
    "--datetime-format", help="Format of input datetime string. Default: %Y-%m-%d_%H:%M:%S", default="%Y-%m-%d_%H:%M:%S"
)
@click.option("--verbose", is_flag=True)
def auto_download_main(
    target_dir,
    preset,
    station_list,
    start_datetime,
    end_datetime,
    most_recent,
    ac,
    atx,
    blq,
    snx,
    nav,
    sp3,
    erp,
    clk,
    bia,
    repro3,
    gpt2,
    product_dir,
    rinex_data_dir,
    trop_dir,
    rinex_file_period,
    datetime_format,
    verbose,
):

    auto_download(
        target_dir,
        preset,
        station_list,
        start_datetime,
        end_datetime,
        most_recent,
        ac,
        atx,
        blq,
        snx,
        nav,
        sp3,
        erp,
        clk,
        bia,
        repro3,
        gpt2,
        product_dir,
        rinex_data_dir,
        trop_dir,
        rinex_file_period,
        datetime_format,
        verbose,
    )


if __name__ == "__main__":
    auto_download_main()
