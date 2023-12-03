""" This script determines the dependency files that are required
    to post-process a given static RINEX obs file with Ginan, and
    downloads them from an appropriate repository.

    These include the Earth Rotation Parameter file (ERP), the orbit
    file (SP3) and the clock file (CLK).
"""
import concurrent.futures
from contextlib import contextmanager
from datetime import datetime, timedelta
from itertools import repeat
from pathlib import Path
from time import sleep

import ftplib
import logging
import random

from parse_rinex_header import parse_v3_header

from gnssanalysis.filenames import generate_IGS_long_filename
from gnssanalysis.gn_download import check_n_download
from gnssanalysis.gn_datetime import dt2gpswk


logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
logging.getLogger().setLevel(logging.INFO)


@contextmanager
def ftp_tls(url: str, **kwargs) -> None:
    kwargs.setdefault("timeout", 30)
    with ftplib.FTP_TLS(url, **kwargs) as ftps:
        ftps.login()
        ftps.prot_p()
        yield ftps
        ftps.quit()


def download_rinex_deps(filepath: Path, target_dir: Path):
    header = parse_v3_header(filepath)

    filenames = generate_filenames(header)

    # TODO: Find stations nearby

    download_queue = []
    for filename in filenames:
        fpath = target_dir / filename
        if fpath.is_file():
            logging.info(f"{filename} already exists in target directory {target_dir}")
        else:
            download_queue.append(filename)

    gps_week = dt2gpswk(header["first_obs_time"])

    if download_queue:
        # Download from IGS
        download_multiple_files_from_cddis(
            download_queue, ftp_folder=f"gnss/products/{gps_week}", output_folder=target_dir
        )
    else:
        logging.info(f"All files exist in target directory {target_dir}")


def generate_filenames(header: dict):
    # TODO: Eventually iterate through each GPS day between start and
    # end epochs in the file. To start with, only process the first GPS day
    # in the file.
    start_epoch = header["first_obs_time"]
    start_date = datetime(start_epoch.year, start_epoch.month, start_epoch.day, 0, 0, 0)
    timespan = timedelta(days=1)

    # TODO: Just download RAP products for now, eventually
    # do best available: FIN, RAP or ULT depending on what
    # is available
    erp_filename = generate_IGS_long_filename(
        analysis_center="IGS",
        content_type="ERP",
        format_type="ERP",
        start_epoch=start_date,
        timespan=timespan,
        solution_type="RAP",
        sampling_rate="01D",
        project="OPS",
    )

    sp3_filename = generate_IGS_long_filename(
        analysis_center="IGS",
        content_type="ORB",
        format_type="SP3",
        start_epoch=start_date,
        timespan=timespan,
        solution_type="RAP",
        sampling_rate="15M",
        project="OPS",
    )

    clk_filename = generate_IGS_long_filename(
        analysis_center="IGS",
        content_type="CLK",
        format_type="CLK",
        start_epoch=start_date,
        timespan=timespan,
        solution_type="RAP",
        sampling_rate="05M",
        project="OPS",
    )

    return [erp_filename, sp3_filename, clk_filename]


def download_file_from_cddis(
    filename: str, ftp_folder: str, output_folder: Path, max_retries: int = 3, uncomp: bool = True
) -> None:
    with ftp_tls("gdc.cddis.eosdis.nasa.gov") as ftps:
        ftps.cwd(ftp_folder)
        retries = 0
        download_done = False
        while not download_done and retries <= max_retries:
            try:
                logging.info(f"Attempting Download of: {filename}")
                # TODO: Need to add .gz to all of the filenames, but no exceptions
                # were raised by this method even though the files without ".gz" did not exist.
                # Need to find this method and look at what it does
                check_n_download(filename + ".gz", str(output_folder) + "/", ftps, uncomp=uncomp)
                download_done = True
                logging.info(f"Downloaded {filename}")
            except ftplib.all_errors as e:
                retries += 1
                if retries > max_retries:
                    logging.warning(f"Failed to download {filename} and reached maximum retry count ({max_retries}).")
                    if (output_folder / filename).is_file():
                        (output_folder / filename).unlink()
                    raise e

                logging.debug(f"Received an error ({e}) while try to download {filename}, retrying({retries}).")
                # Add some backoff time (exponential random as it appears to be contention based?)
                sleep(random.uniform(0.0, 2.0**retries))


def download_multiple_files_from_cddis(files: list, ftp_folder: str, output_folder: Path) -> None:
    with concurrent.futures.ThreadPoolExecutor() as executor:
        # Wrap this in a list to force iteration of results and so get the first exception if any were raised
        list(executor.map(download_file_from_cddis, files, repeat(ftp_folder), repeat(output_folder)))
