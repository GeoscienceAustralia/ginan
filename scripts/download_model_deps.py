""" Downloads the dependencies required by the Ginan model that are common
	to all executions. These include:
	- IGS antenna calibration file (igsXX.atx) where XX is 14 or 20
	- Ocean Loading model (.BLQ)
	- Troposphere model (GPT 2.5)
"""
import logging
from pathlib import Path

from gnssanalysis.gn_download import check_n_download, check_n_download_url, check_file_present


logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
logging.getLogger().setLevel(logging.INFO)


def ensure_folders(paths: [Path]) -> None:
    """
    Ensures the list of directories exist in the file system.

    :param Directories to create if they do not already exist
    """
    for path in paths:
        if not path.is_dir():
            path.mkdir(parents=True)


# TODO: make download functions async
# TODO: get headers for all files and show content-length total
# in a progress bar
def download_all(target_dir) -> None:
    """Download required files for Ginan to run."""
    ensure_folders([target_dir])
    download_atx(target_dir)
    download_blq(target_dir)
    download_trop_model(target_dir)

    # TODO: Could the following be exposed from peanpod like the ATX file etc?
    # Should have one script that is used to download these products
    # that is shared between examples and the autorun scripts
    # TODO: download_egm(target_dir)
    # TODO: download_jpl(target_dir)
    # TODO: download_tides(target_dir)


def download_atx(target_dir) -> None:
    """
    Download the antenna calibrations file for running the PEA, to the
    provided target directory.
    """
    logging.info("Downloading the antenna calibrations (ATX) file from IGS")

    # Only grab the igs14 file for now.
    # My understanding is that igs2020 is for ITRF2020 which is not
    # yet used for GDA2020
    atx_filename = "igs14.atx"

    # Get the ATX file if not present already:
    if not (target_dir / f"{atx_filename}").is_file():
        url = f"https://files.igs.org/pub/station/general/{atx_filename}"
        check_n_download_url(url, str(target_dir))
        logging.info(f"ATX file present in {target_dir}")


def download_trop_model(target_dir, model: str = "gpt2") -> None:
    """
    Download the relevant troposphere model file/s necessary for running the PEA.

    Default is GPT 2.5
    """
    if model == "gpt2":
        url = "https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/gpt_25.grd"
        check_n_download_url(url, str(target_dir))
        logging.info(f"Troposphere Model files - GPT 2.5 - present in {target_dir}")
    else:
        raise NotImplementedError(
            "Only gpt2 files are supported for the troposphere model \
                                    at the moment."
        )


def download_blq(target_dir) -> None:
    """
    Download the BLQ file necessary for running the PEA.
    """
    url = "https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/OLOAD_GO.BLQ"
    check_n_download_url(url, str(target_dir))
    logging.info(f"BLQ file present in {target_dir}")


def download_egm(target_dir) -> None:
    # TODO: Could this be exposed from peanpod as well?
    # Example: https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/tables/EGM2008.gfc
    # I found this version online but it's http not https
    url = "http://icgem.gfz-potsdam.de/getmodel/gfc/c50128797a9cb62e936337c890e4425f03f0461d7329b09a8cc8561504465340/EGM2008.gfc"
    check_n_download_url(url, str(target_dir))
    logging.info(f"EGM file present in {target_dir}")
