#!/usr/bin/env python3

"""
Script to regularly download/update necessary product files used for real-time processing, specifically
- igs20.atx
- igs_satellite_metadata.snx
- yaw files
Other files can be added in the future. It will run infinitely and attempt download periodically once it is started.
"""

import time
import click
import logging
import gnssanalysis as ga
from pathlib import Path


def download_rt_products(
    product_dir: Path,
) -> None:
    """
    Download latest product files, including igs20.atx, igs_satellite_metadata.snx, and yaw files
    for SSR streams recording.

    :param Path product_dir: Directory where downloaded product files to place
    :return None
    """
    logging.info("Updating real-time products ...")

    model_dir = product_dir / "tables"

    # Download required products
    ga.gn_download.download_atx(
        download_dir=product_dir, long_filename=True, if_file_present="replace"
    )
    ga.gn_download.download_satellite_metadata_snx(download_dir=product_dir, if_file_present="replace")
    ga.gn_download.download_yaw_files(download_dir=model_dir, if_file_present="replace")

    logging.info("Real-time products updated\n")


@click.command()
@click.option(
    "--product-dir",
    required=True,
    help="Directory where product files are placed",
    type=Path,
)
@click.option(
    "--interval",
    required=True,
    help="Time interval to check and download products in seconds",
    default=86400,
    type=int,
)
@click.option("--verbose", is_flag=True)
def download_rt_products_main(
    product_dir,
    interval,
    verbose,
):
    ga.gn_utils.configure_logging(verbose)

    # Check every <interval> seconds if local files are outdated, and if so, download products
    while True:
        download_rt_products(product_dir)

        now = time.time()
        seconds = interval - now % interval
        time.sleep(seconds)


if __name__ == "__main__":
    download_rt_products_main()
