"""
Script for automatically running a Ginan PPP solution
- Downloads necessary files
- Create YAML config file 
- Runs Ginan
"""

import os
import copy
import click
import logging
import itertools
import subprocess
from typing import List
from pathlib import Path
from ast import literal_eval
from datetime import datetime
from collections.abc import Iterable

from auto_generate_yaml import out_pea_yaml
from auto_download_PPP import auto_download


def configure_logging(verbose):
    if verbose:
        logging_level = logging.DEBUG
    else:
        logging_level = logging.INFO
    logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
    logging.getLogger().setLevel(logging_level)


@click.command()
@click.option("--run-mode", required=True, help="Auto-run mode: igs-station, manual.", default="igs-station")
@click.option("--target-path", required=True, help="Directory to place files, e.g. /var/PPP")
@click.option("--template-example", required=True, help="Example template to use, e.g. ex11, ex12. Default: ex11")
@click.option("--station-list", help="If igs-station mode chosen, provide comma-separated list of IGS stations")
@click.option("--start-datetime", help="Start of date-time period over which to run PPP")
@click.option("--end-datetime", help="End of date-time period over which to run PPP")
@click.option("--rinex-data-path", help="Directory to RINEX data, e.g. /var/PPP/data. Default: target-path/data")
@click.option("--product-path", help="Directory for product files, e.g. /var/PPP/product. Default: target-path/product")
@click.option("--output-path", help="Directory for output files, e.g. /var/PPP/output. Default: target-path/output")
@click.option(
    "--ginan-path", help="Path to local ginan directory, e.g. /var/ginan. Default: /bin or ../bin", default=""
)
@click.option("--datetime-format", help="Datetime format, Default: %Y-%m-%d_%H:%M:%S", default="%Y-%m-%d_%H:%M:%S")
@click.option("--analysis-centre", help="Analysis centre of files to download. Default: igr", default="igr")
@click.option("--enable-mongo", help="Flag to write to mongoDB. Default: False", default=False, is_flag=True)
@click.option("--verbose", is_flag=True)
def auto_run_main(
    run_mode,
    target_path,
    template_example,
    station_list,
    start_datetime,
    end_datetime,
    rinex_data_path,
    product_path,
    output_path,
    ginan_path,
    datetime_format,
    analysis_centre,
    enable_mongo,
    verbose,
):

    configure_logging(verbose)
    target_path = Path(target_path)

    if product_path:
        product_path = Path(product_path)
    else:
        product_path = target_path / "product"

    trop_path = product_path / "gpt2"

    if rinex_data_path:
        rinex_data_path = Path(rinex_data_path)
    else:
        rinex_data_path = target_path / "data"

    if output_path:
        output_path = Path(output_path)
    else:
        output_path = target_path / "output"

    if ginan_path:
        ginan_path = Path(ginan_path)
        binaries_path = ginan_path / "bin"
        try:
            pea = next((binaries_path / "pea").glob("pea"))
        except StopIteration:
            raise RuntimeError(
                f"Cannot find Ginan binary files at {binaries_path}'\n"
                "If ginan directory is current working directory, do not provide ginan-path option\n"
                "Otherwise, please provide path to ginan directory with the ginan-path option again\n"
            )
    else:
        ginan_path = Path.cwd()
        try:
            pea = next((ginan_path / "bin").glob("pea"))
        except StopIteration:
            ginan_path = Path("..")
            try:
                pea = next((ginan_path / "bin").glob("pea"))
            except StopIteration:
                raise RuntimeError(
                    f"Cannot find Ginan binary files at '/bin', '../bin'\n"
                    "Please provide path to ginan directory with the ginan-path option\n"
                )

    template_path = next((ginan_path / "exampleConfigs").glob(template_example + "*.yaml"))

    if run_mode == "igs-station":
        atx = True
        blq = True
        trop_model = "gpt2"
        snx = True
        nav = True
        sp3 = True
        erp = True
        clk = True
        bia = True

    auto_download(
        target_dir=target_path,
        preset=run_mode,
        station_list=station_list,
        start_datetime=start_datetime,
        end_datetime=end_datetime,
        most_recent=False,
        analysis_centre=analysis_centre,
        atx=atx,
        blq=blq,
        snx=snx,
        nav=nav,
        sp3=sp3,
        erp=erp,
        clk=clk,
        bia=bia,
        repro3=False,
        gpt2=True,
        product_dir=product_path,
        rinex_data_dir=rinex_data_path,
        trop_dir=trop_path,
        rinex_file_period="01D",
        datetime_format=datetime_format,
        verbose=verbose,
    )

    outdatetime_format = "%Y-%m-%d %H:%M:%S"
    # Assign start and end datetimes (if provided)
    if start_datetime:
        start_epoch = datetime.strptime(start_datetime, datetime_format).strftime(outdatetime_format)
    else:
        start_epoch = ""

    if end_datetime:
        end_epoch = datetime.strptime(end_datetime, datetime_format).strftime(outdatetime_format)
    else:
        end_epoch = ""

    out_pea_yaml(
        start_epoch=start_epoch,
        end_epoch=end_epoch,
        config_out_dir=target_path,
        template_path=template_path,
        product_dir=product_path,
        data_dir=rinex_data_path,
        pea_out_dir=output_path,
        relative_to_dir=None,
        trop_model=trop_model,
        trop_dir=trop_path,
        enable_mongo=enable_mongo,
        overrides=(),
    )

    pea_config = next(target_path.glob(f"*{template_example}*.yaml"))
    # Run Ginan PEA for PPP solution
    subprocess.run([str(pea), "--config", str(pea_config)])


if __name__ == "__main__":
    auto_run_main()
