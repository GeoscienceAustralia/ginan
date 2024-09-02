#!/usr/bin/env python3

"""
Script to start a PEA instance to record and decode a SSR stream in real-time.
The PEA instance will run infinitely once it is started.
"""

import json
import click
import logging
import subprocess
import gnssanalysis as ga
import ruamel.yaml as yaml
from typing import Any
from pathlib import Path
from datetime import datetime


def load_yaml(
    yaml_path: Path,
) -> Any:
    """
    Load a YAML file to a dictionary-like object.

    :param Path yaml_path: Path of the YAML file to load
    :return Any: YAML config options as a dictionary-like object
    """
    with yaml_path.open("r", encoding="utf-8") as stream:
        yaml_dict = yaml.safe_load(stream)
    return yaml_dict


def write_yaml(
    yaml_dict: Any,
    yaml_path: Path,
) -> None:
    """
    Write config options saved in a dictionary-like object to a YAML file.

    :param Any yaml_dict: YAML config options as a dictionary-like object
    :param Path yaml_path: Path of the YAML file to write
    :return None
    """
    with yaml_path.open("w", encoding="utf-8") as outfile:
        yaml.safe_dump(yaml_dict, outfile, default_flow_style=False)


@click.command()
@click.option(
    "--template-config", required=True, help="Path of template YAML config", type=Path
)
@click.option(
    "--job-dir", required=True, help="Directory where data is processed", type=Path
)
@click.option(
    "--product-dir",
    required=True,
    help="Directory where product files are placed",
    type=Path,
)
@click.option(
    "--bcep-mountpoint",
    required=True,
    help="Mountpoint of broadcast ephemeris stream",
    default="BCEP00BKG0",
    type=str,
)
@click.option(
    "--ssr-mountpoint",
    required=True,
    help="Mountpoint of SSR stream to record",
    type=str,
)
@click.option(
    "--output-json",
    help="Log decoded messages into json files",
    default=True,
    is_flag=True,
)
@click.option(
    "--rotation-period",
    help="Recording length of each output file in seconds",
    default=86400,
    type=int,
)
@click.option(
    "--interval",
    help="Epoch interval of data processing in seconds",
    default=1,
    type=int,
)
@click.option(
    "--max-epochs",
    help="Maximum number of epochs to process, default is infinite",
    default=0,
    type=int,
)
@click.option("--verbose", is_flag=True)
def record_ssr_stream_main(
    template_config,
    job_dir,
    product_dir,
    bcep_mountpoint,
    ssr_mountpoint,
    output_json,
    rotation_period,
    interval,
    max_epochs,
    verbose,
):
    ga.gn_utils.configure_logging(verbose)

    now = datetime.now()
    yrdoy = now.strftime("%Y%j")
    timestamp = f"{yrdoy}{now.hour:02d}{now.minute:02d}"

    # Prepare job folder and config
    sub_job_dir = job_dir / ssr_mountpoint
    ga.gn_utils.ensure_folders([sub_job_dir])

    config_path = sub_job_dir / f"record_ssr_stream_{ssr_mountpoint}.yaml"

    logging.info(f"Writing config {config_path}")

    # Load config
    config = load_yaml(template_config)

    # Inputs
    config["inputs"]["inputs_root"] = str(product_dir)
    config["inputs"]["satellite_data"]["rtcm_inputs"]["rtcm_inputs"] = [
        bcep_mountpoint,
        ssr_mountpoint,
    ]
    config["inputs"]["satellite_data"]["rtcm_inputs"]["ssr_antenna_offset"] = (
        "APC"
        if ssr_mountpoint[:4] == "SSRA"
        else "COM" if ssr_mountpoint[:4] == "SSRC" else "UNSPECIFIED"
    )

    # Outputs
    config["outputs"]["metadata"]["config_description"] = ssr_mountpoint
    config["outputs"]["outputs_root"] = str(sub_job_dir)
    config["outputs"]["output_rotation"]["period"] = rotation_period
    config["outputs"]["decoded_rtcm"]["output"] = output_json
    config["outputs"]["rinex_nav"][
        "filename"
    ] = f"{bcep_mountpoint}_<YYYY><DDD><HH><mm>_NAV.rnx"

    # Processing options
    config["processing_options"]["epoch_control"]["epoch_interval"] = interval
    config["processing_options"]["epoch_control"]["wait_next_epoch"] = interval
    config["processing_options"]["epoch_control"]["max_epochs"] = max_epochs

    # Write config
    write_yaml(config, config_path)
    logging.info(f"Config written to {config_path}")

    logging.info(f"Running PEA for {ssr_mountpoint} ...")

    # Process
    proc = subprocess.Popen(
        ["nohup", "/data/acs2/ginan/bin/pea", "--config", str(config_path)],
        stdout=(sub_job_dir / f"record_ssr_stream_{timestamp}.log").open("w"),
        stderr=subprocess.STDOUT,
    )

    # Save PID of started PEA instance to file to kill later
    pid_file_path = job_dir / "pid.json"
    with pid_file_path.open("a") as pid_file:
        json.dump({"PID": proc.pid, "command": " ".join(proc.args)}, pid_file)
        pid_file.write("\n")
        logging.debug(f"PID of started PEA instance saved to file {pid_file_path}")

    proc.wait()

    logging.info("PEA completed\n")


if __name__ == "__main__":
    record_ssr_stream_main()
