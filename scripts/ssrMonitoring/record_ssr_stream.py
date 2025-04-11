#!/usr/bin/env python3

"""
Script to start a PEA instance to record and decode a SSR stream in real-time.
The PEA instance will run infinitely once it is started.
"""

import logging
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any

import click
import ruamel.yaml as yaml

import gnssanalysis as ga
from kill_pids import save_pids


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


def update_config(
    template_config: Path,
    config_path: Path,
    output_dir: Path,
    product_dir: Path,
    bcep_mountpoint: str,
    ssr_mountpoint: str,
    rotation_period: int,
    output_json: bool,
    interval: int,
    max_epochs: int = 0,
) -> None:
    """
    Update config options from a template YAML file so that the PEA can record and decode a SSR stream
    based on the input options. The updated config options will be saved to a new YAML file.

    :param Path template_config: Path of template YAML config
    :param Path config_path: Path of the YAML file to write config options to
    :param Path output_dir: Directory to save the PEA outputs
    :param Path product_dir: Directory where product files are placed
    :param str bcep_mountpoint: Mountpoint of broadcast ephemeris stream
    :param str ssr_mountpoint: Mountpoint of the SSR stream to record
    :param int rotation_period: Recording length of each output file in seconds
    :param bool output_json: Log decoded messages into JSON files
    :param int interval: Epoch interval of data processing in seconds
    :param int max_epochs: Maximum number of epochs to process, defaults to 0 (infinite)
    :return None
    """
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
    config["outputs"]["outputs_root"] = str(output_dir)
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


def record_ssr_stream(
    config_path: Path,
    output_dir: Path,
    pid_file_path: Path,
    ssr_mountpoint: str,
) -> None:
    """Record and decode a SSR stream in real-time with the PEA

    :param Path config_path: Path of the YAML file to write config options to
    :param Path output_dir: Directory to save the PEA outputs
    :param Path pid_file_path: Path of the JSON file to log PIDs to
    :param str ssr_mountpoint: Mountpoint of the SSR stream to record
    :return None
    """
    logging.info(f"Running PEA for {ssr_mountpoint} ...")

    now = datetime.now()
    yrdoy = now.strftime("%Y%j")
    timestamp = f"{yrdoy}{now.hour:02d}{now.minute:02d}"

    # Process
    proc = subprocess.Popen(
        ["nohup", "/data/acs2/ginan/bin/pea", "--config", str(config_path)],
        stdout=(output_dir / f"record_ssr_stream_{timestamp}.log").open("w"),
        stderr=subprocess.STDOUT,
    )

    # Save PID of started PEA instance to file to kill later
    pid_list = [{"PID": proc.pid, "command": " ".join(proc.args)}]
    save_pids(pid_file_path, pid_list)

    proc.wait()

    logging.info("PEA completed\n")


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
    help="Mountpoint of the SSR stream to record",
    type=str,
)
@click.option(
    "--output-json",
    help="Log decoded messages into JSON files",
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

    # Prepare output directory
    output_dir = job_dir / ssr_mountpoint
    ga.gn_utils.ensure_folders([output_dir])

    # Prepare config
    config_path = output_dir / f"record_ssr_stream_{ssr_mountpoint}.yaml"
    update_config(
        template_config,
        config_path,
        output_dir,
        product_dir,
        bcep_mountpoint,
        ssr_mountpoint,
        rotation_period,
        output_json,
        interval,
        max_epochs,
    )

    # Run the PEA to record SSR stream
    pid_file_path = job_dir / "pid.json"
    record_ssr_stream(config_path, output_dir, pid_file_path, ssr_mountpoint)


if __name__ == "__main__":
    record_ssr_stream_main()
