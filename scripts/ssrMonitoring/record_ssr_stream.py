#!/usr/bin/env python3

"""
Script to start a PEA instance to record and decode a SSR stream in real-time.
The PEA instance will run infinitely once it is started.
"""

import json
import logging
import subprocess
import time
from datetime import datetime
from pathlib import Path
from typing import Any

import click
from ruamel.yaml import YAML

import gnssanalysis as ga
from kill_pids import save_pids


yaml = YAML(typ="safe", pure=True)
yaml.default_flow_style = False


def load_yaml(yaml_path: Path) -> Any:
    """
    Load a YAML file to a dictionary-like object.

    :param Path yaml_path: Path of the YAML file to load
    :return Any: YAML config options as a dictionary-like object
    """
    with yaml_path.open("r", encoding="utf-8") as stream:
        yaml_dict = yaml.load(stream)
    return yaml_dict


def write_yaml(yaml_dict: Any, yaml_path: Path) -> None:
    """
    Write config options saved in a dictionary-like object to a YAML file.

    :param Any yaml_dict: YAML config options as a dictionary-like object
    :param Path yaml_path: Path of the YAML file to write
    :return None
    """
    with yaml_path.open("w", encoding="utf-8") as outfile:
        yaml.dump(yaml_dict, outfile)


def update_config(
    template_config: Path,
    config_path: Path,
    ntrip_username: str,
    ntrip_password: str,
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

    :param str ntrip_username: Username of NTRIP account
    :param str ntrip_password: Password of NTRIP account
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
    logging.info(f"Writing config '{config_path}'")

    # Load config
    config = load_yaml(template_config)

    if interval <= 0:
        interval = config["processing_options"]["epoch_control"]["epoch_interval"]
    # if not bcep_mountpoint:
    #     bcep_mountpoint = config["inputs"]["satellite_data"]["rtcm_inputs"]["rtcm_inputs"][0]

    # Inputs
    config["inputs"]["inputs_root"] = str(product_dir)
    config["inputs"]["satellite_data"]["rtcm_inputs"]["rtcm_inputs"] = [bcep_mountpoint, ssr_mountpoint]
    config["inputs"]["satellite_data"]["rtcm_inputs"]["ssr_antenna_offset"] = (
        "APC" if ssr_mountpoint[:4] == "SSRA" else "COM" if ssr_mountpoint[:4] == "SSRC" else "UNSPECIFIED"
    )

    # Outputs
    config["outputs"]["metadata"]["config_description"] = ssr_mountpoint
    config["outputs"]["metadata"]["user"] = ntrip_username
    config["outputs"]["metadata"]["pass"] = ntrip_password
    config["outputs"]["outputs_root"] = str(output_dir)
    config["outputs"]["output_rotation"]["period"] = rotation_period
    config["outputs"]["decoded_rtcm"]["output"] = output_json
    config["outputs"]["rinex_nav"]["filename"] = f"{bcep_mountpoint}_<YYYY><DDD><HH><mm>_NAV.rnx"

    # Processing options
    config["processing_options"]["epoch_control"]["epoch_interval"] = interval
    config["processing_options"]["epoch_control"]["max_epochs"] = max_epochs

    # Write config
    write_yaml(config, config_path)
    logging.info(f"Config written to '{config_path}'")


def record_ssr_stream(
    pea_dir: Path, config_path: Path, output_dir: Path, pid_file_path: Path, ssr_mountpoint: str
) -> None:
    """Record and decode a SSR stream in real-time with the PEA

    :param Path pea_dir: Directory where the PEA executable is placed
    :param Path config_path: Path of the YAML file to write config options to
    :param Path output_dir: Directory to save the PEA outputs
    :param Path pid_file_path: Path of the JSON file to log PIDs to
    :param str ssr_mountpoint: Mountpoint of the SSR stream to record
    :return None
    """
    logging.info(f"Running PEA for '{ssr_mountpoint}' ...")

    now = datetime.now()
    yrdoy = now.strftime("%Y%j")
    timestamp = f"{yrdoy}{now.hour:02d}{now.minute:02d}"

    # Process
    pea_path = pea_dir / "pea"
    proc = subprocess.Popen(
        ["nohup", str(pea_path), "--config", str(config_path)],
        stdout=(output_dir / f"record_ssr_stream_{timestamp}.log").open("w"),
        stderr=subprocess.STDOUT,
    )

    # Save PID of started PEA instance to file to kill later
    pid_list = [{"PID": proc.pid, "command": " ".join(proc.args)}]
    save_pids(pid_file_path, pid_list)

    proc.wait()

    logging.info("PEA completed\n")


@click.command()
@click.option("--pea-dir", required=True, help="Directory where the PEA executable is placed", type=Path)
@click.option("--template-config", required=True, help="Path of template YAML config", type=Path)
@click.option("--job-dir", required=True, help="Directory where data is processed", type=Path)
@click.option("--product-dir", required=True, help="Directory where product files are placed", type=Path)
@click.option("--ntrip-username", help="Username of NTRIP account", type=str)
@click.option("--ntrip-password", help="Password of NTRIP account", type=str)
@click.option(
    "--ntrip-cred-file-path",
    help='Path of NTRIP credential JSON file where username and password of NTRIP account is saved, with the format of \'{"username": "your_ntrip_username", "password": "your_ntrip_password"}\'. Required if --ntrip-username and --ntrip-password options are not specified',
    type=Path,
)
@click.option(
    "--bcep-mountpoint",
    required=True,
    help="Mountpoint of broadcast ephemeris stream. Default: 'BCEP00BKG0'",
    default="BCEP00BKG0",
    type=str,
)
@click.option("--ssr-mountpoint", required=True, help="Mountpoint of the SSR stream to record", type=str)
@click.option("--output-json", help="Log decoded messages into JSON files. Default: True", default=True, is_flag=True)
@click.option(
    "--rotation-period", help="Recording length of each output file in seconds. Default: 86400", default=86400, type=int
)
@click.option("--interval", help="Epoch interval of data processing in seconds. Default: 0", default=0, type=int)
@click.option("--max-epochs", help="Maximum number of epochs to process. Default: 0 (infinite)", default=0, type=int)
@click.option("--verbose", is_flag=True)
def record_ssr_stream_main(
    pea_dir,
    template_config,
    job_dir,
    product_dir,
    ntrip_username,
    ntrip_password,
    ntrip_cred_file_path,
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

    if not (ntrip_username and ntrip_password):
        if ntrip_cred_file_path.is_dir():
            logging.error(
                "NTRIP credentials not provided, please either specify NTRIP username and password"
                "or the JSON file containing NTRIP credentials in following format:\n"
                '{"username": "your_ntrip_username", "password": "your_ntrip_password"}'
            )
            return
        elif not ntrip_cred_file_path.exists():
            logging.error(f"NTRIP credential file '{ntrip_cred_file_path}' not found")
            return
        else:
            with ntrip_cred_file_path.open("r") as ntrip_cred_file:
                for line in ntrip_cred_file:
                    cred = json.loads(line)
                    ntrip_username = cred["username"]
                    ntrip_password = cred["password"]
                    if ntrip_username and ntrip_password:
                        break

    # Prepare config
    config_path = output_dir / f"record_ssr_stream_{ssr_mountpoint}.yaml"
    update_config(
        template_config,
        config_path,
        ntrip_username,
        ntrip_password,
        output_dir,
        product_dir,
        bcep_mountpoint,
        ssr_mountpoint,
        rotation_period,
        output_json,
        interval,
        max_epochs,
    )

    # Check if input files are available before processing
    config = load_yaml(config_path)

    input_files = []
    input_files = input_files + config["inputs"]["atx_files"]
    input_files = input_files + config["inputs"]["snx_files"]

    while True:
        inputs_ready = True

        for file in input_files:
            path = product_dir / file
            if not path.exists():
                inputs_ready = False
                logging.warning(
                    f"Input file '{path}' not found. Waiting 30 seconds for it to be downloaded or manually provided ..."
                )

        if inputs_ready:
            break
        else:
            time.sleep(30)

    # Run the PEA to record SSR stream
    pid_file_path = job_dir / "pid.json"
    record_ssr_stream(pea_dir, config_path, output_dir, pid_file_path, ssr_mountpoint)


if __name__ == "__main__":
    record_ssr_stream_main()
