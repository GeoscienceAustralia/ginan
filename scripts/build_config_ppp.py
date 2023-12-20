""" Given a single rinex v3 file, does everything required to prepare
    Ginan for a run.

    - Downloads the dependencies required to post-process a Static session RINEX v3 file with Ginan.
    - Generates the yaml config using a template file in scripts/templates

    Precondition: It is assumed that the ginan/inputData/products directory
    has been downloaded from S3 using python3 s3_filehandler.py -p
"""
import shutil
from pathlib import Path

import download_rinex_deps

from auto_generate_yaml import write_nested_dict_value
from parse_rinex_header import parse_v3_header

import click
import ruamel


def main(config_name: str, rinex_path: Path, target_dir: Path):
    download_dir = target_dir / "downloads"
    data_dir = target_dir / "data"
    ensure_folders([target_dir, download_dir, data_dir])

    new_rinex_path = data_dir / f"{rinex_path.stem}.rnx"
    shutil.copy(rinex_path, new_rinex_path)

    header = parse_v3_header(rinex_path)

    # Rename the rinex file to RXXX.rnx, where XXX are the last 3 characters of the marker name.
    # TODO: The pea does not allow station names of 4 digits eg. 7369 - but it would be good if it did.
    four_char_id = f"R{header['marker_name'][-3:]}"
    shutil.move(new_rinex_path, data_dir / f"{four_char_id}.rnx")

    download_rinex_deps.download(header, download_dir)

    station_overrides = create_station_overrides(header, four_char_id)
    outputs_overrides = [(["outputs", "metadata", "config_description"], config_name)]
    code_priorities_overrides = create_code_priorities_overrides(header)
    overrides = station_overrides + outputs_overrides + code_priorities_overrides
    write_yaml(target_dir, config_name=config_name, overrides=overrides)


def create_station_overrides(rinex_header: dict, four_char_id) -> [tuple]:
    apriori_position = list(rinex_header["approx_position"].values())

    # ENU instead of UNE in SINEX
    antenna_deltas = rinex_header["antenna"]["deltas"]
    eccentricity_offset = [antenna_deltas["east"], antenna_deltas["north"], antenna_deltas["height"]]
    eccentricity = {"enable": True, "offset": eccentricity_offset}

    overrides = [
        (["receiver_options", four_char_id, "antenna_type"], rinex_header["antenna"]["type"]),
        (["receiver_options", four_char_id, "apriori_position"], apriori_position),
        (["receiver_options", four_char_id, "models", "eccentricity"], eccentricity),
        (["receiver_options", four_char_id, "receiver_type"], rinex_header["receiver"]["type"]),
    ]
    return overrides


def create_code_priorities_overrides(rinex_header: dict) -> [tuple]:
    phase_signals = get_phase_signals_per_system(rinex_header["sys_signals"])
    overrides = [
        (["processing_options", "gnss_general", "sys_options", sys, "code_priorities"], list(code_priorities))
        for sys, code_priorities in phase_signals.items()
    ]
    return overrides


def write_yaml(target_dir, config_name="auto", overrides=[]):
    scripts = Path(__file__).resolve().parent
    template_path = scripts / "templates" / "auto_template.yaml"

    target = target_dir / f"{config_name}_{template_path.stem}.yaml"

    # Read in the template
    with open(template_path, "r", encoding="utf-8") as f:
        yaml = ruamel.yaml.YAML(typ="safe", pure=True)
        template = yaml.load(f)

    for override in overrides:
        write_nested_dict_value(template, override[0], override[1])

    # Write this configuration out
    target.parent.mkdir(parents=True, exist_ok=True)
    with open(target, "w", encoding="utf-8") as out_f:
        yaml = ruamel.yaml.YAML(typ="safe", pure=True)
        yaml.indent(mapping=2)
        yaml.default_flow_style = False
        yaml.dump(data=template, stream=out_f)


def get_phase_signals_per_system(sys_signals: dict) -> dict:
    PHASE_PREFIX = "L"
    filtered_sys_signals = {
        sys: {str(s) for s in signals if s.startswith(PHASE_PREFIX)} for sys, signals in sys_signals.items() if signals
    }
    return filtered_sys_signals


def ensure_folders(paths: [Path]) -> None:
    """
    Ensures the list of directories exist in the file system.

    :param Directories to create if they do not already exist
    """
    for path in paths:
        if not path.is_dir():
            path.mkdir(parents=True)


@click.command()
@click.option(
    "--rinex-path",
    required=True,
    type=click.Path(exists=True, path_type=Path),
    help="Path to RINEX v3 file for a static session",
)
@click.option(
    "--target-dir",
    required=False,
    type=click.Path(path_type=Path),
    help="Directory to store the config directory",
    default="workspace",
)
@click.option(
    "--config-name",
    required=False,
    help="Name of the directory which will store the config and files",
    default="network",
)
def cli(rinex_path: Path, target_dir: Path, config_name: str):
    config_dir = target_dir / config_name
    main(config_name, rinex_path, config_dir)


if __name__ == "__main__":
    cli()
