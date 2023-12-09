""" Given a single rinex v3 file, does everything required to prepare
    Ginan for a run.

    - Downloads the dependencies required to post-process a Static session RINEX v3 file with Ginan.
    - Generates the yaml config using a template file in scripts/templates

    Precondition: It is assumed that the ginan/inputData/products directory
    has been downloaded from S3 using python3 s3_filehandler.py -p
"""
import os
from pathlib import Path

import download_rinex_deps

from auto_generate_yaml import write_nested_dict_value
from parse_rinex_header import parse_v3_header

import ruamel


def main(config_name: str, rinex_path: Path, target_dir: Path):
    download_dir = target_dir / "downloads"
    ensure_folders([target_dir, download_dir])

    header = parse_v3_header(rinex_path)

    download_rinex_deps.download(header, download_dir)

    station_overrides = create_station_overrides(header)
    outputs_overrides = [(["outputs", "metadata", "config_description"], config_name)]
    overrides = station_overrides + outputs_overrides
    write_yaml(target_dir, config_name=config_name, overrides=overrides)


def create_station_overrides(rinex_header: dict) -> [tuple]:
    four_char_id = rinex_header["marker_name"][:4]

    apriori_position = list(rinex_header["approx_position"].values())
    eccentricity = list(rinex_header["antenna"]["deltas"].values())
    overrides = [
        (["station_options", four_char_id, "antenna_type"], rinex_header["antenna"]["type"]),
        (["station_options", four_char_id, "apriori_position"], apriori_position),
        (["station_options", four_char_id, "eccentricity"], eccentricity),
        (["station_options", four_char_id, "receiver_type"], rinex_header["receiver"]["type"]),
    ]
    return overrides


def write_yaml(target_dir, config_name="auto", overrides=[]):
    scripts = Path(__file__).resolve().parent
    template_path = scripts / "templates/auto_template.yaml"

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


def ensure_folders(paths: [Path]) -> None:
    """
    Ensures the list of directories exist in the file system.

    :param Directories to create if they do not already exist
    """
    for path in paths:
        if not path.is_dir():
            path.mkdir(parents=True)


# TODO: two arguments - config name and path to rinex file
# - config folder will be created in ginan/workspace/<config>
# - rinex will be copied to config folder
if __name__ == "__main__":
    filename = "PSMA.rnx"
    config_name = "PSMA"
    config_dir = Path("./workspace") / config_name

    rinex_path = Path(config_dir) / "data" / filename
    main(config_name, rinex_path, config_dir)
