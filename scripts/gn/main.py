""" Given a single rinex v3 file, does everything required to prepare
    Ginan for a run.

    - Downloads the dependencies required to post-process a Static session RINEX v3 file with Ginan.
    - Generates the yaml config using a template file in scripts/templates

    Precondition: It is assumed that the ginan/inputData/products directory
    has been downloaded from S3 using python3 s3_filehandler.py -p
"""
import logging
import shutil
from pathlib import Path
from collections.abc import Iterable

from .download_rinex_deps import download
from .parse_rinex_header import parse_v3_header, RinexHeader

import click
import ruamel.yaml


def create_overrides(header: RinexHeader, station_alias: str, workspace_name: str) -> [tuple]:
    """Determines a set of changes to be made to the template yaml.

    - Adds station specific information
    - Adds config name
    - Adds code priorities that are specific to the rinex file

    :param header: RinexHeader object that has been parsed from the input file
    :param station_alias: Four character alias for a station
    :param workspace_name: Override for the config_description

    :returns list of tuples where key is list of str to key into yaml, and value is what to replace the val with
    """
    station_overrides = create_station_overrides(header, station_alias)
    outputs_overrides = [(["outputs", "metadata", "config_description"], workspace_name)]
    code_priorities_overrides = create_code_priorities_overrides(header)
    overrides = station_overrides + outputs_overrides + code_priorities_overrides
    return overrides


def create_station_overrides(rinex_header: RinexHeader, station_alias: str) -> [tuple]:
    """Determines the yaml overrides that are required for the specific station in the rinex.

    :param rinex_header: RinexHeader object that has been parsed from the input file
    :param station_alias: Four character alias for a station

    :returns list of tuples where key is list of str to key into yaml, and value is what to replace the val with
    """
    eccentricity = {"enable": True, "offset": rinex_header.antenna.get_eccentricity()}

    overrides = [
        (["receiver_options", station_alias, "antenna_type"], rinex_header.antenna.type),
        (["receiver_options", station_alias, "apriori_position"], rinex_header.approx_position.get_apriori_position()),
        (["receiver_options", station_alias, "models", "eccentricity"], eccentricity),
    ]
    return overrides


def create_code_priorities_overrides(rinex_header: RinexHeader) -> [tuple]:
    """Determines the yaml overrides that are required for the specific codes in the file.

    :param rinex_header: RinexHeader object that has been parsed from the input file

    :returns list of tuples where key is list of str to key into yaml, and value is what to replace the val with
    """
    phase_signals = get_phase_signals_per_system(rinex_header.sys_signals)
    overrides = [
        (["processing_options", "gnss_general", "sys_options", sys, "code_priorities"], list(code_priorities))
        for sys, code_priorities in phase_signals.items()
    ]
    return overrides


def write_yaml(target_dir: Path, workspace_name: str = "auto", overrides: [tuple] = []):
    """Overrides the yaml template with overrides and writes the file
    to the target directory.

    :param target_dir: Directory to write the file to
    :param workspace_name: Name to include in the yaml file name
    :param overrides: A list of tuples which overwrite parts of the template file

    :returns Path to the output yaml file
    """
    scripts = Path(__file__).resolve().parent
    template_path = scripts / "templates" / "auto_template.yaml"

    target = target_dir / f"{workspace_name}_{template_path.stem}.yaml"

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

    return target


def write_nested_dict_value(nested_dict, nested_keys: Iterable, value):
    """
    Safely write a value into a series of nested dictionaries, making intermediate layers where required

    :param nested_dict: The nested dictionaries (configuration map) to write to
    :param nested_keys: An Iterable containing the sequence of keys to follow to store the value
    :param value: The value to store in the nested dictionary
    :returns: value
    """
    head_key, *tail_keys = nested_keys
    if not tail_keys:
        # Base case, no more layers to go down, store our value here
        nested_dict[head_key] = value
        return value
    # Need to recurse down a level, but if the next level doesn't already exist we need to create it first
    if head_key not in nested_dict:
        nested_dict[head_key] = {}
    return write_nested_dict_value(nested_dict[head_key], tail_keys, value)


def get_phase_signals_per_system(sys_signals: dict) -> dict:
    """Get all of the carrier phase signals (start with L), grouped by system (GPS, GLONASS etc)"""
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


@click.group()
@click.option(
    "--log-level",
    type=click.Choice(["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]),
    default="INFO",
    help="Set the logging level.",
)
def cli(log_level):
    """A collection of tools to prepare Ginan for processing"""
    # Set up logging based on the provided log level
    root_logger = logging.getLogger()
    root_logger.setLevel(getattr(logging, log_level))


@cli.command()
@click.option("--ppp", is_flag=True, help="Use Precise Point Positioning (PPP).")
@click.option("--static", is_flag=True, help="Use static mode.")
@click.option("--rinex-path", type=click.Path(exists=True), help="Path to RINEX file.")
@click.option(
    "--workspace-root", type=click.Path(), help="Path to store workspaces", default="workspace", required=False
)
@click.option("--workspace-name", type=str, help="Name for the workspace", default="network", required=False)
def prep(ppp: bool, static: bool, rinex_path: Path, workspace_root: Path, workspace_name: str):
    """
    Downloads required IGS products and generates template YAML
    """
    if ppp and static:
        rinex_path = Path(rinex_path)
        target_dir = Path(workspace_root) / workspace_name
        download_dir = target_dir / "downloads"
        data_dir = target_dir / "data"
        ensure_folders([target_dir, download_dir, data_dir])

        # Make a copy of the rinex file in the target directory
        new_rinex_path = data_dir / f"{rinex_path.stem}.rnx"
        shutil.copy(rinex_path, new_rinex_path)

        logging.info("Parsing rinex file")
        header = parse_v3_header(rinex_path)

        # TODO: The pea does not allow station names of 4 digits eg. 7369 - but it would be good if it did.
        # For now, create an alias and rename the copy
        station_alias = header.get_station_alias()
        shutil.move(new_rinex_path, data_dir / f"{station_alias}.rnx")

        logging.info("Downloading required files from IGS CDDIS...")
        download(header, download_dir)
        logging.info("Downloads complete")

        logging.info("Generating yaml config file")
        overrides = create_overrides(header, station_alias, workspace_name)
        yaml_path = write_yaml(target_dir, workspace_name=workspace_name, overrides=overrides)

        logging.info("Ready to run the pea...")
        return yaml_path
    else:
        raise NotImplementedError(
            "Only static ppp is supported through this interface - include --static --ppp "
            "You could modify an example template to run other configurations with pea"
        )
