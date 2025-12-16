"""
Script for creating yaml file based on products / data available
"""
import os
import copy
import click
import logging
import itertools
from typing import List
from pathlib import Path
from ast import literal_eval
from datetime import datetime
from collections.abc import Iterable

import ruamel.yaml


def configure_logging(verbose):
    if verbose:
        logging_level = logging.DEBUG
    else:
        logging_level = logging.INFO
    logging.basicConfig(format="%(asctime)s [%(funcName)s] %(levelname)s: %(message)s")
    logging.getLogger().setLevel(logging_level)


def read_nested_dict_value(nested_dict, nested_keys: Iterable, default_value=None, error_on_miss=False):
    """
    Read a value from a series of nested dictionaries, such as returned by ruamel.yaml

    :param nested_dict: The nested dictionaries (configuration map) to read from
    :param nested_keys: An Iterable containing the sequence of keys to follow to get the value
    :param default_value: The value to return if the sequence of keys does not specify a valid
                          path in the nested dictionary (and error_on_miss is False), defaults to None
    :param error_on_miss: A boolean flag (default false) that if set causes this function to raise
                          a KeyError if the sequence of keys does not specify a valid path in the
                          nested dictionary
    :returns: The value located at the end of the path of keys,
              loosely nested_dict[nested_keys[1]][nested_keys[2]]...
    :raises KeyError: If error_on_miss is True and the sequence of keys does note specify a valid
                      path in the nested dictionary
    """
    head_key, *tail_keys = nested_keys
    if head_key not in nested_dict:
        # Can't find the next key, error or return the default value depending on our error handling
        if error_on_miss:
            # Should I actually just try and read from the dictionary and let that raise the exception?
            raise KeyError
        return default_value
    if not tail_keys:
        # Base case, no more layers to go down, return the value we have here
        return nested_dict[head_key]
    # Time to recurse down an extra layer
    return read_nested_dict_value(nested_dict[head_key], tail_keys, default_value, error_on_miss)


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


# This function was a possible option to map strings to integers and floats
# Currently we're using ast.literal_eval instead as there are times where we also need arrays
def to_numeric_if_possible(num_str):
    try:
        return int(num_str)
    except ValueError:
        pass
    try:
        return float(num_str)
    except ValueError:
        return num_str


def parse_yaml_overrides(commandline_override_tuple, commandline_override_string):
    """
    Ensure that override format received from commandline is converted to a common internal representation

    The rest of the code assumes that overrides are provided as an iterable of two-element tuples containing
    the (iterable) sequence of nested keys as the first element and the value as the second.
    This function is responsible for the commandline<->rest of code interface and so is what we need to
    change if we change how we represent these on the command line.
    The current command-line implementation uses click's multiple and nargs options to provide multiple
    overrides each as a pair of period-delimited sequence of keys and the value. This comes into the code
    as a variable-length tuple of two-element tuples, so we split the first element on periods to get the
    sequence of keys.
    We also need to convert string versions of literals to actual python literals to avoid quotes in the yaml.
    """
    overrides_from_string = canonicalize_overrides_string(commandline_override_string)
    # Evaluating arbitrary input is always dangerous from a security perspective.
    # According to the python docs, literal_eval won't execute arbitrary code (only parsing literals)
    # but you can crash the interpreter with a well-crafted input.
    # We need to keep this in mind if this function could ever be exposed to un-controlled input
    return (
        (x[0].split("."), literal_eval(x[1]))
        for x in itertools.chain(commandline_override_tuple, overrides_from_string)
    )


def canonicalize_overrides_string(commandline_override_string: str):
    if not commandline_override_string:
        return ()
    override_list = commandline_override_string.split(":")
    key_value_partitions = [keyval_string.rpartition("=") for keyval_string in override_list if keyval_string]
    try:
        first_invalid_override = next(x for x in key_value_partitions if not x[2])
        raise RuntimeError(
            f"Provided override string failed to parse.\nNo value found in {first_invalid_override[1]}\n"
            "Override string provided was: {commandline_override_string}"
        )
    except StopIteration:
        # All fine, there were no invalid key-value pairs that we found
        pass
    return ((x[0], x[2]) for x in key_value_partitions)


def out_pea_yaml(
    start_epoch,
    end_epoch,
    config_out_dir: Path,
    template_path: Path,
    product_dir: Path,
    data_dir: Path,
    pea_out_dir: Path,
    config_name: str = "auto",
    relative_to_dir: Path = None,
    trop_model="gpt2",
    trop_dir: Path = None,
    enable_mongo: bool = True,
    overrides=[],
):
    """
    Output the YAML file used for rapid run
    """

    # ATX file
    atxfiles = [x.name for x in product_dir.glob("*.atx")]
    # BLQ file
    blqfiles = [x.name for x in product_dir.glob("*.BLQ")]
    # SNX files
    snx_files_present = product_dir.glob("*.snx")
    ssc_files_present = product_dir.glob("*.ssc")
    snx_list = [
        str(x.name)
        for x in snx_files_present
        if str(x.name).startswith(("igs", "jpl", "mit", "cod", "gfz", "sio", "rapid_run"))
    ] + [
        str(x.name)
        for x in ssc_files_present
        if str(x.name).startswith(("igs", "jpl", "mit", "cod", "gfz", "sio", "rapid_run"))
    ]

    # navfile - First line below picks up RINEX3 Broadcast files, second line picks up RINEX2 broadcast files
    navfiles = [x.name for x in product_dir.glob("BRD*.rnx")]
    navfiles += [x.name for x in product_dir.glob("brdc*")]
    # sp3 files
    sp3files = [x.name for x in product_dir.glob("*.sp3")] + [x.name for x in product_dir.glob("*.SP3")]
    # erpfile
    erpfiles = [x.name for x in product_dir.glob("*.erp")] + [x.name for x in product_dir.glob("*.ERP")]
    # clk files
    clkfiles = [x.name for x in product_dir.glob("*.clk")] + [x.name for x in product_dir.glob("*.CLK")]
    # bia files
    bsxfiles = [x.name for x in product_dir.glob("*.bia")] + [x.name for x in product_dir.glob("*.BIA")]
    # Station rnxfiles
    station_rnxfiles = [x.name for x in data_dir.glob("*.rnx")]

    # Generate names for the .clk and .snx files that will be output
    clk_out_name = "auto-<CONFIG>-<YYYY><DDD><HH>.clk"
    snx_out_name = "auto-<CONFIG>-<YYYY><DDD><HH>.snx"

    # Write template to file:
    target = config_out_dir / f"{config_name}_{template_path.stem}.yaml"

    # If we've been given a directory to make things relative to we do that now
    # Unfortunately pathlib.Path.relative_to doesn't do what we want so we have to use os.path.relpath
    if relative_to_dir is not None:
        product_dir_str = os.path.relpath(product_dir, start=relative_to_dir)
        data_dir_str = os.path.relpath(data_dir, start=relative_to_dir)
        pea_out_dir_str = os.path.relpath(pea_out_dir, start=relative_to_dir)
    else:
        product_dir_str = str(product_dir)
        data_dir_str = str(data_dir)
        pea_out_dir_str = str(pea_out_dir)
    write_config_pea(
        target,
        template_path,
        product_dir_str,
        data_dir_str,
        pea_out_dir_str,
        atxfiles,
        blqfiles,
        sp3files,
        erpfiles,
        clkfiles,
        bsxfiles,
        snx_list,
        navfiles,
        station_rnxfiles,
        clk_out_name,
        snx_out_name,
        start_epoch,
        end_epoch,
        trop_model,
        trop_dir,
        enable_mongo,
        overrides,
    )
    return target


def write_config_pea(
    output_path: Path,
    template_path: Path,
    product_dir: str,
    data_dir: str,
    output_dir: str,
    atxfiles: List[str],
    blqfiles: List[str],
    sp3files: List[str],
    erpfiles: List[str],
    clkfiles: List[str],
    bsxfiles: List[str],
    snxfiles: List[str],
    navfiles: List[str],
    station_rnxfiles: List[str],
    clk_out_name: str,
    snx_out_name: str,
    start_epoch: str,
    end_epoch: str,
    trop_model: str,
    trop_dir: str,
    enable_mongo: bool,
    overrides=[],
):
    """
    Read a pea configuration yaml template from file, set it up for a run, and write it out to a file
    """
    # Read in the template
    with open(template_path, "r", encoding="utf-8") as f:
        yaml = ruamel.yaml.YAML(typ="safe", pure=True)
        template = yaml.load(f)

    # Set up the template (inplace) with the appropriate data
    edit_config_template_pea(
        template,
        product_dir,
        data_dir,
        output_dir,
        atxfiles,
        blqfiles,
        sp3files,
        erpfiles,
        clkfiles,
        bsxfiles,
        snxfiles,
        navfiles,
        station_rnxfiles,
        clk_out_name,
        snx_out_name,
        start_epoch,
        end_epoch,
        trop_model,
        trop_dir,
        enable_mongo,
        overrides,
        inplace=True,
    )
    # Write this configuration out
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as out_f:
        yaml = ruamel.yaml.YAML(typ="safe", pure=True)
        yaml.dump(data=template, stream=out_f)


def edit_config_template_pea(
    template,
    product_dir: str,
    data_dir: str,
    output_dir: str,
    atxfiles: List[str],
    blqfiles: List[str],
    sp3files: List[str],
    erpfiles: List[str],
    clkfiles: List[str],
    bsxfiles: List[str],
    snxfiles: List[str],
    navfiles: List[str],
    station_rnxfiles: List[str],
    clk_out_name: str,
    snx_out_name: str,
    start_epoch: str,
    end_epoch: str,
    trop_model: str,
    trop_dir: str,
    enable_mongo: bool,
    overrides=[],
    inplace=False,
):
    """
    Set up the pea configuration template for a specific run
    """
    # Dictionaries are mutable so if we don't want to make changes to the provided template we need to make a deep copy
    if not inplace:
        template = copy.deepcopy(template)

    # TODO: Can we just use globs instead of specific filenames? Then we wouldn't need to modify these
    # part of the template

    # Directories
    write_nested_dict_value(template, ["input_files", "root_input_directory"], product_dir)
    write_nested_dict_value(template, ["station_data", "root_stations_directory"], data_dir)
    write_nested_dict_value(template, ["output_files", "root_output_directory"], output_dir)
    # Input files
    write_nested_dict_value(template, ["input_files", "atxfiles"], atxfiles)
    write_nested_dict_value(template, ["input_files", "blqfiles"], blqfiles)
    write_nested_dict_value(template, ["input_files", "sp3files"], sp3files)
    write_nested_dict_value(template, ["input_files", "erpfiles"], erpfiles)
    write_nested_dict_value(template, ["input_files", "clkfiles"], clkfiles)
    write_nested_dict_value(template, ["input_files", "bsxfiles"], bsxfiles)
    write_nested_dict_value(template, ["input_files", "snxfiles"], snxfiles)
    write_nested_dict_value(template, ["input_files", "navfiles"], navfiles)
    # Station data
    write_nested_dict_value(template, ["station_data", "rnxfiles"], station_rnxfiles)
    # Output files
    write_nested_dict_value(template, ["output_files", "clocks_filename"], clk_out_name)
    write_nested_dict_value(template, ["output_files", "sinex_filename"], snx_out_name)
    # Mongo
    write_nested_dict_value(template, ["output_files", "enable_mongo"], enable_mongo)
    if not enable_mongo:
        write_nested_dict_value(template, ["output_files", "output_mongo_measurements"], False)
        write_nested_dict_value(template, ["output_files", "output_mongo_states"], False)
        write_nested_dict_value(template, ["output_files", "output_mongo_test_stats"], False)
    # Start and end points
    write_nested_dict_value(template, ["processing_options", "start_epoch"], start_epoch)
    write_nested_dict_value(template, ["processing_options", "end_epoch"], end_epoch)
    # Troposphere model
    write_nested_dict_value(template, ["processing_options", "troposphere", "model"], trop_model)
    if trop_model == "gpt2":
        vmf3dir = ""
        orography = ""
        gpt2grid = str(trop_dir)
    elif trop_model == "vmf3":
        vmf3dir = str(trop_dir)
        orography = "orography_ell_5x5"
        gpt2grid = ""
    write_nested_dict_value(template, ["processing_options", "troposphere", "vmf3dir"], vmf3dir)
    write_nested_dict_value(template, ["processing_options", "troposphere", "orography"], orography)
    write_nested_dict_value(template, ["processing_options", "troposphere", "gpt2grid"], gpt2grid)

    # Apply any overrides that we have been given
    for override in overrides:
        write_nested_dict_value(template, override[0], override[1])

    return template


@click.command()
@click.option("--target-dir", required=True, help="Directory to place the final config / YAML file, e.g. /var/config")
@click.option(
    "--template-path", required=True, help="Path to the YAML template to use, e.g. /var/ginan/exampleConfigs/ex12"
)
@click.option("--product-dir", required=True, help="Directory to input products, e.g. /var/product")
@click.option("--data-dir", help="Directory to input data, e.g. /var/data. If missing same as product-dir")
@click.option("--output-dir", help="Directory to output data, e.g. /var/output. If missing same as product-dir")
@click.option("--start-datetime", help="Start of date-time period to download files for")
@click.option("--end-datetime", help="End of date-time period to download files for")
@click.option(
    "--datetime-format", help="Format of input datetime string. Default: %Y-%m-%d_%H:%M:%S", default="%Y-%m-%d_%H:%M:%S"
)
@click.option("--trop-model", help="Model used for the troposphere [gpt2, vmf3]. Default: gpt2", default="gpt2")
@click.option("--trop-dir", help="Directory to find tropospheric model files")
@click.option("--enable-mongo", help="Flag to write to mongoDB. Default: False", default=False, is_flag=True)
@click.option("--verbose", is_flag=True)
def auto_yaml_main(
    target_dir,
    template_path,
    product_dir,
    data_dir,
    output_dir,
    start_datetime,
    end_datetime,
    datetime_format,
    trop_model,
    trop_dir,
    enable_mongo,
    verbose,
):
    configure_logging(verbose)
    target_dir = Path(target_dir)
    template_path = Path(template_path)
    product_dir = Path(product_dir)

    if data_dir:
        data_dir = Path(data_dir)
    else:
        data_dir = product_dir

    if output_dir:
        output_dir = Path(output_dir)
    else:
        output_dir = product_dir

    if trop_dir:
        trop_dir = Path(trop_dir)
    else:
        trop_dir = product_dir

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
        config_out_dir=target_dir,
        template_path=template_path,
        product_dir=product_dir,
        data_dir=data_dir,
        pea_out_dir=output_dir,
        relative_to_dir=None,
        trop_model=trop_model,
        trop_dir=trop_dir,
        enable_mongo=enable_mongo,
        overrides=(),
    )


if __name__ == "__main__":
    auto_yaml_main()
