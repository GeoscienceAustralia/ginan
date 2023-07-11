#!/usr/bin/env python3

"""Downloads/Uploads auxiliary example_input_data files to/from example_input_data dir"""
import logging as _logging
from pathlib import Path as _Path
from shutil import copy as _copy
from shutil import rmtree as _rmtree
from typing import Union as _Union

import click as _click
import gnssanalysis as ga

EX_GLOB_DICT = {
    "ex02": ["*.TRACE"],
    "ex11": ["*.TRACE", "*.snx", "*.*_smoothed"],
    "ex12": ["*.TRACE", "*.snx"],
    "ex13": ["*.TRACE", "*.snx"],
    "ex14": ["*.TRACE", "*.snx"],
    "ex16": ["*Network*.TRACE", "*.*I", "*.stec", "*.snx", "*.BIA", "*.INX*"],
    "ex17": ["*Network*.TRACE", "*.snx", "*.clk*"],
    "ex21": ["pod*.out", "*.sp3"],
    "ex22g": ["pod*.out", "*.sp3"],
    "ex22r": ["pod*.out", "*.sp3"],
    "ex22e": ["pod*.out", "*.sp3"],
    "ex22c": ["pod*.out", "*.sp3"],
    "ex22j": ["pod*.out", "*.sp3"],
    "ex23": ["pod*.out", "*.sp3"],
    "ex24": ["pod*.out", "*.sp3"],
    "ex25": ["pod*.out", "*.sp3"],
    "ex26": ["pod*.out", "*.sp3"],
    "ex31": [
        "pod_fit/pod*.out",
        "pod_fit/*.sp3",
        "pea/*etwork*.TRACE*",  # Network starts with lower case for some reason
        "pea/*.snx",
        "pea/*.erp",
        "pea/*clk*",
        "pod_ic/pod*.out",
        "pod_ic/*.sp3",
    ],
    "ex41": ["*.TRACE", "*.snx", "*.*_smoothed"],
    "ex42": ["*.TRACE", "*.snx"],
    "ex43": ["*.TRACE", "*.snx"],
    "ex43a": ["*.TRACE", "*.snx"],
    "ex44": ["*.TRACE", "*.snx"],
    "ex48": ["*.TRACE", "*.snx"],
    "ex51": [
        "*blq",
    ],
    "ex52": [
        "*blq",
    ],
}


def insert_tag(name: str, tag: str) -> str:
    """inserts tag name right before the filename:
    insert_tag('ex11','some_tag') -> 'some_tag/ex11'
    insert_tag('solutions/ex11','some_tag') -> 'solutions/some_tag/ex11'
    """
    name_split = name.split("/")
    name_split.insert(-1, tag)
    return "/".join(name_split)


def get_example_type(name: str) -> _Union[str, bool]:
    """
    Checks if input string is a type of example dir, e.g. ex22g,
    returns string type of the example test.
    ex13 (name[2]==1) is 'PEA'  and ex21 (name[2]==2) is 'POD'
    """
    ex_type_dict = {"0": "PEA", "1": "PEA", "2": "POD", "3": "PEAPOD", "4": "PEA", "5": "OTHER"}
    if name.startswith("ex"):
        try:
            idx = name[2]
            return ex_type_dict[idx]
        except KeyError:
            raise ValueError(f"Example code '{idx}' could not be matched to a type")
    return False


def update_solutions_dict(example_input_data_dir: _Path, directory: str, ex_glob_dict: dict, tag: str = ""):
    """ """
    if get_example_type(directory):  # room for five-symbol name - ex22g
        example_dir = example_input_data_dir / directory
        ref_sol_dir = example_input_data_dir / "solutions" / tag / directory
        if example_dir.exists() and ref_sol_dir.exists():
            _rmtree(ref_sol_dir)
            _logging.info(
                f"removing {ref_sol_dir} and its content"
            )  # if actual solution exists -> clean respective reference solution before copying
        l = len(example_dir.as_posix())

        if directory not in ex_glob_dict.keys():
            raise ValueError(f"{directory} not in EX_GLOB_DICT dictionary")

        paths_list = []
        for expr in ex_glob_dict[directory]:
            paths = list(example_dir.glob(expr))
            if paths == []:
                _logging.warning(msg=f"no files were found using the {expr} rule")
            paths_list.append(paths)
            for path in paths:
                dst = ref_sol_dir / (path.as_posix()[l + 1 :])
                dst.parent.mkdir(parents=True, exist_ok=True)
                _logging.info(f"Copying {path} -> {_copy(src=path,dst=dst)}")
            if not any(paths_list):
                raise ValueError(
                    f"No files found in '{example_dir}' according to {directory} directory rules from EX_GLOB_DICT: {ex_glob_dict[directory]}"
                )


def upload_example_input_data_tar(
    example_output_data_path: _Union[_Path, str],
    bucket: str,
    target: str,
    dirs: _Union[list, tuple] = ("products", "data", "loading", "solutions"),
    compression: str = "bz2",
    tag: str = "",
    show_progress: bool = False,
    push_no_tar: bool = False,
):
    __doc__ = (
        "tars selected aux dirs from ginan/inputData and compares their checksums with"
        " the ones from s3 bucket. If checksums different - upload, else log info"
        " message and do nothing. Default paths are [bucket] s3://peanpod/aux/ ->"
        " [html] https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/"
    )
    base_url = f"https://{bucket}.s3.ap-southeast-2.amazonaws.com/{target}"
    for directory in dirs:
        # update tarname with tag
        example_type = get_example_type(directory)
        if example_type:
            directory = "solutions/" + (f"{tag}/" if tag != "" else "") + directory
        tarname = directory + ".tar." + compression
        destpath_targz = example_output_data_path / tarname
        dest_url = base_url + "/" + tarname
        if not push_no_tar:
            ga.gn_io.common.tar_compress(
                srcpath=example_output_data_path / directory,
                destpath=destpath_targz,
                reset_info=example_type,  # reset timestamp etc for examples only
                compression=compression,
            )  # overwrite only if push_no_tar is False (default)
        md5_checksum_aws = ga.gn_download.request_metadata(dest_url)
        md5_checksum = ga.gn_io.common.compute_checksum(destpath_targz)
        if md5_checksum_aws != md5_checksum:
            _logging.info(f'checksums different -> uploading "{tarname}"')
            ga.gn_download.upload_with_chunksize_and_meta(
                local_file_path=destpath_targz,
                metadata={"md5checksum": md5_checksum},
                public_read=True,
                bucket_name=bucket,
                object_key=target + "/" + tarname,
                verbose=show_progress,
            )
        else:
            _logging.info(f"checksums the same -> skipping upload")
        _logging.info(f"------------------------------")


def download_example_input_data_tar(
    example_input_data_path: _Union[_Path, str],
    bucket: str,
    target: str,
    dirs: _Union[list, tuple] = ("products", "data", "solutions", "loading"),
    compression: str = "bz2",
    tag: str = "",
    skip_extract: bool = False,
    tags_file_path: _Union[str, None] = None,  # fall back on tags file if no tag was provided
):
    __doc__ = (
        "Downloads compressed selected tarballs from ap-southeast-2 s3 bucket, untars"
        " them to ginan/inputData dir. If tarball is available locally then checksums"
        " are compared at first. If the same - nothing is downloaded, local tarball"
        " gets uncompressed"
    )

    base_url = f"https://{bucket}.s3.ap-southeast-2.amazonaws.com/{target}"
    if tag == "":
        if tags_file_path is None:
            raise ValueError("tag not provided and tags_file_path not provided")
        _logging.info(f"reading tags from {tags_file_path}")
        tag = ga.gn_download.get_vars_from_file(tags_file_path)
    else:
        _logging.info(f"using the provided {tag} tag")

    for directory in dirs:
        example_type = get_example_type(directory)

        if example_type:
            dir_url = f"solutions/{(f'{tag[example_type]}/{directory}' if isinstance(tag,dict) else f'{tag}/{directory}')}.tar.{compression}"
            directory = f"solutions/{directory}.tar.{compression}"
        else:
            directory = dir_url = f"{directory}.tar.{compression}"

        destpath_targz = example_input_data_path / directory
        dest_url = base_url + "/" + dir_url
        md5_checksum_aws = ga.gn_download.request_metadata(dest_url)
        destpath = example_input_data_path / directory
        if not destpath_targz.exists():
            _logging.info(msg=f"{directory} not found on disk ['{md5_checksum_aws}'].")
            destpath_targz.parent.mkdir(parents=True, exist_ok=True)
            ga.gn_download.download_url(url=dest_url, destfile=destpath_targz)
        else:
            _logging.info(msg=f"{directory} found on disk. Validating...")
            md5_checksum = ga.gn_io.common.compute_checksum(destpath_targz)
            if md5_checksum_aws != md5_checksum:
                _logging.info(f'checksums different -> downloading "{dir_url}"')
                ga.gn_download.download_url(url=dest_url, destfile=destpath_targz)
            else:
                _logging.info(f"checksums the same -> skipping download")

        if skip_extract and destpath.exists():
            _logging.info(
                "skipping extraction step as '--skip_extract' provided, checksums the same and destination directory exists"
            )
        else:
            try:
                ga.gn_io.common.tar_extract(srcpath=destpath_targz, destpath=destpath)
            except Exception as e:
                _logging.error(f"could not extract {destpath_targz} to {destpath} due to {e}")


@_click.command()
@_click.argument("dirs", nargs=-1, type=str)
@_click.option("--bucket", default="peanpod", help="s3 bucket name to push and pull from")
@_click.option("--target", default="aux", help="s3 target name (dir) within the selected bucket")
@_click.option(
    "--path",
    default=None,
    help="custom path to inputData dir, a dir that stores products/data etc, default is ginan/inputData",
)
@_click.option("--tag", type=str, default="")
@_click.option(
    "--skip_extract",
    is_flag=True,
    show_default=True,
    default=False,
    help=(
        """Skips extraction of the on-disk tar file if checksum test is OK and the
            destination dir exists. This is done to save time in the pipeline as
            there is no need to overwrite the files."""
    ),
)
@_click.option("-s", "--solutions", is_flag=True, help="download/upload solutions")
@_click.option("-d", "--data", is_flag=True, help="download/upload data")
@_click.option("-p", "--products", is_flag=True, help="download/upload products")
@_click.option("-l", "--loading", is_flag=True, help="download/upload loadings")
@_click.option("--push", is_flag=True, help="tar dirs and push them to aws with checksum metadata and public read")
@_click.option("--push_no_tar", is_flag=True, help="push tar archive which is present on disk")
def download_example_input_data(dirs, bucket, target, path, tag, skip_extract, solutions, data, products, loading, push, push_no_tar):
    """Downloads 'products', 'data', and 'solutions' tarballs from s3 bucket and
     extracts the content into inputData dir. The list of tarballs can be
     changed with the combination of [-p/-d/-s] options. Similar tarballs
     upload functionality is available - can be activated with '--push' key
    To configure the utility for --push functionality it is enough to create
     ~/.aws/credentials file containing
    [default] / aws_access_key_id=ACCESS_KEY /
    aws_secret_access_key=SECRET_KEY"""
    _logging.getLogger().setLevel(_logging.INFO)
    script_path = _Path(__file__).resolve().parent
    if path is None:
        example_input_data_path = (script_path.parent / "inputData").resolve()
        _logging.info(f"default input path relative to script location selected: {example_input_data_path}")
    else:
        example_input_data_path = _Path(path)
        _logging.info(f"custom input path selected: {example_input_data_path}")

    if path is None:
        example_output_data_path = _Path("./")
        _logging.info(f"default output path relative to script location selected: {example_output_data_path}")
    else:
        example_output_data_path = _Path(path)
        _logging.info(f"custom output path selected: {example_output_data_path}")

    if not dirs:
        if products:
            dirs += ("products",)
        if loading:
            dirs += ("loading",)
        if data:
            dirs += ("data",)
        if solutions:
            dirs += tuple(EX_GLOB_DICT.keys())
        if not dirs:  # if nothing has been selected
            dirs = ("products", "data") + tuple(EX_GLOB_DICT.keys())

    _logging.info(f"{dirs} selected")

    if push or push_no_tar:
        # copy over the required files if exist - if solutions/blah -> rm blah, copy from ../blah to solutions/blah
        _logging.info(msg="updating solutions")
        [
            update_solutions_dict(example_input_data_dir=example_output_data_path, directory=directory, ex_glob_dict=EX_GLOB_DICT, tag=tag)
            for directory in dirs
        ]
        upload_example_input_data_tar(
            dirs=dirs,
            compression="bz2",
            tag=tag,
            example_output_data_path=example_output_data_path,
            bucket=bucket,
            target=target,
            show_progress=False,
            push_no_tar=push_no_tar,
        )
    else:
        download_example_input_data_tar(
            dirs=dirs,
            compression="bz2",
            tag=tag,
            example_input_data_path=example_input_data_path,
            skip_extract=skip_extract,
            bucket=bucket,
            target=target,
            tags_file_path=(script_path.parent / "docker" / "tags").as_posix(),
        )


if __name__ == "__main__":
    download_example_input_data()
