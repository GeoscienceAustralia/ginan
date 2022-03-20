#!/usr/bin/env python3

"""Downloads/Uploads auxiliary examples files to/from examples dir"""

import argparse
import logging as _logging
from pathlib import Path as _Path
from shutil import copy as _copy, rmtree as _rmtree

from gn_lib.gn_download import (
    download_url,
    request_checksum,
    upload_with_chunksize_and_meta,
)
from gn_lib.gn_io.common import compute_checksum, tar_comp, tar_extr

EX_GLOB_DICT = {
    "ex11": ["*.TRACE", "*.snx", "*.tro_smoothed"],
    "ex12": ["*.TRACE", "*.snx"],
    "ex13": ["*.TRACE", "*.snx"],
    "ex14": ["TUG/*.TRACE", "*.snx"],
    "ex15": ["*.TRACE", "*.snx"],
    "ex16": ["*.*I", "*.stec", "*.snx", "*SUM", "*.BIA"],
    "ex17": ["*.snx", "*.clk*", "*SUM"],
    "ex21": ["pod*.out", "pod*.rms"],
    "ex22g": ["pod*.out", "pod*.rms"],
    "ex22r": ["pod*.out", "pod*.rms"],
    "ex22e": ["pod*.out", "pod*.rms"],
    "ex22c": ["pod*.out", "pod*.rms"],
    "ex22j": ["pod*.out", "pod*.rms"],
    "ex23": ["pod*.out", "pod*.rms"],
    "ex24": ["pod*.out", "pod*.rms"],
    "ex25": ["pod*.out", "pod*.rms"],
    "ex26": ["pod*.out", "pod*.rms"],
}


def parse_arguments():
    parser = argparse.ArgumentParser(
        description=(
            "Downloads 'products', 'data', and 'solutions' tarballs from s3 bucket and"
            " extracts the content into examples dir. The list of tarballs can be"
            " chaged with the combination of [-p/-d/-s] options. Similar tarballs"
            ' upload functionality is available - can be activated with "--push" key'
        )
    )
    parser.add_argument(
        "--push",
        action="store_true",
        help="tar dirs and push them to aws with checksum metadata and public read",
    )
    parser.add_argument("-s", "--solutions", action="store_true", help="")
    parser.add_argument("-d", "--data", action="store_true", help="")
    parser.add_argument("-p", "--products", action="store_true", help="")
    parser.add_argument("--dirs", nargs="+", default=[])
    parser.add_argument("--tag", default="f82e335") # ideally None or some known key should be a default
    parser.add_argument(
        "--skip_extract",
        action="store_true",
        help=(
            "Skips extraction of the on-disk tar file if checksum test is OK and the"
            " destination dir exists. This is done to save time in the pipeline as"
            " there is no need to overwrite the files."
        ),
    )
    return parser.parse_args()


def insert_tag(name: str, tag: str) -> str:
    """inserts tag name right before the filename:
    insert_tag('ex11','some_tag') -> 'some_tag/ex11'
    insert_tag('solutions/ex11','some_tag') -> 'solutions/some_tag/ex11'
    """
    name_split = name.split("/")
    name_split.insert(-1, tag)
    return "/".join(name_split)


def is_example_name(dir_name):
    return dir_name.startswith("ex") and len(dir_name) <= 5


def update_solutions_dict(examples_dir: _Path, dir: str, ex_glob_dict: dict, tag:str=''):
    """ """
    if is_example_name(dir):  # room for five-symbol name - ex22g
        example_dir = examples_dir / dir
        ref_sol_dir = examples_dir /"solutions" / tag / dir
        if example_dir.exists() and ref_sol_dir.exists():
            _rmtree(ref_sol_dir)
            _logging.info(
                f"removing {ref_sol_dir} and its content"
            )  # if actual solution exists -> clean respective reference solution before copying
        l = len(example_dir.as_posix())
        for expr in ex_glob_dict[dir]:
            expr_counter = 0
            for path in example_dir.glob(expr):
                dst = ref_sol_dir / (path.as_posix()[l + 1 :])
                if expr_counter == 0:
                    dst.parent.mkdir(parents=True, exist_ok=True)
                _logging.info(f"Copying {path} -> {_copy(src=path,dst=dst)}")
    # else:
    #     _logging.info(f"skipping examples syncing as {dir} name does not copply with examples naming convention 'ex.{{1-3}}' dir")


def upload_examples_tar(
    examples_path,
    bucket_name="peanpod",
    target="aux",
    dirs=("products", "data", "solutions"),
    compression="bz2",
    tag='',
):
    __doc__ = (
        "tars selected aux dirs from ginan/examples and compares their checksums with"
        " the ones from s3 bucket. If checksums different - upload, else log info"
        " message and do nothing. Default paths are [bucket] s3://peanpod/aux/ ->"
        " [html] https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/"
    )
    base_url = f"https://{bucket_name}.s3.ap-southeast-2.amazonaws.com/{target}"
    for dir in dirs:
        # update tarname with tag
        is_example = is_example_name(dir)
        if is_example:
            dir = "solutions/" + (f"{tag}/" if tag != '' else '') + dir
        tarname = dir + ".tar." + compression
        destpath_targz = examples_path / tarname
        dest_url = base_url + "/" + tarname
        tar_comp(
            srcpath=examples_path / dir,
            destpath=destpath_targz,
            reset_info=True,
            compression=compression,
        )  # always overwrite
        md5_checksum_aws = request_checksum(dest_url)
        md5_checksum = compute_checksum(destpath_targz)
        if md5_checksum_aws != md5_checksum:
            _logging.info(f'checksums different -> uploading "{tarname}"')
            upload_with_chunksize_and_meta(
                local_file_path=destpath_targz,
                metadata={"md5checksum": md5_checksum},
                public_read=True,
                bucket_name=bucket_name,
                object_key=target + "/" + tarname,
                verbose=True,
            )
        else:
            _logging.info(f"checksums the same -> skipping upload")
        _logging.info(f"------------------------------")


def download_examples_tar(
    examples_path,
    bucket_name="peanpod",
    target="aux",
    dirs=("products", "data", "solutions"),
    compression="bz2",
    tag=None,
    skip_extract=False,
):
    __doc__ = (
        "Downloads compressed selected tarballs from ap-southeast-2 s3 bucket, untars"
        " them to ginan/examples dir. If tarball is available locally then checksums"
        " are compared at first. If the same - nothing is downloaded, local tarball"
        " gets uncompressed"
    )
    script_path = _Path(__file__).resolve().parent
    examples_path = (script_path.parent / "examples").resolve()
    base_url = f"https://{bucket_name}.s3.ap-southeast-2.amazonaws.com/{target}"
    for dir in dirs:
        is_example = is_example_name(dir)
        if is_example:
            dir = f"solutions/{dir}"
        tarname = dir + ".tar." + compression
        destpath_targz = examples_path / tarname
        tagged_tarname = (
            insert_tag(tarname, tag) if (is_example and tag is not None) else tarname
        )
        dest_url = base_url + "/" + tagged_tarname
        md5_checksum_aws = request_checksum(dest_url)
        if md5_checksum_aws is None:
            raise FileNotFoundError
        destpath = examples_path / dir
        if not destpath_targz.exists():
            _logging.warning(msg=f"{tarname} not found on disk ['{md5_checksum_aws}'].")
            destpath_targz.parent.mkdir(parents=True, exist_ok=True)
            download_url(url=dest_url, destfile=destpath_targz)
            tar_extr(srcpath=destpath_targz, destpath=destpath)
        else:
            _logging.info(msg=f"{tarname} found on disk. Validating...")
            md5_checksum = compute_checksum(destpath_targz)
            if md5_checksum_aws != md5_checksum:
                _logging.info(f'checksums different -> downloading "{tarname}"')
                download_url(url=dest_url, destfile=destpath_targz)
                tar_extr(srcpath=destpath_targz, destpath=destpath)
            else:
                _logging.info(f"checksums the same -> skipping download")
                if skip_extract and destpath.exists():
                    _logging.info(
                        "skipping extraction step as '--skip_extract' provided,"
                        " checksums the same and destination directory exists"
                    )
                else:
                    tar_extr(srcpath=destpath_targz, destpath=destpath)


if __name__ == "__main__":
    parsed_args = parse_arguments()
    _logging.getLogger().setLevel(_logging.INFO)

    script_path = _Path(__file__).resolve().parent
    examples_path = (script_path.parent / "examples").resolve()

    if parsed_args.dirs != []:
        dirs = parsed_args.dirs
    else:
        dirs = []
        examples_dirs = list(EX_GLOB_DICT.keys())
        if parsed_args.products:
            dirs.append("products")
        if parsed_args.data:
            dirs.append("data")
        if parsed_args.solutions:
            dirs += examples_dirs
        if dirs == []:
            dirs = ["products", "data", *examples_dirs]
        _logging.info(f"{dirs} selected")
    if parsed_args.push:
        # copy over the required files if exist - if solutions/blah -> rm blah, copy from ../blah to solutions/blah
        print("updating solutions")
        [
            update_solutions_dict(
                examples_dir=examples_path, dir=dir, ex_glob_dict=EX_GLOB_DICT,tag=parsed_args.tag
            )
            for dir in dirs
        ]
        upload_examples_tar(
            dirs=dirs,
            compression="bz2",
            tag=parsed_args.tag,
            examples_path=examples_path,
        )
    else:
        download_examples_tar(
            dirs=dirs,
            compression="bz2",
            tag=parsed_args.tag,
            examples_path=examples_path,
            skip_extract=parsed_args.skip_extract,
        )
