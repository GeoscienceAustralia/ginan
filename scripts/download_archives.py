"""Downloads auxiliary example_input_data files to/from example_input_data dir"""
import logging
import hashlib
import concurrent.futures
import base64
import os

from pathlib import Path
from typing import Union as _Union
import tarfile

import boto3
from botocore import UNSIGNED
from botocore.config import Config

import click as _click

logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)


def compute_checksum(path2file: str) -> str:
    """Computes checksum of a file given its path

    :param str path2file: path to the file
    :return str: checksum value
    """
    logger.debug(f'Computing checksum of "{path2file}"')
    with open(path2file, "rb") as file:
        filehash = hashlib.md5()
        for data in iter(lambda: file.read(8 * 1024 * 1024), b""):
            filehash.update(data)
    checksum = base64.b64encode(filehash.digest()).decode()
    logger.debug(f'Got "{checksum}"')
    return checksum


def create_s3_client(profile_name: str = None, access_key: str = None, secret_key: str = None) -> boto3.client:
    """
    create_s3_client creates a boto3 client for s3
    if profile_name is provided, it will use the credentials from the profile
    if access_key and secret_key are provided, it will use those credentials
    otherwise it will create an anonymous client

    :param str profile_name: _description_, defaults to None
    :param str access_key: _description_, defaults to None
    :param str secret_key: _description_, defaults to None
    :return boto3.client: _description_
    """
    if profile_name:
        session = boto3.Session(profile_name=profile_name)
        logger.debug("setting up s3 client with profile %s", profile_name)
    elif access_key and secret_key:
        session = boto3.Session(aws_access_key_id=access_key, aws_secret_access_key=secret_key)
        logger.debug("setting up s3 client with access key and secret key")
    else:
        session = boto3.Session()
        logger.debug("setting up s3 client with no credentials")
    s3 = session.client("s3", config=Config(signature_version=UNSIGNED, max_pool_connections=50))
    return s3


def read_tags_from_file(file_path):
    """
    read_tags_from_file _summary_

    :param _type_ file_path: _description_
    :return _type_: _description_
    """
    dictionary = {}
    with open(file_path, "r", encoding="utf-8") as file:
        for line in file:
            if "=" in line:
                key, value = line.strip().split("=")
                value = value.strip('"')  # Remove quotes from the value
                dictionary[key] = value
    return dictionary


def get_list_from_tag(s3client, bucket, dictdata, target, dirs, list_to_download):
    """
    get_list_from_tag
    Function to get the list of files to download from the tag,
    for each element in the dictionary dictdata, it will need to download the file at the following address
    https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/solutions/<TAGNAME>/index.json
    concatenate them all in a dictionary.
    :param _type_ dictdata: _description_
    """
    ex_type_dict = {"PEA": [0, 1, 4], "POD": [2], "PEAPOD": [3], "OTHER": [5]}
    ex_type_dict["ALL"] = list(set([item for sublist in ex_type_dict.values() for item in sublist]))

    logger.info(f" to download {dirs}")
    for key, tag in dictdata.items():
        logger.debug(f"Looking for data in {key} tagged {tag}")
        request = s3client.list_objects(Bucket=bucket, Prefix=f"{target}/solutions/{tag}/")
        try:
            for data in request["Contents"]:
                name = data["Key"].split("/")[-1].split(".")[0]
                if (len(dirs) == 0 or name in dirs) and int(name[2]) in ex_type_dict[key]:
                    logger.debug(f"Found {data['Key']} in {key}")
                    list_to_download.append(data["Key"])
        except KeyError:
            logger.warning(f"{request['Prefix']} on bucket {request['Name']} not found")


def check_checksum(s3client, bucket, file, download_file):
    logger.info(f"Checking checksum of {file}")
    response = s3client.head_object(Bucket=bucket, Key=file)
    if compute_checksum(download_file) != response["Metadata"]["md5checksum"]:
        raise Exception(f"Checksum failed for {file}")
    logger.info(" -> Checksum OK")


def download_file(s3client, bucket, file, checksum_check=False):
    logger.info(f"Downloading {file} from {bucket}")
    filename = file.split("/")[-1]
    s3client.download_file(bucket, file, filename)
    if checksum_check:
        try:
            check_checksum(s3client, bucket, file, filename)
        except Exception as excep:
            os.remove(filename)
            raise excep
    return filename


def extract(filename, path):
    logger.info(f" -> Extracting {filename} to {path}")
    with tarfile.open(filename, "r:bz2") as tar:
        for member in tar.getmembers():
            if not member.name.startswith("/") and ".." not in member.name:
                tar.extract(member, path)
            else:
                logging.warning(f"Skipping dangerous member: {member.name}")
    os.remove(filename)
    logger.info(f" -> Extracted {filename} to {path}")


def process_dwl_file(s3client: boto3.client, bucket, file, path, skip_extract):
    """
    download_file
    Function to download the file from the bucket and extract it to the path
    :param _type_ file: _description_
    """
    try:
        filename = download_file(s3client, bucket, file, checksum_check=True)
        if not skip_extract:
            extract(filename, path)
        else:
            logger.info(f" -> Skipping extraction of {file} to {path}")
    except Exception as e:
        raise e


def process_dwl_files_concurrently(
    s3client: boto3.client, bucket: str, files: list, path: Path, skip_extract: bool
) -> None:
    """
    Download files concurrently using a ThreadPoolExecutor.
    """
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
        futures = [executor.submit(process_dwl_file, s3client, bucket, file, path, skip_extract) for file in files]

        for future in concurrent.futures.as_completed(futures):
            try:
                future.result()  # Ensure any exceptions are raised
            except Exception as e:
                logger.warning(f"Failed to download file: {str(e)}")


def generate_list_of_files(dirs, bucket, target, solutions, data, products, loading, s3, tag_dict) -> list:
    list_to_download = []
    if products:
        list_to_download.append(f"{target}/products.tar.bz2")
    if data:
        list_to_download.append(f"{target}/data.tar.bz2")
    if loading:
        list_to_download.append(f"{target}/loading.tar.bz2")
    if solutions:
        get_list_from_tag(s3, bucket, tag_dict, target, dirs, list_to_download)
    return list_to_download


def generate_tag_dict(tag: str, tags_file_path: str) -> dict:
    """
    generate_tag_dict _summary_

    :param _type_ tag: _description_
    :param _type_ tags_file_path: _description_
    :return _type_: _description_
    """
    tag_dict = {}
    if not tag:
        logger.info(f"reading tags from {tags_file_path}")
        tag_dict = read_tags_from_file(tags_file_path)
    else:
        tag_dict["ALL"] = tag
        logger.info(f"using the provided {tag_dict} tag")
    return tag_dict


# function to download_example_data, command line argument viw click p for product, l for loading, s for solution, followed by uknown number of arguments
@_click.command()
@_click.argument("dirs", nargs=-1, type=str, default=None)
@_click.option("--bucket", default="peanpod", help="s3 bucket name to push and pull from")
@_click.option("--target", default="aux", help="s3 target name (dir) within the selected bucket")
@_click.option(
    "--path",
    type=Path,
    default="inputData",
    help="custom path to inputData dir, a dir that stores products/data etc, default is inputData",
)
@_click.option("--tagpath", type=Path, default="docker/tags")
@_click.option("--tag", type=str, default=None)
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
@_click.option("-s", "--solutions", is_flag=True, help="download solutions")
@_click.option("-d", "--data", is_flag=True, help="download data")
@_click.option("-p", "--products", is_flag=True, help="download products")
@_click.option("-l", "--loading", is_flag=True, help="download loadings")
@_click.option("--profile", default=None, help="aws profile name")
@_click.option("-v", "--verbose", is_flag=True, help="verbose output")
def download_example_data(
    dirs: str,
    bucket: str,
    target: str,
    path: Path,
    tagpath: Path,
    tag: str,
    skip_extract: bool,
    solutions: bool,
    data: bool,
    products: bool,
    loading: bool,
    profile: str,
    verbose: bool,
):
    """Downloads auxiliary example_input_data files to/from example_input_data dir"""
    logger.setLevel(logging.DEBUG if verbose else logging.INFO)
    # todo later plug the possible for the keys. (not needed for download)
    s3 = create_s3_client(profile_name=profile, access_key=None, secret_key=None)
    tag_dict = generate_tag_dict(tag, tagpath.resolve())
    logger.info(f"list of tags {tag_dict}")
    list_to_download = generate_list_of_files(dirs, bucket, target, solutions, data, products, loading, s3, tag_dict)
    logger.info("list of files to download")
    for file in list_to_download:
        logger.info(f" - {file}")
    process_dwl_files_concurrently(s3, bucket, list_to_download, path.resolve(), skip_extract)


if __name__ == "__main__":
    download_example_data()
