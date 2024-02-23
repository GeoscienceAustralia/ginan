#!/bin/env python3

import logging
from pathlib import Path
import base64
import hashlib
import os
import tarfile
import concurrent
import argparse

import boto3
from botocore import UNSIGNED
from botocore.config import Config


logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)


class S3Client:
    def __init__(self, profile_name: str = None, access_key: str = None, secret_key: str = None) -> None:
        self.s3 = self._create_s3_client(profile_name, access_key, secret_key)

    def _create_s3_client(
        self, profile_name: str = None, access_key: str = None, secret_key: str = None
    ) -> boto3.client:
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
        config = {"max_pool_connections": 50}
        if profile_name:
            session = boto3.Session(profile_name=profile_name)
            logger.debug("setting up s3 client with profile %s", profile_name)
        elif access_key and secret_key:
            session = boto3.Session(aws_access_key_id=access_key, aws_secret_access_key=secret_key)
            logger.debug("setting up s3 client with access key and secret key")
        else:
            session = boto3.Session()
            config['signature_version'] = UNSIGNED
            logger.debug("setting up s3 client with no credentials")
        s3 = session.client("s3", config=Config(**config))
        
        return s3

    def check_checksum(self, bucket: str, file: str, download_path: str) -> None:
        """
        check_checksum _summary_

        :param str bucket: _description_
        :param str file: _description_
        :param str download_path: _description_
        :raises Exception: When checksum comparison failed.
        """
        logger.info(f"Checking checksum of {file}")
        response = self.s3.head_object(Bucket=bucket, Key=file)
        if self.compute_checksum(download_path) != response["Metadata"]["md5checksum"]:
            raise Exception(f"Checksum failed for {file}")
        logger.info(" -> Checksum OK")

    def compute_checksum(self, path: Path) -> str:
        """
        compute_checksum Compute the checksum of a file given its path

        :param Path path: Path of the file
        :return str: checksum computed
        """
        logger.debug(f'Computing checksum of "{path}"')
        with open(path, "rb") as file:
            filehash = hashlib.md5()
            for data in iter(lambda: file.read(8 * 1024 * 1024), b""):
                filehash.update(data)
        checksum = base64.b64encode(filehash.digest()).decode()
        logger.debug(f'Got "{checksum}"')
        return checksum

    def download_file(self, bucket: str, file: str, download_path: str, checksum_check: bool = True) -> None:
        """
        download_file Download file from s3. if check_sum is True, it will check the checksum of the file

        :param str bucket: bucket where the file is located
        :param str file: path of the file in the bucket
        :param str download_path: download path of he file
        :param bool checksum_check: enable checksum, defaults to True
        :raises exception: when checksum comparison failed
        """
        logger.info(f"Downloading {file} from {bucket}")
        self.s3.download_file(bucket, file, download_path)
        if checksum_check:
            try:
                self.check_checksum(bucket, file, download_path)
            except Exception as excep:
                os.remove(download_path)
                raise excep

    def upload_file(self, bucket: str, source: str, destination: str):
        """
        upload_file Upload a file to s3

        :param str bucket: bucket name
        :param str file_path: path of the file to upload
        :param str key: path of the file in the bucket
        """
        logger.info(f"Uploading {source} to {bucket} ... {destination}")
        checksum = self.compute_checksum(source)
        metadata = {"md5checksum": checksum}
        self.s3.upload_file(source, bucket, destination, ExtraArgs={"Metadata": metadata, "ACL":"public-read"})
        logger.info(" -> Upload completed")
        
# #### below is to be moved in the DataTransfer class
#     def upload_tar_file(self, bucket: str, file_path: str, directory: str, tag: str):
#         logger.info(f"Creating tar file {file_path}")
#         file_path =  f"{directory}.tar.bz2"
#         with tarfile.open(file_path, "w:bz2") as tar:
#             tar.add(directory, arcname=os.path.basename(directory))
#         logger.info(" -> Tar file created")
#         self.upload_file(bucket, file_path, tag)


class DataTransfer:
    def __init__(
        self, s3_client: S3Client, bucket: str, target: str, path: str, tag_dict: dict, skip_extract: bool
    ) -> None:
        self.s3_client:S3Client = s3_client
        self.bucket:str = bucket
        self.target:str = target
        self.path:str = path
        self.tag_dict:dict = tag_dict
        self.skip_extract:bool = skip_extract

    def get_list_from_tag(self, dirs: list):
        """
        get_list_from_tag Generate the list of examples in each tag indicated from a tag file.

        :param list dirs: List of the exampels to process
        :return list: Full path on the bucket of the list of the examples to process
        :except KeyError: When the tag is not find on the bucket
        """
        list_to_process = []
        ex_type_dict = {"PEA": [0, 1, 4], "POD": [2], "PEAPOD": [3], "OTHER": [5]}
        ex_type_dict["ALL"] = list(set([item for sublist in ex_type_dict.values() for item in sublist]))
        logger.info(f"To process {dirs}")
        for key, tag in self.tag_dict.items():
            logger.debug(f"Looking for data in {key} tagged {tag}")
            request = self.s3_client.s3.list_objects(Bucket=self.bucket, Prefix=f"{self.target}/solutions/{tag}/")
            try:
                for data in request["Contents"]:
                    name = data["Key"].split("/")[-1].split(".")[0]
                    if (len(dirs) == 0 or name in dirs) and int(name[2]) in ex_type_dict[key]:
                        logger.debug(f"Found {data['Key']} in {key}")
                        list_to_process.append(data["Key"])
            except KeyError:
                logger.warning(f"{request['Prefix']} on bucket {request['Name']} not found")
        return list_to_process

    def transfer_files_concurrently(self, source_files: list, destination_files: list, operation: str):
        """
        process_files_concurrently process files concurrently using a thread pool

        :param list files: array containing the list of files to process
        :param str operation: operation to perform on the files upload or download
        """
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            futures = []
            for source_file, destination_file in zip(source_files, destination_files):
                if operation == "download":
                    futures.append(
                        executor.submit(self.s3_client.download_file, self.bucket, source_file, destination_file, True)
                    )
                elif operation == "upload":
                    futures.append(
                        executor.submit(self.s3_client.upload_file, self.bucket, source_file, destination_file)
                    )

            for future in concurrent.futures.as_completed(futures):
                try:
                    future.result()  # Ensure any exceptions are raised
                except Exception as e:
                    logger.warning(f"Failed to process file: {str(e)}")

    def extract_files(self, files: list):
        for file in files:
            download_path = os.path.join(self.path, file.split("/")[-1])
            self.extract_tar(download_path, self.path)


    def create_tar_file(self, directory: str):
        file_path =  f"{directory}"
        # remove suffix from directory
        if file_path.endswith(".tar.bz2"):
            directory_ = file_path[:-8]
        with tarfile.open(file_path, "w:bz2") as tar:
            tar.add(directory_, arcname=os.path.basename(directory_))
        logger.info(" -> Tar file created")
        return file_path
        
    
    def extract_tar(self, filename: str, extract_path: str):
        """
        extract_tar Extraca a tar file

        :param str filename: archive to extract
        :param str extract_path: path of extraction
        """
        logger.info(f"Extracting {filename} to {extract_path}")
        with tarfile.open(filename, "r:bz2") as tar:
            for member in tar.getmembers():
                if not member.name.startswith("/") and ".." not in member.name:
                    tar.extract(member, extract_path)
                else:
                    logging.warning(f"Skipping dangerous member: {member.name}")
        os.remove(filename)
        logger.info(f"Extracted {filename} to {extract_path}")
        
        
    def process_example_data(self, list_to_process: dict, data:bool=False, solutions:bool=False, operation:str=""):

        logger.info(f"List of files to {operation}:")
        remote_file = []
        local_file = []
        if data:
            remote_file.extend([f"{self.target}/{file}.tar.bz2" for file in list_to_process['data']])
            local_file.extend([os.path.join(self.path, file.split("/")[-1]+".tar.bz2") for file in list_to_process['data']])
        if solutions:
            if operation == "download":
                list_files = self.get_list_from_tag(list_to_process['solutions'])
            else:
                list_files = [f"{self.target}/solutions/{self.tag_dict['ALL']}/{file}.tar.bz2" for file in list_to_process['solutions']]
            remote_file.extend([f"{file}" for file in list_files])
            local_file.extend([os.path.join(self.path, file.split("/")[-1]) for file in list_files])
        logger.debug(f"local {local_file}")
        logger.debug(f"remote {remote_file}")
        if operation == "download":
            self.transfer_files_concurrently(remote_file, local_file, "download")
            if not self.skip_extract:
                self.extract_files(local_file)
        elif operation == "upload":
            for file in local_file:
                self.create_tar_file(file)
            self.transfer_files_concurrently(local_file, remote_file, "upload")


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


def generate_tag_dict(tag: str, tags_file_path: str) -> dict:
    tag_dict = {}
    if not tag:
        logger.info(f"Reading tags from {tags_file_path}")
        tag_dict = read_tags_from_file(tags_file_path)
        tag_dict["ALL"] = None
    else:
        tag_dict["ALL"] = tag
        logger.info(f"Using the provided {tag_dict} tag")
    return tag_dict


def main():
    parser = argparse.ArgumentParser(
        description="Downloads/uploads auxiliary example_input_data files to/from example_input_data dir"
    )
    parser.add_argument("dirs", nargs="*", help="directories to process")
    parser.add_argument("--bucket", default="peanpod", help="S3 bucket name to push and pull from")
    parser.add_argument("--target", default="aux", help="S3 target name (dir) within the selected bucket")
    parser.add_argument(
        "--path",
        default="inputData",
        type=Path,
        help="Custom path to inputData dir, a dir that stores products/data etc. Default is inputData",
    )
    parser.add_argument("--tagpath", default="docker/tags", help="Path to the tags file")
    parser.add_argument("--tag", help="Tag to filter the files (optional)")
    parser.add_argument(
        "--skip-extract",
        action="store_true",
        help="Skip extraction of the on-disk tar file if checksum test is OK and the destination dir exists. This saves time in the pipeline as there is no need to overwrite the files.",
    )
    parser.add_argument("-p", "--products", action="store_true", help="Process products")
    parser.add_argument("-d", "--data", action="store_true", help="Process data")
    parser.add_argument("-l", "--loading", action="store_true", help="Process loadings")
    parser.add_argument("-s", "--solutions", action="store_true", help="Process solutions")
    parser.add_argument("--profile", default=None, help="AWS profile name (Upload only)")
    parser.add_argument("--accesskey", default=None, help="AWS access key (Upload only)")
    parser.add_argument("--secretkey", default=None, help="AWS secret key (Upload only)")

    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")
    parser.add_argument("--method", choices=["upload", "download"], default="download", help="Method to perform: upload or download")

    args = parser.parse_args()

    logger.setLevel(logging.DEBUG if args.verbose else logging.INFO)
    s3_client = S3Client(profile_name=args.profile, access_key=args.accesskey, secret_key=args.secretkey)
    tag_dict = generate_tag_dict(args.tag, args.tagpath)
    logger.info(f"List of tags {tag_dict}")

    #if args.path is not an existing directory, create it
    if not os.path.exists(args.path):
        logger.debug(f"Creating {args.path}")
        os.makedirs(args.path)
        
    processor = DataTransfer(
        s3_client,
        args.bucket,
        args.target,
        args.path,
        tag_dict,
        args.skip_extract,
    )

    if args.method == "download":
        dirs = args.dirs if args.dirs else []
        to_process = {'data':[], 'solutions':[]}
        if args.solutions:
            to_process['solutions'].extend(args.dirs if args.dirs else [])
        if args.data:
            to_process['data'].append("data")
            # list_to_process.append(f"{args.target}/data.tar.bz2")
        if args.products:
            to_process['data'].append("products")
            # list_to_process.append(f"{args.target}/products.tar.bz2")
        if args.loading:
            to_process['data'].append("loading")
            # list_to_process.append(f"{args.target}/loading.tar.bz2")
        data = args.products or args.data or args.loading
        solutions = args.solutions
        processor.process_example_data(to_process, data=data, solutions=solutions , operation="download")

    if args.method == "upload":
        to_process = {'data':[], 'solutions':[]}

        if args.products:
            to_process['data'].append("products")
        if args.data:
            to_process['data'].append("data")
        if args.loading:
            to_process['data'].append("loading")
        if args.solutions:
            to_process['solutions'].extend(args.dirs)
        data = args.products or args.data or args.loading
        solutions = args.solutions
        processor.process_example_data(to_process, data=data, solutions=solutions , operation= "upload")
        # processor.process_files_concurrently(list_to_process, "upload")


if __name__ == "__main__":
    main()
