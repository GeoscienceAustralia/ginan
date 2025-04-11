#!/usr/bin/env python3

"""
Script to upload outputs and log files in the job directory to an AWS S3 bucket.
It will run infinitely and attempt upload periodically once it is started.
"""

import logging
import os
import re
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Tuple

import boto3
import click

import gnssanalysis as ga
from analyse_orbit_clock import str_to_list


def file_ready_to_upload(local_path: Path, time_threshold: int) -> Tuple[bool, int]:
    """
    Check if a local file is ready to upload to S3 bucket, i.e. has not been modified for the specified time period.

    :param Path local_path: Path of the local file to upload
    :param int time_threshold: Number of seconds the file has not been modified for before uploading to S3
    :return Tuple[bool, int]: True if the file is ready to upload and False otherwise, and timestamp of the local file
    """
    # Only upload files that haven't been modified within the time threshold
    now = round(time.time())
    modified_time_local = round(local_path.stat().st_mtime)
    delta_time = now - modified_time_local

    if delta_time < time_threshold:
        logging.info(
            f"Local file updated within {time_threshold}s - too recent, last modified: {datetime.fromtimestamp(modified_time_local)}, ommitting upload"
        )
        ready = False
    else:
        logging.debug(
            f"Local file not updated for {delta_time}s, last modified: {datetime.fromtimestamp(modified_time_local)}, ready to upload"
        )
        ready = True

    return ready, modified_time_local


def file_up_to_date_s3(
    s3_client: Any,
    s3_bucket: str,
    dest_path: Path,
    timestamp: int,
    no_logging: bool = False,
) -> bool:
    """
    Check if a file exists on S3 bucket and up-to-date by comparing last modified time against the specified timestamp.

    :param Any s3_client: Object for target S3 client
    :param str s3_bucket: S3 bucket for uploading the file
    :param Path dest_path: Path of the destination file to save on the S3 bucket
    :param int timestamp: Timestamp to compare with
    :param bool no_logging: Do not log
    :return bool: True if the file is up-to-date and False otherwise
    """
    object = s3_client.list_objects_v2(
        Bucket=s3_bucket, Prefix=str(dest_path), MaxKeys=1
    ).get("Contents", [])

    if object:
        modified_time_remote = object[0]["LastModified"].timestamp()
        if modified_time_remote > timestamp:
            (
                logging.info(
                    f"Remote file is up-to-date, last modified: {datetime.fromtimestamp(modified_time_remote)}, ommitting upload"
                )
                if not no_logging
                else None
            )
            return True
        else:
            (
                logging.warning(
                    f"Remote file is outdated, last modified: {datetime.fromtimestamp(modified_time_remote)}, will be overwritten"
                )
                if not no_logging
                else None
            )

    return False


def upload_file_to_s3(
    s3_client: Any,
    s3_bucket: str,
    local_path: Path,
    dest_path: Path,
) -> bool:
    """
    Upload a local file to target S3 bucket.

    :param Any s3_client: Object for target S3 client
    :param str s3_bucket: S3 bucket for uploading the file
    :param Path local_path: Path of the local file to upload
    :param Path dest_path: Path of the destination file to save on the S3 bucket
    :return bool: True if the file is successfully uploaded and False if failed
    """
    try:
        s3_client.upload_file(str(local_path), s3_bucket, str(dest_path))
        logging.info("Successfully uploaded")
        return True
    except boto3.exceptions.S3UploadFailedError as error:
        logging.error(f"Unable to upload: {error}")
        return False


def cull_local_file(
    local_path: Path,
    cull_file_types: list[str],
    excluded_files: str,
) -> None:
    """
    Delete old local files to save space.

    :param Path local_path: Path of the local file to upload
    :param list[str] cull_file_types: File types to cull for saving local space, e.g. '.rtcm, .rnx, .json'
    :param str excluded_files: Files to exclude from culls based on regex, e.g. 'a.*txt', 'a\.txt|b\.out', '[a-z]\.txt'
    :return None
    """
    if re.fullmatch(excluded_files, local_path.name):
        return

    if object and local_path.suffix in cull_file_types:
        logging.info("Deleting the local file")
        local_path.unlink()


def upload_recordings(
    job_dir: Path,
    s3_bucket: str,
    s3_root_dir: Path,
    time_threshold: int,
    cull_file_types: list[str],
    exclude_culls: str,
    aws_profile: str = "default",
) -> None:
    """
    Upload SSR recordings and orbit and clock analysis outputs.

    :param Path job_dir: Directory where recordings and analysis outputs are saved
    :param str s3_bucket: S3 bucket for uploading results
    :param Path s3_root_dir: Root directory on S3 bucket for saving results
    :param int time_threshold: Number of seconds the files haven't been modified for before uploading to S3
    :param list[str] cull_file_types: File types to cull for saving local space, e.g. '.rtcm, .rnx, .json'
    :param str exclude_culls: Files to exclude from culls based on regex, e.g. 'a.*txt', 'a\.txt|b\.out', '[a-z]\.txt'
    :param str aws_profile: Profile of credentials for target S3 bucket in AWS credentials file '~/.aws/credentials',
            defaults to 'default'
    :return None
    """
    logging.info("Uploading recording results ...")

    # S3 client
    s3_client = boto3.Session(profile_name=aws_profile).client("s3")

    # Upload recordings to S3 bucket and cull old files if needed
    num = 0
    for path, subdirs, files in os.walk(
        job_dir
    ):  # Eugene: I remember there was an alternative approach but can't recall the details
        for file in files:
            local_path = Path(path) / file
            s3_dir = path.replace(str(job_dir), str(s3_root_dir))
            dest_path = Path(s3_dir) / file

            logging.info(f"{local_path} --> s3://{s3_bucket}/{dest_path}")

            # Check last modified time of the local file
            ready, modified_time_local = file_ready_to_upload(
                local_path, time_threshold
            )
            if (
                not ready
            ):  # only upload files that haven't been modified within the time threshold
                continue

            # Upload the file to s3 bucket if not exists or outdated on s3
            if not file_up_to_date_s3(
                s3_client, s3_bucket, dest_path, modified_time_local
            ):
                uploaded = upload_file_to_s3(
                    s3_client, s3_bucket, local_path, dest_path
                )
                if uploaded:
                    num = num + 1

            # Cull old recordings
            if file_up_to_date_s3(
                s3_client,
                s3_bucket,
                dest_path,
                modified_time_local,
                no_logging=True,
            ):  # confirm that the file exists on s3 before deleting
                cull_local_file(local_path, cull_file_types, exclude_culls)

    logging.info(f"{num} file(s) uploaded\n")


@click.command()
@click.option(
    "--job-dir",
    required=True,
    help="Directory under which files to upload",
    type=Path,
)
@click.option(
    "--aws-profile",
    required=True,
    help="Profile of credentials for target S3 bucket in AWS credentials file '~/.aws/credentials', 'default' will be used when no profile is specified",
    default="default",
    type=str,
)
@click.option(
    "--s3-bucket",
    required=True,
    help="S3 bucket for uploading results",
    type=str,
)
@click.option(
    "--s3-root-dir",
    required=True,
    help="Root directory on S3 bucket for saving results",
    type=Path,
)
@click.option(
    "--time-threshold",
    required=True,
    help="Number of seconds the files haven't been modified for before uploading to S3",
    default=86400,
    type=int,
)
@click.option(
    "--interval",
    required=True,
    help="Interval to check and upload recordings in seconds",
    default=86400,
    type=int,
)
@click.option(
    "--cull-file-types",
    help="File types to cull for saving local space, e.g. '.rtcm, .rnx, .json'",
    default=None,
    type=str,
)
@click.option(
    "--exclude-culls",
    help="Files to exclude from culls based on regex, e.g. 'a.*txt', 'a\.txt|b\.out', '[a-z]\.txt'",
    default="pid\.json",
    type=str,
)
@click.option("--verbose", is_flag=True)
def upload_recordings_main(
    job_dir,
    aws_profile,
    s3_bucket,
    s3_root_dir,
    time_threshold,
    interval,
    cull_file_types,
    exclude_culls,
    verbose,
):
    ga.gn_utils.configure_logging(verbose)

    cull_file_types = str_to_list(cull_file_types)

    # Run the scheduled tasks indefinitely
    while True:
        upload_recordings(
            job_dir,
            s3_bucket,
            s3_root_dir,
            time_threshold,
            cull_file_types,
            exclude_culls,
            aws_profile,
        )

        now = time.time()
        seconds = interval - now % interval
        time.sleep(seconds)


if __name__ == "__main__":
    upload_recordings_main()
