#!/usr/bin/env python3

"""
Master script to automatically
- Download required real-time products for recording SSR streams
- Start PEA instances to record and decode SSR streams
- Compare real-time orbits and clocks against IGS rapid products
- Upload output files to an AWS S3 bucket

Children processes will be running continously in background once they are started. Closing current terminal
will NOT kill those processes. To kill all running processes started by this master script, one can
- Press CTRL + C, if current terminal is not closed
- Run kill_pids.py
- Restart the same job using this master script (this will result in new running processes)
"""

import json
import click
import logging
import subprocess
import gnssanalysis as ga
from pathlib import Path
from datetime import datetime
from kill_pids import kill_pids


@click.command()
@click.option(
    "--job-dir", required=True, help="Directory where data is processed", type=Path
)
@click.option(
    "--product-dir",
    required=True,
    help="Directory where product files are placed",
    type=Path,
)
@click.option(
    "--template-config",
    required=True,
    help="Path of template YAML config for PEA instances",
    type=Path,
)
@click.option(
    "--ssr-streams", required=True, help="SSR streams to monitor quality of", type=str
)
@click.option(
    "--rotation-days",
    required=True,
    help="Recording length of each output file in days. Default: 1 (daily)",
    default=1,
    type=int,
)
@click.option(
    "--analysis-session-len",
    required=True,
    help="Length of a session for each gnssanalysis file/plot in days. Default: 1 (daily)",
    default=1,
    type=int,
)
@click.option(
    "--align-to-gps-week",
    help="Align gnssanalysis session to GPS week for weekly solutions. Only effective when session length is 7 days (weekly)",
    default=False,
    is_flag=True,
)
@click.option(
    "--sat-sys",
    required=True,
    help="Satellite system to analyse, G, R, E, C, or any combination without space",
    default="G",
    type=str,
)
@click.option(
    "--clk-norm-types",
    help="Normalisations to apply for clock files for clock analysis, e.g. 'none', 'sv', 'G01', 'epoch', 'daily', or any combination. Default: 'epoch, daily'",
    default="epoch, daily",
    type=str,
)
@click.option(
    "--min-upload-latency",
    required=True,
    help="Minimum number of hours the files haven't been modified for before uploading to S3. Default: 36 (1.5 days)",
    default=36,
    type=int,
)
@click.option(
    "--cull-file-types",
    help="File types to cull for saving local space, e.g. '.rtcm, .rnx, .json'",
    default=None,
    type=str,
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
@click.option("--verbose", is_flag=True)
def auto_record_ssr_streams_main(
    job_dir,
    product_dir,
    template_config,
    ssr_streams,
    rotation_days,
    analysis_session_len,
    align_to_gps_week,
    sat_sys,
    clk_norm_types,
    min_upload_latency,
    cull_file_types,
    aws_profile,
    s3_bucket,
    s3_root_dir,
    verbose,
):
    ga.gn_utils.ensure_folders([job_dir])

    ssr_list = []
    if ssr_streams:
        ssr_list = ssr_streams.replace(" ", "").split(",")

    rotation_period = str(rotation_days * 86400)
    analysis_session_len = str(analysis_session_len)
    align_to_gps_week_arg = ["--align-to-gps-week"] if align_to_gps_week else []
    upload_time_threshold = str(min_upload_latency * 3600)

    if cull_file_types is None:
        cull_file_types = ""

    ga.gn_utils.configure_logging(verbose)
    verbose_arg = ["--verbose"] if verbose else []

    # Kill running commands associated with current job
    pid_file_path = job_dir / "pid.json"
    kill_pids(pid_file_path)

    now = datetime.now()
    yrdoy = now.strftime("%Y%j")
    timestamp = f"{yrdoy}{now.hour:02d}{now.minute:02d}"

    proc_list = []

    # Command for downloading products
    cmd = [
        "nohup",
        "python",
        "download_rt_products.py",
        "--product-dir",
        str(product_dir),
        "--interval",
        rotation_period,
    ] + verbose_arg
    proc_list = proc_list + [
        subprocess.Popen(
            cmd,
            stdout=(job_dir / f"download_rt_products_{timestamp}.log").open("w"),
            stderr=subprocess.STDOUT,
        )
    ]

    # Command list for PEA instances
    cmd_list = [
        [
            "python",
            "record_ssr_stream.py",
            "--template-config",
            str(template_config),
            "--job-dir",
            str(job_dir),
            "--product-dir",
            str(product_dir),
            "--ssr-mountpoint",
            ssr,
            "--rotation-period",
            rotation_period,
        ]
        + verbose_arg
        for ssr in ssr_list
    ]
    proc_list = proc_list + [subprocess.Popen(cmd) for cmd in cmd_list]

    # Command for orbit and clock analysis/plotting
    cmd = (
        [
            "nohup",
            "python",
            "analyse_orbit_clock.py",
            "--job-dir",
            str(job_dir),
            "--ref-dir",
            str(product_dir),
            "--session-len",
            analysis_session_len,
            "--sat-sys",
            sat_sys,
            "--clk-norm-types",
            clk_norm_types,
        ]
        + align_to_gps_week_arg
        + verbose_arg
    )
    proc_list = proc_list + [
        subprocess.Popen(
            cmd,
            stdout=(job_dir / f"analyse_orbit_clock_{timestamp}.log").open("w"),
            stderr=subprocess.STDOUT,
        )
    ]

    # Command for uploading recordings
    cmd = [
        "nohup",
        "python",
        "upload_recordings.py",
        "--job-dir",
        str(job_dir),
        "--aws-profile",
        aws_profile,
        "--s3-bucket",
        s3_bucket,
        "--s3-root-dir",
        str(s3_root_dir),
        "--time-threshold",
        upload_time_threshold,
        "--interval",
        rotation_period,
        "--cull-file-types",
        cull_file_types,
    ] + verbose_arg
    proc_list = proc_list + [
        subprocess.Popen(
            cmd,
            stdout=(job_dir / f"upload_recordings_{timestamp}.log").open("w"),
            stderr=subprocess.STDOUT,
        )
    ]

    # Save PIDs of started commands to file to kill later
    with pid_file_path.open("a") as pid_file:
        for proc in proc_list:
            json.dump({"PID": proc.pid, "command": " ".join(proc.args)}, pid_file)
            pid_file.write("\n")
        logging.debug(f"PIDs of started commands saved to file {pid_file_path}")

    # Wait above commands to complete
    for proc in proc_list:
        proc.wait()


if __name__ == "__main__":
    auto_record_ssr_streams_main()
