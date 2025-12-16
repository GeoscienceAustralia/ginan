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

import logging
import subprocess
from datetime import datetime
from pathlib import Path

import click

import gnssanalysis as ga
from kill_pids import kill_pids, save_pids
from analyse_orbit_clock import str_to_list


@click.command()
@click.option("--job-dir", required=True, help="Directory where data is processed", type=Path)
@click.option("--product-dir", required=True, help="Directory where product files are placed", type=Path)
@click.option("--pea-dir", required=True, help="Directory where the PEA executable is placed", type=Path)
@click.option("--template-config", required=True, help="Path of template YAML config for PEA instances", type=Path)
@click.option("--ntrip-username", help="Username of NTRIP account", type=str)
@click.option("--ntrip-password", help="Password of NTRIP account", type=str)
@click.option(
    "--ntrip-cred-file-path",
    help='Path of NTRIP credential JSON file where username and password of NTRIP account is saved, with the format of \'{"username": "your_ntrip_username", "password": "your_ntrip_password"}\'. Required if --ntrip-username and --ntrip-password options are not specified',
    type=Path,
)
@click.option(
    "--ssr-streams",
    required=True,
    help="SSR streams to monitor quality of, provided with a comma separated string, e.g. 'SSRA00BKG0, SSRA02IGS0, SSRA03IGS0'",
    type=str,
)
@click.option(
    "--rotation-days",
    required=True,
    help="Recording length of each output file in days. Default: 1 (daily)",
    default=1,
    type=int,
)
@click.option(
    "--ref-prefix",
    required=True,
    help="Prefix of reference products. Default: 'IGS0OPSRAP'",
    default="IGS0OPSRAP",
    type=str,
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
    help="Align gnssanalysis session to GPS week for weekly solutions. Only effective when session length is 7 days (weekly). Default: False",
    default=False,
    is_flag=True,
)
@click.option(
    "--sat-sys",
    required=True,
    help="Satellite system to analyse, 'G', 'R', 'E', 'C', or any combination without space. Default: 'G'",
    default="G",
    type=str,
)
@click.option(
    "--orb-hlm-mode",
    help="Helmert transformation to apply for orbit analysis, 'ECF', 'ECI', or 'none'. Default: 'none'",
    default="none",
    type=str,
)
@click.option(
    "--clk-norm-types",
    help="Normalisations to apply for clock analysis, e.g. 'none', 'sv', 'G01', 'epoch', 'daily', or any combination. Default: 'epoch, daily'",
    default="epoch, daily",
    type=str,
)
@click.option(
    "--min-upload-latency",
    help="Minimum number of hours the files haven't been modified for before uploading to S3. Default: 36 (1.5 days)",
    default=36,
    type=int,
)
@click.option(
    "--cull-file-types",
    help="File types to cull for saving local space, e.g. '.rtcm, .rnx, .json'. Default: None",
    default=None,
    type=str,
)
@click.option(
    "--aws-profile",
    help="Profile of credentials for target S3 bucket in AWS credentials file '~/.aws/credentials', 'default' will be used when no profile is specified. Default: 'default'",
    default="default",
    type=str,
)
@click.option("--s3-bucket", help="S3 bucket for uploading results", default=None, type=str)
@click.option("--s3-root-dir", help="Root directory on S3 bucket for saving results", default=None, type=Path)
@click.option("--verbose", is_flag=True)
def auto_record_ssr_streams_main(
    job_dir,
    product_dir,
    pea_dir,
    template_config,
    ntrip_username,
    ntrip_password,
    ntrip_cred_file_path,
    ssr_streams,
    rotation_days,
    ref_prefix,
    analysis_session_len,
    align_to_gps_week,
    sat_sys,
    orb_hlm_mode,
    clk_norm_types,
    min_upload_latency,
    cull_file_types,
    aws_profile,
    s3_bucket,
    s3_root_dir,
    verbose,
):
    ga.gn_utils.ensure_folders([job_dir])

    if ntrip_username is None:
        ntrip_username = ""
    if ntrip_password is None:
        ntrip_password = ""
    if ntrip_cred_file_path is None:
        ntrip_cred_file_path = ""
    if cull_file_types is None:
        cull_file_types = ""
    if s3_root_dir is None:
        s3_root_dir = job_dir.name

    ssr_list = str_to_list(ssr_streams)

    rotation_period = str(rotation_days * 86400)
    analysis_session_len = str(analysis_session_len)
    align_to_gps_week_arg = ["--align-to-gps-week"] if align_to_gps_week else []
    upload_time_threshold = str(min_upload_latency * 3600)

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
            cmd, stdout=(job_dir / f"download_rt_products_{timestamp}.log").open("w"), stderr=subprocess.STDOUT
        )
    ]

    # Command list for PEA instances
    cmd_list = [
        [
            "python",
            "record_ssr_stream.py",
            "--pea-dir",
            str(pea_dir),
            "--template-config",
            str(template_config),
            "--job-dir",
            str(job_dir),
            "--product-dir",
            str(product_dir),
            "--ntrip-username",
            ntrip_username,
            "--ntrip-password",
            ntrip_password,
            "--ntrip-cred-file-path",
            str(ntrip_cred_file_path),
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
            "--ref-prefix",
            ref_prefix,
            "--session-len",
            analysis_session_len,
            "--sat-sys",
            sat_sys,
            "--orb-hlm-mode",
            orb_hlm_mode,
            "--clk-norm-types",
            clk_norm_types,
        ]
        + align_to_gps_week_arg
        + verbose_arg
    )
    proc_list = proc_list + [
        subprocess.Popen(
            cmd, stdout=(job_dir / f"analyse_orbit_clock_{timestamp}.log").open("w"), stderr=subprocess.STDOUT
        )
    ]

    if not s3_bucket:
        logging.info(f"'--s3-bucket' not specified. Outputs will be saved to '{job_dir}' only")
    else:
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
                cmd, stdout=(job_dir / f"upload_recordings_{timestamp}.log").open("w"), stderr=subprocess.STDOUT
            )
        ]

    # Save PIDs of started commands to file to kill later
    pid_list = [{"PID": proc.pid, "command": " ".join(proc.args)} for proc in proc_list]
    save_pids(pid_file_path, pid_list)

    # Wait above commands to complete
    for proc in proc_list:
        proc.wait()


if __name__ == "__main__":
    auto_record_ssr_streams_main()
