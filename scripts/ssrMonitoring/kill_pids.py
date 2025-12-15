#!/usr/bin/env python3

"""
Script to kill running processes that may conflict with the target job.
The PIDs of and correponding commands for running processes should be
recorded in a JSON file named 'pid.json' in the job directory.
"""

import json
import logging
import os
import signal
from pathlib import Path

import click

import gnssanalysis as ga


def save_pids(pid_file_path: Path, proc_list: dict, overwrite: bool = False) -> None:
    """
    Save PIDs and corresponding commmands to a JSON file.

    :param Path pid_file_path: Path of the JSON file to log PIDs to
    :param dict proc_list: A dictionary containing the PIDs and commands of the processes to log
    :param bool overwrite: Overwrite existing PID file, defaults to False
    :return None
    """
    mode = "w" if overwrite else "a"

    try:
        with pid_file_path.open(mode) as pid_file:
            for proc in proc_list:
                json.dump(proc, pid_file)
                pid_file.write("\n")
        logging.info(f"PIDs of running commands saved to file '{pid_file_path}'")
    except Exception as error:
        logging.error(f"Could not write PID file: {error}")


def kill_pids(pid_file_path: Path) -> None:
    """
    Kill running processes by their PIDs logged in a JSON file.

    :param Path pid_file_path: Path of the JSON file where PIDs are logged
    :return None
    """
    logging.info("Killing running processes ...")

    if not pid_file_path.exists():
        logging.warning(f"PID file '{pid_file_path}' not found")
        return

    # Kill running commands with PIDs listed in the PID file
    procs_not_killed = []
    with pid_file_path.open("r") as pid_file:
        for line in pid_file:
            proc = json.loads(line)
            pid = proc["PID"]
            arg = proc["command"]

            try:
                os.kill(pid, signal.SIGTERM)
                logging.info(f"Process {pid}: '{arg}' killed")
            except ProcessLookupError as error:
                logging.debug(f"Process {pid}: '{arg}' not exists, may have terminated: {error}")
            except Exception as error:
                procs_not_killed.append(proc)
                logging.warning(f"Could not kill {pid}: '{arg}': {error}")

    # Overwrite the PID file with PIDs not killed
    if procs_not_killed:
        logging.info(f"Updating '{pid_file_path}' with PIDs not killed")

    save_pids(pid_file_path, procs_not_killed, overwrite=True)

    logging.info("Processes killed\n")


@click.command()
@click.option("--job-dir", required=True, help="Directory where PID file (pid.json) is saved", type=Path)
@click.option("--verbose", is_flag=True)
def kill_pids_main(job_dir, verbose):
    ga.gn_utils.configure_logging(verbose)

    pid_file_path = job_dir / "pid.json"
    kill_pids(pid_file_path)


if __name__ == "__main__":
    kill_pids_main()
