#!/usr/bin/env python3

"""
Script to kill running processes that may conflict with the target job.
The PIDs of and correponding commands for running processes should be
recorded in a JSON file named 'pid.json' in the job directory.
"""

import os
import json
import click
import signal
import logging
import gnssanalysis as ga
from pathlib import Path


def kill_pids(
    pid_file_path: Path,
) -> None:
    """
    Kill running processes by their PIDs logged in a json file.

    :param Path pid_file_path: Path of the json file where PIDs are logged
    :return None
    """
    logging.info("Killing running processes ...")

    if not pid_file_path.exists():
        logging.warning(f"PID file {pid_file_path} not found")
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
                logging.debug(
                    f"Process {pid}: '{arg}' not exists, may have terminated: {error}"
                )
            except Exception as error:
                procs_not_killed.append(proc)
                logging.exception(f"Could not kill {pid}: '{arg}': {error}")

    # Overwrite the PID file with PIDs not killed
    with pid_file_path.open("w") as pid_file:
        logging.debug(f"Updating {pid_file_path} with PIDs not killed")
        for proc in procs_not_killed:
            json.dump(proc, pid_file)
            pid_file.write("\n")

    logging.info("Processes killed\n")


@click.command()
@click.option(
    "--job-dir",
    required=True,
    help="Directory where PID file (pid.json) is saved",
    type=Path,
)
@click.option("--verbose", is_flag=True)
def kill_pids_main(
    job_dir,
    verbose,
):
    ga.gn_utils.configure_logging(verbose)

    pid_file_path = job_dir / "pid.json"
    kill_pids(pid_file_path)


if __name__ == "__main__":
    kill_pids_main()
