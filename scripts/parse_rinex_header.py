"""
This script parses the necessary info from a RINEX v3 obs file
to determine what needs to be downloaded for Ginan to be able to run.
"""
from datetime import datetime, timedelta, date
from pathlib import Path
from typing import Tuple
import re

import georinex as gr
import xarray
from gnssanalysis.gn_datetime import GPSDate


def parse_v3_header(filepath: Path):
    header = gr.rinexheader(filepath)
    if int(header["version"]) != 3:
        raise NotImplementedError("Only RINEX v3 is currently supported")

    # Load observations to determine which signals have been observed
    # by the receiver for each gnss system (GPS, Galileo etc)
    obs = gr.load(filepath)
    sys_signals = get_signals_per_system(obs)

    marker_name = header["MARKER NAME"].strip()
    # rec_type = parse_receiver(header)
    rec_type = header["REC # / TYPE / VERS"].strip()
    antenna_type, antenna_dh, antenna_de, antenna_dn = parse_antenna(header)
    approx_x, approx_y, approx_z = parse_approx_position(header)
    first_obs_time = parse_first_obs_time(header)
    last_obs_time = parse_last_obs_time(header)

    header = {
        "marker_name": marker_name,
        "receiver": {"number": "", "type": rec_type, "version": ""},
        "antenna": {"type": antenna_type, "deltas": {"height": antenna_dh, "north": antenna_dn, "east": antenna_de}},
        "approx_position": {"x": approx_x, "y": approx_y, "z": approx_z},
        "first_obs_time": first_obs_time,
        "last_obs_time": last_obs_time,
        "sys_signals": sys_signals,
    }
    return header


def parse_receiver(header: dict) -> Tuple[str, str, str]:
    # Receiver
    receiver = header["REC # / TYPE / VERS"].strip()

    # Split on multiple spaces because there could be spaces in receiver type
    rec_num, rec_type, rec_version = re.split(r"\s{2,}", receiver)
    return rec_num, rec_type, rec_version


def parse_antenna(header: dict) -> Tuple[str, str, str, str]:
    antenna_type = header["ANT # / TYPE"].strip()

    # These are the eccentricities to put in the station config for Ginan
    antenna_deltas = header["ANTENNA: DELTA H/E/N"].strip()
    antenna_dh, antenna_de, antenna_dn = (float(i) for i in antenna_deltas.split())
    return antenna_type, antenna_dh, antenna_de, antenna_dn


def parse_approx_position(header: dict) -> (float, float, float):
    # -5015815.7300  2619358.9161 -2933465.3598
    x, y, z = (float(i) for i in header["APPROX POSITION XYZ"].strip().split())
    return x, y, z


def parse_obs_time(time: str) -> datetime:
    # 'TIME OF FIRST OBS': '  2023    10    29     6    13   30.0000000     GPS         '
    # Remove extra whitespace and GPS suffix '2023 10 29 6 13 30.0000000'
    epoch = " ".join(time.strip().split()[:6])
    # Trim off the last digit in micro seconds - strptime only allows 6 decimals
    epoch = epoch[:-1]
    obs_time = datetime.strptime(epoch, "%Y %m %d %H %M %S.%f")
    return obs_time


def parse_first_obs_time(header: str) -> str:
    time = header["TIME OF FIRST OBS"]
    first_obs_time = parse_obs_time(time)
    return first_obs_time


def parse_last_obs_time(header: str) -> str:
    time = header["TIME OF LAST OBS"]
    last_obs_time = parse_obs_time(time)
    return last_obs_time


def get_signals_per_system(obs: xarray.Dataset) -> dict:
    """Find all of the signals observed by the receiver for each system.
    The RINEX header has this information, but it cannot be trusted.
    For example, Victoria submits their CORS data to GA with all signals in the header,
    even if there are no observations.
    """
    # TODO: There is probably cleaner a way to do this with xarray groupby
    signals = {}
    GNSS_SYSTEMS = {"G": "gps", "R": "glo", "S": "sbas", "C": "bds", "E": "gal", "J": "qzs"}
    for syschar, sys in GNSS_SYSTEMS.items():
        sys_index = obs.sv.to_index().str.startswith(syschar)
        sys_data = obs.isel(sv=sys_index)

        sys_signals = set()
        for signal in sys_data.keys():
            sv_signal = sys_data[signal]
            has_data = sv_signal.notnull().any()
            if has_data:
                sys_signals.add(signal)

        signals[sys] = sys_signals
    return signals
