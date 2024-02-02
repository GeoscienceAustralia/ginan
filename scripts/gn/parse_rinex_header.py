"""
This script parses info from a RINEX v3 obs file. This info is used
downstream to:
- determine what needs to be downloaded for Ginan to be able to run.
"""
from datetime import datetime
from pathlib import Path
from typing import Tuple, NamedTuple, Optional, Dict
import re

import georinex
import xarray

# Mappings from character code to Ginan name
GNSS_SYSTEMS = {"G": "gps", "R": "glo", "S": "sbas", "C": "bds", "E": "gal", "J": "qzs"}


class AntennaDeltas(NamedTuple):
    height: float
    north: float
    east: float


class Antenna(NamedTuple):
    type: str
    deltas: AntennaDeltas

    def get_eccentricity(self):
        """Ginan expects ENU, but RINEX is UNE"""
        deltas = self.deltas
        return [deltas.east, deltas.north, deltas.height]


class ApproxPosition(NamedTuple):
    x: float
    y: float
    z: float

    def get_apriori_position(self):
        return [self.x, self.y, self.z]


class RinexHeader(NamedTuple):
    marker_name: str
    antenna: Antenna
    approx_position: ApproxPosition
    first_obs_time: datetime
    last_obs_time: Optional[datetime]
    sys_signals: Dict[str, str]

    def get_station_alias(self):
        """Ginan expects a four character string for each station. A station may have more characters than this,
        so we need to create an alias."""
        alias = self.marker_name
        if len(self.marker_name) > 4:
            # Rename the rinex file to RXXX.rnx, where XXX are the last 3 characters of the marker name.
            alias = f"R{self.marker_name[-3:]}"
        return alias


def parse_v3_header(filepath: Path) -> RinexHeader:
    header = georinex.rinexheader(filepath)
    if int(header["version"]) != 3:
        raise NotImplementedError("Only RINEX v3 is currently supported")

    # Load observations to determine which signals have been observed
    # by the receiver for each gnss system (GPS, Galileo etc)
    obs = georinex.load(filepath)

    first_datetime = obs["time"].min().values
    last_datetime = obs["time"].max().values

    sys_signals = _get_signals_per_system(obs)

    marker_name = header["MARKER NAME"].strip()

    antenna_type, antenna_dh, antenna_de, antenna_dn = _parse_antenna(header)
    approx_x, approx_y, approx_z = _parse_approx_position(header)

    antenna_deltas = AntennaDeltas(height=antenna_dh, north=antenna_dn, east=antenna_de)
    antenna = Antenna(type=antenna_type, deltas=antenna_deltas)
    approx_position = ApproxPosition(x=approx_x, y=approx_y, z=approx_z)

    return RinexHeader(
        marker_name=marker_name,
        antenna=antenna,
        approx_position=approx_position,
        first_obs_time=first_datetime,
        last_obs_time=last_datetime,
        sys_signals=sys_signals,
    )


def _parse_antenna(header: dict) -> Tuple[str, float, float, float]:
    """Parses antenna type and eccentricity (deltas) from header"""
    antenna = header["ANT # / TYPE"]

    # Regular expression to match the string according to the new requirements
    # ^(\S+?)\s{2,}(.*) is the pattern used
    #
    # ^         - Asserts position at the start of the string.
    # (\S+?)    - Captures a group of one or more non-whitespace characters (non-greedy):
    #             \S matches any character that is not a whitespace character.
    #             +? ensures that it matches as few characters as possible, stopping at the first
    #               instance of the subsequent pattern (significant whitespace).
    # \s{2,}    - Matches at least two whitespace characters:
    #             \s matches any whitespace character (spaces, tabs, etc.).
    #             {2,} means two or more times, indicating "significant whitespace" that acts as a delimiter.
    # (.*)      - Captures the rest of the string after the significant whitespace:
    #             . matches any character (except for line terminators).
    #             * means zero or more times, capturing everything till the end of the string.
    #
    # This pattern looks for any initial text (non-greedy), followed by significant whitespace,
    # then captures the rest of the string as the second part.
    match = re.match(r"^(\S+?)\s{2,}(.*)", antenna)

    if match:
        antenna_number = match.group(1)  # Captures the optional first part
        antenna_type = match.group(2).strip()  # Captures the second part after significant whitespace
    else:
        antenna_number = None
        antenna_type = antenna.strip()  # If no match, the whole line is considered as antenna_type

    # These are the eccentricities to put in the station config for Ginan
    antenna_deltas = header["ANTENNA: DELTA H/E/N"].strip()
    antenna_dh, antenna_de, antenna_dn = (float(i) for i in antenna_deltas.split())
    return antenna_type, antenna_dh, antenna_de, antenna_dn


def _parse_approx_position(header: dict) -> (float, float, float):
    """Parses APPROX POSITION XYZ from header

    :returns x, y, z as floats
    """
    # -5015815.7300  2619358.9161 -2933465.3598
    x, y, z = (float(i) for i in header["APPROX POSITION XYZ"].strip().split())
    return x, y, z


def _get_signals_per_system(obs: xarray.Dataset) -> dict:
    """Find all of the signals observed by the receiver for each system,
    where the systems are GPS, GLONASS etc.
    The RINEX header has this information, but it cannot be trusted.
    For example, Victoria submits their CORS data to GA with all signals in the header,
    even if there are no observations associated with a signal.
    Pull it out of the observations themselves.
    """
    # TODO: There is probably cleaner a way to do this with xarray groupby
    signals = {}
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
