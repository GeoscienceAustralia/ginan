import re
from datetime import datetime
from pathlib import Path

from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.utils.yaml import load_yaml
from scripts.GinanUI.app.utils.common_dirs import GENERATED_YAML


class RinexExtractor:
    def __init__(self, rinex_path: str):
        self.rinex_path = rinex_path

    def load_rinex_file(self, rinex_path: str):
        self.rinex_path = rinex_path

    def extract_rinex_data(self, rinex_path: str):
        """
        Opens a .RNX file and extracts the corresponding YAML config information

        Supports RINEX v2, v3, and v4 formats

        :param rinex_path: File path for .RNX file to extract from e.g. "resources/input/ALIC.rnx"
        :raises FileNotFoundError if .RNX file is not found
        :raises ValueError if required metadata cannot be extracted
        """

        system_mapping = {
            "G": "GPS",
            "E": "GAL",
            "R": "GLO",
            "C": "BDS",
            "J": "QZS",
        }
        found_constellations = set()

        # Observation types (code_priorities) for each constellation
        # G = GPS, E = GAL, R = GLO, C = BDS, J = QZS
        # Note: S (SBAS) and I (IRNSS) are intentionally omitted at the moment
        obs_types_by_system = {
            "G": [],  # GPS
            "E": [],  # GAL
            "R": [],  # GLO
            "C": [],  # BDS
            "J": [],  # QZS
        }
        current_obs_system = None  # Track the current system for continuation lines

        def format_time(year, month, day, hour, minute, second):
            """
            Helper function to format the parameters into a usable time string for RNX extraction

            :param year: The year
            :param month: The month
            :param day: The day
            :param hour: The hour
            :param minute: The minute
            :param second: The second
            :returns: Formatting string in format: "[YEAR]-[MONTH]-[DAY]_[HOUR]:[MIN}:[SEC]"
                      i.e. "2000-10-27_16:57:49"
            """
            return f"{year:04d}-{month:02d}-{day:02d}_{hour:02d}:{minute:02d}:{(int(second)):02d}"

        def normalize_year_v2(y: int) -> int:
            """
            RINEX v2 sometimes uses 2-digit years in the header/body, sometimes 4-digit.
            - If y >= 1000 (already 4-digit), return as is.
            - Else map YY < 80 -> 2000+YY; YY >= 80 -> 1900+YY.
            """
            if y >= 1000:
                return y
            return 2000 + y if y < 80 else 1900 + y

        def chunk_sat_ids(s: str):
            """
            Given a string like 'G01G02G10R07R08' (no spaces),
            return ['G01','G02','G10','R07','R08'].
            Used in RINEX v2 body parsing.
            """
            s = s.strip()
            out = []
            for i in range(0, len(s), 3):
                chunk = s[i:i+3]
                if len(chunk) == 3 and chunk[0].isalpha():
                    out.append(chunk)
            return out

        def extract_obs_types_v3(line: str, obs_data: str):
            """
            Extract code_priorities codes from a "SYS / # / OBS TYPES" line (RINEX v3/v4).

            :param line: The full line from the RINEX file
            :param obs_data: The data portion of the line (first 60 characters)
            :returns: Tuple of (system_letter, list of obs type codes) or (None, []) for continuation
            """
            # Check if this is a new system line or a continuation line
            first_char = line[0] if line else " "

            # Check if the first letter is within our valid systems
            if first_char in obs_types_by_system:
                # New system line: "G   22 C1C L1C D1C..."
                # System letter is at position 0, count at positions 3-6
                # Extract observation codes starting after the count (position 7 onwards)
                codes_section = obs_data[6:60].strip()
                codes = [c for c in codes_section.split() if len(c) == 3]
                return (first_char, codes)
            elif first_char == " ":
                # Continuation line: "       S2L C5Q L5Q..."
                codes_section = obs_data.strip()
                codes = [c for c in codes_section.split() if len(c) == 3]
                return (None, codes)  # None indicates continuation
            else:
                # Some other system we don't care about (e.g., "S", "I")
                return (first_char, [])

        rinex_version = None
        previous_observation_dt = None
        epoch_interval = None
        in_header = True
        start_epoch = None
        end_epoch = None
        marker_name = None
        receiver_type = None
        antenna_type = None
        antenna_offset = None

        with open(rinex_path, "r", errors="replace") as f:
            lines = f.readlines()

        i = 0
        n = len(lines)

        # ---------- Header ----------
        while i < n:
            line = lines[i]
            i += 1
            label = line[60:].strip() if len(line) >= 61 else ""

            if rinex_version is None and "RINEX VERSION / TYPE" in label:
                try:
                    rinex_version = float(line[0:9].strip())
                except Exception:
                    pass

            if in_header:
                # ----- RINEX v2 header -----
                if rinex_version and rinex_version < 3.0:
                    if label == "# / TYPES OF OBSERV":
                        obs_data = line[0:60]
                        parts = obs_data.split()

                        # Check if first element is the count (numeric)
                        if parts and parts[0].isdigit():
                            # First line - skip the count, extract observation codes
                            v2_obs_types = [p for p in parts[1:] if len(p) == 2]
                        else:
                            # Continuation line - all elements are observation codes
                            v2_obs_types = [p for p in parts if len(p) == 2]

                        # Store raw v2 obs types for later conversion
                        if not hasattr(self, '_v2_obs_types'):
                            self._v2_obs_types = []
                        self._v2_obs_types.extend(v2_obs_types)
                    elif label == "TIME OF FIRST OBS":
                        parts = line.split()
                        if len(parts) >= 6:
                            y_raw, m, d, hh, mm = map(int, parts[:5])
                            ss = float(parts[5])
                            y = normalize_year_v2(y_raw)
                            start_epoch = format_time(y, m, d, hh, mm, ss)
                    elif label == "TIME OF LAST OBS":
                        parts = line.split()
                        if len(parts) >= 6:
                            y_raw, m, d, hh, mm = map(int, parts[:5])
                            ss = float(parts[5])
                            y = normalize_year_v2(y_raw)
                            end_epoch = format_time(y, m, d, hh, mm, ss)
                    elif label == "INTERVAL":
                        try:
                            epoch_interval = int(float(line[0:10].strip()))
                        except Exception:
                            pass
                    elif label == "MARKER NAME":
                        raw_marker = line[0:60].strip()
                        # v2: first 4 chars are the station ID
                        marker_name = raw_marker[:4] if len(raw_marker) >= 4 else raw_marker
                    elif label == "REC # / TYPE / VERS":
                        receiver_type = line[20:40].strip()
                    elif label == "ANT # / TYPE":
                        antenna_type = line[20:40].strip()
                        second_half = line[40:60].strip()
                        if second_half:
                            antenna_type += f" {second_half}"
                    elif label == "ANTENNA: DELTA H/E/N":
                        try:
                            h = float(line[0:14].strip())
                            e = float(line[14:28].strip())
                            nnn = float(line[28:42].strip())
                            antenna_offset = [e, nnn, h]
                        except Exception:
                            pass
                    elif label == "END OF HEADER":
                        in_header = False
                        break
                # ----- RINEX v3/v4 header -----
                else:
                    if label == "SYS / # / OBS TYPES":
                        system_id = line[0] if line else ""
                        if system_id in system_mapping:
                            found_constellations.add(system_mapping[system_id])

                        # Extract observation types for v3/v4
                        obs_data = line[0:60]
                        system_letter, codes = extract_obs_types_v3(line, obs_data)

                        if system_letter is not None:
                            # New system line
                            if system_letter in obs_types_by_system:
                                obs_types_by_system[system_letter].extend(codes)
                                current_obs_system = system_letter
                            else:
                                # System we don't track (e.g., "S", "I"), reset current
                                current_obs_system = None
                        else:
                            # Continuation line - append to the current system
                            if current_obs_system is not None and current_obs_system in obs_types_by_system:
                                obs_types_by_system[current_obs_system].extend(codes)

                    elif label == "TIME OF FIRST OBS":
                        try:
                            y = int(line[0:6])
                            m = int(line[6:12])
                            d = int(line[12:18])
                            hh = int(line[18:24])
                            mm = int(line[24:30])
                            ss = float(line[30:43])
                            start_epoch = format_time(y, m, d, hh, mm, ss)
                        except Exception:
                            pass
                    elif label == "TIME OF LAST OBS":
                        try:
                            y = int(line[0:6])
                            m = int(line[6:12])
                            d = int(line[12:18])
                            hh = int(line[18:24])
                            mm = int(line[24:30])
                            ss = float(line[30:43])
                            end_epoch = format_time(y, m, d, hh, mm, ss)
                        except Exception:
                            pass
                    elif label == "INTERVAL":
                        try:
                            epoch_interval = int(float(line[0:10]))
                        except Exception:
                            pass
                    elif label == "MARKER NAME":
                        marker_name = line[0:60].strip()
                    elif label == "REC # / TYPE / VERS":
                        receiver_type = line[20:40].strip()
                    elif label == "ANT # / TYPE":
                        antenna_type = line[20:40].strip()
                        second_half = line[40:60].strip()
                        if second_half:
                            antenna_type += f" {second_half}"
                    elif label == "ANTENNA: DELTA H/E/N":
                        try:
                            h = float(line[0:14].strip())
                            e = float(line[14:28].strip())
                            nnn = float(line[28:42].strip())
                            antenna_offset = [e, nnn, h]
                        except Exception:
                            pass
                    elif label == "END OF HEADER":
                        in_header = False
                        break
            else:
                break  # safety

        if rinex_version is None:
            raise ValueError("Could not determine RINEX version.")

        # Detector for a v2 epoch start (now supports 2 or 4 digit years)
        epoch_v2_head_re = re.compile(
            r'^\s*\d{2,4}\s+\d{1,2}\s+\d{1,2}\s+\d{1,2}\s+\d{1,2}\s+[0-9.]'
        )

        if rinex_version < 3.0:
            # ---------- RINEX v2 body ----------
            # YY or YYYY MM DD hh mm ss.sssssss FLAG NSAT [SATLIST...]
            epoch_re = re.compile(
                r'^\s*(\d{2,4})\s+(\d{1,2})\s+(\d{1,2})\s+(\d{1,2})\s+(\d{1,2})\s+([0-9.]+)\s+(\d)\s*(\d+)?(.*)$'
            )

            while i < n:
                line = lines[i]
                m = epoch_re.match(line.rstrip("\n"))
                if not m:
                    i += 1
                    continue

                y_raw = int(m.group(1))
                mo = int(m.group(2))
                dd = int(m.group(3))
                hh = int(m.group(4))
                mmn = int(m.group(5))
                ssf = float(m.group(6))
                flag = int(m.group(7))  # currently unused
                nsat = m.group(8)
                nsat = int(nsat) if nsat is not None else 0
                rest = m.group(9) or ""

                year = normalize_year_v2(y_raw)

                # Epoch interval from first two epochs
                if previous_observation_dt and epoch_interval is None:
                    t1 = datetime(*previous_observation_dt)
                    t2 = datetime(year, mo, dd, hh, mmn, int(ssf))
                    epoch_interval = int((t2 - t1).total_seconds())

                end_epoch = format_time(year, mo, dd, hh, mmn, ssf)
                if start_epoch is None:
                    # Header didn't contain TIME OF FIRST OBS
                    start_epoch = end_epoch

                previous_observation_dt = (year, mo, dd, hh, mmn, int(ssf))

                # Satellites from this line + continuation lines
                sats = chunk_sat_ids(rest)

                # Continuations: until we have nsat satellites or hit next epoch
                j = i + 1
                while len(sats) < nsat and j < n:
                    nxt = lines[j].rstrip("\n")
                    if epoch_v2_head_re.match(nxt):
                        break  # next epoch encountered
                    sats.extend(chunk_sat_ids(nxt))
                    j += 1

                for sid in sats[:nsat]:
                    sys = sid[0]
                    if sys in system_mapping:
                        found_constellations.add(system_mapping[sys])

                i = j

        else:
            # ---------- RINEX v3/v4 body ----------
            while i < n:
                line = lines[i]
                i += 1
                if not line.startswith(">"):
                    continue

                parts = line[1:].split()
                if len(parts) < 6:
                    continue

                y, mo, dd, hh, mmn = map(int, parts[:5])
                ssf = float(parts[5])

                if previous_observation_dt and epoch_interval is None:
                    t1 = datetime(*previous_observation_dt)
                    t2 = datetime(y, mo, dd, hh, mmn, int(ssf))
                    epoch_interval = int((t2 - t1).total_seconds())

                end_epoch = format_time(y, mo, dd, hh, mmn, ssf)
                if start_epoch is None:
                    start_epoch = end_epoch

                previous_observation_dt = (y, mo, dd, hh, mmn, int(ssf))

                sats = []
                if len(parts) > 8:
                    sats.extend(parts[8:])

                j = i
                while j < n and not lines[j].startswith(">"):
                    extra = lines[j].strip().split()
                    if extra and not extra[0][0].isalpha():
                        break
                    for token in extra:
                        if token and token[0] in system_mapping and len(token) >= 2:
                            sats.append(token)
                    j += 1

                for sid in sats:
                    sys = sid[0]
                    if sys in system_mapping:
                        found_constellations.add(system_mapping[sys])

                i = j

        # ---------- Safety checks ----------
        if not start_epoch:
            raise ValueError("TIME OF FIRST OBS not found (header or body)")
        if not end_epoch:
            raise ValueError("TIME OF LAST OBS or last observation not found")
        if epoch_interval is None:
            raise ValueError("Epoch interval could not be determined")

        # ---------- RINEX v2 observation type conversion ----------
        # Convert v2 obs types (C1, C2, P1, P2) to v3 codes using YAML config mappings
        if rinex_version and rinex_version < 3.0:
            v2_obs_types = getattr(self, '_v2_obs_types', [])
            if v2_obs_types:
                Logger.console(f"=== RINEX v2 Observation Types Found: {v2_obs_types} ===")

                # Filter to only the codes we care about: P1, P2, C1, C2
                # Priority order: P1 (highest), P2, C1, C2 (lowest)
                v2_priority_order = ['P1', 'P2', 'C1', 'C2']
                v2_codes_present = [code for code in v2_priority_order if code in v2_obs_types]

                Logger.console(f"=== Filtered v2 codes (priority order): {v2_codes_present} ===")

                # Load rinex2 code conversions from YAML config
                try:
                    template_config = load_yaml(GENERATED_YAML)
                    receiver_options = template_config.get('receiver_options', {}).get('global', {})

                    # Map system letters to constellation names used in YAML
                    system_to_yaml_name = {
                        'G': 'gps',
                        'R': 'glo',
                    }

                    # For each constellation found in the RINEX file, convert v2 codes to v3
                    for sys_letter, const_name in system_to_yaml_name.items():
                        # Check if this constellation has rinex2 conversions defined
                        const_config = receiver_options.get(const_name, {})
                        rinex2_config = const_config.get('rinex2', {})
                        code_conversions = rinex2_config.get('rnx_code_conversions', {})

                        if code_conversions:
                            # Convert v2 codes to v3 codes in priority order
                            v3_codes = []
                            for v2_code in v2_codes_present:
                                if v2_code in code_conversions:
                                    v3_code = str(code_conversions[v2_code])
                                    v3_codes.append(v3_code)

                            if v3_codes:
                                obs_types_by_system[sys_letter] = v3_codes

                except Exception as e:
                    Logger.console(f"⚠️ Could not load rinex2 code conversions from config: {e}")

            # Clean up temporary attribute
            if hasattr(self, '_v2_obs_types'):
                delattr(self, '_v2_obs_types')

        # Cull observation types to only L-codes (converting C to L)
        def cull_observation_codes(obs_list):
            """
            Cull observation codes to only carrier phase (L) codes.
            Rules:
            - Only keep codes starting with 'C' or 'L' (ignore D, S, etc.)
            - Convert 'C' codes to 'L' codes (C1C -> L1C)
            - Remove duplicates (if both C1C and L1C exist, keep only L1C)

            Example: ['C1C', 'L1C', 'D1C', 'S1C', 'C1W'] -> ['L1C', 'L1W']
            """
            # Filter to only C and L codes
            filtered = [code for code in obs_list if code and code[0] in ('C', 'L')]

            # Convert all to L-codes and track unique codes
            l_codes = set()
            for code in filtered:
                if code[0] == 'C':
                    # Convert C to L (e.g., C1C -> L1C)
                    l_code = 'L' + code[1:]
                    l_codes.add(l_code)
                else:
                    # Already an L code
                    l_codes.add(code)

            # Return sorted list to maintain consistent order
            return sorted(list(l_codes))

        # Apply culling to all observation types (only for v3/v4 files)
        # For v2 files, codes are already converted to L-codes with correct priority order
        if not (rinex_version and rinex_version < 3.0):
            obs_types_by_system['G'] = cull_observation_codes(obs_types_by_system['G'])
            obs_types_by_system['E'] = cull_observation_codes(obs_types_by_system['E'])
            obs_types_by_system['R'] = cull_observation_codes(obs_types_by_system['R'])
            obs_types_by_system['C'] = cull_observation_codes(obs_types_by_system['C'])
            obs_types_by_system['J'] = cull_observation_codes(obs_types_by_system['J'])

        # Load default priority order from template config
        def load_default_priorities():
            """
            Load the default code_priorities from the template YAML config.

            Returns:
              dict: Mapping of constellation codes to their priority lists
                    e.g., {'G': ['L1W', 'L1C', ...], 'E': ['L1C', 'L1X', ...]}
            """
            try:
                template_config = load_yaml(GENERATED_YAML)
                sys_options = template_config.get('processing_options', {}).get('gnss_general', {}).get('sys_options',
                                                                                                        {})

                # Map constellation names to system letters
                const_map = {
                    'gps': 'G',
                    'gal': 'E',
                    'glo': 'R',
                    'bds': 'C',
                    'qzs': 'J'
                }

                priorities = {}
                for const_name, sys_letter in const_map.items():
                    if const_name in sys_options:
                        code_priorities = sys_options[const_name].get('code_priorities', [])
                        priorities[sys_letter] = code_priorities
                    else:
                        priorities[sys_letter] = []

                return priorities
            except Exception as e:
                Logger.console(f"⚠️ Could not load default priorities from template: {e}")
                # Return empty priorities if template can't be loaded
                return {'G': [], 'E': [], 'R': [], 'C': [], 'J': []}

        def reorder_by_priority(rinex_codes, priority_order):
            """
            Reorder RINEX codes based on template priority order.

            Rules:
            1. Codes that appear in priority_order are placed first, in that order (enabled by default)
            2. Codes from RINEX that aren't in priority_order are appended at the end (disabled by default)

            Args:
              rinex_codes: List of codes from RINEX (after culling)
              priority_order: Preferred order from template config

            Returns:
              tuple: (ordered_codes, enabled_set)
                - ordered_codes: List of codes in priority order
                - enabled_set: Set of codes that should be enabled by default

            Example:
              rinex_codes = ['L1C', 'L2W', 'L5Q', 'L1L', 'L2L']
              priority_order = ['L1W', 'L1C', 'L2W', 'L5Q']
              result = (['L1C', 'L2W', 'L5Q', 'L1L', 'L2L'], {'L1C', 'L2W', 'L5Q'})
                        ^ in priority order ^  ^ extras ^      ^ only priority codes enabled ^
            """
            if not priority_order:
                # If no priority order defined, enable all codes
                return rinex_codes, set(rinex_codes)

            # Convert to sets for efficient lookup
            rinex_set = set(rinex_codes)
            priority_set = set(priority_order)

            # Start with codes from priority list that exist in RINEX (these are enabled)
            ordered_codes = [code for code in priority_order if code in rinex_set]
            enabled_codes = set(ordered_codes)

            # Append codes from RINEX that aren't in priority list (these are disabled)
            extra_codes = sorted([code for code in rinex_codes if code not in priority_set])

            return ordered_codes + extra_codes, enabled_codes

        # Load default priorities
        default_priorities = load_default_priorities()

        # Reorder observation types based on template priorities and track which should be enabled
        # For v2 files, skip reordering since codes are already in correct priority order from conversion
        if rinex_version and rinex_version < 3.0:
            # For v2 files, all converted codes are enabled and already in priority order
            enabled_gps = set(obs_types_by_system['G'])
            enabled_gal = set(obs_types_by_system['E'])
            enabled_glo = set(obs_types_by_system['R'])
            enabled_bds = set(obs_types_by_system['C'])
            enabled_qzs = set(obs_types_by_system['J'])
        else:
            # For v3/v4 files, use the existing reordering logic
            obs_types_by_system['G'], enabled_gps = reorder_by_priority(obs_types_by_system['G'], default_priorities.get('G', []))
            obs_types_by_system['E'], enabled_gal = reorder_by_priority(obs_types_by_system['E'], default_priorities.get('E', []))
            obs_types_by_system['R'], enabled_glo = reorder_by_priority(obs_types_by_system['R'], default_priorities.get('R', []))
            obs_types_by_system['C'], enabled_bds = reorder_by_priority(obs_types_by_system['C'], default_priorities.get('C', []))
            obs_types_by_system['J'], enabled_qzs = reorder_by_priority(obs_types_by_system['J'], default_priorities.get('J', []))

        # Log extracted observation types for verification
        Logger.console("=== Extracted Observation Types (Code Priorities) ===")
        Logger.console(f"GPS (G): {obs_types_by_system['G']}")
        Logger.console(f"GAL (E): {obs_types_by_system['E']}")
        Logger.console(f"GLO (R): {obs_types_by_system['R']}")
        Logger.console(f"BDS (C): {obs_types_by_system['C']}")
        Logger.console(f"QZS (J): {obs_types_by_system['J']}")
        Logger.console("======================================================")

        return {
            "rinex_version": rinex_version,
            "start_epoch": start_epoch,
            "end_epoch": end_epoch,
            "epoch_interval": epoch_interval,
            "marker_name": marker_name,
            "receiver_type": receiver_type,
            "antenna_type": antenna_type,
            "antenna_offset": antenna_offset,
            "constellations": ", ".join(sorted(found_constellations)) if found_constellations else "Unknown",
            "obs_types_gps": obs_types_by_system["G"],
            "obs_types_gal": obs_types_by_system["E"],
            "obs_types_glo": obs_types_by_system["R"],
            "obs_types_bds": obs_types_by_system["C"],
            "obs_types_qzs": obs_types_by_system["J"],
            "enabled_gps": enabled_gps,
            "enabled_gal": enabled_gal,
            "enabled_glo": enabled_glo,
            "enabled_bds": enabled_bds,
            "enabled_qzs": enabled_qzs,
        }