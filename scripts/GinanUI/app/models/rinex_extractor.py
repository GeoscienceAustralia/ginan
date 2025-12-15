import re
from datetime import datetime


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
                        pass
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
                    elif label == "TIME OF FIRST OBS":
                        try:
                            y = int(line[0:6]); m = int(line[6:12]); d = int(line[12:18])
                            hh = int(line[18:24]); mm = int(line[24:30]); ss = float(line[30:43])
                            start_epoch = format_time(y, m, d, hh, mm, ss)
                        except Exception:
                            pass
                    elif label == "TIME OF LAST OBS":
                        try:
                            y = int(line[0:6]); m = int(line[6:12]); d = int(line[12:18])
                            hh = int(line[18:24]); mm = int(line[24:30]); ss = float(line[30:43])
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
                mo = int(m.group(2)); dd = int(m.group(3))
                hh = int(m.group(4)); mmn = int(m.group(5)); ssf = float(m.group(6))
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
        }