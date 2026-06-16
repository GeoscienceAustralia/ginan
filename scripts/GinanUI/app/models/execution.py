"""
PEA execution model for Ginan-UI.

Manages the lifecycle of a Ginan PEA processing run: locating the PEA binary
(bundled or on PATH), building and writing the YAML config from user inputs,
launching the PEA subprocess, streaming its output, and generating post-run
visualisations. Also exposes the INPUT_PRODUCTS_PATH and GENERATED_YAML
constants re-exported from common_dirs for use by other modules.
"""

import os
import platform
import shutil
import signal
import subprocess
import threading
import time
from pathlib import Path
from ruamel.yaml.comments import CommentedMap, CommentedSeq
from ruamel.yaml.scalarstring import PlainScalarString
from scripts.GinanUI.app.utils.common_dirs import GENERATED_YAML, INPUT_PRODUCTS_PATH, TABLES_PRODUCTS_PATH, TEMPLATE_PATH
from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.utils.yaml import load_yaml, normalise_yaml_value, write_yaml

# Imports external of Ginan-UI entirely
from scripts.plot_pos import plot_pos_files
from scripts.plot_trace_res import plot_trace_res_files

def get_pea_exec():
    """
    Checks system platform and returns a Path to the respective executable. Also searches for "pea" on PATH.

    :return: Path to executable or str of PATH callable
    :raises RuntimeError: If PEA binary cannot be found
    """
    import sys

    # 1. Check if running in PyInstaller bundle
    if getattr(sys, 'frozen', False):
        # Running in bundled mode
        base_path = Path(sys._MEIPASS)

        # On macOS .app bundles, binaries are in Resources/bin/
        if platform.system().lower() == "darwin":
            # Try Resources/bin first (macOS .app structure)
            pea_path = base_path.parent / "Resources" / "bin" / "pea"
            if pea_path.exists():
                print(f"[Execution] Found bundled PEA binary at: {pea_path}")
                return pea_path
            # Fallback to _internal/bin
            pea_path = base_path / "bin" / "pea"
            if pea_path.exists():
                print(f"[Execution] Found bundled PEA binary at: {pea_path}")
                return pea_path

        # Linux/Windows: binaries in _internal/bin
        else:
            # Windows uses .exe extension
            exe_name = "pea.exe" if platform.system().lower() == "windows" else "pea"
            pea_path = base_path / "bin" / exe_name
            if pea_path.exists():
                return pea_path

        print(f"[Execution] Bundled binary not found in expected locations")
        # Fall through to try other methods

    # 2. Check if 'pea' is on PATH (most reliable if user has configured their environment)
    if shutil.which("pea"):
        executable = "pea"
        Logger.console(f"✅ Found PEA on PATH: {shutil.which('pea')}")
        return executable

    # 3. Try to find PEA relative to this script's location
    # Current file: ginan/scripts/GinanUI/app/models/execution.py
    # Target file:  ginan/bin/pea
    try:
        current_file = Path(__file__).resolve()
        # Navigate from: "ginan/scripts/GinanUI/app/models/execution.py" to "ginan/"
        ginan_root = current_file.parents[4]  # Go up: models -> app -> GinanUI -> scripts -> ginan

        # Check for the binary in ginan/bin/pea
        pea_binary = ginan_root / "bin" / "pea"

        if pea_binary.exists() and pea_binary.is_file():
            # Make sure it's executable (permissions are set up right)
            if not os.access(pea_binary, os.X_OK):
                Logger.console(f"✅ Found PEA at {pea_binary} but it's not executable. Attempting to fix...")
                try:
                    pea_binary.chmod(pea_binary.stat().st_mode | 0o111)  # Add "execute" permissions
                    Logger.console(f"✅ Made PEA executable")
                except Exception as e:
                    Logger.console(f"⚠️ Could not make PEA executable: {e}")
                    raise RuntimeError(f"⚠️ PEA binary found at {pea_binary} but is not executable and cannot be fixed")

            Logger.console(f"✅ Found PEA binary at: {pea_binary}")
            return pea_binary
        else:
            Logger.console(f"⚠️ Expected PEA binary at {pea_binary} but not found")

    except Exception as e:
        Logger.console(f"⚠️ Error while searching for PEA relative to script location: {e}")

    # 4. Platform-specific fallbacks (optional - can be removed if not needed)
    system = platform.system().lower()

    if system == "windows":
        # Windows may have pea.exe set up
        if shutil.which("pea.exe"):
            executable = "pea.exe"
            Logger.console(f"✅ Found pea.exe on PATH: {shutil.which('pea.exe')}")
            return executable
        raise RuntimeError(
            "PEA executable not found. Please:\n"
            "1. Build the PEA binary (see ginan build instructions)\n"
            "2. Add ginan/bin to your PATH, or\n"
            "3. Run from within the ginan directory structure"
        )

    # 5. If nothing found, provide a helpful error message
    raise RuntimeError(
        f"PEA executable not found. Please ensure:\n"
        f"1. You have built the PEA binary (should be at ginan/bin/pea)\n"
        f"2. You are running GinanUI from within the ginan directory structure, or\n"
        f"3. The 'pea' executable is available on your system PATH\n"
        f"\nSearched locations:\n"
        f"  - System PATH\n"
        f"  - {ginan_root / 'bin' / 'pea' if 'ginan_root' in locals() else 'Could not determine ginan root'}"
    )

def get_interpolate_loading_exec():
    """
    Locate the interpolate_loading binary using the same search strategy as get_pea_exec().

    :return: Path to executable or str of PATH callable
    :raises RuntimeError: If interpolate_loading binary cannot be found
    """
    import sys

    binary_name = "interpolate_loading"

    # 1. Check if running in PyInstaller bundle
    if getattr(sys, 'frozen', False):
        base_path = Path(sys._MEIPASS)

        if platform.system().lower() == "darwin":
            pea_path = base_path.parent / "Resources" / "bin" / binary_name
            if pea_path.exists():
                return pea_path
            pea_path = base_path / "bin" / binary_name
            if pea_path.exists():
                return pea_path
        else:
            exe_name = f"{binary_name}.exe" if platform.system().lower() == "windows" else binary_name
            pea_path = base_path / "bin" / exe_name
            if pea_path.exists():
                return pea_path

    # 2. Check if binary is on PATH
    if shutil.which(binary_name):
        Logger.console(f"✅ Found {binary_name} on PATH: {shutil.which(binary_name)}")
        return binary_name

    # 3. Try to find binary relative to this script's location
    try:
        current_file = Path(__file__).resolve()
        ginan_root = current_file.parents[4]
        loading_binary = ginan_root / "bin" / binary_name

        if loading_binary.exists() and loading_binary.is_file():
            if not os.access(loading_binary, os.X_OK):
                try:
                    loading_binary.chmod(loading_binary.stat().st_mode | 0o111)
                except Exception as e:
                    raise RuntimeError(f"⚠️ {binary_name} found at {loading_binary} but is not executable: {e}")

            Logger.console(f"✅ Found {binary_name} binary at: {loading_binary}")
            return loading_binary

    except Exception as e:
        Logger.console(f"⚠️ Error while searching for {binary_name} relative to script location: {e}")

    raise RuntimeError(
        f"{binary_name} executable not found. Please ensure:\n"
        f"1. You have built the Ginan binaries (should be at ginan/bin/{binary_name})\n"
        f"2. You are running GinanUI from within the ginan directory structure, or\n"
        f"3. The '{binary_name}' executable is available on your system PATH"
    )

class Execution:
    def __init__(self, config_path: Path = GENERATED_YAML):
        """
        Caches config changes, interacts with config file, and finally can call pea executable.

        :param config_path: Path to a config file, defaulted to GENERATED_YAML
        """
        self.config_path = config_path
        self.executable = get_pea_exec()  # the PEA executable
        self.changes = False # Flag to track if config has been changed
        self.yaml_overwrite = True # Whether UI changes should be written to the YAML file
        self._procs = []
        self._stop_event = threading.Event()

        template_file = Path(TEMPLATE_PATH)

        if config_path.exists():
            Logger.console(f"Using existing config file: {config_path}")
        else:
            Logger.console(f"Existing config not found, copying default template: {template_file} → {config_path}")
            try:
                config_path.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy(template_file, config_path)
            except Exception as e:
                raise RuntimeError(f"❌ Failed to copy default config: {e}")

        self.config = load_yaml(config_path)

    #region YAML Config Manipulation

    def reload_config(self):
        """
        Force reload of the YAML config from disk into memory.
        This allows any manual edits to be picked up before GUI changes are applied.

        :raises RuntimeError: Any error occurred during load_yaml(config_path)
        """
        try:
            self.config = load_yaml(self.config_path)
        except Exception as e:
            raise RuntimeError(f"❌ Failed to reload config from {self.config_path}: {e}")

    def reset_config(self):
        """
        Delete the generated yaml config file and regenerate it from the template.
        This restores the config to its default state.

        :raises RuntimeError: If the reset operation fails
        """
        template_file = Path(TEMPLATE_PATH)

        try:
            # Delete the existing generated yaml config if it exists
            if self.config_path.exists():
                self.config_path.unlink()
                Logger.console(f"🗑️ Deleted existing config: {self.config_path}")

            # Copy fresh template to generated config location
            self.config_path.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy(template_file, self.config_path)
            Logger.console(f"📄 Regenerated config from template: {template_file} → {self.config_path}")

            # Reload the fresh config into memory
            self.config = load_yaml(self.config_path)
            self.changes = False

        except Exception as e:
            raise RuntimeError(f"❌ Failed to reset config: {e}")

    def edit_config(self, key_path: str, value, add_field=False):
        """
        Edits the cached config while preserving YAML formatting and comments.

        :param key_path: Dot-separated YAML key path (e.g., "inputs.gnss_observations.rnx_inputs")
        :param value: New value to assign (will be converted to ruamel-safe types)
        :param add_field: Whether to add the field if it doesn't exist
        :raises KeyError if path doesn't exist and add_field is False
        """
        self.changes = True  # Mark config as changed
        keys = key_path.split(".")
        node = self.config

        for key in keys[:-1]:
            if key not in node:
                if add_field:
                    node[key] = CommentedMap()
                else:
                    raise KeyError(f"Key '{key}' not found in {node}")
            node = node[key]

        final_key = keys[-1]
        value = normalise_yaml_value(value)

        # Preserve any existing comment on the final_key
        if final_key in node:
            old_value = node[final_key]
            if hasattr(old_value, 'ca') and not hasattr(value, 'ca'):
                value.ca = old_value.ca

        if not add_field and final_key not in node:
            raise KeyError(f"Key '{final_key}' not found in {key_path}")

        node[final_key] = value

    def set_loading_params(self, marker_name: str, marker_number: str, apriori_position: list):
        """
        Store loading BLQ parameters without applying the full UI config
        Used when YAML overwrite is disabled so that ensure_loading_blq()
        can still generate BLQ files if needed

        :param marker_name: 4-character station marker name (e.g. 'ALIC')
        :param marker_number: DOMES marker number (e.g. '50137M0014') or None
        :param apriori_position: [X, Y, Z] ECEF coordinates in metres
        """
        self._loading_marker_name = marker_name
        self._loading_marker_number = marker_number
        self._loading_apriori_position = apriori_position

    def apply_ui_config(self, inputs):
        """
        Applies UI settings to **cached** config. **Call write_cached_changes()** to write them.

        :param inputs:
        """
        self.changes = True

        # 1. Set core inputs / outputs
        self.edit_config("inputs.inputs_root", str(INPUT_PRODUCTS_PATH) + "/", False)

        # Extract directory and filename from RINEX path
        rnx_path = Path(inputs.rnx_path)
        rnx_directory = str(rnx_path.parent)
        rnx_filename = rnx_path.name

        # Set gnss_observations_root to the directory containing the RINEX file
        self.edit_config("inputs.gnss_observations.gnss_observations_root", rnx_directory, False)

        # Use only the filename (relative path) for rnx_inputs
        rnx_val = normalise_yaml_value(rnx_filename)

        # 1a. Set rnx_inputs safely, preserving formatting
        try:
            existing = self.config["inputs"]["gnss_observations"].get("rnx_inputs")
            if isinstance(existing, CommentedSeq):
                existing.clear()
                existing.append(rnx_val)
                existing.fa.set_block_style()
            else:
                new_seq = CommentedSeq([rnx_val])
                new_seq.fa.set_block_style()
                self.config["inputs"]["gnss_observations"]["rnx_inputs"] = new_seq
        except Exception as e:
            Logger.console(f"[apply_ui_config] Error setting rnx_inputs: {e}")

        # Normalise outputs_root
        out_val = normalise_yaml_value(inputs.output_path)
        self.edit_config("outputs.outputs_root", out_val, False)

        # Output toggles from UI
        self.edit_config("outputs.gpx.output", bool(inputs.gpx_output), True)
        self.edit_config("outputs.pos.output", bool(inputs.pos_output), True)
        self.edit_config("outputs.trace.output_network", bool(inputs.trace_output_network), True)
        self.edit_config("outputs.sinex.output", bool(inputs.snx_output), True)

        # 2. Replace 'TEST' receiver block with real marker name
        if "TEST" in self.config.get("receiver_options", {}):
            self.config["receiver_options"][inputs.marker_name] = self.config["receiver_options"].pop("TEST")

        # 3. Include UI-extracted values
        self.edit_config("processing_options.epoch_control.start_epoch", PlainScalarString(inputs.start_epoch), False)
        self.edit_config("processing_options.epoch_control.end_epoch", PlainScalarString(inputs.end_epoch), False)
        epoch_interval = inputs.epoch_interval
        epoch_tolerance = min(0.5, inputs.rinex_epoch_interval / 2)
        self.edit_config("processing_options.epoch_control.epoch_interval", int(epoch_interval) if epoch_interval == int(epoch_interval) else float(epoch_interval), False)
        self.edit_config("processing_options.epoch_control.epoch_tolerance", int(epoch_tolerance) if epoch_tolerance == int(epoch_tolerance) else float(epoch_tolerance), True)
        self.edit_config(f"receiver_options.{inputs.marker_name}.receiver_type", inputs.receiver_type, True)
        self.edit_config(f"receiver_options.{inputs.marker_name}.antenna_type", inputs.antenna_type, True)

        # Handle apriori_position: remove if all zeros, add if non-zero
        receiver_node = self.config.get("receiver_options", {}).get(inputs.marker_name, {})

        if all(v == 0.0 for v in inputs.apriori_position):
            # Remove apriori_position if it exists and is all zeros
            if "apriori_position" in receiver_node:
                del receiver_node["apriori_position"]
        else:
            # Add / update apriori_position if non-zero
            self.edit_config(f"receiver_options.{inputs.marker_name}.apriori_position", inputs.apriori_position, True)

        self.edit_config(f"receiver_options.{inputs.marker_name}.models.eccentricity.offset", inputs.antenna_offset,
                         True)

        # Always format process_noise as a list
        self.edit_config("estimation_parameters.receivers.global.pos.process_noise", [inputs.mode], False)

        # 4. GNSS constellation toggles
        all_constellations = ["gps", "gal", "glo", "bds", "qzs"]
        for const in all_constellations:
            self.edit_config(f"processing_options.gnss_general.sys_options.{const}.process", False, False)

        # Then enable only the selected constellations
        if inputs.constellations_raw:
            selected = [c.strip().lower() for c in inputs.constellations_raw.split(",") if c.strip()]
            for const in selected:
                if const in all_constellations:
                    self.edit_config(f"processing_options.gnss_general.sys_options.{const}.process", True, False)

        # 5. Handle observation code priorities for each constellation
        obs_code_map = {
            'gps': getattr(inputs, 'gps_codes', []),
            'gal': getattr(inputs, 'gal_codes', []),
            'glo': getattr(inputs, 'glo_codes', []),
            'bds': getattr(inputs, 'bds_codes', []),
            'qzs': getattr(inputs, 'qzs_codes', [])
        }

        for const, codes in obs_code_map.items():
            if const in all_constellations:
                if codes and len(codes) > 0:
                    # Convert codes list to a yaml compatible format
                    code_seq = CommentedSeq(codes)
                    code_seq.fa.set_flow_style()
                    self.edit_config(f"processing_options.gnss_general.sys_options.{const}.code_priorities", code_seq, False)
                else:
                    empty_seq = CommentedSeq([])
                    empty_seq.fa.set_flow_style()
                    self.edit_config(f"processing_options.gnss_general.sys_options.{const}.code_priorities", empty_seq,False)

        # 6. Add SINEX file to config if available
        sinex_filename = getattr(inputs, 'sinex_filename', None)
        if sinex_filename:
            self._add_sinex_to_config(sinex_filename)

        # 7. Store loading BLQ parameters for ensure_loading_blq() during execute_config()
        self._loading_marker_name = inputs.marker_name
        self._loading_marker_number = getattr(inputs, 'marker_number', None)
        self._loading_apriori_position = inputs.apriori_position

    def _add_sinex_to_config(self, sinex_filename: str):
        """
        Append the SINEX filename to the config's inputs.snx_files list.

        Does NOT overwrite existing entries - only appends if not already present.
        Removes any old IGS CRD SINEX files (IGS*_CRD.SNX pattern) before adding new one.

        :param sinex_filename: Name of the SINEX file (e.g., "IGS0OPSSNX_20250310000_01D_01D_CRD.SNX")
        """
        import re

        try:
            # Ensure inputs section exists
            if "inputs" not in self.config:
                self.config["inputs"] = CommentedMap()

            # Get or create snx_files list
            existing = self.config["inputs"].get("snx_files")

            if existing is None:
                # Create new list with the SINEX file
                new_seq = CommentedSeq([normalise_yaml_value(sinex_filename)])
                new_seq.fa.set_block_style()
                self.config["inputs"]["snx_files"] = new_seq
            elif isinstance(existing, CommentedSeq):
                # Remove any old IGS CRD SINEX files (pattern: IGS*SNX_*_CRD.SNX)
                # Keep other entries like igs_satellite_metadata.snx or tables/*.snx
                igs_crd_pattern = re.compile(r'^IGS.*SNX_.*_CRD\.SNX$', re.IGNORECASE)

                # Filter out old IGS CRD files
                filtered = [item for item in existing if not igs_crd_pattern.match(str(item))]

                # Check if new file is already present
                if sinex_filename not in filtered:
                    filtered.append(normalise_yaml_value(sinex_filename))

                # Update the list
                existing.clear()
                for item in filtered:
                    existing.append(normalise_yaml_value(item) if not isinstance(item, PlainScalarString) else item)
                existing.fa.set_block_style()
            else:
                # Convert to list if it's a single value
                old_value = str(existing)
                new_seq = CommentedSeq()

                # Keep old value if it's not an IGS CRD file
                igs_crd_pattern = re.compile(r'^IGS.*SNX_.*_CRD\.SNX$', re.IGNORECASE)
                if not igs_crd_pattern.match(old_value):
                    new_seq.append(normalise_yaml_value(old_value))

                # Add new SINEX file
                new_seq.append(normalise_yaml_value(sinex_filename))
                new_seq.fa.set_block_style()
                self.config["inputs"]["snx_files"] = new_seq

        except Exception as e:
            Logger.workflow(f"⚠️ Failed to write SINEX to config: {e}")

    def _station_in_blq(self, blq_path: Path, marker_name: str) -> bool:
        """
        Check whether a station (by 4-character marker name) already has an entry in a BLQ file.

        BLQ station entries start with two leading spaces followed by the
        4-character marker name.  The marker may be followed by a space and
        DOMES number (e.g. "  ALIC 50137M001"), trailing whitespace only
        (e.g. "  AGGO      "), or nothing before the newline (e.g. "  AGGO").
        Comment lines starting with "$$" are ignored.

        :param blq_path: Path to the BLQ file
        :param marker_name: 4-character station marker name (e.g. 'ALIC')
        :return: True if the station is found in the BLQ file
        """
        if not blq_path.exists():
            return False

        upper_marker = marker_name.upper()
        # Match "  XXXX" at start of line: 2 spaces + exact 4-char code,
        # then verify the next character (if any) is whitespace to avoid
        # false positives like "ALIC2"
        entry_prefix = f"  {upper_marker}"
        prefix_len = len(entry_prefix)
        try:
            with blq_path.open('r', encoding='utf-8', errors='replace') as f:
                for line in f:
                    if line.startswith("$$"):
                        continue
                    if line.upper().startswith(entry_prefix):
                        # Ensure the character after the marker (if present) is whitespace
                        if len(line) <= prefix_len or line[prefix_len].isspace():
                            return True
        except Exception as e:
            Logger.console(f"⚠️ Error reading BLQ file {blq_path}: {e}")

        return False

    def _station_in_configured_blq_files(self, config_key: str, marker_name: str) -> bool:
        """
        Check whether a station is present in any of the non-wildcard BLQ files
        listed under the given YAML config key.

        Only checks concrete filenames from the config (not wildcard patterns like
        '*_ocean.BLQ'), resolved relative to inputs_root. This ensures we only
        validate against files that PEA will actually read.

        :param config_key: Dot-separated YAML key (e.g. 'inputs.tides.ocean_tide_loading_blq_files')
        :param marker_name: 4-character station marker name (e.g. 'ALIC')
        :return: True if the station is found in any configured non-wildcard BLQ file
        """
        try:
            keys = config_key.split(".")
            node = self.config
            for k in keys:
                node = node[k]
        except (KeyError, TypeError):
            return False

        if not isinstance(node, (list, CommentedSeq)):
            return False

        # Resolve inputs_root for building absolute paths
        inputs_root = self.config.get("inputs", {}).get("inputs_root", "")
        inputs_root = Path(str(inputs_root)) if inputs_root else INPUT_PRODUCTS_PATH

        for entry in node:
            entry_str = str(entry).strip()
            # Skip wildcard patterns - those will pick up generated files automatically
            if "*" in entry_str or "?" in entry_str:
                continue

            # Resolve the BLQ file path relative to inputs_root
            blq_path = Path(inputs_root) / entry_str
            if self._station_in_blq(blq_path, marker_name):
                return True

        return False

    def ensure_loading_blq(self, marker_name: str, marker_number: str,
                           apriori_position: list, progress_callback=None, stop_requested=None):
        """
        Ensure ocean and atmospheric tide loading BLQ files exist for the given station.

        Reads the BLQ file lists from the YAML config and checks only the
        non-wildcard entries that PEA will actually use. If the station is not
        found, downloads the loading grid netCDF files (if needed) and runs
        interpolate_loading to generate station-specific BLQ files.

        :param marker_name: 4-character station marker name (e.g. 'ALIC')
        :param marker_number: DOMES marker number (e.g. '50137M0014') or None
        :param apriori_position: [X, Y, Z] ECEF coordinates in metres
        :param progress_callback: Optional (description, percent) callback
        :param stop_requested: Optional bool callback for cancellation
        :raises RuntimeError: If interpolate_loading fails
        """
        from scripts.GinanUI.app.models.dl_products import download_loading_grids

        if not marker_name or not apriori_position or all(v == 0.0 for v in apriori_position):
            Logger.workflow("⚠️ Missing marker name or apriori position - skipping loading BLQ generation")
            return

        # Check BLQ files that are actually referenced in the YAML config
        ocean_present = self._station_in_configured_blq_files("inputs.tides.ocean_tide_loading_blq_files", marker_name)
        atmos_present = self._station_in_configured_blq_files("inputs.tides.atmos_tide_loading_blq_files", marker_name)

        if ocean_present and atmos_present:
            Logger.workflow(f"✅ Station '{marker_name}' already present in configured ocean and atmospheric loading BLQ files")
            return

        # Locate the interpolate_loading binary
        try:
            loading_exec = get_interpolate_loading_exec()
        except RuntimeError as e:
            Logger.workflow(f"⚠️ {e} - skipping loading BLQ generation")
            return

        # Download loading grid files if not already present
        Logger.workflow("📥 Ensuring loading grid files are available...")
        if progress_callback:
            progress_callback("Downloading loading grids", 10)
        download_loading_grids(
            download_dir=TABLES_PRODUCTS_PATH,
            progress_callback=progress_callback,
            stop_requested=stop_requested,
        )

        if stop_requested and stop_requested():
            return

        ocean_grid = TABLES_PRODUCTS_PATH / "oceantide.nc"
        atmos_grid = TABLES_PRODUCTS_PATH / "atmtide.nc"

        if not ocean_grid.exists() or not atmos_grid.exists():
            Logger.workflow("⚠️ Loading grid files not available - skipping loading BLQ generation")
            return

        # Build the --code argument: 'ALIC 50137M0014' or just 'ALIC'
        if marker_number:
            station_code = f"{marker_name} {marker_number}"
        else:
            station_code = marker_name

        # Build XYZ location arguments from apriori_position
        x, y, z = apriori_position

        # Generate ocean loading BLQ if needed
        if not ocean_present:
            if stop_requested and stop_requested():
                return

            ocean_output = INPUT_PRODUCTS_PATH / f"{marker_name}_ocean.BLQ"
            Logger.workflow(f"🌊 Computing ocean tide loading for '{station_code}'...")
            if progress_callback:
                progress_callback("Computing ocean loading", 40)

            self._run_interpolate_loading(
                loading_exec, "o", ocean_grid, station_code, x, y, z, ocean_output
            )

            if ocean_output.exists():
                Logger.workflow(f"✅ Ocean loading BLQ generated: {ocean_output.name}")
                self._update_blq_config("inputs.tides.ocean_tide_loading_blq_files",
                                        ocean_output.name)
            else:
                Logger.workflow("⚠️ Ocean loading BLQ file was not generated")

        # Generate atmospheric loading BLQ if needed
        if not atmos_present:
            if stop_requested and stop_requested():
                return

            atmos_output = INPUT_PRODUCTS_PATH / f"{marker_name}_atmos.BLQ"
            Logger.workflow(f"🌬️ Computing atmospheric tide loading for '{station_code}'...")
            if progress_callback:
                progress_callback("Computing atmospheric loading", 70)

            self._run_interpolate_loading(
                loading_exec, "a", atmos_grid, station_code, x, y, z, atmos_output
            )

            if atmos_output.exists():
                Logger.workflow(f"✅ Atmospheric loading BLQ generated: {atmos_output.name}")
                self._update_blq_config("inputs.tides.atmos_tide_loading_blq_files",
                                        atmos_output.name)
            else:
                Logger.workflow("⚠️ Atmospheric loading BLQ file was not generated")

        # Write BLQ config updates to disk (always allowed, even when yaml_overwrite is disabled)
        if self.changes:
            self.write_cached_changes()

        if progress_callback:
            progress_callback("Loading BLQ complete", 100)

    def _run_interpolate_loading(self, loading_exec, loading_type: str, grid_path: Path,
                                 station_code: str, x: float, y: float, z: float, output_path: Path):
        """
        Execute the interpolate_loading binary for a single loading type.

        :param loading_exec: Path or name of the interpolate_loading executable
        :param loading_type: 'o' for ocean or 'a' for atmospheric
        :param grid_path: Path to the loading grid netCDF file
        :param station_code: Station code string for --code (e.g. 'ALIC 50137M0014')
        :param x: ECEF X coordinate
        :param y: ECEF Y coordinate
        :param z: ECEF Z coordinate
        :param output_path: Path for the output BLQ file
        :raises RuntimeError: If the subprocess returns a non-zero exit code
        """
        command = [
            str(loading_exec),
            "--type", loading_type,
            "--grid", str(grid_path),
            "--code", station_code,
            "--xyz",
            "--location", str(x), str(y), str(z),
            "--output", str(output_path),
        ]

        Logger.console(f"🚀 Running: {' '.join(command)}")

        try:
            result = subprocess.run(
                command,
                capture_output=True,
                text=True,
                timeout=120,
            )

            if result.stdout:
                for line in result.stdout.strip().splitlines():
                    Logger.console(line)

            if result.returncode != 0:
                stderr_msg = result.stderr.strip() if result.stderr else "No error output"
                Logger.workflow(f"⚠️ interpolate_loading exited with code {result.returncode}: {stderr_msg}")
                raise RuntimeError(f"interpolate_loading failed (exit code {result.returncode}): {stderr_msg}")

        except subprocess.TimeoutExpired:
            Logger.workflow("⚠️ interpolate_loading timed out after 120 seconds")
            raise RuntimeError("interpolate_loading timed out")

    def _update_blq_config(self, key_path: str, blq_filename: str):
        """
        Append a generated BLQ filename to the existing BLQ file list in the YAML config.

        Does not add the filename if it is already present or if an existing
        wildcard pattern already covers it. Preserves existing entries
        (e.g. the global OLOAD_GO.BLQ / ALOAD_GO.BLQ and any wildcards).

        :param key_path: Dot-separated YAML key path (e.g. 'inputs.tides.ocean_tide_loading_blq_files')
        :param blq_filename: New BLQ filename relative to inputs_root (e.g. 'ALIC_ocean.BLQ')
        """
        import fnmatch

        try:
            keys = key_path.split(".")
            node = self.config
            for k in keys[:-1]:
                node = node[k]
            final_key = keys[-1]

            existing = node.get(final_key)

            if isinstance(existing, CommentedSeq):
                existing_strs = [str(item) for item in existing]

                # Skip if an existing wildcard pattern already covers this filename
                for entry in existing_strs:
                    if fnmatch.fnmatch(blq_filename, entry):
                        return

                # Skip if the exact filename is already present
                if blq_filename not in existing_strs:
                    existing.append(normalise_yaml_value(blq_filename))
                    existing.fa.set_block_style()
                    self.changes = True
            else:
                # No existing list - create one with just the new file
                new_seq = CommentedSeq([normalise_yaml_value(blq_filename)])
                new_seq.fa.set_block_style()
                self.edit_config(key_path, new_seq, False)
        except Exception as e:
            Logger.workflow(f"⚠️ Failed to update BLQ config at {key_path}: {e}")

    def write_cached_changes(self):
        write_yaml(self.config_path, self.config)
        self.changes = False

    #endregion

    #region PEA Processing Execution

    def _ensure_loading_before_pea(self):
        """
        Check whether the station needs loading BLQ files generated and run
        ensure_loading_blq() synchronously before PEA execution begins.

        When YAML overwrite is disabled, reloads the config from disk first so
        that BLQ updates are applied on top of the user's manual edits rather
        than the in-memory UI-applied config.

        Uses marker_name, marker_number, and apriori_position stored by
        apply_ui_config() or set_loading_params() to ensure the correct
        station is checked even when receiver_options contains keys from
        previous runs.
        """
        try:
            marker_name = getattr(self, '_loading_marker_name', None)
            marker_number = getattr(self, '_loading_marker_number', None)
            apriori_position = getattr(self, '_loading_apriori_position', None)

            if not marker_name:
                return

            if not apriori_position or all(v == 0.0 for v in apriori_position):
                Logger.workflow("⚠️ No valid apriori position - skipping loading BLQ check")
                return

            # When YAML overwrite is disabled, reload from disk so BLQ updates
            # are applied on top of the user's manual edits
            if not self.yaml_overwrite:
                self.reload_config()

            def check_stop():
                return self._stop_event.is_set()

            self.ensure_loading_blq(
                marker_name=marker_name,
                marker_number=marker_number,
                apriori_position=list(apriori_position),
                stop_requested=check_stop,
            )

        except Exception as e:
            Logger.workflow(f"⚠️ Loading BLQ pre-check failed (non-fatal): {e}")

    def execute_config(self):
        """
        If changes were made since last write, writes config, then executes pea with config.
        Ensures ocean/atmospheric loading BLQ files are generated before PEA runs.
        All PEA output is logged to the console widget.

        When YAML overwrite is disabled, skips writing UI changes to the config file
        but still allows BLQ updates via ensure_loading_blq().
        """
        # Check if executable is available
        if self.executable is None:
            raise RuntimeError("❌ PEA executable not configured yet. Cannot run processing.")

        # clear stop flag before each run
        self.reset_stop_flag()

        if self.changes and self.yaml_overwrite:
            self.write_cached_changes()
            self.changes = False

        # Ensure loading BLQ files exist before PEA execution
        self._ensure_loading_before_pea()

        # Reset yaml_overwrite back to default for next run
        self.yaml_overwrite = True

        if self._stop_event.is_set():
            Logger.console("🛑 Execution stopped by user during loading BLQ generation")
            return

        command = [self.executable, "--config", str(self.config_path)]
        workdir = str(Path(self.config_path).parent)

        Logger.console(f"🚀 Starting PEA: {' '.join(str(c) for c in command)}")
        Logger.console(f"📂 Working directory: {workdir}")
        Logger.console("=" * 60)

        try:
            # spawn process with process group
            p = self.spawn_process(command, cwd=workdir)

            # forward stdout/stderr line by line to console, can be stopped at any time
            assert p.stdout is not None and p.stderr is not None

            # Use a separate thread to read stderr so we don't miss any output
            stderr_lines = []

            def read_stderr():
                for line in p.stderr:
                    if line:
                        stderr_lines.append(line.rstrip())

            stderr_thread = threading.Thread(target=read_stderr, daemon=True)
            stderr_thread.start()

            while True:
                if self._stop_event.is_set():
                    # UI clicked "stop", exit loop, cleanup handled by stop_all()
                    Logger.console("🛑 PEA execution stopped by user")
                    break

                line = p.stdout.readline()
                if line:
                    # Log each line of PEA output to console
                    Logger.console(line.rstrip())
                else:
                    # no new output, check if process has ended
                    if p.poll() is not None:
                        # Process finished, log any remaining stderr
                        stderr_thread.join(timeout=1.0)
                        for err_line in stderr_lines:
                            if err_line:
                                Logger.console(f"⚠️ {err_line}")

                        if p.returncode != 0:
                            Logger.console(f"❌ PEA exited with code {p.returncode}")
                            e = subprocess.CalledProcessError(p.returncode, command)
                            e.add_note("Error executing PEA command")
                            raise e
                        else:
                            Logger.console("=" * 60)
                            Logger.console("✅ PEA execution completed successfully")
                        break

                # slight sleep to avoid busy polling
                time.sleep(0.01)

        finally:
            # after execution, clean up finished processes
            self._procs = [proc for proc in self._procs if proc.poll() is None]

    def spawn_process(self, args, cwd=None, env=None) -> subprocess.Popen:
        """
        Unified process spawning: use independent process groups for easy kill (macOS/Linux)
        """
        p = subprocess.Popen(
            args,
            cwd=cwd,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            start_new_session=True,  # critical: new session = new process group
        )
        self._procs.append(p)
        return p

    def stop_all(self):
        """
        One-click stop: set stop flag + terminate all child process groups
        """
        self._stop_event.set()

        # try graceful termination first
        for p in list(self._procs):
            try:
                if p.poll() is None:
                    os.killpg(p.pid, signal.SIGTERM)
            except Exception:
                pass

        time.sleep(0.5)  # give it a little time

        # if still not exited, force kill
        for p in list(self._procs):
            try:
                if p.poll() is None:
                    os.killpg(p.pid, signal.SIGKILL)
            except Exception:
                pass

    def reset_stop_flag(self):
        self._stop_event.clear()

    #endregion

    #region Visualisation Plotting

    def build_pos_plots(self, out_dir=None):
        """
        Search for .pos and .POS files directly under outputs_root (not in archive/visual),
        and generate one .html per file in outputs_root/visual.
        Return a list of generated html paths (str).
        """
        try:
            outputs_root = self.config["outputs"]["outputs_root"]
            root = Path(outputs_root).expanduser().resolve()
        except Exception:
            # Fallback to default
            root = Path(__file__).resolve().parents[2] / "tests" / "resources" / "outputData"
            root = root.resolve()

        # Set output dir for HTML plots
        if out_dir is None:
            out_dir = root / "visual"
        else:
            out_dir = Path(out_dir).expanduser().resolve()
        out_dir.mkdir(parents=True, exist_ok=True)

        # Only look in the top-level of outputs_root
        pos_files = list(root.glob("*.pos")) + list(root.glob("*.POS"))

        if pos_files:
            Logger.workflow(f"📂 Found {len(pos_files)} .pos files in {root}:")
            for f in pos_files:
                Logger.workflow(f"   • {f.name}")
        else:
            Logger.workflow(f"⚠️ No .pos files found in {root}")

        # Separate forward and smoothed POS files into two groups
        forward_pos = [f for f in pos_files if "_smoothed" not in f.stem.lower()]
        smoothed_pos = [f for f in pos_files if "_smoothed" in f.stem.lower()]

        htmls = []

        # Plot forward (regular) POS files as one unified set
        if forward_pos:
            try:
                forward_paths = [str(f) for f in forward_pos]
                save_prefix = out_dir / "plot_pos"
                html_files = plot_pos_files(
                    input_files=forward_paths,
                    save_prefix=str(save_prefix)
                )
                htmls.extend(html_files)
            except Exception as e:
                Logger.workflow(f"[plot_pos] ❌ Failed for forward pos files: {e}")

        # Plot smoothed POS files as a separate unified set
        if smoothed_pos:
            try:
                smoothed_paths = [str(f) for f in smoothed_pos]
                save_prefix = out_dir / "plot_pos_smoothed"
                html_files = plot_pos_files(
                    input_files=smoothed_paths,
                    save_prefix=str(save_prefix)
                )
                htmls.extend(html_files)
            except Exception as e:
                Logger.workflow(f"[plot_pos] ❌ Failed for smoothed pos files: {e}")

        # Final summary
        if htmls:
            Logger.workflow(f"✅ Generated {len(htmls)} plot(s) → saved in {out_dir}")
        else:
            Logger.workflow("⚠️ No plots were generated.")

        return htmls

    def build_trace_plots(self, out_dir=None):
        """
        Search for .TRACE files directly under outputs_root
        and generate HTML plots using plot_trace_res in outputs_root/visual.
        Return a list of generated html paths (str).

        Uses configuration:
            --mark-amb-resets --mark-large-errors --show-stats-table
            --ambiguity-counts --ambiguity-totals --amb-totals-orient h
        """
        try:
            outputs_root = self.config["outputs"]["outputs_root"]
            root = Path(outputs_root).expanduser().resolve()
        except Exception:
            # Fallback to default
            root = Path(__file__).resolve().parents[2] / "tests" / "resources" / "outputData"
            root = root.resolve()

        # Set output dir for HTML plots
        if out_dir is None:
            out_dir = root / "visual"
        else:
            out_dir = Path(out_dir).expanduser().resolve()
        out_dir.mkdir(parents=True, exist_ok=True)

        # Look for Network*.TRACE files in the outputs_root
        trace_files = list(root.glob("Network*.TRACE")) + list(root.glob("network*.TRACE")) + list(root.glob("Network*.trace")) + list(root.glob("network*.trace"))

        # Also check for any other .TRACE files if no Network files found
        if not trace_files:
            trace_files = list(root.glob("*.TRACE")) + list(root.glob("*.trace"))

        if trace_files:
            Logger.workflow(f"📂 Found {len(trace_files)} .TRACE files in {root}:")
            for f in trace_files:
                Logger.workflow(f"   • {f.name}")
        else:
            Logger.workflow(f"⚠️ No .TRACE files found in {root}")
            return []

        htmls = []
        try:
            # Convert trace files to string paths for the plotting function
            trace_file_paths = [str(f) for f in trace_files]

            html_files = plot_trace_res_files(
                files=trace_file_paths,
                out_dir=str(out_dir),
                mark_amb_resets=True,
                mark_large_errors=True,
                show_stats_table=True,
                ambiguity_counts=True,
                ambiguity_totals=True,
                amb_totals_orient="h",
            )
            htmls.extend(html_files)
        except Exception as e:
            Logger.workflow(f"[plot_trace_res] ❌ Failed to generate trace plots: {e}")

        # Final summary
        if htmls:
            Logger.workflow(f"✅ Generated {len(htmls)} trace plot(s) → saved in {out_dir}")
        else:
            Logger.workflow("⚠️ No trace plots were generated.")

        return htmls

    #endregion