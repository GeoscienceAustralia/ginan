# app/utils/archive_manager.py

from pathlib import Path
import shutil
import os
from datetime import datetime
from typing import Optional, Dict, Any

from scripts.GinanUI.app.utils.common_dirs import INPUT_PRODUCTS_PATH

from scripts.GinanUI.app.utils.logger import Logger


def archive_old_outputs(output_dir: Path, visual_dir: Path = None):
    """
    Moves existing output files to an archive directory to keep the workspace clean.

    THIS FUNCTION LOOKS FOR ALL TXT, LOG, JSON, POS, GPX, TRACE files.
    DON'T USE THE INPUT PRODUCTS DIRECTORY.

    :param output_dir: Path to the user-selected output directory.
    :param visual_dir: Optional path to associated visualisation directory.
    """
    # Move visual folder contents if it exists (visual_dir is typically output_dir / "visual")
    if visual_dir is None:
        visual_dir = output_dir / "visual"

    # First, collect all files that would be archived (only .POS, .GPX, .TRACE)
    files_to_archive = []
    for ext in [".pos", ".POS", ".gpx", ".GPX", ".trace", ".TRACE"]:
        files_to_archive.extend(list(output_dir.glob(f"*{ext}")))

    # Check visual directory for files
    visual_files_to_archive = []
    if visual_dir.exists() and visual_dir.is_dir():
        visual_files_to_archive = [f for f in visual_dir.glob("*") if f.is_file()]

    # Only proceed if there's something to archive
    if not files_to_archive and not visual_files_to_archive:
        Logger.console("📂 No previous outputs found to archive.")
        return

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = output_dir / "archive" / timestamp

    # Make the visual directory
    os.makedirs(archive_dir, exist_ok=True)

    # Move .pos, .gpx, .trace from output_dir
    moved_files = 0
    for file in files_to_archive:
        try:
            shutil.move(str(file), str(archive_dir / file.name))
            moved_files += 1
        except Exception as e:
            Logger.console(f"Failed to archive {file.name}: {e}")

    # Move visual folder contents
    if visual_files_to_archive:
        visual_archive = archive_dir / "visual"

        # Make the visual archive directory
        os.makedirs(visual_archive, exist_ok=True)

        for visual_file in visual_files_to_archive:
            try:
                shutil.move(str(visual_file), str(visual_archive / visual_file.name))
                moved_files += 1
            except Exception as e:
                Logger.console(f"Failed to archive {visual_file.name}: {e}")
        # Remove the now-empty visual archive directory
        try:
            visual_dir.rmdir()
        except OSError:
            pass  # Directory not empty or other issue

    if moved_files > 0:
        Logger.console(f"📦 Archived {moved_files} old output file(s) to: {archive_dir}")
    else:
        Logger.console("📂 No previous outputs found to archive.")

def archive_products(products_dir: Path = INPUT_PRODUCTS_PATH, reason: str = "manual", startup_archival: bool = False,
                     include_patterns: Optional[list[str]] = None) -> Optional[Path]:
    """
    Archive GNSS product files from products_dir into a timestamped subfolder
    under products_dir/archive/.

    :param products_dir: Directory containing GNSS product files
    :param reason: String describing why the archive is happening (e.g., "rinex_change", "ppp_selection_change")
    :param startup_archival: If True, archives files which are meant to be archived only once during app startup
    :param include_patterns: Optional list of glob patterns to include when archiving
    :return: Path to the archive folder if files were archived, else None
    """
    if not products_dir.exists():
        Logger.console(f"Products dir {products_dir} does not exist.")
        return None

    product_patterns = [
        "*.SP3",                    # precise orbit
        "*.CLK",                    # clock files
        "*.BIA",                    # biases
        "*.ION",                    # ionosphere products (if used)
        "*.TRO",                    # troposphere products (if used)
        "BRDC*.rnx",                # broadcast ephemeris
        "BRDC*.rnx.*",              # compressed broadcast ephemeris
    ]

    if startup_archival:
        startup_patterns = [
            "finals.data.iau2000.txt",
            "igs_satellite_metadata*.snx",
            "igs20*.atx",
            "tables/ALOAD*",
            "tables/OLOAD*",
            "tables/gpt_*.grd",
            "tables/qzss_*"
            "tables/igrf*coeffs.txt",
            "tables/DE436*",
            "tables/fes2014*.dat",
            "tables/opoleloadcoefcmcor.txt",
            "tables/sat_yaw*.snx",
            "tables/bds_yaw*.snx",
            "tables/qzss_yaw*.snx",
            "tables/bds_yaw*.snx"
        ]
        # Ensure directories exist
        products_dir.mkdir(parents=True, exist_ok=True)
        (products_dir / "tables").mkdir(parents=True, exist_ok=True)

        # Scans every file and checks created within 7 days
        for pattern in startup_patterns:
            for file in products_dir.glob(pattern):
                creation_time = datetime.fromtimestamp(file.stat().st_ctime)
                if (datetime.now() - creation_time).days > 7:
                    Logger.console(f"Startup archival: {file.name} is older than 7 days, archiving.")
                    product_patterns.append(pattern)

    # Include explicit patterns if provided
    if include_patterns:
        product_patterns.extend(include_patterns)

    # First, collect all files that match the patterns
    files_to_archive = []
    for pattern in product_patterns:
        files_to_archive.extend(list(products_dir.glob(pattern)))

    # Only proceed if there's something to archive
    if not files_to_archive:
        Logger.console("No matching product files found to archive.")
        return None

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = products_dir / "archive" / f"{reason}_{timestamp}"

    # Create the archive directory
    os.makedirs(archive_dir, exist_ok=True)

    archived_files = []
    for file in files_to_archive:
        try:
            target = archive_dir / file.name
            shutil.move(str(file), str(target))
            archived_files.append(file.name)
        except Exception as e:
            Logger.console(f"Failed to archive {file.name}: {e}")

    if archived_files:
        Logger.console(f"Archived {', '.join(archived_files)} → {archive_dir}")
        return archive_dir
    else:
        # Clean up empty archive directory if all moves failed
        try:
            archive_dir.rmdir()
        except OSError:
            pass
        Logger.console("No matching product files found to archive.")
        return None


def archive_products_if_rinex_changed(current_rinex: Path,
                                      last_rinex: Optional[Path],
                                      products_dir: Path) -> Optional[Path]:
    """
    If the RINEX file has changed since last load, archive the cached products.
    """
    if last_rinex and current_rinex.resolve() == last_rinex.resolve():
        Logger.console("RINEX file unchanged, skipping product cleanup.")
        return None

    Logger.console("RINEX file changed, archiving old products.")
    # Shouldn't remove BRDC if date isn't changed but would require extracting current and last rnx
    return archive_products(products_dir, reason="rinex_change")


def archive_products_if_selection_changed(current_selection: Dict[str, Any],
                                          last_selection: Optional[Dict[str, Any]],
                                          products_dir: Path) -> Optional[Path]:
    """
    If the PPP product selection (AC/project/solution) has changed, archive the cached products.
    Excludes BRDC and finals.data.iau2000.txt since they are reusable.
    """
    if last_selection and current_selection == last_selection:
        Logger.console("[Archiver] PPP product selection unchanged, skipping product cleanup.")
        return None

    if last_selection:
        diffs = {k: (last_selection.get(k), current_selection.get(k))
                 for k in set(last_selection) | set(current_selection)
                 if last_selection.get(k) != current_selection.get(k)}
        Logger.console(f"[Archiver] PPP selection changed → differences: {diffs}")

    return archive_products(products_dir,reason="ppp_selection_change")

