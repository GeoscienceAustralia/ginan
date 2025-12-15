# app/utils/archive_manager.py

from pathlib import Path
import shutil
from datetime import datetime
from typing import Optional, Dict, Any

from scripts.GinanUI.app.utils.common_dirs import INPUT_PRODUCTS_PATH

from scripts.GinanUI.app.utils.logger import Logger


def archive_old_outputs(output_dir: Path, visual_dir: Path = None):
    """
    Moves existing output files to an archive directory to keep the workspace clean.

    THIS FUNCTION LOOKS FOR ALL TXT, LOG, JSON, POS files.
    DON'T USE THE INPUT PRODUCTS DIRECTORY.

    :param output_dir: Path to the user-selected output directory.
    :param visual_dir: Optional path to associated visualisation directory.
    """
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = output_dir / "archive" / timestamp
    archive_dir.mkdir(parents=True, exist_ok=True)

    # Move .pos, .log, .txt, etc. from output_dir
    moved_files = 0
    for ext in [".pos", ".POS", ".log", ".txt", ".json"]:
        for file in output_dir.glob(f"*{ext}"):
            shutil.move(str(file), archive_dir / file.name)
            moved_files += 1

    # Move HTML visual files (optional)
    if visual_dir and visual_dir.exists():
        visual_archive = archive_dir / "visual"
        visual_archive.mkdir(parents=True, exist_ok=True)
        for html_file in visual_dir.glob("*.html"):
            shutil.move(str(html_file), visual_archive / html_file.name)
            moved_files += 1

    if moved_files > 0:
        Logger.console(f"📦 Archived {moved_files} old output file(s) to: {archive_dir}")
    else:
        Logger.console("📂 No previous outputs found to archive.")

def archive_products(products_dir: Path = INPUT_PRODUCTS_PATH, reason: str = "manual", startup_archival: bool = False,
                     include_patterns: Optional[list[str]] = None) -> Optional[Path]:
    """
    Archive GNSS product files from products_dir into a timestamped subfolder
    under products_dir/archived/.

    :param products_dir: Directory containing GNSS product files
    :param reason: String describing why the archive is happening (e.g., "rinex_change", "ppp_selection_change")
    :param startup_archival: If True, archives files which are meant to be archived only once during app startup
    :param include_patterns: Optional list of glob patterns to include when archiving
    :return: Path to the archive folder if files were archived, else None
    """
    if not products_dir.exists():
        Logger.console(f"Products dir {products_dir} does not exist.")
        return None

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = products_dir / "archived" / f"{reason}_{timestamp}"
    archive_dir.mkdir(parents=True, exist_ok=True)

    product_patterns = [
        "*.SP3",                    # precise orbit
        "*.CLK",                    # clock files
        "*.BIA",                    # biases
        "*.ION",                    # ionosphere products (if used)
        "*.TRO",                    # troposphere products (if used)
    ]

    if startup_archival:
        startup_patterns = [
            "finals.data.iau2000.txt",
            "BRDC*.rnx",
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

    archived_files = []
    for pattern in product_patterns:
        for file in products_dir.glob(pattern):
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
        Logger.console("No matching product files found to archive.")
        return None


def archive_products_if_rinex_changed(current_rinex: Path,
                                      last_rinex: Optional[Path],
                                      products_dir: Path) -> Optional[Path]:
    """
    If the RINEX file has changed since last load, archive the cached products.
    """
    if last_rinex and current_rinex.resolve() == last_rinex.resolve():
        Logger.console("RINEX file unchanged — skipping product cleanup.")
        return None

    Logger.console("RINEX file changed — archiving old products.")
    # Shouldn't remove BRDC if date isn't changed but would require extracting current and last rnx
    return archive_products(products_dir, reason="rinex_change", include_patterns=["BRDC*.rnx*"])


def archive_products_if_selection_changed(current_selection: Dict[str, Any],
                                          last_selection: Optional[Dict[str, Any]],
                                          products_dir: Path) -> Optional[Path]:
    """
    If the PPP product selection (AC/project/solution) has changed, archive the cached products.
    Excludes BRDC and finals.data.iau2000.txt since they are reusable.
    """
    if last_selection and current_selection == last_selection:
        Logger.console("[Archiver] PPP product selection unchanged — skipping product cleanup.")
        return None

    if last_selection:
        diffs = {k: (last_selection.get(k), current_selection.get(k))
                 for k in set(last_selection) | set(current_selection)
                 if last_selection.get(k) != current_selection.get(k)}
        Logger.console(f"[Archiver] PPP selection changed → differences: {diffs}")

    return archive_products(products_dir,reason="ppp_selection_change")

