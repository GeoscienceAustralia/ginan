//! Where each product lives under `products_dir`:
//!
//! ```text
//! products/
//!   static/                 support files fetched once (STATIC_PRODUCTS)
//!   daily/2024-09-04/       one day's SP3, CLK, BIA and BRDC
//!   weekly/2330/            products spanning a GPS week (07D)
//!   .bia_code_cache/
//! ```
//!
//! Every dated product's folder comes from its own file name, so the
//! downloader, the move of files left flat by earlier versions of the app, and
//! the config's file lists cannot disagree.

use super::catalogue_parse::parse_igs_filename;
use super::naming::{gps_week, parse_date};
use super::STATIC_PRODUCTS;
use chrono::NaiveDate;
use std::path::{Path, PathBuf};

pub(crate) const STATIC_DIR: &str = "static";

pub(crate) fn daily_dir(date: NaiveDate) -> String {
    format!("daily/{}", date.format("%Y-%m-%d"))
}

pub(crate) fn weekly_dir(week: u32) -> String {
    format!("weekly/{week:04}")
}

/// Folder (relative to `products_dir`) a dated product belongs in, or `None`
/// for a name that carries no date. Handles IGS long names and BRDC nav files
/// (`BRDC00IGS_R_20242480000_01D_MN.rnx`).
pub(crate) fn product_folder(name: &str) -> Option<String> {
    let (year, doy, timespan) = match parse_igs_filename(name) {
        Some((_, _, _, y, d, span, ..)) => (y as i32, d, span),
        None if name.starts_with("BRDC") && name.len() >= 27 && name.is_char_boundary(27) => (
            name[12..16].parse().ok()?,
            name[16..19].parse().ok()?,
            name[24..27].to_string(),
        ),
        None => return None,
    };
    let date = NaiveDate::from_yo_opt(year, doy)?;
    Some(if timespan == "07D" { weekly_dir(gps_week(date)) } else { daily_dir(date) })
}

/// Where a product file is stored: its folder when dated, `root` otherwise.
pub(crate) fn product_path(root: &Path, name: &str) -> PathBuf {
    match product_folder(name) {
        Some(folder) => root.join(folder).join(name),
        None => root.join(name),
    }
}

/// Every folder a run over `start..=end` reads products from: each day's, then
/// each GPS week's. Empty when either end does not parse.
pub(crate) fn window_folders(start: &str, end: &str) -> Vec<String> {
    let Ok(start) = parse_date(start) else { return Vec::new() };
    let end = if end.trim().is_empty() { start } else {
        match parse_date(end) { Ok(d) => d, Err(_) => return Vec::new() }
    };
    let days: Vec<NaiveDate> = start.iter_days().take_while(|d| *d <= end).collect();
    let mut weeks: Vec<u32> = days.iter().map(|d| gps_week(*d)).collect();
    weeks.dedup();
    days.into_iter().map(daily_dir).chain(weeks.into_iter().map(weekly_dir)).collect()
}

/// Move products left flat in `root` by earlier versions of the app into their
/// folders: dated files by name, support files under `static/`. A file whose
/// destination already exists stays where it is. Returns the names moved.
pub(crate) fn migrate_flat_layout(root: &Path) -> Vec<String> {
    let mut moved = Vec::new();
    let mut move_to = |from: PathBuf, to: PathBuf, label: String| {
        if from.is_file() && !to.exists() {
            let ok = to.parent().map_or(true, |p| std::fs::create_dir_all(p).is_ok());
            if ok && std::fs::rename(&from, &to).is_ok() {
                moved.push(label);
            }
        }
    };
    for rel in STATIC_PRODUCTS {
        move_to(root.join(rel), root.join(STATIC_DIR).join(rel), rel.to_string());
    }
    if let Ok(dir) = std::fs::read_dir(root) {
        for item in dir.filter_map(Result::ok) {
            let Some(name) = item.file_name().to_str().map(str::to_string) else { continue };
            // A half-written `.part` parses as dated too; leave it be.
            if name.ends_with(".part") || name.ends_with(".gz") {
                continue;
            }
            if let Some(folder) = product_folder(&name) {
                move_to(item.path(), root.join(folder).join(&name), name);
            }
        }
    }
    // Only goes when empty, i.e. everything in it was a support file.
    let _ = std::fs::remove_dir(root.join("tables"));
    moved
}

/// Product files PEA could be given: those in every day and week folder, and
/// any still flat in `root`.
pub(crate) fn dated_product_files(root: &Path) -> Vec<PathBuf> {
    let files = |dir: &Path| -> Vec<PathBuf> {
        std::fs::read_dir(dir)
            .map(|d| d.filter_map(Result::ok).map(|e| e.path()).filter(|p| p.is_file()).collect())
            .unwrap_or_default()
    };
    let mut out = files(root);
    for group in ["daily", "weekly"] {
        if let Ok(dirs) = std::fs::read_dir(root.join(group)) {
            for d in dirs.filter_map(Result::ok) {
                out.extend(files(&d.path()));
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp(label: &str) -> PathBuf {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let p = std::env::temp_dir().join(format!("ginan_layout_{label}_{}_{nanos}", std::process::id()));
        std::fs::create_dir_all(&p).unwrap();
        p
    }

    #[test]
    fn dated_products_go_in_their_day_or_week() {
        // 2024 day 248 is 2024-09-04, GPS week 2330.
        assert_eq!(
            product_folder("IGS0OPSFIN_20242480000_01D_15M_ORB.SP3").as_deref(),
            Some("daily/2024-09-04"),
        );
        assert_eq!(
            product_folder("BRDC00IGS_R_20242480000_01D_MN.rnx").as_deref(),
            Some("daily/2024-09-04"),
        );
        assert_eq!(
            product_folder("COD0MGXFIN_20242480000_07D_01D_OSB.BIA").as_deref(),
            Some("weekly/2330"),
        );
        assert_eq!(product_folder("igs20.atx"), None);
        assert_eq!(product_folder("BRDC"), None);
    }

    #[test]
    fn a_window_reads_each_day_then_each_week() {
        // Sat 2024-09-07 is the last day of week 2330; Sunday starts 2331.
        assert_eq!(
            window_folders("2024-09-07 00:00:00", "2024-09-08 23:59:30"),
            ["daily/2024-09-07", "daily/2024-09-08", "weekly/2330", "weekly/2331"],
        );
        assert_eq!(window_folders("2024-09-07 00:00:00", ""), ["daily/2024-09-07", "weekly/2330"]);
        assert!(window_folders("", "").is_empty());
    }

    #[test]
    fn flat_files_from_an_older_build_move_into_place() {
        let root = tmp("migrate");
        std::fs::create_dir_all(root.join("tables")).unwrap();
        for rel in ["igs20.atx", "tables/gpt_25.grd", "IGS0OPSFIN_20242480000_01D_15M_ORB.SP3",
                    "BRDC00IGS_R_20242480000_01D_MN.rnx", "IGS0OPSFIN_20242490000_01D_30S_CLK.CLK.part",
                    "notes.txt"] {
            std::fs::write(root.join(rel), "x").unwrap();
        }

        let moved = migrate_flat_layout(&root);

        assert_eq!(moved.len(), 4);
        assert!(root.join("static/igs20.atx").is_file());
        assert!(root.join("static/tables/gpt_25.grd").is_file());
        assert!(!root.join("tables").exists());
        assert!(root.join("daily/2024-09-04/IGS0OPSFIN_20242480000_01D_15M_ORB.SP3").is_file());
        assert!(root.join("daily/2024-09-04/BRDC00IGS_R_20242480000_01D_MN.rnx").is_file());
        assert!(root.join("IGS0OPSFIN_20242490000_01D_30S_CLK.CLK.part").is_file());
        assert!(root.join("notes.txt").is_file());
        assert_eq!(dated_product_files(&root).len(), 4); // 2 dated + .part + notes
        assert!(migrate_flat_layout(&root).is_empty());
        std::fs::remove_dir_all(&root).ok();
    }

    #[test]
    fn an_existing_destination_is_never_overwritten() {
        let root = tmp("keep");
        let name = "IGS0OPSFIN_20242480000_01D_15M_ORB.SP3";
        std::fs::create_dir_all(root.join("daily/2024-09-04")).unwrap();
        std::fs::write(root.join("daily/2024-09-04").join(name), "new").unwrap();
        std::fs::write(root.join(name), "old").unwrap();

        assert!(migrate_flat_layout(&root).is_empty());
        assert_eq!(std::fs::read_to_string(product_path(&root, name)).unwrap(), "new");
        std::fs::remove_dir_all(&root).ok();
    }
}
