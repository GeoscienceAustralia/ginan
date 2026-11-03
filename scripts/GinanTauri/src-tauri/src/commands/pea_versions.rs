//! The local cache of downloaded pea releases.
//!
//! Every release the user downloads is kept in its own folder,
//! `<app data>/pea-versions/<tag>/pea`, so switching back to one is instant and
//! works offline. `pea_version.txt` in the app data dir records which cached
//! release is the selected managed binary.
//!
//! Earlier versions of the app kept a single binary at `<app data>/pea`,
//! overwritten on every download; `migrate_legacy` moves that into the cache on first launch.

use std::path::{Path, PathBuf};

pub(crate) const EXE_NAME: &str = if cfg!(windows) { "pea.exe" } else { "pea" };

/// `<app data>/pea-versions`.
pub(crate) fn versions_dir(app_data_dir: &Path) -> PathBuf {
    app_data_dir.join("pea-versions")
}

/// Release tags become folder names, so refuse anything that could step out of
/// the cache. GitHub tags are plain (`v4.1.0`); this only guards odd input.
pub(crate) fn is_safe_tag(tag: &str) -> bool {
    !tag.is_empty()
        && tag != "."
        && tag != ".."
        && !tag.contains(['/', '\\', ':', '\0'])
}

/// Where the cached binary for `tag` lives (whether or not it exists yet).
pub(crate) fn binary_for(versions_dir: &Path, tag: &str) -> Result<PathBuf, String> {
    if !is_safe_tag(tag) {
        return Err(format!("Invalid release tag: {tag}"));
    }
    Ok(versions_dir.join(tag).join(EXE_NAME))
}

/// The tag of a cached binary, when `path` is one; `None` for anything else,
/// such as a custom build elsewhere on disk.
pub(crate) fn tag_of(versions_dir: &Path, path: &Path) -> Option<String> {
    if path.file_name()? != EXE_NAME {
        return None;
    }
    let tag_dir = path.parent()?;
    if tag_dir.parent()? != versions_dir {
        return None;
    }
    let tag = tag_dir.file_name()?.to_str()?.to_string();
    is_safe_tag(&tag).then_some(tag)
}

/// Numeric parts of a tag, for newest-first ordering (`v4.10.0` after `v4.9.2`).
fn version_key(tag: &str) -> Vec<u64> {
    tag.split(|c: char| !c.is_ascii_digit())
        .filter(|s| !s.is_empty())
        .filter_map(|s| s.parse().ok())
        .collect()
}

/// Every cached release that still has its binary, newest first.
pub(crate) fn installed(versions_dir: &Path) -> Vec<String> {
    let Ok(entries) = std::fs::read_dir(versions_dir) else {
        return Vec::new();
    };
    let mut tags: Vec<String> = entries
        .filter_map(|e| e.ok())
        .filter(|e| e.path().join(EXE_NAME).is_file())
        .filter_map(|e| e.file_name().to_str().map(str::to_string))
        .filter(|t| is_safe_tag(t))
        .collect();
    tags.sort_by(|a, b| version_key(b).cmp(&version_key(a)).then_with(|| b.cmp(a)));
    tags
}

/// Move a binary left by an earlier version of the app (`<app data>/pea`, tag
/// in `version_file`) into the cache. Without a recorded tag there is no folder to
/// file it under, so it is left where it is. Failure is not fatal: the legacy
/// binary keeps working from its old place.
pub(crate) fn migrate_legacy(legacy: &Path, version_file: &Path, versions_dir: &Path) {
    if !legacy.is_file() {
        return;
    }
    let Some(tag) = super::execution_helpers::read_local_version(version_file) else {
        return;
    };
    let Ok(dest) = binary_for(versions_dir, &tag) else {
        return;
    };
    if dest.is_file() {
        // Already cached (e.g. a half-finished earlier migration); the copy at
        // the old place is redundant.
        let _ = std::fs::remove_file(legacy);
        return;
    }
    if let Some(parent) = dest.parent() {
        if std::fs::create_dir_all(parent).is_err() {
            return;
        }
    }
    let _ = std::fs::rename(legacy, &dest);
}

/// The managed binary to use when the user hasn't picked a custom one: the
/// release recorded in `version_file` if it is still cached, else the newest
/// cached release, else an untagged binary from an earlier version of the app.
/// `None` when nothing has ever been downloaded.
pub(crate) fn select_managed(
    versions_dir: &Path,
    legacy: &Path,
    version_file: &Path,
) -> Option<(PathBuf, Option<String>)> {
    let recorded = super::execution_helpers::read_local_version(version_file)
        .and_then(|t| binary_for(versions_dir, &t).ok().filter(|b| b.is_file()).map(|b| (b, Some(t))));
    recorded
        .or_else(|| {
            installed(versions_dir)
                .into_iter()
                .next()
                .and_then(|t| binary_for(versions_dir, &t).ok().map(|b| (b, Some(t))))
        })
        .or_else(|| legacy.is_file().then(|| (legacy.to_path_buf(), None)))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("ginan-pea-versions-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn put(path: &Path) {
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, b"bin").unwrap();
    }

    #[test]
    fn rejects_tags_that_would_leave_the_cache() {
        for bad in ["", ".", "..", "../x", "a/b", "a\\b", "c:x"] {
            assert!(!is_safe_tag(bad), "{bad:?} should be rejected");
        }
        assert!(is_safe_tag("v4.1.0"));
        assert!(binary_for(Path::new("/c"), "../evil").is_err());
    }

    #[test]
    fn recognises_cached_binaries_and_nothing_else() {
        let dir = Path::new("/data/pea-versions");
        let cached = dir.join("v4.1.0").join(EXE_NAME);
        assert_eq!(tag_of(dir, &cached).as_deref(), Some("v4.1.0"));
        assert_eq!(tag_of(dir, &PathBuf::from("/usr/local/bin").join(EXE_NAME)), None);
        assert_eq!(tag_of(dir, &dir.join("v4.1.0").join("other")), None);
        assert_eq!(tag_of(dir, &dir.join("a").join("v4.1.0").join(EXE_NAME)), None);
    }

    #[test]
    fn lists_installed_releases_newest_first() {
        let root = scratch("installed");
        let dir = versions_dir(&root);
        for tag in ["v4.9.2", "v4.10.0", "v3.0.0"] {
            put(&dir.join(tag).join(EXE_NAME));
        }
        // A folder whose binary is gone does not count.
        std::fs::create_dir_all(dir.join("v5.0.0")).unwrap();
        assert_eq!(installed(&dir), ["v4.10.0", "v4.9.2", "v3.0.0"]);
        assert!(installed(&root.join("missing")).is_empty());
    }

    #[test]
    fn moves_a_tagged_legacy_binary_into_the_cache() {
        let root = scratch("migrate");
        let legacy = root.join(EXE_NAME);
        let version_file = root.join("pea_version.txt");
        put(&legacy);
        std::fs::write(&version_file, "v4.1.0\n").unwrap();

        migrate_legacy(&legacy, &version_file, &versions_dir(&root));

        assert!(!legacy.exists());
        assert!(versions_dir(&root).join("v4.1.0").join(EXE_NAME).is_file());
    }

    #[test]
    fn selects_the_recorded_release_then_the_newest_cached_one() {
        let root = scratch("select");
        let dir = versions_dir(&root);
        let legacy = root.join(EXE_NAME);
        let version_file = root.join("pea_version.txt");
        assert_eq!(select_managed(&dir, &legacy, &version_file), None);

        put(&dir.join("v4.1.0").join(EXE_NAME));
        put(&dir.join("v4.2.0").join(EXE_NAME));
        let picked = |v: &str| Some((dir.join(v).join(EXE_NAME), Some(v.to_string())));
        assert_eq!(select_managed(&dir, &legacy, &version_file), picked("v4.2.0"));

        std::fs::write(&version_file, "v4.1.0").unwrap();
        assert_eq!(select_managed(&dir, &legacy, &version_file), picked("v4.1.0"));

        // Recorded release deleted from the cache: fall back to the newest.
        std::fs::write(&version_file, "v3.0.0").unwrap();
        assert_eq!(select_managed(&dir, &legacy, &version_file), picked("v4.2.0"));
    }

    #[test]
    fn leaves_an_untagged_legacy_binary_alone() {
        let root = scratch("untagged");
        let legacy = root.join(EXE_NAME);
        put(&legacy);

        migrate_legacy(&legacy, &root.join("pea_version.txt"), &versions_dir(&root));

        assert!(legacy.is_file());
        assert!(installed(&versions_dir(&root)).is_empty());
    }
}
