use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use crate::commands::products::layout::STATIC_DIR;

/// Validates whether the given antenna `name` is listed in the IGS ANTEX file at `atx_path`.
///
/// Scans every line ending with `TYPE / SERIAL NO`, extracts columns 0..20 as the antenna
/// type token, trims whitespace, and compares (case-sensitive) against the trimmed `name`.
// `async`: parsing a large file must not run on (and freeze) the main thread.
#[tauri::command(async)]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn validate_antenna(name: String, atx_path: String) -> Result<bool, String> {
    let target = name.trim().to_string();
    if target.is_empty() {
        return Ok(false);
    }

    let path = Path::new(&atx_path);
    let file = File::open(path)
        .map_err(|e| format!("Failed to open ANTEX file '{}': {}", atx_path, e))?;
    let reader = BufReader::new(file);

    for line_result in reader.lines() {
        let line = match line_result {
            Ok(l) => l,
            Err(e) => return Err(format!("Error reading ANTEX file: {}", e)),
        };

        if !line.trim_end().ends_with("TYPE / SERIAL NO") {
            continue;
        }

        // Extract chars 0..20 as the antenna type token.
        let token: String = line.chars().take(20).collect();
        if token.trim() == target {
            return Ok(true);
        }
    }

    Ok(false)
}

/// Searches `products_dir/static`, then `products_dir`, for an ANTEX file, returning the first match in priority order:
/// `igs20.atx`, `igs14.atx`, `igs13.atx`, `igs08.atx`, `igs05.atx`.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn find_antex_file(products_dir: String) -> Result<Option<String>, String> {
    let dir = Path::new(&products_dir);
    if !dir.exists() {
        return Err(format!("Products directory does not exist: {}", products_dir));
    }

    // `static/` is where the support files live; the root is where earlier
    // versions of the app left them.
    let candidates = ["igs20.atx", "igs14.atx", "igs13.atx", "igs08.atx", "igs05.atx"];
    for name in candidates.iter() {
        for p in [dir.join(STATIC_DIR).join(name), dir.join(name)] {
            if p.exists() {
                return Ok(Some(p.to_string_lossy().into_owned()));
            }
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::fs;
    use std::io::Write;

    fn unique_tmp_dir(label: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        p.push(format!("ginan_antex_test_{}_{}_{}", label, std::process::id(), nanos));
        fs::create_dir_all(&p).expect("create tmp dir");
        p
    }

    const SAMPLE_ATX: &str = "\
                                                            START OF HEADER
                                                            END OF HEADER
                                                            START OF ANTENNA
TRM59800.00     SCIS                                    TYPE / SERIAL NO
                                                            END OF ANTENNA
                                                            START OF ANTENNA
LEIAR25.R3      NONE                                    TYPE / SERIAL NO
                                                            END OF ANTENNA
";

    #[test]
    fn validate_antenna_matches_exact_name() {
        let dir = unique_tmp_dir("match");
        let atx = dir.join("igs20.atx");
        let mut f = File::create(&atx).unwrap();
        f.write_all(SAMPLE_ATX.as_bytes()).unwrap();

        let result = validate_antenna(
            "TRM59800.00     SCIS".to_string(),
            atx.to_string_lossy().into_owned(),
        );
        assert_eq!(result, Ok(true));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn validate_antenna_unknown_returns_false() {
        let dir = unique_tmp_dir("unknown");
        let atx = dir.join("igs20.atx");
        let mut f = File::create(&atx).unwrap();
        f.write_all(SAMPLE_ATX.as_bytes()).unwrap();

        let result = validate_antenna(
            "NONEXISTENT_ANTENNA".to_string(),
            atx.to_string_lossy().into_owned(),
        );
        assert_eq!(result, Ok(false));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_antex_file_priority_order() {
        let dir = unique_tmp_dir("priority");
        File::create(dir.join("igs14.atx")).unwrap();
        File::create(dir.join("igs20.atx")).unwrap();

        let result = find_antex_file(dir.to_string_lossy().into_owned()).unwrap();
        let path = result.expect("expected a match");
        assert!(path.contains("igs20.atx"), "expected igs20.atx, got {}", path);

        let _ = fs::remove_dir_all(&dir);
    }
}
