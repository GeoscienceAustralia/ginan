//! Shared libraries PEA needs that a Mac may not have.
//!
//! The macOS PEA release links OpenMP from Homebrew's absolute path
//! (`/opt/homebrew/opt/libomp/lib/libomp.dylib`). Without `brew install
//! libomp` the loader refuses to start PEA at all, which surfaces as a run or
//! stream test that dies with SIGABRT before writing anything.
//!
//! Rather than ask for Homebrew, the app fetches the same library itself: the
//! libomp bottle Homebrew publishes, checked against the SHA-256 in Homebrew's
//! formula index, with only `libomp.dylib` kept, in the app's data directory.
//! PEA is then started with that directory on `DYLD_FALLBACK_LIBRARY_PATH`,
//! which `dyld` searches by file name when an absolute install path is missing.
//! A system copy, when there is one, is left to be found first.

use std::io::Read;
use std::path::{Path, PathBuf};

use sha2::{Digest, Sha256};
use tauri::{Emitter, Manager};

/// Homebrew's formula index entry for libomp.
const FORMULA_URL: &str = "https://formulae.brew.sh/api/formula/libomp.json";
const LIB_NAME: &str = "libomp.dylib";
/// Where Homebrew installs it on Apple Silicon and Intel Macs.
const SYSTEM_PATHS: &[&str] = &["/opt/homebrew/opt/libomp/lib/libomp.dylib", "/usr/local/opt/libomp/lib/libomp.dylib"];
/// `dyld`'s own fallback search, kept after the app's directory.
const DEFAULT_FALLBACK: &str = "/usr/local/lib:/usr/lib";

/// The app's library directory: `<app data>/lib`.
pub fn lib_dir(app: &tauri::AppHandle) -> Result<PathBuf, String> {
    let data = app.path().app_data_dir().map_err(|e| format!("No app data directory: {e}"))?;
    Ok(data.join("lib"))
}

/// Whether PEA can find libomp without the app's help.
fn system_has_libomp() -> bool {
    SYSTEM_PATHS.iter().any(|p| Path::new(p).is_file())
}

/// macOS release names in Homebrew bottle tags, by major version, oldest first.
const MACOS_NAMES: &[(u32, &str)] = &[(12, "monterey"), (13, "ventura"), (14, "sonoma"), (15, "sequoia"), (26, "tahoe")];

/// The bottle to use from the formula's `bottle.stable.files` keys, for this
/// Mac's architecture and macOS major version: its own release if there is a
/// bottle for it, else the newest release older than it (older builds load on
/// newer macOS, not the reverse), else the oldest one there is.
pub(crate) fn pick_bottle<'a>(keys: &[&'a str], arm64: bool, macos_major: u32) -> Option<&'a str> {
    let tag = |name: &str| if arm64 { format!("arm64_{name}") } else { name.to_string() };
    let known: Vec<(u32, &'a str)> = MACOS_NAMES
        .iter()
        .filter_map(|(major, name)| keys.iter().find(|k| **k == tag(name)).map(|k| (*major, *k)))
        .collect();
    known
        .iter()
        .rev()
        .find(|(major, _)| *major <= macos_major)
        .or_else(|| known.first())
        .map(|(_, k)| *k)
}

/// The running macOS major version (`sw_vers -productVersion`), e.g. 26.
fn macos_major() -> Option<u32> {
    let out = std::process::Command::new("sw_vers").arg("-productVersion").output().ok()?;
    String::from_utf8_lossy(&out.stdout).trim().split('.').next()?.parse().ok()
}

/// `libomp.dylib` out of a bottle (`libomp/<version>/lib/libomp.dylib`).
pub(crate) fn extract_dylib(bottle_tgz: &[u8]) -> Result<Vec<u8>, String> {
    let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(bottle_tgz));
    for entry in archive.entries().map_err(|e| format!("Unreadable bottle: {e}"))? {
        let mut entry = entry.map_err(|e| format!("Unreadable bottle: {e}"))?;
        let path = entry.path().map_err(|e| e.to_string())?.into_owned();
        if path.ends_with(Path::new("lib").join(LIB_NAME)) {
            let mut bytes = Vec::new();
            entry.read_to_end(&mut bytes).map_err(|e| e.to_string())?;
            return Ok(bytes);
        }
    }
    Err(format!("The libomp bottle has no lib/{LIB_NAME}"))
}

/// Download the bottle for this Mac and put `libomp.dylib` in `dir`.
async fn fetch_libomp(dir: &Path) -> Result<String, String> {
    let client = reqwest::Client::new();
    let formula: serde_json::Value = client
        .get(FORMULA_URL)
        .send()
        .await
        .and_then(|r| r.error_for_status())
        .map_err(|e| format!("Cannot read Homebrew's libomp entry: {e}"))?
        .json()
        .await
        .map_err(|e| format!("Unexpected Homebrew libomp entry: {e}"))?;
    let version = formula["versions"]["stable"].as_str().unwrap_or("?").to_string();
    let files = formula["bottle"]["stable"]["files"].as_object().ok_or("Homebrew lists no libomp bottles")?;
    let keys: Vec<&str> = files.keys().map(String::as_str).collect();
    let arm64 = cfg!(target_arch = "aarch64");
    let major = macos_major().unwrap_or(15);
    let key = pick_bottle(&keys, arm64, major).ok_or_else(|| {
        format!("Homebrew has no libomp build for this Mac ({}); install it with `brew install libomp`", if arm64 { "Apple Silicon" } else { "Intel" })
    })?;
    let url = files[key]["url"].as_str().ok_or("libomp bottle has no URL")?;
    let sha = files[key]["sha256"].as_str().ok_or("libomp bottle has no checksum")?;

    // Homebrew's own anonymous token for its public GitHub Packages bottles.
    let bytes = client
        .get(url)
        .header("Authorization", "Bearer QQ==")
        .send()
        .await
        .and_then(|r| r.error_for_status())
        .map_err(|e| format!("Cannot download libomp: {e}"))?
        .bytes()
        .await
        .map_err(|e| format!("Cannot download libomp: {e}"))?;
    let got = format!("{:x}", Sha256::digest(&bytes));
    if !got.eq_ignore_ascii_case(sha) {
        return Err(format!("The libomp download failed its checksum (expected {sha}, got {got})"));
    }
    let dylib = extract_dylib(&bytes)?;

    std::fs::create_dir_all(dir).map_err(|e| format!("Cannot create {}: {e}", dir.display()))?;
    // Written beside, then renamed, so a half-written file is never loaded.
    let part = dir.join(format!("{LIB_NAME}.part"));
    std::fs::write(&part, dylib).map_err(|e| format!("Cannot write {}: {e}", part.display()))?;
    std::fs::rename(&part, dir.join(LIB_NAME)).map_err(|e| e.to_string())?;
    Ok(format!("{version} ({key})"))
}

/// Make sure PEA will find libomp: present on the system, already fetched, or
/// fetched now. A no-op off macOS. Reports what it did to the Console.
pub async fn ensure_libomp(app: &tauri::AppHandle) -> Result<(), String> {
    if !cfg!(target_os = "macos") || system_has_libomp() {
        return Ok(());
    }
    let dir = lib_dir(app)?;
    if dir.join(LIB_NAME).is_file() {
        return Ok(());
    }
    app.emit("console-line", "PEA needs the OpenMP library (libomp), which this Mac does not have. Downloading it from Homebrew...").ok();
    match fetch_libomp(&dir).await {
        Ok(what) => {
            app.emit("console-line", format!("libomp {what} saved to {}", dir.display())).ok();
            Ok(())
        }
        Err(e) => {
            app.emit("console-line", format!("Could not get libomp: {e}. PEA will not start until it is installed (`brew install libomp`).")).ok();
            Err(e)
        }
    }
}

/// Fetch anything PEA needs that is missing, at startup.
#[tauri::command]
pub async fn ensure_pea_runtime(app: tauri::AppHandle) -> Result<(), String> {
    ensure_libomp(&app).await
}

/// Point a PEA process at the app's library directory (macOS only), after any
/// fallback path it already has.
pub fn apply_env(cmd: &mut tokio::process::Command, app: &tauri::AppHandle) {
    if !cfg!(target_os = "macos") {
        return;
    }
    let Ok(dir) = lib_dir(app) else { return };
    let existing = std::env::var("DYLD_FALLBACK_LIBRARY_PATH").unwrap_or_else(|_| DEFAULT_FALLBACK.to_string());
    cmd.env("DYLD_FALLBACK_LIBRARY_PATH", format!("{}:{existing}", dir.display()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn picks_this_macs_bottle_or_the_newest_older_one() {
        let keys = ["arm64_golden_gate", "arm64_tahoe", "arm64_sequoia", "arm64_linux", "x86_64_linux"];
        assert_eq!(pick_bottle(&keys, true, 26), Some("arm64_tahoe"));
        assert_eq!(pick_bottle(&keys, true, 15), Some("arm64_sequoia"));
        // Older than every bottle: the oldest one is the best chance.
        assert_eq!(pick_bottle(&keys, true, 14), Some("arm64_sequoia"));
        // A release newer than any known name: the newest known bottle.
        assert_eq!(pick_bottle(&keys, true, 27), Some("arm64_tahoe"));
        // No Intel macOS bottles here.
        assert_eq!(pick_bottle(&keys, false, 15), None);
        assert_eq!(pick_bottle(&["sonoma", "ventura"], false, 15), Some("sonoma"));
    }

    /// Needs the network: `cargo test fetches_the_real_bottle -- --ignored`.
    #[cfg(target_os = "macos")]
    #[tokio::test]
    #[ignore]
    async fn fetches_the_real_bottle() {
        let dir = std::env::temp_dir().join(format!("ginan-libomp-{}", std::process::id()));
        let what = fetch_libomp(&dir).await.expect("fetch");
        let bytes = std::fs::read(dir.join(LIB_NAME)).unwrap();
        // A Mach-O 64-bit binary.
        assert_eq!(&bytes[..4], &[0xcf, 0xfa, 0xed, 0xfe], "{what}");
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn takes_only_the_dylib_out_of_a_bottle() {
        let mut tgz = Vec::new();
        {
            let gz = flate2::write::GzEncoder::new(&mut tgz, flate2::Compression::fast());
            let mut b = tar::Builder::new(gz);
            for (path, body) in [
                ("libomp/23.1.4/include/omp.h", &b"header"[..]),
                ("libomp/23.1.4/lib/libomp.a", b"static"),
                ("libomp/23.1.4/lib/libomp.dylib", b"dylib bytes"),
            ] {
                let mut h = tar::Header::new_gnu();
                h.set_size(body.len() as u64);
                h.set_mode(0o644);
                h.set_cksum();
                b.append_data(&mut h, path, body).unwrap();
            }
            b.into_inner().unwrap().finish().unwrap();
        }
        assert_eq!(extract_dylib(&tgz).unwrap(), b"dylib bytes");
        assert!(extract_dylib(b"not a tarball").is_err());
    }
}
