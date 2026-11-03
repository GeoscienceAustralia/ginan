//! Earthdata changes must not remove the caster logins in the same netrc.
use super::credentials::{
    netrc_candidates, parse_netrc_ordered, write_secure, NetrcAuth, CDDIS_HOST, URS_HOST,
};
use super::credentials_ntrip::render_netrc;
use std::path::PathBuf;

#[tauri::command]
pub fn forget_cddis_credentials() -> Result<(), String> {
    update_files(&netrc_candidates(), None)
}

pub(crate) fn update_files(paths: &[PathBuf], login: Option<(&str, &str)>) -> Result<(), String> {
    if paths.is_empty() {
        return Err("Could not resolve home directory for netrc".to_string());
    }
    for path in paths {
        let existing = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                if login.is_none() {
                    continue;
                }
                String::new()
            }
            Err(e) => return Err(format!("Cannot read netrc: {e}")),
        };
        let mut entries = parse_netrc_ordered(&existing);
        let before = entries.len();
        entries.retain(|(host, _)| host != URS_HOST && host != CDDIS_HOST);
        let removed = before != entries.len();
        if let Some((username, password)) = login {
            for host in [URS_HOST, CDDIS_HOST] {
                entries.push((
                    host.to_string(),
                    NetrcAuth {
                        login: username.to_string(),
                        password: password.to_string(),
                    },
                ));
            }
        }
        if removed || login.is_some() {
            write_secure(path, &render_netrc(&entries))?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_dir() -> PathBuf {
        std::env::temp_dir().join(format!(
            "ginan-earthdata-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
    }

    #[test]
    fn earthdata_save_and_sign_out_preserve_other_logins_in_both_files() {
        let dir = temp_dir();
        std::fs::create_dir_all(&dir).unwrap();
        let paths = [dir.join(".netrc"), dir.join("_netrc")];
        for path in &paths {
            std::fs::write(path, "machine caster.example login caster password fixture\nmachine urs.earthdata.nasa.gov login old password old\nmachine cddis.nasa.gov login old password old\ndefault login fallback password fixture\n").unwrap();
        }
        update_files(&paths, Some(("new-user", "new-fixture"))).unwrap();
        for path in &paths {
            let entries = parse_netrc_ordered(&std::fs::read_to_string(path).unwrap());
            assert_eq!(entries.len(), 4);
            for host in [URS_HOST, CDDIS_HOST] {
                let auth = &entries.iter().find(|(h, _)| h == host).unwrap().1;
                assert_eq!(auth.login, "new-user");
                assert_eq!(auth.password, "new-fixture");
            }
        }
        update_files(&paths, None).unwrap();
        update_files(&paths, None).unwrap(); // Already signed out is harmless.
        for path in &paths {
            let entries = parse_netrc_ordered(&std::fs::read_to_string(path).unwrap());
            assert_eq!(entries.len(), 2);
            assert_eq!(entries[0].0, "caster.example");
            assert_eq!(entries[0].1.password, "fixture");
            assert_eq!(entries[1].0, "default");
        }
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn unreadable_netrc_is_not_replaced() {
        let dir = temp_dir();
        std::fs::create_dir_all(&dir).unwrap();
        // A directory at the file path consistently fails to read on every OS.
        assert!(update_files(&[dir.clone()], Some(("user", "fixture"))).is_err());
        assert!(dir.is_dir());
        std::fs::remove_dir(dir).unwrap();
    }
}
