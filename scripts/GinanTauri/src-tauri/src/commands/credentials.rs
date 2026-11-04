//! NASA Earthdata / CDDIS credential management.
//!
//! Credentials live in the user's netrc file (`~/.netrc`, plus `_netrc` on
//! Windows) under the hosts `urs.earthdata.nasa.gov` and `cddis.nasa.gov`. This
//! is the *same* file the Python sibling app (GinanUI) reads and writes, so a
//! user who configured credentials in either app gets them in the other for
//! free. The netrc line format we emit matches GinanUI exactly:
//!
//! ```text
//! machine urs.earthdata.nasa.gov   login <user> password <pass>
//! machine cddis.nasa.gov login <user> password <pass>
//! ```
//!
//! NTRIP caster logins live in the same file, keyed by caster hostname, so a
//! single place holds every credential the app needs.


use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::Serialize;

pub const URS_HOST: &str = "urs.earthdata.nasa.gov";
pub const CDDIS_HOST: &str = "cddis.nasa.gov";

/// Result of a credential validation check, surfaced to the frontend so it can
/// decide whether to open the credentials modal.
#[derive(Debug, Serialize)]
pub struct CredentialStatus {
    pub ok: bool,
    /// Path of the netrc file that was inspected (if any).
    #[serde(rename = "where")]
    pub location: Option<String>,
    /// Human-readable reason when `ok` is false.
    pub why: Option<String>,
}

/// A single host's login/password as parsed from netrc.
#[derive(Debug, Clone)]
pub struct NetrcAuth {
    pub login: String,
    pub password: String,
}

/// Candidate netrc paths for this platform, in preference order.
///
/// Windows is inconsistent about which name tools read, so both `.netrc` and
/// `_netrc` under `%USERPROFILE%` are returned (mirrors GinanUI).
pub fn netrc_candidates() -> Vec<PathBuf> {
    if cfg!(windows) {
        match win_home() {
            Some(home) => vec![home.join(".netrc"), home.join("_netrc")],
            None => Vec::new(),
        }
    } else {
        match std::env::var_os("HOME") {
            Some(h) => vec![PathBuf::from(h).join(".netrc")],
            None => Vec::new(),
        }
    }
}

#[cfg(windows)]
pub(crate) fn win_home() -> Option<PathBuf> {
    std::env::var_os("USERPROFILE")
        .or_else(|| std::env::var_os("HOME"))
        .map(PathBuf::from)
}

#[cfg(not(windows))]
pub(crate) fn win_home() -> Option<PathBuf> {
    None
}

/// Parse netrc content into a `host -> auth` map.
///
/// netrc is a flat token stream; `machine <name>` opens a block and the
/// following `login`/`password` tokens belong to it until the next `machine`
/// (or `default`). `macdef` macros are not emitted by us and are not supported.
pub(crate) fn parse_netrc(content: &str) -> HashMap<String, NetrcAuth> {
    parse_netrc_ordered(content).into_iter().collect()
}

/// Parse netrc preserving the order entries appear in, so a rewrite can keep the
/// user's existing file layout.
pub(crate) fn parse_netrc_ordered(content: &str) -> Vec<(String, NetrcAuth)> {
    let mut entries: Vec<(String, NetrcAuth)> = Vec::new();
    let mut tokens = content.split_whitespace();

    let mut machine: Option<String> = None;
    let mut login: Option<String> = None;
    let mut password: Option<String> = None;

    fn flush(
        entries: &mut Vec<(String, NetrcAuth)>,
        machine: &mut Option<String>,
        login: &mut Option<String>,
        password: &mut Option<String>,
    ) {
        if let Some(host) = machine.take() {
            let auth = NetrcAuth {
                login: login.take().unwrap_or_default(),
                password: password.take().unwrap_or_default(),
            };
            match entries.iter_mut().find(|(h, _)| *h == host) {
                Some((_, existing)) => *existing = auth,
                None => entries.push((host, auth)),
            }
        }
        *login = None;
        *password = None;
    }

    while let Some(tok) = tokens.next() {
        match tok {
            "machine" => {
                flush(&mut entries, &mut machine, &mut login, &mut password);
                machine = tokens.next().map(str::to_string);
            }
            "default" => {
                flush(&mut entries, &mut machine, &mut login, &mut password);
                machine = Some("default".to_string());
            }
            "login" => login = tokens.next().map(str::to_string),
            "password" => password = tokens.next().map(str::to_string),
            // `account` carries one value we don't use; skip it.
            "account" => {
                let _ = tokens.next();
            }
            _ => {}
        }
    }
    flush(&mut entries, &mut machine, &mut login, &mut password);
    entries
}

/// Read netrc and return Basic-auth credentials for `host`, if both fields are
/// present and non-empty. The first candidate file that yields a hit wins.
pub fn netrc_auth_for_host(host: &str) -> Option<(String, String)> {
    for path in netrc_candidates() {
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        let map = parse_netrc(&content);
        if let Some(auth) = map.get(host) {
            if !auth.login.is_empty() && !auth.password.is_empty() {
                return Some((auth.login.clone(), auth.password.clone()));
            }
        }
    }
    None
}

/// Extract the hostname (no scheme, no userinfo, no port, no path) from a URL.
pub fn ntrip_host_from_url(url: &str) -> Option<String> {
    let (_, after_scheme) = url.split_once("://")?;
    let authority = after_scheme.split('/').next()?;
    let no_userinfo = authority.rsplit('@').next()?;
    let no_port = no_userinfo.split(':').next()?;
    if no_port.is_empty() {
        None
    } else {
        Some(no_port.to_string())
    }
}

/// Return credentials to attach to a request for `url`, but only when the URL
/// targets a CDDIS/Earthdata host. Other hosts (BKG, IGN, GA, peanpod) stay
/// anonymous. CDDIS credentials are tried first, falling back to the URS entry
/// since both carry the same Earthdata login in practice.
pub fn netrc_auth_for_url(url: &str) -> Option<(String, String)> {
    let host = host_of(url)?;
    if host == CDDIS_HOST || host == URS_HOST {
        netrc_auth_for_host(CDDIS_HOST).or_else(|| netrc_auth_for_host(URS_HOST))
    } else {
        None
    }
}

pub(crate) fn host_of(url: &str) -> Option<&str> {
    url.splitn(3, '/')
        .nth(2)
        .map(|s| s.split('/').next().unwrap_or(s))
        .map(|s| s.split(':').next().unwrap_or(s))
}

pub(crate) fn write_secure(path: &Path, content: &str) -> Result<(), String> {
    std::fs::write(path, content).map_err(|e| format!("write {}: {e}", path.display()))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o600))
            .map_err(|e| format!("chmod {}: {e}", path.display()))?;
    }
    Ok(())
}

// ─── Tauri commands ──────────────────────────────────────────────────────────

/// Check that a usable Earthdata login exists in netrc for both URS and CDDIS.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn validate_cddis_credentials() -> CredentialStatus {
    let candidates = netrc_candidates();
    let path = match candidates.iter().find(|p| p.exists()) {
        Some(p) => p.clone(),
        None => {
            return CredentialStatus {
                ok: false,
                location: candidates.first().map(|p| p.display().to_string()),
                why: Some("no netrc file found".to_string()),
            };
        }
    };

    let content = match std::fs::read_to_string(&path) {
        Ok(c) => c,
        Err(e) => {
            return CredentialStatus {
                ok: false,
                location: Some(path.display().to_string()),
                why: Some(format!("cannot read netrc: {e}")),
            };
        }
    };

    let map = parse_netrc(&content);
    for host in [URS_HOST, CDDIS_HOST] {
        let ok = map
            .get(host)
            .map(|a| !a.login.is_empty() && !a.password.is_empty())
            .unwrap_or(false);
        if !ok {
            return CredentialStatus {
                ok: false,
                location: Some(path.display().to_string()),
                why: Some(format!("missing credentials for {host}")),
            };
        }
    }

    CredentialStatus {
        ok: true,
        location: Some(path.display().to_string()),
        why: None,
    }
}

/// Write Earthdata credentials to every candidate netrc path, with `0600`
/// permissions on POSIX. The password is never persisted anywhere else.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn save_cddis_credentials(username: String, password: String) -> Result<(), String> {
    let username = username.trim();
    if username.is_empty() || password.is_empty() {
        return Err("Username and password must not be empty".to_string());
    }

    let candidates = netrc_candidates();
    if candidates.is_empty() {
        return Err("Could not resolve home directory for netrc".to_string());
    }
    super::credentials_earthdata::update_files(&candidates, Some((username, &password)))
}

/// Probe CDDIS with the stored credentials to confirm they authenticate.
///
/// The full CDDIS → URS → CDDIS OAuth handshake starts with a 302, so we must
/// follow redirects manually via `get_following_redirects`: `build_client()`
/// sets `Policy::none()`, so a bare `.send()` stops at the first 302 and never
/// completes the handshake. The cookie jar carries the URS session back to
/// CDDIS across the hops.
///
/// CDDIS also serves the Earthdata login HTML with HTTP 200 — not a 401 — when
/// auth fails, so we have to inspect the body, mirroring `check_cddis_connectivity`.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn test_cddis_connection() -> Result<(), String> {
    const PROBE_URL: &str = "https://cddis.nasa.gov/archive/gnss/products/";

    if netrc_auth_for_host(CDDIS_HOST)
        .or_else(|| netrc_auth_for_host(URS_HOST))
        .is_none()
    {
        return Err("No CDDIS credentials found in netrc".to_string());
    }

    let client = crate::commands::products::build_client()?;
    let resp = crate::commands::products::get_following_redirects(&client, PROBE_URL)
        .await
        .map_err(|e| format!("connection failed: {e}"))?;

    let status = resp.status();
    if status == reqwest::StatusCode::UNAUTHORIZED || status == reqwest::StatusCode::FORBIDDEN {
        return Err(format!(
            "Authentication failed (HTTP {status}). Check your Earthdata username and password."
        ));
    }

    let body = resp
        .text()
        .await
        .map_err(|e| format!("could not read CDDIS response: {e}"))?;

    // CDDIS returns the URS login page (HTTP 200) instead of 401 when auth fails.
    if body.contains("Earthdata Login") {
        return Err(
            "Authentication failed. Check your Earthdata username and password.".to_string(),
        );
    }

    if status.is_success() {
        Ok(())
    } else {
        Err(format!("CDDIS returned HTTP {status}"))
    }
}
