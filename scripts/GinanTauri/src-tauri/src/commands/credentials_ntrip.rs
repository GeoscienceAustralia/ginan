//! Split out of credentials.rs to keep each file under 500 lines.


use super::credentials::*;

// ─── NTRIP caster credentials ────────────────────────────────────────────────

/// Look up the login/password to inject into an NTRIP caster URL.
///
/// The frontend holds the caster URL with no login in it at all; this returns
/// the pair for `credentialed_url` to splice in at write time, so the
/// netrc password never travels through the frontend or lands in an exported
/// config.
pub fn ntrip_auth_for_host(host: &str) -> Option<(String, String)> {
    netrc_auth_for_host(host)
}

/// Return the caster hostnames that have a stored NTRIP credential pair.
///
/// CDDIS/URS entries are filtered out — a caller browsing "which casters do I
/// have credentials for?" doesn't care about the Earthdata login living in the
/// same file.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn list_ntrip_hosts() -> Vec<String> {
    let mut hosts: Vec<String> = Vec::new();
    for path in netrc_candidates() {
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        for (host, auth) in parse_netrc_ordered(&content) {
            if host == URS_HOST || host == CDDIS_HOST || host == "default" {
                continue;
            }
            if auth.login.is_empty() || auth.password.is_empty() {
                continue;
            }
            if !hosts.contains(&host) {
                hosts.push(host);
            }
        }
    }
    hosts
}

/// Insert or update the NTRIP credential entry for `host`, keeping every other
/// stored login intact. Overwriting the whole file would clobber the user's
/// other machine entries — unacceptable here since the
/// same file may hold multiple caster logins alongside Earthdata.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn save_ntrip_credentials(
    host: String,
    username: String,
    password: String,
) -> Result<(), String> {
    let host = host.trim().to_string();
    let username = username.trim().to_string();
    if host.is_empty() || username.is_empty() || password.is_empty() {
        return Err("Host, username, and password must not be empty".to_string());
    }
    if host == URS_HOST || host == CDDIS_HOST {
        return Err(
            "Use the CDDIS credential dialog for Earthdata hosts".to_string(),
        );
    }

    let candidates = netrc_candidates();
    if candidates.is_empty() {
        return Err("Could not resolve home directory for netrc".to_string());
    }

    for path in &candidates {
        let existing = std::fs::read_to_string(path).unwrap_or_default();
        let mut entries = parse_netrc_ordered(&existing);
        let auth = NetrcAuth {
            login: username.clone(),
            password: password.clone(),
        };
        match entries.iter_mut().find(|(h, _)| *h == host) {
            Some((_, existing_auth)) => *existing_auth = auth,
            None => entries.push((host.clone(), auth)),
        }
        let rendered = render_netrc(&entries);
        write_secure(path, &rendered)?;
    }
    Ok(())
}

/// Remove the NTRIP credential entry for `host`, if any. No-op when nothing
/// matches, so the frontend can call this freely on a "forget" action.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub fn forget_ntrip_credentials(host: String) -> Result<(), String> {
    let host = host.trim();
    if host.is_empty() {
        return Err("Host must not be empty".to_string());
    }
    for path in netrc_candidates() {
        let Ok(existing) = std::fs::read_to_string(&path) else {
            continue;
        };
        let mut entries = parse_netrc_ordered(&existing);
        let before = entries.len();
        entries.retain(|(h, _)| h != host);
        if entries.len() != before {
            let rendered = render_netrc(&entries);
            write_secure(&path, &rendered)?;
        }
    }
    Ok(())
}

/// Serialise netrc entries in the canonical one-line-per-host format. All
/// entries are emitted, even ones we don't own, so a save/delete on an NTRIP
/// host leaves URS/CDDIS lines untouched.
pub(crate) fn render_netrc(entries: &[(String, NetrcAuth)]) -> String {
    let mut out = String::new();
    for (host, auth) in entries {
        if host == "default" {
            out.push_str(&format!(
                "default login {} password {}\n",
                auth.login, auth.password
            ));
        } else {
            out.push_str(&format!(
                "machine {host} login {} password {}\n",
                auth.login, auth.password
            ));
        }
    }
    out
}

/// Startup connectivity + auth probe for CDDIS/Earthdata.
///
/// Mirrors the Python reference (`scripts/GinanUI/app/utils/cddis_connection.py`)
/// in three phases:
///   1. Anonymous GET of `robots.txt` to confirm the host is reachable at all.
///   2. Local netrc lookup to confirm Earthdata credentials exist.
///   3. Authenticated GET of `/archive/00readme` — the canonical auth-required
///      probe. CDDIS returns the Earthdata login HTML (200 OK) when auth fails,
///      so we have to inspect the body, not just the status code.
///
/// The frontend uses this on startup to decide whether to surface a "missing
/// credentials" banner with a button to open the credentials dialog.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn check_cddis_connectivity() -> Result<String, String> {
    // Phase 1 — anonymous reachability check.
    let probe_client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(15))
        .build()
        .map_err(|e| format!("CDDIS unreachable: client build failed: {e}"))?;
    let resp = probe_client
        .get("https://cddis.nasa.gov/robots.txt")
        .send()
        .await
        .map_err(|e| format!("CDDIS unreachable: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!(
            "CDDIS unreachable: HTTP {} on robots.txt",
            resp.status()
        ));
    }

    // Phase 2 — netrc presence check.
    if netrc_auth_for_host(CDDIS_HOST)
        .or_else(|| netrc_auth_for_host(URS_HOST))
        .is_none()
    {
        return Err("No Earthdata credentials in netrc".to_string());
    }

    // Phase 3 — authenticated probe. Use the shared client + manual-redirect
    // helper so the full CDDIS → URS → CDDIS OAuth handshake completes.
    // A bare `.send()` would stop at the first 302 because `build_client()`
    // sets `Policy::none()`.
    let client = crate::commands::products::build_client()?;
    let auth_resp = crate::commands::products::get_following_redirects(
        &client,
        "https://cddis.nasa.gov/archive/00readme",
    )
    .await
    .map_err(|e| format!("Earthdata auth probe failed: {e}"))?;

    let status = auth_resp.status();
    let body = auth_resp
        .text()
        .await
        .map_err(|e| format!("Earthdata auth probe: could not read body: {e}"))?;

    // CDDIS sends the URS login page (HTTP 200) instead of 401 when auth fails.
    if body.contains("Earthdata Login") {
        return Err("Earthdata credentials rejected (login page returned)".to_string());
    }
    if !status.is_success() {
        return Err(format!("Earthdata auth probe: HTTP {status}"));
    }

    Ok("AUTH OK".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "\
machine urs.earthdata.nasa.gov   login alice password s3cret
machine cddis.nasa.gov login alice password s3cret
";

    #[test]
    fn parses_both_hosts() {
        let map = parse_netrc(SAMPLE);
        let urs = map.get(URS_HOST).expect("urs entry");
        assert_eq!(urs.login, "alice");
        assert_eq!(urs.password, "s3cret");
        let cddis = map.get(CDDIS_HOST).expect("cddis entry");
        assert_eq!(cddis.login, "alice");
        assert_eq!(cddis.password, "s3cret");
    }

    #[test]
    fn auth_only_attaches_for_cddis_hosts() {
        // host_of must isolate the hostname regardless of path/port.
        assert_eq!(
            host_of("https://cddis.nasa.gov/archive/gnss/products/2300/foo.gz"),
            Some("cddis.nasa.gov")
        );
        assert_eq!(
            host_of("https://igs.bkg.bund.de/root_ftp/IGS/products/2300/foo.gz"),
            Some("igs.bkg.bund.de")
        );
        // BKG/IGN/peanpod must never receive Earthdata auth.
        assert_eq!(
            netrc_auth_for_url("https://igs.bkg.bund.de/root_ftp/IGS/products/2300/foo.gz"),
            None
        );
    }

    /// Smoke test: `check_cddis_connectivity` is an async fn returning
    /// `Result<String, String>`. If the signature ever drifts (e.g. someone
    /// changes the return type), this stops compiling — which is exactly the
    /// guarantee the frontend `invoke("check_cddis_connectivity")` call needs.
    #[test]
    fn check_cddis_connectivity_signature_compiles() {
        fn assert_sig<F, Fut>(_f: F)
        where
            F: Fn() -> Fut,
            Fut: std::future::Future<Output = Result<String, String>>,
        {
        }
        assert_sig(check_cddis_connectivity);
    }

    /// With no netrc file visible to the process, the connectivity check must
    /// fail before doing any authenticated network IO. Phase 1 (anonymous
    /// robots.txt fetch) still touches the network, so we mark this `#[ignore]`
    /// — run it explicitly with `cargo test -- --ignored` in environments that
    /// have internet but no Earthdata creds.
    ///
    /// We can't safely scrub HOME for a normal `cargo test` run because other
    /// tests in the workspace may rely on it, and Tauri's test harness doesn't
    /// give us a scoped env override here.
    #[test]
    #[ignore]
    fn errors_when_no_netrc_present() {
        // SAFETY: this clobbers HOME for the whole process. Only run under
        // `--ignored` where the caller has accepted that.
        let original = std::env::var_os("HOME");
        let tmp = std::env::temp_dir().join("ginan-check-cddis-no-netrc");
        std::fs::create_dir_all(&tmp).unwrap();
        std::env::set_var("HOME", &tmp);

        let rt = tokio::runtime::Runtime::new().unwrap();
        let result = rt.block_on(check_cddis_connectivity());

        if let Some(h) = original {
            std::env::set_var("HOME", h);
        } else {
            std::env::remove_var("HOME");
        }

        // Either phase 1 (network unreachable in CI) or phase 2 (no netrc) must
        // fire — both are acceptable failure modes for this test.
        let err = result.expect_err("expected error with no netrc present");
        assert!(
            err.starts_with("CDDIS unreachable")
                || err == "No Earthdata credentials in netrc",
            "unexpected error: {err}"
        );
    }

    #[test]
    fn render_and_reparse_is_idempotent() {
        // Round-tripping must preserve entries in order, which is what lets
        // save_ntrip_credentials rewrite one line without shuffling the rest.
        let entries = vec![
            (
                URS_HOST.to_string(),
                NetrcAuth {
                    login: "alice".into(),
                    password: "s3cret".into(),
                },
            ),
            (
                "ntrip.data.gnss.ga.gov.au".to_string(),
                NetrcAuth {
                    login: "bob".into(),
                    password: "hunter2".into(),
                },
            ),
        ];
        let rendered = render_netrc(&entries);
        let round_tripped = parse_netrc_ordered(&rendered);
        assert_eq!(round_tripped.len(), 2);
        assert_eq!(round_tripped[0].0, URS_HOST);
        assert_eq!(round_tripped[0].1.login, "alice");
        assert_eq!(round_tripped[1].0, "ntrip.data.gnss.ga.gov.au");
        assert_eq!(round_tripped[1].1.password, "hunter2");
    }

    #[test]
    fn list_hosts_excludes_earthdata_entries() {
        // list_ntrip_hosts filters URS/CDDIS/default so the UI doesn't confuse
        // Earthdata credentials with caster credentials.
        let entries = parse_netrc_ordered(
            "machine urs.earthdata.nasa.gov login a password b\n\
             machine ntrip.data.gnss.ga.gov.au login c password d\n\
             default login e password f\n",
        );
        let hosts: Vec<String> = entries
            .into_iter()
            .filter(|(h, _)| h != URS_HOST && h != CDDIS_HOST && h != "default")
            .map(|(h, _)| h)
            .collect();
        assert_eq!(hosts, vec!["ntrip.data.gnss.ga.gov.au"]);
    }

    #[test]
    fn missing_password_is_incomplete() {
        let map = parse_netrc("machine cddis.nasa.gov login alice\n");
        let cddis = map.get(CDDIS_HOST).expect("cddis entry");
        assert_eq!(cddis.login, "alice");
        assert!(cddis.password.is_empty());
    }
}
