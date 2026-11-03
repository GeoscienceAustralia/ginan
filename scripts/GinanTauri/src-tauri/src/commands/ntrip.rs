//! NTRIP sourcetable lookup.
//!
//! An NTRIP caster answers a plain `GET /` with a sourcetable: one `STR;`-
//! prefixed line per mountpoint, semicolon-delimited, terminated by
//! `ENDSOURCETABLE`. This lets the realtime config UI offer a dropdown of real
//! mountpoints (split into observation vs. SSR/nav streams) instead of asking
//! the user to type them from memory.
//!
//! Reference: NTRIP 2.0 spec §7 (sourcetable format). Field order:
//! `STR;mountpoint;identifier;format;format-details;carrier;nav-system;
//! network;country;latitude;longitude;nmea;solution;generator;
//! compression-encryption;authentication;fee;bitrate;misc`.

use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct NtripStream {
    pub mountpoint: String,
    pub identifier: String,
    pub format: String,
    pub format_details: String,
    pub carrier: String,
    pub nav_system: String,
    pub network: String,
    pub country: String,
    /// "obs", "ssr", or "other" — a best-effort guess from `format_details`.
    pub category: String,
}

/// Message-type substrings that mark a stream as SSR/orbit-clock corrections
/// rather than raw observations. RTCM SSR messages fall in the 1057-1068 and
/// 1240+ ranges; MSM/legacy observation messages don't include these tokens.
const SSR_HINTS: [&str; 3] = ["SSR", "ORBIT", "CLOCK"];
const OBS_HINTS: [&str; 2] = ["MSM", "OBS"];

fn classify(format_details: &str, identifier: &str) -> &'static str {
    let haystack = format!("{format_details} {identifier}").to_uppercase();
    if SSR_HINTS.iter().any(|h| haystack.contains(h)) {
        "ssr"
    } else if OBS_HINTS.iter().any(|h| haystack.contains(h))
        || contains_obs_message_number(&haystack)
    {
        "obs"
    } else {
        "other"
    }
}

/// RTCM3 MSM (1071-1127) and legacy observation (1001-1004, 1009-1012)
/// message numbers, as they appear comma-separated in `format-details`.
fn contains_obs_message_number(haystack: &str) -> bool {
    haystack.split(|c: char| !c.is_ascii_digit()).any(|tok| {
        tok.parse::<u32>()
            .map(|n| (1071..=1127).contains(&n) || (1001..=1004).contains(&n) || (1009..=1012).contains(&n))
            .unwrap_or(false)
    })
}

fn parse_sourcetable(body: &str) -> Vec<NtripStream> {
    body.lines()
        .filter(|l| l.starts_with("STR;"))
        .filter_map(|line| {
            let f: Vec<&str> = line.split(';').collect();
            let get = |i: usize| f.get(i).copied().unwrap_or("").to_string();
            let mountpoint = get(1);
            if mountpoint.is_empty() {
                return None;
            }
            let format_details = get(4);
            let identifier = get(2);
            let category = classify(&format_details, &identifier).to_string();
            Some(NtripStream {
                mountpoint,
                identifier,
                format: get(3),
                format_details,
                carrier: get(5),
                nav_system: get(6),
                network: get(7),
                country: get(8),
                category,
            })
        })
        .collect()
}

/// Fetch and parse the sourcetable from a caster's root URL, exactly as entered
/// in the "Caster URL" field. Only the scheme and hostname are used: listing
/// mountpoints on a public caster needs no auth, so anything else the URL
/// happens to carry — userinfo, a path — is discarded rather than resolved.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn fetch_ntrip_sourcetable(caster_url: String) -> Result<Vec<NtripStream>, String> {
    let host = super::credentials::ntrip_host_from_url(&caster_url)
        .ok_or_else(|| "Could not extract a hostname from the caster URL".to_string())?;
    let scheme = if caster_url.starts_with("https://") {
        "https"
    } else {
        "http"
    };
    let root = format!("{scheme}://{host}/");

    let client = reqwest::Client::builder()
        .user_agent("NTRIP GinanTauri/1.0")
        .timeout(std::time::Duration::from_secs(15))
        .build()
        .map_err(|e| format!("HTTP client init failed: {e}"))?;

    let mut req = client.get(&root).header("Ntrip-Version", "Ntrip/2.0");
    if let Some((user, pass)) = super::credentials_ntrip::ntrip_auth_for_host(&host) {
        req = req.basic_auth(user, Some(pass));
    }

    let resp = req
        .send()
        .await
        .map_err(|e| format!("Could not reach {root}: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!("{root} returned HTTP {}", resp.status()));
    }
    let body = resp
        .text()
        .await
        .map_err(|e| format!("Could not read sourcetable response: {e}"))?;

    let streams = parse_sourcetable(&body);
    if streams.is_empty() {
        return Err("No STR; entries found in the caster's sourcetable".to_string());
    }
    Ok(streams)
}

/// Outcome of an NTRIP credential check, as the UI needs to tell them apart.
#[derive(serde::Serialize, Debug, PartialEq)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum NtripAuthResult {
    /// The caster accepted the login and started a stream.
    Accepted { host: String, mountpoint: String },
    /// The caster answered, and said no. The login is wrong.
    Rejected { host: String, detail: String },
    /// Neither — the caster could not be asked, or did not answer usefully.
    /// Deliberately distinct from `Rejected`: telling someone their password is
    /// wrong when the network is down sends them to reset a working password.
    Inconclusive { host: String, detail: String },
}

/// Does this response body belong to a sourcetable rather than a stream?
///
/// A caster that does not recognise a mountpoint answers `200` with its
/// sourcetable instead of a 404, so the status code alone would read as a
/// successful sign-in.
fn is_sourcetable_response(content_type: Option<&str>) -> bool {
    content_type.is_some_and(|c| c.to_ascii_lowercase().contains("sourcetable"))
}

/// Ask the caster to open `mountpoint`, and report what it said about the login.
///
/// This is the only thing that proves a caster login works. Saving credentials
/// writes them to netrc and contacts nothing, and the sourcetable in
/// `fetch_ntrip_sourcetable` is served to anonymous clients, so neither tells
/// you whether the password is right. A mountpoint request does: NTRIP 2.0
/// answers `401` for a bad login and `200` with `Content-Type: gnss/data` when
/// it is about to stream.
///
/// The response body is a stream that never ends, so nothing here reads it —
/// `send()` resolves once the headers arrive and the response is dropped, which
/// closes the connection.
///
/// The password travels in the `Authorization` header built by `basic_auth`,
/// never in the URL, so it cannot reach an error string: every message below is
/// built from `root` and `mountpoint`, which carry no userinfo.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn verify_ntrip_credentials(
    caster_url: String,
    mountpoint: Option<String>,
) -> Result<NtripAuthResult, String> {
    let host = super::credentials::ntrip_host_from_url(&caster_url)
        .ok_or_else(|| "Could not extract a hostname from the caster URL".to_string())?;
    let scheme = if caster_url.starts_with("http://") { "http" } else { "https" };
    let root = format!("{scheme}://{host}/");

    let Some((user, pass)) = super::credentials_ntrip::ntrip_auth_for_host(&host) else {
        return Ok(NtripAuthResult::Inconclusive {
            host,
            detail: "No login is saved for this caster yet.".to_string(),
        });
    };

    // Any mountpoint settles the login; the caller's own is used when it has
    // one so the check covers the stream it will actually ask for.
    let mount = match mountpoint.map(|m| m.trim().to_string()).filter(|m| !m.is_empty()) {
        Some(m) => m,
        None => match fetch_ntrip_sourcetable(caster_url.clone()).await {
            Ok(streams) if !streams.is_empty() => streams[0].mountpoint.clone(),
            Ok(_) => {
                return Ok(NtripAuthResult::Inconclusive {
                    host,
                    detail: "The caster listed no mountpoints to test the login against."
                        .to_string(),
                })
            }
            Err(e) => return Ok(NtripAuthResult::Inconclusive { host, detail: e }),
        },
    };

    Ok(probe_ntrip_mountpoint(&root, &mount, &user, &pass, &host).await)
}

/// The HTTP half of [`verify_ntrip_credentials`], with the login passed in.
///
/// Separated so the outcomes can be tested against a local caster without a
/// netrc entry and without an HTTP-mocking dependency.
pub(crate) async fn probe_ntrip_mountpoint(
    root: &str,
    mount: &str,
    user: &str,
    pass: &str,
    host: &str,
) -> NtripAuthResult {
    let inconclusive = |detail: String| NtripAuthResult::Inconclusive {
        host: host.to_string(),
        detail,
    };

    let client = match reqwest::Client::builder()
        .user_agent("NTRIP GinanTauri/1.0")
        .timeout(std::time::Duration::from_secs(15))
        .build()
    {
        Ok(c) => c,
        Err(e) => return inconclusive(format!("HTTP client init failed: {e}")),
    };

    let resp = match client
        .get(format!("{root}{mount}"))
        .header("Ntrip-Version", "Ntrip/2.0")
        .basic_auth(user, Some(pass))
        .send()
        .await
    {
        Ok(r) => r,
        // A caster that only speaks NTRIP 1.0 replies `ICY 200 OK`, which is not
        // HTTP and fails to parse here. That is not a rejected password.
        Err(e) => return inconclusive(format!("Could not reach {root}{mount}: {e}")),
    };

    let status = resp.status();
    let content_type = resp
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .map(str::to_string);
    // The body is an endless stream; dropping the response closes the socket.
    drop(resp);

    if status == reqwest::StatusCode::UNAUTHORIZED {
        return NtripAuthResult::Rejected {
            host: host.to_string(),
            detail: "The caster rejected the username or password.".to_string(),
        };
    }
    if status == reqwest::StatusCode::FORBIDDEN {
        return NtripAuthResult::Rejected {
            host: host.to_string(),
            detail: "The login is valid but not permitted to use this mountpoint.".to_string(),
        };
    }
    if status == reqwest::StatusCode::NOT_FOUND {
        return inconclusive(format!("The caster has no mountpoint named {mount}."));
    }
    if status.is_success() {
        if is_sourcetable_response(content_type.as_deref()) {
            return inconclusive(format!(
                "The caster answered with its sourcetable instead of {mount}, so the login was not tested."
            ));
        }
        return NtripAuthResult::Accepted {
            host: host.to_string(),
            mountpoint: mount.to_string(),
        };
    }

    inconclusive(format!("The caster answered HTTP {status}."))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A caster that answers one request with `response`, then closes.
    ///
    /// A real socket rather than a mocking crate: it keeps reqwest, the headers
    /// and the status parsing in the test, and adds no dependency.
    /// Returns the root URL to point the probe at.
    async fn fake_caster(response: &'static str) -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        tokio::spawn(async move {
            if let Ok((mut socket, _)) = listener.accept().await {
                use tokio::io::{AsyncReadExt, AsyncWriteExt};
                let mut buf = [0u8; 2048];
                let _ = socket.read(&mut buf).await;
                let _ = socket.write_all(response.as_bytes()).await;
                let _ = socket.flush().await;
            }
        });
        format!("http://{addr}/")
    }

    #[tokio::test]
    async fn accepts_a_login_the_caster_opens_a_stream_for() {
        let root = fake_caster(
            "HTTP/1.1 200 OK\r\nContent-Type: gnss/data\r\n\r\n",
        )
        .await;
        let got = probe_ntrip_mountpoint(&root, "ALIC00AUS0", "u", "p", "caster.test").await;
        assert_eq!(
            got,
            NtripAuthResult::Accepted {
                host: "caster.test".to_string(),
                mountpoint: "ALIC00AUS0".to_string(),
            }
        );
    }

    #[tokio::test]
    async fn reports_a_rejected_login_as_rejected() {
        let root = fake_caster("HTTP/1.1 401 Unauthorized\r\nContent-Length: 0\r\n\r\n").await;
        let got = probe_ntrip_mountpoint(&root, "ALIC00AUS0", "u", "wrong", "caster.test").await;
        match got {
            NtripAuthResult::Rejected { detail, .. } => {
                assert!(detail.contains("rejected the username or password"), "{detail}")
            }
            other => panic!("expected Rejected, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn a_forbidden_mountpoint_is_still_a_rejection() {
        let root = fake_caster("HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n").await;
        assert!(matches!(
            probe_ntrip_mountpoint(&root, "M", "u", "p", "caster.test").await,
            NtripAuthResult::Rejected { .. }
        ));
    }

    /// An unreachable caster must never read as a wrong password: that would
    /// send someone to reset a login that works.
    #[tokio::test]
    async fn an_unreachable_caster_is_inconclusive_not_rejected() {
        // Port 1 on loopback refuses immediately, so this needs no timeout.
        let got = probe_ntrip_mountpoint("http://127.0.0.1:1/", "M", "u", "p", "caster.test").await;
        match got {
            NtripAuthResult::Inconclusive { detail, .. } => {
                assert!(detail.contains("Could not reach"), "{detail}")
            }
            other => panic!("expected Inconclusive, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn an_unknown_mountpoint_does_not_claim_the_login_works() {
        let root = fake_caster("HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n").await;
        match probe_ntrip_mountpoint(&root, "NOPE", "u", "p", "caster.test").await {
            NtripAuthResult::Inconclusive { detail, .. } => {
                assert!(detail.contains("no mountpoint named NOPE"), "{detail}")
            }
            other => panic!("expected Inconclusive, got {other:?}"),
        }
    }

    /// Some casters answer an unknown mountpoint with 200 and their sourcetable,
    /// which the status code alone would read as a successful sign-in.
    #[tokio::test]
    async fn a_sourcetable_answer_does_not_count_as_a_sign_in() {
        let root = fake_caster(
            "HTTP/1.1 200 OK\r\nContent-Type: gnss/sourcetable\r\nContent-Length: 0\r\n\r\n",
        )
        .await;
        match probe_ntrip_mountpoint(&root, "NOPE", "u", "p", "caster.test").await {
            NtripAuthResult::Inconclusive { detail, .. } => {
                assert!(detail.contains("sourcetable instead of NOPE"), "{detail}")
            }
            other => panic!("expected Inconclusive, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn no_outcome_can_carry_the_password() {
        let root = fake_caster("HTTP/1.1 401 Unauthorized\r\nContent-Length: 0\r\n\r\n").await;
        let got = probe_ntrip_mountpoint(&root, "M", "alice", "hunter2", "caster.test").await;
        let rendered = format!("{got:?}");
        assert!(!rendered.contains("hunter2"), "password leaked: {rendered}");
    }

    #[test]
    fn only_a_sourcetable_content_type_counts_as_one() {
        assert!(is_sourcetable_response(Some("gnss/sourcetable")));
        assert!(is_sourcetable_response(Some("GNSS/SOURCETABLE; charset=utf-8")));
        assert!(!is_sourcetable_response(Some("gnss/data")));
        assert!(!is_sourcetable_response(None));
    }

    #[test]
    fn parses_and_classifies_obs_and_ssr_streams() {
        let body = "\
CAS;ntrip.data.gnss.ga.gov.au;2101;NTRIP Caster;GA;0;AUS;-35;149;0;0
STR;ALIC00AUS0;Alice Springs;RTCM 3.3;1004(1),1012(1),1077(1),1087(1);2;GPS+GLO+GAL;IGS-IP;AUS;-23.67;133.89;0;0;GA;none;B;N;9600;\n\
STR;BCEP00BKG0;GPS/GLO/GAL/BDS SSR;RTCM 3.3;1057,1058,1059,1063,1064,1065;0;GPS+GLO+GAL+BDS;IGS-SSR;DEU;50.0;7.0;0;0;BKG;none;N;N;5300;\n\
ENDSOURCETABLE\n";
        let streams = parse_sourcetable(body);
        assert_eq!(streams.len(), 2);
        assert_eq!(streams[0].mountpoint, "ALIC00AUS0");
        assert_eq!(streams[0].category, "obs");
        assert_eq!(streams[1].mountpoint, "BCEP00BKG0");
        assert_eq!(streams[1].category, "ssr");
    }
}
