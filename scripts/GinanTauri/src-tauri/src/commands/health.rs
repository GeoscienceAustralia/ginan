use std::time::{Duration, SystemTime, UNIX_EPOCH};

use serde::Serialize;

use super::products::provider_base_urls;

/// Reachability status of a single PPP provider, as observed by a HEAD probe.
#[derive(Debug, Serialize)]
pub struct HealthStatus {
    pub provider: String,
    /// "up" if at least one base URL answered 2xx/3xx, "down" otherwise.
    /// "unknown" is reserved for cases the backend cannot resolve a URL at all.
    pub status: String,
    pub checked_at: u64,
}

/// Probe each known mirror for `provider` with a short HEAD request. Returns
/// `up` if any mirror replies with a 2xx/3xx, `down` on timeouts/errors/4xx/5xx.
///
/// This is a shallow reachability check only — it does NOT verify that any
/// specific SP3/CLK/BIA file is present.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn check_ppp_provider_health(provider: String) -> Result<HealthStatus, String> {
    let urls = provider_base_urls(&provider);
    let checked_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    if urls.is_empty() {
        return Ok(HealthStatus {
            provider,
            status: "unknown".into(),
            checked_at,
        });
    }

    let client = match reqwest::Client::builder()
        .user_agent("GinanTauri/1.0")
        .timeout(Duration::from_secs(3))
        .build()
    {
        Ok(c) => c,
        Err(e) => return Err(format!("HTTP client init failed: {e}")),
    };

    let mut any_up = false;
    for url in urls {
        match client.head(url).send().await {
            Ok(resp) => {
                let s = resp.status();
                // 2xx/3xx → up. 401/403 → server is reachable, just refusing
                // anonymous requests (CDDIS without Earthdata creds). Treat as
                // up so the dropdown doesn't mislead the user — they'll see
                // the auth failure at download time, not at picker time.
                if s.is_success()
                    || s.is_redirection()
                    || s == reqwest::StatusCode::UNAUTHORIZED
                    || s == reqwest::StatusCode::FORBIDDEN
                {
                    any_up = true;
                    break;
                }
            }
            Err(_) => {
                // timeout, DNS fail, TLS error → treat this mirror as down
            }
        }
    }

    Ok(HealthStatus {
        provider,
        status: if any_up { "up".into() } else { "down".into() },
        checked_at,
    })
}
