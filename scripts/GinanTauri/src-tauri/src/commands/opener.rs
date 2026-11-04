//! Opening links in the user's own browser.
//!
//! The webview is the application window: letting a link navigate it replaces
//! the whole UI with a web page and leaves no way back, since there is no
//! address bar or Back button. Anything outbound therefore goes to
//! the system browser instead.

/// Accept only a plain `http(s)://host…` URL.
///
/// A prefix check rather than a URL parser: this needs to answer one narrow
/// question, and being strict is the goal. Whitespace and control characters
/// are refused outright — they have no place in a URL that reached us from a
/// link, and they are how an argument gets split or a terminal escaped.
fn validate_external_url(url: &str) -> Result<(), String> {
    let lower = url.to_ascii_lowercase();
    if !(lower.starts_with("http://") || lower.starts_with("https://")) {
        return Err("Refusing to open a non-http(s) URL".to_string());
    }
    // Scheme with no host behind it.
    let rest = lower
        .strip_prefix("https://")
        .or_else(|| lower.strip_prefix("http://"))
        .unwrap_or("");
    if rest.is_empty() {
        return Err("Refusing to open a URL with no host".to_string());
    }
    if url.chars().any(|c| c.is_whitespace() || c.is_control()) {
        return Err("Refusing to open a URL containing whitespace".to_string());
    }
    Ok(())
}

/// Open `url` in the system browser.
///
/// Only `http` and `https` are accepted. That is not decoration: the URL is
/// handed to a platform opener, and `file://` would open arbitrary local paths
/// while schemes such as `smb://` or a registered custom handler can reach
/// further still. Rejecting everything else keeps this to the one job it has.
///
/// The URL is passed as a single argument to the opener process — never
/// through a shell — so shell metacharacters in it carry no meaning.
#[tauri::command]
#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
pub async fn open_external(url: String) -> Result<(), String> {
    validate_external_url(&url)?;

    #[cfg(target_os = "macos")]
    let mut cmd = {
        let mut c = std::process::Command::new("open");
        c.arg(&url);
        c
    };

    #[cfg(target_os = "linux")]
    let mut cmd = {
        let mut c = std::process::Command::new("xdg-open");
        c.arg(&url);
        c
    };

    // Not `cmd /C start`: cmd would treat `&` in a query string as a command
    // separator, running whatever follows it. The URL protocol handler opens
    // the default browser without going through a shell.
    #[cfg(target_os = "windows")]
    let mut cmd = {
        let mut c = std::process::Command::new("rundll32");
        c.args(["url.dll,FileProtocolHandler", &url]);
        c
    };

    cmd.spawn()
        .map(|_| ())
        .map_err(|e| format!("Could not open {url}: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_non_http_schemes() {
        // Each of these reaches outside "open a web page" if allowed through.
        for url in [
            "file:///etc/passwd",
            "smb://host/share",
            "javascript:alert(1)",
            "data:text/html,<script>alert(1)</script>",
            "not a url",
            "",
        ] {
            assert!(
                validate_external_url(url).is_err(),
                "{url} should have been rejected",
            );
        }
    }

    #[test]
    fn rejects_scheme_without_host() {
        assert!(validate_external_url("https://").is_err());
    }

    #[test]
    fn rejects_whitespace_and_control_chars() {
        assert!(validate_external_url("https://example.com/a b").is_err());
        assert!(validate_external_url("https://example.com/\n").is_err());
    }

    #[test]
    fn accepts_the_links_the_map_actually_renders() {
        for url in [
            "https://www.openstreetmap.org/copyright",
            "https://leafletjs.com",
            "HTTPS://EXAMPLE.COM/Path?q=1#frag",
            "http://localhost:1420/",
        ] {
            assert!(validate_external_url(url).is_ok(), "{url} should be allowed");
        }
    }
}
