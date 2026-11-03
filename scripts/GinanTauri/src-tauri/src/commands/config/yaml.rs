use super::PathScope;


// ─── helpers ──────────────────────────────────────────────────────────────────

/// Remove a key at `keys` path. No-op if any intermediate node is missing.
pub(crate) fn delete_val(config: &mut serde_yaml::Value, keys: &[&str]) {
    if keys.is_empty() {
        return;
    }
    if let Some(map) = config.as_mapping_mut() {
        let key = serde_yaml::Value::String(keys[0].to_string());
        if keys.len() == 1 {
            map.remove(&key);
        } else if let Some(child) = map.get_mut(&key) {
            delete_val(child, &keys[1..]);
        }
    }
}

/// Recursively navigate `config` by `keys` and set the leaf to `value`.
/// Creates missing intermediate Mapping nodes.
pub(crate) fn set_val(config: &mut serde_yaml::Value, keys: &[&str], value: serde_yaml::Value) {
    if keys.is_empty() {
        *config = value;
        return;
    }
    if !config.is_mapping() {
        *config = serde_yaml::Value::Mapping(serde_yaml::Mapping::new());
    }
    let map = config.as_mapping_mut().unwrap();
    let key = serde_yaml::Value::String(keys[0].to_string());
    if keys.len() == 1 {
        map.insert(key, value);
    } else {
        let entry = map
            .entry(key)
            .or_insert_with(|| serde_yaml::Value::Mapping(serde_yaml::Mapping::new()));
        set_val(entry, &keys[1..], value);
    }
}

/// Rename the `old_key` entry inside `receiver_options` to `new_key`.
pub(crate) fn rename_receiver_key(config: &mut serde_yaml::Value, old_key: &str, new_key: &str) {
    if old_key == new_key {
        return;
    }
    if let Some(recv_opts) = config.get_mut("receiver_options") {
        if let Some(map) = recv_opts.as_mapping_mut() {
            let old = serde_yaml::Value::String(old_key.to_string());
            if let Some(val) = map.remove(&old) {
                map.insert(serde_yaml::Value::String(new_key.to_string()), val);
            }
        }
    }
}

/// Looks up the stored `(login, password)` for a hostname — netrc in production,
/// a stub in tests.
pub(crate) type CredentialLookup<'a> = &'a dyn Fn(&str) -> Option<(String, String)>;

/// Userinfo PEA fills in itself from `outputs.metadata.user` / `.pass` (or the
/// `-u` / `-p` flags). What an `Export` config carries instead of a login.
pub(crate) const USERINFO_PLACEHOLDER: &str = "<USER>:<PASS>";

/// Split `scheme://authority/rest`. The authority ends at the first `/`, as in
/// PEA's own URL parser, so a `/` inside a password cannot be represented.
fn split_url(url: &str) -> Option<(&str, &str, &str)> {
    let (scheme, after) = url.split_once("://")?;
    let end = after.find('/').unwrap_or(after.len());
    Some((scheme, &after[..end], &after[end..]))
}

/// Make one stream URL carry the login PEA needs, for `scope`.
///
/// Only `http`, `https` and `ntrip` URLs are touched. A bare mountpoint (the
/// caster root supplies its login), `serial://` and `sisnet://` pass through.
///
/// * `Local` — a URL with no userinfo gets the stored login for its host
///   spliced in; no stored login leaves it as written (PEA then connects
///   without a login). A URL that already has userinfo, including the
///   `<USER>:<PASS>` placeholder, is left alone, except that the placeholder is
///   filled from the store when it has an entry.
/// * `Export` — the userinfo is replaced by the placeholder, so the file
///   neither carries nor loses track of a login. PEA cannot parse a URL with no
///   userinfo at all (it reads the host as empty), so the placeholder is also
///   what makes the file runnable.
///
/// Errors when the stored login holds a character PEA's URL parser cannot take
/// (see `check_login_is_url_safe`).
pub(crate) fn credentialed_url(
    url: &str,
    scope: PathScope,
    lookup: CredentialLookup,
) -> Result<String, String> {
    let Some((scheme, authority, rest)) = split_url(url) else {
        return Ok(url.to_string());
    };
    if !matches!(scheme.to_ascii_lowercase().as_str(), "http" | "https" | "ntrip" | "sisnet") {
        return Ok(url.to_string());
    }
    let (userinfo, hostport) = match authority.rsplit_once('@') {
        Some((u, h)) => (Some(u), h),
        None => (None, authority),
    };
    if hostport.is_empty() {
        return Ok(url.to_string());
    }
    if scope == PathScope::Export {
        return Ok(format!("{scheme}://{USERINFO_PLACEHOLDER}@{hostport}{rest}"));
    }
    if userinfo.is_some_and(|u| u != USERINFO_PLACEHOLDER) {
        return Ok(url.to_string());
    }
    let host = hostport.split(':').next().unwrap_or(hostport);
    let Some((user, pass)) = lookup(host) else {
        return Ok(url.to_string());
    };
    check_login_is_url_safe(host, &user, &pass)?;
    Ok(format!("{scheme}://{user}:{pass}@{hostport}{rest}"))
}

/// PEA's URL parser (`URL` in `src/cpp/common/tcpSocket.hpp`) splits the login on
/// the first `:` and the first `@`, ends the authority at the first `/`, and
/// does no percent-decoding (checked against the bundled `pea`: a password with
/// `@` gives host `ss@127.0.0.1`, `%40` is sent literally). So `@` or `/` in a
/// password, or `:`, `@`, `/` in a username, would silently connect to the wrong
/// place. Refuse instead, naming the host and never the login.
fn check_login_is_url_safe(host: &str, user: &str, pass: &str) -> Result<(), String> {
    if user.contains([':', '@', '/']) || pass.contains(['@', '/']) {
        return Err(format!(
            "The saved login for {host} contains a character (@, / or a ':' in the username) \
             that PEA cannot read inside a stream URL. Change that password or username, \
             then try again."
        ));
    }
    Ok(())
}

/// Replace the userinfo of every `scheme://userinfo@host` URL inside `s` with
/// `replacement`. Strings that are not URLs, and URLs without userinfo, come
/// back unchanged. A `<USER>:<PASS>` placeholder is not a login and is kept.
pub(crate) fn replace_userinfo(s: &str, replacement: &str) -> String {
    let Some((scheme, authority, rest)) = split_url(s) else {
        return s.to_string();
    };
    match authority.rsplit_once('@') {
        Some((u, hostport)) if u != USERINFO_PLACEHOLDER => {
            format!("{scheme}://{replacement}@{hostport}{rest}")
        }
        _ => s.to_string(),
    }
}

/// Apply `f` to every string in the tree (keys are left alone).
pub(crate) fn map_strings(v: &mut serde_yaml::Value, f: &dyn Fn(&str) -> String) {
    match v {
        serde_yaml::Value::String(s) => *s = f(s),
        serde_yaml::Value::Sequence(seq) => seq.iter_mut().for_each(|x| map_strings(x, f)),
        serde_yaml::Value::Mapping(m) => m.iter_mut().for_each(|(_, x)| map_strings(x, f)),
        _ => {}
    }
}

/// The dotted key path of the first string in `v` holding a URL with an `@`
/// anywhere after its authority. That is what a login containing `/` looks
/// like once `split_url` has ended the authority at that `/`, and also what a
/// second URL with a login looks like in the same string, which the userinfo
/// sweep never reaches because it only rewrites the first authority. Any `@`
/// counts, whitespace included: a narrower rule is one a login can slip past,
/// and a stream URL has no other reason to carry one. Sequence entries report
/// the key of their list.
pub(crate) fn find_unreadable_login(v: &serde_yaml::Value) -> Option<String> {
    fn walk(v: &serde_yaml::Value, path: &mut Vec<String>) -> Option<String> {
        match v {
            serde_yaml::Value::String(s) => {
                let (_, _, rest) = split_url(s)?;
                rest.contains('@').then(|| path.join("."))
            }
            serde_yaml::Value::Sequence(seq) => seq.iter().find_map(|x| walk(x, path)),
            serde_yaml::Value::Mapping(m) => m.iter().find_map(|(k, x)| {
                path.push(match k.as_str() {
                    Some(k) => k.to_string(),
                    None => serde_yaml::to_string(k).unwrap_or_default().trim().to_string(),
                });
                let found = walk(x, path);
                path.pop();
                found
            }),
            _ => None,
        }
    }
    walk(v, &mut Vec::new())
}

/// `yaml` with every URL login shown as `***@`, for the YAML inspector.
///
/// Works on the text so a hand-edited file keeps its comments and layout, and
/// the inspector's "on disk" vs "would be written" diff still lines up. A login
/// runs from `://` to the last `@` before the first `/` or whitespace, which is
/// also where PEA ends the authority, so a password with a quote in it is still
/// covered; one with `/` or whitespace cannot reach a run config (see
/// `check_login_is_url_safe`).
pub(crate) fn mask_yaml_credentials(yaml: &str) -> String {
    let mut out = String::with_capacity(yaml.len());
    let mut rest = yaml;
    while let Some(i) = rest.find("://") {
        let (head, tail) = rest.split_at(i + 3);
        out.push_str(head);
        let end = tail.find(|c: char| c == '/' || c.is_whitespace()).unwrap_or(tail.len());
        match tail[..end].rsplit_once('@') {
            Some((u, _)) if u != USERINFO_PLACEHOLDER => {
                out.push_str("***");
                rest = &tail[u.len()..];
            }
            _ => rest = tail,
        }
    }
    out.push_str(rest);
    out
}

/// Build a YAML sequence of strings.
pub(crate) fn str_seq(items: &[String]) -> serde_yaml::Value {
    serde_yaml::Value::Sequence(
        items
            .iter()
            .map(|s| serde_yaml::Value::String(s.clone()))
            .collect(),
    )
}

/// Represent a float as a serde_yaml Value, preserving the decimal point.
pub(crate) fn float_val(v: f64) -> serde_yaml::Value {
    serde_yaml::Value::Number(serde_yaml::Number::from(v))
}

pub(crate) fn constellation_to_yaml_key(c: &str) -> String {
    match c {
        "GPS" => "gps",
        "GAL" => "gal",
        "GLO" => "glo",
        "BDS" => "bds",
        "QZS" => "qzs",
        _ => c,
    }
    .to_string()
}
