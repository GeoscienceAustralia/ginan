use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ─── dynamic PPP catalogue scan ─────────────────────────────────────────────

/// One entry in the PPP catalogue: a (provider, series, project) combination
/// confirmed to have SP3 + CLK + BIA products on at least one mirror.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PppCatalogueEntry {
    pub provider: String,
    pub series: String,
    pub project: String,
    pub has_sp3: bool,
    pub has_clk: bool,
    pub has_bia: bool,
    /// Constellation names (GPS/GLO/GAL/BDS/QZS) carried by this entry's SP3.
    /// Empty when not yet resolved — the frontend treats empty as permissive
    /// (all RINEX systems allowed).
    pub constellations: Vec<String>,
    /// Map of content type ("SP3", "CLK", "BIA") -> actual filename observed
    /// on the mirror (without `.gz`), e.g. `"COD0OPSRAP_20261560000_01D_05M_ORB.SP3"`.
    /// Empty for entries deserialised from data saved by earlier versions of the app.
    #[serde(default)]
    pub files: HashMap<String, String>,
}

/// The set of valid PPP combinations discovered on the mirrors for a window.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PppCatalogue {
    pub entries: Vec<PppCatalogueEntry>,
}

/// True when `s` could be the basename of a product file we care about.
/// Rejects query strings, parent-dir links, subdirectory links, and hidden files.
pub(crate) fn is_product_filename(s: &str) -> bool {
    !s.is_empty()
        && !s.starts_with('?')
        && !s.starts_with('.')
        && !s.starts_with("http")
        && !s.ends_with('/')
        && s.contains('.')
        && s.len() >= 5
}

/// Extract the filename part from an `href` value that might be a relative or
/// absolute path. Returns `None` for parent-directory links, query strings, and
/// external URLs.
pub(crate) fn href_to_filename(href: &str) -> Option<&str> {
    if href.starts_with('?') || href.starts_with('#') {
        return None;
    }
    if href.starts_with("http://") || href.starts_with("https://") {
        return None;
    }
    if href == ".." || href == "../" {
        return None;
    }
    if href.is_empty() {
        return None;
    }
    // Take the last path component so both relative ("file.gz") and absolute
    // ("/root/2300/file.gz") hrefs yield the same filename.
    let name = href.trim_end_matches('/').rsplit('/').next().unwrap_or(href);
    if is_product_filename(name) { Some(name) } else { None }
}

/// Parse filenames from an HTML directory listing.
///
/// Handles two formats:
/// - Apache/nginx `<a href="filename">` (BKG, IGN)
/// - CDDIS `<div class="archiveItemTextContainer">filename …</div>`
pub(crate) fn parse_directory_listing_html(html: &str) -> Vec<String> {
    let mut files: std::collections::HashSet<String> = Default::default();

    // Apache/nginx-style: href="..."
    {
        let mut pos = 0;
        while let Some(rel) = html[pos..].find("href=\"") {
            let start = pos + rel + 6; // skip `href="`
            if let Some(end_rel) = html[start..].find('"') {
                let href = &html[start..start + end_rel];
                if let Some(name) = href_to_filename(href) {
                    files.insert(name.to_string());
                }
                pos = start + end_rel + 1;
            } else {
                break;
            }
        }
    }

    // CDDIS archiveItemTextContainer format.
    // Python: SoupStrainer("div", class_="archiveItemTextContainer") then div.get_text().split(" ")[0]
    // We match Python by finding any element that has "archiveItemTextContainer" anywhere in its
    // class attribute (handles extra classes / whitespace before the closing ">"), then grabbing
    // the first whitespace-separated token of the element's text content — after stripping any
    // nested HTML tags, just like BeautifulSoup's get_text().
    {
        let needle = "archiveItemTextContainer";
        let mut pos = 0;
        while let Some(rel) = html[pos..].find(needle) {
            let after_class = pos + rel + needle.len();
            // Find the ">" that closes the opening tag.
            if let Some(gt_rel) = html[after_class..].find('>') {
                let content_start = after_class + gt_rel + 1;
                // Scan to the first closing tag. The content may contain nested
                // elements (e.g. <a href="…">filename</a>) — Python's get_text()
                // concatenates all child text nodes; we do the same by stripping tags.
                let raw_end = html[content_start..]
                    .find("</")
                    .unwrap_or(html.len() - content_start);
                let raw_content = &html[content_start..content_start + raw_end];
                let mut text = String::new();
                let mut in_tag = false;
                for c in raw_content.chars() {
                    match c {
                        '<' => { in_tag = true; text.push(' '); }
                        '>' => in_tag = false,
                        _ if !in_tag => text.push(c),
                        _ => {}
                    }
                }
                // Text may be "filename   size   date" — take the first token.
                if let Some(filename) = text.split_whitespace().next() {
                    if is_product_filename(filename) {
                        files.insert(filename.to_string());
                    }
                }
                pos = content_start + raw_end + 1;
            } else {
                break;
            }
        }
    }

    let mut result: Vec<String> = files.into_iter().collect();
    result.sort();
    result
}

/// Fields of an IGS long-name filename, see [`parse_igs_filename`].
pub(crate) type IgsFilenameParts = (String, String, String, u32, u32, String, String, String, String);

/// Parse an IGS long-name product filename (GPS-week ≥ 2237 format) and return
/// its key fields. Returns `None` for non-conforming names (old format, checksums,
/// README files, etc.).
///
/// Format: `{AC3}{SEP1}{PPP3}{TTT3}_{YYYY4}{DOY3}{HH2}{MM2}_{PERIOD3}_{SAMPLE3}_{CONTENT3}.{EXT3}[.gz]`
/// Example: `IGS0OPSFIN_20240010000_01D_15M_ORB.SP3.gz`
///
/// Returns `(provider, project, series, year, doy, timespan, sample, content, ext)`.
pub(crate) fn parse_igs_filename(name: &str) -> Option<IgsFilenameParts> {
    // Strip optional .gz suffix.
    let name = name.strip_suffix(".gz").unwrap_or(name);
    let b = name.as_bytes();

    // Minimum valid length after stripping .gz: 38 chars
    // e.g. IGS0OPSFIN_20240010000_01D_15M_ORB.SP3
    //      0         1         2         3    3
    //      0123456789012345678901234567890123456789
    if b.len() < 38 {
        return None;
    }

    // Char 3: separator digit (0-9 in the new long-name format).
    if !b[3].is_ascii_digit() {
        return None;
    }
    // Char 10: '_' — end of "AC·sep·project·series" block.
    if b[10] != b'_' {
        return None;
    }
    // Char 22: '_' — end of "YYYYDDDHHMM" block.
    if b[22] != b'_' {
        return None;
    }
    // Char 26: '_' — end of period field (e.g. "01D").
    if b[26] != b'_' {
        return None;
    }
    // Char 30: '_' — end of sample field (e.g. "15M").
    if b[30] != b'_' {
        return None;
    }
    // Char 34: '.' — extension separator.
    if b[34] != b'.' {
        return None;
    }

    let year: u32 = name[11..15].parse().ok()?;
    let doy: u32 = name[15..18].parse().ok()?;
    if !(1980..=2100).contains(&year) || !(1..=366).contains(&doy) {
        return None;
    }

    let provider = name[0..3].to_string();
    let project  = name[4..7].to_string();
    let series   = name[7..10].to_string();
    let timespan = name[23..26].to_string(); // e.g. "01D"
    let sample   = name[27..30].to_string(); // e.g. "15M", "30S", "05M"
    let content  = name[31..34].to_string(); // e.g. "ORB", "CLK", "OSB"
    let ext      = name[35..38].to_string(); // e.g. "SP3", "CLK", "BIA"

    // Provider, project, series must be uppercase ASCII letters or digits.
    let valid_id = |s: &str| s.chars().all(|c| c.is_ascii_uppercase() || c.is_ascii_digit());
    if !valid_id(&provider) || !valid_id(&project) || !valid_id(&series) {
        return None;
    }

    Some((provider, project, series, year, doy, timespan, sample, content, ext))
}

/// Build a PPP catalogue from filenames scraped from mirror directory listings.
///
/// A (provider, project, series) combination is included only when SP3, CLK,
/// and BIA/BSX are all present — matching Python's `get_valid_analysis_centers()`
/// which requires `REQUIRED_FILES = {"SP3", "BIA", "CLK"}`.
pub(crate) fn sample_rank(sample: &str) -> u32 {
    match sample.to_uppercase().as_str() {
        "30S" => 100,
        "01M" => 90,
        "05M" => 80,
        "15M" => 70,
        "30M" => 60,
        "01H" => 50,
        "01D" => 10,
        _ => 40,
    }
}

pub(crate) fn build_catalogue_from_filenames(filenames: &[String]) -> PppCatalogue {
    use std::collections::HashSet;

    type Combo = (String, String, String);
    let mut combos: HashMap<Combo, HashSet<String>> = HashMap::new();
    // (combo) -> content_type -> (best filename, rank)
    let mut files_by_combo: HashMap<Combo, HashMap<String, (String, u32)>> = HashMap::new();

    for name in filenames {
        if let Some((provider, project, series, _year, _doy, _timespan, sample, _content, ext)) =
            parse_igs_filename(name)
        {
            let file_type = match ext.to_uppercase().as_str() {
                "SP3" => "SP3",
                "CLK" => "CLK",
                "BIA" | "BSX" => "BIA",
                _ => continue,
            };
            let combo = (provider, project, series);
            combos
                .entry(combo.clone())
                .or_default()
                .insert(file_type.to_string());
            let rank = sample_rank(&sample);
            let entry_files = files_by_combo.entry(combo).or_default();
            let take = entry_files
                .get(file_type)
                .map(|(_, r)| rank > *r)
                .unwrap_or(true);
            if take {
                entry_files.insert(
                    file_type.to_string(),
                    (name.trim_end_matches(".gz").to_string(), rank),
                );
            }
        }
    }

    let mut entries: Vec<PppCatalogueEntry> = combos
        .into_iter()
        .filter(|(_, types)| {
            types.contains("SP3") && types.contains("CLK") && types.contains("BIA")
        })
        .map(|((provider, project, series), types)| {
            let files: HashMap<String, String> = files_by_combo
                .get(&(provider.clone(), project.clone(), series.clone()))
                .map(|m| m.iter().map(|(k, (n, _))| (k.clone(), n.clone())).collect())
                .unwrap_or_default();
            PppCatalogueEntry {
                provider,
                series,
                project,
                has_sp3: types.contains("SP3"),
                has_clk: types.contains("CLK"),
                has_bia: types.contains("BIA"),
                constellations: Vec::new(),
                files,
            }
        })
        .collect();

    entries.sort_by(|a, b| {
        (&a.provider, &a.series, &a.project).cmp(&(&b.provider, &b.series, &b.project))
    });

    PppCatalogue { entries }
}


