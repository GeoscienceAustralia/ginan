//! Summaries of PEA's decoded-RTCM output (`decoded_rtcm_*.json`, one JSON
//! record per line).
//!
//! A Rust port of GA's `rtcm_realtime_monitor.py`: the record ingest and the
//! JSON snapshot are field-for-field the same, so the snapshot keeps the
//! script's shape. The terminal table and command-line options are not ported;
//! the Streams tab shows the snapshot instead. Used by the stream test (replay
//! every file once) and the live run (follow the newest file).
//!
//! The snapshot covers:
//! - MSM observations by mountpoint, constellation, satellite and signal
//! - SSR orbit/clock/code-bias/phase-bias records by satellite and signal
//! - broadcast ephemeris records by satellite

use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::time::Instant;

use chrono::NaiveDateTime;
use serde_json::{json, Map, Value};

/// Fields checked, in order, for a record's "last seen" time.
const TIME_FIELDS: [&str; 5] = [
    "ReceivedSentTimeGPST",
    "EpochTimeGPST",
    "ReferenceTimeGPST",
    "ToeGPST",
    "TocGPST",
];

/// The "recent activity" window the snapshot's `window_*` counts cover.
pub const WINDOW_SECONDS: f64 = 60.0;

fn constellation_from_sat(sat: &str) -> String {
    let Some(first) = sat.chars().next() else { return "?".into() };
    match first {
        'G' => "GPS",
        'R' => "GLO",
        'E' => "GAL",
        'C' => "BDS",
        'J' => "QZS",
        'I' => "IRN",
        'S' => "SBS",
        _ => return first.to_string(),
    }
    .into()
}

fn is_real_signal_code(code: &str) -> bool {
    !matches!(code, "" | "-" | "NONE" | "None" | "null")
}

/// Python's `str(rec.get(key) or default)`: missing, null, false, 0 and ""
/// all fall back to `default`.
fn field_or(rec: &Map<String, Value>, key: &str, default: &str) -> String {
    match rec.get(key) {
        None | Some(Value::Null) | Some(Value::Bool(false)) => default.into(),
        Some(Value::String(s)) if s.is_empty() => default.into(),
        Some(Value::Number(n)) if n.as_f64() == Some(0.0) => default.into(),
        Some(Value::String(s)) => s.clone(),
        Some(v) => v.to_string(),
    }
}

fn as_text(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        other => other.to_string(),
    }
}

fn truthy(v: Option<&Value>) -> bool {
    match v {
        None | Some(Value::Null) | Some(Value::Bool(false)) => false,
        Some(Value::Number(n)) => n.as_f64() != Some(0.0),
        Some(Value::String(s)) => !s.is_empty(),
        Some(Value::Array(a)) => !a.is_empty(),
        Some(Value::Object(o)) => !o.is_empty(),
        Some(Value::Bool(true)) => true,
    }
}

fn sorted(set: &BTreeSet<String>) -> Value {
    json!(set.iter().collect::<Vec<_>>())
}

fn sets_by_key(m: &BTreeMap<String, BTreeSet<String>>) -> Value {
    Value::Object(m.iter().map(|(k, v)| (k.clone(), sorted(v))).collect())
}

#[derive(Default)]
struct MsmStats {
    records: u64,
    invalid: u64,
    snr_sum: f64,
    snr_count: u64,
    const_counts: BTreeMap<String, u64>,
    sat_codes: BTreeMap<String, BTreeSet<String>>,
    const_codes: BTreeMap<String, BTreeSet<String>>,
}

#[derive(Default)]
struct SsrStats {
    message_counts: BTreeMap<String, u64>,
    sats: BTreeSet<String>,
    codes: BTreeSet<String>,
    const_codes: BTreeMap<String, BTreeSet<String>>,
}

#[derive(Default)]
struct EphStats {
    records: u64,
    sats: BTreeSet<String>,
}

/// "YYYY-MM-DD HH:MM:SS(.fff)" read as a naive (UTC-like) time, in ms since 1970.
fn parse_gpst_ms(text: &str) -> Option<i64> {
    let t = text.trim();
    NaiveDateTime::parse_from_str(t, "%Y-%m-%d %H:%M:%S%.f")
        .or_else(|_| NaiveDateTime::parse_from_str(t, "%Y-%m-%dT%H:%M:%S%.f"))
        .ok()
        .map(|d| d.and_utc().timestamp_millis())
}

fn median(values: &VecDeque<f64>) -> Option<f64> {
    if values.is_empty() {
        return None;
    }
    let mut v: Vec<f64> = values.iter().copied().collect();
    v.sort_by(|a, b| a.total_cmp(b));
    let mid = v.len() / 2;
    Some(if v.len() % 2 == 1 { v[mid] } else { (v[mid - 1] + v[mid]) / 2.0 })
}

fn round3(x: f64) -> f64 {
    (x * 1000.0).round() / 1000.0
}

/// Most recent inter-arrival gaps kept per message type.
const MAX_GAPS: usize = 60;

/// One record-time second of a mount's recent activity.
#[derive(Default)]
struct Bucket {
    families: BTreeMap<String, u64>,
    epochs: BTreeSet<String>,
}

#[derive(Default)]
struct MessageStats {
    name: String,
    instances: u64,
    records: u64,
    /// Key of the current instance (see `instance_key`); a change starts a new one.
    key: String,
    /// Record time of the current instance's first record.
    instance_start_ms: Option<i64>,
    last_rx_ms: Option<i64>,
    inter_arrival: VecDeque<f64>,
    nominal_udi: Option<f64>,
    last_sats: BTreeSet<String>,
    last_signals: BTreeSet<String>,
}

#[derive(Default)]
struct SatRx {
    signals: BTreeSet<String>,
    snr_sum: f64,
    snr_count: u64,
    last_rx_ms: Option<i64>,
}

/// Everything the Streams tab shows for one mountpoint, whatever its role.
#[derive(Default)]
struct MountStats {
    families: BTreeMap<String, u64>,
    records: u64,
    last_rx_ms: Option<i64>,
    /// Record time (ms) of the newest record, for pruning `buckets`.
    newest_s: Option<i64>,
    /// Last timestamp text parsed, so the many records of one message parse once.
    parsed: Option<(String, Option<i64>)>,
    buckets: BTreeMap<i64, Bucket>,
    messages: BTreeMap<String, MessageStats>,
    sats: BTreeMap<String, SatRx>,
}

impl MountStats {
    fn rx_ms(&mut self, text: &str) -> Option<i64> {
        if let Some((t, ms)) = &self.parsed {
            if t == text {
                return *ms;
            }
        }
        let ms = parse_gpst_ms(text);
        self.parsed = Some((text.to_string(), ms));
        ms
    }

    fn role_hint(&self) -> Option<&'static str> {
        let (mut obs, mut corr, mut nav) = (0u64, 0u64, 0u64);
        for (f, n) in &self.families {
            match f.as_str() {
                "MSM" => obs += n,
                "brdcEph" => nav += n,
                f if f.starts_with("ssr") => corr += n,
                _ => {}
            }
        }
        let best = obs.max(corr).max(nav);
        match best {
            0 => None,
            b if b == obs => Some("OBS"),
            b if b == corr => Some("CORR"),
            _ => Some("NAV"),
        }
    }
}

/// Which message instance a record belongs to: the epoch for observations and
/// SSR, the reference time for SSR without an epoch, toe + satellite for
/// ephemeris (one message per satellite).
fn instance_key(rec: &Map<String, Value>, sat: &str) -> Option<String> {
    for f in ["EpochTimeGPST", "ReferenceTimeGPST"] {
        if let Some(v) = rec.get(f).filter(|v| !v.is_null()) {
            return Some(as_text(v));
        }
    }
    rec.get("ToeGPST").filter(|v| !v.is_null()).map(|v| format!("{}|{sat}", as_text(v)))
}

/// Running totals over every record ingested, plus a sliding window of recent
/// activity. The per-satellite and per-signal sets stay bounded by the number
/// of satellites and signals, so memory is flat however long a run is.
pub struct Monitor {
    start: Instant,
    files_seen: BTreeSet<String>,
    malformed_lines: u64,
    unknown_records: u64,
    total_by_type: BTreeMap<String, u64>,
    total_by_message: BTreeMap<String, u64>,
    last_seen_by_family: HashMap<String, String>,
    last_seen_by_mount: HashMap<String, String>,
    last_epoch_by_mount: HashMap<String, String>,
    /// Bumped whenever a mount's epoch time moves on, so a follower can push
    /// a snapshot per epoch rather than on a fixed timer.
    new_epochs: u64,
    msm: BTreeMap<String, MsmStats>,
    ssr: BTreeMap<String, SsrStats>,
    eph: BTreeMap<String, EphStats>,
    /// (when, family, mountpoint), oldest first; pruned at each snapshot.
    window: VecDeque<(Instant, String, String)>,
    /// Per mountpoint, whatever the role; windows by record time.
    streams: BTreeMap<String, MountStats>,
    /// Newest record time seen on any mount, ms.
    latest_rx_ms: Option<i64>,
}

impl Default for Monitor {
    fn default() -> Self {
        Self {
            start: Instant::now(),
            files_seen: BTreeSet::new(),
            malformed_lines: 0,
            unknown_records: 0,
            total_by_type: BTreeMap::new(),
            total_by_message: BTreeMap::new(),
            last_seen_by_family: HashMap::new(),
            last_seen_by_mount: HashMap::new(),
            last_epoch_by_mount: HashMap::new(),
            new_epochs: 0,
            msm: BTreeMap::new(),
            ssr: BTreeMap::new(),
            eph: BTreeMap::new(),
            window: VecDeque::new(),
            streams: BTreeMap::new(),
            latest_rx_ms: None,
        }
    }
}

impl Monitor {
    /// Zero every counter. The caller's `Follower` keeps its file offsets, so
    /// old lines are not read again.
    pub fn reset(&mut self) {
        let files = std::mem::take(&mut self.files_seen);
        *self = Self::default();
        self.files_seen = files;
    }

    /// Zero one mountpoint. Per-family SSR / ephemeris tables are not keyed by
    /// mount and stay as they are.
    pub fn reset_mount(&mut self, mount: &str) {
        self.msm.remove(mount);
        self.last_seen_by_mount.remove(mount);
        self.last_epoch_by_mount.remove(mount);
        self.window.retain(|(_, _, m)| m != mount);
        if let Some(st) = self.streams.remove(mount) {
            for (family, n) in &st.families {
                if let Some(c) = self.total_by_type.get_mut(family) {
                    *c = c.saturating_sub(*n);
                }
            }
            for msg in st.messages.values() {
                if let Some(c) = self.total_by_message.get_mut(&msg.name) {
                    *c = c.saturating_sub(msg.records);
                }
            }
            self.total_by_type.retain(|_, c| *c > 0);
            self.total_by_message.retain(|_, c| *c > 0);
        }
    }

    fn ingest_stream(
        &mut self,
        rec: &Map<String, Value>,
        family: &str,
        message_type: &str,
        mount: &str,
        sat: &str,
        code: &str,
    ) {
        // `?` records and records without a mountpoint belong to no stream.
        if family == "?" || mount == "-" {
            return;
        }
        let st = self.streams.entry(mount.to_string()).or_default();
        st.records += 1;
        *st.families.entry(family.to_string()).or_default() += 1;

        let rx_ms = TIME_FIELDS
            .iter()
            .find_map(|f| rec.get(*f))
            .map(as_text)
            .and_then(|t| st.rx_ms(&t));
        if let Some(ms) = rx_ms {
            st.last_rx_ms = st.last_rx_ms.max(Some(ms));
            self.latest_rx_ms = self.latest_rx_ms.max(Some(ms));
            let sec = ms.div_euclid(1000);
            st.newest_s = st.newest_s.max(Some(sec));
            let b = st.buckets.entry(sec).or_default();
            *b.families.entry(family.to_string()).or_default() += 1;
            if let Some(e) = rec.get("EpochTimeGPST").filter(|v| !v.is_null()) {
                b.epochs.insert(as_text(e));
            }
            let cutoff = st.newest_s.unwrap_or(sec) - WINDOW_SECONDS as i64;
            while st.buckets.first_key_value().is_some_and(|(k, _)| *k <= cutoff) {
                st.buckets.pop_first();
            }
        }

        let real_code = is_real_signal_code(code);
        if sat != "-" {
            let s = st.sats.entry(sat.to_string()).or_default();
            if real_code {
                s.signals.insert(code.to_string());
            }
            if let Some(snr) = rec.get("SNR").filter(|v| v.is_number()).and_then(Value::as_f64) {
                s.snr_sum += snr;
                s.snr_count += 1;
            }
            s.last_rx_ms = s.last_rx_ms.max(rx_ms);
        }

        let Some(msg_no) = rec.get("MessageNumber").filter(|v| !v.is_null()).map(as_text) else {
            return;
        };
        let Some(key) = instance_key(rec, sat) else { return };
        let m = st.messages.entry(msg_no).or_default();
        if m.instances == 0 || m.key != key {
            m.instances += 1;
            m.key = key;
            m.name = message_type.to_string();
            m.last_sats.clear();
            m.last_signals.clear();
            if let (Some(prev), Some(now)) = (m.instance_start_ms, rx_ms) {
                let gap = (now - prev) as f64 / 1000.0;
                if gap >= 0.0 {
                    if m.inter_arrival.len() == MAX_GAPS {
                        m.inter_arrival.pop_front();
                    }
                    m.inter_arrival.push_back(round3(gap));
                }
            }
            if rx_ms.is_some() {
                m.instance_start_ms = rx_ms;
            }
        }
        m.records += 1;
        m.last_rx_ms = m.last_rx_ms.max(rx_ms);
        if let Some(u) = rec.get("SSRUpdateIntervalSec") {
            m.nominal_udi = u.as_f64().or_else(|| u.as_str().and_then(|s| s.parse().ok())).or(m.nominal_udi);
        }
        if sat != "-" {
            m.last_sats.insert(sat.to_string());
        }
        if real_code {
            m.last_signals.insert(code.to_string());
        }
    }

    fn streams_json(&self) -> Value {
        let cutoff = self.latest_rx_ms.map(|ms| ms.div_euclid(1000) - WINDOW_SECONDS as i64);
        let out: Map<String, Value> = self
            .streams
            .iter()
            .map(|(mount, st)| {
                let mut window_families: BTreeMap<&str, u64> = BTreeMap::new();
                let mut epochs: BTreeSet<&str> = BTreeSet::new();
                let (mut first_s, mut last_s) = (i64::MAX, i64::MIN);
                for (sec, b) in st.buckets.range(cutoff.map_or(i64::MIN, |c| c + 1)..) {
                    first_s = first_s.min(*sec);
                    last_s = last_s.max(*sec);
                    for (f, n) in &b.families {
                        *window_families.entry(f).or_default() += n;
                    }
                    epochs.extend(b.epochs.iter().map(String::as_str));
                }
                let span = if first_s > last_s { 1 } else { (last_s - first_s + 1).min(WINDOW_SECONDS as i64) } as f64;
                let epochs_per_s =
                    (!epochs.is_empty()).then(|| round3(epochs.len() as f64 / span));

                let mut sats_by_constellation: BTreeMap<String, u64> = BTreeMap::new();
                for sat in st.sats.keys() {
                    *sats_by_constellation.entry(constellation_from_sat(sat)).or_default() += 1;
                }
                let messages: Map<String, Value> = st
                    .messages
                    .iter()
                    .map(|(no, m)| {
                        (
                            no.clone(),
                            json!({
                                "name": m.name,
                                "instances": m.instances,
                                "records": m.records,
                                "last_rx_ms": m.last_rx_ms,
                                "periodicity_s": median(&m.inter_arrival).map(round3),
                                "inter_arrival_s": m.inter_arrival,
                                "nominal_udi_s": m.nominal_udi,
                                "last_sats": m.last_sats,
                                "last_signals": m.last_signals,
                            }),
                        )
                    })
                    .collect();
                let satellites: Map<String, Value> = st
                    .sats
                    .iter()
                    .map(|(sat, s)| {
                        let cn0 = (s.snr_count > 0)
                            .then(|| (s.snr_sum / s.snr_count as f64 * 100.0).round() / 100.0);
                        (
                            sat.clone(),
                            json!({
                                "constellation": constellation_from_sat(sat),
                                "cn0_avg": cn0,
                                "signals": s.signals,
                                "last_rx_ms": s.last_rx_ms,
                            }),
                        )
                    })
                    .collect();
                (
                    mount.clone(),
                    json!({
                        "role_hint": st.role_hint(),
                        "families": st.families,
                        "window_families": window_families,
                        "records": st.records,
                        "window_records": window_families.values().sum::<u64>(),
                        "epochs_per_s": epochs_per_s,
                        "last_rx_ms": st.last_rx_ms,
                        "sats_by_constellation": sats_by_constellation,
                        "messages": messages,
                        "satellites": satellites,
                    }),
                )
            })
            .collect();
        Value::Object(out)
    }

    /// Ingest one line of decoded RTCM. Blank lines are skipped; lines that are
    /// not a JSON object count as malformed.
    pub fn ingest_line(&mut self, line: &str, now: Instant) {
        let line = line.trim();
        if line.is_empty() {
            return;
        }
        match serde_json::from_str::<Value>(line) {
            Ok(Value::Object(rec)) => self.ingest(&rec, now),
            _ => self.malformed_lines += 1,
        }
    }

    fn ingest(&mut self, rec: &Map<String, Value>, now: Instant) {
        let rec_type = field_or(rec, "type", "?");
        let message_type = match field_or(rec, "MessageType", "") {
            m if !m.is_empty() => m,
            _ => field_or(rec, "label", &rec_type),
        };
        let mountpoint = match field_or(rec, "Mountpoint", "") {
            m if !m.is_empty() => m,
            _ => field_or(rec, "Stream", "-"),
        };
        let sat = field_or(rec, "Sat", "-");
        let code = field_or(rec, "Code", "-");

        *self.total_by_type.entry(rec_type.clone()).or_default() += 1;
        *self.total_by_message.entry(message_type.clone()).or_default() += 1;
        self.ingest_stream(rec, &rec_type, &message_type, &mountpoint, &sat, &code);
        if let Some(t) = TIME_FIELDS.iter().find_map(|f| rec.get(*f)) {
            self.last_seen_by_family.insert(rec_type.clone(), as_text(t));
            self.last_seen_by_mount.insert(mountpoint.clone(), as_text(t));
        }
        if let Some(t) = rec.get("EpochTimeGPST") {
            let t = as_text(t);
            if self.last_epoch_by_mount.get(&mountpoint) != Some(&t) {
                self.new_epochs += 1;
            }
            self.last_epoch_by_mount.insert(mountpoint.clone(), t);
        }

        if rec_type == "MSM" {
            let constellation = constellation_from_sat(&sat);
            let m = self.msm.entry(mountpoint.clone()).or_default();
            m.records += 1;
            *m.const_counts.entry(constellation.clone()).or_default() += 1;
            if is_real_signal_code(&code) {
                m.sat_codes.entry(sat).or_default().insert(code.clone());
                m.const_codes.entry(constellation).or_default().insert(code);
            }
            if truthy(rec.get("IsInvalid")) {
                m.invalid += 1;
            }
            if let Some(snr) = rec.get("SNR").filter(|v| v.is_number()).and_then(Value::as_f64) {
                m.snr_sum += snr;
                m.snr_count += 1;
            }
        } else if rec_type.starts_with("ssr") {
            let s = self.ssr.entry(rec_type.clone()).or_default();
            *s.message_counts.entry(message_type).or_default() += 1;
            if sat != "-" {
                s.sats.insert(sat.clone());
            }
            if is_real_signal_code(&code) {
                s.codes.insert(code.clone());
                s.const_codes.entry(constellation_from_sat(&sat)).or_default().insert(code);
            }
        } else if rec_type == "brdcEph" {
            let e = self.eph.entry(message_type).or_default();
            e.records += 1;
            if sat != "-" {
                e.sats.insert(sat);
            }
        } else {
            if rec_type == "?" {
                self.unknown_records += 1;
            }
            return; // other families are counted but not windowed
        }
        self.window.push_back((now, rec_type, mountpoint));
    }

    /// How many times any mount's epoch has moved on. Only ever compared with
    /// an earlier reading; `reset` restarts it.
    pub fn new_epochs(&self) -> u64 {
        self.new_epochs
    }

    /// The summary, in the same shape as the script's `--snapshot-json`.
    #[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
    pub fn snapshot(&mut self) -> Value {
        let now = Instant::now();
        while self
            .window
            .front()
            .is_some_and(|(t, _, _)| now.duration_since(*t).as_secs_f64() > WINDOW_SECONDS)
        {
            self.window.pop_front();
        }
        let mut window_by_type: BTreeMap<&str, u64> = BTreeMap::new();
        let mut window_msm: BTreeMap<&str, u64> = BTreeMap::new();
        for (_, family, mount) in &self.window {
            *window_by_type.entry(family).or_default() += 1;
            if family == "MSM" {
                *window_msm.entry(mount).or_default() += 1;
            }
        }

        let msm: Map<String, Value> = self
            .msm
            .iter()
            .map(|(mount, m)| {
                let avg_snr = (m.snr_count > 0)
                    .then(|| (m.snr_sum / m.snr_count as f64 * 100.0).round() / 100.0);
                (
                    mount.clone(),
                    json!({
                        "records": m.records,
                        "invalid_records": m.invalid,
                        "avg_snr": avg_snr,
                        "last_received": self.last_seen_by_mount.get(mount),
                        "last_epoch": self.last_epoch_by_mount.get(mount),
                        "satellite_count": m.sat_codes.len(),
                        "constellations": m.const_counts,
                        "signals_by_satellite": sets_by_key(&m.sat_codes),
                        "signals_by_constellation": sets_by_key(&m.const_codes),
                    }),
                )
            })
            .collect();

        let ssr: Map<String, Value> = self
            .ssr
            .iter()
            .map(|(rec_type, s)| {
                (
                    rec_type.clone(),
                    json!({
                        "records": s.message_counts.values().sum::<u64>(),
                        "message_counts": s.message_counts,
                        "last_received": self.last_seen_by_family.get(rec_type),
                        "satellites": sorted(&s.sats),
                        "codes": sorted(&s.codes),
                        "codes_by_constellation": sets_by_key(&s.const_codes),
                    }),
                )
            })
            .collect();

        let eph: Map<String, Value> = self
            .eph
            .iter()
            .map(|(message, e)| {
                (
                    message.clone(),
                    json!({
                        "records": e.records,
                        "last_received": self.last_seen_by_family.get("brdcEph"),
                        "satellites": sorted(&e.sats),
                    }),
                )
            })
            .collect();

        json!({
            "runtime_seconds": (self.start.elapsed().as_secs_f64() * 10.0).round() / 10.0,
            "files_seen": sorted(&self.files_seen),
            "malformed_lines": self.malformed_lines,
            "unknown_records": self.unknown_records,
            "total_by_type": self.total_by_type,
            "total_by_message": self.total_by_message,
            "window_seconds": WINDOW_SECONDS,
            "window_by_type": window_by_type,
            "window_msm_by_mountpoint": window_msm,
            "msm": msm,
            "ssr": ssr,
            "brdc_ephemeris": eph,
            "snapshot_gpst_ms": self.latest_rx_ms,
            "streams": self.streams_json(),
        })
    }
}

fn is_decoded_file(path: &Path) -> bool {
    path.file_name()
        .map(|n| n.to_string_lossy())
        .is_some_and(|n| n.starts_with("decoded_rtcm_") && n.ends_with(".json"))
}

fn decoded_files(dir: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = std::fs::read_dir(dir)
        .map(|d| d.flatten().map(|e| e.path()).filter(|p| is_decoded_file(p)).collect())
        .unwrap_or_default();
    files.sort();
    files
}

/// The file PEA is writing: newest by modification time, then by name.
fn newest_decoded_file(dir: &Path) -> Option<PathBuf> {
    decoded_files(dir).into_iter().max_by_key(|p| {
        let mtime = std::fs::metadata(p).and_then(|m| m.modified()).ok();
        (mtime, p.clone())
    })
}

/// Summarise every decoded file in `dir` from the start. For the stream test.
pub fn replay_dir(dir: &Path) -> Value {
    let mut monitor = Monitor::default();
    for path in decoded_files(dir) {
        monitor.files_seen.insert(path.to_string_lossy().into());
        let now = Instant::now();
        if let Ok(text) = std::fs::read_to_string(&path) {
            for line in text.lines() {
                monitor.ingest_line(line, now);
            }
        }
    }
    monitor.snapshot()
}

/// Reads a directory's decoded output as PEA writes it: only new, complete
/// lines of the newest file, moving on when PEA rotates to a new file. Like the
/// script's default mode, it starts at the end of the file that is newest when
/// it starts, so only new records are counted.
pub struct Follower {
    dir: PathBuf,
    path: Option<PathBuf>,
    offset: u64,
    file_id: Option<u64>,
    /// An incomplete last line, kept until the rest of it is written.
    partial: String,
    pub monitor: Monitor,
}

impl Follower {
    pub fn new(dir: &Path) -> Self {
        let mut f = Self {
            dir: dir.to_path_buf(),
            path: None,
            offset: 0,
            file_id: None,
            partial: String::new(),
            monitor: Monitor::default(),
        };
        if let Some(path) = newest_decoded_file(dir) {
            if let Ok(meta) = std::fs::metadata(&path) {
                f.offset = meta.len();
                f.file_id = file_id(&meta);
            }
            f.monitor.files_seen.insert(path.to_string_lossy().into());
            f.path = Some(path);
        }
        f
    }

    /// Read the current file from its start instead of its end.
    pub fn rewind(&mut self) {
        self.offset = 0;
        self.partial.clear();
    }

    /// Ingest whatever has been written since the last poll.
    #[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]
    pub fn poll(&mut self) {
        if let Some(latest) = newest_decoded_file(&self.dir) {
            if self.path.as_ref() != Some(&latest) {
                self.monitor.files_seen.insert(latest.to_string_lossy().into());
                self.path = Some(latest);
                self.offset = 0;
                self.file_id = None;
                self.partial.clear();
            }
        }
        let Some(path) = &self.path else { return };
        let Ok(mut file) = std::fs::File::open(path) else { return };
        let Ok(meta) = file.metadata() else { return };

        // The same name now holds a different or truncated file: start over.
        let id = file_id(&meta);
        if self.file_id.is_none() {
            self.file_id = id;
        }
        if id != self.file_id || meta.len() < self.offset {
            self.offset = 0;
            self.file_id = id;
            self.partial.clear();
        }

        let mut bytes = Vec::new();
        if file.seek(SeekFrom::Start(self.offset)).is_err() || file.read_to_end(&mut bytes).is_err() {
            return;
        }
        self.offset += bytes.len() as u64;
        self.partial.push_str(&String::from_utf8_lossy(&bytes));
        let Some(end) = self.partial.rfind('\n') else { return };
        let rest = self.partial.split_off(end + 1);
        let now = Instant::now();
        for line in self.partial.lines() {
            self.monitor.ingest_line(line, now);
        }
        self.partial = rest;
    }
}

#[cfg(unix)]
fn file_id(meta: &std::fs::Metadata) -> Option<u64> {
    use std::os::unix::fs::MetadataExt;
    Some(meta.ino())
}

#[cfg(not(unix))]
fn file_id(_meta: &std::fs::Metadata) -> Option<u64> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    const MSM_G: &str = r#"{"type":"MSM","Mountpoint":"RX1","Sat":"G05","Code":"L1C","SNR":40,"EpochTimeGPST":"2026-09-22 08:59:09.00","ReceivedSentTimeGPST":"2026-09-22 08:59:09.50"}"#;
    const MSM_E: &str = r#"{"type":"MSM","Mountpoint":"RX1","Sat":"E11","Code":"L5Q","SNR":30,"IsInvalid":true,"EpochTimeGPST":"2026-09-22 08:59:10.00"}"#;
    const SSR: &str = r#"{"type":"ssrCodeBias","Mountpoint":"SSRA","MessageType":"GPS_SSR_CODE_BIAS","Sat":"G05","Code":"L1C"}"#;
    const EPH: &str = r#"{"type":"brdcEph","Mountpoint":"BCEP","MessageType":"GPS_EPHEMERIS","Sat":"G05","ToeGPST":"2026-09-22 08:00:00.00"}"#;

    #[test]
    fn snapshot_matches_the_scripts_fields() {
        let mut m = Monitor::default();
        let now = Instant::now();
        for line in [MSM_G, MSM_E, SSR, EPH, r#"{"type":"?"}"#, "not json", "[1]", ""] {
            m.ingest_line(line, now);
        }
        let s = m.snapshot();

        assert_eq!(s["malformed_lines"], 2);
        assert_eq!(s["unknown_records"], 1);
        assert_eq!(s["total_by_type"]["MSM"], 2);
        assert_eq!(s["total_by_message"]["MSM"], 2, "MessageType falls back to type");
        let rx = &s["msm"]["RX1"];
        assert_eq!(rx["records"], 2);
        assert_eq!(rx["invalid_records"], 1);
        assert_eq!(rx["avg_snr"], 35.0);
        assert_eq!(rx["satellite_count"], 2);
        assert_eq!(rx["constellations"], json!({"GAL": 1, "GPS": 1}));
        assert_eq!(rx["signals_by_constellation"], json!({"GAL": ["L5Q"], "GPS": ["L1C"]}));
        assert_eq!(rx["last_epoch"], "2026-09-22 08:59:10.00");
        assert_eq!(s["ssr"]["ssrCodeBias"]["records"], 1);
        assert_eq!(s["ssr"]["ssrCodeBias"]["codes_by_constellation"], json!({"GPS": ["L1C"]}));
        assert_eq!(s["brdc_ephemeris"]["GPS_EPHEMERIS"]["satellites"], json!(["G05"]));
        assert_eq!(s["window_by_type"], json!({"MSM": 2, "brdcEph": 1, "ssrCodeBias": 1}));
        assert_eq!(s["window_msm_by_mountpoint"], json!({"RX1": 2}));
    }

    fn msm(mount: &str, no: u32, epoch: &str, rx: &str, sat: &str, code: &str, snr: u32) -> String {
        format!(
            r#"{{"type":"MSM","Mountpoint":"{mount}","MessageNumber":{no},"MessageType":"MSM7_GPS","ReceivedSentTimeGPST":"{rx}","EpochTimeGPST":"{epoch}","Sat":"{sat}","Code":"{code}","SNR":{snr}}}"#
        )
    }

    fn feed(m: &mut Monitor, lines: &[String]) {
        let now = Instant::now();
        for l in lines {
            m.ingest_line(l, now);
        }
    }

    #[test]
    fn counts_each_new_epoch_once_per_mount() {
        let mut m = Monitor::default();
        let t0 = "2026-09-22 08:59:00.00";
        let t1 = "2026-09-22 08:59:01.00";
        // Many records per epoch, two mounts on the same epoch.
        feed(&mut m, &[msm("RX1", 1077, t0, t0, "G05", "L1C", 40), msm("RX1", 1077, t0, t0, "G07", "L1C", 40)]);
        assert_eq!(m.new_epochs(), 1);
        feed(&mut m, &[msm("RX2", 1077, t0, t0, "G05", "L1C", 40)]);
        assert_eq!(m.new_epochs(), 2);
        feed(&mut m, &[msm("RX1", 1077, t0, t0, "G09", "L1C", 40)]);
        assert_eq!(m.new_epochs(), 2);
        feed(&mut m, &[msm("RX1", 1077, t1, t1, "G05", "L1C", 40)]);
        assert_eq!(m.new_epochs(), 3);
    }

    #[test]
    fn streams_group_instances_and_compute_periodicity_and_cn0() {
        let mut m = Monitor::default();
        // Epochs 1 s, 1 s, 3 s apart (by receive time); two sats per epoch in the first.
        feed(
            &mut m,
            &[
                msm("RX1", 1077, "2026-09-22 08:59:00.00", "2026-09-22 08:59:00.50", "G05", "L1C", 40),
                msm("RX1", 1077, "2026-09-22 08:59:00.00", "2026-09-22 08:59:00.50", "G07", "L2W", 30),
                msm("RX1", 1077, "2026-09-22 08:59:01.00", "2026-09-22 08:59:01.50", "G05", "L1C", 44),
                msm("RX1", 1077, "2026-09-22 08:59:02.00", "2026-09-22 08:59:02.50", "G05", "L1C", 42),
                msm("RX1", 1077, "2026-09-22 08:59:05.00", "2026-09-22 08:59:05.50", "G05", "L2W", 40),
            ],
        );
        let s = m.snapshot();
        let st = &s["streams"]["RX1"];
        assert_eq!(st["role_hint"], "OBS");
        assert_eq!(st["records"], 5);
        let msg = &st["messages"]["1077"];
        assert_eq!(msg["name"], "MSM7_GPS");
        assert_eq!(msg["instances"], 4);
        assert_eq!(msg["records"], 5);
        assert_eq!(msg["inter_arrival_s"], json!([1.0, 1.0, 3.0]));
        assert_eq!(msg["periodicity_s"], 1.0);
        assert_eq!(msg["last_sats"], json!(["G05"]));
        assert_eq!(msg["last_signals"], json!(["L2W"]));
        let g05 = &st["satellites"]["G05"];
        assert_eq!(g05["cn0_avg"], 41.5);
        assert_eq!(g05["signals"], json!(["L1C", "L2W"]));
        assert_eq!(g05["constellation"], "GPS");
        assert_eq!(st["sats_by_constellation"], json!({"GPS": 2}));
        assert_eq!(st["epochs_per_s"], 0.667); // 4 epochs over a 6 s span
        assert_eq!(s["snapshot_gpst_ms"], st["last_rx_ms"]);
    }

    #[test]
    fn median_of_even_count_and_gap_cap() {
        let mut m = Monitor::default();
        let lines: Vec<String> = (0..70)
            .map(|i| {
                let t = format!("2026-09-22 09:{:02}:{:02}.00", i / 60, i % 60);
                msm("RX1", 1077, &t, &t, "G05", "L1C", 40)
            })
            .collect();
        feed(&mut m, &lines);
        let msg = m.snapshot()["streams"]["RX1"]["messages"]["1077"].clone();
        assert_eq!(msg["instances"], 70);
        assert_eq!(msg["inter_arrival_s"].as_array().unwrap().len(), 60);
        assert_eq!(msg["periodicity_s"], 1.0);

        let mut v: VecDeque<f64> = VecDeque::from([1.0, 3.0]);
        assert_eq!(median(&v), Some(2.0));
        v.clear();
        assert_eq!(median(&v), None);
    }

    #[test]
    fn window_follows_record_time_not_arrival() {
        let mut m = Monitor::default();
        // All ingested at the same instant, but 100 s of record time.
        let lines: Vec<String> = [0, 50, 100]
            .iter()
            .map(|s| {
                let t = format!("2026-09-22 10:{:02}:{:02}.00", s / 60, s % 60);
                msm("RX1", 1077, &t, &t, "G05", "L1C", 40)
            })
            .collect();
        feed(&mut m, &lines);
        let st = m.snapshot()["streams"]["RX1"].clone();
        assert_eq!(st["records"], 3);
        assert_eq!(st["window_records"], 2, "t=0 is more than 60 s before t=100");
        assert_eq!(st["window_families"], json!({"MSM": 2}));
    }

    #[test]
    fn corr_and_nav_mounts_appear_with_role_hints() {
        let mut m = Monitor::default();
        feed(
            &mut m,
            &[
                r#"{"type":"ssrClk","Mountpoint":"SSRA","MessageNumber":1060,"MessageType":"GPS_SSR_CLOCK","ReceivedSentTimeGPST":"2026-09-22 08:00:00.00","EpochTimeGPST":"2026-09-22 08:00:00.00","SSRUpdateIntervalSec":5,"Sat":"G05"}"#.into(),
                r#"{"type":"brdcEph","Mountpoint":"BCEP","MessageNumber":1019,"MessageType":"GPS_EPHEMERIS","ReceivedSentTimeGPST":"2026-09-22 08:00:01.00","ToeGPST":"2026-09-22 08:00:00.00","Sat":"G05"}"#.into(),
                r#"{"type":"brdcEph","Mountpoint":"BCEP","MessageNumber":1019,"MessageType":"GPS_EPHEMERIS","ReceivedSentTimeGPST":"2026-09-22 08:00:02.00","ToeGPST":"2026-09-22 08:00:00.00","Sat":"G06"}"#.into(),
                r#"{"type":"?"}"#.into(),
            ],
        );
        let s = m.snapshot();
        assert_eq!(s["streams"]["SSRA"]["role_hint"], "CORR");
        assert_eq!(s["streams"]["SSRA"]["messages"]["1060"]["nominal_udi_s"], 5.0);
        assert_eq!(s["streams"]["BCEP"]["role_hint"], "NAV");
        let eph = &s["streams"]["BCEP"]["messages"]["1019"];
        assert_eq!(eph["instances"], 2, "each satellite is its own instance");
        assert_eq!(eph["periodicity_s"], 1.0);
        assert_eq!(s["streams"].as_object().unwrap().len(), 2, "`?` belongs to no stream");
    }

    #[test]
    fn reset_clears_counters_and_reset_mount_only_one() {
        let mut m = Monitor::default();
        m.files_seen.insert("f".into());
        feed(
            &mut m,
            &[
                msm("A", 1077, "2026-09-22 08:00:00.00", "2026-09-22 08:00:00.00", "G05", "L1C", 40),
                msm("B", 1077, "2026-09-22 08:00:00.00", "2026-09-22 08:00:00.00", "G05", "L1C", 40),
            ],
        );
        m.reset_mount("A");
        let s = m.snapshot();
        assert!(s["streams"].get("A").is_none());
        assert!(s["msm"].get("A").is_none());
        assert_eq!(s["streams"]["B"]["records"], 1);
        assert_eq!(s["total_by_type"]["MSM"], 1);
        m.reset();
        let s = m.snapshot();
        assert_eq!(s["streams"], json!({}));
        assert_eq!(s["snapshot_gpst_ms"], Value::Null);
        assert_eq!(s["files_seen"], json!(["f"]));
    }

    #[test]
    fn parses_gpst_with_and_without_fraction() {
        assert_eq!(parse_gpst_ms("1970-01-01 00:00:01"), Some(1000));
        assert_eq!(parse_gpst_ms("1970-01-01 00:00:01.25"), Some(1250));
        assert_eq!(parse_gpst_ms("garbage"), None);
    }

    #[test]
    fn follower_reads_new_complete_lines_and_follows_rotation() {
        let dir = std::env::temp_dir().join(format!("ginan-rtcm-monitor-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let first = dir.join("decoded_rtcm_202626509000_DEC.json");
        std::fs::write(&first, format!("{MSM_G}\n")).unwrap();

        let mut f = Follower::new(&dir); // starts at the end: the existing line is skipped
        f.poll();
        assert_eq!(f.monitor.total_by_type.get("MSM"), None);

        let mut out = std::fs::OpenOptions::new().append(true).open(&first).unwrap();
        write!(out, "{MSM_E}\n{}", &SSR[..20]).unwrap(); // one line plus half of the next
        f.poll();
        assert_eq!(f.monitor.total_by_type.get("MSM"), Some(&1));
        assert_eq!(f.monitor.total_by_type.get("ssrCodeBias"), None, "half a line waits");
        writeln!(out, "{}", &SSR[20..]).unwrap();
        f.poll();
        assert_eq!(f.monitor.total_by_type.get("ssrCodeBias"), Some(&1));

        // PEA rotates to the next minute's file; it is read from the start.
        std::thread::sleep(std::time::Duration::from_millis(20));
        std::fs::write(dir.join("decoded_rtcm_202626509001_DEC.json"), format!("{EPH}\n")).unwrap();
        f.poll();
        assert_eq!(f.monitor.total_by_type.get("brdcEph"), Some(&1));
        assert_eq!(f.monitor.files_seen.len(), 2);

        let replayed = replay_dir(&dir);
        assert_eq!(replayed["total_by_type"], json!({"MSM": 2, "brdcEph": 1, "ssrCodeBias": 1}));
        std::fs::remove_dir_all(&dir).ok();
    }
}
