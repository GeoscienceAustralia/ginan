//! SBAS (SouthPAN over SISNet) monitoring for the Streams tab.
//!
//! PEA reports nothing about its SISNet stream where the tab looks for the
//! other streams: SBAS messages never reach the decoded RTCM, and only NTRIP
//! streams get a `ntrip_stats.jsonl` log. Without changing PEA, the tab gets
//! its SBAS view from three places instead:
//!
//! * [`probe`]: the stream test logs in to SISNet itself, while its own PEA
//!   run leaves SISNet out, and counts what arrives. Only then, so a run never
//!   holds two sessions on one login.
//! * [`SbasConsole`]: during a run, the SISNet lines PEA prints (bad logins,
//!   invalid messages, CRC errors).
//! * [`SppLevels`]: during a run, the protection levels PEA writes to `.SPP`.
//!
//! [`parse_msg`] decodes a SISNet line the way `DS2DCParser::parse` does
//! (`src/cpp/sbas/sisnet.cpp`), CRC check included.

use std::collections::{BTreeMap, VecDeque};
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use serde::Serialize;
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

use super::config::realtime::{SOUTHPAN_HOST, SOUTHPAN_PORT, SOUTHPAN_PRN};
use super::visualisation_spp::{parse_spp_levels, SPP_FIELDS};

/// GPS time zero, 1980-01-06, as Unix milliseconds.
const GPS_EPOCH_UNIX_MS: i64 = 315_964_800_000;

/// PEA's `sbas_frequency` for a mode: L1 SBAS is on L1, DFMC and PVS on L5.
/// It decides where the message type sits in the frame.
pub(crate) fn frequency(mode: &str) -> u8 {
    if mode == "L1" {
        1
    } else {
        5
    }
}

/// CRC-24Q (RTCM / SBAS), initial value zero.
pub(crate) fn crc24q(data: &[u8]) -> u32 {
    let mut crc = 0u32;
    for &b in data {
        crc ^= (b as u32) << 16;
        for _ in 0..8 {
            crc <<= 1;
            if crc & 0x100_0000 != 0 {
                crc ^= 0x186_4CFB;
            }
        }
    }
    crc & 0xFF_FFFF
}

/// One SBAS message off SISNet.
#[derive(Debug, PartialEq)]
pub(crate) struct SisnetMessage {
    /// Transmission time, GPST as if it were Unix ms (like the other GPST
    /// times the tab shows).
    pub gpst_ms: i64,
    pub msg_type: u8,
    pub crc_ok: bool,
}

/// SISNet's run-length coding: `c|n` is `c` n times (one hex digit), `c/nn`
/// two hex digits. PEA expands every `|` first, then every `/`.
fn expand(mut mess: String) -> Option<String> {
    for (marker, digits) in [('|', 1usize), ('/', 2usize)] {
        while let Some(i) = mess.find(marker) {
            let c = mess[..i].chars().last()?;
            let n = usize::from_str_radix(mess.get(i + 1..i + 1 + digits)?, 16).ok()?;
            let repeat = n.checked_sub(1)?;
            mess.replace_range(i..i + 1 + digits, &c.to_string().repeat(repeat));
        }
    }
    Some(mess)
}

/// Decode one `*MSG,week,tow,<message>*<checksum>` line. `None` for anything
/// that is not a well-formed message; `crc_ok` false for one that is but
/// fails its CRC (PEA drops those).
pub(crate) fn parse_msg(line: &str, freq: u8) -> Option<SisnetMessage> {
    let rest = &line[line.find("*MSG,")? + 5..];
    let mut parts = rest.splitn(3, ',');
    let week: i64 = parts.next()?.trim().parse().ok()?;
    let tow: f64 = parts.next()?.trim().parse().ok()?;
    let mess = expand(parts.next()?.trim().to_string())?;
    let message = &mess[..mess.find('*').unwrap_or(mess.len())];

    let hex = |range: std::ops::Range<usize>| u8::from_str_radix(message.get(range)?, 16).ok();
    let msg_type = if freq == 1 { hex(2..4)? } else { hex(1..3)? } >> 2;

    // 32 bytes hold the 250-bit message. Shifted right by 6 bits, the first
    // 226 (preamble/type/data) fill 29 bytes; the next 24 are the CRC.
    let mut bytes = [0u8; 32];
    for (i, b) in bytes.iter_mut().enumerate() {
        *b = hex(2 * i..2 * i + 2)?;
    }
    let mut frame = [0u8; 29];
    for i in 0..29 {
        frame[i] = frame[i].wrapping_add(bytes[i] >> 6);
        if i < 28 {
            frame[i + 1] = frame[i + 1].wrapping_add(bytes[i] << 2);
        }
    }
    let frame_crc = ((bytes[28] as u32) << 18 & 0xFC_0000)
        | ((bytes[29] as u32) << 10 & 0x03_FC00)
        | ((bytes[30] as u32) << 2 & 0x00_03FC)
        | ((bytes[31] as u32) >> 6 & 0x00_0003);

    let gpst_ms = GPS_EPOCH_UNIX_MS + week * 604_800_000 + (tow * 1000.0).round() as i64;
    Some(SisnetMessage { gpst_ms, msg_type, crc_ok: crc24q(&frame) == frame_crc })
}

/// What arrived on SISNet: totals, per message type, and the time span.
#[derive(Debug, Default, Serialize, Clone)]
pub struct SbasCounts {
    pub messages: u64,
    pub crc_errors: u64,
    pub invalid_lines: u64,
    /// Good messages per SBAS message type.
    pub by_type: BTreeMap<u8, u64>,
    pub first_gpst_ms: Option<i64>,
    pub last_gpst_ms: Option<i64>,
}

impl SbasCounts {
    pub(crate) fn ingest(&mut self, line: &str, freq: u8) {
        let line = line.trim();
        if line.is_empty() {
            return;
        }
        match parse_msg(line, freq) {
            Some(m) if m.crc_ok => {
                self.messages += 1;
                *self.by_type.entry(m.msg_type).or_default() += 1;
                self.first_gpst_ms.get_or_insert(m.gpst_ms);
                self.last_gpst_ms = Some(m.gpst_ms);
            }
            Some(_) => self.crc_errors += 1,
            None => self.invalid_lines += 1,
        }
    }

    /// Good messages per second of SBAS time; SouthPAN sends one a second.
    pub(crate) fn messages_per_s(&self) -> Option<f64> {
        let span = (self.last_gpst_ms? - self.first_gpst_ms?) as f64 / 1000.0;
        (span > 0.0 && self.messages > 1).then(|| (self.messages - 1) as f64 / span)
    }
}

/// How the stream test's SISNet check went.
#[derive(Debug, Serialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ProbeState {
    /// Logged in and SBAS messages arrived.
    Ok,
    /// No SISNet login is saved for the host.
    NoLogin,
    /// Could not reach the host.
    Unreachable,
    /// The host turned the login down.
    AuthFailed,
    /// Logged in, but the stream did not start.
    StartFailed,
    /// Started, but no message arrived in time.
    NoData,
}

/// The stream test's SBAS result, in `StreamTestResult.sbas`.
#[derive(Debug, Serialize, Clone)]
pub struct SbasProbe {
    pub host: String,
    pub prn: i64,
    pub mode: String,
    pub state: ProbeState,
    /// What went wrong, in words, for any state but `ok`. Never holds the login.
    pub error: Option<String>,
    /// How long messages were counted for.
    pub seconds: f64,
    pub messages_per_s: Option<f64>,
    #[serde(flatten)]
    pub counts: SbasCounts,
}

/// Log in to SouthPAN's SISNet with the saved login and count messages for
/// `duration`, or until `cancel` is set.
pub(crate) async fn probe(mode: &str, duration: Duration, cancel: Arc<AtomicBool>) -> SbasProbe {
    let login = super::credentials_ntrip::ntrip_auth_for_host(SOUTHPAN_HOST);
    probe_at(&format!("{SOUTHPAN_HOST}:{SOUTHPAN_PORT}"), login, mode, duration, cancel).await
}

/// [`probe`] against any address, so it can be tested on a local server.
pub(crate) async fn probe_at(
    addr: &str,
    login: Option<(String, String)>,
    mode: &str,
    duration: Duration,
    cancel: Arc<AtomicBool>,
) -> SbasProbe {
    let mut out = SbasProbe {
        host: SOUTHPAN_HOST.to_string(),
        prn: SOUTHPAN_PRN,
        mode: mode.to_string(),
        state: ProbeState::Ok,
        error: None,
        seconds: 0.0,
        messages_per_s: None,
        counts: SbasCounts::default(),
    };
    let fail = |mut out: SbasProbe, state, error: String| {
        out.state = state;
        out.error = Some(error);
        out
    };
    let Some((user, pass)) = login else {
        return fail(out, ProbeState::NoLogin, format!("No SISNet login is saved for {SOUTHPAN_HOST}. Add it in Settings › Credentials › SISNet."));
    };

    const STEP: Duration = Duration::from_secs(10);
    let stream = match tokio::time::timeout(STEP, tokio::net::TcpStream::connect(addr)).await {
        Ok(Ok(s)) => s,
        Ok(Err(e)) => return fail(out, ProbeState::Unreachable, format!("Cannot reach {addr}: {e}")),
        Err(_) => return fail(out, ProbeState::Unreachable, format!("No answer from {addr} in 10 s")),
    };
    let (read, mut write) = stream.into_split();
    let mut lines = BufReader::new(read).lines();

    // The handshake `SisnetStream` makes: AUTH, wait for `*AUTH`, START, wait for `*START`.
    if write.write_all(format!("AUTH,{user},{pass}\n").as_bytes()).await.is_err() {
        return fail(out, ProbeState::Unreachable, format!("{addr} closed the connection"));
    }
    if !next_line(&mut lines, STEP).await.is_some_and(|l| l.contains("*AUTH")) {
        // The reply is not passed on: it may echo what was sent.
        return fail(out, ProbeState::AuthFailed, "SISNet did not accept the saved login.".into());
    }
    if write.write_all(b"START\n").await.is_err()
        || !next_line(&mut lines, STEP).await.is_some_and(|l| l.contains("*START"))
    {
        return fail(out, ProbeState::StartFailed, "SISNet accepted the login but did not start the stream.".into());
    }

    let freq = frequency(mode);
    let started = Instant::now();
    while started.elapsed() < duration && !cancel.load(Ordering::SeqCst) {
        let left = duration.saturating_sub(started.elapsed()).min(Duration::from_millis(500));
        match tokio::time::timeout(left, lines.next_line()).await {
            Ok(Ok(Some(line))) => out.counts.ingest(&line, freq),
            Ok(_) => break, // closed or broken
            Err(_) => {}    // nothing in this slice; check the clock and cancel
        }
    }
    out.seconds = started.elapsed().as_secs_f64();
    out.messages_per_s = out.counts.messages_per_s();
    if out.counts.messages == 0 {
        let secs = out.seconds.round();
        return fail(out, ProbeState::NoData, format!("Logged in, but no valid SBAS message arrived in {secs} s."));
    }
    out
}

type Lines = tokio::io::Lines<BufReader<tokio::net::tcp::OwnedReadHalf>>;

/// The next line within `limit`, or `None` on timeout, close or error.
async fn next_line(lines: &mut Lines, limit: Duration) -> Option<String> {
    match tokio::time::timeout(limit, lines.next_line()).await {
        Ok(Ok(line)) => line,
        _ => None,
    }
}

/// PEA's own SISNet log lines during a run (`src/cpp/sbas/sisnet.cpp`).
#[derive(Debug, Default, Serialize, Clone)]
pub struct SbasConsole {
    pub auth_failures: u64,
    pub start_failures: u64,
    pub invalid_lines: u64,
    pub crc_errors: u64,
    /// Only counted when PEA logs at debug level.
    pub messages: u64,
    pub last_error: Option<String>,
    /// Unix ms of `last_error`.
    pub last_error_at_ms: Option<i64>,
}

/// Whether `line` may be one of PEA's SISNet lines. The login and start errors
/// do not say SISNET, but only `sisnet.cpp` prints them.
pub fn is_sisnet_line(line: &str) -> bool {
    line.contains("SISNET") || line.contains("Error in Authentication") || line.contains("Error in starting string")
}

impl SbasConsole {
    /// Count `line` if it is one of PEA's SISNet lines; anything else is ignored.
    pub fn observe(&mut self, line: &str, now_ms: i64) {
        if !is_sisnet_line(line) {
            return;
        }
        let error = if line.contains("Error in Authentication") {
            self.auth_failures += 1;
            "SISNet did not accept the login"
        } else if line.contains("Error in starting string") {
            self.start_failures += 1;
            "SISNet did not start the stream"
        } else if line.contains("CRC error in SISNET") {
            self.crc_errors += 1;
            "A message failed its CRC check"
        } else if line.contains("Invalid SISNET message") {
            self.invalid_lines += 1;
            "A line from SISNet was not a message"
        } else {
            if line.contains("SISNET message:") {
                self.messages += 1;
            }
            return;
        };
        self.last_error = Some(error.to_string());
        self.last_error_at_ms = Some(now_ms);
    }
}

/// Protection levels kept for the Streams tab: the last few minutes at 1 Hz.
const LEVELS_CAP: usize = 300;

/// Follows the newest `.SPP` in a run's output directory for HPL/VPL.
#[derive(Default)]
pub struct SppLevels {
    path: Option<PathBuf>,
    offset: u64,
    partial: String,
    /// `[gpst_ms, hpl, vpl]`, oldest first.
    points: VecDeque<[f64; 3]>,
}

impl SppLevels {
    /// Read whatever PEA has appended since the last call.
    pub fn poll(&mut self, dir: &Path) {
        let Some(newest) = newest_spp(dir) else { return };
        if self.path.as_ref() != Some(&newest) {
            *self = Self { path: Some(newest.clone()), ..Self::default() };
        }
        let Ok(mut f) = std::fs::File::open(&newest) else { return };
        let len = f.metadata().map(|m| m.len()).unwrap_or(0);
        if len < self.offset {
            self.offset = 0; // rewritten
            self.partial.clear();
        }
        if len == self.offset || f.seek(SeekFrom::Start(self.offset)).is_err() {
            return;
        }
        let mut buf = String::new();
        if f.read_to_string(&mut buf).is_err() {
            return;
        }
        self.offset = len;
        self.partial.push_str(&buf);
        // PEA starts each line with its newline rather than ending it with
        // one, so the last line is judged by its fields: all 23 written means
        // complete, fewer means PEA is still writing it.
        let mut text = std::mem::take(&mut self.partial);
        let tail_at = text.rfind('\n').map_or(0, |i| i + 1);
        if text[tail_at..].split_whitespace().count() < SPP_FIELDS {
            self.partial = text.split_off(tail_at);
        }
        for line in text.lines() {
            if let Some(p) = parse_spp_levels(line) {
                self.points.push_back(p);
                if self.points.len() > LEVELS_CAP {
                    self.points.pop_front();
                }
            }
        }
    }

    /// `null` until a `.SPP` with an epoch has been seen.
    pub fn to_json(&self) -> Value {
        let Some(path) = &self.path else { return Value::Null };
        let name = path.file_name().map(|n| n.to_string_lossy().into_owned());
        json!({ "file": name, "points": self.points.iter().collect::<Vec<_>>() })
    }
}

/// The most recently written `.SPP` directly in `dir`.
fn newest_spp(dir: &Path) -> Option<PathBuf> {
    std::fs::read_dir(dir)
        .ok()?
        .flatten()
        .filter(|e| e.path().extension().is_some_and(|x| x.eq_ignore_ascii_case("spp")))
        .filter_map(|e| Some((e.metadata().ok()?.modified().ok()?, e.path())))
        .max_by_key(|(t, _)| *t)
        .map(|(_, p)| p)
}

#[cfg(test)]
#[path = "sbas_monitor_tests.rs"]
mod tests;
