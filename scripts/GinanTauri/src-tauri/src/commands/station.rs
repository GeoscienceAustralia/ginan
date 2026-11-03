//! Reference-station metadata (RTCM 1005/1006/1007/1008/1033) read back from
//! the `<MOUNT>_*_OBS.rtcm` recordings PEA writes. Recordings interleave PEA's
//! custom timestamp frames with the caster's frames; both are ordinary
//! CRC-checked RTCM3 frames, so a generic scanner skips what it doesn't decode.

use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use serde::Serialize;

/// Bytes read from the end of each recording. Station messages repeat every
/// few seconds, so a tail is enough. A fixed window; raise it if a
/// stream sends station messages less than once per ~8 MB.
const TAIL_BYTES: u64 = 8 * 1024 * 1024;
/// Newest recordings examined per mount.
const MAX_FILES: usize = 3;

#[derive(Debug, Default, Clone, PartialEq, Serialize)]
pub struct StationInfo {
    pub mount: String,
    pub station_id: Option<u32>,
    pub ecef_x: Option<f64>,
    pub ecef_y: Option<f64>,
    pub ecef_z: Option<f64>,
    pub antenna_height: Option<f64>,
    pub antenna_desc: Option<String>,
    pub antenna_setup_id: Option<u32>,
    pub antenna_serial: Option<String>,
    pub receiver_type: Option<String>,
    pub receiver_firmware: Option<String>,
    pub receiver_serial: Option<String>,
    pub messages: Vec<u32>,
    pub source_file: Option<String>,
    pub updated_ms: Option<i64>,
}

// ------------------------------------------------------------------ framing

fn crc24q(data: &[u8]) -> u32 {
    let mut crc: u32 = 0;
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

/// Valid frames in `buf` as `(message_number, payload)`. Garbage, bad CRCs and
/// a truncated head/tail are skipped by resyncing on the next 0xD3.
fn scan_frames(buf: &[u8]) -> Vec<(u32, &[u8])> {
    let mut out = Vec::new();
    let mut i = 0;
    while i + 6 <= buf.len() {
        if buf[i] == 0xD3 {
            let len = (((buf[i + 1] & 0x03) as usize) << 8) | buf[i + 2] as usize;
            let end = i + 3 + len;
            if len >= 2 && end + 3 <= buf.len() {
                let crc = ((buf[end] as u32) << 16) | ((buf[end + 1] as u32) << 8) | buf[end + 2] as u32;
                if crc24q(&buf[i..end]) == crc {
                    let p = &buf[i + 3..end];
                    out.push((((p[0] as u32) << 4) | (p[1] as u32 >> 4), p));
                    i = end + 3;
                    continue;
                }
            }
        }
        i += 1;
    }
    out
}

// ---------------------------------------------------------------- bit reader

struct Bits<'a> {
    b: &'a [u8],
    pos: usize,
}

impl Bits<'_> {
    fn u(&mut self, n: usize) -> Option<u64> {
        if self.pos + n > self.b.len() * 8 {
            return None;
        }
        let mut v = 0u64;
        for _ in 0..n {
            v = (v << 1) | ((self.b[self.pos / 8] >> (7 - self.pos % 8)) & 1) as u64;
            self.pos += 1;
        }
        Some(v)
    }
    fn s38(&mut self) -> Option<i64> {
        Some(((self.u(38)? as i64) << 26) >> 26)
    }
    /// 8-bit length then that many bytes.
    fn text(&mut self) -> Option<String> {
        let n = self.u(8)? as usize;
        let mut v = Vec::with_capacity(n);
        for _ in 0..n {
            v.push(self.u(8)? as u8);
        }
        Some(String::from_utf8_lossy(&v).into_owned())
    }
}

fn nonempty(s: String) -> Option<String> {
    (!s.is_empty()).then_some(s)
}

/// Fold one frame's values into `info` (only fields not already set, so the
/// newest frame must be fed first). Returns whether it was a station message.
fn apply(info: &mut StationInfo, msg: u32, p: &[u8]) -> bool {
    let mut r = Bits { b: p, pos: 12 };
    let ok = (|| -> Option<()> {
        match msg {
            1005 | 1006 => {
                let id = r.u(12)? as u32;
                r.u(10)?; // reserved 6 + GPS/GLO/GAL/ref-station indicators
                let x = r.s38()?;
                r.u(2)?;
                let y = r.s38()?;
                r.u(2)?;
                let z = r.s38()?;
                let h = if msg == 1006 { Some(r.u(16)?) } else { None };
                info.station_id.get_or_insert(id);
                info.ecef_x.get_or_insert(x as f64 * 1e-4);
                info.ecef_y.get_or_insert(y as f64 * 1e-4);
                info.ecef_z.get_or_insert(z as f64 * 1e-4);
                if let Some(h) = h {
                    info.antenna_height.get_or_insert(h as f64 * 1e-4);
                }
            }
            1007 | 1008 | 1033 => {
                let id = r.u(12)? as u32;
                let desc = r.text()?;
                let setup = r.u(8)? as u32;
                let ant_serial = if msg == 1007 { None } else { Some(r.text()?) };
                let rx = if msg == 1033 { Some((r.text()?, r.text()?, r.text()?)) } else { None };
                info.station_id.get_or_insert(id);
                if info.antenna_desc.is_none() {
                    info.antenna_desc = nonempty(desc);
                }
                info.antenna_setup_id.get_or_insert(setup);
                if let (None, Some(s)) = (&info.antenna_serial, ant_serial) {
                    info.antenna_serial = nonempty(s);
                }
                if let (None, Some((t, f, s))) = (&info.receiver_type, rx) {
                    info.receiver_type = nonempty(t);
                    info.receiver_firmware = nonempty(f);
                    info.receiver_serial = nonempty(s);
                }
            }
            _ => return None,
        }
        Some(())
    })();
    ok.is_some()
}

// -------------------------------------------------------------------- files

/// Recordings for `mount`, newest first.
fn recordings_for(dir: &Path, mount: &str) -> Vec<(PathBuf, std::time::SystemTime)> {
    let prefix = format!("{}_", mount.trim_start_matches('/').to_ascii_lowercase());
    let mut v: Vec<_> = std::fs::read_dir(dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().to_ascii_lowercase();
            if !(name.starts_with(&prefix) && name.ends_with("_obs.rtcm")) {
                return None;
            }
            let m = e.metadata().ok().filter(|m| m.is_file())?;
            Some((e.path(), m.modified().unwrap_or(UNIX_EPOCH)))
        })
        .collect();
    v.sort_by_key(|e| std::cmp::Reverse(e.1));
    v
}

fn read_tail(path: &Path) -> std::io::Result<Vec<u8>> {
    let mut f = std::fs::File::open(path)?;
    let len = f.metadata()?.len();
    f.seek(SeekFrom::Start(len.saturating_sub(TAIL_BYTES)))?;
    let mut buf = Vec::new();
    f.read_to_end(&mut buf)?;
    Ok(buf)
}

fn station_info_blocking(dir: &Path, mount: &str) -> Option<StationInfo> {
    let mut info = StationInfo { mount: mount.trim_start_matches('/').to_string(), ..Default::default() };
    for (path, mtime) in recordings_for(dir, mount).into_iter().take(MAX_FILES) {
        let Ok(buf) = read_tail(&path) else { continue };
        let mut used = false;
        for (msg, p) in scan_frames(&buf).into_iter().rev() {
            if apply(&mut info, msg, p) {
                used = true;
                if !info.messages.contains(&msg) {
                    info.messages.push(msg);
                }
            }
        }
        if used && info.source_file.is_none() {
            info.source_file = Some(path.to_string_lossy().into_owned());
            info.updated_ms = mtime
                .duration_since(UNIX_EPOCH)
                .ok()
                .map(|d| d.as_millis() as i64);
        }
        // Position and equipment both found: older files add nothing.
        if info.ecef_x.is_some() && info.receiver_type.is_some() {
            break;
        }
    }
    info.messages.sort_unstable();
    (!info.messages.is_empty()).then_some(info)
}

#[tauri::command]
pub async fn get_station_info(output_dir: String, mount: String) -> Result<Option<StationInfo>, String> {
    tokio::task::spawn_blocking(move || station_info_blocking(Path::new(&output_dir), &mount))
        .await
        .map_err(|e| format!("station join error: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct W {
        bits: Vec<bool>,
    }
    impl W {
        fn put(&mut self, v: u64, n: usize) -> &mut Self {
            for i in (0..n).rev() {
                self.bits.push((v >> i) & 1 == 1);
            }
            self
        }
        fn s38(&mut self, v: i64) -> &mut Self {
            self.put((v as u64) & ((1 << 38) - 1), 38)
        }
        fn text(&mut self, s: &str) -> &mut Self {
            self.put(s.len() as u64, 8);
            for b in s.bytes() {
                self.put(b as u64, 8);
            }
            self
        }
        /// Zero-padded payload wrapped in a valid frame.
        fn frame(&self) -> Vec<u8> {
            let mut p: Vec<u8> = self
                .bits
                .chunks(8)
                .map(|c| (0..8).fold(0u8, |a, i| (a << 1) | *c.get(i).unwrap_or(&false) as u8))
                .collect();
            let mut f = vec![0xD3, (p.len() >> 8) as u8, p.len() as u8];
            f.append(&mut p);
            let c = crc24q(&f);
            f.extend([(c >> 16) as u8, (c >> 8) as u8, c as u8]);
            f
        }
    }

    fn f1005(id: u64, x: i64, y: i64, z: i64, h: Option<u64>) -> Vec<u8> {
        let mut w = W::default();
        w.put(if h.is_some() { 1006 } else { 1005 }, 12).put(id, 12).put(0, 10);
        w.s38(x).put(0, 2).s38(y).put(0, 2).s38(z);
        if let Some(h) = h {
            w.put(h, 16);
        }
        w.frame()
    }

    fn f1033() -> Vec<u8> {
        let mut w = W::default();
        w.put(1033, 12).put(7, 12).text("TRM59800.00").put(3, 8).text("ANT123");
        w.text("TRIMBLE ALLOY").text("5.45").text("RX999");
        w.frame()
    }

    fn tmpdir(tag: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!("station-{tag}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&d);
        std::fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn crc24q_known_vector() {
        assert_eq!(crc24q(b"123456789"), 0xCDE703);
    }

    #[test]
    fn scans_through_garbage_timestamp_and_truncation() {
        let mut buf = vec![0x00, 0xD3, 0x00, 0x01, 0xFF, 0x12]; // garbage incl. fake preamble
        let ts = {
            let mut w = W::default();
            w.put(4062, 12).put(0, 52);
            w.frame()
        };
        buf.extend(&ts);
        buf.extend(f1005(7, 1, 2, 3, None));
        let mut bad = f1005(7, 1, 2, 3, None);
        let n = bad.len();
        bad[n - 1] ^= 1; // bad CRC
        buf.extend(&bad);
        let tail = f1005(8, 1, 2, 3, None);
        buf.extend(&tail[..tail.len() - 2]); // truncated
        let nums: Vec<u32> = scan_frames(&buf).iter().map(|f| f.0).collect();
        assert_eq!(nums, vec![4062, 1005]);
    }

    #[test]
    fn decodes_1005_1006_1033() {
        let mut i = StationInfo::default();
        let f = f1005(7, -40_000_001_234, 20_000_000_001, -35_000_000_000, Some(12_345));
        let (m, p) = scan_frames(&f)[0];
        assert!(apply(&mut i, m, p));
        assert_eq!(i.station_id, Some(7));
        assert!((i.ecef_x.unwrap() + 4_000_000.123_4).abs() < 1e-6);
        assert!((i.ecef_y.unwrap() - 2_000_000.000_1).abs() < 1e-6);
        assert!((i.ecef_z.unwrap() + 3_500_000.0).abs() < 1e-6);
        assert!((i.antenna_height.unwrap() - 1.2345).abs() < 1e-9);

        let f = f1033();
        let (m, p) = scan_frames(&f)[0];
        assert!(apply(&mut i, m, p));
        assert_eq!(i.antenna_desc.as_deref(), Some("TRM59800.00"));
        assert_eq!(i.antenna_setup_id, Some(3));
        assert_eq!(i.antenna_serial.as_deref(), Some("ANT123"));
        assert_eq!(i.receiver_type.as_deref(), Some("TRIMBLE ALLOY"));
        assert_eq!(i.receiver_firmware.as_deref(), Some("5.45"));
        assert_eq!(i.receiver_serial.as_deref(), Some("RX999"));
    }

    #[test]
    fn reads_newest_file_for_mount_only() {
        let d = tmpdir("select");
        let mut a = vec![0u8; 5];
        a.extend(f1005(1, 10, 20, 30, None)); // OTHER mount
        std::fs::write(d.join("OTHER_2024001_OBS.rtcm"), &a).unwrap();
        let mut old = f1005(5, 10, 20, 30, None);
        std::fs::write(d.join("ALIC_2024001_OBS.rtcm"), &old).unwrap();
        std::thread::sleep(std::time::Duration::from_millis(30));
        old = f1005(7, 100_000, 200_000, 300_000, Some(5));
        old.extend(f1033());
        old.extend([0xD3, 0x00]); // truncated tail
        std::fs::write(d.join("ALIC_2024002_OBS.rtcm"), &old).unwrap();

        let i = station_info_blocking(&d, "alic").unwrap();
        assert_eq!(i.station_id, Some(7));
        assert_eq!(i.messages, vec![1006, 1033]);
        assert!(i.source_file.unwrap().ends_with("ALIC_2024002_OBS.rtcm"));
        assert!(i.updated_ms.is_some());
        assert!(station_info_blocking(&d, "NONE").is_none());
        std::fs::remove_dir_all(&d).unwrap();
    }
}
