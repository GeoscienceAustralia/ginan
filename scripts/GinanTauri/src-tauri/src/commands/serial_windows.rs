//! Serial receiver setup on Windows: the same job as
//! `scripts/serial/start_gnss_stream.sh`, without bash or python3.
//!
//! It puts the COM port in the requested line mode, turns on the LC29H's RTCM
//! output with the `$PAIR` commands, checks that observations arrive, and then
//! closes the port. Unlike macOS, Windows keeps a port's line settings and
//! timeouts after the last handle closes, so PEA (which opens the port with
//! `CreateFileA` and never configures it) inherits them. Nothing has to hold
//! the port open, and nothing could: PEA opens it exclusively.
//!
//! The byte-level helpers are plain Rust so they are tested on every platform.
#![cfg_attr(not(windows), allow(dead_code))]

use std::collections::BTreeMap;

/// The `$PAIR` commands that enable RTCM, and what each turns on.
const RTCM_ENABLES: [(&str, &str); 3] = [
    ("PAIR432,1", "MSM7 observations"),
    ("PAIR434,1", "RTCM 1005 station position"),
    ("PAIR436,1", "broadcast ephemerides"),
];

/// `body` as an NMEA sentence: `$body*CS\r\n`.
fn nmea(body: &str) -> Vec<u8> {
    let cs = body.bytes().fold(0u8, |a, b| a ^ b);
    format!("${body}*{cs:02X}\r\n").into_bytes()
}

/// The receiver's ack code for a `$PAIRnnn` command, if `buf` holds one.
fn pair_ack(buf: &[u8], cmd: &str) -> Option<String> {
    let prefix = format!("$PAIR001,{},", &cmd[4..7]);
    String::from_utf8_lossy(buf)
        .split("\r\n")
        .find_map(|l| l.strip_prefix(&prefix).map(|rest| rest.split('*').next().unwrap_or("").to_string()))
}

fn crc24q(data: &[u8]) -> u32 {
    let mut c = 0u32;
    for &b in data {
        c ^= (b as u32) << 16;
        for _ in 0..8 {
            c <<= 1;
            if c & 0x100_0000 != 0 {
                c ^= 0x186_4CFB;
            }
        }
    }
    c & 0xFF_FFFF
}

/// Message numbers of the CRC-valid RTCM3 frames in `data`, with counts.
fn rtcm_messages(data: &[u8]) -> BTreeMap<u16, u32> {
    let mut seen = BTreeMap::new();
    let mut i = 0;
    while i + 6 < data.len() {
        let len = (((data[i + 1] & 0x03) as usize) << 8) | data[i + 2] as usize;
        let end = i + 6 + len;
        if data[i] != 0xD3 || len == 0 || end > data.len() {
            i += 1;
            continue;
        }
        let crc = u32::from_be_bytes([0, data[end - 3], data[end - 2], data[end - 1]]);
        if crc24q(&data[i..end - 3]) == crc {
            *seen.entry(((data[i + 3] as u16) << 4) | (data[i + 4] as u16 >> 4)).or_insert(0) += 1;
            i = end;
        } else {
            i += 1;
        }
    }
    seen
}

/// The preflight report for five seconds of receiver output, and whether it
/// carries observations (MSM 1071–1137), without which PEA gets nothing.
fn report(buf: &[u8]) -> (Vec<String>, bool) {
    let seen = rtcm_messages(buf);
    let text = String::from_utf8_lossy(buf);
    let lines: Vec<&str> = text.split("\r\n").collect();
    let nmea_n = lines
        .iter()
        .filter(|l| ["$GN", "$GP", "$GL", "$GA", "$GB", "$GQ"].iter().any(|p| l.starts_with(p)))
        .count();
    let mut out = vec![format!(
        "stream  : {} bytes/5s, {nmea_n} NMEA sentences, {} valid RTCM3 frames",
        buf.len(),
        seen.values().sum::<u32>()
    )];
    if !seen.is_empty() {
        let msgs: Vec<String> = seen.iter().map(|(m, n)| format!("{m} x{n}")).collect();
        out.push(format!("rtcm    : {}", msgs.join("  ")));
    }
    let obs: Vec<String> = seen.keys().filter(|m| (1071..=1137).contains(*m)).map(u16::to_string).collect();
    out.push(format!(
        "obs msgs: {}",
        if obs.is_empty() { "NONE -- Ginan will get zero observations".into() } else { obs.join(", ") }
    ));
    if let Some(q) = lines.iter().find(|l| l.starts_with("$GNGGA")).and_then(|l| l.split(',').nth(6)) {
        out.push(format!("fix     : GGA quality={q} (0=none 1=SPS 2=DGPS 4=RTK-fix 5=RTK-float)"));
    }
    (out, !obs.is_empty())
}

/// COM ports Windows knows about, from the registry key serial drivers fill in.
#[cfg(windows)]
pub fn list_ports() -> Vec<String> {
    use std::os::windows::process::CommandExt;
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    let Ok(out) = std::process::Command::new("reg")
        .args(["query", r"HKLM\HARDWARE\DEVICEMAP\SERIALCOMM"])
        .creation_flags(CREATE_NO_WINDOW)
        .output()
    else {
        return Vec::new();
    };
    let mut ports: Vec<String> = String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter(|l| l.contains("REG_SZ"))
        .filter_map(|l| l.split_whitespace().last().map(str::to_string))
        .collect();
    // COM2 before COM10.
    ports.sort_by_key(|p| p.trim_start_matches("COM").parse::<u32>().unwrap_or(u32::MAX));
    ports
}

#[cfg(windows)]
pub use win::prepare;

#[cfg(windows)]
mod win {
    use std::time::{Duration, Instant};

    use windows_sys::Win32::Devices::Communication::*;
    use windows_sys::Win32::Foundation::*;
    use windows_sys::Win32::Storage::FileSystem::*;

    use super::{nmea, pair_ack, report, RTCM_ENABLES};
    use crate::commands::streams::SerialSettings;

    struct Port(HANDLE);

    impl Drop for Port {
        fn drop(&mut self) {
            unsafe { CloseHandle(self.0) };
        }
    }

    fn check(ok: BOOL, what: &str) -> Result<(), String> {
        if ok == 0 {
            return Err(format!("{what} failed: {}", std::io::Error::last_os_error()));
        }
        Ok(())
    }

    impl Port {
        fn open(device: &str, s: &SerialSettings) -> Result<Self, String> {
            // The \\.\ form is the only one that opens COM10 and up.
            let path: Vec<u16> = format!(r"\\.\{device}").encode_utf16().chain([0]).collect();
            let h = unsafe {
                CreateFileW(
                    path.as_ptr(),
                    GENERIC_READ | GENERIC_WRITE,
                    0,
                    std::ptr::null(),
                    OPEN_EXISTING,
                    0,
                    std::ptr::null_mut(),
                )
            };
            if h == INVALID_HANDLE_VALUE {
                let e = std::io::Error::last_os_error();
                return Err(match e.raw_os_error() {
                    Some(5) => format!("{device} is already open by another program (Access denied)"),
                    Some(2) => format!("{device} does not exist (check Device Manager › Ports)"),
                    _ => format!("Cannot open {device}: {e}"),
                });
            }
            let port = Port(h);

            let mut dcb: DCB = unsafe { std::mem::zeroed() };
            dcb.DCBlength = std::mem::size_of::<DCB>() as u32;
            check(unsafe { GetCommState(h, &mut dcb) }, "GetCommState")?;
            dcb.BaudRate = s.baud;
            dcb.ByteSize = s.data;
            dcb.Parity = match s.parity.as_str() {
                "none" => NOPARITY,
                "even" => EVENPARITY,
                "odd" => ODDPARITY,
                p => return Err(format!("bad parity `{p}`")),
            };
            dcb.StopBits = match s.stop {
                1 => ONESTOPBIT,
                2 => TWOSTOPBITS,
                n => return Err(format!("bad stop bits `{n}`")),
            };
            // DCB flag bits: fBinary 0, fParity 1, fOutxCtsFlow 2, fDtrControl 4-5,
            // fOutX 8, fInX 9, fRtsControl 12-13. Everything else off.
            // windows-sys has no constants for the DTR/RTS control values.
            const DTR_CONTROL_ENABLE: u32 = 1;
            const RTS_CONTROL_ENABLE: u32 = 1;
            const RTS_CONTROL_HANDSHAKE: u32 = 2;
            let mut flags = 1 | (DTR_CONTROL_ENABLE << 4);
            if s.parity != "none" {
                flags |= 1 << 1;
            }
            match s.flow.as_str() {
                "none" => flags |= RTS_CONTROL_ENABLE << 12,
                "rtscts" => flags |= (1 << 2) | (RTS_CONTROL_HANDSHAKE << 12),
                "xonxoff" => {
                    flags |= (1 << 8) | (1 << 9) | (RTS_CONTROL_ENABLE << 12);
                    dcb.XonChar = 0x11;
                    dcb.XoffChar = 0x13;
                }
                f => return Err(format!("bad flow control `{f}`")),
            }
            dcb._bitfield = flags;
            check(unsafe { SetCommState(h, &dcb) }, "SetCommState")?;

            // Reads return at once with whatever is buffered. PEA reads until a
            // read returns nothing, so with blocking reads it would stall.
            let timeouts = COMMTIMEOUTS {
                ReadIntervalTimeout: u32::MAX,
                ReadTotalTimeoutMultiplier: 0,
                ReadTotalTimeoutConstant: 0,
                WriteTotalTimeoutMultiplier: 0,
                WriteTotalTimeoutConstant: 1000,
            };
            check(unsafe { SetCommTimeouts(h, &timeouts) }, "SetCommTimeouts")?;
            Ok(port)
        }

        fn read(&self, buf: &mut Vec<u8>) {
            let mut chunk = [0u8; 8192];
            let mut n = 0u32;
            let ok = unsafe { ReadFile(self.0, chunk.as_mut_ptr(), chunk.len() as u32, &mut n, std::ptr::null_mut()) };
            if ok != 0 {
                buf.extend_from_slice(&chunk[..n as usize]);
            }
            if n == 0 {
                std::thread::sleep(Duration::from_millis(50));
            }
        }

        fn write(&self, data: &[u8]) -> Result<(), String> {
            let mut n = 0u32;
            check(
                unsafe { WriteFile(self.0, data.as_ptr(), data.len() as u32, &mut n, std::ptr::null_mut()) },
                "WriteFile",
            )
        }

        /// Everything that arrives in the next `d`.
        fn collect(&self, d: Duration) -> Vec<u8> {
            let (mut buf, end) = (Vec::new(), Instant::now() + d);
            while Instant::now() < end {
                self.read(&mut buf);
            }
            buf
        }
    }

    /// Configure `device`, enable RTCM and check observations arrive, calling
    /// `log` with each report line. The port is closed again on return.
    pub fn prepare(device: &str, s: &SerialSettings, log: impl Fn(String)) -> Result<(), String> {
        let port = Port::open(device, s)?;
        log(format!(
            "port    : {device} @ {} {}{}{} flow={}",
            s.baud,
            s.data,
            &s.parity[..1],
            s.stop,
            s.flow
        ));
        if s.flush {
            unsafe { PurgeComm(port.0, PURGE_RXCLEAR | PURGE_TXCLEAR) };
            port.collect(Duration::from_millis(s.flush_duration_ms.into()));
        }

        for (cmd, what) in RTCM_ENABLES {
            port.write(&nmea(cmd))?;
            let (mut buf, end, mut ack) = (Vec::new(), Instant::now() + Duration::from_secs(1), None);
            while Instant::now() < end && ack.is_none() {
                port.read(&mut buf);
                ack = pair_ack(&buf, cmd);
            }
            let ack = match ack.as_deref() {
                Some("0") => "0 (ok)",
                Some(a) => a,
                None => "no reply",
            };
            log(format!("enable  : {what:<28} ack={ack}"));
        }

        let (lines, has_obs) = report(&port.collect(Duration::from_secs(5)));
        lines.into_iter().for_each(&log);
        if !has_obs {
            return Err(format!("No RTCM observation messages on {device}"));
        }
        log(format!("ready   : {device} configured; PEA opens it next"));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An RTCM3 frame carrying message `num`, with a valid CRC.
    fn frame(num: u16) -> Vec<u8> {
        let mut f = vec![0xD3, 0x00, 0x02, (num >> 4) as u8, ((num & 0xF) << 4) as u8];
        let crc = crc24q(&f);
        f.extend_from_slice(&crc.to_be_bytes()[1..]);
        f
    }

    #[test]
    fn nmea_checksum_matches_the_receiver_docs() {
        // $PQTMVERNO*58 is the query quoted in scripts/serial/README.md.
        assert_eq!(nmea("PQTMVERNO"), b"$PQTMVERNO*58\r\n");
    }

    #[test]
    fn finds_pair_acks_amid_nmea() {
        let buf = b"$GNGGA,1,2*00\r\n$PAIR001,434,0*3C\r\n$PAIR001,432,1*3B\r\n";
        assert_eq!(pair_ack(buf, "PAIR432,1").as_deref(), Some("1"));
        assert_eq!(pair_ack(buf, "PAIR434,1").as_deref(), Some("0"));
        assert_eq!(pair_ack(buf, "PAIR436,1"), None);
    }

    #[test]
    fn counts_valid_frames_and_skips_corrupt_ones() {
        let mut data = b"$GNGGA,x,x,x,x,x,1,x\r\n".to_vec();
        data.extend(frame(1077));
        data.extend(frame(1077));
        let mut bad = frame(1005);
        bad[4] ^= 0x01;
        data.extend(bad);
        data.extend(frame(1019));
        assert_eq!(rtcm_messages(&data), BTreeMap::from([(1019, 1), (1077, 2)]));

        let (lines, has_obs) = report(&data);
        assert!(has_obs);
        assert!(lines.iter().any(|l| l == "obs msgs: 1077"));
        assert!(lines.iter().any(|l| l.starts_with("fix     : GGA quality=1")));
        assert!(!report(&frame(1019)).1);
    }
}
