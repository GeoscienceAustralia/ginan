use super::*;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt};

/// A SISNet `*MSG` line holding one SBAS message of `msg_type`, with a valid
/// CRC, laid out as `DS2DCParser` expects for `freq`.
fn msg_line(week: i64, tow: f64, msg_type: u8, freq: u8) -> String {
    // 226 data bits: preamble or sync, the 6-bit type, then a fixed pattern.
    let mut bits = vec![0u8; 226];
    let set = |bits: &mut Vec<u8>, at: usize, width: usize, value: u32| {
        for k in 0..width {
            bits[at + k] = ((value >> (width - 1 - k)) & 1) as u8;
        }
    };
    if freq == 1 {
        set(&mut bits, 0, 8, 0x53);
        set(&mut bits, 8, 6, msg_type as u32);
    } else {
        set(&mut bits, 0, 4, 0xA);
        set(&mut bits, 4, 6, msg_type as u32);
    }
    // Zeros, then a pattern: SISNet run-length codes the zeros.
    for (k, b) in bits.iter_mut().enumerate().skip(120) {
        *b = (k % 3 == 0) as u8;
    }
    // CRC over 6 zero bits then the 226, as 29 bytes.
    let pack = |bits: &[u8]| -> Vec<u8> {
        bits.chunks(8).map(|c| c.iter().fold(0u8, |a, b| (a << 1) | b)).collect()
    };
    let mut framed = vec![0u8; 6];
    framed.extend(&bits);
    let crc = crc24q(&pack(&framed));
    let mut all = bits.clone();
    all.extend((0..24).map(|k| ((crc >> (23 - k)) & 1) as u8));
    all.extend([0u8; 6]);
    let hex: String = pack(&all).iter().map(|b| format!("{b:02X}")).collect();
    format!("*MSG,{week},{tow:.3},{hex}*5A")
}

#[test]
fn crc24q_matches_the_standard_check_value() {
    assert_eq!(crc24q(b"123456789"), 0xCD_E703);
}

#[test]
fn decodes_type_time_and_crc_on_l1_and_l5() {
    let l1 = parse_msg(&msg_line(2390, 345_600.0, 2, 1), 1).expect("L1 message");
    assert_eq!(l1, SisnetMessage { gpst_ms: GPS_EPOCH_UNIX_MS + 2390 * 604_800_000 + 345_600_000, msg_type: 2, crc_ok: true });
    let l5 = parse_msg(&msg_line(2390, 345_601.0, 35, 5), 5).expect("L5 message");
    assert_eq!((l5.msg_type, l5.crc_ok), (35, true));
}

#[test]
fn flags_a_message_that_fails_its_crc() {
    let line = msg_line(2390, 345_600.0, 2, 1);
    // Flip one data nibble well inside the message.
    let i = line.find(",345600.000,").unwrap() + 12 + 20;
    let mut bad = line.clone();
    let flipped = if &line[i..i + 1] == "F" { "0" } else { "F" };
    bad.replace_range(i..i + 1, flipped);
    assert!(!parse_msg(&bad, 1).unwrap().crc_ok);
}

#[test]
fn expands_sisnet_run_length_coding() {
    let line = msg_line(2390, 345_600.0, 2, 1);
    let (head, rest) = line.split_at(line.find(",345600.000,").unwrap() + 12);
    // Rewrite the longest run of one character as `c|n` or `c/nn`.
    let bytes: Vec<char> = rest.chars().collect();
    let (mut best, mut at, mut i) = (0, 0, 0);
    while i < bytes.len() {
        let mut j = i;
        while j < bytes.len() && bytes[j] == bytes[i] {
            j += 1;
        }
        if j - i > best && bytes[i] != '*' {
            (best, at) = (j - i, i);
        }
        i = j;
    }
    assert!(best >= 3, "the pattern has a run to compress");
    let marker = if best < 16 { format!("|{best:X}") } else { format!("/{best:02X}") };
    let coded = format!("{head}{}{}{marker}{}", &rest[..at], bytes[at], &rest[at + best..]);
    assert_eq!(parse_msg(&coded, 1), parse_msg(&line, 1));
}

#[test]
fn rejects_lines_that_are_not_messages() {
    assert!(parse_msg("*AUTH,ok", 1).is_none());
    assert!(parse_msg("*MSG,2390,345600.000,ZZ", 1).is_none());
    assert!(parse_msg("*MSG,2390,345600.000,53", 1).is_none(), "too short for a frame");
}

#[test]
fn counts_messages_types_errors_and_rate() {
    let mut c = SbasCounts::default();
    for (k, t) in [2u8, 3, 2, 25].into_iter().enumerate() {
        c.ingest(&msg_line(2390, 345_600.0 + k as f64, t, 1), 1);
    }
    c.ingest("*MSG,2390,345604.000,garbage", 1);
    c.ingest("", 1);
    assert_eq!((c.messages, c.invalid_lines, c.crc_errors), (4, 1, 0));
    assert_eq!(c.by_type, BTreeMap::from([(2, 2), (3, 1), (25, 1)]));
    assert_eq!(c.messages_per_s(), Some(1.0));
}

/// A one-connection SISNet stand-in: checks the login, then sends `lines`.
async fn fake_sisnet(accept: bool, lines: Vec<String>) -> String {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap().to_string();
    tokio::spawn(async move {
        let (sock, _) = listener.accept().await.unwrap();
        let (r, mut w) = sock.into_split();
        let mut r = tokio::io::BufReader::new(r).lines();
        let auth = r.next_line().await.unwrap().unwrap();
        if !accept || auth != "AUTH,simon,s3cret" {
            w.write_all(b"*ERR,bad login\n").await.ok();
            return;
        }
        w.write_all(b"*AUTH,simon\n").await.unwrap();
        assert_eq!(r.next_line().await.unwrap().unwrap(), "START");
        w.write_all(b"*START\n").await.unwrap();
        for l in lines {
            w.write_all(format!("{l}\n").as_bytes()).await.unwrap();
        }
        tokio::time::sleep(Duration::from_secs(5)).await;
    });
    addr
}

fn login() -> Option<(String, String)> {
    Some(("simon".into(), "s3cret".into()))
}

#[tokio::test]
async fn probe_logs_in_and_counts_what_arrives() {
    let lines = (0..3).map(|k| msg_line(2390, 345_600.0 + k as f64, 2, 5)).collect();
    let addr = fake_sisnet(true, lines).await;
    let p = probe_at(&addr, login(), "DFMC", Duration::from_millis(400), Arc::default()).await;
    assert_eq!(p.state, ProbeState::Ok, "{:?}", p.error);
    assert_eq!(p.counts.messages, 3);
    assert_eq!(p.messages_per_s, Some(1.0));
    assert_eq!((p.prn, p.mode.as_str()), (122, "DFMC"));
}

#[tokio::test]
async fn probe_reports_a_refused_login_without_repeating_it() {
    let addr = fake_sisnet(false, vec![]).await;
    let p = probe_at(&addr, login(), "PVS", Duration::from_millis(200), Arc::default()).await;
    assert_eq!(p.state, ProbeState::AuthFailed);
    let json = serde_json::to_string(&p).unwrap();
    assert!(!json.contains("s3cret") && !json.contains("simon"), "{json}");
}

#[tokio::test]
async fn probe_without_a_login_or_a_host() {
    let p = probe_at("127.0.0.1:9", None, "PVS", Duration::from_millis(200), Arc::default()).await;
    assert_eq!(p.state, ProbeState::NoLogin);

    let closed = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap().local_addr().unwrap().to_string();
    let p = probe_at(&closed, login(), "PVS", Duration::from_millis(200), Arc::default()).await;
    assert_eq!(p.state, ProbeState::Unreachable);
}

#[tokio::test]
async fn probe_with_a_silent_stream_is_no_data() {
    let addr = fake_sisnet(true, vec![]).await;
    let p = probe_at(&addr, login(), "PVS", Duration::from_millis(300), Arc::default()).await;
    assert_eq!(p.state, ProbeState::NoData);
}

#[test]
fn console_counts_peas_sisnet_lines_only() {
    let mut c = SbasConsole::default();
    c.observe("[error] CRC error in SISNET channel", 10);
    c.observe("[error] Invalid SISNET message:  junk", 11);
    c.observe("[error] Error in Authentication : Invalid Server Response:  *ERR", 12);
    c.observe("[debug] SISNET message: 53089A", 13);
    c.observe("Some other PEA line about SSR", 14);
    assert_eq!((c.crc_errors, c.invalid_lines, c.auth_failures, c.messages), (1, 1, 1, 1));
    assert_eq!(c.last_error.as_deref(), Some("SISNet did not accept the login"));
    assert_eq!(c.last_error_at_ms, Some(12));
}

#[test]
fn spp_levels_follow_the_newest_file_and_wait_for_whole_lines() {
    let dir = std::env::temp_dir().join(format!("ginan-spp-levels-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("ALIC_202630001.SPP");
    let sample = include_str!("../../fixtures/sample_sbas.spp");
    let lines: Vec<&str> = sample.split('\n').collect(); // "", legend, 3 epochs
    let mut levels = SppLevels::default();

    // Header and first epoch, then half of the second.
    let half = &lines[3][..lines[3].len() / 2];
    std::fs::write(&path, format!("\n{}\n{}\n{half}", lines[1], lines[2])).unwrap();
    levels.poll(&dir);
    let points = |l: &SppLevels| l.to_json()["points"].as_array().unwrap().len();
    assert_eq!(points(&levels), 1);

    // The rest of the second, then the third.
    std::fs::write(&path, format!("\n{}\n{}\n{}\n{}", lines[1], lines[2], lines[3], lines[4])).unwrap();
    levels.poll(&dir);
    let v = levels.to_json();
    assert_eq!(v["file"], "ALIC_202630001.SPP");
    assert_eq!(points(&levels), 3);
    assert_eq!(v["points"][1][1], 5.2);
    assert_eq!(v["points"][1][2], 8.1);

    std::fs::remove_dir_all(&dir).ok();
}
