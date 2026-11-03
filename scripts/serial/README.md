# Ginan + Waveshare LC29H(AA) over USB serial

How to feed a Waveshare LC29H(AA) receiver to Ginan over USB serial, with
`start_gnss_stream.sh` preparing the port. This was verified against the
physical device on macOS with Ginan v4.1.3. Read the **Gotchas** section first:
three of the four are non-obvious.

## Hardware

| | |
|---|---|
| Board | Waveshare LC29H(AA) GPS/RTK HAT |
| Firmware | `LC29HAANR11A03S` (query with `$PQTMVERNO*58`) |
| USB bridge | CP2102N, Silicon Labs — VID `0x10C4` / PID `0xEA60` |
| Port | `/dev/cu.usbserial-*` on macOS (e.g. `/dev/cu.usbserial-2110`) |
| Line settings | 115200 8N1, no flow control |

## Gotchas (read this)

**1. RTCM3 output is OFF by default.** Out of the box the LC29H(AA) emits only
NMEA. Ginan has no NMEA input decoder — its decoders are RTCM, SBF and UBX
(`src/cpp/common/`), and the only NMEA code is `outputPppNmea`, which is output.
So with a factory-default receiver Ginan decodes exactly zero observations.

**2. Use `$PAIR` commands, NOT `$PQTM`.** This firmware ignores the Quectel RTK
command set. `$PQTMCFGRTCM` and `$PQTMCFGRCVRMODE` get *no reply at all*. The
MediaTek `$PAIR` set works and is what enables RTCM:

| Command | Enables | Expected ack |
|---|---|---|
| `$PAIR432,1` | MSM7 observations | `$PAIR001,432,0` |
| `$PAIR434,1` | RTCM 1005 station position | `$PAIR001,434,0` |
| `$PAIR436,1` | Broadcast ephemerides | `$PAIR001,436,0` |

Ack field `0` = OK. Anything else means it did not take.

**3. The RTCM setting lives in RAM and is lost on every power cycle.** Re-send
the three commands after each replug. `start_gnss_stream.sh` does this on every
launch, which is why you should always start Ginan through it rather than
running `pea` directly. (Deliberately not saved to flash — no flash wear, and
the config stays visible in the script instead of hidden in the device.)

**4. Ginan never sets the baud rate, and macOS resets it.**
`SerialStream::openStream()` (`src/cpp/common/streamSerial.cpp`) calls plain
`open()` and never touches termios, so it inherits whatever the tty currently
is. macOS resets a `/dev/cu.*` device to 9600 the moment the last file
descriptor closes — so running `stty ... 115200` and *then* launching Ginan does
nothing at all; the setting is gone before Ginan opens the port.

The fix, implemented in the script: hold one fd open at 115200 for the whole
session **without ever reading from it**, so the line stays configured and every
byte still goes to Ginan.

## Setup

Run these from a working directory that holds a `pea` binary and a real-time
config (`rt_ppp_config.yaml` here). `<ginan>` is your Ginan checkout.

```bash
# 1. Find the port
ls /dev/cu.usbserial-* /dev/cu.slab_USBtoUART* 2>/dev/null

# 2. Point the config at it (back up first)
cp rt_ppp_config.yaml config_backup.yaml
sed -i '' 's|serial:///dev/cu\.[A-Za-z0-9_-]*|serial:///dev/cu.usbserial-XXXX|' rt_ppp_config.yaml

# 3. Run
CONFIG=rt_ppp_config.yaml PEA=./pea <ginan>/scripts/serial/start_gnss_stream.sh /dev/cu.usbserial-XXXX -n 45
```

The YAML field is `inputs.gnss_observations.rtcm_inputs`, one entry:

```yaml
  gnss_observations:
    rtcm_inputs:
      - serial:///dev/cu.usbserial-2110
```

**Plain strings only.** A detailed dict (`port:`, `baud:`, `parity:` …) will NOT
work — `acsConfig.hpp:129` types this as `map<string, vector<string>>`, and
`inputs.cpp:180` just splits on `://` and hands the remainder to
`SerialStream(path)`. Baud belongs in the script, not the YAML.

## From the Ginan UI

In Realtime mode, set the observation stream source to **Direct URI** and enter
`serial:///dev/cu.usbserial-XXXX` (the field suggests the ports it finds). The
line settings below the URL go to the script as the env knobs listed at the end;
they do not change the PEA config. When you press Start, the app runs this script
for each `serial://` entry (no `CONFIG`, so it only prepares and holds the
port), waits for its `ready` line, then launches PEA. The script's output shows
in the Console with a `[serial]` prefix. The port is released when PEA exits or
you press Stop. Code: `scripts/GinanTauri/src-tauri/src/commands/streams.rs`.

### Windows

The script is not used on Windows (no bash/python3). The app does the same
setup natively (`src-tauri/src/commands/serial_windows.rs`): enter
`serial://COM3` (the field lists the COM ports in the registry), and on Start
it sets the line settings, sends the `$PAIR` enables, prints the same preflight
block, then **closes** the port. Windows, unlike macOS, keeps a COM port's
baud and timeouts after close, so PEA inherits them. It also sets reads to
non-blocking, which PEA's `ReadFile` loop needs. Only `COM1`–`COM9` work in
the URI: PEA opens the bare name, and Windows needs `\\.\COM10` for 10 and
up, which breaks PEA's receiver ID. Reassign the port number in Device
Manager › Ports › Advanced if needed.

## Expected output

`start_gnss_stream.sh` prints a preflight block before launching Ginan. A
healthy receiver looks like this:

```
port    : /dev/cu.usbserial-2110 @ 115200n81
enable  : MSM7 observations            ack=0 (ok)
enable  : RTCM 1005 station position   ack=0 (ok)
enable  : broadcast ephemerides        ack=0 (ok)
stream  : 14727 bytes/5s, 135 NMEA sentences, 55 valid RTCM3 frames
rtcm    : 1005 x5  1019 x5  1020 x2  1042 x10  1044 x2  1046 x6
          1077 x5  1087 x5  1097 x5  1117 x5  1127 x5
obs msgs: 1077, 1087, 1097, 1117, 1127
fix     : GGA quality=1 (0=none 1=SPS 2=DGPS 4=RTK-fix 5=RTK-float)
```

`obs msgs` must be non-empty — the script refuses to launch Ginan otherwise.
1077/1087/1097/1117/1127 are MSM7 for GPS / GLONASS / Galileo / QZSS / BeiDou.

Then from Ginan: `Synced 1/1 receivers` each epoch, and the receiver appears as
**`CU.U`** (Ginan derives the 4-char ID from `cu.usbserial-…`).

## Verifying it actually worked

From the run's output directory:

```bash
# Messages Ginan decoded from the receiver (not from the NTRIP streams)
grep '"Mountpoint":"cu.usbserial-2110"' decoded_rtcm_*.json \
  | grep -oE '"MessageType":"[^"]*"' | sort | uniq -c | sort -rn

# Position solutions -- NOTE the leading space on data rows
grep -cE '^ 2026-' CU.U_rt_ppp_config_*.POS
```

A good 45-epoch run gives ~2,100 MSM7 messages, ~230 ephemerides, and ~39
solution epochs with vertical sigma dropping from ~94 m to ~1.7 m.

## Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `no CP2102N serial port found` | Unplugged, or a different bridge chip | `ls /dev/cu.*`; set `PORT=` explicitly |
| `port is already open by another process` | Orphaned holder from a crashed run | `lsof /dev/cu.usbserial-*` then kill the PID |
| `ack=no reply` on a `$PAIR` command | Command raced the NMEA stream | Harmless if the others ack `0`; re-run |
| 0 RTCM frames, NMEA fine | RTCM never enabled (power cycled?) | Re-run the script — it re-sends the enables |
| Garbage bytes, 0 NMEA and 0 RTCM | Port at 9600, not 115200 | You launched `pea` directly; use the script |
| `SPP failed` every epoch | Config has no constellations enabled | Enable at least one constellation; a config with none only decodes |
| Position off by tens of metres | PPP has not converged | Normal. Needs 20–30 min, not 45 s |

## Known limits

- **No survey-in / base mode.** `$PQTMCFGRCVRMODE` is unsupported on this
  firmware, so RTCM 1005 carries an unsurveyed position. Fine for feeding
  observations to Ginan; not usable as a real RTK base without a known
  coordinate.
- **Port name is not stable.** `usbserial-2110` encodes the USB *port location*,
  so replugging into a different hub port renames it and the YAML goes stale.
  The script auto-detects; the YAML does not.
- **A few CRC failures per run are normal.** Ginan reports e.g. 7 failures
  against 245 decoded. NMEA and RTCM are interleaved on one UART, so stray
  `0xD3` bytes inside NMEA text look like false preambles. Only worry if the
  pass rate collapses.
- **45 s proves the pipeline, not the accuracy.** Run 20–30 min for a
  meaningful PPP solution.

## Files

| Path | What |
|---|---|
| `scripts/serial/start_gnss_stream.sh` | Port setup + RTCM enable + preflight + launch. Always use this. |
| `scripts/GinanTauri/src-tauri/resources/rt_ppp_config.yaml` | The real-time config template the desktop app starts from |
| `<output dir>/` | `decoded_rtcm_*.json`, `*.POS`, `*.GPX`, `*.TRACE` |

The port is the first argument (a device path or `serial://` URI); leave it
out to auto-detect the newest CP2102N port. Env knobs for the script: `PORT`, `BAUD` (default 115200), `DATA` (8),
`PARITY` (none|even|odd), `STOP` (1|2), `FLOW` (none|rtscts|xonxoff), `FLUSH`
(true), `FLUSH_MS` (0, extra discard time after the flush), `CONFIG`, `PEA`
(default `./pea`). Extra args pass through to `pea` — e.g. `-n 45` caps epochs.
