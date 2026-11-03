#!/usr/bin/env bash
# Prepare a Waveshare LC29H(AA) on a CP2102N bridge, then hand the port to Ginan.
#
# Two reasons this wrapper is needed:
#  1. RTCM3 output is OFF by default on LC29H(AA). It is enabled at runtime with
#     the MediaTek $PAIR commands (NOT the Quectel $PQTM RTK set, which this
#     firmware does not implement). The setting lives in RAM, so it must be
#     re-sent after every power cycle -- hence here, not saved to flash.
#  2. Ginan's SerialStream (src/cpp/common/streamSerial.cpp) calls open() and
#     never touches termios, so it inherits the tty default (9600) and reads
#     framing garbage. macOS also resets a /dev/cu.* back to 9600 as soon as the
#     last fd closes, so a plain `stty` beforehand does nothing. We keep one fd
#     open at 115200 -- without ever reading it -- so Ginan gets the bytes.
#
# Usage: start_gnss_stream.sh [PORT] [pea args...]
#   PORT is a device path or a `serial://` URI; omitted, it falls back to the
#   PORT env var, then to the newest CP2102N port. Remaining args go to pea.
set -euo pipefail

PORT="${PORT:-}"
if [[ $# -gt 0 && $1 != -* ]]; then   # pea args all start with '-'
  PORT="${1#serial://}"
  shift
fi
BAUD="${BAUD:-115200}"          # LC29H default; knob for other firmware
DATA="${DATA:-8}"               # data bits: 5-8
PARITY="${PARITY:-none}"        # none | even | odd
STOP="${STOP:-1}"               # stop bits: 1 | 2
FLOW="${FLOW:-none}"            # none | rtscts | xonxoff
FLUSH="${FLUSH:-true}"          # drop whatever the port buffered before we opened it
FLUSH_MS="${FLUSH_MS:-0}"       # then also read and discard for this long
export DATA PARITY STOP FLOW FLUSH FLUSH_MS
CONFIG="${CONFIG:-}"
PEA="${PEA:-./pea}"

if [[ -z $PORT ]]; then
  # shellcheck disable=SC2012
  PORT=$(ls -t /dev/cu.usbserial-* /dev/cu.slab_USBtoUART* 2>/dev/null | head -1 || true)
fi
[[ -n $PORT ]] || { echo "no CP2102N serial port found (pass the port as the first argument)" >&2; exit 1; }
[[ -c $PORT ]] || { echo "port    : $PORT is not a serial device (check the name with: ls /dev/cu.*)" >&2; exit 1; }
if command -v lsof >/dev/null && lsof "$PORT" >/dev/null 2>&1; then
  echo "port    : $PORT is already open by another process" >&2; exit 1
fi
echo "port    : $PORT @ ${BAUD} ${DATA}${PARITY:0:1}${STOP} flow=${FLOW}"

ready=$(mktemp)
trap 'kill $HOLDER 2>/dev/null || true; rm -f "$ready"' EXIT

# Configure, enable RTCM, report what is actually on the wire, then hold the
# port open without reading so Ginan receives every subsequent byte.
python3 - "$PORT" "$BAUD" "$ready" <<'PY' &
import collections, functools, os, select, sys, termios, time

port, baud, ready = sys.argv[1], sys.argv[2], sys.argv[3]

def nmea(body):
    cs = functools.reduce(lambda a, c: a ^ c, body.encode(), 0)
    return ("$%s*%02X\r\n" % (body, cs)).encode()

def crc24q(d):
    c = 0
    for b in d:
        c ^= b << 16
        for _ in range(8):
            c <<= 1
            if c & 0x1000000:
                c ^= 0x1864CFB
    return c & 0xFFFFFF

def frames(data):
    """Yield CRC-valid RTCM3 message numbers."""
    i = 0
    while i < len(data) - 6:
        if data[i] != 0xD3:
            i += 1
            continue
        ln = ((data[i + 1] & 0x03) << 8) | data[i + 2]
        if ln == 0 or i + 6 + ln > len(data):
            i += 1
            continue
        if crc24q(data[i:i + 3 + ln]) == int.from_bytes(data[i + 3 + ln:i + 6 + ln], 'big'):
            yield (data[i + 3] << 4) | (data[i + 4] >> 4)
            i += 6 + ln
        else:
            i += 1

fd = os.open(port, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
a = termios.tcgetattr(fd)
env = os.environ
try:
    speed = getattr(termios, 'B' + baud)
    csize = {'5': termios.CS5, '6': termios.CS6, '7': termios.CS7, '8': termios.CS8}[env['DATA']]
    parity = {'none': 0, 'even': termios.PARENB, 'odd': termios.PARENB | termios.PARODD}[env['PARITY']]
    stop = {'1': 0, '2': termios.CSTOPB}[env['STOP']]
    flow = env['FLOW']
    if flow not in ('none', 'rtscts', 'xonxoff'):
        raise KeyError(flow)
except (AttributeError, KeyError) as e:
    sys.exit("bad line setting %s (BAUD=%s DATA=%s PARITY=%s STOP=%s FLOW=%s)"
             % (e, baud, env['DATA'], env['PARITY'], env['STOP'], env['FLOW']))
a[0] = termios.IXON | termios.IXOFF if flow == 'xonxoff' else 0   # raw input
a[1] = a[3] = 0                                                   # raw output, no echo
a[2] = csize | parity | stop | termios.CREAD | termios.CLOCAL     # ignore modem lines
if flow == 'rtscts':
    a[2] |= termios.CRTSCTS
a[4] = a[5] = speed
a[6][termios.VMIN], a[6][termios.VTIME] = 0, 0
termios.tcsetattr(fd, termios.TCSANOW, a)
if env['FLUSH'] == 'true':
    termios.tcflush(fd, termios.TCIOFLUSH)               # drop buffer garbage
    end = time.time() + int(env['FLUSH_MS']) / 1000
    while time.time() < end:                             # and anything still arriving
        if select.select([fd], [], [], 0.05)[0]:
            try:
                os.read(fd, 8192)
            except BlockingIOError:
                pass

acks = {}
for cmd, what in (("PAIR432,1", "MSM7 observations"),
                  ("PAIR434,1", "RTCM 1005 station position"),
                  ("PAIR436,1", "broadcast ephemerides")):
    os.write(fd, nmea(cmd))
    deadline, got = time.time() + 1.0, None
    buf = b""
    while time.time() < deadline and got is None:
        if select.select([fd], [], [], 0.2)[0]:
            try:
                buf += os.read(fd, 8192)
            except BlockingIOError:
                continue
            for line in buf.split(b"\r\n"):
                if line.startswith(b"$PAIR001," + cmd[4:7].encode()):
                    got = line.split(b",")[2].split(b"*")[0].decode()
    acks[what] = got
    print("enable  : %-28s ack=%s" % (what, "0 (ok)" if got == "0" else got or "no reply"))

buf, end = bytearray(), time.time() + 5
while time.time() < end:
    if select.select([fd], [], [], 0.3)[0]:
        try:
            buf += os.read(fd, 8192)
        except BlockingIOError:
            continue

seen = collections.Counter(frames(bytes(buf)))
nmea_n = sum(1 for l in bytes(buf).split(b"\r\n") if l[:3] in (b"$GN", b"$GP", b"$GL", b"$GA", b"$GB", b"$GQ"))
print("stream  : %d bytes/5s, %d NMEA sentences, %d valid RTCM3 frames"
      % (len(buf), nmea_n, sum(seen.values())))
if seen:
    print("rtcm    : " + "  ".join("%d x%d" % (m, n) for m, n in sorted(seen.items())))
obs = sorted(m for m in seen if 1071 <= m <= 1137)
print("obs msgs: %s" % (", ".join(map(str, obs)) if obs else "NONE -- Ginan will get zero observations"))

for line in bytes(buf).split(b"\r\n"):
    if line.startswith(b"$GNGGA"):
        q = line.split(b",")[6].decode()
        print("fix     : GGA quality=%s (0=none 1=SPS 2=DGPS 4=RTK-fix 5=RTK-float)" % q)
        break

open(ready, "w").write("1" if obs else "")
sys.stdout.flush()
# Hold the line, never read. Exit if the bash wrapper disappears without
# stopping us (e.g. SIGKILL skips its EXIT trap), so the port is never orphaned.
parent = os.getppid()
while os.getppid() == parent:
    time.sleep(1)
PY
HOLDER=$!

for _ in $(seq 60); do
  [[ -s $ready ]] && break
  kill -0 $HOLDER 2>/dev/null || { echo "holder died -- could not open $PORT" >&2; exit 1; }
  sleep 0.5
done
[[ -s $ready ]] || { echo "no RTCM observation messages on $PORT -- not launching Ginan" >&2; exit 1; }

if [[ -z $CONFIG ]]; then
  echo "ready   : set CONFIG=<yaml> to launch Ginan; holding port, ^C to stop"
  # Whoever started us (e.g. the Ginan app) may die without stopping us, e.g. on
  # a crash. When it does, we are reparented: exit so the trap frees the port.
  parent=$PPID
  while [[ $(ps -o ppid= -p $$ | tr -d ' ') == "$parent" ]]; do
    kill -0 $HOLDER 2>/dev/null || { echo "holder died -- port no longer held" >&2; exit 1; }
    sleep 1
  done
  echo "parent $parent exited -- releasing $PORT" >&2
  exit 0
fi
echo "launch  : $PEA --config $CONFIG $*"
# NOT exec: exec would replace this shell and the EXIT trap would never
# run, orphaning the holder and leaving the port locked.
"$PEA" --config "$CONFIG" "$@"
