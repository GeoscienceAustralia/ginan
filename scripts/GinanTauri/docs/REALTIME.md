# Real-time processing

In real-time mode PEA reads observations from live streams instead of a RINEX
file: reference stations on an NTRIP caster, or a receiver on a serial port.
Orbit, clock and bias corrections arrive over the stream as SSR messages, so
the app does not download any products and does not use a time window. PEA
keeps running until you press **Stop**.

For installing the app and the first-run setup, see the
[user guide](USER_GUIDE.md).

## Before you start

- **An NTRIP account.** It is separate from your NASA Earthdata login. The
  default caster is Geoscience Australia's `ntrip.data.gnss.ga.gov.au`. Enter
  the login in **Settings > Credentials** (below), or in the optional
  "Livestream credentials" step of first-run setup.
- **An output directory**, set in **Settings > Output**. Recordings, the
  generated config and the position output all go there.
- **A PEA binary.** See [First run](USER_GUIDE.md#first-run).
- **A products folder that holds the static support files.** A live run
  downloads no dated products (no SP3, CLK or BIA files), but PEA still reads
  the ANTEX file and the other support tables from the products folder. The app
  downloads them at startup once a products folder is set and CDDIS answers
  with your Earthdata login, the same as for a file run.

## Switch to real-time mode

On the welcome screen press **Setup Real-time Processing**. If a file is
already open, press **Switch to Real-time** at the top of the sidebar and
confirm the prompt, which warns that it will clear your current input
settings. Switching to real-time closes the open RINEX file. The sidebar's
current mode then reads **Live stream (NTRIP)**. **Switch to File** takes you
back, with the same warning, and puts the stream lists back to their defaults.

The first time you enter real-time mode after setup, the app offers a guided
tour of the streaming screen.

## Set the caster

Open **Settings > Credentials** and find **NTRIP caster (live streams)**, below
the Earthdata login. The caster is machine setup, not per-run input, so it
lives here and not in the sidebar.

| Control | What it does |
| --- | --- |
| Caster URL | The server address only, such as `https://ntrip.data.gnss.ga.gov.au/`. Mountpoints go in the sidebar. Clear the field and click away to put the GA caster back. |
| **Sign in** / **Update sign-in** | Opens "Sign in to the caster". Enter the **Host**, **Username** and **Password** and press **Save**. The login goes to your netrc file. The status line reads "Signed in to <host>" or "Not signed in to <host>". |
| **Paste full URL…** | Takes one complete NTRIP URL of the form `https://USER:PASSWORD@host/MOUNTPOINT`, splits it, sets the caster, saves the login and adds the mountpoint to the right stream list. A mountpoint with `bkg`, `bcep`, `ssr`, `cne` or `nav` in its name goes to the correction list, any other to the observation list. |
| **Browse streams** | Reads the caster's sourcetable so the stream fields can suggest mountpoints. Shows "N streams on <host>". The refresh button beside it fetches the list again. |
| **Sign out** | Beside each saved host. Removes its stored login. |

The Caster URL field warns you if the URL carries a login ("Remove the username
and password from the URL…"), still has the old `<USER>:<PASS>` placeholders,
or ends in what looks like a mountpoint.

Never put real credentials in a screenshot, a bug report or a config file you
share. When PEA starts, the app adds the saved login to the caster URLs in the
generated `rt_ppp_config.yaml` so PEA can connect, which means that file in your
output directory can contain it. Treat it as private.

The login is written into two keys of the generated config, not one:
`inputs.gnss_observations.gnss_observations_root` and
`inputs.satellite_data.satellite_data_root`.

**Export config** writes `<USER>:<PASS>` in place of the login in both keys,
so an exported config never contains it. See [Export config](SETTINGS.md#configuration)
for how to supply the login when you run it.

## Choose the streams

With real-time mode on, the top of the sidebar shows two lists.

**Observation streams.** The reference stations you position against, as raw
RTCM. You need at least one; Process stays disabled without one. The default is
`ALIC00AUS0`.

**Nav / SSR correction streams.** A live run needs both:

- **Broadcast ephemeris** (BCE), the satellites' own orbits and clocks, for
  example `BCEP00BKG0`. Always required, SBAS included.
- **Orbit and clock corrections** applied to it: an SSR stream, for example
  `SSRA00CNE0`, or SBAS in its place. While an SBAS card is there, SSR-only
  streams are paused: they stay in the list, struck through, but are left out
  of the run. Remove SBAS and they are used again.

The Corrections section shows the two as separate groups, each with its own
**+** (NTRIP, HTTP or Serial; the SSR group also offers SBAS), and a box to add
whichever is missing. Process lists what is missing until both are there.
Which group a stream belongs in is worked out for you: from what the last
**Monitor stream** saw it send, else from its name (`BCEP…`/`BRDC…` is
ephemeris, `SSR…` is SSR), else from the group whose **+** you added it with.
A stream none of these places (for example from an imported config) shows
under **Not identified yet** and holds Process until Monitor stream identifies
it, or you remove it and add it again under its group. The defaults are `BCEP00BKG0` and `SSRA00CNE0`.

Each list has its own source switch:

- **NTRIP mountpoint.** Type a mountpoint code, such as `ALIC00AUS0`. The
  caster from Settings is added in front of it for you. After **Browse
  streams**, the field suggests mountpoints from the caster: the observation
  list offers streams the caster classes as observations or other, and the
  correction list offers those classed as SSR or other.
- **Direct URI.** Write the full address yourself. The only stream type the
  app offers is **Serial**, which fixes the `serial://` prefix and leaves you to
  fill in the device, for example `COM3` on Windows or `/dev/ttyUSB0` on Linux.
  The field suggests the serial ports it finds and refreshes the list when you
  click into it.

Use **+ Add stream** to add a row and the **×** beside a row to remove it. Each
list picks its source independently, so mountpoints for observations and a
serial receiver for corrections is a valid setup. Switching a list to Direct URI
replaces its mountpoint codes with the `serial://` prefix; switching back
restores the default mountpoints.

### Serial receivers

Under a Serial stream you get the line settings: **Baud**, **Data bits**,
**Parity**, **Stop bits**, **Flow control**, **Flush the port before reading**
and **Flush duration (ms)**. The defaults are 115200 baud, 8 data bits, no
parity, 1 stop bit, no flow control, flush on and 0 ms. These settings go to the
app's port setup step, not into the PEA config.

Before PEA starts, the app prepares each serial port: it sets the line mode,
turns on the receiver's RTCM output and checks that observations are arriving.
The commands it sends are the `$PAIR` set for the Waveshare LC29H(AA). Whether
another receiver accepts them is not known. On Windows the app does this
itself. On macOS and Linux it runs a bundled shell script that needs `bash` and
`python3` on your path. Progress appears in the Console tab with a `[serial]`
prefix.

## Other real-time settings

These are in the same sidebar panel, below the stream lists.

There is no SSR antenna offset setting. The SSR stream's name says whether its satellite positions refer to the antenna phase centre (`SSRA...`, written as `APC`) or the centre of mass (`SSRC...`, written as `COM`), and the app writes that into the config. If no correction stream follows that naming, it writes `APC`, or the value from an imported config.

| Setting | Default | Meaning |
| --- | --- | --- |
| Storage cap (MB) | 500 | Total size of rotated recordings kept in the output directory. Older ones are deleted past this. 0 turns the clean-up off. |
| Wait next epoch (s) | 10 | How long PEA waits for an epoch before moving on. |
| Max receiver latency (s) | 1 | How long after an epoch's first data PEA waits for receivers that have not reported. |
| Recordings: Raw RTCM bytes | on | Saves the raw streams as `*_OBS.rtcm` and `*_NAV.rtcm`. |
| Recordings: Decoded RTCM (JSON) | on | Saves decoded messages. The live stats in the Streams tab are built from this, so keep it on. |
| Recordings: Observations as RINEX | off | Saves observations as `*_OBS.rnx`. |
| Exit if no observation source is available | off | Stops PEA instead of waiting when no observation stream is available. |

Every recording is rotated each minute. Every 15 seconds the app checks the
total size of the `_OBS.rtcm`, `_NAV.rtcm`, `_DEC.json` and `_OBS.rnx` files
and deletes the oldest until the total is under the cap. It never touches your
`.POS` or other results.

Other sidebar sections in real-time mode:

- **General**: only **Processing Mode** applies, and with an SBAS correction
  it is fixed to **Dynamic** (your choice comes back when SBAS is removed).
  The time window and the receiver and antenna fields are for files. The
  **Interval (seconds)** is set from the stream test.
- **Products**: not used.
- **Constellations**: empty until the stream test has run, then filled from
  what the streams carry. If you pick nothing, a live run processes GPS,
  Galileo and GLONASS.
- **Output**: the output directory and the formats, as for a file. Turn on
  `.TRACE` if you want the satellite table to fill in.

## Test the streams

Before a run you check what the streams carry with **Monitor stream**: the
blue button beside **Process** in the top bar (shown until the test has passed
for the current settings, and as **Monitor again** once they change), or the
same button on the **Streams** tab. PEA runs for about ten
seconds in decode-only mode against the streams you set, and the app then fills
in the constellations, the observation codes and the epoch interval. The result
shows each stream's constellations, interval, records, satellites, invalid
records and average SNR. The test's own files are written to a `stream_test`
folder inside your output directory, including a `pea.log`.

The test gives up after about 55 seconds. If the streams carry no observations
it fails with "The stream test received no observations. Check the streams and
the Console." If PEA itself stops, the error says why, and the Console shows
how it exited and the end of its `pea.log` (logins masked).

You do not have to run the test yourself: **Process** runs it first.

## Run

Press **Process**. In real-time mode the app:

1. writes `rt_ppp_config.yaml` into the output directory,
2. runs the stream test,
3. writes the config again with what the test found, and
4. starts PEA with `Starting PEA in realtime mode…`.

Process stays disabled, with a tooltip, if there is no PEA binary, no output
directory or no observation stream. If there is no caster URL and none of the
observation streams is a full URI, the tooltip says "Set the caster on Settings
› Credentials, or switch the observation source to Direct URI and give each
stream a full URI".

Press **Stop** to end the run. PEA does not stop by itself.

## The Streams tab while running

| Section | What it shows |
| --- | --- |
| **Stream test** | The last test result, as above. |
| **Live stream stats** | Every new epoch, or every 5 seconds if no epoch arrives: records in the last minute and since the run started, then tables for observations (per stream: records, satellites, invalid, average SNR, constellations, signals and last epoch), SSR corrections and broadcast ephemeris. Needs **Decoded RTCM (JSON)** on. |
| **Connections** | Per stream: connected or idle, kB/s, the total received and the time since the last event. |
| **Satellites** | Which satellites the solution is using, from the `.TRACE`. See [Reading the outputs](OUTPUTS.md#satellite-status). |
| **SBAS** | With an SBAS correction only. PEA does not report its SISNet stream like the NTRIP ones, so this card has its own sources. **Last stream test**: during **Monitor stream** the app logs in to SISNet itself with your saved login (the test's PEA run leaves SISNet out, so the login never has two sessions) and shows whether it got in, messages per second, CRC errors and the SBAS message types received. **This run**: login failures, CRC errors and unreadable lines from PEA's log, and in L1 or DFMC mode the latest HPL and VPL from the `.SPP` with a 5-minute trend. PVS mode writes no `.SPP`, so it shows no protection levels. |

While a run is live and the Position view is open, the Visualization tab
appends each new epoch as PEA writes it. There is nothing to turn on. Until
PEA has written a `.POS` file, it checks for one every two seconds. An SBAS
L1 or DFMC run writes an `.SPP` file instead, listed as **SBAS SPP**; it has no
formal errors, and without an a priori position its offsets are measured from
its first epoch.
