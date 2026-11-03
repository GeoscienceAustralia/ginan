# Troubleshooting

This page lists messages the app actually shows, what usually causes them, and
what to do. Messages are quoted as the app words them; parts that change are in
angle brackets.

## Where to look first

- **Status bar** (bottom of the window): the current step, or the last error.
- **Banner** (under the top bar): connection and credential problems.
- **Workflow tab**: the steps of the session in order. Rows marked **Attention**
  or **Error** open on their own.
- **Console tab**: PEA's raw output and the app's low-level detail. Lines PEA
  writes to its error stream are red.
- **`stream_test/pea.log`** in your output directory, for a failed stream test.

## Network access

The app talks to these hosts. If you are behind a firewall or proxy, they need
to be reachable.

| For | Host |
| --- | --- |
| Finding and downloading PEA | `api.github.com` and GitHub release downloads |
| PPP products and the product directory scan | `cddis.nasa.gov`, which signs you in through `urs.earthdata.nasa.gov` |
| Static support files (ANTEX, tables) | `peanpod.s3.ap-southeast-2.amazonaws.com` |
| Broadcast navigation files | `data.gnss.ga.gov.au`, which returns a download link on another host |
| Map tiles | `tile.openstreetmap.org` |
| Real-time streams | Your NTRIP caster; the default is `ntrip.data.gnss.ga.gov.au` |

## Startup and the PEA binary

**"Pea binary unavailable: pea binary not found and no release could be fetched
(…). Use Settings → PEA Binary to locate it manually."** The first-run download
failed, usually because GitHub could not be reached. Connect, then open
**Settings > PEA Binary**, pick a version and press **Download & use**. If you
have a `pea` build of your own, use **Override…** to point at it.

**"Auto-download not supported on this platform. Use the 'PEA Binary' button."**
The app only downloads PEA for Windows x64, macOS (both architectures) and
Linux x64. Use **Override…** under **Settings > PEA Binary** to pick your own
build. (The message names a "PEA Binary" button; in the app that is the
**Override…** button on the PEA Binary page.)

**"No release provides a 'ginan-<platform>.zip' (or GUI) asset (checked latest
<tag> and the previous minor version)."** The newest Ginan releases have no PEA
build for your platform. The version list on **Settings > PEA Binary** only
shows releases that do, so pick an older one from it.

**"GitHub API request failed: …"** or **"GitHub API returned <status>"** The app
could not read the release list. Check your connection and try again later.

**"Invalid PEA binary: Not a file: <path>"** The file you chose with
**Override…** does not exist or is a folder.

**"PEA binary not found. Use the 'PEA Binary' button to locate it."** Shown when
you press Process with no usable binary. Same fix as above.

**"Failed to start PEA: …"** The operating system refused to run the binary. For
your own build, check it is executable and built for this CPU.

## Credentials and connection

**Banner: "No Earthdata credentials — product downloads are unavailable. CDDIS is
the only product source, so a free NASA Earthdata account is required."** Press
**Set up account**, enter your Earthdata **Username** and **Password** and press
**Save & Test**. If you have no account, register at
<https://urs.earthdata.nasa.gov/users/new>. After you sign in, the app repeats
its startup checks and rescans for products without you reopening the file.

**Banner: "Can't reach the CDDIS server. Ginan will keep trying and reconnect on
its own."** CDDIS did not answer. The app checks again every ten seconds and
whenever your computer reports it is back online. **Try now** checks
immediately. Processing a file stays disabled until CDDIS answers.

**Banner: "No internet connection. Ginan will reconnect on its own once you're
back online."** Your computer reports no network.

**Banner: "Cannot reach <host>. Check your internet connection."** A download
failed to connect or timed out. Dismiss it with the **x** and try again once the
connection is back.

**Status bar: "CDDIS: <message>"**, or **Workflow: "CDDIS connectivity check
failed: <message>. Please verify Earthdata credentials via the CDDIS Credentials
dialog"** and **"Credentials invalid: <message>"**. The login is missing, wrong,
or CDDIS could not be reached. Open **Settings > Credentials > Set
credentials…** and re-enter it.

**Download error: "got HTML (likely Earthdata login page) from <url>: auth
handshake did not complete"** CDDIS answered with its login page instead of the
file, so the sign-in was not accepted. Re-enter your credentials as above.

**Workflow: "Skipping metadata download - running in offline mode"** The support
files are only fetched once the CDDIS check passes. Fix the connection or the
login and they download on their own.

The exact wording for each case is listed under
[Settings > CDDIS Credentials](SETTINGS.md#cddis-credentials). The two worth
knowing apart:

- **`Authentication failed. Check your Earthdata username and password.`** The
  login reached CDDIS and was refused. Re-enter it.
- **`No CDDIS credentials found in netrc`** There is no saved login to check.
  Open the credentials window and sign in once.

A caster login behaves the same way but reports separately: an empty field gives
`Host, username, and password must not be empty`, an Earthdata host typed into
the caster box gives `Use the CDDIS credential dialog for Earthdata hosts`, and a
refused caster login gives
`Earthdata credentials rejected (login page returned)`.

## Opening a RINEX file

**Status: "Not a RINEX observation file: <names>"** You dropped a file whose
extension is not one the app opens. Rename it, or open it with **Browse…** and
the **All Files** filter.

**"File not found: <path>"** The file moved after you chose it.

**"Could not read <file>: …"** The unpacking step failed. The detail names the
cause: "Gzip decode failed", "Hatanaka (CRX to RNX) decompression failed" or
"Could not write <path>". Unpacked files are written next to the original, so a
read-only folder stops this. Copy the file to a writable folder. A `.zip`
archive is not unpacked at all; extract it first.

**"Parse error: …"** or **"<file> could not be read: …"** The header could not
be read, so the file is probably not RINEX observation data, or is damaged.

**Workflow: "No observation codes found in RINEX"** The header listed no
observation types the app recognises for GPS, Galileo, GLONASS, BeiDou or QZSS.

**Workflow: "No start and/or end date, can't check valid analysis centers"** The
header has no first epoch, so the product scan was skipped. Enter a **Start
Epoch** under General.

**"Could not reach the product mirrors: …"**, **"PPP product scan failed: …"** or
**Workflow: "Error loading CDDIS data: …"** The scan itself failed, for example
because the start epoch could not be read as a date. An unreachable CDDIS
usually does not produce these: the scan returns an empty list instead, which is
the next entry. See also
[Credentials and connection](#credentials-and-connection).

**Products shows "No PPP products available for this observation window."**, and
the Workflow tab says "No valid PPP providers found". No provider has a full set
of SP3, CLK and BIA files in the CDDIS directory for the week of your start
epoch. Common causes:

- You are not signed in. CDDIS returns a login page, which the app reads as an
  empty directory. Sign in and the scan reruns by itself.
- The data is too recent. Final products take about two weeks. Rapid ones about
  a day.
- CDDIS is unreachable. The Console may say "All mirrors unreachable -- no PPP
  products found."

## Products and downloads

**Workflow: "Skipping <series> for <date>: not published yet."** The series you
chose, or its fallback, should not exist yet for that date. Choose Rapid or
Ultra-Rapid for recent data. **"Skipping FIN bias for <date>: not published
yet."** means no bias file was requested at all.

**Workflow: "Could not fetch <orbit (SP3) | clock (CLK) | bias (BIA)> for <date>:
<reason> (tried <n> URL(s))"** A product was not downloaded and the run
carried on. The reason says why:

- "HTTP 404 Not Found from <url>": that provider, series and project has no such
  file for that date. Choose another combination under Products.
- "got HTML (likely Earthdata login page) …": sign-in problem, as above.
- "GET <url> failed: …": a network failure.
- "Download error (<file>): … after <n> of <m> bytes": CDDIS kept cutting the
  transfer. The app resumes from where it stopped up to 12 times, then gives up.
  Run again.

The line "Downloaded all products successfully." can still appear after these, so
read the warnings above it. PEA then runs without the missing file and may stop
with its own error in the Console.

**Workflow: "BRDC for <date>: GA API lookup failed, <reason>"** or **"download
failed, <reason>"** The broadcast navigation file could not be fetched. The
default config marks navigation files as not required, so this alone does not
stop a run.

**Workflow: "PPP download error: <message>"** A hard failure such as "Cannot
create products dir <path>: …". Check the products directory exists, is
writable and has space.

**Workflow: "Error whilst downloading metadata: …"** The static support files
could not be fetched, and PEA needs them. They are tried again when you change
the products directory, save credentials, or the connection recovers.

**Workflow: "No igs*.atx ANTEX file found in products directory."** The
support-file download has not completed. The antenna check is skipped until it
has.

## Constellations and the bias table

**Workflow: "Failed to fetch BIA file for selected PPP products - unable to
validate codes"**, with "BIA fetch failed for <provider>/<series>/<project>: …"
in the Console, and the **Fetch signal bias table** step marked as failed. The
sample bias file could not be downloaded. The file still loads and you can
still run, but the codes are not checked. Fix the connection or login, change
the provider or series, then press **Validate BIA**.

**Next to Validate BIA: "Failed to validate against BIA"** The check itself
failed. Try again after the bias table has loaded.

**A system greyed out with "not in <provider>"** The selected products do not
carry that constellation for your date, so it is left out of the config. Choose
a provider or project that does.

**Every code struck through** The selected products carry none of the signals in
your file. Choose another provider or project.

## The Process button and runs

If **Process** is disabled, hover over it. The tooltip is explained in the
[user guide](USER_GUIDE.md#7-process).

**Status: "Select a RINEX file and output directory first"** or **"Add at least
one observation stream and an output directory first"** A required input is
missing.

**Status: "Processing requires an internet connection"** The app is in offline
mode. A file run needs CDDIS. Fix the connection or login.

**"Processing already in progress. Please wait..."** (Workflow) or **"Error: PEA
is already running"** (status bar) A run is already going. Press **Stop** first.

**Status: "Error: …"**, with a matching warning in the Workflow tab. The step
that failed is in the text, for example "Cannot write config: …" or "Cannot
create output dir: …", which mean the output directory is not writable.

**Workflow: "PEA execution failed: Exit code: <n>"** PEA stopped with an error.
Open the Console and read the red lines just above the end.

**"Killed by signal <n> (<name>). PEA crashed or was terminated externally."**
macOS and Linux only. PEA was killed, for example because it ran out of memory.

**The run finished but there is nothing to plot** The Workflow tab says "No .pos
files found in <folder>". Check that **.POS** was ticked under Output, and read
the Console for PEA's own error.

## Visualization

**"No output directory set"** Choose one in Settings > Output.

**"Could not read the output directory: Output directory does not exist: <path>"**
The folder was moved or deleted. Choose it again in Settings > Output.

**"No .pos files yet"** or **"No .TRACE files yet"** Nothing of that type is in
the output directory. Turn on **.POS** or **.TRACE** under Output and run again.

**"<file>.pos is empty: PEA wrote no epochs to it. Pick another solution in the
toolbar."** The run stopped before its first epoch.

**The Map shows no background** The base map is loaded from OpenStreetMap, so it
needs an internet connection.

**Satellites: "No .TRACE file in the output directory. Turn on TRACE output in
the sidebar to see which satellites the solution is using."** Turn on **.TRACE**
and run again.

## Real-time and serial streams

**Browse streams: "Set a caster URL first"**, **"Could not extract a hostname
from the caster URL"**, **"Could not reach <url>: …"**, **"<url> returned HTTP
<status>"** or **"No STR; entries found in the caster's sourcetable"** The
caster URL is blank or wrong, the caster could not be reached, or it refused
the request (a 401 or 403 usually means you need to **Sign in**).

**"The stream test received no observations. Check the streams and the
Console."** None of the observation streams delivered data in the test.
Check the mountpoints, the caster login, and the Console's `[test]` lines.

**"Stream test timed out after 55s; see <folder>/pea.log"** PEA did not finish
the ten-second test in time. Open that log.

**"Stream test stopped"** You pressed Stop during the test.

**Streams tab: "Turn on Recordings › Decoded RTCM (JSON) to see live stats."**
The live stats are built from the decoded recording, which is off.

**Connections: "No stream data yet. Waiting for PEA to connect to the caster…"**
PEA has not connected. If it stays this way, check the caster login and the
Console.

**"Serial setup for <device> failed (…); see the Console for the reason."** or
**"Cannot start serial setup for <device>: …"** The port setup failed before
PEA started. Read the Console lines that start with `[serial]`: common causes
are the port being open in another program, the wrong device name, or
`bash` or `python3` missing on macOS and Linux.

**"`<uri>` is a file, not a live stream"** or **"PEA has no stream type for
`<scheme>://`"** A stream entry has a URI scheme that real-time mode cannot use.

## Configuration files

**"Import failed: Read error: …"** or **"Import failed: Parse error: …"** The
file could not be read, or is not valid YAML.

**"Export failed: Write error: …"** The destination is not writable.

**"Could not generate config: …"** in the PEA config YAML panel. The config
could not be built from the current settings.

## Reporting a problem

See [CONTRIBUTING.md](../CONTRIBUTING.md#reporting-bugs). Include your OS, the
app version (Settings > About), the PEA version (Settings > PEA Binary) and the
steps. Remove usernames and passwords from anything you attach. A real-time
`rt_ppp_config.yaml` can contain your NTRIP login, so do not attach one without
checking.
