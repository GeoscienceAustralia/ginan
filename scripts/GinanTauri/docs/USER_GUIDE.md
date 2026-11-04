# Ginan UI user guide

Ginan UI runs Precise Point Positioning (PPP) with Geoscience Australia's PEA
engine from a desktop window. You give it a RINEX observation file or a live
stream, choose the products and options, and it downloads what PEA needs, runs
it and plots the result.

This page covers installing the app, first-run setup and processing a RINEX
file from start to finish. The rest of the guide is split by topic:

- [Real-time processing](REALTIME.md): NTRIP casters, serial receivers, the
  stream test and the Streams tab.
- [Reading the outputs](OUTPUTS.md): position plots, the map, `.TRACE`
  residuals and satellite status.
- [Settings reference](SETTINGS.md): every page in Settings, plus where each
  setting is stored.
- [Troubleshooting](TROUBLESHOOTING.md): the messages the app shows and what to
  do about them.

If you want to build the app from source instead, see the
[README](../README.md).

## Install

Installers come from the project's GitHub releases page
(<https://github.com/GeoscienceAustralia/ginan/releases>). The
release workflow (`.github/workflows/release.yml`) builds them when a `v*` tag
is pushed on `main`: a draft release is created, the Windows, macOS and Linux
builds are attached to it, and it is published once every build has succeeded.
A build started by hand from the Actions page, without a release tag, keeps the
installers as workflow artifacts for 14 days instead.

| Platform | Files | Notes |
| --- | --- | --- |
| Windows (x64) | `GinanTauri_<version>_x64_en-US.msi`, `GinanTauri_<version>_x64-setup.exe` | MSI or NSIS installer; use either. |
| macOS, Apple silicon | `GinanTauri_<version>_aarch64.dmg` | |
| macOS, Intel | `GinanTauri_<version>_x64.dmg` | |
| Linux (x64) | `GinanTauri_<version>_amd64.deb`, `GinanTauri-<version>-1.x86_64.rpm`, `GinanTauri_<version>_amd64.AppImage` | Built on Ubuntu 22.04 so they run on older distributions. |

There are no Windows ARM or Linux ARM builds, and the app cannot download PEA
on those platforms (see [First run](#first-run)).

**Windows.** Run the `.msi` or the `-setup.exe`. The installers are not
Authenticode-signed, so Windows SmartScreen may warn you before it lets the
installer run.

**macOS.** Open the `.dmg` and drag the app into Applications.

Whether macOS opens it straight away depends on the build. `tauri.conf.json`
sets no signing identity, so signing and notarisation happen only if the Apple
secrets (`APPLE_CERTIFICATE`, `APPLE_SIGNING_IDENTITY`, `APPLE_ID`,
`APPLE_PASSWORD`, `APPLE_TEAM_ID`) are set on the repository; they are optional
in the release workflow and the build succeeds without them. A build made
without them is ad-hoc signed, which current macOS refuses on first launch.

To open an ad-hoc signed build: double-click it and dismiss the warning, then
go to **System Settings > Privacy & Security**, scroll to the message naming
the app and press **Open Anyway**. Recent macOS versions no longer accept the
old right-click-then-Open shortcut.

**Linux.** Install the `.deb` or `.rpm` with your package manager, or make the
`.AppImage` executable and run it:

```bash
sudo apt install ./GinanTauri_<version>_amd64.deb
sudo dnf install ./GinanTauri-<version>-1.x86_64.rpm
chmod +x GinanTauri_<version>_amd64.AppImage && ./GinanTauri_<version>_amd64.AppImage
```

The app draws its window with the system webview: WebView2 on Windows,
WebKitGTK 4.1 on Linux. See the Tauri prerequisites linked from the
[README](../README.md) if the window does not open.

## First run

On the first launch two things happen at the same time.

**PEA is downloaded.** The app does not bundle PEA. If it cannot find a PEA
binary in its own data folder, it downloads one from the
`GeoscienceAustralia/ginan` GitHub releases: the latest release's
`ginan-<platform>.zip`, then its `ginan-gui-<platform>.zip`, then the previous
minor release if the latest has neither. The status bar at the bottom of the
window shows `Downloading pea · NN%` and then `pea <version> ready`. Once a
binary exists the app reuses it and never replaces it on its own. If a newer
release appears later, a popup titled **PEA binary update available** offers
**Update to <version>** or **Not now**, and a `pea <version> available` button
stays in the status bar. You can also pick a version yourself in
[Settings > PEA Binary](SETTINGS.md#pea-binary).

Automatic download works on Windows x64, macOS (Apple silicon and Intel) and
Linux x64. On any other platform you need to point the app at your own `pea`
build in Settings > PEA Binary.

The app fetches the release's own archive for your platform and keeps just the
`pea` binary out of it. As of Ginan v4.1.4 that archive is 32.1 MiB on Windows,
29.7 MiB on Linux, 23.2 MiB on macOS Apple silicon and 23.5 MiB on macOS Intel,
so on a normal connection this is seconds rather than minutes. The extracted
binary is about 20 MiB.

If a release is ever published without that archive, the app falls back to the
`ginan-gui-*` one, which carries the whole GUI distribution and runs from 309
MiB to 743 MiB depending on platform. A first run that seems to be downloading
far too much is this fallback.

**A setup screen asks for the basics.** It has up to five steps, and every step
except the welcome and the last can be skipped with **Skip for now**:

1. **Welcome.** Choose **Files** or **Live stream**, then **Get started**.
   Choosing Live stream adds a step for a caster login.
2. **Credentials.** "Connect to NASA Earthdata". PPP products come from NASA's
   CDDIS archive, which needs a free Earthdata account
   (<https://urs.earthdata.nasa.gov/users/new>). Enter your **Username** and
   **Password** and press **Save & test**. If a login is already saved on this
   machine you will see "You're signed in." instead of the form. The login is
   written to your netrc file (see
   [where things are stored](../README.md#where-things-are-stored)), not to the
   app's settings.
3. **Livestream** (only if you chose Live stream). "Livestream credentials".
   Enter the **Caster host** (default `ntrip.data.gnss.ga.gov.au`), **Username**
   and **Password**, then **Save credentials**. This is a different account
   from Earthdata.
4. **Products.** "Choose a products folder". Press **Browse…** and pick a
   folder with a few GB free. Downloaded orbit, clock, bias and support files
   are kept there and reused, so each one is only fetched once.
5. **Finish.** "You're set up." Choose **Skip the tour** or **Show me around**.

If you skip the credentials step the app goes into offline mode: a banner
under the top bar says "No Earthdata credentials", and processing a file stays
blocked until you sign in. The setup is remembered. To go through it again, use
**Settings > Help > Run setup again**.

Setup does not ask for an output directory. Set that before your first run in
**Settings > Output** (see [Settings reference](SETTINGS.md#output)).

Once you have a products folder, the app checks the connection to CDDIS and
downloads the static support files PEA needs (the ANTEX antenna file, Earth
orientation parameters, satellite metadata, and tide-loading, geomagnetic and
troposphere tables). The **Workflow** tab records each step, for example
"Earthdata Credentials found: …", "CDDIS connectivity check passed in 1.20
seconds" and "Downloaded metadata successfully." Until a products folder is
set, the status bar reads "Set a products directory in Settings" and none of
this happens.

## The window at a glance

- **Top bar.** Sidebar toggle, the app name, a **?** button for the guided
  tour, **Settings**, and, once a file is open or real-time mode is chosen, the
  **Process** and **Stop** buttons. On Windows and Linux the window has no
  system title bar, so the app draws its own minimise, maximise and close
  buttons at the top right. On macOS the usual traffic lights stay.
- **Banner.** Appears under the top bar when something needs attention, such as
  no Earthdata credentials or no connection to CDDIS.
- **Sidebar.** On the left. At the top, a switch between **RINEX file** and
  **Live stream (NTRIP)** input, then the observation source, then four
  collapsible sections: **General**, **Products**, **Constellations** and
  **Output**. Drag its right edge to resize it, double-click the edge to reset
  the width, or focus the edge and use the arrow keys.
- **Workspace.** Four tabs: **Workflow**, **Console**, **Streams** and
  **Visualization**. It opens on Workflow.
- **Status bar.** One line at the bottom saying what the app is doing right
  now.

Every launch starts on a welcome screen ("No observation file open") with two
buttons: **Add RINEX Observation File** and **Setup Real-time Processing**.
The app does not reopen the file you used last time. The first time you open a
file after setup, it offers a short guided tour; the **?** button replays it.

## Processing a RINEX file

### 1. Open the file

On the welcome screen, press **Add RINEX Observation File** and choose a file,
or drop a file onto the window. After a file is open, use **Browse…** under
**RINEX Observations** at the top of the sidebar to switch to another one.

Dropping a file on the window only works on the welcome screen. The drop
listener belongs to that screen, so it is gone once the workspace replaces it,
and a file dropped after that is ignored with no message. Use **Browse…**
instead.

The file dialog's **RINEX Observation Files** filter offers `.rnx`, `.obs`,
`.crx`, `.gz`, and the RINEX 2 short names (`.20o` to `.24o`, and the
Hatanaka forms `.20d` to `.24d`). **All Files** lets you pick anything else.
The app opens one file at a time. If you drop several, it opens the first one
that looks like RINEX and says so in the status bar.

Gzip (`.gz`) and Hatanaka (`.crx`, `.??d`) files are unpacked for you into a
plain `.rnx` file written next to the original, so the folder the file is in
must be writable. A zip archive is not unpacked; extract it first.

While the file loads, a panel titled **Opening <file name>** lists four steps:

1. Prepare observation file
2. Read RINEX header
3. Scan CDDIS for products
4. Fetch signal bias table

The panel closes itself when everything has worked. Its close button hides it
but does not cancel anything. If a step fails the panel stays open and shows
the reason; see [Troubleshooting](TROUBLESHOOTING.md#opening-a-rinex-file).

### 2. What gets filled in

Everything below is read from the RINEX header, and you can change most of it
before you run.

| Field | Where | Notes |
| --- | --- | --- |
| Marker | General > RINEX Metadata | Read-only. For RINEX 3 and 4 it is cut to four characters, so `ALIC00AUS` shows as `ALIC`. |
| Receiver, Antenna | General > RINEX Metadata | Editable. Spaces are kept exactly as typed, because they are part of the name. |
| Antenna Offset (m) | General > RINEX Metadata | Editable height, east and north (**H**, **E**, **N**). |
| Apriori Position (ARP) | General > RINEX Metadata | Read-only X, Y, Z from the header. |
| Start Epoch, End Epoch | General > Time Window | In GPS time. Epochs recorded in GLONASS, BeiDou or UTC time are converted when the file loads. |
| Interval (seconds) | General > Time Window | From the header; 30 if the header has none. |
| Constellations and observation codes | Constellations | One list per system found in the header. |

If the header has no `TIME OF LAST OBS`, the app reads through the file to find
the last epoch, so very large files take a moment longer to open.

The app also checks the antenna name against the `igs20.atx` file in your
products folder and records "Antenna type verified from .atx file" or "Antenna
type failed to verify from .atx file" in the Workflow tab. It does not stop you
either way.

### 3. Choose the products

Open **Products** in the sidebar. The **PPP Product Source** block has three
drop-downs:

- **Provider**: the analysis centre (`IGS`, `COD`, `ESA`, `GFZ`, `JPL`, `WHU`
  and so on).
- **Series**: how quickly the products were made, shown as `FIN · Final`,
  `RAP · Rapid`, `ULT · Ultra-Rapid` or `NRT · Near-Real-Time`.
- **Project**: the product campaign, such as `OPS · Operational` or
  `MGX · Multi-GNSS`.

The app lists only the combinations that CDDIS has SP3 (orbit), CLK (clock) and
BIA (bias) files for. It looks in the CDDIS directory for the GPS week of the
start epoch. Picking a provider narrows the series and projects to the ones it
publishes, and picking a project can change the series to match. If the
combination saved from last time is not available for this file, the app picks
the first one that is. Before anything has been scanned the selection is
`IGS` / `FIN` / `OPS`.

The coloured dot beside the provider name is a quick check that CDDIS answered
within three seconds (green reachable, red unreachable, grey unknown). Every
provider is served from CDDIS, so the dot tells you about CDDIS, not about that
provider's data.

A note shown in place of the drop-downs explains why they are missing:

| Message | Meaning |
| --- | --- |
| Load a RINEX file to see available providers. | No file is open yet. |
| Scanning CDDIS for available products… | The directory scan is running. |
| No PPP products available for this observation window. | The scan finished without finding a complete set. See [Troubleshooting](TROUBLESHOOTING.md#products-and-downloads). |

Choose a series that exists for your dates. The app will not request a series
that should not be published yet: Final is treated as available 14 days after
the observation date and Rapid after 1 day. Every other series is treated as
available straight away. The bias file is only requested when Final should
exist for the date, so very recent data may have no bias file to download.

### 4. Choose constellations and observation codes

Open **Constellations**. Each system found in the file gets a row:
`GPS (Global Navigation System)`, `GAL (Galileo)`, `GLO (GLONASS)`,
`BDS (BeiDou)` or `QZS (Quasi-Zenith Satellite System)`.

- The checkbox on the row switches all of that system's codes on or off.
- Click the row to expand it. Each code has its own checkbox and up and down
  arrow buttons. The order is the priority order, with the top code preferred.
  The enabled codes, in that order, are written to
  `processing_options.gnss_general.sys_options.<system>.code_priorities` in the
  PEA config.
- A system the selected products do not cover is greyed out, with a badge such
  as `not in IGS`, and is left out of the config. Hover over the row for the
  reason.
- A code shown in red with a strikethrough is not in the `.BIA` bias file for
  the selected provider, series and project, so PEA cannot use it.

> **Known defect: a struck-through code is still sent to PEA.** The bias check
> marks the code invalid but leaves it enabled, and the tick box is drawn from
> validity rather than from the stored value, so the box looks unticked while
> the code is still enabled underneath. The config writer keeps every enabled
> code, so the struck-through code is written to `code_priorities` anyway.
> Clicking the box does not clear it either. To keep such a code out of the
> config, turn the whole constellation off, or choose a provider, series and
> project whose bias file covers the code. Check what was actually written
> under [Settings > Configuration](SETTINGS.md#configuration), in the
> **PEA config YAML** tab.

The bias check runs by itself whenever you open a file or change provider,
series or project. The app downloads one sample bias file for that combination,
reads which signals it covers, and caches the result for 30 days in a
`.bia_code_cache` folder inside your products folder. The Workflow tab shows
"Validating constellation signal frequencies against BIA file for
IGS/FIN/OPS...", then "BIA code priorities cached for ..." and "Validated
constellation codes against BIA for ...". The Console tab has the per-system
breakdown.

Press **Validate BIA** at the top of the section to check again. The button
reads "validating…" while it runs, and any error appears next to it.

The code list for each system shows the observation types the header declares,
in header order.

### 5. Set the mode, time window and interval

Open **General**.

**Processing Mode** is **Static**, **Kinematic** or **Dynamic**. Static assumes
the receiver does not move. Kinematic and Dynamic let the position change from
epoch to epoch; the app writes position process noise of 0, 100 and 1000 for
the three modes.

**Time Window** has a **Start Epoch** and an **End Epoch**. Both are in GPS
time, which has no leap seconds and runs a few seconds ahead of UTC (the amount
is shown under the fields). Each is a text field in the form
`YYYY-MM-DD HH:MM:SS`:

- Type or paste a time, then press Enter or click away to apply it. A bad
  format shows "Use YYYY-MM-DD HH:MM:SS, e.g. 2024-01-02 00:00:00".
- With the cursor in the field, the up and down arrow keys step the part of
  the time under the cursor. Hold Shift to step by ten.
- Clicking the field or the calendar button opens a calendar below it, with
  the file's own window shaded.
- Under each field the app shows the GPS week, the day of the week (0 is
  Sunday), the day of year and the UTC time.

Two notes can appear under the fields: "End is before start, so PEA would
process nothing", and "Reaches outside the loaded file…", which means only the
epochs inside both the file and your window are processed. If the window
differs from the file's, **Use file window** puts it back.

**Interval (seconds)** is the sampling interval PEA processes at, from 1 to
3600.

### 6. Choose outputs

Open **Output**. The **Output Directory** is shown read-only; **Settings…**
takes you to the page where you change it. Under **Output Formats**, tick the
files you want PEA to write:

| Format | Default | Used for |
| --- | --- | --- |
| `.POS` | on | The position solution that the Position plots read. |
| `.GPX` | off | A track file for other mapping tools. |
| `.TRACE` | off | Per-measurement residuals, used by the Residuals view and the satellite table. Turn it on if you want to diagnose a run. |
| `.SNX` | off | A SINEX solution file. |

The app only plots `.POS` and `.TRACE`. See [Reading the outputs](OUTPUTS.md).

### 7. Process

Press **Process** in the top bar. If it is greyed out, a **N to fix** button
sits beside it. Press it (or Tab to it and press Enter) to see everything that
is missing, each with a button that opens the place to fix it. Hovering over
Process shows the same list.

| Message | What to do |
| --- | --- |
| PEA is not ready... | Wait for the PEA download, or set a binary in Settings > PEA Binary. |
| No output directory... | Settings > Output > **Browse…**. |
| No products directory... | Settings > Products Cache > **Browse…**. |
| No RINEX file... | Open a file as in step 1. |
| No start epoch... | The header had no first epoch. Enter a **Start Epoch** under General. |
| Checking the CDDIS connection... | Wait a few seconds. |
| The CDDIS login is missing or was rejected... | Settings > Credentials > **Set credentials…**. |

If you press Process while offline, the Workflow tab says "Cannot process:
Ginan UI is running in offline mode (no internet connection)" and the status
bar says "Processing requires an internet connection".

### 8. What happens during a run

1. **Config.** The app writes `ppp_config.yaml` into the output directory, built
   from your choices and the PEA template. The Console shows "Config written
   to: …". If you turned **Overwrite on run** off (Settings > Configuration),
   it reuses the file already there instead.
2. **Products.** For each day in your time window, the app checks the SP3
   orbit, CLK clock, BIA bias and broadcast navigation (BRDC) files in your
   products folder and downloads whatever is missing. The Workflow tab shows
   "Starting PPP product downloads..." and the count of files to check, and the
   status bar shows `Downloading <file> · NN%`. CDDIS often cuts a transfer
   short, so the app resumes from where it stopped, up to 12 passes per file.
3. **PEA.** The Workflow tab shows "Starting PEA execution in background..."
   and an **Epoch N of M** progress bar with the number of epochs left. The
   **Console** tab streams PEA's own output; lines PEA writes to its error
   stream are marked in red.
4. **Finish.** "PEA processing completed", or "PEA execution failed: …". The
   status bar reads `Done — exit code N`. The Workflow tab then lists the
   `.POS` files it found in the output directory, and the Visualization tab
   refreshes.

A product that cannot be fetched is logged as a warning, for example "Could not
fetch orbit (SP3) for 2024-01-02: … (tried 2 URL(s))", and the run carries on.
That means the line "Downloaded all products successfully." does not prove that
every file was found. Read the warnings above it, and check the Console if PEA
stops early.

A missing product never stops the run by itself. The downloader logs the
warning and reports success for the day regardless, so the workflow always
moves on to PEA, and PEA decides for itself whether it can process the window
with the files that did arrive. If it cannot, it exits with an error in the
Console rather than at the download step, so a run that fails for a missing
product looks like a PEA failure at first glance.

To cancel, press **Stop**. It cancels any download in progress and ends PEA
(with SIGTERM on macOS and Linux, and by force on Windows). The Workflow tab
records "Stop requested - stopping all running tasks..." and "Stopped", and
the status bar reads "Stopped".

The Workflow tab keeps its history for the whole session, so you can see how
the file was opened and which products were chosen. Press **Clear** to empty
it. The Console is cleared at the start of each run.

## Where next

- Look at the result: [Reading the outputs](OUTPUTS.md).
- Change a default, or set up credentials, directories or the PEA binary:
  [Settings reference](SETTINGS.md).
- Something went wrong: [Troubleshooting](TROUBLESHOOTING.md).
