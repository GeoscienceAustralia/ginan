# Settings reference

Open Settings with the **Settings** button in the top bar. It fills the window.
The pages are listed down the left; **Back** (or Escape) returns to the app.
The **Search settings** box filters every page at once and shows the matching
rows under their page names.

Settings holds the machine-level setup: directories, the PEA binary, logins,
appearance and the processing parameters. Things tied to one run (the RINEX
file, provider and series, constellations, time window, output formats and the
stream lists) are in the sidebar. See [What is not in Settings](#what-is-not-in-settings).

## Configuration

Import, export and reset the whole configuration, and tune PEA's processing
parameters. Each action shows its outcome beside its button for about eight
seconds.

| Row | Button | What it does |
| --- | --- | --- |
| **Import config** | **Import…** | Loads settings from a PEA YAML file: processing mode, time window, interval, output formats and constellation codes. Directories on this machine are left as they are. If the file names a RINEX file that exists on this machine, it is reloaded and the imported time window and codes are put back over what the file set. Result: "Imported <file> — RINEX reloaded", "Imported <file> — RINEX could not be reloaded", "Imported <file>" or "Import failed: …". |
| **Export config** | **Export…** | Saves the config a run on this machine would use, paths included (default name `ppp_config.yaml`, or `rt_ppp_config.yaml` in real-time mode). Caster logins are left as `<USER>:<PASS>`; supply yours when you run it with `pea --config <file> --user <name> --pass <password>`. A password containing `@`, `/` or `~` will not work there, and the one login is sent to every caster in the file. Keys the app does not expose are kept if they came from a file you imported earlier in the same session. Result: "Exported to <file>" or "Export failed: …". With nothing to export, it says "Nothing to export yet — open an observation file and set an output directory". |
| **Reset config** | **Reset** | Puts the processing mode, time window and output formats back to their defaults (Static, empty window, 30 s, `.POS` only). Directories, credentials and the loaded RINEX are left alone. The window is left empty, not set back to the file's, so press **Use file window** under General > Time Window (or enter a Start Epoch) before you process again. |

### Processing parameters

The next row, **Processing parameters**, says how many parameters differ from
the template defaults ("N of M changed from the template defaults") and has
**Reset all**. Below it are the parameters themselves, grouped in the order
they appear in the generated YAML:

- **Satellite options** and **Receiver options**: error models, code and phase
  sigmas, the **Elevation mask** (default 15°), troposphere modelling, the tide
  and pole-tide models, and second and third order ionosphere.
- **Process modes**: whether the preprocessor, SPP and PPP stages run.
- **Preprocessor**, **SPP** and **PPP filter**: cycle-slip threshold, RAIM,
  maximum GDOP, the chi-square test and the prefit and postfit checks.
- **Error handling**: deweighting and rejecting bad measurements, and when to
  reset ambiguities.

Every row has a description. A value that differs from the default shows a
reset arrow beside it. Only changed values are stored and sent, so an untouched
install produces the same YAML as the template.

The app also reads PEA's config template when it starts. Any parameter found
there that has no hand-written entry appears in an extra group called
"<section> — other parameters", labelled from its YAML path, so a newer PEA's
options show up without an app update.

### PEA config YAML

The last row, **PEA config YAML**, shows the exact config a run would write,
the path it will be written to, and whether that file "exists on disk" or is
"not written yet". It needs an output directory and a loaded file (or an
observation stream in real-time mode) before it shows anything.

- **Overwrite on run** (on by default) rewrites the config on every run. Turn it
  off to hand-edit the file for PEA options the app does not expose. The next
  run then reuses the file as it is, and a notice warns you if the file on disk
  differs from the preview.
- **Refresh** regenerates the preview. **Copy** copies it to the clipboard.

## Products Cache

**Products directory** (**Browse…**): the local cache for the SP3, CLK, BRDC,
BIA and ANTEX files the app downloads. It is required for a file run. When you
change it, the app checks CDDIS again, downloads any missing support files into
the new folder and looks for the ANTEX file there. Pick a folder with a few GB
free.

## Output

**Output directory** (**Browse…**): where PEA writes its results, and where the
config for each run is saved. The sidebar's Output section shows it read-only,
with a **Settings…** button that jumps here. Which formats are written is set in
the sidebar.

## PEA Binary

| Row | Control | What it does |
| --- | --- | --- |
| **Version** | The tag of the installed binary, or "Custom" for a binary you picked yourself, or "None". A drop-down of available versions and **Download & use**. | Lists the Ginan releases that have a PEA build for your platform, newest first, with "(installed)" on the current one. **Download & use** downloads the selected release and makes it the active binary. The button reads "Installed" when the selection is already in use. A **Failed** pill appears if it could not be fetched. |
| **Binary location** | The path in use, and **Override…** | Pick any `pea` executable on disk to use instead. The path is checked ("Invalid PEA binary: Not a file: …" if not) and remembered between launches. |

The app downloads PEA on first launch and then leaves it alone. When a newer
release exists, the row says so ("A newer release (<tag>) is available") and the
status bar shows an update button; nothing is replaced until you choose to.

On macOS, PEA also needs the OpenMP library `libomp`, which comes from
Homebrew. If it is not installed, the app downloads Homebrew's build of it
at startup (checked against Homebrew's published SHA-256) into
`lib/libomp.dylib` in the app's data folder, and starts PEA with that folder
on its library search path. Nothing is installed system-wide. Without it PEA
cannot start at all, and a run or Monitor stream fails with "the library
libomp.dylib is missing". There is no Homebrew build for Intel Macs; there,
run `brew install libomp`.

## Credentials

**CDDIS connection** shows the state of your NASA Earthdata login and has
**Set credentials…**, which opens the "Earthdata / CDDIS Credentials" window
(**Username**, **Password**, **Cancel**, **Save & Test**). The login is saved
to your netrc file, then tested against CDDIS. If the test fails the window
stays open and shows why, so you can correct it. The status reads one of:

| Status | Meaning |
| --- | --- |
| Connected to CDDIS | The login works. |
| Checking CDDIS… | The startup check is running. |
| No Earthdata account — downloads unavailable | The app is in offline mode: no login was found, you cancelled the sign-in window, or the CDDIS check failed. The banner under the top bar says which. |
| Not checked yet, set a products directory | The CDDIS check only runs once a products directory is set. |
| (an error message) | The check failed. See [Troubleshooting](TROUBLESHOOTING.md#credentials-and-connection). |

If you do not have an account, the window links to
<https://urs.earthdata.nasa.gov/users/new>.

The window shows whatever the check itself reported, with no rewording, so the
text you see is one of these:

| What went wrong | Text |
| --- | --- |
| Either field left blank | `Username and password must not be empty` |
| Wrong username or password | `Authentication failed. Check your Earthdata username and password.` |
| Wrong login, and CDDIS answered 401 or 403 rather than its login page | `Authentication failed (HTTP 401 Unauthorized). Check your Earthdata username and password.` |
| No network, or CDDIS unreachable | `connection failed: …`, ending in the underlying network error |
| No login saved at all | `No CDDIS credentials found in netrc` |
| CDDIS answered, but with neither success nor a login page | `CDDIS returned HTTP <status>` |

The usual wrong-password case is the second one, not the 401: CDDIS serves the
Earthdata login page with HTTP 200 instead of an auth failure, so the app looks
at the body rather than the status code to tell them apart.

### NTRIP caster (live streams)

The NTRIP caster URL and its login, on the same page as the Earthdata login
(it was its own Live Stream page before). The controls (**Sign in**, **Paste
full URL…**, **Browse streams**, **Sign out**) are described in
[Real-time processing](REALTIME.md#set-the-caster). The caster URL is remembered
between launches; clearing it puts the GA caster back. The stream lists in the
sidebar are not remembered.

## Appearance

**Theme**: **Light** or **Dark**. It changes immediately. Until you choose one,
the app follows your operating system's light or dark setting.

## Help

- **Guided tour** (**Start tour**): walks through the window one section at a
  time. Which tour you get depends on what is on screen: a file run, or a live
  stream.
- **First-run setup** (**Run setup again**): shows the welcome, credentials and
  products-folder steps again. Nothing you have configured is cleared, but the
  record of which tours you have seen is, so they offer themselves again.

## About

The app's version (and the Tauri version it is built with), a short description,
links to the GitHub repository, upstream Ginan and its documentation, the
Apache-2.0 licence and the upstream issue tracker, and the third-party notices
the app has to carry, including OpenStreetMap's map data credit and the full
licence texts under **Show licence texts**. Links open in your system browser,
not in the app window.

## What is not in Settings

| Setting | Where |
| --- | --- |
| Input mode (RINEX file or live stream) | Top of the sidebar |
| RINEX file | Welcome screen, or **RINEX Observations** in the sidebar |
| Provider, series, project | Sidebar > Products |
| Constellations and observation codes | Sidebar > Constellations |
| Processing mode, time window, interval, receiver, antenna | Sidebar > General |
| Output formats (`.POS`, `.GPX`, `.TRACE`, `.SNX`) | Sidebar > Output |
| Observation and correction streams, serial settings, recordings, storage cap | Sidebar, in real-time mode |
| Sidebar width, collapsed or open | Drag the sidebar edge; the **Hide sidebar** button |
| Visualization layout | The Visualization tab |

There is no time-zone setting. Epochs you enter and see in the sidebar and in
the plots are GPS time. The write time shown beside each solution file in the
Position view uses your computer's local time.

## Where settings are stored

The [README](../README.md#where-things-are-stored) lists the locations. In more
detail:

| What | Where |
| --- | --- |
| Directories, processing mode, time window, interval, output formats, constellation codes, provider/series/project, overwrite choice, changed processing parameters, PEA binary path, whether setup is done, tours seen, caster URL and stream sources | `app-settings.json` in the app's data folder |
| The PEA binary and the record of its version (`pea` or `pea.exe`, `pea_version.txt`) | The same data folder |
| Theme, sidebar width and state, Visualization layout and Link axes | The webview's local storage |
| Earthdata and NTRIP logins | Your netrc file |
| Cached bias-code tables | `.bia_code_cache` inside the products directory |
| The RINEX file, the stream lists | Not stored. Every launch starts with none open. |

The data folder is the Tauri app-data directory for the identifier
`au.gov.ga.ginan`.

| OS | Folder |
| --- | --- |
| Windows | `%APPDATA%\au.gov.ga.ginan` |
| macOS | `~/Library/Application Support/au.gov.ga.ginan` |
| Linux | `~/.local/share/au.gov.ga.ginan` |

Confirmed on macOS: the folder holds `app-settings.json`, the downloaded `pea`
binary and `pea_version.txt`.

To reset the app to a fresh state, close it and delete `app-settings.json`.
Setup runs again on the next launch.

Deleting that one file is enough. Every setting above lives in it, so removing
it takes the products directory with it, and the app treats "no record of
onboarding and no products directory" as a first run. Your Earthdata and NTRIP
logins are not in that file and survive, because they live in your netrc.
