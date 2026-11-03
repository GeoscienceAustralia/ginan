# Ginan UI (desktop app)

A cross-platform desktop front end for [Ginan](https://github.com/GeoscienceAustralia/ginan)'s
Parameter Estimation Algorithm (PEA). It lets you run Precise Point Positioning
(PPP) on a RINEX observation file or on live NTRIP / serial streams without
hand-editing YAML:

- load a RINEX file and pre-fill the PEA configuration from its header
  (marker, receiver, antenna, a priori position, time window, constellations,
  observation codes);
- download the precise orbit, clock, bias and support products the run needs
  from NASA CDDIS;
- pick and order phase signals (`L` codes) per constellation, checked against
  the biases the selected PPP products actually provide;
- configure real-time inputs (NTRIP casters, mountpoints, serial receivers);
- run PEA, follow its progress and console output, and plot the resulting
  position (`.POS`) and residual (`.TRACE`) files.

It is built with [Tauri 2](https://tauri.app): a React + TypeScript UI in `src/`
and a Rust backend in `src-tauri/` that does the file parsing, downloads and
process management.

To use the app rather than build it, see the [user guide](docs/USER_GUIDE.md)
(the [docs index](docs/README.md) lists the other pages).

## Prerequisites

| Tool | Version | Notes |
| --- | --- | --- |
| [Node.js](https://nodejs.org) | 20 or newer | |
| npm | 10 or newer (ships with Node) | Dependencies are locked in `package-lock.json`; please don't commit a pnpm/yarn lockfile. |
| [Rust](https://rustup.rs) | stable (1.77.2+) | |
| Tauri system dependencies | — | See the [Tauri prerequisites](https://tauri.app/start/prerequisites/) for your OS (WebView2 on Windows, Xcode CLT on macOS, `webkit2gtk-4.1` and friends on Linux). |

To actually process data you will also need:

- a **NASA Earthdata account** (free, <https://urs.earthdata.nasa.gov/users/new>)
  for downloading PPP products from CDDIS. The app asks for it on first run.
- for real-time processing, an account on an **NTRIP caster**, e.g.
  Geoscience Australia's `ntrip.data.gnss.ga.gov.au` (the default).

The PEA binary itself does not need to be built: on first run the app downloads
the latest Ginan release for your platform from GitHub. You can switch to
another release, or point at your own build, in **Settings**.

## Getting started

```bash
cd scripts/GinanTauri
npm install
npm run tauri dev  # starts Vite on :1420 and opens the app with hot reload
```

From the repository root, `just i` and `just rt` do the same two steps.

The first `npm run tauri dev` compiles the Rust backend, which takes a few
minutes; later runs are incremental.

## Common tasks

| Command (in `scripts/GinanTauri`) | What it does |
| --- | --- |
| `npm run tauri dev` | Run the app in development mode. |
| `npm run tauri build` | Build a release bundle/installer for the current OS (output in `src-tauri/target/release/bundle/`). |
| `npm run build` | Typecheck and build the frontend only (into `dist/`). |
| `npx tsc --noEmit` | Typecheck the frontend. |
| `cargo test` (in `src-tauri/`) | Run the backend unit tests. They need `dist/` to exist, so run `npm run build` once first. |
| `cargo clippy --all-targets` (in `src-tauri/`) | Lint the backend. |

## Profiling

A separate profiling build measures the app while it runs. None of it is
compiled into a normal build: the backend half sits behind the `profiling`
Cargo feature, the frontend half behind Vite's `profiling` mode, and
`src-tauri/tauri.profiling.conf.json` turns both on.

```bash
npm run profile                        # optimised build, no installer
./src-tauri/target/release/ginan-tauri # run it (ginan-tauri.exe on Windows)

npm run profile:dev                    # or: dev build with hot reload
```

Use the optimised build for numbers you intend to act on. The dev build is
unoptimised and runs React in development mode, so it is only good for
checking that the instrumentation works.

Then use the app as normal, for example start a realtime run and watch the
Visualization tab. The overlay in the bottom-left corner shows frame rate and
render cost, and has three buttons:

| Output | What it shows | Where it goes |
| --- | --- | --- |
| `perf {...}` log lines, every 5 s | Frontend: fps, slow frames, React render time per tab, round-trip time per command | `app.<date>.log` in the app log directory, and stdout |
| **Flamegraph** / **Flamechart** | Backend wall-clock time per span: every command, the realtime loops, Tauri's IPC and event emits | `profile.svg` / `profile.flamechart.svg` in the app log directory |
| **Start / Stop CPU profile** | Backend CPU time, sampled at 100 Hz | `profiles/` in the app data directory |

The app log directory is `~/Library/Logs/au.gov.ga.ginan` on macOS,
`~/.local/share/au.gov.ga.ginan/logs` on Linux and
`%LOCALAPPDATA%\au.gov.ga.ginan\logs` on Windows. Each button prints the path
of the file it wrote.

Each `perf` line carries its JSON at the end, after the logger's span context.
To pull the summaries out on their own:

```bash
sed -E -n 's/.* (perf \{.*\})[^}]*$/\1/p' ~/Library/Logs/au.gov.ga.ginan/app.*.log
```

Limits worth knowing:

- Span recording starts at launch and `profile.folded` is overwritten on each
  launch. Generate the flamegraph at the end of a session; the first click
  flushes the recording, and later clicks may miss the most recent spans.
- `profile.folded` grows quickly (about 1 MB in the first 15 s of an idle
  launch, more once PEA is streaming output). Keep a session to minutes, not
  hours.
- Keep the app window visible while measuring. macOS stops the webview's
  timers and animation frames while the window is hidden or still opening,
  so those windows report near-zero fps and, in one case, the page logged
  nothing at all until the app was restarted.
- The CPU profile covers the Rust backend only. The webview and PEA are
  separate processes; the overlay's numbers cover the webview, and PEA needs
  an external profiler.
- The profiling build shares its settings and credentials with the normal app.

To check the three buttons without clicking, build with
`VITE_PROFILE_SELFTEST=<seconds> npm run profile`. Two seconds after launch the
build records a CPU profile for that many seconds, then generates the
flamegraph and flamechart, and logs each result as a `selftest ...` line.

To time a new backend function, add
`#[cfg_attr(feature = "profiling", tracing::instrument(skip_all))]` above it.
To time a new part of the UI, wrap it in
`<Profiler id="..." onRender={onRender}>` (see `src/profiling/onRender.ts`).

## Where things are stored

| What | Where |
| --- | --- |
| Earthdata / CDDIS and NTRIP caster logins | Your netrc file (`~/.netrc`, and `~/_netrc` on Windows), the same file `wget`, `curl` and the Python GinanUI use. Passwords are never written into generated configs or app settings. |
| App settings (paths, PPP selection, processing parameters, …) | `app-settings.json` in the Tauri app-data directory for the identifier `au.gov.ga.ginan`. |
| UI preferences (theme, sidebar width, chart layout) | Browser `localStorage` inside the webview. |
| Generated PEA config | `ppp_config.yaml` (file mode) or `rt_ppp_config.yaml` (real-time) in the chosen output directory. |

## Project structure

```text
scripts/GinanTauri/
├── index.html, vite.config.ts      Vite entry point (dev server on port 1420)
├── src/                            React + TypeScript frontend
│   ├── main.tsx, App.tsx           Bootstrap and app shell (startup checks, layout)
│   ├── components/                 UI: config panel, run controls, workspace tabs,
│   │   │                           settings, onboarding/tour, charts
│   │   └── viz/                    Tiling visualisation workspace (panels, layout, charts)
│   ├── hooks/                      Tauri event wiring, file pickers, config import/export,
│   │                               BIA validation, NTRIP telemetry
│   ├── store/appStore.ts           Global Zustand store and persistence
│   ├── config/                     Static definitions: processing parameters, NTRIP helpers
│   └── types/                      Types shared with the backend
└── src-tauri/                      Rust backend
    ├── tauri.conf.json             App/window/bundle configuration
    ├── capabilities/               Permissions granted to the webview
    ├── resources/                  PEA YAML templates (file and real-time)
    ├── fixtures/                   Sample RINEX files used by the tests
    └── src/
        ├── lib.rs                  Registers every command the frontend can `invoke`
        ├── state.rs                Shared state (running process, cancel flags)
        └── commands/               One module per area:
            ├── rinex.rs, crinex.rs     RINEX / Hatanaka (CRX) parsing
            ├── config.rs, ui_config.rs PEA YAML generation, import and export
            ├── products.rs             CDDIS product catalogue and downloads
            ├── credentials.rs          netrc handling for Earthdata and NTRIP logins
            ├── execution.rs            PEA binary management and file-mode runs
            ├── realtime.rs             Real-time runs
            ├── ntrip.rs, streams.rs, stream_test.rs, serial_windows.rs, rtcm_monitor.rs
            │                           NTRIP sourcetables, stream classification and probing
            ├── visualisation.rs, trace.rs  .POS / .TRACE parsing for the charts
            ├── antex.rs                ANTEX lookup and antenna validation
            ├── health.rs               Product-server reachability checks
            └── opener.rs               Opens external links in the system browser
```

## How it fits together

The frontend never touches the file system or network directly. Every side
effect goes through a Rust command (`invoke("parse_rinex", …)`,
`invoke("write_config", …)`, …) registered in `src-tauri/src/lib.rs`, and
long-running work (downloads, PEA runs, live telemetry) reports back through
Tauri events that `src/hooks/useTauriEvents.ts` feeds into the store.

A typical file-mode run:

1. **Load RINEX.** `parse_rinex` reads the header (and scans to the last epoch
   if the header has no `TIME OF LAST OBS`) and returns the metadata plus the
   observation codes present for each constellation.
2. **Choose products.** `scan_ppp_catalogue` lists which provider / series /
   project combinations CDDIS has for the observation window; the user picks
   one.
3. **Choose phase signals.** For every constellation in the file, the
   *Constellations* section lists its phase signals: the `L` codes from the
   header (`L1C`, `L2W`, ...). The code, Doppler and strength observables the
   header also lists are not offered. Users enable signals and drag them into
   priority order. `fetch_bia_codes_for_combo` downloads the `.BIA` file for the
   selected products. A signal is covered when the table has its phase bias
   (`L1C`) or its code bias (`C1C`), since some centres publish code biases only
   for a constellation; signals with neither are shown struck through.
   Constellations the products don't cover are disabled. The enabled signals, in
   order, are written to
   `processing_options.gnss_general.sys_options.<sys>.code_priorities`, which
   only ever holds `L` codes.
4. **Run.** `ensure_dynamic_products` downloads whatever is missing,
   `write_config` renders the YAML from `resources/default_config.yaml` plus the
   user's choices, and `start_pea` runs PEA, streaming its output to the
   *Workflow* and *Console* tabs.
5. **Inspect.** The *Visualization* tab reads the `.POS` and `.TRACE` files from
   the output directory.

Real-time runs follow the same shape, using `resources/rt_ppp_config.yaml`,
`start_pea_realtime`, and the *Streams* tab for live telemetry.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

Apache License 2.0. See [LICENSE.md](../../LICENSE.md) at the repository root.
