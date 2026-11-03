# Contributing to the Ginan desktop app

Thanks for helping out. This file covers the desktop app in
`scripts/GinanTauri/`; for Ginan itself see the repository root README. Setup
instructions are in [README.md](README.md).

## Pull requests

- **Keep each pull request to one change.** Give it a title that says what it
  does, and say in the description what was wrong, why, what you changed and
  how you checked it.
- **Write commit messages the same way:** a short summary line, then a blank
  line and a body that explains the reason for the change.
- **Releases.** A release is a `v*` tag on `main` (`just release v1.2.0`), which
  starts the build and release workflow in `.github/workflows/release.yml`.

## Before you open a pull request

Run these from `scripts/GinanTauri/` and make sure they pass:

```bash
npm run build                  # typechecks (tsc) and builds the frontend
cd src-tauri
cargo clippy --all-targets     # no new warnings
cargo test
```

Then try the change in the running app (`npm run tauri dev`), ideally on the
platform(s) it affects. Window chrome, file dialogs, serial ports and path
handling all differ between Windows, macOS and Linux.

## Guidelines

- **Keep the frontend and backend in step.** Every `#[tauri::command]` is
  registered in `src-tauri/src/lib.rs`. Its argument and return types are
  mirrored in `src/types/index.ts`. When you rename a command, an argument or a
  serialised field, update both sides in the same change.
- **Side effects belong in Rust.** File system, network and process work goes
  in a command, not in the webview. Long or blocking work should not hold up
  the async runtime: use `tokio` I/O or `spawn_blocking`. Report progress with
  events.
- **Never let a secret leave the backend.** Earthdata and NTRIP passwords live
  in the user's netrc file. The backend adds them when it needs them (HTTP
  requests, the PEA config written to disk). They must not be sent to the
  frontend, written to app settings or exported configs, or logged.
- **No machine-specific values in code.** Don't hard-code paths, hostnames or
  credentials. Make them a setting or a user choice.
- **State lives in the store.** Shared UI state goes in `src/store/appStore.ts`.
  Select only the fields a component needs (`useAppStore((s) => s.field)`), so
  unrelated updates don't re-render it.
- **Comments explain why.** Describe the reason or the constraint, not the
  ticket or the history of the change. That belongs in the commit message or
  the pull request.
- **Use npm.** Commit `package-lock.json` changes along with `package.json`, and
  don't add other lockfiles.
- **Tests.** Backend tests live next to the code in `#[cfg(test)]` modules.
  Sample input files go in `src-tauri/fixtures/`. Add a test with every parser
  or config-generation change.
- **Docs.** The [user guide](docs/README.md) quotes button labels, tab names and
  messages from the UI. If your change renames one or changes what a screen
  does, update the page that describes it in the same pull request.

## Reporting bugs

Open an issue that includes your OS, the app version, the
PEA version, and the steps to reproduce. Attach the relevant RINEX file or
config if you can share it. When you include configs or logs, remove any
usernames and passwords first.

## License

By contributing you agree that your contributions are licensed under the
Apache License 2.0 (see [LICENSE.md](../../LICENSE.md)).
