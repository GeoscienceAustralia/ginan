# Ginan UI documentation

Documentation for the Ginan UI desktop app, split into guides for people who
use it and pages for people who work on it.

## Using the app

| Page | Read it to |
| --- | --- |
| [User guide](USER_GUIDE.md) | Install the app, finish first-run setup and process a RINEX file from start to finish. |
| [Real-time processing](REALTIME.md) | Process live NTRIP or serial streams: the caster, the stream lists, the stream test and the Streams tab. |
| [Reading the outputs](OUTPUTS.md) | Use the position plots, the map, the `.TRACE` residual view and the satellite table. |
| [Settings reference](SETTINGS.md) | Look up every page in Settings and where each setting is stored. |
| [Troubleshooting](TROUBLESHOOTING.md) | Find a message the app showed you and what to do about it. |

If you are new to the app, read the user guide first, then the page for the
kind of run you want to do.

## Working on the app

| Page | Read it to |
| --- | --- |
| [README](../README.md) | See the prerequisites, run the app from source, and find your way around the code. |
| [CONTRIBUTING](../CONTRIBUTING.md) | Check your change before a pull request and follow the code guidelines. |
| [TESTING](../TESTING.md) | Run and write the frontend tests. |

## Conventions in these pages

- **Bold** is a label you can see in the app: a button, a tab, a heading or a
  field name.
- `Settings > PEA Binary` means open Settings, then choose the PEA Binary page.
- Text in quotation marks is a message the app shows, worded as the app words it.
  Parts that change are in angle brackets, like `<file>`.
- Times in the app are GPS time unless a page says otherwise.
