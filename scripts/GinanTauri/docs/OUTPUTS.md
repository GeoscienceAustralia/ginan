# Reading the outputs

PEA writes its results into the **output directory** you chose in
**Settings > Output**. The app plots two of the files: the `.POS` position
solution and the `.TRACE` residuals. Everything is on the **Visualization** tab
or, for the satellite table, the **Streams** tab.

See the [user guide](USER_GUIDE.md) for how to run PEA.

## What PEA writes

| File | Written when | Viewed in the app |
| --- | --- | --- |
| `ppp_config.yaml` / `rt_ppp_config.yaml` | Every run (unless **Overwrite on run** is off) | Settings > Configuration > PEA config YAML |
| `.POS` | **Output Formats > .POS** is on (default) | Visualization > Position |
| `.TRACE` | **.TRACE** is on | Visualization > Residuals, Streams > Satellites |
| `.SPP` | **Output Formats > .SPP** is on (default) and a real-time run uses SBAS; the option is greyed out otherwise | Visualization > Position (as **SBAS SPP**), Streams > SBAS |
| `.GPX` | **.GPX** is on | No. Open it in a mapping tool. |
| `.SNX` | **.SNX** is on | No. |
| `*_OBS.rtcm`, `*_NAV.rtcm`, `*_DEC.json`, `*_OBS.rnx`, `ntrip_stats.jsonl` | Real-time runs, depending on **Recordings** | Streams tab (stats only) |
| `stream_test/` | A real-time stream test | Streams > Stream test |

PEA names the position, GPX and SINEX files
`<RECEIVER>_<CONFIG>_<YYYY><DDD><HH>` plus the extension, and the `.TRACE` file
`<CONFIG>_<RECEIVER>_<YYYY><DDD><HH>`, where the last three fields are the year,
the day of the year and the hour. The Visualization tab finds files by
extension, and only looks directly in the output directory, not in subfolders.
A `*_smoothed.pos` file, when PEA writes one, is the backward-smoothed solution
and is labelled as such.

## The Visualization tab

If no output directory is set the tab says "No output directory set". Otherwise
a switch at the top chooses the source:

- **Position** reads the `.POS` file.
- **Residuals** reads the `.TRACE` file.

Both re-read the output directory when you open them and again when a run
finishes, so a finished run is already on screen. Both also work on output left
by an earlier session. If a view has nothing to show it says why, for example
"No .pos files yet" (turn on **.POS** and run), "No .TRACE files yet" (turn on
**.TRACE** and run) or "Could not read the output directory" with a **Check
again** button.

### Position

The Position view shows a set of figures of the solution. Every figure uses the
same data: the offsets **dN**, **dE** and **dU** (north, east and up, in
millimetres) from the a priori position, with the 1-sigma formal error of each.
The figures can be drawn from at most 20,000 epochs, so a very long file is
thinned for drawing, but the statistics are always computed over every epoch.
The panels say "Sampled" or "Drawn from N sampled epochs" when this happens.

An SBAS `.SPP` file is plotted the same way, with these differences. It has
no formal errors, so **Formal error** is unavailable, the ±σ values and
whiskers are left out, means are unweighted, and the summary shows the latest
protection levels where a PPP solution shows its convergence time. It has
protection levels instead: the **Protection levels** figure draws HPL and VPL
over time (it is only offered for an `.SPP`). When PEA had no a priori
position, the offsets are measured from the file's first epoch.

**Toolbar.** The toolbar sits on the tab bar, to the right of the tabs.

| Control | What it does |
| --- | --- |
| **Solution file** | Which `.POS` file to plot. Each entry shows the name, its size and when it was written, plus "(smoothed)", "empty" (0 bytes) or "header only" where that applies. The app picks the newest forward solution that has epochs. |
| **Link axes** | On by default. Zooming or hovering any time-based panel moves all of them together, and highlights the hovered epoch on the scatter and map. |
| **Add view** | Adds a figure beside the active panel. Click to add it, or drag it from the menu onto the layout. |
| Layout presets (grid icon) | **Overview** (the default), **Single**, **Side by side**, **Stacked**, **2 × 2 grid**, **Accuracy**, **Convergence** and **Map focus**, plus **Even out sizes**. |
| Refresh | Scans the output directory again. |

**Timeline.** Above the figures, a scrubber picks an epoch, and every panel
marks it. It has a strip for the whole run, where you can drag to choose a
zoomed range, and a detail strip for that range. Click or drag on the detail
strip, scroll over it, or press the play button to step through the run at the
chosen speed (1 epoch per step unless you pick another). The timeline opens on
the whole run. With the timeline focused you can also use the arrow keys (Shift
for ten at a time, Page Up and Page Down for a hundred, Home and End for the
ends of the visible range), Space to play or pause, `+`, `-` and `0` to zoom in,
out and back to the whole run, and Escape to clear the selection. Clicking a
point in a chart selects that epoch as well.

**Panels.** Each panel has a header with a drop-down to change its figure and
five buttons: **Panel settings**, **Split right**, **Split down**,
**Maximise** (**Restore layout** when maximised) and **Close panel**. Drag a
header onto the edge of another panel to put them side by side, onto its middle
to swap them, or onto the outer edge of the area to span the full width or
height. Drag the dividers to resize, and double-click one to even them out.
**Panel settings** has a **Solution** drop-down, so a panel can be pinned to a
different `.POS` file from the toolbar's, for example to compare the forward and
smoothed solutions side by side, plus the options for that figure. Your layout
and the Link axes choice are remembered between sessions.

The figures:

| Figure | Shows | Options |
| --- | --- | --- |
| **Time series** | dN, dE and dU against time, one strip each. | Components; **Formal error ±1σ**; **Mean ±2σ**. |
| **Horizontal scatter** | dE against dN on equal axes, coloured by time. | **Mean and ±1σ/±2σ rings**. |
| **Map** | Epoch positions on an OpenStreetMap base map. | |
| **Error magnitude** | Horizontal, vertical and 3D error over time. | What to show; **95% lines**; **Log scale**. |
| **Formal error** | σN, σE and σU over time, to see how the filter converges. | Components; **Log scale**; **Convergence threshold** and **Threshold** (5, 10, 20 or 50 cm). |
| **Histogram** | The distribution of dN, dE, dU or the horizontal, vertical or 3D error, with a normal fit. | **Quantity**; **Normal fit**; **Trim outer 1% each side**. |
| **Error CDF** | The share of epochs within a given error, with the 50% and 95% points marked. | What to show; **Clip at 99th percentile**. |
| **Summary** | Epochs, duration, horizontal, vertical and 3D RMS, CEP50, the 95% values and the convergence time, plus the latest or selected epoch. | **Convergence at** (5, 10, 20 or 50 cm of horizontal σ). |
| **Statistics** | Weighted mean, standard deviation, RMS, mean σ and range for dN, dE and dU. | |

In the time series, click to select an epoch, drag across a span to zoom and
double-click to reset (or press **Reset zoom**). In the scatter, click a point
to select it, drag a box to zoom and double-click to reset. Hovering shows the
time and values. The map is a normal slippy map: drag to pan, scroll or pinch to
zoom, and hover a point for its epoch and offsets. Points are coloured from
oldest to newest, and the latest is ringed in white. The map needs an internet
connection for its tiles, and shows an "© OpenStreetMap contributors" credit.

### Residuals

The Residuals view reads the `.TRACE` file and draws one satellite at a time,
because one file holds every satellite and signal. A `.TRACE` holds a prefit and
a postfit residual for every measurement at every epoch, so a bad satellite or
signal usually shows up here before it is visible in the position.

| Control | What it does |
| --- | --- |
| **Trace** | Which `.TRACE` file, newest first. |
| **Satellite** | The satellite to plot, grouped by constellation. The first one in the file is chosen for you. |
| **Receiver** | Only appears when the file holds more than one receiver. |
| **Prefit** | Draws the residual before the filter update as a dashed line. |
| **Refresh** | Scans the output directory again. |

In the chart, drag across the time axis to zoom, and double-click or press
**Reset zoom** to go back. Below it, a table gives each series' postfit mean,
postfit RMS, prefit RMS, mean sigma and sample count, one row per
satellite, signal and kind of measurement. A line under the table gives the
number of epochs, the time span and the number of satellites in the file. It may
add "Drawn from N sampled residuals" (the table is still over every sample) or
"N unreadable lines skipped".

### Satellite status

The **Satellites** panel is at the bottom of the **Streams** tab, and works for
file runs as well as real-time ones. It reads the `.TRACE` and lists, for each
constellation, the satellites the solution has used: signals, postfit RMS,
number of samples and the last epoch. The heading says "N contributing of M
seen". A satellite is contributing if it has a residual at the newest epoch in
the file. One that stopped appearing is shown dimmed with "· dropped" after its
last epoch, usually because it set below the elevation mask, lost lock or was
rejected.

It re-reads the file every five seconds during a run, once more when the run
ends, and when you press **Refresh**. With no `.TRACE` it says "No .TRACE file
in the output directory. Turn on TRACE output in the sidebar to see which
satellites the solution is using." Right after a run starts it may say "No
residuals in the trace file yet. It appears as soon as PEA processes an epoch."

## After a run: the Workflow tab

When PEA finishes the Workflow tab adds "Generating position plots from PEA
output...", lists the `.POS` files it found ("Found N .pos files in <folder>:",
then one line per file) and ends with "Generated N plot(s)". If there are none
it says "No .pos files found in <folder>", which usually means **.POS** was off
or PEA failed before it wrote an epoch. The Console tab has PEA's own output for
the cause.
