import { useAppStore } from "../store/appStore";
import type { ConfigSectionKey, TourKey } from "../store/appStore";
import type { InputMode } from "../types";

/**
 * Content for the guided tours.
 *
 * Steps address the UI through `data-tour="…"` attributes rather than class
 * names, so restyling a component cannot silently break the tour — the
 * attribute is the contract, and a step whose target is missing or zero-sized
 * is skipped rather than pointing at nothing.
 */

export type TourPlacement = "top" | "bottom" | "left" | "right";

export interface TourStep {
  /** Value of the target's `data-tour` attribute; null centres the card. */
  target: string | null;
  title: string;
  body: string;
  /** Preferred side of the target. Flipped automatically when it won't fit. */
  placement?: TourPlacement;
  /** Put the app into the state this step describes before showing it. */
  onEnter?: () => void;
}

/** Open a sidebar section, and the rail itself if the user has it shut. */
function openSection(key: ConfigSectionKey) {
  return () => {
    const { sidebarCollapsed, toggleSidebar, setConfigSection } =
      useAppStore.getState();
    if (sidebarCollapsed) toggleSidebar();
    setConfigSection(key);
  };
}

/** Shown straight after first-run setup, when no file is open yet. */
const WELCOME_TOUR: TourStep[] = [
  {
    target: null,
    title: "A two-minute tour",
    body: "Nothing is open yet, so this is the app at its emptiest. Use the arrow keys or the buttons to move along, and press Esc whenever you want out.",
  },
  {
    target: "add-rinex",
    title: "Everything starts with a file",
    body: "Open a RINEX observation file to begin, then add more files from Observation in the sidebar. Zipped and compressed files are supported too.",
    placement: "bottom",
  },
  {
    target: "settings",
    title: "Machine setup lives here",
    body: "This is where you set things once and forget about them: your download folder, your NASA login, and how the app looks. You can also load and save config files here.",
    placement: "bottom",
  },
  {
    target: "help",
    title: "This tour, again",
    body: "Most of the app only shows up once you have a file open, so the tour carries on from there. Click this button any time you want it back.",
    placement: "bottom",
  },
];

/** Show the sidebar if the user has hidden it. */
function showSidebar() {
  const { sidebarCollapsed, toggleSidebar } = useAppStore.getState();
  if (sidebarCollapsed) toggleSidebar();
}

/**
 * Runs itself the first time a RINEX is open and the full shell is on screen.
 *
 * Every target here is on screen whatever the user has opened or shut: the
 * Corrections step points at the whole section, not at the controls inside
 * its card, so a collapsed card does not make the tour skip a step.
 */
const WORKSPACE_TOUR: TourStep[] = [
  {
    target: "sidebar",
    title: "The run is set up here",
    body: "Your inputs sit at the top: Observation and Corrections. Under them are General, Constellations and Output, roughly in the order you will use them. Drag the sidebar's right edge to make it wider, or hide it with the button in the top left.",
    placement: "right",
    onEnter: showSidebar,
  },
  {
    target: "section-input",
    title: "Observation: the RINEX file",
    body: "The file you opened shows as a FILE row, and the bin on it removes it. Under it you can add more files from the same day, by clicking or dropping them. Ginan reads a new file straight away and looks for corrections that match its dates. The + next to Observation can switch this run to a real-time data stream instead.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "ppp-source",
    title: "Corrections: whose orbits and clocks",
    body: "Open the PRODUCTS row to choose who supplies the satellite orbits, clocks and biases. Only the groups with a complete set for your dates are listed, and the dot beside the provider says whether its server can be reached. The files are downloaded when you press Process.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "section-general",
    title: "General: mode, window and metadata",
    body: "Say whether the receiver was sitting still or moving, and pick the time range to process. The times are in GPS time, as PEA reads them. The receiver and antenna details come from the file, but you can fix them if they are wrong.",
    placement: "right",
    onEnter: openSection("general"),
  },
  {
    target: "section-constellations",
    title: "Constellations: which signals to use",
    body: "The satellite systems your file recorded, and the signals from each, in priority order. Validate BIA checks them against the bias file, and any signal it cannot correct is struck through so you can switch it off before you run.",
    placement: "right",
    onEnter: openSection("constellations"),
  },
  {
    target: "section-output",
    title: "Output: where results land",
    body: "Name the run, and its results go into a folder of that name. Pick which file formats you want out of it.",
    placement: "right",
    onEnter: openSection("output"),
  },
  {
    target: "tabbar",
    title: "The workspace",
    body: "Workflow shows each step of the run as it happens. Console has everything Ginan prints, and keeps it when you switch tabs. Visualisation plots the position and residuals once there are results.",
    placement: "bottom",
  },
  {
    target: "run-controls",
    title: "Process, then Stop",
    body: "Process does the whole job: saves your settings, downloads what it needs, then runs. While it runs the same button becomes Stop. If something is missing, a list beside it says what, with a way to fix each item.",
    placement: "bottom",
  },
  {
    target: "status-bar",
    title: "What the app is doing",
    body: "A one-line summary of what is happening right now. Worth a look if something seems to be taking a while.",
    placement: "top",
  },
];

/**
 * The same shell as WORKSPACE_TOUR, driven from real-time data streams instead
 * of a file. Kept as its own list rather than a filtered copy of the workspace
 * steps because the differences are not cosmetic: a streaming run reads no
 * epoch window, downloads no SP3/CLK products and never sees a RINEX header
 * (see getRealtimeConstellations and buildConfigInputs in the store).
 *
 * The `rt-*` targets live inside the input area, which scrolls. Tour scrolls
 * each target into view on entry, so a step near the bottom of the rail is
 * reachable without the user moving anything. `rt-caster` is the first stream
 * card, or the section's add box while it has none, so it is always on screen.
 * `rt-recording` is in the Output section, which its step opens.
 */
const STREAMING_TOUR: TourStep[] = [
  {
    target: "sidebar",
    title: "The run is set up here",
    body: "This is the same sidebar as a file run. At the top are the streams to read from, Observation and Corrections, with the settings for a live run under them. General, Constellations and Output follow, as with a file.",
    placement: "right",
    onEnter: showSidebar,
  },
  {
    target: "section-input",
    title: "Input: streams, not a file",
    body: "This run is real-time because Observation holds streams instead of a FILE row. Adding a file from the + menu switches back to a file run, after asking first. Each stream is one row: its type, its field and a bin to remove it. Only Serial opens up, for its line settings.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "rt-obs",
    title: "Observation streams",
    body: "The reference stations you are positioning against, as raw RTCM. You need at least one, or Process will not start. When the list is empty, the dashed box adds an NTRIP stream in one click.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "rt-caster",
    title: "NTRIP, HTTP or serial, per stream",
    body: "Each stream's type is fixed when you add it with the + next to the heading. An NTRIP stream lets you pick a short code like ALIC00AUS0 from the caster's list, and the caster address is added for you. HTTP takes a URL, and Serial the port your receiver is plugged into. To change a type, remove the stream and add another.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "rt-nav",
    title: "Corrections streams",
    body: "Where the satellite corrections come from. A file run downloads them for its dates, but a live run has no dates, so they keep arriving as it runs. It needs two kinds of stream: broadcast ephemeris (the satellites' own orbits), and SSR corrections to sharpen them, or SBAS in place of SSR. Each kind has its own group here, with its own + and a box to add it when it is missing; SBAS is offered under SSR only. Whether SSR corrections are for the antenna or the satellite's centre is read from the SSR stream's name.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "rt-settings",
    title: "Settings for a live run",
    body: "The storage cap limits how much the recordings may take up: once it is reached, the oldest are cleared out. Wait next epoch and max receiver latency say how long PEA waits for data before it moves on.",
    placement: "right",
    onEnter: openSection("input"),
  },
  {
    target: "section-general",
    title: "General: the processing mode",
    body: "Whether the receiver is sitting still or moving still applies. The time window and the receiver and antenna details do not: a stream has no start and end time, and no file header to read them from.",
    placement: "right",
    onEnter: openSection("general"),
  },
  {
    target: "section-constellations",
    title: "Constellations: which signals to use",
    body: "Monitor stream on the Streams tab lists the signals each constellation is sending. Pick the ones to use and their order here, and the run keeps your choice.",
    placement: "right",
    onEnter: openSection("constellations"),
  },
  {
    target: "section-output",
    title: "Output: where results land",
    body: "As with a file run: name the run, and choose where results are saved and in which formats.",
    placement: "right",
    onEnter: openSection("output"),
  },
  {
    target: "rt-recording",
    title: "What gets kept on disk",
    body: "Choose what is saved while the streams run: the raw RTCM bytes, a readable version of them, or ordinary RINEX files. A live run keeps writing for as long as it is up, which is what the storage cap is for.",
    placement: "right",
    onEnter: openSection("output"),
  },
  {
    target: "tab-streams",
    title: "Streams: check them first",
    body: "Press Monitor stream here to check every mountpoint for about 10 seconds: whether it connects, the login is accepted and data arrives. Process waits for a monitor run that passed. During a run this tab shows how each stream is doing.",
    placement: "bottom",
  },
  {
    target: "tabbar",
    title: "The rest of the workspace",
    body: "Workflow shows each step of the run, and Console the full log. Visualisation plots the position as it comes in, following the latest epoch.",
    placement: "bottom",
  },
  {
    target: "run-controls",
    title: "Process, then Stop",
    body: "Process saves your settings and connects to the streams. There is nothing to download first, so it starts straight away, and the same button becomes Stop while it runs. If something is missing, a list beside it says what.",
    placement: "bottom",
  },
  {
    target: "status-bar",
    title: "What the app is doing",
    body: "A one-line summary of what is happening. Worth a look if the Streams tab has gone quiet.",
    placement: "top",
  },
];

export const TOURS: Record<TourKey, TourStep[]> = {
  welcome: WELCOME_TOUR,
  workspace: WORKSPACE_TOUR,
  streaming: STREAMING_TOUR,
};

/**
 * The tour that matches what is on screen.
 *
 * One place decides this, because three call sites need the same answer: the
 * auto-start in App, the help button in the navbar and the one in Settings.
 * Realtime is checked before `rinexData` — switching to realtime clears the
 * parsed RINEX (see setInputMode), but a stale file would otherwise win here.
 */
export function tourForCurrentShell(
  inputMode: InputMode,
  hasRinex: boolean,
): TourKey {
  if (inputMode === "realtime") return "streaming";
  return hasRinex ? "workspace" : "welcome";
}
