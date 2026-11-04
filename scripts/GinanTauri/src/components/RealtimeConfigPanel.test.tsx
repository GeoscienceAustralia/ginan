import { beforeEach, describe, expect, it } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import RealtimeConfigPanel from "./RealtimeConfigPanel";
import type { NtripStream } from "../config/ntrip";
import { resetAppStore, store } from "../test/storeHelpers";
import { mockInvoke } from "../test/tauriMocks";
import { DEFAULT_SERIAL_SETTINGS } from "../types";

beforeEach(() => {
  resetAppStore();
  store().setInputMode("realtime");
});

const obsSection = () => document.querySelector<HTMLElement>('[data-tour="rt-obs"]')!;
const navSection = () => document.querySelector<HTMLElement>('[data-tour="rt-nav"]')!;
const mountInputs = (section: HTMLElement) =>
  within(section).queryAllByRole("combobox", { name: "Stream" }) as HTMLInputElement[];
const deviceInputs = (section: HTMLElement) =>
  within(section).queryAllByRole("combobox", { name: "Serial device" }) as HTMLInputElement[];
/**
 * Text of each suggestion in the open listbox. The list is portalled to
 * <body>, so it is found from the screen, not from a section. Scoped to the
 * listbox because the serial line settings contain native `<option>`s too.
 */
const optionLabels = () =>
  within(screen.getByRole("listbox"))
    .getAllByRole("option")
    .map((o) => o.firstElementChild!.textContent);

/**
 * The knob inputs carry a sibling `<label>` with no `htmlFor`, so they have no
 * accessible name to query by — reach them through the label's wrapper instead.
 */
const fieldFor = <T extends HTMLElement>(label: RegExp): T =>
  screen.getByText(label).parentElement!.querySelector("input, select") as T;

const sourcetable = (rows: Partial<NtripStream>[]): NtripStream[] =>
  rows.map((r) => ({
    mountpoint: "ALIC00AUS0",
    identifier: "Alice Springs",
    format: "RTCM 3.3",
    format_details: "",
    carrier: "2",
    nav_system: "GPS",
    network: "AUSCORS",
    country: "AUS",
    category: "obs",
    ...r,
  }));

describe("stream lists", () => {
  it("shows the shipped observation and correction streams", () => {
    render(<RealtimeConfigPanel />);
    expect(mountInputs(obsSection()).map((i) => i.value)).toEqual(["ALIC00AUS0"]);
    expect(mountInputs(navSection()).map((i) => i.value)).toEqual([
      "BCEP00BKG0",
      "SSRA00CNE0",
    ]);
  });

  it("edits one stream without disturbing its neighbours", () => {
    render(<RealtimeConfigPanel />);

    fireEvent.change(mountInputs(navSection())[0], { target: { value: "BCEP00DLR0" } });

    expect(store().realtime.nav_streams).toEqual(["BCEP00DLR0", "SSRA00CNE0"]);
  });

  it("adds an empty stream row", async () => {
    render(<RealtimeConfigPanel />);

    await userEvent.click(within(obsSection()).getByRole("button", { name: "Add to Observation" }));
    await userEvent.click(screen.getByRole("menuitem", { name: "NTRIP" }));

    expect(store().realtime.obs_streams).toEqual(["ALIC00AUS0", ""]);
  });

  it("removes a stream by index", async () => {
    render(<RealtimeConfigPanel />);

    const removes = within(navSection()).getAllByTitle("Remove this stream");
    await userEvent.click(removes[0]);

    expect(store().realtime.nav_streams).toEqual(["SSRA00CNE0"]);
  });

  it("keeps the two lists independent", async () => {
    render(<RealtimeConfigPanel />);

    await userEvent.click(within(obsSection()).getAllByTitle("Remove this stream")[0]);

    expect(store().realtime.obs_streams).toEqual([]);
    expect(store().realtime.nav_streams).toHaveLength(2);
  });
});

describe("serial streams", () => {
  it("shows a passive type label on each card, with no stream type dropdown", () => {
    store().updateRealtime({ obs_streams: ["serial://"] });
    render(<RealtimeConfigPanel />);

    const labels = (section: HTMLElement) =>
      Array.from(section.querySelectorAll(".rt-stream-type")).map((el) => el.textContent);
    expect(labels(obsSection())).toEqual(["Serial"]);
    expect(labels(navSection())).toEqual(["NTRIP", "NTRIP"]);
    expect(screen.queryByRole("combobox", { name: "Stream type" })).toBeNull();
  });

  it("shows a serial source's full device path in its row, like a mountpoint", () => {
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyAMA0"], nav_streams: ["serial://COM3"] });
    render(<RealtimeConfigPanel />);

    expect(deviceInputs(obsSection())[0].value).toBe("/dev/ttyAMA0");
    expect(deviceInputs(navSection())[0].value).toBe("COM3");
    expect(obsSection().querySelector(".rt-stream-title")).toBeNull();
  });

  it("shows a serial URI with a fixed scheme prefix and a device placeholder", () => {
    store().updateRealtime({ obs_streams: ["serial://"] });
    render(<RealtimeConfigPanel />);

    // jsdom's user agent is not Windows, so the POSIX device example is shown.
    expect(deviceInputs(obsSection())[0]).toHaveAttribute("placeholder", "/dev/ttyAMA0");
    expect(within(obsSection()).getByText("serial://")).toBeInTheDocument();
    expect(store().realtime.obs_streams).toEqual(["serial://"]);
    expect(mountInputs(obsSection())).toHaveLength(0);
  });

  it("stores the full URI while only the device part is edited", () => {
    store().updateRealtime({ obs_streams: ["serial://"] });
    render(<RealtimeConfigPanel />);

    // A pasted full URI is not double-prefixed.
    fireEvent.change(deviceInputs(obsSection())[0], {
      target: { value: "serial:///dev/ttyUSB0" },
    });
    expect(store().realtime.obs_streams).toEqual(["serial:///dev/ttyUSB0"]);
    expect(deviceInputs(obsSection())[0].value).toBe("/dev/ttyUSB0");
  });

  it("blanks the effective caster once no stream is an NTRIP mountpoint", () => {
    store().updateRealtime({ obs_streams: ["serial://"] });
    render(<RealtimeConfigPanel />);
    // The nav list still has NTRIP mountpoints, so the caster is still in use.
    expect(store().realtime.caster_url).not.toBe("");

    act(() => store().updateRealtime({ nav_streams: ["serial://", "serial://"] }));

    expect(store().realtime.caster_url).toBe("");
  });
});

describe("the caster reference", () => {
  it("does not sit under the stream list", () => {
    render(<RealtimeConfigPanel />);
    expect(screen.queryByText(/Mountpoints on/)).not.toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /Change caster or sign in/ })).not.toBeInTheDocument();
  });

  it("is reached from the stream list's error note instead", async () => {
    store().setCasterUrl("");
    render(<RealtimeConfigPanel />);

    await userEvent.click(mountInputs(obsSection())[0]);
    await userEvent.click(await screen.findByRole("button", { name: "Open caster settings" }));

    expect(store()).toMatchObject({ settingsOpen: true, settingsPage: "credentials" });
  });
});

describe("sourcetable suggestions", () => {
  it("offers no suggestions before the sourcetable is fetched", async () => {
    mockInvoke({ fetch_ntrip_sourcetable: [] });
    render(<RealtimeConfigPanel />);

    await userEvent.click(mountInputs(obsSection())[0]);

    // The list opens, empty.
    expect(within(screen.getByRole("listbox")).queryAllByRole("option")).toHaveLength(0);
  });

  it("routes observation and correction mountpoints to their own lists", async () => {
    mockInvoke({
      fetch_ntrip_sourcetable: sourcetable([
        { mountpoint: "ALIC00AUS0", category: "obs" },
        { mountpoint: "SSRA00CNE0", category: "ssr", identifier: "SSR" },
        { mountpoint: "MIXED0", category: "other", identifier: "Both" },
      ]),
    });
    render(<RealtimeConfigPanel />);

    await userEvent.click(mountInputs(obsSection())[0]);
    await waitFor(() => expect(optionLabels()).toEqual(["ALIC00AUS0", "MIXED0"]));

    await userEvent.click(mountInputs(navSection())[0]);
    await waitFor(() => expect(optionLabels()).toEqual(["SSRA00CNE0", "MIXED0"]));
  });

  it("shows an NTRIP stream as one row, with no card to open", async () => {
    mockInvoke({ fetch_ntrip_sourcetable: sourcetable([{ mountpoint: "ALIC00AUS0", category: "obs" }]) });
    render(<RealtimeConfigPanel />);
    expect(obsSection().querySelector("details")).toBeNull();

    await userEvent.click(mountInputs(obsSection())[0]);
    await userEvent.click(await screen.findByRole("option", { name: /ALIC00AUS0/ }));
    expect(mountInputs(obsSection())[0]).toHaveValue("ALIC00AUS0");
  });

  it("shows a suggestion's location and format on separate lines", async () => {
    mockInvoke({ fetch_ntrip_sourcetable: sourcetable([{}]) });
    render(<RealtimeConfigPanel />);

    await userEvent.click(mountInputs(obsSection())[0]);

    await waitFor(() => {
      const row = within(screen.getByRole("listbox")).getAllByRole("option")[0];
      expect(row.querySelector(".dd-option-location")!.textContent).toBe("Alice Springs");
      expect(row.querySelector(".dd-option-format")!.textContent).toBe("RTCM 3.3");
    });
  });

  it("offers ports, not mountpoints, on a serial stream", async () => {
    mockInvoke({
      fetch_ntrip_sourcetable: sourcetable([{}]),
      list_serial_ports: ["/dev/ttyUSB0"],
    });
    store().updateRealtime({ obs_streams: ["serial://"] });
    render(<RealtimeConfigPanel />);

    await userEvent.click(deviceInputs(obsSection())[0]);

    await waitFor(() => expect(optionLabels()).toEqual(["/dev/ttyUSB0"]));
    expect(optionLabels()).not.toContain("ALIC00AUS0");
  });
});

describe("streaming knobs", () => {
  it("does not ask for the SSR antenna offset, which the SSR stream name decides", () => {
    render(<RealtimeConfigPanel />);
    expect(screen.queryByText(/SSR antenna offset/)).not.toBeInTheDocument();
  });

  it("sets the epoch pacing values", () => {
    render(<RealtimeConfigPanel />);

    fireEvent.change(fieldFor(/Wait next epoch/), { target: { value: "5" } });
    fireEvent.change(fieldFor(/Max receiver latency/), { target: { value: "2.5" } });

    expect(store().realtime).toMatchObject({ wait_next_epoch: 5, max_rec_latency: 2.5 });
  });

  it("sets the storage cap, flooring it to whole megabytes", () => {
    render(<RealtimeConfigPanel />);

    fireEvent.change(fieldFor(/Storage cap/), { target: { value: "250.7" } });

    expect(store().realtimeStorageCapMb).toBe(250);
  });
});

describe("serial line settings", () => {
  it("offers the machine's serial ports as suggestions", async () => {
    mockInvoke({ list_serial_ports: ["/dev/ttyUSB0", "/dev/ttyACM0"] });
    store().updateRealtime({ obs_streams: ["serial://"] });
    render(<RealtimeConfigPanel />);

    await userEvent.click(deviceInputs(obsSection())[0]);

    await waitFor(() =>
      expect(optionLabels()).toEqual(["/dev/ttyUSB0", "/dev/ttyACM0"]),
    );
  });

  it("keeps one settings entry per stream, index-aligned, defaulting the rest", async () => {
    const nav = [...store().realtime.nav_streams];
    nav[1] = "serial://";
    store().updateRealtime({ nav_streams: nav });
    render(<RealtimeConfigPanel />);
    const parity = within(navSection()).getByText("Parity").parentElement!.querySelector("select")!;
    await userEvent.selectOptions(parity, "even");

    expect(store().streamSerial.corr).toEqual([
      DEFAULT_SERIAL_SETTINGS,
      { ...DEFAULT_SERIAL_SETTINGS, parity: "even" },
    ]);
    // Derived per stream: the untouched observation stream reads as the defaults.
    expect(store().streamSerial.obs).toEqual([DEFAULT_SERIAL_SETTINGS]);
  });

  it("drops a removed stream's settings with it", async () => {
    const nav = [...store().realtime.nav_streams];
    nav[1] = "serial://";
    store().updateRealtime({ nav_streams: nav });
    render(<RealtimeConfigPanel />);
    const baud = within(navSection()).getByText("Baud").parentElement!.querySelector("input")!;
    fireEvent.change(baud, { target: { value: "4800" } });

    await userEvent.click(within(navSection()).getAllByTitle("Remove this stream")[0]);

    expect(store().realtime.nav_streams).toHaveLength(1);
    expect(store().streamSerial.corr).toEqual([{ ...DEFAULT_SERIAL_SETTINGS, baud: 4800 }]);
  });
});

describe("card identity", () => {
  const navCards = () => Array.from(navSection().querySelectorAll<HTMLElement>(".rt-stream-block"));

  it("keeps the surviving row's node when an earlier row is removed", async () => {
    render(<RealtimeConfigPanel />);
    const survivor = navCards()[1];

    await userEvent.click(within(navSection()).getAllByTitle("Remove this stream")[0]);

    expect(navCards()).toHaveLength(1);
    expect(navCards()[0]).toBe(survivor);
  });

  it("draws a serial source as one box: device on the row, line settings under it", () => {
    store().replaceSection("obs", [{ type: "serial", value: "/dev/ttyUSB0" }]);
    render(<RealtimeConfigPanel />);

    const box = obsSection().querySelector<HTMLElement>(".rt-stream-block")!;
    expect(box).toHaveAttribute("data-type", "serial");
    expect(box).toHaveClass("rt-stream-flat", "rt-stream-box");
    expect(box.tagName).toBe("DIV");
    expect(box.querySelector(".rt-stream-field")).toHaveAttribute("title", "/dev/ttyUSB0");
    expect(within(box.querySelector<HTMLElement>(".rt-stream-box-body")!).getByText("Baud")).toBeInTheDocument();
  });
});

describe("tour anchors", () => {
  it("carries the targets the streaming tour points at", () => {
    const { container } = render(<RealtimeConfigPanel />);
    for (const id of ["rt-obs", "rt-caster", "rt-nav", "rt-settings"]) {
      expect(container.querySelector(`[data-tour="${id}"]`), id).not.toBeNull();
    }
  });
});
