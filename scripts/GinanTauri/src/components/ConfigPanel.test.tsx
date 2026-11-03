import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ConfigPanel from "./ConfigPanel";
import {
  SIDEBAR_DEFAULT_WIDTH,
  SIDEBAR_MAX_WIDTH,
  SIDEBAR_MIN_WIDTH,
  useAppStore,
} from "../store/appStore";
import {
  makeCatalogue,
  makeRinexData,
  resetAppStore,
  store,
} from "../test/storeHelpers";
import { mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  // The PPP product source probes every provider's health on mount.
  mockInvoke({ check_ppp_provider_health: { status: "up", checked_at: 1_700_000_000 } });
});

const header = (label: string) => screen.getByRole("button", { name: new RegExp(label) });
const openSection = (label: string) => userEvent.click(header(label));
const bodyOf = (key: string) => document.getElementById(`config-accordion-body-${key}`)!;
/** The field inputs carry sibling labels with no `htmlFor`. */
const fieldFor = <T extends HTMLElement>(label: RegExp | string, root: HTMLElement = document.body): T => {
  const el = within(root).getByText(label);
  // A label may sit in a heading line inside its field row (Provider does).
  return (el.closest(".field-row") ?? el.parentElement)!.querySelector("input, select") as T;
};

describe("the accordion", () => {
  it("opens on General, the first section below the input sources", () => {
    render(<ConfigPanel />);
    expect(header("General")).toHaveAttribute("aria-expanded", "true");
    expect(bodyOf("general")).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /^Input/ })).not.toBeInTheDocument();
  });

  it("opens one section at a time", async () => {
    render(<ConfigPanel />);

    await openSection("Output");

    expect(store().configSection).toBe("output");
    expect(header("General")).toHaveAttribute("aria-expanded", "false");
    expect(document.getElementById("config-accordion-body-general")).toBeNull();
  });

  it("closes the open section when its header is pressed again", async () => {
    render(<ConfigPanel />);

    await openSection("General");

    expect(store().configSection).toBeNull();
  });

  it("follows the store, which is how the guided tour drives it", () => {
    render(<ConfigPanel />);
    fireEvent.click(header("General")); // close it
    act(() => store().setConfigSection("constellations"));
    expect(header("Constellations")).toHaveAttribute("aria-expanded", "true");
  });

  it("wires each header to the region it controls", () => {
    render(<ConfigPanel />);
    expect(header("General")).toHaveAttribute("aria-controls", "config-accordion-body-general");
    expect(bodyOf("general")).toHaveAttribute("role", "region");
  });

  it("carries the tour anchor for every section", () => {
    const { container } = render(<ConfigPanel />);
    for (const key of ["input", "general", "constellations", "output"]) {
      expect(container.querySelector(`[data-tour="section-${key}"]`), key).not.toBeNull();
    }
    // There is no mode switch; run mode follows the Observation products.
    expect(container.querySelector('[data-tour="input-mode"]')).toBeNull();
  });

  it("marks itself collapsed without unmounting, so nothing loses its state", () => {
    useAppStore.setState({ sidebarCollapsed: true });
    const { container } = render(<ConfigPanel />);
    expect(container.querySelector(".config-panel")).toHaveClass("is-collapsed");
    expect(bodyOf("general")).toBeInTheDocument();
  });
});

describe("the scroll hints", () => {
  /** jsdom lays nothing out, so give the scroller a size and a position. */
  const scrollTo = (el: HTMLElement, { top, client, scroll }: { top: number; client: number; scroll: number }) => {
    Object.defineProperty(el, "clientHeight", { configurable: true, value: client });
    Object.defineProperty(el, "scrollHeight", { configurable: true, value: scroll });
    el.scrollTop = top;
    fireEvent.scroll(el);
  };
  const hint = (where: "above" | "below") => document.querySelector(`.config-scroll-hint.is-${where}`)!;

  it("shows nothing when everything fits", () => {
    const { container } = render(<ConfigPanel />);
    scrollTo(container.querySelector(".config-accordion")!, { top: 0, client: 600, scroll: 600 });
    expect(hint("above")).not.toHaveClass("is-visible");
    expect(hint("below")).not.toHaveClass("is-visible");
  });

  it("hints below when sections run off the bottom, and above once scrolled", () => {
    const { container } = render(<ConfigPanel />);
    const scroller = container.querySelector<HTMLElement>(".config-accordion")!;

    scrollTo(scroller, { top: 0, client: 400, scroll: 900 });
    expect(hint("above")).not.toHaveClass("is-visible");
    expect(hint("below")).toHaveClass("is-visible");

    scrollTo(scroller, { top: 200, client: 400, scroll: 900 });
    expect(hint("above")).toHaveClass("is-visible");
    expect(hint("below")).toHaveClass("is-visible");

    scrollTo(scroller, { top: 500, client: 400, scroll: 900 });
    expect(hint("above")).toHaveClass("is-visible");
    expect(hint("below")).not.toHaveClass("is-visible");
  });

  it("keeps the hints out of the accessibility tree", () => {
    render(<ConfigPanel />);
    expect(hint("above")).toHaveAttribute("aria-hidden", "true");
    expect(hint("below")).toHaveAttribute("aria-hidden", "true");
  });

  it("scrolls a section into view when it opens, but not on mount", async () => {
    const spy = vi.spyOn(Element.prototype, "scrollIntoView");
    render(<ConfigPanel />);
    await act(async () => { await new Promise((r) => setTimeout(r, 20)); });
    expect(spy).not.toHaveBeenCalled();

    await openSection("Output");
    await waitFor(() => expect(spy).toHaveBeenCalledWith({ block: "nearest", behavior: "smooth" }));
    expect(spy.mock.contexts[spy.mock.contexts.length - 1]).toBe(document.querySelector('[data-tour="section-output"]'));
    spy.mockRestore();
  });
});

describe("the resize handle", () => {
  const resizer = () => screen.getByRole("separator", { name: "Resize sidebar" });

  it("reports the current width and its bounds", () => {
    render(<ConfigPanel />);
    expect(resizer()).toHaveAttribute("aria-valuenow", String(store().sidebarWidth));
    expect(resizer()).toHaveAttribute("aria-valuemin", String(SIDEBAR_MIN_WIDTH));
    expect(resizer()).toHaveAttribute("aria-valuemax", String(SIDEBAR_MAX_WIDTH));
  });

  it("resizes from the keyboard, in a bigger step with shift", () => {
    render(<ConfigPanel />);
    const start = store().sidebarWidth;

    fireEvent.keyDown(resizer(), { key: "ArrowRight" });
    expect(store().sidebarWidth).toBe(start + 16);

    fireEvent.keyDown(resizer(), { key: "ArrowLeft", shiftKey: true });
    expect(store().sidebarWidth).toBe(start + 16 - 48);
  });

  it("returns to the default width on Home and on a double-click", () => {
    render(<ConfigPanel />);
    store().setSidebarWidth(500, true);

    fireEvent.keyDown(resizer(), { key: "Home" });
    expect(store().sidebarWidth).toBe(SIDEBAR_DEFAULT_WIDTH);

    store().setSidebarWidth(500, true);
    fireEvent.doubleClick(resizer());
    expect(store().sidebarWidth).toBe(SIDEBAR_DEFAULT_WIDTH);
  });

  it("ignores keys it does not handle", () => {
    render(<ConfigPanel />);
    const start = store().sidebarWidth;
    fireEvent.keyDown(resizer(), { key: "a" });
    expect(store().sidebarWidth).toBe(start);
  });

  it("tracks a pointer drag and only commits the released width", () => {
    render(<ConfigPanel />);
    const start = store().sidebarWidth;

    // jsdom's PointerEvent carries no clientX, so the drag is driven with
    // MouseEvents dispatched under the pointer event names.
    fireEvent(resizer(), new MouseEvent("pointerdown", { bubbles: true, clientX: 100 }));
    expect(document.body).toHaveClass("is-resizing");

    fireEvent(window, new MouseEvent("pointermove", { clientX: 140 }));
    expect(store().sidebarWidth).toBe(start + 40);
    expect(localStorage.getItem("ginan-sidebar-width")).toBeNull();

    fireEvent(window, new MouseEvent("pointerup", {}));
    expect(document.body).not.toHaveClass("is-resizing");
    expect(localStorage.getItem("ginan-sidebar-width")).toBe(String(start + 40));
  });
});

/** The empty File card, which is the drop box that browses for a RINEX file. */
const browseZone = () => screen.getByRole("button", { name: /Click to browse for a RINEX file/i });
const queryBrowseZone = () => screen.queryByRole("button", { name: /Click to browse for a RINEX file/i });

describe("the Input section", () => {
  const plusButton = (section: "Observation" | "Corrections") =>
    screen.getByRole("button", { name: `Add to ${section}` });
  const openAddProduct = async (section: "Observation" | "Corrections") => {
    await userEvent.click(plusButton(section));
  };
  const typeItem = (name: string) => screen.getByRole("menuitem", { name });

  it("shows the loaded file and a summary of its header", () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setRinexData(makeRinexData());
    render(<ConfigPanel />);

    expect(screen.getByText("ALIC.rnx")).toBeInTheDocument();
    expect(screen.queryByText("/data/ALIC.rnx")).not.toBeInTheDocument();
    expect(screen.getByText("ALIC00AUS - GPS, GAL")).toBeInTheDocument();
  });

  it("hides the browse zone once a file is loaded and brings it back when the file is removed", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setRinexData(makeRinexData());
    render(<ConfigPanel />);
    expect(screen.queryByRole("button", { name: /Click to browse for a RINEX file/i })).not.toBeInTheDocument();
    expect(screen.getByText("ALIC00AUS - GPS, GAL")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

    expect(screen.getByRole("button", { name: /Click to browse for a RINEX file/i })).toBeInTheDocument();
  });

  it("is the drop box itself when nothing is loaded, filling the file card", () => {
    render(<ConfigPanel />);
    const zone = screen.getByRole("button", { name: /Click to browse for a RINEX file/i });
    const card = zone.closest<HTMLElement>(".file-card")!;
    expect(card).toHaveClass("is-static", "is-empty");
    // The box is the card's first child: no row above it, nothing under it.
    expect(card.firstElementChild).toBe(zone);
    expect(card.querySelector(".file-card-header, .file-card-body")).toBeNull();
    expect(screen.queryByText("No file selected")).not.toBeInTheDocument();
  });

  it("gives Provider, Series and Project the same row: a label and one dropdown", () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(makeCatalogue([{ provider: "COD", series: "RAP", project: "OPS" }]));
    render(<ConfigPanel />);
    for (const label of ["Provider", "Series", "Project"]) {
      const row = screen.getByText(label).closest<HTMLElement>(".field-row")!;
      expect(row.querySelectorAll("select")).toHaveLength(1);
      // Nothing under the dropdown: the reachability note sits by the label.
      expect(row.lastElementChild!.tagName).toBe("SELECT");
    }
  });

  it("shows the Corrections card with the PPP product source inside it", () => {
    render(<ConfigPanel />);
    const card = screen.getByText("Product files").closest<HTMLElement>(".file-card")!;
    // One box like a serial source: the pick on its row, the settings under
    // it in the same box, and no control to open or close it.
    expect(card).toHaveClass("is-static");
    expect(within(card).queryByRole("button", { expanded: true })).toBeNull();
    expect(card.querySelector(".file-card-body")).toContainElement(
      within(card).getByText("Load a RINEX file to see available providers."),
    );
    expect(within(card).getByText("Load a RINEX file to see available providers.")).toBeInTheDocument();
    expect(screen.queryByText(/Products section/)).not.toBeInTheDocument();
  });

  describe("run mode follows the Observation products", () => {
    const dialog = () => screen.getByRole("alertdialog");
    /** Products without their generated ids, for comparing against the defaults. */
    const streams = (list: { type: string; value: string }[]) =>
      list.map(({ type, value }) => ({ type, value }));
    const DEFAULT_OBS = [{ type: "ntrip", value: "ALIC00AUS0" }];
    const DEFAULT_CORR = [
      { type: "ntrip", value: "BCEP00BKG0" },
      { type: "ntrip", value: "SSRA00CNE0" },
    ];

    it("gives both file-run sections a plus button", () => {
      render(<ConfigPanel />);
      expect(plusButton("Observation")).toBeInTheDocument();
      expect(plusButton("Corrections")).toBeInTheDocument();
    });

    it("enables NTRIP, Serial, HTTP and File for Observation in a file run", async () => {
      render(<ConfigPanel />);
      await openAddProduct("Observation");

      expect(typeItem("NTRIP")).toBeEnabled();
      expect(typeItem("Serial")).toBeEnabled();
      expect(typeItem("File")).toBeEnabled();
      expect(typeItem("HTTP")).toBeEnabled();
      // SBAS is a correction source only, so Observation does not list it.
      expect(screen.queryByRole("menuitem", { name: "SBAS" })).toBeNull();
    });

    it("blocks every live type in Corrections in a file run", async () => {
      render(<ConfigPanel />);
      await openAddProduct("Corrections");

      for (const name of ["NTRIP", "Serial", "File", "HTTP", "SBAS"]) {
        expect(typeItem(name), name).toBeDisabled();
      }
      expect(store().inputMode).toBe("file");
    });

    it("asks before a live product replaces the file run, and does not use window.confirm", async () => {
      const confirm = vi.spyOn(window, "confirm");
      store().setRinexPath("/data/ALIC.rnx");
      render(<ConfigPanel />);

      await openAddProduct("Observation");
      await userEvent.click(typeItem("NTRIP"));

      expect(dialog()).toHaveAccessibleName("Switch to a live run?");
      expect(store().inputMode).toBe("file");
      expect(confirm).not.toHaveBeenCalled();
    });

    it("leaves the file run and its products alone when the user cancels", async () => {
      store().setRinexPath("/data/ALIC.rnx");
      render(<ConfigPanel />);
      const before = store().inputProducts;

      await openAddProduct("Observation");
      await userEvent.click(typeItem("NTRIP"));
      await userEvent.click(within(dialog()).getByRole("button", { name: "Cancel" }));

      expect(screen.queryByRole("alertdialog")).not.toBeInTheDocument();
      expect(store().inputMode).toBe("file");
      expect(store().rinexPath).toBe("/data/ALIC.rnx");
      expect(store().inputProducts).toBe(before);
    });

    it("switches to a live run on confirm, with one new stream on Observation", async () => {
      store().setRinexPath("/data/ALIC.rnx");
      store().setRinexData(makeRinexData());
      render(<ConfigPanel />);

      await openAddProduct("Observation");
      await userEvent.click(typeItem("NTRIP"));
      await userEvent.click(within(dialog()).getByRole("button", { name: "Switch to live run" }));

      expect(screen.queryByRole("alertdialog")).not.toBeInTheDocument();
      expect(store().inputMode).toBe("realtime");
      expect(store().rinexPath).toBeNull();
      expect(store().rinexData).toBeNull();
      expect(streams(store().inputProducts.obs)).toEqual([{ type: "ntrip", value: "" }]);
      expect(streams(store().inputProducts.corr)).toEqual(DEFAULT_CORR);
      expect(screen.queryByRole("button", { name: /Click to browse for a RINEX file/i })).not.toBeInTheDocument();
    });

    it("does not block live types in Corrections during a live run", async () => {
      store().setInputMode("realtime");
      render(<ConfigPanel />);

      // A live run adds Corrections from each group's own plus.
      await userEvent.click(screen.getByRole("button", { name: "Add to SSR corrections" }));

      expect(typeItem("NTRIP")).toBeEnabled();
      expect(typeItem("Serial")).toBeEnabled();
      expect(screen.queryByRole("menuitem", { name: "File" })).toBeNull();
    });

    it("asks before a file product replaces the live run", async () => {
      store().setInputMode("realtime");
      render(<ConfigPanel />);
      const before = store().inputProducts;

      await openAddProduct("Observation");
      expect(typeItem("File")).toBeEnabled();
      await userEvent.click(typeItem("File"));

      expect(dialog()).toHaveAccessibleName("Switch to a file run?");
      expect(store().inputMode).toBe("realtime");

      await userEvent.click(within(dialog()).getByRole("button", { name: "Cancel" }));
      expect(screen.queryByRole("alertdialog")).not.toBeInTheDocument();
      expect(store().inputMode).toBe("realtime");
      expect(store().inputProducts).toBe(before);
    });

    it("switches to a file run on confirm", async () => {
      store().setInputMode("realtime");
      render(<ConfigPanel />);

      await openAddProduct("Observation");
      await userEvent.click(typeItem("File"));
      await userEvent.click(within(dialog()).getByRole("button", { name: "Switch to file run" }));

      expect(store().inputMode).toBe("file");
      expect(browseZone()).toBeInTheDocument();
      expect(streams(store().inputProducts.obs)).toEqual(DEFAULT_OBS);
    });

    it("adds a live product in a live run with no dialog", async () => {
      store().setInputMode("realtime");
      render(<ConfigPanel />);
      const before = store().inputProducts.obs.length;

      await openAddProduct("Observation");
      await userEvent.click(typeItem("NTRIP"));

      expect(screen.queryByRole("alertdialog")).not.toBeInTheDocument();
      expect(store().inputMode).toBe("realtime");
      expect(store().inputProducts.obs).toHaveLength(before + 1);
    });
  });

  describe("removing the loaded file", () => {
    it("removes the file from the card's remove button, leaving an empty file run", async () => {
      const confirm = vi.spyOn(window, "confirm");
      store().setRinexPath("/data/ALIC.rnx");
      store().setRinexData(makeRinexData());
      render(<ConfigPanel />);

      await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

      expect(store()).toMatchObject({ inputMode: "file", rinexPath: null, rinexData: null });
      expect(browseZone()).toBeInTheDocument();
      expect(screen.getByRole("button", { name: "Remove this file" })).toBeInTheDocument();
      expect(confirm).not.toHaveBeenCalled();
    });

    it("removes the empty File card from its remove button", async () => {
      render(<ConfigPanel />);

      await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

      expect(store()).toMatchObject({ inputMode: "file", rinexPath: null });
      expect(queryBrowseZone()).not.toBeInTheDocument();
      expect(screen.queryByRole("button", { name: "Remove this file" })).not.toBeInTheDocument();
      await openAddProduct("Observation");
      expect(typeItem("File")).toBeEnabled();
    });

    it("removes the empty File card from the right-click menu", async () => {
      render(<ConfigPanel />);

      fireEvent.contextMenu(browseZone());
      await userEvent.click(screen.getByRole("menuitem", { name: "Remove product" }));

      expect(queryBrowseZone()).not.toBeInTheDocument();
      expect(screen.queryByRole("menu")).not.toBeInTheDocument();
    });

    it("brings the empty File card back from the plus menu", async () => {
      render(<ConfigPanel />);
      await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

      await openAddProduct("Observation");
      await userEvent.click(typeItem("File"));

      // The dialog resolves null by default, so nothing loads and the card stays empty.
      expect(browseZone()).toBeInTheDocument();
      expect(store().rinexPath).toBeNull();
    });

    it("removes the file from the right-click menu on the card header", async () => {
      store().setRinexPath("/data/ALIC.rnx");
      render(<ConfigPanel />);

      fireEvent.contextMenu(screen.getByText("ALIC.rnx"));
      await userEvent.click(screen.getByRole("menuitem", { name: "Remove product" }));

      expect(store()).toMatchObject({ inputMode: "file", rinexPath: null });
      expect(browseZone()).toBeInTheDocument();
      expect(screen.queryByRole("menu")).not.toBeInTheDocument();
    });

    it("gives the Product files card no remove button and no context menu", () => {
      store().setRinexPath("/data/ALIC.rnx");
      render(<ConfigPanel />);
      const card = screen.getByText("Product files").closest<HTMLElement>(".file-card")!;

      expect(within(card).queryByRole("button", { name: "Remove this file" })).not.toBeInTheDocument();
      fireEvent.contextMenu(screen.getByText("Product files"));
      expect(screen.queryByRole("menu")).not.toBeInTheDocument();
    });

    it("offers no removal while a run is processing", () => {
      store().setRinexPath("/data/ALIC.rnx");
      store().setIsProcessing(true);
      render(<ConfigPanel />);

      expect(screen.queryByRole("button", { name: "Remove this file" })).not.toBeInTheDocument();
      fireEvent.contextMenu(screen.getByText("ALIC.rnx"));
      expect(screen.queryByRole("menuitem", { name: "Remove product" })).not.toBeInTheDocument();
    });

    it("offers no removal on the empty card while a run is processing", () => {
      store().setIsProcessing(true);
      render(<ConfigPanel />);

      expect(screen.queryByRole("button", { name: "Remove this file" })).not.toBeInTheDocument();
      fireEvent.contextMenu(browseZone());
      expect(screen.queryByRole("menuitem", { name: "Remove product" })).not.toBeInTheDocument();
    });
  });
});

describe("the General section", () => {
  beforeEach(() => {
    render(<ConfigPanel />);
  });

  it("sets the processing mode", async () => {
    await userEvent.selectOptions(
      within(bodyOf("general")).getByLabelText<HTMLSelectElement>("Processing mode"),
      "Kinematic",
    );
    expect(store().mode).toBe("Kinematic");
  });

  it("offers every processing mode", () => {
    const select = within(bodyOf("general")).getByLabelText<HTMLSelectElement>("Processing mode");
    expect([...select.options].map((o) => o.value)).toEqual([
      "Static",
      "Kinematic",
      "Dynamic",
    ]);
  });

  it("offers only Dynamic while a realtime run has SBAS, and every mode again once it is removed", () => {
    let id = "";
    act(() => {
      store().setMode("Kinematic");
      store().setInputMode("realtime");
      id = store().addProduct("corr", "sbas");
    });
    const select = () => within(bodyOf("general")).getByLabelText<HTMLSelectElement>("Processing mode");
    expect([...select().options].map((o) => o.value)).toEqual(["Dynamic"]);
    expect(select().value).toBe("Dynamic");

    act(() => store().removeProduct("corr", id));
    expect([...select().options].map((o) => o.value)).toEqual(["Static", "Kinematic", "Dynamic"]);
    expect(select().value).toBe("Kinematic");
  });

  it("sets the epoch window from typed GPS times", async () => {
    await userEvent.type(fieldFor("Start Epoch"), "2024-01-02 00:00:00{Enter}");
    expect(store().startEpoch).toBe("2024-01-02T00:00:00");

    await userEvent.type(fieldFor("End Epoch"), "2024-01-02 06:00:30");
    await userEvent.tab();
    expect(store().endEpoch).toBe("2024-01-02T06:00:30");
  });

  it("sets the epoch interval", () => {
    fireEvent.change(fieldFor(/Interval/), { target: { value: "1" } });
    expect(store().epochInterval).toBe(1);
  });

  it("labels the epoch fields as GPS time", () => {
    expect(within(bodyOf("general")).getAllByText("GPST")).toHaveLength(2);
    expect(within(bodyOf("general")).getByRole("img", { name: /Times are GPS time, 18 s ahead of UTC/ })).toBeInTheDocument();
  });
});

describe("the time window against a loaded RINEX file", () => {
  const load = (overrides = {}) =>
    act(() => useAppStore.getState().setRinexData(makeRinexData(overrides)));

  it("is seeded from the file", () => {
    render(<ConfigPanel />);
    load();
    expect(fieldFor<HTMLInputElement>("Start Epoch")).toHaveValue("2024-01-02 00:00:00");
    expect(fieldFor<HTMLInputElement>("End Epoch")).toHaveValue("2024-01-02 23:59:30");
    expect(screen.queryByRole("button", { name: "Use file window" })).not.toBeInTheDocument();
  });

  it("replaces a manual edit when another file is loaded", async () => {
    render(<ConfigPanel />);
    load();
    const start = fieldFor<HTMLInputElement>("Start Epoch");
    await userEvent.clear(start);
    await userEvent.type(start, "2024-01-02 06:00:00{Enter}");
    expect(store().startEpoch).toBe("2024-01-02T06:00:00");

    load({ start_epoch: "2024-03-05 00:00:00", end_epoch: "2024-03-05 12:00:00" });

    expect(start).toHaveValue("2024-03-05 00:00:00");
    expect(store().startEpoch).toBe("2024-03-05T00:00:00");
  });

  it("warns when the window leaves the file and puts it back on request", async () => {
    render(<ConfigPanel />);
    load();
    const end = fieldFor<HTMLInputElement>("End Epoch");
    await userEvent.clear(end);
    await userEvent.type(end, "2024-01-03 06:00:00{Enter}");

    expect(screen.getByText(/Reaches outside the loaded file/)).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Use file window" }));

    expect(store().endEpoch).toBe("2024-01-02T23:59:30");
    expect(end).toHaveValue("2024-01-02 23:59:30");
    expect(screen.queryByText(/Reaches outside the loaded file/)).not.toBeInTheDocument();
  });

  it("flags an end before the start", async () => {
    render(<ConfigPanel />);
    load();
    const end = fieldFor<HTMLInputElement>("End Epoch");
    await userEvent.clear(end);
    await userEvent.type(end, "2024-01-01 23:00:00{Enter}");
    expect(screen.getByRole("alert")).toHaveTextContent("End is before start");
  });

  it("says when the file's epochs were converted from another time system", () => {
    render(<ConfigPanel />);
    load({ time_system: "GLO" });
    expect(screen.getByRole("img", { name: /The file records GLONASS time/ })).toBeInTheDocument();
  });
});

describe("the RINEX metadata block", () => {
  it("is absent until a file is parsed", () => {
    render(<ConfigPanel />);
    expect(screen.queryByText("RINEX Metadata")).not.toBeInTheDocument();
  });
});

describe("the RINEX metadata block, with a file open", () => {
  beforeEach(() => {
    store().setRinexData(makeRinexData());
    render(<ConfigPanel />);
  });

  it("shows the clipped marker read-only", () => {
    const marker = fieldFor<HTMLInputElement>("Marker");
    expect(marker).toHaveValue("ALIC");
    expect(marker).toHaveAttribute("readonly");
  });

  it("edits the receiver and antenna without trimming whitespace", () => {
    fireEvent.change(fieldFor("Receiver"), { target: { value: "  SEPT POLARX5  " } });
    fireEvent.change(fieldFor("Antenna"), { target: { value: "LEIAR25.R3      NONE" } });

    expect(store().receiverType).toBe("  SEPT POLARX5  ");
    expect(store().antennaType).toBe("LEIAR25.R3      NONE");
  });

  it("edits one antenna offset axis at a time", () => {
    const cells = document.querySelectorAll<HTMLInputElement>(".offset-cell input");
    fireEvent.change(cells[1], { target: { value: "1.5" } });
    expect(store().antennaOffset).toEqual([0.0015, 1.5, 0]);
  });

  it("treats an unparseable offset as zero rather than NaN", () => {
    const cells = document.querySelectorAll<HTMLInputElement>(".offset-cell input");
    fireEvent.change(cells[0], { target: { value: "" } });
    expect(store().antennaOffset[0]).toBe(0);
  });

  it("shows the a priori position read-only, to millimetres", () => {
    const input = fieldFor<HTMLInputElement>(/Apriori Position/);
    expect(input).toHaveValue("-4052052.700, 4212836.000, -2545105.000");
    expect(input).toHaveAttribute("readonly");
  });
});

describe("the PPP product source (Corrections card, file run)", () => {
  it("waits for a file before offering providers", async () => {
    render(<ConfigPanel />);
    expect(
      screen.getByText("Load a RINEX file to see available providers."),
    ).toBeInTheDocument();
  });

  it("says it is scanning while the catalogue is still null", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    render(<ConfigPanel />);
    expect(screen.getByText(/Scanning CDDIS/)).toBeInTheDocument();
  });

  it("says so when the window has no products at all", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue({ entries: [] });
    render(<ConfigPanel />);
    expect(screen.getByText(/No PPP products available/)).toBeInTheDocument();
  });

  it("lists the providers, series and projects the catalogue carries", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "IGS", series: "RAP", project: "OPS" },
        { provider: "COD", series: "FIN", project: "MGX" },
      ]),
    );
    render(<ConfigPanel />);

    expect([...fieldFor<HTMLSelectElement>("Provider").options].map((o) => o.value)).toEqual([
      "COD",
      "IGS",
    ]);
    expect([...fieldFor<HTMLSelectElement>("Series").options].map((o) => o.value)).toEqual([
      "FIN",
      "RAP",
    ]);
  });

  it("spells out what each series and project code means", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(makeCatalogue([{}]));
    render(<ConfigPanel />);

    expect(screen.getByRole("option", { name: "FIN · Final" })).toBeInTheDocument();
    expect(screen.getByRole("option", { name: "OPS · Operational" })).toBeInTheDocument();
  });

  it("changes the selection through the store's cascade", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(
      makeCatalogue([
        { provider: "IGS", series: "FIN", project: "OPS" },
        { provider: "COD", series: "RAP", project: "MGX" },
      ]),
    );
    render(<ConfigPanel />);

    await userEvent.selectOptions(fieldFor<HTMLSelectElement>("Provider"), "COD");

    expect(store().pppSelection).toEqual({
      provider: "COD",
      series: "RAP",
      project: "MGX",
    });
  });

  it("probes provider health and reports the current one", async () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(makeCatalogue([{}]));
    render(<ConfigPanel />);

    expect(await screen.findByText("IGS is reachable")).toBeInTheDocument();
  });

  it("reports an unreachable provider", async () => {
    vi.spyOn(Date, "now").mockReturnValue(1_700_000_000_000);
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(makeCatalogue([{}]));
    mockInvoke({ check_ppp_provider_health: () => Promise.reject(new Error("timeout")) });
    render(<ConfigPanel />);

    expect(await screen.findByText("IGS is unreachable")).toBeInTheDocument();
  });

  it("fills the Corrections card when a file and its catalogue load, and shows the hint again when the file is removed", async () => {
    render(<ConfigPanel />);
    expect(screen.getByText("Load a RINEX file to see available providers.")).toBeInTheDocument();

    act(() => {
      store().setRinexPath("/data/ALIC.rnx");
      store().setRinexData(makeRinexData());
    });
    expect(screen.getByText(/Scanning CDDIS/)).toBeInTheDocument();

    act(() => store().setPppCatalogue(makeCatalogue([{ provider: "COD", series: "RAP", project: "OPS" }])));
    expect(fieldFor<HTMLSelectElement>("Provider").value).toBe("COD");
    expect(screen.getByText("COD · RAP")).toBeInTheDocument();

    // Remove sits on the file row itself.
    await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

    expect(screen.getByText("Load a RINEX file to see available providers.")).toBeInTheDocument();
    expect(screen.getByText("Product files")).toBeInTheDocument();
  });

  it("names the Corrections card after the current pick", () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(makeCatalogue([{ provider: "COD", series: "RAP", project: "OPS" }]));
    render(<ConfigPanel />);

    expect(screen.getByText("COD · RAP")).toBeInTheDocument();
  });

  it("renders in the Corrections card, and there is no Products section", () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setPppCatalogue(makeCatalogue([{}]));
    render(<ConfigPanel />);

    const card = screen.getByText("IGS · FIN").closest<HTMLElement>(".file-card")!;
    expect(within(card).getByText("Provider")).toBeInTheDocument();
    expect(card.closest("[data-tour='ppp-source']")).not.toBeNull();
    expect(screen.queryByRole("button", { name: /^Products/ })).not.toBeInTheDocument();
    expect(document.querySelector('[data-tour="section-products"]')).toBeNull();
  });
});

describe("the Output section", () => {
  beforeEach(async () => {
    render(<ConfigPanel />);
    await openSection("Output");
  });

  it("has no output directory control; that lives in Settings", () => {
    expect(screen.queryByRole("button", { name: "Settings..." })).not.toBeInTheDocument();
    expect(screen.getByLabelText("Run name")).toBeInTheDocument();
  });

  it("shows the recordings only for a realtime run", () => {
    expect(screen.queryByRole("checkbox", { name: "Raw RTCM bytes" })).not.toBeInTheDocument();
  });

  it("offers the output formats, with .POS on by default", () => {
    expect(screen.getByRole("checkbox", { name: ".POS" })).toBeChecked();
    for (const label of [".GPX", ".TRACE", ".SNX"]) {
      expect(screen.getByRole("checkbox", { name: label })).not.toBeChecked();
    }
  });

  it("toggles each format independently", async () => {
    await userEvent.click(screen.getByRole("checkbox", { name: ".GPX" }));
    await userEvent.click(screen.getByRole("checkbox", { name: ".POS" }));

    expect(store()).toMatchObject({ outputGpx: true, outputPos: false, outputSnx: false });
  });

  it("offers .SPP only while a live run uses SBAS, on by default", async () => {
    const spp = () => screen.getByRole("checkbox", { name: ".SPP" });
    expect(spp()).toBeDisabled();
    expect(spp()).not.toBeChecked();

    let id = "";
    act(() => {
      store().setInputMode("realtime");
      id = store().addProduct("corr", "sbas");
    });
    expect(spp()).toBeEnabled();
    expect(spp()).toBeChecked();
    await userEvent.click(spp());
    expect(store().outputSpp).toBe(false);

    act(() => store().removeProduct("corr", id));
    expect(spp()).toBeDisabled();
  });
});

describe("the Output section, realtime", () => {
  beforeEach(async () => {
    store().setInputMode("realtime");
    render(<ConfigPanel />);
    await openSection("Output");
  });

  it("records raw and decoded RTCM by default, not RINEX", () => {
    expect(screen.getByRole("checkbox", { name: "Raw RTCM bytes" })).toBeChecked();
    expect(screen.getByRole("checkbox", { name: /^Decoded RTCM \(JSON\)/ })).toBeChecked();
    expect(screen.getByRole("checkbox", { name: "Observations as RINEX" })).not.toBeChecked();
  });

  it("toggles the recording options and the exit guard", async () => {
    await userEvent.click(screen.getByRole("checkbox", { name: "Observations as RINEX" }));
    await userEvent.click(screen.getByRole("checkbox", { name: "Raw RTCM bytes" }));
    await userEvent.click(screen.getByRole("checkbox", { name: /Exit if no observation source/ }));
    expect(store().realtime).toMatchObject({
      record_rinex: true,
      record_rtcm: false,
      require_obs: true,
    });
  });
});

describe("the Constellations section", () => {
  it("hands off to the constellation filter", async () => {
    render(<ConfigPanel />);
    await openSection("Constellations");
    await waitFor(() =>
      expect(screen.getByText(/Load a RINEX file to see observation codes/)).toBeInTheDocument(),
    );
  });
});
