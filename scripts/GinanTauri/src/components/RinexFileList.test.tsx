import { beforeEach, describe, expect, it } from "vitest";
import { act, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ConfigPanel from "./ConfigPanel";
import WelcomeScreen from "./WelcomeScreen";
import RinexFileList from "./RinexFileList";
import { makeCatalogue, makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { emitDragDrop, invokeCount, mockInvoke, open } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

const A = "/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx";
const B = "/data/ALIC00AUS_R_20240030000_01D_30S_MO.rnx";
const C = "/data/DARW00AUS_R_20240040000_01D_30S_MO.rnx";

function dayOf(d: number, marker = "ALIC00AUS") {
  const dd = String(d).padStart(2, "0");
  return makeRinexData({
    marker_name: marker,
    start_epoch: `2024-01-${dd} 00:00:00`,
    end_epoch: `2024-01-${dd} 23:59:30`,
  });
}

const HEADERS: Record<string, ReturnType<typeof dayOf>> = {
  [A]: dayOf(2),
  [B]: dayOf(3),
  [C]: dayOf(4, "DARW00AUS"),
};

function loadPrimary() {
  store().setRinexPath(A);
  store().setRinexData(HEADERS[A]);
}

function mockFiles() {
  mockInvoke({
    prepare_rinex_input: (a) => a.path,
    parse_rinex: (a) => HEADERS[a.path as string],
    validate_antenna: true,
    scan_ppp_catalogue: makeCatalogue([{}]),
  });
}

describe("RinexFileList", () => {
  it("renders nothing before a file has been read", () => {
    const { container } = render(<RinexFileList />);
    expect(container).toBeEmptyDOMElement();
  });

  it("offers only the add button while there is a single file", () => {
    loadPrimary();
    render(<RinexFileList />);

    expect(screen.queryByRole("list", { name: "Selected RINEX files" })).toBeNull();
    expect(screen.getByRole("button", { name: /Add more files/ })).toBeEnabled();
  });

  it("lists every selected file, naming the primary", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    store().addRinexFile({ path: C, data: HEADERS[C] });
    render(<RinexFileList />);

    const items = within(screen.getByRole("list", { name: "Selected RINEX files" })).getAllByRole(
      "listitem",
    );
    expect(items).toHaveLength(3);
    expect(items[0]).toHaveTextContent("ALIC00AUS_R_20240020000_01D_30S_MO.rnx");
    expect(items[0]).toHaveTextContent("Primary");
    expect(items[0]).toHaveTextContent("2024-01-02");
    expect(items[1]).toHaveTextContent("ALIC00AUS_R_20240030000_01D_30S_MO.rnx");
    expect(items[1]).not.toHaveTextContent("Primary");
    expect(items[2]).toHaveTextContent("DARW00AUS");
  });

  it("removes one file from the list when its button is pressed", async () => {
    mockFiles();
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    store().addRinexFile({ path: C, data: HEADERS[C] });
    render(<RinexFileList />);

    await userEvent.click(
      screen.getByRole("button", { name: "Remove ALIC00AUS_R_20240030000_01D_30S_MO.rnx" }),
    );

    await waitFor(() => expect(store().extraRinex.map((f) => f.path)).toEqual([C]));
    expect(store().rinexPath).toBe(A);
  });

  it("lets the primary be removed too, promoting the next file", async () => {
    mockFiles();
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    render(<RinexFileList />);

    await userEvent.click(
      screen.getByRole("button", { name: "Remove ALIC00AUS_R_20240020000_01D_30S_MO.rnx" }),
    );

    await waitFor(() => expect(store().rinexPath).toBe(B));
    expect(store().extraRinex).toEqual([]);
  });

  it("notes when the files come from different stations", () => {
    loadPrimary();
    store().addRinexFile({ path: C, data: HEADERS[C] });
    render(<RinexFileList />);

    expect(screen.getByRole("status")).toHaveTextContent(/different stations \(ALIC, DARW\)/);
  });

  it("stays quiet about stations when every file is the same one", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    render(<RinexFileList />);

    expect(screen.queryByRole("status")).toBeNull();
  });

  it("offers a zone to add more files by click or drop", () => {
    loadPrimary();
    render(<RinexFileList />);

    expect(screen.getByRole("button", { name: /Add more files\.\.\. or drop them here/ })).toHaveClass(
      "file-drop-zone",
    );
  });

  it("says what the first file controls and what spans all of them", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    render(<RinexFileList />);

    expect(screen.getByLabelText(/The first file sets the marker, receiver, antenna/)).toBeInTheDocument();
  });

  it("will not change the selection while a run is in progress", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    store().setIsProcessing(true);
    render(<RinexFileList />);

    for (const button of screen.getAllByRole("button")) expect(button).toBeDisabled();
  });

  it("adds the files the dialog returns", async () => {
    mockFiles();
    loadPrimary();
    open.mockResolvedValueOnce([B, C]);
    render(<RinexFileList />);

    await userEvent.click(screen.getByRole("button", { name: /Add more files/ }));

    await waitFor(() => expect(store().extraRinex.map((f) => f.path)).toEqual([B, C]));
  });
});

describe("the sidebar Input section", () => {
  it("names one file on its card, with no list, and offers more", () => {
    loadPrimary();
    render(<ConfigPanel />);

    // The File card shows the file name, not the full path.
    expect(screen.getByText("ALIC00AUS_R_20240020000_01D_30S_MO.rnx")).toBeInTheDocument();
    expect(screen.queryByRole("list", { name: "Selected RINEX files" })).toBeNull();
    expect(screen.queryByRole("button", { name: /Replace all files/ })).toBeNull();
    expect(screen.getByRole("button", { name: /Add more files/ })).toBeInTheDocument();
  });

  it("names the primary and counts the rest on the card from the second file on", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    render(<ConfigPanel />);

    expect(screen.getByText("ALIC00AUS_R_20240020000_01D_30S_MO.rnx + 1 more")).toBeInTheDocument();
  });

  it("clears every file, not only the primary, from the File card's remove button", async () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    render(<ConfigPanel />);

    await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

    expect(store().rinexPath).toBeNull();
    expect(store().extraRinex).toEqual([]);
  });

  it("swaps the path for the list from the second file on", () => {
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    render(<ConfigPanel />);

    expect(screen.getByRole("list", { name: "Selected RINEX files" })).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: "Browse..." })).toBeNull();
    expect(screen.getByRole("button", { name: /Replace all files/ })).toBeInTheDocument();
  });

  it("replaces the whole selection from Replace all files", async () => {
    mockFiles();
    loadPrimary();
    store().addRinexFile({ path: B, data: HEADERS[B] });
    open.mockResolvedValueOnce(C);
    render(<ConfigPanel />);

    await userEvent.click(screen.getByRole("button", { name: /Replace all files/ }));

    await waitFor(() => expect(store().rinexPath).toBe(C));
    expect(store().extraRinex).toEqual([]);
  });
});

describe("dropping several files", () => {
  async function renderWelcome() {
    mockFiles();
    render(<WelcomeScreen />);
    await waitFor(() => expect(document.querySelector(".welcome")).not.toBeNull());
  }

  it("opens the first as primary and the rest alongside it", async () => {
    await renderWelcome();

    act(() =>
      emitDragDrop({ type: "drop", paths: [A, B, C], position: { x: 0, y: 0 } }),
    );

    await waitFor(() => expect(store().extraRinex).toHaveLength(2));
    expect(store().rinexPath).toBe(A);
    expect(store().extraRinex.map((f) => f.path)).toEqual([B, C]);
  });

  it("leaves a non-RINEX file out of the batch and logs that it did", async () => {
    await renderWelcome();

    act(() =>
      emitDragDrop({
        type: "drop",
        paths: [A, "/data/notes.txt", B],
        position: { x: 0, y: 0 },
      }),
    );

    await waitFor(() => expect(store().extraRinex).toHaveLength(1));
    expect(invokeCount("prepare_rinex_input")).toBe(2);
    expect(store().workflowLines.join("\n")).toMatch(/skipped: \/data\/notes\.txt/);
  });
});
