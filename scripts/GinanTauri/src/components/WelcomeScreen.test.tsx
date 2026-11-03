import { beforeEach, describe, expect, it } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import WelcomeScreen from "./WelcomeScreen";
import { resetAppStore, store } from "../test/storeHelpers";
import {
  emitDragDrop,
  invokeCount,
  lastInvokeArgs,
  mockInvoke,
  open,
} from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

describe("WelcomeScreen", () => {
  it("offers the two ways to start a run", () => {
    render(<WelcomeScreen />);
    expect(
      screen.getByRole("button", { name: /Add RINEX Observation File/ }),
    ).toBeInTheDocument();
    expect(
      screen.getByRole("button", { name: /Setup Real-time Processing/ }),
    ).toBeInTheDocument();
  });

  it("switches to realtime without needing a file", async () => {
    render(<WelcomeScreen />);
    await userEvent.click(screen.getByRole("button", { name: /Setup Real-time Processing/ }));
    expect(store().inputMode).toBe("realtime");
  });

  it("opens the file dialog and does nothing when it is cancelled", async () => {
    open.mockResolvedValueOnce(null);
    render(<WelcomeScreen />);

    await userEvent.click(screen.getByRole("button", { name: /Add RINEX Observation File/ }));

    await waitFor(() => expect(open).toHaveBeenCalled());
    expect(invokeCount("prepare_rinex_input")).toBe(0);
  });

  it("loads the file the dialog returned", async () => {
    open.mockResolvedValueOnce("/data/ALIC.rnx");
    mockInvoke({
      prepare_rinex_input: (a) => a.path,
      parse_rinex: () => Promise.reject(new Error("stop here")),
    });
    render(<WelcomeScreen />);

    await userEvent.click(screen.getByRole("button", { name: /Add RINEX Observation File/ }));

    await waitFor(() =>
      expect(lastInvokeArgs("prepare_rinex_input")).toEqual({ path: "/data/ALIC.rnx" }),
    );
  });

  it("shows a load failure rather than leaving the screen silent", () => {
    store().setRinexPath("/data/ALIC.rnx");
    store().setStatus("Parse error: bad header");
    render(<WelcomeScreen />);
    expect(screen.getByText("Parse error: bad header")).toBeInTheDocument();
  });

  it("stays quiet before any file has been chosen", () => {
    store().setStatus("Ready");
    const { container } = render(<WelcomeScreen />);
    expect(container.querySelector(".welcome-note")).toBeNull();
  });

  it("carries the tour anchor the welcome tour points at", () => {
    const { container } = render(<WelcomeScreen />);
    expect(container.querySelector('[data-tour="add-rinex"]')).not.toBeNull();
  });
});

describe("drag and drop", () => {
  it("highlights while a file is over the window", async () => {
    const { container } = render(<WelcomeScreen />);
    await waitFor(() => expect(container.querySelector(".welcome")).not.toBeNull());

    act(() => emitDragDrop({ type: "over", paths: [], position: { x: 0, y: 0 } }));

    expect(container.querySelector(".welcome-dragging")).not.toBeNull();
    expect(screen.getByText("Drop to open")).toBeInTheDocument();
  });

  it("drops the highlight when the file leaves again", async () => {
    const { container } = render(<WelcomeScreen />);
    await waitFor(() => expect(container.querySelector(".welcome")).not.toBeNull());

    act(() => emitDragDrop({ type: "enter", paths: [], position: { x: 0, y: 0 } }));
    act(() => emitDragDrop({ type: "leave" }));

    expect(container.querySelector(".welcome-dragging")).toBeNull();
  });

  it("loads a dropped RINEX file", async () => {
    mockInvoke({
      prepare_rinex_input: (a) => a.path,
      parse_rinex: () => Promise.reject(new Error("stop here")),
    });
    render(<WelcomeScreen />);
    await waitFor(() => expect(document.querySelector(".welcome")).not.toBeNull());

    act(() =>
      emitDragDrop({ type: "drop", paths: ["/data/ALIC.23o"], position: { x: 0, y: 0 } }),
    );

    await waitFor(() =>
      expect(lastInvokeArgs("prepare_rinex_input")).toEqual({ path: "/data/ALIC.23o" }),
    );
  });

  it("rejects a file that is not a RINEX observation, naming it", async () => {
    render(<WelcomeScreen />);
    await waitFor(() => expect(document.querySelector(".welcome")).not.toBeNull());

    act(() =>
      emitDragDrop({ type: "drop", paths: ["/data/notes.txt"], position: { x: 0, y: 0 } }),
    );

    await waitFor(() =>
      expect(store().status).toBe("Not a RINEX observation file: notes.txt"),
    );
    expect(invokeCount("prepare_rinex_input")).toBe(0);
  });

  it("opens the first usable file of a multi-file drop and says which", async () => {
    mockInvoke({
      prepare_rinex_input: (a) => a.path,
      parse_rinex: () => Promise.reject(new Error("stop here")),
    });
    render(<WelcomeScreen />);
    await waitFor(() => expect(document.querySelector(".welcome")).not.toBeNull());

    act(() =>
      emitDragDrop({
        type: "drop",
        paths: ["/data/readme.md", "/data/ALIC.rnx"],
        position: { x: 0, y: 0 },
      }),
    );

    await waitFor(() =>
      expect(lastInvokeArgs("prepare_rinex_input")).toEqual({ path: "/data/ALIC.rnx" }),
    );
  });

  it("ignores an empty drop", async () => {
    render(<WelcomeScreen />);
    await waitFor(() => expect(document.querySelector(".welcome")).not.toBeNull());

    act(() => emitDragDrop({ type: "drop", paths: [], position: { x: 0, y: 0 } }));

    expect(invokeCount("prepare_rinex_input")).toBe(0);
  });

  it("refuses to swap the file out from under a running PEA process", async () => {
    store().setIsProcessing(true);
    render(<WelcomeScreen />);
    await waitFor(() => expect(document.querySelector(".welcome")).not.toBeNull());

    act(() =>
      emitDragDrop({ type: "drop", paths: ["/data/ALIC.rnx"], position: { x: 0, y: 0 } }),
    );

    expect(invokeCount("prepare_rinex_input")).toBe(0);
  });
});
