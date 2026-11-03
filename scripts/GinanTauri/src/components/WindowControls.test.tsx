import { afterEach, describe, expect, it, vi } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import WindowControls from "./WindowControls";
import type { TitlebarStyle } from "../hooks/useWindowChrome";
import * as mocks from "../test/tauriMocks";

/**
 * The real `titlebarStyle` is fixed at module load from the user agent and
 * `isTauri()` (both covered in useWindowChrome.test.ts). Here it is swapped for
 * a getter so each test can pick the platform without re-importing modules.
 */
const chrome = vi.hoisted(() => ({ style: "native" as TitlebarStyle }));
vi.mock("../hooks/useWindowChrome", async (importOriginal) => {
  const actual = await importOriginal<typeof import("../hooks/useWindowChrome")>();
  return {
    ...actual,
    get titlebarStyle() {
      return chrome.style;
    },
  };
});

/** Windows/Linux in Tauri: undecorated, so the app draws the buttons. */
function asWindows() {
  chrome.style = "custom";
  mocks.isTauri.mockReturnValue(true);
}

afterEach(() => {
  document.documentElement.removeAttribute("data-window-fullscreen");
  chrome.style = "native";
});

describe("WindowControls on Windows", () => {
  it("draws minimise, maximise and close, each driving the window", async () => {
    asWindows();
    render(<WindowControls />);

    const group = screen.getByRole("group", { name: "Window controls" });
    expect(group.querySelectorAll("button")).toHaveLength(3);

    await userEvent.click(screen.getByRole("button", { name: "Minimize" }));
    await userEvent.click(screen.getByRole("button", { name: "Maximize" }));
    await userEvent.click(screen.getByRole("button", { name: "Close" }));

    expect(mocks.minimize).toHaveBeenCalledTimes(1);
    expect(mocks.toggleMaximize).toHaveBeenCalledTimes(1);
    expect(mocks.close).toHaveBeenCalledTimes(1);
  });

  it("offers Restore once the window reports it is maximised", async () => {
    asWindows();
    render(<WindowControls />);
    expect(screen.getByRole("button", { name: "Maximize" })).toBeInTheDocument();

    mocks.isMaximized.mockResolvedValue(true);
    act(() => mocks.emitResized());

    expect(await screen.findByRole("button", { name: "Restore" })).toBeInTheDocument();
  });

  it("steps aside in fullscreen, where the OS hides its own buttons", async () => {
    asWindows();
    mocks.isFullscreen.mockResolvedValue(true);
    render(<WindowControls />);

    await waitFor(() =>
      expect(document.documentElement).toHaveAttribute("data-window-fullscreen"),
    );
    expect(screen.queryByRole("group", { name: "Window controls" })).not.toBeInTheDocument();
  });

  it("dims while the window is in the background", async () => {
    asWindows();
    render(<WindowControls />);
    const group = screen.getByRole("group", { name: "Window controls" });

    act(() => void window.dispatchEvent(new Event("blur")));
    expect(group).toHaveClass("is-inactive");
    act(() => void window.dispatchEvent(new Event("focus")));
    expect(group).not.toHaveClass("is-inactive");
  });
});

describe("WindowControls elsewhere", () => {
  it("draws nothing on macOS, where the native traffic lights stay", () => {
    chrome.style = "overlay";
    mocks.isTauri.mockReturnValue(true);
    const { container } = render(<WindowControls />);
    expect(container).toBeEmptyDOMElement();
  });

  it("draws nothing in a plain browser, and never asks for a window", () => {
    chrome.style = "native";
    const { container } = render(<WindowControls />);
    expect(container).toBeEmptyDOMElement();
    expect(mocks.getCurrentWindow).not.toHaveBeenCalled();
  });
});
