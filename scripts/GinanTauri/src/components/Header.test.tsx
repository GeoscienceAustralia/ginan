import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Header, { AppMark } from "./Header";
import { useAppStore } from "../store/appStore";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { getVersion } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  // RunControls reads the binary path off the store; nothing in these tests
  // presses Process, so a stub keeps its disabled reason stable.
  useAppStore.setState({ peaBinaryPath: "/bin/pea" });
});

const withFile = () => {
  store().setRinexPath("/data/file.rnx");
  store().setRinexData(makeRinexData());
};

describe("Header with nothing open", () => {
  it("keeps settings and the tour, but not the run controls", () => {
    render(<Header />);
    expect(screen.getByRole("button", { name: /Settings/ })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Guided tour of the app" })).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /Process/ })).not.toBeInTheDocument();
  });

  it("marks the bar as the window's drag region", () => {
    useAppStore.setState({ sidebarCollapsed: true });
    const { container } = render(<Header />);
    expect(container.querySelector(".header")).toHaveAttribute("data-tauri-drag-region");
    expect(screen.getByRole("heading", { name: "Ginan UI" })).toHaveAttribute(
      "data-tauri-drag-region",
    );
    expect(screen.getByRole("button", { name: /Settings/ })).not.toHaveAttribute(
      "data-tauri-drag-region",
    );
  });

  it("starts the welcome tour, the one whose targets exist", async () => {
    render(<Header />);
    await userEvent.click(screen.getByRole("button", { name: "Guided tour of the app" }));
    expect(store().activeTour).toBe("welcome");
  });

  it("opens settings without changing the page", async () => {
    store().setSettingsPage("appearance");
    render(<Header />);

    await userEvent.click(screen.getByRole("button", { name: /Settings/ }));

    expect(store()).toMatchObject({ settingsOpen: true, settingsPage: "appearance" });
  });
});

describe("Header with a file open", () => {
  beforeEach(withFile);

  it("shows the run controls", () => {
    render(<Header />);
    expect(screen.getByRole("button", { name: /Process/ })).toBeInTheDocument();
    // Stop only takes Process's place once a run starts.
    expect(screen.queryByRole("button", { name: /Stop/ })).not.toBeInTheDocument();
  });

  it("toggles the rail and reports its state to assistive technology", async () => {
    render(<AppMark />);
    const toggle = screen.getByRole("button", { name: "Hide sidebar" });
    expect(toggle).toHaveAttribute("aria-expanded", "true");
    expect(toggle).toHaveAttribute("aria-controls", "config-panel");

    await userEvent.click(toggle);

    expect(store().sidebarCollapsed).toBe(true);
    expect(screen.getByRole("button", { name: "Show sidebar" })).toHaveAttribute(
      "aria-expanded",
      "false",
    );
  });

  it("starts the workspace tour once the shell is up", async () => {
    render(<Header />);
    await userEvent.click(screen.getByRole("button", { name: "Guided tour of the app" }));
    expect(store().activeTour).toBe("workspace");
  });

  it("starts the streaming tour in realtime mode", async () => {
    store().setInputMode("realtime");
    render(<Header />);
    await userEvent.click(screen.getByRole("button", { name: "Guided tour of the app" }));
    expect(store().activeTour).toBe("streaming");
  });
});

describe("app mark placement", () => {
  it("leaves the mark and toggle to the sidebar while it is open", () => {
    render(<Header />);
    expect(screen.queryByRole("heading", { name: "Ginan UI" })).not.toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /sidebar/ })).not.toBeInTheDocument();
  });

  it("carries the mark and the show-sidebar button once the sidebar is hidden", async () => {
    useAppStore.setState({ sidebarCollapsed: true });
    render(<Header />);
    expect(screen.getByRole("heading", { name: "Ginan UI" })).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Show sidebar" }));

    expect(store().sidebarCollapsed).toBe(false);
  });

  it("puts the tabs it is given on the bar", () => {
    render(
      <Header>
        <div role="tablist" aria-label="Workspace" />
      </Header>,
    );
    expect(screen.getByRole("tablist", { name: "Workspace" }).closest(".header")).not.toBeNull();
  });
});

describe("after leaving realtime mode with no file", () => {
  it("keeps the shell controls, because the user has already entered the workspace", () => {
    store().setInputMode("realtime");
    store().setInputMode("file");
    expect(store().rinexData).toBeNull();
    render(<Header />);
    expect(screen.getByRole("button", { name: /Process/ })).toBeInTheDocument();
  });

  it("carries no input-mode switch", () => {
    withFile();
    render(<Header />);
    expect(screen.queryByRole("button", { name: /Switch to/ })).not.toBeInTheDocument();
  });
});

describe("Header version", () => {
  // The version sits beside the name in the app mark (sidebar top, or the
  // Header once the sidebar is hidden).
  it("shows the installed build's version beside the name", async () => {
    getVersion.mockResolvedValue("1.2.3");
    render(<AppMark />);
    expect(await screen.findByText("v1.2.3")).toBeInTheDocument();
  });

  it("falls back to the package version outside Tauri", async () => {
    getVersion.mockRejectedValue(new Error("not in Tauri"));
    const { version } = await import("../../package.json");
    render(<AppMark />);
    await waitFor(() => expect(getVersion).toHaveBeenCalled());
    expect(screen.getByText(`v${version}`)).toBeInTheDocument();
  });

  it("drags the window like the name it sits beside", async () => {
    getVersion.mockResolvedValue("1.2.3");
    render(<AppMark />);
    expect(await screen.findByText("v1.2.3")).toHaveAttribute("data-tauri-drag-region");
  });
});
