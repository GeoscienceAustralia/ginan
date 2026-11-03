import { beforeEach, describe, expect, it } from "vitest";
import { act, render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Workspace from "./Workspace";
import { useAppStore } from "../store/appStore";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

describe("Workspace before anything is open", () => {
  it("shows the start page in place of the tabs on a fresh launch", () => {
    render(<Workspace />);
    expect(screen.getByRole("heading", { name: "No observation file open" })).toBeInTheDocument();
    expect(screen.queryByRole("tablist")).not.toBeInTheDocument();
  });

  it("swaps the start page for the tabs once a file is loaded", () => {
    render(<Workspace />);
    act(() => store().setRinexData(makeRinexData()));
    expect(screen.getByRole("tablist", { name: "Workspace" })).toBeInTheDocument();
    expect(screen.queryByText("No observation file open")).not.toBeInTheDocument();
  });

  it("goes straight to the tabs in realtime mode", () => {
    store().setInputMode("realtime");
    render(<Workspace />);
    expect(screen.getByRole("tablist", { name: "Workspace" })).toBeInTheDocument();
  });

  it("keeps the tabs when switching back to file mode with no file loaded", () => {
    store().setInputMode("realtime");
    render(<Workspace />);
    act(() => store().setInputMode("file"));
    expect(store().rinexData).toBeNull();
    expect(screen.getByRole("tablist", { name: "Workspace" })).toBeInTheDocument();
  });
});

describe("Workspace", () => {
  // Past the start page: the tabs only appear once a file or realtime has been
  // entered, which is what every test below is about.
  beforeEach(() => {
    useAppStore.setState({ hasEnteredWorkspace: true });
  });

  it("exposes the tabs as an accessible tablist", () => {
    useAppStore.setState({ inputMode: "realtime" });
    render(<Workspace />);
    expect(screen.getByRole("tablist", { name: "Workspace" })).toBeInTheDocument();
    expect(screen.getAllByRole("tab").map((t) => t.textContent)).toEqual([
      "Workflow",
      "Console",
      "Streams",
      "Visualisation",
    ]);
  });

  it("shows a tab another part of the app asks for, then clears the request", () => {
    useAppStore.setState({ inputMode: "realtime" });
    render(<Workspace />);
    act(() => store().requestWorkspaceTab("streams"));
    expect(screen.getByRole("tab", { name: "Streams" })).toHaveAttribute("aria-selected", "true");
    expect(store().workspaceTabRequest).toBeNull();
  });

  it("hides Streams for a file run", () => {
    useAppStore.setState({ inputMode: "file" });
    render(<Workspace />);
    expect(screen.queryByRole("tab", { name: "Streams" })).not.toBeInTheDocument();
  });

  it("opens on Workflow for a file run", () => {
    render(<Workspace />);
    expect(screen.getByRole("tab", { name: "Workflow" })).toHaveAttribute(
      "aria-selected",
      "true",
    );
    expect(screen.getByRole("region", { name: "Workflow" })).toBeInTheDocument();
  });

  it("opens on Workflow in realtime too, with Streams one click away", async () => {
    store().setInputMode("realtime");
    render(<Workspace />);
    expect(screen.getByRole("tab", { name: "Workflow" })).toHaveAttribute(
      "aria-selected",
      "true",
    );

    await userEvent.click(screen.getByRole("tab", { name: "Streams" }));

    expect(screen.getByRole("tab", { name: "Streams" })).toHaveAttribute("aria-selected", "true");
    expect(screen.getByRole("tabpanel")).toHaveAttribute("aria-labelledby", "tab-streams");
  });

  it("switches panels when a tab is clicked", async () => {
    store().appendLine("raw pea output");
    render(<Workspace />);

    await userEvent.click(screen.getByRole("tab", { name: "Console" }));

    expect(screen.getByRole("tab", { name: "Console" })).toHaveAttribute(
      "aria-selected",
      "true",
    );
    expect(screen.getByText("raw pea output")).toBeInTheDocument();
  });

  it("keeps each channel's lines across a tab switch", async () => {
    store().appendLine("console line");
    store().appendWorkflowLine("workflow line");
    render(<Workspace />);

    await userEvent.click(screen.getByRole("tab", { name: "Console" }));
    await userEvent.click(screen.getByRole("tab", { name: "Workflow" }));

    expect(screen.getByText("workflow line")).toBeInTheDocument();
  });

  it("wires each tab to its panel for assistive technology", async () => {
    render(<Workspace />);
    const tab = screen.getByRole("tab", { name: "Console" });
    await userEvent.click(tab);

    const panel = screen.getByRole("tabpanel");
    expect(tab).toHaveAttribute("aria-controls", "tabpanel-console");
    expect(panel).toHaveAttribute("id", "tabpanel-console");
    expect(panel).toHaveAttribute("aria-labelledby", "tab-console");
  });

  it("carries the per-tab tour anchors", () => {
    // The Streams anchor only exists in realtime, which is when the streaming tour runs.
    useAppStore.setState({ inputMode: "realtime" });
    const { container } = render(<Workspace />);
    expect(container.querySelector('[data-tour="tabbar"]')).not.toBeNull();
    expect(container.querySelector('[data-tour="tab-streams"]')).not.toBeNull();
  });
});
