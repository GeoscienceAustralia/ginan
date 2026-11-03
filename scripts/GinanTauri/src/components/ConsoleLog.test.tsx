import { beforeEach, describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ConsoleLog from "./ConsoleLog";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

const output = () => document.querySelector(".console-output")!;

describe("ConsoleLog", () => {
  it("defaults to the console channel", () => {
    store().appendLine("raw pea output");
    store().appendWorkflowLine("high-level status");
    render(<ConsoleLog />);
    expect(screen.getByText("Console Output")).toBeInTheDocument();
    expect(screen.getByText("raw pea output")).toBeInTheDocument();
    expect(screen.queryByText("high-level status")).not.toBeInTheDocument();
  });

  it("renders the workflow channel when asked", () => {
    store().appendLine("raw pea output");
    store().appendWorkflowLine("high-level status");
    render(<ConsoleLog channel="workflow" />);
    expect(screen.getByText("Workflow")).toBeInTheDocument();
    expect(screen.getByText("high-level status")).toBeInTheDocument();
    expect(screen.queryByText("raw pea output")).not.toBeInTheDocument();
  });

  it("shows a channel-specific placeholder while empty", () => {
    const { unmount } = render(<ConsoleLog />);
    expect(screen.getByText(/Waiting for PEA output/)).toBeInTheDocument();
    unmount();

    render(<ConsoleLog channel="workflow" />);
    expect(screen.getByText(/Workflow status will appear here/)).toBeInTheDocument();
  });

  it("clears only its own channel", async () => {
    store().appendLine("console line");
    store().appendWorkflowLine("workflow line");
    render(<ConsoleLog />);

    await userEvent.click(screen.getByRole("button", { name: /Clear/ }));

    expect(useAppStore.getState().consoleLines).toEqual([]);
    expect(useAppStore.getState().workflowLines).toEqual(["workflow line"]);
  });

  it("marks error lines so they can be styled apart", () => {
    store().appendLines(["[ERR] segfault", "ordinary line"]);
    render(<ConsoleLog />);
    expect(screen.getByText("[ERR] segfault")).toHaveClass("err-line");
    expect(screen.getByText("ordinary line")).not.toHaveClass("err-line");
  });

  it("renders ANSI colour codes as inline styles", () => {
    store().appendLine("[32mgreen text[0m");
    render(<ConsoleLog />);
    const span = output().querySelector("span[style]")!;
    expect(span).toHaveTextContent("green text");
    expect(span.getAttribute("style")).toContain("#98c379");
  });

  it("carries bold through into the colour span", () => {
    store().appendLine("[1m[31mbold red[0m");
    render(<ConsoleLog />);
    expect(output().querySelector("span[style]")!.getAttribute("style")).toContain(
      "font-weight:bold",
    );
  });

  it("escapes HTML so PEA output cannot inject markup", () => {
    store().appendLine("<img src=x onerror=alert(1)> & <b>bold</b>");
    render(<ConsoleLog />);
    expect(output().querySelector("img")).toBeNull();
    expect(output().querySelector("b")).toBeNull();
    expect(output().textContent).toContain("<img src=x onerror=alert(1)> & <b>bold</b>");
  });

  it("strips unrecognised ANSI codes rather than printing them", () => {
    store().appendLine("[38;5;208mexotic[0m");
    render(<ConsoleLog />);
    expect(output().textContent).not.toContain("[38;5;208m");
    expect(output().textContent).toContain("exotic");
  });

  it("renders every line in order", () => {
    store().appendLines(["first", "second", "third"]);
    render(<ConsoleLog />);
    expect(output().textContent).toBe("firstsecondthird");
  });

  it("adds the workflow modifier class only on that channel", () => {
    const { unmount } = render(<ConsoleLog channel="workflow" />);
    expect(document.querySelector(".workflow-output")).not.toBeNull();
    unmount();

    render(<ConsoleLog channel="console" />);
    expect(document.querySelector(".workflow-output")).toBeNull();
  });
});
