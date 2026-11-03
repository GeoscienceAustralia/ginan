import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import WorkflowPanel from "./WorkflowPanel";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

const event = (text: RegExp | string) => screen.getByText(text).closest("li")!;
/** The header of the (single) collapsible group: it summarises the run so far. */
const group = () => screen.getByRole("button", { expanded: true, name: /steps/ }).closest("li")!;
const collapsedGroup = () =>
  screen.getByRole("button", { expanded: false, name: /steps/ }).closest("li")!;

describe("the empty state", () => {
  it("explains what will appear, in file-run terms", () => {
    render(<WorkflowPanel />);
    expect(screen.getByText("Your run will appear here")).toBeInTheDocument();
    expect(screen.getByText(/Select an observation file/)).toBeInTheDocument();
  });

  it("rewords itself for a live stream", () => {
    store().setInputMode("realtime");
    render(<WorkflowPanel />);
    expect(screen.getByText(/Configure a stream/)).toBeInTheDocument();
  });

  it("disables Clear with nothing to clear", () => {
    render(<WorkflowPanel />);
    expect(screen.getByRole("button", { name: /Clear/ })).toBeDisabled();
  });
});

describe("the timeline", () => {
  it("renders one entry per line, in order", () => {
    store().appendWorkflowLines([
      "CDDIS connectivity OK.",
      "Static support files ready.",
      "✅ PEA finished with exit code 0",
    ]);
    render(<WorkflowPanel />);

    const items = screen.getAllByRole("listitem");
    expect(items).toHaveLength(3);
    expect(items[0]).toHaveTextContent("CDDIS connectivity OK.");
    expect(items[2]).toHaveTextContent("PEA finished with exit code 0");
  });

  it("labels each entry with its category and status", () => {
    store().appendWorkflowLine("CDDIS connectivity OK.");
    render(<WorkflowPanel />);

    const item = event("CDDIS connectivity OK.");
    expect(within(item).getByText("Connection")).toBeInTheDocument();
    expect(within(item).getByText("Complete")).toBeInTheDocument();
    expect(item).toHaveClass("wf-item--success");
  });

  it("marks a failure as an error", () => {
    store().appendWorkflowLine("❌ PEA error: process killed");
    render(<WorkflowPanel />);
    expect(event(/process killed/)).toHaveClass("wf-item--error");
  });

  it("counts the events, singular and plural", () => {
    store().appendWorkflowLine("one");
    const { rerender } = render(<WorkflowPanel />);
    // The header count; a group of two also says "2 steps" in its own meta.
    const count = () => document.querySelector(".wf-count")!;
    expect(count()).toHaveTextContent(/^1 step$/);

    store().appendWorkflowLine("two");
    rerender(<WorkflowPanel />);
    expect(count()).toHaveTextContent(/^2 steps$/);
  });

  it("clears the history", async () => {
    store().appendWorkflowLine("something happened");
    render(<WorkflowPanel />);

    await userEvent.click(screen.getByRole("button", { name: /Clear/ }));

    expect(useAppStore.getState().workflowLines).toEqual([]);
  });

  it("announces new entries politely for screen readers", () => {
    store().appendWorkflowLine("downloading");
    render(<WorkflowPanel />);
    const log = screen.getByRole("log");
    expect(log).toHaveAttribute("aria-live", "polite");
  });
});

describe("the group summary", () => {
  it("reads as in progress while a run is up, whatever the earlier lines said", () => {
    store().appendWorkflowLines(["❌ PEA error: boom", "Running PEA again"]);
    store().setIsProcessing(true);
    render(<WorkflowPanel />);
    expect(group()).toHaveTextContent("In progress");
    expect(group()).toHaveClass("wf-item--active");
  });

  it("reports the worst outcome once the run is over", () => {
    store().appendWorkflowLines(["Running PEA", "❌ PEA error: boom"]);
    const { rerender } = render(<WorkflowPanel />);
    expect(screen.getByText("Needs attention")).toBeInTheDocument();
    expect(group()).toHaveClass("wf-item--error");

    store().clearWorkflow();
    store().appendWorkflowLines(["BIA validation for IGS/FIN/OPS:", "⚠ GPS: no overlap with BIA"]);
    rerender(<WorkflowPanel />);
    expect(screen.getByText("Review warning")).toBeInTheDocument();

    store().clearWorkflow();
    store().appendWorkflowLines(["Running PEA", "✅ PEA finished with exit code 0"]);
    rerender(<WorkflowPanel />);
    expect(collapsedGroup()).toHaveTextContent("Complete");
    expect(collapsedGroup()).toHaveClass("wf-item--success");
  });

  it("falls back to a neutral summary for informational lines", () => {
    store().appendWorkflowLines(["BIA validation for IGS/FIN/OPS:", "BIA validation for COD/FIN/OPS:"]);
    render(<WorkflowPanel />);
    expect(screen.getByText("Recorded")).toBeInTheDocument();
  });

  it("marks only the newest line of a live run as in flight", () => {
    store().appendWorkflowLines([
      "Downloading IGS0OPSFIN.SP3.gz",
      "Downloading IGS0OPSFIN.CLK.gz",
    ]);
    store().setIsProcessing(true);
    render(<WorkflowPanel />);

    expect(event(/SP3/)).toHaveClass("wf-item--info");
    expect(event(/CLK/)).toHaveClass("wf-item--active");
  });
});
