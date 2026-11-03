import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ActivityOverlay from "./ActivityOverlay";
import StatusBar from "./StatusBar";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

const STEPS = [
  { key: "prepare", label: "Prepare observation file" },
  { key: "parse", label: "Read RINEX header" },
  { key: "bia", label: "Fetch signal bias table" },
];

/** Steps run through the store, so every mutation needs an act() wrapper. */
const start = () => act(() => store().startActivity("Opening ALIC.rnx", STEPS));

describe("ActivityOverlay", () => {
  it("renders nothing when no activity is running", () => {
    const { container } = render(<ActivityOverlay />);
    expect(container).toBeEmptyDOMElement();
  });

  it("shows the title and every step", () => {
    start();
    render(<ActivityOverlay />);
    expect(screen.getByRole("heading", { name: "Opening ALIC.rnx" })).toBeInTheDocument();
    for (const step of STEPS) {
      expect(screen.getByText(step.label)).toBeInTheDocument();
    }
  });

  it("counts progress through the steps", () => {
    start();
    render(<ActivityOverlay />);
    expect(screen.getByText("Step 1 of 3")).toBeInTheDocument();

    act(() => store().resolveActivityStep("prepare", "done"));
    expect(screen.getByText("Step 2 of 3")).toBeInTheDocument();
  });

  it("counts a skipped step as progress, not a stall", () => {
    start();
    render(<ActivityOverlay />);
    act(() => store().resolveActivityStep("prepare", "skipped", "Already plain RINEX"));
    expect(screen.getByText("Step 2 of 3")).toBeInTheDocument();
  });

  it("shows a step's live detail", () => {
    start();
    render(<ActivityOverlay />);
    act(() => store().beginActivityStep("prepare", "Checking compression..."));
    expect(screen.getByText("Checking compression...")).toBeInTheDocument();
  });

  it("sweeps the bar while progress is unknown and fills it once bytes arrive", () => {
    start();
    const { container } = render(<ActivityOverlay />);

    act(() => store().beginActivityStep("bia"));
    expect(container.querySelector(".activity-bar.is-indeterminate")).not.toBeNull();

    act(() => store().updateActivityStep("bia", { percent: 40 }));
    const fill = container.querySelector(".activity-bar-fill") as HTMLElement;
    expect(container.querySelector(".activity-bar.is-indeterminate")).toBeNull();
    expect(fill.style.width).toBe("40%");
  });

  it("marks each step's state on its row", () => {
    start();
    const { container } = render(<ActivityOverlay />);
    act(() => {
      store().resolveActivityStep("prepare", "done");
      store().beginActivityStep("parse");
    });
    const rows = container.querySelectorAll(".activity-step");
    expect(rows[0]).toHaveClass("is-done");
    expect(rows[1]).toHaveClass("is-active");
    expect(rows[2]).toHaveClass("is-pending");
  });

  it("closes itself shortly after a clean finish", async () => {
    vi.useFakeTimers();
    try {
      act(() => store().startActivity("Opening ALIC.rnx", STEPS));
      render(<ActivityOverlay />);
      act(() => store().endActivity());
      expect(screen.getByText("Ready")).toBeInTheDocument();

      act(() => void vi.advanceTimersByTime(1000));
      expect(store().activity).toBeNull();
    } finally {
      vi.useRealTimers();
    }
  });

  it("stays up after a failure so the reason stays readable", async () => {
    vi.useFakeTimers();
    try {
      act(() => store().startActivity("Opening ALIC.rnx", STEPS));
      render(<ActivityOverlay />);
      act(() => store().endActivity("Could not reach the product mirrors"));

      act(() => void vi.advanceTimersByTime(5000));

      expect(store().activity).not.toBeNull();
      expect(screen.getByText("Stopped")).toBeInTheDocument();
      expect(
        screen.getByText("Could not reach the product mirrors"),
      ).toBeInTheDocument();
    } finally {
      vi.useRealTimers();
    }
  });

  it("treats a failed step as a failure even with no activity-level error", () => {
    start();
    render(<ActivityOverlay />);
    act(() => {
      store().resolveActivityStep("prepare", "error", "gunzip failed");
      store().endActivity();
    });
    expect(screen.getByText("Stopped")).toBeInTheDocument();
  });

  it("hides on request without cancelling the work behind it", async () => {
    start();
    render(<ActivityOverlay />);

    await userEvent.click(screen.getByTitle(/Hide/));

    await waitFor(() => expect(screen.queryByRole("status")).not.toBeInTheDocument());
    expect(store().activity?.finished).toBe(false);
    expect(store().activityHidden).toBe(true);
  });

  it("reopens hidden progress from the status bar with the latest step details", async () => {
    start();
    render(<><ActivityOverlay /><StatusBar /></>);
    await userEvent.click(screen.getByTitle(/Hide/));
    act(() => {
      store().beginActivityStep("bia", "Downloading biases");
      store().updateActivityStep("bia", { percent: 60 });
    });

    const reopen = screen.getByRole("button", { name: "Show progress: Opening ALIC.rnx" });
    reopen.focus();
    await userEvent.keyboard("{Enter}");

    expect(screen.getByRole("heading", { name: "Opening ALIC.rnx" })).toBeInTheDocument();
    expect(screen.getByText("Downloading biases")).toBeInTheDocument();
    expect(document.querySelector<HTMLElement>(".activity-bar-fill")?.style.width).toBe("60%");
    expect(screen.queryByRole("button", { name: /Show progress:/ })).not.toBeInTheDocument();
  });

  it.each([null, "Mirror failed"])("removes the reopen control when hidden work ends (%s)", async (error) => {
    start();
    render(<><ActivityOverlay /><StatusBar /></>);
    await userEvent.click(screen.getByTitle(/Hide/));
    act(() => store().endActivity(error));

    expect(screen.queryByRole("button", { name: /Show progress:/ })).not.toBeInTheDocument();
    expect(screen.queryByRole("status")).not.toBeInTheDocument();
    expect(store().activity).toBeNull();
  });

  it("shows a newly started activity even when the previous one was hidden", async () => {
    start();
    render(<ActivityOverlay />);
    await userEvent.click(screen.getByTitle(/Hide/));
    act(() => store().startActivity("Opening DARW.rnx", STEPS));
    expect(screen.getByRole("heading", { name: "Opening DARW.rnx" })).toBeInTheDocument();
    expect(store().activityHidden).toBe(false);
  });

  it("dismisses a finished failure outright", async () => {
    start();
    render(<ActivityOverlay />);
    act(() => store().endActivity("Mirror failed"));
    await userEvent.click(screen.getByTitle(/Hide/));
    expect(store().activity).toBeNull();
    expect(store().activityHidden).toBe(false);
  });

  it("announces itself politely for screen readers", () => {
    start();
    render(<ActivityOverlay />);
    const card = screen.getByRole("status");
    expect(card).toHaveAttribute("aria-live", "polite");
  });
});
