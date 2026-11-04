import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen } from "@testing-library/react";
import Tour from "./Tour";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";

/**
 * The real tours point at the whole app; these tests use a small stand-in tour
 * whose targets they control, so the walk-through mechanics are tested on their
 * own. `tourSteps.test.ts` covers the real content.
 */
const onEnterSecond = vi.fn();
vi.mock("./tourSteps", () => ({
  TOURS: {
    welcome: [
      { target: null, title: "Hello", body: "A centred intro card." },
      { target: "alpha", title: "Alpha", body: "The first control.", onEnter: () => onEnterSecond() },
      { target: "beta", title: "Beta", body: "The second control.", placement: "right" },
    ],
    workspace: [{ target: "gone", title: "Gone", body: "Never on screen." }],
    streaming: [
      { target: null, title: "Streams", body: "Only step." },
    ],
  },
}));

beforeEach(() => {
  resetAppStore();
  onEnterSecond.mockClear();
});

afterEach(() => {
  vi.useRealTimers();
});

/** A `data-tour` target with a real, measurable box. */
function addTarget(id: string, rect = { top: 100, left: 100, width: 80, height: 30 }) {
  const el = document.createElement("div");
  el.setAttribute("data-tour", id);
  el.getBoundingClientRect = () =>
    ({ ...rect, right: rect.left + rect.width, bottom: rect.top + rect.height, x: rect.left, y: rect.top, toJSON() {} }) as DOMRect;
  document.body.appendChild(el);
  return el;
}

const title = () => screen.getByRole("heading", { level: 2 });
const nextBtn = () => screen.getByRole("button", { name: /^(Next|Done)$/ });
const backBtn = () => screen.getByRole("button", { name: /Back/ });

function start(tour: "welcome" | "workspace" | "streaming" = "welcome") {
  act(() => store().startTour(tour));
}

afterEach(() => {
  document.querySelectorAll("[data-tour]").forEach((el) => el.remove());
});

describe("Tour", () => {
  it("renders nothing without an active tour", () => {
    render(<Tour />);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("opens on the first step with a step count", () => {
    render(<Tour />);
    start();
    expect(screen.getByRole("dialog", { name: "Guided tour" })).toBeInTheDocument();
    expect(title()).toHaveTextContent("Hello");
    expect(screen.getByText("1 of 3")).toBeInTheDocument();
    expect(backBtn()).toBeDisabled();
  });

  it("walks forward and back, running a step's onEnter", () => {
    addTarget("alpha");
    addTarget("beta");
    render(<Tour />);
    start();

    fireEvent.click(nextBtn());
    expect(title()).toHaveTextContent("Alpha");
    expect(onEnterSecond).toHaveBeenCalledTimes(1);

    fireEvent.click(nextBtn());
    expect(title()).toHaveTextContent("Beta");
    expect(nextBtn()).toHaveTextContent("Done");

    fireEvent.click(backBtn());
    expect(title()).toHaveTextContent("Alpha");
  });

  it("records the tour as seen when it is finished", () => {
    addTarget("alpha");
    addTarget("beta");
    render(<Tour />);
    start();
    fireEvent.click(nextBtn());
    fireEvent.click(nextBtn());

    fireEvent.click(nextBtn()); // Done

    expect(store().activeTour).toBeNull();
    expect(store().completedTours).toEqual(["welcome"]);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("records a skipped tour too, so it never auto-starts again", () => {
    render(<Tour />);
    start();
    fireEvent.click(screen.getByRole("button", { name: "Skip" }));
    expect(store().activeTour).toBeNull();
    expect(store().completedTours).toEqual(["welcome"]);
  });

  it("leaves from the close button", () => {
    render(<Tour />);
    start();
    fireEvent.click(screen.getByRole("button", { name: "Leave the tour" }));
    expect(store().activeTour).toBeNull();
  });

  it("is driven from the keyboard", () => {
    addTarget("alpha");
    render(<Tour />);
    start();

    fireEvent.keyDown(window, { key: "ArrowRight" });
    expect(title()).toHaveTextContent("Alpha");
    fireEvent.keyDown(window, { key: "ArrowLeft" });
    expect(title()).toHaveTextContent("Hello");
    fireEvent.keyDown(window, { key: "Enter" });
    expect(title()).toHaveTextContent("Alpha");

    fireEvent.keyDown(window, { key: "Escape" });
    expect(store().activeTour).toBeNull();
  });

  it("treats a click on the dimmed backdrop as Next", () => {
    render(<Tour />);
    start();
    fireEvent.click(document.querySelector(".tour-scrim")!);
    expect(title()).toHaveTextContent("Alpha");
  });

  it("spotlights a measurable target and scrolls it into view", async () => {
    vi.useFakeTimers();
    const target = addTarget("alpha");
    const scroll = vi.spyOn(target, "scrollIntoView");
    render(<Tour />);
    start();
    fireEvent.click(nextBtn());

    await act(async () => {
      vi.advanceTimersByTime(50);
    });

    const spot = document.querySelector<HTMLElement>(".tour-spot");
    expect(spot).not.toBeNull();
    // Padded 6px out from the target's own box.
    expect(spot!.style.top).toBe("94px");
    expect(spot!.style.width).toBe("92px");
    expect(scroll).toHaveBeenCalledTimes(1);
  });

  it("steps over a target that never appears", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "performance"] });
    addTarget("beta");
    render(<Tour />);
    start();
    fireEvent.click(nextBtn()); // onto "alpha", which is not in the DOM
    expect(title()).toHaveTextContent("Alpha");

    for (let i = 0; i < 10; i++) {
      await act(async () => {
        vi.advanceTimersByTime(100);
      });
    }

    expect(title()).toHaveTextContent("Beta");
  });

  it("treats a zero-sized target as missing", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "performance"] });
    addTarget("alpha", { top: 0, left: 0, width: 0, height: 0 });
    addTarget("beta");
    render(<Tour />);
    start();
    fireEvent.click(nextBtn());

    for (let i = 0; i < 10; i++) {
      await act(async () => {
        vi.advanceTimersByTime(100);
      });
    }

    expect(title()).toHaveTextContent("Beta");
  });

  it("ends the tour when the last target is missing and there is nowhere left to go", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "performance"] });
    render(<Tour />);
    start("workspace");
    expect(title()).toHaveTextContent("Gone");

    for (let i = 0; i < 10; i++) {
      await act(async () => {
        vi.advanceTimersByTime(100);
      });
    }

    expect(store().activeTour).toBeNull();
  });

  it("closes when the input mode changes underneath it", () => {
    render(<Tour />);
    start();
    act(() => store().setInputMode("realtime"));
    expect(store().activeTour).toBeNull();
  });

  it("restarts at the first step when reopened", () => {
    addTarget("alpha");
    render(<Tour />);
    start();
    fireEvent.click(nextBtn());
    fireEvent.click(screen.getByRole("button", { name: "Skip" }));

    start();

    expect(title()).toHaveTextContent("Hello");
  });

  it("calls a single-step tour's only button Done", () => {
    useAppStore.setState({ inputMode: "realtime" });
    render(<Tour />);
    start("streaming");
    expect(nextBtn()).toHaveTextContent("Done");
    fireEvent.click(nextBtn());
    expect(store().completedTours).toEqual(["streaming"]);
  });
});
