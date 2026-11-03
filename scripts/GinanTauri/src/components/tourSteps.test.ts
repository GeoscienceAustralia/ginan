import { beforeEach, describe, expect, it } from "vitest";
import { TOURS, tourForCurrentShell } from "./tourSteps";
import type { TourKey } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { useAppStore } from "../store/appStore";

beforeEach(() => {
  resetAppStore();
});

describe("tourForCurrentShell", () => {
  it("picks the welcome tour when nothing is open", () => {
    expect(tourForCurrentShell("file", false)).toBe("welcome");
  });

  it("picks the workspace tour once a file is parsed", () => {
    expect(tourForCurrentShell("file", true)).toBe("workspace");
  });

  it("picks the streaming tour in realtime mode", () => {
    expect(tourForCurrentShell("realtime", false)).toBe("streaming");
  });

  it("prefers streaming over a stale parsed file", () => {
    // Switching to realtime clears rinexData, but a caller holding an old value
    // must not be steered into the file tour.
    expect(tourForCurrentShell("realtime", true)).toBe("streaming");
  });
});

describe("TOURS", () => {
  const keys: TourKey[] = ["welcome", "workspace", "streaming"];

  it("defines a non-empty list for every tour key", () => {
    for (const key of keys) {
      expect(TOURS[key].length, key).toBeGreaterThan(0);
    }
  });

  it("gives every step a title and a body", () => {
    for (const key of keys) {
      for (const step of TOURS[key]) {
        expect(step.title.length, `${key}: ${step.title}`).toBeGreaterThan(0);
        expect(step.body.length, `${key}: ${step.title}`).toBeGreaterThan(0);
      }
    }
  });

  it("only uses placements the Tour component understands", () => {
    const valid = new Set(["top", "bottom", "left", "right", undefined]);
    for (const key of keys) {
      for (const step of TOURS[key]) {
        expect(valid.has(step.placement), `${key}: ${step.title}`).toBe(true);
      }
    }
  });

  it("centres only the opening card, and only in the welcome tour", () => {
    expect(TOURS.welcome[0].target).toBeNull();
    for (const key of ["workspace", "streaming"] as TourKey[]) {
      expect(TOURS[key].every((s) => s.target !== null), key).toBe(true);
    }
  });

  it("does not point two steps of a tour at the same target", () => {
    for (const key of keys) {
      const targets = TOURS[key].map((s) => s.target).filter((t): t is string => t !== null);
      expect(new Set(targets).size, key).toBe(targets.length);
    }
  });

  it("finishes the workspace and streaming tours on the status bar", () => {
    expect(TOURS.workspace[TOURS.workspace.length - 1].target).toBe("status-bar");
    expect(TOURS.streaming[TOURS.streaming.length - 1].target).toBe("status-bar");
  });

  it("walks the streaming tour past the four realtime-only targets", () => {
    const targets = TOURS.streaming.map((s) => s.target);
    expect(targets).toEqual(
      expect.arrayContaining(["rt-obs", "rt-caster", "rt-nav", "rt-recording"]),
    );
  });

  it("never sends the streaming tour to the file run's product source", () => {
    // A live run downloads no products, so pointing at them would describe a
    // dead panel. Constellations does apply: a live run keeps the signals
    // picked there.
    const targets = TOURS.streaming.map((s) => s.target);
    expect(targets).not.toContain("section-products");
    expect(targets).not.toContain("ppp-source");
    expect(targets).toContain("section-constellations");
  });
});

describe("step onEnter hooks", () => {
  it("opens the sidebar section a step describes", () => {
    store().setConfigSection(null);
    const step = TOURS.workspace.find((s) => s.target === "section-output")!;
    step.onEnter!();
    expect(store().configSection).toBe("output");
  });

  it("reopens a collapsed rail before pointing at it", () => {
    useAppStore.setState({ sidebarCollapsed: true });
    TOURS.workspace[0].onEnter!();
    expect(store().sidebarCollapsed).toBe(false);
  });

  it("leaves an already-open rail alone", () => {
    useAppStore.setState({ sidebarCollapsed: false });
    TOURS.workspace[0].onEnter!();
    expect(store().sidebarCollapsed).toBe(false);
  });

  it("keeps the streaming rt-* steps inside the Input section", () => {
    for (const target of ["rt-obs", "rt-caster", "rt-nav", "rt-settings"]) {
      store().setConfigSection(null);
      TOURS.streaming.find((s) => s.target === target)!.onEnter!();
      expect(store().configSection, target).toBe("input");
    }
  });

  it("opens Output for the recordings step, where the recordings live", () => {
    store().setConfigSection(null);
    TOURS.streaming.find((s) => s.target === "rt-recording")!.onEnter!();
    expect(store().configSection).toBe("output");
  });
});
