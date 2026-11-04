import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, within } from "@testing-library/react";
import CodePrioritiesSummary from "./CodePrioritiesSummary";
import { getConstellationObsCodesForConfig } from "../store/appStore";
import { makeObsCodes, resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

/** The codes a parsed RINEX would have produced: GPS L1C/L2W/L5Q, GAL L1X/L5X. */
function withCodes() {
  const codes = makeObsCodes();
  store().setAvailableObsCodes(codes);
  store().initializeConstellationCodes(codes);
}

const row = (label: string) =>
  screen.getByText(label).closest(".codeprio-row") as HTMLElement;

describe("the written priorities", () => {
  beforeEach(withCodes);

  it("lists every system the config writes a key for", () => {
    render(<CodePrioritiesSummary />);
    expect(within(row("GPS")).getByText("L1C")).toBeInTheDocument();
    expect(within(row("GPS")).getByText("L2W")).toBeInTheDocument();
    expect(within(row("GPS")).getByText("L5Q")).toBeInTheDocument();
    expect(within(row("Galileo")).getByText("L1X")).toBeInTheDocument();
  });

  it("names the YAML key each list lands on", () => {
    render(<CodePrioritiesSummary />);
    expect(within(row("GPS")).getByText("sys_options.gps")).toBeInTheDocument();
    expect(within(row("BeiDou")).getByText("sys_options.bds")).toBeInTheDocument();
  });

  it("shows priority order, since that is what PEA reads", () => {
    render(<CodePrioritiesSummary />);
    const codes = within(row("GPS")).getAllByRole("listitem").map((li) => li.textContent);
    expect(codes).toEqual(["1L1C", "2L2W", "3L5Q"]);
  });

  it("follows a reorder, so it reflects the sidebar rather than the file order", () => {
    store().reorderConstellationCodes("GPS", 2, 0);
    render(<CodePrioritiesSummary />);
    const codes = within(row("GPS")).getAllByRole("listitem").map((li) => li.textContent);
    expect(codes).toEqual(["1L5Q", "2L1C", "3L2W"]);
  });

  it("drops a code the user switched off", () => {
    store().updateConstellationCode("GPS", 1, { enabled: false });
    render(<CodePrioritiesSummary />);
    expect(within(row("GPS")).queryByText("L2W")).not.toBeInTheDocument();
    expect(within(row("GPS")).getByText("L1C")).toBeInTheDocument();
  });

  it("matches exactly what buildConfigInputs would send", () => {
    render(<CodePrioritiesSummary />);
    const allowed = store().availableConstellationsForCurrentSelection();
    const written = getConstellationObsCodesForConfig(store().constellationCodes, allowed);
    written.gps.forEach((code) => {
      expect(within(row("GPS")).getByText(code)).toBeInTheDocument();
    });
  });
});

describe("a system with nothing written", () => {
  it("says the template's own priorities survive, not that there are no signals", () => {
    withCodes();
    render(<CodePrioritiesSummary />);
    // GLONASS is absent from the fixture file, so no key is written for it.
    expect(
      within(row("GLONASS")).getByText(/PEA keeps the template's own priorities/),
    ).toBeInTheDocument();
  });

  it("explains the whole-config case when no signal is selected anywhere", () => {
    render(<CodePrioritiesSummary />);
    expect(screen.getByText(/No constellation has a signal selected/)).toBeInTheDocument();
  });

  it("drops the explanation once something is selected", () => {
    withCodes();
    render(<CodePrioritiesSummary />);
    expect(screen.queryByText(/No constellation has a signal selected/)).not.toBeInTheDocument();
  });
});
