import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ConstellationFilter from "./ConstellationFilter";
import { useAppStore } from "../store/appStore";
import {
  makeCatalogue,
  makeObsCodes,
  resetAppStore,
  store,
} from "../test/storeHelpers";
import { mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

/** Seed the codes a parsed RINEX would have produced. */
function withCodes() {
  const codes = makeObsCodes();
  store().setAvailableObsCodes(codes);
  store().initializeConstellationCodes(codes);
}

const section = (name: RegExp) =>
  screen.getByText(name).closest(".cf-section") as HTMLElement;
const expand = (name: RegExp) => userEvent.click(screen.getByText(name));

describe("before a file is loaded", () => {
  it("says there is nothing to configure yet", () => {
    render(<ConstellationFilter />);
    expect(screen.getByText(/Load a RINEX file to see observation codes/)).toBeInTheDocument();
  });
});

describe("with a parsed file", () => {
  beforeEach(withCodes);

  it("lists only the constellations the file actually observed", () => {
    render(<ConstellationFilter />);
    expect(screen.getByText(/GPS \(Global Navigation System\)/)).toBeInTheDocument();
    expect(screen.getByText(/GAL \(Galileo\)/)).toBeInTheDocument();
    expect(screen.queryByText(/GLO \(GLONASS\)/)).not.toBeInTheDocument();
  });

  it("keeps the code list collapsed until the header is clicked", async () => {
    render(<ConstellationFilter />);
    expect(screen.queryByText("L1C")).not.toBeInTheDocument();

    await expand(/GPS \(Global/);

    expect(screen.getByText("L1C")).toBeInTheDocument();
    expect(screen.getByText("L2W")).toBeInTheDocument();
  });

  it("lists the phase signals only, not the header's code, Doppler or strength codes", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);

    expect(screen.getAllByText(/^L\d[A-Z]$/)).toHaveLength(3);
    for (const hidden of ["C1C", "C2W", "D1C"]) {
      expect(screen.queryByText(hidden)).not.toBeInTheDocument();
    }
  });

  it("collapses again on a second click", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);
    await expand(/GPS \(Global/);
    expect(screen.queryByText("L1C")).not.toBeInTheDocument();
  });

  it("toggles a whole constellation from its header checkbox", async () => {
    render(<ConstellationFilter />);
    const box = within(section(/GPS \(Global/)).getAllByRole("checkbox")[0];
    expect(box).toBeChecked();

    await userEvent.click(box);

    expect(store().constellationCodes.gps.every((c) => !c.enabled)).toBe(true);
  });

  it("shows a partly-enabled constellation as indeterminate", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);
    const boxes = within(section(/GPS \(Global/)).getAllByRole("checkbox");

    await userEvent.click(boxes[1]); // first code row

    const header = within(section(/GPS \(Global/)).getAllByRole("checkbox")[0] as HTMLInputElement;
    expect(header.indeterminate).toBe(true);
  });

  it("toggles a single observation code", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);
    const codeBox = within(section(/GPS \(Global/)).getAllByRole("checkbox")[1];

    await userEvent.click(codeBox);

    expect(store().constellationCodes.gps[0].enabled).toBe(false);
  });

  it("reorders codes to change their priority", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);

    const downButtons = screen.getAllByTitle("Move down (lower priority)");
    await userEvent.click(downButtons[0]);

    expect(store().constellationCodes.gps.map((c) => c.code)).toEqual([
      "L2W",
      "L1C",
      "L5Q",
    ]);
  });

  it("disables the move buttons at each end of the list", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);

    const ups = screen.getAllByTitle("Move up (higher priority)");
    const downs = screen.getAllByTitle("Move down (lower priority)");
    expect(ups[0]).toBeDisabled();
    expect(downs[downs.length - 1]).toBeDisabled();
  });

  it("marks a code the bias table does not support", async () => {
    store().updateConstellationCode("GPS", 0, { valid: false });
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);
    expect(screen.getByText("L1C")).toHaveClass("cf-code-name--invalid");
  });

  it("starts a code the bias table drops deselected, but still lets it be ticked", async () => {
    store().setConstellationCodeValidity("GPS", 0, false);
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);
    const codeBox = within(section(/GPS \(Global/)).getAllByRole("checkbox")[1];
    expect(codeBox).not.toBeChecked();

    await userEvent.click(codeBox);

    expect(codeBox).toBeChecked();
    expect(store().constellationCodes.gps[0]).toMatchObject({ valid: false, enabled: true });
    expect(screen.getByText("L1C")).toHaveClass("cf-code-name--invalid");
  });

  it("keeps a user's pick on an invalid code across re-validation", () => {
    store().setConstellationCodeValidity("GPS", 0, false);
    store().updateConstellationCode("GPS", 0, { enabled: true });
    store().setConstellationCodeValidity("GPS", 0, false);
    expect(store().constellationCodes.gps[0].enabled).toBe(true);
  });
});

describe("a stored selection that still holds code, Doppler or strength entries", () => {
  const item = (code: string) => ({ code, enabled: true, valid: true });

  beforeEach(() => {
    store().setAvailableObsCodes(makeObsCodes());
    useAppStore.setState({
      constellationCodes: {
        gps: [item("C1C"), item("L1C"), item("D1C"), item("L2W")],
        gal: [item("C1X"), item("S1X")],
        glo: [],
        bds: [],
        qzs: [],
      },
    });
  });

  it("shows only the L rows", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);

    expect(screen.getByText("L1C")).toBeInTheDocument();
    expect(screen.getByText("L2W")).toBeInTheDocument();
    expect(screen.queryByText("C1C")).not.toBeInTheDocument();
    expect(screen.queryByText("D1C")).not.toBeInTheDocument();
  });

  it("hides a constellation that has no L signal left to choose", () => {
    render(<ConstellationFilter />);
    expect(screen.queryByText(/GAL \(Galileo\)/)).not.toBeInTheDocument();
  });

  it("toggles the stored entry the row stands for", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);
    const boxes = within(section(/GPS \(Global/)).getAllByRole("checkbox"); // header, L1C, L2W

    await userEvent.click(boxes[2]);

    expect(store().constellationCodes.gps.map((c) => [c.code, c.enabled])).toEqual([
      ["C1C", true],
      ["L1C", true],
      ["D1C", true],
      ["L2W", false],
    ]);
  });

  it("reorders past the rows it does not show", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);

    // L2W is the last row on screen; moving it up puts it ahead of L1C.
    const ups = screen.getAllByTitle("Move up (higher priority)");
    await userEvent.click(ups[ups.length - 1]);

    const shown = store()
      .constellationCodes.gps.map((c) => c.code)
      .filter((code) => code.startsWith("L"));
    expect(shown).toEqual(["L2W", "L1C"]);
  });

  it("keeps the end-of-list buttons keyed to the visible rows", async () => {
    render(<ConstellationFilter />);
    await expand(/GPS \(Global/);

    const ups = screen.getAllByTitle("Move up (higher priority)");
    const downs = screen.getAllByTitle("Move down (lower priority)");
    expect(ups).toHaveLength(2);
    expect(ups[0]).toBeDisabled();
    expect(downs[1]).toBeDisabled();
    expect(downs[0]).toBeEnabled();
  });
});

describe("constellations the selected product cannot correct", () => {
  beforeEach(() => {
    withCodes();
    store().setPppCatalogue(makeCatalogue([{ constellations: ["GPS"] }]));
  });

  it("disables the section and says which product excluded it", () => {
    render(<ConstellationFilter />);
    const gal = section(/GAL \(Galileo\)/);
    expect(gal).toHaveClass("cf-section--disabled");
    expect(within(gal).getByText("not in IGS")).toBeInTheDocument();
    expect(within(gal).getAllByRole("checkbox")[0]).toBeDisabled();
  });

  it("spells the reason out in the tooltip", () => {
    render(<ConstellationFilter />);
    expect(within(section(/GAL \(Galileo\)/)).getByText("not in IGS")).toHaveAttribute(
      "title",
      "Not published by IGS/FIN/OPS for this date",
    );
  });

  it("leaves the permitted constellation alone", () => {
    render(<ConstellationFilter />);
    expect(section(/GPS \(Global/)).not.toHaveClass("cf-section--disabled");
  });
});

describe("the Validate BIA action", () => {
  beforeEach(withCodes);

  it("flags each code against the fetched bias table", async () => {
    useAppStore.setState({ bia: { gps: ["C1C", "L1C"], gal: [], glo: [], bds: [], qzs: [] } });
    mockInvoke({});
    render(<ConstellationFilter />);

    await userEvent.click(screen.getByRole("button", { name: "Validate BIA" }));

    await waitFor(() =>
      expect(store().constellationCodes.gps.map((c) => c.valid)).toEqual([
        true,
        false,
        false,
      ]),
    );
  });

  it("disables itself and says so while validating", () => {
    store().setBiaValidation({ isLoading: true });
    render(<ConstellationFilter />);
    const button = screen.getByRole("button", { name: "validating..." });
    expect(button).toBeDisabled();
  });

  it("surfaces a validation failure beside the button", () => {
    store().setBiaValidation({ error: "no BIA file in products directory" });
    render(<ConstellationFilter />);
    expect(screen.getByText("no BIA file in products directory")).toBeInTheDocument();
  });
});
