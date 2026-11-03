import { describe, expect, it, vi } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import InputSectionHeader, { type InputSectionHeaderProps } from "./InputSectionHeader";

function renderHeader(overrides: Partial<InputSectionHeaderProps> = {}) {
  const props: InputSectionHeaderProps = {
    title: "Corrections",
    onAdd: vi.fn(),
    enabledTypes: ["ntrip", "serial"],
    section: "corr",
    ...overrides,
  };
  render(
    <>
      <InputSectionHeader {...props} />
      <button type="button">Outside</button>
    </>,
  );
  return props;
}

const plusButton = () => screen.getByRole("button", { name: /^Add to / });
const menu = () => screen.getByRole("menu", { name: / options$/ });
const item = (name: string) => screen.getByRole("menuitem", { name });

describe("InputSectionHeader", () => {
  it("renders the title as a heading with the info node after it", () => {
    renderHeader({ info: <span data-testid="info-node">i</span> });
    expect(screen.getByRole("heading", { name: /Corrections/ })).toBeInTheDocument();
    expect(screen.getByTestId("info-node")).toBeInTheDocument();
  });

  it("starts with the menu closed", () => {
    renderHeader();
    expect(screen.queryByRole("menu")).toBeNull();
    expect(plusButton()).toHaveAttribute("aria-expanded", "false");
  });

  it("lists exactly File, HTTP, NTRIP, Serial, SBAS in order, with no extra items and no submenu", async () => {
    const user = userEvent.setup();
    renderHeader();
    await user.click(plusButton());
    expect(plusButton()).toHaveAttribute("aria-expanded", "true");
    expect(within(menu()).getAllByRole("menuitem").map((el) => el.textContent)).toEqual([
      "File",
      "HTTP",
      "NTRIP",
      "Serial",
      "SBAS",
    ]);
    expect(screen.queryByText("Add product")).toBeNull();
    expect(screen.getAllByRole("menu")).toHaveLength(1);
  });

  it("disables types not in enabledTypes and keeps them in the menu", async () => {
    const user = userEvent.setup();
    const { onAdd } = renderHeader({ enabledTypes: ["ntrip", "serial"] });
    await user.click(plusButton());
    for (const name of ["File", "HTTP", "SBAS"]) {
      expect(item(name)).toBeDisabled();
    }
    for (const name of ["NTRIP", "Serial"]) {
      expect(item(name)).toBeEnabled();
    }
    await user.click(item("File"));
    expect(onAdd).not.toHaveBeenCalled();
    expect(menu()).toBeInTheDocument();
  });

  it("opening the menu focuses the first enabled item", async () => {
    const user = userEvent.setup();
    renderHeader({ enabledTypes: ["ntrip", "serial"] });
    await user.click(plusButton());
    expect(item("NTRIP")).toHaveFocus();
  });

  it("clicking an enabled type calls onAdd once with that type, closes the menu and focuses the plus button", async () => {
    const user = userEvent.setup();
    const { onAdd } = renderHeader();
    await user.click(plusButton());
    await user.click(item("Serial"));
    expect(onAdd).toHaveBeenCalledTimes(1);
    expect(onAdd).toHaveBeenCalledWith("serial");
    expect(screen.queryByRole("menu")).toBeNull();
    expect(plusButton()).toHaveFocus();
  });

  it("Escape closes the menu and returns focus to the plus button", async () => {
    const user = userEvent.setup();
    renderHeader();
    await user.click(plusButton());
    await user.keyboard("{Escape}");
    expect(screen.queryByRole("menu")).toBeNull();
    expect(plusButton()).toHaveFocus();
  });

  it("a mousedown outside the header closes the menu", async () => {
    const user = userEvent.setup();
    renderHeader();
    await user.click(plusButton());
    expect(screen.getByRole("menu")).toBeInTheDocument();
    await user.click(screen.getByRole("button", { name: "Outside" }));
    expect(screen.queryByRole("menu")).toBeNull();
  });

  it("ArrowDown and ArrowUp skip disabled items and wrap around", async () => {
    const user = userEvent.setup();
    renderHeader({ enabledTypes: ["ntrip", "serial"] });
    await user.click(plusButton());
    expect(item("NTRIP")).toHaveFocus();
    await user.keyboard("{ArrowDown}");
    expect(item("Serial")).toHaveFocus();
    await user.keyboard("{ArrowDown}");
    expect(item("NTRIP")).toHaveFocus();
    await user.keyboard("{ArrowUp}");
    expect(item("Serial")).toHaveFocus();
  });

  it("Enter on a focused enabled item chooses it", async () => {
    const user = userEvent.setup();
    const { onAdd } = renderHeader({ enabledTypes: ["ntrip", "serial"] });
    await user.click(plusButton());
    await user.keyboard("{ArrowDown}");
    expect(item("Serial")).toHaveFocus();
    await user.keyboard("{Enter}");
    expect(onAdd).toHaveBeenCalledTimes(1);
    expect(onAdd).toHaveBeenCalledWith("serial");
    expect(screen.queryByRole("menu")).toBeNull();
    expect(plusButton()).toHaveFocus();
  });

  it("each item has a dot span for its type, NTRIP's being input-plus-dot--ntrip", async () => {
    const user = userEvent.setup();
    renderHeader();
    await user.click(plusButton());
    for (const [name, type] of [
      ["File", "file"],
      ["HTTP", "http"],
      ["NTRIP", "ntrip"],
      ["Serial", "serial"],
      ["SBAS", "sbas"],
    ] as const) {
      expect(item(name).querySelector(`.input-plus-dot--${type}`)).not.toBeNull();
    }
    expect(item("NTRIP").querySelector(".input-plus-dot--ntrip")).toBeInTheDocument();
  });

  it("leaves SBAS out of the Observation menu: it is a correction source only", async () => {
    const user = userEvent.setup();
    renderHeader({ title: "Observation", section: "obs" });
    await user.click(plusButton());
    expect(within(menu()).getAllByRole("menuitem").map((el) => el.textContent)).toEqual([
      "File",
      "HTTP",
      "NTRIP",
      "Serial",
    ]);
  });
});
