import { useState } from "react";
import { describe, expect, it, vi } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Dropdown, { type DropdownOption } from "./Dropdown";

const stream: DropdownOption = {
  value: "ALIC00AUS0",
  location: "Alice Springs",
  format: "RTCM 3.3",
};

/** Dropdown whose `value` follows its own onChange, for typing and picking. */
function Harness({ options, initial = "" }: { options: DropdownOption[]; initial?: string }) {
  const [value, setValue] = useState(initial);
  return (
    <Dropdown value={value} onChange={setValue} options={options} ariaLabel="Stream" editable />
  );
}

describe("Dropdown", () => {
  it("draws the open list under document.body, outside the clipping card", async () => {
    render(
      <div className="rt-stream-block" data-testid="card" style={{ overflow: "hidden" }}>
        <Dropdown value="a" onChange={() => {}} options={[{ value: "a" }, { value: "b" }]} ariaLabel="Stream" />
      </div>,
    );

    await userEvent.click(screen.getByRole("combobox", { name: "Stream" }));

    const list = screen.getByRole("listbox");
    expect(list.parentElement).toBe(document.body);
    expect(screen.getByTestId("card")).not.toContainElement(list);
    expect(screen.getByRole("combobox", { name: "Stream" })).toHaveAttribute("aria-controls", list.id);
  });

  it("opens on click and closes after picking an option", async () => {
    const onChange = vi.fn();
    render(
      <Dropdown
        value="a"
        onChange={onChange}
        options={[{ value: "a" }, { value: "b" }]}
        ariaLabel="Stream"
      />,
    );

    await userEvent.click(screen.getByRole("combobox", { name: "Stream" }));
    expect(screen.getByRole("listbox")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("option", { name: "b" }));
    expect(onChange).toHaveBeenCalledWith("b");
    expect(screen.queryByRole("listbox")).not.toBeInTheDocument();
  });

  it("shows an option's value, location and format on separate lines", async () => {
    render(
      <Dropdown value="" onChange={vi.fn()} options={[stream]} ariaLabel="Stream" />,
    );

    await userEvent.click(screen.getByRole("combobox", { name: "Stream" }));

    const row = screen.getByRole("option");
    expect(row.querySelector(".dd-option-code")!.textContent).toBe("ALIC00AUS0");
    expect(row.querySelector(".dd-option-location")!.textContent).toBe("Alice Springs");
    expect(row.querySelector(".dd-option-format")!.textContent).toBe("RTCM 3.3");
    expect(row.textContent).not.toContain("·");
  });

  it("shows the two lines in the closed button when the current value matches", () => {
    render(
      <Dropdown value="ALIC00AUS0" onChange={vi.fn()} options={[stream]} ariaLabel="Stream" />,
    );

    const button = screen.getByRole("combobox", { name: "Stream" });
    const cover = button.querySelector(".dd-cover-text")!;
    expect(cover.querySelector(".dd-option-location")!.textContent).toBe("Alice Springs");
    expect(cover.querySelector(".dd-option-format")!.textContent).toBe("RTCM 3.3");
    expect(screen.queryByRole("listbox")).not.toBeInTheDocument();
  });

  it("covers a filled-in editable input with a hidden two-line overlay until it opens", async () => {
    const { container } = render(
      <Dropdown value="ALIC00AUS0" onChange={vi.fn()} options={[stream]} ariaLabel="Stream" editable />,
    );

    const cover = container.querySelector<HTMLElement>(".dd-cover")!;
    expect(cover).toHaveAttribute("aria-hidden", "true");

    await userEvent.click(screen.getByRole("combobox", { name: "Stream" }));

    expect(container.querySelector(".dd-cover")).toBeNull();
  });

  it("falls back to the detail line for options without location or format", async () => {
    render(
      <Dropdown
        value=""
        onChange={vi.fn()}
        options={[{ value: "x", detail: "Some detail" }]}
        ariaLabel="Stream"
      />,
    );

    await userEvent.click(screen.getByRole("combobox", { name: "Stream" }));

    const row = screen.getByRole("option");
    expect(row.querySelector(".dd-option-detail")!.textContent).toBe("Some detail");
    expect(row.querySelector(".dd-option-code")).toBeNull();
  });

  describe("disabled options", () => {
    const options: DropdownOption[] = [{ value: "a" }, { value: "b", disabled: true }, { value: "c" }];
    const combobox = () => screen.getByRole("combobox", { name: "Stream" });

    it("marks the option aria-disabled", async () => {
      render(<Dropdown value="a" onChange={vi.fn()} options={options} ariaLabel="Stream" />);

      await userEvent.click(combobox());

      expect(screen.getByRole("option", { name: "b" })).toHaveAttribute("aria-disabled", "true");
    });

    it("does not pick a disabled option and keeps the list open", async () => {
      const onChange = vi.fn();
      render(<Dropdown value="a" onChange={onChange} options={options} ariaLabel="Stream" />);

      await userEvent.click(combobox());
      await userEvent.click(screen.getByRole("option", { name: "b" }));

      expect(onChange).not.toHaveBeenCalled();
      expect(screen.getByRole("listbox")).toBeInTheDocument();
    });

    it("skips a disabled option when moving the highlight with the arrow keys", async () => {
      render(<Dropdown value="a" onChange={vi.fn()} options={options} ariaLabel="Stream" />);

      await userEvent.click(combobox());
      await userEvent.keyboard("{ArrowDown}");
      expect(combobox()).toHaveAttribute(
        "aria-activedescendant",
        screen.getByRole("option", { name: "c" }).id,
      );

      await userEvent.keyboard("{ArrowUp}");
      expect(combobox()).toHaveAttribute(
        "aria-activedescendant",
        screen.getByRole("option", { name: "a" }).id,
      );
    });

    it("picks the highlighted option with Enter after skipping a disabled one", async () => {
      const onChange = vi.fn();
      render(<Dropdown value="a" onChange={onChange} options={options} ariaLabel="Stream" />);

      await userEvent.click(combobox());
      await userEvent.keyboard("{ArrowDown}{Enter}");

      expect(onChange).toHaveBeenCalledWith("c");
    });
  });

  it("narrows suggestions by location and format as the user types", async () => {
    const options: DropdownOption[] = [
      stream,
      { value: "BCEP00BKG0", location: "Brisbane", format: "RTCM 3.2" },
    ];
    render(<Harness options={options} />);

    await userEvent.type(screen.getByRole("combobox", { name: "Stream" }), "alice");

    const rows = within(screen.getByRole("listbox")).getAllByRole("option");
    expect(rows).toHaveLength(1);
    expect(rows[0]).toHaveTextContent("ALIC00AUS0");
  });
});
