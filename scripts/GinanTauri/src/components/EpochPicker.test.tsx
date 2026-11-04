import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { useState } from "react";
import EpochPicker from "./EpochPicker";
import type { EpochWindow } from "./EpochPicker";

/** A picker wired to local state, standing in for the store. */
function Harness({
  initial = "",
  fileWindow,
  onChange,
}: {
  initial?: string;
  fileWindow?: EpochWindow;
  onChange?: (v: string) => void;
}) {
  const [value, setValue] = useState(initial);
  return (
    <>
      <EpochPicker
        id="t"
        label="Start Epoch"
        value={value}
        onChange={(v) => {
          setValue(v);
          onChange?.(v);
        }}
        fileWindow={fileWindow}
      />
      <button onClick={() => setValue("2025-06-01T12:00:00")}>external</button>
    </>
  );
}

const field = () => screen.getByLabelText("Start Epoch") as HTMLInputElement;

describe("EpochPicker", () => {
  it("shows the stored value in PEA's format, with its GPS week only on hover", () => {
    render(<Harness initial="2024-01-02T00:00:00" />);
    expect(field()).toHaveValue("2024-01-02 00:00:00");
    expect(screen.queryByText(/Week 2295/)).toBeNull();
    expect(field()).toHaveAttribute("title", expect.stringMatching(/^Week 2295 · day 2 · DOY 002 · UTC .*23:59:42$/));
  });

  it("accepts a pasted timestamp and normalises it on commit", async () => {
    const onChange = vi.fn();
    render(<Harness onChange={onChange} />);
    await userEvent.type(field(), "2024-1-2 3:04:05{Enter}");
    expect(onChange).toHaveBeenLastCalledWith("2024-01-02T03:04:05");
    expect(field()).toHaveValue("2024-01-02 03:04:05");
  });

  it("holds an invalid entry back and explains the format", async () => {
    const onChange = vi.fn();
    render(<Harness initial="2024-01-02T00:00:00" onChange={onChange} />);
    await userEvent.clear(field());
    await userEvent.type(field(), "2024-02-30 00:00:00{Enter}");
    expect(onChange).not.toHaveBeenCalled();
    expect(field()).toHaveAttribute("aria-invalid", "true");
    expect(screen.getByText(/Use YYYY-MM-DD HH:MM:SS/)).toBeInTheDocument();

    await userEvent.keyboard("{Escape}");
    expect(field()).toHaveValue("2024-01-02 00:00:00");
  });

  it("steps the field under the caret with the arrow keys", () => {
    const onChange = vi.fn();
    render(<Harness initial="2024-01-31T23:00:00" onChange={onChange} />);
    const input = field();
    input.setSelectionRange(12, 12); // inside the hour
    fireEvent.keyDown(input, { key: "ArrowUp" });
    expect(onChange).toHaveBeenLastCalledWith("2024-02-01T00:00:00");

    input.setSelectionRange(6, 6); // inside the month
    fireEvent.keyDown(input, { key: "ArrowDown" });
    expect(onChange).toHaveBeenLastCalledWith("2024-01-01T00:00:00");
  });

  it("follows the value when it changes from outside", async () => {
    render(<Harness initial="2024-01-02T00:00:00" />);
    await userEvent.click(screen.getByText("external"));
    expect(field()).toHaveValue("2025-06-01 12:00:00");
  });

  it("picks a day on the calendar and keeps the time of day", async () => {
    const onChange = vi.fn();
    render(<Harness initial="2024-01-02T06:30:15" onChange={onChange} />);
    await userEvent.click(screen.getByRole("button", { name: "Open calendar for Start Epoch" }));

    expect(screen.getByText("January 2024")).toBeInTheDocument();
    await userEvent.click(screen.getByRole("gridcell", { name: "2024-01-10" }));
    expect(onChange).toHaveBeenLastCalledWith("2024-01-10T06:30:15");

    await userEvent.click(screen.getByRole("button", { name: "Next month" }));
    expect(screen.getByText("February 2024")).toBeInTheDocument();
  });

  it("opens the calendar when the field is clicked and closes it from the swapped icon", async () => {
    render(<Harness initial="2024-01-02T00:00:00" />);
    await userEvent.click(field());
    expect(screen.getByRole("group", { name: "Start Epoch calendar" })).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Close calendar for Start Epoch" }));
    expect(screen.queryByRole("group", { name: "Start Epoch calendar" })).not.toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Open calendar for Start Epoch" })).toBeInTheDocument();
  });

  it("labels calendar rows with GPS weeks and shades the file's days", async () => {
    render(
      <Harness
        initial="2024-01-02T00:00:00"
        fileWindow={{ start: Date.UTC(2024, 0, 2), end: Date.UTC(2024, 0, 2, 23, 59, 30) }}
      />,
    );
    await userEvent.click(screen.getByRole("button", { name: /Open calendar/ }));
    expect(screen.getByRole("rowheader", { name: "2295" })).toBeInTheDocument();
    expect(screen.getByRole("gridcell", { name: "2024-01-02" })).toHaveClass("in-window", "is-selected");
    expect(screen.getByRole("gridcell", { name: "2024-01-03" })).not.toHaveClass("in-window");
  });

  it("sets the clock from the calendar's time fields", () => {
    const onChange = vi.fn();
    render(<Harness initial="2024-01-02T00:00:00" onChange={onChange} />);
    fireEvent.click(screen.getByRole("button", { name: /Open calendar/ }));
    fireEvent.change(screen.getByLabelText("Hour"), { target: { value: "13" } });
    expect(onChange).toHaveBeenLastCalledWith("2024-01-02T13:00:00");
    fireEvent.change(screen.getByLabelText("Minute"), { target: { value: "75" } });
    expect(onChange).toHaveBeenCalledTimes(1);
  });
});
