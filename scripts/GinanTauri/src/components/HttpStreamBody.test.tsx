import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import HttpStreamBody from "./HttpStreamBody";
import type { InputProduct } from "../types";

const renderBody = (value: string, onChange = vi.fn()) => {
  const product: InputProduct = { id: "1", type: "http", value };
  render(<HttpStreamBody section="obs" product={product} onChange={onChange} />);
  return { onChange, input: screen.getByRole("textbox", { name: "URL" }) };
};

const hint = () => screen.queryByText(/http:\/\/ or https:\/\//);

describe("HttpStreamBody", () => {
  it("commits on Enter or blur only once the URL is valid", () => {
    const onDone = vi.fn();
    const { rerender } = render(<HttpStreamBody section="obs" product={{ id: "1", type: "http", value: "example.org" }} onChange={vi.fn()} onDone={onDone} />);
    const input = screen.getByRole("textbox", { name: "URL" });
    fireEvent.keyDown(input, { key: "Enter" });
    fireEvent.blur(input);
    expect(onDone).not.toHaveBeenCalled();
    rerender(<HttpStreamBody section="obs" product={{ id: "1", type: "http", value: "https://host/MOUNT" }} onChange={vi.fn()} onDone={onDone} />);
    fireEvent.keyDown(input, { key: "Enter" });
    expect(onDone).toHaveBeenCalledTimes(1);
  });

  it("shows a URL field", () => {
    renderBody("");
    expect(screen.getByRole("textbox", { name: "URL" })).toBeInTheDocument();
  });

  it("shows no hint and no invalid flag while the value is empty", () => {
    const { input } = renderBody("");
    expect(hint()).toBeNull();
    expect(input).toHaveAttribute("aria-invalid", "false");
  });

  it("flags a value without a scheme", () => {
    const { input } = renderBody("example.org/x");
    expect(hint()).not.toBeNull();
    expect(input).toHaveAttribute("aria-invalid", "true");
  });

  it("shows no hint for a valid URL", () => {
    const { input } = renderBody("https://ntrip.example.org/ALIC00AUS0");
    expect(hint()).toBeNull();
    expect(input).toHaveAttribute("aria-invalid", "false");
  });

  it("reports the edited value trimmed", () => {
    const { onChange, input } = renderBody("");
    fireEvent.change(input, { target: { value: "  https://h.example/x  " } });
    expect(onChange).toHaveBeenCalledWith({ value: "https://h.example/x" });
  });
});
