import { describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import PathControl from "./PathControl";

describe("PathControl", () => {
  it("shows the path and hangs the full value off a tooltip", () => {
    render(
      <PathControl value="/very/long/path/to/file.rnx" empty="None" onBrowse={() => {}} />,
    );
    const value = screen.getByText("/very/long/path/to/file.rnx");
    expect(value).toHaveAttribute("title", "/very/long/path/to/file.rnx");
    expect(value).not.toHaveClass("is-empty");
  });

  it("shows the empty placeholder, marked as empty and with no tooltip", () => {
    render(<PathControl value={null} empty="No file selected" onBrowse={() => {}} />);
    const value = screen.getByText("No file selected");
    expect(value).toHaveClass("is-empty");
    expect(value).not.toHaveAttribute("title");
  });

  it("styles an empty string as empty, though only null substitutes the placeholder", () => {
    const { container } = render(
      <PathControl value="" empty="No file selected" onBrowse={() => {}} />,
    );
    const value = container.querySelector(".path-value")!;
    expect(value).toHaveClass("is-empty");
    expect(value).toBeEmptyDOMElement();
  });

  it("calls onBrowse when the button is pressed", async () => {
    const onBrowse = vi.fn();
    render(<PathControl value={null} empty="None" onBrowse={onBrowse} />);

    await userEvent.click(screen.getByRole("button", { name: "Browse..." }));

    expect(onBrowse).toHaveBeenCalledOnce();
  });

  it("uses a custom browse label", () => {
    render(
      <PathControl value={null} empty="None" onBrowse={() => {}} browseLabel="Choose folder" />,
    );
    expect(screen.getByRole("button", { name: "Choose folder" })).toBeInTheDocument();
  });

  it("adds the stacked modifier only when asked", () => {
    const { container, rerender } = render(
      <PathControl value={null} empty="None" onBrowse={() => {}} />,
    );
    expect(container.querySelector(".path-control--stacked")).toBeNull();

    rerender(<PathControl value={null} empty="None" onBrowse={() => {}} stacked />);
    expect(container.querySelector(".path-control--stacked")).not.toBeNull();
  });
});
