import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { BrowseZone, FileCard, fileBaseName, RINEX_EXTENSION_HINT } from "./FileDropZone";
import { RINEX_EXTENSIONS } from "../hooks/usePickers";

describe("fileBaseName", () => {
  it("keeps the last segment of a POSIX path", () => {
    expect(fileBaseName("/data/run/ALIC.rnx")).toBe("ALIC.rnx");
  });

  it("keeps the last segment of a Windows path", () => {
    expect(fileBaseName("C:\\data\\ALIC.rnx")).toBe("ALIC.rnx");
  });

  it("returns a bare file name unchanged", () => {
    expect(fileBaseName("ALIC.rnx")).toBe("ALIC.rnx");
  });
});

describe("RINEX_EXTENSION_HINT", () => {
  it("names every fixed extension in RINEX_EXTENSIONS", () => {
    const fixed = RINEX_EXTENSIONS.filter((ext) => !/^\d{2}[od]$/.test(ext));
    for (const ext of fixed) {
      expect(RINEX_EXTENSION_HINT).toContain(`.${ext}`);
    }
  });

  it("names the two-digit-year short forms", () => {
    expect(RINEX_EXTENSION_HINT).toContain(".YYo");
    expect(RINEX_EXTENSION_HINT).toContain(".YYd");
  });
});

describe("BrowseZone", () => {
  it("shows the call to action and the accepted extensions", () => {
    render(<BrowseZone onBrowse={() => {}} />);
    const zone = screen.getByRole("button", { name: /Click to browse for a RINEX file/i });
    expect(zone).toHaveTextContent(RINEX_EXTENSION_HINT);
  });

  it("calls onBrowse once when pressed", async () => {
    const onBrowse = vi.fn();
    render(<BrowseZone onBrowse={onBrowse} />);

    await userEvent.click(screen.getByRole("button", { name: /Click to browse for a RINEX file/i }));

    expect(onBrowse).toHaveBeenCalledOnce();
  });
});

describe("FileCard", () => {
  it("collapses once a file is chosen, but not when it opens with one", () => {
    const { rerender } = render(<FileCard name={null} empty="No file selected"><p>Card body</p></FileCard>);
    expect(screen.getByText("Card body")).toBeInTheDocument();
    rerender(<FileCard name="ALIC.rnx" empty="No file selected"><p>Card body</p></FileCard>);
    expect(screen.queryByText("Card body")).not.toBeInTheDocument();
  });

  it("shows the name in a disclosure button and toggles its body", async () => {
    render(
      <FileCard name="ALIC.rnx" empty="No file selected">
        <p>Card body</p>
      </FileCard>,
    );
    const name = screen.getByRole("button", { name: "ALIC.rnx" });
    expect(name).toHaveAttribute("aria-expanded", "true");
    expect(screen.getByText("File")).toBeInTheDocument();
    expect(screen.getByText("Card body")).toBeInTheDocument();

    await userEvent.click(name);
    expect(name).toHaveAttribute("aria-expanded", "false");
    expect(screen.queryByText("Card body")).not.toBeInTheDocument();

    await userEvent.click(name);
    expect(name).toHaveAttribute("aria-expanded", "true");
    expect(screen.getByText("Card body")).toBeInTheDocument();
  });

  it("toggles the body when the File type label is clicked", async () => {
    render(
      <FileCard name="ALIC.rnx" empty="No file selected">
        <p>Card body</p>
      </FileCard>,
    );
    const name = screen.getByRole("button", { name: "ALIC.rnx" });
    const label = screen.getByText("File");

    await userEvent.click(label);
    expect(name).toHaveAttribute("aria-expanded", "false");
    expect(screen.queryByText("Card body")).not.toBeInTheDocument();

    await userEvent.click(label);
    expect(name).toHaveAttribute("aria-expanded", "true");
    expect(screen.getByText("Card body")).toBeInTheDocument();
  });

  it("shows the placeholder, marked as empty, when no name is given", () => {
    render(
      <FileCard name={null} empty="No file selected">
        <p>Card body</p>
      </FileCard>,
    );
    expect(screen.getByText("No file selected")).toHaveClass("is-empty");
  });
});

describe("FileCard remove", () => {
  const card = (onRemove?: () => void) => (
    <FileCard name="ALIC.rnx" empty="No file selected" onRemove={onRemove}>
      <p>Card body</p>
    </FileCard>
  );

  it("shows the remove button only when given onRemove", () => {
    const { rerender } = render(card());
    expect(screen.queryByRole("button", { name: "Remove this file" })).not.toBeInTheDocument();

    rerender(card(() => {}));
    expect(screen.getByRole("button", { name: "Remove this file" })).toBeInTheDocument();
  });

  it("calls onRemove once when the remove button is pressed", async () => {
    const onRemove = vi.fn();
    render(card(onRemove));

    await userEvent.click(screen.getByRole("button", { name: "Remove this file" }));

    expect(onRemove).toHaveBeenCalledOnce();
  });

  it("calls onRemove once from the right-click Remove product item", async () => {
    const onRemove = vi.fn();
    render(card(onRemove));

    fireEvent.contextMenu(screen.getByRole("button", { name: "ALIC.rnx" }));
    await userEvent.click(screen.getByRole("menuitem", { name: "Remove product" }));

    expect(onRemove).toHaveBeenCalledOnce();
    expect(screen.queryByRole("menu")).not.toBeInTheDocument();
  });

  it("opens no context menu when the card cannot be removed", () => {
    render(card());

    fireEvent.contextMenu(screen.getByRole("button", { name: "ALIC.rnx" }));

    expect(screen.queryByRole("menu")).not.toBeInTheDocument();
  });
});
