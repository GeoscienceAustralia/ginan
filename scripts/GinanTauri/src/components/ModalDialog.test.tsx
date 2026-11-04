import { describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { useState, type ReactNode } from "react";
import { ConfirmDialog, PromptDialog, type ConfirmDialogProps, type PromptDialogProps } from "./ModalDialog";

/** Mount = open: a button that mounts the dialog from `children` until it closes itself. */
function Opener({ children }: { children: (close: () => void) => ReactNode }) {
  const [open, setOpen] = useState(false);
  return (
    <>
      <button type="button" onClick={() => setOpen(true)}>
        Open
      </button>
      {open && children(() => setOpen(false))}
    </>
  );
}

function renderConfirm(overrides: Partial<ConfirmDialogProps> = {}) {
  const props: ConfirmDialogProps = {
    title: "Delete product?",
    message: "This cannot be undone.",
    onConfirm: vi.fn(),
    onCancel: vi.fn(),
    ...overrides,
  };
  const view = render(<ConfirmDialog {...props} />);
  return { props, ...view };
}

function renderPrompt(overrides: Partial<PromptDialogProps> = {}) {
  const props: PromptDialogProps = {
    title: "Save as template",
    label: "Template name",
    onSubmit: vi.fn(),
    onCancel: vi.fn(),
    ...overrides,
  };
  const view = render(<PromptDialog {...props} />);
  return { props, ...view };
}

const getAlertDialog = () => screen.getByRole("alertdialog", { name: "Delete product?" });
const getPromptInput = () => screen.getByRole("textbox", { name: "Template name" }) as HTMLInputElement;
const getSubmit = () => screen.getByRole("button", { name: "OK" });

describe("ConfirmDialog", () => {
  it("renders an alertdialog named by the title, modal, portalled to document.body", () => {
    const { container } = renderConfirm();
    const dialog = getAlertDialog();
    expect(dialog).toHaveAttribute("aria-modal", "true");
    expect(screen.getByText("This cannot be undone.")).toBeVisible();
    // The overlay is the portal root's direct child; the card sits inside it.
    expect(dialog.parentElement?.parentElement).toBe(document.body);
    expect(container.contains(dialog)).toBe(false);
  });

  it("uses default labels and respects custom ones", () => {
    const { unmount } = renderConfirm();
    expect(screen.getByRole("button", { name: "Confirm" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Cancel" })).toBeInTheDocument();
    unmount();

    renderConfirm({ confirmLabel: "Delete", cancelLabel: "Keep" });
    expect(screen.getByRole("button", { name: "Delete" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Keep" })).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: "Confirm" })).not.toBeInTheDocument();
  });

  it("focuses Cancel on open", () => {
    renderConfirm();
    expect(screen.getByRole("button", { name: "Cancel" })).toHaveFocus();
  });

  it("clicking Confirm calls onConfirm once only; clicking Cancel calls onCancel once only", async () => {
    const user = userEvent.setup();
    const { props } = renderConfirm();
    await user.click(screen.getByRole("button", { name: "Confirm" }));
    expect(props.onConfirm).toHaveBeenCalledTimes(1);
    expect(props.onCancel).not.toHaveBeenCalled();

    await user.click(screen.getByRole("button", { name: "Cancel" }));
    expect(props.onCancel).toHaveBeenCalledTimes(1);
    expect(props.onConfirm).toHaveBeenCalledTimes(1);
  });

  it("Escape calls onCancel", async () => {
    const user = userEvent.setup();
    const { props } = renderConfirm();
    await user.keyboard("{Escape}");
    expect(props.onCancel).toHaveBeenCalledTimes(1);
    expect(props.onConfirm).not.toHaveBeenCalled();
  });

  it("danger adds modal-btn-danger to the confirm button only", () => {
    const { unmount } = renderConfirm({ confirmLabel: "Delete", danger: true });
    expect(screen.getByRole("button", { name: "Delete" })).toHaveClass("modal-btn-danger");
    unmount();

    renderConfirm({ confirmLabel: "Delete" });
    expect(screen.getByRole("button", { name: "Delete" })).not.toHaveClass("modal-btn-danger");
  });

  it("traps Tab inside the dialog in both directions", async () => {
    const user = userEvent.setup();
    renderConfirm();
    const cancel = screen.getByRole("button", { name: "Cancel" });
    const confirm = screen.getByRole("button", { name: "Confirm" });

    expect(cancel).toHaveFocus();
    await user.tab();
    expect(confirm).toHaveFocus();
    await user.tab();
    expect(cancel).toHaveFocus();

    await user.tab({ shift: true });
    expect(confirm).toHaveFocus();
  });

  it("returns focus to the opener after Escape closes it", async () => {
    const user = userEvent.setup();
    const onCancel = vi.fn();
    render(
      <Opener>
        {(close) => (
          <ConfirmDialog
            title="Delete product?"
            message="This cannot be undone."
            onConfirm={vi.fn()}
            onCancel={() => {
              onCancel();
              close();
            }}
          />
        )}
      </Opener>,
    );
    const open = screen.getByRole("button", { name: "Open" });
    await user.click(open);
    expect(getAlertDialog()).toBeInTheDocument();

    await user.keyboard("{Escape}");
    expect(onCancel).toHaveBeenCalledTimes(1);
    expect(screen.queryByRole("alertdialog")).not.toBeInTheDocument();
    expect(open).toHaveFocus();
  });

  it("mousedown on the overlay calls onCancel; mousedown inside the card does not", async () => {
    const user = userEvent.setup();
    const { props } = renderConfirm();
    const overlay = getAlertDialog().parentElement as HTMLElement;

    await user.click(screen.getByText("This cannot be undone."));
    expect(props.onCancel).not.toHaveBeenCalled();

    await user.click(overlay);
    expect(props.onCancel).toHaveBeenCalledTimes(1);
  });
});

describe("PromptDialog", () => {
  it("renders a dialog named by the title with the input labelled, focused, and its value selected", () => {
    renderPrompt({ initialValue: "Starter" });
    expect(screen.getByRole("dialog", { name: "Save as template" })).toBeInTheDocument();
    const input = getPromptInput();
    expect(input).toHaveValue("Starter");
    expect(input).toHaveFocus();
    expect(input.selectionStart).toBe(0);
    expect(input.selectionEnd).toBe("Starter".length);
  });

  it("disables submit while the input is empty or whitespace, and enables it once text is typed", async () => {
    const user = userEvent.setup();
    renderPrompt();
    expect(getSubmit()).toBeDisabled();

    await user.type(getPromptInput(), "   ");
    expect(getSubmit()).toBeDisabled();

    await user.type(getPromptInput(), "a");
    expect(getSubmit()).toBeEnabled();
  });

  it("Enter submits once with the trimmed value", async () => {
    const user = userEvent.setup();
    const { props } = renderPrompt();
    await user.type(getPromptInput(), "  My template  {Enter}");
    expect(props.onSubmit).toHaveBeenCalledTimes(1);
    expect(props.onSubmit).toHaveBeenCalledWith("My template");
  });

  it("Enter with an empty input does not submit", async () => {
    const user = userEvent.setup();
    const { props } = renderPrompt();
    await user.click(getPromptInput());
    await user.keyboard("{Enter}");
    expect(props.onSubmit).not.toHaveBeenCalled();
  });

  it("shows validate errors, marks the input invalid, blocks submit, and clears on a valid value", async () => {
    const user = userEvent.setup();
    const validate = vi.fn((value: string) => (value === "dup" ? "Name already used" : null));
    const { props } = renderPrompt({ validate });
    const input = getPromptInput();

    await user.type(input, "  dup  ");
    const alert = screen.getByRole("alert");
    expect(alert).toHaveTextContent("Name already used");
    expect(input).toHaveAttribute("aria-invalid", "true");
    expect(input).toHaveAttribute("aria-describedby", alert.id);
    expect(getSubmit()).toBeDisabled();

    await user.keyboard("{Enter}");
    expect(props.onSubmit).not.toHaveBeenCalled();

    // validate only ever sees the trimmed value.
    expect(validate).toHaveBeenCalledWith("dup");
    expect(validate).not.toHaveBeenCalledWith(expect.stringMatching(/^\s|\s$/));

    await user.clear(input);
    await user.type(input, "ok");
    expect(screen.queryByRole("alert")).not.toBeInTheDocument();
    expect(input).toHaveAttribute("aria-invalid", "false");
    expect(getSubmit()).toBeEnabled();
  });

  it("does not show a validate error for an empty or whitespace-only input", async () => {
    const user = userEvent.setup();
    renderPrompt({ validate: () => "Always bad" });
    expect(screen.queryByRole("alert")).not.toBeInTheDocument();

    await user.type(getPromptInput(), "   ");
    expect(screen.queryByRole("alert")).not.toBeInTheDocument();
    expect(getPromptInput()).toHaveAttribute("aria-invalid", "false");
    expect(getSubmit()).toBeDisabled();
  });

  it("Escape and the Cancel button both call onCancel", async () => {
    const user = userEvent.setup();
    const { props } = renderPrompt();
    await user.keyboard("{Escape}");
    expect(props.onCancel).toHaveBeenCalledTimes(1);

    await user.click(screen.getByRole("button", { name: "Cancel" }));
    expect(props.onCancel).toHaveBeenCalledTimes(2);
    expect(props.onSubmit).not.toHaveBeenCalled();
  });

  it("returns focus to the opener after Escape closes it", async () => {
    const user = userEvent.setup();
    render(
      <Opener>
        {(close) => (
          <PromptDialog
            title="Save as template"
            label="Template name"
            onSubmit={vi.fn()}
            onCancel={() => close()}
          />
        )}
      </Opener>,
    );
    const open = screen.getByRole("button", { name: "Open" });
    await user.click(open);
    expect(getPromptInput()).toHaveFocus();

    await user.keyboard("{Escape}");
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
    expect(open).toHaveFocus();
  });

  it("traps Tab: input -> Cancel -> OK -> input, and Shift+Tab from input wraps to OK", async () => {
    const user = userEvent.setup();
    renderPrompt();
    const input = getPromptInput();
    await user.type(input, "name");

    await user.tab();
    expect(screen.getByRole("button", { name: "Cancel" })).toHaveFocus();
    await user.tab();
    expect(getSubmit()).toHaveFocus();
    await user.tab();
    expect(input).toHaveFocus();

    await user.tab({ shift: true });
    expect(getSubmit()).toHaveFocus();
  });
});
