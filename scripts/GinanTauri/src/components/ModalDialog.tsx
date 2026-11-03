import {
  useEffect,
  useId,
  useRef,
  useState,
  type FormEvent,
  type KeyboardEvent,
  type ReactNode,
  type RefObject,
} from "react";
import { createPortal } from "react-dom";

/**
 * Prop-driven dialogs that replace window.confirm / window.prompt.
 *
 * Mount = open: render `{open && <ConfirmDialog ... />}`; unmounting closes it.
 * Focus: the element focused before mount is restored on unmount. Confirm
 * starts on Cancel (the safe option); Prompt starts on its input with the
 * text selected. Escape cancels, Tab wraps inside the dialog, and a mousedown
 * on the backdrop cancels.
 */

export interface ConfirmDialogProps {
  title: string;
  message: ReactNode;
  confirmLabel?: string; // default "Confirm"
  cancelLabel?: string; // default "Cancel"
  danger?: boolean; // destructive tone for the confirm button
  onConfirm: () => void;
  onCancel: () => void;
}

export interface PromptDialogProps {
  title: string;
  label: string; // visible label of the text input
  initialValue?: string; // default ""
  submitLabel?: string; // default "OK"
  cancelLabel?: string; // default "Cancel"
  /** Returns an error message, or null when the (trimmed) value is acceptable. */
  validate?: (value: string) => string | null;
  onSubmit: (value: string) => void; // called with the trimmed value
  onCancel: () => void;
}

interface DialogShellProps {
  title: string;
  alertDialog?: boolean;
  descId?: string;
  initialFocusRef: RefObject<HTMLElement | null>;
  onCancel: () => void;
  children: ReactNode;
}

/** Shared overlay, card, focus management and keyboard handling. */
function DialogShell({ title, alertDialog, descId, initialFocusRef, onCancel, children }: DialogShellProps) {
  const titleId = useId();
  // Captured on the first render, before the focus effect moves focus.
  const returnFocusRef = useRef<Element | null>(document.activeElement);

  useEffect(() => {
    const target = initialFocusRef.current;
    target?.focus();
    if (target instanceof HTMLInputElement) target.select();
    const returnTo = returnFocusRef.current;
    return () => {
      if (returnTo instanceof HTMLElement && returnTo.isConnected) returnTo.focus();
    };
  }, [initialFocusRef]);

  const handleKeyDown = (e: KeyboardEvent<HTMLDivElement>) => {
    if (e.key === "Escape") {
      e.preventDefault();
      e.stopPropagation();
      onCancel();
      return;
    }
    if (e.key !== "Tab") return;

    const card = e.currentTarget;
    const items = Array.from(card.querySelectorAll<HTMLElement>("button:not(:disabled), input:not(:disabled)"));
    if (items.length === 0) {
      e.preventDefault();
      return;
    }
    const first = items[0];
    const last = items[items.length - 1];
    const active = document.activeElement;
    const outside = !card.contains(active);
    if (e.shiftKey && (active === first || outside)) {
      e.preventDefault();
      last.focus();
    } else if (!e.shiftKey && (active === last || outside)) {
      e.preventDefault();
      first.focus();
    }
  };

  return createPortal(
    <div className="modal-overlay" onMouseDown={(e) => { if (e.target === e.currentTarget) onCancel(); }}>
      <div
        className="modal-card"
        role={alertDialog ? "alertdialog" : "dialog"}
        aria-modal="true"
        aria-labelledby={titleId}
        aria-describedby={descId}
        onKeyDown={handleKeyDown}
      >
        <h2 id={titleId}>{title}</h2>
        {children}
      </div>
    </div>,
    document.body,
  );
}

/** Yes/no dialog. Cancel is focused on open; Enter activates the focused button. */
export function ConfirmDialog({
  title,
  message,
  confirmLabel = "Confirm",
  cancelLabel = "Cancel",
  danger,
  onConfirm,
  onCancel,
}: ConfirmDialogProps) {
  const descId = useId();
  const cancelRef = useRef<HTMLButtonElement>(null);
  return (
    <DialogShell title={title} alertDialog descId={descId} initialFocusRef={cancelRef} onCancel={onCancel}>
      <p className="modal-desc" id={descId}>{message}</p>
      <div className="modal-actions">
        <button type="button" className="btn-ghost" ref={cancelRef} onClick={onCancel}>
          {cancelLabel}
        </button>
        <button type="button" className={danger ? "btn-primary modal-btn-danger" : "btn-primary"} onClick={onConfirm}>
          {confirmLabel}
        </button>
      </div>
    </DialogShell>
  );
}

/** Single text-input dialog. Submit is disabled until the trimmed value is non-empty and valid. */
export function PromptDialog({
  title,
  label,
  initialValue = "",
  submitLabel = "OK",
  cancelLabel = "Cancel",
  validate,
  onSubmit,
  onCancel,
}: PromptDialogProps) {
  const [value, setValue] = useState(initialValue);
  const inputId = useId();
  const errorId = useId();
  const inputRef = useRef<HTMLInputElement>(null);

  const trimmed = value.trim();
  const error = trimmed ? (validate?.(trimmed) ?? null) : null;
  const canSubmit = trimmed !== "" && !error;

  const handleSubmit = (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (canSubmit) onSubmit(trimmed);
  };

  return (
    <DialogShell title={title} initialFocusRef={inputRef} onCancel={onCancel}>
      <form onSubmit={handleSubmit}>
        <div className="field-row">
          <label htmlFor={inputId}>{label}</label>
          <input
            id={inputId}
            ref={inputRef}
            type="text"
            value={value}
            onChange={(e) => setValue(e.target.value)}
            aria-invalid={!!error}
            aria-describedby={error ? errorId : undefined}
            autoComplete="off"
            spellCheck={false}
          />
        </div>
        {error && <p className="modal-field-error" id={errorId} role="alert">{error}</p>}
        <div className="modal-actions">
          <button type="button" className="btn-ghost" onClick={onCancel}>
            {cancelLabel}
          </button>
          <button type="submit" className="btn-primary" disabled={!canSubmit}>
            {submitLabel}
          </button>
        </div>
      </form>
    </DialogShell>
  );
}
