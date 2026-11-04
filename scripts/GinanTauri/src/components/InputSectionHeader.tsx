import { useEffect, useRef, useState, type KeyboardEvent, type ReactNode } from "react";
import type { InputSection, ProductType } from "../types";
import { PlusIcon } from "./icons";

export interface InputSectionHeaderProps {
  title: string;
  info?: ReactNode;
  /** Picks a product type from the plus menu. Without it there is no plus. */
  onAdd?: (type: ProductType) => void;
  /** Types that can be added to THIS section; the rest render greyed (disabled). */
  enabledTypes: ProductType[];
  /** Hover text on the greyed types. */
  disabledReason?: string;
  /** Observation never lists SBAS: it is a correction source only. */
  section: InputSection;
  /** The types the menu lists, when not every type for `section`. */
  types?: ProductType[];
  /** A group heading inside a section rather than the section's own. */
  group?: boolean;
}

const ENABLED_ITEM = '[role="menuitem"]:not(:disabled)';

const PRODUCT_TYPES: { type: ProductType; label: string }[] = [
  { type: "file", label: "File" },
  { type: "http", label: "HTTP" },
  { type: "ntrip", label: "NTRIP" },
  { type: "serial", label: "Serial" },
  { type: "sbas", label: "SBAS" },
];

function enabledItems(menu: HTMLElement | null): HTMLButtonElement[] {
  return Array.from(menu?.querySelectorAll<HTMLButtonElement>(ENABLED_ITEM) ?? []);
}

/**
 * Section heading for the input sidebar: the title, an optional info node
 * (usually a tooltip) right after it, and a square plus button on the right
 * that opens a menu of the product types.
 */
export default function InputSectionHeader({
  title,
  info,
  onAdd,
  enabledTypes,
  disabledReason,
  section,
  types,
  group,
}: InputSectionHeaderProps) {
  const [open, setOpen] = useState(false);
  const wrapRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLButtonElement>(null);
  const menuRef = useRef<HTMLDivElement>(null);

  // Move focus to the first enabled item when the menu opens.
  useEffect(() => {
    if (open) enabledItems(menuRef.current)[0]?.focus();
  }, [open]);

  // Close on a mousedown outside the wrap. The listener exists only while open.
  useEffect(() => {
    if (!open) return;
    const onMouseDown = (event: MouseEvent) => {
      if (!wrapRef.current?.contains(event.target as Node)) setOpen(false);
    };
    document.addEventListener("mousedown", onMouseDown);
    return () => document.removeEventListener("mousedown", onMouseDown);
  }, [open]);

  function closeToButton() {
    setOpen(false);
    buttonRef.current?.focus();
  }

  function handleKeyDown(event: KeyboardEvent<HTMLDivElement>) {
    if (!open) return;
    if (event.key === "Escape") {
      event.preventDefault();
      closeToButton();
      return;
    }
    if (event.key === "ArrowDown" || event.key === "ArrowUp") {
      // Arrows wrap over the enabled items, skipping the greyed ones.
      const items = enabledItems(menuRef.current);
      if (items.length === 0) return;
      event.preventDefault();
      const current = items.indexOf(document.activeElement as HTMLButtonElement);
      const step = event.key === "ArrowDown" ? 1 : -1;
      const next =
        current === -1
          ? (step === 1 ? 0 : items.length - 1)
          : (current + step + items.length) % items.length;
      items[next].focus();
    }
  }

  function handleAdd(type: ProductType) {
    onAdd?.(type);
    closeToButton();
  }

  const listed = PRODUCT_TYPES.filter(({ type }) =>
    types ? types.includes(type) : section === "corr" || type !== "sbas",
  );

  return (
    <div className={`input-section-header${group ? " is-group" : ""}`}>
      {group ? (
        <h4 className="input-section-title">{title}{info}</h4>
      ) : (
        <h3 className="input-section-title">{title}{info}</h3>
      )}
      {onAdd && (
      <div className="input-plus-wrap" ref={wrapRef} onKeyDown={handleKeyDown}>
        <button
          ref={buttonRef}
          type="button"
          className="input-plus-btn"
          aria-label={`Add to ${title}`}
          aria-haspopup="menu"
          aria-expanded={open}
          onClick={() => setOpen((value) => !value)}
        >
          <PlusIcon size={14} />
        </button>
        {open && (
          <div
            ref={menuRef}
            role="menu"
            className="input-plus-menu"
            aria-label={`${title} options`}
          >
            {listed.map(({ type, label }) => {
              const enabled = enabledTypes.includes(type);
              return (
                <button
                  key={type}
                  type="button"
                  role="menuitem"
                  className="input-plus-item"
                  disabled={!enabled}
                  title={enabled ? undefined : disabledReason}
                  onClick={enabled ? () => handleAdd(type) : undefined}
                >
                  <span className={`input-plus-dot input-plus-dot--${type}`} aria-hidden="true" />
                  <span className="input-plus-label">{label}</span>
                </button>
              );
            })}
          </div>
        )}
      </div>
      )}
    </div>
  );
}
