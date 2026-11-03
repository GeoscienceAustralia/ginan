import { useEffect, useLayoutEffect, useRef, useState } from "react";
import { createPortal } from "react-dom";

export interface ProductContextMenuProps {
  /** Pointer position (clientX / clientY) the menu opens at. */
  x: number;
  y: number;
  /** Element that gets focus back when the menu closes (the product bar). */
  returnFocusTo?: HTMLElement | null;
  onRemove: () => void;
  onClose: () => void;
}

const ENABLED_ITEM = '[role="menuitem"]:not(:disabled)';

function enabledItems(menu: HTMLElement | null): HTMLButtonElement[] {
  return Array.from(menu?.querySelectorAll<HTMLButtonElement>(ENABLED_ITEM) ?? []);
}

/**
 * Right-click menu for a product card bar. Rendered in a portal so the sidebar's
 * overflow and stacking never clip it; it closes on Escape, an outside mousedown,
 * window blur, or any scroll, and hands focus back to the bar on unmount.
 */
export default function ProductContextMenu({
  x,
  y,
  returnFocusTo,
  onRemove,
  onClose,
}: ProductContextMenuProps) {
  const menuRef = useRef<HTMLDivElement>(null);
  const [pos, setPos] = useState({ left: x, top: y });
  // Listeners are attached once per mount; they read the latest props through this ref.
  const latest = useRef({ onClose, returnFocusTo });
  latest.current = { onClose, returnFocusTo };

  // Keep the menu inside the viewport. jsdom reports 0x0, so tests keep x,y as-is.
  useLayoutEffect(() => {
    const el = menuRef.current;
    if (!el) return;
    const { width, height } = el.getBoundingClientRect();
    const left = x + width > window.innerWidth ? Math.max(4, window.innerWidth - width - 4) : x;
    const top = y + height > window.innerHeight ? Math.max(4, window.innerHeight - height - 4) : y;
    setPos({ left, top });
  }, [x, y]);

  useEffect(() => {
    enabledItems(menuRef.current)[0]?.focus();
  }, []);

  useEffect(() => {
    function onKeyDown(event: KeyboardEvent) {
      if (event.key === "Escape") {
        event.preventDefault();
        latest.current.onClose();
        return;
      }
      if (event.key === "ArrowDown" || event.key === "ArrowUp") {
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
    function onMouseDown(event: MouseEvent) {
      if (!menuRef.current?.contains(event.target as Node)) latest.current.onClose();
    }
    const onClosing = () => latest.current.onClose();

    document.addEventListener("keydown", onKeyDown);
    document.addEventListener("mousedown", onMouseDown);
    window.addEventListener("blur", onClosing);
    // Capture, so scrolling any scroll container (the sidebar included) closes the menu.
    window.addEventListener("scroll", onClosing, true);
    return () => {
      document.removeEventListener("keydown", onKeyDown);
      document.removeEventListener("mousedown", onMouseDown);
      window.removeEventListener("blur", onClosing);
      window.removeEventListener("scroll", onClosing, true);
    };
  }, []);

  useEffect(
    () => () => {
      latest.current.returnFocusTo?.focus();
    },
    [],
  );

  function handleRemove() {
    onRemove();
    onClose();
  }

  return createPortal(
    <div
      ref={menuRef}
      role="menu"
      aria-label="Product options"
      className="ctx-menu"
      style={{ left: pos.left, top: pos.top }}
    >
      <button type="button" role="menuitem" className="ctx-menu-item" onClick={handleRemove}>
        <span className="ctx-menu-label">Remove product</span>
      </button>
    </div>,
    document.body,
  );
}
