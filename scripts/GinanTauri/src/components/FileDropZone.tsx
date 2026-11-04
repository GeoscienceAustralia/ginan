/**
 * Card shell and browse zone for a file-run input product. The header is the
 * same row as a live stream's (a type badge and the file name, with what kind
 * of file it is on hover) and collapses the card. Removing is kept out of the header, at
 * the foot of the open card (and on the header's right-click menu), so it is
 * never one slip away from the collapse control.
 */

import { useEffect, useRef, useState, type ReactNode } from "react";
import { ChevronRightIcon, TrashIcon } from "./icons";
import ProductContextMenu from "./ProductContextMenu";

/** Last path segment, so the header never shows a full path. */
export function fileBaseName(path: string): string {
  return path.split(/[\\/]/).pop() || path;
}

/**
 * Written out in the canvas order. FileDropZone.test.tsx checks it stays in
 * step with RINEX_EXTENSIONS in usePickers.
 */
export const RINEX_EXTENSION_HINT = ".rnx, .crx, .obs, .YYo, .YYd or .gz";

export function FileCard({
  name,
  empty,
  children,
  onRemove,
  detail = "File",
  icon = "file",
  collapsible = true,
  onBrowse,
}: {
  /** File name shown in the header; null falls back to `empty`. */
  name: string | null;
  empty: string;
  /** Second line under the name: what kind of file this is. */
  detail?: string;
  /** A single attached file, or a set of downloaded products. */
  icon?: "file" | "products";
  children: ReactNode;
  /** Adds the remove button and the right-click Remove item; omit for a card that cannot be removed. */
  onRemove?: () => void;
  /**
   * False for a source that is just a file: the row is the file and its
   * remove button, with whatever goes under it always shown. Only cards with
   * settings of their own (Corrections) open and close.
   */
  collapsible?: boolean;
  /**
   * With no file yet, the whole card is the drop box that browses for one,
   * instead of a "No file selected" row with the box under it. Static cards only.
   */
  onBrowse?: () => void;
}) {
  const [open, setOpen] = useState(true);
  // Collapse once a file is chosen (no name -> a name); a card that always has
  // a name, like Corrections, stays as the user left it.
  const hadName = useRef(name !== null);
  useEffect(() => {
    if (name !== null && !hadName.current) setOpen(false);
    hadName.current = name !== null;
  }, [name]);
  // Right-click menu: where it opened, and the bar to give focus back to.
  const [ctx, setCtx] = useState<{ x: number; y: number; bar: HTMLElement } | null>(null);
  const onContextMenu = onRemove
    ? (e: React.MouseEvent<HTMLDivElement>) => {
        e.preventDefault();
        setCtx({ x: e.clientX, y: e.clientY, bar: e.currentTarget });
      }
    : undefined;
  const menu = ctx && onRemove && (
    <ProductContextMenu
      x={ctx.x}
      y={ctx.y}
      returnFocusTo={ctx.bar}
      onRemove={onRemove}
      onClose={() => setCtx(null)}
    />
  );

  if (!collapsible && name === null && onBrowse) {
    return (
      <div className="file-card is-static is-empty" onContextMenu={onContextMenu}>
        <button type="button" className="file-drop-zone file-card-drop" onClick={onBrowse} title={detail}>
          <span className="file-drop-zone-title">Click to browse for a RINEX file</span>
          <span className="file-drop-zone-hint">{RINEX_EXTENSION_HINT}</span>
        </button>
        {onRemove && (
          <button
            type="button"
            className="btn-icon source-remove"
            onClick={onRemove}
            title="Remove this file"
            aria-label="Remove this file"
          >
            <TrashIcon size={14} />
          </button>
        )}
        {menu}
      </div>
    );
  }

  if (!collapsible) {
    return (
      <div className="file-card is-static">
        <div className="file-card-header" onContextMenu={onContextMenu}>
          <span className="file-card-badge" aria-hidden="true">
            {icon === "products" ? "Products" : "File"}
          </span>
          <span className={`file-card-name${name ? "" : " is-empty"}`} title={detail}>
            {name ?? empty}
          </span>
          {onRemove && (
            <button
              type="button"
              className="btn-icon source-remove"
              onClick={onRemove}
              title="Remove this file"
              aria-label="Remove this file"
            >
              <TrashIcon size={14} />
            </button>
          )}
        </div>
        <div className="file-card-body">{children}</div>
        {menu}
      </div>
    );
  }

  return (
    <div className="file-card">
      <div className="file-card-header" onContextMenu={onContextMenu}>
        <span className="file-card-type" aria-hidden="true" onClick={() => setOpen((o) => !o)}>
          <ChevronRightIcon size={14} className={`file-card-chevron${open ? " is-open" : ""}`} />
        </span>
        {/* Same row as a live stream: the type badge, then the name. */}
        <span className="file-card-badge" aria-hidden="true" onClick={() => setOpen((o) => !o)}>
          {icon === "products" ? "Products" : "File"}
        </span>
        <button
          type="button"
          className={`file-card-name${name ? "" : " is-empty"}`}
          aria-expanded={open}
          title={detail}
          onClick={() => setOpen((o) => !o)}
        >
          {name ?? empty}
        </button>
      </div>
      {open && (
        <div className="file-card-body">
          {children}
          {onRemove && (
            <button
              type="button"
              className="btn-icon source-remove"
              onClick={onRemove}
              title="Remove this file"
              aria-label="Remove this file"
            >
              <TrashIcon size={14} />
            </button>
          )}
        </div>
      )}
      {menu}
    </div>
  );
}

/** Dashed, full-width click-to-browse button for a RINEX observation file. */
export function BrowseZone({ onBrowse }: { onBrowse: () => void }) {
  return (
    <button type="button" className="file-drop-zone" onClick={onBrowse}>
      <span className="file-drop-zone-title">Click to browse for a RINEX file</span>
      <span className="file-drop-zone-hint">{RINEX_EXTENSION_HINT}</span>
    </button>
  );
}
