import { useEffect, useId, useRef, useState } from "react";
import { useAppStore } from "../store/appStore";
import { AlertIcon } from "./icons";
import type { BlockerFix, RunBlocker } from "./runGate";

/**
 * Sits beside a disabled Process button and lists everything stopping the run,
 * each with a button that opens the setting that fixes it.
 *
 * A disabled button cannot take focus and its tooltip needs a hover, so
 * keyboard and touchpad users would have no way to learn why Process is greyed out.
 * This toggle is an ordinary focusable button, and the list stays open until
 * dismissed (Escape, a click outside, or following one of its fixes).
 */
export default function RunBlockers({ blockers }: { blockers: RunBlocker[] }) {
  const [open, setOpen] = useState(false);
  const rootRef = useRef<HTMLDivElement>(null);
  const toggleRef = useRef<HTMLButtonElement>(null);
  const listId = useId();
  const openSettings = useAppStore((s) => s.openSettings);
  const setConfigSection = useAppStore((s) => s.setConfigSection);

  useEffect(() => {
    if (!open) return;
    const onPointer = (e: PointerEvent) => {
      if (!rootRef.current?.contains(e.target as Node)) setOpen(false);
    };
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") {
        setOpen(false);
        toggleRef.current?.focus();
      }
    };
    window.addEventListener("pointerdown", onPointer);
    window.addEventListener("keydown", onKey);
    return () => {
      window.removeEventListener("pointerdown", onPointer);
      window.removeEventListener("keydown", onKey);
    };
  }, [open]);

  const follow = (fix: BlockerFix) => {
    setOpen(false);
    if (fix.kind === "settings") {
      openSettings(fix.page);
      return;
    }
    if (fix.kind === "tab") {
      useAppStore.getState().requestWorkspaceTab(fix.tab);
      return;
    }
    // A shut rail would hide the section being pointed at.
    if (useAppStore.getState().sidebarCollapsed) useAppStore.getState().toggleSidebar();
    setConfigSection(fix.section);
  };

  const count = blockers.length;
  const summary = `${count} ${count === 1 ? "thing" : "things"} to fix before processing`;

  return (
    <div className="run-blockers" ref={rootRef}>
      <button
        ref={toggleRef}
        type="button"
        className="btn-ghost run-blockers-toggle"
        aria-expanded={open}
        aria-controls={listId}
        aria-label={summary}
        title={summary}
        onClick={() => setOpen((o) => !o)}
      >
        <AlertIcon size={14} />
        {count}
        <span className="run-blockers-toggle-label"> to fix</span>
      </button>
      {open && (
        <div id={listId} className="run-blockers-popover" role="region" aria-label="Why Process is unavailable">
          <p className="run-blockers-title">Before you can process</p>
          <ul>
            {blockers.map((b) => (
              <li key={b.id}>
                <span>{b.message}</span>
                {b.fix && (
                  <button type="button" className="btn-ghost run-blockers-fix" onClick={() => follow(b.fix!)}>
                    {b.fix.label}
                  </button>
                )}
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
