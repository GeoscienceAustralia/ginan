import { useEffect, useRef, useState, type RefObject } from "react";
import { getCurrentWebview } from "@tauri-apps/api/webview";
import { useAppStore } from "../store/appStore";
import { looksLikeRinex } from "./usePickers";
import { WF } from "../config/workflowSteps";

/**
 * Drag-and-drop loading for the RINEX observation file.
 *
 * Uses Tauri's own drag-drop events rather than the HTML5 drop API: the
 * webview only ever sees a sandboxed `File`, never a path on disk, and the
 * whole load chain (`prepare_rinex_input`, `parse_rinex`) works on real paths.
 * Tauri's events carry the actual paths, which is the only reason this can
 * reuse `loadRinexFile` unchanged.
 *
 * Several files may be dropped at once: the first becomes the primary file and
 * the rest ride along with it, replacing whatever was open — the same outcome as
 * picking them in the dialog. Adding to the open files is the "Add files"
 * button's job, so a drop never grows a selection by surprise.
 *
 * Returns `isDraggingOver` so the drop target can highlight itself, and takes
 * the loader as an argument so the caller supplies the one from its own
 * `usePickers()` instance.
 */
export function useRinexDrop(
  loadRinexFile: (path: string, more?: string[]) => Promise<void>,
) {
  const [isDraggingOver, setIsDraggingOver] = useState(false);
  const setStatus = useAppStore((s) => s.setStatus);

  useEffect(() => {
    let unlisten: (() => void) | undefined;
    let cancelled = false;

    (async () => {
      const fn = await getCurrentWebview().onDragDropEvent((event) => {
        // A drop mid-run would swap the file out from under a running PEA
        // process; ignore it and leave the current run alone. Read at event
        // time so a run starting or stopping doesn't re-register the listener.
        if (useAppStore.getState().isProcessing) return;

        if (event.payload.type === "enter" || event.payload.type === "over") {
          setIsDraggingOver(true);
          return;
        }

        if (event.payload.type === "leave") {
          setIsDraggingOver(false);
          return;
        }

        if (event.payload.type === "drop") {
          setIsDraggingOver(false);
          const paths = event.payload.paths;
          if (paths.length === 0) return;

          const candidates = rinexCandidates(paths, setStatus);
          if (candidates.length === 0) return;
          void loadRinexFile(candidates[0], candidates.slice(1));
        }
      });

      if (cancelled) fn();
      else unlisten = fn;
    })();

    return () => {
      cancelled = true;
      unlisten?.();
    };
  }, [loadRinexFile, setStatus]);

  return { isDraggingOver };
}

/**
 * The RINEX files among `paths`. A non-RINEX file dropped alongside the rest
 * is left out, but the log says so — loading overwrites the status line
 * straight away, and a silent skip would look like a file that failed to load.
 */
function rinexCandidates(paths: string[], setStatus: (s: string) => void): string[] {
  const candidates = paths.filter(looksLikeRinex);
  if (candidates.length === 0) {
    const names = paths.map((p) => p.split(/[\\/]/).pop()).join(", ");
    setStatus(`Not a RINEX observation file: ${names}`);
    return [];
  }
  const ignored = paths.filter((p) => !looksLikeRinex(p));
  for (const p of ignored) {
    useAppStore
      .getState()
      .appendWorkflowLine(WF.warning(`Not a RINEX observation file, skipped: ${p}`));
  }
  return candidates;
}

/** Whether a drag position (physical pixels, from Tauri) is over `el`. */
function isOver(el: HTMLElement | null, pos: { x: number; y: number }): boolean {
  if (!el) return false;
  const rect = el.getBoundingClientRect();
  const scale = window.devicePixelRatio || 1;
  const x = pos.x / scale;
  const y = pos.y / scale;
  return x >= rect.left && x <= rect.right && y >= rect.top && y <= rect.bottom;
}

/**
 * A drop target for RINEX files on one element, such as the sidebar's
 * add-files zone. Unlike `useRinexDrop`, which takes a drop anywhere in the
 * window, this only acts on files released over `target`, and hands them to
 * `onFiles` rather than replacing the selection.
 *
 * Returns `isDraggingOver`, true while a drag hovers the element.
 */
export function useRinexDropTarget(
  target: RefObject<HTMLElement | null>,
  onFiles: (paths: string[]) => void,
) {
  const [isDraggingOver, setIsDraggingOver] = useState(false);
  const setStatus = useAppStore((s) => s.setStatus);
  // Read at drop time, so a new callback each render does not re-subscribe.
  const onFilesRef = useRef(onFiles);
  onFilesRef.current = onFiles;

  useEffect(() => {
    let unlisten: (() => void) | undefined;
    let cancelled = false;

    (async () => {
      const fn = await getCurrentWebview().onDragDropEvent((event) => {
        if (useAppStore.getState().isProcessing) return;
        const payload = event.payload;
        if (payload.type === "leave") {
          setIsDraggingOver(false);
          return;
        }
        const inside = isOver(target.current, payload.position);
        if (payload.type === "enter" || payload.type === "over") {
          setIsDraggingOver(inside);
          return;
        }
        if (payload.type === "drop") {
          setIsDraggingOver(false);
          if (!inside || payload.paths.length === 0) return;
          const candidates = rinexCandidates(payload.paths, setStatus);
          if (candidates.length > 0) onFilesRef.current(candidates);
        }
      });

      if (cancelled) fn();
      else unlisten = fn;
    })();

    return () => {
      cancelled = true;
      unlisten?.();
    };
  }, [target, setStatus]);

  return { isDraggingOver };
}
