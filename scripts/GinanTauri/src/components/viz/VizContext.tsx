import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useId,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from "react";
import { invoke } from "@tauri-apps/api/core";
import type { PosFileInfo, PosSeries } from "../../types";
import { nearestIndex } from "../chartUtils";

/**
 * Shared state for the tiled Visualization tab.
 *
 * Two contexts, split by how often they change:
 *
 * - **Data** — the `.pos` files in the output directory, the solution chosen
 *   in the toolbar, and a cache of any other file a panel has been pointed at.
 *   Changes on a load or a refresh.
 * - **Link** — the time window and hover time the time-based panels share
 *   when "Link time axes" is on. Changes on every pointer move, so it is kept
 *   out of the data context to avoid re-rendering panels that don't care.
 */

// ── Data ─────────────────────────────────────────────────────────────────────

export interface SeriesEntry {
  series: PosSeries | null;
  loading: boolean;
  error: string | null;
}

interface VizData {
  files: PosFileInfo[];
  /** The solution picked in the toolbar; what a panel shows by default. */
  primaryPath: string | null;
  primary: SeriesEntry;
  /** Loaded entries for files other than the primary, keyed by path. */
  extra: Record<string, SeriesEntry>;
  ensure: (path: string) => void;
}

const VizDataContext = createContext<VizData | null>(null);

const EMPTY: SeriesEntry = { series: null, loading: false, error: null };

export function VizDataProvider({
  files,
  primaryPath,
  primary,
  reloadKey,
  children,
}: {
  files: PosFileInfo[];
  primaryPath: string | null;
  primary: SeriesEntry;
  /** Changes whenever files on disk may have changed (a run finishing). */
  reloadKey: number;
  children: ReactNode;
}) {
  const [extra, setExtra] = useState<Record<string, SeriesEntry>>({});
  const inflight = useRef(new Set<string>());

  // Anything cached may now be stale; panels re-request what they still show.
  useEffect(() => {
    setExtra({});
    inflight.current.clear();
  }, [reloadKey]);

  const ensure = useCallback(
    (path: string) => {
      if (inflight.current.has(path)) return;
      inflight.current.add(path);
      setExtra((prev) => ({ ...prev, [path]: { series: prev[path]?.series ?? null, loading: true, error: null } }));
      invoke<PosSeries>("load_pos_series", { path, maxPoints: null })
        .then((series) => setExtra((prev) => ({ ...prev, [path]: { series, loading: false, error: null } })))
        .catch((e) => setExtra((prev) => ({ ...prev, [path]: { series: null, loading: false, error: String(e) } })));
    },
    [],
  );

  const value = useMemo<VizData>(
    () => ({ files, primaryPath, primary, extra, ensure }),
    [files, primaryPath, primary, extra, ensure],
  );
  return <VizDataContext.Provider value={value}>{children}</VizDataContext.Provider>;
}

export function useVizData(): VizData {
  const ctx = useContext(VizDataContext);
  if (!ctx) throw new Error("useVizData outside VizDataProvider");
  return ctx;
}

/**
 * The series a panel draws: the toolbar's solution, unless the panel has been
 * pinned to a specific file (so smoothed and forward solutions, or two runs,
 * can sit side by side). A pinned file that has since disappeared falls back
 * to the toolbar's choice instead of leaving the panel stuck on an error.
 */
export function usePanelSeries(source: string | undefined): SeriesEntry & { pinned: boolean } {
  const { files, primaryPath, primary, extra, ensure } = useVizData();
  const pinned = !!source && source !== primaryPath && files.some((f) => f.path === source);

  useEffect(() => {
    if (pinned && source && !extra[source]) ensure(source);
  }, [pinned, source, extra, ensure]);

  if (!pinned || !source) return { ...primary, pinned: false };
  return { ...(extra[source] ?? { ...EMPTY, loading: true }), pinned: true };
}

// ── Linked time axes ─────────────────────────────────────────────────────────

type TimeWindow = [number, number] | null;

interface VizLink {
  linked: boolean;
  setLinked: (v: boolean) => void;
  view: TimeWindow;
  setView: (v: TimeWindow) => void;
  hover: LinkedHover | null;
  setHover: (h: LinkedHover | null) => void;
}

interface LinkedHover {
  t: number;
  source: string;
}

const VizLinkContext = createContext<VizLink | null>(null);

export function VizLinkProvider({
  linked,
  setLinked,
  resetKey,
  children,
}: {
  linked: boolean;
  setLinked: (v: boolean) => void;
  /** A new primary file is a new time axis — drop the shared zoom. */
  resetKey: string | null;
  children: ReactNode;
}) {
  const [view, setView] = useState<TimeWindow>(null);
  const [hover, setHoverState] = useState<LinkedHover | null>(null);
  // Keep the previous object when nothing changed, so a pointer move that stays
  // on the same epoch does not re-render every linked panel.
  const setHover = useCallback((h: LinkedHover | null) => {
    setHoverState((prev) => (prev?.t === h?.t && prev?.source === h?.source ? prev : h));
  }, []);

  useEffect(() => {
    setView(null);
    setHover(null);
  }, [resetKey]);

  const value = useMemo<VizLink>(
    () => ({ linked, setLinked, view, setView, hover, setHover }),
    [linked, setLinked, view, hover, setHover],
  );
  return <VizLinkContext.Provider value={value}>{children}</VizLinkContext.Provider>;
}

export function useVizLink(): VizLink | null {
  return useContext(VizLinkContext);
}

// ── Selected time ────────────────────────────────────────────────────────────

/**
 * The epoch picked on the timeline scrubber (or by clicking a chart). Unlike
 * hover it persists, and it applies whether or not the time axes are linked:
 * it is the "where am I in the run" cursor every panel marks.
 *
 * Kept in its own context so moving the pointer (hover) doesn't re-render the
 * scrubber, and scrubbing doesn't churn the hover state.
 */
interface VizSelection {
  selectedT: number | null;
  setSelectedT: (t: number | null) => void;
}

const VizSelectionContext = createContext<VizSelection | null>(null);

export function VizSelectionProvider({ resetKey, children }: { resetKey: string | null; children: ReactNode }) {
  const [selectedT, setSelectedT] = useState<number | null>(null);
  useEffect(() => setSelectedT(null), [resetKey]);
  const value = useMemo(() => ({ selectedT, setSelectedT }), [selectedT]);
  return <VizSelectionContext.Provider value={value}>{children}</VizSelectionContext.Provider>;
}

const NO_SELECTION: VizSelection = { selectedT: null, setSelectedT: () => {} };

export function useSelectedTime(): VizSelection {
  return useContext(VizSelectionContext) ?? NO_SELECTION;
}

/** Index of the selected epoch in `t`, or null if nothing is selected or it falls outside this series. */
export function useSelectedIndex(t: number[]): number | null {
  const { selectedT } = useSelectedTime();
  return useMemo(() => {
    if (selectedT === null || t.length === 0) return null;
    if (selectedT < t[0] || selectedT > t[t.length - 1]) return null;
    return nearestIndex(t, selectedT);
  }, [selectedT, t]);
}

/**
 * Zoom window and hover time for one chart. With linking on, both live in the
 * shared context, so zooming one time axis zooms them all and the crosshair
 * follows the pointer across panels; with it off, each chart keeps its own.
 *
 * `hoverIsMine` tells a chart whether the pointer is actually over it — only
 * that chart draws the tooltip, the rest just draw the crosshair.
 */
export function useTimeLink(resetKey: string) {
  const ctx = useContext(VizLinkContext);
  const id = useId();
  const [localView, setLocalView] = useState<TimeWindow>(null);
  const [localHover, setLocalHover] = useState<number | null>(null);

  useEffect(() => {
    setLocalView(null);
    setLocalHover(null);
  }, [resetKey]);

  const linked = !!ctx?.linked;
  const ctxSetHover = ctx?.setHover;
  const setHoverT = useCallback(
    (t: number | null) => {
      if (linked && ctxSetHover) ctxSetHover(t === null ? null : { t, source: id });
      else setLocalHover(t);
    },
    [linked, ctxSetHover, id],
  );

  if (linked && ctx) {
    return {
      view: ctx.view,
      setView: ctx.setView,
      hoverT: ctx.hover?.t ?? null,
      hoverIsMine: ctx.hover?.source === id,
      setHoverT,
    };
  }
  return { view: localView, setView: setLocalView, hoverT: localHover, hoverIsMine: true, setHoverT };
}

/**
 * Just the shared hover time, for panels that aren't on a time axis (the
 * scatter, the map) but can still highlight the epoch being pointed at
 * elsewhere.
 */
export function useLinkedHover() {
  const ctx = useContext(VizLinkContext);
  const id = useId();
  const linked = !!ctx?.linked;
  const ctxSetHover = ctx?.setHover;
  const setHoverT = useCallback(
    (t: number | null) => {
      if (linked && ctxSetHover) ctxSetHover(t === null ? null : { t, source: id });
    },
    [linked, ctxSetHover, id],
  );
  return {
    hoverT: linked ? (ctx?.hover?.t ?? null) : null,
    hoverIsMine: linked ? ctx?.hover?.source === id : true,
    setHoverT,
  };
}
