import {
  Fragment,
  createContext,
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
  type ReactNode,
  type RefObject,
} from "react";
import { AlertIcon, CloseIcon, GripIcon, MaximizeIcon, MinimizeIcon, SlidersIcon, SplitColumnsIcon, SplitRowsIcon } from "../icons";
import { PANEL_KINDS, type DropSide, type LayoutNode, type PanelKind, type PanelNode, type PanelOptions, type SplitNode } from "./layout";
import { PANELS, unavailableReason } from "./panels";
import { usePanelSeries, useVizData } from "./VizContext";
import { posFileLabel } from "./files";
import { useTimeZone } from "../../hooks/useTimeZone";

/**
 * The tiling surface: renders a layout tree as nested flex rows/columns with
 * draggable dividers, and lets a tile be picked up by its header and dropped
 * on any edge of another tile (to split beside it), its centre (to swap), or
 * an outer edge of the whole surface (to span the full width or height).
 *
 * Dragging is done with pointer events rather than HTML5 drag-and-drop. The
 * Tauri window has native file-drop enabled for RINEX files, and on Windows
 * WebView2 that swallows HTML5 drag events inside the page.
 */

/** Target id meaning "the outer edge of the whole surface". */
export const ROOT_TARGET = "__root__";
/** How close to the surface edge (px) a drop counts as a full-span drop. */
const ROOT_EDGE_PX = 22;
/** Fraction of a tile's size from an edge that counts as that edge. */
const TILE_EDGE_FRACTION = 0.3;
/** Pointer travel before a press on a header becomes a drag. */
const DRAG_THRESHOLD_PX = 5;

export type DragPayload = { type: "move"; id: string } | { type: "new"; kind: PanelKind };
export interface DropTarget {
  id: string;
  side: DropSide;
}

interface DragState {
  payload: DragPayload;
  label: string;
  x: number;
  y: number;
  target: DropTarget | null;
}

export interface TilingActions {
  resize: (splitId: string, index: number, fraction: number) => void;
  setKind: (id: string, kind: PanelKind) => void;
  setOptions: (id: string, patch: PanelOptions) => void;
  splitPanel: (id: string, side: "right" | "bottom") => void;
  close: (id: string) => void;
  toggleMaximize: (id: string) => void;
  focus: (id: string) => void;
}

interface TilingCtx {
  actions: TilingActions;
  maximizedId: string | null;
  focusedId: string | null;
  drag: DragState | null;
  beginDrag: (e: React.PointerEvent, payload: DragPayload, label: string) => void;
}

const TilingContext = createContext<TilingCtx | null>(null);

function useTiling(): TilingCtx {
  const ctx = useContext(TilingContext);
  if (!ctx) throw new Error("useTiling outside TilingProvider");
  return ctx;
}

/** Starts a tile drag from outside the tree, e.g. the "Add view" menu. */
export function useTilingDrag(): TilingCtx["beginDrag"] {
  return useTiling().beginDrag;
}

// ── Drag controller ──────────────────────────────────────────────────────────

/**
 * Pointer-driven drag with a movement threshold, live hit-testing against the
 * tiles under the pointer, and Escape to cancel. Exposed via context so tile
 * headers and the toolbar's "Add view" menu start drags the same way.
 */
export function TilingProvider({
  surfaceRef,
  actions,
  maximizedId,
  focusedId,
  onDrop,
  children,
}: {
  surfaceRef: RefObject<HTMLElement>;
  actions: TilingActions;
  maximizedId: string | null;
  focusedId: string | null;
  onDrop: (payload: DragPayload, target: DropTarget) => void;
  children: ReactNode;
}) {
  const [drag, setDrag] = useState<DragState | null>(null);
  const onDropRef = useRef(onDrop);
  onDropRef.current = onDrop;

  const hitTest = useCallback(
    (x: number, y: number, payload: DragPayload): DropTarget | null => {
      const surface = surfaceRef.current;
      if (!surface) return null;
      const s = surface.getBoundingClientRect();
      if (x < s.left || x > s.right || y < s.top || y > s.bottom) return null;

      const edges: [DropSide, number][] = [
        ["left", x - s.left],
        ["right", s.right - x],
        ["top", y - s.top],
        ["bottom", s.bottom - y],
      ];
      const nearest = edges.reduce((a, b) => (b[1] < a[1] ? b : a));
      if (nearest[1] < ROOT_EDGE_PX) return { id: ROOT_TARGET, side: nearest[0] };

      const el = document.elementFromPoint(x, y)?.closest<HTMLElement>("[data-tile-id]");
      const id = el?.dataset.tileId;
      if (!el || !id) return null;
      if (payload.type === "move" && payload.id === id) return null;

      const r = el.getBoundingClientRect();
      const fx = (x - r.left) / r.width;
      const fy = (y - r.top) / r.height;
      const sides: [DropSide, number][] = [
        ["left", fx],
        ["right", 1 - fx],
        ["top", fy],
        ["bottom", 1 - fy],
      ];
      const best = sides.reduce((a, b) => (b[1] < a[1] ? b : a));
      return { id, side: best[1] < TILE_EDGE_FRACTION ? best[0] : "center" };
    },
    [surfaceRef],
  );

  const beginDrag = useCallback(
    (e: React.PointerEvent, payload: DragPayload, label: string) => {
      if (e.button !== 0) return;
      const startX = e.clientX;
      const startY = e.clientY;
      let active = false;
      let last: DragState | null = null;

      const move = (ev: PointerEvent) => {
        if (!active) {
          if (Math.hypot(ev.clientX - startX, ev.clientY - startY) < DRAG_THRESHOLD_PX) return;
          active = true;
          document.body.classList.add("viz-dragging");
        }
        ev.preventDefault();
        last = { payload, label, x: ev.clientX, y: ev.clientY, target: hitTest(ev.clientX, ev.clientY, payload) };
        setDrag(last);
      };
      const finish = (commit: boolean) => {
        window.removeEventListener("pointermove", move);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", cancel);
        window.removeEventListener("keydown", key, true);
        document.body.classList.remove("viz-dragging");
        if (commit && active && last?.target) onDropRef.current(last.payload, last.target);
        setDrag(null);
      };
      const up = () => finish(true);
      const cancel = () => finish(false);
      const key = (ev: KeyboardEvent) => {
        if (ev.key === "Escape") {
          ev.stopPropagation();
          finish(false);
        }
      };

      window.addEventListener("pointermove", move);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", cancel);
      window.addEventListener("keydown", key, true);
    },
    [hitTest],
  );

  return (
    <TilingContext.Provider value={{ actions, maximizedId, focusedId, drag, beginDrag }}>
      {children}
      {drag && <DragGhost drag={drag} />}
      {drag?.target?.id === ROOT_TARGET && <RootDropPreview surfaceRef={surfaceRef} side={drag.target.side} />}
    </TilingContext.Provider>
  );
}

function DragGhost({ drag }: { drag: DragState }) {
  const hint = !drag.target
    ? "Drop on a tile"
    : drag.target.id === ROOT_TARGET
      ? `Full ${drag.target.side === "left" || drag.target.side === "right" ? "height" : "width"}, ${drag.target.side}`
      : drag.target.side === "center"
        ? drag.payload.type === "move"
          ? "Swap"
          : "Replace"
        : `Split ${drag.target.side}`;
  return (
    <div className="viz-drag-ghost" style={{ left: drag.x + 12, top: drag.y + 12 }}>
      <strong>{drag.label}</strong>
      <span>{hint}</span>
    </div>
  );
}

function RootDropPreview({ surfaceRef, side }: { surfaceRef: RefObject<HTMLElement>; side: DropSide }) {
  const r = surfaceRef.current?.getBoundingClientRect();
  if (!r) return null;
  const third = 0.33;
  const style: React.CSSProperties =
    side === "left"
      ? { left: r.left, top: r.top, width: r.width * third, height: r.height }
      : side === "right"
        ? { left: r.right - r.width * third, top: r.top, width: r.width * third, height: r.height }
        : side === "top"
          ? { left: r.left, top: r.top, width: r.width, height: r.height * third }
          : { left: r.left, top: r.bottom - r.height * third, width: r.width, height: r.height * third };
  return <div className="viz-drop-preview viz-drop-preview--root" style={{ position: "fixed", ...style }} />;
}

// ── Tree ─────────────────────────────────────────────────────────────────────

export function TileNode({ node }: { node: LayoutNode }) {
  if (node.type === "panel") return <Tile node={node} />;
  return <Split node={node} />;
}

function Split({ node }: { node: SplitNode }) {
  return (
    <div className={`viz-split viz-split--${node.dir}`}>
      {node.children.map((child, i) => (
        <Fragment key={child.id}>
          {i > 0 && <Divider split={node} index={i - 1} />}
          <div className="viz-split-cell" style={{ flex: `${node.sizes[i]} 1 0px` }}>
            <TileNode node={child} />
          </div>
        </Fragment>
      ))}
    </div>
  );
}

function Divider({ split, index }: { split: SplitNode; index: number }) {
  const { actions } = useTiling();
  const [active, setActive] = useState(false);
  const horizontal = split.dir === "row";
  const current = split.sizes[index] / (split.sizes[index] + split.sizes[index + 1]);

  const onPointerDown = (e: React.PointerEvent<HTMLDivElement>) => {
    if (e.button !== 0) return;
    const el = e.currentTarget;
    const prev = el.previousElementSibling?.getBoundingClientRect();
    const next = el.nextElementSibling?.getBoundingClientRect();
    if (!prev || !next) return;
    e.preventDefault();
    const start = horizontal ? prev.left : prev.top;
    const end = horizontal ? next.right : next.bottom;
    try {
      el.setPointerCapture(e.pointerId);
    } catch {
      /* resize still works while the pointer stays over the divider */
    }
    setActive(true);
    document.body.classList.add(horizontal ? "viz-resizing-col" : "viz-resizing-row");

    const move = (ev: PointerEvent) => {
      const pos = horizontal ? ev.clientX : ev.clientY;
      actions.resize(split.id, index, (pos - start) / Math.max(1, end - start));
    };
    const up = () => {
      el.removeEventListener("pointermove", move);
      el.removeEventListener("pointerup", up);
      el.removeEventListener("pointercancel", up);
      document.body.classList.remove("viz-resizing-col", "viz-resizing-row");
      setActive(false);
    };
    el.addEventListener("pointermove", move);
    el.addEventListener("pointerup", up);
    el.addEventListener("pointercancel", up);
  };

  const onKeyDown = (e: React.KeyboardEvent) => {
    const back = horizontal ? "ArrowLeft" : "ArrowUp";
    const fwd = horizontal ? "ArrowRight" : "ArrowDown";
    if (e.key === back || e.key === fwd) {
      e.preventDefault();
      const step = e.shiftKey ? 0.1 : 0.02;
      actions.resize(split.id, index, current + (e.key === fwd ? step : -step));
    } else if (e.key === "Home" || e.key === "End") {
      e.preventDefault();
      actions.resize(split.id, index, e.key === "Home" ? 0 : 1);
    }
  };

  return (
    <div
      className={`viz-divider viz-divider--${horizontal ? "col" : "row"}${active ? " is-active" : ""}`}
      role="separator"
      aria-orientation={horizontal ? "vertical" : "horizontal"}
      aria-valuenow={Math.round(current * 100)}
      aria-valuemin={0}
      aria-valuemax={100}
      aria-label="Resize panels"
      tabIndex={0}
      title="Drag to resize · double-click to split evenly"
      onPointerDown={onPointerDown}
      onKeyDown={onKeyDown}
      onDoubleClick={() => actions.resize(split.id, index, 0.5)}
    />
  );
}

// ── Tile ─────────────────────────────────────────────────────────────────────

function Tile({ node }: { node: PanelNode }) {
  const { actions, maximizedId, focusedId, drag, beginDrag } = useTiling();
  const { files } = useVizData();
  const def = PANELS[node.kind];
  const source = typeof node.options.source === "string" ? node.options.source : undefined;
  const entry = usePanelSeries(source);
  const [settingsOpen, setSettingsOpen] = useState(false);
  const maximized = maximizedId === node.id;
  const dropSide = drag?.target?.id === node.id ? drag.target.side : null;
  const isDragSource = drag?.payload.type === "move" && drag.payload.id === node.id;

  const onHeaderPointerDown = (e: React.PointerEvent) => {
    // Controls inside the header keep their own clicks.
    if ((e.target as HTMLElement).closest("button, select, input, label, .viz-tile-popover")) return;
    beginDrag(e, { type: "move", id: node.id }, def.title);
  };

  const pinnedName = entry.pinned ? files.find((f) => f.path === source)?.name : null;
  const why = unavailableReason(def, entry.series);

  return (
    <section
      className={`viz-tile${focusedId === node.id ? " is-focused" : ""}${isDragSource ? " is-drag-source" : ""}`}
      data-tile-id={node.id}
      aria-label={def.title}
      onPointerDownCapture={() => actions.focus(node.id)}
    >
      <header className="viz-tile-header" onPointerDown={onHeaderPointerDown} title="Drag to move this panel">
        <GripIcon size={13} className="viz-tile-grip" />
        <select
          className="viz-tile-kind"
          value={node.kind}
          onChange={(e) => actions.setKind(node.id, e.target.value as PanelKind)}
          aria-label="Figure shown in this panel"
          title={def.description}
        >
          {PANEL_KINDS.map((k) => {
            // Figures this solution is too short to draw stay listed, so the
            // menu doesn't change shape as a live run grows, but can't be picked.
            const reason = k === node.kind ? null : unavailableReason(PANELS[k], entry.series);
            return (
              <option key={k} value={k} disabled={!!reason}>
                {PANELS[k].title}
                {reason ? ` (${reason})` : ""}
              </option>
            );
          })}
        </select>
        {pinnedName && (
          <span className="viz-badge viz-tile-source" title={`Pinned to ${pinnedName}`}>
            {pinnedName}
          </span>
        )}
        <div className="viz-tile-actions">
          <button
            className={`viz-icon-btn${settingsOpen ? " is-active" : ""}`}
            onClick={() => setSettingsOpen((v) => !v)}
            title="Panel settings"
            aria-label="Panel settings"
            aria-expanded={settingsOpen}
          >
            <SlidersIcon size={13} />
          </button>
          {!maximized && (
            <>
              <button className="viz-icon-btn viz-icon-btn--split" onClick={() => actions.splitPanel(node.id, "right")} title="Split right" aria-label="Split right">
                <SplitColumnsIcon size={13} />
              </button>
              <button className="viz-icon-btn viz-icon-btn--split" onClick={() => actions.splitPanel(node.id, "bottom")} title="Split down" aria-label="Split down">
                <SplitRowsIcon size={13} />
              </button>
            </>
          )}
          <button
            className="viz-icon-btn"
            onClick={() => actions.toggleMaximize(node.id)}
            title={maximized ? "Restore layout" : "Maximise"}
            aria-label={maximized ? "Restore layout" : "Maximise"}
          >
            {maximized ? <MinimizeIcon size={13} /> : <MaximizeIcon size={13} />}
          </button>
          <button className="viz-icon-btn viz-icon-btn--close" onClick={() => actions.close(node.id)} title="Close panel" aria-label="Close panel">
            <CloseIcon size={13} />
          </button>
        </div>
        {settingsOpen && (
          <SettingsPopover node={node} source={source} onClose={() => setSettingsOpen(false)} />
        )}
      </header>

      <div className="viz-tile-body">
        {entry.series && why ? (
          <div className="viz-state">
            {def.title}: {why}
            {why.startsWith("needs") && <>; this solution has {entry.series.t.length.toLocaleString()} so far</>}.
          </div>
        ) : entry.series ? (
          <def.Body series={entry.series} options={node.options} />
        ) : entry.error ? (
          <div className="viz-state viz-state--error">
            <AlertIcon size={15} /> {entry.error}
          </div>
        ) : entry.loading ? (
          <div className="viz-state">Reading...</div>
        ) : (
          <div className="viz-state">Select a solution file.</div>
        )}
        {entry.loading && entry.series && <div className="viz-tile-loading" aria-hidden="true" />}
      </div>

      {dropSide && <div className={`viz-drop-preview viz-drop-preview--${dropSide}`} />}
    </section>
  );
}

function SettingsPopover({ node, source, onClose }: { node: PanelNode; source: string | undefined; onClose: () => void }) {
  const timeZone = useTimeZone();
  const { actions } = useTiling();
  const { files, primaryPath } = useVizData();
  const def = PANELS[node.kind];
  const ref = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const onDown = (e: PointerEvent) => {
      const el = ref.current;
      const target = e.target as HTMLElement;
      // The toggle button owns its own click; don't close-then-reopen.
      if (el && !el.contains(target) && !target.closest(".viz-icon-btn.is-active")) onClose();
    };
    const onKey = (e: KeyboardEvent) => e.key === "Escape" && onClose();
    window.addEventListener("pointerdown", onDown, true);
    window.addEventListener("keydown", onKey);
    return () => {
      window.removeEventListener("pointerdown", onDown, true);
      window.removeEventListener("keydown", onKey);
    };
  }, [onClose]);

  const set = (patch: PanelOptions) => actions.setOptions(node.id, patch);
  const primaryName = files.find((f) => f.path === primaryPath)?.name;

  return (
    <div className="viz-tile-popover" ref={ref} role="dialog" aria-label={`${def.title} settings`}>
      <div className="viz-popover-title">{def.title}</div>
      <p className="viz-popover-desc">{def.description}</p>
      <label className="viz-opt-row">
        <span className="viz-opt-label">Solution</span>
        <select
          className="viz-select"
          value={source && files.some((f) => f.path === source) ? source : ""}
          onChange={(e) => set({ source: e.target.value || undefined })}
          title="Pin this panel to a file, e.g. to compare the smoothed and forward solutions side by side"
        >
          <option value="">Follow toolbar{primaryName ? ` (${primaryName})` : ""}</option>
          {files.map((f) => (
            <option key={f.path} value={f.path}>
              {posFileLabel(f, timeZone)}
            </option>
          ))}
        </select>
      </label>
      {def.Options && <def.Options options={node.options} set={set} />}
    </div>
  );
}
