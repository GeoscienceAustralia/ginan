import { useCallback, useEffect, useMemo, useRef, useState, type ReactNode } from "react";
import { invoke } from "@tauri-apps/api/core";
import { listen } from "@tauri-apps/api/event";
import "leaflet/dist/leaflet.css";
import { resultsDir, useAppStore } from "../store/appStore";
import type { PosEpochEvent, PosFileInfo, PosSeries } from "../types";
import { ChevronDownIcon, LayoutIcon, LinkIcon, PlusIcon, ResetIcon } from "./icons";
import {
  DEFAULT_PRESET,
  PANEL_KINDS,
  PRESETS,
  addAtEdge,
  equalize,
  findPanel,
  insertBeside,
  movePanel,
  panel,
  parseLayout,
  removePanel,
  resizeSplit,
  updatePanel,
  updatePanelOptions,
  type LayoutNode,
  type PanelKind,
} from "./viz/layout";
import { PANELS, unavailableReason } from "./viz/panels";
import { ROOT_TARGET, TileNode, TilingProvider, useTilingDrag, type DragPayload, type DropTarget, type TilingActions } from "./viz/TilingLayout";
import { VizDataProvider, VizLinkProvider, VizSelectionProvider, useVizData, type SeriesEntry } from "./viz/VizContext";
import TimelineScrubber from "./viz/TimelineScrubber";
import { TabBarActions } from "./TabBarSlot";
import { pickDefaultPos, posFileLabel } from "./viz/files";
import VizEmpty from "./VizEmpty";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * The `.pos` half of the Visualization tab.
 *
 * The Python UI generated a Plotly HTML file per figure into `outputs/visual/`
 * and left the user to find and open them; nothing appeared in the app itself.
 * Here the `.pos` files PEA wrote are read directly and the figures are drawn
 * in the tab, so a finished run is one click from being looked at.
 *
 * The figures live in a tiling workspace: any number of panels, split side by
 * side or stacked, resized by their dividers, rearranged by dragging a panel's
 * header onto another panel's edge. Each panel picks its own figure and
 * settings, and can be pinned to a different `.pos` file than the toolbar's so
 * two solutions can be compared directly. With "Link time axes" on, zooming
 * or hovering any time-based panel moves all of them together. The layout is
 * remembered between sessions.
 *
 * Across the top, a timeline scrubber picks an epoch — by click, drag,
 * scroll, arrow keys or playback — and every panel marks it; clicking a
 * point in a chart picks it too.
 *
 * The view owns no run state: it lists whatever is in the output directory each
 * time it is opened, and `posEpoch` in the store nudges it to re-list when a
 * run finishes. That way it also works on output from a previous session. A
 * realtime run never "finishes" while it's streaming, so while one is active
 * this component also bumps `posEpoch` itself on a timer — see the polling
 * effect below — so the plot keeps growing without the user hitting Refresh.
 *
 * Once a `.pos` file is selected, a running realtime session is always live:
 * `start_pos_tail` tails that file on the backend and emits one `pos-epoch`
 * event per new line, which is appended straight into `series` state instead
 * of re-reading and re-decimating the whole file every tick. The timer only
 * covers the start of a run, before PEA has written a file to tail.
 */

// How often to re-read the selected .pos file while a realtime run is active.
// PEA writes at most one line per epoch (epoch_control.epoch_interval is >= 1s
// in every realtime config this UI generates), so polling faster than the
// write rate would only reload the same bytes.
const REALTIME_POLL_MS = 2000;

const LAYOUT_KEY = "ginan.viz.layout.v1";
const LINKED_KEY = "ginan.viz.linked";

function loadLayout(): LayoutNode | null {
  try {
    const raw = localStorage.getItem(LAYOUT_KEY);
    if (raw === null) return DEFAULT_PRESET.build();
    if (raw === "null") return null;
    return parseLayout(JSON.parse(raw)) ?? DEFAULT_PRESET.build();
  } catch {
    return DEFAULT_PRESET.build();
  }
}

function loadLinked(): boolean {
  try {
    return localStorage.getItem(LINKED_KEY) !== "false";
  } catch {
    return true;
  }
}

export default function PosSolutionView() {
  const timeZone = useTimeZone();
  const outputDir = useAppStore(resultsDir);
  // Bumped when PEA finishes, so a run that has just written new .pos files
  // refreshes the list without the user having to press anything.
  const posEpoch = useAppStore((s) => s.posEpoch);
  const bumpPosEpoch = useAppStore((s) => s.bumpPosEpoch);
  const inputMode = useAppStore((s) => s.inputMode);
  const isProcessing = useAppStore((s) => s.isProcessing);

  const [files, setFiles] = useState<PosFileInfo[] | null>(null);
  const [selected, setSelected] = useState<string | null>(null);
  const [series, setSeries] = useState<PosSeries | null>(null);
  const [listError, setListError] = useState<string | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const [layout, setLayout] = useState<LayoutNode | null>(loadLayout);
  const [linked, setLinked] = useState(loadLinked);
  const [maximizedId, setMaximizedId] = useState<string | null>(null);
  const [focusedId, setFocusedId] = useState<string | null>(null);
  const surfaceRef = useRef<HTMLDivElement>(null);

  /** Files with epochs in them. PEA also leaves empty or header-only files
   *  behind (the smoothed companion of a run, a run that never got going);
   *  offering those only leads to "nothing to plot" in every panel. */
  const usable = useMemo(() => (files ?? []).filter((f) => f.has_epochs), [files]);

  // Live updating is not optional in a realtime run: it is on
  // whenever PEA is running and there is a file to follow, and off otherwise,
  // so it can never be left on with nothing feeding it.
  const liveStream = inputMode === "realtime" && isProcessing && !!selected;

  useEffect(() => {
    try {
      localStorage.setItem(LAYOUT_KEY, JSON.stringify(layout));
    } catch {
      /* storage unavailable — the layout just won't survive a restart */
    }
  }, [layout]);

  useEffect(() => {
    try {
      localStorage.setItem(LINKED_KEY, String(linked));
    } catch {
      /* as above */
    }
  }, [linked]);

  const refresh = useCallback(async () => {
    if (!outputDir) {
      setFiles(null);
      setListError(null);
      return;
    }
    try {
      const found = await invoke<PosFileInfo[]>("list_pos_files", { outputDir });
      setFiles(found);
      setListError(null);
      // Keep the current selection across a refresh when it is still there —
      // re-running the same config rewrites the same filenames, and the user
      // should not be bounced back to the first file each time.
      const withEpochs = found.filter((f) => f.has_epochs);
      setSelected((prev) => {
        if (prev && withEpochs.some((f) => f.path === prev)) return prev;
        return pickDefaultPos(withEpochs);
      });
    } catch (e) {
      setFiles(null);
      setListError(String(e));
    }
  }, [outputDir]);

  useEffect(() => {
    refresh();
  }, [refresh, posEpoch]);

  // Realtime streams indefinitely and never fires pea-finished until the user
  // stops it, so nothing else nudges posEpoch while it's running. Poll here
  // instead; the effect tears itself down the moment the run isn't realtime
  // or isn't processing, and the final pea-finished/pea-error bump (fired
  // even when the user stops the run) still delivers one last refresh.
  // Stops once a file is selected and live tailing takes over: `pos-epoch`
  // events keep `series` current then, and re-reading the whole file on top
  // would both waste the read and reset any incremental chart state.
  useEffect(() => {
    if (inputMode !== "realtime" || !isProcessing || liveStream) return;
    const id = setInterval(bumpPosEpoch, REALTIME_POLL_MS);
    return () => clearInterval(id);
  }, [inputMode, isProcessing, liveStream, bumpPosEpoch]);

  const appendEpoch = useCallback((path: string, e: PosEpochEvent) => {
    setSeries((prev) => {
      if (!prev || prev.path !== path) return prev;
      return {
        ...prev,
        total_epochs: prev.total_epochs + 1,
        returned_epochs: prev.returned_epochs + 1,
        t: [...prev.t, e.t],
        dn: [...prev.dn, e.dn],
        de: [...prev.de, e.de],
        du: [...prev.du, e.du],
        sn: [...prev.sn, e.sn],
        se: [...prev.se, e.se],
        su: [...prev.su, e.su],
        lat: [...prev.lat, e.lat],
        lon: [...prev.lon, e.lon],
        hpl: e.hpl == null ? prev.hpl : [...prev.hpl, e.hpl],
        vpl: e.vpl == null ? prev.vpl : [...prev.vpl, e.vpl],
      };
    });
  }, []);

  // Tail the selected .pos file on the backend and append each new epoch as
  // it's emitted. Starting a new tail (a different file, or a new run) is
  // enough to retire the previous one — see PosTailGen — so the only cleanup
  // needed here is an explicit stop on unmount or when the run ends, so a
  // background tail doesn't keep running once nothing is listening.
  useEffect(() => {
    if (!liveStream || !selected) return;
    const path = selected;
    let cancelled = false;
    let unlisten: (() => void) | null = null;

    invoke("start_pos_tail", { path }).catch(() => {});
    (async () => {
      const u = await listen<PosEpochEvent>("pos-epoch", (e) => {
        if (e.payload.path === path) appendEpoch(path, e.payload);
      });
      if (cancelled) {
        u();
        return;
      }
      unlisten = u;
    })();

    return () => {
      cancelled = true;
      if (unlisten) unlisten();
      invoke("stop_pos_tail").catch(() => {});
    };
  }, [liveStream, selected, appendEpoch]);

  useEffect(() => {
    if (!selected) {
      setSeries(null);
      setLoadError(null);
      return;
    }
    let cancelled = false;
    setLoading(true);
    setLoadError(null);
    invoke<PosSeries>("load_pos_series", { path: selected, maxPoints: null })
      .then((s) => {
        if (cancelled) return;
        setSeries(s);
      })
      .catch((e) => {
        if (cancelled) return;
        setSeries(null);
        setLoadError(String(e));
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => {
      cancelled = true;
    };
    // posEpoch is a dependency so a re-run of the same file reloads its contents.
  }, [selected, posEpoch]);

  const primary = useMemo<SeriesEntry>(
    // A series from the previously selected file is not this file's data.
    () => ({ series: series && series.path === selected ? series : null, loading, error: loadError }),
    [series, selected, loading, loadError],
  );

  // ── Layout actions ─────────────────────────────────────────────────────────

  const addPanel = useCallback(
    (kind: PanelKind) => {
      const incoming = panel(kind);
      setLayout((prev) => {
        // Beside the panel last worked in, along its longer side; otherwise
        // as a new column at the right of the whole surface.
        const anchor = focusedId && prev ? findPanel(prev, focusedId) : null;
        if (!prev || !anchor) return addAtEdge(prev, incoming, "right");
        const el = document.querySelector<HTMLElement>(`[data-tile-id="${anchor.id}"]`);
        const wide = !el || el.offsetWidth >= el.offsetHeight;
        return insertBeside(prev, anchor.id, incoming, wide ? "right" : "bottom");
      });
      setMaximizedId(null);
      setFocusedId(incoming.id);
    },
    [focusedId],
  );

  const actions = useMemo<TilingActions>(
    () => ({
      resize: (splitId, index, fraction) => setLayout((prev) => (prev ? resizeSplit(prev, splitId, index, fraction) : prev)),
      setKind: (id, kind) => setLayout((prev) => (prev ? updatePanel(prev, id, { kind }) : prev)),
      setOptions: (id, patch) => setLayout((prev) => (prev ? updatePanelOptions(prev, id, patch) : prev)),
      splitPanel: (id, side) =>
        setLayout((prev) => {
          if (!prev) return prev;
          const src = findPanel(prev, id);
          // The copy starts as the same figure, so "split" reads as "another
          // one of these" — change it from the header dropdown.
          return src ? insertBeside(prev, id, panel(src.kind, { ...src.options }), side) : prev;
        }),
      close: (id) => {
        setLayout((prev) => (prev ? removePanel(prev, id) : prev));
        setMaximizedId((m) => (m === id ? null : m));
      },
      toggleMaximize: (id) => setMaximizedId((m) => (m === id ? null : id)),
      focus: (id) => setFocusedId(id),
    }),
    [],
  );

  const onDrop = useCallback((payload: DragPayload, target: DropTarget) => {
    setMaximizedId(null);
    // Built outside the updater so the id focused is the id placed.
    const incoming = payload.type === "new" ? panel(payload.kind) : null;
    if (incoming) setFocusedId(incoming.id);
    setLayout((prev) => {
      if (payload.type === "new") {
        if (!incoming) return prev;
        if (!prev) return incoming;
        if (target.id === ROOT_TARGET) return addAtEdge(prev, incoming, target.side === "center" ? "right" : target.side);
        if (target.side === "center") return updatePanel(prev, target.id, { kind: payload.kind, options: {} });
        return insertBeside(prev, target.id, incoming, target.side);
      }
      if (!prev) return prev;
      if (target.id === ROOT_TARGET) {
        const src = findPanel(prev, payload.id);
        const rest = removePanel(prev, payload.id);
        if (!src || !rest) return prev;
        return addAtEdge(rest, src, target.side === "center" ? "right" : target.side);
      }
      return movePanel(prev, payload.id, target.id, target.side);
    });
  }, []);

  const applyPreset = (key: string) => {
    const preset = PRESETS.find((p) => p.key === key);
    if (!preset) return;
    setLayout(preset.build());
    setMaximizedId(null);
    setFocusedId(null);
  };

  if (listError) {
    return <VizEmpty title="Could not read the output directory" body={listError} onRetry={refresh} />;
  }

  if (files && usable.length === 0) {
    return (
      <VizEmpty
        title={files.length === 0 ? "No .pos files yet" : "No position epochs yet"}
        body={
          files.length === 0
            ? `Nothing to plot in ${outputDir}. Turn on POS output in the sidebar and run a solution - the plots appear here as soon as it finishes. An SBAS L1 or DFMC run writes an .SPP file instead, which is plotted the same way.`
            : `The .pos ${files.length === 1 ? "file" : "files"} in ${outputDir} (${files
                .map((f) => f.name)
                .join(", ")}) ${files.length === 1 ? "has" : "have"} no epochs in ${files.length === 1 ? "it" : "them"} yet. A run that is still starting, or one that stopped before its first epoch, looks like this.`
        }
        onRetry={refresh}
      />
    );
  }

  const maximized = maximizedId && layout ? findPanel(layout, maximizedId) : null;

  return (
    <VizDataProvider files={usable} primaryPath={selected} primary={primary} reloadKey={posEpoch}>
      <VizLinkProvider linked={linked} setLinked={setLinked} resetKey={selected}>
       <VizSelectionProvider resetKey={selected}>
        <TilingProvider
          surfaceRef={surfaceRef}
          actions={actions}
          maximizedId={maximized ? maximized.id : null}
          focusedId={focusedId}
          onDrop={onDrop}
        >
          <div className="viz-pos">
            {/* On the tab bar's line, right of the tabs, rather than a row of its own. */}
            <TabBarActions>
            <div className="viz-toolbar">
              <label className="viz-file" title={selected ?? "Solution file"}>
                <select
                  aria-label="Solution file"
                  value={selected ?? ""}
                  onChange={(e) => setSelected(e.target.value)}
                  disabled={usable.length === 0}
                >
                  {usable.map((f) => (
                    <option key={f.path} value={f.path}>
                      {posFileLabel(f, timeZone)}
                    </option>
                  ))}
                </select>
              </label>

              <button
                className={`viz-tool-btn${linked ? " is-on" : ""}`}
                aria-pressed={linked}
                onClick={() => setLinked(!linked)}
                title="Zoom and hover every time-based panel together, and highlight the hovered epoch on the scatter and map"
              >
                <LinkIcon size={13} /> Link axes
              </button>

              <div className="viz-toolbar-right">
                <Menu
                  label={
                    <>
                      <PlusIcon size={13} /> Add view <ChevronDownIcon size={12} />
                    </>
                  }
                  title="Add a figure. Click to place it beside the active panel, or drag it onto the layout."
                >
                  {(close) => <AddViewList onPick={(k) => { addPanel(k); close(); }} onDragStart={close} />}
                </Menu>
                <Menu className="viz-icon-btn" label={<LayoutIcon size={14} />} title="Layout presets">
                  {(close) => (
                    <div className="viz-menu-list" role="menu">
                      <div className="viz-menu-heading">Presets</div>
                      {PRESETS.map((p) => (
                        <button
                          key={p.key}
                          role="menuitem"
                          className="viz-menu-item"
                          onClick={() => {
                            applyPreset(p.key);
                            close();
                          }}
                        >
                          <span className="viz-menu-item-title">{p.label}</span>
                          <span className="viz-menu-item-desc">{p.description}</span>
                        </button>
                      ))}
                      <div className="viz-menu-sep" />
                      <button
                        role="menuitem"
                        className="viz-menu-item"
                        disabled={!layout}
                        onClick={() => {
                          setLayout((prev) => (prev ? equalize(prev) : prev));
                          close();
                        }}
                      >
                        <span className="viz-menu-item-title">Even out sizes</span>
                      </button>
                    </div>
                  )}
                </Menu>
                <button
                  className="viz-tool-btn viz-icon-btn"
                  onClick={refresh}
                  title="Re-scan the output directory"
                  aria-label="Refresh"
                >
                  <ResetIcon size={14} />
                </button>
              </div>
            </div>
            </TabBarActions>

            {primary.series && primary.series.t.length > 1 && <TimelineScrubber series={primary.series} />}

            <div className="viz-surface" ref={surfaceRef}>
              {maximized ? (
                <TileNode node={maximized} />
              ) : layout ? (
                <TileNode node={layout} />
              ) : (
                <div className="viz-surface-empty">
                  <div className="viz-empty-title">No panels open</div>
                  <p>Add a figure from “Add view”, or start from a layout.</p>
                  <div className="viz-surface-empty-actions">
                    <button className="btn-secondary" onClick={() => applyPreset(DEFAULT_PRESET.key)}>
                      <LayoutIcon size={13} /> Restore default layout
                    </button>
                    <button className="btn-secondary" onClick={() => addPanel("timeseries")}>
                      <PlusIcon size={13} /> Time series
                    </button>
                  </div>
                </div>
              )}
            </div>
          </div>
        </TilingProvider>
       </VizSelectionProvider>
      </VizLinkProvider>
    </VizDataProvider>
  );
}

// ── Toolbar menus ────────────────────────────────────────────────────────────

function Menu({
  label,
  title,
  className,
  children,
}: {
  label: ReactNode;
  title: string;
  className?: string;
  children: (close: () => void) => ReactNode;
}) {
  const [open, setOpen] = useState(false);
  const ref = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!open) return;
    const onDown = (e: PointerEvent) => {
      if (ref.current && !ref.current.contains(e.target as Node)) setOpen(false);
    };
    const onKey = (e: KeyboardEvent) => e.key === "Escape" && setOpen(false);
    window.addEventListener("pointerdown", onDown, true);
    window.addEventListener("keydown", onKey);
    return () => {
      window.removeEventListener("pointerdown", onDown, true);
      window.removeEventListener("keydown", onKey);
    };
  }, [open]);

  return (
    <div className="viz-menu" ref={ref}>
      <button
        className={`viz-tool-btn${className ? ` ${className}` : ""}${open ? " is-on" : ""}`}
        aria-haspopup="menu"
        aria-label={title}
        aria-expanded={open}
        onClick={() => setOpen((v) => !v)}
        title={title}
      >
        {label}
      </button>
      {open && <div className="viz-menu-pop">{children(() => setOpen(false))}</div>}
    </div>
  );
}

/**
 * The figure palette. Each entry is a click target and a drag source: click
 * places the figure beside the active panel, dragging drops it exactly where
 * the user wants it — on a panel edge, a panel centre (replace), or the outer
 * edge of the surface.
 */
function AddViewList({ onPick, onDragStart }: { onPick: (k: PanelKind) => void; onDragStart: () => void }) {
  return (
    <div className="viz-menu-list" role="menu">
      <div className="viz-menu-heading">Click to add · drag to place</div>
      {PANEL_KINDS.map((k) => (
        <AddViewItem key={k} kind={k} onPick={onPick} onDragStart={onDragStart} />
      ))}
    </div>
  );
}

function AddViewItem({ kind, onPick, onDragStart }: { kind: PanelKind; onPick: (k: PanelKind) => void; onDragStart: () => void }) {
  const def = PANELS[kind];
  const tiling = useTilingDrag();
  const dragged = useRef(false);
  const why = unavailableReason(def, useVizData().primary.series);

  return (
    <button
      role="menuitem"
      className="viz-menu-item viz-menu-item--draggable"
      disabled={!!why}
      title={why ? `${def.title}: ${why}` : undefined}
      onPointerDown={(e) => {
        if (why) return;
        dragged.current = false;
        const sx = e.clientX;
        const sy = e.clientY;
        tiling(e, { type: "new", kind }, def.title);
        // Close the menu once the press turns into a drag, so it doesn't sit
        // over the drop targets.
        const watch = (ev: PointerEvent) => {
          if (Math.hypot(ev.clientX - sx, ev.clientY - sy) >= 5) {
            dragged.current = true;
            window.removeEventListener("pointermove", watch);
            onDragStart();
          }
        };
        window.addEventListener("pointermove", watch);
        window.addEventListener("pointerup", () => window.removeEventListener("pointermove", watch), { once: true });
      }}
      onClick={() => {
        if (!dragged.current) onPick(kind);
      }}
    >
      <span className="viz-menu-item-title">{def.title}</span>
      <span className="viz-menu-item-desc">{why ? `Unavailable (${why})` : def.description}</span>
    </button>
  );
}
