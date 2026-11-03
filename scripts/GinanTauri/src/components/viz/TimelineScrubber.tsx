import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import type { PosSeries } from "../../types";
import {
  clamp,
  formatDuration,
  formatEpoch,
  formatMillimetres,
  nearestIndex,
  prepareCanvas,
  readVizTheme,
  scale,
  timeAxisTicks,
  toData,
  toPx,
  useElementSize,
  withAlpha,
  type Scale,
  type VizTheme,
  useCanvasRedraw,
} from "../chartUtils";
import {
  ChevronLeftIcon,
  ChevronRightIcon,
  CloseIcon,
  MaximizeIcon,
  MinusIcon,
  PlayIcon,
  PlusIcon,
  StopIcon,
  TargetIcon,
} from "../icons";
import { activityWindow, deriveErrors } from "./metrics";
import { useSelectedIndex, useSelectedTime, useVizLink } from "./VizContext";
import { useTimeZone } from "../../hooks/useTimeZone";
import { formatTime } from "../../utils/time";

/**
 * The run as a two-row strip across the top of the Visualization tab.
 *
 * - The **overview** row always shows the whole run, with a window marking
 *   the part the detail row shows. Drag the window to pan, its edges to
 *   resize, or sweep a new one; the wheel zooms around the pointer.
 * - The **detail** row shows that window. Click or drag to pick an epoch,
 *   wheel to step through epochs, Ctrl+wheel (or pinch) to zoom, Shift+wheel
 *   to pan. The picked epoch is marked on every panel below.
 *
 * With "Link time axes" on, the window *is* the charts' shared zoom, so
 * zooming here zooms the time panels and zooming a panel zooms here. When a
 * file opens, the window jumps to where the action is (a converging filter,
 * a jump) if that is concentrated in one part of the run.
 *
 * The trace is the horizontal error on a log scale — the one quantity whose
 * shape says where in the run to look.
 *
 * Every row has a fixed height and the hover readout is drawn in the canvas,
 * so nothing reflows as the pointer moves.
 */

const OVERVIEW_H = 20;
const DETAIL_H = 46;
const PAD_X = 10;
/** Grab margin, in px, for the overview window's edges. */
const EDGE_PX = 6;
/** Narrowest window, in epochs. */
const MIN_EPOCHS = 8;
const PLAY_MS = 60;
const SPEEDS = [1, 5, 20, 60];
const ZOOM_STEP = 1.6;

type Win = [number, number];
type OverviewDrag =
  | { mode: "pan"; startX: number; win: Win }
  | { mode: "left" | "right"; win: Win }
  | { mode: "new"; startX: number; moved: boolean };

export default function TimelineScrubber({ series }: { series: PosSeries }) {
  const timeZone = useTimeZone();
  const [wrapRef, size] = useElementSize<HTMLDivElement>();
  const overviewRef = useRef<HTMLCanvasElement>(null);
  const detailRef = useRef<HTMLCanvasElement>(null);
  const { selectedT, setSelectedT } = useSelectedTime();
  const selIndex = useSelectedIndex(series.t);
  const link = useVizLink();

  const [hover, setHover] = useState<{ row: "overview" | "detail"; px: number } | null>(null);
  const [scrubbing, setScrubbing] = useState(false);
  const [ovDrag, setOvDrag] = useState<OverviewDrag | null>(null);
  const [playing, setPlaying] = useState(false);
  // Real time by default: one epoch per step. Faster is a choice, not a surprise.
  const [speed, setSpeed] = useState(1);
  const [autoFocused, setAutoFocused] = useState(false);

  const n = series.t.length;
  const t0 = series.t[0];
  const t1 = series.t[n - 1];
  const full = Math.max(1, t1 - t0);
  const minSpan = Math.min(full, Math.max(1000, (full / Math.max(1, n - 1)) * MIN_EPOCHS));

  // The window: the linked charts' zoom when linked, else the strip's own.
  const [localWin, setLocalWin] = useState<Win | null>(null);
  const linked = !!link?.linked;
  const rawWin = linked ? link!.view : localWin;
  const setRawWin = linked ? link!.setView : setLocalWin;
  const win: Win = rawWin ? [clamp(rawWin[0], t0, t1), clamp(rawWin[1], t0, t1)] : [t0, t1];
  const zoomed = win[1] - win[0] < full * 0.999;

  const setWin = useCallback(
    (next: Win | null, fromAuto = false) => {
      if (!fromAuto) setAutoFocused(false);
      if (!next) return setRawWin(null);
      let [a, b] = next;
      let span = Math.max(minSpan, b - a);
      if (span >= full * 0.999) return setRawWin(null);
      a = clamp(a, t0, t1 - span);
      b = a + span;
      setRawWin([a, b]);
    },
    [setRawWin, minSpan, full, t0, t1],
  );

  const zoomAround = useCallback(
    (center: number, factor: number) => {
      const span = win[1] - win[0];
      const next = clamp(span * factor, minSpan, full);
      const ratio = (center - win[0]) / Math.max(1, span);
      setWin([center - ratio * next, center - ratio * next + next]);
    },
    [win, minSpan, full, setWin],
  );

  // The window "Focus on activity" zooms to. Not applied on its own: opening a
  // 13-hour run on its first 15 minutes left every time axis showing a sliver
  // of the data with nothing saying where the rest went.
  const focused = useMemo(() => activityWindow(series), [series.path, n > 20]);
  const shownPathRef = useRef<string | null>(null);
  useEffect(() => {
    if (shownPathRef.current === series.path) return;
    // Deferred so it runs after the link provider's own reset for a new file
    // (parent effects run after this one). Marked done only when applied: if
    // this effect is torn down first (StrictMode's double run), it retries.
    const id = setTimeout(() => {
      shownPathRef.current = series.path;
      setPlaying(false);
      setWin(null);
    }, 0);
    return () => clearTimeout(id);
  }, [series.path, setWin]);

  const width = size.width;
  const xOverview = useMemo(() => scale(t0, t1, PAD_X, Math.max(PAD_X + 1, width - PAD_X)), [t0, t1, width]);
  const xDetail = useMemo(() => scale(win[0], win[1], PAD_X, Math.max(PAD_X + 1, width - PAD_X)), [win[0], win[1], width]);

  const logH = useMemo(() => deriveErrors(series).h.map((v) => Math.log10(Math.max(v, 1e-4))), [series]);

  // ── drawing ────────────────────────────────────────────────────────────────

  const draw = useCallback(() => {
    const theme = readVizTheme(wrapRef.current);

    const ov = prepareCanvas(overviewRef.current, width, OVERVIEW_H);
    if (ov && width > 2 * PAD_X + 10) {
      drawTrace(ov, theme, series.t, logH, xOverview, 2, OVERVIEW_H - 2, 0.55);
      const a = toPx(xOverview, win[0]);
      const b = toPx(xOverview, win[1]);
      if (zoomed) {
        // Dim outside the window so the window reads as the "lens".
        ov.fillStyle = withAlpha(theme.surface, 0.6);
        ov.fillRect(xOverview.p0, 0, a - xOverview.p0, OVERVIEW_H);
        ov.fillRect(b, 0, xOverview.p1 - b, OVERVIEW_H);
      }
      ov.strokeStyle = zoomed ? theme.accent : withAlpha(theme.text, 0.25);
      ov.lineWidth = zoomed ? 1.5 : 1;
      ov.strokeRect(Math.round(a) + 0.5, 0.75, Math.max(2, Math.round(b - a) - 1), OVERVIEW_H - 1.5);
      if (zoomed) {
        ov.fillStyle = theme.accent;
        ov.fillRect(Math.round(a), 5, 2, OVERVIEW_H - 10);
        ov.fillRect(Math.round(b) - 2, 5, 2, OVERVIEW_H - 10);
      }
      if (selIndex !== null) {
        ov.fillStyle = theme.accent;
        ov.fillRect(Math.round(toPx(xOverview, series.t[selIndex])), 0, 1.5, OVERVIEW_H);
      }
      if (hover?.row === "overview" && !ovDrag) {
        ov.fillStyle = withAlpha(theme.text, 0.4);
        ov.fillRect(Math.round(hover.px), 0, 1, OVERVIEW_H);
      }
    }

    const ctx = prepareCanvas(detailRef.current, width, DETAIL_H);
    if (!ctx || width <= 2 * PAD_X + 10) return;
    const top = 4;
    const bottom = DETAIL_H - 16;
    drawTrace(ctx, theme, series.t, logH, xDetail, top, bottom, 0.75);
    ctx.strokeStyle = theme.grid;
    ctx.beginPath();
    ctx.moveTo(xDetail.p0, bottom + 0.5);
    ctx.lineTo(xDetail.p1, bottom + 0.5);
    ctx.stroke();

    ctx.fillStyle = theme.textDim;
    ctx.font = "10.5px ui-sans-serif, system-ui, sans-serif";
    ctx.textBaseline = "top";
    ctx.textAlign = "center";
    for (const tick of timeAxisTicks(win[0], win[1], Math.max(2, Math.floor(width / 120)), timeZone)) {
      const px = toPx(xDetail, tick.value);
      ctx.fillRect(Math.round(px), bottom, 1, 3);
      ctx.fillText(tick.label.replace("\n", " "), clamp(px, xDetail.p0 + 30, xDetail.p1 - 30), bottom + 4);
    }

    if (hover?.row === "detail" && !scrubbing) {
      const px = Math.round(hover.px) + 0.5;
      ctx.strokeStyle = withAlpha(theme.text, 0.4);
      ctx.beginPath();
      ctx.moveTo(px, top);
      ctx.lineTo(px, bottom);
      ctx.stroke();
      // Hover time, in the canvas so showing it can't reflow the layout.
      const label = formatTime(series.t[nearestIndex(series.t, toData(xDetail, hover.px))], timeZone);
      ctx.font = "10.5px ui-sans-serif, system-ui, sans-serif";
      const w = ctx.measureText(label).width + 8;
      const lx = hover.px + w + 6 > xDetail.p1 ? hover.px - w - 4 : hover.px + 4;
      ctx.fillStyle = withAlpha(theme.surface, 0.9);
      ctx.fillRect(lx, top, w, 14);
      ctx.fillStyle = theme.text;
      ctx.textAlign = "left";
      ctx.fillText(label, lx + 4, top + 2);
    }

    if (selIndex !== null) {
      const t = series.t[selIndex];
      const px = Math.round(toPx(xDetail, t)) + 0.5;
      ctx.fillStyle = theme.accent;
      ctx.strokeStyle = theme.accent;
      if (t >= win[0] && t <= win[1]) {
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(px, top - 2);
        ctx.lineTo(px, bottom);
        ctx.stroke();
        ctx.beginPath();
        ctx.roundRect(px - 4, top - 3, 8, 11, 2);
        ctx.fill();
      } else {
        // Off-window: an arrow at the edge it lies beyond.
        const left = t < win[0];
        const ex = left ? xDetail.p0 + 2 : xDetail.p1 - 2;
        const my = (top + bottom) / 2;
        ctx.beginPath();
        ctx.moveTo(ex + (left ? -1 : 1) * 1, my);
        ctx.lineTo(ex + (left ? 8 : -8), my - 6);
        ctx.lineTo(ex + (left ? 8 : -8), my + 6);
        ctx.closePath();
        ctx.fill();
      }
    }
  }, [width, series.t, logH, xOverview, xDetail, win[0], win[1], zoomed, selIndex, hover, scrubbing, ovDrag, timeZone]);

  useCanvasRedraw(draw);

  // ── selection & playback ───────────────────────────────────────────────────

  const selectIndex = useCallback(
    (i: number) => setSelectedT(series.t[clamp(Math.round(i), 0, n - 1)]),
    [series.t, n, setSelectedT],
  );
  const step = useCallback((d: number) => selectIndex((selIndex ?? (d > 0 ? -1 : n)) + d), [selIndex, selectIndex, n]);

  // Keep the cursor inside the window: pan (same width) when the selection
  // moves past either edge — by playback, the keyboard, or a click in a panel.
  const winRef = useRef(win);
  winRef.current = win;
  const setWinRef = useRef(setWin);
  setWinRef.current = setWin;
  useEffect(() => {
    if (selectedT === null) return;
    const [a, b] = winRef.current;
    if (selectedT >= a && selectedT <= b) return;
    const w = b - a;
    const start = selectedT < a ? selectedT - w * 0.1 : selectedT - w * 0.9;
    setWinRef.current([start, start + w]);
  }, [selectedT]);

  const indexRef = useRef(selIndex);
  indexRef.current = selIndex;
  useEffect(() => {
    if (!playing) return;
    if (indexRef.current === null || indexRef.current >= n - 1) {
      // Start from the left edge of what is on screen.
      selectIndex(nearestIndex(series.t, winRef.current[0]));
    }
    const id = setInterval(() => {
      const cur = indexRef.current ?? 0;
      if (cur >= n - 1) {
        setPlaying(false);
        return;
      }
      selectIndex(cur + speed);
    }, PLAY_MS);
    return () => clearInterval(id);
  }, [playing, speed, n, selectIndex, series.t]);

  // ── pointer: detail row ────────────────────────────────────────────────────

  const localX = (e: { clientX: number }, el: HTMLElement) => e.clientX - el.getBoundingClientRect().left;

  const onDetailDown = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (e.button !== 0) return;
    const el = e.currentTarget;
    try {
      el.setPointerCapture(e.pointerId);
    } catch {
      /* scrubbing still works while the pointer stays over the strip */
    }
    setPlaying(false);
    setScrubbing(true);
    selectIndex(nearestIndex(series.t, toData(xDetail, clamp(localX(e, el), xDetail.p0, xDetail.p1))));
  };

  const onDetailMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const px = clamp(localX(e, e.currentTarget), xDetail.p0, xDetail.p1);
    setHover({ row: "detail", px });
    if (scrubbing) selectIndex(nearestIndex(series.t, toData(xDetail, px)));
  };

  // ── pointer: overview row ──────────────────────────────────────────────────

  const onOverviewDown = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (e.button !== 0) return;
    const el = e.currentTarget;
    try {
      el.setPointerCapture(e.pointerId);
    } catch {
      /* the drag still works while the pointer stays over the strip */
    }
    const px = localX(e, el);
    const a = toPx(xOverview, win[0]);
    const b = toPx(xOverview, win[1]);
    if (zoomed && Math.abs(px - a) <= EDGE_PX) setOvDrag({ mode: "left", win });
    else if (zoomed && Math.abs(px - b) <= EDGE_PX) setOvDrag({ mode: "right", win });
    else if (zoomed && px > a && px < b) setOvDrag({ mode: "pan", startX: px, win });
    else setOvDrag({ mode: "new", startX: px, moved: false });
  };

  const onOverviewMove = (e: React.PointerEvent<HTMLCanvasElement>) => {
    const px = clamp(localX(e, e.currentTarget), xOverview.p0, xOverview.p1);
    setHover({ row: "overview", px });
    if (!ovDrag) return;
    const t = toData(xOverview, px);
    if (ovDrag.mode === "pan") {
      const dt = t - toData(xOverview, ovDrag.startX);
      setWin([ovDrag.win[0] + dt, ovDrag.win[1] + dt]);
    } else if (ovDrag.mode === "left") {
      setWin([Math.min(t, ovDrag.win[1] - minSpan), ovDrag.win[1]]);
    } else if (ovDrag.mode === "right") {
      setWin([ovDrag.win[0], Math.max(t, ovDrag.win[0] + minSpan)]);
    } else if (ovDrag.mode === "new" && Math.abs(px - ovDrag.startX) > 3) {
      const s = toData(xOverview, ovDrag.startX);
      setWin([Math.min(s, t), Math.max(s, t)]);
      if (!ovDrag.moved) setOvDrag({ ...ovDrag, moved: true });
    }
  };

  const onOverviewUp = (e: React.PointerEvent<HTMLCanvasElement>) => {
    if (ovDrag?.mode === "new" && !ovDrag.moved) {
      // A click: zoom in on that point, or re-centre an existing window on it.
      const t = toData(xOverview, clamp(localX(e, e.currentTarget), xOverview.p0, xOverview.p1));
      const span = zoomed ? win[1] - win[0] : full / 4;
      setWin([t - span / 2, t + span / 2]);
    }
    setOvDrag(null);
  };

  const overviewCursor = (() => {
    if (ovDrag) return ovDrag.mode === "pan" ? "grabbing" : ovDrag.mode === "new" ? "col-resize" : "ew-resize";
    if (!hover || hover.row !== "overview" || !zoomed) return "zoom-in";
    const a = toPx(xOverview, win[0]);
    const b = toPx(xOverview, win[1]);
    if (Math.abs(hover.px - a) <= EDGE_PX || Math.abs(hover.px - b) <= EDGE_PX) return "ew-resize";
    return hover.px > a && hover.px < b ? "grab" : "zoom-in";
  })();

  // ── wheel & keys ───────────────────────────────────────────────────────────

  // Native, non-passive listeners: React's wheel handler can't preventDefault,
  // and the page (and a Ctrl+wheel browser zoom) must not move instead.
  const wheelState = useRef({ win, xDetail, xOverview, step, zoomAround, setWin });
  wheelState.current = { win, xDetail, xOverview, step, zoomAround, setWin };
  useEffect(() => {
    const detail = detailRef.current;
    const overview = overviewRef.current;
    if (!detail || !overview) return;
    const onDetailWheel = (ev: WheelEvent) => {
      ev.preventDefault();
      const s = wheelState.current;
      const delta = Math.abs(ev.deltaY) >= Math.abs(ev.deltaX) ? ev.deltaY : ev.deltaX;
      if (delta === 0) return;
      const px = ev.clientX - detail.getBoundingClientRect().left;
      if (ev.ctrlKey || ev.metaKey) {
        s.zoomAround(toData(s.xDetail, px), delta > 0 ? ZOOM_STEP : 1 / ZOOM_STEP);
      } else if (ev.shiftKey || Math.abs(ev.deltaX) > Math.abs(ev.deltaY)) {
        const w = s.win[1] - s.win[0];
        const dt = (delta > 0 ? 1 : -1) * w * 0.15;
        s.setWin([s.win[0] + dt, s.win[1] + dt]);
      } else {
        setPlaying(false);
        s.step(delta > 0 ? 1 : -1);
      }
    };
    const onOverviewWheel = (ev: WheelEvent) => {
      ev.preventDefault();
      const s = wheelState.current;
      const px = ev.clientX - overview.getBoundingClientRect().left;
      s.zoomAround(toData(s.xOverview, px), ev.deltaY > 0 ? ZOOM_STEP : 1 / ZOOM_STEP);
    };
    detail.addEventListener("wheel", onDetailWheel, { passive: false });
    overview.addEventListener("wheel", onOverviewWheel, { passive: false });
    return () => {
      detail.removeEventListener("wheel", onDetailWheel);
      overview.removeEventListener("wheel", onOverviewWheel);
    };
  }, []);

  const zoomCentre = selectedT !== null && selectedT >= win[0] && selectedT <= win[1] ? selectedT : (win[0] + win[1]) / 2;

  const onKeyDown = (e: React.KeyboardEvent) => {
    const big = e.shiftKey ? 10 : 1;
    const keys: Record<string, () => void> = {
      ArrowRight: () => step(big),
      ArrowLeft: () => step(-big),
      PageDown: () => step(100),
      PageUp: () => step(-100),
      Home: () => selectIndex(nearestIndex(series.t, win[0])),
      End: () => selectIndex(nearestIndex(series.t, win[1])),
      "+": () => zoomAround(zoomCentre, 1 / ZOOM_STEP),
      "=": () => zoomAround(zoomCentre, 1 / ZOOM_STEP),
      "-": () => zoomAround(zoomCentre, ZOOM_STEP),
      "0": () => setWin(null),
      " ": () => setPlaying((p) => !p),
      Escape: () => {
        setPlaying(false);
        setSelectedT(null);
      },
    };
    const fn = keys[e.key];
    if (fn) {
      e.preventDefault();
      fn();
    }
  };

  const h = selIndex !== null ? deriveErrors(series).h[selIndex] : null;

  return (
    <div className="viz-timeline" onKeyDown={onKeyDown}>
      <div className="viz-timeline-controls">
        <button className="viz-icon-btn" onClick={() => step(-1)} title="Previous epoch (←)" aria-label="Previous epoch">
          <ChevronLeftIcon size={14} />
        </button>
        <button
          className={`viz-icon-btn${playing ? " is-active" : ""}`}
          onClick={() => setPlaying((p) => !p)}
          title={playing ? "Pause (space)" : "Play through the run (space)"}
          aria-label={playing ? "Pause" : "Play"}
        >
          {playing ? <StopIcon size={13} /> : <PlayIcon size={13} />}
        </button>
        <button className="viz-icon-btn" onClick={() => step(1)} title="Next epoch (→)" aria-label="Next epoch">
          <ChevronRightIcon size={14} />
        </button>
        <select
          className="viz-timeline-speed"
          value={speed}
          onChange={(e) => setSpeed(Number(e.target.value))}
          title="Playback speed, in epochs per step"
          aria-label="Playback speed"
        >
          {SPEEDS.map((s) => (
            <option key={s} value={s}>
              {s}×
            </option>
          ))}
        </select>

        <div className="viz-timeline-readout" aria-live="polite">
          {selIndex !== null ? (
            <>
              <span className="viz-timeline-time">{formatEpoch(series.t[selIndex], timeZone)}</span>
              <span className="viz-dim">
                epoch {(selIndex + 1).toLocaleString()} / {n.toLocaleString()}
              </span>
              {(["dn", "de", "du"] as const).map((k, i) => (
                <span key={k} className="viz-timeline-val">
                  <span className="viz-key" style={{ background: `var(--viz-series-${i + 1})` }} />
                  {k === "dn" ? "dN" : k === "de" ? "dE" : "dU"} {formatMillimetres(series[k][selIndex])}
                </span>
              ))}
              {h !== null && <span className="viz-timeline-val viz-dim">H {formatMillimetres(h)}</span>}
              <button
                className="viz-icon-btn"
                onClick={() => {
                  setPlaying(false);
                  setSelectedT(null);
                }}
                title="Clear the selected time (Esc)"
                aria-label="Clear selected time"
              >
                <CloseIcon size={12} />
              </button>
            </>
          ) : (
            <span className="viz-dim">Click the lower strip to pick a time</span>
          )}
        </div>

        <div className="viz-timeline-zoom">
          {zoomed && (
            <span className="viz-timeline-span viz-dim" title="Span shown in the detail strip">
              {autoFocused && <span className="viz-badge">Auto-focused</span>}
              {formatDuration(win[1] - win[0])} of {formatDuration(full)}
            </span>
          )}
          <button className="viz-icon-btn" onClick={() => zoomAround(zoomCentre, ZOOM_STEP)} disabled={!zoomed} title="Zoom out (−)" aria-label="Zoom out">
            <MinusIcon size={13} />
          </button>
          <button
            className="viz-icon-btn"
            onClick={() => zoomAround(zoomCentre, 1 / ZOOM_STEP)}
            disabled={win[1] - win[0] <= minSpan * 1.01}
            title="Zoom in (+)"
            aria-label="Zoom in"
          >
            <PlusIcon size={13} />
          </button>
          <button
            className="viz-icon-btn"
            onClick={() => {
              if (focused) {
                setWin(focused, true);
                setAutoFocused(true);
              }
            }}
            disabled={!focused}
            title={focused ? "Zoom to where most of the action is" : "The activity is spread across the whole run"}
            aria-label="Focus on activity"
          >
            <TargetIcon size={13} />
          </button>
          <button className="viz-icon-btn" onClick={() => setWin(null)} disabled={!zoomed} title="Show the whole run (0)" aria-label="Show whole run">
            <MaximizeIcon size={13} />
          </button>
        </div>
      </div>

      <div className="viz-timeline-strip" ref={wrapRef}>
        <canvas
          ref={overviewRef}
          className="viz-canvas viz-timeline-overview"
          style={{ width: "100%", height: OVERVIEW_H, cursor: overviewCursor }}
          aria-label="Whole run. Drag to choose the zoomed range."
          onPointerDown={onOverviewDown}
          onPointerMove={onOverviewMove}
          onPointerUp={onOverviewUp}
          onPointerCancel={() => setOvDrag(null)}
          onPointerLeave={() => setHover(null)}
          onDoubleClick={() => setWin(null)}
        />
        <canvas
          ref={detailRef}
          className="viz-canvas"
          style={{ width: "100%", height: DETAIL_H, cursor: scrubbing ? "grabbing" : "pointer" }}
          role="slider"
          tabIndex={0}
          aria-label="Selected time"
          aria-valuemin={1}
          aria-valuemax={n}
          aria-valuenow={selIndex !== null ? selIndex + 1 : undefined}
          aria-valuetext={selIndex !== null ? formatEpoch(series.t[selIndex], timeZone) : "No time selected"}
          onPointerDown={onDetailDown}
          onPointerMove={onDetailMove}
          onPointerUp={() => setScrubbing(false)}
          onPointerCancel={() => setScrubbing(false)}
          onPointerLeave={() => setHover(null)}
        />
      </div>
    </div>
  );
}

/**
 * Log horizontal error between `top` and `bottom`, one min/max column per
 * pixel over the visible part of `x`, scaled to the visible values so a zoomed
 * window shows its own detail.
 */
function drawTrace(
  ctx: CanvasRenderingContext2D,
  theme: VizTheme,
  t: number[],
  values: number[],
  x: Scale,
  top: number,
  bottom: number,
  alpha: number,
) {
  const cols = Math.max(1, Math.round(x.p1 - x.p0));
  const lo = new Array<number>(cols).fill(Infinity);
  const hi = new Array<number>(cols).fill(-Infinity);
  let min = Infinity;
  let max = -Infinity;
  const i0 = Math.max(0, nearestIndex(t, x.d0) - 1);
  const i1 = Math.min(t.length - 1, nearestIndex(t, x.d1) + 1);
  for (let i = i0; i <= i1; i++) {
    const c = Math.floor(((t[i] - x.d0) / Math.max(1, x.d1 - x.d0)) * cols);
    if (c < 0 || c >= cols) continue;
    const v = values[i];
    if (v < lo[c]) lo[c] = v;
    if (v > hi[c]) hi[c] = v;
    if (v < min) min = v;
    if (v > max) max = v;
  }
  if (!Number.isFinite(min)) return;
  const span = Math.max(1e-6, max - min);
  const yOf = (v: number) => bottom - ((v - min) / span) * (bottom - top);
  // Sparse windows (fewer epochs than pixels) get joined with a line so the
  // trace doesn't dissolve into dots when zoomed right in.
  if (i1 - i0 < cols / 2) {
    ctx.strokeStyle = withAlpha(theme.series[0], alpha);
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    for (let i = i0; i <= i1; i++) {
      const px = toPx(x, t[i]);
      const py = yOf(values[i]);
      if (i === i0) ctx.moveTo(px, py);
      else ctx.lineTo(px, py);
    }
    ctx.stroke();
    return;
  }
  for (let c = 0; c < cols; c++) {
    if (!Number.isFinite(lo[c])) continue;
    const a = yOf(hi[c]);
    const b = yOf(lo[c]);
    ctx.fillStyle = withAlpha(theme.series[0], alpha * 0.22);
    ctx.fillRect(x.p0 + c, a, 1, bottom - a);
    ctx.fillStyle = withAlpha(theme.series[0], alpha);
    ctx.fillRect(x.p0 + c, a, 1, Math.max(1, b - a));
  }
}
