import { useCallback, useEffect, useLayoutEffect, useRef, useState } from "react";
import { useAppStore } from "../store/appStore";
import { TOURS } from "./tourSteps";
import type { TourPlacement, TourStep } from "./tourSteps";
import { ChevronLeftIcon, ChevronRightIcon, CloseIcon } from "./icons";

/**
 * Guided tour: a dimming scrim with a hole cut around one control at a time,
 * and a card next to it explaining what that control is for.
 *
 * The app underneath is inert for the duration — the scrim swallows clicks —
 * so a step can describe a control without the user having to operate it, and
 * without a stray click changing the state the next step is about to describe.
 * Steps that need the app in a particular state (a sidebar section open, say)
 * put it there themselves via `onEnter`.
 *
 * Two things make the geometry safe to trust: the target rectangle is
 * re-measured every frame, so panel animations, window resizes and layout
 * shifts are all handled by the same code path; and a target that never becomes
 * measurable is skipped after a short grace period rather than leaving the card
 * pointing at empty space.
 *
 * A measurable target is not necessarily a visible one — the sidebar accordion
 * scrolls (`overflow-y: auto`), so a target low in an open section has a real
 * rectangle that happens to sit outside the rail. Each step scrolls its target
 * into view the first frame it measures, which is what makes the streaming
 * tour's stream and recording steps reachable at all.
 *
 * Finally, a tour describes one input mode. If the mode changes underneath it
 * the remaining steps are about a panel that is no longer there, so the tour
 * closes instead of walking the user through the wrong app.
 */

const CARD_WIDTH = 320;
/** Space between the spotlight and the card, and the card and the viewport. */
const GAP = 14;
const EDGE = 12;
/** Ring drawn outside the target's own bounds. */
const PAD = 6;

/** How long a target may stay unmeasurable before the step is skipped. */
const MISSING_GRACE_MS = 450;

interface Rect {
  top: number;
  left: number;
  width: number;
  height: number;
}

function measure(step: TourStep | undefined): Rect | null {
  if (!step?.target) return null;
  const el = document.querySelector<HTMLElement>(`[data-tour="${step.target}"]`);
  if (!el) return null;
  const r = el.getBoundingClientRect();
  // A collapsed rail or a hidden tab body still answers querySelector, so
  // presence alone is not enough — an element with no area cannot be pointed at.
  if (r.width < 1 || r.height < 1) return null;
  return { top: r.top, left: r.left, width: r.width, height: r.height };
}

function sameRect(a: Rect | null, b: Rect | null): boolean {
  if (a === null || b === null) return a === b;
  return (
    Math.abs(a.top - b.top) < 0.5 &&
    Math.abs(a.left - b.left) < 0.5 &&
    Math.abs(a.width - b.width) < 0.5 &&
    Math.abs(a.height - b.height) < 0.5
  );
}

/**
 * Place the card beside the spotlight, trying the step's preferred side first
 * and falling back around the target until one fits. The result is clamped to
 * the viewport, so the last fallback is always usable even if it overlaps.
 */
function placeCard(
  rect: Rect | null,
  cardHeight: number,
  preferred: TourPlacement,
): { top: number; left: number; placement: TourPlacement | "center" } {
  const vw = window.innerWidth;
  const vh = window.innerHeight;

  if (!rect) {
    return {
      top: Math.max(EDGE, (vh - cardHeight) / 2),
      left: Math.max(EDGE, (vw - CARD_WIDTH) / 2),
      placement: "center",
    };
  }

  const fits: Record<TourPlacement, boolean> = {
    right: rect.left + rect.width + GAP + CARD_WIDTH + EDGE <= vw,
    left: rect.left - GAP - CARD_WIDTH - EDGE >= 0,
    bottom: rect.top + rect.height + GAP + cardHeight + EDGE <= vh,
    top: rect.top - GAP - cardHeight - EDGE >= 0,
  };

  const order: TourPlacement[] = [
    preferred,
    ...(["bottom", "right", "top", "left"] as TourPlacement[]).filter(
      (p) => p !== preferred,
    ),
  ];
  const placement = order.find((p) => fits[p]) ?? preferred;

  let top: number;
  let left: number;
  if (placement === "right" || placement === "left") {
    // Centred on the target vertically reads better than top-aligned for the
    // tall sidebar sections.
    top = rect.top + rect.height / 2 - cardHeight / 2;
    left =
      placement === "right"
        ? rect.left + rect.width + GAP
        : rect.left - GAP - CARD_WIDTH;
  } else {
    left = rect.left + rect.width / 2 - CARD_WIDTH / 2;
    top =
      placement === "bottom"
        ? rect.top + rect.height + GAP
        : rect.top - GAP - cardHeight;
  }

  return {
    top: Math.min(Math.max(EDGE, top), Math.max(EDGE, vh - cardHeight - EDGE)),
    left: Math.min(Math.max(EDGE, left), Math.max(EDGE, vw - CARD_WIDTH - EDGE)),
    placement,
  };
}

export default function Tour() {
  const activeTour = useAppStore((s) => s.activeTour);
  const endTour = useAppStore((s) => s.endTour);
  const inputMode = useAppStore((s) => s.inputMode);

  const [index, setIndex] = useState(0);
  const [rect, setRect] = useState<Rect | null>(null);
  const [cardHeight, setCardHeight] = useState(180);

  const cardRef = useRef<HTMLDivElement>(null);
  // Which way a skipped step should step over to. Kept in a ref because the
  // rAF loop reads it outside React's render cycle.
  const directionRef = useRef<1 | -1>(1);
  const missingSinceRef = useRef<number | null>(null);
  // The input mode this tour opened under. Compared against the live one, not
  // captured in a step, because every step of a tour assumes the same mode.
  const openedInModeRef = useRef(inputMode);

  const steps = activeTour ? TOURS[activeTour] : [];
  const step: TourStep | undefined = steps[index];

  const go = useCallback(
    (dir: 1 | -1) => {
      directionRef.current = dir;
      missingSinceRef.current = null;
      setIndex((i) => {
        const next = i + dir;
        if (next < 0) return 0;
        // Off the end going forwards is a completed tour, which endTour records.
        if (next >= steps.length) return i;
        return next;
      });
    },
    [steps.length],
  );

  const finish = useCallback(() => {
    endTour();
    setIndex(0);
  }, [endTour]);

  const isLast = index >= steps.length - 1;

  const advance = useCallback(() => {
    if (isLast) finish();
    else go(1);
  }, [isLast, finish, go]);

  // Restart at the top whenever a tour is (re)opened.
  useEffect(() => {
    if (activeTour) {
      setIndex(0);
      directionRef.current = 1;
      missingSinceRef.current = null;
      // Read through the store rather than the subscribed value: this runs in
      // the same commit as the mode-change guard below, and that guard has to
      // compare against the mode as of *this* moment.
      openedInModeRef.current = useAppStore.getState().inputMode;
    }
  }, [activeTour]);

  // Switching between file and stream input swaps most of what the sidebar
  // shows, so the rest of a tour written for the old mode would be describing
  // panels that are gone or inert. Close rather than mislead. Clearing
  // `activeTour` also re-runs App's auto-start, so if the tour for the new mode
  // has not been seen yet it opens straight away; otherwise the help button
  // brings back whichever tour matches what is on screen now.
  //
  // Declared after the effect above so opening a tour refreshes the reference
  // mode before it is compared. No path through the UI reaches this today — the
  // scrim swallows the clicks that would change mode — but the tour is the one
  // surface in the app that keeps a stale assumption on screen for minutes at a
  // time, so it is worth the four lines.
  useEffect(() => {
    if (!activeTour) return;
    if (inputMode === openedInModeRef.current) return;
    openedInModeRef.current = inputMode;
    endTour();
  }, [activeTour, inputMode, endTour]);

  // Put the app into the state this step describes. Runs before the measuring
  // loop settles, which is why a target that isn't there yet gets a grace
  // period rather than being skipped on its first frame.
  useEffect(() => {
    if (!activeTour || !step) return;
    missingSinceRef.current = null;
    step.onEnter?.();
  }, [activeTour, step]);

  // Re-measure every frame: panel width transitions, window resizes and the
  // accordion opening all move the target, and polling costs less than
  // subscribing to each of those separately.
  useEffect(() => {
    if (!activeTour) return;
    let raf = 0;
    // Per step: scroll once, the first frame the target has a rectangle. The
    // effect re-runs on every index change, so this resets with it.
    let scrolled = false;

    const tick = () => {
      const current = TOURS[activeTour][index];
      const next = measure(current);

      if (current && current.target && next === null) {
        // Nothing to point at (yet). Give it a moment for a transition to
        // finish, then step over it in whichever direction we were going.
        const now = performance.now();
        if (missingSinceRef.current === null) missingSinceRef.current = now;
        else if (now - missingSinceRef.current > MISSING_GRACE_MS) {
          missingSinceRef.current = null;
          const dir = directionRef.current;
          const candidate = index + dir;
          if (candidate < 0 || candidate >= TOURS[activeTour].length) {
            // Ran out of steps in that direction — nothing left to show.
            endTour();
          } else {
            setIndex(candidate);
          }
        }
      } else {
        missingSinceRef.current = null;
        if (next && !scrolled) {
          scrolled = true;
          document
            .querySelector<HTMLElement>(`[data-tour="${current.target}"]`)
            // `nearest` moves the least it can get away with, and keeps the
            // scroll inside the accordion instead of dragging the page.
            // Instant, not smooth: the rectangle is re-measured every frame, so
            // an animated scroll drags the spotlight along behind the target,
            // and it would spend most of MISSING_GRACE_MS getting there.
            ?.scrollIntoView({ block: "nearest", inline: "nearest", behavior: "auto" });
        }
      }

      setRect((prev) => (sameRect(prev, next) ? prev : next));
      raf = requestAnimationFrame(tick);
    };

    raf = requestAnimationFrame(tick);
    return () => cancelAnimationFrame(raf);
  }, [activeTour, index, endTour]);

  useLayoutEffect(() => {
    const h = cardRef.current?.offsetHeight;
    if (h && Math.abs(h - cardHeight) > 0.5) setCardHeight(h);
  });

  useEffect(() => {
    if (!activeTour) return;
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") {
        e.preventDefault();
        finish();
      } else if (e.key === "ArrowRight" || e.key === "Enter") {
        e.preventDefault();
        advance();
      } else if (e.key === "ArrowLeft") {
        e.preventDefault();
        go(-1);
      }
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [activeTour, advance, go, finish]);

  // The card takes focus so the arrow keys work without a click first.
  useEffect(() => {
    if (activeTour) cardRef.current?.focus();
  }, [activeTour]);

  if (!activeTour || !step) return null;

  const { top, left, placement } = placeCard(
    rect,
    cardHeight,
    step.placement ?? "bottom",
  );

  return (
    <div className="tour" role="dialog" aria-modal="true" aria-label="Guided tour">
      {/* Swallows every click aimed at the app underneath. Clicking the dim
          area is the same as pressing Next, which is what people try first. */}
      <div className="tour-scrim" onClick={advance} />

      {rect && (
        <div
          className="tour-spot"
          style={{
            top: rect.top - PAD,
            left: rect.left - PAD,
            width: rect.width + PAD * 2,
            height: rect.height + PAD * 2,
          }}
        />
      )}

      <div
        ref={cardRef}
        className={`tour-card tour-card--${placement}`}
        style={{ top, left, width: CARD_WIDTH }}
        tabIndex={-1}
      >
        <div className="tour-card-head">
          <span className="tour-count">
            {index + 1} of {steps.length}
          </span>
          <button
            className="btn-icon"
            onClick={() => finish()}
            title="Leave the tour (Esc)"
            aria-label="Leave the tour"
          >
            <CloseIcon size={14} />
          </button>
        </div>

        <h2 className="tour-title">{step.title}</h2>
        <p className="tour-body">{step.body}</p>

        <div className="tour-progress" aria-hidden="true">
          {steps.map((s, i) => (
            <span
              key={s.title}
              className={`tour-pip${i === index ? " is-current" : ""}${
                i < index ? " is-done" : ""
              }`}
            />
          ))}
        </div>

        <div className="tour-actions">
          <button
            className="btn-ghost"
            onClick={() => finish()}
            title="Skip the rest of the tour"
          >
            Skip
          </button>
          <div className="tour-actions-right">
            <button className="btn-ghost" onClick={() => go(-1)} disabled={index === 0}>
              <ChevronLeftIcon size={14} /> Back
            </button>
            <button className="btn-primary" onClick={advance}>
              {isLast ? "Done" : "Next"}
              {!isLast && <ChevronRightIcon size={14} />}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
