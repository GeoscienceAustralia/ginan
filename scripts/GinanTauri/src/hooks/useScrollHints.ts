import { useEffect, useRef, useState } from "react";

export interface ScrollHints {
  /** Content is scrolled out of view above. */
  above: boolean;
  /** Content runs on below the bottom edge. */
  below: boolean;
}

/**
 * Whether a scroll container has content hidden above or below it, kept up to
 * date as the user scrolls and as the content grows or shrinks (a section
 * opening, a card being added, the window resizing).
 *
 * For scrollers that hide their scrollbar, where nothing else tells the user
 * there is more to see.
 */
export function useScrollHints<T extends HTMLElement>() {
  const ref = useRef<T>(null);
  const [hints, setHints] = useState<ScrollHints>({ above: false, below: false });

  useEffect(() => {
    const el = ref.current;
    if (!el) return;

    // A pixel of slack, so sub-pixel layout does not leave a hint stuck on.
    const update = () => {
      const above = el.scrollTop > 1;
      const below = el.scrollTop + el.clientHeight < el.scrollHeight - 1;
      setHints((h) => (h.above === above && h.below === below ? h : { above, below }));
    };

    // The container's own box changes with the window; its children's boxes
    // change as sections open and close. Children come and go, so they are
    // re-observed whenever the tree changes.
    const resize = new ResizeObserver(update);
    const observeChildren = () => {
      resize.observe(el);
      for (const child of Array.from(el.children)) resize.observe(child);
    };
    const mutation = new MutationObserver(() => {
      observeChildren();
      update();
    });

    observeChildren();
    mutation.observe(el, { childList: true, subtree: true });
    el.addEventListener("scroll", update, { passive: true });
    update();

    return () => {
      el.removeEventListener("scroll", update);
      resize.disconnect();
      mutation.disconnect();
    };
  }, []);

  return [ref, hints] as const;
}
