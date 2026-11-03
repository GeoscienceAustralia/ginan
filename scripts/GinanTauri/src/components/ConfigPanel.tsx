import { useEffect, useRef } from "react";
import {
  useAppStore,
  SIDEBAR_MIN_WIDTH,
  SIDEBAR_MAX_WIDTH,
  SIDEBAR_DEFAULT_WIDTH,
} from "../store/appStore";
import type { ConfigSectionKey } from "../store/appStore";
import type { ProcessingMode } from "../types";
import { GeneralTab, ConstellationsTab, OutputTab } from "./ConfigPanelTabs";
import { InputSource } from "./ConfigInputSource";
import { AppMark } from "./Header";
import {
  SlidersIcon,
  SatelliteIcon,
  OutputIcon,
  ChevronRightIcon,
  ChevronDownIcon,
} from "./icons";
import { useScrollHints } from "../hooks/useScrollHints";

export const MODES: ProcessingMode[] = ["Static", "Kinematic", "Dynamic"];

// Which section is open lives in the store (see ConfigSectionKey) so the guided
// tour can open the one it is describing.
const SECTIONS: {
  key: ConfigSectionKey;
  label: string;
  icon: () => JSX.Element;
  render: () => JSX.Element;
}[] = [
  { key: "general",        label: "General",        icon: () => <SlidersIcon />,   render: () => <GeneralTab /> },
  { key: "constellations", label: "Constellations", icon: () => <SatelliteIcon />, render: () => <ConstellationsTab /> },
  { key: "output",         label: "Output",         icon: () => <OutputIcon />,    render: () => <OutputTab /> },
];

export default function ConfigPanel() {
  const openSection = useAppStore((s) => s.configSection);
  const setOpenSection = useAppStore((s) => s.setConfigSection);
  const sidebarCollapsed = useAppStore((s) => s.sidebarCollapsed);
  const sidebarWidth = useAppStore((s) => s.sidebarWidth);
  const setSidebarWidth = useAppStore((s) => s.setSidebarWidth);

  // The rail hides its scrollbar, so these fades are what tell the user there
  // are more sections above or below; without them the lower sections can
  // seem to have disappeared.
  const [accordionRef, scrollHints] = useScrollHints<HTMLDivElement>();

  // Bring a section into view when it opens, whether from its header or from
  // the guided tour, so opening one near the bottom does not leave its body
  // below the fold. A body taller than the rail is aligned to its top so the
  // header stays visible; otherwise it scrolls only as far as it has to. The
  // section open on mount is left where it is.
  const mounted = useRef(false);
  useEffect(() => {
    if (!mounted.current) {
      mounted.current = true;
      return;
    }
    const scroller = accordionRef.current;
    if (!openSection || !scroller) return;
    // Wait a frame for the body to render and take its height.
    const frame = requestAnimationFrame(() => {
      const item = scroller.querySelector<HTMLElement>(`[data-tour="section-${openSection}"]`);
      if (!item) return;
      const block = item.offsetHeight > scroller.clientHeight ? "start" : "nearest";
      item.scrollIntoView({ block, behavior: "smooth" });
    });
    return () => cancelAnimationFrame(frame);
  }, [openSection, accordionRef]);

  // Drag the right edge to resize. The move/up listeners go on the window, not
  // the 5px handle, so the drag survives the pointer outrunning it; `is-resizing`
  // on <body> kills the width transition for the duration, or the rail would
  // trail the cursor by the animation length on every move.
  const startResize = (e: React.PointerEvent) => {
    e.preventDefault();
    const startX = e.clientX;
    const startWidth = useAppStore.getState().sidebarWidth;
    let latest = startWidth;

    const onMove = (ev: PointerEvent) => {
      latest = startWidth + (ev.clientX - startX);
      setSidebarWidth(latest);
    };
    const onUp = () => {
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", onUp);
      document.body.classList.remove("is-resizing");
      // Commit once, on release — the store clamps before it writes.
      setSidebarWidth(latest, true);
    };

    document.body.classList.add("is-resizing");
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
  };

  // Keyboard equivalent, so the rail is resizable without a pointer. Each step
  // is measured from the store rather than the render-time width: a held arrow
  // key can fire faster than React re-renders, and a stale closure would make
  // every repeat land on the same value.
  const onResizerKey = (e: React.KeyboardEvent) => {
    const step = e.shiftKey ? 48 : 16;
    const current = useAppStore.getState().sidebarWidth;
    if (e.key === "ArrowLeft") {
      e.preventDefault();
      setSidebarWidth(current - step, true);
    } else if (e.key === "ArrowRight") {
      e.preventDefault();
      setSidebarWidth(current + step, true);
    } else if (e.key === "Home") {
      e.preventDefault();
      setSidebarWidth(SIDEBAR_DEFAULT_WIDTH, true);
    }
  };

  return (
    <div
      id="config-panel"
      data-tour="sidebar"
      className={`config-panel${sidebarCollapsed ? " is-collapsed" : ""}`}
      style={{ "--sidebar-width": `${sidebarWidth}px` } as React.CSSProperties}
    >
      <div className="sidebar-brand" data-tauri-drag-region>
        <AppMark />
      </div>

      {/* There is no mode control: the run mode follows the Observation
          products (a File card is a file run, a live card a live run), so the
          input sources are the first thing in the scroll area. */}
      <div className="config-accordion" ref={accordionRef}>
        <InputSource />
        {SECTIONS.map(({ key, label, icon, render }) => {
          const isOpen = openSection === key;
          return (
            <div
              key={key}
              data-tour={`section-${key}`}
              className={`config-accordion-item${isOpen ? " is-open" : ""}`}
            >
              <button
                type="button"
                className="config-accordion-header"
                aria-expanded={isOpen}
                aria-controls={`config-accordion-body-${key}`}
                onClick={() => setOpenSection(isOpen ? null : key)}
              >
                {icon()}
                {label}
                <span className="config-accordion-chevron" aria-hidden="true">
                  <ChevronRightIcon size={14} />
                </span>
              </button>
              {isOpen && (
                <div
                  id={`config-accordion-body-${key}`}
                  className="config-accordion-body"
                  role="region"
                >
                  {render()}
                </div>
              )}
            </div>
          );
        })}
      </div>

      <div
        className={`config-scroll-hint is-above${scrollHints.above ? " is-visible" : ""}`}
        aria-hidden="true"
      />
      <div
        className={`config-scroll-hint is-below${scrollHints.below ? " is-visible" : ""}`}
        aria-hidden="true"
      >
        <ChevronDownIcon size={14} />
      </div>

      {/* Right-edge resize handle. Needs no collapsed-state guard of its own —
          the shut rail is `visibility: hidden`, which takes this with it. */}
      <div
        className="config-resizer"
        role="separator"
        aria-orientation="vertical"
        aria-label="Resize sidebar"
        aria-valuenow={sidebarWidth}
        aria-valuemin={SIDEBAR_MIN_WIDTH}
        aria-valuemax={SIDEBAR_MAX_WIDTH}
        tabIndex={0}
        onPointerDown={startResize}
        onKeyDown={onResizerKey}
        onDoubleClick={() => setSidebarWidth(SIDEBAR_DEFAULT_WIDTH, true)}
        title="Drag to resize · double-click to reset"
      />
    </div>
  );
}
