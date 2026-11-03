import { useEffect, useLayoutEffect, useRef, useState, type CSSProperties } from "react";
import { createPortal } from "react-dom";
import {
  EPOCH_FIELDS,
  dayOfYear,
  formatEpoch,
  gpsMinusUtc,
  gpsWeek,
  parseEpoch,
  stepEpoch,
  toStoreEpoch,
} from "./gpsTime";
import { CalendarIcon, ChevronLeftIcon, ChevronRightIcon } from "./icons";

const DAY_MS = 86_400_000;
const MONTHS = [
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December",
];
const WEEKDAYS = ["S", "M", "T", "W", "T", "F", "S"];

/** An observation window in GPS-time ms (see gpsTime.ts). */
export interface EpochWindow {
  start: number;
  end: number;
}

interface Props {
  id: string;
  label: string;
  /** Store form, "YYYY-MM-DDTHH:MM:SS" in GPS time; "" when unset. */
  value: string;
  onChange: (value: string) => void;
  /** The loaded file's observation window, shaded on the calendar. */
  fileWindow?: EpochWindow | null;
}

/**
 * An epoch field in GPS time, replacing the browser's `datetime-local`.
 *
 * The native picker had two faults here: it renders in the user's locale and
 * reads as local time, while PEA takes GPST; and its segments cannot be typed
 * or pasted as the "YYYY-MM-DD HH:MM:SS" string GNSS tools print. This is a
 * plain text field in that format, so a timestamp can be pasted straight in,
 * with arrow keys stepping whichever field the caret is in, and a calendar
 * that pops up over the field when it is clicked. The popup is portalled to
 * `document.body` and fixed to the field's position, so the scrolling sidebar
 * does not clip it; it opens below the field, or above when there is more
 * room there, and closes on Escape or a click outside it.
 *
 * The draft is local and committed on Enter or blur. Whenever the stored
 * value changes underneath (a RINEX load, a config import, a reset) the draft
 * follows it, so the field never shows a stale epoch.
 */
export default function EpochPicker({ id, label, value, onChange, fileWindow }: Props) {
  const committed = parseEpoch(value);
  const shown = committed == null ? "" : formatEpoch(committed);

  const [draft, setDraft] = useState(shown);
  const [invalid, setInvalid] = useState(false);
  const [open, setOpen] = useState(false);
  const inputRef = useRef<HTMLInputElement>(null);
  const fieldRef = useRef<HTMLDivElement>(null);
  const popRef = useRef<HTMLDivElement>(null);
  const [place, setPlace] = useState<CSSProperties | null>(null);

  // Place the popup against the field, and keep it there as the sidebar
  // scrolls or the window resizes. Runs after the popup is in the DOM, so its
  // real height decides whether it fits below.
  useLayoutEffect(() => {
    if (!open) {
      setPlace(null);
      return;
    }
    const update = () => {
      const field = fieldRef.current?.getBoundingClientRect();
      if (!field) return;
      const height = popRef.current?.offsetHeight ?? 0;
      const below = window.innerHeight - field.bottom;
      const top = below >= height + 12 || below >= field.top
        ? field.bottom + 4
        : field.top - height - 4;
      setPlace({ left: field.left, top: Math.max(8, top), minWidth: field.width });
    };
    update();
    const outside = (e: PointerEvent) => {
      const target = e.target as Node;
      if (fieldRef.current?.contains(target) || popRef.current?.contains(target)) return;
      setOpen(false);
    };
    window.addEventListener("resize", update);
    window.addEventListener("scroll", update, true);
    document.addEventListener("pointerdown", outside);
    return () => {
      window.removeEventListener("resize", update);
      window.removeEventListener("scroll", update, true);
      document.removeEventListener("pointerdown", outside);
    };
  }, [open]);

  useEffect(() => {
    setDraft(shown);
    setInvalid(false);
  }, [shown]);

  const commitMs = (ms: number) => {
    setInvalid(false);
    setDraft(formatEpoch(ms));
    const next = toStoreEpoch(ms);
    if (next !== value) onChange(next);
  };

  const commit = (text: string) => {
    if (!text.trim()) {
      setInvalid(false);
      if (value) onChange("");
      return;
    }
    const ms = parseEpoch(text);
    if (ms == null) setInvalid(true);
    else commitMs(ms);
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Enter") {
      commit(draft);
    } else if (e.key === "Escape") {
      setDraft(shown);
      setInvalid(false);
      setOpen(false);
    } else if (e.key === "ArrowUp" || e.key === "ArrowDown") {
      const ms = parseEpoch(draft) ?? committed;
      if (ms == null) return;
      e.preventDefault();
      const caret = e.currentTarget.selectionStart ?? 0;
      const field = EPOCH_FIELDS.find((f) => caret <= f.to) ?? EPOCH_FIELDS[5];
      const by = (e.key === "ArrowUp" ? 1 : -1) * (e.shiftKey ? 10 : 1);
      commitMs(stepEpoch(ms, field.unit, by));
      // Keep the stepped field selected so the next press lands on it too.
      requestAnimationFrame(() => inputRef.current?.setSelectionRange(field.from, field.to));
    }
  };

  const errorId = `${id}-error`;
  const leap = committed != null ? gpsMinusUtc(committed) : null;

  return (
    <div className="field-row epoch-field">
      <label htmlFor={id}>{label}</label>
      <div ref={fieldRef} className={`epoch-input${invalid ? " is-invalid" : ""}`}>
        <input
          ref={inputRef}
          id={id}
          type="text"
          inputMode="numeric"
          autoComplete="off"
          spellCheck={false}
          placeholder="YYYY-MM-DD HH:MM:SS"
          // Kept off the sidebar to keep it clean; still there on hover.
          title={committed != null ? epochDetails(committed, leap ?? 0) : undefined}
          value={draft}
          aria-invalid={invalid}
          aria-describedby={invalid ? errorId : undefined}
          onChange={(e) => {
            setDraft(e.target.value);
            setInvalid(false);
          }}
          onClick={() => setOpen(true)}
          onBlur={() => commit(draft)}
          onKeyDown={onKeyDown}
        />
        <span
          className="epoch-tag"
          title={
            leap != null
              ? `GPS time. No leap seconds, so it runs ${leap} s ahead of UTC at this epoch.`
              : "GPS time. No leap seconds, so it runs ahead of UTC."
          }
        >
          GPST
        </span>
        <button
          type="button"
          className="btn-icon epoch-toggle"
          aria-label={`${open ? "Close" : "Open"} calendar for ${label}`}
          aria-expanded={open}
          aria-controls={`${id}-calendar`}
          onClick={() => setOpen((o) => !o)}
        >
          <CalendarIcon size={14} />
        </button>
      </div>

      {invalid ? (
        <p className="epoch-error" id={errorId}>
          Use YYYY-MM-DD HH:MM:SS, e.g. 2024-01-02 00:00:00
        </p>
      ) : null}

      {open &&
        createPortal(
          <div
            ref={popRef}
            className="epoch-popover"
            // Hidden for the first frame, until it has been measured and placed.
            style={place ?? { visibility: "hidden" }}
          >
            <EpochCalendar
              id={`${id}-calendar`}
              label={label}
              valueMs={committed}
              fileWindow={fileWindow ?? null}
              onPick={commitMs}
              onClose={() => {
                setOpen(false);
                inputRef.current?.focus();
              }}
            />
          </div>,
          document.body,
        )}
    </div>
  );
}

/** GPS week/day and day of year, which is how product files are named, plus
 *  the UTC time for anyone checking against a UTC log: the field's tooltip. */
export function epochDetails(ms: number, leap: number): string {
  const { week, day } = gpsWeek(ms);
  const utc = formatEpoch(ms - leap * 1000);
  return `Week ${week} · day ${day} · DOY ${String(dayOfYear(ms)).padStart(3, "0")} · UTC ${utc}`;
}

function EpochCalendar({
  id,
  label,
  valueMs,
  fileWindow,
  onPick,
  onClose,
}: {
  id: string;
  label: string;
  valueMs: number | null;
  fileWindow: EpochWindow | null;
  onPick: (ms: number) => void;
  onClose: () => void;
}) {
  const anchor = valueMs ?? fileWindow?.start ?? Date.now();
  const [view, setView] = useState(() => monthOf(anchor));

  // Follow the value to its month when it moves from outside (typing, a
  // RINEX load), but leave the user's own paging alone otherwise.
  const valueMonth = valueMs == null ? null : monthOf(valueMs);
  useEffect(() => {
    if (valueMonth) setView(valueMonth);
  }, [valueMonth?.y, valueMonth?.m]);

  const timeOfDay = valueMs == null ? 0 : ((valueMs % DAY_MS) + DAY_MS) % DAY_MS;
  const first = Date.UTC(view.y, view.m, 1);
  const lead = new Date(first).getUTCDay();
  const daysInMonth = new Date(Date.UTC(view.y, view.m + 1, 0)).getUTCDate();
  const rows = Math.ceil((lead + daysInMonth) / 7);
  const gridStart = first - lead * DAY_MS;
  const selectedDay = valueMs == null ? null : valueMs - timeOfDay;

  const page = (by: number) =>
    setView(({ y, m }) => monthOf(Date.UTC(y, m + by, 1)));

  const setClock = (part: "h" | "m" | "s", raw: string) => {
    const n = Number(raw);
    const max = part === "h" ? 23 : 59;
    if (raw === "" || !Number.isInteger(n) || n < 0 || n > max) return;
    const base = valueMs ?? Date.UTC(view.y, view.m, 1);
    const d = new Date(base);
    const h = part === "h" ? n : d.getUTCHours();
    const mi = part === "m" ? n : d.getUTCMinutes();
    const s = part === "s" ? n : d.getUTCSeconds();
    onPick(Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate(), h, mi, s));
  };

  const clock = valueMs == null ? null : new Date(valueMs);

  return (
    <div
      className="epoch-calendar"
      id={id}
      role="group"
      aria-label={`${label} calendar`}
      onKeyDown={(e) => {
        if (e.key === "Escape") {
          e.stopPropagation();
          onClose();
        }
      }}
    >
      <div className="epoch-cal-head">
        <button type="button" className="btn-icon" aria-label="Previous month" onClick={() => page(-1)}>
          <ChevronLeftIcon size={13} />
        </button>
        <span className="epoch-cal-title" aria-live="polite">
          {MONTHS[view.m]} {view.y}
        </span>
        <button type="button" className="btn-icon" aria-label="Next month" onClick={() => page(1)}>
          <ChevronRightIcon size={13} />
        </button>
      </div>

      {/* GPS weeks run Sunday to Saturday, the same as these rows, so each
          row is labelled with its week number. */}
      <div className="epoch-cal-grid" role="grid">
        <div className="epoch-cal-row" role="row">
          <span className="epoch-cal-wk" role="columnheader" title="GPS week">Wk</span>
          {WEEKDAYS.map((d, i) => (
            <span key={i} className="epoch-cal-dow" role="columnheader">{d}</span>
          ))}
        </div>
        {Array.from({ length: rows }, (_, r) => {
          const rowStart = gridStart + r * 7 * DAY_MS;
          return (
            <div className="epoch-cal-row" role="row" key={r}>
              <span className="epoch-cal-wk" role="rowheader">{gpsWeek(rowStart).week}</span>
              {Array.from({ length: 7 }, (_, c) => {
                const day = rowStart + c * DAY_MS;
                const date = new Date(day);
                const inMonth = date.getUTCMonth() === view.m;
                const inWindow =
                  fileWindow != null && day <= fileWindow.end && day + DAY_MS > fileWindow.start;
                const selected = day === selectedDay;
                const cls = [
                  "epoch-cal-day",
                  inMonth ? "" : "is-outside",
                  inWindow ? "in-window" : "",
                  selected ? "is-selected" : "",
                ].filter(Boolean).join(" ");
                return (
                  <button
                    key={c}
                    type="button"
                    role="gridcell"
                    className={cls}
                    aria-label={formatEpoch(day).slice(0, 10)}
                    aria-selected={selected}
                    onClick={() => onPick(day + timeOfDay)}
                  >
                    {date.getUTCDate()}
                  </button>
                );
              })}
            </div>
          );
        })}
      </div>

      <div className="epoch-cal-clock">
        <span className="epoch-cal-clock-label">Time</span>
        <ClockPart label="Hour" max={23} value={clock?.getUTCHours()} onChange={(v) => setClock("h", v)} />
        <span aria-hidden="true">:</span>
        <ClockPart label="Minute" max={59} value={clock?.getUTCMinutes()} onChange={(v) => setClock("m", v)} />
        <span aria-hidden="true">:</span>
        <ClockPart label="Second" max={59} value={clock?.getUTCSeconds()} onChange={(v) => setClock("s", v)} />
        <button type="button" className="btn-secondary epoch-cal-done" onClick={onClose}>
          Done
        </button>
      </div>
      {fileWindow && (
        <p className="epoch-cal-legend">
          <span className="epoch-cal-swatch" aria-hidden="true" /> Days the loaded file covers
        </p>
      )}
    </div>
  );
}

function ClockPart({
  label,
  max,
  value,
  onChange,
}: {
  label: string;
  max: number;
  value: number | undefined;
  onChange: (raw: string) => void;
}) {
  return (
    <input
      type="number"
      className="epoch-cal-part"
      aria-label={label}
      min={0}
      max={max}
      placeholder="00"
      value={value == null ? "" : String(value).padStart(2, "0")}
      onChange={(e) => onChange(e.target.value)}
    />
  );
}

function monthOf(ms: number) {
  const d = new Date(ms);
  return { y: d.getUTCFullYear(), m: d.getUTCMonth() };
}
