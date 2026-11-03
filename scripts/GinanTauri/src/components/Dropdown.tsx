import { useEffect, useId, useLayoutEffect, useRef, useState, type CSSProperties, type KeyboardEvent, type ReactNode } from "react";
import { createPortal } from "react-dom";
import { ChevronDownIcon } from "./icons";

export interface DropdownOption {
  value: string;
  /** Shown instead of `value`. */
  label?: string;
  /** Second, dimmer line under the label. */
  detail?: string;
  /** With `format`, replaces `detail`: location at 12px, format in small mono. */
  location?: string;
  format?: string;
  /** Greyed, not selectable, and skipped by the arrow keys. */
  disabled?: boolean;
}

/** An option's text: the value as is, or the two-line stream look. */
function OptionText({ o }: { o: DropdownOption }) {
  if (o.location || o.format) {
    return (
      <>
        <span className="dd-option-code">{o.label ?? o.value}</span>
        <span className="dd-option-sub">
          {o.location && <span className="dd-option-location">{o.location}</span>}
          {o.format && <span className="dd-option-format">{o.format}</span>}
        </span>
      </>
    );
  }
  return (
    <>
      <span>{o.label ?? o.value}</span>
      {o.detail && <span className="dd-option-detail">{o.detail}</span>}
    </>
  );
}

interface DropdownProps {
  value: string;
  onChange: (value: string) => void;
  options: DropdownOption[];
  ariaLabel: string;
  /** Free text with the options as suggestions. Off means pick-only. */
  editable?: boolean;
  placeholder?: string;
  /** Called each time the list opens: the place to fetch or refresh `options`. */
  onOpen?: () => void;
  /** Row under the options, for a loading or error line. */
  note?: ReactNode;
  /** Called after an option is picked from the list (not on typing). */
  onPick?: (value: string) => void;
  /**
   * Keep the closed field to one line, the picked value only, for a field that
   * sits in a one-row source beside a badge. The list still shows the details.
   */
  singleLine?: boolean;
}

// Only the first 100 matches are rendered; typing narrows the rest
// in. Virtualise the list if a whole sourcetable ever has to be scrollable.
const MAX_ROWS = 100;
/** Tallest the open list gets, in px; must match the `.dd-list` fallback in App.css. */
const LIST_MAX_HEIGHT = 240;
const LIST_GAP = 4;
const VIEWPORT_MARGIN = 8;
/**
 * Widest the open list grows past its field to fit its longest row (a code
 * with its location and format). A short list stays the field's width.
 */
const LIST_MAX_WIDTH = 360;

/**
 * Select and combobox with an in-app option list.
 *
 * The native `<select>` and `<datalist>` popups are drawn by the OS, and on
 * macOS that means a translucent menu the sidebar shows through. This draws
 * the list itself on an opaque `--panel-2` surface instead (`.dd-*` in
 * App.css).
 *
 * The open list is portalled to `document.body` and fixed to the field's
 * position, so no card, `<details>` or scroll area clips it. It opens below
 * the field, or above when there is more room there.
 */
export default function Dropdown({
  value,
  onChange,
  options,
  ariaLabel,
  editable = false,
  placeholder,
  onOpen,
  note,
  onPick,
  singleLine = false,
}: DropdownProps) {
  const [open, setOpen] = useState(false);
  const [active, setActive] = useState(0);
  // Suggestions only narrow once the user types, so opening a field that is
  // already filled in still shows the whole list.
  const [typed, setTyped] = useState(false);
  const wrap = useRef<HTMLDivElement>(null);
  const activeRow = useRef<HTMLLIElement>(null);
  const list = useRef<HTMLUListElement>(null);
  const [place, setPlace] = useState<CSSProperties | null>(null);
  const listId = useId();

  const query = value.trim().toLowerCase();
  const shown = (
    editable && typed && query
      ? options.filter((o) =>
          `${o.value} ${o.label ?? ""} ${o.detail ?? ""} ${o.location ?? ""} ${o.format ?? ""}`.toLowerCase().includes(query),
        )
      : options
  ).slice(0, MAX_ROWS);
  const at = Math.min(active, shown.length - 1);
  const current = options.find((o) => o.value === value);

  const show = () => {
    if (open) return;
    setOpen(true);
    setTyped(false);
    setActive(Math.max(0, options.findIndex((o) => o.value === value)));
    onOpen?.();
  };
  const pick = (o: DropdownOption) => {
    if (o.disabled) return;
    onChange(o.value);
    setOpen(false);
    onPick?.(o.value);
  };

  // Focus alone can't close it: WebKit doesn't focus a button on click, so the
  // pick-only variant would never blur.
  useEffect(() => {
    if (!open) return;
    const close = (e: MouseEvent) => {
      const t = e.target as Node;
      if (!wrap.current?.contains(t) && !list.current?.contains(t)) setOpen(false);
    };
    document.addEventListener("mousedown", close);
    return () => document.removeEventListener("mousedown", close);
  }, [open]);

  // Anchor the fixed list to the field: same left edge and width, below it or
  // (with more room above) over it. Runs before paint, and again on scroll or
  // resize, which would otherwise leave the list behind.
  useLayoutEffect(() => {
    if (!open) return setPlace(null);
    const anchor = () => {
      if (!wrap.current) return;
      const r = wrap.current.getBoundingClientRect();
      const want = Math.min(list.current?.scrollHeight ?? LIST_MAX_HEIGHT, LIST_MAX_HEIGHT);
      const below = window.innerHeight - r.bottom - LIST_GAP - VIEWPORT_MARGIN;
      const above = r.top - LIST_GAP - VIEWPORT_MARGIN;
      const flip = below < want && above > below;
      // As wide as its longest row needs: never narrower than the field, never
      // wider than LIST_MAX_WIDTH (or the field, if that is wider), and never
      // past the window's right edge. The list is fixed-position, so before it
      // is given a width it lays out at its content's width, which is measured.
      const natural = Math.max(list.current?.offsetWidth ?? 0, list.current?.scrollWidth ?? 0);
      const cap = Math.max(r.width, LIST_MAX_WIDTH);
      const width = Math.min(Math.max(r.width, natural), cap, window.innerWidth - 2 * VIEWPORT_MARGIN);
      const left = Math.max(VIEWPORT_MARGIN, Math.min(r.left, window.innerWidth - VIEWPORT_MARGIN - width));
      setPlace({
        left,
        width,
        maxHeight: Math.max(0, Math.min(LIST_MAX_HEIGHT, flip ? above : below)),
        ...(flip ? { bottom: window.innerHeight - r.top + LIST_GAP } : { top: r.bottom + LIST_GAP }),
      });
    };
    anchor();
    window.addEventListener("resize", anchor);
    window.addEventListener("scroll", anchor, true);
    return () => {
      window.removeEventListener("resize", anchor);
      window.removeEventListener("scroll", anchor, true);
    };
  }, [open, shown.length, note]);

  useEffect(() => {
    if (open) activeRow.current?.scrollIntoView({ block: "nearest" });
  }, [open, at]);

  const onKeyDown = (e: KeyboardEvent) => {
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();
      if (!open) return show();
      const step = e.key === "ArrowDown" ? 1 : -1;
      let next = at + step;
      while (shown[next]?.disabled) next += step;
      if (next >= 0 && next < shown.length) setActive(next);
      else if (at < 0) setActive(0);
    } else if (e.key === "Enter" && open) {
      e.preventDefault();
      // No row highlighted means the typed text stands as it is.
      if (shown[at]) pick(shown[at]);
      else setOpen(false);
    } else if (e.key === "Escape" && open) {
      e.preventDefault();
      setOpen(false);
    }
  };

  // A filled-in stream shows as two lines. The input can't, so it is covered
  // by this while closed, and clicks still reach it.
  const twoLine = !singleLine && !!current && !!(current.location || current.format);
  const control = {
    "aria-label": ariaLabel,
    "aria-expanded": open,
    "aria-controls": listId,
    "aria-activedescendant": open && at >= 0 ? `${listId}-${at}` : undefined,
  };

  return (
    <div
      className="dd"
      ref={wrap}
      onKeyDown={onKeyDown}
      onBlur={(e) => {
        if (!e.currentTarget.contains(e.relatedTarget)) setOpen(false);
      }}
    >
      {editable ? (
        <input
          {...control}
          className={`rt-input${twoLine && !open ? " dd-covered" : ""}`}
          type="text"
          role="combobox"
          aria-autocomplete="list"
          autoComplete="off"
          spellCheck={false}
          value={value}
          placeholder={placeholder}
          onFocus={show}
          onClick={show}
          onChange={(e) => {
            onChange(e.target.value);
            show();
            setTyped(true);
            setActive(-1);
          }}
        />
      ) : (
        <button
          {...control}
          type="button"
          className="rt-input dd-button"
          role="combobox"
          aria-haspopup="listbox"
          onClick={(e) => {
            e.currentTarget.focus();
            if (open) setOpen(false);
            else show();
          }}
        >
          {twoLine ? (
            <span className="dd-cover-text">
              <OptionText o={current} />
            </span>
          ) : (
            (current?.label ?? value)
          )}
          <ChevronDownIcon size={12} />
        </button>
      )}
      {editable && twoLine && !open && (
        <span className="dd-cover" aria-hidden="true">
          <span className="dd-cover-text">
            <OptionText o={current} />
          </span>
          <ChevronDownIcon size={12} />
        </span>
      )}
      {open &&
        createPortal(
        // Keeps focus on the control while the list is clicked or scrolled.
        <ul
          ref={list}
          className="dd-list"
          id={listId}
          role="listbox"
          style={place ?? { visibility: "hidden" }}
          onMouseDown={(e) => e.preventDefault()}
        >
          {shown.map((o, i) => (
            <li
              key={o.value}
              id={`${listId}-${i}`}
              ref={i === at ? activeRow : undefined}
              role="option"
              aria-selected={o.value === value}
              aria-disabled={o.disabled || undefined}
              className={`dd-option${i === at ? " is-active" : ""}${o.disabled ? " is-disabled" : ""}`}
              onMouseEnter={() => !o.disabled && setActive(i)}
              onClick={() => pick(o)}
            >
              <OptionText o={o} />
            </li>
          ))}
          {(note || shown.length === 0) && (
            <li className="dd-note" role="presentation">
              {note ?? "No matches"}
            </li>
          )}
        </ul>,
        document.body,
      )}
    </div>
  );
}
