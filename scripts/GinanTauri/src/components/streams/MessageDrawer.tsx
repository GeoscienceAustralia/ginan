import { useEffect, useRef } from "react";
import { createPortal } from "react-dom";
import type { StreamView } from "../../types/streams";
import { fmtAgo, fmtClock, fmtPct, fmtPeriod, msgTone } from "./detailUtils";
import "./streams-detail.css";

const FOCUSABLE = 'button:not([disabled]), a[href], input, select, textarea, [tabindex]:not([tabindex="-1"])';
const LATE_FACTOR = 1.2;

function ArrivalChart({ values, expected }: { values: number[]; expected: number | null }) {
  if (values.length === 0) return <p className="sd-muted sd-small">Not enough arrivals yet.</p>;
  const min = Math.min(...values);
  const max = Math.max(...values);
  const scale = Math.max(max, (expected ?? 0) * 1.5) || 1;
  const lateAt = expected == null ? Infinity : expected * LATE_FACTOR;
  const late = values.filter((v) => v > lateAt).length;
  return (
    <>
      <div className="sd-bars-head">
        <span className="sd-mono sd-muted sd-small">
          min {min.toFixed(2)} · max {max.toFixed(2)} s
        </span>
      </div>
      <div
        className="sd-bars"
        role="img"
        aria-label={`Last ${values.length} arrival intervals between ${min.toFixed(2)} and ${max.toFixed(2)} seconds${
          late ? `, ${late} late` : ""
        }`}
      >
        {expected != null && <span className="sd-bars-ref" aria-hidden="true" style={{ bottom: `${(100 * expected) / scale}%` }} />}
        {values.map((v, i) => (
          <span key={i} className={v > lateAt ? "is-late" : ""} style={{ height: `${Math.max((100 * v) / scale, 2)}%` }} />
        ))}
      </div>
      <span className="sd-muted sd-small">
        {expected != null && `Dashed line = expected ${expected.toFixed(2)} s. `}
        {late ? `${late} late arrival${late > 1 ? "s" : ""} (over ${LATE_FACTOR}× expected).` : "No late arrivals."}
      </span>
    </>
  );
}

export default function MessageDrawer({
  stream,
  msgNo,
  order,
  onNavigate,
  onClose,
}: {
  stream: StreamView;
  msgNo: string;
  order: string[];
  onNavigate: (msgNo: string) => void;
  onClose: () => void;
}) {
  const ref = useRef<HTMLElement>(null);
  const closeRef = useRef<HTMLButtonElement>(null);
  const onCloseRef = useRef(onClose);
  onCloseRef.current = onClose;

  // Focus in on open, back to the opener on close.
  useEffect(() => {
    const opener = document.activeElement as HTMLElement | null;
    closeRef.current?.focus();
    const esc = (e: KeyboardEvent) => e.key === "Escape" && onCloseRef.current();
    document.addEventListener("keydown", esc);
    return () => {
      document.removeEventListener("keydown", esc);
      if (opener && opener.isConnected) opener.focus();
    };
  }, []);

  const onKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Tab") {
      const items = ref.current?.querySelectorAll<HTMLElement>(FOCUSABLE);
      if (!items?.length) return;
      const first = items[0];
      const last = items[items.length - 1];
      const active = document.activeElement;
      if (e.shiftKey && (active === first || !ref.current?.contains(active))) {
        e.preventDefault();
        last.focus();
      } else if (!e.shiftKey && (active === last || !ref.current?.contains(active))) {
        e.preventDefault();
        first.focus();
      }
    }
  };

  const m = stream.messages.find((x) => x.msgNo === msgNo);
  const idx = order.indexOf(msgNo);
  const prev = idx > 0 ? stream.messages.find((x) => x.msgNo === order[idx - 1]) : undefined;
  const next = idx >= 0 && idx < order.length - 1 ? stream.messages.find((x) => x.msgNo === order[idx + 1]) : undefined;
  const total = stream.messages.reduce((n, x) => n + x.instances, 0);
  const tracked = m?.constellation ? stream.satsByConstellation[m.constellation] : undefined;

  return createPortal(
    <div className="sd sd-overlay" onMouseDown={(e) => e.target === e.currentTarget && onClose()}>
      <aside
        ref={ref}
        className="sd-drawer"
        role="dialog"
        aria-modal="true"
        aria-labelledby="sd-msg-title"
        onKeyDown={onKeyDown}
      >
        <div className="sd-drawer-head">
          <span className="sd-muted">
            <span className="sd-mono sd-strong">{stream.mount}</span> › Message {idx >= 0 ? idx + 1 : "–"} of {order.length}
          </span>
          <span className="sd-grow" />
          <button ref={closeRef} type="button" className="sd-iconbtn" aria-label="Close (Esc)" onClick={onClose}>
            <svg aria-hidden="true" width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round">
              <path d="M4 4l8 8M12 4l-8 8" />
            </svg>
          </button>
        </div>

        {!m ? (
          <p className="sd-empty" id="sd-msg-title">Message {msgNo} is no longer in this stream.</p>
        ) : (
          <div className="sd-drawer-body">
            <div className="sd-hero">
              <span className="sd-chip-lg" style={{ background: `var(--sd-bg-${msgTone(m)})`, color: `var(--sd-ink-${msgTone(m)})` }}>
                {m.name}
              </span>
              <h2 id="sd-msg-title" className="sd-mono">{m.msgNo}</h2>
              <p>{m.description}</p>
            </div>

            <dl className="sd-stats">
              <div>
                <dt>Last received</dt>
                <dd className="sd-mono">{fmtClock(m.lastRxMs, true)}</dd>
                <dd className="sd-muted sd-small">GPST · {fmtAgo(m.ageS)}</dd>
              </div>
              <div>
                <dt>Periodicity</dt>
                <dd className="sd-mono">{fmtPeriod(m.periodicityS)}</dd>
                <dd className={`sd-small ${m.late ? "sd-late" : "sd-muted"}`}>{m.late ? "Late" : "On time"}</dd>
              </div>
              <div>
                <dt>Count</dt>
                <dd className="sd-mono">{m.instances.toLocaleString()}</dd>
                <dd className="sd-muted sd-small">{fmtPct(total ? m.instances / total : 0)} of this stream</dd>
              </div>
              <div>
                <dt>Satellites</dt>
                <dd className="sd-mono">{m.lastSats.length}</dd>
                <dd className="sd-muted sd-small">{tracked != null ? `of ${tracked} ${m.constellation} tracked` : "in latest message"}</dd>
              </div>
            </dl>

            <section aria-labelledby="sd-h-arr" className="sd-dsec">
              <h3 id="sd-h-arr" className="sd-eyebrow">Arrival interval · last {m.interArrivalS.length || 60}</h3>
              <ArrivalChart values={m.interArrivalS} expected={m.periodicityS} />
            </section>

            {m.lastSignals.length > 0 && (
              <section aria-labelledby="sd-h-sig" className="sd-dsec">
                <h3 id="sd-h-sig" className="sd-eyebrow">Signals</h3>
                <div className="sd-chips">
                  {m.lastSignals.map((s) => (
                    <span key={s} className="sd-chip sd-chip-md sd-mono">{s}</span>
                  ))}
                </div>
              </section>
            )}

            {m.lastSats.length > 0 && (
              <section aria-labelledby="sd-h-in" className="sd-dsec">
                <div className="sd-bars-head">
                  <h3 id="sd-h-in" className="sd-eyebrow">Satellites in latest message</h3>
                  <span className="sd-mono sd-muted sd-small">{m.lastSats.length}</span>
                </div>
                <ul className="sd-prn-grid">
                  {m.lastSats.map((p) => (
                    <li key={p} className="sd-mono">{p}</li>
                  ))}
                </ul>
              </section>
            )}
          </div>
        )}

        <div className="sd-drawer-foot">
          {prev ? (
            <button type="button" className="sd-pager" onClick={() => onNavigate(prev.msgNo)}>
              ‹ {prev.msgNo} {prev.name}
            </button>
          ) : (
            <button type="button" className="sd-pager" disabled>No previous</button>
          )}
          {next ? (
            <button type="button" className="sd-pager" onClick={() => onNavigate(next.msgNo)}>
              {next.msgNo} {next.name} ›
            </button>
          ) : (
            <button type="button" className="sd-pager" disabled>No next</button>
          )}
        </div>
      </aside>
    </div>,
    document.body,
  );
}
