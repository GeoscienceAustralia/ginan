import { useMemo, useRef, useState } from "react";
import type { MessageView, StreamsModel, StreamView } from "../../types/streams";
import Donut from "./Donut";
import {
  GROUP_LABEL,
  fmtAgo,
  fmtClock,
  fmtPct,
  fmtPeriod,
  msgTone,
} from "./detailUtils";
import "./streams-detail.css";

type SortKey = "msgNo" | "last" | "period" | "count";
type Sort = { key: SortKey; dir: "asc" | "desc" };

const val = (m: MessageView, k: SortKey): number =>
  k === "msgNo"
    ? Number(m.msgNo) || 0
    : k === "last"
      ? (m.lastRxMs ?? -1)
      : k === "period"
        ? (m.periodicityS ?? -1)
        : m.instances;

const LateIcon = ({ label }: { label?: string }) => (
  <svg {...(label ? { role: "img", "aria-label": label } : { "aria-hidden": true })} width="13" height="13" viewBox="0 0 14 14" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
    <path d="M7 1.8L13 12H1z" strokeLinejoin="round" />
    <path d="M7 5.6v3M7 10.2v.1" />
  </svg>
);

export default function MessagesView({
  stream,
  model,
  onOpenMessage,
}: {
  stream: StreamView;
  model: StreamsModel;
  onOpenMessage: (msgNo: string) => void;
}) {
  const [query, setQuery] = useState("");
  const [sort, setSort] = useState<Sort>({ key: "count", dir: "desc" });
  const [focused, setFocused] = useState(false);
  const frozen = useRef<string[]>([]);

  const all = stream.messages;
  const total = all.reduce((n, m) => n + m.instances, 0);

  const rows = useMemo(() => {
    const q = query.trim().toLowerCase();
    const hit = q
      ? all.filter((m) => m.msgNo.startsWith(q) || m.name.toLowerCase().includes(q))
      : all;
    return [...hit].sort((a, b) => {
      const d = val(a, sort.key) - val(b, sort.key);
      return (sort.dir === "asc" ? d : -d) || a.msgNo.localeCompare(b.msgNo);
    });
  }, [all, query, sort]);

  // Hold the order while a row has focus so a 5 s snapshot can't move it away.
  let shown = rows;
  if (focused && frozen.current.length) {
    const pos = (m: MessageView) => {
      const i = frozen.current.indexOf(m.msgNo);
      return i < 0 ? Infinity : i;
    };
    shown = [...rows].sort((a, b) => pos(a) - pos(b));
  } else {
    frozen.current = rows.map((m) => m.msgNo);
  }

  const groups = useMemo(() => {
    const g = new Map<MessageView["group"], { n: number; tone: string }>();
    for (const m of all) {
      const e = g.get(m.group) ?? { n: 0, tone: msgTone(m) };
      e.n += m.instances;
      g.set(m.group, e);
    }
    return [...g.entries()].sort((a, b) => b[1].n - a[1].n);
  }, [all]);

  const sortBy = (key: SortKey) =>
    setSort((s) => ({ key, dir: s.key === key && s.dir === "desc" ? "asc" : "desc" }));
  const th = (key: SortKey, text: string, cls = "") => (
    <th
      scope="col"
      className={cls}
      aria-sort={sort.key === key ? (sort.dir === "asc" ? "ascending" : "descending") : "none"}
    >
      <button type="button" onClick={() => sortBy(key)}>
        {text}
        {sort.key === key && (sort.dir === "asc" ? " ▲" : " ▼")}
      </button>
    </th>
  );

  const since =
    model.resetAtMs != null
      ? `since reset ${new Date(model.resetAtMs).toLocaleTimeString([], { hour12: false })}`
      : "since connect";

  if (all.length === 0) {
    return (
      <section className="sd-card" aria-labelledby="sd-h-list">
        <h2 id="sd-h-list">Messages</h2>
        <p className="sd-empty">
          {model.recordDecodedOff
            ? "Decoded records are off. Turn on Recordings › Decoded RTCM (JSON) and restart the run to see message types, counts and periodicity."
            : "Waiting for the first decoded records (updates every 5 s)..."}
        </p>
      </section>
    );
  }

  return (
    <div className="sd-split">
      <section className="sd-card sd-donut-card" aria-labelledby="sd-h-donut">
        <div className="sd-card-head">
          <h2 id="sd-h-donut">Message mix</h2>
          <span className="sd-muted">{since}</span>
        </div>
        <Donut
          total={total}
          label={`Messages ${since}, ${total} in all: ${groups
            .map(([g, e]) => `${GROUP_LABEL[g]} ${fmtPct(e.n / total)}`)
            .join(", ")}`}
          segments={all.map((m) => ({
            key: m.msgNo,
            label: `${m.msgNo} · ${m.name}`,
            value: m.instances,
            color: `var(--sd-bg-${msgTone(m)})`,
          }))}
        />
        <ul className="sd-legend">
          {groups.map(([g, e]) => (
            <li key={g}>
              <span className="sd-swatch" aria-hidden="true" style={{ background: `var(--sd-bg-${e.tone})` }} />
              <span>{GROUP_LABEL[g]}</span>
              <span className="sd-grow" />
              <span className="sd-mono sd-muted">{fmtPct(total ? e.n / total : 0)}</span>
            </li>
          ))}
        </ul>
      </section>

      <section className="sd-card sd-list-card" aria-labelledby="sd-h-list">
        <div className="sd-toolbar">
          <div className="sd-title">
            <h2 id="sd-h-list">Messages</h2>
            <span className="sd-muted sd-small">
              {shown.length} of {all.length} types · counts {since}
            </span>
          </div>
          <label className="sd-search">
            <svg aria-hidden="true" width="14" height="14" viewBox="0 0 14 14" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
              <circle cx="6" cy="6" r="4.2" />
              <path d="M9.2 9.2L12.5 12.5" />
            </svg>
            <span className="sd-sr">Search messages</span>
            <input type="search" placeholder="Number or name" value={query} onChange={(e) => setQuery(e.target.value)} />
          </label>
          <button type="button" className="sd-btn" onClick={() => void model.resetCounters(stream.mount)}>
            Reset this stream
          </button>
        </div>
        <div className="sd-scroll">
          <table className="sd-table sd-msgs">
            <thead>
              <tr>
                {th("msgNo", "Type")}
                <th scope="col">Content</th>
                {th("last", "Last received (GPST)")}
                {th("period", "Periodicity", "sd-num")}
                {th("count", "Count", "sd-num")}
                <th scope="col" className="sd-num">Share</th>
              </tr>
            </thead>
            <tbody onFocus={() => setFocused(true)} onBlur={() => setFocused(false)}>
              {shown.map((m) => (
                <tr
                  key={m.msgNo}
                  onClick={(e) => {
                    e.currentTarget.querySelector<HTMLElement>("button.sd-rowlink")?.focus();
                    onOpenMessage(m.msgNo);
                  }}
                >
                  <td data-label="Type">
                    <span className="sd-type sd-mono" style={{ background: `var(--sd-bg-${msgTone(m)})`, color: `var(--sd-ink-${msgTone(m)})` }}>
                      {m.msgNo}
                    </span>
                  </td>
                  <td data-label="Content" className="sd-wrap">
                    <button type="button" className="sd-rowlink" aria-label={`${m.msgNo} ${m.name}`}>
                      {m.name}
                    </button>
                  </td>
                  <td data-label="Last received" className="sd-wrap">
                    <span className={`sd-mono${m.late ? " sd-late" : ""}`}>
                      {fmtClock(m.lastRxMs, true)}
                      {m.late && <LateIcon label="late" />}
                    </span>
                    <span className="sd-muted sd-small">{fmtAgo(m.ageS)}</span>
                  </td>
                  <td data-label="Periodicity" className="sd-mono sd-num">{fmtPeriod(m.periodicityS)}</td>
                  <td data-label="Count" className="sd-mono sd-num">{m.instances.toLocaleString()}</td>
                  <td data-label="Share" className="sd-mono sd-num sd-muted">{fmtPct(m.share)}</td>
                </tr>
              ))}
              {shown.length === 0 && (
                <tr className="sd-nomatch">
                  <td colSpan={6}>No messages match “{query}”.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
        <span className="sd-note">
          <LateIcon />
          Late: nothing received for more than 1.5× the message's usual interval.
        </span>
      </section>
    </div>
  );
}
