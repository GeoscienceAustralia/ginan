import { useEffect, useMemo, useRef, useState } from "react";
import { useAppStore } from "../store/appStore";
import {
  AlertIcon,
  CheckIcon,
  ChevronDownIcon,
  ChevronRightIcon,
  CloseIcon,
  PackageIcon,
  TerminalIcon,
  TrashIcon,
} from "./icons";

export type WorkflowEventStatus =
  | "active"
  | "success"
  | "warning"
  | "error"
  | "skipped"
  | "info";

export type WorkflowEvent = {
  message: string;
  category: string;
  status: WorkflowEventStatus;
  statusLabel: string;
};

const ANSI_ESCAPE = /\x1b\[[0-9;]*m/g;
// Every marker GinanUI puts at the head of a Workflow line (see
// src/config/workflowSteps.ts). Stripped here because the row draws its own
// status glyph in the gutter.
const LEADING_MARKER =
  /^(?:✅|❌|⚠️?|⏭|⬇|↻|✗|📡|⚙️?|🛑|📄|📂|📁|📊|📦|📥|📋|📖|🔍|🔄|⚒️?|✉️?|ℹ️?)\s*/u;

function categoryFor(message: string): string {
  // Ordered most-specific first: a plot line names PEA, and a bias line names
  // both BIA and RINEX, so the earlier test has to win.
  if (/plots?|visualisations?|\.pos\b/i.test(message)) return "Visualisation";
  if (/\bpea\b|execution|processing|cancelled/i.test(message)) return "Processing";
  // Ahead of the product tests: the support-file step says "downloaded
  // metadata", which would otherwise read as a product download.
  if (
    /antex|\.atx\b|antenna|static support|support files?|(?:download\w*)\s+metadata/i
      .test(message)
  ) {
    return "Support files";
  }
  if (
    /\b(?:sp3|clk|brdc|bia|ppp)\b|date-dependent|analysis cent|not published|\bproducts?\b|download/i
      .test(message)
  ) {
    return "Correction data";
  }
  if (/cddis|connectivity|credential|earthdata|netrc|server:/i.test(message)) {
    return "Connection";
  }
  if (/rinex|\brnx\b|observation code|compression|prepar/i.test(message)) {
    return "Observation data";
  }
  if (/output directory/i.test(message)) return "Output";
  if (/binary|version/i.test(message)) return "Application setup";
  return "Workflow";
}

function statusFor(
  raw: string,
  isLatestActive: boolean,
): Pick<WorkflowEvent, "status" | "statusLabel"> {
  if (
    /^(?:\s*❌|\s*✗)|\[ERR\]|finished with exit code (?!0\b)\d+/i.test(raw)
  ) {
    return { status: "error", statusLabel: "Error" };
  }
  // A stopped run is neither a failure nor a warning; GinanUI marks it 🛑.
  if (/^\s*🛑|^\s*stopped\b/i.test(raw)) {
    return { status: "skipped", statusLabel: "Stopped" };
  }
  // GinanUI marks a failed run ⚠️, but a run that produced no solution is an
  // error here, so this has to be tested before the warning rule below.
  if (/execution failed|pea error/i.test(raw)) {
    return { status: "error", statusLabel: "Error" };
  }
  if (/^\s*⚠|\b(?:warning|no overlap|not found)\b/i.test(raw)) {
    return { status: "warning", statusLabel: "Attention" };
  }
  if (/\b(?:error|failed|failure|cannot|unavailable)\b|^\s*could not/i.test(raw)) {
    return { status: "error", statusLabel: "Error" };
  }
  if (/^\s*⏭|\bskipp(?:ed|ing)|processing cancelled\b/i.test(raw)) {
    return { status: "skipped", statusLabel: "Skipped" };
  }
  if (
    /^\s*✅|\bsuccessfully\b|\b(?:selected|extracted|ready|present|connectivity ok|finished with exit code 0|already plain rinex)\b/i.test(raw)
  ) {
    return { status: "success", statusLabel: "Complete" };
  }
  if (
    isLatestActive &&
    /(^\s*⬇)|\b(?:starting|checking|downloading|fetching|preparing|running|using|resuming|writing|scanning|retrieving|validating|generating|populating)\b/i.test(raw)
  ) {
    return { status: "active", statusLabel: "In progress" };
  }
  return { status: "info", statusLabel: "Update" };
}

export function parseWorkflowLine(raw: string, isLatestActive = false): WorkflowEvent {
  const plain = raw.replace(ANSI_ESCAPE, "").trim();
  const message = plain
    .replace(/^\[ERR\]\s*/i, "")
    .replace(LEADING_MARKER, "")
    .trim();
  const status = statusFor(plain, isLatestActive);

  return {
    message: message || "Workflow update",
    category: categoryFor(message),
    ...status,
  };
}

/**
 * A run of consecutive events that share a category.
 *
 * A single run produces long stretches of one topic — four Connection steps
 * while credentials are checked, six Correction data steps while the catalogue
 * is scanned — and listing every one flat buries the shape of the run. Runs of
 * two or more collapse behind one header; the rest render as plain rows.
 */
type WorkflowGroup = {
  /** Stable across appends: the first event of a run keeps its index. */
  key: string;
  category: string;
  events: WorkflowEvent[];
  /** Index into `lines` of the first event, for React keys. */
  startIndex: number;
  status: WorkflowEventStatus;
  statusLabel: string;
};

/**
 * Worst-first, except that anything still running outranks a settled failure —
 * a group the run is currently inside should read as in progress.
 */
function groupStatus(events: WorkflowEvent[]): WorkflowEventStatus {
  const has = (s: WorkflowEventStatus) => events.some((e) => e.status === s);
  if (has("active")) return "active";
  if (has("error")) return "error";
  if (has("warning")) return "warning";
  if (has("success")) return "success";
  if (events.every((e) => e.status === "skipped")) return "skipped";
  return "info";
}

const GROUP_STATUS_LABEL: Record<WorkflowEventStatus, string> = {
  active: "In progress",
  error: "Needs attention",
  warning: "Review warning",
  success: "Complete",
  skipped: "Skipped",
  info: "Recorded",
};

export function groupWorkflowEvents(events: WorkflowEvent[]): WorkflowGroup[] {
  const groups: WorkflowGroup[] = [];
  events.forEach((event, index) => {
    const last = groups[groups.length - 1];
    if (last && last.category === event.category) {
      last.events.push(event);
      return;
    }
    groups.push({
      key: `${index}:${event.category}`,
      category: event.category,
      events: [event],
      startIndex: index,
      status: event.status,
      statusLabel: event.statusLabel,
    });
  });
  return groups.map((g) => {
    const status = groupStatus(g.events);
    return { ...g, status, statusLabel: GROUP_STATUS_LABEL[status] };
  });
}

/**
 * Collapsed by default so the list reads as a sequence of topics. Two kinds of
 * group open themselves: the one the run is currently inside, and any group
 * holding a problem — exactly the ones worth reading without being asked for.
 */
function groupOpensByDefault(group: WorkflowGroup, isLast: boolean): boolean {
  if (group.status === "error" || group.status === "warning") return true;
  return isLast && group.status === "active";
}

function StatusIcon({ status }: { status: WorkflowEventStatus }) {
  if (status === "active") return <span className="wf-spinner" />;
  if (status === "success") return <CheckIcon size={14} />;
  if (status === "warning") return <AlertIcon size={14} />;
  if (status === "error") return <CloseIcon size={14} />;
  if (status === "skipped") return <ChevronRightIcon size={14} />;
  return <TerminalIcon size={14} />;
}

/**
 * One step. The category is shown only on a row that stands on its own —
 * inside a group the header already names it.
 */
function EventRow({
  event,
  showCategory = false,
}: {
  event: WorkflowEvent;
  showCategory?: boolean;
}) {
  return (
    <li className={`wf-item wf-item--${event.status}`}>
      <div className="wf-row">
        <span className="wf-glyph" aria-hidden="true">
          <StatusIcon status={event.status} />
        </span>
        <span className="wf-body">
          <span className="wf-title">{event.message}</span>
        </span>
        <span className="wf-meta">
          {showCategory && <span className="wf-meta-dim">{event.category}</span>}
          <span className="wf-meta-state">{event.statusLabel}</span>
        </span>
      </div>
    </li>
  );
}

function GroupRow({
  group,
  open,
  onToggle,
}: {
  group: WorkflowGroup;
  open: boolean;
  onToggle: () => void;
}) {
  const listId = `wf-group-${group.key.replace(/[^\w-]/g, "-")}`;
  // Collapsed, the header stands in for the whole run, so it carries the most
  // recent line — the outcome, not the first thing that happened. Expanded,
  // that same line is the last row below, so the header drops it.
  const latest = group.events[group.events.length - 1];

  return (
    <li className={`wf-item wf-item--${group.status}${open ? " is-open" : ""}`}>
      <button
        type="button"
        className="wf-row wf-row--toggle"
        onClick={onToggle}
        aria-expanded={open}
        aria-controls={listId}
      >
        <span className="wf-glyph" aria-hidden="true">
          <StatusIcon status={group.status} />
        </span>
        <span className="wf-body">
          <span className="wf-title">{group.category}</span>
          {!open && <span className="wf-sub">{latest.message}</span>}
        </span>
        <span className="wf-meta">
          <span className="wf-meta-dim">{group.events.length} steps</span>
          <span className="wf-meta-state">{group.statusLabel}</span>
          <span className="wf-chevron" aria-hidden="true">
            <ChevronDownIcon size={13} />
          </span>
        </span>
      </button>

      <ol className="wf-children" id={listId} hidden={!open}>
        {group.events.map((event, index) => (
          <EventRow key={`${group.startIndex + index}-${event.message}`} event={event} />
        ))}
      </ol>
    </li>
  );
}

/**
 * How far PEA is through the run: epochs processed, how many remain and a bar,
 * when the window is bounded; just the running count when it isn't (realtime,
 * or a run with no end epoch).
 */
function EpochProgress({
  processed,
  total,
  running,
}: {
  processed: number;
  total: number | null;
  running: boolean;
}) {
  const fmt = (n: number) => n.toLocaleString();
  if (total === null) {
    return (
      <div className="wf-epochs">
        <span className="wf-epochs-text">
          <span><strong>{fmt(processed)}</strong> {processed === 1 ? "epoch" : "epochs"} processed</span>
        </span>
      </div>
    );
  }
  const done = Math.min(processed, total);
  const left = total - done;
  const pct = total > 0 ? (done / total) * 100 : 0;
  return (
    <div className="wf-epochs">
      <span className="wf-epochs-text">
        <span>Epoch <strong>{fmt(done)}</strong> of {fmt(total)}</span>
        <span className="wf-epochs-left">
          {left === 0 ? "none left" : `${fmt(left)} left`}
          {!running && left > 0 ? " · stopped" : ""}
        </span>
      </span>
      <div
        className="wf-epochs-bar"
        role="progressbar"
        aria-label="Epochs processed"
        aria-valuemin={0}
        aria-valuemax={total}
        aria-valuenow={done}
      >
        <span style={{ width: `${pct}%` }} />
      </div>
    </div>
  );
}

/**
 * The user-facing workflow history. The Console deliberately remains a raw
 * terminal; this view turns the same high-level workflow messages into a
 * readable, chronological list without changing the event contract.
 *
 * Laid out as full-bleed rows on one flat panel — the same idiom as the
 * settings cards and the console — rather than a column of bordered cards, so
 * every row spans the same width whether or not it carries a disclosure
 * control, and nothing is pushed inward to make room for one.
 */
export default function WorkflowPanel() {
  const lines = useAppStore((s) => s.workflowLines);
  const clearWorkflow = useAppStore((s) => s.clearWorkflow);
  const isProcessing = useAppStore((s) => s.isProcessing);
  const inputMode = useAppStore((s) => s.inputMode);
  const epochProgress = useAppStore((s) => s.epochProgress);
  const scrollRef = useRef<HTMLDivElement>(null);
  const stickToBottomRef = useRef(true);

  const events = useMemo(
    () => lines.map((line, index) => (
      parseWorkflowLine(line, isProcessing && index === lines.length - 1)
    )),
    [lines, isProcessing],
  );
  const groups = useMemo(() => groupWorkflowEvents(events), [events]);
  // Only groups the user has clicked; everything else follows the default
  // rule, so a group that turns active or errors still opens on its own.
  const [toggled, setToggled] = useState<Record<string, boolean>>({});
  const stepCount = `${events.length} ${events.length === 1 ? "step" : "steps"}`;

  const onScroll = () => {
    const el = scrollRef.current;
    if (!el) return;
    const distanceFromBottom = el.scrollHeight - el.scrollTop - el.clientHeight;
    stickToBottomRef.current = distanceFromBottom < 40;
  };

  useEffect(() => {
    if (!stickToBottomRef.current) return;
    const el = scrollRef.current;
    if (el) el.scrollTop = el.scrollHeight;
  }, [lines]);

  return (
    <section className="wf-panel" aria-label="Workflow">
      <div className="wf-header">
        <span className="wf-count">{stepCount}</span>
        <button
          className="btn-ghost wf-clear"
          onClick={clearWorkflow}
          title="Clear workflow history"
          disabled={events.length === 0}
        >
          <TrashIcon size={13} /> Clear
        </button>
      </div>

      {epochProgress && (
        <EpochProgress
          processed={epochProgress.processed}
          total={epochProgress.total}
          running={isProcessing}
        />
      )}

      <div
        className={`wf-scroll${events.length === 0 ? " is-empty" : ""}`}
        ref={scrollRef}
        onScroll={onScroll}
        role="log"
        aria-live="polite"
        aria-relevant="additions"
      >
        {events.length === 0 ? (
          <div className="wf-empty">
            <PackageIcon size={20} />
            <p className="wf-empty-title">Your run will appear here</p>
            <p className="wf-empty-text">{inputMode === "realtime"
              ? "Configure a stream and start processing. Connection and processing steps appear as they happen."
              : "Select an observation file and start processing. Each preparation, download and processing step appears as it completes."
            }</p>
          </div>
        ) : (
          <ol className="wf-list">
            {groups.map((group, index) =>
              group.events.length === 1 ? (
                <EventRow
                  key={`${group.startIndex}-${lines[group.startIndex]}`}
                  event={group.events[0]}
                  showCategory
                />
              ) : (
                <GroupRow
                  key={group.key}
                  group={group}
                  open={
                    toggled[group.key] ??
                    groupOpensByDefault(group, index === groups.length - 1)
                  }
                  onToggle={() =>
                    setToggled((prev) => ({
                      ...prev,
                      [group.key]: !(
                        prev[group.key] ??
                        groupOpensByDefault(group, index === groups.length - 1)
                      ),
                    }))
                  }
                />
              ),
            )}
          </ol>
        )}
      </div>
    </section>
  );
}
