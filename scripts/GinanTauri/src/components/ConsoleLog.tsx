import { useEffect, useRef } from "react";
import { useAppStore } from "../store/appStore";
import { TrashIcon } from "./icons";

// A small hand-written ANSI colour code to HTML converter, to avoid pulling in a dependency.
function ansiToHtml(line: string): string {
  const ansiColors: Record<number, string> = {
    30: "#000",    31: "#e06c75", 32: "#98c379", 33: "#e5c07b",
    34: "#61afef", 35: "#c678dd", 36: "#56b6c2", 37: "#abb2bf",
    90: "#5c6370", 91: "#e06c75", 92: "#98c379", 93: "#e5c07b",
    94: "#61afef", 95: "#c678dd", 96: "#56b6c2", 97: "#ffffff",
  };

  let html = line
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");

  let bold = false;
  html = html.replace(/\x1b\[([0-9;]*)m/g, (_match, codes: string) => {
    const parts = codes.split(";").map(Number);
    const out: string[] = [];
    for (const code of parts) {
      if (code === 0) { bold = false; out.push("</span>"); }
      else if (code === 1) { bold = true; }
      else if (ansiColors[code]) {
        const weight = bold ? "font-weight:bold;" : "";
        out.push(`<span style="color:${ansiColors[code]};${weight}">`);
      }
    }
    return out.join("");
  });

  return html;
}

/**
 * A single log channel, rendered as a scrolling terminal. Mirrors the original
 * GinanUI (Python) which split output into two tabs:
 *   • "workflow" — high-level status the user follows.
 *   • "console"  — raw PEA subprocess output + low-level technical detail.
 * The lines live in the store, so switching workspace tabs preserves the log;
 * only the scroll position resets.
 */
export default function ConsoleLog({
  channel = "console",
}: {
  channel?: "workflow" | "console";
}) {
  const lines = useAppStore((s) =>
    channel === "workflow" ? s.workflowLines : s.consoleLines,
  );
  const clear = useAppStore((s) =>
    channel === "workflow" ? s.clearWorkflow : s.clearConsole,
  );
  const outputRef = useRef<HTMLDivElement>(null);
  const stickToBottomRef = useRef(true);

  const isWorkflow = channel === "workflow";
  const title = isWorkflow ? "Workflow" : "Console Output";
  const clearTitle = isWorkflow ? "Clear workflow log" : "Clear console output";
  const placeholder = isWorkflow
    ? "Workflow status will appear here - select a RINEX file and click Process."
    : "Waiting for PEA output... Select a RINEX file and click Process.";

  const onScroll = () => {
    const el = outputRef.current;
    if (!el) return;
    const distanceFromBottom = el.scrollHeight - el.scrollTop - el.clientHeight;
    stickToBottomRef.current = distanceFromBottom < 32;
  };

  useEffect(() => {
    if (!stickToBottomRef.current) return;
    const el = outputRef.current;
    if (el) el.scrollTop = el.scrollHeight;
  }, [lines]);

  return (
    <div className="console-panel">
      <div className="console-header">
        <span>{title}</span>
        <button
          className="btn-ghost"
          style={{ padding: "3px 8px", fontSize: 12 }}
          onClick={clear}
          title={clearTitle}
        >
          <TrashIcon size={13} /> Clear
        </button>
      </div>
      <div
        className={`console-output${isWorkflow ? " workflow-output" : ""}`}
        ref={outputRef}
        onScroll={onScroll}
      >
        {lines.length === 0 ? (
          <span className="placeholder">{placeholder}</span>
        ) : (
          lines.map((line, i) => (
            <div
              key={i}
              className={line.startsWith("[ERR]") ? "err-line" : undefined}
              dangerouslySetInnerHTML={{ __html: ansiToHtml(line) }}
            />
          ))
        )}
      </div>
    </div>
  );
}
