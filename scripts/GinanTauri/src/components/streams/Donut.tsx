import { useState } from "react";
import "./streams-detail.css";

export interface DonutSegment {
  key: string;
  value: number;
  /** Shown in the centre while the segment is hovered; defaults to `key`. */
  label?: string;
  /** Any CSS colour, typically a var(). */
  color: string;
}

const R = 80;
const C = 2 * Math.PI * R;

/** Ring of proportional segments with the total in the centre. */
export default function Donut({
  segments,
  total,
  label,
  caption = "messages",
}: {
  segments: DonutSegment[];
  total: number;
  label: string;
  caption?: string;
}) {
  const sum = segments.reduce((n, s) => n + s.value, 0);
  const [hover, setHover] = useState<DonutSegment | null>(null);
  let acc = 0;
  return (
    <div className={`sd-donut${hover ? " is-hovering" : ""}`}>
      <svg role="img" aria-label={label} width="240" height="240" viewBox="0 0 200 200">
        <circle className="sd-donut-track" cx="100" cy="100" r={R} />
        {sum > 0 &&
          segments
            .filter((s) => s.value > 0)
            .map((s) => {
              const len = (C * s.value) / sum;
              const el = (
                <circle
                  key={s.key}
                  className={`sd-donut-seg${hover?.key === s.key ? " is-hover" : ""}`}
                  onMouseEnter={() => setHover(s)}
                  onMouseLeave={() => setHover(null)}
                  cx="100"
                  cy="100"
                  r={R}
                  style={{ stroke: s.color }}
                  strokeDasharray={`${Math.max(len - 1.2, 0.5).toFixed(2)} ${(C - len + 1.2).toFixed(2)}`}
                  strokeDashoffset={(-acc).toFixed(2)}
                />
              );
              acc += len;
              return el;
            })}
      </svg>
      {/* Hovering a segment swaps the total for that segment's exact count. */}
      <div className="sd-donut-centre" aria-hidden="true">
        <span className="sd-muted sd-donut-label">{hover ? hover.label ?? hover.key : "Total"}</span>
        <span className="sd-donut-total">{(hover ? hover.value : total).toLocaleString()}</span>
        <span className="sd-muted">
          {hover && sum > 0 ? `${((100 * hover.value) / sum).toFixed(1)}% of ${caption}` : caption}
        </span>
      </div>
    </div>
  );
}
