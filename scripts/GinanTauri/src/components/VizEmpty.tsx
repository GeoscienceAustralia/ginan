import { ResetIcon } from "./icons";

/**
 * The Visualization tab's "nothing to draw yet" state, shared by the position
 * and residual views so the two read the same when there is no output to plot.
 */
export default function VizEmpty({
  title,
  body,
  onRetry,
}: {
  title: string;
  body: string;
  onRetry?: () => void;
}) {
  return (
    <div className="viz-empty">
      <div className="viz-empty-title">{title}</div>
      <p>{body}</p>
      {onRetry && (
        <button className="btn-secondary" onClick={onRetry}>
          <ResetIcon size={13} /> Check again
        </button>
      )}
    </div>
  );
}

/** Last path segment, for progress and error text. */
export function shortName(path: string | null): string {
  if (!path) return "file";
  return path.split(/[\\/]/).pop() ?? path;
}
