/**
 * Read-only path value plus its Browse action.
 *
 * `stacked` puts the button under the value instead of beside it — use it in
 * the sidebar, where there isn't room for both on one line.
 */
export default function PathControl({
  value,
  empty,
  onBrowse,
  browseLabel = "Browse...",
  stacked = false,
}: {
  value: string | null;
  empty: string;
  onBrowse: () => void;
  browseLabel?: string;
  stacked?: boolean;
}) {
  return (
    <div className={`path-control${stacked ? " path-control--stacked" : ""}`}>
      <span className={`path-value${value ? "" : " is-empty"}`} title={value ?? undefined}>
        {value ?? empty}
      </span>
      <button className="btn-secondary" onClick={onBrowse}>
        {browseLabel}
      </button>
    </div>
  );
}
