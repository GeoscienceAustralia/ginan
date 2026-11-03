import { isHttpUrl } from "./httpUrl";
import type { StreamBodyProps } from "./streamBody";

/**
 * Body of an HTTP card: one full-URL field. A saved login for the host is added
 * to the run config by the backend, so none is typed here.
 */
export default function HttpStreamBody({ product, onChange, onDone }: StreamBodyProps) {
  const invalid = product.value !== "" && !isHttpUrl(product.value);
  return (
    <div className="rt-field">
      <label className="rt-label">
        <span className="rt-label-text">URL</span>
        <input
          className={`rt-input rt-input-mono${invalid ? " rt-input-warn" : ""}`}
          placeholder="https://host/MOUNTPOINT"
          spellCheck={false}
          aria-invalid={invalid}
          value={product.value}
          onChange={(e) => onChange({ value: e.target.value.trim() })}
          // A typed URL has no pick, so Enter or leaving a valid field commits it.
          onBlur={() => isHttpUrl(product.value) && onDone?.()}
          onKeyDown={(e) => e.key === "Enter" && isHttpUrl(product.value) && onDone?.()}
        />
      </label>
      {invalid && <div className="rt-hint">Enter a full http:// or https:// address, such as https://host/MOUNTPOINT.</div>}
    </div>
  );
}
