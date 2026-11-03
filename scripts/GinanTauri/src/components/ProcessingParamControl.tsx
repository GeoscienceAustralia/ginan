import { useAppStore } from "../store/appStore";
import type { ParamDef } from "../config/processingParams";
import { ResetIcon } from "./icons";

/**
 * One processing parameter, rendered from its definition.
 *
 * The store holds only parameters moved off the template default, so the
 * displayed value falls back to `def.defaultValue` when the path is absent.
 * That keeps "never touched" and "set back to the default" identical, and it
 * means an untouched install generates the template's YAML unchanged.
 */
export default function ProcessingParamControl({ def }: { def: ParamDef }) {
  const stored = useAppStore((s) => s.processingParams[def.path]);
  const setParam = useAppStore((s) => s.setProcessingParam);
  const resetParam = useAppStore((s) => s.resetProcessingParam);

  const value = stored ?? def.defaultValue;
  const isModified = stored !== undefined && stored !== def.defaultValue;

  // Writing the default back removes the key rather than storing it.
  const commit = (next: boolean | number | string) => {
    if (next === def.defaultValue) resetParam(def.path);
    else setParam(def.path, next);
  };

  return (
    <div className="param-control">
      {def.type === "boolean" ? (
        <label className="param-switch">
          <input
            type="checkbox"
            checked={value as boolean}
            onChange={(e) => commit(e.target.checked)}
          />
        </label>
      ) : def.type === "enum" ? (
        <select
          value={value as string}
          onChange={(e) => commit(e.target.value)}
        >
          {def.options?.map((o) => (
            <option key={o.value} value={o.value}>
              {o.label}
            </option>
          ))}
        </select>
      ) : def.type === "string" ? (
        <input
          type="text"
          className="param-text"
          value={value as string}
          onChange={(e) => commit(e.target.value)}
        />
      ) : (
        <div className="param-number">
          <input
            type="number"
            value={value as number}
            min={def.min}
            max={def.max}
            step={def.step}
            onChange={(e) => {
              const n = e.target.value === "" ? def.defaultValue : Number(e.target.value);
              if (!Number.isNaN(n)) commit(n);
            }}
          />
          {def.unit && <span className="param-unit">{def.unit}</span>}
        </div>
      )}

      {isModified && (
        <button
          className="btn-icon param-reset"
          onClick={() => resetParam(def.path)}
          title={`Reset to default (${String(def.defaultValue)})`}
        >
          <ResetIcon size={13} />
        </button>
      )}
    </div>
  );
}
