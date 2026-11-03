import { useMemo } from "react";
import { useAppStore, getConstellationObsCodesForConfig } from "../store/appStore";
import { phaseCodes } from "../config/obsCodes";
import type { ConstellationObsCodes } from "../types";

/** The five systems PEA takes `sys_options` for, in the order the config writes them. */
const SYSTEMS: { key: keyof ConstellationObsCodes; label: string }[] = [
  { key: "gps", label: "GPS" },
  { key: "gal", label: "Galileo" },
  { key: "glo", label: "GLONASS" },
  { key: "bds", label: "BeiDou" },
  { key: "qzs", label: "QZSS" },
];

/**
 * Read-only view of the `code_priorities` a run would write, per constellation.
 *
 * Without it the Configuration page shows every other part of the generated
 * config but not the signal list, which is the part a user is most likely to
 * have changed by hand in the sidebar.
 *
 * It is derived from `getConstellationObsCodesForConfig` with the same `allowed`
 * set `buildConfigInputs` passes, and then filtered with `phaseCodes` the way
 * `write_observation_code_priorities` filters with `is_phase_code`, so what is
 * listed here is what reaches the file. An empty list is not "no signals" but
 * "key omitted", which makes PEA fall back to the template's own priorities —
 * worth saying, because the difference is invisible in the YAML below.
 */
export default function CodePrioritiesSummary() {
  const constellationCodes = useAppStore((s) => s.constellationCodes);
  // `availableConstellationsForCurrentSelection` reads these four, so they are
  // what this has to re-run on.
  const availableObsCodes = useAppStore((s) => s.availableObsCodes);
  const pppCatalogue = useAppStore((s) => s.pppCatalogue);
  const pppSelection = useAppStore((s) => s.pppSelection);
  const inputMode = useAppStore((s) => s.inputMode);

  const written = useMemo(() => {
    const allowed = useAppStore.getState().availableConstellationsForCurrentSelection();
    const codes = getConstellationObsCodesForConfig(constellationCodes, allowed);
    return SYSTEMS.map(({ key, label }) => ({
      key,
      label,
      codes: phaseCodes(codes[key]),
    }));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [constellationCodes, availableObsCodes, pppCatalogue, pppSelection, inputMode]);

  const anyWritten = written.some((s) => s.codes.length > 0);

  return (
    <div className="codeprio">
      {written.map(({ key, label, codes }) => (
        <div className="codeprio-row" key={key}>
          <div className="codeprio-sys">
            <span className="codeprio-name">{label}</span>
            <code className="codeprio-path">sys_options.{key}</code>
          </div>
          {codes.length > 0 ? (
            <ol className="codeprio-codes" aria-label={`${label} code priorities`}>
              {codes.map((code, i) => (
                <li className="codeprio-code" key={code}>
                  <span className="codeprio-rank">{i + 1}</span>
                  {code}
                </li>
              ))}
            </ol>
          ) : (
            <span className="codeprio-none">
              Not written - PEA keeps the template's own priorities
            </span>
          )}
        </div>
      ))}

      {!anyWritten && (
        <p className="codeprio-hint">
          No constellation has a signal selected, so the config sets no
          priorities at all and PEA uses the template throughout. Pick signals
          under <strong>Constellations</strong> in the sidebar.
        </p>
      )}
    </div>
  );
}
