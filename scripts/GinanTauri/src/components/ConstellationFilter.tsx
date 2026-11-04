import { useMemo, useState } from "react";
import { useAppStore } from "../store/appStore";
import { isPhaseCode } from "../config/obsCodes";
import type { Constellation } from "../types";
import { ChevronRightIcon, ChevronUpIcon, ChevronDownIcon } from "./icons";

const CONSTELLATION_FULL_NAMES: Record<Constellation, string> = {
  GPS: "GPS (Global Navigation System)",
  GAL: "GAL (Galileo)",
  GLO: "GLO (GLONASS)",
  BDS: "BDS (BeiDou)",
  QZS: "QZS (Quasi-Zenith Satellite System)",
};

const ALL_CONSTELLATIONS: Constellation[] = ["GPS", "GAL", "GLO", "BDS", "QZS"];

export default function ConstellationFilter() {
  const [expandedConstellations, setExpandedConstellations] = useState<Set<Constellation>>(new Set());
  const availableObsCodes = useAppStore((s) => s.availableObsCodes);
  const isRealtime = useAppStore((s) => s.inputMode === "realtime");
  const constellationCodes = useAppStore((s) => s.constellationCodes);
  const biaValidation = useAppStore((s) => s.biaValidation);
  const validateCodesAgainstBia = useAppStore((s) => s.validateCodesAgainstBia);
  const updateConstellationCode = useAppStore((s) => s.updateConstellationCode);
  const reorderConstellationCodes = useAppStore((s) => s.reorderConstellationCodes);
  const setConstellationEnabled = useAppStore((s) => s.setConstellationEnabled);
  const pppSelection = useAppStore((s) => s.pppSelection);
  const pppCatalogue = useAppStore((s) => s.pppCatalogue);
  const availableConstellationsForCurrentSelection = useAppStore(
    (s) => s.availableConstellationsForCurrentSelection
  );

  // Determine which constellations are present in the loaded RINEX data
  const activeConstellations = ALL_CONSTELLATIONS.filter((c) => {
    if (!availableObsCodes) return false;
    const key = c.toLowerCase() as keyof typeof availableObsCodes;
    return availableObsCodes[key] && availableObsCodes[key].length > 0;
  });

  // The subset the selected PPP combo can bias-correct. Constellations present
  // in the RINEX but outside this set are shown disabled with a tooltip.
  const allowed = useMemo(
    () => availableConstellationsForCurrentSelection(),
    [availableObsCodes, pppCatalogue, pppSelection, availableConstellationsForCurrentSelection]
  );

  return (
    <div className="constellation-filter">
      {/* Header row with description and Validate BIA button */}
      <div className="cf-header-row">
        <div className="cf-validate-area">
          <button
            className="cf-validate-btn"
            onClick={() => validateCodesAgainstBia()}
            disabled={biaValidation.isLoading}
            title="Check which codes are available in downloaded BIA files"
          >
            {biaValidation.isLoading ? "validating..." : "Validate BIA"}
          </button>
          {biaValidation.error && (
            <span className="cf-error-text" title={biaValidation.error}>
              {biaValidation.error}
            </span>
          )}
        </div>
      </div>

      {/* Constellation sections */}
      {activeConstellations.length === 0 ? (
        <div className="cf-placeholder">
          {isRealtime
            ? "Run a stream test to list the signals."
            : "Load a RINEX file to see observation codes."}
        </div>
      ) : (
        activeConstellations.map((constellation) => {
          const key = constellation.toLowerCase() as keyof typeof constellationCodes;
          // Phase signals only. The store already narrows to these, but a list it was
          // handed some other way must not put C/D/S rows on screen. Each row keeps its
          // index into the stored list, which the update and reorder actions take.
          const rows = constellationCodes[key]
            .map((item, index) => ({ item, index }))
            .filter(({ item }) => isPhaseCode(item.code));
          if (rows.length === 0) return null;
          const isAllowed = allowed.has(constellation);
          const disallowReason = isAllowed
            ? undefined
            : `Not published by ${pppSelection.provider}/${pppSelection.series}/${pppSelection.project} for this date`;
          const isExpanded = expandedConstellations.has(constellation);
          const toggleExpand = () => {
            const newSet = new Set(expandedConstellations);
            if (isExpanded) newSet.delete(constellation);
            else newSet.add(constellation);
            setExpandedConstellations(newSet);
          };
          return (
            <div
              key={constellation}
              className={`cf-section${isAllowed ? "" : " cf-section--disabled"}`}
            >
              <div
                className="cf-section-header"
                style={{ cursor: "pointer" }}
                onClick={toggleExpand}
                title={disallowReason}
              >
                <span className={`cf-section-chevron${isExpanded ? " is-open" : ""}`}>
                  <ChevronRightIcon size={13} />
                </span>
                <input
                  type="checkbox"
                  className="cf-checkbox"
                  checked={isAllowed && rows.some((r) => r.item.enabled)}
                  disabled={!isAllowed}
                  ref={(el) => {
                    if (el) el.indeterminate =
                      isAllowed &&
                      rows.some((r) => r.item.enabled) && rows.some((r) => !r.item.enabled);
                  }}
                  onChange={(e) => {
                    e.stopPropagation();
                    setConstellationEnabled(constellation, e.target.checked);
                  }}
                  title={
                    disallowReason ?? "Enable / disable all codes for this constellation"
                  }
                />
                {CONSTELLATION_FULL_NAMES[constellation]}
                {/* Spell the reason out — a title tooltip alone left the
                    disabled state easy to miss when the product changed. */}
                {!isAllowed && (
                  <span className="cf-unavailable-badge" title={disallowReason}>
                    not in {pppSelection.provider}
                  </span>
                )}
              </div>
              {isExpanded && (
                <div className="cf-code-list">
                  {rows.map(({ item, index }, position) => (
                    <div
                      key={`${item.code}-${index}`}
                      className={`cf-code-row${item.enabled && isAllowed ? "" : " cf-code-row--disabled"}`}
                    >
                      <input
                        type="checkbox"
                        className="cf-checkbox"
                        checked={item.enabled && isAllowed}
                        disabled={!isAllowed}
                        onChange={(e) =>
                          updateConstellationCode(constellation, index, {
                            enabled: e.target.checked,
                          })
                        }
                      />
                      <span
                        className={`cf-code-name${!item.valid ? " cf-code-name--invalid" : ""}${!item.enabled || !isAllowed ? " cf-code-name--greyed" : ""}`}
                      >
                        {item.code}
                      </span>
                      <div className="cf-reorder-btns">
                        <button
                          className="cf-reorder-btn"
                          title="Move up (higher priority)"
                          disabled={position === 0}
                          onClick={() =>
                            reorderConstellationCodes(constellation, index, rows[position - 1].index)
                          }
                        >
                          <ChevronUpIcon size={12} />
                        </button>
                        <button
                          className="cf-reorder-btn"
                          title="Move down (lower priority)"
                          disabled={position === rows.length - 1}
                          onClick={() =>
                            reorderConstellationCodes(constellation, index, rows[position + 1].index)
                          }
                        >
                          <ChevronDownIcon size={12} />
                        </button>
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>
          );
        })
      )}
    </div>
  );
}
