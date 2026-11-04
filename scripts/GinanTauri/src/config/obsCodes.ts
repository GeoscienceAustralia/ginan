import type { ConstellationCodeConfig, ObservationCodeItem } from "../types";

/**
 * A RINEX 3 phase observable: `L`, a frequency digit and a tracking attribute
 * (`L1C`, `L5Q`).
 *
 * The constellation lists are phase signals only. PEA's `code_priorities` is the
 * list of phase signals it may use, and the bias tables validate a signal by its
 * phase bias; the code (`C`), Doppler (`D`) and strength (`S`) observables a RINEX
 * header also lists describe the same signals but are not entries in that list.
 */
const PHASE_CODE = /^L[0-9][A-Za-z]$/;

export function isPhaseCode(code: unknown): code is string {
  return typeof code === "string" && PHASE_CODE.test(code);
}

/** The phase codes of `codes`, in their original order. */
export function phaseCodes(codes: readonly string[] | null | undefined): string[] {
  return (codes ?? []).filter(isPhaseCode);
}

const BIAS_CODE = /^[CL][0-9][A-Za-z]$/;

/**
 * The phase signals a bias table covers, as `L` codes.
 *
 * A table lists a code bias (`C1C`) and, depending on the analysis centre, a phase
 * bias (`L1C`) for a signal. Some centres publish phase biases for only some
 * constellations (CODE's GLONASS and BeiDou records are code-only), and PEA does not
 * drop a signal whose bias is missing: it logs a warning and uses the default bias
 * (src/cpp/pea/ppp_obs.cpp, "Satellite phase bias not found"). So a signal counts as
 * covered when the table has either entry for it, which is also how GinanUI read the
 * table (`parse_bia_code_priorities` maps C codes to their L counterparts).
 */
export function biaPhaseSignals(codes: readonly string[] | null | undefined): Set<string> {
  const signals = new Set<string>();
  for (const code of codes ?? []) {
    if (typeof code === "string" && BIAS_CODE.test(code)) signals.add(`L${code.slice(1)}`);
  }
  return signals;
}

/**
 * A stored or imported selection reduced to its phase codes, keeping each
 * code's enabled/valid state and the user's order.
 *
 * Earlier versions of the app listed every observable in the RINEX header, so a
 * selection persisted by one holds C/D/S entries; those are dropped on the way in rather
 * than rejected. Tolerant of a missing constellation or a malformed entry, since
 * the input comes from disk.
 */
export function phaseConstellationCodes(
  config: Partial<ConstellationCodeConfig> | null | undefined,
): ConstellationCodeConfig {
  const keep = (items: unknown): ObservationCodeItem[] =>
    Array.isArray(items)
      ? items.filter((item): item is ObservationCodeItem => isPhaseCode(item?.code))
      : [];
  return {
    gps: keep(config?.gps),
    gal: keep(config?.gal),
    glo: keep(config?.glo),
    bds: keep(config?.bds),
    qzs: keep(config?.qzs),
  };
}
