import type { CorrectionRole, InputProduct } from "../types";

/**
 * What a Corrections stream carries. A live run needs broadcast ephemeris
 * (BCE: the orbits and clocks every correction is applied to) and either SSR
 * corrections or SBAS. SBAS replaces SSR, so while it is present SSR-only
 * streams are paused: kept in the sidebar, left out of the config.
 *
 * A stream's role is worked out, in order, from what the last stream test saw
 * on it (`detected`), its mountpoint name, and last the group it was added
 * under (`role`), for a stream nothing else identifies.
 */

/** The live product types; file and sbas cards are not streams. */
export const isStreamProduct = (p: InputProduct) =>
  p.type === "ntrip" || p.type === "http" || p.type === "serial";

/**
 * The name a stream's records carry as `Mountpoint` in decoded RTCM, and the
 * name its role is guessed from: the last path segment of an NTRIP or HTTP
 * value, or a serial device's file name.
 */
export function streamMount(p: Pick<InputProduct, "type" | "value">): string {
  const v = p.value.replace(/^[a-z]+:\/\/[^/]*/i, "");
  return v.split(/[\\/]/).filter(Boolean).pop() ?? "";
}

/**
 * The role a mountpoint's name implies under the IGS / GA naming: `BCEP…` and
 * `BRDC…` are broadcast ephemeris, `SSRA…` / `SSRC…` are SSR corrections.
 * Anything else is unknown, not guessed.
 */
export function roleFromName(mount: string): CorrectionRole | null {
  const m = mount.toUpperCase();
  if (m.startsWith("BCE") || m.startsWith("BRDC")) return "bce";
  if (m.startsWith("SSR")) return "ssr";
  return null;
}

/**
 * Roles shown by a stream test's record families for one mount (`rtcm_monitor`
 * `streams[m].families`): `brdcEph` is BCE, any `ssr*` family is SSR.
 */
export function rolesFromFamilies(families: Record<string, number>): CorrectionRole[] {
  const roles: CorrectionRole[] = [];
  if ((families.brdcEph ?? 0) > 0) roles.push("bce");
  if (Object.entries(families).some(([f, n]) => f.startsWith("ssr") && n > 0)) roles.push("ssr");
  return roles;
}

/** The roles a stream counts for; empty when nothing says. */
export function correctionRoles(p: InputProduct): CorrectionRole[] {
  if (p.detected && p.detected.length > 0) return p.detected;
  const named = roleFromName(streamMount(p));
  if (named) return [named];
  return p.role ? [p.role] : [];
}

/** SSR only: what SBAS replaces. A stream that also carries BCE is still needed. */
export function isPausedBySbas(p: InputProduct, sbas: boolean): boolean {
  if (!sbas || !isStreamProduct(p)) return false;
  const roles = correctionRoles(p);
  return roles.length === 1 && roles[0] === "ssr";
}

export interface CorrectionCoverage {
  bce: boolean;
  /** SSR from a stream, or SBAS. */
  corrections: boolean;
  sbas: boolean;
  /** Streams whose role nothing tells. */
  unknown: InputProduct[];
}

/** Whether a set of Corrections products is enough to run. */
export function correctionCoverage(corr: InputProduct[]): CorrectionCoverage {
  const sbas = corr.some((p) => p.type === "sbas");
  const streams = corr.filter(isStreamProduct);
  const roles = streams.map(correctionRoles);
  return {
    bce: roles.some((r) => r.includes("bce")),
    corrections: sbas || roles.some((r) => r.includes("ssr")),
    sbas,
    unknown: streams.filter((_, i) => roles[i].length === 0),
  };
}
