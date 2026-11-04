/**
 * Pure helpers for a multi-file RINEX selection.
 *
 * The store keeps the first file as the primary (`rinexPath` / `rinexData`) and
 * any further files in `extraRinex`. Everything that has to reason about the
 * set as a whole lives here, free of the store, so it can be tested without it.
 */

import type { RinexData } from "../types";

/** One observation file that has been prepared and had its header read. */
export interface RinexFile {
  /** The path PEA will read — after any gunzip / Hatanaka conversion. */
  path: string;
  data: RinexData;
}

/** Just the store fields the selectors below read, so they need not import it. */
interface SelectionState {
  rinexPath: string | null;
  rinexData: RinexData | null;
  extraRinex: RinexFile[];
}

/**
 * Earliest start and latest end across `files`. Epochs are "YYYY-MM-DD
 * HH:MM:SS" in GPS time, which sorts correctly as plain text. A file with no
 * epoch of its own is skipped rather than collapsing the whole span to null.
 */
export function rinexSpan(files: RinexData[]): { start: string | null; end: string | null } {
  let start: string | null = null;
  let end: string | null = null;
  for (const f of files) {
    if (f.start_epoch && (start === null || f.start_epoch < start)) start = f.start_epoch;
    if (f.end_epoch && (end === null || f.end_epoch > end)) end = f.end_epoch;
  }
  return { start, end };
}

/** `primary`, with its observation window widened to cover every extra file. */
export function withRinexSpan(primary: RinexData, extras: RinexFile[]): RinexData {
  if (extras.length === 0) return primary;
  const span = rinexSpan([primary, ...extras.map((f) => f.data)]);
  return {
    ...primary,
    start_epoch: span.start ?? primary.start_epoch,
    end_epoch: span.end ?? primary.end_epoch,
  };
}

/** Every selected file path, primary first. Empty when nothing is selected. */
export function allRinexPaths(s: Pick<SelectionState, "rinexPath" | "extraRinex">): string[] {
  return s.rinexPath ? [s.rinexPath, ...s.extraRinex.map((f) => f.path)] : [];
}

/** Start of the selection's observation window, for use as a store selector. */
export const selectRinexStart = (s: SelectionState): string | null =>
  s.rinexData ? withRinexSpan(s.rinexData, s.extraRinex).start_epoch : null;

/** End of the selection's observation window, for use as a store selector. */
export const selectRinexEnd = (s: SelectionState): string | null =>
  s.rinexData ? withRinexSpan(s.rinexData, s.extraRinex).end_epoch : null;

/**
 * The distinct station names across the selection, upper-cased and clipped to
 * the four characters PEA keys a receiver on (it takes them from the file
 * name). More than one means the files are different stations rather than
 * days of one — which PEA accepts, but which the user should know about.
 */
export function distinctMarkers(files: RinexData[]): string[] {
  const seen: string[] = [];
  for (const f of files) {
    const name = (f.marker_name ?? "").trim().slice(0, 4).toUpperCase();
    if (name && !seen.includes(name)) seen.push(name);
  }
  return seen;
}
