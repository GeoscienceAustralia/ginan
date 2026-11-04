/**
 * GPS-time arithmetic for the Time Window pickers.
 *
 * Epochs are handled as milliseconds on a plain, zone-less timeline: the value
 * `Date.UTC(2024, 0, 2)` means "2024-01-02 00:00:00 GPST", never a moment in
 * the user's own zone. Every read and write goes through the `getUTC*` family
 * for that reason; a bare `getHours()` would shift the epoch by the local
 * offset, which is exactly the bug a browser `datetime-local` invites.
 */

const DAY_MS = 86_400_000;
const WEEK_MS = 7 * DAY_MS;
/** Start of GPS time: 1980-01-06 00:00:00, a Sunday. */
export const GPS_EPOCH_MS = Date.UTC(1980, 0, 6);

/**
 * GPS − UTC in seconds, as (first UTC instant it applies, offset). GPS time
 * has no leap seconds, so the gap grows by one at every UTC leap second.
 * Mirrors `GPS_UTC_LEAPS` in src-tauri/src/commands/rinex.rs.
 */
const LEAPS: [number, number][] = [
  [Date.UTC(1981, 6, 1), 1], [Date.UTC(1982, 6, 1), 2], [Date.UTC(1983, 6, 1), 3],
  [Date.UTC(1985, 6, 1), 4], [Date.UTC(1988, 0, 1), 5], [Date.UTC(1990, 0, 1), 6],
  [Date.UTC(1991, 0, 1), 7], [Date.UTC(1992, 6, 1), 8], [Date.UTC(1993, 6, 1), 9],
  [Date.UTC(1994, 6, 1), 10], [Date.UTC(1996, 0, 1), 11], [Date.UTC(1997, 6, 1), 12],
  [Date.UTC(1999, 0, 1), 13], [Date.UTC(2006, 0, 1), 14], [Date.UTC(2009, 0, 1), 15],
  [Date.UTC(2012, 6, 1), 16], [Date.UTC(2015, 6, 1), 17], [Date.UTC(2017, 0, 1), 18],
];

/** GPS − UTC (seconds) at a GPS-time instant. */
export function gpsMinusUtc(gpsMs: number): number {
  for (let i = LEAPS.length - 1; i >= 0; i--) {
    const [fromUtc, n] = LEAPS[i];
    if (gpsMs - n * 1000 >= fromUtc) return n;
  }
  return 0;
}

const EPOCH_RE =
  /^\s*(\d{4})[-/](\d{1,2})[-/](\d{1,2})(?:[ T_]+(\d{1,2}):(\d{1,2})(?::(\d{1,2})(?:\.\d+)?)?)?\s*$/;

/**
 * Read an epoch as typed or stored. Takes PEA's "YYYY-MM-DD HH:MM:SS", the
 * store's "YYYY-MM-DDTHH:MM[:SS]", GinanUI's underscore form, and a bare date
 * (midnight). Null for anything else, including out-of-range fields such as
 * a 31 February, which `Date.UTC` would otherwise roll into March.
 */
export function parseEpoch(text: string | null | undefined): number | null {
  if (!text) return null;
  const m = EPOCH_RE.exec(text);
  if (!m) return null;
  const [y, mo, d, h = 0, mi = 0, s = 0] = m.slice(1).map((v) => (v == null ? 0 : Number(v)));
  if (mo < 1 || mo > 12 || h > 23 || mi > 59 || s > 59) return null;
  const ms = Date.UTC(y, mo - 1, d, h, mi, s);
  if (new Date(ms).getUTCDate() !== d) return null;
  return ms;
}

const p2 = (n: number) => String(n).padStart(2, "0");

/** "YYYY-MM-DD HH:MM:SS", the form PEA's config takes and the picker shows. */
export function formatEpoch(ms: number): string {
  const d = new Date(ms);
  return (
    `${d.getUTCFullYear()}-${p2(d.getUTCMonth() + 1)}-${p2(d.getUTCDate())} ` +
    `${p2(d.getUTCHours())}:${p2(d.getUTCMinutes())}:${p2(d.getUTCSeconds())}`
  );
}

/** The store keeps the old `datetime-local` shape, "YYYY-MM-DDTHH:MM:SS". */
export function toStoreEpoch(ms: number): string {
  return formatEpoch(ms).replace(" ", "T");
}

/** GPS week, day of week (0 = Sunday) and seconds of week. */
export function gpsWeek(ms: number): { week: number; day: number; sow: number } {
  const since = ms - GPS_EPOCH_MS;
  const week = Math.floor(since / WEEK_MS);
  const sow = Math.floor((since - week * WEEK_MS) / 1000);
  return { week, day: Math.floor(sow / 86_400), sow };
}

/** Day of year, 1-based. */
export function dayOfYear(ms: number): number {
  const d = new Date(ms);
  return Math.floor((ms - Date.UTC(d.getUTCFullYear(), 0, 1)) / DAY_MS) + 1;
}

/** Character ranges of each field in "YYYY-MM-DD HH:MM:SS". */
export const EPOCH_FIELDS = [
  { unit: "year", from: 0, to: 4 },
  { unit: "month", from: 5, to: 7 },
  { unit: "day", from: 8, to: 10 },
  { unit: "hour", from: 11, to: 13 },
  { unit: "minute", from: 14, to: 16 },
  { unit: "second", from: 17, to: 19 },
] as const;

export type EpochUnit = (typeof EPOCH_FIELDS)[number]["unit"];

/**
 * Step one field of an epoch. Calendar fields carry the way a calendar does
 * (31 Jan + 1 month clamps to 29 Feb rather than spilling into March); the
 * clock fields are plain arithmetic, so 23:59:59 + 1 s is the next day.
 */
export function stepEpoch(ms: number, unit: EpochUnit, by: number): number {
  const d = new Date(ms);
  if (unit === "year" || unit === "month") {
    const months = d.getUTCFullYear() * 12 + d.getUTCMonth() + (unit === "year" ? by * 12 : by);
    const y = Math.floor(months / 12);
    const mo = months - y * 12;
    const lastDay = new Date(Date.UTC(y, mo + 1, 0)).getUTCDate();
    return Date.UTC(
      y, mo, Math.min(d.getUTCDate(), lastDay),
      d.getUTCHours(), d.getUTCMinutes(), d.getUTCSeconds(),
    );
  }
  const unitMs = { day: DAY_MS, hour: 3_600_000, minute: 60_000, second: 1000 }[unit];
  return ms + by * unitMs;
}
