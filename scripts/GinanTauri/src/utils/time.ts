/**
 * Date and time display in the user's chosen time zone (Settings → Appearance).
 *
 * Presentation only. Everything the app stores, writes to a PEA config or
 * sends to the backend stays in UTC/GPS time; these helpers only decide how an
 * instant is *shown*. Every user-facing date or time goes through here so a
 * change of zone reaches all of them at once.
 *
 * Epoch milliseconds parsed from PEA output (.pos, .TRACE, decoded RTCM) carry
 * GPS-time digits as though they were UTC — the 18 s
 * leap-second difference is not applied, so choosing UTC shows exactly the
 * digits written in the file.
 */

export const UTC_ZONE = "UTC";

/** The zone the operating system is set to, as an IANA name. */
export function deviceTimeZone(): string {
  try {
    return Intl.DateTimeFormat().resolvedOptions().timeZone || UTC_ZONE;
  } catch {
    return UTC_ZONE;
  }
}

/** True when `tz` is a zone name this runtime can format in. */
export function isValidTimeZone(tz: string): boolean {
  return normalizeTimeZone(tz) !== null;
}

/** The runtime's spelling of a zone name ("utc" → "UTC"), or null if unknown. */
export function normalizeTimeZone(tz: string): string | null {
  if (!tz.trim()) return null;
  try {
    return new Intl.DateTimeFormat("en-US", { timeZone: tz.trim() }).resolvedOptions().timeZone;
  } catch {
    return null;
  }
}

/**
 * Used only where `Intl.supportedValuesOf` is missing (older WebKit). Any
 * valid IANA name can still be typed in; this is just what gets suggested.
 */
const FALLBACK_ZONES = [
  "UTC",
  "Africa/Cairo", "Africa/Johannesburg", "Africa/Lagos", "Africa/Nairobi",
  "America/Anchorage", "America/Argentina/Buenos_Aires", "America/Bogota",
  "America/Chicago", "America/Denver", "America/Halifax", "America/Los_Angeles",
  "America/Mexico_City", "America/New_York", "America/Phoenix", "America/Santiago",
  "America/Sao_Paulo", "America/St_Johns", "America/Toronto", "America/Vancouver",
  "Antarctica/Casey", "Antarctica/Davis", "Antarctica/Mawson",
  "Asia/Bangkok", "Asia/Dhaka", "Asia/Dubai", "Asia/Hong_Kong", "Asia/Jakarta",
  "Asia/Jerusalem", "Asia/Karachi", "Asia/Kathmandu", "Asia/Kolkata", "Asia/Manila",
  "Asia/Seoul", "Asia/Shanghai", "Asia/Singapore", "Asia/Taipei", "Asia/Tehran",
  "Asia/Tokyo",
  "Atlantic/Azores", "Atlantic/Reykjavik",
  "Australia/Adelaide", "Australia/Brisbane", "Australia/Darwin", "Australia/Eucla",
  "Australia/Hobart", "Australia/Lord_Howe", "Australia/Melbourne", "Australia/Perth",
  "Australia/Sydney",
  "Europe/Amsterdam", "Europe/Athens", "Europe/Berlin", "Europe/Istanbul",
  "Europe/Lisbon", "Europe/London", "Europe/Madrid", "Europe/Moscow", "Europe/Paris",
  "Europe/Rome", "Europe/Stockholm", "Europe/Warsaw", "Europe/Zurich",
  "Indian/Christmas", "Indian/Cocos", "Indian/Maldives",
  "Pacific/Auckland", "Pacific/Chatham", "Pacific/Fiji", "Pacific/Guam",
  "Pacific/Honolulu", "Pacific/Norfolk", "Pacific/Port_Moresby", "Pacific/Tongatapu",
];

let zoneList: string[] | null = null;

/** Every zone to suggest in the picker: UTC first, then alphabetical. */
export function listTimeZones(): string[] {
  if (zoneList) return zoneList;
  let zones: string[] = FALLBACK_ZONES;
  const supported = (Intl as { supportedValuesOf?: (key: string) => string[] }).supportedValuesOf;
  if (typeof supported === "function") {
    try {
      zones = supported("timeZone");
    } catch {
      // Keep the fallback list.
    }
  }
  const device = deviceTimeZone();
  const set = new Set(zones.filter((z) => z !== UTC_ZONE));
  if (device !== UTC_ZONE) set.add(device);
  zoneList = [UTC_ZONE, ...Array.from(set).sort()];
  return zoneList;
}

// ── Formatting ───────────────────────────────────────────────────────────────

const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

const partsFormatters = new Map<string, Intl.DateTimeFormat>();
const zoneFormatters = new Map<string, Intl.DateTimeFormat>();

/** Charts format thousands of ticks; building a formatter per call is slow. */
function partsFormatter(tz: string): Intl.DateTimeFormat {
  let f = partsFormatters.get(tz);
  if (!f) {
    try {
      f = new Intl.DateTimeFormat("en-US", {
        timeZone: tz,
        year: "numeric",
        month: "numeric",
        day: "numeric",
        hour: "numeric",
        minute: "numeric",
        second: "numeric",
        hourCycle: "h23",
      });
    } catch {
      // An unknown zone (e.g. a stored name a newer OS dropped) falls back to UTC.
      return partsFormatter(UTC_ZONE);
    }
    partsFormatters.set(tz, f);
  }
  return f;
}

export interface WallClock {
  year: number;
  /** 1–12. */
  month: number;
  day: number;
  hour: number;
  minute: number;
  second: number;
}

/** The calendar date and clock reading of `ms` in zone `tz`. */
export function wallClock(ms: number, tz: string): WallClock {
  const out: WallClock = { year: 0, month: 1, day: 1, hour: 0, minute: 0, second: 0 };
  for (const p of partsFormatter(tz).formatToParts(new Date(ms))) {
    if (p.type === "year") out.year = Number(p.value);
    else if (p.type === "month") out.month = Number(p.value);
    else if (p.type === "day") out.day = Number(p.value);
    else if (p.type === "hour") out.hour = Number(p.value) % 24;
    else if (p.type === "minute") out.minute = Number(p.value);
    else if (p.type === "second") out.second = Number(p.value);
  }
  return out;
}

/** Milliseconds `tz` is ahead of UTC at instant `ms` (negative west of Greenwich). */
export function timeZoneOffsetMs(ms: number, tz: string): number {
  const w = wallClock(ms, tz);
  const asUtc = Date.UTC(w.year, w.month - 1, w.day, w.hour, w.minute, w.second);
  return asUtc - Math.floor(ms / 1000) * 1000;
}

/** "AEST", "UTC", or "GMT+10" where the locale has no short name for the zone. */
export function timeZoneAbbr(ms: number, tz: string): string {
  let f = zoneFormatters.get(tz);
  if (!f) {
    try {
      f = new Intl.DateTimeFormat(undefined, { timeZone: tz, timeZoneName: "short" });
    } catch {
      return UTC_ZONE;
    }
    zoneFormatters.set(tz, f);
  }
  return f.formatToParts(new Date(ms)).find((p) => p.type === "timeZoneName")?.value ?? tz;
}

/** "UTC+10:00", "UTC−03:30", "UTC" — the zone's current offset, for the picker. */
export function timeZoneOffsetLabel(tz: string, ms = Date.now()): string {
  const off = Math.round(timeZoneOffsetMs(ms, tz) / 60_000);
  if (off === 0) return "UTC";
  const abs = Math.abs(off);
  return `UTC${off < 0 ? "−" : "+"}${p2(Math.floor(abs / 60))}:${p2(abs % 60)}`;
}

const p2 = (n: number) => String(n).padStart(2, "0");

/** "08:59" or "08:59:09". */
export function formatTime(ms: number, tz: string, opts: { seconds?: boolean } = {}): string {
  const w = wallClock(ms, tz);
  const hm = `${p2(w.hour)}:${p2(w.minute)}`;
  return opts.seconds === false ? hm : `${hm}:${p2(w.second)}`;
}

/** "22 Sep" */
export function formatDayMonth(ms: number, tz: string): string {
  const w = wallClock(ms, tz);
  return `${w.day} ${MONTHS[w.month - 1]}`;
}

/**
 * "2026-09-22 08:59:09", optionally followed by the zone abbreviation —
 * the same layout PEA uses for epochs, so UTC reads exactly like the file.
 */
export function formatDateTime(
  ms: number,
  tz: string,
  opts: { seconds?: boolean; zone?: boolean } = {},
): string {
  const w = wallClock(ms, tz);
  const date = `${w.year}-${p2(w.month)}-${p2(w.day)}`;
  const text = `${date} ${formatTime(ms, tz, opts)}`;
  return opts.zone ? `${text} ${timeZoneAbbr(ms, tz)}` : text;
}

/** "22 Sep 08:59" — compact, for lists and dropdowns. */
export function formatShortDateTime(ms: number, tz: string): string {
  return `${formatDayMonth(ms, tz)} ${formatTime(ms, tz, { seconds: false })}`;
}

/**
 * PEA/RTCM epoch text ("YYYY-MM-DD HH:MM:SS[.f]", GPS time) to epoch ms, read
 * as UTC digits like the rest of the app. Null when it doesn't parse.
 */
export function parseEpochString(s: string): number | null {
  const m = /^(\d{4})-(\d{2})-(\d{2})[ T](\d{2}):(\d{2}):(\d{2})(\.\d+)?/.exec(s.trim());
  if (!m) return null;
  const ms = m[7] ? Math.round(Number(m[7]) * 1000) : 0;
  return Date.UTC(+m[1], +m[2] - 1, +m[3], +m[4], +m[5], +m[6], ms);
}
