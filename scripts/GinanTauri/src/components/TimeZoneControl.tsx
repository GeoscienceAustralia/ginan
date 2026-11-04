import { useEffect, useMemo, useState } from "react";
import { useAppStore } from "../store/appStore";
import {
  UTC_ZONE,
  deviceTimeZone,
  listTimeZones,
  normalizeTimeZone,
  timeZoneOffsetLabel,
} from "../utils/time";
import { LocateIcon } from "./icons";

/**
 * Time zone picker (Settings → Appearance): a type-to-search field over every
 * IANA zone, a one-click UTC, and a locate button that takes the zone the
 * device is set to. "Device location" here means the operating system's zone
 * setting — no geolocation is requested.
 */
export default function TimeZoneControl() {
  const timeZone = useAppStore((s) => s.timeZone);
  const setTimeZone = useAppStore((s) => s.setTimeZone);

  // The field holds a draft while the user types; it only reaches the store
  // once it names a real zone, and snaps back to the stored zone otherwise.
  const [draft, setDraft] = useState(timeZone);
  useEffect(() => setDraft(timeZone), [timeZone]);

  // Offsets are computed once per open: DST can move them, but not mid-visit.
  const options = useMemo(
    () => listTimeZones().map((z) => ({ zone: z, offset: timeZoneOffsetLabel(z) })),
    [],
  );
  const known = useMemo(() => new Set(options.map((o) => o.zone)), [options]);

  const commit = (value: string) => {
    const tz = normalizeTimeZone(value);
    if (tz) setTimeZone(tz);
    setDraft(tz ?? timeZone);
  };

  const device = deviceTimeZone();

  return (
    <div className="tz-control">
      <input
        type="text"
        className="tz-input"
        list="tz-options"
        value={draft}
        spellCheck={false}
        aria-label="Time zone"
        placeholder="Search time zones"
        onFocus={(e) => e.target.select()}
        onChange={(e) => {
          setDraft(e.target.value);
          // Picking a suggestion fires a change with the full name — take it
          // straight away rather than waiting for blur.
          if (known.has(e.target.value)) setTimeZone(e.target.value);
        }}
        onBlur={(e) => commit(e.target.value)}
        onKeyDown={(e) => {
          if (e.key === "Enter") commit(e.currentTarget.value);
        }}
      />
      <datalist id="tz-options">
        {options.map((o) => (
          <option key={o.zone} value={o.zone}>
            {o.offset}
          </option>
        ))}
      </datalist>
      <button
        className="btn-secondary"
        onClick={() => setTimeZone(UTC_ZONE)}
        title="Show every time in UTC"
      >
        UTC
      </button>
      <button
        className="btn-icon"
        onClick={() => setTimeZone(device)}
        aria-label="Use device timezone"
        title={`Use device timezone (${device})`}
      >
        <LocateIcon size={14} />
      </button>
    </div>
  );
}
