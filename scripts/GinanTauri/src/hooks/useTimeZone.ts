import { useAppStore } from "../store/appStore";

/**
 * The IANA zone the user picked in Settings → Appearance, for passing to the
 * formatters in `utils/time`. Components re-render when it changes, so put it
 * in the dependency list of anything that formats times inside a memo/effect.
 */
export function useTimeZone(): string {
  return useAppStore((s) => s.timeZone);
}
