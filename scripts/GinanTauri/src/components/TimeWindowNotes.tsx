/**
 * The GPS-time helpers behind the General tab's Time Window, split out of
 * ConfigPanelTabs to keep each file under 500 lines.
 */

import { useMemo } from "react";
import { useShallow } from "zustand/react/shallow";
import { useAppStore } from "../store/appStore";
import { selectRinexEnd, selectRinexStart } from "../store/rinexFiles";
import type { EpochWindow } from "./EpochPicker";
import { formatEpoch, gpsMinusUtc, parseEpoch, toStoreEpoch } from "./gpsTime";
import { ResetIcon } from "./icons";
import InfoTip from "./InfoTip";

/**
 * The loaded RINEX files' observation window, already in GPS time: for several
 * files, the earliest start to the latest end.
 */
export function useFileWindow(): EpochWindow | null {
  const fileStart = useAppStore(selectRinexStart);
  const fileEnd = useAppStore(selectRinexEnd);
  return useMemo(() => {
    const start = parseEpoch(fileStart);
    const end = parseEpoch(fileEnd);
    return start != null && end != null ? { start, end } : null;
  }, [fileStart, fileEnd]);
}

const TIME_SYSTEM_NAMES: Record<string, string> = {
  GLO: "GLONASS time",
  BDT: "BeiDou time",
  UTC: "UTC",
};

/**
 * What the user should know about the window as set: it is backwards, it
 * strays outside the loaded file, or the file was not in GPS time to begin
 * with. Loading a file reseeds both epochs from it, so these only appear
 * once someone has edited them, and "Use file window" puts them back.
 */
export function TimeWindowNotes({ fileWindow }: { fileWindow: EpochWindow | null }) {
  const { startEpoch, endEpoch, setStartEpoch, setEndEpoch } = useAppStore(
    useShallow((s) => ({
      startEpoch: s.startEpoch,
      endEpoch: s.endEpoch,
      setStartEpoch: s.setStartEpoch,
      setEndEpoch: s.setEndEpoch,
    })),
  );
  const start = parseEpoch(startEpoch);
  const end = parseEpoch(endEpoch);

  const backwards = start != null && end != null && end < start;
  const outside =
    fileWindow != null &&
    [start, end].some((t) => t != null && (t < fileWindow.start || t > fileWindow.end));
  const differs = fileWindow != null && (start !== fileWindow.start || end !== fileWindow.end);

  return (
    <>
      {backwards && (
        <p className="epoch-note is-error" role="alert">
          End is before start, so PEA would process nothing.
        </p>
      )}
      {!backwards && outside && fileWindow && (
        <p className="epoch-note is-warning">
          Reaches outside the loaded file, which covers {formatEpoch(fileWindow.start)} to{" "}
          {formatEpoch(fileWindow.end)}. Only epochs inside both are processed.
        </p>
      )}
      {differs && fileWindow && (
        <button
          type="button"
          className="btn-ghost epoch-reset"
          onClick={() => {
            setStartEpoch(toStoreEpoch(fileWindow.start));
            setEndEpoch(toStoreEpoch(fileWindow.end));
          }}
        >
          <ResetIcon size={13} />
          Use file window
        </button>
      )}
    </>
  );
}

/** Static guidance for the Time Window heading, with the live leap-second value. */
export function TimeWindowTip() {
  const startEpoch = useAppStore((s) => s.startEpoch);
  const timeSystem = useAppStore((s) => s.rinexData?.time_system ?? null);
  const converted = timeSystem != null ? TIME_SYSTEM_NAMES[timeSystem] : undefined;
  const leap = gpsMinusUtc(parseEpoch(startEpoch) ?? Date.now());
  return (
    <InfoTip
      text={`Times are GPS time, ${leap} s ahead of UTC.${
        converted ? ` The file records ${converted}; its epochs were converted to match.` : ""
      }`}
    />
  );
}
