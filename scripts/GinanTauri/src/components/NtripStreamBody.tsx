import { useMemo } from "react";
import { useAppStore } from "../store/appStore";
import { extractHost } from "../config/ntrip";
import Dropdown from "./Dropdown";
import type { StreamBodyProps } from "./streamBody";

/**
 * Body of an NTRIP card: an editable mountpoint field. Opening it fetches the
 * caster's sourcetable and offers the matching streams as suggestions.
 */
export default function NtripStreamBody({ section, product, onChange, onDone }: StreamBodyProps) {
  const casterUrl = useAppStore((s) => s.casterUrl);
  const sourcetable = useAppStore((s) => s.ntripSourcetable);
  const sourcetableStatus = useAppStore((s) => s.ntripSourcetableStatus);
  const fetchSourcetable = useAppStore((s) => s.fetchNtripSourcetable);
  const setSettingsOpen = useAppStore((s) => s.setSettingsOpen);
  const setSettingsPage = useAppStore((s) => s.setSettingsPage);

  const casterHost = extractHost(casterUrl);
  const openCasterSettings = () => {
    setSettingsPage("credentials");
    setSettingsOpen(true);
  };

  const mountOptions = useMemo(
    () =>
      sourcetable
        .filter((s) => (section === "obs" ? s.category !== "ssr" : s.category !== "obs"))
        .map((o) => ({
          value: o.mountpoint,
          location: o.identifier,
          format: o.format,
        })),
    [sourcetable, section],
  );
  // Each open of a mountpoint field is one attempt, so a failed fetch retries
  // the next time the user comes back to it, and a changed caster refetches.
  const loadMounts = () => {
    const s = sourcetableStatus;
    if (s.state === "idle" || s.state === "error" || (s.state === "ok" && s.host !== casterHost)) {
      fetchSourcetable();
    }
  };
  const mountNote =
    sourcetableStatus.state === "loading" ? (
      "Fetching streams..."
    ) : sourcetableStatus.state === "error" ? (
      <>
        {sourcetableStatus.message}{" "}
        <button className="btn-link" onClick={openCasterSettings}>
          Open caster settings
        </button>
      </>
    ) : undefined;

  return (
    <Dropdown
      editable
      ariaLabel="Stream"
      value={product.value}
      placeholder="ALIC00AUS0"
      options={mountOptions}
      onOpen={loadMounts}
      note={mountNote}
      onChange={(v) => onChange({ value: v })}
      onPick={onDone}
      // One line, the height of the NTRIP badge beside it in the row.
      singleLine
    />
  );
}
