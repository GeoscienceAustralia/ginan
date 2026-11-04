import type { SbasMode } from "../types";
import type { StreamBodyProps } from "./streamBody";

/** SouthPAN's SISNet host; the backend fixes the rest (config/realtime.rs). */
export const SISNET_HOST = "sisnet.data.gnss.ga.gov.au";
// PVS is the only mode verified on the Awarua L5 feed: DFMC and L1 got no
// corrections from it in a live test.
export const DEFAULT_SBAS_MODE: SbasMode = "PVS";

/** The mode, in the row: the service is always SouthPAN; the title says what each mode is. */
const MODES: { value: SbasMode; label: string; title: string }[] = [
  { value: "PVS", label: "SouthPAN · PVS", title: "PPP with SouthPAN corrections" },
  { value: "DFMC", label: "SouthPAN · DFMC", title: "Dual-frequency SBAS, a single-point (.SPP) solution" },
  { value: "L1", label: "SouthPAN · L1", title: "Single-frequency SBAS, a single-point (.SPP) solution" },
];

/**
 * An SBAS source's one field: the mode, on the source's row. The service is
 * fixed to SouthPAN (Awarua, L5, PRN 122); its SISNet login is a credential,
 * set in Settings › Credentials (`SisnetLoginSettings`).
 */
export function SbasModeField({ product, onChange }: StreamBodyProps) {
  const mode = (product.value || DEFAULT_SBAS_MODE) as SbasMode;
  return (
    <select
      className="rt-input"
      aria-label="Mode"
      value={mode}
      title={MODES.find((m) => m.value === mode)?.title}
      onChange={(e) => onChange({ value: e.target.value })}
    >
      {MODES.map((m) => (
        <option key={m.value} value={m.value} title={m.title}>{m.label}</option>
      ))}
    </select>
  );
}
