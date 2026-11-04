import { useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { SERIAL_PREFIX } from "../store/appStore";
import { DEFAULT_SERIAL_SETTINGS, type SerialSettings } from "../types";
import Dropdown from "./Dropdown";
import InfoTip from "./InfoTip";
import type { StreamBodyProps } from "./streamBody";

/**
 * A serial source's two parts: the device field, on the source's row (the
 * `serial://` scheme is fixed text in front of it), and the line settings
 * under it in the same box.
 */
export function SerialDeviceField({ product, onChange }: StreamBodyProps) {
  const [ports, setPorts] = useState<string[]>([]);
  const refreshPorts = () => {
    invoke<string[]>("list_serial_ports").then(setPorts).catch(() => setPorts([]));
  };

  return (
      /* The scheme is fixed text; only the device path after it is stored. */
      <div className="rt-uri">
        <span className="rt-uri-prefix">{SERIAL_PREFIX}</span>
        <Dropdown
          editable
          ariaLabel="Serial device"
          value={product.value}
          placeholder={navigator.userAgent.includes("Windows") ? "COM3" : "/dev/ttyAMA0"}
          options={ports.map((p) => ({ value: p }))}
          onOpen={refreshPorts}
          note={ports.length === 0 ? "No ports found. Type the device path." : undefined}
          onChange={(v) => onChange({ value: v.replace(/^[a-z]+:\/\//i, "") })}
        />
      </div>
  );
}

export function SerialSettingsFields({ product, onChange }: StreamBodyProps) {
  const settings = product.serial ?? DEFAULT_SERIAL_SETTINGS;
  return <SerialFields value={settings} onChange={(patch) => onChange({ serial: { ...settings, ...patch } })} />;
}

/**
 * Line settings for serial streams. These only reach the serial setup script
 * (`scripts/serial/start_gnss_stream.sh`), which configures the port; the PEA
 * config is not changed. One set per stream.
 */
function SerialFields({
  value: serial,
  onChange,
}: {
  value: SerialSettings;
  onChange: (patch: Partial<SerialSettings>) => void;
}) {
  const set = <K extends keyof SerialSettings>(key: K, value: SerialSettings[K]) =>
    onChange({ [key]: value } as Partial<SerialSettings>);

  return (
    <div className="rt-grid-2">
      <div>
        <label className="rt-label">Baud</label>
        <input
          className="rt-input"
          type="number"
          min={1}
          value={serial.baud}
          onChange={(e) => set("baud", Number(e.target.value))}
        />
      </div>
      <div>
        <label className="rt-label">Data bits</label>
        <select
          className="rt-input"
          value={serial.data}
          onChange={(e) => set("data", Number(e.target.value) as SerialSettings["data"])}
        >
          {[5, 6, 7, 8].map((n) => (
            <option key={n} value={n}>{n}</option>
          ))}
        </select>
      </div>
      <div>
        <label className="rt-label">Parity</label>
        <select
          className="rt-input"
          value={serial.parity}
          onChange={(e) => set("parity", e.target.value as SerialSettings["parity"])}
        >
          <option value="none">None</option>
          <option value="even">Even</option>
          <option value="odd">Odd</option>
        </select>
      </div>
      <div>
        <label className="rt-label">Stop bits</label>
        <select
          className="rt-input"
          value={serial.stop}
          onChange={(e) => set("stop", Number(e.target.value) as SerialSettings["stop"])}
        >
          <option value={1}>1</option>
          <option value={2}>2</option>
        </select>
      </div>
      <div>
        <label className="rt-label">Flow control</label>
        <select
          className="rt-input"
          value={serial.flow}
          onChange={(e) => set("flow", e.target.value as SerialSettings["flow"])}
        >
          <option value="none">None</option>
          <option value="rtscts">RTS/CTS</option>
          <option value="xonxoff">XON/XOFF</option>
        </select>
      </div>
      <div>
        <label className="rt-label">
          Flush duration (ms)
          <InfoTip text="After clearing the port's buffer, keep discarding incoming bytes for this long" />
        </label>
        <input
          className="rt-input"
          type="number"
          min={0}
          step={100}
          value={serial.flush_duration_ms}
          disabled={!serial.flush}
          onChange={(e) => set("flush_duration_ms", Math.max(0, Math.floor(Number(e.target.value))))}
        />
      </div>
      <label className="rt-checkbox">
        <input
          type="checkbox"
          checked={serial.flush}
          onChange={(e) => set("flush", e.target.checked)}
        />
        Flush the port before reading
      </label>
    </div>
  );
}
