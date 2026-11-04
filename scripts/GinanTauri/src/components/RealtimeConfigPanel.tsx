import { useState, type ComponentType, type ReactNode } from "react";
import { useAppStore } from "../store/appStore";
import type { CorrectionRole, InputProduct, InputSection, ProductType } from "../types";
import { correctionRoles, isPausedBySbas, isStreamProduct, streamMount } from "../config/correctionRoles";
import { TrashIcon } from "./icons";
import InfoTip from "./InfoTip";
import InputSectionHeader from "./InputSectionHeader";
import NtripStreamBody from "./NtripStreamBody";
import ProductContextMenu from "./ProductContextMenu";
import { SerialDeviceField, SerialSettingsFields } from "./SerialStreamBody";
import HttpStreamBody from "./HttpStreamBody";
import { SbasModeField } from "./SbasStreamBody";
import { httpMount, maskLogin } from "./httpUrl";
import { useProductAdd } from "../hooks/useProductAdd";
import type { StreamBodyProps } from "./streamBody";

/**
 * Realtime input configuration surface.
 *
 * Shown while the Observation products are live streams (a realtime run). The cards are
 * drawn from the store's `inputProducts`; edits go through its product actions,
 * and `realtime` / `streamSerial` are derived from them. Process reads that
 * derived object and sends it as `ConfigInputs.realtime`, which swaps the whole
 * `inputs` block on the backend.
 *
 * What the user actually has to decide, top to bottom:
 *
 *   1. the observation streams, each either a mountpoint on the caster or a
 *      local serial device;
 *   2. the correction streams, same choice, made per stream;
 *   3. the streaming knobs (SSR offset, epoch pacing) and what gets recorded.
 *
 * The caster URL and the caster login are *not* here — they are machine setup,
 * so they live on Settings › Credentials (`NtripCasterSettings`) alongside the
 * other credentials. The NTRIP card body reads the fetched sourcetable back out
 * of the store to suggest mountpoints.
 *
 * The run itself is started by `start_pea_realtime` (src-tauri/src/commands/realtime.rs).
 */
export default function RealtimeConfigPanel() {
  const realtime = useAppStore((s) => s.realtime);
  const updateRealtime = useAppStore((s) => s.updateRealtime);
  const realtimeStorageCapMb = useAppStore((s) => s.realtimeStorageCapMb);
  const setRealtimeStorageCapMb = useAppStore((s) => s.setRealtimeStorageCapMb);

  return (
    <div className="rt-panel">
      <StreamSection
        label="Observation"
        help="The reference stations you are positioning against, as raw RTCM. At least one is required - Process will not start without one."
        section="obs"
        tourId="rt-obs"
        sourceTourId="rt-caster"
      />

      <StreamSection
        label="Corrections"
        help="Where orbits, clocks and biases come from. A file run downloads SP3 and CLK products for the observation dates; a live run has no dates to download for, so it takes broadcast ephemeris and corrects it continuously with SSR, or with SBAS in place of SSR. It needs both."
        section="corr"
        tourId="rt-nav"
      />

      <div className="rt-section rt-grid-2" data-tour="rt-settings">
        {/* No SSR antenna offset control: the SSR stream's name already says
            whether it is APC (SSRA...) or COM (SSRC...), and the backend writes
            that (config/realtime.rs, ssr_antenna_offset). */}
        <div>
          <label className="rt-label">
            Storage cap (MB)
            <InfoTip text="Rotated recordings past this total are pruned automatically. 0 disables the sweep." />
          </label>
          <input
            className="rt-input"
            type="number"
            min={0}
            step={50}
            value={realtimeStorageCapMb}
            onChange={(e) => setRealtimeStorageCapMb(Number(e.target.value))}
          />
        </div>
      </div>

      <div className="rt-section rt-grid-2">
        <div>
          <label className="rt-label">Wait next epoch (s)</label>
          <input
            className="rt-input"
            type="number"
            min={0}
            step={0.5}
            value={realtime.wait_next_epoch}
            onChange={(e) =>
              updateRealtime({ wait_next_epoch: Number(e.target.value) })
            }
          />
        </div>
        <div>
          <label className="rt-label">Max receiver latency (s)</label>
          <input
            className="rt-input"
            type="number"
            min={0}
            step={0.1}
            value={realtime.max_rec_latency}
            onChange={(e) =>
              updateRealtime({ max_rec_latency: Number(e.target.value) })
            }
          />
        </div>
      </div>
    </div>
  );
}

/**
 * Product types the plus menu enables in both sections.
 */
const ENABLED_TYPES: ProductType[] = ["ntrip", "serial", "http"];

const TYPE_LABEL: Record<ProductType, string> = {
  file: "File",
  http: "HTTP",
  ntrip: "NTRIP",
  serial: "Serial",
  sbas: "SBAS",
};

/**
 * Per type: the field on a source's row (none shows its name) and, for the
 * types with more to set, the settings under the row in the same box.
 */
const PARTS: Record<ProductType, { Field?: ComponentType<StreamBodyProps>; Settings?: ComponentType<StreamBodyProps> }> = {
  ntrip: { Field: NtripStreamBody },
  http: { Field: HttpStreamBody },
  serial: { Field: SerialDeviceField, Settings: SerialSettingsFields },
  // Its SISNet login is a credential, on Settings › Credentials.
  sbas: { Field: SbasModeField },
  file: {},
};

/** The row's text for a type with no field: what the source is. */
function productTitle(p: InputProduct): string {
  return p.type === "serial"
    ? (p.value.split(/[\\/]/).pop() ?? "")
    : p.type === "http"
      ? httpMount(p.value)
      : p.value;
}

/** The `data-type` colour token. Only http uses the base (http) tokens. */
function dataType(type: ProductType): string {
  return type === "serial" || type === "file" || type === "sbas" || type === "ntrip" ? type : "http";
}

interface StreamSectionProps {
  label: string;
  help: string;
  section: InputSection;
  /** `data-tour` for the stream list itself. */
  tourId?: string;
  /** `data-tour` for the stream cards — only the observation section sets it. */
  sourceTourId?: string;
}

/**
 * One input list, one box per product. The boxes come from
 * `inputProducts[section]`, keyed by product id, so a box keeps its own DOM
 * node when another is removed. A product's `type` is fixed once added, and
 * picks its type label and fields (see `PARTS`).
 *
 * Corrections are drawn in two groups, because a live run needs both:
 * broadcast ephemeris, and SSR or SBAS on top. Each group has its own plus
 * (SBAS only on the SSR one), and an empty group shows a box to add the
 * missing stream. Which group a stream sits in is worked out, not chosen
 * (see `correctionRoles`); one nothing identifies sits in a third group.
 */
function StreamSection({ label, help, section, tourId, sourceTourId }: StreamSectionProps) {
  const products = useAppStore((s) => s.inputProducts[section]);
  // SBAS is a correction source only, and one at a time: it replaces SSR.
  const hasSbas = products.some((p) => p.type === "sbas");
  const { onAdd, enabledTypes, disabledReason, dialog } = useProductAdd(
    section,
    section === "corr" && !hasSbas ? [...ENABLED_TYPES, "sbas"] : ENABLED_TYPES,
  );
  const addProduct = useAppStore((s) => s.addProduct);
  const removeProduct = useAppStore((s) => s.removeProduct);

  // Right-click menu on a product bar: where it opened, and which product.
  const [ctx, setCtx] = useState<{ x: number; y: number; id: string; bar: HTMLElement } | null>(null);

  const card = (p: InputProduct) => (
    <StreamCard
      key={p.id}
      section={section}
      product={p}
      paused={section === "corr" && isPausedBySbas(p, hasSbas)}
      onContextMenu={(x, y, bar) => setCtx({ x, y, id: p.id, bar })}
    />
  );
  const canAddNtrip = enabledTypes.includes("ntrip");
  const corr = section === "corr";

  return (
    <div className="rt-section rt-stream-section" data-tour={tourId}>
      <InputSectionHeader
        title={label}
        info={<InfoTip text={help} />}
        // Corrections adds from each group's own plus instead.
        onAdd={corr ? undefined : onAdd}
        enabledTypes={enabledTypes}
        disabledReason={disabledReason}
        section={section}
      />
      {dialog}

      {corr ? (
        <CorrectionGroups
          products={products}
          card={card}
          enabledTypes={enabledTypes}
          disabledReason={disabledReason}
          // A stream remembers the group it was added under, for when
          // neither Monitor stream nor its name says what it carries.
          onAdd={(type, role) => addProduct("corr", type, "", type === "sbas" ? undefined : role)}
        />
      ) : (
        <>
          {/* Empty section: the same dashed box as a file run's "browse for a
              RINEX file", adding the usual first source. The plus menu has the rest. */}
          {products.length === 0 && enabledTypes.length > 0 && (
            <button
              type="button"
              className="file-drop-zone"
              data-tour={sourceTourId}
              onClick={() => onAdd(canAddNtrip ? "ntrip" : enabledTypes[0])}
            >
              <span className="file-drop-zone-title">
                Click to add {canAddNtrip ? "an NTRIP stream" : "a source"}
              </span>
              <span className="file-drop-zone-hint">Or use + for HTTP, serial or a file</span>
            </button>
          )}
          <div className="rt-stream-list" data-tour={products.length > 0 ? sourceTourId : undefined}>
            {products.map(card)}
          </div>
        </>
      )}
      {ctx && (
        <ProductContextMenu
          x={ctx.x}
          y={ctx.y}
          returnFocusTo={ctx.bar}
          onRemove={() => removeProduct(section, ctx.id)}
          onClose={() => setCtx(null)}
        />
      )}
    </div>
  );
}

/** What each group's plus menu lists: SBAS replaces SSR, so only SSR offers it. */
const BCE_TYPES: ProductType[] = ["ntrip", "http", "serial"];
const SSR_TYPES: ProductType[] = ["ntrip", "http", "serial", "sbas"];

function CorrectionGroups({
  products,
  card,
  enabledTypes,
  disabledReason,
  onAdd,
}: {
  products: InputProduct[];
  card: (p: InputProduct) => ReactNode;
  /** The section's addable types; empty while adding is not allowed. */
  enabledTypes: ProductType[];
  disabledReason?: string;
  onAdd: (type: ProductType, role: CorrectionRole) => void;
}) {
  const streams = products.filter(isStreamProduct);
  const roles = new Map(streams.map((p) => [p.id, correctionRoles(p)]));
  const bce = streams.filter((p) => roles.get(p.id)!.includes("bce"));
  // A stream carrying both sits with ephemeris and also covers SSR.
  const ssrOnly = streams.filter((p) => roles.get(p.id)!.join() === "ssr");
  const unknown = streams.filter((p) => roles.get(p.id)!.length === 0);
  const sbas = products.filter((p) => p.type === "sbas");
  const coveredBy = bce.find((p) => roles.get(p.id)!.includes("ssr"));
  const canAddStream = enabledTypes.includes("ntrip");
  const canAddSbas = enabledTypes.includes("sbas");

  return (
    <>
      <div className="rt-corr-group">
        <InputSectionHeader
          group
          title="Broadcast ephemeris"
          info={<span className="rt-corr-need">required</span>}
          section="corr"
          types={BCE_TYPES}
          enabledTypes={enabledTypes}
          disabledReason={disabledReason}
          onAdd={(type) => onAdd(type, "bce")}
        />
        <div className="rt-stream-list">{bce.map(card)}</div>
        {bce.length === 0 && canAddStream && (
          <button type="button" className="file-drop-zone" onClick={() => onAdd("ntrip", "bce")}>
            <span className="file-drop-zone-title">Add a broadcast ephemeris stream</span>
            <span className="file-drop-zone-hint">Orbits and clocks to correct, e.g. BCEP00BKG0</span>
          </button>
        )}
      </div>

      <div className="rt-corr-group">
        <InputSectionHeader
          group
          title="SSR corrections"
          info={<span className="rt-corr-need">or SBAS</span>}
          section="corr"
          types={SSR_TYPES}
          enabledTypes={enabledTypes}
          disabledReason={disabledReason}
          onAdd={(type) => onAdd(type, "ssr")}
        />
        <div className="rt-stream-list">
          {sbas.map(card)}
          {ssrOnly.map(card)}
        </div>
        {sbas.length === 0 && ssrOnly.length === 0 && coveredBy && (
          <div className="rt-hint">SSR comes from {streamMount(coveredBy)}, which carries both.</div>
        )}
        {sbas.length === 0 && ssrOnly.length === 0 && !coveredBy && (canAddStream || canAddSbas) && (
          <div className="rt-corr-empty">
            {canAddStream && (
              <button type="button" className="file-drop-zone" onClick={() => onAdd("ntrip", "ssr")}>
                <span className="file-drop-zone-title">Add an SSR stream</span>
                <span className="file-drop-zone-hint">e.g. SSRA00CNE0</span>
              </button>
            )}
            {canAddSbas && (
              <button type="button" className="file-drop-zone" onClick={() => onAdd("sbas", "ssr")}>
                <span className="file-drop-zone-title">Use SBAS</span>
                <span className="file-drop-zone-hint">SouthPAN, in place of SSR</span>
              </button>
            )}
          </div>
        )}
      </div>

      {unknown.length > 0 && (
        <div className="rt-corr-group">
          <InputSectionHeader
            group
            title="Not identified yet"
            info={<span className="rt-corr-need is-warn">run Monitor stream, or remove it and add it under its group</span>}
            section="corr"
            enabledTypes={[]}
          />
          <div className="rt-stream-list">{unknown.map(card)}</div>
        </div>
      )}
    </>
  );
}

interface StreamCardProps {
  section: InputSection;
  product: InputProduct;
  /** An SSR stream SBAS replaces: kept, but left out of the run. */
  paused: boolean;
  onContextMenu: (x: number, y: number, bar: HTMLElement) => void;
}

/**
 * One source, as one box: its type badge, its main field (mountpoint, URL,
 * device or SBAS mode) and remove on a row, and, for serial and SBAS, their
 * other settings under that row in the same box. Every type looks the same,
 * and nothing opens or closes.
 */
function StreamCard({ section, product: p, paused, onContextMenu }: StreamCardProps) {
  const updateProduct = useAppStore((s) => s.updateProduct);
  const removeProduct = useAppStore((s) => s.removeProduct);
  const onChange: StreamBodyProps["onChange"] = (patch) => updateProduct(section, p.id, patch);
  const { Field, Settings } = PARTS[p.type];
  return (
    <div
      className={`rt-stream-block rt-stream-flat${Settings ? " rt-stream-box" : ""}${paused ? " is-paused" : ""}`}
      data-type={dataType(p.type)}
      // Focusable so the right-click menu can hand focus back on Escape.
      tabIndex={-1}
      onContextMenu={(e) => {
        e.preventDefault();
        onContextMenu(e.clientX, e.clientY, e.currentTarget);
      }}
    >
      <span className="rt-stream-type">{TYPE_LABEL[p.type]}</span>
      <div className="rt-stream-field" title={maskLogin(p.value)}>
        {Field ? (
          <Field section={section} product={p} onChange={onChange} />
        ) : (
          <span className="rt-stream-title">{productTitle(p)}</span>
        )}
      </div>
      {paused && <span className="rt-paused">paused while SBAS is on</span>}
      <button
        type="button"
        className="btn-icon source-remove"
        onClick={() => removeProduct(section, p.id)}
        title="Remove this stream"
        aria-label="Remove this stream"
      >
        <TrashIcon size={14} />
      </button>
      {Settings && (
        <div className="rt-stream-box-body">
          <Settings section={section} product={p} onChange={onChange} />
        </div>
      )}
    </div>
  );
}

/** The live types, for the file-run plus menus in ConfigInputSource. */
export { ENABLED_TYPES as LIVE_PRODUCT_TYPES };
