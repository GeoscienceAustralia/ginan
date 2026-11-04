/**
 * The observation source the Observation products select, split out of
 * ConfigPanelTabs to keep each file under 500 lines. ConfigPanel renders it
 * above the accordion sections.
 *
 * There is no mode switch: the run mode follows the products (see
 * hooks/useProductAdd). A File product is a file run, a live product a
 * realtime run, and the plus menus ask before adding one of the other kind.
 */

import { useEffect, useState, type ReactNode } from "react";
import { useAppStore } from "../store/appStore";
import { useProductAdd } from "../hooks/useProductAdd";
import { cancelRinexLoad, usePickers } from "../hooks/usePickers";
import { FileCard, fileBaseName } from "./FileDropZone";
import { PppProductSource } from "./ConfigPanelTabs";
import InfoTip from "./InfoTip";
import InputSectionHeader from "./InputSectionHeader";
import RealtimeConfigPanel, { LIVE_PRODUCT_TYPES } from "./RealtimeConfigPanel";
import RinexFileList from "./RinexFileList";

/**
 * Drop the loaded RINEX files and everything derived from them. The run stays a
 * file run, now with an empty File card ("No file selected").
 */
function clearRinex() {
  // A load still in flight would otherwise put the file straight back.
  cancelRinexLoad();
  const { setPppCatalogue, setBia, setCodeIntersection } = useAppStore.getState();
  // Same reset as loadRinexFile, so the Corrections card does not keep
  // offering the removed file's catalogue.
  setPppCatalogue(null);
  setBia(null);
  setCodeIntersection(null);
  // The extra files go with the primary: they were opened as one selection.
  useAppStore.setState({ rinexPath: null, rinexData: null, extraRinex: [] });
}

/** One file-run section: the shared heading and plus menu, then its card. */
function FileSection({
  title,
  help,
  section,
  onPickFile,
  tourId,
  children,
}: {
  title: string;
  help: string;
  section: "obs" | "corr";
  /** `data-tour` for the whole section, which stays on screen when its card is shut. */
  tourId?: string;
  /** The plus menu's File item. */
  onPickFile: () => void;
  children: ReactNode;
}) {
  const { onAdd, enabledTypes, disabledReason, dialog } = useProductAdd(section, LIVE_PRODUCT_TYPES, onPickFile);
  return (
    // Same classes and header as the live run's sections (RealtimeConfigPanel).
    <div className="rt-section rt-stream-section" data-tour={tourId}>
      <InputSectionHeader
        title={title}
        info={<InfoTip text={help} />}
        onAdd={onAdd}
        enabledTypes={enabledTypes}
        disabledReason={disabledReason}
        section={section}
      />
      {children}
      {dialog}
    </div>
  );
}

/**
 * The observation source the run mode selects: the File cards in a file run,
 * otherwise the live-stream panel. Rendered as the first thing in the scroll
 * area.
 */
export function InputSource() {
  const rinexPath = useAppStore((s) => s.rinexPath);
  const rinexData = useAppStore((s) => s.rinexData);
  const fileCount = useAppStore((s) => (s.rinexPath ? 1 + s.extraRinex.length : 0));
  const inputMode = useAppStore((s) => s.inputMode);
  const isProcessing = useAppStore((s) => s.isProcessing);
  const pppSelection = useAppStore((s) => s.pppSelection);
  const pppCatalogue = useAppStore((s) => s.pppCatalogue);
  const { pickRinex } = usePickers();
  // The File card was removed while empty: Observation shows just its plus
  // menu. A file run holds at most one File card, so this is not product state;
  // it resets when a file loads, and the plus menu's File item brings it back.
  const [removed, setRemoved] = useState(false);
  useEffect(() => {
    if (rinexPath) setRemoved(false);
  }, [rinexPath]);

  if (inputMode !== "file") {
    return (
      <div className="config-mode-source" data-tour="section-input">
        <RealtimeConfigPanel />
      </div>
    );
  }

  return (
    <div className="config-mode-source" data-tour="section-input">
      <div className="rt-panel">
        <FileSection
          title="Observation"
          help="The RINEX file you are positioning from. Process will not start without one."
          section="obs"
          onPickFile={() => {
            setRemoved(false);
            pickRinex();
          }}
        >
          {!removed && (
            <FileCard
              // From two files up the card names the primary and counts the rest;
              // the list below says which they are.
              name={
                rinexPath
                  ? fileCount >= 2
                    ? `${fileBaseName(rinexPath)} + ${fileCount - 1} more`
                    : fileBaseName(rinexPath)
                  : null
              }
              empty="No file selected"
              collapsible={false}
              detail={
                fileCount >= 2
                  ? `${fileCount} RINEX observation files`
                  : rinexPath
                    ? "RINEX observation file"
                    : "Attach a RINEX observation file"
              }
              // A running PEA is reading the file. With a file loaded the cross
              // clears it; on the empty card it removes the card.
              onRemove={isProcessing ? undefined : rinexPath ? clearRinex : () => setRemoved(true)}
              // With no file the card itself is the drop box; × on a loaded
              // file clears it and the drop box returns.
              onBrowse={pickRinex}
            >
              {fileCount < 2 && rinexData && (
                <p className="field-hint">
                  {rinexData.marker_name ?? "unknown"} - {rinexData.constellations.join(", ")}
                </p>
              )}
              {/* Lists the files once there are two, and offers "Add more files…";
                  the plus menu's File item and Browse replace the whole selection. */}
              <RinexFileList />
              {fileCount >= 2 && !isProcessing && (
                <button className="btn-ghost rinex-replace" onClick={pickRinex}>
                  Replace all files...
                </button>
              )}
            </FileCard>
          )}
        </FileSection>
        <FileSection
          title="Corrections"
          help="Orbit, clock and bias products are downloaded for the observation dates when you press Process. Choose the analysis centre, series and project here."
          section="corr"
          tourId="ppp-source"
          onPickFile={pickRinex}
        >
          <FileCard
            // The current pick, e.g. "COD · RAP", once a catalogue is scanned.
            name={
              pppCatalogue && pppCatalogue.entries.length > 0
                ? `${pppSelection.provider} · ${pppSelection.series}`
                : "Product files"
            }
            empty=""
            icon="products"
            detail="Orbit, clock & bias products"
            // One box like a serial source: the pick on the row, its settings
            // under it in the same box. Nothing opens or closes.
            collapsible={false}
          >
            <PppProductSource />
          </FileCard>
        </FileSection>
      </div>
    </div>
  );
}
