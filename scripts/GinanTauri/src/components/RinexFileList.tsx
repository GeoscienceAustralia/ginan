/**
 * The list of selected observation files, with a remove button on each and the
 * dashed zone that adds more, by click or by dropping files on it. Kept apart from
 * ConfigInputSource so the single-file layout there stays simple: with one
 * file open this renders only the zone, and the list appears once there are two.
 */

import "./RinexFileList.css";
import { useRef } from "react";
import { useAppStore } from "../store/appStore";
import { distinctMarkers } from "../store/rinexFiles";
import { usePickers } from "../hooks/usePickers";
import { useRinexDropTarget } from "../hooks/useRinexDrop";
import type { RinexData } from "../types";
import InfoTip from "./InfoTip";
import { CloseIcon, PlusIcon } from "./icons";

/** `2024-01-02`, or `2024-01-02 to 2024-01-04` when the file spans days. */
function dateSpan(data: RinexData | null): string | null {
  const start = data?.start_epoch?.slice(0, 10);
  const end = data?.end_epoch?.slice(0, 10);
  if (!start) return null;
  return end && end !== start ? `${start} to ${end}` : start;
}

function fileName(path: string): string {
  return path.split(/[\\/]/).pop() || path;
}

export default function RinexFileList() {
  const rinexPath = useAppStore((s) => s.rinexPath);
  const rinexData = useAppStore((s) => s.rinexData);
  const extraRinex = useAppStore((s) => s.extraRinex);
  const isProcessing = useAppStore((s) => s.isProcessing);
  const { pickMoreRinex, removeRinexFile, addRinexFiles } = usePickers();
  const addZone = useRef<HTMLButtonElement>(null);
  const { isDraggingOver } = useRinexDropTarget(addZone, (paths) => void addRinexFiles(paths));

  // Nothing to add to until a primary file has been read.
  if (!rinexPath || !rinexData) return null;

  const files = [{ path: rinexPath, data: rinexData }, ...extraRinex];
  const stations = distinctMarkers(files.map((f) => f.data));

  return (
    <div className="rinex-files">
      {files.length > 1 && (
        <>
          <ul className="rinex-file-list" aria-label="Selected RINEX files">
            {files.map((f, i) => {
              const name = fileName(f.path);
              const span = dateSpan(f.data);
              return (
                <li key={f.path} className="rinex-file">
                  <span className="rinex-file-text">
                    <span className="rinex-file-name" title={f.path}>
                      {name}
                    </span>
                    <span className="rinex-file-meta">
                      {[i === 0 ? "Primary" : null, f.data.marker_name, span]
                        .filter(Boolean)
                        .join(" · ")}
                    </span>
                  </span>
                  <button
                    type="button"
                    className="btn-icon"
                    aria-label={`Remove ${name}`}
                    title={isProcessing ? "Not while processing" : `Remove ${name}`}
                    disabled={isProcessing}
                    onClick={() => void removeRinexFile(f.path)}
                  >
                    <CloseIcon size={14} />
                  </button>
                </li>
              );
            })}
          </ul>
          {stations.length > 1 && (
            <p className="epoch-note is-warning" role="status">
              These files are from different stations ({stations.join(", ")}). PEA
              processes each station as its own receiver.
            </p>
          )}
        </>
      )}
      <button
        ref={addZone}
        type="button"
        className={`file-drop-zone rinex-add${isDraggingOver ? " is-over" : ""}`}
        disabled={isProcessing}
        onClick={() => void pickMoreRinex()}
      >
        <span className="file-drop-zone-title">
          <PlusIcon size={13} />
          Add more files...
        </span>
        <span className="file-drop-zone-hint">or drop them here</span>
      </button>
      {files.length > 1 && (
        <InfoTip text="The first file sets the marker, receiver, antenna and observation codes. The time window and product downloads cover every file." />
      )}
    </div>
  );
}
