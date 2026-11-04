import { useAppStore } from "../store/appStore";
import { switchToLive } from "../hooks/useProductAdd";
import { usePickers } from "../hooks/usePickers";
import { useRinexDrop } from "../hooks/useRinexDrop";
import { PlusIcon, SatelliteIcon } from "./icons";
import ginanLogo from "../assets/ginan-logo.png";

/**
 * Shown until a RINEX observation file is loaded. Nothing else is configurable
 * without one — every sidebar section, the workspace tabs and the run controls
 * all read from the parsed file — so the app opens on a single "add a file"
 * action rather than a shell full of dead controls.
 *
 * This is where every launch starts: the chosen file is session-only and is
 * never restored from the persisted config.
 *
 * It fills the workspace column in place of the tabs (see Workspace). Load
 * feedback is repeated here so it sits next to the button: `rinexPath` set without
 * `rinexData` means a file was chosen but preparing/parsing it is either still
 * running or has failed, and `status` carries the reason.
 */
export default function WelcomeScreen() {
  const { pickRinex, loadRinexFile } = usePickers();
  const { isDraggingOver } = useRinexDrop(loadRinexFile);
  const rinexPath = useAppStore((s) => s.rinexPath);
  const status = useAppStore((s) => s.status);

  // A path with no parsed data behind it: mid-load, or the load failed.
  const loadNote = rinexPath ? status : null;

  return (
    <div className={`welcome${isDraggingOver ? " welcome-dragging" : ""}`}>
      <div className="welcome-inner">
        <img className="welcome-logo" src={ginanLogo} alt="" />
        <h2 className="welcome-title">
          {isDraggingOver ? "Drop to open" : "No observation file open"}
        </h2>
        <p className="welcome-text">
          Open a RINEX file from your receiver, or connect to a real-time data stream.
        </p>

        <div className="welcome-options">
          <button
            className="welcome-add"
            data-tour="add-rinex"
            onClick={pickRinex}
            title="Open one or more RINEX observation files"
          >
            <PlusIcon size={26} />
            <span>Add RINEX Observation File</span>
            <span className="welcome-add-sub">or drop one or more anywhere</span>
          </button>

          <button
            className="welcome-add"
            onClick={() => switchToLive()}
            title="Set up real-time processing with streams"
          >
            <SatelliteIcon size={26} />
            <span>Setup Real-time Processing</span>
            <span className="welcome-add-sub">Configure NTRIP streams</span>
          </button>
        </div>

        {loadNote && <p className="welcome-note">{loadNote}</p>}
      </div>
    </div>
  );
}
