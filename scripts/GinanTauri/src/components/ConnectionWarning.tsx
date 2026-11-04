import { useAppStore } from "../store/appStore";
import { useCddisRetry, useIsOnline } from "../hooks/useCddisRetry";
import { AlertIcon, CloseIcon, ResetIcon } from "./icons";

/**
 * The app-wide banner under the navbar.
 *
 * Three things can be wrong, and they want different words and different
 * buttons, so they are separate branches rather than one message that tries to
 * cover all of them:
 *
 *   1. A transient fault reported by a download (dismissible).
 *   2. CDDIS is unreachable — usually the network being down.
 *   3. There are no Earthdata credentials.
 *
 * Case 2 outranks case 3: with no connection, telling someone to go and make an
 * account sends them somewhere they cannot reach.
 */
export default function ConnectionWarning() {
  const fault = useAppStore((s) => s.connectionFault);
  const clear = useAppStore((s) => s.clearConnectionFault);
  const offlineMode = useAppStore((s) => s.offlineMode);
  const cddisStatus = useAppStore((s) => s.cddisStatus);
  const cddisStatusMessage = useAppStore((s) => s.cddisStatusMessage);
  const openCredentials = useAppStore((s) => s.setCredentialsModalOpen);

  // Mounted here because this component is always rendered, so the polling
  // keeps running whichever banner is (or is not) on screen.
  const { retry, retrying } = useCddisRetry();
  const online = useIsOnline();

  // An explicit connection fault takes precedence over the standing offline
  // notice, since it's the more actionable/transient signal.
  if (fault) {
    return (
      <div className="connection-warning">
        <span className="connection-warning-icon"><AlertIcon size={14} /></span>
        <span className="connection-warning-text">{fault}</span>
        <button className="connection-warning-dismiss" onClick={clear} title="Dismiss">
          <CloseIcon size={13} />
        </button>
      </div>
    );
  }

  // The backend prefixes every network-layer failure with "CDDIS unreachable";
  // a missing netrc entry reports itself differently. Checking the machine's
  // own connection as well catches the case where the check has not run yet.
  const unreachable =
    cddisStatus === "error" &&
    (!online || (cddisStatusMessage ?? "").includes("unreachable"));

  if (unreachable) {
    return (
      <div className="connection-warning">
        <span className="connection-warning-icon"><AlertIcon size={14} /></span>
        <span className="connection-warning-text">
          {online
            ? "Can't reach the CDDIS server. Ginan will keep trying and reconnect on its own."
            : "No internet connection. Ginan will reconnect on its own once you're back online."}
        </span>
        <button
          className="connection-warning-dismiss"
          onClick={() => void retry()}
          disabled={retrying}
          title="Check the connection again now"
        >
          <ResetIcon size={13} />{" "}
          {retrying ? "Checking..." : "Try now"}
        </button>
      </div>
    );
  }

  if (offlineMode) {
    return (
      <div className="connection-warning">
        <span className="connection-warning-icon"><AlertIcon size={14} /></span>
        <span className="connection-warning-text">
          No Earthdata credentials - product downloads are unavailable. CDDIS is the
          only product source, so a free NASA Earthdata account is required.
        </span>
        <button
          className="connection-warning-dismiss"
          onClick={() => openCredentials(true)}
          title="Set CDDIS credentials or create a free Earthdata account"
        >
          Set up account
        </button>
      </div>
    );
  }

  return null;
}
