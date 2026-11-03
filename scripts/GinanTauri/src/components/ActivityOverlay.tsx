import { useEffect } from "react";
import { useAppStore } from "../store/appStore";
import type { ActivityStep } from "../store/appStore";
import { CheckIcon, CloseIcon, AlertIcon } from "./icons";

/**
 * Live view of the fetch chain that runs when a file is opened: unpacking, the
 * header read, the mirror scan and the bias-table download. Showing the steps
 * keeps a 20-second network round trip from looking like a hang.
 *
 * Dismissing it does not cancel anything — the work carries on and the console
 * keeps its full record. The panel closes itself once everything succeeds;
 * after a failure it stays up so the reason is readable.
 */
export default function ActivityOverlay() {
  const activity = useAppStore((s) => s.activity);
  const hidden = useAppStore((s) => s.activityHidden);
  const clearActivity = useAppStore((s) => s.clearActivity);
  const hideActivity = useAppStore((s) => s.hideActivity);

  const failed = activity
    ? activity.error !== null || activity.steps.some((s) => s.state === "error")
    : false;

  useEffect(() => {
    if (!activity?.finished || failed) return;
    // Hold the completed list on screen just long enough to read the last tick.
    const t = setTimeout(clearActivity, 900);
    return () => clearTimeout(t);
  }, [activity?.finished, failed, clearActivity]);

  if (!activity || hidden) return null;

  const done = activity.steps.filter((s) => s.state === "done" || s.state === "skipped").length;

  return (
    <div className="activity-scrim">
      <div className="activity-card" role="status" aria-live="polite">
        <div className="activity-head">
          <div className="activity-heading">
            <h2>{activity.title}</h2>
            <p>
              {activity.finished
                ? failed
                  ? "Stopped"
                  : "Ready"
                : `Step ${Math.min(done + 1, activity.steps.length)} of ${activity.steps.length}`}
            </p>
          </div>
          <button className="btn-icon" onClick={hideActivity} title="Hide. This does not cancel anything">
            <CloseIcon size={15} />
          </button>
        </div>

        <ol className="activity-steps">
          {activity.steps.map((step) => (
            <StepRow key={step.key} step={step} />
          ))}
        </ol>

        {activity.error && (
          <p className="activity-error">
            <AlertIcon size={14} />
            {activity.error}
          </p>
        )}
      </div>
    </div>
  );
}

function StepRow({ step }: { step: ActivityStep }) {
  const isActive = step.state === "active";

  return (
    <li className={`activity-step is-${step.state}`}>
      <span className="activity-step-mark" aria-hidden="true">
        {isActive ? (
          <span className="spinner" />
        ) : step.state === "done" ? (
          <CheckIcon size={13} />
        ) : step.state === "error" ? (
          <CloseIcon size={13} />
        ) : step.state === "skipped" ? (
          <span className="activity-step-dash" />
        ) : (
          <span className="activity-step-dot" />
        )}
      </span>

      <div className="activity-step-text">
        <span className="activity-step-label">{step.label}</span>
        {step.detail && <span className="activity-step-detail">{step.detail}</span>}
        {isActive && (
          // Determinate only while the backend reports bytes; otherwise the bar
          // sweeps, which is the honest signal for a directory listing.
          <span className={`activity-bar${step.percent === null ? " is-indeterminate" : ""}`}>
            <span
              className="activity-bar-fill"
              style={step.percent === null ? undefined : { width: `${step.percent}%` }}
            />
          </span>
        )}
      </div>
    </li>
  );
}
