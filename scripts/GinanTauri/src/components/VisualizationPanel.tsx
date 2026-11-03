import { useState } from "react";
import { resultsDir, useAppStore } from "../store/appStore";
import PosSolutionView from "./PosSolutionView";
import TraceResidualsView from "./TraceResidualsView";
import VizEmpty from "./VizEmpty";
import { TabBarActions } from "./TabBarSlot";

/**
 * The Visualization tab: a source switch over one of two views.
 *
 * PEA writes two plottable things per run. The `.pos` is the answer — where
 * the receiver ended up. The `.TRACE` is the
 * working: a prefit and postfit residual per measurement per epoch, which is
 * where a bad satellite or signal is visible while the position is still only
 * slightly wrong. They are different enough in shape (one solution against
 * time, versus one line per satellite-signal-measurement) that sharing a
 * toolbar would serve neither, so each owns its own file list, controls and
 * statistics table and this component only chooses between them.
 *
 * Both views read the output directory fresh each time they mount, so their
 * state is cheap to throw away on a switch.
 *
 * The switch sits at the left end of the tab's toolbar row, beside each view's
 * own controls (see TabBarSlot); CSS orders it first, since the views mount
 * their portals after it.
 */

type SourceKey = "position" | "residuals";

export default function VisualizationPanel() {
  const outputDir = useAppStore(resultsDir);
  const [source, setSource] = useState<SourceKey>("position");

  if (!outputDir) {
    return (
      <VizEmpty
        title="No output directory set"
        body="Pick an output directory in the sidebar. The plots are read from the .pos and .TRACE files PEA writes there."
      />
    );
  }

  return (
    <div className="viz-panel">
      <TabBarActions>
        <div className="segmented viz-source" role="group" aria-label="Data source">
          <button
            className={source === "position" ? "is-active" : ""}
            onClick={() => setSource("position")}
            title="The solved position from PEA's .pos output"
          >
            Position
          </button>
          <button
            className={source === "residuals" ? "is-active" : ""}
            onClick={() => setSource("residuals")}
            title="Per-measurement prefit and postfit residuals from PEA's .TRACE output"
          >
            Residuals
          </button>
        </div>
      </TabBarActions>

      {source === "position" ? <PosSolutionView /> : <TraceResidualsView />}
    </div>
  );
}
