import { useEffect } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";
import type {
  BiaCodePriorities,
  CodeIntersection,
  ConstellationObsCodes,
} from "../types";
import { WF } from "../config/workflowSteps";
import { biaPhaseSignals, phaseCodes } from "../config/obsCodes";

/**
 * GPS week number from an ISO-like epoch ("YYYY-MM-DD HH:MM:SS").
 * GPS epoch is 1980-01-06 UTC; one week = 7 days. Returns 0 if unparseable.
 */
function gpsWeekFromEpoch(epoch: string | null | undefined): number {
  if (!epoch) return 0;
  // Accept both "YYYY-MM-DD HH:MM:SS" and "YYYY-MM-DDTHH:MM:SS".
  const iso = epoch.replace(" ", "T") + (epoch.length <= 10 ? "T00:00:00Z" : "Z");
  const t = Date.parse(iso);
  if (Number.isNaN(t)) return 0;
  const gpsEpoch = Date.UTC(1980, 0, 6);
  return Math.floor((t - gpsEpoch) / (7 * 24 * 3600 * 1000));
}

/**
 * Compute the per-constellation intersection of RINEX-observed phase codes with
 * the phase signals the BIA covers. Constellations only appear in the output when
 * RINEX actually contains phase observations for them.
 *
 * Phase (L) codes only, since those are the signals the UI offers. The RINEX
 * header also lists a Doppler and a strength code for each of them, which no
 * bias table carries, so counting those would inflate the unsupported tally.
 */
function computeIntersection(
  rinexCodes: ConstellationObsCodes | null | undefined,
  bia: BiaCodePriorities,
): Record<string, CodeIntersection> {
  const out: Record<string, CodeIntersection> = {};
  if (!rinexCodes) return out;
  (["gps", "gal", "glo", "bds", "qzs"] as const).forEach((k) => {
    const rinex = phaseCodes(rinexCodes[k]);
    if (rinex.length === 0) return; // no phase signals in RINEX → skip
    const biaSet = biaPhaseSignals(bia[k]);
    const biaCodes = Array.from(biaSet).sort();
    const rinexSet = new Set(rinex);
    out[k.toUpperCase()] = {
      usable: rinex.filter((c) => biaSet.has(c)),
      rinexOnly: rinex.filter((c) => !biaSet.has(c)),
      biaOnly: biaCodes.filter((c) => !rinexSet.has(c)),
    };
  });
  return out;
}

/**
 * When the catalogue contains the current (provider, series, project)
 * combo, fetch the sample BIA (cached after first call) and compute the
 * RINEX∩BIA intersection per constellation. Re-runs whenever the selection or
 * the start epoch changes. Failures are swallowed (intersection cleared).
 *
 * Lives as a hook rather than inside a component because it is app-lifetime
 * behaviour with no UI of its own. Mount it exactly once (App).
 */
export function useBiaValidation() {
  // Only the inputs the effect keys on are subscribed to. Taking the whole
  // store here would re-render the app shell (where this is mounted) on every
  // console line during a run.
  const productsDir = useAppStore((s) => s.productsDir);
  const startEpoch = useAppStore((s) => s.startEpoch);
  const rinexStartEpoch = useAppStore((s) => s.rinexData?.start_epoch);
  const provider = useAppStore((s) => s.pppSelection.provider);
  const series = useAppStore((s) => s.pppSelection.series);
  const project = useAppStore((s) => s.pppSelection.project);
  const pppCatalogue = useAppStore((s) => s.pppCatalogue);

  useEffect(() => {
    const {
      rinexData,
      appendLine, appendWorkflowLine, setBia, setCodeIntersection,
      setConstellationCodeValidity,
      beginActivityStep, resolveActivityStep, endActivity,
    } = useAppStore.getState();

    // This is the last step of the open-a-file chain, so every exit below has
    // to settle the activity — an unresolved step would spin forever. The
    // store ignores these calls when no activity is running (a provider change
    // re-runs this effect long after the panel is gone).
    const settle = (state: "done" | "error" | "skipped", detail?: string) => {
      resolveActivityStep("bia", state, detail);
      endActivity();
    };

    if (!pppCatalogue || pppCatalogue.entries.length === 0) {
      setBia(null);
      setCodeIntersection(null);
      return;
    }
    const match = pppCatalogue.entries.find(
      (e) => e.provider === provider && e.series === series && e.project === project,
    );
    if (!match) {
      setBia(null);
      setCodeIntersection(null);
      settle("skipped", `No products for ${provider}/${series}/${project}`);
      return;
    }
    const gpsWeek = gpsWeekFromEpoch(startEpoch || rinexData?.start_epoch);
    if (gpsWeek <= 0) {
      settle("skipped", "Observation window has no usable date");
      return;
    }
    const dir = productsDir;
    if (!dir) {
      settle("skipped", "No products directory set");
      return;
    }

    let cancelled = false;
    beginActivityStep("bia", `${provider}/${series}/${project}`);
    appendWorkflowLine(WF.validatingBia(provider, series, project));
    invoke<BiaCodePriorities>("fetch_bia_codes_for_combo", {
      provider,
      series,
      project,
      gpsWeek,
      productsDir: dir,
    })
      .then((bia) => {
        if (cancelled) return;
        setBia(bia);
        const intersection = computeIntersection(
          rinexData?.observation_codes ?? null,
          bia,
        );
        setCodeIntersection(intersection);
        // Flip each constellation code's `valid` flag based on whether the
        // BIA actually supports it. Without this the ConstellationFilter UI
        // keeps every code greyed out because its `valid` field never updates.
        // Mirrors the per-code update loop in store.validateCodesAgainstBia.
        const biaSets: Record<string, Set<string>> = {
          GPS: biaPhaseSignals(bia.gps),
          GAL: biaPhaseSignals(bia.gal),
          GLO: biaPhaseSignals(bia.glo),
          BDS: biaPhaseSignals(bia.bds),
          QZS: biaPhaseSignals(bia.qzs),
        };
        const codes = useAppStore.getState().constellationCodes;
        (["GPS", "GAL", "GLO", "BDS", "QZS"] as const).forEach((c) => {
          const key = c.toLowerCase() as keyof typeof codes;
          const validSet = biaSets[c];
          codes[key].forEach((item, index) => {
            setConstellationCodeValidity(c, index, validSet.has(item.code));
          });
        });
        // GinanUI logs exactly two Workflow steps around this — the cache and
        // the validation — and keeps the per-constellation breakdown on the
        // Console channel. The breakdown still goes out below, just there.
        appendWorkflowLine(WF.biaCached(provider, series, project));
        // Same tally the console lines below report, condensed to one line for
        // the activity panel: how many observed phase signals the bias table covers.
        let usable = 0;
        let observed = 0;
        Object.values(intersection).forEach((info) => {
          usable += info.usable.length;
          observed += info.usable.length + info.rinexOnly.length;
        });
        settle(
          "done",
          observed > 0
            ? `${usable} of ${observed} phase signals supported`
            : `${provider}/${series}/${project}`,
        );
        // Permanent record of which RINEX phase codes the chosen BIA supports.
        appendLine(`BIA validation for ${provider}/${series}/${project}:`);
        (["GPS", "GAL", "GLO", "BDS", "QZS"] as const).forEach((sys) => {
          const info = intersection?.[sys];
          if (!info) return;
          const total = info.usable.length + info.rinexOnly.length;
          if (total === 0) return;
          if (info.usable.length === 0) {
            appendLine(`  ${sys}: no overlap with BIA`);
          } else {
            appendLine(
              `  ${sys}: ${info.usable.length} of ${total} RINEX phase codes supported (${info.usable.join(", ")})`,
            );
          }
        });
        appendWorkflowLine(WF.biaValidated(provider, series, project));
      })
      .catch((e) => {
        if (cancelled) return;
        console.error("fetch_bia_codes_for_combo failed:", e);
        appendWorkflowLine(WF.biaFetchFailed());
        appendLine(`BIA fetch failed for ${provider}/${series}/${project}: ${e}`);
        setBia(null);
        setCodeIntersection(null);
        // Not fatal to the file itself — the config panel is usable without a
        // bias table, so the panel reports the step as failed, not the run.
        settle("error", String(e));
      });
    return () => { cancelled = true; };
  }, [provider, series, project, pppCatalogue, startEpoch, productsDir, rinexStartEpoch]);
}
