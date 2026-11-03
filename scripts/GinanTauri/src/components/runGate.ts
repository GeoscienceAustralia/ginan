import type { ConfigSectionKey, SettingsPageKey, WorkspaceTabKey } from "../store/appStore";
import type { InputProduct } from "../types";
import { correctionCoverage, streamMount } from "../config/correctionRoles";

/** Where the setting that clears a blocker lives. */
export type BlockerFix =
  | { kind: "settings"; page: SettingsPageKey; label: string }
  | { kind: "sidebar"; section: ConfigSectionKey; label: string }
  | { kind: "tab"; tab: WorkspaceTabKey; label: string };

export interface RunBlocker {
  id: string;
  message: string;
  /** Absent when there is nothing to open, e.g. a check still in flight. */
  fix?: BlockerFix;
}

/** True when this app runs on Windows (the webview's user agent says so). */
export function onWindows(): boolean {
  return typeof navigator !== "undefined" && navigator.userAgent.includes("Windows");
}

/**
 * True when `path` cannot be a directory on this machine: a Unix path such as
 * `/home/...` on Windows (it would land in `C:\home\...`), or a drive path
 * such as `C:\...` elsewhere. A config imported from a teammate's machine is
 * the usual way one gets in.
 */
export function isForeignPath(path: string, windows = onWindows()): boolean {
  const p = path.trim();
  if (!p) return false;
  const drive = /^[A-Za-z]:[\\/]/.test(p);
  const unc = p.startsWith("\\\\");
  return windows ? !drive && !unc && p.startsWith("/") : drive || unc;
}

/** The slice of the store the Process gate reads. */
export interface RunGateState {
  peaBinaryPath: string | null;
  outputDir: string | null;
  productsDir: string | null;
  rinexPath: string | null;
  startEpoch: string | null;
  cddisStatus: "idle" | "checking" | "ok" | "error";
  inputMode: "file" | "realtime";
  realtime: { obs_streams: string[]; caster_url: string };
  inputProducts: { obs: { type: string }[]; corr: InputProduct[] };
}

/**
 * Everything that stops Process, in the order worth fixing them, each saying
 * where to fix it. Empty when a run can start.
 *
 * A real-time run skips the file-run gates: it has no RINEX file or epoch
 * window, and no CDDIS check, since SSR corrections come from the caster
 * rather than a downloaded product. It needs broadcast ephemeris, and SSR or
 * SBAS on top (see `correctionRoles`).
 */
export function runBlockers(s: RunGateState): RunBlocker[] {
  const realtime = s.inputMode === "realtime";
  const out: RunBlocker[] = [];

  if (!s.peaBinaryPath) {
    out.push({
      id: "pea",
      message: "PEA is not ready. Wait for its download to finish, or choose a binary in Settings › PEA Binary.",
      fix: { kind: "settings", page: "binary", label: "Open PEA Binary" },
    });
  }
  if (!s.outputDir) {
    out.push({
      id: "output",
      message: "No output directory. Choose one in Settings › Output.",
      fix: { kind: "settings", page: "output", label: "Open Output" },
    });
  } else if (isForeignPath(s.outputDir)) {
    out.push({
      id: "output-foreign",
      message: `The output directory ${s.outputDir} is not a path on this computer, probably from an imported config. Choose one in Settings › Output.`,
      fix: { kind: "settings", page: "output", label: "Open Output" },
    });
  }

  if (realtime) {
    if (s.realtime.obs_streams.length === 0) {
      out.push({
        id: "obs-streams",
        message: "No observation stream. Add one in the Input section.",
        fix: { kind: "sidebar", section: "input", label: "Show Input" },
      });
    }
    const showInput: BlockerFix = { kind: "sidebar", section: "input", label: "Show Input" };
    const cover = correctionCoverage(s.inputProducts.corr);
    if (!cover.bce) {
      out.push({
        id: "bce-stream",
        message:
          "No broadcast ephemeris stream (e.g. BCEP00BKG0). PEA needs it for orbits and clocks, with SSR or SBAS. Add one under Corrections.",
        fix: showInput,
      });
    }
    if (!cover.corrections) {
      out.push({
        id: "ssr-stream",
        message: "No SSR corrections stream (e.g. SSRA00CNE0) or SBAS. Add one under Corrections.",
        fix: showInput,
      });
    }
    for (const p of cover.unknown) {
      const mount = streamMount(p);
      out.push({
        id: `role-${p.id}`,
        message: mount
          ? `Cannot tell whether ${mount} carries broadcast ephemeris or SSR. Run Monitor stream, or remove it and add it again under its group in Corrections.`
          : "A Corrections stream has no mountpoint yet. Choose one, or remove the card.",
        fix: showInput,
      });
    }
    if (!s.realtime.caster_url && s.inputProducts.obs.some((p) => p.type === "ntrip")) {
      out.push({
        id: "caster",
        message:
          "No NTRIP caster. Set it in Settings › Credentials, or switch the NTRIP observation streams to Serial.",
        fix: { kind: "settings", page: "credentials", label: "Open Credentials" },
      });
    }
    return out;
  }

  if (!s.productsDir) {
    out.push({
      id: "products",
      message: "No products directory. Choose one in Settings › Products Cache.",
      fix: { kind: "settings", page: "products", label: "Open Products Cache" },
    });
  } else if (isForeignPath(s.productsDir)) {
    out.push({
      id: "products-foreign",
      message: `The products directory ${s.productsDir} is not a path on this computer. Choose one in Settings › Products Cache.`,
      fix: { kind: "settings", page: "products", label: "Open Products Cache" },
    });
  }
  if (!s.rinexPath) {
    out.push({
      id: "rinex",
      message: "No RINEX file. Open one in the Input section.",
      fix: { kind: "sidebar", section: "input", label: "Show Input" },
    });
  } else if (!s.startEpoch) {
    // Only worth saying once there is a file: without one, the file is the fix.
    out.push({
      id: "start-epoch",
      message: "No start epoch. The RINEX header did not give one, so enter it under General.",
      fix: { kind: "sidebar", section: "general", label: "Show General" },
    });
  }
  if (s.cddisStatus === "checking") {
    out.push({ id: "cddis-checking", message: "Checking the CDDIS connection..." });
  } else if (s.cddisStatus === "error") {
    out.push({
      id: "cddis",
      message: "The CDDIS login is missing or was rejected. Set it in Settings › Credentials.",
      fix: { kind: "settings", page: "credentials", label: "Open Credentials" },
    });
  }
  return out;
}
