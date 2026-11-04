/**
 * Profiling HUD. Loaded only by a `--mode profiling` build (see main.tsx), so
 * neither this file nor the tracing plugin reaches a normal bundle.
 *
 * Measures the page from the inside, because the backend's profilers cannot
 * see into the webview: frame pacing, React render cost per <Profiler>, and
 * the round trip of every command. A summary goes to the tracing log every
 * few seconds as a `perf {json}` line, next to the backend's own output.
 */
import { useEffect, useState } from "react";
import { createRoot } from "react-dom/client";
import { invoke } from "@tauri-apps/api/core";
import { generateFlamechart, generateFlamegraph, info } from "@fltsci/tauri-plugin-tracing";
import { invokes } from "./core";
import { renders } from "./onRender";

const REPORT_MS = 5000;
/** A frame this long is a visible hitch: three dropped frames at 60 Hz. */
const LONG_FRAME_MS = 50;

type Stat = { n: number; ms: number; max: number };
type Summary = {
  fps: number;
  frameP95: number;
  frameMax: number;
  longFrames: number;
  renders: Record<string, Stat>;
  invokes: Record<string, Stat>;
};

const round = (x: number) => Math.round(x * 10) / 10;
const roundStats = (m: Map<string, Stat>) =>
  Object.fromEntries([...m].map(([k, s]) => [k, { n: s.n, ms: round(s.ms), max: round(s.max) }]));

// ── Frame pacing ─────────────────────────────────────────────────────────────
let frames: number[] = [];
let lastFrame = performance.now();
const onFrame = (now: number) => {
  // rAF stops while the window is hidden; the gap is not a slow frame.
  if (!document.hidden) frames.push(now - lastFrame);
  lastFrame = now;
  requestAnimationFrame(onFrame);
};
requestAnimationFrame(onFrame);
document.addEventListener("visibilitychange", () => {
  lastFrame = performance.now();
});

function takeSummary(): Summary {
  const sorted = frames.slice().sort((a, b) => a - b);
  const total = sorted.reduce((a, b) => a + b, 0);
  const summary: Summary = {
    fps: total > 0 ? round((sorted.length * 1000) / total) : 0,
    frameP95: round(sorted[Math.floor(sorted.length * 0.95)] ?? 0),
    frameMax: round(sorted[sorted.length - 1] ?? 0),
    longFrames: sorted.filter((f) => f > LONG_FRAME_MS).length,
    renders: roundStats(renders),
    invokes: roundStats(invokes),
  };
  frames = [];
  renders.clear();
  invokes.clear();
  return summary;
}

const startCpu = () => invoke("plugin:profiling|start_cpu_profile");
const stopCpu = async () => {
  const r = await invoke<{ flamegraph_path: string; sample_count: number }>(
    "plugin:profiling|stop_cpu_profile",
  );
  return `${r.sample_count} samples, ${r.flamegraph_path}`;
};

/**
 * Runs what the three buttons do, unattended, and logs each outcome, so a
 * profiling build can be checked without clicking in the window. Build with
 * VITE_PROFILE_SELFTEST=<seconds> to record the CPU for that long, starting
 * 2 s after launch.
 */
async function selftest(seconds: number) {
  const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));
  const step = async (label: string, job: () => Promise<unknown>) => {
    try {
      void info(`selftest ${label}: ${String(await job())}`);
    } catch (err) {
      void info(`selftest ${label} FAILED: ${String(err)}`);
    }
  };
  await sleep(2000);
  await step("cpu start", startCpu);
  await sleep(seconds * 1000);
  await step("cpu stop", stopCpu);
  await step("flamegraph", generateFlamegraph);
  await step("flamechart", generateFlamechart);
}
const selftestSeconds = Number(import.meta.env.VITE_PROFILE_SELFTEST);
if (selftestSeconds > 0) void selftest(selftestSeconds);

function Hud() {
  const [summary, setSummary] = useState<Summary | null>(null);
  const [cpuRunning, setCpuRunning] = useState(false);
  const [message, setMessage] = useState("");

  useEffect(() => {
    const id = setInterval(() => {
      const s = takeSummary();
      setSummary(s);
      void info(`perf ${JSON.stringify(s)}`);
    }, REPORT_MS);
    return () => clearInterval(id);
  }, []);

  const run = (label: string, job: () => Promise<string>) => () =>
    job().then(
      (out) => setMessage(`${label}: ${out}`),
      (err) => setMessage(`${label} failed: ${String(err)}`),
    );

  const toggleCpu = cpuRunning
    ? run("CPU profile", async () => {
        const out = await stopCpu();
        setCpuRunning(false);
        return out;
      })
    : run("CPU profile", async () => {
        await startCpu();
        setCpuRunning(true);
        return "recording";
      });

  const slowest = Object.entries(summary?.renders ?? {}).sort((a, b) => b[1].ms - a[1].ms);

  return (
    <div
      style={{
        position: "fixed", left: 8, bottom: 32, zIndex: 99999, maxWidth: 360,
        padding: "6px 8px", borderRadius: 6, background: "rgba(0,0,0,0.82)", color: "#e6e6e6",
        font: "11px/1.45 ui-monospace, Menlo, Consolas, monospace", userSelect: "text",
      }}
    >
      <div>
        {summary
          ? `${summary.fps} fps · p95 ${summary.frameP95} ms · max ${summary.frameMax} ms · ${summary.longFrames} long`
          : "profiling: measuring"}
      </div>
      {slowest.map(([id, s]) => (
        <div key={id}>
          {id}: {s.n} renders, {s.ms} ms (max {s.max})
        </div>
      ))}
      <div style={{ display: "flex", gap: 4, marginTop: 4 }}>
        <button onClick={toggleCpu}>{cpuRunning ? "Stop CPU profile" : "Start CPU profile"}</button>
        <button onClick={run("Flamegraph", generateFlamegraph)}>Flamegraph</button>
        <button onClick={run("Flamechart", generateFlamechart)}>Flamechart</button>
      </div>
      {message && <div style={{ marginTop: 4, wordBreak: "break-all" }}>{message}</div>}
    </div>
  );
}

const host = document.createElement("div");
document.body.appendChild(host);
createRoot(host).render(<Hud />);
