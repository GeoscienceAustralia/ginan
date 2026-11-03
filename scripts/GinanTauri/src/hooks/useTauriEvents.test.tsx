import { beforeEach, describe, expect, it } from "vitest";
import { act, renderHook, waitFor } from "@testing-library/react";
import { useTauriEvents } from "./useTauriEvents";
import { flushFrame, resetAppStore, store } from "../test/storeHelpers";
import { emit, listenerCount } from "../test/tauriMocks";
import { WF } from "../config/workflowSteps";

beforeEach(() => {
  resetAppStore();
});

/** Mount the hook and wait for every `listen()` subscription to land. */
async function mounted() {
  const view = renderHook(() => useTauriEvents());
  await waitFor(() => expect(listenerCount("ntrip-stats")).toBe(1));
  return view;
}

describe("subscriptions", () => {
  it("subscribes to every backend channel it consumes", async () => {
    await mounted();
    for (const event of [
      "pea-line",
      "console-line",
      "pea-finished",
      "pea-error",
      "pea-stopped",
      "product-line",
      "product-progress",
      "connection-fault",
      "prepare-progress",
      "ppp-scan-progress",
      "live-stream-stats",
      "ntrip-stats",
    ]) {
      expect(listenerCount(event), event).toBe(1);
    }
  });

  it("unsubscribes on unmount", async () => {
    const { unmount } = await mounted();
    unmount();
    expect(listenerCount("pea-line")).toBe(0);
    expect(listenerCount("ppp-scan-progress")).toBe(0);
  });
});

describe("log line buffering", () => {
  it("routes raw PEA output to the console channel", async () => {
    await mounted();

    act(() => emit("pea-line", "epoch 1 solved"));
    await act(flushFrame);

    expect(store().consoleLines).toEqual(["epoch 1 solved"]);
    expect(store().workflowLines).toEqual([]);
  });

  it("routes low-level backend detail to the console channel too", async () => {
    await mounted();

    act(() => emit("console-line", "running: pea --config x.yaml"));
    await act(flushFrame);

    expect(store().consoleLines).toEqual(["running: pea --config x.yaml"]);
  });

  it("routes product downloads to the workflow channel", async () => {
    await mounted();

    act(() => emit("product-line", "Downloading IGS0OPSFIN.SP3.gz"));
    await act(flushFrame);

    expect(store().workflowLines).toEqual(["Downloading IGS0OPSFIN.SP3.gz"]);
    expect(store().consoleLines).toEqual([]);
  });

  it("batches a burst of lines into one store update", async () => {
    await mounted();

    act(() => {
      for (let i = 0; i < 200; i++) emit("pea-line", `line ${i}`);
    });
    // Nothing lands until the frame flushes — that buffering is the whole point.
    expect(store().consoleLines).toEqual([]);

    await act(flushFrame);

    expect(store().consoleLines).toHaveLength(200);
    expect(store().consoleLines[199]).toBe("line 199");
  });

  it("keeps the two channels' buffers apart", async () => {
    await mounted();

    act(() => {
      emit("pea-line", "console one");
      emit("product-line", "workflow one");
      emit("pea-line", "console two");
    });
    await act(flushFrame);

    expect(store().consoleLines).toEqual(["console one", "console two"]);
    expect(store().workflowLines).toEqual(["workflow one"]);
  });
});

describe("run completion", () => {
  it("flushes the buffer, ends the run, logs completion to Workflow and the exit code to Console", async () => {
    await mounted();

    act(() => {
      emit("pea-line", "last line before exit");
      emit("pea-finished", 0);
    });

    expect(store().isProcessing).toBe(false);
    expect(store().status).toBe("Done - exit code 0");
    // The pending line was flushed before the completion marker, so ordering holds.
    expect(store().consoleLines[0]).toBe("last line before exit");
    // Workflow gets the high-level step; the exit code is Console detail only.
    expect(store().workflowLines[0]).toBe(WF.peaCompleted());
    expect(store().workflowLines.join("\n")).not.toMatch(/exit code/);

    await act(flushFrame);
    const console_ = store().consoleLines;
    expect(console_[console_.length - 1]).toContain("PEA finished with exit code 0");
  });

  it("reports a non-zero exit code as a failed step", async () => {
    await mounted();
    act(() => emit("pea-finished", 3));
    expect(store().status).toBe("Done - exit code 3");
    expect(store().workflowLines[0]).toBe(WF.peaFailed("exited with code 3"));
  });

  it("reports a crash on both channels", async () => {
    await mounted();

    act(() => emit("pea-error", "process killed"));

    expect(store().isProcessing).toBe(false);
    expect(store().status).toBe("Error: process killed");
    expect(store().workflowLines[0]).toBe(WF.peaFailed("process killed"));

    await act(flushFrame);
    const console_ = store().consoleLines;
    expect(console_[console_.length - 1]).toContain("PEA error: process killed");
  });

  it("settles a Stop calmly, with no failure step", async () => {
    await mounted();
    store().setIsProcessing(true);

    act(() => emit("pea-stopped", null));

    expect(store().isProcessing).toBe(false);
    expect(store().status).toBe("Stopped");
    // handleStop logs the Stop steps itself; nothing here reads as a failure.
    expect(store().workflowLines.join("\n")).not.toMatch(/failed|error/i);
    expect(store().posEpoch).toBe(1);

    await act(flushFrame);
    const console_ = store().consoleLines;
    expect(console_[console_.length - 1]).toContain("PEA stopped by you");
  });

  it("nudges the visualization tab to re-list output after a run, errored or not", async () => {
    await mounted();

    act(() => emit("pea-finished", 0));
    expect(store().posEpoch).toBe(1);

    act(() => emit("pea-error", "boom"));
    expect(store().posEpoch).toBe(2);
  });
});

describe("progress events", () => {
  it("shows download progress in the status line", async () => {
    await mounted();
    act(() => emit("product-progress", { filename: "IGS0OPSFIN.SP3.gz", percent: 42 }));
    expect(store().status).toBe("Downloading IGS0OPSFIN.SP3.gz · 42%");
  });

  it("gives the percentage to whichever activity step is running", async () => {
    await mounted();
    act(() => {
      store().startActivity("Opening", [
        { key: "prepare", label: "Prepare" },
        { key: "bia", label: "Bias table" },
      ]);
      store().beginActivityStep("bia");
      emit("product-progress", { filename: "IGS.BIA.gz", percent: 70 });
    });
    expect(store().activity!.steps[1]).toMatchObject({
      detail: "Downloading IGS.BIA.gz",
      percent: 70,
    });
  });

  it("feeds unpack progress to the prepare step", async () => {
    await mounted();
    act(() => {
      store().startActivity("Opening", [{ key: "prepare", label: "Prepare" }]);
      store().beginActivityStep("prepare");
      emit("prepare-progress", "Hatanaka decoding...");
    });
    expect(store().status).toBe("Hatanaka decoding...");
    expect(store().activity!.steps[0].detail).toBe("Hatanaka decoding...");
  });

  it("treats an empty prepare-progress payload as 'finished', not as a blank status", async () => {
    await mounted();
    store().setStatus("Preparing ALIC.crx.gz...");
    act(() => emit("prepare-progress", ""));
    expect(store().status).toBe("Preparing ALIC.crx.gz...");
  });

  it("feeds mirror-scan progress to the scan step", async () => {
    await mounted();
    act(() => {
      store().startActivity("Opening", [{ key: "scan", label: "Scan" }]);
      store().beginActivityStep("scan");
      emit("ppp-scan-progress", "Contacting CDDIS...");
    });
    expect(store().activity!.steps[0].detail).toBe("Contacting CDDIS...");
  });

  it("ignores an empty scan-progress payload", async () => {
    await mounted();
    store().setStatus("Scanning...");
    act(() => emit("ppp-scan-progress", ""));
    expect(store().status).toBe("Scanning...");
  });
});

describe("connection faults", () => {
  it("turns a backend fault into an actionable banner message", async () => {
    await mounted();
    act(() => emit("connection-fault", { host: "cddis.nasa.gov", message: "timeout" }));
    expect(store().connectionFault).toBe(
      "Cannot reach cddis.nasa.gov. Check your internet connection.",
    );
  });
});
