import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, within } from "@testing-library/react";
import type { StreamsModel } from "../../types/streams";
import {
  fileModel, idleModel, sampleModel, stoppedModel, testFailedModel, testRunningModel,
  waitingDecodedOffModel, waitingModel,
} from "./__fixtures__/model";

let current: StreamsModel = sampleModel;
vi.mock("../../hooks/useStreamsModel", () => ({ useStreamsModel: () => current }));
vi.mock("../../hooks/useTauriEvents", () => ({ pullLiveStreamStats: vi.fn() }));
vi.mock("./StreamDetail", () => ({
  default: (p: { stream: { mount: string }; sub: string; onBack(): void }) => (
    <div>detail {p.stream.mount} {p.sub}<button onClick={p.onBack}>back</button></div>
  ),
}));
vi.mock("./MessageDrawer", () => ({ default: () => null }));

import StreamsTab from "./StreamsTab";

const show = (m: StreamsModel, onNavigate = vi.fn()) => {
  current = m;
  return { onNavigate, ...render(<StreamsTab onNavigate={onNavigate} />) };
};

beforeEach(() => {
  current = sampleModel;
  vi.restoreAllMocks();
});

describe("StreamsTab phases", () => {
  it("live: KPIs, one card per mount, mix, test card", () => {
    show(sampleModel);
    expect(screen.getByRole("heading", { level: 1, name: "Streams" })).toBeInTheDocument();
    expect(screen.getByText(/LIVE/)).toHaveTextContent("00:14:3");
    expect(screen.getAllByRole("article")).toHaveLength(3);
    expect(screen.getByRole("heading", { name: "ALIC00AUS0" })).toBeInTheDocument();
    expect(screen.getByText("Reconnecting")).toBeInTheDocument();
    expect(screen.getByText("attempt 3")).toBeInTheDocument();
    expect(screen.getByRole("img", { name: /Decoded messages in the last 60 seconds/ })).toBeInTheDocument();
    expect(screen.getByText("Passed")).toBeInTheDocument();
    expect(screen.getByText("2.48")).toBeInTheDocument();
  });

  it("stopped: frozen banner, reset disabled", () => {
    show(stoppedModel);
    expect(screen.getByText(/Values are frozen/)).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /Reset counters/ })).toBeDisabled();
  });

  it("file mode: empty state, test disabled, CTA navigates", () => {
    const { onNavigate } = show(fileModel);
    expect(screen.getByRole("heading", { name: "No streams to monitor" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Monitor stream" })).toBeDisabled();
    fireEvent.click(screen.getByRole("button", { name: "See satellites in solution" }));
    expect(onNavigate).toHaveBeenCalledWith("visualization");
  });

  it("idle: points at Process, with no detour to Workflow", () => {
    show(idleModel);
    expect(screen.getByRole("heading", { name: "No live run yet" })).toBeInTheDocument();
    expect(screen.getByText(/Press Process to start a real-time run/)).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: "Go to Workflow" })).not.toBeInTheDocument();
  });

  it("idle: Monitor stream runs the test", () => {
    const run = vi.fn();
    show({ ...idleModel, runStreamTest: run });
    fireEvent.click(screen.getByRole("button", { name: "Monitor stream" }));
    expect(run).toHaveBeenCalled();
  });

  it("waiting: skeleton cards, decoded-off banner only when off", () => {
    const { unmount } = show(waitingModel);
    expect(screen.getAllByText("Connecting...", { selector: ".stream-status" })).toHaveLength(2);
    expect(screen.queryByText(/Decoded records are off/)).not.toBeInTheDocument();
    unmount();
    show(waitingDecodedOffModel);
    expect(screen.getByText(/Decoded records are off/)).toBeInTheDocument();
  });

  it("test running: busy button and progress bar", () => {
    show(testRunningModel);
    expect(screen.getByRole("progressbar", { name: "Stream test progress" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /Monitoring/ })).toHaveAttribute("aria-busy", "true");
  });

  it("test failed: alert with per-mount reasons and Test again", () => {
    show(testFailedModel);
    const alert = screen.getByRole("alert");
    expect(within(alert).getByText("401 Unauthorized")).toBeInTheDocument();
    expect(within(alert).getByText(/Check the caster credentials/)).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Monitor again" })).toBeInTheDocument();
  });
});

describe("StreamsTab interactions", () => {
  it("card buttons route to the stream detail and back", () => {
    show(sampleModel);
    fireEvent.click(screen.getByRole("button", { name: /^SSRA00CNE0 satellites/ }));
    expect(screen.getByText("detail SSRA00CNE0 satellites")).toBeInTheDocument();
    fireEvent.click(screen.getByRole("button", { name: "back" }));
    expect(screen.getAllByRole("article")).toHaveLength(3);
    fireEvent.click(screen.getByRole("button", { name: /^ALIC00AUS0 messages/ }));
    expect(screen.getByText("detail ALIC00AUS0 messages")).toBeInTheDocument();
  });

  it("Reset counters calls the model and shows a toast", async () => {
    const reset = vi.fn().mockResolvedValue(undefined);
    show({ ...sampleModel, resetCounters: reset });
    fireEvent.click(screen.getByRole("button", { name: /Reset counters/ }));
    expect(reset).toHaveBeenCalledWith();
    expect(await screen.findByText(/Counters reset at/)).toBeInTheDocument();
  });
});
