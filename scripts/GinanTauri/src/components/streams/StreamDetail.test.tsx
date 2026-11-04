import { useState } from "react";
import { describe, expect, it, vi } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import StreamDetail, { type StreamSub } from "./StreamDetail";
import MessageDrawer from "./MessageDrawer";
import { ecefToGeodetic } from "./StationView";
import { sampleModel } from "./__fixtures__/model";
import type { SatelliteView, StreamsModel } from "../../types/streams";

const alic = sampleModel.streams[0];

const sat = (s: string, over: Partial<SatelliteView> = {}): SatelliteView => ({
  sat: s, constellation: "GPS", signals: ["1C"], cn0: 44, inSolution: true,
  postfitRms: 0.4, samples: 100, lastEpochMs: 0, dropped: false, ...over,
});

function Harness({ model = sampleModel, start = "messages" }: { model?: StreamsModel; start?: StreamSub }) {
  const [sub, setSub] = useState<StreamSub>(start);
  const [msg, setMsg] = useState<string | null>(null);
  const stream = model.streams[0];
  return (
    <>
      <StreamDetail stream={stream} model={model} sub={sub} onSubChange={setSub} onBack={() => {}} onOpenMessage={setMsg} />
      {msg && (
        <MessageDrawer stream={stream} msgNo={msg} order={stream.messages.map((m) => m.msgNo)} onNavigate={setMsg} onClose={() => setMsg(null)} />
      )}
    </>
  );
}

describe("StreamDetail", () => {
  it("shows the header and switches sub views with aria-current", async () => {
    const user = userEvent.setup();
    render(<Harness />);
    expect(screen.getByRole("heading", { level: 1, name: alic.mount })).toBeInTheDocument();
    const nav = screen.getByRole("navigation", { name: "Stream views" });
    expect(within(nav).getByRole("button", { name: /Messages/ })).toHaveAttribute("aria-current", "page");
    expect(screen.getByRole("table")).toBeInTheDocument();

    await user.click(within(nav).getByRole("button", { name: /Satellites/ }));
    expect(within(nav).getByRole("button", { name: /Satellites/ })).toHaveAttribute("aria-current", "page");
    expect(within(nav).getByRole("button", { name: /Messages/ })).not.toHaveAttribute("aria-current");

    await user.click(within(nav).getByRole("button", { name: "Station" }));
    expect(screen.getByText(/No station messages yet/)).toBeInTheDocument();
    expect(screen.getByText(/Recordings › RTCM on/)).toBeInTheDocument();
  });

  it("calls onBack", async () => {
    const onBack = vi.fn();
    render(<StreamDetail stream={alic} model={sampleModel} sub="messages" onSubChange={() => {}} onBack={onBack} onOpenMessage={() => {}} />);
    await userEvent.click(screen.getByRole("button", { name: /All streams/ }));
    expect(onBack).toHaveBeenCalled();
  });

  it("filters messages by number prefix or name, with a count and a no-match row", async () => {
    const user = userEvent.setup();
    render(<Harness />);
    expect(screen.getByText(/10 of 10 types/)).toBeInTheDocument();
    const box = screen.getByRole("searchbox", { name: "Search messages" });
    await user.type(box, "107");
    expect(screen.getByText(/1 of 10 types/)).toBeInTheDocument();
    expect(screen.getAllByRole("row")).toHaveLength(2);
    await user.clear(box);
    await user.type(box, "ephemeris");
    expect(screen.getByText(/3 of 10 types/)).toBeInTheDocument();
    await user.clear(box);
    await user.type(box, "zzz");
    expect(screen.getByText(/No messages match/)).toBeInTheDocument();
  });

  it("sorts by a column header and resets this stream", async () => {
    const user = userEvent.setup();
    const resetCounters = vi.fn().mockResolvedValue(undefined);
    render(<Harness model={{ ...sampleModel, resetCounters }} />);
    const first = () => screen.getAllByRole("row")[1];
    expect(first()).toHaveTextContent("1077");
    await user.click(screen.getByRole("button", { name: "Type" }));
    expect(screen.getByRole("columnheader", { name: /Type/ })).toHaveAttribute("aria-sort", "descending");
    expect(first()).toHaveTextContent("1230");
    await user.click(screen.getByRole("button", { name: "Reset this stream" }));
    expect(resetCounters).toHaveBeenCalledWith(alic.mount);
  });

  it("flags late messages", () => {
    render(<Harness />);
    expect(screen.getAllByRole("img", { name: "late" })).toHaveLength(2);
  });

  it("explains missing decoded records", () => {
    const stream = { ...alic, messages: [] };
    render(<StreamDetail stream={stream} model={{ ...sampleModel, recordDecodedOff: true }} sub="messages" onSubChange={() => {}} onBack={() => {}} onOpenMessage={() => {}} />);
    expect(screen.getByText(/Decoded records are off/)).toBeInTheDocument();
  });
});

describe("MessageDrawer", () => {
  it("opens from a row, steps with prev/next, and closes with Esc, restoring focus", async () => {
    const user = userEvent.setup();
    render(<Harness />);
    const rowBtn = screen.getByRole("button", { name: "1097 Galileo MSM7" });
    await user.click(rowBtn);

    const dialog = screen.getByRole("dialog");
    expect(dialog).toHaveAttribute("aria-modal", "true");
    expect(within(dialog).getByRole("heading", { name: "1097" })).toBeInTheDocument();
    expect(within(dialog).getByText(/Message 2 of 10/)).toBeInTheDocument();
    expect(within(dialog).getByRole("button", { name: "Close (Esc)" })).toHaveFocus();

    await user.click(within(dialog).getByRole("button", { name: /^1087/ }));
    expect(screen.getByRole("heading", { name: "1087" })).toBeInTheDocument();
    await user.click(screen.getByRole("button", { name: /^‹ 1097/ }));
    expect(screen.getByRole("heading", { name: "1097" })).toBeInTheDocument();

    await user.keyboard("{Escape}");
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
    expect(rowBtn).toHaveFocus();
  });

  it("has no previous at the start and no next at the end; overlay and close button close", async () => {
    const user = userEvent.setup();
    render(<Harness />);
    await user.click(screen.getByRole("button", { name: "1077 GPS MSM7" }));
    expect(screen.getByRole("button", { name: "No previous" })).toBeDisabled();
    await user.click(screen.getByRole("button", { name: "Close (Esc)" }));
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();

    await user.click(screen.getByRole("button", { name: "1042 BeiDou ephemeris" }));
    expect(screen.getByRole("button", { name: "No next" })).toBeDisabled();
    await user.click(screen.getByRole("dialog").parentElement!);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("traps Tab inside the dialog", async () => {
    const user = userEvent.setup();
    render(<Harness />);
    await user.click(screen.getByRole("button", { name: "1077 GPS MSM7" }));
    const dialog = screen.getByRole("dialog");
    // close, [no previous: disabled], next => two tabbable controls
    await user.tab();
    await user.tab();
    expect(dialog).toContainElement(document.activeElement as HTMLElement);
    await user.tab({ shift: true });
    await user.tab({ shift: true });
    await user.tab({ shift: true });
    expect(dialog).toContainElement(document.activeElement as HTMLElement);
  });

  it("charts arrivals and lists PRNs", async () => {
    const m = { ...alic.messages[0], periodicityS: 1, interArrivalS: [1, 1, 1.4, 1], lastSats: ["G05", "G07"], lastSignals: ["1C", "2W"] };
    const stream = { ...alic, messages: [m] };
    render(<MessageDrawer stream={stream} msgNo="1077" order={["1077"]} onNavigate={() => {}} onClose={() => {}} />);
    expect(screen.getByRole("img", { name: /Last 4 arrival intervals.*1 late/ })).toBeInTheDocument();
    expect(screen.getByText("G07")).toBeInTheDocument();
    expect(screen.getByText("2W")).toBeInTheDocument();
  });
});

describe("SatellitesView", () => {
  const satModel: StreamsModel = {
    ...sampleModel,
    satelliteStatus: { available: true } as StreamsModel["satelliteStatus"],
    satellitesFor: () => [
      sat("G05"), sat("G15", { cn0: 30, dropped: true, inSolution: false }), sat("E11", { constellation: "GAL" }),
    ],
  };

  it("filters by constellation, PRN and dropped, and disables untracked constellations", async () => {
    const user = userEvent.setup();
    render(<Harness model={satModel} start="satellites" />);
    expect(screen.getAllByRole("row")).toHaveLength(4);
    expect(screen.getByRole("radio", { name: /QZSS/ })).toBeDisabled();
    expect(screen.getAllByText("Not tracked by this stream", { exact: false }).length).toBeGreaterThan(0);

    await user.click(screen.getByRole("radio", { name: /Galileo/ }));
    expect(screen.getAllByRole("row")).toHaveLength(2);
    await user.click(screen.getByRole("radio", { name: /^All/ }));

    await user.type(screen.getByRole("searchbox", { name: "Search satellites" }), "g5");
    expect(screen.getAllByRole("row")).toHaveLength(2);
    expect(screen.getByText("G05")).toBeInTheDocument();
    await user.clear(screen.getByRole("searchbox"));

    await user.click(screen.getByRole("checkbox", { name: "Dropped only" }));
    expect(screen.getAllByRole("row")).toHaveLength(2);
    expect(screen.getByText("Dropped")).toBeInTheDocument();
    expect(screen.getByText("weak")).toBeInTheDocument();
  });

  it("says when no .TRACE is available", () => {
    const model = { ...sampleModel, satelliteStatus: { available: false, message: "No .TRACE file found." } as StreamsModel["satelliteStatus"] };
    render(<Harness model={model} start="satellites" />);
    expect(screen.getByRole("status")).toHaveTextContent("No .TRACE file found.");
  });
});

describe("ecefToGeodetic", () => {
  it("matches the Alice Springs reference position", () => {
    const g = ecefToGeodetic(-4052052.73, 4212835.99, -2545104.59);
    expect(g.lat).toBeCloseTo(-23.67012, 3);
    expect(g.lon).toBeCloseTo(133.88551, 3);
    expect(g.h).toBeGreaterThan(550);
    expect(g.h).toBeLessThan(650);
  });
});
