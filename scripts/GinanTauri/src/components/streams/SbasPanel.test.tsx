import { describe, expect, it } from "vitest";
import { render, screen, within } from "@testing-library/react";
import SbasPanel from "./SbasPanel";
import type { SbasProbe } from "../../types";
import type { LiveSbas, SbasView } from "../../types/streams";

const probe = (o: Partial<SbasProbe> = {}): SbasProbe => ({
  host: "sisnet.data.gnss.ga.gov.au", prn: 122, mode: "DFMC", state: "ok", error: null, seconds: 10, messages_per_s: 1,
  messages: 10, crc_errors: 1, invalid_lines: 0, by_type: { "32": 6, "34": 4 }, first_gpst_ms: 0, last_gpst_ms: 9000, ...o,
});
const live = (o: Partial<LiveSbas> = {}): LiveSbas => ({
  mode: "DFMC",
  console: { auth_failures: 0, start_failures: 0, invalid_lines: 0, crc_errors: 0, messages: 0, last_error: null, last_error_at_ms: null },
  levels: { file: "ALIC.SPP", points: [[1, 5.1, 8.2], [2, 5.3, 8.4]] },
  ...o,
});
const view = (o: Partial<SbasView> = {}): SbasView => ({ mode: "DFMC", probe: null, live: null, ...o });

describe("SbasPanel", () => {
  it("asks for a stream test before there is one", () => {
    render(<SbasPanel sbas={view()} />);
    expect(screen.getByText(/Press Monitor stream to log in to SISNet/)).toBeInTheDocument();
    expect(screen.getByText(/PRN 122 · DFMC/)).toBeInTheDocument();
  });

  it("shows what the stream test's SISNet check received, by message type", () => {
    render(<SbasPanel sbas={view({ probe: probe() })} />);
    const test = screen.getByLabelText("Last stream test");
    expect(within(test).getByText("Receiving")).toBeInTheDocument();
    expect(within(test).getByText("10 in 10 s")).toBeInTheDocument();
    const types = within(test).getByLabelText("Message types received");
    expect(within(types).getByText(/MT32/)).toHaveTextContent("MT32 Clock-ephemeris corrections×6");
  });

  it("says why the check failed", () => {
    render(<SbasPanel sbas={view({ probe: probe({ state: "auth_failed", error: "SISNet did not accept the saved login.", messages: 0 }) })} />);
    expect(screen.getByText("Login rejected")).toBeInTheDocument();
    expect(screen.getByText("SISNet did not accept the saved login.")).toBeInTheDocument();
  });

  it("follows the run: PEA's SISNet problems and the latest protection levels", () => {
    const l = live({ console: { ...live().console, crc_errors: 2, last_error: "A message failed its CRC check", last_error_at_ms: 1_000 } });
    render(<SbasPanel sbas={view({ live: l })} nowMs={11_000} />);
    const run = screen.getByLabelText("This run");
    expect(within(run).getByText("A message failed its CRC check, 10.0 s ago")).toBeInTheDocument();
    expect(within(run).getByText("5.30 m")).toBeInTheDocument();
    expect(within(run).getByText("8.40 m")).toBeInTheDocument();
    expect(within(run).getByRole("img", { name: /Horizontal protection level/ })).toBeInTheDocument();
  });

  it("explains why a PVS run has no protection levels", () => {
    render(<SbasPanel sbas={view({ mode: "PVS", live: live({ mode: "PVS", levels: null }) })} />);
    expect(screen.getByText(/PVS mode writes a .POS, not an .SPP/)).toBeInTheDocument();
  });
});
