import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import RealtimeConfigPanel from "./RealtimeConfigPanel";
import { resetAppStore, store } from "../test/storeHelpers";
import { mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  store().setInputMode("realtime");
});

const section = (tour: string) => document.querySelector<HTMLElement>(`[data-tour="${tour}"]`)!;
const openMenu = (tour: string, name: string) =>
  userEvent.click(within(section(tour)).getByRole("button", { name }));

describe("SBAS product", () => {
  it("adds once from Corrections, defaulting to PVS, and pauses the SSR stream it replaces", async () => {
    mockInvoke({ list_ntrip_hosts: [] });
    render(<RealtimeConfigPanel />);

    await openMenu("rt-nav", "Add to SSR corrections");
    await userEvent.click(screen.getByRole("menuitem", { name: "SBAS" }));

    expect(store().realtime.sbas).toEqual({ mode: "PVS" });
    expect(store().realtime.nav_streams).toEqual(["BCEP00BKG0"]);
    expect(within(section("rt-nav")).getByText("SouthPAN · PVS")).toBeInTheDocument();
    expect(within(section("rt-nav")).getByText("paused while SBAS is on")).toBeInTheDocument();

    // One at a time: the item is disabled once a card exists.
    await openMenu("rt-nav", "Add to SSR corrections");
    expect(screen.getByRole("menuitem", { name: "SBAS" })).toBeDisabled();
  });

  it("is not offered for observations", async () => {
    render(<RealtimeConfigPanel />);
    await openMenu("rt-obs", "Add to Observation");
    expect(screen.queryByRole("menuitem", { name: "SBAS" })).toBeNull();
  });

  it("sends the chosen mode, and nothing once the card is removed", async () => {
    mockInvoke({ list_ntrip_hosts: [] });
    store().addProduct("corr", "sbas");
    render(<RealtimeConfigPanel />);

    await userEvent.selectOptions(screen.getByRole("combobox", { name: "Mode" }), "DFMC");
    expect(store().realtime.sbas).toEqual({ mode: "DFMC" });

    const card = within(section("rt-nav")).getByRole("combobox", { name: "Mode" }).closest<HTMLElement>(".rt-stream-block")!;
    // One row like the other sources: the mode, no explanation text.
    expect(card).toHaveClass("rt-stream-flat");
    expect(card).not.toHaveTextContent(/paused while this card|Awarua|in place of SSR/);
    await userEvent.click(within(card).getByTitle("Remove this stream"));
    expect(store().realtime.sbas).toBeUndefined();
    // The paused SSR stream comes back.
    expect(store().realtime.nav_streams).toEqual(["BCEP00BKG0", "SSRA00CNE0"]);
  });

  it("has no login field: the SISNet login is on Settings › Credentials", () => {
    mockInvoke({ list_ntrip_hosts: [] });
    store().addProduct("corr", "sbas");
    render(<RealtimeConfigPanel />);
    expect(screen.queryByRole("button", { name: /Add login/ })).toBeNull();
    expect(screen.queryByPlaceholderText("Username")).toBeNull();
  });
});
