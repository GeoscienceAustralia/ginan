import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import RealtimeConfigPanel from "./RealtimeConfigPanel";
import { resetAppStore, store } from "../test/storeHelpers";
import { mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  store().setInputMode("realtime");
  mockInvoke({ list_ntrip_hosts: [] });
});

const corrections = () => document.querySelector<HTMLElement>('[data-tour="rt-nav"]')!;
/** The group headed `heading`, or null when it is not drawn. */
const group = (heading: string) =>
  [...corrections().querySelectorAll<HTMLElement>(".rt-corr-group")].find((g) =>
    g.querySelector(".input-section-title")?.textContent?.startsWith(heading),
  ) ?? null;
/** A one-row source shows its mountpoint as the value of its field. */
const rowFor = (mount: string) => within(corrections()).getByDisplayValue(mount).closest<HTMLElement>(".rt-stream-block")!;
const inGroup = (heading: string, mount: string) => within(group(heading)!).queryByDisplayValue(mount);
const menuItems = async (heading: string) => {
  await userEvent.click(within(group(heading)!).getByRole("button", { name: `Add to ${heading}` }));
  return screen.getAllByRole("menuitem").map((m) => m.textContent);
};

describe("Corrections groups", () => {
  it("files the shipped streams under ephemeris and SSR by name, with no control to choose", () => {
    render(<RealtimeConfigPanel />);
    expect(inGroup("Broadcast ephemeris", "BCEP00BKG0")).toBeInTheDocument();
    expect(inGroup("SSR corrections", "SSRA00CNE0")).toBeInTheDocument();
    expect(within(corrections()).queryByRole("combobox", { name: /Carries/ })).toBeNull();
    expect(group("Not identified yet")).toBeNull();
  });

  it("adds from each group's own plus; only the SSR group offers SBAS", async () => {
    render(<RealtimeConfigPanel />);
    expect(within(corrections()).queryByRole("button", { name: "Add to Corrections" })).toBeNull();
    expect(await menuItems("Broadcast ephemeris")).toEqual(["HTTP", "NTRIP", "Serial"]);
    await userEvent.keyboard("{Escape}");
    expect(await menuItems("SSR corrections")).toEqual(["HTTP", "NTRIP", "Serial", "SBAS"]);
  });

  it("puts a stream added from a group's plus in that group until its name says otherwise", async () => {
    render(<RealtimeConfigPanel />);
    await userEvent.click(within(group("Broadcast ephemeris")!).getByRole("button", { name: "Add to Broadcast ephemeris" }));
    await userEvent.click(screen.getByRole("menuitem", { name: "NTRIP" }));
    const added = store().inputProducts.corr.slice(-1)[0];
    expect(added).toMatchObject({ type: "ntrip", role: "bce" });
    expect(group("Broadcast ephemeris")!.querySelectorAll(".rt-stream-block")).toHaveLength(2);

    // An SSR mountpoint typed into it is filed under SSR automatically.
    store().updateProduct("corr", added.id, { value: "SSRC00BKG0" });
    expect(await within(group("SSR corrections")!).findByDisplayValue("SSRC00BKG0")).toBeInTheDocument();
  });

  it("offers to add broadcast ephemeris when there is none", async () => {
    render(<RealtimeConfigPanel />);
    await userEvent.click(within(rowFor("BCEP00BKG0")).getByTitle("Remove this stream"));
    await userEvent.click(screen.getByRole("button", { name: /Add a broadcast ephemeris stream/ }));
    expect(store().inputProducts.corr.slice(-1)[0]).toMatchObject({ type: "ntrip", role: "bce" });
  });

  it("offers SSR or SBAS when neither is there", async () => {
    render(<RealtimeConfigPanel />);
    await userEvent.click(within(rowFor("SSRA00CNE0")).getByTitle("Remove this stream"));
    expect(screen.getByRole("button", { name: /Add an SSR stream/ })).toBeInTheDocument();
    await userEvent.click(screen.getByRole("button", { name: /Use SBAS/ }));
    expect(store().realtime.sbas).toEqual({ mode: "PVS" });
  });

  it("holds apart a stream nothing identifies", () => {
    store().addProduct("corr", "ntrip", "MYCORR");
    render(<RealtimeConfigPanel />);
    expect(inGroup("Not identified yet", "MYCORR")).toBeInTheDocument();
  });

  it("shows SSR streams as paused while SBAS is on", () => {
    store().addProduct("corr", "sbas");
    render(<RealtimeConfigPanel />);
    expect(rowFor("SSRA00CNE0")).toHaveClass("is-paused");
    expect(within(rowFor("SSRA00CNE0")).getByText("paused while SBAS is on")).toBeInTheDocument();
    expect(rowFor("BCEP00BKG0")).not.toHaveClass("is-paused");
  });
});
