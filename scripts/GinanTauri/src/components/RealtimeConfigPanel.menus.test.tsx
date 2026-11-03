import { beforeEach, describe, expect, it } from "vitest";
import { fireEvent, render, screen, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import RealtimeConfigPanel from "./RealtimeConfigPanel";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
  store().setInputMode("realtime");
});

const navSection = () => document.querySelector<HTMLElement>('[data-tour="rt-nav"]')!;

describe("section plus menu", () => {
  it("adds a serial product from plus menu > Serial", async () => {
    render(<RealtimeConfigPanel />);

    await userEvent.click(within(navSection()).getByRole("button", { name: "Add to SSR corrections" }));
    await userEvent.click(screen.getByRole("menuitem", { name: "Serial" }));

    expect(store().realtime.nav_streams).toEqual(["BCEP00BKG0", "SSRA00CNE0", "serial://"]);
  });

  it("has no Use default item in the plus menu", async () => {
    render(<RealtimeConfigPanel />);

    await userEvent.click(within(navSection()).getByRole("button", { name: "Add to SSR corrections" }));

    expect(screen.queryByRole("menuitem", { name: /Use default/ })).toBeNull();
  });

  it("re-adds an empty NTRIP card after a removal", async () => {
    render(<RealtimeConfigPanel />);
    await userEvent.click(within(navSection()).getAllByTitle("Remove this stream")[0]);
    expect(store().realtime.nav_streams).toEqual(["SSRA00CNE0"]);

    await userEvent.click(within(navSection()).getByRole("button", { name: "Add to Broadcast ephemeris" }));
    await userEvent.click(screen.getByRole("menuitem", { name: "NTRIP" }));

    expect(store().realtime.nav_streams).toEqual(["SSRA00CNE0", ""]);
  });
});

describe("product bar context menu", () => {
  it("removes the right-clicked product", async () => {
    render(<RealtimeConfigPanel />);
    const bars = navSection().querySelectorAll(".rt-stream-block");

    fireEvent.contextMenu(bars[0]);
    await userEvent.click(screen.getByRole("menuitem", { name: "Remove product" }));

    expect(store().realtime.nav_streams).toEqual(["SSRA00CNE0"]);
    expect(screen.queryByRole("menu")).not.toBeInTheDocument();
  });

  it("closes on Escape and returns focus to the bar", async () => {
    render(<RealtimeConfigPanel />);
    const bar = navSection().querySelector<HTMLElement>(".rt-stream-block")!;

    fireEvent.contextMenu(bar);
    await userEvent.keyboard("{Escape}");

    expect(screen.queryByRole("menu")).not.toBeInTheDocument();
    expect(bar).toHaveFocus();
    expect(store().realtime.nav_streams).toHaveLength(2);
  });
});

describe("HTTP products", () => {
  const obsSection = () => document.querySelector<HTMLElement>('[data-tour="rt-obs"]')!;
  // Pick the HTTP card by its type label, so the lookup does not depend on the colour token.
  const httpCard = (section: HTMLElement) =>
    Array.from(section.querySelectorAll<HTMLElement>(".rt-stream-block")).find(
      (card) => card.querySelector(".rt-stream-type")?.textContent === "HTTP",
    )!;

  it("adds an http product from plus menu > HTTP in the observation section", async () => {
    render(<RealtimeConfigPanel />);

    await userEvent.click(within(obsSection()).getByRole("button", { name: "Add to Observation" }));
    expect(screen.getByRole("menuitem", { name: "HTTP" })).toBeEnabled();
    await userEvent.click(screen.getByRole("menuitem", { name: "HTTP" }));

    const products = store().inputProducts.obs;
    expect(products[products.length - 1]).toMatchObject({ type: "http" });
  });

  it("adds an http product from plus menu > HTTP in the corrections section", async () => {
    render(<RealtimeConfigPanel />);

    await userEvent.click(within(navSection()).getByRole("button", { name: "Add to SSR corrections" }));
    await userEvent.click(screen.getByRole("menuitem", { name: "HTTP" }));

    const products = store().inputProducts.corr;
    expect(products[products.length - 1]).toMatchObject({ type: "http" });
  });

  it("shows an http stream as one row with the full URL in its field", () => {
    store().addProduct("obs", "http", "https://ntrip.example.org/ALIC00AUS0");
    render(<RealtimeConfigPanel />);

    const card = httpCard(obsSection());
    expect(card).toHaveAttribute("data-type", "http");
    expect(card.querySelector(".rt-stream-type")).toHaveTextContent(/^HTTP$/);
    expect(card.querySelector("details")).toBeNull();
    expect(within(card).getByRole("textbox", { name: "URL" })).toHaveValue(
      "https://ntrip.example.org/ALIC00AUS0",
    );
  });

  it("gives an NTRIP card its own colour token, separate from the HTTP card", () => {
    store().addProduct("obs", "http", "https://ntrip.example.org/ALIC00AUS0");
    store().addProduct("obs", "ntrip", "ALIC00AUS0");
    render(<RealtimeConfigPanel />);

    const ntripCard = Array.from(obsSection().querySelectorAll<HTMLElement>(".rt-stream-block")).find(
      (card) => card.querySelector(".rt-stream-type")?.textContent === "NTRIP",
    )!;
    expect(ntripCard).toHaveAttribute("data-type", "ntrip");
    expect(httpCard(obsSection())).toHaveAttribute("data-type", "http");
  });

  it("masks the login in the title tooltip and keeps the password out of it", () => {
    store().addProduct("obs", "http", "https://u:secret@h.example/MNT");
    render(<RealtimeConfigPanel />);

    const title = httpCard(obsSection()).querySelector(".rt-stream-field")!;
    expect(title).toHaveAttribute("title", "https://***@h.example/MNT");
    expect(title.getAttribute("title")).not.toContain("secret");
  });
});
