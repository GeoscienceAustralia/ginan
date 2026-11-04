import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import PeaBinaryCards from "./PeaBinaryCards";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke, open } from "../test/tauriMocks";

const radio = (name: string) => screen.getByRole("radio", { name });
const card = (name: string) => radio(name).closest(".pea-card") as HTMLElement;

beforeEach(() => {
  resetAppStore();
});

describe("PeaBinaryCards", () => {
  it("switches back to the managed release when its card is clicked", async () => {
    useAppStore.setState({ peaBinaryPath: "/opt/pea", peaVersion: null });
    mockInvoke({
      list_pea_versions: [],
      use_managed_pea: { path: "/data/v4.2.0/pea", version: "v4.2.0", source: "existing", update_available: null },
    });
    render(<PeaBinaryCards />);

    await userEvent.click(card("Managed release"));

    await waitFor(() => expect(radio("Managed release")).toHaveAttribute("aria-checked", "true"));
    expect(store()).toMatchObject({ peaBinaryPath: "/data/v4.2.0/pea", peaVersion: "v4.2.0" });
  });

  it("switches back to the last custom binary without asking for a file", async () => {
    useAppStore.setState({ peaBinaryPath: "/data/pea", peaVersion: "v4.2.0", lastCustomPeaPath: "/opt/pea" });
    mockInvoke({ list_pea_versions: [], set_pea_binary: null });
    render(<PeaBinaryCards />);

    await userEvent.click(card("Custom binary"));

    await waitFor(() => expect(radio("Custom binary")).toHaveAttribute("aria-checked", "true"));
    expect(lastInvokeArgs("set_pea_binary")).toEqual({ path: "/opt/pea" });
    expect(open).not.toHaveBeenCalled();
    expect(store()).toMatchObject({ peaBinaryPath: "/opt/pea", peaVersion: null });
  });

  it("asks for a file when there is no custom binary to go back to", async () => {
    useAppStore.setState({ peaBinaryPath: "/data/pea", peaVersion: "v4.2.0" });
    open.mockResolvedValueOnce("/opt/pea");
    mockInvoke({ list_pea_versions: [], set_pea_binary: null });
    render(<PeaBinaryCards />);

    await userEvent.click(card("Custom binary"));

    await waitFor(() => expect(store().peaBinaryPath).toBe("/opt/pea"));
    expect(store().lastCustomPeaPath).toBe("/opt/pea");
  });

  it("forgets a remembered custom binary that has gone, and asks for a file", async () => {
    useAppStore.setState({ peaBinaryPath: "/data/pea", peaVersion: "v4.2.0", lastCustomPeaPath: "/gone/pea" });
    open.mockResolvedValueOnce(null);
    mockInvoke({ list_pea_versions: [], set_pea_binary: () => Promise.reject("Not a file") });
    render(<PeaBinaryCards />);

    await userEvent.click(card("Custom binary"));

    await waitFor(() => expect(open).toHaveBeenCalled());
    expect(store().lastCustomPeaPath).toBeNull();
    expect(store().peaBinaryPath).toBe("/data/pea");
  });

  it("does nothing when the active card is clicked", async () => {
    useAppStore.setState({ peaBinaryPath: "/data/pea", peaVersion: "v4.2.0" });
    mockInvoke({ list_pea_versions: [] });
    render(<PeaBinaryCards />);

    await userEvent.click(card("Managed release"));

    expect(invokeCount("use_managed_pea")).toBe(0);
  });

  it("does not treat a click on a control inside the card as a switch", async () => {
    useAppStore.setState({ peaBinaryPath: "/data/pea", peaVersion: "v4.2.0", lastCustomPeaPath: "/opt/pea" });
    open.mockResolvedValueOnce(null);
    mockInvoke({ list_pea_versions: [], set_pea_binary: null });
    render(<PeaBinaryCards />);

    await userEvent.click(within(card("Custom binary")).getByRole("button", { name: "Choose file..." }));

    expect(open).toHaveBeenCalledTimes(1);
    expect(invokeCount("set_pea_binary")).toBe(0);
  });

  it("switches with the keyboard", async () => {
    useAppStore.setState({ peaBinaryPath: "/opt/pea", peaVersion: null });
    mockInvoke({
      list_pea_versions: [],
      use_managed_pea: { path: "/data/v4.2.0/pea", version: "v4.2.0", source: "existing", update_available: null },
    });
    render(<PeaBinaryCards />);

    radio("Managed release").focus();
    await userEvent.keyboard("{Enter}");

    await waitFor(() => expect(store().peaVersion).toBe("v4.2.0"));
  });

  it("shows why a switch to the managed release failed", async () => {
    useAppStore.setState({ peaBinaryPath: "/opt/pea", peaVersion: null });
    mockInvoke({ list_pea_versions: [], use_managed_pea: () => Promise.reject("offline") });
    render(<PeaBinaryCards />);

    await userEvent.click(card("Managed release"));

    expect(await screen.findByRole("alert")).toHaveTextContent("offline");
    expect(store().peaBinaryPath).toBe("/opt/pea");
  });
});
