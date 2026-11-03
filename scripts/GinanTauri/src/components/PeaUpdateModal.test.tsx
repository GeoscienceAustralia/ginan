import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import PeaUpdateModal from "./PeaUpdateModal";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { lastInvokeArgs, mockInvoke, readStore } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  useAppStore.setState({
    peaVersion: "v4.1.0",
    peaUpdateAvailable: "v4.2.0",
    peaBinaryPath: "/bin/pea-4.1.0",
    peaUpdateModalOpen: true,
  });
});

const dialog = () => screen.queryByRole("dialog", { name: "PEA Binary Update" });

describe("PeaUpdateModal", () => {
  it("stays hidden while closed", () => {
    useAppStore.setState({ peaUpdateModalOpen: false });
    render(<PeaUpdateModal />);
    expect(dialog()).not.toBeInTheDocument();
  });

  it("stays hidden once there is no update to offer", () => {
    useAppStore.setState({ peaUpdateAvailable: null });
    render(<PeaUpdateModal />);
    expect(dialog()).not.toBeInTheDocument();
  });

  it("shows the installed and the available version", () => {
    render(<PeaUpdateModal />);
    expect(dialog()).toBeInTheDocument();
    expect(screen.getByText("v4.1.0")).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Update to v4.2.0" })).toBeInTheDocument();
  });

  it("calls an untagged install Unknown", () => {
    useAppStore.setState({ peaVersion: null });
    render(<PeaUpdateModal />);
    expect(screen.getByText("Unknown")).toBeInTheDocument();
  });

  it("leaves the current binary alone when dismissed", async () => {
    render(<PeaUpdateModal />);
    await userEvent.click(screen.getByRole("button", { name: "Not now" }));
    expect(store().peaUpdateModalOpen).toBe(false);
    expect(store().peaUpdateAvailable).toBe("v4.2.0");
    expect(store().peaBinaryPath).toBe("/bin/pea-4.1.0");
  });

  it("downloads the new release and switches to it", async () => {
    mockInvoke({
      download_pea_version: {
        path: "/bin/pea-4.2.0",
        version: "v4.2.0",
        source: "downloaded",
        update_available: null,
      },
    });
    render(<PeaUpdateModal />);

    await userEvent.click(screen.getByRole("button", { name: "Update to v4.2.0" }));

    await waitFor(() => expect(store().peaUpdateModalOpen).toBe(false));
    expect(lastInvokeArgs("download_pea_version")).toEqual({ tag: "v4.2.0" });
    expect(store()).toMatchObject({
      peaBinaryPath: "/bin/pea-4.2.0",
      peaVersion: "v4.2.0",
      peaUpdateAvailable: null,
      status: "pea v4.2.0 ready",
    });
    // The new binary is persisted so it survives a restart.
    await waitFor(() => expect(readStore("peaBinaryPath")).toBe("/bin/pea-4.2.0"));
  });

  it("locks the buttons while downloading", async () => {
    let finish!: (v: unknown) => void;
    mockInvoke({ download_pea_version: () => new Promise((r) => (finish = r)) });
    render(<PeaUpdateModal />);

    await userEvent.click(screen.getByRole("button", { name: "Update to v4.2.0" }));

    expect(screen.getByRole("button", { name: "Updating..." })).toBeDisabled();
    expect(screen.getByRole("button", { name: "Not now" })).toBeDisabled();
    finish({ path: "/bin/p", version: "v4.2.0", source: "downloaded", update_available: null });
    await waitFor(() => expect(dialog()).not.toBeInTheDocument());
  });

  it("stays open and shows the error when the download fails", async () => {
    mockInvoke({ download_pea_version: () => Promise.reject("checksum mismatch") });
    render(<PeaUpdateModal />);

    await userEvent.click(screen.getByRole("button", { name: "Update to v4.2.0" }));

    expect(await screen.findByText("checksum mismatch")).toBeInTheDocument();
    expect(dialog()).toBeInTheDocument();
    expect(store().peaBinaryPath).toBe("/bin/pea-4.1.0");
    expect(screen.getByRole("button", { name: "Update to v4.2.0" })).toBeEnabled();
  });
});
