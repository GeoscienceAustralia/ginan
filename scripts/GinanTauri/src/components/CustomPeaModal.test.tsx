import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import CustomPeaModal from "./CustomPeaModal";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { lastInvokeArgs, mockInvoke, readStore } from "../test/tauriMocks";

const RELEASES = [
  { tag: "v4.2.0", asset: "ginan-linux-x64.zip" },
  { tag: "v4.1.0", asset: "ginan-linux-x64.zip" },
];

beforeEach(() => {
  resetAppStore();
  useAppStore.setState({
    peaVersion: null,
    peaBinaryPath: "/usr/local/bin/pea",
    customPeaModalOpen: true,
  });
  mockInvoke({ list_pea_versions: RELEASES });
});

const dialog = () => screen.queryByRole("dialog", { name: "Custom PEA Binary" });

describe("CustomPeaModal", () => {
  it("stays hidden while closed", () => {
    useAppStore.setState({ customPeaModalOpen: false });
    render(<CustomPeaModal />);
    expect(dialog()).not.toBeInTheDocument();
  });

  it("stays hidden for the managed release", () => {
    useAppStore.setState({ peaVersion: "v4.2.0" });
    render(<CustomPeaModal />);
    expect(dialog()).not.toBeInTheDocument();
  });

  it("shows the path in use and offers the latest release", async () => {
    render(<CustomPeaModal />);
    expect(screen.getByText("/usr/local/bin/pea")).toBeInTheDocument();
    expect(await screen.findByRole("button", { name: "Switch to v4.2.0" })).toBeEnabled();
  });

  it("remembers the path when kept, and leaves the binary alone", async () => {
    render(<CustomPeaModal />);
    await userEvent.click(screen.getByRole("button", { name: "Keep this binary" }));

    expect(store().customPeaModalOpen).toBe(false);
    expect(store().customPeaNoticeSeen).toBe("/usr/local/bin/pea");
    expect(store().peaBinaryPath).toBe("/usr/local/bin/pea");
    await waitFor(() => expect(readStore("customPeaNoticeSeen")).toBe("/usr/local/bin/pea"));
  });

  it("switches to the latest managed release", async () => {
    mockInvoke({
      list_pea_versions: RELEASES,
      download_pea_version: {
        path: "/data/pea",
        version: "v4.2.0",
        source: "downloaded",
        update_available: null,
      },
    });
    render(<CustomPeaModal />);

    await userEvent.click(await screen.findByRole("button", { name: "Switch to v4.2.0" }));

    await waitFor(() => expect(store().customPeaModalOpen).toBe(false));
    expect(lastInvokeArgs("download_pea_version")).toEqual({ tag: "v4.2.0" });
    expect(store()).toMatchObject({
      peaBinaryPath: "/data/pea",
      peaVersion: "v4.2.0",
      status: "pea v4.2.0 ready",
    });
  });

  it("keeps the switch disabled when releases cannot be listed", async () => {
    mockInvoke({ list_pea_versions: () => Promise.reject("offline") });
    render(<CustomPeaModal />);

    expect(await screen.findByText("Could not check for releases: offline")).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Switch to latest" })).toBeDisabled();
    expect(screen.getByRole("button", { name: "Keep this binary" })).toBeEnabled();
  });

  it("stays open and shows the error when the download fails", async () => {
    mockInvoke({
      list_pea_versions: RELEASES,
      download_pea_version: () => Promise.reject("checksum mismatch"),
    });
    render(<CustomPeaModal />);

    await userEvent.click(await screen.findByRole("button", { name: "Switch to v4.2.0" }));

    expect(await screen.findByText("checksum mismatch")).toBeInTheDocument();
    expect(dialog()).toBeInTheDocument();
    expect(store().peaBinaryPath).toBe("/usr/local/bin/pea");
  });
});
