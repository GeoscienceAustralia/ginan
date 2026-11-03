import { beforeEach, describe, expect, it } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import PeaVersionControl from "./PeaVersionControl";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke, type CommandStub } from "../test/tauriMocks";

const VERSIONS = [
  { tag: "v4.2.0", asset: "pea-linux-x64", downloaded: true },
  { tag: "v4.1.0", asset: "pea-linux-x64", downloaded: true },
  { tag: "v4.0.0", asset: "pea-linux-x64", downloaded: false },
];

beforeEach(() => {
  resetAppStore();
});

function mockBackend(over: Record<string, CommandStub> = {}) {
  mockInvoke({
    list_pea_versions: VERSIONS,
    download_pea_version: ({ tag }) => ({
      path: `/bin/pea-${tag}`,
      version: tag,
      source: "downloaded",
      update_available: null,
    }),
    ...over,
  });
}

const select = () => screen.getByRole("combobox", { name: "Available pea versions" });
const action = () =>
  screen.getByRole("button", { name: /Download & use|^Use$|In use|Downloading\.\.\.|Switching\.\.\./ });

describe("PeaVersionControl", () => {
  it("lists the releases once, newest first, and pre-selects the latest", async () => {
    mockBackend();
    render(<PeaVersionControl />);

    await waitFor(() => expect(select()).toBeEnabled());
    expect([...(select() as HTMLSelectElement).options].map((o) => o.value)).toEqual([
      "v4.2.0",
      "v4.1.0",
      "v4.0.0",
    ]);
    expect(select()).toHaveValue("v4.2.0");
    expect(invokeCount("list_pea_versions")).toBe(1);
  });

  it("pre-selects the installed release, not the latest", async () => {
    useAppStore.getState().setPeaVersion("v4.0.0");
    mockBackend();
    render(<PeaVersionControl />);

    await waitFor(() => expect(select()).toBeEnabled());
    expect(select()).toHaveValue("v4.0.0");
    expect(action()).toHaveAccessibleName("In use");
  });

  it("pre-selects the managed release while a custom binary is active", async () => {
    useAppStore.setState({ peaVersion: null, managedPeaVersion: "v4.1.0" });
    mockBackend();
    render(<PeaVersionControl />);

    await waitFor(() => expect(select()).toBeEnabled());
    expect(select()).toHaveValue("v4.1.0");
    expect(action()).toHaveAccessibleName("Use");
  });

  it("falls back to the latest when the installed release is not listed", async () => {
    useAppStore.getState().setPeaVersion("v3.9.0");
    mockBackend();
    render(<PeaVersionControl />);

    await waitFor(() => expect(select()).toBeEnabled());
    expect(select()).toHaveValue("v4.2.0");
  });

  it("follows the installed release when it changes elsewhere", async () => {
    useAppStore.getState().setPeaVersion("v4.2.0");
    mockBackend();
    render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());

    act(() => useAppStore.getState().setPeaVersion("v4.1.0"));
    expect(select()).toHaveValue("v4.1.0");
  });

  it("still shows the chosen release after Settings is reopened", async () => {
    useAppStore.getState().setPeaVersion("v4.2.0");
    mockBackend();
    const { unmount } = render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());

    await userEvent.selectOptions(select(), "v4.0.0");
    await userEvent.click(action());
    await waitFor(() => expect(store().peaVersion).toBe("v4.0.0"));
    unmount();

    render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());
    expect(select()).toHaveValue("v4.0.0");
  });

  it("marks the release in use and does not offer to re-download it", async () => {
    useAppStore.setState({ peaVersion: "v4.2.0" });
    mockBackend();
    render(<PeaVersionControl />);

    await waitFor(() => expect(select()).toBeEnabled());
    expect(screen.getByRole("option", { name: "v4.2.0 (in use)" })).toBeInTheDocument();
    expect(action()).toHaveAccessibleName("In use");
    expect(action()).toBeDisabled();
  });

  it("marks downloaded releases and offers to use them without a download", async () => {
    useAppStore.setState({ peaVersion: "v4.2.0" });
    mockBackend();
    render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());

    expect(screen.getByRole("option", { name: "v4.1.0 (downloaded)" })).toBeInTheDocument();
    expect(screen.getByRole("option", { name: "v4.0.0" })).toBeInTheDocument();
    await userEvent.selectOptions(select(), "v4.1.0");
    expect(action()).toHaveAccessibleName("Use");
    await userEvent.selectOptions(select(), "v4.0.0");
    expect(action()).toHaveAccessibleName("Download & use");
  });

  it("downloads the chosen release and makes it current", async () => {
    useAppStore.setState({ peaVersion: "v4.2.0", peaUpdateAvailable: "v4.3.0" });
    mockBackend();
    render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());

    await userEvent.selectOptions(select(), "v4.0.0");
    await userEvent.click(action());

    await waitFor(() => expect(store().peaVersion).toBe("v4.0.0"));
    expect(lastInvokeArgs("download_pea_version")).toEqual({ tag: "v4.0.0" });
    expect(store()).toMatchObject({
      peaBinaryPath: "/bin/pea-v4.0.0",
      peaUpdateAvailable: null,
      status: "pea v4.0.0 ready",
    });
    expect(action()).toHaveAccessibleName("In use");
    // Remembered as downloaded once fetched.
    expect(screen.getByRole("option", { name: "v4.0.0 (in use)" })).toBeInTheDocument();
  });

  it("keeps every label in the button, showing only the current one", async () => {
    useAppStore.setState({ peaVersion: "v4.2.0" });
    mockBackend();
    render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());

    // The hidden labels are what hold the button at the widest one's width.
    const labels = () => [...action().querySelectorAll(".pea-version-labels > span")];
    expect(labels().map((l) => l.textContent)).toEqual([
      "Download & use", "Downloading...", "Switching...", "In use", "Use",
    ]);
    expect(labels().filter((l) => !l.classList.contains("is-hidden")).map((l) => l.textContent)).toEqual(["In use"]);

    await userEvent.selectOptions(select(), "v4.1.0");
    expect(labels().map((l) => l.textContent)).toHaveLength(5);
    expect(action()).toHaveAccessibleName("Use");
  });

  it("reports a failed download", async () => {
    mockBackend({ download_pea_version: () => Promise.reject("network down") });
    render(<PeaVersionControl />);
    await waitFor(() => expect(select()).toBeEnabled());

    await userEvent.click(action());

    expect(await screen.findByRole("status")).toHaveTextContent("Failed");
    expect(screen.getByRole("status")).toHaveAttribute("title", "network down");
    expect(store().status).toBe("Could not download pea v4.2.0: network down");
    expect(store().peaBinaryPath).toBeNull();
  });

  it("reports a release list that could not be fetched", async () => {
    mockBackend({ list_pea_versions: () => Promise.reject("rate limited") });
    render(<PeaVersionControl />);

    expect(await screen.findByRole("status")).toHaveAttribute("title", "rate limited");
    expect(screen.getByRole("option", { name: "No versions available" })).toBeInTheDocument();
    expect(select()).toBeDisabled();
    expect(action()).toBeDisabled();
  });

  it("shows a loading placeholder until the list arrives", async () => {
    let resolve!: (v: unknown) => void;
    mockBackend({ list_pea_versions: () => new Promise((r) => (resolve = r)) });
    render(<PeaVersionControl />);

    expect(screen.getByRole("option", { name: "Loading..." })).toBeInTheDocument();
    expect(select()).toBeDisabled();

    resolve(VERSIONS);
    await waitFor(() => expect(select()).toBeEnabled());
  });
});
