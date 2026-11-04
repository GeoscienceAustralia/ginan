import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ConnectionWarning from "./ConnectionWarning";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
});

describe("ConnectionWarning", () => {
  it("renders nothing when online with no fault", () => {
    const { container } = render(<ConnectionWarning />);
    expect(container).toBeEmptyDOMElement();
  });

  it("shows an explicit connection fault", () => {
    store().setConnectionFault("Cannot reach cddis.nasa.gov. Check your internet connection.");
    render(<ConnectionWarning />);
    expect(
      screen.getByText(/Cannot reach cddis\.nasa\.gov/),
    ).toBeInTheDocument();
  });

  it("dismisses the fault without touching offline mode", async () => {
    store().setConnectionFault("Cannot reach cddis.nasa.gov.");
    store().setOfflineMode(true);
    render(<ConnectionWarning />);

    await userEvent.click(screen.getByTitle("Dismiss"));

    expect(store().connectionFault).toBeNull();
    expect(store().offlineMode).toBe(true);
  });

  it("falls back to the standing offline notice once the fault is cleared", () => {
    store().setOfflineMode(true);
    render(<ConnectionWarning />);
    expect(screen.getByText(/No Earthdata credentials/)).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Set up account" })).toBeInTheDocument();
  });

  it("lets a transient fault outrank the standing offline notice", () => {
    store().setOfflineMode(true);
    store().setConnectionFault("Cannot reach cddis.nasa.gov.");
    render(<ConnectionWarning />);
    expect(screen.queryByText(/No Earthdata credentials/)).not.toBeInTheDocument();
  });

  it("opens the credentials modal from the offline notice", async () => {
    store().setOfflineMode(true);
    render(<ConnectionWarning />);

    await userEvent.click(screen.getByRole("button", { name: "Set up account" }));

    expect(useAppStore.getState().credentialsModalOpen).toBe(true);
  });
});

describe("ConnectionWarning when CDDIS is unreachable", () => {
  const unreachable = () =>
    store().setCddisStatus("error", "CDDIS unreachable: connection timed out");

  it("blames the server, not the account, while the machine is online", () => {
    vi.spyOn(navigator, "onLine", "get").mockReturnValue(true);
    unreachable();
    store().setOfflineMode(true);
    render(<ConnectionWarning />);

    expect(screen.getByText(/Can't reach the CDDIS server/)).toBeInTheDocument();
    expect(screen.queryByText(/No Earthdata credentials/)).not.toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Try now" })).toBeEnabled();
  });

  it("says there is no internet when the machine is offline", () => {
    vi.spyOn(navigator, "onLine", "get").mockReturnValue(false);
    store().setCddisStatus("error", "anything");
    render(<ConnectionWarning />);

    expect(screen.getByText(/No internet connection/)).toBeInTheDocument();
  });

  it("leaves a non-network CDDIS error to the credentials notice", () => {
    vi.spyOn(navigator, "onLine", "get").mockReturnValue(true);
    store().setCddisStatus("error", "no netrc entry for urs.earthdata.nasa.gov");
    store().setOfflineMode(true);
    render(<ConnectionWarning />);

    expect(screen.queryByText(/Can't reach the CDDIS server/)).not.toBeInTheDocument();
    expect(screen.getByText(/No Earthdata credentials/)).toBeInTheDocument();
  });

  it("checks again on demand and re-runs the product chain when it is back", async () => {
    vi.spyOn(navigator, "onLine", "get").mockReturnValue(true);
    mockInvoke({ check_cddis_connectivity: "ok" });
    unreachable();
    const epoch = store().cddisEpoch;
    render(<ConnectionWarning />);

    await userEvent.click(screen.getByRole("button", { name: "Try now" }));

    expect(invokeCount("check_cddis_connectivity")).toBe(1);
    expect(store().cddisEpoch).toBe(epoch + 1);
  });

  it("shows the check in progress and blocks a second click", async () => {
    vi.spyOn(navigator, "onLine", "get").mockReturnValue(true);
    let resolve!: () => void;
    mockInvoke({ check_cddis_connectivity: () => new Promise<void>((r) => (resolve = r)) });
    unreachable();
    render(<ConnectionWarning />);

    await userEvent.click(screen.getByRole("button", { name: "Try now" }));

    expect(screen.getByRole("button", { name: "Checking..." })).toBeDisabled();
    await act(async () => resolve());
    expect(screen.getByRole("button", { name: "Try now" })).toBeEnabled();
  });
});
