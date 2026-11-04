import { beforeEach, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import CddisSignOut from "./CddisSignOut";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  store().setCddisStatus("ok", null);
});

it("removes Earthdata credentials and invalidates pending checks", async () => {
  mockInvoke({ forget_cddis_credentials: null });
  render(<CddisSignOut />);
  await userEvent.click(screen.getByRole("button", { name: "Sign out of Earthdata" }));
  await waitFor(() => expect(store().offlineMode).toBe(true));
  expect(store().cddisStatus).toBe("error");
  expect(store().cddisEpoch).toBe(1);
  expect(invokeCount("forget_cddis_credentials")).toBe(1);
  expect(invokeCount("forget_ntrip_credentials")).toBe(0);
});

it("preserves the connection state and explains a failed removal", async () => {
  mockInvoke({ forget_cddis_credentials: () => Promise.reject("File is read-only") });
  render(<CddisSignOut />);
  await userEvent.click(screen.getByRole("button", { name: "Sign out of Earthdata" }));
  expect(await screen.findByRole("alert")).toHaveTextContent("File is read-only");
  expect(store().offlineMode).toBe(false);
  expect(store().cddisStatus).toBe("ok");
  expect(store().cddisEpoch).toBe(0);
});
