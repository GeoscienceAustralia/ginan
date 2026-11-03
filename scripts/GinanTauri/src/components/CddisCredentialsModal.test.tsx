import { beforeEach, describe, expect, it } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import CddisCredentialsModal from "./CddisCredentialsModal";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  store().setCredentialsModalOpen(true);
});

// The two `<label>`s carry no `htmlFor` and do not wrap their input, so neither
// field has an accessible name — they are reached through their autocomplete
// hint instead, which is the only stable handle the markup offers.
const field = (autocomplete: string) =>
  document.querySelector(`input[autocomplete="${autocomplete}"]`) as HTMLInputElement;
const username = () => field("username");
const password = () => field("current-password");
const saveBtn = () => screen.getByRole("button", { name: /Save & Test|Verifying/ });

async function fillIn(user = "alice", pass = "s3cr3t") {
  await userEvent.type(username(), user);
  await userEvent.type(password(), pass);
}

describe("visibility", () => {
  it("renders nothing while closed", () => {
    store().setCredentialsModalOpen(false);
    const { container } = render(<CddisCredentialsModal />);
    expect(container).toBeEmptyDOMElement();
  });

  it("is a labelled modal dialog", () => {
    render(<CddisCredentialsModal />);
    const dialog = screen.getByRole("dialog", { name: "CDDIS Credentials" });
    expect(dialog).toHaveAttribute("aria-modal", "true");
  });

  it("spells out the Earthdata URLs, since the webview has no address bar", () => {
    render(<CddisCredentialsModal />);
    expect(screen.getByText("urs.earthdata.nasa.gov")).toHaveAttribute(
      "href",
      "https://urs.earthdata.nasa.gov/",
    );
    expect(screen.getByText("urs.earthdata.nasa.gov/users/new")).toHaveAttribute(
      "href",
      "https://urs.earthdata.nasa.gov/users/new",
    );
  });
});

describe("the save gate", () => {
  it("stays disabled until both fields are filled", async () => {
    render(<CddisCredentialsModal />);
    expect(saveBtn()).toBeDisabled();

    await userEvent.type(username(), "alice");
    expect(saveBtn()).toBeDisabled();

    await userEvent.type(password(), "s3cr3t");
    expect(saveBtn()).toBeEnabled();
  });

  it("treats a whitespace-only username as empty", async () => {
    render(<CddisCredentialsModal />);
    await userEvent.type(username(), "   ");
    await userEvent.type(password(), "s3cr3t");
    expect(saveBtn()).toBeDisabled();
  });
});

describe("saving", () => {
  it("stores the credentials, verifies them, and comes back online", async () => {
    store().setOfflineMode(true);
    mockInvoke({ save_cddis_credentials: null, test_cddis_connection: null });
    render(<CddisCredentialsModal />);

    await fillIn();
    await userEvent.click(saveBtn());

    await waitFor(() => expect(store().credentialsModalOpen).toBe(false));
    expect(lastInvokeArgs("save_cddis_credentials")).toEqual({
      username: "alice",
      password: "s3cr3t",
    });
    expect(invokeCount("test_cddis_connection")).toBe(1);
    expect(store().offlineMode).toBe(false);
  });

  it("re-runs the startup chain so the app does not stay stuck offline", async () => {
    mockInvoke({ save_cddis_credentials: null, test_cddis_connection: null });
    render(<CddisCredentialsModal />);

    await fillIn();
    await userEvent.click(saveBtn());

    await waitFor(() => expect(store().cddisEpoch).toBe(1));
  });

  it("stays open with the reason when the credentials are rejected", async () => {
    store().setCddisStatus("ok", null);
    mockInvoke({
      save_cddis_credentials: null,
      test_cddis_connection: () => Promise.reject("401 Unauthorized"),
    });
    render(<CddisCredentialsModal />);

    await fillIn();
    await userEvent.click(saveBtn());

    await waitFor(() => expect(screen.getByText("401 Unauthorized")).toBeInTheDocument());
    expect(store().credentialsModalOpen).toBe(true);
    expect(store().cddisEpoch).toBe(1);
    expect(store().cddisStatus).toBe("error");
    expect(store().offlineMode).toBe(true);
    await userEvent.click(screen.getByRole("button", { name: "Cancel" }));
    expect(store().cddisStatus).toBe("error");
  });

  it("reports a failure to write netrc at all", async () => {
    mockInvoke({
      save_cddis_credentials: () => Promise.reject(new Error("netrc is read-only")),
    });
    render(<CddisCredentialsModal />);

    await fillIn();
    await userEvent.click(saveBtn());

    await waitFor(() => expect(screen.getByText(/netrc is read-only/)).toBeInTheDocument());
    expect(invokeCount("test_cddis_connection")).toBe(0);
  });

  it("locks the form while verifying", async () => {
    let finish!: () => void;
    mockInvoke({
      save_cddis_credentials: () => new Promise<void>((r) => (finish = r)),
    });
    render(<CddisCredentialsModal />);

    await fillIn();
    await userEvent.click(saveBtn());

    await waitFor(() => expect(screen.getByRole("button", { name: "Verifying..." })).toBeDisabled());
    expect(username()).toBeDisabled();
    expect(screen.getByRole("button", { name: "Cancel" })).toBeDisabled();

    finish();
  });
});

describe("cancelling", () => {
  it.each([false, true])("cancel preserves offline mode %s and connection state", async (offline) => {
    store().setOfflineMode(offline);
    store().setCddisStatus(offline ? "error" : "ok", offline ? "No saved login" : null);
    const previous = { status: store().cddisStatus, message: store().cddisStatusMessage };
    render(<CddisCredentialsModal />);

    await userEvent.click(screen.getByRole("button", { name: "Cancel" }));

    expect(store().credentialsModalOpen).toBe(false);
    expect(store().offlineMode).toBe(offline);
    expect(store().cddisStatus).toBe(previous.status);
    expect(store().cddisStatusMessage).toBe(previous.message);
    expect(store().cddisEpoch).toBe(0);
  });

  it("does not touch netrc", async () => {
    render(<CddisCredentialsModal />);

    await fillIn();
    await userEvent.click(screen.getByRole("button", { name: "Cancel" }));

    expect(invokeCount("save_cddis_credentials")).toBe(0);
    act(() => store().setCredentialsModalOpen(true));
    expect(username()).toHaveValue("");
    expect(password()).toHaveValue("");
  });
});
