import { describe, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import NtripCredentialsForm from "./NtripCredentialsForm";
import { invokeCount, lastInvokeArgs, mockInvoke, type CommandStub } from "../test/tauriMocks";

function mockBackend(over: Record<string, CommandStub> = {}) {
  let hosts = ["caster.example"];
  mockInvoke({
    list_ntrip_hosts: () => [...hosts],
    save_ntrip_credentials: ({ host }) => {
      hosts = [...hosts, host as string];
      return null;
    },
    forget_ntrip_credentials: ({ host }) => {
      hosts = hosts.filter((h) => h !== host);
      return null;
    },
    ...over,
  });
}

const host = () => screen.getByRole("textbox", { name: "Caster host" });
const username = () => screen.getByRole("textbox", { name: "Username" });
const password = () => screen.getByLabelText("Password");
const saveBtn = () => screen.getByRole("button", { name: /Save credentials|Saving\.\.\./ });

describe("NtripCredentialsForm", () => {
  it("defaults to the Geoscience Australia caster", async () => {
    mockBackend();
    render(<NtripCredentialsForm />);
    expect(host()).toHaveValue("ntrip.data.gnss.ga.gov.au");
    // Let the saved-hosts fetch land inside the test.
    await screen.findByText("caster.example");
  });

  it("requires a host and a username", async () => {
    mockBackend();
    render(<NtripCredentialsForm />);
    await userEvent.clear(host());

    await userEvent.click(saveBtn());

    expect(screen.getByText("Host and username are required.")).toHaveClass("is-err");
    expect(invokeCount("save_ntrip_credentials")).toBe(0);
  });

  it("saves trimmed values and forgets the password", async () => {
    mockBackend();
    render(<NtripCredentialsForm />);

    await userEvent.clear(host());
    await userEvent.type(host(), "  my.caster  ");
    await userEvent.type(username(), " alice ");
    await userEvent.type(password(), " s3cret ");
    await userEvent.click(saveBtn());

    await waitFor(() => expect(invokeCount("save_ntrip_credentials")).toBe(1));
    // The password is passed as typed: leading/trailing spaces can be part of it.
    expect(lastInvokeArgs("save_ntrip_credentials")).toEqual({
      host: "my.caster",
      username: "alice",
      password: " s3cret ",
    });
    expect(password()).toHaveValue("");
    expect(screen.getByText("Saved credentials for my.caster")).toHaveClass("is-ok");
  });

  it("saves on Enter in the password field", async () => {
    mockBackend();
    render(<NtripCredentialsForm />);
    await userEvent.type(username(), "alice");
    await userEvent.type(password(), "pw{Enter}");
    await waitFor(() => expect(invokeCount("save_ntrip_credentials")).toBe(1));
  });

  it("reports a failed save", async () => {
    mockBackend({ save_ntrip_credentials: () => Promise.reject("keyring locked") });
    render(<NtripCredentialsForm />);
    await userEvent.type(username(), "alice");

    await userEvent.click(saveBtn());

    expect(await screen.findByText("Save failed: keyring locked")).toHaveClass("is-err");
    expect(saveBtn()).toBeEnabled();
  });

  it("lists saved casters and forgets one", async () => {
    mockBackend();
    render(<NtripCredentialsForm />);
    expect(await screen.findByText("caster.example")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "forget" }));

    expect(lastInvokeArgs("forget_ntrip_credentials")).toEqual({ host: "caster.example" });
    expect(await screen.findByText("Removed credentials for caster.example")).toBeInTheDocument();
    await waitFor(() => expect(screen.queryByText("caster.example")).not.toBeInTheDocument());
  });

  it("refreshes the saved list after a save", async () => {
    mockBackend();
    render(<NtripCredentialsForm />);
    await screen.findByText("caster.example");
    await userEvent.type(username(), "alice");

    await userEvent.click(saveBtn());

    expect(await screen.findByText("ntrip.data.gnss.ga.gov.au", { selector: "code" })).toBeInTheDocument();
  });

  it("hides the saved list in the compact onboarding layout", async () => {
    mockBackend();
    render(<NtripCredentialsForm compact />);
    await waitFor(() => expect(invokeCount("list_ntrip_hosts")).toBe(1));
    expect(screen.queryByText("Saved casters")).not.toBeInTheDocument();
  });

  it("treats an unreadable credential store as empty", async () => {
    mockBackend({ list_ntrip_hosts: () => Promise.reject("no keyring") });
    render(<NtripCredentialsForm />);
    await waitFor(() => expect(invokeCount("list_ntrip_hosts")).toBe(1));
    expect(screen.queryByText("Saved casters")).not.toBeInTheDocument();
  });

  it("reports a host it could not forget", async () => {
    mockBackend({ forget_ntrip_credentials: () => Promise.reject("denied") });
    render(<NtripCredentialsForm />);
    await screen.findByText("caster.example");

    await userEvent.click(screen.getByRole("button", { name: "forget" }));

    expect(await screen.findByText("Could not remove caster.example: denied")).toHaveClass(
      "is-err",
    );
  });
});
