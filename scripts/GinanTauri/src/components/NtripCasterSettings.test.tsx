import { beforeEach, describe, expect, it } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import NtripCasterSettings from "./NtripCasterSettings";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

const GA = "ntrip.data.gnss.ga.gov.au";

beforeEach(() => {
  resetAppStore();
  mockInvoke({ list_ntrip_hosts: [] });
});

const casterField = () => screen.getByLabelText("Caster URL");
const modal = () => document.querySelector(".rt-modal") as HTMLElement;
/** The sign-in modal's fields carry unassociated labels; reach them by type. */
const modalField = (n: number) =>
  modal().querySelectorAll("input")[n] as HTMLInputElement;

/** Wait past the `list_ntrip_hosts` call the panel makes on mount. */
const settled = () => waitFor(() => expect(invokeCount("list_ntrip_hosts")).toBeGreaterThan(0));

describe("the caster URL field", () => {
  it("shows the configured caster and writes edits into the store", async () => {
    render(<NtripCasterSettings />);
    await settled();
    expect(casterField()).toHaveValue("https://ntrip.data.gnss.ga.gov.au/");

    fireEvent.change(casterField(), { target: { value: "https://other.example/" } });

    expect(store().casterUrl).toBe("https://other.example/");
  });

  it("puts the GA caster back when the field is left empty", async () => {
    render(<NtripCasterSettings />);
    await settled();

    fireEvent.change(casterField(), { target: { value: "" } });
    expect(store().casterUrl).toBe("");
    fireEvent.blur(casterField());

    expect(store().casterUrl).toBe("https://ntrip.data.gnss.ga.gov.au/");
    expect(casterField()).toHaveValue("https://ntrip.data.gnss.ga.gov.au/");
  });

  it("leaves a typed caster alone when the field loses focus", async () => {
    render(<NtripCasterSettings />);
    await settled();

    fireEvent.change(casterField(), { target: { value: "https://other.example/" } });
    fireEvent.blur(casterField());

    expect(store().casterUrl).toBe("https://other.example/");
  });

  it("warns about a login left in the URL", async () => {
    render(<NtripCasterSettings />);
    await settled();

    fireEvent.change(casterField(), {
      target: { value: "https://alice:s3cr3t@host.example/" },
    });

    expect(screen.getByText(/Remove the username and password/)).toBeInTheDocument();
    expect(casterField()).toHaveClass("rt-input-warn");
  });

  it("warns about a mountpoint left on the end", async () => {
    render(<NtripCasterSettings />);
    await settled();

    fireEvent.change(casterField(), { target: { value: "https://host.example/ALIC00AUS0" } });

    expect(screen.getByText(/looks like a mountpoint/)).toBeInTheDocument();
  });

  it("says nothing about a well-formed caster", async () => {
    render(<NtripCasterSettings />);
    await settled();
    expect(document.querySelector(".rt-warning")).toBeNull();
  });
});

describe("the sign-in state", () => {
  it("reports a caster with no stored login", async () => {
    render(<NtripCasterSettings />);
    await settled();
    expect(await screen.findByText(`Not signed in to ${GA}`)).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Sign in" })).toBeEnabled();
  });

  it("calls a stored login saved, not signed in, until the caster has said so", async () => {
    mockInvoke({ list_ntrip_hosts: [GA] });
    render(<NtripCasterSettings />);

    // Saving writes to netrc and contacts nothing, so a login read back from
    // disk is unverified and must not be reported as "Signed in".
    expect(
      await screen.findByText(`Login saved for ${GA} - not checked yet`),
    ).toBeInTheDocument();
    expect(screen.queryByText(`Signed in to ${GA}`)).not.toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Update sign-in" })).toBeInTheDocument();
  });

  it("checks a stored login on request and reports acceptance", async () => {
    mockInvoke({
      list_ntrip_hosts: [GA],
      verify_ntrip_credentials: { state: "accepted", host: GA, mountpoint: "ALIC00AUS0" },
    });
    render(<NtripCasterSettings />);
    await userEvent.click(await screen.findByRole("button", { name: "Check now" }));

    expect(await screen.findByText(`Signed in to ${GA}`)).toBeInTheDocument();
  });

  it("says plainly when the caster refuses a stored login", async () => {
    mockInvoke({
      list_ntrip_hosts: [GA],
      verify_ntrip_credentials: {
        state: "rejected",
        host: GA,
        detail: "The caster rejected the username or password.",
      },
    });
    render(<NtripCasterSettings />);
    await userEvent.click(await screen.findByRole("button", { name: "Check now" }));

    expect(await screen.findByText(/rejected the saved login/)).toBeInTheDocument();
    expect(screen.queryByText(`Signed in to ${GA}`)).not.toBeInTheDocument();
  });

  it("does not call an unreachable caster a rejection", async () => {
    mockInvoke({
      list_ntrip_hosts: [GA],
      verify_ntrip_credentials: {
        state: "inconclusive",
        host: GA,
        detail: "Could not reach the caster.",
      },
    });
    render(<NtripCasterSettings />);
    await userEvent.click(await screen.findByRole("button", { name: "Check now" }));

    // Telling someone their password is wrong when the network is down sends
    // them to reset a working login.
    expect(await screen.findByText(/not checked: Could not reach/)).toBeInTheDocument();
    expect(screen.queryByText(/rejected/)).not.toBeInTheDocument();
  });

  it("cannot sign in without a caster to sign in to", async () => {
    store().setCasterUrl("");
    render(<NtripCasterSettings />);
    await settled();

    expect(screen.getByRole("button", { name: "Sign in" })).toBeDisabled();
    expect(screen.queryByText(/signed in to/)).not.toBeInTheDocument();
  });

  it("survives a backend that cannot list the stored hosts", async () => {
    mockInvoke({ list_ntrip_hosts: () => Promise.reject(new Error("no netrc")) });
    render(<NtripCasterSettings />);

    expect(await screen.findByText(`Not signed in to ${GA}`)).toBeInTheDocument();
  });
});

describe("the sign-in modal", () => {
  it("keeps the form locked until verification completes, then closes", async () => {
    let finish!: (value: unknown) => void;
    mockInvoke({
      list_ntrip_hosts: [GA], save_ntrip_credentials: null,
      verify_ntrip_credentials: () => new Promise((resolve) => { finish = resolve; }),
    });
    render(<NtripCasterSettings />);
    await userEvent.click(await screen.findByRole("button", { name: "Update sign-in" }));
    expect(within(modal()).getByRole("button", { name: "Save" })).toBeDisabled();
    await userEvent.type(modalField(1), "alice");
    await userEvent.type(modalField(2), "fixture");
    await userEvent.click(within(modal()).getByRole("button", { name: "Save" }));
    expect(await screen.findByRole("button", { name: "Verifying..." })).toBeDisabled();
    expect(modalField(1)).toBeDisabled();
    expect(within(modal()).getByRole("button", { name: "Close" })).toBeDisabled();
    fireEvent.click(document.querySelector(".rt-modal-backdrop")!);
    expect(modal()).toBeInTheDocument();
    await act(async () => finish({ state: "accepted", host: GA, mountpoint: "ALIC" }));
    expect(modal()).toBeNull();
    expect(invokeCount("save_ntrip_credentials")).toBe(1);
  });

  it("keeps an inconclusive check open with an explanation", async () => {
    mockInvoke({
      list_ntrip_hosts: [], save_ntrip_credentials: null,
      verify_ntrip_credentials: { state: "inconclusive", host: GA, detail: "Caster unreachable" },
    });
    render(<NtripCasterSettings />);
    await settled();
    await userEvent.click(screen.getByRole("button", { name: "Sign in" }));
    await userEvent.type(modalField(1), "alice");
    await userEvent.type(modalField(2), "fixture");
    await userEvent.click(within(modal()).getByRole("button", { name: "Save" }));
    expect(await within(modal()).findByText(/could not be checked: Caster unreachable/)).toBeInTheDocument();
    expect(modal()).toBeInTheDocument();
  });

  it("does not carry a previous caster's accepted result to a new URL", async () => {
    let finish!: (value: unknown) => void;
    mockInvoke({ list_ntrip_hosts: [GA, "other.example"],
      verify_ntrip_credentials: () => new Promise((resolve) => { finish = resolve; }) });
    render(<NtripCasterSettings />);
    await userEvent.click(await screen.findByRole("button", { name: "Check now" }));
    fireEvent.change(casterField(), { target: { value: "https://other.example/" } });
    await act(async () => finish({ state: "accepted", host: GA, mountpoint: "ALIC" }));
    expect(screen.getByText(/Login saved for other.example - not checked yet/)).toBeInTheDocument();
    expect(screen.queryByText(/^Signed in to/)).not.toBeInTheDocument();
  });
  it("opens pre-filled with the caster host", async () => {
    render(<NtripCasterSettings />);
    await settled();

    await userEvent.click(screen.getByRole("button", { name: "Sign in" }));

    expect(screen.getByRole("heading", { name: "Sign in to the caster" })).toBeInTheDocument();
    expect(modalField(0)).toHaveValue(GA);
  });

  it("stores the login and re-reads the host list", async () => {
    let saved = false;
    mockInvoke({
      list_ntrip_hosts: () => saved ? [GA] : [],
      save_ntrip_credentials: () => { saved = true; },
      verify_ntrip_credentials: { state: "accepted", host: GA, mountpoint: "ALIC00AUS0" },
    });
    render(<NtripCasterSettings />);
    await settled();
    await userEvent.click(screen.getByRole("button", { name: "Sign in" }));

    await userEvent.type(modalField(1), "alice");
    await userEvent.type(modalField(2), "s3cr3t");
    await userEvent.click(within(modal()).getByRole("button", { name: "Save" }));

    await waitFor(() =>
      expect(lastInvokeArgs("save_ntrip_credentials")).toEqual({
        host: GA,
        username: "alice",
        password: "s3cr3t",
      }),
    );
    await waitFor(() => expect(modal()).toBeNull());
    expect(screen.getByText(`Signed in to ${GA}`)).toBeInTheDocument();
    expect(screen.getByRole("status")).toHaveTextContent("Last login check completed at");
    await userEvent.click(screen.getByRole("button", { name: "Update sign-in" }));
    expect(modalField(2)).toHaveValue("");
  });

  it("does not claim a sign-in when the caster refuses the new login", async () => {
    mockInvoke({
      list_ntrip_hosts: [],
      save_ntrip_credentials: null,
      verify_ntrip_credentials: {
        state: "rejected",
        host: GA,
        detail: "The caster rejected the username or password.",
      },
    });
    render(<NtripCasterSettings />);
    await settled();
    await userEvent.click(screen.getByRole("button", { name: "Sign in" }));

    await userEvent.type(modalField(1), "alice");
    await userEvent.type(modalField(2), "wrong");
    await userEvent.click(within(modal()).getByRole("button", { name: "Save" }));

    expect(
      await screen.findByText(/rejected the username or password.*will not work for a run/),
    ).toBeInTheDocument();
  });

  it("reports a failed sign-in without closing", async () => {
    mockInvoke({
      list_ntrip_hosts: [],
      save_ntrip_credentials: () => Promise.reject(new Error("netrc is read-only")),
    });
    render(<NtripCasterSettings />);
    await settled();
    await userEvent.click(screen.getByRole("button", { name: "Sign in" }));
    await userEvent.type(modalField(1), "alice");
    await userEvent.type(modalField(2), "fixture");
    await userEvent.click(within(modal()).getByRole("button", { name: "Save" }));

    expect(await screen.findByText(/Could not save the login: .*read-only/)).toBeInTheDocument();
    expect(modal()).toBeInTheDocument();
  });

  it("closes on request", async () => {
    render(<NtripCasterSettings />);
    await settled();
    await userEvent.click(screen.getByRole("button", { name: "Sign in" }));

    await userEvent.click(within(modal()).getByRole("button", { name: "Close" }));

    expect(document.querySelector(".rt-modal")).toBeNull();
  });
});

describe("the stored-hosts list", () => {
  it("is absent when nothing is stored", async () => {
    render(<NtripCasterSettings />);
    await settled();
    expect(document.querySelector(".rt-hosts-list")).toBeNull();
  });

  it("lists each stored host with a way to sign out", async () => {
    mockInvoke({ list_ntrip_hosts: [GA, "other.example"], forget_ntrip_credentials: null });
    render(<NtripCasterSettings />);

    const signOut = await screen.findAllByRole("button", { name: "Sign out" });
    expect(signOut).toHaveLength(2);

    mockInvoke({ list_ntrip_hosts: ["other.example"], forget_ntrip_credentials: null });
    await userEvent.click(signOut[0]);

    await waitFor(() => expect(lastInvokeArgs("forget_ntrip_credentials")).toEqual({ host: GA }));
    await waitFor(() =>
      expect(screen.getAllByRole("button", { name: "Sign out" })).toHaveLength(1),
    );
  });
});

describe("browsing the sourcetable", () => {
  it("needs a caster before it can browse", async () => {
    store().setCasterUrl("");
    render(<NtripCasterSettings />);
    await settled();
    expect(screen.getByRole("button", { name: "Browse streams" })).toBeDisabled();
  });

  it("fetches the sourcetable and reports what it found", async () => {
    mockInvoke({
      list_ntrip_hosts: [],
      fetch_ntrip_sourcetable: [
        {
          mountpoint: "ALIC00AUS0",
          identifier: "Alice Springs",
          format: "RTCM 3.3",
          format_details: "",
          carrier: "2",
          nav_system: "GPS",
          network: "AUSCORS",
          country: "AUS",
          category: "obs",
        },
      ],
    });
    render(<NtripCasterSettings />);
    await settled();

    await userEvent.click(screen.getByRole("button", { name: "Browse streams" }));

    expect(await screen.findByText(/1 stream on/)).toBeInTheDocument();
    expect(store().ntripSourcetable).toHaveLength(1);
  });

  it("shows the reason the fetch failed", async () => {
    mockInvoke({
      list_ntrip_hosts: [],
      fetch_ntrip_sourcetable: () => Promise.reject(new Error("connection refused")),
    });
    render(<NtripCasterSettings />);
    await settled();

    await userEvent.click(screen.getByRole("button", { name: "Browse streams" }));

    expect(await screen.findByText(/connection refused/)).toBeInTheDocument();
  });

  it("says so while a fetch is in flight", async () => {
    useAppStore.setState({ ntripSourcetableStatus: { state: "loading" } });
    render(<NtripCasterSettings />);
    await settled();

    expect(screen.getByRole("button", { name: "Browsing..." })).toBeDisabled();
    expect(screen.getByRole("button", { name: "Refresh the stream list" })).toBeDisabled();
  });
});

describe("pasting a full URL", () => {
  async function openParse() {
    render(<NtripCasterSettings />);
    await settled();
    await userEvent.click(screen.getByRole("button", { name: /Paste full URL/ }));
    return modal().querySelector("input") as HTMLInputElement;
  }

  it("splits the server, login and mountpoint into their own places", async () => {
    mockInvoke({
      list_ntrip_hosts: [],
      save_ntrip_credentials: null,
      verify_ntrip_credentials: {
        state: "accepted",
        host: "ntrip.example.com",
        mountpoint: "ALIC00AUS0",
      },
    });
    const input = await openParse();

    fireEvent.change(input, {
      target: { value: "https://alice:s3cr3t@ntrip.example.com/ALIC00AUS0" },
    });
    await userEvent.click(within(modal()).getByRole("button", { name: "Apply" }));

    await waitFor(() => expect(store().casterUrl).toBe("https://ntrip.example.com/"));
    expect(lastInvokeArgs("verify_ntrip_credentials")).toMatchObject({ casterUrl: "https://ntrip.example.com/" });
    expect(store().realtime.obs_streams).toContain("ALIC00AUS0");
    expect(lastInvokeArgs("save_ntrip_credentials")).toEqual({
      host: "ntrip.example.com",
      username: "alice",
      password: "s3cr3t",
    });
    expect(
      await within(modal()).findByText(
        "Caster set to ntrip.example.com and signed in. Mountpoint ALIC00AUS0 added.",
      ),
    ).toBeInTheDocument();
  });

  it("routes a correction mountpoint to the nav list", async () => {
    mockInvoke({ list_ntrip_hosts: [], save_ntrip_credentials: null });
    const input = await openParse();

    fireEvent.change(input, { target: { value: "https://host.example/SSRA00CNE0" } });
    await userEvent.click(within(modal()).getByRole("button", { name: "Apply" }));

    await waitFor(() => expect(store().casterUrl).toBe("https://host.example/"));
    expect(store().realtime.obs_streams).not.toContain("SSRA00CNE0");
    // Already in the shipped defaults, so it must not be duplicated by count.
    expect(store().realtime.nav_streams.filter((s) => s === "SSRA00CNE0")).toHaveLength(2);
  });

  it("asks for a sign-in when the URL carried no login", async () => {
    const input = await openParse();

    fireEvent.change(input, { target: { value: "https://host.example/" } });
    await userEvent.click(within(modal()).getByRole("button", { name: "Apply" }));

    expect(await screen.findByText(/No login found in the URL/)).toBeInTheDocument();
    expect(invokeCount("save_ntrip_credentials")).toBe(0);
  });

  it("keeps the caster even when the login could not be stored", async () => {
    mockInvoke({
      list_ntrip_hosts: [],
      save_ntrip_credentials: () => Promise.reject(new Error("netrc is read-only")),
    });
    const input = await openParse();

    fireEvent.change(input, { target: { value: "https://alice:pw@host.example/" } });
    await userEvent.click(within(modal()).getByRole("button", { name: "Apply" }));

    expect(
      await screen.findByText(/Caster set, but the sign-in could not be saved/),
    ).toBeInTheDocument();
    expect(store().casterUrl).toBe("https://host.example/");
  });

  it("explains what it expected when the URL will not parse", async () => {
    const input = await openParse();

    fireEvent.change(input, { target: { value: "not a url" } });
    await userEvent.click(within(modal()).getByRole("button", { name: "Apply" }));

    expect(await screen.findByText(/Could not parse/)).toBeInTheDocument();
    expect(store().casterUrl).toBe("https://ntrip.data.gnss.ga.gov.au/");
  });

  it("clears the last message as soon as the URL is edited again", async () => {
    const input = await openParse();
    fireEvent.change(input, { target: { value: "nope" } });
    await userEvent.click(within(modal()).getByRole("button", { name: "Apply" }));
    expect(await screen.findByText(/Could not parse/)).toBeInTheDocument();

    fireEvent.change(input, { target: { value: "https://host.example/" } });

    expect(screen.queryByText(/Could not parse/)).not.toBeInTheDocument();
  });
});
