import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import SettingsView from "./SettingsView";
import { useAppStore } from "../store/appStore";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, mockInvoke, open } from "../test/tauriMocks";
import { ABOUT_LINKS, THIRD_PARTY_NOTICES } from "../config/about";

// The heavyweight panels embedded in rows have their own suites; here they only
// need to exist, so the page structure can be tested without their backends.
vi.mock("./ConfigYamlInspector", () => ({
  default: () => <div data-testid="yaml-inspector" />,
}));
vi.mock("./NtripCasterSettings", () => ({
  default: () => <div data-testid="caster-settings" />,
}));
vi.mock("./PeaVersionControl", () => ({
  default: () => <div data-testid="pea-version-control" />,
}));

beforeEach(() => {
  resetAppStore();
});

const dialog = () => screen.getByRole("dialog", { name: "Settings" });
const navItem = (label: string) =>
  within(dialog()).getByRole("button", { name: new RegExp(`^${label}$`) });
const heading = () => screen.getByRole("heading", { level: 1 });
const search = () => screen.getByRole("textbox", { name: "Search settings" });

it.each(["third party", "third-party"])("finds notice groups when searching for %s", async (query) => {
  render(<SettingsView />);
  openOn("config");
  await userEvent.type(search(), query);
  expect(screen.getByText("Third-party notices")).toBeInTheDocument();
  for (const notice of THIRD_PARTY_NOTICES) expect(screen.getByText(notice.name)).toBeInTheDocument();
  expect(screen.queryByText("CDDIS connection")).not.toBeInTheDocument();
});

function openOn(page?: Parameters<ReturnType<typeof store>["openSettings"]>[0]) {
  act(() => store().openSettings(page));
}

describe("opening and closing", () => {
  it("renders nothing while closed", () => {
    render(<SettingsView />);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("opens on the page the store names, which is how deep links work", () => {
    render(<SettingsView />);
    openOn("credentials");
    expect(heading()).toHaveTextContent("Credentials");
    expect(screen.getByTestId("caster-settings")).toBeInTheDocument();
  });

  it("keeps the caster login beside the Earthdata login on Credentials", () => {
    render(<SettingsView />);
    openOn("credentials");
    expect(screen.getByText("NASA Earthdata")).toBeInTheDocument();
    expect(screen.getByText("NTRIP caster (real-time data streams)")).toBeInTheDocument();
    // The SBAS login lives here too, not on the SBAS source in the sidebar.
    expect(screen.getByText("SISNet (SBAS corrections)")).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /Live Stream/ })).not.toBeInTheDocument();
  });

  it("still lands an old Live Stream deep link on the caster", () => {
    render(<SettingsView />);
    act(() => store().openSettings("livestream"));
    expect(heading()).toHaveTextContent("Credentials");
    expect(screen.getByTestId("caster-settings")).toBeInTheDocument();
  });

  it("closes from the back button", async () => {
    render(<SettingsView />);
    openOn();
    await userEvent.click(screen.getByRole("button", { name: "Back" }));
    expect(store().settingsOpen).toBe(false);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("closes on Escape", () => {
    render(<SettingsView />);
    openOn();
    fireEvent.keyDown(window, { key: "Escape" });
    expect(store().settingsOpen).toBe(false);
  });

  it("leaves Escape to the credentials modal while that is on top", () => {
    render(<SettingsView />);
    openOn("credentials");
    act(() => store().setCredentialsModalOpen(true));
    fireEvent.keyDown(window, { key: "Escape" });
    expect(store().settingsOpen).toBe(true);
  });
});

describe("page navigation", () => {
  it("lists every page", () => {
    render(<SettingsView />);
    openOn();
    for (const label of [
      "Configuration",
      "Products Cache",
      "Output",
      "PEA Binary",
      "Credentials",
      "Appearance",
      "Help",
      "About",
    ]) {
      expect(navItem(label)).toBeInTheDocument();
    }
  });

  it("switches page from the nav and marks it active", async () => {
    render(<SettingsView />);
    openOn("config");

    await userEvent.click(navItem("Appearance"));

    expect(store().settingsPage).toBe("appearance");
    expect(heading()).toHaveTextContent("Appearance");
    expect(navItem("Appearance")).toHaveClass("is-active");
    expect(navItem("Configuration")).not.toHaveClass("is-active");
  });
});

describe("search", () => {
  it("finds rows across every page and labels each page with hits", async () => {
    render(<SettingsView />);
    openOn("appearance");

    await userEvent.type(search(), "directory");

    expect(heading()).toHaveTextContent("Results for “directory”");
    // Products directory and Output directory live on two different pages.
    expect(screen.getByText("Products directory")).toBeInTheDocument();
    expect(screen.getByText("Output directory")).toBeInTheDocument();
    expect(screen.queryByText("Theme")).not.toBeInTheDocument();
    // Pages without a hit stay listed, but dimmed.
    expect(navItem("Appearance")).toHaveClass("is-dimmed");
    expect(navItem("Output")).not.toHaveClass("is-dimmed");
  });

  it("matches descriptions case-insensitively", async () => {
    render(<SettingsView />);
    openOn();

    await userEvent.type(search(), "NETRC");

    expect(screen.getByText("CDDIS connection")).toBeInTheDocument();
  });

  it("says so when nothing matches", async () => {
    render(<SettingsView />);
    openOn();

    await userEvent.type(search(), "zzzz-nothing");

    expect(screen.getByText("No settings match “zzzz-nothing”.")).toBeInTheDocument();
  });

  it("clears the query when a page is picked from the nav", async () => {
    render(<SettingsView />);
    openOn();
    await userEvent.type(search(), "theme");

    await userEvent.click(navItem("Help"));

    expect(search()).toHaveValue("");
    expect(heading()).toHaveTextContent("Help");
  });

  it("opens on a full list again after being closed mid-search", async () => {
    render(<SettingsView />);
    openOn();
    await userEvent.type(search(), "theme");
    act(() => store().setSettingsOpen(false));

    openOn();

    expect(search()).toHaveValue("");
  });
});

describe("the Configuration page", () => {
  it("resets the run config and says what it did", async () => {
    store().setMode("Kinematic");
    render(<SettingsView />);
    openOn("config");

    await userEvent.click(screen.getByRole("button", { name: /Reset$/ }));

    expect(store().mode).toBe("Static");
    expect(screen.getByRole("status")).toHaveTextContent(
      "Processing mode, time window and output formats reset",
    );
  });

  it("refuses to export before there is anything to export", async () => {
    render(<SettingsView />);
    openOn("config");

    await userEvent.click(screen.getByRole("button", { name: /Export\.\.\./ }));

    expect(screen.getByRole("status")).toHaveTextContent(/Nothing to export yet/);
  });

  it("describes Export as this machine's run config with the caster logins left out", () => {
    render(<SettingsView />);
    openOn("config");

    const row = screen.getByText("Export config").closest(".settings-row") as HTMLElement;
    const text = row.querySelector(".settings-row-desc")!.textContent!;
    expect(text).toMatch(/this machine/);
    expect(text).toMatch(/paths included/);
    expect(text).toContain("<USER>:<PASS>");
    // Export keeps directories, so nothing may promise they are stripped.
    expect(text).not.toMatch(/without|strip|portable|share/i);
  });

  it("does nothing when the import dialog is cancelled", async () => {
    open.mockResolvedValueOnce(null);
    render(<SettingsView />);
    openOn("config");

    await userEvent.click(screen.getByRole("button", { name: /Import\.\.\./ }));

    expect(open).toHaveBeenCalledTimes(1);
    expect(invokeCount("import_pea_config")).toBe(0);
    expect(screen.queryByRole("status")).not.toBeInTheDocument();
  });

  it("disables Reset all until a processing parameter has been changed", () => {
    render(<SettingsView />);
    openOn("config");
    expect(screen.getByRole("button", { name: /Reset all/ })).toBeDisabled();
    expect(screen.getByText(/^0 of \d+ changed/)).toBeInTheDocument();
  });

  /** The parameter groups start folded away; open one by its heading. */
  const expandGroup = (label: string | RegExp) =>
    userEvent.click(screen.getByRole("button", { name: label }));

  it("folds each parameter heading away, and opens it on a press", async () => {
    render(<SettingsView />);
    openOn("config");

    const heading = screen.getByRole("button", { name: /Generated config/ });
    expect(heading).toHaveAttribute("aria-expanded", "false");
    expect(screen.queryByTestId("yaml-inspector")).not.toBeInTheDocument();

    await userEvent.click(heading);

    expect(heading).toHaveAttribute("aria-expanded", "true");
    expect(screen.getByTestId("yaml-inspector")).toBeInTheDocument();

    await userEvent.click(heading);
    expect(screen.queryByTestId("yaml-inspector")).not.toBeInTheDocument();
  });

  it("opens and closes a configuration group with the keyboard", async () => {
    render(<SettingsView />);
    openOn("config");
    const heading = screen.getByRole("button", { name: /Generated config/ });
    heading.focus();
    await userEvent.keyboard("{Enter}");
    expect(heading).toHaveAttribute("aria-expanded", "true");
    expect(screen.getByTestId("yaml-inspector")).toBeInTheDocument();
    await userEvent.keyboard(" ");
    expect(heading).toHaveAttribute("aria-expanded", "false");
    expect(screen.queryByTestId("yaml-inspector")).not.toBeInTheDocument();
  });

  it("keeps the import, export and reset rows out in the open", () => {
    render(<SettingsView />);
    openOn("config");
    expect(screen.getByRole("button", { name: /Import\.\.\./ })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /Reset all/ })).toBeInTheDocument();
  });

  it("remembers which groups are open across a close and reopen", async () => {
    render(<SettingsView />);
    openOn("config");
    await expandGroup(/Generated config/);
    expect(screen.getByTestId("yaml-inspector")).toBeInTheDocument();

    act(() => store().setSettingsOpen(false));
    openOn("config");

    expect(screen.getByTestId("yaml-inspector")).toBeInTheDocument();
  });

  it("shows a search hit even though its group is collapsed", async () => {
    render(<SettingsView />);
    openOn("config");
    expect(screen.queryByText("Observation code priorities")).not.toBeInTheDocument();

    await userEvent.type(screen.getByRole("textbox", { name: "Search settings" }), "code priorities");

    expect(screen.getByText("Observation code priorities")).toBeInTheDocument();
  });

  it("ends on the generated YAML inspector", async () => {
    render(<SettingsView />);
    openOn("config");
    await expandGroup(/Generated config/);
    expect(screen.getByTestId("yaml-inspector")).toBeInTheDocument();
  });

  it("shows the code priorities a run would write, above the YAML", async () => {
    render(<SettingsView />);
    openOn("config");
    await expandGroup(/Generated config/);
    expect(screen.getByText("Observation code priorities")).toBeInTheDocument();
    expect(screen.getByText("sys_options.gps")).toBeInTheDocument();
  });

  // A filename template is too long to read in the usual control column.
  // The width itself is CSS, but which rows opt into the wider column is not.
  it("gives a filename template the roomy control column", async () => {
    const template = "<RECEIVER>_<CONFIG>_<YYYY><DDD><HH>.GPX";
    act(() =>
      useAppStore.setState({
        discoveredParams: [
          { path: "outputs.gpx.filename", value_type: "string", default_value: template },
        ],
      }),
    );
    render(<SettingsView />);
    openOn("config");
    await expandGroup(/Outputs/);

    const field = screen.getByDisplayValue(template);
    expect(field.closest(".settings-row-control")).toHaveClass("is-roomy");
  });

  it("leaves a narrow control in the normal column", async () => {
    act(() =>
      useAppStore.setState({
        discoveredParams: [
          { path: "outputs.gpx.enable", value_type: "boolean", default_value: true },
        ],
      }),
    );
    render(<SettingsView />);
    openOn("config");
    await expandGroup(/Outputs/);

    const row = screen.getByText("Enable").closest(".settings-row") as HTMLElement;
    expect(row.querySelector(".settings-row-control")).not.toHaveClass("is-roomy");
  });
});

describe("the run mode row", () => {
  const modeGroup = () => screen.getByRole("group", { name: "Run mode" });

  it("shows the current mode and switches to real-time after asking", async () => {
    render(<SettingsView />);
    openOn("config");
    expect(within(modeGroup()).getByRole("button", { name: "File" })).toHaveAttribute("aria-pressed", "true");

    await userEvent.click(within(modeGroup()).getByRole("button", { name: "Real-time" }));
    expect(store().inputMode).toBe("file");
    await userEvent.click(screen.getByRole("button", { name: "Switch to real-time run" }));

    expect(store().inputMode).toBe("realtime");
    expect(within(modeGroup()).getByRole("button", { name: "Real-time" })).toHaveAttribute("aria-pressed", "true");
  });

  it("changes nothing when the switch is cancelled", async () => {
    render(<SettingsView />);
    openOn("config");
    await userEvent.click(within(modeGroup()).getByRole("button", { name: "Real-time" }));
    await userEvent.click(screen.getByRole("button", { name: "Cancel" }));
    expect(store().inputMode).toBe("file");
  });

  it("will not switch while PEA is running", () => {
    store().setIsProcessing(true);
    render(<SettingsView />);
    openOn("config");
    expect(within(modeGroup()).getByRole("button", { name: "Real-time" })).toBeDisabled();
  });
});

describe("directory rows", () => {
  it("shows the products directory and picks a new one", async () => {
    store().setProductsDir("/old/products");
    open.mockResolvedValueOnce("/new/products");
    render(<SettingsView />);
    openOn("products");
    expect(screen.getByText("/old/products")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Browse..." }));

    await waitFor(() => expect(store().productsDir).toBe("/new/products"));
  });

  it("says when no output directory is set", () => {
    render(<SettingsView />);
    openOn("output");
    expect(screen.getByText("Not set")).toBeInTheDocument();
  });
});

describe("the PEA Binary page", () => {
  const radio = (name: string) => screen.getByRole("radio", { name });
const card = (name: string) => radio(name).closest(".pea-card") as HTMLElement;

  it("marks the managed card active and names its version", () => {
    useAppStore.setState({ peaVersion: "v4.1.0", managedPeaVersion: "v4.1.0", peaBinaryPath: "/bin/pea" });
    render(<SettingsView />);
    openOn("binary");
    const managed = card("Managed release");
    expect(radio("Managed release")).toHaveAttribute("aria-checked", "true");
    expect(within(managed).getByText("v4.1.0")).toBeInTheDocument();
    expect(radio("Custom binary")).toHaveAttribute("aria-checked", "false");
  });

  it("marks the custom card active, with an unknown version and its path", () => {
    useAppStore.setState({ peaVersion: null, peaBinaryPath: "/my/pea", lastCustomPeaPath: "/my/pea" });
    render(<SettingsView />);
    openOn("binary");
    const custom = card("Custom binary");
    expect(radio("Custom binary")).toHaveAttribute("aria-checked", "true");
    expect(within(custom).getByText("Version unknown")).toBeInTheDocument();
    expect(within(custom).getByText("/my/pea")).toBeInTheDocument();
    expect(radio("Managed release")).toHaveAttribute("aria-checked", "false");
  });

  it("marks neither card active when there is no binary at all", () => {
    useAppStore.setState({ peaVersion: null, peaBinaryPath: null });
    render(<SettingsView />);
    openOn("binary");
    expect(radio("Managed release")).toHaveAttribute("aria-checked", "false");
    expect(radio("Custom binary")).toHaveAttribute("aria-checked", "false");
    expect(within(card("Managed release")).getByText("Not downloaded")).toBeInTheDocument();
    expect(within(card("Custom binary")).getByText("No file chosen")).toBeInTheDocument();
  });

  // PeaVersionControl is stubbed in this file, so only the cards' own text is compared.
  it("keeps each card's text the same whichever one is active", () => {
    useAppStore.setState({
      peaVersion: "v4.2.0",
      managedPeaVersion: "v4.2.0",
      peaBinaryPath: "/data/pea",
      lastCustomPeaPath: "/opt/pea",
    });
    render(<SettingsView />);
    openOn("binary");
    const text = () => [card("Managed release").textContent, card("Custom binary").textContent];
    const before = text();

    act(() => useAppStore.setState({ peaVersion: null, peaBinaryPath: "/opt/pea" }));

    expect(radio("Custom binary")).toHaveAttribute("aria-checked", "true");
    expect(text()).toEqual(before);
  });

  it("moves the active card to Custom after choosing a file", async () => {
    useAppStore.setState({ peaVersion: "v4.2.0", peaBinaryPath: "/data/pea" });
    open.mockResolvedValueOnce("/opt/pea");
    mockInvoke({ set_pea_binary: null, list_pea_versions: [] });
    render(<SettingsView />);
    openOn("binary");

    await userEvent.click(within(card("Custom binary")).getByRole("button", { name: "Choose file..." }));

    await waitFor(() => expect(radio("Custom binary")).toHaveAttribute("aria-checked", "true"));
    expect(within(card("Custom binary")).getByText("/opt/pea")).toBeInTheDocument();
    expect(radio("Managed release")).toHaveAttribute("aria-checked", "false");
  });

  it("tags the page heading Managed or Custom", () => {
    useAppStore.setState({ peaVersion: "v4.1.0", peaBinaryPath: "/bin/pea" });
    render(<SettingsView />);
    openOn("binary");
    const heading = screen.getByRole("heading", { level: 1 });
    expect(within(heading).getByText("Managed")).toHaveClass("settings-tag");

    act(() => useAppStore.setState({ peaVersion: null, peaBinaryPath: "/my/pea" }));
    expect(within(heading).getByText("Custom")).toHaveClass("settings-tag");
  });

  it("mentions an available update on the managed card", () => {
    useAppStore.setState({ peaVersion: "v4.1.0", peaBinaryPath: "/bin/pea", peaUpdateAvailable: "v4.2.0" });
    render(<SettingsView />);
    openOn("binary");
    expect(within(card("Managed release")).getByText("v4.2.0 available")).toBeInTheDocument();
  });

  it("is still found by the settings search", async () => {
    render(<SettingsView />);
    openOn("products");
    await userEvent.type(search(), "custom binary");
    expect(card("Custom binary")).toBeInTheDocument();
  });
});

describe("the Credentials page", () => {
  it.each([
    [{ offlineMode: true }, "Earthdata sign-in required - downloads unavailable"],
    [{ offlineMode: true, cddisStatusMessage: "Signed out of Earthdata" }, "Signed out of Earthdata"],
    [{ cddisStatus: "ok" as const }, "Connected to CDDIS"],
    [{ cddisStatus: "checking" as const }, "Checking CDDIS..."],
    [{ cddisStatus: "idle" as const }, "Not checked yet, set a products directory"],
    [{ cddisStatus: "error" as const, cddisStatusMessage: "401" }, "401"],
    [{ cddisStatus: "error" as const }, "Credentials missing or invalid"],
  ])("reports %o as %s", (state, text) => {
    useAppStore.setState({ offlineMode: false, ...state });
    render(<SettingsView />);
    openOn("credentials");
    expect(screen.getByText(text)).toBeInTheDocument();
  });

  it("opens the credentials modal", async () => {
    render(<SettingsView />);
    openOn("credentials");
    await userEvent.click(screen.getByRole("button", { name: "Set credentials..." }));
    expect(store().credentialsModalOpen).toBe(true);
  });
});

describe("the Appearance page", () => {
  it("switches theme and remembers it was chosen by hand", async () => {
    store().setTheme("light");
    render(<SettingsView />);
    openOn("appearance");
    const group = screen.getByRole("group", { name: "Theme" });
    expect(within(group).getByRole("button", { name: "Light" })).toHaveClass("is-active");

    await userEvent.click(within(group).getByRole("button", { name: "Dark" }));

    expect(store().theme).toBe("dark");
    expect(document.documentElement.getAttribute("data-theme")).toBe("dark");
    expect(localStorage.getItem("ginan-theme-manual")).toBe("true");
    expect(within(group).getByRole("button", { name: "Dark" })).toHaveClass("is-active");
  });
});

describe("the Help page", () => {
  it("starts the tour that matches what is on screen", async () => {
    render(<SettingsView />);
    openOn("help");
    await userEvent.click(screen.getByRole("button", { name: /Start tour/ }));
    expect(store().activeTour).toBe("welcome");
    // Starting a tour closes settings so the tour can point at the app.
    expect(store().settingsOpen).toBe(false);
  });

  it("starts the workspace tour once a file is open", async () => {
    store().setRinexData(makeRinexData());
    render(<SettingsView />);
    openOn("help");
    await userEvent.click(screen.getByRole("button", { name: /Start tour/ }));
    expect(store().activeTour).toBe("workspace");
  });

  it("replays first-run setup", async () => {
    mockInvoke({});
    useAppStore.setState({ onboardingStatus: "done" });
    render(<SettingsView />);
    openOn("help");
    await userEvent.click(screen.getByRole("button", { name: /Run setup again/ }));
    expect(store().onboardingStatus).toBe("active");
  });
});

describe("the About page", () => {
  it("states the project licence and links out to upstream, docs and issues", () => {
    render(<SettingsView />);
    openOn("about");

    expect(
      screen.getByText(/Copyright 2020 Geoscience Australia\. Licensed under the Apache License 2\.0/),
    ).toBeInTheDocument();
    for (const [name, href] of [
      ["GitHub repository", ABOUT_LINKS.repo],
      ["Documentation", ABOUT_LINKS.docs],
      ["Licence text", ABOUT_LINKS.license],
      ["GitHub issues", ABOUT_LINKS.issues],
    ] as const) {
      const link = screen.getByRole("link", { name });
      expect(link).toHaveAttribute("href", href);
      expect(link).toHaveAttribute("target", "_blank");
    }
  });

  it("lists every third-party notice and carries the full licence texts", () => {
    render(<SettingsView />);
    openOn("about");

    for (const n of THIRD_PARTY_NOTICES) {
      expect(screen.getByText(n.name, { selector: "*:not(pre)" })).toBeInTheDocument();
    }
    expect(screen.getByText("OpenStreetMap")).toBeInTheDocument();
    const texts = screen.getByText("Show licence texts").closest("details")!;
    expect(texts).toHaveTextContent("Permission is hereby granted, free of charge");
    expect(texts).toHaveTextContent("Redistribution and use in source and binary forms");
  });
});
