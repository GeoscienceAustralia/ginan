import { beforeEach, describe, expect, it, vi } from "vitest";
import { act, render, screen, waitFor } from "@testing-library/react";
import App from "./App";
import { useAppStore } from "./store/appStore";
import { WF } from "./config/workflowSteps";
import { makeRinexData, resetAppStore, store } from "./test/storeHelpers";
import { invoke, invokeCount, lastInvokeArgs, mockInvoke, seedStore } from "./test/tauriMocks";

/**
 * App owns the startup chain and decides which of the two shells is on screen.
 * Everything here goes through the real child components, so these are the
 * closest thing the suite has to an end-to-end check.
 */

/** The commands App fires on mount, all succeeding. */
function mockStartup(over: Record<string, unknown> = {}) {
  mockInvoke({
    discover_config_params: [],
    ensure_default_pea_binary: {
      path: "/bin/pea",
      version: null,
      source: "existing",
      update_available: null,
    },
    validate_cddis_credentials: { ok: true, why: null },
    check_cddis_connectivity: "ok",
    ensure_products: null,
    ensure_pea_runtime: null,
    find_antex_file: "/products/igs20.atx",
    scan_ppp_catalogue: { entries: [] },
    fetch_bia_codes_for_combo: { gps: [], gal: [], glo: [], bds: [], qzs: [] },
    ...over,
  });
}

beforeEach(() => {
  resetAppStore();
  vi.spyOn(console, "error").mockImplementation(() => {});
});

describe("which shell is on screen", () => {
  it("opens on the welcome screen with nothing loaded", async () => {
    mockStartup();
    render(<App />);

    expect(screen.getByText("No observation file open")).toBeInTheDocument();
    // The sidebar, tabs and status bar do not exist yet.
    expect(screen.queryByRole("tablist")).not.toBeInTheDocument();
    await waitFor(() => expect(invokeCount("ensure_default_pea_binary")).toBe(1));
  });

  it("puts up the full shell once a file is parsed", async () => {
    mockStartup();
    render(<App />);

    await waitFor(() => expect(invokeCount("ensure_default_pea_binary")).toBe(1));
    await waitFor(() => {
      store().setRinexData(makeRinexData());
    });

    expect(screen.queryByText("No observation file open")).not.toBeInTheDocument();
    expect(await screen.findByRole("tablist", { name: "Workspace" })).toBeInTheDocument();
  });

  it("puts up the shell for a realtime run with no file at all", async () => {
    mockStartup();
    render(<App />);
    await waitFor(() => expect(invokeCount("ensure_default_pea_binary")).toBe(1));

    await waitFor(() => {
      store().setInputMode("realtime");
    });

    expect(await screen.findByRole("tablist", { name: "Workspace" })).toBeInTheDocument();
  });
});

describe("startup", () => {
  it("hydrates the persisted setup and registers the pea binary", async () => {
    seedStore({ outputDir: "/out", productsDir: "/products", mode: "Kinematic" });
    mockStartup();
    render(<App />);

    await waitFor(() => expect(store().outputDir).toBe("/out"));
    expect(store().mode).toBe("Kinematic");
    await waitFor(() => expect(store().peaBinaryPath).toBe("/bin/pea"));
    expect(store().status).toBe("Ready");
  });

  it("names the installed pea version once it is resolved", async () => {
    mockStartup({
      ensure_default_pea_binary: {
        path: "/bin/pea",
        version: "v4.1.0",
        source: "existing",
        update_available: null,
      },
    });
    render(<App />);

    await waitFor(() => expect(store().status).toBe("pea v4.1.0 ready"));
    expect(store().peaVersion).toBe("v4.1.0");
    expect(store().peaUpdateModalOpen).toBe(false);
  });

  it("prompts for an update rather than installing it", async () => {
    mockStartup({
      ensure_default_pea_binary: {
        path: "/bin/pea",
        version: "v4.1.0",
        source: "existing",
        update_available: "v4.2.0",
      },
    });
    render(<App />);

    await waitFor(() => expect(store().peaUpdateModalOpen).toBe(true));
    expect(store().peaUpdateAvailable).toBe("v4.2.0");
    expect(store().status).toBe("pea v4.1.0 ready - v4.2.0 available");
  });

  it("tells the user once when the binary is not the managed release", async () => {
    mockStartup({
      ensure_default_pea_binary: {
        path: "/usr/local/bin/pea",
        version: null,
        source: "override",
        update_available: null,
      },
    });
    render(<App />);

    await waitFor(() => expect(store().customPeaModalOpen).toBe(true));
  });

  it("does not raise the custom-binary notice again for a path already dismissed", async () => {
    seedStore({ customPeaNoticeSeen: "/usr/local/bin/pea" });
    mockStartup({
      ensure_default_pea_binary: {
        path: "/usr/local/bin/pea",
        version: null,
        source: "override",
        update_available: null,
      },
    });
    render(<App />);

    await waitFor(() => expect(store().lastCustomPeaPath).toBe("/usr/local/bin/pea"));
    expect(store().customPeaModalOpen).toBe(false);
  });

  it("re-registers a persisted binary before resolving the default", async () => {
    seedStore({ peaBinaryPath: "/custom/pea" });
    mockStartup({ set_pea_binary: null });
    render(<App />);

    await waitFor(() => expect(invokeCount("ensure_default_pea_binary")).toBe(1));
    expect(lastInvokeArgs("set_pea_binary")).toEqual({ path: "/custom/pea" });
    const order = invoke.mock.calls.map((c) => c[0]);
    expect(order.indexOf("set_pea_binary")).toBeLessThan(
      order.indexOf("ensure_default_pea_binary"),
    );
  });

  it("falls through to the managed binary when the persisted one is gone", async () => {
    seedStore({ peaBinaryPath: "/deleted/pea" });
    mockStartup({ set_pea_binary: () => Promise.reject(new Error("no such file")) });
    render(<App />);

    await waitFor(() => expect(store().peaBinaryPath).toBe("/bin/pea"));
  });

  it("reports an unavailable pea binary in the status line", async () => {
    mockStartup({
      ensure_default_pea_binary: () => Promise.reject(new Error("no release for this platform")),
    });
    render(<App />);

    await waitFor(() => expect(store().status).toMatch(/Pea binary unavailable/));
  });

  it("reports a failure to read the saved settings", async () => {
    const { storeLoad } = await import("./test/tauriMocks");
    storeLoad.mockRejectedValueOnce(new Error("settings file is corrupt"));
    mockStartup();
    render(<App />);

    await waitFor(() => expect(store().status).toMatch(/Could not read saved settings/));
  });

  it("applies the persisted theme to the document", async () => {
    mockStartup();
    render(<App />);
    await waitFor(() =>
      expect(document.documentElement.getAttribute("data-theme")).toBe(store().theme),
    );
  });
});

describe("the CDDIS chain", () => {
  it("retries a transient failure after a directory change, even after a previous sign-in", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    store().bumpCddisEpoch();
    let attempts = 0;
    mockStartup({ check_cddis_connectivity: () => {
      if (++attempts === 1) return Promise.reject("Network unavailable");
      return "ok";
    } });
    render(<App />);
    await waitFor(() => expect(store().cddisStatus).toBe("error"));
    expect(store().offlineMode).toBe(true);
    act(() => store().setProductsDir("/other-products"));
    await waitFor(() => expect(store().cddisStatus).toBe("ok"));
    expect(store().offlineMode).toBe(false);
  });
  it("fetches the libraries PEA needs once the binary is resolved, and survives a failure", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    mockStartup({ ensure_pea_runtime: () => Promise.reject("offline") });
    render(<App />);
    await waitFor(() => expect(invokeCount("ensure_pea_runtime")).toBe(1));
    await waitFor(() => expect(store().peaBinaryPath).toBe("/bin/pea"));
  });

  it.each([false, true])("clears support download progress after completion (failure %s)", async (failed) => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    let finish!: () => void;
    mockStartup({ ensure_products: () => new Promise<void>((resolve, reject) => {
      finish = () => failed ? reject("disk full") : resolve();
    }) });
    render(<App />);
    await waitFor(() => expect(invokeCount("ensure_products")).toBe(1));
    await waitFor(() => expect(store().peaBinaryPath).toBeTruthy());
    act(() => store().setStatus("Downloading tables/gpt_25.grd · 100%"));
    await act(async () => finish());
    await waitFor(() => expect(store().status).not.toContain("Downloading"));
    expect(store().status).toMatch(failed ? /Support files unavailable: disk full/ : /pea(?: .*?)? ready/);
  });

  it("does not replace a newer run status when startup downloads finish", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    let finish!: () => void;
    mockStartup({ ensure_products: () => new Promise<void>((resolve) => { finish = resolve; }) });
    render(<App />);
    await waitFor(() => expect(invokeCount("ensure_products")).toBe(1));
    act(() => useAppStore.setState({ status: "Running PEA", isProcessing: true }));
    await act(async () => finish());
    expect(store().status).toBe("Running PEA");
  });

  it("ignores a pending connectivity result after signing out", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    let finish!: () => void;
    mockStartup({ check_cddis_connectivity: () => new Promise<void>((resolve) => { finish = resolve; }) });
    render(<App />);
    await waitFor(() => expect(store().cddisStatus).toBe("checking"));
    act(() => {
      store().setOfflineMode(true);
      store().setCddisStatus("error", "Signed out");
      store().bumpCddisEpoch();
    });
    await act(async () => finish());
    expect(store().offlineMode).toBe(true);
    expect(store().cddisStatus).toBe("error");
    expect(invokeCount("ensure_products")).toBe(0);
  });
  it("does not run without a products directory", async () => {
    mockStartup();
    render(<App />);

    await waitFor(() => expect(invokeCount("ensure_default_pea_binary")).toBe(1));
    expect(invokeCount("check_cddis_connectivity")).toBe(0);
    expect(store().cddisStatus).toBe("idle");
  });

  it("checks connectivity, fetches static products and finds the ANTEX", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    mockStartup();
    render(<App />);

    await waitFor(() => expect(store().cddisStatus).toBe("ok"));
    await waitFor(() => expect(store().atxPath).toBe("/products/igs20.atx"));
    expect(store().offlineMode).toBe(false);
    const log = store().workflowLines.join("\n");
    // One Workflow line per step, in order: connectivity → metadata → ANTEX.
    expect(log).toMatch(/CDDIS connectivity check passed in [\d.]+ seconds/);
    expect(log).toContain(WF.metadataDownloaded());
    expect(log).toContain(WF.atxSelected("igs20.atx", "priority"));
    expect(log.indexOf("CDDIS connectivity check passed")).toBeLessThan(
      log.indexOf(WF.metadataDownloaded()),
    );
    expect(log.indexOf(WF.metadataDownloaded())).toBeLessThan(
      log.indexOf(WF.atxSelected("igs20.atx", "priority")),
    );
  });

  it("goes offline and stops the chain when connectivity fails", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    mockStartup({
      check_cddis_connectivity: () => Promise.reject(new Error("401 Unauthorized")),
    });
    render(<App />);

    await waitFor(() => expect(store().cddisStatus).toBe("error"));
    expect(store().offlineMode).toBe(true);
    // No point downloading products with broken credentials.
    expect(invokeCount("ensure_products")).toBe(0);
  });

  it("stops after a failed product fetch rather than looking for an ANTEX", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    mockStartup({ ensure_products: () => Promise.reject(new Error("disk full")) });
    render(<App />);

    await waitFor(() =>
      expect(store().workflowLines.join("\n")).toContain(WF.metadataFailed("Error: disk full")),
    );
    expect(invokeCount("find_antex_file")).toBe(0);
  });

  it("always re-detects the ANTEX, ignoring an override left by an earlier version of the app", async () => {
    seedStore({
      productsDir: "/products",
      onboardingDone: true,
      atxManual: true,
      atxPath: "/elsewhere/igs20.atx",
    });
    mockStartup();
    render(<App />);

    await waitFor(() => expect(store().atxPath).toBe("/products/igs20.atx"));
    expect(invokeCount("find_antex_file")).toBe(1);
  });

  it("says so when the products directory has no ANTEX", async () => {
    seedStore({ productsDir: "/products", onboardingDone: true });
    mockStartup({ find_antex_file: null });
    render(<App />);

    await waitFor(() =>
      expect(store().workflowLines.join("\n")).toMatch(/No igs\*\.atx ANTEX file found/),
    );
  });
});

describe("the credentials prompt", () => {
  it("waits for the onboarding flag to be read before deciding", () => {
    mockStartup();
    render(<App />);
    // Pre-hydrate the flag is "unknown", which is not the same as "done".
    expect(invokeCount("validate_cddis_credentials")).toBe(0);
  });

  it("opens the modal and goes offline when the credentials are invalid", async () => {
    seedStore({ onboardingDone: true });
    mockStartup({ validate_cddis_credentials: { ok: false, why: "no netrc entry" } });
    render(<App />);

    await waitFor(() => expect(store().credentialsModalOpen).toBe(true));
    expect(store().offlineMode).toBe(true);
  });

  it("stays quiet when the credentials check passes", async () => {
    seedStore({ onboardingDone: true });
    mockStartup();
    render(<App />);

    await waitFor(() => expect(invokeCount("validate_cddis_credentials")).toBe(1));
    expect(store().credentialsModalOpen).toBe(false);
  });

  it("assumes the worst when the check itself cannot run", async () => {
    seedStore({ onboardingDone: true });
    mockStartup({
      validate_cddis_credentials: () => Promise.reject(new Error("command missing")),
    });
    render(<App />);

    await waitFor(() => expect(store().offlineMode).toBe(true));
  });

  it("does not ask twice when onboarding already asked", async () => {
    mockStartup();
    render(<App />);

    // First run: onboarding is active, so it owns the credentials question.
    await waitFor(() => expect(store().onboardingStatus).toBe("active"));
    await waitFor(() => {
      store().completeOnboarding();
    });

    expect(invokeCount("validate_cddis_credentials")).toBe(0);
  });
});

describe("the guided tour", () => {
  it("does not start one before onboarding is finished", async () => {
    mockStartup();
    render(<App />);

    await waitFor(() => expect(store().onboardingStatus).toBe("active"));
    await waitFor(() => {
      store().setRinexData(makeRinexData());
    });

    expect(store().activeTour).toBeNull();
  });

  it("starts the workspace tour the first time a file opens", async () => {
    seedStore({ onboardingDone: true });
    mockStartup();
    render(<App />);
    await waitFor(() => expect(store().onboardingStatus).toBe("done"));

    await waitFor(() => {
      store().setRinexData(makeRinexData());
    });

    await waitFor(() => expect(store().activeTour).toBe("workspace"));
  });

  it("starts the streaming tour the first time a live run is set up", async () => {
    seedStore({ onboardingDone: true });
    mockStartup();
    render(<App />);
    await waitFor(() => expect(store().onboardingStatus).toBe("done"));

    await waitFor(() => {
      store().setInputMode("realtime");
    });

    await waitFor(() => expect(store().activeTour).toBe("streaming"));
  });

  it("does not re-offer a tour the user has already completed", async () => {
    seedStore({ onboardingDone: true, completedTours: ["workspace"] });
    mockStartup();
    render(<App />);
    await waitFor(() => expect(store().completedTours).toEqual(["workspace"]));

    await waitFor(() => {
      store().setRinexData(makeRinexData());
    });

    expect(store().activeTour).toBeNull();
  });

  it("does not restart a tour the user walked out of this session", async () => {
    seedStore({ onboardingDone: true });
    mockStartup();
    render(<App />);
    await waitFor(() => expect(store().onboardingStatus).toBe("done"));
    await waitFor(() => {
      store().setRinexData(makeRinexData());
    });
    await waitFor(() => expect(store().activeTour).toBe("workspace"));

    await waitFor(() => {
      store().endTour();
    });

    expect(store().activeTour).toBeNull();
    // Dismissing records it as seen, so it never auto-starts again.
    expect(store().completedTours).toEqual(["workspace"]);
  });

  it("holds the tour back while the opening activity panel is up", async () => {
    seedStore({ onboardingDone: true });
    mockStartup();
    render(<App />);
    await waitFor(() => expect(store().onboardingStatus).toBe("done"));

    await waitFor(() => {
      useAppStore.setState({ rinexData: makeRinexData() });
      store().startActivity("Opening ALIC.rnx", [{ key: "parse", label: "Parse" }]);
    });

    expect(store().activeTour).toBeNull();
  });
});
