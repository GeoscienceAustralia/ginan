import { beforeEach, describe, expect, it } from "vitest";
import { act, render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Onboarding from "./Onboarding";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";
import { invokeCount, lastInvokeArgs, mockInvoke, open, type CommandStub } from "../test/tauriMocks";

beforeEach(() => {
  resetAppStore();
  useAppStore.setState({ onboardingStatus: "active" });
});

function mockBackend(over: Record<string, CommandStub> = {}) {
  mockInvoke({
    validate_cddis_credentials: { ok: false, why: "no netrc entry" },
    save_cddis_credentials: null,
    test_cddis_connection: null,
    list_ntrip_hosts: [],
    save_ntrip_credentials: null,
    ...over,
  });
}

const btn = (name: string | RegExp) => screen.getByRole("button", { name });
const currentStep = () =>
  within(screen.getByRole("list", { name: "Setup progress" }))
    .getAllByRole("listitem")
    .find((li) => li.getAttribute("aria-current") === "step")!;
const stepLabels = () =>
  within(screen.getByRole("list", { name: "Setup progress" }))
    .getAllByRole("listitem")
    .map((li) => li.textContent?.replace(/^\d+/, ""));
/** The "what will you process?" cards on the welcome step, by their title. */
const choice = (title: string) =>
  within(screen.getByRole("group", { name: "What will you process?" })).getByRole("button", {
    name: new RegExp(`^${title}`),
  });
const fieldFor = (label: string) =>
  screen.getByText(label, { selector: "label" }).parentElement!.querySelector("input")!;

describe("visibility", () => {
  it("renders nothing once onboarding is done", () => {
    useAppStore.setState({ onboardingStatus: "done" });
    render(<Onboarding />);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("renders nothing while the persisted flag is still unknown", () => {
    useAppStore.setState({ onboardingStatus: "unknown" });
    render(<Onboarding />);
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("opens on the welcome step with no Back button", () => {
    mockBackend();
    render(<Onboarding />);
    expect(screen.getByRole("dialog", { name: "Set up Ginan UI" })).toBeInTheDocument();
    expect(currentStep()).toHaveTextContent("Welcome");
    expect(screen.queryByRole("button", { name: /Back/ })).not.toBeInTheDocument();
  });
});

describe("the step list", () => {
  it("asks RINEX users for no caster account", () => {
    mockBackend();
    render(<Onboarding />);
    expect(stepLabels()).toEqual(["Welcome", "Credentials", "Products", "Finish"]);
  });

  it("adds the Livestream step once the user says they will stream", async () => {
    mockBackend();
    render(<Onboarding />);
    expect(choice("Files")).toHaveAttribute("aria-pressed", "true");

    await userEvent.click(choice("Real-time data stream"));

    expect(choice("Real-time data stream")).toHaveAttribute("aria-pressed", "true");
    expect(choice("Files")).toHaveAttribute("aria-pressed", "false");
    expect(stepLabels()).toEqual([
      "Welcome",
      "Credentials",
      "Real-time data stream",
      "Products",
      "Finish",
    ]);
  });
});

describe("walking through", () => {
  it("moves forward, back, and skips without saving anything", async () => {
    mockBackend();
    render(<Onboarding />);

    await userEvent.click(btn(/Get started/));
    expect(currentStep()).toHaveTextContent("Credentials");

    await userEvent.click(btn("Skip for now"));
    expect(currentStep()).toHaveTextContent("Products");

    await userEvent.click(btn(/Back/));
    expect(currentStep()).toHaveTextContent("Credentials");
    expect(invokeCount("save_cddis_credentials")).toBe(0);
  });

  it("finishes without the tour", async () => {
    mockBackend();
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
    await userEvent.click(btn(/Continue/));
    await userEvent.click(btn(/Continue/));

    await userEvent.click(btn("Skip the tour"));

    expect(store().onboardingStatus).toBe("done");
    expect(store().activeTour).toBeNull();
    expect(screen.queryByRole("dialog")).not.toBeInTheDocument();
  });

  it("finishes into the welcome tour", async () => {
    mockBackend();
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
    await userEvent.click(btn(/Continue/));
    await userEvent.click(btn(/Continue/));

    await userEvent.click(btn(/Show me around/));

    expect(store().onboardingStatus).toBe("done");
    expect(store().activeTour).toBe("welcome");
  });

  it("summarises what is and is not set up on the last step", async () => {
    mockBackend();
    useAppStore.setState({ offlineMode: true, productsDir: "" });
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
    await userEvent.click(btn(/Continue/));
    await userEvent.click(btn(/Continue/));

    expect(screen.getByText("Not signed in, so downloads won't work yet")).toBeInTheDocument();
    expect(screen.getByText("No download folder yet, pick one in Settings")).toBeInTheDocument();
  });

  it("summarises a complete setup on the last step", async () => {
    mockBackend({ validate_cddis_credentials: { ok: true, why: null } });
    useAppStore.setState({ productsDir: "/data/products" });
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
    await screen.findByText("You're signed in.");
    await userEvent.click(btn(/Continue/));
    await userEvent.click(btn(/Continue/));

    expect(screen.getByText("Signed in to NASA Earthdata")).toBeInTheDocument();
    expect(screen.getByText("Downloads go to /data/products")).toBeInTheDocument();
  });

  it("starts over at the welcome step when replayed from Settings", async () => {
    mockBackend();
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
    await userEvent.click(btn(/Continue/));
    await userEvent.click(btn(/Continue/));
    await userEvent.click(btn("Skip the tour"));

    act(() => store().restartOnboarding());

    expect(currentStep()).toHaveTextContent("Welcome");
  });
});

describe("the credentials step", () => {
  const goToCredentials = async () => {
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
  };

  it("recognises credentials that are already saved", async () => {
    mockBackend({ validate_cddis_credentials: { ok: true, why: null } });
    await goToCredentials();

    expect(await screen.findByText("You're signed in.")).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /Save & test/ })).not.toBeInTheDocument();
    expect(store().offlineMode).toBe(false);
  });

  it("goes offline when the check itself cannot run", async () => {
    mockBackend({
      validate_cddis_credentials: () => Promise.reject(new Error("command missing")),
    });
    await goToCredentials();

    await waitFor(() => expect(store().offlineMode).toBe(true));
    expect(btn(/Save & test/)).toBeInTheDocument();
  });

  it("needs both fields before it can save", async () => {
    mockBackend();
    await goToCredentials();
    const save = await screen.findByRole("button", { name: /Save & test/ });
    expect(save).toBeDisabled();

    await userEvent.type(fieldFor("Username"), "alice");
    expect(save).toBeDisabled();
    await userEvent.type(fieldFor("Password"), "secret");
    expect(save).toBeEnabled();
  });

  it("saves, tests and re-runs the product chain", async () => {
    mockBackend();
    await goToCredentials();
    await screen.findByRole("button", { name: /Save & test/ });
    const epoch = store().cddisEpoch;

    await userEvent.type(fieldFor("Username"), "alice");
    await userEvent.type(fieldFor("Password"), "secret");
    await userEvent.click(btn(/Save & test/));

    expect(await screen.findByText("You're signed in.")).toBeInTheDocument();
    expect(lastInvokeArgs("save_cddis_credentials")).toEqual({
      username: "alice",
      password: "secret",
    });
    expect(invokeCount("test_cddis_connection")).toBe(1);
    expect(store().offlineMode).toBe(false);
    expect(store().cddisEpoch).toBe(epoch + 1);
  });

  it("shows why a save failed and stays on the form", async () => {
    mockBackend({ test_cddis_connection: () => Promise.reject("401 Unauthorized") });
    await goToCredentials();
    await screen.findByRole("button", { name: /Save & test/ });

    await userEvent.type(fieldFor("Username"), "alice");
    await userEvent.type(fieldFor("Password"), "wrong");
    await userEvent.click(btn(/Save & test/));

    expect(await screen.findByText("401 Unauthorized")).toBeInTheDocument();
    expect(btn(/Save & test/)).toBeEnabled();
  });

  it("links to the Earthdata login and sign-up pages", async () => {
    mockBackend();
    await goToCredentials();
    expect(await screen.findByRole("link", { name: "urs.earthdata.nasa.gov" })).toHaveAttribute(
      "href",
      "https://urs.earthdata.nasa.gov/",
    );
    expect(
      screen.getByRole("link", { name: "urs.earthdata.nasa.gov/users/new" }),
    ).toHaveAttribute("href", "https://urs.earthdata.nasa.gov/users/new");
  });
});

describe("the livestream step", () => {
  it("embeds the caster login form", async () => {
    mockBackend();
    render(<Onboarding />);
    await userEvent.click(choice("Real-time data stream"));
    await userEvent.click(btn(/Get started/));
    await userEvent.click(btn("Skip for now"));

    expect(currentStep()).toHaveTextContent("Real-time data stream");
    expect(screen.getByRole("heading", { name: "Real-time data stream credentials" })).toBeInTheDocument();
    expect(btn("Save credentials")).toBeInTheDocument();
  });
});

describe("the products step", () => {
  it("picks a products folder", async () => {
    mockBackend();
    open.mockResolvedValueOnce("/data/products");
    render(<Onboarding />);
    await userEvent.click(btn(/Get started/));
    await userEvent.click(btn("Skip for now"));
    expect(screen.getByRole("heading", { name: "Choose a products folder" })).toBeInTheDocument();
    expect(screen.getByText(/won't be able to process anything until you do/)).toBeInTheDocument();

    await userEvent.click(btn("Browse..."));

    await waitFor(() => expect(store().productsDir).toBe("/data/products"));
    expect(screen.getByText("Downloads will be saved here.")).toBeInTheDocument();
  });
});
