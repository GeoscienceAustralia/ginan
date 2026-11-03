import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { act, render, screen } from "@testing-library/react";
import StatusBar from "./StatusBar";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

const spinner = () => document.querySelector(".spinner");

describe("StatusBar", () => {
  it("shows the plain status line, without a spinner, when nothing is in flight", () => {
    store().setProductsDir("/products");
    store().setStatus("Config exported: ppp_config.yaml");
    render(<StatusBar />);
    expect(screen.getByText("Config exported: ppp_config.yaml")).toBeInTheDocument();
    expect(spinner()).toBeNull();
  });

  it("shows the live run status with a spinner, outranking everything else", () => {
    useAppStore.setState({
      isProcessing: true,
      status: "Running PEA...",
      cddisStatus: "error",
      cddisStatusMessage: "bad credentials",
    });
    render(<StatusBar />);
    expect(screen.getByText("Running PEA...")).toBeInTheDocument();
    expect(spinner()).not.toBeNull();
  });

  it("reports the CDDIS check while it runs", () => {
    store().setCddisStatus("checking");
    render(<StatusBar />);
    expect(screen.getByText("Checking CDDIS...")).toBeInTheDocument();
    expect(spinner()).not.toBeNull();
  });

  it("reports a CDDIS failure with its reason, and does not spin", () => {
    store().setCddisStatus("error", "401 Unauthorized");
    render(<StatusBar />);
    expect(screen.getByText("CDDIS: 401 Unauthorized")).toBeInTheDocument();
    expect(spinner()).toBeNull();
  });

  it("falls back to a generic reason when the failure carried no message", () => {
    store().setCddisStatus("error");
    render(<StatusBar />);
    expect(
      screen.getByText("CDDIS: credentials missing or invalid"),
    ).toBeInTheDocument();
  });

  it("asks for a products directory instead of saying 'Ready' with none set", () => {
    render(<StatusBar />);
    expect(
      screen.getByText("Set a products directory in Settings"),
    ).toBeInTheDocument();
  });

  it("never replaces a real message with the products-directory hint", () => {
    store().setStatus("Pea binary unavailable: network error");
    render(<StatusBar />);
    expect(
      screen.getByText("Pea binary unavailable: network error"),
    ).toBeInTheDocument();
  });

  it("goes back to 'Ready' once a products directory exists", () => {
    store().setProductsDir("/products");
    render(<StatusBar />);
    expect(screen.getByText("Ready")).toBeInTheDocument();
  });

  it("carries the tour anchor the guided tour points at", () => {
    const { container } = render(<StatusBar />);
    expect(container.querySelector('[data-tour="status-bar"]')).not.toBeNull();
  });
});

describe("StatusBar during a stream test", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("counts the 10 s watch with a progress bar, then says it is finishing, and stops when the test ends", () => {
    store().setProductsDir("/products");
    store().setStatus("Testing streams...");
    act(() => store().setStreamTestState({ status: "running" }));
    render(<StatusBar />);
    expect(screen.getByText("Monitoring streams... 0 s of 10 s")).toBeInTheDocument();
    expect(spinner()).not.toBeNull();
    expect(screen.getByRole("progressbar", { name: "Stream test" })).toHaveAttribute("aria-valuenow", "0");

    act(() => vi.advanceTimersByTime(4_000));
    expect(screen.getByText("Monitoring streams... 4 s of 10 s")).toBeInTheDocument();
    expect(screen.getByRole("progressbar")).toHaveAttribute("aria-valuenow", "4");

    act(() => vi.advanceTimersByTime(8_000));
    expect(screen.getByText("Monitoring streams... finishing (12 s)")).toBeInTheDocument();

    act(() => {
      store().setStreamTestState({ status: "passed", at: Date.now() });
      store().setStatus("Ready");
    });
    expect(screen.queryByRole("progressbar")).toBeNull();
    expect(screen.queryByText(/Monitoring streams/)).toBeNull();
    expect(spinner()).toBeNull();
  });

  it("outranks a run's own status while Process is testing the streams first", () => {
    useAppStore.setState({ isProcessing: true, status: "Testing streams..." });
    act(() => store().setStreamTestState({ status: "running" }));
    render(<StatusBar />);
    expect(screen.getByText("Monitoring streams... 0 s of 10 s")).toBeInTheDocument();
  });
});
