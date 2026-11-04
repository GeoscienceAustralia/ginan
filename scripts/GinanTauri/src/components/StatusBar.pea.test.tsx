import { beforeEach, describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import StatusBar from "./StatusBar";
import { useAppStore } from "../store/appStore";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

const pill = () => screen.queryByRole("button", { name: /available →/ });

describe("the pea update pill", () => {
  it("is absent when there is no update", () => {
    render(<StatusBar />);
    expect(pill()).not.toBeInTheDocument();
  });

  it("reopens the update prompt after it was dismissed", async () => {
    useAppStore.setState({ peaUpdateAvailable: "v4.2.0", peaUpdateModalOpen: false });
    render(<StatusBar />);

    expect(pill()).toHaveTextContent("pea v4.2.0 available →");
    await userEvent.click(pill()!);

    expect(store().peaUpdateModalOpen).toBe(true);
  });

  it("stays out of the way during a run", () => {
    useAppStore.setState({ peaUpdateAvailable: "v4.2.0", isProcessing: true });
    render(<StatusBar />);
    expect(pill()).not.toBeInTheDocument();
  });

  it("sits beside the status line rather than replacing it", () => {
    useAppStore.setState({
      peaUpdateAvailable: "v4.2.0",
      productsDir: "/products",
      cddisStatus: "ok",
      status: "pea v4.1.0 ready - v4.2.0 available",
    });
    render(<StatusBar />);
    expect(screen.getByText("pea v4.1.0 ready - v4.2.0 available")).toBeInTheDocument();
    expect(pill()).toBeInTheDocument();
  });
});
