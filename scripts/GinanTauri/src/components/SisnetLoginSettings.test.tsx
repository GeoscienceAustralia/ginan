import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import SisnetLoginSettings from "./SisnetLoginSettings";
import { resetAppStore, store } from "../test/storeHelpers";
import { lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

beforeEach(resetAppStore);

describe("SisnetLoginSettings", () => {
  it("says when no login is saved, and saves one to the SISNet host, never to the store", async () => {
    mockInvoke({ list_ntrip_hosts: [], save_ntrip_credentials: null });
    render(<SisnetLoginSettings />);
    expect(await screen.findByText("No login saved")).toBeInTheDocument();

    await userEvent.click(screen.getByRole("button", { name: "Add login..." }));
    await userEvent.type(screen.getByPlaceholderText("Username"), "simon");
    await userEvent.type(screen.getByPlaceholderText("Password"), "s3cret");
    mockInvoke({ list_ntrip_hosts: ["sisnet.data.gnss.ga.gov.au"], save_ntrip_credentials: null });
    await userEvent.click(screen.getByRole("button", { name: "Save" }));

    expect(lastInvokeArgs("save_ntrip_credentials")).toEqual({
      host: "sisnet.data.gnss.ga.gov.au",
      username: "simon",
      password: "s3cret",
    });
    await waitFor(() => expect(screen.getByText("Saved for sisnet.data.gnss.ga.gov.au")).toBeInTheDocument());
    expect(screen.getByRole("button", { name: "Change login..." })).toBeInTheDocument();
    expect(JSON.stringify(store())).not.toContain("s3cret");
  });
});
