import { describe, expect, it } from "vitest";
import { useState } from "react";
import { render, screen } from "@testing-library/react";
import { TabBarActions, TabBarSlotContext } from "./TabBarSlot";

function Host({ children }: { children: React.ReactNode }) {
  const [slot, setSlot] = useState<HTMLDivElement | null>(null);
  return (
    <div>
      <div data-testid="slot" ref={setSlot} />
      <TabBarSlotContext.Provider value={slot}>
        <div data-testid="body">{children}</div>
      </TabBarSlotContext.Provider>
    </div>
  );
}

describe("TabBarActions", () => {
  it("renders in place outside a workspace", () => {
    render(
      <div data-testid="here">
        <TabBarActions>
          <button>Export</button>
        </TabBarActions>
      </div>,
    );
    expect(screen.getByTestId("here")).toContainElement(
      screen.getByRole("button", { name: "Export" }),
    );
  });

  it("portals its controls into the tab-bar slot once it exists", () => {
    render(
      <Host>
        <TabBarActions>
          <button>Export</button>
        </TabBarActions>
      </Host>,
    );
    const button = screen.getByRole("button", { name: "Export" });
    expect(screen.getByTestId("slot")).toContainElement(button);
    expect(screen.getByTestId("body")).not.toContainElement(button);
  });
});
