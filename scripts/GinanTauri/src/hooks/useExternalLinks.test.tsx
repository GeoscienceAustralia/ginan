import { beforeEach, describe, expect, it, vi } from "vitest";
import { render, waitFor } from "@testing-library/react";
import { useExternalLinks } from "./useExternalLinks";
import { invokeCount, lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

function Harness({ children }: { children?: React.ReactNode }) {
  useExternalLinks();
  return <div>{children}</div>;
}

beforeEach(() => {
  mockInvoke({ open_external: null });
});

/** Click an element and report whether the webview's navigation was cancelled. */
function click(el: Element, init: MouseEventInit = {}) {
  const ev = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, ...init });
  // Record the outcome at the end of the bubble phase, then cancel whatever is
  // left so jsdom does not attempt (and loudly fail) a real navigation.
  let prevented = false;
  const settle = (e: Event) => {
    prevented = e.defaultPrevented;
    e.preventDefault();
  };
  window.addEventListener("click", settle, { once: true });
  el.dispatchEvent(ev);
  window.removeEventListener("click", settle);
  return prevented;
}

describe("useExternalLinks", () => {
  it("hands an outbound link to the system browser instead of navigating", async () => {
    const { getByText } = render(
      <Harness>
        <a href="https://urs.earthdata.nasa.gov/">Earthdata</a>
      </Harness>,
    );

    expect(click(getByText("Earthdata"))).toBe(true);
    await waitFor(() => expect(invokeCount("open_external")).toBe(1));
    expect(lastInvokeArgs("open_external")).toEqual({ url: "https://urs.earthdata.nasa.gov/" });
  });

  it("catches a click on something nested inside the anchor", () => {
    const { getByText } = render(
      <Harness>
        <a href="http://example.com/">
          <span>inner</span>
        </a>
      </Harness>,
    );
    expect(click(getByText("inner"))).toBe(true);
    expect(invokeCount("open_external")).toBe(1);
  });

  it.each([
    ["#section", "an in-page anchor"],
    ["/local/page", "a relative link"],
    ["mailto:a@b.c", "a non-http scheme"],
  ])("leaves %s alone (%s)", (href) => {
    const { getByText } = render(
      <Harness>
        <a href={href}>link</a>
      </Harness>,
    );
    click(getByText("link"));
    expect(invokeCount("open_external")).toBe(0);
  });

  it.each([
    { ctrlKey: true },
    { metaKey: true },
    { shiftKey: true },
    { altKey: true },
    { button: 1 },
  ])("leaves a modified click alone: %o", (mods) => {
    const { getByText } = render(
      <Harness>
        <a href="https://example.com/">link</a>
      </Harness>,
    );
    expect(click(getByText("link"), mods)).toBe(false);
    expect(invokeCount("open_external")).toBe(0);
  });

  it("respects a click something earlier in the capture phase already handled", () => {
    const { getByText } = render(
      <Harness>
        <a href="https://example.com/">link</a>
      </Harness>,
    );
    // Window capture listeners run before the hook's document capture listener.
    const handled = (e: Event) => e.preventDefault();
    window.addEventListener("click", handled, true);
    try {
      click(getByText("link"));
    } finally {
      window.removeEventListener("click", handled, true);
    }
    expect(invokeCount("open_external")).toBe(0);
  });

  it("logs rather than throws when the browser cannot be opened", async () => {
    mockInvoke({ open_external: () => Promise.reject(new Error("no browser")) });
    const err = vi.spyOn(console, "error").mockImplementation(() => {});
    const { getByText } = render(
      <Harness>
        <a href="https://example.com/">link</a>
      </Harness>,
    );
    click(getByText("link"));
    await waitFor(() =>
      expect(err).toHaveBeenCalledWith("open_external failed:", expect.any(Error)),
    );
  });

  it("stops listening when unmounted", () => {
    const { unmount } = render(<Harness />);
    unmount();
    const a = document.createElement("a");
    a.href = "https://example.com/";
    document.body.appendChild(a);
    const prevented = click(a);
    a.remove();
    expect(prevented).toBe(false);
    expect(invokeCount("open_external")).toBe(0);
  });
});
