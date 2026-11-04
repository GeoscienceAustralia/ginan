import { afterEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ProductContextMenu, { type ProductContextMenuProps } from "./ProductContextMenu";

afterEach(() => {
  vi.restoreAllMocks();
});

function renderMenu(overrides: Partial<ProductContextMenuProps> = {}) {
  const props: ProductContextMenuProps = {
    x: 120,
    y: 80,
    onRemove: vi.fn(),
    onClose: vi.fn(),
    ...overrides,
  };
  const view = render(<ProductContextMenu {...props} />);
  return { props, ...view };
}

const removeItem = () => screen.getByRole("menuitem", { name: "Remove product" });

describe("ProductContextMenu", () => {
  it("renders a named menu with Remove product as its only item, focused", () => {
    renderMenu();
    expect(screen.getByRole("menu", { name: "Product options" })).toBeInTheDocument();
    expect(screen.getAllByRole("menuitem")).toHaveLength(1);
    expect(removeItem()).toBeEnabled();
    expect(removeItem()).toHaveFocus();
  });

  it("clicking Remove product calls onRemove and onClose once each", async () => {
    const user = userEvent.setup();
    const { props } = renderMenu();
    await user.click(removeItem());
    expect(props.onRemove).toHaveBeenCalledTimes(1);
    expect(props.onClose).toHaveBeenCalledTimes(1);
  });

  it("Escape calls onClose", async () => {
    const user = userEvent.setup();
    const { props } = renderMenu();
    await user.keyboard("{Escape}");
    expect(props.onClose).toHaveBeenCalledTimes(1);
  });

  it("mousedown outside the menu calls onClose", async () => {
    const user = userEvent.setup();
    const { props } = renderMenu();
    await user.click(document.body);
    expect(props.onClose).toHaveBeenCalledTimes(1);
  });

  it("mousedown inside the menu does not call onClose", async () => {
    const user = userEvent.setup();
    const { props } = renderMenu();
    await user.click(screen.getByRole("menu", { name: "Product options" }));
    expect(props.onClose).not.toHaveBeenCalled();
  });

  it("closes when the window loses focus", () => {
    const { props } = renderMenu();
    fireEvent(window, new Event("blur"));
    expect(props.onClose).toHaveBeenCalledTimes(1);
  });

  it("closes on any scroll", () => {
    const { props } = renderMenu();
    fireEvent.scroll(document.body);
    expect(props.onClose).toHaveBeenCalledTimes(1);
  });

  it("returns focus to the product bar when unmounted", () => {
    const bar = render(<button type="button">Product bar</button>).getByRole("button", {
      name: "Product bar",
    });
    const menu = render(
      <ProductContextMenu x={120} y={80} returnFocusTo={bar} onRemove={vi.fn()} onClose={vi.fn()} />,
    );
    expect(removeItem()).toHaveFocus();
    menu.unmount();
    expect(bar).toHaveFocus();
  });

  it("opens at the pointer position when it fits in the viewport", () => {
    renderMenu({ x: 120, y: 80 });
    const menu = screen.getByRole("menu", { name: "Product options" });
    expect(menu.style.left).toBe("120px");
    expect(menu.style.top).toBe("80px");
  });

  it("moves back inside the viewport when the pointer is too close to the right and bottom edges", () => {
    // jsdom lays out nothing, so give the menu a size and the window a size.
    vi.spyOn(HTMLElement.prototype, "getBoundingClientRect").mockReturnValue({
      width: 200,
      height: 100,
    } as DOMRect);
    vi.stubGlobal("innerWidth", 1024);
    vi.stubGlobal("innerHeight", 768);
    try {
      renderMenu({ x: 900, y: 700 });
      const menu = screen.getByRole("menu", { name: "Product options" });
      expect(menu.style.left).toBe(`${1024 - 200 - 4}px`);
      expect(menu.style.top).toBe(`${768 - 100 - 4}px`);
    } finally {
      vi.unstubAllGlobals();
    }
  });
});
