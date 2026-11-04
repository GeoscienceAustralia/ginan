import { createContext, useContext, type ReactNode } from "react";
import { createPortal } from "react-dom";

/**
 * A slot at the right-hand end of the workspace tab bar, so the active tab
 * can put its own controls on the same line as the tabs instead of spending
 * a row of its body on them. The tab renders into it with `<TabBarActions>`;
 * React context still flows through the portal, so those controls behave as
 * if they were rendered in place.
 */
export const TabBarSlotContext = createContext<HTMLElement | null>(null);

export function TabBarActions({ children }: { children: ReactNode }) {
  const slot = useContext(TabBarSlotContext);
  // Before the slot has mounted (first render) or outside a Workspace, fall
  // back to rendering in place rather than dropping the controls.
  return slot ? createPortal(children, slot) : <>{children}</>;
}
