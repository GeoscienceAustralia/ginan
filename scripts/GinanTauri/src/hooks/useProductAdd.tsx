import { useState, type ReactNode } from "react";
import { ConfirmDialog } from "../components/ModalDialog";
import { defaultInputProducts, useAppStore } from "../store/appStore";
import type { InputSection, ProductType } from "../types";

/**
 * Run mode follows the Observation products: a File product is a file run, any
 * live product (ntrip / serial / http) is a realtime run. `inputMode` stays a
 * store field, and these two functions are the only ways products move it, so
 * it cannot disagree with what the cards show. Both go through `setInputMode`,
 * which clears the other side (the parsed RINEX, or the live setup).
 *
 * Removing the last card of the current kind does not change the mode: an empty
 * file run says "No file selected", an empty live run is refused by Process
 * ("at least one observation stream"). Only adding a product of the other kind
 * switches, and that asks first (see `useProductAdd`).
 */

/** Live run -> file run: the live products go, an empty File card appears. */
export function switchToFile() {
  useAppStore.getState().setInputMode("file");
}

/**
 * File run -> live run, starting from the default live products. With `first`,
 * Observation holds just that new (empty) product instead of the default
 * streams, so the product the user asked for is not buried beside another one.
 */
export function switchToLive(first?: ProductType) {
  const s = useAppStore.getState();
  if (s.isProcessing) return;
  s.setInputMode("realtime");
  const d = defaultInputProducts();
  s.replaceSection("corr", d.corr);
  s.replaceSection("obs", first ? [] : d.obs);
  if (first) s.addProduct("obs", first);
}

/**
 * The add handler, enabled types and confirm dialog for one section's plus
 * menu. Adding a product of the other run kind opens `dialog` (render it
 * anywhere); confirming switches, cancelling changes nothing.
 *
 * - File is enabled in Observation only. In a file run it opens `onPickFile`,
 *   since a file run holds exactly one file card.
 * - In a file run the live types are blocked in Corrections, not switched:
 *   Corrections stay the Products-section files.
 * - While PEA runs, the types that would switch run kind are greyed, and
 *   `disabledReason` says why.
 */
export function useProductAdd(
  section: InputSection,
  liveTypes: ProductType[],
  onPickFile?: () => void,
): {
  onAdd: (type: ProductType) => void;
  enabledTypes: ProductType[];
  disabledReason?: string;
  dialog: ReactNode;
} {
  const fileRun = useAppStore((s) => s.inputMode === "file");
  const processing = useAppStore((s) => s.isProcessing);
  const addProduct = useAppStore((s) => s.addProduct);
  const [pending, setPending] = useState<ProductType | null>(null);

  // Crossing = a File product in a live run, or a live product in a file run.
  const crosses = (type: ProductType) => (type === "file") !== fileRun;

  const sectionTypes: ProductType[] =
    section === "obs" ? [...liveTypes, "file"] : fileRun ? [] : liveTypes;
  const enabledTypes = sectionTypes.filter((type) => !(processing && crosses(type)));

  const apply = (type: ProductType) => {
    if (type === "file") {
      if (fileRun) onPickFile?.();
      else switchToFile();
    } else if (fileRun) switchToLive(type);
    else addProduct(section, type);
  };

  const onAdd = (type: ProductType) => {
    if (crosses(type)) setPending(type);
    else apply(type);
  };

  const dialog = pending && (
    <ConfirmDialog
      title={pending === "file" ? "Switch to a file run?" : "Switch to a live run?"}
      message={
        pending === "file"
          ? "A file run reads one RINEX file instead of real-time data streams. Adding a file clears your real-time data stream setup."
          : "A live run reads streams instead of a RINEX file. Adding a stream closes the loaded file and clears its settings."
      }
      confirmLabel={pending === "file" ? "Switch to file run" : "Switch to live run"}
      danger
      onConfirm={() => {
        apply(pending);
        setPending(null);
      }}
      onCancel={() => setPending(null)}
    />
  );

  const disabledReason = processing
    ? "Stop processing to switch between a file run and a live run"
    : undefined;

  return { onAdd, enabledTypes, disabledReason, dialog };
}
