import type { InputProduct, InputSection } from "../types";

/** What every per-type card body receives. */
export interface StreamBodyProps {
  section: InputSection;
  product: InputProduct;
  onChange: (patch: Partial<Pick<InputProduct, "value" | "serial">>) => void;
  /** The user has committed a choice; the card collapses. */
  onDone?: () => void;
}
