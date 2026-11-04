/**
 * Hover/focus-only field guidance — reachable but not permanently on screen.
 * `title` for the pointer, `aria-label` on an `img`-role span for a
 * screen reader, `tabIndex` so it is reachable without a mouse.
 */
export default function InfoTip({ text }: { text: string }) {
  return (
    <span className="rt-info" title={text} role="img" aria-label={text} tabIndex={0}>
      ?
    </span>
  );
}
