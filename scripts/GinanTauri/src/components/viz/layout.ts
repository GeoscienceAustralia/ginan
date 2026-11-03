/**
 * Tiling layout model for the Visualization tab.
 *
 * The layout is a tree: every leaf is one panel (a figure), every inner node
 * lays its children out in a row or a column with fractional sizes. N-ary
 * splits rather than binary ones, so "three columns" is one node with three
 * equal children instead of a lopsided nest — which is also what makes the
 * dividers behave the way a user expects when dragged.
 *
 * Everything here is pure: each operation takes a tree and returns a new one,
 * so the React side is a single `useState` and undo-by-reload is free.
 */

export type PanelKind =
  | "timeseries"
  | "scatter"
  | "map"
  | "error"
  | "sigma"
  | "levels"
  | "histogram"
  | "cdf"
  | "stats"
  | "summary";

export const PANEL_KINDS: PanelKind[] = [
  "timeseries",
  "scatter",
  "map",
  "error",
  "sigma",
  "levels",
  "histogram",
  "cdf",
  "summary",
  "stats",
];

/** Per-panel settings. Each figure reads the keys it understands. */
export type PanelOptions = Record<string, unknown>;

export interface PanelNode {
  type: "panel";
  id: string;
  kind: PanelKind;
  options: PanelOptions;
}

export interface SplitNode {
  type: "split";
  id: string;
  /** "row" lays children left→right, "column" top→bottom. */
  dir: "row" | "column";
  children: LayoutNode[];
  /** Fractions of the parent, same length as `children`, summing to 1. */
  sizes: number[];
}

export type LayoutNode = PanelNode | SplitNode;

/** Where a dragged panel lands relative to the panel it is dropped on. */
export type DropSide = "left" | "right" | "top" | "bottom" | "center";

/** No pane may be dragged smaller than this fraction of its split. */
const MIN_FRACTION = 0.08;

let counter = 0;
function newId(prefix = "p"): string {
  counter += 1;
  return `${prefix}${Date.now().toString(36)}${counter.toString(36)}`;
}

export function panel(kind: PanelKind, options: PanelOptions = {}): PanelNode {
  return { type: "panel", id: newId("p"), kind, options };
}

export function split(dir: "row" | "column", children: LayoutNode[], sizes?: number[]): SplitNode {
  return {
    type: "split",
    id: newId("s"),
    dir,
    children,
    sizes: sizes ?? children.map(() => 1 / children.length),
  };
}

// ── Queries ──────────────────────────────────────────────────────────────────

export function findPanel(node: LayoutNode, id: string): PanelNode | null {
  if (node.type === "panel") return node.id === id ? node : null;
  for (const c of node.children) {
    const hit = findPanel(c, id);
    if (hit) return hit;
  }
  return null;
}

// ── Edits ────────────────────────────────────────────────────────────────────

/** Replace the panel `id` with whatever `fn` returns (a panel or a split). */
function mapPanel(node: LayoutNode, id: string, fn: (p: PanelNode) => LayoutNode): LayoutNode {
  if (node.type === "panel") return node.id === id ? fn(node) : node;
  let changed = false;
  const children = node.children.map((c) => {
    const next = mapPanel(c, id, fn);
    if (next !== c) changed = true;
    return next;
  });
  return changed ? normalize({ ...node, children }) : node;
}

export function updatePanel(root: LayoutNode, id: string, patch: Partial<Omit<PanelNode, "type" | "id">>): LayoutNode {
  return mapPanel(root, id, (p) => ({ ...p, ...patch }));
}

export function updatePanelOptions(root: LayoutNode, id: string, patch: PanelOptions): LayoutNode {
  return mapPanel(root, id, (p) => ({ ...p, options: { ...p.options, ...patch } }));
}

/**
 * Put `incoming` beside the panel `targetId` on `side`. When the target
 * already sits in a split running the same way, the new panel joins that
 * split and takes half of the target's share; otherwise the target is wrapped
 * in a fresh two-way split. "center" swaps nothing here — see `movePanel`.
 */
export function insertBeside(
  root: LayoutNode,
  targetId: string,
  incoming: PanelNode,
  side: Exclude<DropSide, "center">,
): LayoutNode {
  const dir = side === "left" || side === "right" ? "row" : "column";
  const before = side === "left" || side === "top";

  const visit = (node: LayoutNode): LayoutNode => {
    if (node.type === "panel") {
      if (node.id !== targetId) return node;
      return split(dir, before ? [incoming, node] : [node, incoming]);
    }
    const idx = node.children.findIndex((c) => c.type === "panel" && c.id === targetId);
    if (idx >= 0 && node.dir === dir) {
      const children = [...node.children];
      const sizes = [...node.sizes];
      const half = sizes[idx] / 2;
      const at = before ? idx : idx + 1;
      sizes[idx] = half;
      children.splice(at, 0, incoming);
      sizes.splice(at, 0, half);
      return { ...node, children, sizes };
    }
    let changed = false;
    const children = node.children.map((c) => {
      const next = visit(c);
      if (next !== c) changed = true;
      return next;
    });
    return changed ? normalize({ ...node, children }) : node;
  };

  return visit(root);
}

/** Remove panel `id`. Returns null if it was the last one. */
export function removePanel(root: LayoutNode, id: string): LayoutNode | null {
  if (root.type === "panel") return root.id === id ? null : root;
  const visit = (node: SplitNode): LayoutNode => {
    const children: LayoutNode[] = [];
    const sizes: number[] = [];
    let freed = 0;
    node.children.forEach((c, i) => {
      if (c.type === "panel" && c.id === id) {
        freed += node.sizes[i];
        return;
      }
      children.push(c.type === "split" ? visit(c) : c);
      sizes.push(node.sizes[i]);
    });
    // Hand the removed pane's share to its siblings in proportion, so the
    // layout around it doesn't jump.
    const total = sizes.reduce((a, b) => a + b, 0) || 1;
    const scaled = sizes.map((s) => s + (freed * s) / total);
    return normalize({ ...node, children, sizes: scaled });
  };
  return visit(root);
}

/**
 * Move panel `srcId` next to (or onto) `targetId`. Dropping on the centre
 * swaps the two panels' contents, which is how a user rearranges a full
 * layout without first making room.
 */
export function movePanel(root: LayoutNode, srcId: string, targetId: string, side: DropSide): LayoutNode {
  if (srcId === targetId) return root;
  const src = findPanel(root, srcId);
  const target = findPanel(root, targetId);
  if (!src || !target) return root;

  if (side === "center") {
    // Swap content, keep ids with positions so React keeps each slot stable.
    let next = updatePanel(root, srcId, { kind: target.kind, options: target.options });
    next = updatePanel(next, targetId, { kind: src.kind, options: src.options });
    return next;
  }

  const without = removePanel(root, srcId);
  if (!without) return root;
  return insertBeside(without, targetId, src, side);
}

/** Add a panel at the edge of the whole layout. */
export function addAtEdge(root: LayoutNode | null, incoming: PanelNode, side: Exclude<DropSide, "center"> = "right"): LayoutNode {
  if (!root) return incoming;
  const dir = side === "left" || side === "right" ? "row" : "column";
  const before = side === "left" || side === "top";
  if (root.type === "split" && root.dir === dir) {
    const n = root.children.length + 1;
    const share = 1 / n;
    const sizes = root.sizes.map((s) => s * (1 - share));
    const children = before ? [incoming, ...root.children] : [...root.children, incoming];
    return { ...root, children, sizes: before ? [share, ...sizes] : [...sizes, share] };
  }
  return split(dir, before ? [incoming, root] : [root, incoming]);
}

/**
 * Drag the divider between children `index` and `index + 1` of split
 * `splitId` so the leading edge sits at `fraction` of the pair's combined
 * extent. Both panes are clamped to MIN_FRACTION of the split.
 */
export function resizeSplit(root: LayoutNode, splitId: string, index: number, fraction: number): LayoutNode {
  const visit = (node: LayoutNode): LayoutNode => {
    if (node.type === "panel") return node;
    if (node.id === splitId) {
      const sizes = [...node.sizes];
      const pair = sizes[index] + sizes[index + 1];
      const min = Math.min(MIN_FRACTION, pair / 2);
      const a = Math.min(pair - min, Math.max(min, fraction * pair));
      sizes[index] = a;
      sizes[index + 1] = pair - a;
      return { ...node, sizes };
    }
    let changed = false;
    const children = node.children.map((c) => {
      const next = visit(c);
      if (next !== c) changed = true;
      return next;
    });
    return changed ? { ...node, children } : node;
  };
  return visit(root);
}

/** Equalise every split's children — the "tidy up" action. */
export function equalize(node: LayoutNode): LayoutNode {
  if (node.type === "panel") return node;
  return {
    ...node,
    children: node.children.map(equalize),
    sizes: node.children.map(() => 1 / node.children.length),
  };
}

/**
 * Collapse single-child splits, flatten a split nested directly inside one
 * running the same direction, and renormalise sizes to sum to 1.
 */
function normalize(node: SplitNode): LayoutNode {
  if (node.children.length === 0) return node;
  if (node.children.length === 1) return node.children[0];

  const children: LayoutNode[] = [];
  const sizes: number[] = [];
  node.children.forEach((c, i) => {
    if (c.type === "split" && c.dir === node.dir) {
      c.children.forEach((cc, j) => {
        children.push(cc);
        sizes.push(node.sizes[i] * c.sizes[j]);
      });
    } else {
      children.push(c);
      sizes.push(node.sizes[i]);
    }
  });
  const total = sizes.reduce((a, b) => a + b, 0) || 1;
  return { ...node, children, sizes: sizes.map((s) => s / total) };
}

// ── Persistence ──────────────────────────────────────────────────────────────

/**
 * Validates a tree read back from storage. Anything malformed — an older
 * shape, a hand-edited value, a panel kind this build no longer has — returns
 * null and the caller falls back to a preset, rather than rendering garbage.
 */
export function parseLayout(raw: unknown): LayoutNode | null {
  const seen = new Set<string>();
  const visit = (v: unknown): LayoutNode | null => {
    if (!v || typeof v !== "object") return null;
    const o = v as Record<string, unknown>;
    if (typeof o.id !== "string" || seen.has(o.id)) return null;
    seen.add(o.id);
    if (o.type === "panel") {
      if (!PANEL_KINDS.includes(o.kind as PanelKind)) return null;
      const options = o.options && typeof o.options === "object" ? (o.options as PanelOptions) : {};
      return { type: "panel", id: o.id, kind: o.kind as PanelKind, options };
    }
    if (o.type === "split") {
      if (o.dir !== "row" && o.dir !== "column") return null;
      if (!Array.isArray(o.children) || !Array.isArray(o.sizes)) return null;
      if (o.children.length < 2 || o.children.length !== o.sizes.length) return null;
      const children: LayoutNode[] = [];
      for (const c of o.children) {
        const parsed = visit(c);
        if (!parsed) return null;
        children.push(parsed);
      }
      const sizes = o.sizes.map(Number);
      if (sizes.some((s) => !Number.isFinite(s) || s <= 0)) return null;
      const total = sizes.reduce((a, b) => a + b, 0);
      return { type: "split", id: o.id, dir: o.dir, children, sizes: sizes.map((s) => s / total) };
    }
    return null;
  };
  return visit(raw);
}

// ── Presets ──────────────────────────────────────────────────────────────────

export interface LayoutPreset {
  key: string;
  label: string;
  description: string;
  build: () => LayoutNode;
}

export const PRESETS: LayoutPreset[] = [
  {
    key: "overview",
    label: "Overview",
    description: "Time series and scatter above, summary below",
    // Three panels, not four: at the app's default window size a fourth left
    // every tile too small to read. Statistics is one "Add view" away.
    build: () =>
      split(
        "column",
        [split("row", [panel("timeseries"), panel("scatter")], [0.62, 0.38]), panel("summary")],
        [0.68, 0.32],
      ),
  },
  {
    key: "single",
    label: "Single",
    description: "One figure filling the tab",
    build: () => panel("timeseries"),
  },
  {
    key: "side-by-side",
    label: "Side by side",
    description: "Two figures in columns",
    build: () => split("row", [panel("timeseries"), panel("scatter")]),
  },
  {
    key: "stacked",
    label: "Stacked",
    description: "Two figures in rows",
    build: () => split("column", [panel("timeseries"), panel("error")]),
  },
  {
    key: "grid",
    label: "2 × 2 grid",
    description: "Four figures in a grid",
    build: () =>
      split("column", [
        split("row", [panel("timeseries"), panel("scatter")]),
        split("row", [panel("error"), panel("histogram")]),
      ]),
  },
  {
    key: "accuracy",
    label: "Accuracy",
    description: "Error magnitude, its distribution and the headline numbers",
    build: () =>
      split(
        "row",
        [
          split("column", [panel("error"), panel("cdf")]),
          split("column", [panel("summary"), panel("histogram", { component: "du" })], [0.45, 0.55]),
        ],
        [0.6, 0.4],
      ),
  },
  {
    key: "convergence",
    label: "Convergence",
    description: "Formal error and residuals over time, time axes linked",
    build: () =>
      split("column", [panel("sigma"), panel("timeseries", { showSigma: false }), panel("error")]),
  },
  {
    key: "map",
    label: "Map focus",
    description: "Large map with the time series and summary beside it",
    build: () =>
      split("row", [panel("map"), split("column", [panel("timeseries"), panel("summary")], [0.6, 0.4])], [0.58, 0.42]),
  },
];

export const DEFAULT_PRESET = PRESETS[0];
