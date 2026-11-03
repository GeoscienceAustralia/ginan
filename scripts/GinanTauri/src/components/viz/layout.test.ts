import { describe, expect, it } from "vitest";
import {
  DEFAULT_PRESET,
  PANEL_KINDS,
  PRESETS,
  addAtEdge,
  equalize,
  findPanel,
  insertBeside,
  movePanel,
  panel,
  parseLayout,
  removePanel,
  resizeSplit,
  split,
  updatePanel,
  updatePanelOptions,
  type LayoutNode,
  type PanelNode,
  type SplitNode,
} from "./layout";

/** Mirrors the private floor in layout.ts: no pane may shrink below this. */
const MIN_FRACTION = 0.08;

function countPanels(node: LayoutNode): number {
  return node.type === "panel" ? 1 : node.children.reduce((n, c) => n + countPanels(c), 0);
}

function panelIds(node: LayoutNode): string[] {
  return node.type === "panel" ? [node.id] : node.children.flatMap(panelIds);
}

/** Every split in the tree has ≥ 2 children and sizes summing to 1. */
function expectWellFormed(node: LayoutNode) {
  if (node.type === "panel") return;
  expect(node.children.length).toBeGreaterThanOrEqual(2);
  expect(node.sizes).toHaveLength(node.children.length);
  expect(node.sizes.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 10);
  for (const s of node.sizes) expect(s).toBeGreaterThan(0);
  for (const c of node.children) {
    // normalize() flattens a same-direction split nested directly in another.
    if (c.type === "split") expect(c.dir).not.toBe(node.dir);
    expectWellFormed(c);
  }
}

const asSplit = (n: LayoutNode | null): SplitNode => {
  expect(n?.type).toBe("split");
  return n as SplitNode;
};

/** Shape without ids, for structural comparisons. */
function shape(n: LayoutNode): unknown {
  return n.type === "panel"
    ? n.kind
    : { [n.dir]: n.children.map(shape) };
}

describe("ids and constructors", () => {
  it("issues unique ids", () => {
    const ids = new Set(Array.from({ length: 200 }, () => panel("map").id));
    expect(ids.size).toBe(200);
  });

  it("builds a panel with empty options by default", () => {
    const p = panel("map");
    expect(p).toMatchObject({ type: "panel", kind: "map", options: {} });
    expect(p.id.startsWith("p")).toBe(true);
  });

  it("splits evenly unless sizes are given", () => {
    const s = split("row", [panel("map"), panel("cdf"), panel("stats")]);
    expect(s.sizes).toEqual([1 / 3, 1 / 3, 1 / 3]);
    expect(split("column", [panel("map"), panel("cdf")], [0.3, 0.7]).sizes).toEqual([0.3, 0.7]);
  });
});

describe("queries", () => {
  const a = panel("timeseries");
  const b = panel("scatter");
  const c = panel("map");
  const tree = split("row", [a, split("column", [b, c])]);

  it("finds a panel at any depth", () => {
    expect(findPanel(tree, c.id)).toBe(c);
    expect(findPanel(tree, "nope")).toBeNull();
    expect(findPanel(a, a.id)).toBe(a);
  });

  it("does not report a split as a panel", () => {
    expect(findPanel(tree, tree.id)).toBeNull();
  });

  it("counts and lists panels in reading order", () => {
    expect(countPanels(tree)).toBe(3);
    expect(panelIds(tree)).toEqual([a.id, b.id, c.id]);
    expect(countPanels(a)).toBe(1);
  });
});

describe("updatePanel / updatePanelOptions", () => {
  it("changes only the targeted panel and keeps its id", () => {
    const a = panel("timeseries");
    const b = panel("scatter");
    const tree = split("row", [a, b]);
    const next = asSplit(updatePanel(tree, b.id, { kind: "map" }));
    expect(next.children[0]).toBe(a);
    expect(next.children[1]).toMatchObject({ id: b.id, kind: "map" });
    // The original is untouched.
    expect(b.kind).toBe("scatter");
  });

  it("returns the same tree when the id is unknown", () => {
    const tree = split("row", [panel("map"), panel("cdf")]);
    expect(updatePanel(tree, "missing", { kind: "stats" })).toBe(tree);
  });

  it("merges options rather than replacing them", () => {
    const a = panel("histogram", { component: "du", bins: 20 });
    const next = updatePanelOptions(a, a.id, { bins: 40 }) as PanelNode;
    expect(next.options).toEqual({ component: "du", bins: 40 });
  });
});

describe("insertBeside", () => {
  it("wraps a lone panel in a two-way split on the requested side", () => {
    const a = panel("timeseries");
    const b = panel("map");
    const right = asSplit(insertBeside(a, a.id, b, "right"));
    expect(right.dir).toBe("row");
    expect(right.children).toEqual([a, b]);
    expect(right.sizes).toEqual([0.5, 0.5]);

    const top = asSplit(insertBeside(a, a.id, b, "top"));
    expect(top.dir).toBe("column");
    expect(top.children).toEqual([b, a]);
  });

  it("joins a split running the same way and halves the target's share", () => {
    const a = panel("timeseries");
    const b = panel("scatter");
    const tree = split("row", [a, b], [0.6, 0.4]);
    const c = panel("map");
    const next = asSplit(insertBeside(tree, a.id, c, "right"));
    expect(next.children.map((n) => n.id)).toEqual([a.id, c.id, b.id]);
    expect(next.sizes).toEqual([0.3, 0.3, 0.4]);
    expectWellFormed(next);
  });

  it("inserts before the target on the left", () => {
    const a = panel("timeseries");
    const b = panel("scatter");
    const c = panel("map");
    const next = asSplit(insertBeside(split("row", [a, b]), b.id, c, "left"));
    expect(next.children.map((n) => n.id)).toEqual([a.id, c.id, b.id]);
  });

  it("nests a cross-direction split in place of the target", () => {
    const a = panel("timeseries");
    const b = panel("scatter");
    const c = panel("map");
    const next = asSplit(insertBeside(split("row", [a, b]), b.id, c, "bottom"));
    expect(shape(next)).toEqual({ row: ["timeseries", { column: ["scatter", "map"] }] });
    expectWellFormed(next);
  });

  it("leaves the tree alone when the target does not exist", () => {
    const tree = split("row", [panel("map"), panel("cdf")]);
    expect(insertBeside(tree, "missing", panel("stats"), "left")).toBe(tree);
  });
});

describe("removePanel", () => {
  it("returns null when the last panel goes", () => {
    const a = panel("map");
    expect(removePanel(a, a.id)).toBeNull();
  });

  it("keeps a lone panel when asked to remove something else", () => {
    const a = panel("map");
    expect(removePanel(a, "other")).toBe(a);
  });

  it("collapses a two-way split to the survivor", () => {
    const a = panel("map");
    const b = panel("cdf");
    expect(removePanel(split("row", [a, b]), a.id)).toBe(b);
  });

  it("hands the freed share to the siblings in proportion", () => {
    const a = panel("map");
    const b = panel("cdf");
    const c = panel("stats");
    const next = asSplit(removePanel(split("row", [a, b, c], [0.5, 0.2, 0.3]), a.id));
    expect(next.children.map((n) => n.id)).toEqual([b.id, c.id]);
    expect(next.sizes[0]).toBeCloseTo(0.4);
    expect(next.sizes[1]).toBeCloseTo(0.6);
  });

  it("flattens a same-direction split left behind by a collapse", () => {
    // row[ A, column[ B, row[C, D] ] ] — removing B leaves row[C, D] inside
    // the outer row, which must merge into it.
    const [a, b, c, d] = [panel("map"), panel("cdf"), panel("stats"), panel("error")];
    const tree = split("row", [a, split("column", [b, split("row", [c, d])])]);
    const next = asSplit(removePanel(tree, b.id));
    expect(next.dir).toBe("row");
    expect(next.children.map((n) => n.id)).toEqual([a.id, c.id, d.id]);
    expect(next.sizes).toEqual([0.5, 0.25, 0.25]);
    expectWellFormed(next);
  });

  it("is a structural no-op for an unknown id", () => {
    const tree = split("row", [panel("map"), panel("cdf")], [0.3, 0.7]);
    const next = asSplit(removePanel(tree, "missing"));
    expect(panelIds(next)).toEqual(panelIds(tree));
    expect(next.sizes[0]).toBeCloseTo(0.3);
  });
});

describe("movePanel", () => {
  it("does nothing when dropped on itself or on a missing panel", () => {
    const a = panel("map");
    const tree = split("row", [a, panel("cdf")]);
    expect(movePanel(tree, a.id, a.id, "left")).toBe(tree);
    expect(movePanel(tree, a.id, "missing", "left")).toBe(tree);
    expect(movePanel(tree, "missing", a.id, "left")).toBe(tree);
  });

  it("swaps content on a centre drop but keeps ids in place", () => {
    const a = panel("map", { zoom: 3 });
    const b = panel("cdf");
    const next = asSplit(movePanel(split("row", [a, b]), a.id, b.id, "center"));
    expect(next.children[0]).toMatchObject({ id: a.id, kind: "cdf", options: {} });
    expect(next.children[1]).toMatchObject({ id: b.id, kind: "map", options: { zoom: 3 } });
  });

  it("moves a panel beside another, preserving the panel count", () => {
    const [a, b, c] = [panel("map"), panel("cdf"), panel("stats")];
    const tree = split("row", [a, b, c]);
    const next = movePanel(tree, a.id, c.id, "bottom");
    expect(countPanels(next)).toBe(3);
    expect(shape(next)).toEqual({ row: ["cdf", { column: ["stats", "map"] }] });
    expectWellFormed(next);
  });

  it("keeps the source's id when it moves", () => {
    const [a, b, c] = [panel("map"), panel("cdf"), panel("stats")];
    const next = movePanel(split("row", [a, b, c]), a.id, c.id, "right");
    expect(panelIds(next)).toEqual([b.id, c.id, a.id]);
  });
});

describe("addAtEdge", () => {
  it("returns the incoming panel for an empty layout", () => {
    const p = panel("map");
    expect(addAtEdge(null, p)).toBe(p);
  });

  it("appends to a same-direction root, giving the newcomer an equal share", () => {
    const tree = split("row", [panel("map"), panel("cdf")], [0.75, 0.25]);
    const p = panel("stats");
    const next = asSplit(addAtEdge(tree, p, "right"));
    expect(next.children[next.children.length - 1]).toBe(p);
    expect(next.sizes[2]).toBeCloseTo(1 / 3);
    expect(next.sizes[0]).toBeCloseTo(0.5);
    expect(next.sizes[1]).toBeCloseTo(1 / 6);
    expectWellFormed(next);
  });

  it("prepends on the left", () => {
    const tree = split("row", [panel("map"), panel("cdf")]);
    const p = panel("stats");
    expect(asSplit(addAtEdge(tree, p, "left")).children[0]).toBe(p);
  });

  it("wraps a cross-direction root", () => {
    const tree = split("row", [panel("map"), panel("cdf")]);
    const p = panel("stats");
    const next = asSplit(addAtEdge(tree, p, "bottom"));
    expect(next.dir).toBe("column");
    expect(next.children).toEqual([tree, p]);
  });
});

describe("resizeSplit", () => {
  const tree = () => split("row", [panel("map"), panel("cdf"), panel("stats")], [0.4, 0.4, 0.2]);

  it("moves the divider within the pair, leaving the other panes alone", () => {
    const t = tree();
    const next = asSplit(resizeSplit(t, t.id, 0, 0.25));
    expect(next.sizes[0]).toBeCloseTo(0.2);
    expect(next.sizes[1]).toBeCloseTo(0.6);
    expect(next.sizes[2]).toBe(0.2);
  });

  it("clamps both panes to the minimum fraction", () => {
    const t = tree();
    expect(asSplit(resizeSplit(t, t.id, 0, -5)).sizes[0]).toBeCloseTo(MIN_FRACTION);
    expect(asSplit(resizeSplit(t, t.id, 0, 5)).sizes[1]).toBeCloseTo(MIN_FRACTION);
  });

  it("splits a pair already below twice the minimum evenly instead of inverting it", () => {
    const t = split("row", [panel("map"), panel("cdf"), panel("stats")], [0.05, 0.05, 0.9]);
    const next = asSplit(resizeSplit(t, t.id, 0, 0));
    expect(next.sizes[0]).toBeCloseTo(0.05);
    expect(next.sizes[1]).toBeCloseTo(0.05);
  });

  it("reaches a nested split and leaves unrelated subtrees by reference", () => {
    const left = panel("map");
    const inner = split("column", [panel("cdf"), panel("stats")]);
    const root = split("row", [left, inner]);
    const next = asSplit(resizeSplit(root, inner.id, 0, 0.7));
    expect(next.children[0]).toBe(left);
    expect((next.children[1] as SplitNode).sizes[0]).toBeCloseTo(0.7);
  });

  it("returns the same tree for an unknown split id", () => {
    const t = tree();
    expect(resizeSplit(t, "missing", 0, 0.5)).toBe(t);
  });
});

describe("equalize", () => {
  it("evens out every level", () => {
    const tree = split("row", [panel("map"), split("column", [panel("cdf"), panel("stats"), panel("error")], [0.1, 0.1, 0.8])], [0.9, 0.1]);
    const next = asSplit(equalize(tree));
    expect(next.sizes).toEqual([0.5, 0.5]);
    expect((next.children[1] as SplitNode).sizes).toEqual([1 / 3, 1 / 3, 1 / 3]);
  });

  it("returns a lone panel unchanged", () => {
    const p = panel("map");
    expect(equalize(p)).toBe(p);
  });
});

describe("parseLayout", () => {
  it("round-trips every preset through JSON", () => {
    for (const preset of PRESETS) {
      const tree = preset.build();
      const parsed = parseLayout(JSON.parse(JSON.stringify(tree)));
      expect(parsed, preset.key).not.toBeNull();
      expect(shape(parsed!)).toEqual(shape(tree));
      expect(panelIds(parsed!)).toEqual(panelIds(tree));
    }
  });

  it("renormalises sizes that do not sum to 1", () => {
    const parsed = asSplit(
      parseLayout({
        type: "split",
        id: "s",
        dir: "row",
        sizes: [2, 6],
        children: [
          { type: "panel", id: "a", kind: "map", options: {} },
          { type: "panel", id: "b", kind: "cdf" },
        ],
      }),
    );
    expect(parsed.sizes).toEqual([0.25, 0.75]);
    // Missing options default to an empty object.
    expect((parsed.children[1] as PanelNode).options).toEqual({});
  });

  const p = (id: string, kind = "map") => ({ type: "panel", id, kind, options: {} });
  it.each([
    ["null", null],
    ["a string", "layout"],
    ["a missing id", { type: "panel", kind: "map" }],
    ["an unknown panel kind", p("a", "radar")],
    ["an unknown node type", { type: "tab", id: "x" }],
    ["a bad direction", { type: "split", id: "s", dir: "diagonal", sizes: [1, 1], children: [p("a"), p("b")] }],
    ["a single-child split", { type: "split", id: "s", dir: "row", sizes: [1], children: [p("a")] }],
    ["mismatched sizes", { type: "split", id: "s", dir: "row", sizes: [1], children: [p("a"), p("b")] }],
    ["a zero size", { type: "split", id: "s", dir: "row", sizes: [0, 1], children: [p("a"), p("b")] }],
    ["a non-numeric size", { type: "split", id: "s", dir: "row", sizes: ["x", 1], children: [p("a"), p("b")] }],
    ["duplicate ids", { type: "split", id: "s", dir: "row", sizes: [1, 1], children: [p("a"), p("a")] }],
    ["a malformed grandchild", { type: "split", id: "s", dir: "row", sizes: [1, 1], children: [p("a"), { type: "panel", id: "b" }] }],
  ])("rejects %s", (_label, raw) => {
    expect(parseLayout(raw)).toBeNull();
  });
});

describe("presets", () => {
  it("has unique keys and well-formed trees using known kinds only", () => {
    expect(new Set(PRESETS.map((p) => p.key)).size).toBe(PRESETS.length);
    for (const preset of PRESETS) {
      const tree = preset.build();
      if (tree.type === "split") expectWellFormed(tree);
      const kinds: string[] = [];
      const walk = (n: LayoutNode) => (n.type === "panel" ? kinds.push(n.kind) : n.children.forEach(walk));
      walk(tree);
      for (const k of kinds) expect(PANEL_KINDS).toContain(k);
    }
  });

  it("builds fresh ids each time, so two instances never collide", () => {
    const a = panelIds(DEFAULT_PRESET.build());
    const b = panelIds(DEFAULT_PRESET.build());
    expect(a.some((id) => b.includes(id))).toBe(false);
  });

  it("defaults to the overview", () => {
    expect(DEFAULT_PRESET.key).toBe("overview");
    expect(countPanels(DEFAULT_PRESET.build())).toBe(3);
  });
});
