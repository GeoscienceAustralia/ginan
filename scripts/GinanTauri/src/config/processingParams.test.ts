import { describe, expect, it } from "vitest";
import {
  PARAMS_BY_PATH,
  PARAM_GROUPS,
  changedParams,
  defsByPath,
  mergeParamGroups,
  type DiscoveredParam,
} from "./processingParams";

const allParams = PARAM_GROUPS.flatMap((g) => g.params);

describe("PARAM_GROUPS", () => {
  it("has no duplicate paths — a duplicate would silently shadow in the flat lookup", () => {
    const paths = allParams.map((p) => p.path);
    expect(new Set(paths).size).toBe(paths.length);
  });

  it("keys every parameter by a dotted YAML path", () => {
    for (const p of allParams) {
      expect(p.path).toMatch(/^[a-z0-9_]+(\.[a-z0-9_]+)+$/);
    }
  });

  it("gives every parameter a defaultValue matching its declared type", () => {
    for (const p of allParams) {
      const expected = p.type === "enum" ? "string" : p.type;
      expect(typeof p.defaultValue, `${p.path}`).toBe(expected);
    }
  });

  it("gives every enum parameter options that include its default", () => {
    for (const p of allParams.filter((p) => p.type === "enum")) {
      expect(p.options, p.path).toBeDefined();
      expect(p.options!.map((o) => o.value)).toContain(p.defaultValue);
    }
  });

  it("keeps numeric defaults inside their advisory bounds", () => {
    for (const p of allParams.filter((p) => p.type === "number")) {
      const v = p.defaultValue as number;
      if (p.min !== undefined) expect(v, p.path).toBeGreaterThanOrEqual(p.min);
      if (p.max !== undefined) expect(v, p.path).toBeLessThanOrEqual(p.max);
    }
  });

  it("labels every group and parameter", () => {
    for (const g of PARAM_GROUPS) {
      expect(g.label.length).toBeGreaterThan(0);
      expect(g.params.length).toBeGreaterThan(0);
      for (const p of g.params) expect(p.label.length).toBeGreaterThan(0);
    }
  });
});

describe("PARAMS_BY_PATH", () => {
  it("indexes every curated parameter", () => {
    expect(Object.keys(PARAMS_BY_PATH)).toHaveLength(allParams.length);
    expect(PARAMS_BY_PATH["receiver_options.global.elevation_mask"].defaultValue).toBe(15);
  });
});

describe("mergeParamGroups", () => {
  const discovered = (paths: string[]): DiscoveredParam[] =>
    paths.map((path) => ({ path, value_type: "boolean", default_value: true }));

  it("returns the curated table untouched when discovery found nothing", () => {
    expect(mergeParamGroups([])).toBe(PARAM_GROUPS);
  });

  it("drops a curated parameter the template no longer carries", () => {
    const kept = "receiver_options.global.elevation_mask";
    const merged = mergeParamGroups([
      { path: kept, value_type: "number", default_value: 15 },
    ]);
    const paths = merged.flatMap((g) => g.params).map((p) => p.path);
    expect(paths).toEqual([kept]);
  });

  it("drops a group left with no parameters at all", () => {
    const merged = mergeParamGroups([
      { path: "processing_options.spp.max_lsq_iterations", value_type: "number", default_value: 12 },
    ]);
    expect(merged.map((g) => g.label)).toEqual(["SPP"]);
  });

  it("appends uncurated parameters as their own per-section group", () => {
    const merged = mergeParamGroups([
      ...discovered(allParams.map((p) => p.path)),
      { path: "processing_options.brand_new_option", value_type: "boolean", default_value: false },
    ]);
    const extra = merged.find((g) => g.label.includes("other parameters"));
    expect(extra).toBeDefined();
    expect(extra!.label).toBe("Processing options - other parameters");
    expect(extra!.params[0]).toMatchObject({
      path: "processing_options.brand_new_option",
      label: "Brand new option",
      desc: "processing_options.brand_new_option",
      type: "boolean",
      defaultValue: false,
    });
  });

  it("groups uncurated parameters by their top-level YAML section", () => {
    const merged = mergeParamGroups([
      { path: "alpha_options.one", value_type: "number", default_value: 1 },
      { path: "beta_options.two", value_type: "string", default_value: "x" },
      { path: "alpha_options.three", value_type: "boolean", default_value: true },
    ]);
    const labels = merged.map((g) => g.label);
    expect(labels).toEqual([
      "Alpha options - other parameters",
      "Beta options - other parameters",
    ]);
    expect(merged[0].params.map((p) => p.path)).toEqual([
      "alpha_options.one",
      "alpha_options.three",
    ]);
  });

  it("preserves template order inside a discovered section", () => {
    const merged = mergeParamGroups([
      { path: "zed_options.later", value_type: "number", default_value: 2 },
      { path: "zed_options.earlier", value_type: "number", default_value: 1 },
    ]);
    expect(merged[0].params.map((p) => p.path)).toEqual([
      "zed_options.later",
      "zed_options.earlier",
    ]);
  });

  it("keeps a curated definition rather than the discovered stub", () => {
    const path = "receiver_options.global.elevation_mask";
    const merged = mergeParamGroups([
      { path, value_type: "number", default_value: 99 },
    ]);
    const def = merged.flatMap((g) => g.params).find((p) => p.path === path)!;
    // The curated label, unit and bounds survive; the discovered value does not
    // get to replace a default the UI compares against.
    expect(def.label).toBe("Elevation mask");
    expect(def.unit).toBe("°");
    expect(def.defaultValue).toBe(15);
  });
});

describe("defsByPath", () => {
  it("flattens merged groups into a path lookup", () => {
    const groups = mergeParamGroups([
      { path: "some_section.flag", value_type: "boolean", default_value: true },
    ]);
    const defs = defsByPath(groups);
    expect(defs["some_section.flag"].defaultValue).toBe(true);
    expect(Object.keys(defs)).toHaveLength(1);
  });
});

describe("changedParams", () => {
  it("drops values equal to the template default", () => {
    expect(
      changedParams({ "receiver_options.global.elevation_mask": 15 }),
    ).toEqual({});
  });

  it("keeps values moved off the default", () => {
    expect(
      changedParams({ "receiver_options.global.elevation_mask": 10 }),
    ).toEqual({ "receiver_options.global.elevation_mask": 10 });
  });

  it("keeps an unknown path rather than dropping a value the user set", () => {
    expect(changedParams({ "not.in.the.table": 7 })).toEqual({ "not.in.the.table": 7 });
  });

  it("compares against the supplied defaults, not just the curated table", () => {
    const defaults = { "discovered.only": { defaultValue: 5 } };
    expect(changedParams({ "discovered.only": 5 }, defaults)).toEqual({});
    expect(changedParams({ "discovered.only": 6 }, defaults)).toEqual({
      "discovered.only": 6,
    });
  });

  it("distinguishes false from the default true", () => {
    const path = "receiver_options.global.models.tides.enable";
    expect(changedParams({ [path]: false })).toEqual({ [path]: false });
    expect(changedParams({ [path]: true })).toEqual({});
  });

  it("returns an empty map for empty input", () => {
    expect(changedParams({})).toEqual({});
  });

  it("only carries the parameters that actually differ", () => {
    const mask = "receiver_options.global.elevation_mask";
    const gdop = "processing_options.spp.outlier_screening.max_gdop";
    expect(changedParams({ [mask]: 15, [gdop]: 25 })).toEqual({ [gdop]: 25 });
  });
});
