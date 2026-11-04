import { describe, expect, it } from "vitest";
import type { PosSeries } from "../../types";
import {
  ERROR_METRICS,
  activityWindow,
  convergenceTime,
  deriveErrors,
  histogram,
  meanStd,
  percentileSorted,
  rms,
  sortedCopy,
  typicalInterval,
} from "./metrics";

const STATS = { weighted_mean: 0, std_dev: 0, rms: 0, min: 0, max: 0, mean_sigma: 0 };

/** A series of `n` epochs one second apart; per-epoch arrays default to zeros. */
function makeSeries(n: number, over: Partial<Record<"dn" | "de" | "du" | "sn" | "se" | "su", number[]>> = {}): PosSeries {
  const zeros = () => new Array<number>(n).fill(0);
  return {
    path: "/out/run.pos",
    name: "run.pos",
    format: "pos",
    hpl: [],
    vpl: [],
    station: "ALIC",
    smoothed: false,
    total_epochs: n,
    returned_epochs: n,
    decimated: false,
    skipped_lines: 0,
    t: Array.from({ length: n }, (_, i) => 1_700_000_000_000 + i * 1000),
    dn: over.dn ?? zeros(),
    de: over.de ?? zeros(),
    du: over.du ?? zeros(),
    sn: over.sn ?? zeros(),
    se: over.se ?? zeros(),
    su: over.su ?? zeros(),
    lat: zeros(),
    lon: zeros(),
    stats: { dn: STATS, de: STATS, du: STATS },
  };
}

describe("ERROR_METRICS", () => {
  it("lists horizontal, vertical and 3D in that order", () => {
    expect(ERROR_METRICS.map((m) => m.key)).toEqual(["h", "v", "3d"]);
  });
});

describe("deriveErrors", () => {
  it("computes horizontal, vertical, 3D and horizontal sigma per epoch", () => {
    const s = makeSeries(2, { dn: [3, 0], de: [4, 0], du: [-12, 2], sn: [0.3, 0], se: [0.4, 1] });
    const d = deriveErrors(s);
    expect(d.h).toEqual([5, 0]);
    expect(d.v).toEqual([12, 2]);
    expect(d["3d"]).toEqual([13, 2]);
    expect(d.sh[0]).toBeCloseTo(0.5);
    expect(d.sh[1]).toBe(1);
  });

  it("memoises per series object", () => {
    const s = makeSeries(3);
    expect(deriveErrors(s)).toBe(deriveErrors(s));
    expect(deriveErrors(makeSeries(3))).not.toBe(deriveErrors(s));
  });

  it("handles an empty series", () => {
    expect(deriveErrors(makeSeries(0))).toEqual({ h: [], v: [], "3d": [], sh: [] });
  });
});

describe("percentileSorted", () => {
  it("is NaN for no data and the value itself for one sample", () => {
    expect(percentileSorted([], 50)).toBeNaN();
    expect(percentileSorted([7], 95)).toBe(7);
  });

  it("hits the ends at 0 and 100", () => {
    expect(percentileSorted([1, 2, 3, 4], 0)).toBe(1);
    expect(percentileSorted([1, 2, 3, 4], 100)).toBe(4);
  });

  it("interpolates linearly between ranks", () => {
    expect(percentileSorted([1, 2, 3, 4], 50)).toBe(2.5);
    expect(percentileSorted([0, 10], 25)).toBe(2.5);
    expect(percentileSorted([10, 20, 30, 40, 50], 95)).toBeCloseTo(48);
  });
});

describe("sortedCopy", () => {
  it("sorts numerically without touching the input", () => {
    const input = [10, 9, 1, 100];
    expect(sortedCopy(input)).toEqual([1, 9, 10, 100]);
    expect(input).toEqual([10, 9, 1, 100]);
  });
});

describe("rms and meanStd", () => {
  it("are NaN for no data", () => {
    expect(rms([])).toBeNaN();
    expect(meanStd([])).toEqual({ mean: NaN, std: NaN });
  });

  it("computes the root mean square", () => {
    expect(rms([3, -4])).toBeCloseTo(Math.sqrt(12.5));
    expect(rms([2, 2, 2])).toBe(2);
  });

  it("uses the sample (n − 1) standard deviation", () => {
    const { mean, std } = meanStd([2, 4, 4, 4, 5, 5, 7, 9]);
    expect(mean).toBe(5);
    expect(std).toBeCloseTo(Math.sqrt(32 / 7));
  });

  it("reports zero spread for a single sample rather than dividing by zero", () => {
    expect(meanStd([3])).toEqual({ mean: 3, std: 0 });
  });
});

describe("convergenceTime", () => {
  it("is null for an empty run", () => {
    expect(convergenceTime(makeSeries(0), 0.1)).toBeNull();
  });

  it("is null when the run ends above the threshold", () => {
    const s = makeSeries(3, { sn: [0.01, 0.01, 1] });
    expect(convergenceTime(s, 0.1)).toBeNull();
  });

  it("is zero when the run is converged from the first epoch", () => {
    expect(convergenceTime(makeSeries(5), 0.1)).toBe(0);
  });

  it("measures from the start to the first epoch that stays converged", () => {
    // Dips below at epoch 1, bounces back up at 2, then settles from epoch 3.
    const s = makeSeries(6, { sn: [1, 0.05, 0.5, 0.05, 0.04, 0.03] });
    expect(convergenceTime(s, 0.1)).toBe(3000);
  });

  it("treats a value exactly at the threshold as converged", () => {
    const s = makeSeries(3, { sn: [1, 0.1, 0.1] });
    expect(convergenceTime(s, 0.1)).toBe(1000);
  });
});

describe("typicalInterval", () => {
  it("needs at least two epochs", () => {
    expect(typicalInterval([])).toBeNull();
    expect(typicalInterval([5])).toBeNull();
  });

  it("returns the most common spacing, ignoring gaps", () => {
    expect(typicalInterval([0, 30, 60, 90, 300, 330])).toBe(30);
  });

  it("ignores repeated or backwards timestamps", () => {
    expect(typicalInterval([0, 0, 0, 5])).toBe(5);
    expect(typicalInterval([10, 5, 0])).toBeNull();
  });

  it("rounds sub-millisecond jitter", () => {
    expect(typicalInterval([0, 999.7, 2000.2, 2999.9])).toBe(1000);
  });
});

describe("histogram", () => {
  it("is empty for no data", () => {
    expect(histogram([])).toEqual({ edges: [], counts: [], max: 0, outside: 0 });
  });

  it("puts constant data in one narrow bin around the value", () => {
    const h = histogram([2, 2, 2]);
    expect(h.counts).toEqual([3]);
    expect(h.max).toBe(3);
    expect(h.edges[0]).toBeLessThan(2);
    expect(h.edges[1]).toBeGreaterThan(2);
  });

  it("counts every sample exactly once and spans the data", () => {
    const values = Array.from({ length: 500 }, (_, i) => Math.sin(i) * 3);
    const h = histogram(values);
    expect(h.counts.reduce((a, b) => a + b, 0)).toBe(500);
    expect(h.edges).toHaveLength(h.counts.length + 1);
    expect(h.edges[0]).toBe(Math.min(...values));
    expect(h.edges[h.edges.length - 1]).toBeCloseTo(Math.max(...values));
    expect(h.max).toBe(Math.max(...h.counts));
    expect(h.outside).toBe(0);
  });

  it("keeps the bin count within the bounds", () => {
    const wide = histogram(Array.from({ length: 10_000 }, (_, i) => i));
    expect(wide.counts.length).toBeLessThanOrEqual(60);
    const few = histogram([0, 1, 2]);
    expect(few.counts.length).toBeGreaterThanOrEqual(8);
    expect(histogram([0, 1, 2], 0, 3, 5).counts.length).toBeLessThanOrEqual(5);
  });

  it("puts the maximum in the last bin rather than past the end", () => {
    const h = histogram([0, 0.5, 1]);
    expect(h.counts[h.counts.length - 1]).toBeGreaterThanOrEqual(1);
  });

  it("clips the tails and reports how many samples fell outside", () => {
    const values = [...Array.from({ length: 98 }, (_, i) => i / 1000), 50, -50];
    const h = histogram(values, 1);
    expect(h.outside).toBe(2);
    expect(h.counts.reduce((a, b) => a + b, 0)).toBe(98);
    expect(h.edges[h.edges.length - 1]!).toBeLessThan(1);
  });
});

describe("activityWindow", () => {
  it("needs a reasonable number of epochs", () => {
    expect(activityWindow(makeSeries(19))).toBeNull();
  });

  it("is null for a run with no activity", () => {
    const n = 100;
    expect(activityWindow(makeSeries(n, { dn: new Array(n).fill(0.01) }))).toBeNull();
  });

  it("zooms onto a short burst inside an otherwise settled run", () => {
    const n = 200;
    const dn = new Array<number>(n).fill(0.01);
    for (let i = 100; i < 110; i++) dn[i] = 5;
    const s = makeSeries(n, { dn });
    const w = activityWindow(s);
    expect(w).not.toBeNull();
    const [a, b] = w!;
    expect(a).toBeLessThanOrEqual(s.t[100]);
    expect(b).toBeGreaterThanOrEqual(s.t[100]);
    expect(a).toBeGreaterThanOrEqual(s.t[0]);
    expect(b).toBeLessThanOrEqual(s.t[n - 1]);
    // Much shorter than the whole run.
    expect(b - a).toBeLessThan(0.6 * (s.t[n - 1] - s.t[0]));
  });

  it("gives up when the activity covers most of the run", () => {
    const n = 100;
    // Alternating levels everywhere: activity spread end to end.
    const dn = Array.from({ length: n }, (_, i) => (i % 2 ? 10 : 0.001));
    expect(activityWindow(makeSeries(n, { dn }))).toBeNull();
  });
});
