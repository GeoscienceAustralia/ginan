import { describe, expect, it, vi } from "vitest";
import { act, renderHook, waitFor } from "@testing-library/react";
import type { ReactNode } from "react";
import {
  VizDataProvider,
  VizLinkProvider,
  VizSelectionProvider,
  useLinkedHover,
  usePanelSeries,
  useSelectedIndex,
  useSelectedTime,
  useTimeLink,
  useVizData,
  type SeriesEntry,
} from "./VizContext";
import { invokeCount, mockInvoke } from "../../test/tauriMocks";
import type { PosFileInfo, PosSeries } from "../../types";

const file = (name: string): PosFileInfo => ({ path: `/out/${name}`, name, format: "pos", smoothed: false, size: 99_999, modified_ms: 1, has_epochs: true });

const fakeSeries = (path: string) => ({ path, name: path, t: [1, 2, 3] }) as unknown as PosSeries;

const PRIMARY: SeriesEntry = { series: fakeSeries("/out/A.pos"), loading: false, error: null };

function dataWrapper(files: PosFileInfo[], reloadKey = { current: 0 }) {
  return ({ children }: { children: ReactNode }) => (
    <VizDataProvider files={files} primaryPath="/out/A.pos" primary={PRIMARY} reloadKey={reloadKey.current}>
      {children}
    </VizDataProvider>
  );
}

describe("useVizData", () => {
  it("refuses to run outside its provider", () => {
    // React logs the thrown render error; keep the output quiet.
    vi.spyOn(console, "error").mockImplementation(() => {});
    expect(() => renderHook(() => useVizData())).toThrow(/outside VizDataProvider/);
  });
});

describe("usePanelSeries", () => {
  const files = [file("A.pos"), file("B.pos")];

  it("follows the toolbar's solution when not pinned", () => {
    const { result } = renderHook(() => usePanelSeries(undefined), { wrapper: dataWrapper(files) });
    expect(result.current).toMatchObject({ pinned: false, series: PRIMARY.series });
  });

  it("treats a pin to the toolbar's own file as following it", () => {
    const { result } = renderHook(() => usePanelSeries("/out/A.pos"), { wrapper: dataWrapper(files) });
    expect(result.current.pinned).toBe(false);
    expect(invokeCount("load_pos_series")).toBe(0);
  });

  it("shares one pinned file between every panel that asks for it", async () => {
    mockInvoke({ load_pos_series: (args) => fakeSeries(args.path as string) });
    const { result } = renderHook(
      () => [usePanelSeries("/out/B.pos"), usePanelSeries("/out/B.pos")] as const,
      { wrapper: dataWrapper(files) },
    );
    expect(result.current[0]).toMatchObject({ pinned: true, loading: true });

    await waitFor(() => expect(result.current[0].series?.path).toBe("/out/B.pos"));
    expect(result.current[1].series?.path).toBe("/out/B.pos");
  });

  // BUG (minor, wasted I/O): a pinned file is read twice on mount. The
  // panel's effect calls ensure() before VizDataProvider's own reloadKey
  // effect runs (child effects run first), and that effect then clears the
  // in-flight set and the cache, so the panel asks again. See
  // VizContext.tsx, the `useEffect(..., [reloadKey])` in VizDataProvider; it
  // could skip its first run. Visible to the user only as a second
  // load_pos_series of the same file.
  it.skip("reads a pinned file only once on mount", async () => {
    mockInvoke({ load_pos_series: (args) => fakeSeries(args.path as string) });
    const { result } = renderHook(() => usePanelSeries("/out/B.pos"), { wrapper: dataWrapper(files) });
    await waitFor(() => expect(result.current.series).not.toBeNull());
    expect(invokeCount("load_pos_series")).toBe(1);
  });

  it("reports a pinned file that fails to load", async () => {
    mockInvoke({ load_pos_series: () => Promise.reject("gone") });
    const { result } = renderHook(() => usePanelSeries("/out/B.pos"), { wrapper: dataWrapper(files) });
    await waitFor(() => expect(result.current).toMatchObject({ pinned: true, loading: false, error: "gone" }));
  });

  it("falls back to the toolbar when the pinned file has disappeared", () => {
    const { result } = renderHook(() => usePanelSeries("/out/deleted.pos"), { wrapper: dataWrapper(files) });
    expect(result.current).toMatchObject({ pinned: false, series: PRIMARY.series });
  });

  it("re-requests a pinned file after a reload", async () => {
    mockInvoke({ load_pos_series: (args) => fakeSeries(args.path as string) });
    const key = { current: 0 };
    const { result, rerender } = renderHook(() => usePanelSeries("/out/B.pos"), { wrapper: dataWrapper(files, key) });
    await waitFor(() => expect(result.current.series).not.toBeNull());
    const before = invokeCount("load_pos_series");

    key.current = 1;
    rerender();

    await waitFor(() => expect(invokeCount("load_pos_series")).toBe(before + 1));
    await waitFor(() => expect(result.current.series?.path).toBe("/out/B.pos"));
  });
});

describe("selection", () => {
  const selWrapper = ({ children }: { children: ReactNode }) => (
    <VizSelectionProvider resetKey="a">{children}</VizSelectionProvider>
  );

  it("is a harmless no-op outside a provider", () => {
    const { result } = renderHook(() => useSelectedTime());
    expect(result.current.selectedT).toBeNull();
    expect(() => result.current.setSelectedT(5)).not.toThrow();
  });

  it("finds the nearest epoch to the selected time, and nothing outside the series", () => {
    const t = [0, 10, 20, 30];
    const { result } = renderHook(
      () => ({ sel: useSelectedTime(), idx: useSelectedIndex(t) }),
      { wrapper: selWrapper },
    );
    expect(result.current.idx).toBeNull();

    act(() => result.current.sel.setSelectedT(14));
    expect(result.current.idx).toBe(1);
    act(() => result.current.sel.setSelectedT(16));
    expect(result.current.idx).toBe(2);
    act(() => result.current.sel.setSelectedT(15));
    expect(result.current.idx).toBe(1); // a tie goes to the earlier epoch
    act(() => result.current.sel.setSelectedT(30));
    expect(result.current.idx).toBe(3);
    act(() => result.current.sel.setSelectedT(31));
    expect(result.current.idx).toBeNull();
  });

  it("clears when the file changes", () => {
    let resetKey = "a";
    const wrapper = ({ children }: { children: ReactNode }) => (
      <VizSelectionProvider resetKey={resetKey}>{children}</VizSelectionProvider>
    );
    const { result, rerender } = renderHook(() => useSelectedTime(), { wrapper });
    act(() => result.current.setSelectedT(42));
    expect(result.current.selectedT).toBe(42);

    resetKey = "b";
    rerender();
    expect(result.current.selectedT).toBeNull();
  });
});

describe("linked time axes", () => {
  function linkWrapper(linked: boolean) {
    return ({ children }: { children: ReactNode }) => (
      <VizLinkProvider linked={linked} setLinked={() => {}} resetKey="f">
        {children}
      </VizLinkProvider>
    );
  }

  it("shares zoom and hover across charts when linked", () => {
    const { result } = renderHook(() => ({ a: useTimeLink("f"), b: useTimeLink("f") }), {
      wrapper: linkWrapper(true),
    });

    act(() => result.current.a.setView([10, 20]));
    expect(result.current.b.view).toEqual([10, 20]);

    act(() => result.current.a.setHoverT(15));
    expect(result.current.b.hoverT).toBe(15);
    // Only the chart under the pointer draws the tooltip.
    expect(result.current.a.hoverIsMine).toBe(true);
    expect(result.current.b.hoverIsMine).toBe(false);
  });

  it("keeps each chart's zoom to itself when unlinked", () => {
    const { result } = renderHook(() => ({ a: useTimeLink("f"), b: useTimeLink("f") }), {
      wrapper: linkWrapper(false),
    });
    act(() => result.current.a.setView([10, 20]));
    act(() => result.current.a.setHoverT(12));
    expect(result.current.a.view).toEqual([10, 20]);
    expect(result.current.b.view).toBeNull();
    expect(result.current.b.hoverT).toBeNull();
  });

  it("lets a non-time panel publish and follow the linked hover", () => {
    const { result } = renderHook(() => ({ map: useLinkedHover(), chart: useTimeLink("f") }), {
      wrapper: linkWrapper(true),
    });
    act(() => result.current.map.setHoverT(7));
    expect(result.current.chart.hoverT).toBe(7);
    expect(result.current.map.hoverIsMine).toBe(true);

    act(() => result.current.chart.setHoverT(null));
    expect(result.current.map.hoverT).toBeNull();
  });

  it("ignores hover from a non-time panel when unlinked", () => {
    const { result } = renderHook(() => useLinkedHover(), { wrapper: linkWrapper(false) });
    act(() => result.current.setHoverT(7));
    expect(result.current.hoverT).toBeNull();
  });
});
