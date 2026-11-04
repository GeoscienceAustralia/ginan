import { useEffect, useRef } from "react";
import L from "leaflet";
import "leaflet/dist/leaflet.css";
import type { PosSeries } from "../types";
import { formatEpoch, formatMillimetres, nearestIndex, recencyColor } from "./chartUtils";
import { useLinkedHover, useSelectedIndex, useSelectedTime } from "./viz/VizContext";
import { useTimeZone } from "../hooks/useTimeZone";

/**
 * Horizontal scatter, plotted on a real basemap instead of a local dE/dN
 * frame — the `lat`/`lon` columns `load_pos_series` carries let every
 * epoch be placed on the ground it was actually measured on.
 *
 * Leaflet owns panning and zooming here (drag, wheel, pinch, double-click),
 * so the gesture set matches any consumer map product instead of the
 * box-drag/scroll model the local scatter uses. Tiles come from the public
 * OpenStreetMap raster endpoint; `L.TileLayer` only ever requests the tiles
 * covering the current viewport at the current zoom, so panning or zooming
 * streams in just the newly-visible squares rather than the whole world.
 *
 * The map instance is created once and kept for the component's lifetime —
 * a `series` update (a realtime poll re-reading a growing file) only moves
 * the point layer, never the camera, so the user's pan/zoom survives a
 * refresh. The camera is only reframed when the *selected file* changes,
 * since that's a different station with a different position on Earth.
 */

interface Props {
  series: PosSeries;
}

export default function PosMapChart({ series }: Props) {
  const containerRef = useRef<HTMLDivElement>(null);
  const mapRef = useRef<L.Map | null>(null);
  const pointsLayerRef = useRef<L.LayerGroup | null>(null);
  const framedPathRef = useRef<string | null>(null);
  const highlightRef = useRef<L.CircleMarker | null>(null);
  const selectedRef = useRef<L.CircleMarker | null>(null);
  const { setSelectedT } = useSelectedTime();
  const setSelectedRef = useRef(setSelectedT);
  setSelectedRef.current = setSelectedT;
  const selIndex = useSelectedIndex(series.t);
  // Linked hover: hovering a point here moves the other panels' crosshair,
  // and the epoch hovered elsewhere is ringed here. Leaflet handlers are
  // bound once per point, so they reach the latest setter through a ref.
  const linked = useLinkedHover();
  const setHoverRef = useRef(linked.setHoverT);
  setHoverRef.current = linked.setHoverT;
  // Read when a tooltip opens rather than baked in, so a zone change doesn't
  // have to rebuild every marker.
  const timeZone = useTimeZone();
  const timeZoneRef = useRef(timeZone);
  timeZoneRef.current = timeZone;

  // Map created once; torn down only when the panel itself unmounts.
  useEffect(() => {
    const el = containerRef.current;
    if (!el) return;

    const map = L.map(el, {
      preferCanvas: true,
      renderer: safeCanvasRenderer(),
      zoomControl: true,
      attributionControl: true,
      worldCopyJump: true,
      // Past the tiles' z19: the useful detail is sub-metre, and the markers
      // and track are vectors, so they stay sharp while the tiles upscale.
      maxZoom: 24,
    }).setView([0, 0], 2);

    // Leaflet's own "Leaflet" prefix link is dropped: OSM's tile terms require
    // the data attribution below, not a library credit, and every anchor here
    // is one more thing that could navigate the window.
    map.attributionControl.setPrefix(false);

    L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
      // OSM serves up to z19; Leaflet upscales that level beyond it rather
      // than requesting tiles that don't exist.
      maxNativeZoom: 19,
      maxZoom: 24,
      // Kept as a real link — attribution is a condition of using the tiles —
      // but target/rel stop the webview treating it as in-window navigation.
      // useExternalLinks then routes the click to the system browser; both
      // layers matter, since the attribute alone would open a blank webview.
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank" rel="noreferrer noopener">OpenStreetMap</a> contributors',
    }).addTo(map);

    mapRef.current = map;
    pointsLayerRef.current = L.layerGroup().addTo(map);

    return () => {
      map.remove();
      mapRef.current = null;
      pointsLayerRef.current = null;
      framedPathRef.current = null;
      highlightRef.current = null;
      selectedRef.current = null;
    };
  }, []);

  // Points and camera framing. Re-runs on every series update (including a
  // realtime poll of the same file), but only moves the camera the first
  // time a given file is seen.
  useEffect(() => {
    const map = mapRef.current;
    const layer = pointsLayerRef.current;
    if (!map || !layer) return;

    layer.clearLayers();

    const n = series.lat.length;
    if (n === 0) return;

    const bounds = L.latLngBounds([]);
    const radius = n > 4000 ? 2.5 : 4;

    for (let i = 0; i < n; i++) {
      const lat = series.lat[i];
      const lon = series.lon[i];
      if (!Number.isFinite(lat) || !Number.isFinite(lon)) continue;
      const t = n > 1 ? i / (n - 1) : 1;
      const isLatest = i === n - 1;
      const marker = L.circleMarker([lat, lon], {
        radius: isLatest ? radius + 2.5 : radius,
        color: isLatest ? "#ffffff" : recencyColor(t),
        weight: isLatest ? 2 : 0.5,
        fillColor: recencyColor(t),
        fillOpacity: isLatest ? 1 : 0.75,
        // No per-marker `renderer`: `preferCanvas` shares one canvas across all
        // markers. A per-marker `L.canvas()` adds a map-sized canvas that
        // `clearLayers()` never removes, so each refresh leaked n of them and
        // the WebView ran out of memory within minutes.
      });
      marker.bindTooltip(
        () =>
          `${formatEpoch(series.t[i], timeZoneRef.current)}<br>dE ${formatMillimetres(series.de[i])} · dN ${formatMillimetres(series.dn[i])}`,
      );
      const epoch = series.t[i];
      marker.on("mouseover", () => setHoverRef.current(epoch));
      marker.on("mouseout", () => setHoverRef.current(null));
      marker.on("click", () => setSelectedRef.current(epoch));
      marker.addTo(layer);
      bounds.extend([lat, lon]);
    }

    if (framedPathRef.current !== series.path && bounds.isValid()) {
      framedPathRef.current = series.path;
      if (n === 1) {
        map.setView(bounds.getCenter(), 16);
      } else {
        map.fitBounds(bounds, { padding: [32, 32], maxZoom: 18 });
      }
    }
  }, [series]);

  useEffect(() => {
    const map = mapRef.current;
    if (!map) return;
    const t = linked.hoverT;
    const n = series.t.length;
    const inRange = t !== null && n > 0 && t >= series.t[0] && t <= series.t[n - 1];
    if (!inRange || linked.hoverIsMine) {
      highlightRef.current?.remove();
      highlightRef.current = null;
      return;
    }
    const i = nearestIndex(series.t, t);
    const ll: L.LatLngExpression = [series.lat[i], series.lon[i]];
    if (!Number.isFinite(series.lat[i]) || !Number.isFinite(series.lon[i])) return;
    if (!highlightRef.current) {
      highlightRef.current = L.circleMarker(ll, {
        radius: 8,
        color: "#ffffff",
        weight: 2.5,
        fillOpacity: 0,
        interactive: false,
      }).addTo(map);
    } else {
      highlightRef.current.setLatLng(ll);
    }
  }, [linked.hoverT, linked.hoverIsMine, series]);

  // The selected epoch: an accent ring that stays put, unlike the hover one.
  // Deliberately doesn't move the camera — panning under the user while they
  // scrub would fight their own pan/zoom.
  useEffect(() => {
    const map = mapRef.current;
    if (!map) return;
    const ok = selIndex !== null && Number.isFinite(series.lat[selIndex]) && Number.isFinite(series.lon[selIndex]);
    if (!ok) {
      selectedRef.current?.remove();
      selectedRef.current = null;
      return;
    }
    const ll: L.LatLngExpression = [series.lat[selIndex!], series.lon[selIndex!]];
    if (!selectedRef.current) {
      const accent = getComputedStyle(document.documentElement).getPropertyValue("--accent").trim() || "#0d99ff";
      selectedRef.current = L.circleMarker(ll, {
        radius: 9,
        color: accent,
        weight: 3,
        fillOpacity: 0,
        interactive: false,
      }).addTo(map);
    } else {
      selectedRef.current.setLatLng(ll);
    }
  }, [selIndex, series]);

  // The container has no intrinsic size of its own; Leaflet needs an explicit
  // nudge whenever the pane it lives in is resized (e.g. the sidebar toggling).
  useEffect(() => {
    const map = mapRef.current;
    const el = containerRef.current;
    if (!map || !el) return;
    const observer = new ResizeObserver(() => map.invalidateSize());
    observer.observe(el);
    return () => observer.disconnect();
  }, []);

  return (
    <div className="viz-map-wrap">
      <div ref={containerRef} className="viz-map" />
      <div className="viz-map-legend">
        <span>Oldest</span>
        <span className="viz-map-legend-bar" />
        <span>Newest</span>
      </div>
    </div>
  );
}

/**
 * The map's one canvas renderer, with its redraw skipped once the canvas has
 * been torn down. Leaflet can still run a redraw frame queued during
 * `map.remove()` after it has deleted the context, which throws
 * whenever the map panel is closed, moved or maximised.
 */
function safeCanvasRenderer(): L.Canvas {
  const renderer = L.canvas() as L.Canvas & { _redraw: () => void; _ctx?: CanvasRenderingContext2D };
  const redraw = renderer._redraw;
  renderer._redraw = function (this: typeof renderer) {
    if (this._ctx) redraw.call(this);
  };
  return renderer;
}
