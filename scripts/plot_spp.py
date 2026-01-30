#!/usr/bin/env python3
"""
plot_spp.py

Plotly plotting for Ginan SBAS .SPP / *_SPP.POS style output.
Reads ONE OR MORE input files, concatenates into a single DataFrame,
sorts by time, and plots as one continuous time series (no per-file differentiation).

Outputs:
1) Main HTML (always):
   - Time series vs ISO time for: HDOP, VDOP, GDOP, dN, dE, dU, dH, HPL, VPL
   - X-axis range slider
   - Legend on the right
   - Box-zoom (x+y) + scroll-wheel zoom
   - Plain HTML stats table below (always visible):
       N, MEAN, MEDIAN, STD DEV, RMS for dE, dN, dU, dH (computed over concatenated data)

2) Optional map HTML (--map):
   - 2D lon/lat scatter (Eref vs Nref) on top
   - Href time series underneath
   - Hover sync between the two plots (same point index)

Usage:
  python3 plot_spp.py -i ALIC-202602303.SPP
  python3 plot_spp.py -i ALIC-202602303.SPP ALIC-202602304.SPP ALIC-202602305.SPP --map
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Dict, List

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from plotly.utils import PlotlyJSONEncoder


PLOT_COLS = ["HDOP", "VDOP", "GDOP", "dN", "dE", "dU", "dH", "HPL", "VPL"]
STATS_COLS = ["dE", "dN", "dU", "dH"]
ALL_NUM_COLS = ["HDOP", "VDOP", "GDOP", "Nref", "Eref", "Href", "dN", "dE", "dU", "dH", "HPL", "VPL"]


def parse_spp_file(path: Path) -> pd.DataFrame:
    """
    Parse .SPP / *_SPP.POS file into a DataFrame.

    Based on the example token positions (0-based indexes):
      0  ISO time
      5  HDOP
      6  VDOP
      7  GDOP
      11 Nref(lat)
      12 Eref(lon)
      13 Href
      14 dN
      15 dE
      16 dU
      17 dH
      18 HPL
      19 VPL
    """
    rows: List[Dict[str, object]] = []

    with path.open("rt", encoding="utf-8", errors="replace") as f:
        # Skip until header marker (starts with '*'), if present.
        for line in f:
            if line.lstrip().startswith("*"):
                break

        for line in f:
            line = line.strip()
            if not line or line.startswith("*"):
                continue

            toks = line.split()
            if len(toks) < 20:
                continue

            try:
                rows.append(
                    {
                        "time": toks[0],
                        "HDOP": toks[5],
                        "VDOP": toks[6],
                        "GDOP": toks[7],
                        "Nref": toks[11],
                        "Eref": toks[12],
                        "Href": toks[13],
                        "dN": toks[14],
                        "dE": toks[15],
                        "dU": toks[16],
                        "dH": toks[17],
                        "HPL": toks[18],
                        "VPL": toks[19],
                    }
                )
            except IndexError:
                continue

    if not rows:
        raise ValueError(f"No data rows parsed from {path}")

    df = pd.DataFrame(rows)
    df["time"] = pd.to_datetime(df["time"], errors="coerce")

    for c in ALL_NUM_COLS:
        df[c] = pd.to_numeric(df[c], errors="coerce")

    df = df.dropna(subset=["time"]).sort_values("time").reset_index(drop=True)
    return df


def _finite(series: pd.Series) -> np.ndarray:
    x = pd.to_numeric(series, errors="coerce").to_numpy(dtype=float)
    return x[np.isfinite(x)]


def compute_stats_table(df: pd.DataFrame) -> pd.DataFrame:
    """Rows: dE/dN/dU/dH; Cols: N, MEAN, MEDIAN, STD DEV, RMS"""
    rows = []
    for c in STATS_COLS:
        x = _finite(df[c])
        if x.size == 0:
            rows.append([c, 0, np.nan, np.nan, np.nan, np.nan])
        else:
            rows.append(
                [
                    c,
                    int(x.size),
                    float(np.mean(x)),
                    float(np.median(x)),
                    float(np.std(x, ddof=0)),
                    float(math.sqrt(np.mean(x * x))),
                ]
            )
    return pd.DataFrame(rows, columns=["Component", "N", "MEAN", "MEDIAN", "STD DEV", "RMS"])


def build_timeseries_figure(df: pd.DataFrame, title: str) -> go.Figure:
    fig = go.Figure()
    for c in PLOT_COLS:
        fig.add_trace(go.Scatter(x=df["time"], y=df[c], mode="lines", name=c))

    fig.update_layout(
        title=title,
        height=820,
        margin=dict(l=70, r=260, t=70, b=60),
        legend=dict(x=1.02, y=1.0, xanchor="left", yanchor="top", orientation="v"),
        dragmode="zoom",
        hovermode="x unified",
    )

    fig.update_xaxes(
        title_text="Time",
        fixedrange=False,
        rangeslider=dict(visible=True, thickness=0.08),
    )
    fig.update_yaxes(title_text="Value (m or dimensionless)", fixedrange=False)
    return fig


def write_html_with_stats(fig: go.Figure, stats: pd.DataFrame, out_path: Path) -> None:
    plot_div = fig.to_html(
        full_html=False,
        include_plotlyjs="cdn",
        config={"scrollZoom": True, "responsive": True, "displaylogo": False},
    )

    stats_fmt = stats.copy()
    for col in ["MEAN", "MEDIAN", "STD DEV", "RMS"]:
        stats_fmt[col] = stats_fmt[col].map(lambda v: f"{v:.4f}" if np.isfinite(v) else "NaN")

    table_html = stats_fmt.to_html(index=False, classes="stats", border=0)

    html = f"""<!doctype html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>SPP Plot</title>
  <style>
    body {{ font-family: Arial, sans-serif; margin: 18px; }}
    .wrap {{ max-width: 1400px; }}
    h3 {{ margin: 14px 0 8px; }}
    table.stats {{
      border-collapse: collapse;
      width: 100%;
      font-size: 14px;
    }}
    table.stats th, table.stats td {{
      border: 1px solid #ddd;
      padding: 8px 10px;
      text-align: right;
      white-space: nowrap;
    }}
    table.stats th:first-child, table.stats td:first-child {{ text-align: left; }}
  </style>
</head>
<body>
  <div class="wrap">
    {plot_div}
    <h3>Statistics — dE, dN, dU, dH</h3>
    {table_html}
  </div>
</body>
</html>
"""
    out_path.write_text(html, encoding="utf-8")


def build_map_figure(df_map: pd.DataFrame, title: str) -> go.Figure:
    """
    2-row linked view (single concatenated dataset):
      row1: lon/lat scatter (Eref vs Nref)
      row2: Href time series
    """
    g = df_map.reset_index(drop=True).copy()
    g["time_str"] = g["time"].dt.strftime("%Y-%m-%dT%H:%M:%S.%f").str.slice(0, 23)

    fig = make_subplots(
        rows=2,
        cols=1,
        row_heights=[0.62, 0.38],
        vertical_spacing=0.08,
    )

    fig.add_trace(
        go.Scatter(
            x=g["Eref"],
            y=g["Nref"],
            mode="markers",
            name="Nref/Eref",
            customdata=np.stack([g["time_str"].to_numpy(), g["Href"].to_numpy()], axis=1),
            hovertemplate=(
                "Time: %{customdata[0]}<br>"
                "Lat (Nref): %{y:.8f}<br>"
                "Lon (Eref): %{x:.8f}<br>"
                "H (Href): %{customdata[1]:.3f} m<br>"
                "<extra></extra>"
            ),
        ),
        row=1,
        col=1,
    )

    fig.add_trace(
        go.Scatter(
            x=g["time"],
            y=g["Href"],
            mode="lines+markers",
            name="Href",
            customdata=np.stack([g["Nref"].to_numpy(), g["Eref"].to_numpy()], axis=1),
            hovertemplate=(
                "Time: %{x|%Y-%m-%dT%H:%M:%S.%L}<br>"
                "H (Href): %{y:.3f} m<br>"
                "Lat (Nref): %{customdata[0]:.8f}<br>"
                "Lon (Eref): %{customdata[1]:.8f}<br>"
                "<extra></extra>"
            ),
        ),
        row=2,
        col=1,
    )

    fig.update_layout(
        title=title,
        height=900,
        margin=dict(l=70, r=40, t=70, b=60),
        dragmode="zoom",
        hovermode="closest",
        showlegend=False,
    )

    fig.update_xaxes(title_text="Longitude (deg)", fixedrange=False, row=1, col=1)
    fig.update_yaxes(title_text="Latitude (deg)", fixedrange=False, row=1, col=1)
    fig.update_xaxes(title_text="Time", fixedrange=False, row=2, col=1)
    fig.update_yaxes(title_text="Href (m)", fixedrange=False, row=2, col=1)

    return fig


def write_map_html_linked(fig: go.Figure, out_path: Path) -> None:
    """
    Robust HTML writer with hover sync between trace 0 and 1 (single dataset).
    """
    div_id = "spp_map_linked"
    fig_dict = fig.to_plotly_json()

    data_json = json.dumps(fig_dict["data"], cls=PlotlyJSONEncoder)
    layout_json = json.dumps(fig_dict["layout"], cls=PlotlyJSONEncoder)
    config_json = json.dumps({"scrollZoom": True, "responsive": True, "displaylogo": False})

    html = f"""<!doctype html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>SPP Map</title>
  <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
  <style>
    body {{ font-family: Arial, sans-serif; margin: 18px; }}
    .wrap {{ max-width: 1400px; }}
  </style>
</head>
<body>
  <div class="wrap">
    <div id="{div_id}"></div>
  </div>

<script>
  var data = {data_json};
  var layout = {layout_json};
  var config = {config_json};

  var gd = document.getElementById("{div_id}");

  Plotly.newPlot(gd, data, layout, config).then(function() {{

    var isSyncing = false;

    gd.on('plotly_hover', function(ev) {{
      if (isSyncing) return;
      if (!ev || !ev.points || !ev.points.length) return;

      var pt = ev.points[0];
      var pn = pt.pointNumber;
      var cn = pt.curveNumber;

      // only link trace 0 <-> 1
      if (cn !== 0 && cn !== 1) return;
      var other = (cn === 0) ? 1 : 0;

      isSyncing = true;
      try {{
        Plotly.Fx.hover(gd, [{{curveNumber: other, pointNumber: pn}}]);
      }} finally {{
        isSyncing = false;
      }}
    }});

    gd.on('plotly_unhover', function(ev) {{
      try {{ Plotly.Fx.unhover(gd); }} catch(e) {{}}
    }});

  }});
</script>
</body>
</html>
"""
    out_path.write_text(html, encoding="utf-8")


def build_pl_scatter_figure(df: pd.DataFrame, title: str) -> go.Figure:
    """
    Create a log/log integrity scatter plot:
      |dH| vs HPL
      |dU| vs VPL
    with y = x reference line.
    """
    # Use absolute errors
    dH = np.abs(df["dH"].to_numpy())
    dU = np.abs(df["dU"].to_numpy())
    HPL = df["HPL"].to_numpy()
    VPL = df["VPL"].to_numpy()

    # Keep only finite & positive values (required for log scale)
    mask_h = np.isfinite(dH) & np.isfinite(HPL) & (dH > 0) & (HPL > 0)
    mask_v = np.isfinite(dU) & np.isfinite(VPL) & (dU > 0) & (VPL > 0)

    fig = go.Figure()

    fig.add_trace(
        go.Scatter(
            x=dH[mask_h],
            y=HPL[mask_h],
            mode="markers",
            name="Horizontal: |dH| vs HPL",
            marker=dict(size=6),
        )
    )

    fig.add_trace(
        go.Scatter(
            x=dU[mask_v],
            y=VPL[mask_v],
            mode="markers",
            name="Vertical: |dU| vs VPL",
            marker=dict(size=6),
        )
    )

    # Diagonal y = x
    all_vals = np.concatenate([dH[mask_h], HPL[mask_h], dU[mask_v], VPL[mask_v]])
    if all_vals.size > 0:
        lo = all_vals.min()
        hi = all_vals.max()
        fig.add_trace(
            go.Scatter(
                x=[lo, hi],
                y=[lo, hi],
                mode="lines",
                name="y = x",
                line=dict(dash="dot", color="black"),
            )
        )

    fig.update_layout(
        title=title,
        xaxis=dict(
            title="Position Error |d| (m)",
            type="log",
        ),
        yaxis=dict(
            title="Protection Level (m)",
            type="log",
        ),
        height=700,
        legend=dict(x=1.02, y=1.0, xanchor="left", yanchor="top"),
        margin=dict(l=80, r=260, t=70, b=60),
        dragmode="zoom",
    )

    return fig


def write_simple_html(fig: go.Figure, out_path: Path) -> None:
    fig.write_html(
        str(out_path),
        include_plotlyjs="cdn",
        full_html=True,
        config={"scrollZoom": True, "responsive": True, "displaylogo": False},
    )


def derive_map_output_path(main_out: Path) -> Path:
    return main_out.with_name(f"{main_out.stem}_map{main_out.suffix}")


def main() -> int:
    p = argparse.ArgumentParser(description="Plot Ginan SBAS .SPP output as Plotly HTML (concatenated inputs).")
    p.add_argument("-i", "--input", required=True, nargs="+", help="One or more input .SPP / *_SPP.POS files")
    p.add_argument("-o", "--output", default=None, help="Output .html file path (default: <first_input>.html)")
    p.add_argument("--title", default=None, help="Main plot title (default: derived from input filenames)")
    p.add_argument("--map", action="store_true", help="Also write a separate linked-hover lat/lon + Href HTML")
    p.add_argument(
        "--pl", action="store_true", help="Also write a protection-level vs error log/log plot (dH vs HPL, dU vs VPL)"
    )
    args = p.parse_args()

    in_paths = [Path(x).expanduser().resolve() for x in args.input]
    for pth in in_paths:
        if not pth.exists():
            raise FileNotFoundError(f"Input file not found: {pth}")

    main_out = Path(args.output).expanduser().resolve() if args.output else in_paths[0].with_suffix(".html")
    main_out.parent.mkdir(parents=True, exist_ok=True)

    # Read and concatenate
    dfs = [parse_spp_file(pth) for pth in in_paths]
    df = pd.concat(dfs, ignore_index=True).sort_values("time").reset_index(drop=True)

    if args.title:
        title = args.title
    else:
        title = (
            f"SPP Time Series: {len(in_paths)} file(s) (concatenated)  (n={len(df)})"
            if len(in_paths) > 1
            else f"SPP Time Series: {in_paths[0].name}  (n={len(df)})"
        )

    fig = build_timeseries_figure(df, title=title)
    stats = compute_stats_table(df)
    write_html_with_stats(fig, stats, main_out)
    print(f"✅ Wrote main: {main_out}")

    if args.map:
        df_map = df.dropna(subset=["Nref", "Eref", "Href"]).copy()
        if len(df_map) < 2:
            print("⚠️  --map requested, but not enough valid Nref/Eref/Href samples to plot.")
            return 0

        map_out = derive_map_output_path(main_out)
        map_title = f"Reference Position Map + Height (concatenated)  (n={len(df_map)})"
        fig_map = build_map_figure(df_map, title=map_title)
        write_map_html_linked(fig_map, map_out)
        print(f"✅ Wrote map:  {map_out}")

    if args.pl:
        pl_out = main_out.with_name(f"{main_out.stem}_pl{main_out.suffix}")
        pl_title = "SPP Protection Level vs Position Error (log/log)"
        fig_pl = build_pl_scatter_figure(df, pl_title)
        write_simple_html(fig_pl, pl_out)
        print(f"✅ Wrote PL plot: {pl_out}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
