from __future__ import annotations

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
GNSS TRACE residual plotter with optional large-error and ambiguity reset markers.

Features
---------
- Parse residual lines beginning with '%'
- Parse 'LARGE STATE ERROR' and 'LARGE MEAS ERROR' lines (if --mark-large-errors)
- Keep only the highest iteration per observation
- Create separate CODE / PHASE HTML plots, optionally split per satellite or receiver
- Overlay LARGE MEAS (black ▲) and LARGE STATE (orange dashed) markers with concise tooltips
- Overlay ambiguity resets from the preprocessor (green), from the filter (blue)
- Create CODE & PHASE weighted and unweighted residual heatmaps of mean/std_dev/rms.
- Create cumulative and total ambiguity reset plots
"""

import os, glob
import re
import argparse
from pathlib import Path
from typing import Iterable, Optional, List, Dict, Tuple
from datetime import datetime


def ensure_parent(p) -> None:
    """Create parent directory for a path if it doesn't exist."""
    from pathlib import Path as _P

    try:
        _P(p).parent.mkdir(parents=True, exist_ok=True)
    except Exception:
        pass


def _sanitize_filename_piece(s: str) -> str:
    """Sanitize string for filenames using underscores."""
    return re.sub(r"[^A-Za-z0-9._-]+", "_", str(s))


def build_out_path(
    base: str,
    variant_suffix: str,
    short: str,
    *,
    split: str | None = None,  # "recv" | "sat" | None
    key: str | None = None,  # station or satellite ID
    tag: str | None = None,  # e.g., "residual", "h"/"v" for totals
    ext: str = "html",
) -> str:
    """
    Build consistent output names like:
      <base><variant>_<short>[_<tag>][_recv|_sat]_[<key>].<ext>
    """
    parts = [f"{base}{variant_suffix}", short]
    if tag:
        parts.append(tag)
    if split in ("recv", "sat"):
        parts.append(split)
    if key:
        parts.append(_sanitize_filename_piece(key))
    return "_".join(parts) + f".{ext}"


def slugify(text: str) -> str:
    """Return a safe slug for filenames: lowercase, alnum-plus-dashes."""
    import re

    t = re.sub(r"[^A-Za-z0-9]+", "-", text).strip("-").lower()
    return re.sub(r"-{2,}", "-", t) or "out"


import logging

logger = logging.getLogger("plot_trace_res")


def _setup_logging(level: str = "INFO") -> None:
    """Configure root logger for the script."""
    lvl = getattr(logging, level.upper(), logging.INFO)
    logging.basicConfig(
        level=lvl,
        format="%(asctime)s | %(levelname)s | %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )
    logger.setLevel(lvl)


from typing import Any, Dict, Iterable, Iterator, List, Literal, Optional, Sequence, Tuple

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.io as pio

pio.templates.default = None

import numpy as np
from collections import defaultdict

# -------- Parsing --------

FLOAT = r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?"

# AJC - added -? to iter group to allow parsing -1 from smoothed TRACE file
LINE_RE = re.compile(
    rf"""
    ^%\s+
    (?P<iter>-?\d+)\s+
    (?P<date>\d{{4}}-\d{{2}}-\d{{2}})\s+                 # e.g. 2025-10-05
    (?P<time>\d{{2}}:\d{{2}}:\d{{2}}(?:\.\d+)?)\s+       # e.g. 00:01:00.00
    (?P<meas>(?:CODE_MEAS|PHAS_MEAS))\s+
    (?P<sat>\S+)\s+
    (?P<recv>\S+)\s+
    (?P<sig>\S+)\s+
    (?P<prefit>{FLOAT})\s+
    (?P<postfit>{FLOAT})\s+
    (?P<sigma>{FLOAT})
    (?:\s+(?P<prefit_ratio>{FLOAT})\s+(?P<postfit_ratio>{FLOAT}))?  # optional at higher trace level
    \s+(?P<label>\S+)\s*$
    """,
    re.VERBOSE,
)

# -------- Large-error parser --------

LARGE_RE = re.compile(
    r"""^(?P<date>\d{4}-\d{2}-\d{2})\s+
        (?P<time>\d{2}:\d{2}:\d{2}\.\d+)\s+
        LARGE\s+(?P<kind>STATE|MEAS)\s+ERROR\s+OF\s*:\s*
        (?P<value>[0-9.]+)\s+AT\s+\d+\s*:\s*
        (?:(?P<meas_type>\S+)\s+(?P<sat>\S+)\s+(?P<recv>\S+)\s+(?P<sig>\S+)
        |
        (?P<param>\S+)\s+(?P<recv2>\S+)\s+(?P<comp>\S+))""",
    re.VERBOSE,
)

# -------- Ambiguity reset parser --------
# Examples:
# 2025-10-06 04:25:00.00  Ambiguity Removed      - PREPROC  AMBIGUITY  G03  BOGT  L1C  - GF  - SCDIA
# 2025-10-06 04:25:00.00  Ambiguity Removed      -  REJECT  AMBIGUITY  G27  DGAR  L2W

AMB_RE = re.compile(
    r"""^(?P<date>\d{4}-\d{2}-\d{2})\s+
        (?P<time>\d{2}:\d{2}:\d{2}\.\d+)\s+
        Ambiguity\ Removed\s+
        -\s+(?P<action>PREPROC|REJECT)\s+
        AMBIGUITY\s+
        (?P<sat>\S+)\s+
        (?P<recv>\S+)\s+
        (?P<sig>\S+)
        (?P<rest>.*)$
    """,
    re.VERBOSE,
)


def parse_ambiguity_resets(lines: Iterable[str]) -> pd.DataFrame:
    """
    Parse 'Ambiguity Removed' lines into a DataFrame.

    Returns columns:
      datetime, action (PREPROC/REJECT), sat, recv, sig, reasons (comma-joined)
    """
    recs = []
    for ln in lines:
        if "Ambiguity Removed" not in ln:
            continue
        m = AMB_RE.match(ln.rstrip("\n"))
        if not m:
            continue
        gd = m.groupdict()
        dt = pd.to_datetime(f"{gd['date']} {gd['time']}", errors="coerce")

        # --- Parse reasons from tail ---
        tail = gd.get("rest", "")
        reasons = []
        for chunk in tail.split("-"):
            t = chunk.strip()
            if not t:
                continue
            if t.upper() in {"PREPROC", "REJECT", "AMBIGUITY"}:
                continue
            reasons.append(t)

        # Clean, dedupe while preserving order
        clean = []
        seen = set()
        for r in reasons:
            rr = re.sub(r"\s+", " ", r)
            if rr not in seen:
                seen.add(rr)
                clean.append(rr)

        # --- Add 'KF' reason for REJECT (Kalman filter), if not already present ---
        action_str = gd["action"].strip().upper()
        if action_str == "REJECT":
            # check case-insensitively if KF already present
            present_upper = {x.upper() for x in clean}
            if "KF" not in present_upper:
                clean.insert(0, "KF")  # put first so it’s prominent (optional)

        recs.append(
            {
                "datetime": dt,
                "action": action_str,  # PREPROC / REJECT
                "sat": gd["sat"].strip(),
                "recv": gd["recv"].strip(),
                "sig": gd["sig"].strip(),
                "reasons": ", ".join(clean),
            }
        )

    amb = pd.DataFrame.from_records(recs)
    return amb


def _to_float_or_nan(x: str) -> float:
    try:
        return float(x)
    except Exception:
        return float("nan")


def parse_trace_lines(lines: Iterable[str]) -> pd.DataFrame:
    records = []
    for ln in lines:
        if not ln.startswith("%"):
            continue
        m = LINE_RE.match(ln)
        if not m:
            continue
        gd = m.groupdict()

        rec = {
            "iter": int(gd["iter"]),
            "date": gd["date"],
            "time": gd["time"],
            "meas": gd["meas"],
            "sat": gd["sat"],
            "recv": gd["recv"],
            "sig": gd["sig"],
            "prefit": _to_float_or_nan(gd["prefit"]),
            "postfit": _to_float_or_nan(gd["postfit"]),
            "sigma": _to_float_or_nan(gd["sigma"]),
            "label": gd["label"],
        }

        # Optional higher-trace columns
        pr = gd.get("prefit_ratio")
        por = gd.get("postfit_ratio")
        rec["prefit_ratio"] = _to_float_or_nan(pr) if pr is not None else float("nan")
        rec["postfit_ratio"] = _to_float_or_nan(por) if por is not None else float("nan")

        records.append(rec)

    if not records:
        return pd.DataFrame(
            columns=[
                "iter",
                "date",
                "time",
                "meas",
                "sat",
                "recv",
                "sig",
                "prefit",
                "postfit",
                "sigma",
                "label",
                "prefit_ratio",
                "postfit_ratio",
                "datetime",
            ]
        )

    df = pd.DataFrame.from_records(records)
    # keep your strict format if your inputs are always YYYY-MM-DD
    df["datetime"] = pd.to_datetime(df["date"] + " " + df["time"], format="%Y-%m-%d %H:%M:%S.%f", errors="coerce")
    return df


def parse_trace_lines_fast(lines: Iterable[str]) -> pd.DataFrame:
    """
    Supports both classic 12-field format and the higher-trace 14-field format:
      Classic:
        % iter date time meas sat recv sig prefit postfit sigma label
      With ratios:
        % iter date time meas sat recv sig prefit postfit sigma prefit_ratio postfit_ratio label

    Parameters
    ----------
    lines : Iterable[str]
        Iterable of text lines (e.g. from open(file).readlines()).

    Returns
    -------
    pandas.DataFrame
        Columns:
            - iter     : int   — filter iteration number
            - date     : str   — date token from TRACE line
            - time     : str   — time token from TRACE line
            - meas     : str   — measurement type ("PHAS_MEAS" or "CODE_MEAS")
            - sat      : str   — satellite identifier (e.g. "G20")
            - recv     : str   — receiver/station code
            - sig      : str   — signal name or frequency (e.g. "L1C")
            - prefit   : float — prefit residual value
            - postfit  : float — postfit residual value
            - sigma    : float — measurement sigma
            - label    : str   — signal label or channel
            - datetime : pandas.Timestamp — parsed from date+time (NaT if invalid)

    Notes
    -----
    - Lines not beginning with '%' or with fewer than 12 fields are ignored.
    - This "fast" variant performs minimal validation and assumes consistent formatting.
      For more robust parsing (e.g. mixed whitespace, missing fields), use
      `parse_trace_lines()` instead.
    - Conversion to datetime is vectorized for efficiency.
    """

    iters, dates, times, meas, sats, recvs, sigs = [], [], [], [], [], [], []
    prefit, postfit, sigma, labels = [], [], [], []
    pratio, poratio = [], []

    a_it, a_d, a_t, a_m, a_s, a_r, a_g = (
        iters.append,
        dates.append,
        times.append,
        meas.append,
        sats.append,
        recvs.append,
        sigs.append,
    )
    a_pf, a_po, a_si, a_l = prefit.append, postfit.append, sigma.append, labels.append
    a_pr, a_por = pratio.append, poratio.append

    for ln in lines:
        if not ln or ln[0] != "%":
            continue
        parts = ln.split()
        # Classic must have at least 12 tokens; with ratios it's 14 tokens
        if len(parts) < 12:
            continue

        # Common fields
        a_it(int(parts[1]))
        a_d(parts[2])
        a_t(parts[3])
        a_m(parts[4])
        a_s(parts[5])
        a_r(parts[6])
        a_g(parts[7])
        a_pf(float(parts[8]))
        a_po(float(parts[9]))
        a_si(float(parts[10]))

        if len(parts) >= 14:
            # With ratios
            a_pr(float(parts[11]))
            a_por(float(parts[12]))
            a_l(parts[13])
        else:
            # Classic
            a_pr(float("nan"))
            a_por(float("nan"))
            a_l(parts[11])

    if not iters:
        return pd.DataFrame(
            columns=[
                "iter",
                "date",
                "time",
                "meas",
                "sat",
                "recv",
                "sig",
                "prefit",
                "postfit",
                "sigma",
                "label",
                "prefit_ratio",
                "postfit_ratio",
                "datetime",
            ]
        )

    df = pd.DataFrame(
        {
            "iter": iters,
            "date": dates,
            "time": times,
            "meas": meas,
            "sat": sats,
            "recv": recvs,
            "sig": sigs,
            "prefit": prefit,
            "postfit": postfit,
            "sigma": sigma,
            "label": labels,
            "prefit_ratio": pratio,
            "postfit_ratio": poratio,
        }
    )
    # Flexible parsing here to tolerate either “-” or “/” in date, and variable subsecond
    df["datetime"] = pd.to_datetime(df["date"] + " " + df["time"], errors="coerce")
    return df


def parse_large_errors(lines: Iterable[str]) -> pd.DataFrame:
    recs = []
    for ln in lines:
        if "LARGE" not in ln:
            continue
        m = LARGE_RE.match(ln)
        if not m:
            continue
        gd = m.groupdict()
        dt = pd.to_datetime(gd["date"] + " " + gd["time"])
        kind = gd["kind"]
        val = float(gd["value"])
        if kind == "STATE":
            recs.append(
                {
                    "datetime": dt,
                    "kind": kind,
                    "value": val,
                    "recv": gd["recv2"],
                    "param": gd["param"],
                    "comp": gd["comp"],
                }
            )
        else:
            recs.append(
                {
                    "datetime": dt,
                    "kind": kind,
                    "value": val,
                    "meas_type": gd["meas_type"],
                    "sat": gd["sat"],
                    "recv": gd["recv"],
                    "sig": gd["sig"],
                }
            )
    return pd.DataFrame.from_records(recs)


# -------- Filtering & last-iteration selection --------


def filter_df(
    df: pd.DataFrame,
    receivers: Optional[List[str]],
    sats: Optional[List[str]],
    label_regex: Optional[str],
) -> pd.DataFrame:
    out = df
    if receivers:
        recvu = [r.upper() for r in receivers]
        out = out[out["recv"].str.upper().isin(recvu)]
    if sats:
        sats_u = [s.upper() for s in sats]
        out = out[out["sat"].str.upper().isin(sats_u)]
    if label_regex:
        rx = re.compile(label_regex)
        out = out[out["label"].apply(lambda s: bool(rx.search(s)))]
    return out


def keep_last_iteration(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return df
    keys = ["datetime", "meas", "sat", "recv", "sig", "label"]
    return (
        df.sort_values(["datetime", "iter"])
        .drop_duplicates(subset=keys, keep="last")
        .sort_values(["datetime", "sat", "sig"])
        .reset_index(drop=True)
    )


def _sat_sort_key(sat: str):
    """Return a tuple (constellation, number) for natural GNSS satellite sorting."""
    if not isinstance(sat, str):
        return ("", 0)
    m = re.match(r"([A-Z])(\d+)", sat)
    if m:
        prefix, num = m.groups()
        return (prefix, int(num))
    return (sat, 0)


def _recv_sort_key(recv: str):
    return (recv or "").upper()


def _parse_dt_like(s: str, df_dates: pd.Series) -> Optional[pd.Timestamp]:
    if not s:
        return None
    s = s.strip()
    try:
        return pd.to_datetime(s, errors="raise")
    except Exception:
        pass
    for fmt in ("%H:%M:%S.%f", "%H:%M:%S"):
        try:
            t = pd.to_datetime(s, format=fmt, errors="raise").time()
            break
        except Exception:
            t = None
    if t is None or df_dates.empty:
        return None
    d0 = df_dates.iloc[0]
    base_date = pd.Timestamp(year=d0.year, month=d0.month, day=d0.day)
    return pd.Timestamp.combine(base_date.date(), t)


def decimate_per_pair(df: pd.DataFrame, n: int) -> pd.DataFrame:
    if n <= 1 or df.empty:
        return df
    df = df.sort_values(["sat", "label", "datetime"]).copy()
    idx = (df.groupby(["sat", "label"]).cumcount() % n) == 0
    return df[idx]


def filter_ambiguity_resets(
    df: pd.DataFrame,
    receivers: Optional[List[str]],
    sats: Optional[List[str]],
    start_dt: Optional[pd.Timestamp],
    end_dt: Optional[pd.Timestamp],
) -> pd.DataFrame:
    if df is None or df.empty:
        return df
    out = df.copy()
    if receivers:
        rset = {r.upper() for r in receivers}
        out = out[out["recv"].str.upper().isin(rset)]
    if sats:
        sset = {s.upper() for s in sats}
        out = out[out["sat"].str.upper().isin(sset)]
    if start_dt is not None:
        out = out[out["datetime"] >= start_dt]
    if end_dt is not None:
        out = out[out["datetime"] < end_dt]
    return out.reset_index(drop=True)


def require_cols(df, name, cols):
    missing = [c for c in cols if c not in df.columns]
    if missing:
        raise ValueError(f"{name}: missing required columns {missing}; have={list(df.columns)}")
    return df


def log_cols(df, tag):
    logger.info("%s: %s", tag, list(df.columns))


# -------- Large-error filtering to match CLI/time --------


def filter_large_errors(
    df_large: pd.DataFrame,
    receivers: Optional[List[str]],
    sats: Optional[List[str]],
    start_dt: Optional[pd.Timestamp],
    end_dt: Optional[pd.Timestamp],
) -> pd.DataFrame:
    if df_large is None or df_large.empty:
        return df_large

    out = df_large.copy()

    # Ensure columns exist
    for col in ["sat", "recv"]:
        if col not in out.columns:
            out[col] = pd.NA

    # Time window
    if start_dt is not None:
        out = out[out["datetime"] >= start_dt]
    if end_dt is not None:
        out = out[out["datetime"] < end_dt]

    # Receiver filter (applies to STATE and MEAS)
    if receivers:
        recvu = [r.upper() for r in receivers]
        out = out[out["recv"].astype("string").str.upper().isin(recvu)]

    # Satellite filter (MEAS only)
    if sats:
        sats_u = [s.upper() for s in sats]
        is_meas = out["kind"] == "MEAS"
        sat_ok = out["sat"].astype("string").str.upper().isin(sats_u)
        out = out[~is_meas | (is_meas & sat_ok)]

    return out.reset_index(drop=True)


def _insert_gap_breaks_multi(
    x: np.ndarray,  # datetime64[ns]
    y: np.ndarray,  # numeric 1D
    cd: np.ndarray,  # customdata 2D (N,K)
    gap_seconds: float = 3600.0,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Insert NaN/NaT rows after any time gap > gap_seconds so Plotly breaks the line.
    Expands x, y, and cd together so hover data stays perfectly aligned.

    Returns:
      x2, y2, cd2 — same shape relationship as inputs but with extra NaN/NaT rows.
    """
    if x.size == 0:
        return x, y, cd

    # Compute time gaps in seconds
    dt = (x[1:].astype("datetime64[ns]").astype("int64") - x[:-1].astype("datetime64[ns]").astype("int64")) / 1e9
    gaps = dt > float(gap_seconds)
    n_breaks = int(np.count_nonzero(gaps))
    if n_breaks == 0:
        return x, y, cd

    N, K = cd.shape if cd.ndim == 2 else (cd.shape[0], 1)
    new_len = x.size + n_breaks

    x2 = np.empty(new_len, dtype="datetime64[ns]")
    y2 = np.empty(new_len, dtype=float)
    cd2 = np.empty((new_len, K), dtype=object)

    j = 0
    for i in range(x.size):
        x2[j] = x[i]
        y2[j] = y[i]
        cd2[j, :] = cd[i, :] if cd.ndim == 2 else [cd[i]]
        j += 1
        if i < x.size - 1 and gaps[i]:
            # Insert a visual break
            x2[j] = np.datetime64("NaT")
            y2[j] = np.nan
            cd2[j, :] = [None] * K  # placeholder row to preserve alignment
            j += 1

    return x2, y2, cd2


def build_lookup_cache(
    df_lookup: pd.DataFrame, yfield: str
) -> Dict[Tuple[str, str, str], Tuple[np.ndarray, np.ndarray]]:
    """
    Map (sat, recv, label) -> (t_ns_sorted:int64[ns], y_sorted:float64) for quick nearest lookup.
    """
    cache: Dict[Tuple[str, str, str], Tuple[np.ndarray, np.ndarray]] = {}
    if df_lookup is None or df_lookup.empty:
        return cache

    # Only the columns we need; enforce dtypes
    w = df_lookup[["sat", "recv", "label", "datetime", yfield]].copy()
    w[yfield] = pd.to_numeric(w[yfield], errors="coerce")

    # Group once; turn each group into two NumPy arrays, sorted by time
    for (sat, recv, label), g in w.groupby(["sat", "recv", "label"], sort=False):
        t = g["datetime"].to_numpy("datetime64[ns]").astype("int64")
        y = g[yfield].to_numpy(dtype="float64")
        if t.size == 0:
            continue
        order = np.argsort(t)
        cache[(str(sat), str(recv), str(label))] = (t[order], y[order])
    return cache


# -------- Plotting --------
def make_plot(
    df: pd.DataFrame,
    residual_field: str,
    title: str,
    use_webgl: bool = False,
    df_large: pd.DataFrame = pd.DataFrame(),
    context: Dict[str, str] = None,
    hover_unified: bool = False,
    df_lookup: pd.DataFrame = None,
    lookup_cache: Dict[Tuple[str, str, str], Tuple[np.ndarray, np.ndarray]] = None,
    *,  # ← everything after this must be keyword-only
    show_stats: bool = False,
    df_amb: pd.DataFrame = pd.DataFrame(),
) -> go.Figure:
    """
    Residual time-series with:
      - range slider on the main x-axis
      - box-zoom (x+y)
      - optional stats table (row 2, never overlapped)
      - large error overlays and PHASE ambiguity overlays
    """

    if df.empty:
        fig = go.Figure()
        fig.update_layout(
            title="No residuals matched your filters",
            xaxis_title="Time",
            yaxis_title=f"{residual_field.capitalize()} residual",
            template="plotly_white",
            height=560,
        )
        return fig

    # at the top of make_plot(), after the empty-df early return
    if df_amb is None:
        raise ValueError("df_amb is None (expected a DataFrame).")

    # Strong check only when we expect ambiguities (i.e., PHAS_MEAS pages)
    if (context or {}).get("meas_type") == "PHAS_MEAS":
        if df_amb.empty:
            logging.getLogger().warning("make_plot: df_amb is empty on a PHAS_MEAS page.")
        else:
            required = {"datetime", "sat", "recv", "sig", "action"}
            missing = required - set(df_amb.columns)
            if missing:
                raise ValueError(f"df_amb missing required columns {missing}; have={list(df_amb.columns)}")

    # ---------- Subplots: 3 rows when stats (main | slider | table), else 2 rows ----------
    if show_stats:
        fig = make_subplots(
            rows=3,
            cols=1,
            shared_xaxes=False,
            row_heights=[0.72, 0.10, 0.18],  # main | slider | table
            vertical_spacing=0.06,
            specs=[
                [{"type": "xy"}],
                [{"type": "xy"}],  # host the rangeslider only (no data)
                [{"type": "table"}],
            ],
        )
        add_trace_xy = lambda tr: fig.add_trace(tr, row=1, col=1)
        add_vline_xy = lambda **kw: fig.add_vline(row=1, col=1, **kw)
        table_row = 3
        fig_height = 900
    else:
        fig = go.Figure()
        add_trace_xy = lambda tr: fig.add_trace(tr)
        add_vline_xy = lambda **kw: fig.add_vline(**kw)
        table_row = None
        fig_height = 600

    # ---------- Main series ----------
    TraceCls = go.Scattergl if (use_webgl and len(df) > 20000) else go.Scatter
    sat_legend_shown: Dict[str, bool] = {}

    traces = []
    # Natural satellite order (e.g., G01 < G02 < ... < R01 ...)
    sorted_groups = sorted(df.groupby(["sat", "label"], sort=False), key=lambda kv: _sat_sort_key(kv[0][0]))
    for (sat, label), g in sorted_groups:
        if g.empty:
            continue
        g = g.sort_values("datetime").copy()

        x = g["datetime"].to_numpy("datetime64[ns]")
        y = pd.to_numeric(g[residual_field], errors="coerce").to_numpy()

        cd = np.column_stack(
            [
                g["sat"].to_numpy(object),
                g["recv"].to_numpy(object),
                g["sig"].to_numpy(object),
                pd.to_numeric(g["sigma"], errors="coerce").to_numpy(),
                g["meas"].to_numpy(object),
                g["label"].to_numpy(object),
            ]
        )

        x2, y2, cd2 = _insert_gap_breaks_multi(x, y, cd, gap_seconds=3600.0)

        show_leg = not sat_legend_shown.get(sat, False)
        sat_legend_shown[sat] = True

        traces.append(
            TraceCls(
                x=x2,
                y=y2,
                mode="lines+markers",
                name=str(sat),
                legendgroup=str(sat),
                showlegend=show_leg,
                customdata=cd2,
                hovertemplate=(
                    "Time=%{x|%Y-%m-%d %H:%M:%S.%f}<br>"
                    f"{('Normalised residual (res/σ)' if residual_field.startswith('norm_') else residual_field.capitalize() + ' residual')}=%{{y:.6g}}<br>"
                    "Sat=%{customdata[0]} | Recv=%{customdata[1]}<br>"
                    "Sig=%{customdata[2]} | σ=%{customdata[3]:.6g}<br>"
                    "Type=%{customdata[4]}<br>"
                    "Label=%{customdata[5]}<extra></extra>"
                ),
            )
        )

    if traces:
        if show_stats:
            for tr in traces:
                fig.add_trace(tr, row=1, col=1)
        else:
            fig.add_traces(traces)

    # ---------- Y-range & overlay anchors ----------
    y_vals = pd.to_numeric(df[residual_field], errors="coerce").to_numpy()
    finite = np.isfinite(y_vals)
    if finite.any():
        y_min = float(np.min(y_vals[finite]))
        y_max = float(np.max(y_vals[finite]))
        if y_max == y_min:
            y_min, y_max = y_min - 1.0, y_max + 1.0
    else:
        y_min, y_max = -1.0, 1.0
    y_rng = max(1e-12, (y_max - y_min))

    # Marker anchors & v-line span
    y_line_pad = 0.02 * y_rng
    # separate lanes for ambiguity markers:
    y_top_marker_amb_preproc = y_max + 0.05 * y_rng  # PREPROC (green) higher
    y_top_marker_amb_reject = y_max + 0.04 * y_rng  # REJECT  (blue) a bit lower
    y_top_marker_error = y_max + 0.03 * y_rng  # large errors (▲/▼) below those
    y_line_lo = y_min - y_line_pad
    y_line_hi = max(y_max + y_line_pad, y_top_marker_amb_preproc + 0.02 * y_rng)

    def _add_vline_trace(dt, *, color="gray", dash="dash", group=None, width=0.5):
        add_trace_xy(
            go.Scatter(
                x=[dt, dt],
                y=[y_line_lo, y_line_hi],
                mode="lines",
                line=dict(color=color, width=width, dash=dash),
                hoverinfo="skip",
                showlegend=False,
                legendgroup=(str(group) if group is not None else None),
            )
        )

    # ---------- LARGE errors overlay ----------
    if df_large is not None and not df_large.empty:
        ctx_sat = (context or {}).get("sat")
        ctx_recv = (context or {}).get("recv")
        ctx_meas = (context or {}).get("meas_type")

        dfL = df_large.copy()
        if ctx_meas:
            dfL = dfL[(dfL["kind"] == "STATE") | ((dfL["kind"] == "MEAS") & (dfL["meas_type"] == ctx_meas))]
        if ctx_sat:
            dfL = dfL[(dfL["kind"] == "MEAS") & (dfL["sat"] == ctx_sat)]
        elif ctx_recv:
            dfL = dfL[
                ((dfL["kind"] == "MEAS") & (dfL["recv"] == ctx_recv))
                | ((dfL["kind"] == "STATE") & (dfL["recv"] == ctx_recv))
            ]

        for _, row in dfL.iterrows():
            if row["kind"] == "STATE":
                hovertext = (
                    "<b>LARGE STATE ERROR</b><br>"
                    f"Value={row['value']:.3f}<br>"
                    f"Param={row['param']}<br>"
                    f"Recv={row['recv']}<br>"
                    f"Comp={row['comp']}"
                )
                _add_vline_trace(row["datetime"], color="orange", dash="dot", group=ctx_sat, width=0.6)
                add_trace_xy(
                    go.Scatter(
                        x=[row["datetime"]],
                        y=[y_top_marker_error],
                        mode="markers",
                        marker=dict(color="orange", size=10, symbol="triangle-down"),
                        hovertemplate=hovertext + "<extra></extra>",
                        showlegend=False,
                        legendgroup=(str(ctx_sat) if ctx_sat else None),
                    )
                )
            else:
                hovertext = (
                    "<b>LARGE MEAS ERROR</b><br>"
                    f"Value={row['value']:.3f}<br>"
                    f"Type={row['meas_type']}<br>"
                    f"Sat={row['sat']}<br>"
                    f"Recv={row['recv']}<br>"
                    f"Sig={row['sig']}"
                )
                # Place ▲ at nearest series y to event time (if available)
                y_val = 0.0
                if lookup_cache is not None:
                    label_prefix = "L-" if str(row.get("meas_type", "")).startswith("PHAS") else "P-"
                    target_label = f"{label_prefix}{row.get('sig', '')}"
                    key = (str(row.get("sat")), str(row.get("recv")), target_label)
                    series = lookup_cache.get(key)
                    if series is not None:
                        t_ns, y_arr = series
                        t_err = pd.Timestamp(row["datetime"]).to_datetime64().astype("int64")
                        pos = np.searchsorted(t_ns, t_err)
                        best_idx, best_d = None, None
                        for j in (pos - 1, pos):
                            if 0 <= j < t_ns.size:
                                d = abs(t_ns[j] - t_err)
                                if best_d is None or d < best_d:
                                    best_d, best_idx = d, j
                        if best_idx is not None and best_d is not None and best_d <= 1_000_000_000:
                            y_val = float(y_arr[best_idx])

                add_trace_xy(
                    go.Scatter(
                        x=[row["datetime"]],
                        y=[y_val],
                        mode="markers",
                        marker=dict(color="black", size=10, symbol="triangle-up"),
                        hovertemplate=hovertext + "<extra></extra>",
                        showlegend=False,
                        legendgroup=str(row["sat"]),
                    )
                )

    # ---------- Ambiguity resets (PHAS_MEAS only) ----------
    ctx_recv = (context or {}).get("recv")
    ctx_sat = (context or {}).get("sat")
    ctx_meas = (context or {}).get("meas_type")

    amb_overlay = pd.DataFrame()
    amb_counts = pd.DataFrame()

    if ctx_meas == "PHAS_MEAS" and df_amb is not None and not df_amb.empty:
        # Start from the single source df (already schema-checked in main/make_plot)
        amb_src = df_amb.copy()

        # Normalize the fields we filter/group on (string, trimmed)
        for c in ("recv", "sat", "sig", "action"):
            amb_src[c] = amb_src[c].astype(str).str.strip()

        # Apply page filters (case-insensitive compare)
        if ctx_recv:
            amb_src = amb_src[amb_src["recv"].str.upper() == str(ctx_recv).upper()]
        if ctx_sat:
            amb_src = amb_src[amb_src["sat"].str.upper() == str(ctx_sat).upper()]

        # ---- 1) Overlays: keep per-signal rows for detailed hover text ----
        # Don't aggregate here - let add_ambiguity_markers_combined create one marker
        # per datetime/sat/recv/action while preserving per-signal details in hover
        amb_overlay = amb_src[["datetime", "sat", "recv", "action", "sig", "reasons"]].copy()

        # Determine split context for hover text grouping
        split_ctx = None
        if ctx_recv:
            split_ctx = "recv"
        elif ctx_sat:
            split_ctx = "sat"

        if not amb_overlay.empty:
            amb_preproc = amb_overlay[amb_overlay["action"].str.upper() == "PREPROC"]
            if not amb_preproc.empty:
                add_ambiguity_markers_combined(
                    fig,
                    amb_preproc,
                    y_anchor=y_top_marker_amb_preproc,
                    y_span=(y_line_lo, y_line_hi),
                    line_width=0.5,
                    marker_size=11,
                    line_color_preproc="rgba(0,128,0,1.0)",
                    line_color_reject="rgba(65,105,225,1.0)",  # unused in this call
                    split_by_reason=True,
                    split_context=split_ctx,
                    add_trace_fn=add_trace_xy,
                )

            amb_reject = amb_overlay[amb_overlay["action"].str.upper() == "REJECT"]
            if not amb_reject.empty:
                add_ambiguity_markers_combined(
                    fig,
                    amb_reject,
                    y_anchor=y_top_marker_amb_reject,
                    y_span=(y_line_lo, y_line_hi),
                    line_width=0.5,
                    marker_size=11,
                    line_color_preproc="rgba(0,128,0,1.0)",  # unused in this call
                    line_color_reject="rgba(65,105,225,1.0)",
                    split_by_reason=True,
                    split_context=split_ctx,
                    add_trace_fn=add_trace_xy,
                )

        # ---- 2) Counts: explode sig so we get one row per signal ----
        if "sig" in amb_src.columns and not amb_src.empty:
            amb_counts = amb_src.copy()
            amb_counts["sig"] = (
                amb_counts["sig"]
                .astype(str)
                .str.replace(r"\s+", "", regex=True)  # strip spaces in "L1C, L2W"
                .str.split(",")
            )
            amb_counts = amb_counts.explode("sig", ignore_index=True)
            amb_counts["sig"] = amb_counts["sig"].astype(str).str.upper().str.strip()
            amb_counts = amb_counts[amb_counts["sig"] != ""]  # drop empties

    # ---------- Stats table (with per-label counts) ----------
    if show_stats:
        work = df.copy()
        work["_y"] = pd.to_numeric(work[residual_field], errors="coerce")
        work["_sigma"] = pd.to_numeric(work["sigma"], errors="coerce")
        work_w = work[np.isfinite(work["_y"]) & np.isfinite(work["_sigma"]) & (work["_sigma"] > 0)].copy()
        work_u = work[np.isfinite(work["_y"])].copy()

        def _uw_stats(grp: pd.DataFrame):
            y = grp["_y"].to_numpy(float)
            n = y.size
            if n == 0:
                return pd.Series({"N": 0, "Mean": np.nan, "Std": np.nan, "RMS": np.nan})
            return pd.Series(
                {
                    "N": int(n),
                    "Mean": float(np.nanmean(y)),
                    "Std": float(np.nanstd(y, ddof=1)) if n > 1 else np.nan,
                    "RMS": float(np.sqrt(np.nanmean(y**2))),
                }
            )

        def _w_stats(grp: pd.DataFrame):
            y = grp["_y"].to_numpy(float)
            w = 1.0 / (grp["_sigma"].to_numpy(float) ** 2)
            sw = float(w.sum())
            n = y.size
            if n == 0 or sw <= 0:
                return pd.Series({"WMean": np.nan, "WStd": np.nan, "WRMS": np.nan})
            mu = float((w * y).sum() / sw)
            var = float((w * (y - mu) ** 2).sum() / sw)
            return pd.Series(
                {"WMean": mu, "WStd": float(np.sqrt(var)), "WRMS": float(np.sqrt((w * (y**2)).sum() / sw))}
            )

        try:
            uw = work_u.groupby("label", sort=False).apply(_uw_stats, include_groups=False).reset_index()
        except TypeError:
            uw = (
                work_u.groupby("label", sort=False, group_keys=False)
                .apply(lambda g: _uw_stats(g.drop(columns=["label"])))
                .reset_index()
            )

        if not work_w.empty:
            try:
                w = work_w.groupby("label", sort=False).apply(_w_stats, include_groups=False).reset_index()
            except TypeError:
                w = (
                    work_w.groupby("label", sort=False, group_keys=False)
                    .apply(lambda g: _w_stats(g.drop(columns=["label"])))
                    .reset_index()
                )
        else:
            w = pd.DataFrame({"label": [], "WMean": [], "WStd": [], "WRMS": []})

        stats = pd.merge(uw, w, on="label", how="outer")
        labels = stats["label"].astype(str).tolist() if not stats.empty else sorted(df["label"].astype(str).unique())

        # ---- Per-label counts (only for PHAS_MEAS pages) ----
        pp_map, kf_map = {}, {}
        if ctx_meas == "PHAS_MEAS" and amb_counts is not None and not amb_counts.empty:
            act = amb_counts["action"].astype(str).str.upper()
            lbl = "L-" + amb_counts["sig"].astype(str)

            pp_map = {str(k): int(v) for k, v in lbl[act == "PREPROC"].value_counts().items()}
            kf_map = {str(k): int(v) for k, v in lbl[act == "REJECT"].value_counts().items()}

        # Large MEAS per label (▲), in this page’s context (PHASE only)
        me_map = {}
        if df_large is not None and not df_large.empty and ctx_meas == "PHAS_MEAS":
            dfl = df_large.copy()
            if ctx_recv:
                dfl = dfl[dfl["recv"].astype(str) == str(ctx_recv)]
            if ctx_sat:
                dfl = dfl[dfl["sat"].astype(str) == str(ctx_sat)]
            dfl_me = dfl[(dfl["kind"] == "MEAS") & (dfl["meas_type"] == "PHAS_MEAS")]
            if not dfl_me.empty and "sig" in dfl_me.columns:
                vc_me = ("L-" + dfl_me["sig"].astype(str)).value_counts()
                me_map = {str(k): int(v) for k, v in vc_me.items()}

        # Large STATE total (orange ▼) — receiver-level; same value per row
        state_total = 0
        if df_large is not None and not df_large.empty:
            dfl2 = df_large.copy()
            if ctx_recv:
                dfl2 = dfl2[dfl2["recv"].astype(str) == str(ctx_recv)]
            state_total = int(dfl2[dfl2["kind"] == "STATE"].shape[0])

        # Build columns aligned to the table’s label order
        pp_col = [str(pp_map.get(lbl, 0)) for lbl in labels]
        kf_col = [str(kf_map.get(lbl, 0)) for lbl in labels]
        me_col = [str(me_map.get(lbl, 0)) for lbl in labels]
        se_col = [str(state_total) for _ in labels]

        headers = [
            "Signal",
            "N",
            "N PP_AR",
            "N KF_AR",
            "N M_ERR",
            "N S_ERR",
            "Mean",
            "Std",
            "RMS",
            "WMean",
            "WStd",
            "WRMS",
        ]

        def fmt(x):
            try:
                return f"{x:.3f}"
            except Exception:
                return ""

        cells = [
            labels,
            [str(int(n)) if pd.notna(n) else "0" for n in stats.get("N", pd.Series(dtype=float)).fillna(0)],
            pp_col,
            kf_col,
            me_col,
            se_col,
            [fmt(v) for v in stats.get("Mean", pd.Series(dtype=float))],
            [fmt(v) for v in stats.get("Std", pd.Series(dtype=float))],
            [fmt(v) for v in stats.get("RMS", pd.Series(dtype=float))],
            [fmt(v) for v in stats.get("WMean", pd.Series(dtype=float))],
            [fmt(v) for v in stats.get("WStd", pd.Series(dtype=float))],
            [fmt(v) for v in stats.get("WRMS", pd.Series(dtype=float))],
        ]

        fig.add_trace(
            go.Table(
                header=dict(values=headers, fill_color="#f2f2f2", align="left"),
                cells=dict(values=cells, align="left"),
            ),
            row=table_row,
            col=1,
        )

    # ---------- Axes / slider / zoom ----------
    if show_stats:
        # Row 1 (main): no rangeslider; keep quick range buttons here
        fig.update_xaxes(
            row=1,
            col=1,
            rangeslider=dict(visible=True),
            rangeselector=dict(
                y=0.98,
                yanchor="bottom",  # nudge below title if desired
                buttons=[
                    dict(count=1, label="1h", step="hour", stepmode="backward"),
                    dict(count=6, label="6h", step="hour", stepmode="backward"),
                    dict(count=12, label="12h", step="hour", stepmode="backward"),
                    dict(count=1, label="1d", step="day", stepmode="backward"),
                    dict(step="all", label="All"),
                ],
            ),
        )
        # Row 2: the slider lane (linked to row 1)
        fig.update_xaxes(
            row=2,
            col=1,
            showgrid=False,
            zeroline=False,
            showticklabels=False,
            rangeslider=dict(visible=True, thickness=0.20),
            matches="x1",
        )
        fig.update_yaxes(row=2, col=1, visible=False)
        # Hide axes UI on the table row
        fig.update_xaxes(visible=False, row=3, col=1)
        fig.update_yaxes(visible=False, row=3, col=1)
    else:
        # Single-pane version: keep the slider on the main axis
        fig.update_xaxes(
            rangeslider=dict(visible=True, thickness=0.10),
            rangeselector=dict(
                y=0.98,
                yanchor="bottom",
                buttons=[
                    dict(count=1, label="1h", step="hour", stepmode="backward"),
                    dict(count=6, label="6h", step="hour", stepmode="backward"),
                    dict(count=12, label="12h", step="hour", stepmode="backward"),
                    dict(count=1, label="1d", step="day", stepmode="backward"),
                    dict(step="all", label="All"),
                ],
            ),
        )

    # ---------- Final layout ----------
    fig.update_layout(dragmode="zoom")  # x+y box zoom
    hover_mode = "x unified" if hover_unified else "closest"
    axis_label = (
        "Normalised residual (res/σ)"
        if residual_field.startswith("norm_")
        else f"{residual_field.capitalize()} residual"
    )

    fig.update_layout(
        title=title,
        xaxis_title="Time",
        yaxis_title=axis_label,
        legend_title="Satellite",
        template="plotly_white",
        hovermode=hover_mode,
        legend=dict(groupclick="togglegroup"),
        margin=dict(l=60, r=28, t=28, b=28),
        height=fig_height,
    )
    # Ensure y is zoomable on the main plot and set explicit range to include markers
    if show_stats:
        fig.update_yaxes(fixedrange=False, row=1, col=1, range=[y_line_lo, y_line_hi], autorange=False)
    else:
        fig.update_yaxes(fixedrange=False, range=[y_line_lo, y_line_hi], autorange=False)

    return fig


def write_index_html(
    index_path: Path,
    base_title: str,
    meas_map: Dict[str, List[Tuple[str, str]]],
    meta: Dict[str, str],
    item_kind: str = "sat",
) -> None:
    """
    Write a lightweight index HTML with CODE and PHASE sections.
    Links are written as paths relative to the index file location.
    meas_map: {"code": [(item, filename), ...], "phase": [(item, filename), ...]}
    item_kind: "sat" or "recv"
    """
    out_dir = index_path.parent.resolve()

    # Sort entries nicely
    if item_kind == "sat":
        sort_fn = lambda tup: _sat_sort_key(tup[0])
        filter_placeholder = "Filter satellites (regex ok, e.g. G11, ^E, ^G(1[0-9])$)"
        item_title = "Satellite"
    else:
        sort_fn = lambda tup: (_recv_sort_key(tup[0]),)
        filter_placeholder = "Filter receivers (regex ok, e.g. ^TT, RCV1$)"
        item_title = "Receiver"

    meas_map = {k: sorted(v, key=sort_fn) for k, v in meas_map.items()}

    def html_escape(s: str) -> str:
        return (
            str(s)
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace('"', "&quot;")
            .replace("'", "&#39;")
        )

    meta_rows = "".join(f"<tr><th>{html_escape(k)}</th><td>{html_escape(v)}</td></tr>" for k, v in meta.items() if v)

    def _rel_href(target: str) -> str:
        """Return path to target relative to the index file directory."""
        try:
            rel = os.path.relpath(Path(target), start=out_dir)
        except Exception:
            # Fallback: just use the original string
            rel = str(target)
        return rel

    def section(meas_key: str, title: str) -> str:
        items = meas_map.get(meas_key, [])
        if not items:
            # Keep the header in DOM (hidden) so toggles still work consistently
            return f'<h2 class="section {meas_key}" style="display:none">{title}</h2>'
        lis = "\n".join(
            (
                f'<li class="item {meas_key}" data-key="{html_escape(item)}">'
                f'<a href="{html_escape(_rel_href(fname))}" target="_blank" rel="noopener noreferrer">'
                f"{html_escape(item)} — {title} plot</a>"
                f"</li>"
            )
            for item, fname in items
        )

        return f'<h2 class="section {meas_key}">{title}</h2><ul class="grid {meas_key}">\n{lis}\n</ul>'

    code_sec = section("code", "CODE")
    phase_sec = section("phase", "PHASE")
    now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    html = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>{html_escape(base_title)} — residual plots index ({item_title}s)</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
  :root {{ --bg:#fff; --fg:#111; --muted:#666; --accent:#0b5fff; --chip:#eef3ff; --border:#ddd; }}
  body {{ font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin:24px; color:var(--fg); background:var(--bg); }}
  header h1 {{ margin:0 0 4px 0; font-size:22px; }}
  header .meta {{ color:var(--muted); font-size:13px; }}
  .panel {{ margin:16px 0 24px; padding:12px; border:1px solid var(--border); border-radius:12px; }}
  .controls {{ display:flex; gap:12px; flex-wrap:wrap; align-items:center; }}
  .controls input[type="text"] {{ padding:8px 10px; border:1px solid var(--border); border-radius:10px; min-width:260px; }}
  .toggle {{ padding:6px 10px; border:1px solid var(--border); border-radius:999px; background:var(--chip); cursor:pointer; }}
  h2.section {{ margin:22px 0 8px; font-size:16px; color:var(--accent); }}
  ul.grid {{ list-style:none; padding:0; margin:0; display:grid; grid-template-columns:repeat(auto-fill, minmax(220px,1fr)); gap:8px; }}
  li.item a {{ display:block; padding:10px 12px; border:1px solid var(--border); border-radius:10px; text-decoration:none; color:inherit; }}
  li.item a:hover {{ border-color:var(--accent); box-shadow:0 0 0 3px #0b5fff22; }}
  table.meta {{ border-collapse:collapse; margin-top:6px; }}
  table.meta th, table.meta td {{ border:1px solid var(--border); padding:6px 8px; font-size:12px; }}
  table.meta th {{ text-align:left; background:#fafafa; }}
  footer {{ margin-top:28px; color:var(--muted); font-size:12px; }}
</style>
</head>
<body>
<header>
  <h1>{html_escape(base_title)} — residual plots index ({html_escape(item_title)}s)</h1>
  <div class="meta">Generated {html_escape(now)}</div>
  <div class="panel">
    <div><strong>Run info</strong></div>
    <table class="meta">{meta_rows}</table>
  </div>
</header>

<div class="panel controls">
  <label class="toggle"><input type="checkbox" id="codeToggle" checked> Show CODE</label>
  <label class="toggle"><input type="checkbox" id="phaseToggle" checked> Show PHASE</label>
  <input id="search" type="text" placeholder="{html_escape(filter_placeholder)}">
</div>

{code_sec}
{phase_sec}

<footer>Tip: The search box accepts regular expressions.</footer>

<script>
  const codeToggle = document.getElementById('codeToggle');
  const phaseToggle = document.getElementById('phaseToggle');
  const search = document.getElementById('search');

  function applyFilters() {{
    let re;
    try {{ re = new RegExp(search.value.trim() || '.*', 'i'); }}
    catch (e) {{ re = new RegExp('a^'); }}

    document.querySelectorAll('li.item').forEach(li => {{
      const key = li.dataset.key || '';
      const isCode = li.classList.contains('code');
      const isPhase = li.classList.contains('phase');
      const onMeas = (isCode && codeToggle.checked) || (isPhase && phaseToggle.checked);
      const onKey = re.test(key);
      li.style.display = (onMeas && onKey) ? '' : 'none';
    }});

    ['code','phase'].forEach(m => {{
      const secH = document.querySelector('h2.section.'+m);
      const items = Array.from(document.querySelectorAll('li.item.'+m));
      const hasVisible = items.some(li => li.style.display !== 'none');
      if (secH) secH.style.display = hasVisible ? '' : 'none';
    }});
  }}

  [codeToggle, phaseToggle, search].forEach(el => el.addEventListener('input', applyFilters));
  applyFilters();
</script>
</body>
</html>
"""
    ensure_parent(index_path)
    index_path.write_text(html, encoding="utf-8")


def build_recv_sat_stats(df: pd.DataFrame, yfield: str, weighted: bool) -> Dict[str, pd.DataFrame]:
    """
    Super-fast aggregator using NumPy bincount. Returns dict of DataFrames: 'mean','std','rms'.
    Includes margins: 'ALL_SAT' (left column), 'ALL_RECV' (top row), with grand cell at top-left.
    """
    if df is None or df.empty:
        return {"mean": pd.DataFrame(), "std": pd.DataFrame(), "rms": pd.DataFrame()}

    work = df[["recv", "sat", yfield, "sigma"]].copy()
    y = pd.to_numeric(work[yfield], errors="coerce").to_numpy()
    sig = pd.to_numeric(work["sigma"], errors="coerce").to_numpy()

    # masks: keep finite (and positive w when weighted)
    if weighted:
        w = 1.0 / (sig**2)
        m = np.isfinite(y) & np.isfinite(w) & (w > 0)
    else:
        w = None
        m = np.isfinite(y)
    if not m.any():
        return {"mean": pd.DataFrame(), "std": pd.DataFrame(), "rms": pd.DataFrame()}

    y = y[m]
    recv_vals = work.loc[m, "recv"].astype(str).to_numpy()
    sat_vals = work.loc[m, "sat"].astype(str).to_numpy()
    w = w[m] if weighted else None

    # Factorize to integer codes (preserves first-seen order)
    recv_codes, recv_uniques = pd.factorize(recv_vals, sort=False)
    sat_codes, sat_uniques = pd.factorize(sat_vals, sort=False)
    R = recv_uniques.size
    S = sat_uniques.size

    # Flat index r*S + s
    flat = recv_codes * S + sat_codes

    # Unweighted tallies
    if not weighted:
        cnt = np.bincount(flat, minlength=R * S).astype(float)
        sumy = np.bincount(flat, weights=y, minlength=R * S)
        sumy2 = np.bincount(flat, weights=y * y, minlength=R * S)

        cnt2D = cnt.reshape(R, S)
        sum2D = sumy.reshape(R, S)
        sum22D = sumy2.reshape(R, S)

        # Mean
        with np.errstate(invalid="ignore", divide="ignore"):
            mean = np.where(cnt2D > 0, sum2D / cnt2D, np.nan)
            # Sample variance (ddof=1) where cnt>1
            var = np.where(cnt2D > 1, (sum22D - (sum2D * sum2D) / cnt2D) / (cnt2D - 1.0), np.nan)
            std = np.sqrt(var)
            rms = np.where(cnt2D > 0, np.sqrt(sum22D / cnt2D), np.nan)

        # Margins (per-receiver = across columns, per-sat = across rows)
        cnt_r = cnt2D.sum(axis=1)
        sum_r = sum2D.sum(axis=1)
        sum2_r = sum22D.sum(axis=1)

        cnt_s = cnt2D.sum(axis=0)
        sum_s = sum2D.sum(axis=0)
        sum2_s = sum22D.sum(axis=0)

        # per-receiver margins
        mean_r = np.where(cnt_r > 0, sum_r / cnt_r, np.nan)
        var_r = np.where(cnt_r > 1, (sum2_r - (sum_r * sum_r) / cnt_r) / (cnt_r - 1.0), np.nan)
        std_r = np.sqrt(var_r)
        rms_r = np.where(cnt_r > 0, np.sqrt(sum2_r / cnt_r), np.nan)

        # per-sat margins
        mean_s = np.where(cnt_s > 0, sum_s / cnt_s, np.nan)
        var_s = np.where(cnt_s > 1, (sum2_s - (sum_s * sum_s) / cnt_s) / (cnt_s - 1.0), np.nan)
        std_s = np.sqrt(var_s)
        rms_s = np.where(cnt_s > 0, np.sqrt(sum2_s / cnt_s), np.nan)

        # grand
        cnt_all = cnt2D.sum()
        sum_all = sum2D.sum()
        sum2_all = sum22D.sum()
        mean_all = (sum_all / cnt_all) if cnt_all > 0 else np.nan
        var_all = ((sum2_all - (sum_all * sum_all) / cnt_all) / (cnt_all - 1.0)) if cnt_all > 1 else np.nan
        std_all = np.sqrt(var_all) if np.isfinite(var_all) else np.nan
        rms_all = np.sqrt(sum2_all / cnt_all) if cnt_all > 0 else np.nan

    else:
        # Weighted tallies
        wsum = np.bincount(flat, weights=w, minlength=R * S)
        wysum = np.bincount(flat, weights=w * y, minlength=R * S)
        wy2sum = np.bincount(flat, weights=w * y * y, minlength=R * S)

        wsum2D = wsum.reshape(R, S)
        wysum2D = wysum.reshape(R, S)
        wy22D = wy2sum.reshape(R, S)

        with np.errstate(invalid="ignore", divide="ignore"):
            mean = np.where(wsum2D > 0, wysum2D / wsum2D, np.nan)
            var = np.where(wsum2D > 0, (wy22D / wsum2D) - mean * mean, np.nan)  # your previous definition
            std = np.sqrt(var)
            rms = np.where(wsum2D > 0, np.sqrt(wy22D / wsum2D), np.nan)

        # Margins
        wsum_r = wsum2D.sum(axis=1)
        wysum_r = wysum2D.sum(axis=1)
        wy2_r = wy22D.sum(axis=1)

        wsum_s = wsum2D.sum(axis=0)
        wysum_s = wysum2D.sum(axis=0)
        wy2_s = wy22D.sum(axis=0)

        mean_r = np.where(wsum_r > 0, wysum_r / wsum_r, np.nan)
        var_r = np.where(wsum_r > 0, (wy2_r / wsum_r) - mean_r * mean_r, np.nan)
        std_r = np.sqrt(var_r)
        rms_r = np.where(wsum_r > 0, np.sqrt(wy2_r / wsum_r), np.nan)

        mean_s = np.where(wsum_s > 0, wysum_s / wsum_s, np.nan)
        var_s = np.where(wsum_s > 0, (wy2_s / wsum_s) - mean_s * mean_s, np.nan)
        std_s = np.sqrt(var_s)
        rms_s = np.where(wsum_s > 0, np.sqrt(wy2_s / wsum_s), np.nan)

        wsum_all = wsum2D.sum()
        wysum_all = wysum2D.sum()
        wy2_all = wy22D.sum()
        mean_all = (wysum_all / wsum_all) if wsum_all > 0 else np.nan
        var_all = ((wy2_all / wsum_all) - mean_all * mean_all) if wsum_all > 0 else np.nan
        std_all = np.sqrt(var_all) if np.isfinite(var_all) else np.nan
        rms_all = np.sqrt(wy2_all / wsum_all) if wsum_all > 0 else np.nan

    # Build matrices with margins at top/left
    def _mk_df(mcore: np.ndarray, row_margin: np.ndarray, col_margin: np.ndarray, grand: float) -> pd.DataFrame:
        # Assemble with ALL_RECV as top row, ALL_SAT as left col
        #   rows:  ALL_RECV + recv_uniques
        #   cols:  ALL_SAT  + sat_uniques (sorted with your GNSS sorter)
        sats_sorted = sorted(list(sat_uniques), key=_sat_sort_key)
        # map sat_uniques -> order indices
        sat_order = np.array([np.where(sat_uniques == s)[0][0] for s in sats_sorted], dtype=int)
        m_sorted = mcore[:, sat_order]

        # left column (per-recv margin)
        col_left = row_margin.reshape(-1, 1)
        body_with_left = np.concatenate([col_left, m_sorted], axis=1)

        # top row (per-sat margin)
        top_row = np.concatenate([[grand], col_margin[sat_order]], axis=0).reshape(1, -1)

        full = np.concatenate([top_row, body_with_left], axis=0)

        rows = ["ALL_RECV"] + [str(r) for r in recv_uniques.tolist()]
        cols = ["ALL_SAT"] + [str(s) for s in sats_sorted]
        return pd.DataFrame(full, index=rows, columns=cols)

    mean_df = _mk_df(mean, mean_r, mean_s, mean_all)
    std_df = _mk_df(std, std_r, std_s, std_all)
    rms_df = _mk_df(rms, rms_r, rms_s, rms_all)

    return {"mean": mean_df, "std": std_df, "rms": rms_df}


def _counts_to_customdata(counts_sorted: Optional[pd.DataFrame], z_shape: tuple):
    """Convert a counts DataFrame into numeric customdata with shape (rows, cols, 1)."""
    if counts_sorted is None or counts_sorted.empty:
        return None
    try:
        cd = counts_sorted.to_numpy(dtype=float)  # (rows, cols)
        cd = np.nan_to_num(cd, nan=0.0, posinf=0.0, neginf=0.0)
        if cd.shape != z_shape:
            logger.warning("Counts shape %s != Z shape %s; omitting customdata.", cd.shape, z_shape)
            return None
        return cd[..., np.newaxis]  # -> (rows, cols, 1)
    except Exception as e:
        logger.warning("Failed to build numeric customdata: %s", e)
        return None


def write_heatmap_html(
    df_mat: pd.DataFrame,
    title: str,
    out_html: str,
    colorscale: str = "RdBu",
    reversescale: bool = True,
    zmid: Optional[float] = 0.0,
    zmin: Optional[float] = None,
    zmax: Optional[float] = None,
    # Annotation knobs:
    annotate: str = "none",  # "none" | "auto" | "all" | "margins"  (bool True->"all", False->"none")
    precision: int = 3,
    max_annot_cells: int = 2500,
    # Hover counts matrix (aligned to df_mat's index/columns)
    counts: Optional[pd.DataFrame] = None,
    # sorting controls
    row_sort: str = "alpha",  # "alpha" | "none"
    sat_sort: str = "alpha",  # "alpha" | "natural" | "none"
    # html writing
    include_plotlyjs: str = "inline",  # "inline" (default) or "cdn"
) -> str:
    """
    Render a heatmap with optional annotations, sorted rows/columns, and robust hover counts.

    Sorting:
      - Rows: 'ALL_RECV' is kept at the top if present; remaining rows sorted by `row_sort`.
      - Columns: 'ALL_SAT' is kept at the left if present; remaining columns sorted by `sat_sort`.
      - `sat_sort='natural'` uses `_sat_sort_key('G12') -> ('G', 12)` if available.

    Notes:
      - `counts` (receiver×satellite sample counts) are shown in hover via `customdata[0]`.
      - HTML is written with Plotly JS **inline** by default to avoid CDN caching issues.
    """
    fig = go.Figure()
    if df_mat is None or df_mat.empty:
        fig.update_layout(title=title, template="plotly_white")
        ensure_parent(out_html)
        fig.write_html(out_html, include_plotlyjs=include_plotlyjs, validate=False, config={"scrollZoom": True})
        return out_html

    # --- Build target row/col orders (preserve margins, sort the rest) ---
    rows = list(df_mat.index)
    cols = list(df_mat.columns)

    # Rows: keep ALL_RECV first if present
    if rows and rows[0] == "ALL_RECV":
        row_header, row_body = ["ALL_RECV"], rows[1:]
    else:
        row_header = [r for r in rows if r == "ALL_RECV"]
        row_body = [r for r in rows if r != "ALL_RECV"]

    if row_sort == "alpha":
        row_body_sorted = sorted(row_body, key=lambda s: str(s).upper())
    else:
        row_body_sorted = row_body
    rows_order = row_header + row_body_sorted

    # Columns: keep ALL_SAT first if present
    if cols and cols[0] == "ALL_SAT":
        col_header, col_body = ["ALL_SAT"], cols[1:]
    else:
        col_header = [c for c in cols if c == "ALL_SAT"]
        col_body = [c for c in cols if c != "ALL_SAT"]

    if sat_sort == "alpha":
        col_body_sorted = sorted(col_body, key=lambda s: str(s).upper())
    elif sat_sort == "natural" and "_sat_sort_key" in globals():
        col_body_sorted = sorted(col_body, key=_sat_sort_key)
    else:
        col_body_sorted = col_body
    cols_order = col_header + col_body_sorted

    # --- Reindex matrices to this order (keeps alignment with hover counts) ---
    mat_sorted = df_mat.reindex(index=rows_order, columns=cols_order)
    counts_sorted = (
        counts.reindex(index=rows_order, columns=cols_order) if (counts is not None and not counts.empty) else None
    )

    # --- Extract arrays ---
    Z = mat_sorted.values
    X = list(mat_sorted.columns)
    Y = list(mat_sorted.index)

    # --- Decide annotation mode ---
    if isinstance(annotate, bool):
        annotate = "all" if annotate else "none"

    if annotate == "auto":
        do_text, margins_only = (Z.size <= max_annot_cells), False
    elif annotate == "all":
        do_text, margins_only = True, False
    elif annotate == "margins":
        do_text, margins_only = True, True
    else:
        do_text, margins_only = False, False

    # --- Build text labels ONLY if annotating ---
    text = None
    if do_text:
        text = np.full(Z.shape, "", dtype=object)
        if margins_only:
            if Z.shape[0] > 0:
                top = Z[0, :]
                text[0, :] = np.where(np.isfinite(top), np.round(top, precision).astype(str), "")
            if Z.shape[1] > 0:
                left = Z[:, 0]
                text[1:, 0] = np.where(np.isfinite(left[1:]), np.round(left[1:], precision).astype(str), "")
        else:
            text = np.where(np.isfinite(Z), np.round(Z, precision).astype(str), "")

    # --- Prepare customdata and hovertemplate ---
    customdata = _counts_to_customdata(counts_sorted, Z.shape)
    if customdata is not None:
        hovertemplate = (
            "Receiver=%{y}<br>Satellite=%{x}"
            f"<br>Value=%{{z:.{precision}f}}"
            "<br>Count=%{customdata[0]:.0f}"
            "<extra></extra>"
        )
    else:
        hovertemplate = "Receiver=%{y}<br>Satellite=%{x}" f"<br>Value=%{{z:.{precision}f}}" "<extra></extra>"

    # --- Heatmap trace ---
    hm_kwargs = dict(
        z=Z,
        x=X,
        y=Y,
        colorscale=colorscale,
        reversescale=reversescale,
        hoverongaps=False,
        hovertemplate=hovertemplate,
    )
    if customdata is not None:
        hm_kwargs["customdata"] = customdata
    if zmid is not None:
        hm_kwargs["zmid"] = zmid
    if zmin is not None:
        hm_kwargs["zmin"] = zmin
    if zmax is not None:
        hm_kwargs["zmax"] = zmax
    if do_text:
        hm_kwargs["text"] = text
        hm_kwargs["texttemplate"] = "%{text}"
        hm_kwargs["textfont"] = {"size": 11}

    fig.add_trace(go.Heatmap(**hm_kwargs))

    # Axes
    fig.update_yaxes(
        autorange="reversed",
        categoryorder="array",
        categoryarray=Y,
        side="top",
        mirror=True,
        ticks="outside",
        showline=True,
    )
    fig.update_xaxes(
        side="top",
        mirror=True,
        ticks="outside",
        showline=True,
    )

    fig.update_layout(
        title=title,
        xaxis_title="Satellite",
        yaxis_title="Receiver",
        template="plotly_white",
        uniformtext_minsize=8,
        uniformtext_mode="hide",
        margin=dict(l=20, r=20, t=20, b=20),
    )

    # Interactions
    fig.update_layout(dragmode="zoom")
    fig.update_xaxes(fixedrange=False)
    fig.update_yaxes(fixedrange=False)

    # Write HTML (inline Plotly by default)
    ensure_parent(out_html)
    fig.write_html(out_html, include_plotlyjs=include_plotlyjs, validate=False, config={"scrollZoom": True})
    logger.info("Wrote: %s", out_html)
    return out_html


def prepare_ambiguity_reasons(df_amb: pd.DataFrame) -> pd.DataFrame:
    """
    Process ambiguity reset data for counting by reason.

    - Explodes comma-separated reasons into individual rows
    - Deduplicates by (datetime, sat, reason) to count each reason once per satellite per epoch

    Returns:
        DataFrame with one row per unique (datetime, sat, reason) combination
    """
    if df_amb is None or df_amb.empty:
        return pd.DataFrame()

    df = df_amb.copy()
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce")

    # Explode comma-separated reasons into individual rows
    df = df.assign(reason=df["reasons"].fillna("").str.split(","))
    df = df.explode("reason")
    df["reason"] = df["reason"].str.strip()
    df = df[df["reason"] != ""]

    if df.empty:
        return df

    # Deduplicate: count each unique (datetime, sat, recv, reason) only once
    # (same reason on multiple signals for same sat at same time at same receiver = count once)
    df = df.drop_duplicates(subset=["datetime", "sat", "recv", "reason"])

    return df


def prepare_ambiguity_events(df_amb: pd.DataFrame) -> pd.DataFrame:
    """
    Process ambiguity reset data for counting satellite reset events.

    - Deduplicates by (datetime, sat, action) to count each reset event once
    - Each satellite reset = one event, regardless of how many signals or reasons

    Returns:
        DataFrame with one row per unique (datetime, sat, action) combination
    """
    if df_amb is None or df_amb.empty:
        return pd.DataFrame()

    df = df_amb.copy()
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce")
    df = df.dropna(subset=["datetime"])

    if df.empty:
        return df

    # Deduplicate: count each satellite reset once per epoch per receiver
    # (same sat at same time with multiple signals/reasons at same receiver = one reset event)
    df = df.drop_duplicates(subset=["datetime", "sat", "recv", "action"])

    return df


def plot_ambiguity_reason_counts(
    df_amb: pd.DataFrame,
    split: str = "combined",
    *,
    base: str,
    variant_suffix: str,
) -> List[str]:
    """
    Generate cumulative ambiguity-reset reason count plots (diagnostic view).
    Shows how often each detection method (GF, MW, LLI, SCDIA) triggers.
    Also shows unique satellite resets for comparison.
    """
    outputs = []
    if df_amb is None or df_amb.empty:
        logger.warning("No ambiguity-reset data to plot.")
        return outputs

    # Process and deduplicate ambiguity reasons
    df_reasons = prepare_ambiguity_reasons(df_amb)
    if df_reasons.empty:
        logger.warning("No ambiguity-reset reasons found after processing.")
        return outputs

    df_reasons = df_reasons.sort_values("datetime")

    # Also prepare unique satellite reset events
    df_events = prepare_ambiguity_events(df_amb)
    df_events = df_events.sort_values("datetime") if not df_events.empty else df_events

    # Choose grouping key
    if split == "recv":
        groups = df_reasons.groupby("recv", dropna=False)
        event_groups = df_events.groupby("recv", dropna=False) if not df_events.empty else []
    elif split == "sat":
        groups = df_reasons.groupby("sat", dropna=False)
        event_groups = df_events.groupby("sat", dropna=False) if not df_events.empty else []
    else:
        groups = [("ALL", df_reasons)]
        event_groups = [("ALL", df_events)] if not df_events.empty else []

    # Convert event_groups to dict for easy lookup
    event_dict = {k: v for k, v in event_groups}

    for key, g in groups:
        if g.empty:
            continue

        counts = g.groupby(["datetime", "reason"], sort=True).size().reset_index(name="count").sort_values("datetime")
        counts["cumcount"] = counts.groupby("reason")["count"].cumsum()
        pivot = counts.pivot(index="datetime", columns="reason", values="cumcount").ffill().fillna(0)

        fig = go.Figure()
        for reason in pivot.columns:
            fig.add_trace(go.Scatter(x=pivot.index, y=pivot[reason], mode="lines", name=reason))

        # Add unique resets trace if available
        if key in event_dict and not event_dict[key].empty:
            g_events = event_dict[key]
            event_counts = (
                g_events.groupby("datetime", sort=True).size().reset_index(name="count").sort_values("datetime")
            )
            event_counts["cumcount"] = event_counts["count"].cumsum()

            fig.add_trace(
                go.Scatter(
                    x=event_counts["datetime"],
                    y=event_counts["cumcount"],
                    mode="lines",
                    name="Unique Resets",
                    line=dict(dash="dash", width=2.5),
                )
            )

        title_suffix = {
            "recv": f"Receiver {key}",
            "sat": f"Satellite {key}",
            "combined": "All receivers/satellites",
        }.get(split, "All")

        fig.update_layout(
            title=f"Cumulative Ambiguity Resets — {title_suffix}<br><sub>Each reason counted once per epoch-satellite across all signals. 'Unique Resets' shows total satellites affected.</sub>",
            xaxis_title="Time",
            yaxis_title="Cumulative Count",
            template="plotly_white",
            legend_title="Metric",
            hovermode="x unified",
        )

        safe_key = _sanitize_filename_piece(str(key))
        out_html = build_out_path(base, variant_suffix, "ambiguity_counts", split=split, key=safe_key)
        ensure_parent(out_html)
        fig.write_html(out_html, include_plotlyjs="cdn", validate=False, config={"scrollZoom": True})
        logger.info(f"Wrote: {out_html}")
        outputs.append(out_html)

    return outputs


def plot_ambiguity_reason_totals(
    df_amb: pd.DataFrame,
    split: str = "combined",
    orientation: str = "h",
    top_n: int = None,
    *,
    base: str,
    variant_suffix: str,
) -> List[str]:
    """
    Write a stacked bar chart of total ambiguity reset reasons (diagnostic view).
    Shows total counts of each detection method (GF, MW, LLI, SCDIA) per receiver/satellite.
    """
    outputs = []
    if df_amb is None or df_amb.empty:
        logger.warning("No ambiguity-reset data to plot.")
        return outputs

    # Process and deduplicate ambiguity reasons
    df = prepare_ambiguity_reasons(df_amb)
    if df.empty:
        logger.info("No ambiguity-reset reasons found after processing.")
        return outputs

    key_name = {"recv": "recv", "sat": "sat"}.get(split, None)

    if key_name is None:
        gdf = df.groupby("reason", sort=True).size().rename("count").reset_index()
        gdf["group"] = "ALL"  # AJC - Add group column for pivot
        pivot = gdf.pivot_table(index="group", columns="reason", values="count", fill_value=0)
    else:
        pivot = (
            df.groupby([key_name, "reason"], sort=True)
            .size()
            .rename("count")
            .reset_index()
            .pivot(index=key_name, columns="reason", values="count")
            .fillna(0)
        )

    reason_totals = pivot.sum(axis=0).sort_values(ascending=False)
    pivot = pivot.reindex(columns=reason_totals.index)

    # Compute unique reset counts per group (sat/recv/combined)
    df_events = prepare_ambiguity_events(df_amb)
    if not df_events.empty and key_name is not None:
        unique_resets = df_events.groupby(key_name).size().reindex(pivot.index, fill_value=0)
    elif not df_events.empty:
        # Combined case: count all unique resets
        unique_resets = pd.Series([len(df_events)], index=pivot.index)
    else:
        unique_resets = pd.Series([0] * len(pivot), index=pivot.index)

    if top_n is not None and top_n > 0 and pivot.shape[0] > top_n:
        group_totals = pivot.sum(axis=1).sort_values(ascending=False)
        keep = group_totals.index[:top_n]
        pivot = pivot.loc[keep]
        unique_resets = unique_resets.loc[keep]

    fig = go.Figure()
    groups = pivot.index.astype(str).tolist()
    is_h = orientation == "h"
    x_title = "Total resets" if is_h else (key_name or "All")
    y_title = (key_name or "All") if is_h else "Total resets"

    for reason in pivot.columns:
        vals = pivot[reason].to_numpy()
        if is_h:
            fig.add_trace(go.Bar(y=groups, x=vals, orientation="h", name=reason))
        else:
            fig.add_trace(go.Bar(x=groups, y=vals, name=reason))

    # Add unique reset markers (star) on each bar at their actual value
    unique_reset_vals = unique_resets.values

    if is_h:
        # Horizontal bars: place star markers at unique reset count position
        fig.add_trace(
            go.Scatter(
                x=unique_reset_vals,
                y=groups,
                mode="markers",
                marker=dict(symbol="star", size=12, color="gold", line=dict(width=0)),
                showlegend=True,
                name="Unique Resets",
                hovertemplate="Unique Resets: %{x}<extra></extra>",
            )
        )
    else:
        # Vertical bars: place star markers at unique reset count position
        fig.add_trace(
            go.Scatter(
                x=groups,
                y=unique_reset_vals,
                mode="markers",
                marker=dict(symbol="star", size=12, color="gold", line=dict(width=0)),
                showlegend=True,
                name="Unique Resets",
                hovertemplate="Unique Resets: %{y}<extra></extra>",
            )
        )

    split_title = {"recv": "by Receiver", "sat": "by Satellite", "combined": "All"}[split]
    fig.update_layout(
        title=f"Total Ambiguity Resets {split_title} (stacked by reason)<br><sub>Each reason counted once per epoch-satellite across all signals. Any signal triggering a reason resets all ambiguities (ionosphere no longer constrained). ⭐ = unique satellite resets.</sub>",
        barmode="stack",
        template="plotly_white",
        legend_title="Reason",
        xaxis_title=x_title,
        yaxis_title=y_title,
        hovermode="closest",
        margin=dict(l=80, r=20, t=60, b=60),
    )

    if is_h and pivot.shape[0] > 1:
        fig.update_yaxes(categoryorder="array", categoryarray=groups[::-1])

    out_html = build_out_path(base, variant_suffix, "ambiguity_totals", split=split, tag=orientation)
    ensure_parent(out_html)
    fig.write_html(out_html, include_plotlyjs="cdn", validate=False, config={"scrollZoom": True})
    logger.info(f"Wrote: {out_html}")
    outputs.append(out_html)
    return outputs


def add_ambiguity_markers_combined(
    fig: go.Figure,
    df_amb: pd.DataFrame,
    *,
    y_anchor: Optional[float] = None,
    y_span: Optional[Tuple[float, float]] = None,  # (y_lo, y_hi) for the v-line span
    line_width: float = 0.5,
    marker_size: int = 10,
    line_color_preproc: str = "rgba(0,128,0,0.85)",  # green
    line_color_reject: str = "rgba(65,105,225,0.85)",  # royal blue
    split_by_reason: bool = True,
    split_context: Optional[str] = None,  # "recv" or "sat" to indicate grouping
    add_trace_fn=None,  # Function to add traces (handles subplot layout)
) -> None:
    """
    Add ambiguity-reset overlays (PREPROC/REJECT) that toggle with each satellite's legend item.
    """
    if add_trace_fn is None:
        add_trace_fn = fig.add_trace
    if df_amb is None or df_amb.empty:
        return

    amb = df_amb.copy()
    amb["datetime"] = pd.to_datetime(amb["datetime"], errors="coerce")
    amb = amb.dropna(subset=["datetime"])
    if "reasons" not in amb.columns:
        amb["reasons"] = ""

    if amb.empty:
        return

    # If no explicit y_anchor given, place markers near the top of the line span
    if y_anchor is None and y_span and isinstance(y_span, tuple) and len(y_span) == 2:
        y_anchor = y_span[1]

    def _add_sat_vline(ts, y_lo, y_hi, sat, color="rgba(120,120,120,0.55)"):
        add_trace_fn(
            go.Scatter(
                x=[ts, ts],
                y=[y_lo, y_hi],
                mode="lines",
                line=dict(color=color, width=line_width, dash="dash"),
                hoverinfo="skip",
                showlegend=False,
                legendgroup=str(sat),
                name="",
            )
        )

    for ts, g_ts in amb.groupby("datetime", sort=True):
        y_lo, y_hi = (None, None)
        if y_span and isinstance(y_span, tuple) and len(y_span) == 2:
            y_lo, y_hi = y_span

        # For recv context, aggregate all satellites at this timestamp into one marker
        # For sat context, create separate markers per satellite
        # For combined/default, aggregate all at timestamp
        if split_context == "recv" or split_context is None:
            # Add vertical lines for each satellite at this timestamp
            for sat in g_ts["sat"].unique() if "sat" in g_ts.columns else []:
                _add_sat_vline(ts, y_lo, y_hi, sat)

            # Create markers aggregated across all satellites
            if split_by_reason:
                if "action" in g_ts.columns:
                    action_key = g_ts["action"].where(g_ts["action"].notna(), "").astype(str).str.upper()
                else:
                    action_key = pd.Series([""], index=g_ts.index)
                for action_val, g_act in g_ts.groupby(action_key, sort=False):
                    mcolor = line_color_preproc if action_val == "PREPROC" else line_color_reject

                    # Build hierarchical hover text
                    if split_context == "recv":
                        # Group by Sat, then show Sig | Reasons for each
                        lines = []
                        for sat_key, sat_grp in g_act.groupby("sat", sort=False):
                            lines.append(f"<b>Sat= {sat_key}</b>")
                            for _, row in sat_grp.iterrows():
                                lines.append(f"Sig= {row.get('sig','')} | Reasons= {row.get('reasons','(none)')}")
                        rows_html = lines
                        hover_recv = g_act.iloc[0].get("recv", "")
                        hover = (
                            "<b>Phase Ambiguity Reset</b><br>"
                            f"Time: {ts}<br>"
                            f"Action: {action_val}<br>"
                            f"Recv= {hover_recv}<br>" + "<br>".join(rows_html)
                        )
                    else:
                        # Combined/default: show both Sat and Recv for each signal
                        rows_html = [
                            f"Sat={r.get('sat','')} | Recv={r.get('recv','')} | Sig={r.get('sig','')} | Reasons={r.get('reasons','(none)')}"
                            for _, r in g_act.iterrows()
                        ]
                        hover = (
                            "<b>Phase Ambiguity Reset</b><br>"
                            f"Time: {ts}<br>"
                            f"Action: {action_val}<br>" + "<br>".join(rows_html)
                        )
                    # Use first satellite for legendgroup (allows toggling visibility)
                    first_sat = g_act.iloc[0].get("sat", "") if not g_act.empty else ""
                    add_trace_fn(
                        go.Scatter(
                            x=[ts],
                            y=[y_anchor],
                            mode="markers",
                            marker=dict(color=mcolor, size=marker_size, symbol="triangle-down"),
                            hovertemplate=hover + "<extra></extra>",
                            showlegend=False,
                            legendgroup=str(first_sat),
                            name="",
                        )
                    )
            else:
                # One combined marker at this timestamp (mixed actions possible)
                actions = set(g_ts["action"].str.upper() if "action" in g_ts.columns else [""])

                # Build hierarchical hover text
                if split_context == "recv":
                    # Group by Action, then Sat, then show Sig | Reasons
                    lines = []
                    for act_key, act_grp in g_ts.groupby(
                        g_ts["action"].str.upper() if "action" in g_ts.columns else "", sort=False
                    ):
                        if act_key:
                            lines.append(f"<b>Action: {act_key}</b>")
                        for sat_key, sat_grp in act_grp.groupby("sat", sort=False):
                            lines.append(f"<b>Sat= {sat_key}</b>")
                            for _, row in sat_grp.iterrows():
                                lines.append(f"Sig= {row.get('sig','')} | Reasons= {row.get('reasons','(none)')}")
                    hover_recv = g_ts.iloc[0].get("recv", "") if not g_ts.empty else ""
                    hover = f"<b>Phase Ambiguity Reset</b><br>Time: {ts}<br>Recv= {hover_recv}<br>" + "<br>".join(lines)
                else:
                    # Combined/default
                    lines = []
                    for _, r in g_ts.iterrows():
                        act = str(r.get("action", "")).upper()
                        lines.append(
                            f"{act}: Sat={r.get('sat','')} | Recv={r.get('recv','')} | Sig={r.get('sig','')} | Reasons={r.get('reasons','(none)')}"
                        )
                    hover = f"<b>Phase Ambiguity Reset</b><br>Time: {ts}<br>" + "<br>".join(lines)

                if actions == {"PREPROC"}:
                    mcolor = line_color_preproc
                elif actions == {"REJECT"}:
                    mcolor = line_color_reject
                else:
                    mcolor = "rgba(90,90,90,0.8)"  # mixed/unknown
                first_sat = g_ts.iloc[0].get("sat", "") if not g_ts.empty else ""
                add_trace_fn(
                    go.Scatter(
                        x=[ts],
                        y=[y_anchor],
                        mode="markers",
                        marker=dict(color=mcolor, size=marker_size, symbol="triangle-down"),
                        hovertemplate=hover + "<extra></extra>",
                        showlegend=False,
                        legendgroup=str(first_sat),
                        name="",
                    )
                )
        else:
            # split_context == "sat": Aggregate all receivers at this timestamp for the satellite
            # Add vertical lines for the satellite(s) at this timestamp
            for sat in g_ts["sat"].unique() if "sat" in g_ts.columns else []:
                _add_sat_vline(ts, y_lo, y_hi, sat)

            # Create markers aggregated across all receivers for this satellite
            if split_by_reason:
                if "action" in g_ts.columns:
                    action_key = g_ts["action"].where(g_ts["action"].notna(), "").astype(str).str.upper()
                else:
                    action_key = pd.Series([""], index=g_ts.index)
                for action_val, g_act in g_ts.groupby(action_key, sort=False):
                    mcolor = line_color_preproc if action_val == "PREPROC" else line_color_reject

                    # Group by Recv, then show Sig | Reasons for each
                    lines = []
                    for recv_key, recv_grp in g_act.groupby("recv", sort=False):
                        lines.append(f"<b>Recv= {recv_key}</b>")
                        for _, row in recv_grp.iterrows():
                            lines.append(f"Sig= {row.get('sig','')} | Reasons= {row.get('reasons','(none)')}")
                    rows_html = lines
                    hover_sat = g_act.iloc[0].get("sat", "") if not g_act.empty else ""
                    hover = (
                        "<b>Phase Ambiguity Reset</b><br>"
                        f"Time: {ts}<br>"
                        f"Action: {action_val}<br>"
                        f"Sat= {hover_sat}<br>" + "<br>".join(rows_html)
                    )
                    # Use first satellite for legendgroup
                    first_sat = g_act.iloc[0].get("sat", "") if not g_act.empty else ""
                    add_trace_fn(
                        go.Scatter(
                            x=[ts],
                            y=[y_anchor],
                            mode="markers",
                            marker=dict(color=mcolor, size=marker_size, symbol="triangle-down"),
                            hovertemplate=hover + "<extra></extra>",
                            showlegend=False,
                            legendgroup=str(first_sat),
                            name="",
                        )
                    )
            else:
                # One combined marker at this timestamp (mixed actions possible)
                actions = set(g_ts["action"].str.upper() if "action" in g_ts.columns else [""])

                # Group by Action, then Recv, then show Sig | Reasons
                lines = []
                for act_key, act_grp in g_ts.groupby(
                    g_ts["action"].str.upper() if "action" in g_ts.columns else "", sort=False
                ):
                    if act_key:
                        lines.append(f"<b>Action: {act_key}</b>")
                    for recv_key, recv_grp in act_grp.groupby("recv", sort=False):
                        lines.append(f"<b>Recv= {recv_key}</b>")
                        for _, row in recv_grp.iterrows():
                            lines.append(f"Sig= {row.get('sig','')} | Reasons= {row.get('reasons','(none)')}")
                hover_sat = g_ts.iloc[0].get("sat", "") if not g_ts.empty else ""
                hover = f"<b>Phase Ambiguity Reset</b><br>Time: {ts}<br>Sat= {hover_sat}<br>" + "<br>".join(lines)

                if actions == {"PREPROC"}:
                    mcolor = line_color_preproc
                elif actions == {"REJECT"}:
                    mcolor = line_color_reject
                else:
                    mcolor = "rgba(90,90,90,0.8)"  # mixed/unknown
                first_sat = g_ts.iloc[0].get("sat", "") if not g_ts.empty else ""
                add_trace_fn(
                    go.Scatter(
                        x=[ts],
                        y=[y_anchor],
                        mode="markers",
                        marker=dict(color=mcolor, size=marker_size, symbol="triangle-down"),
                        hovertemplate=hover + "<extra></extra>",
                        showlegend=False,
                        legendgroup=str(first_sat),
                        name="",
                    )
                )


# -------- CLI / Main --------


def pair_forward_smoothed_files(in_paths: List[Path], use_forward_residuals: bool):
    """
    Separate and pair forward and smoothed TRACE files.

    Returns:
        residual_paths: List[Path] - files to use for residuals
        forward_paths: List[Path] - files to use for ambiguity resets and large errors
        warnings: List[str] - warning messages about missing files
    """
    forward_files = []
    smoothed_files = []

    # Separate files into forward and smoothed
    for p in in_paths:
        if "_smoothed" in p.stem:
            smoothed_files.append(p)
        else:
            forward_files.append(p)

    warnings = []

    # Build pairing maps: base_name -> (forward_path, smoothed_path)
    pairs = {}

    # Add forward files to pairs
    for fwd in forward_files:
        base = fwd.stem
        if base not in pairs:
            pairs[base] = {"forward": fwd, "smoothed": None}

    # Add smoothed files to pairs
    for smo in smoothed_files:
        # Remove "_smoothed" suffix to get base name
        base = smo.stem.replace("_smoothed", "")
        if base in pairs:
            pairs[base]["smoothed"] = smo
        else:
            # Smoothed file without matching forward
            pairs[base] = {"forward": None, "smoothed": smo}
            warnings.append(
                f"Found smoothed file but no matching forward file: {smo.name}\n"
                f"       Ambiguity reset and large error data will not be available for this file"
            )

    # Determine which files to use for what
    if use_forward_residuals:
        # Use forward for everything
        residual_paths = forward_files.copy()
        forward_paths = forward_files.copy()
        if smoothed_files:
            logger.info("Using residuals from FORWARD files (--use-forward-residuals specified)")
    else:
        # Default: use smoothed for residuals if available, otherwise forward
        residual_paths = []
        forward_paths = []

        has_smoothed = False
        has_forward = False

        for base, files in pairs.items():
            fwd = files["forward"]
            smo = files["smoothed"]

            if smo is not None:
                residual_paths.append(smo)
                has_smoothed = True
            elif fwd is not None:
                residual_paths.append(fwd)
                warnings.append(
                    f"No smoothed file found for: {fwd.name}\n"
                    f"       Using forward file for residuals. For more accurate residuals, include *_smoothed.TRACE files"
                )

            if fwd is not None:
                forward_paths.append(fwd)
                has_forward = True

        if has_smoothed:
            logger.info("Using residuals from SMOOTHED files (more accurate)")
        if has_forward:
            logger.info("Using ambiguity resets and large errors from FORWARD files")

        if not has_smoothed and forward_files:
            warnings.append(
                "No smoothed files found - using residuals from forward files\n"
                "       For more accurate residuals, include *_smoothed.TRACE files in your pattern"
            )

    # Log file pairing summary
    if forward_files and smoothed_files:
        logger.info(f"Found {len(forward_files)} forward file(s) and {len(smoothed_files)} smoothed file(s)")
        for base, files in pairs.items():
            fwd = files["forward"]
            smo = files["smoothed"]
            if fwd and smo:
                logger.debug(f"Paired: {fwd.name} (forward) with {smo.name} (smoothed)")

    return residual_paths, forward_paths, warnings


def main():
    """CLI entry point for plot_trace_res - parses arguments and calls _process_and_plot_trace()."""
    p = argparse.ArgumentParser(description="Extract and plot GNSS residuals with optional large-error markers.")
    p.add_argument(
        "--files",
        required=True,
        nargs="+",
        help="One or more TRACE files (space and/or comma separated), e.g. 'A.trace B.trace,C.trace' (wildcards allowed eg. *.TRACE)",
    )
    p.add_argument("--residual", choices=["prefit", "postfit"], default="postfit")
    p.add_argument("--receivers", help="One or more receiver names (comma or space separated), e.g. 'ABMF,CHUR ALGO' ")
    p.add_argument("--sat", "-s", action="append", help="Filter by satellite ID")
    p.add_argument("--label-regex", help="Regex to filter labels")
    p.add_argument("--max-abs", type=float, default=None)
    p.add_argument("--start", help="Start datetime or time-only")
    p.add_argument("--end", help="End datetime (exclusive)")
    p.add_argument("--decimate", type=int, default=1)
    split_group = p.add_mutually_exclusive_group()
    split_group.add_argument("--split-per-sat", action="store_true")
    split_group.add_argument("--split-per-recv", action="store_true")
    p.add_argument("--out-dir", help="Output directory for HTML files; defaults to CWD.")
    p.add_argument("--basename", help="Base filename prefix for outputs (no extension).")
    p.add_argument("--webgl", action="store_true")
    p.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Logging verbosity.",
    )
    p.add_argument("--out-prefix", default=None)
    p.add_argument("--mark-large-errors", action="store_true", help="Mark LARGE STATE/MEAS ERROR events on plots.")
    p.add_argument(
        "--hover-unified",
        action="store_true",
        help="Use unified hover tooltips across all traces (default: closest point hover).",
    )
    p.add_argument(
        "--plot-normalised-res",
        action="store_true",
        help="Also generate plots of normalised residuals (residual / sigma).",
    )
    p.add_argument("--plot-normalized-res", action="store_true", help=argparse.SUPPRESS)
    p.add_argument(
        "--show-stats-table",
        action="store_true",
        help="Add a Mean / Std / RMS table per (sat × signal) at the bottom of each plot.",
    )
    p.add_argument(
        "--stats-matrix",
        action="store_true",
        help="Generate receiver×satellite heatmaps (Mean/Std/RMS) aggregated across signals.",
    )
    p.add_argument(
        "--stats-matrix-weighted",
        action="store_true",
        help="Use sigma-weighted statistics in the heatmaps (weights 1/σ²).",
    )
    p.add_argument(
        "--annotate-stats-matrix",
        action="store_true",
        help="Write the numeric value (mean/std/rms) into each stats heatmap cell. Hover still shows full details.",
    )
    p.add_argument(
        "--mark-amb-resets",
        action="store_true",
        help="Overlay PHASE ambiguity reset events (PREPROC=green, REJECT=blue) on PHASE per-receiver plots.",
    )

    # Ambiguity reset plots (includes both reasons and unique satellite resets)
    p.add_argument(
        "--ambiguity-counts",
        action="store_true",
        help="Plot cumulative counts of ambiguity reset reasons and unique satellite resets over time.",
    )
    p.add_argument(
        "--ambiguity-totals",
        action="store_true",
        help="Bar chart of total ambiguity reset reasons (diagnostic view of detection methods).",
    )

    p.add_argument(
        "--amb-totals-orient",
        choices=["h", "v"],
        default="h",
        help="Orientation for totals bar charts: 'h' (horizontal, default) or 'v' (vertical).",
    )
    p.add_argument(
        "--amb-totals-topn",
        type=int,
        default=None,
        help="Show only the top-N receivers/satellites by total resets (to avoid clutter).",
    )
    p.add_argument(
        "--use-forward-residuals",
        action="store_true",
        help="Use residuals from forward (non-smoothed) files instead of smoothed files (default: use smoothed for more accurate residuals).",
    )

    args = p.parse_args()

    # Call the shared processing function (CLI mode splits on comma / space)
    _process_and_plot_trace(args, from_cli=True)


def plot_trace_res_files(
    files: List[str],
    out_dir: str,
    mark_amb_resets: bool = True,
    mark_large_errors: bool = True,
    show_stats_table: bool = True,
    ambiguity_counts: bool = True,
    ambiguity_totals: bool = True,
    amb_totals_orient: str = "h",
    residual: str = "postfit",
    receivers: Optional[str] = None,
    sat: Optional[List[str]] = None,
    label_regex: Optional[str] = None,
    max_abs: Optional[float] = None,
    start: Optional[str] = None,
    end: Optional[str] = None,
    decimate: int = 1,
    split_per_sat: bool = False,
    split_per_recv: bool = False,
    basename: Optional[str] = None,
    webgl: bool = False,
    log_level: str = "INFO",
    hover_unified: bool = False,
    plot_normalised_res: bool = False,
    stats_matrix: bool = False,
    stats_matrix_weighted: bool = False,
    annotate_stats_matrix: bool = False,
    amb_totals_topn: Optional[int] = None,
    use_forward_residuals: bool = False,
    include_plotlyjs: bool = True,
) -> List[str]:
    """
    Generate interactive HTML plots from TRACE residual files (programmatic call for Ginan-UI).

    This function provides a programmatic way of generating visualisations from TRACE files,
    mirroring the CLI interface but suitable for calling from Python code.

    Arguments:
        files (List[str]): List of TRACE file paths (wildcards allowed, e.g., "*.TRACE").
        out_dir (str): Output directory for HTML files.
        mark_amb_resets (bool): Overlay PHASE ambiguity reset events on plots.
        mark_large_errors (bool): Mark LARGE STATE / MEAS ERROR events on plots.
        show_stats_table (bool): Add Mean / Std / RMS table per (sat * signal) at the bottom.
        ambiguity_counts (bool): Plot cumulative ambiguity reset counts over time.
        ambiguity_totals (bool): Bar chart of total ambiguity reset reasons.
        amb_totals_orient (str): Orientation for totals bar charts ('h' or 'v').
        residual (str): Which residual to plot ('prefit' or 'postfit').
        receivers (str, optional): Comma/space separated receiver filter.
        sat (List[str], optional): Filter by satellite ID(s).
        label_regex (str, optional): Regex to filter labels.
        max_abs (float, optional): Maximum absolute residual value to include.
        start (str, optional): Start datetime or time-only filter.
        end (str, optional): End datetime (exclusive) filter.
        decimate (int): Decimation factor for plotting.
        split_per_sat (bool): Create separate plots per satellite.
        split_per_recv (bool): Create separate plots per receiver.
        basename (str, optional): Base filename prefix for outputs.
        webgl (bool): Use WebGL for rendering.
        log_level (str): Logging verbosity level.
        hover_unified (bool): Use unified hover tooltips.
        plot_normalised_res (bool): Also generate normalised residual plots.
        stats_matrix (bool): Generate receiver×satellite heatmaps.
        stats_matrix_weighted (bool): Use sigma-weighted statistics in heatmaps.
        annotate_stats_matrix (bool): Write numeric values into heatmap cells.
        amb_totals_topn (int, optional): Show only top-N receivers/satellites by resets.
        use_forward_residuals (bool): Use forward file residuals instead of smoothed.
        include_plotlyjs (bool): If True (default), embed Plotly.js in HTML for offline viewing.

    Returns:
        List[str]: Paths to generated HTML files.

    Example:
        >>> html_files = plot_trace_res_files(
        ...     files=["output/Network*.TRACE"],
        ...     out_dir="output/visual",
        ...     mark_amb_resets=True,
        ...     show_stats_table=True,
        ... )
    """

    class Args:
        def __init__(self):
            self.files = files
            self.out_dir = out_dir
            self.mark_amb_resets = mark_amb_resets
            self.mark_large_errors = mark_large_errors
            self.show_stats_table = show_stats_table
            self.ambiguity_counts = ambiguity_counts
            self.ambiguity_totals = ambiguity_totals
            self.amb_totals_orient = amb_totals_orient
            self.residual = residual
            self.receivers = receivers
            self.sat = sat
            self.label_regex = label_regex
            self.max_abs = max_abs
            self.start = start
            self.end = end
            self.decimate = decimate
            self.split_per_sat = split_per_sat
            self.split_per_recv = split_per_recv
            self.basename = basename
            self.webgl = webgl
            self.log_level = log_level
            self.hover_unified = hover_unified
            self.plot_normalised_res = plot_normalised_res
            self.stats_matrix = stats_matrix
            self.stats_matrix_weighted = stats_matrix_weighted
            self.annotate_stats_matrix = annotate_stats_matrix
            self.amb_totals_topn = amb_totals_topn
            self.use_forward_residuals = use_forward_residuals
            self.out_prefix = None
            self.include_plotlyjs = include_plotlyjs

    args = Args()
    return _process_and_plot_trace(args, from_cli=False)


def _process_and_plot_trace(args, from_cli: bool = False) -> List[str]:
    """
    Internal helper to process TRACE files and generate plots.
    Shared function between CLI and programmatic calls.

    Arguments:
        args: Arguments object with all configuration options.
        from_cli: If True, split file arguments on comma / space (CLI mode).
                  If False, treat each file argument as a complete path (programmatic mode).

    Returns:
        List[str]: Paths to generated HTML files.
    """
    _setup_logging(args.log_level)

    # Handle normalised/normalized spelling
    if hasattr(args, "plot_normalized_res"):
        args.plot_normalised_res = args.plot_normalised_res or args.plot_normalized_res

    import glob, os, re
    from pathlib import Path
    from typing import List

    # --- Expand wildcards and handle comma/space separated patterns ---
    # For programmatic calls (from_cli=False), each element in args.files is treated
    # as a complete path that may contain spaces.
    # For CLI calls (from_cli=True), we split on comma/space for user convenience.
    patterns: List[str] = []
    for tok in args.files:
        tok_str = str(tok)
        if from_cli:
            # CLI mode: split on comma/space for user convenience
            # But only split on comma, not space, to better handle paths with spaces
            # Actually, for CLI the shell typically handles quoting, so we split on comma only
            parts = [s.strip() for s in tok_str.split(",") if s.strip()]
            patterns.extend(parts)
        else:
            # Programmatic mode: treat each path as complete (may contain spaces)
            patterns.append(tok_str)

    expanded_files: List[str] = []
    for pattern in patterns:
        # Expand ~ and wildcards; keep deterministic order
        matches = sorted(glob.glob(os.path.expanduser(pattern)))
        if not matches:
            print(f"⚠️ No files matched pattern: {pattern}")
        expanded_files.extend(matches)

    # Replace the original with the expanded list (still strings at this point)
    args.files = expanded_files

    if not args.files:
        logger.error("No input files found after wildcard expansion.")
        return []

    # --- Normalize into Path objects, validate existence, and de-duplicate (preserve order) ---
    in_paths: List[Path] = []
    seen = set()
    for f in args.files:
        pth = Path(f)
        if not pth.exists():
            logger.error(f"File not found: {pth}")
            continue
        key = str(pth.resolve())
        if key not in seen:
            seen.add(key)
            in_paths.append(pth)

    if not in_paths:
        logger.error("No valid input files provided.")
        return []

    # de-duplicate, preserve order
    seen = set()
    in_paths = [p for p in in_paths if not (str(p.resolve()) in seen or seen.add(str(p.resolve())))]

    if not in_paths:
        logger.error("No input files provided to --files.")
        return []

    # --- Pair forward and smoothed files ---
    residual_paths, forward_paths, file_warnings = pair_forward_smoothed_files(in_paths, args.use_forward_residuals)

    # Display any file pairing warnings
    for warn in file_warnings:
        logger.warning(warn)

    # For output filenames, use only forward files (or unique base names) to avoid duplication
    # This prevents names like "file_file_smoothed" when both forward and smoothed exist
    output_basename_paths = forward_paths if forward_paths else residual_paths

    # --- Normalize receiver filters from --receivers into an ordered, deduplicated list ---
    if args.receivers:
        recv_list = [tok.strip() for tok in re.split(r"[,\s]+", args.receivers) if tok.strip()]
        # dedupe case-insensitively, preserve order
        seen = set()
        recv_list = [r for r in recv_list if not (r.upper() in seen or seen.add(r.upper()))]
    else:
        recv_list = None

    # AJC --- Generator to iterate through all files without loading into memory ---
    def iter_all_lines(paths):
        """Generator that yields lines from all input files."""
        for pth in paths:
            with pth.open("r", encoding="utf-8", errors="ignore") as fh:
                yield from fh

    # Parse files using generator (makes multiple passes to reduce memory usage)
    # Use residual_paths for residuals (smoothed if available), forward_paths for other data

    # If we have both smoothed and forward files, merge them:
    # - Use postfit from smoothed (more accurate residuals)
    # - Use sigma from forward (has measurement weights)
    if residual_paths != forward_paths:
        logger.info("Merging smoothed and forward files: postfit from smoothed, sigma from forward")
        df_smoothed = parse_trace_lines(iter_all_lines(residual_paths))
        df_forward = parse_trace_lines(iter_all_lines(forward_paths))

        # Normalize string columns for robust matching
        for col in ["meas", "sat", "recv", "sig"]:
            if col in df_smoothed.columns:
                df_smoothed[col] = df_smoothed[col].astype(str).str.strip()
            if col in df_forward.columns:
                df_forward[col] = df_forward[col].astype(str).str.strip()

        # Merge on matching keys
        merge_keys = ["datetime", "meas", "sat", "recv", "sig"]
        df = df_forward.merge(
            df_smoothed[merge_keys + ["postfit"]], on=merge_keys, how="left", suffixes=("_fwd", "_smo")
        )

        # Use smoothed postfit where available, otherwise fall back to forward
        if "postfit_smo" in df.columns:
            n_smoothed = df["postfit_smo"].notna().sum()
            n_forward = df["postfit_smo"].isna().sum()
            df["postfit"] = df["postfit_smo"].fillna(df["postfit_fwd"])

            # Recalculate postfit_ratio if it exists (postfit_ratio = postfit / sigma)
            if "postfit_ratio" in df.columns and "sigma" in df.columns:
                # Use smoothed postfit with forward sigma
                df["postfit_ratio"] = df["postfit"] / df["sigma"].replace(0, float("nan"))

            df = df.drop(columns=["postfit_fwd", "postfit_smo"])
            logger.info(f"Merged: {len(df)} measurements total ({n_smoothed} from smoothed, {n_forward} forward-only)")
        else:
            logger.warning("Merge did not produce postfit_smo column - using forward values only")
    else:
        df = parse_trace_lines(iter_all_lines(residual_paths))
        # df = parse_trace_lines_fast(iter_all_lines(residual_paths))

    df_large = parse_large_errors(iter_all_lines(forward_paths)) if args.mark_large_errors else pd.DataFrame()

    # --- Apply CLI filters to residuals ---
    df = filter_df(df, receivers=recv_list, sats=args.sat, label_regex=args.label_regex)
    df = keep_last_iteration(df)

    # --- Time window filtering ---
    start_dt = _parse_dt_like(args.start, df["datetime"]) if args.start else None
    end_dt = _parse_dt_like(args.end, df["datetime"]) if args.end else None
    if start_dt is not None:
        df = df[df["datetime"] >= start_dt]
    if end_dt is not None:
        df = df[df["datetime"] < end_dt]

    # --- Apply same filters/time window to large-error events ---
    if args.mark_large_errors and not df_large.empty:
        df_large = filter_large_errors(df_large, receivers=recv_list, sats=args.sat, start_dt=start_dt, end_dt=end_dt)

    # Parse ambiguity resets if needed by any ambiguity feature (from forward files only)
    need_amb = bool(args.mark_amb_resets or args.ambiguity_counts or args.ambiguity_totals)
    df_amb = parse_ambiguity_resets(iter_all_lines(forward_paths)) if need_amb else pd.DataFrame()

    # --- Schema normalize & assert (Part 1) ---
    REQUIRED_AMB_COLS = ["datetime", "sat", "recv", "action", "sig", "reasons"]

    if not need_amb:
        # Ensure empty-but-well-formed frame so downstream never breaks
        df_amb = pd.DataFrame(columns=REQUIRED_AMB_COLS)

    else:
        if df_amb is None or df_amb.empty:
            # Still provide the required columns for consistency
            df_amb = pd.DataFrame(columns=REQUIRED_AMB_COLS)
        else:
            # Ensure string col names
            df_amb = df_amb.rename(columns=str).copy()

            # Drop duplicate columns by name (keep first occurrence)
            if df_amb.columns.duplicated().any():
                # Optional: log duplicates for visibility
                dup_names = df_amb.columns[df_amb.columns.duplicated()].tolist()
                logger.warning("df_amb had duplicate columns, dropping dups: %s", dup_names)
                df_amb = df_amb.loc[:, ~df_amb.columns.duplicated(keep="first")]

            # Assert required columns are present
            missing = set(REQUIRED_AMB_COLS) - set(df_amb.columns)
            if missing:
                raise ValueError(f"df_amb missing required columns: {sorted(missing)}; have={list(df_amb.columns)}")

            # Coerce dtypes (lightweight, no extra normalization logic)
            df_amb["datetime"] = pd.to_datetime(df_amb["datetime"], errors="coerce")
            for c in ("sat", "recv", "action", "sig", "reasons"):
                df_amb[c] = df_amb[c].astype(str)

    # --- Now apply the same CLI/time-window filters ---
    if need_amb and not df_amb.empty:
        df_amb = filter_ambiguity_resets(
            df_amb,
            receivers=recv_list,
            sats=args.sat,
            start_dt=start_dt,
            end_dt=end_dt,
        )

    # after building df_amb (parse_ambiguity_resets + filter_ambiguity_resets)
    dups = df_amb.columns[df_amb.columns.duplicated()].tolist()
    if dups:
        raise RuntimeError(f"df_amb has duplicate columns: {dups}")

    if "action" not in df_amb.columns:
        raise RuntimeError(f"df_amb missing 'action' column; cols={list(df_amb.columns)}")

    # Y-field & outlier trim
    yfield = args.residual
    if args.max_abs is not None:
        df = df[df[yfield].abs() <= args.max_abs]

    # Compute normalised residual columns
    sigma = pd.to_numeric(df["sigma"], errors="coerce")
    sigma_nz = sigma.mask(sigma == 0)  # -> NaN where zero to avoid divide-by-zero
    df["norm_prefit"] = pd.to_numeric(df["prefit"], errors="coerce") / sigma_nz
    df["norm_postfit"] = pd.to_numeric(df["postfit"], errors="coerce") / sigma_nz

    # Keep a non-decimated copy for precise y lookups of MEAS error markers
    df_lookup = df.copy()

    # Decimate for plotting
    df = decimate_per_pair(df, args.decimate)

    # Output filenames root -  A.trace + B.sum -> base_root = "A_B"; final base = "A_B_postfit"
    # Use output_basename_paths to avoid duplicating forward+smoothed in filename

    # Smart filename generation to avoid "File name too long" errors
    if len(output_basename_paths) == 1:
        # Single file: use the name
        base_root = output_basename_paths[0].stem
    else:
        # Multiple files: find common prefix or use generic name with count
        stems = [p.stem for p in output_basename_paths]

        # Try to find common prefix (e.g., "network-" for network files)
        if len(stems) > 0:
            # Find longest common prefix
            prefix = stems[0]
            for stem in stems[1:]:
                while stem[: len(prefix)] != prefix and prefix:
                    prefix = prefix[:-1]

            # Use common prefix if meaningful (at least 5 chars), otherwise use first file
            if len(prefix) >= 5:
                # Remove trailing dashes/underscores
                prefix = prefix.rstrip("-_")
                base_root = f"{prefix}_{len(stems)}_files"
            else:
                base_root = f"{stems[0]}_{len(stems)}_files"
        else:
            base_root = "network"

    chosen_base = args.basename if args.basename else f"{base_root}_{args.residual}"
    chosen_base = slugify(chosen_base)
    out_dir = Path(args.out_dir).expanduser().resolve() if args.out_dir else Path.cwd()
    out_dir.mkdir(parents=True, exist_ok=True)

    # --- canonical naming roots ---
    base_root = slugify(args.basename) if getattr(args, "basename", None) else "output"
    base = str((out_dir / base_root))
    variant_suffix = ""
    if getattr(args, "stats_matrix_weighted", False):
        variant_suffix = "_w"
    try:
        logger.debug("base=%s variant_suffix=%s", base, variant_suffix)
    except Exception:
        pass
    base = str(out_dir / chosen_base)

    # Graphics acceleration
    use_webgl = args.webgl

    include_plotlyjs = getattr(args, "include_plotlyjs", True)
    plotlyjs_setting = True if include_plotlyjs else "cdn"

    # Build which variants we will plot (raw + optional normalised)
    plot_variants = [("raw", yfield, "", "residual")]
    if args.plot_normalised_res:
        plot_variants.append(("norm", f"norm_{yfield}", "_norm", "normalised residual (res/σ)"))

    all_outputs: List[str] = []

    for variant_tag, variant_yfield, variant_suffix, ytitle in plot_variants:

        lookup_cache = build_lookup_cache(df_lookup, variant_yfield)

        meas_map: Dict[str, List[Tuple[str, str]]] = {"code": [], "phase": []}
        outputs_variant: List[str] = []

        for meas_name, short in [("CODE_MEAS", "code"), ("PHAS_MEAS", "phase")]:
            df_m = df[df["meas"] == meas_name]
            if df_m.empty:
                continue

            is_phase = meas_name == "PHAS_MEAS"

            if args.split_per_sat:
                for sat in sorted(df_m["sat"].unique(), key=_sat_sort_key):
                    df_ms = df_m[df_m["sat"] == sat]
                    if df_ms.empty:
                        continue
                    fig = make_plot(
                        df_ms,
                        variant_yfield,
                        title=f"{sat} {short.upper()} {ytitle}",
                        use_webgl=use_webgl,
                        df_large=df_large,
                        context={"sat": sat, "meas_type": meas_name},
                        hover_unified=args.hover_unified,
                        df_lookup=df_lookup,
                        lookup_cache=lookup_cache,
                        show_stats=args.show_stats_table,
                        df_amb=(df_amb if (args.mark_amb_resets and is_phase) else pd.DataFrame()),
                    )
                    out_html = build_out_path(
                        base,
                        variant_suffix,
                        short,
                        split="sat",
                        key=sat,
                        tag="residual",
                        ext="html",
                    )
                    ensure_parent(out_html)
                    fig.write_html(
                        out_html, include_plotlyjs=plotlyjs_setting, validate=False, config={"scrollZoom": True}
                    )
                    logger.info("Wrote: %s", out_html)
                    outputs_variant.append(out_html)
                    meas_map[short].append((sat, out_html))

            elif args.split_per_recv:
                for recv in sorted(df_m["recv"].unique(), key=_recv_sort_key):
                    df_mr = df_m[df_m["recv"] == recv]
                    if df_mr.empty:
                        continue

                    # right before the make_plot() call
                    is_phase = meas_name == "PHAS_MEAS"  # make this exact comparison
                    df_amb_for_plot = df_amb if (args.mark_amb_resets and is_phase) else pd.DataFrame()

                    fig = make_plot(
                        df=df_mr,
                        residual_field=variant_yfield,
                        title=f"{recv} {short.upper()} {ytitle}",
                        use_webgl=use_webgl,
                        df_large=df_large,
                        context={"recv": recv, "meas_type": meas_name},
                        hover_unified=args.hover_unified,
                        df_lookup=df_lookup,
                        lookup_cache=lookup_cache,
                        show_stats=args.show_stats_table,
                        df_amb=df_amb_for_plot,  # <- pass the guarded DF
                    )
                    out_html = build_out_path(
                        base,
                        variant_suffix,
                        short,
                        split="recv",
                        key=recv,
                        tag="residual",
                        ext="html",
                    )
                    ensure_parent(out_html)
                    fig.write_html(
                        out_html, include_plotlyjs=plotlyjs_setting, validate=False, config={"scrollZoom": True}
                    )
                    logger.info("Wrote: %s", out_html)
                    outputs_variant.append(out_html)
                    meas_map[short].append((recv, out_html))

            else:
                fig = make_plot(
                    df_m,
                    variant_yfield,
                    title=f"GNSS {short.upper()} {ytitle}",
                    use_webgl=use_webgl,
                    df_large=df_large,
                    context={"meas_type": meas_name},
                    hover_unified=args.hover_unified,
                    df_lookup=df_lookup,
                    lookup_cache=lookup_cache,
                    show_stats=args.show_stats_table,
                    df_amb=(df_amb if (args.mark_amb_resets and is_phase) else pd.DataFrame()),
                )

                out_html = build_out_path(base, variant_suffix, short, tag="residual", ext="html")
                ensure_parent(out_html)
                fig.write_html(out_html, include_plotlyjs=plotlyjs_setting, validate=False, config={"scrollZoom": True})
                outputs_variant.append(out_html)

        # Build an index page for this variant if split mode was used
        if (args.split_per_sat or args.split_per_recv) and (meas_map["code"] or meas_map["phase"]):
            mode_tag = "sat" if args.split_per_sat else "recv"
            index_path = Path(f"{base}{variant_suffix}_index_{mode_tag}.html")
            # Build trace file info showing which files were used for what
            residual_file_names = ", ".join(p.name for p in residual_paths)
            if residual_paths != forward_paths:
                trace_files_info = (
                    f"Residuals: {residual_file_names} | Amb/Errors: {', '.join(p.name for p in forward_paths)}"
                )
            else:
                trace_files_info = residual_file_names

            meta = {
                "Trace files": trace_files_info,
                "Residual type": variant_yfield,
                "Split mode": "per-sat" if args.split_per_sat else "per-recv",
                "Receiver filter(s)": ", ".join(recv_list) if recv_list else "",
                "Sat filter(s)": ", ".join(args.sat) if args.sat else "",
                "Label regex": args.label_regex or "",
                "Start": start_dt.isoformat() if start_dt is not None else "",
                "End (exclusive)": end_dt.isoformat() if end_dt is not None else "",
                "Decimate (N)": str(args.decimate),
                "WebGL": "yes" if use_webgl else "no",
                "Hover mode": "unified" if args.hover_unified else "closest",
                "Large errors": "on" if args.mark_large_errors else "off",
                "Variant": variant_tag,
            }
            write_index_html(
                index_path,
                base_title=Path(base + variant_suffix).name,
                meas_map=meas_map,
                meta=meta,
                item_kind="sat" if args.split_per_sat else "recv",
            )
            logger.info(f"Wrote: {index_path}")

        # Print this variant’s outputs (once), then proceed
        if outputs_variant:
            all_outputs.extend(outputs_variant)
            logger.info("Wrote %d plot files for variant '%s'.", len(outputs_variant), variant_tag)

        # ---- Receiver × Satellite heatmaps (weighted OR unweighted, not both) ----
        if args.stats_matrix or args.stats_matrix_weighted:
            # choose data source for stats (full-res recommended)
            mat_source = df_lookup.copy() if df_lookup is not None else df.copy()

            # Single mode: weighted if --stats-matrix-weighted, else unweighted
            weighted = bool(args.stats_matrix_weighted)

            annotate_mode = "all" if getattr(args, "annotate_stats_matrix", False) else "none"

            # loop per measurement type so CODE and PHASE get separate heatmaps
            for meas_name, dtype in [("CODE_MEAS", "code"), ("PHAS_MEAS", "phase")]:
                ms = mat_source[mat_source["meas"] == meas_name].copy()
                if ms.empty:
                    logger.info("No samples for %s; skipping stats-matrix.", dtype)
                    continue

                stats_mats = build_recv_sat_stats(ms, variant_yfield, weighted=weighted)  # {"mean","std","rms"}

                # ---- Build counts matrix aligned to stats_mats ----
                yv = pd.to_numeric(ms[variant_yfield], errors="coerce")
                counts = ms.loc[yv.notna()].groupby(["recv", "sat"], sort=False).size().unstack(fill_value=0)

                # Add ALL_SAT (per receiver) as first column
                counts["ALL_SAT"] = counts.sum(axis=1)

                # Add ALL_RECV (per satellite) as first row
                all_recv_row = pd.DataFrame([counts.sum(axis=0)], index=["ALL_RECV"])
                counts_full = pd.concat([all_recv_row, counts], axis=0)

                # Add top-left grand total
                counts_full.loc["ALL_RECV", "ALL_SAT"] = int(counts.values.sum())

                # Ensure ordering matches stats matrices (ALL_RECV top, ALL_SAT left)
                def _align_counts(mat: pd.DataFrame) -> pd.DataFrame:
                    if mat is None or mat.empty:
                        return counts_full
                    return counts_full.reindex(index=mat.index, columns=mat.columns)

                # ---------- Titles ----------
                wtxt = "weighted" if weighted else "unweighted"
                titles = {
                    "mean": f"{dtype.upper()} — Receiver × Satellite — Mean ({wtxt}) — {ytitle}",
                    "std": f"{dtype.upper()} — Receiver × Satellite — Std Dev ({wtxt}) — {ytitle}",
                    "rms": f"{dtype.upper()} — Receiver × Satellite — RMS ({wtxt}) — {ytitle}",
                }

                # ---------- Mean (diverging, centered at 0.0) ----------
                mat = stats_mats.get("mean")
                if mat is not None and not mat.empty:
                    out_html = build_out_path(base, variant_suffix, f"stats_matrix_mean_{dtype}")
                    ensure_parent(out_html)
                    write_heatmap_html(
                        mat,
                        titles["mean"],
                        out_html,
                        colorscale="RdBu",
                        reversescale=True,
                        zmid=0.0,
                        zmin=None,
                        zmax=None,
                        counts=_align_counts(mat).astype(float),
                        row_sort="alpha",
                        sat_sort="natural",
                        annotate=annotate_mode,
                    )
                    logger.info("Wrote: %s", out_html)
                    all_outputs.append(out_html)
                else:
                    logger.info("Mean matrix empty for %s; skipping.", dtype)

                # ---------- Std & RMS (sequential from 0) ----------
                for metric in ("std", "rms"):
                    mat = stats_mats.get(metric)
                    if mat is None or mat.empty:
                        logger.info("%s matrix empty for %s; skipping.", metric.upper(), dtype)
                        continue

                    short = f"stats_matrix_{metric}_{dtype}"

                    # HTML
                    out_html = build_out_path(base, variant_suffix, short)
                    ensure_parent(out_html)
                    zmax = float(np.nanmax(mat.values)) if mat.size else None
                    write_heatmap_html(
                        mat,
                        titles[metric],
                        out_html,
                        colorscale="Blues",
                        reversescale=False,
                        zmid=None,
                        zmin=0.0,
                        zmax=zmax,
                        counts=_align_counts(mat).astype(float),
                        row_sort="alpha",
                        sat_sort="natural",
                        annotate=annotate_mode,
                    )
                    logger.info("Wrote: %s", out_html)
                    all_outputs.append(out_html)

                    # CSV
                    out_csv = build_out_path(base, variant_suffix, short, ext="csv")
                    ensure_parent(out_csv)
                    mat.to_csv(out_csv)
                    logger.info("Wrote: %s", out_csv)

    # Shared prefix (same as other outputs)
    prefix = base

    # Ambiguity reset plots (reasons + unique resets)
    if args.ambiguity_counts:
        split_mode = "recv" if args.split_per_recv else ("sat" if args.split_per_sat else "combined")
        amb_count_outputs = plot_ambiguity_reason_counts_inline(
            df_amb,
            split=split_mode,
            base=base,
            variant_suffix=variant_suffix,
            include_plotlyjs=plotlyjs_setting,
        )
        all_outputs.extend(amb_count_outputs)

    if args.ambiguity_totals:
        split_mode = "recv" if args.split_per_recv else ("sat" if args.split_per_sat else "combined")
        amb_total_outputs = plot_ambiguity_reason_totals_inline(
            df_amb,
            split=split_mode,
            orientation=args.amb_totals_orient,
            top_n=args.amb_totals_topn,
            base=base,
            variant_suffix=variant_suffix,
            include_plotlyjs=plotlyjs_setting,
        )
        all_outputs.extend(amb_total_outputs)

    if not all_outputs:
        logger.warning("No residuals matched your filters.")

    return all_outputs


def plot_ambiguity_reason_counts_inline(
    df_amb: pd.DataFrame,
    split: str = "combined",
    *,
    base: str,
    variant_suffix: str,
    include_plotlyjs=True,
) -> List[str]:
    """
    Generate cumulative ambiguity-reset reason count plots with configurable plotlyjs embedding.
    This is a wrapper around the plotting logic for programmatic use.
    """
    outputs = []
    if df_amb is None or df_amb.empty:
        logger.warning("No ambiguity-reset data to plot.")
        return outputs

    # Process and deduplicate ambiguity reasons
    df_reasons = prepare_ambiguity_reasons(df_amb)
    if df_reasons.empty:
        logger.warning("No ambiguity-reset reasons found after processing.")
        return outputs

    df_reasons = df_reasons.sort_values("datetime")

    # Also prepare unique satellite reset events
    df_events = prepare_ambiguity_events(df_amb)
    df_events = df_events.sort_values("datetime") if not df_events.empty else df_events

    # Choose grouping key
    if split == "recv":
        groups = df_reasons.groupby("recv", dropna=False)
        event_groups = df_events.groupby("recv", dropna=False) if not df_events.empty else []
    elif split == "sat":
        groups = df_reasons.groupby("sat", dropna=False)
        event_groups = df_events.groupby("sat", dropna=False) if not df_events.empty else []
    else:
        groups = [("ALL", df_reasons)]
        event_groups = [("ALL", df_events)] if not df_events.empty else []

    # Convert event_groups to dict for easy lookup
    event_dict = {k: v for k, v in event_groups}

    for key, g in groups:
        if g.empty:
            continue

        counts = g.groupby(["datetime", "reason"], sort=True).size().reset_index(name="count").sort_values("datetime")
        counts["cumcount"] = counts.groupby("reason")["count"].cumsum()
        pivot = counts.pivot(index="datetime", columns="reason", values="cumcount").ffill().fillna(0)

        fig = go.Figure()
        for reason in pivot.columns:
            fig.add_trace(go.Scatter(x=pivot.index, y=pivot[reason], mode="lines", name=reason))

        # Add unique resets trace if available
        if key in event_dict and not event_dict[key].empty:
            ev = event_dict[key]
            ev_counts = ev.groupby("datetime").size().cumsum().reset_index(name="cumcount")
            fig.add_trace(
                go.Scatter(
                    x=ev_counts["datetime"],
                    y=ev_counts["cumcount"],
                    mode="lines",
                    name="Unique Resets",
                    line=dict(dash="dash", color="black", width=2),
                )
            )

        title_suffix = {
            "recv": f"Receiver {key}",
            "sat": f"Satellite {key}",
            "combined": "All receivers/satellites",
        }.get(split, "All")

        fig.update_layout(
            title=f"Cumulative Ambiguity Resets — {title_suffix}<br><sub>Each reason counted once per epoch-satellite across all signals. 'Unique Resets' shows total satellites affected.</sub>",
            xaxis_title="Time",
            yaxis_title="Cumulative Count",
            template="plotly_white",
            legend_title="Metric",
            hovermode="x unified",
        )

        safe_key = _sanitize_filename_piece(str(key))
        out_html = build_out_path(base, variant_suffix, "ambiguity_counts", split=split, key=safe_key)
        ensure_parent(out_html)
        fig.write_html(out_html, include_plotlyjs=include_plotlyjs, validate=False, config={"scrollZoom": True})
        logger.info(f"Wrote: {out_html}")
        outputs.append(out_html)

    return outputs


def plot_ambiguity_reason_totals_inline(
    df_amb: pd.DataFrame,
    split: str = "combined",
    orientation: str = "h",
    top_n: int = None,
    *,
    base: str,
    variant_suffix: str,
    include_plotlyjs=True,
) -> List[str]:
    """
    Generate stacked bar chart of total ambiguity reset reasons with configurable plotlyjs embedding.
    This is a wrapper around the plotting logic for programmatic use.
    """
    outputs = []
    if df_amb is None or df_amb.empty:
        logger.warning("No ambiguity-reset data to plot.")
        return outputs

    # Process and deduplicate ambiguity reasons
    df_reasons = prepare_ambiguity_reasons(df_amb)
    if df_reasons.empty:
        logger.warning("No ambiguity-reset reasons found after processing.")
        return outputs

    # Choose grouping key
    if split == "recv":
        group_col = "recv"
    elif split == "sat":
        group_col = "sat"
    else:
        group_col = None

    if group_col:
        totals = df_reasons.groupby([group_col, "reason"]).size().unstack(fill_value=0)
        totals["_total"] = totals.sum(axis=1)
        totals = totals.sort_values("_total", ascending=False)
        if top_n:
            totals = totals.head(top_n)
        totals = totals.drop(columns=["_total"])
    else:
        totals = df_reasons.groupby("reason").size()
        totals = totals.sort_values(ascending=False)
        totals = totals.to_frame(name="count").T
        totals.index = ["All"]

    fig = go.Figure()
    reasons = [c for c in totals.columns if c != "_total"]
    for reason in reasons:
        if orientation == "h":
            fig.add_trace(
                go.Bar(
                    y=totals.index,
                    x=totals[reason],
                    name=reason,
                    orientation="h",
                )
            )
        else:
            fig.add_trace(
                go.Bar(
                    x=totals.index,
                    y=totals[reason],
                    name=reason,
                )
            )

    title_suffix = {
        "recv": "by Receiver",
        "sat": "by Satellite",
        "combined": "Total",
    }.get(split, "")

    fig.update_layout(
        title=f"Ambiguity Reset Reasons {title_suffix}",
        barmode="stack",
        template="plotly_white",
        legend_title="Reason",
    )

    if orientation == "h":
        fig.update_layout(xaxis_title="Count", yaxis_title=group_col.capitalize() if group_col else "")
    else:
        fig.update_layout(yaxis_title="Count", xaxis_title=group_col.capitalize() if group_col else "")

    out_html = build_out_path(base, variant_suffix, f"ambiguity_totals_{orientation}")
    ensure_parent(out_html)
    fig.write_html(out_html, include_plotlyjs=include_plotlyjs, validate=False, config={"scrollZoom": True})
    logger.info(f"Wrote: {out_html}")
    outputs.append(out_html)

    return outputs


if __name__ == "__main__":
    main()
