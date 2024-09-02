#!/usr/bin/env python3

"""
Script to compare orbits and clocks in SP3 and CLK files against corresponding reference products,
such as IGS rapid products, for specified time period to
- Calculate orbit/clock differences
- Compute statistics including percentiles, mean, standard deviation, and RMS for each satellite
    and the whole constellation
- Plot out orbit/clock differences and RMS errors
It can run in post-processing (finite) mode or real-time (infinite) mode.
"""

import os
import sys
import time
import click
import logging
import matplotlib
import matplotlib.font_manager
import numpy as np
import pandas as pd
import gnssanalysis as ga
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from pathlib import Path
from matplotlib.figure import Figure
from datetime import date, datetime, timedelta

parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, parent_dir)

from auto_download_PPP import generate_sampling_rate


def set_up_matplotlib(
    font: str,
    colormap: str,
) -> None:
    """
    Set up font and colormap for matplotlib

    :param str font: font name
    :param str colormap: colormap name
    :return None
    """
    try:
        matplotlib.rcParams["font.family"] = font
    except:
        logging.exception(
            f"Preferred font '{font}' for plotting not available, using default font {matplotlib.rcParams['font.family']}"
        )
        logging.debug(
            f"Available fonts: {', '.join([ft.name for ft in matplotlib.font_manager.fontManager.ttflist])}"
        )

    try:
        matplotlib.rcParams["image.cmap"] = colormap
    except:
        logging.exception(
            f"Preferred colormap '{colormap}' for plotting not available, using default colormap {matplotlib.rcParams['image.cmap']}"
        )
        logging.debug(f"Available colormaps: {', '.join(plt.colormaps())}")


def sp3_diff(
    ref_sp3_file: Path,
    job_sp3_file: Path,
) -> pd.DataFrame:
    """
    Compare two SP3 files to calculate orbit and clock differences. The orbit differences will be represented
    in both X/Y/Z ECEF frame and R/A/C orbit frame, and the clock differences will NOT be normalised.

    :param Path ref_sp3_file: Path of the reference SP3 file
    :param Path job_sp3_file: Path of the testing SP3 file
    :return pd.DataFrame: The Pandas DataFrame containing orbit and clock differences
    """
    tic = time.time()
    ref_sp3_df = ga.gn_io.sp3.read_sp3(str(ref_sp3_file))
    job_sp3_df = ga.gn_io.sp3.read_sp3(str(job_sp3_file))
    toc = time.time()
    logging.debug(f"Took {toc-tic} seconds to read SP3 files")

    # Select rows with matching indices and calculate ECEF differences
    common_indices = ref_sp3_df.index.intersection(
        job_sp3_df.index
    )  # get common indices
    diff_EST_df = (
        job_sp3_df.loc[common_indices, "EST"] - ref_sp3_df.loc[common_indices, "EST"]
    )

    # The index is in J2000 seconds but python datetimes are a little easier to read
    diff_EST_df.index = pd.MultiIndex.from_tuples(
        (
            (idx[0] + ga.gn_const.J2000_ORIGIN, idx[1])
            for idx in diff_EST_df.index.values
        )
    )

    # Rename the indices
    diff_EST_df.index = diff_EST_df.index.set_names(["Epoch", "Satellite"])

    # Extract clocks and change the units from ms to ns
    diff_CLK_df = diff_EST_df["CLK"].to_frame(name="CLK") * 1e3

    # Drop the clocks and then change the units from km to m
    diff_XYZ_df = diff_EST_df.drop(columns=["CLK"]) * 1e3

    # RAC difference
    diff_RAC_df = ga.gn_io.sp3.diff_sp3_rac(
        ref_sp3_df, job_sp3_df, hlm_mode=None
    )  # TODO: hlm_mode

    # Change the units from km to m (read_sp3 and diff_sp3_rac will result in a dataframe in sp3 units (km))
    diff_RAC_df = diff_RAC_df * 1e3

    # The index is in J2000 seconds but python datetimes are a little easier to read
    diff_RAC_df.index = pd.MultiIndex.from_tuples(
        (
            (idx[0] + ga.gn_const.J2000_ORIGIN, idx[1])
            for idx in diff_RAC_df.index.values
        )
    )

    # Name the indices
    diff_RAC_df.index = diff_RAC_df.index.set_names(["Epoch", "Satellite"])

    # Drop the not-particularly needed 'EST_RAC' multi-index level
    diff_RAC_df.columns = diff_RAC_df.columns.droplevel(0)

    diff_df = diff_XYZ_df.join(diff_RAC_df)
    diff_df["3D-Total"] = diff_XYZ_df.pow(2).sum(axis=1, min_count=3).pow(0.5)
    diff_df["Clock"] = diff_CLK_df

    return diff_df


def clk_diff(
    ref_clk_file: Path,
    job_clk_file: Path,
    norm_types: list[str],
) -> pd.DataFrame:
    """
    Compare two CLK files to calculate clock differences with common mode removed (if specified)
    based on the chosen normalisations.

    :param Path ref_clk_file: Path of the reference CLK file
    :param Path job_clk_file: Path of the testing CLK file
    :param norm_types list[str]: Normalizations to apply. Available options include 'epoch', 'daily', 'sv',
            any satellite PRN, or any combination of them, defaults to None
    :return pd.DataFrame: The Pandas DataFrame containing clock differences
    """
    tic = time.time()
    ref_clk_df = ga.gn_io.clk.read_clk(ref_clk_file)
    job_clk_df = ga.gn_io.clk.read_clk(job_clk_file)
    toc = time.time()
    logging.debug(f"Took {toc-tic} seconds to read CLK files")

    diff_CLK_df = ga.gn_diffaux.compare_clk(
        job_clk_df, ref_clk_df, norm_types=norm_types
    )
    diff_CLK_df = diff_CLK_df.stack()  # compare_clk() returns unstacked dataframe

    # Change the units from s to ns (read_clk and compare_clk will result in a dataframe in clk units (s))
    diff_CLK_df = diff_CLK_df.to_frame(name="Clock") * 1e9

    # The index is in J2000 seconds but python datetimes are a little easier to read
    diff_CLK_df.index = pd.MultiIndex.from_tuples(
        (
            (idx[0] + ga.gn_const.J2000_ORIGIN, idx[1])
            for idx in diff_CLK_df.index.values
        )
    )

    # Name the indices
    diff_CLK_df.index = diff_CLK_df.index.set_names(["Epoch", "Satellite"])

    return diff_CLK_df


def sp3_stats(
    res: pd.DataFrame,
) -> pd.DataFrame:
    """
    Compute statistics of SP3 differences in a Pandas DataFrame.

    :param pd.DataFrame res: The Pandas DataFrame containing SP3 differences
    :return pd.DataFrame: The Pandas DataFrame containing statistics of SP3 differences
    """
    df = res[["Radial", "Along-track", "Cross-track", "3D-Total", "Clock"]]
    stats = df.describe(percentiles=[0.25, 0.50, 0.75, 0.90, 0.95])
    stats.loc["rms"] = df.pow(2).mean().pow(0.5)

    stats.index = pd.MultiIndex.from_tuples(
        (("All", idx) for idx in stats.index.values)
    )
    stats.index = stats.index.set_names(["Satellite", "Stats"])

    sat_index = res.index.get_level_values("Satellite")
    svs = sorted(list(sat_index.unique()))

    for sat in svs:
        res_sat = res[sat_index == sat]

        df = res_sat[["Radial", "Along-track", "Cross-track", "3D-Total", "Clock"]]
        stats_sat = df.describe(percentiles=[0.25, 0.50, 0.75, 0.90, 0.95])
        stats_sat.loc["rms"] = df.pow(2).mean().pow(0.5)

        stats_sat.index = pd.MultiIndex.from_tuples(
            ((sat, idx) for idx in stats_sat.index.values)
        )
        stats_sat.index = stats_sat.index.set_names(["Satellite", "Stats"])

        stats = pd.concat([stats, stats_sat])

    return stats


def clk_stats(
    res: pd.DataFrame,
) -> pd.DataFrame:
    """
    Compute statistics of CLK differences in a Pandas DataFrame.

    :param pd.DataFrame res: The Pandas DataFrame containing CLK differences
    :return pd.DataFrame: The Pandas DataFrame containing statistics of CLK differences
    """
    stats = res.describe(percentiles=[0.25, 0.50, 0.75, 0.90, 0.95])
    stats.loc["rms"] = res.pow(2).mean().pow(0.5)

    stats.index = pd.MultiIndex.from_tuples(
        (("All", idx) for idx in stats.index.values)
    )
    stats.index = stats.index.set_names(["Satellite", "Stats"])

    sat_index = res.index.get_level_values("Satellite")
    svs = sorted(list(sat_index.unique()))

    for sat in svs:
        res_sat = res[sat_index == sat]

        stats_sat = res_sat.describe(percentiles=[0.25, 0.50, 0.75, 0.90, 0.95])
        stats_sat.loc["rms"] = res_sat.pow(2).mean().pow(0.5)

        stats_sat.index = pd.MultiIndex.from_tuples(
            ((sat, idx) for idx in stats_sat.index.values)
        )
        stats_sat.index = stats_sat.index.set_names(["Satellite", "Stats"])

        stats = pd.concat([stats, stats_sat])

    return stats


def plot_orb_diff(
    res: pd.DataFrame,
    svs: list[str],
    title: str,
    start_epoch: datetime,
    end_epoch: datetime,
) -> Figure:
    """
    Plot orbit differences from a Pandas DataFrame.

    :param pd.DataFrame res: The Pandas DataFrame containing orbit differences
    :param list[str] svs: Satellite list to plot differences for
    :param str title: Figure title
    :param datetime start_epoch: Start of epoch time
    :param datetime end_epoch: End of epoch time
    :return Figure: The Figure object for the plot
    """
    dim_desc = {
        "R": "Radial",
        "T": "Along-track",
        "N": "Cross-track",
        "3D": "3D-Total",
    }

    # Filter data with given satellite list and unstack the dataframe to push the satellite PRNs
    # from the index to columns as it's easier to plot
    unstacked_diff_df = res[res.index.get_level_values("Satellite").isin(svs)].unstack()
    svs = sorted(list(unstacked_diff_df.columns.get_level_values("Satellite").unique()))
    if not svs:
        logging.error("No data found for given satellites")
        return None

    # Plot epoch-by-epoch orbit comparison results
    fig, ax = plt.subplots(nrows=3, dpi=300.0, figsize=(12, 6))

    locator = mdates.AutoDateLocator()
    formatter = mdates.ConciseDateFormatter(locator)
    formatter.formats = ["%Y", "%m", "%Y-%j", "%H:%M", "%H:%M", "%S.%f"]
    formatter.zero_formats = ["", "%Y", "%Y-%j", "%Y-%j", "%H:%M", "%H:%M:%S"]
    formatter.offset_formats = ["", "", "", "", "%Y-%j", "%Y-%j"]

    for i, dim in enumerate(["R", "T", "N"]):  # choose dimension to plot
        ax[i].plot(
            unstacked_diff_df[dim_desc[dim]].index.values,
            unstacked_diff_df[dim_desc[dim]].values,
        )

        ax[i].grid(True)
        ymin, ymax = ax[i].get_ylim()
        if ymin >= -0.25 and ymax <= 0.25:
            ymin = -0.25
            ymax = 0.25
        ax[i].set_ylim(ymin, ymax)
        ax[i].set_ylabel(f"{dim_desc[dim]} [m]")
        ax[i].set_xlim(start_epoch, end_epoch)
        ax[i].set_xticklabels([])

    ax[-1].xaxis.set_major_locator(locator)
    ax[-1].xaxis.set_major_formatter(formatter)
    ax[-1].set_xlabel("Time")
    ax[0].set_title(title)
    plt.legend(svs, ncol=8, loc="upper center", bbox_to_anchor=(0.5, -0.3))
    plt.close()

    return fig


def plot_clk_diff(
    res: pd.DataFrame,
    svs: list[str],
    title: str,
    start_epoch: datetime,
    end_epoch: datetime,
) -> Figure:
    """
    Plot clock differences from a Pandas DataFrame.

    :param pd.DataFrame res: The Pandas DataFrame containing clock differences
    :param list[str] svs: Satellite list to plot differences for
    :param str title: Figure title
    :param datetime start_epoch: Start of epoch time
    :param datetime end_epoch: End of epoch time
    :return Figure: The Figure object for the plot
    """
    # Filter data with given satellite list and unstack the dataframe to push the satellite PRNs
    # from the index to columns as it's easier to plot
    unstacked_diff_df = res[res.index.get_level_values("Satellite").isin(svs)].unstack()
    svs = sorted(list(unstacked_diff_df.columns.get_level_values("Satellite").unique()))
    if not svs:
        logging.error("No data found for given satellites")
        return None

    # Plot epoch-by-epoch clock comparison results
    fig, ax = plt.subplots(dpi=300.0, figsize=(12, 3))

    locator = mdates.AutoDateLocator()
    formatter = mdates.ConciseDateFormatter(locator)
    formatter.formats = ["%Y", "%m", "%Y-%j", "%H:%M", "%H:%M", "%S.%f"]
    formatter.zero_formats = ["", "%Y", "%Y-%j", "%Y-%j", "%H:%M", "%H:%M:%S"]
    formatter.offset_formats = ["", "", "", "", "%Y-%j", "%Y-%j"]

    ax.plot(
        unstacked_diff_df["Clock"].index.values,
        unstacked_diff_df["Clock"].values,
    )

    ax.grid(True)
    ymin, ymax = ax.get_ylim()
    if ymin >= -0.40 and ymax <= 0.40:
        ymin = -0.40
        ymax = 0.40
    ax.set_ylim(ymin, ymax)
    ax.set_ylabel(f"Clock [ns]")
    ax.set_xlim(start_epoch, end_epoch)
    ax.xaxis.set_major_locator(locator)
    ax.xaxis.set_major_formatter(formatter)
    ax.set_xlabel("Time")
    ax.set_title(title)
    plt.legend(svs, ncol=8, loc="upper center", bbox_to_anchor=(0.5, -0.2))
    plt.close()

    return fig


def plot_orb_rms(
    stats: pd.DataFrame,
    svs: list[str],
    title: str,
) -> Figure:
    """
    Plot orbit RMS errors from a Pandas DataFrame.

    :param pd.DataFrame stats: The Pandas DataFrame containing orbit statistics
    :param list[str] svs: Satellite list to plot orbit RMS errors for
    :param str title: Figure title
    :return Figure: The Figure object for the plot
    """
    dim_desc = {
        "R": "Radial",
        "T": "Along-track",
        "N": "Cross-track",
        "3D": "3D-Total",
    }

    svs = sorted(list(stats.index.get_level_values("Satellite").intersection(svs)))
    if not svs:
        logging.error("No data found for given satellites")
        return None

    # Extract data for Radial, Along-track, and Cross-track RMS
    rms = {}
    for sat in svs:
        rms[sat] = {
            dim_desc[dim]: stats.loc[(sat, "rms"), dim_desc[dim]]
            for dim in ["R", "T", "N"]
        }
    rms_all = {
        dim_desc[dim]: round(stats.loc[("All", "rms"), dim_desc[dim]], 3)
        for dim in ["R", "T", "N"]
    }

    # Plot Radial, Along-track, and Cross-track RMS of each satellite
    fig, ax = plt.subplots(dpi=300.0, figsize=(12, 3))

    bar_width = 0.2
    x = np.arange(len(svs))

    for i, dim in enumerate(["R", "T", "N"]):  # choose dimension to plot
        pos = x + (i - 1) * bar_width
        ax.bar(
            pos,
            [rms[sat][dim_desc[dim]] for sat in svs],
            bar_width,
            label=dim_desc[dim],
        )

    ax.grid(True)
    ymin, ymax = ax.get_ylim()
    ymax = max(0.15, ymax)
    ax.set_ylim([ymin, ymax])
    ax.set_ylabel("RMS [m]")
    ax.set_xlim([x[0] - 1, x[-1] + 1])
    ax.set_xticks(x)
    ax.set_xticklabels(svs, ha="center", rotation=90)
    ax.set_xlabel("Satellite")
    ax.set_title(title)
    ax.legend(loc="upper right")
    ax.text(
        x[0] - 0.5,
        0.95 * ymax,
        f'All satellites: R={rms_all["Radial"]}, A={rms_all["Along-track"]}, C={rms_all["Cross-track"]} m',
        ha="left",
        va="top",
    )  # Eugene: change to mean RMS?
    plt.close()

    return fig


def plot_clk_rms(
    stats: pd.DataFrame,
    svs: list[str],
    title: str,
) -> Figure:
    """
    Plot clock RMS errors from a Pandas DataFrame.

    :param pd.DataFrame stats: The Pandas DataFrame containing clock statistics
    :param list[str] svs: Satellite list to plot clock RMS errors for
    :param str title: Figure title
    :return Figure: The Figure object for the plot
    """
    svs = sorted(list(stats.index.get_level_values("Satellite").intersection(svs)))
    if not svs:
        logging.error("No data found for given satellites")
        return None

    # Extract data for Clock
    rms = {}
    for sat in svs:
        rms[sat] = {"Clock": stats.loc[(sat, "rms"), "Clock"]}
    rms_all = {"Clock": round(stats.loc[("All", "rms"), "Clock"], 3)}

    # Plot Clock RMS of each satellite
    fig, ax = plt.subplots(dpi=300.0, figsize=(12, 3))

    bar_width = 0.2
    x = np.arange(len(svs))

    ax.bar(x, [rms[sat]["Clock"] for sat in svs], bar_width, label="Clock")

    ax.grid(True)
    ymin, ymax = ax.get_ylim()
    ymax = max(0.20, ymax)
    ax.set_ylim([ymin, ymax])
    ax.set_ylabel("RMS [ns]")
    ax.set_xlim([x[0] - 1, x[-1] + 1])
    ax.set_xticks(x)
    ax.set_xticklabels(svs, ha="center", rotation=90)
    ax.set_xlabel("Satellite")
    ax.set_title(title)
    ax.legend(loc="upper right")
    ax.text(
        x[0] - 0.5,
        0.95 * ymax,
        f'All satellites: {rms_all["Clock"]} ns',
        ha="left",
        va="top",
    )  # Eugene: change to mean RMS?
    plt.close()

    return fig


def analyse_orbit_clock(
    job_dir: Path,
    ref_dir: Path,
    ref_prefix: str,
    start_date: datetime,
    session_len: int,
    sat_sys: str,
    excluded_svs: list[str],
    clk_norm_types: list[str],
    rel_output_dir: Path,
) -> None:
    """
    Compare orbits and clocks against reference products and plot differences and RMS errors.

    :param Path job_dir: Directory where data is processed
    :param Path ref_dir: Directory where reference products are placed
    :param str ref_prefix: Prefix of reference products, e.g. IGS0OPSRAP
    :param datetime start_date: Start of epoch time
    :param int session_len: Length of each analysis session
    :param str sat_sys: Satellite systems of orbits and clocks to analyse
    :param list[str] excluded_svs: Satellites to exclude
    :param list[str] clk_norm_types: Normalizations to apply to clock differences. Available options include
            'epoch', 'daily', 'sv', any satellite PRN, or any combination of them, defaults to None
    :param Path rel_output_dir: Relative path of output directory under each sub job directory
    :return None
    """
    sys_meta = {
        "G": {"name": "GPS", "max_sats": 32},
        "R": {"name": "GLO", "max_sats": 27},
        "E": {"name": "GAL", "max_sats": 36},
        "C": {"name": "BDS", "max_sats": 62},
    }

    # Set matplotlib font and colormap
    set_up_matplotlib("DejaVu Sans", "jet")

    # Get session dates
    dates = [start_date + timedelta(days=i) for i in range(session_len)]
    yrdoys = [date.strftime("%Y%j") for date in dates]
    start_yrdoy = yrdoys[0]

    # Create a full satellite list for each constellation
    svs = {}
    for sys in sat_sys:
        svs[sys] = [
            f"{sys}%02d" % i
            for i in range(1, sys_meta[sys]["max_sats"] + 1)
            if f"{sys}%02d" % i not in excluded_svs
        ]

    # Download reference sp3 and clock files
    logging.info("Downloading reference products ...")

    start_epoch = dates[0]
    end_epoch = dates[-1] + timedelta(days=1)

    sampling_rate = {}
    for product in ["SP3", "CLK"]:
        analysis_center = ref_prefix[0:3]
        project_type = ref_prefix[4:7]
        solution_type = ref_prefix[7:10]
        sampling_rate[product] = generate_sampling_rate(
            file_ext=product,
            analysis_center=analysis_center,
            solution_type=solution_type,
        )

        ga.gn_download.download_product_from_cddis(
            download_dir=ref_dir,
            start_epoch=start_epoch,
            end_epoch=end_epoch,
            file_ext=product,
            long_filename=True,
            analysis_center=analysis_center,
            project_type=project_type,
            solution_type=solution_type,
            timespan=timedelta(days=1),
            sampling_rate=sampling_rate[product],
            if_file_present="dont_replace",
        )

    logging.info("Reference products downloaded\n")

    ssr_list = [
        f.name for f in job_dir.iterdir() if f.is_dir() and f.name[:3] == "SSR"
    ]  # we assume that sub folders are named with SSR mountpoints
    for ssr_mountpoint in ssr_list:
        # Set up output directory
        output_dir = job_dir / ssr_mountpoint / rel_output_dir
        ga.gn_utils.ensure_folders([output_dir])

        # Compare sp3 using gnssanalysis
        logging.info("Comparing SP3 files ...")

        # Get input file paths
        input_paths = []
        for yrdoy in yrdoys:
            input_paths.append(
                [
                    ref_dir
                    / f"{ref_prefix}_{yrdoy}0000_01D_{sampling_rate['SP3']}_ORB.SP3",
                    job_dir / ssr_mountpoint / f"{ssr_mountpoint}_{yrdoy}0000_ORB.sp3",
                ]
            )

        res_sp3 = pd.DataFrame()

        for input in input_paths:
            ref_sp3_file = input[0]
            job_sp3_file = input[1]

            logging.info(f"-- {job_sp3_file} vs {ref_sp3_file}")

            diff_df = pd.DataFrame()
            try:
                diff_df = sp3_diff(ref_sp3_file, job_sp3_file)
            except Exception as error:
                logging.exception(f"Error with sp3_diff(): {error}")
                continue

            res_sp3 = pd.concat([res_sp3, diff_df])

        if not res_sp3.empty:
            res_sp3.to_csv(output_dir / f"diff_{start_yrdoy}_{session_len}D_SP3.csv")

            # Compute statistics
            logging.info("Computing statistics of SP3 differences ...")

            stats_sp3 = sp3_stats(res_sp3)
            stats_sp3.to_csv(output_dir / f"stats_{start_yrdoy}_{session_len}D_SP3.csv")

            # Plot sp3 comparison results
            logging.info("Plotting SP3 comparison results ...")

            for sys in sat_sys:
                sys_name = sys_meta[sys]["name"]
                sys_svs = svs[sys]

                # Plot epoch-by-epoch orbit differences
                logging.info(f"-- Orbit differences - {sys_name}")

                title = f"Satellite Orbit Comparison (gnssanalysis): {ssr_mountpoint} vs {ref_prefix}"
                fig = plot_orb_diff(res_sp3, sys_svs, title, start_epoch, end_epoch)
                if fig:
                    fig.savefig(
                        output_dir / f"diff_{start_yrdoy}_{session_len}D_ORB_{sys}.png",
                        bbox_inches="tight",
                    )

                # Plot epoch-by-epoch clock differences
                logging.info(f"-- Clock differences - {sys_name}")

                title = f"Satellite Clock Comparison (gnssanalysis): {ssr_mountpoint} vs {ref_prefix}"
                fig = plot_clk_diff(res_sp3, sys_svs, title, start_epoch, end_epoch)
                if fig:
                    fig.savefig(
                        output_dir
                        / f"diff_{start_yrdoy}_{session_len}D_CLK_{sys}_none.png",
                        bbox_inches="tight",
                    )

                # Plot orbit RMS during the whole session
                logging.info(f"-- Orbit RMS - {sys_name}")

                title = f"Satellite Orbit Comparison (gnssanalysis): {ssr_mountpoint} vs {ref_prefix}"
                fig = plot_orb_rms(stats_sp3, sys_svs, title)
                if fig:
                    fig.savefig(
                        output_dir / f"rms_{start_yrdoy}_{session_len}D_ORB_{sys}.png",
                        bbox_inches="tight",
                    )

                # Plot clock RMS during the whole session
                logging.info(f"-- Clock RMS - {sys_name}")

                title = f"Satellite Clock Comparison (gnssanalysis): {ssr_mountpoint} vs {ref_prefix}"
                fig = plot_clk_rms(stats_sp3, sys_svs, title)
                if fig:
                    fig.savefig(
                        output_dir
                        / f"rms_{start_yrdoy}_{session_len}D_CLK_{sys}_none.png",
                        bbox_inches="tight",
                    )

        # Compare clocks using gnssanalysis
        logging.info("Comparing CLK files ...")

        # Get input file paths
        input_paths = []
        for yrdoy in yrdoys:
            input_paths.append(
                [
                    ref_dir
                    / f"{ref_prefix}_{yrdoy}0000_01D_{sampling_rate['CLK']}_CLK.CLK",
                    job_dir / ssr_mountpoint / f"{ssr_mountpoint}_{yrdoy}0000_CLK.clk",
                ]
            )

        res_clk = pd.DataFrame()

        for input in input_paths:
            ref_clk_file = input[0]
            job_clk_file = input[1]

            logging.info(f"-- {job_clk_file} vs {ref_clk_file}")

            diff_df = pd.DataFrame()
            try:
                diff_df = clk_diff(ref_clk_file, job_clk_file, clk_norm_types)
            except Exception as error:
                logging.exception(f"Error with clk_diff(): {error}")
                continue

            res_clk = pd.concat([res_clk, diff_df])

        if not res_clk.empty:
            clk_norm_types = [
                item if isinstance(item, str) else "_".join(item)
                for item in clk_norm_types
            ]
            norm = "_".join(clk_norm_types) if clk_norm_types else "none"

            res_clk.to_csv(
                output_dir / f"diff_{start_yrdoy}_{session_len}D_CLK_{norm}.csv"
            )

            # Compute statistics
            logging.info("Computing statistics of CLK differences ...")

            stats_clk = clk_stats(res_clk)
            stats_clk.to_csv(
                output_dir / f"stats_{start_yrdoy}_{session_len}D_CLK_{norm}.csv"
            )

            # Plot clock comparison results
            logging.info("Plotting CLK comparison results ...")

            for sys in sat_sys:
                sys_name = sys_meta[sys]["name"]
                sys_svs = svs[sys]

                # Plot epoch-by-epoch clock differences
                logging.info(f"-- Clock differences - {sys_name}")

                title = f"Satellite Clock Comparison (gnssanalysis): {ssr_mountpoint} vs {ref_prefix}"
                fig = plot_clk_diff(res_clk, sys_svs, title, start_epoch, end_epoch)
                if fig:
                    fig.savefig(
                        output_dir
                        / f"diff_{start_yrdoy}_{session_len}D_CLK_{sys}_{norm}.png",
                        bbox_inches="tight",
                    )

                # Plot clock RMS during the whole session
                logging.info(f"-- Clock RMS - {sys_name}")

                title = f"Satellite Clock Comparison (gnssanalysis): {ssr_mountpoint} vs {ref_prefix}"
                fig = plot_clk_rms(stats_clk, sys_svs, title)
                if fig:
                    fig.savefig(
                        output_dir
                        / f"rms_{start_yrdoy}_{session_len}D_CLK_{sys}_{norm}.png",
                        bbox_inches="tight",
                    )

        logging.info("Orbit and clock comparison done\n")


@click.command()
@click.option(
    "--job-dir", required=True, help="Directory where data is processed", type=Path
)
@click.option(
    "--ref-dir",
    required=True,
    help="Directory where reference products are placed",
    type=Path,
)
@click.option(
    "--ref-prefix",
    required=True,
    help="Prefix of reference products, e.g. IGS0OPSRAP",
    default="IGS0OPSRAP",
    type=str,
)
@click.option(
    "--start-yrdoy",
    help="Start of date period (day-of-year) to analyse data for in the format of YYYYDDD. Current day will be used if not set",
    default=None,
    type=int,
)
@click.option(
    "--end-yrdoy",
    help="End of date period (day-of-year, inclusive) to analyse data for in the format of YYYYDDD. Do not set for real-time mode",
    default=None,
    type=int,
)
@click.option(
    "--session-len",
    required=True,
    help="Length of a session for each rotation period in days. If not set, no ratation (i.e. only one session) will be applied between start- and end-yrdoy",
    default=0,
    type=int,
)
@click.option(
    "--align-to-gps-week",
    help="Align rotation period to GPS week for weekly solutions. Only effective when session length is 7 days (weekly)",
    default=False,
    is_flag=True,
)
@click.option(
    "--sat-sys",
    required=True,
    help="Satellite system to analyse, G, R, E, C, or any combination without space",
    default="G",
    type=str,
)
@click.option(
    "--exclude",
    help="PRNs of satellites to exclude, e.g. 'G01, G03'",
    default=None,
    type=str,
)
@click.option(
    "--clk-norm-types",
    help="Normalisations to apply for clock files, e.g. 'sv', 'G01', 'epoch, daily'",
    default=None,
    type=str,
)
@click.option(
    "--rel-output-dir",
    required=True,
    help="Relative path of output directory under each sub job directory",
    default="gnssanalysis",
    type=Path,
)
@click.option("--verbose", is_flag=True)
def analyse_orbit_clock_main(
    job_dir,
    ref_dir,
    ref_prefix,
    start_yrdoy,
    end_yrdoy,
    session_len,
    align_to_gps_week,
    sat_sys,
    exclude,
    clk_norm_types,
    rel_output_dir,
    verbose,
):
    ga.gn_utils.configure_logging(verbose)

    excluded_svs = []
    if exclude:
        excluded_svs = exclude.replace(" ", "").split(",")

    if clk_norm_types:
        clk_norm_types = clk_norm_types.replace("none", "")
        clk_norm_types = clk_norm_types.replace(" ", "").split(",")
    else:
        clk_norm_types = []

    # Post-processing mode
    if end_yrdoy is not None:
        logging.info(
            "Orbit and clock analysis in post-processing mode as '--end-yrdoy' is set"
        )

        if start_yrdoy is None:
            logging.error(
                "'--start-yrdoy' must be specified for post-processing mode ('--end-yrdoy' is set)"
            )
            return
        elif start_yrdoy > end_yrdoy:
            logging.error("Start date must be no later than end date")
            return

        start_date = datetime.strptime(str(start_yrdoy), "%Y%j")
        end_date = datetime.strptime(str(end_yrdoy), "%Y%j")

        if session_len <= 0:
            session_len = (end_date - start_date).days + 1

        if align_to_gps_week and session_len == 7:
            start_date -= timedelta(
                start_date.isoweekday() % 7
            )  # round down start date to nearest Sunday (start of a GPS week)

        while start_date <= end_date:
            analyse_orbit_clock(
                job_dir,
                ref_dir,
                ref_prefix,
                start_date,
                session_len,
                sat_sys,
                excluded_svs,
                clk_norm_types,
                rel_output_dir,
            )

            start_date += timedelta(days=session_len)

    # Real-time mode
    else:
        logging.info(
            "Orbit and clock analysis in real-time mode as '--end-yrdoy' is not set"
        )

        if session_len <= 0:
            logging.error(
                "'--session-len' must be specified for real-time mode ('--end-yrdoy' is not set)"
            )
            return

        if start_yrdoy is None:
            start_date = datetime.strptime(str(date.today()), "%Y-%m-%d")
        else:
            start_date = datetime.strptime(str(start_yrdoy), "%Y%j")

        if align_to_gps_week and session_len == 7:
            start_date -= timedelta(
                start_date.isoweekday() % 7
            )  # round down start date to nearest Sunday (start of a GPS week)

        latency = (
            2 * 86400
        )  # allow 2 days of latency for reference products to be available

        while True:
            next_start_date = start_date + timedelta(days=session_len)
            end_date = next_start_date - timedelta(seconds=1)
            now = time.time()
            seconds = next_start_date.timestamp() - now + latency
            logging.debug(
                f"Start time of current session: {start_date}, end time of current session: {end_date}"
            )
            logging.debug(
                f"{seconds} seconds to wait (for reference products to be available) to start analysis"
            )

            if seconds > 0:
                time.sleep(seconds)

            analyse_orbit_clock(
                job_dir,
                ref_dir,
                ref_prefix,
                start_date,
                session_len,
                sat_sys,
                excluded_svs,
                clk_norm_types,
                rel_output_dir,
            )

            start_date = next_start_date


if __name__ == "__main__":
    analyse_orbit_clock_main()
