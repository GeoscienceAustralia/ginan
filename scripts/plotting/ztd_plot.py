"""
Plot ZTD comparison for PEA vs Bernese vs SNX ref
"""

from io import StringIO
import logging
import boto3
import click
import numpy as np
import pandas as pd
import subprocess
import matplotlib

matplotlib.use("Agg")
from matplotlib import pyplot as plt
import sys
import matplotlib as plb

from pathlib import Path
import math
from numpy import loadtxt
from matplotlib.ticker import MultipleLocator

import argparse
import datetime as dt
from datetime import datetime, timedelta
import re

ROOT = "/data/acs/pea/output/exs/bernese_test"
GPS_ST = datetime.strptime("1980 006", "%Y %j")


def parse_rts_smoothed_file(tracefile):
    def epoch(row):
        return GPS_ST + timedelta(weeks=row["gpswk"], seconds=row["seconds"])

    with open(tracefile) as fl:
        header = "dummy1,gpswk,seconds,dummy2,dummy3,trop_value,trop_var\n"
        csv_txt = "".join(l for l in fl if l.startswith("$TROP"))
        result = pd.read_csv(StringIO(header + csv_txt))
        result = result[["gpswk", "seconds", "trop_value", "trop_var"]]
        result["datetime"] = result.apply(epoch, axis=1)
        result["trop_std"] = np.sqrt(result["trop_var"])
        return result


def bernese_timeseries(time_series_ztd_file):
    names = ["Decimal_year", "Year", "DOY", "Month", "Day", "Hour", "Minute", "ZTD", "std"]

    result = pd.read_csv(
        time_series_ztd_file,
        delim_whitespace=True,
        header=None,
        names=names,
    )
    result["ZTD"] = result["ZTD"] / 1000
    result["std"] = result["std"] / 1000

    def bernese_datetime(row):
        return datetime(
            int(row["Year"]),
            int(row["Month"]),
            int(row["Day"]),
            int(row["Hour"]),
            int(row["Minute"]),
        )

    result["datetime"] = result.apply(bernese_datetime, axis=1)
    return result


def plot_with_errors(ax, x, y, err, alpha=0.2):
    ax.fill_between(x, y - err, y + err, alpha=alpha)


def pydate(np_date):
    return np_date.dt.to_pydatetime()


def plot_separate(df_pea, df_bern, station_code):
    fig, ax = plt.subplots(2, 1, figsize=(12, 12))

    ax[0].plot(pydate(df_pea["datetime"]), df_pea["trop_value"], label="ZTD - PEA")
    plot_with_errors(ax[0], pydate(df_pea["datetime"]), df_pea["trop_value"], df_pea["trop_std"])
    ax[0].set_ylabel("ZTD (m) - PEA")
    ax[0].title.set_text(f"Zenith Tropospheric Delay - {station_code} - PEA (10-sec) vs. Bernese (hourly)")
    ax[0].grid(True)
    ax[0].legend()

    ax[1].plot(pydate(df_bern["datetime"]), df_bern["ZTD"], label="ZTD - Bernese")
    plot_with_errors(ax[1], pydate(df_bern["datetime"]), df_bern["ZTD"], df_bern["std"])
    ax[1].set_ylabel("ZTD (m) - Bernese")
    ax[1].set_xlabel(f"Epoch")
    ax[1].grid(True)

    return fig


def plot_together(df_pea, df_bern, station_code):
    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    ax.plot(pydate(df_pea["datetime"]), df_pea["trop_value"], label="ZTD - PEA")
    plot_with_errors(ax, pydate(df_pea["datetime"]), df_pea["trop_value"], df_pea["trop_std"])
    ax.plot(pydate(df_bern["datetime"]), df_bern["ZTD"], label="ZTD - Bernese")
    plot_with_errors(ax, pydate(df_bern["datetime"]), df_bern["ZTD"], df_bern["std"])

    ax.set_xlabel(f"Epoch")
    ax.set_ylabel("ZTD (m)")
    ax.title.set_text(f"Zenith Tropospheric Delay - {station_code} - PEA (10-sec) vs. Bernese (hourly)")
    ax.grid(True)
    ax.legend()

    return fig


def diff_pea_vs_bernese(df_pea, df_bern):
    # Comparison against snx reference
    df_pea = df_pea.set_index("epoch")
    df_bern = df_bern.set_index("epoch")
    df_diff = pd.concat([df_pea, df_bern], axis=1, join="inner")

    # Difference plot
    df_diff["Bernese - PEA"] = (df_diff.ZTD - df_diff.trop_value) * 1000
    df_diff["B-P std"] = np.sqrt(df_diff["std"] ** 2 + df_diff["trop_std"] ** 2)
    df_diff["Mean diff"] = df_diff["Bernese - PEA"].mean()
    df_diff["Std diff"] = df_diff["Bernese - PEA"].std()
    df_diff["RMS diff"] = np.sqrt((df_diff["Bernese - PEA"] ** 2).mean())
    return df_diff


def plot_compare(df_pea, df_bern, df_diff, station_code):
    fig, ax = plt.subplots(2, 1, figsize=(12, 12))
    ax[0].plot(pydate(df_pea["datetime"]), df_pea["trop_value"], label="ZTD - PEA")
    plot_with_errors(ax[0], pydate(df_pea["datetime"]), df_pea["trop_value"], df_pea["trop_std"])
    ax[0].plot(pydate(df_bern["datetime"]), df_bern["ZTD"], label="ZTD - Bernese")
    plot_with_errors(ax[0], pydate(df_bern["datetime"]), df_bern["ZTD"], df_bern["std"])

    ax[0].set_xlabel(f"Epoch")
    ax[0].set_ylabel("ZTD (m)")
    ax[0].title.set_text(f"Zenith Tropospheric Delay - {station_code} - PEA (10-sec) vs. Bernese (hourly)")
    ax[0].grid(True)
    ax[0].legend()

    df_diff[["Bernese - PEA"]].plot(style=".-", ax=ax[1])
    df_diff[["Mean diff"]].plot(style=".-", ax=ax[1])
    df_diff[["Std diff"]].plot(style=".-", ax=ax[1])
    df_diff[["RMS diff"]].plot(style=".-", ax=ax[1])
    plot_with_errors(ax[1], df_diff.index, df_diff["Bernese - PEA"], df_diff["B-P std"])
    ax[1].set_xlabel(f"Epoch")
    ax[1].set_ylabel("ZTD Difference (mm)")
    ax[1].title.set_text(f"Difference Plots - Bernese vs PEA Reference from CODE Analysis Center (2-hourly)")
    ax[1].grid(True)
    ax[1].legend()

    return fig


def plot_station_2(station_code, year, doy_pair, output_folder, smooth_input_folder):
    # Load Data
    start_doy, end_doy = doy_pair

    trace_file = f"{ROOT}/{smooth_input_folder}/NRT_{station_code}.rts_smoothed"

    df_bern = bernese_timeseries(f"{ROOT}/bernese_nrt/{station_code}_time_series.ztd")
    df_bern = df_bern[(df_bern["Year"] == year) & (df_bern["DOY"] >= start_doy) & (df_bern["DOY"] <= end_doy)].copy()
    df_bern["epoch"] = (df_bern["DOY"] - start_doy) * 360 * 24 + df_bern["Hour"] * 360

    df_pea = parse_rts_smoothed_file(trace_file)
    df_pea = df_pea[
        (df_pea["datetime"] >= (datetime(year, 1, 1) + timedelta(days=start_doy - 1)))
        & (df_pea["datetime"] <= (datetime(year, 1, 1) + timedelta(days=end_doy)))
    ]
    df_pea["epoch"] = list(range(len(df_pea)))
    # df_bern = pd.read_csv(f'{ROOT}/bernese_time_series/{station_code}_time_series.ztd',delim_whitespace=True,header=None)

    bucket_name = "jamiacstest"
    s3_client = boto3.client("s3")

    def save_and_upload(fig, file_name):
        fig.savefig(file_name)
        fig.clear()
        s3_client.upload_file(file_name, bucket_name, file_name[len(ROOT + "/") :])

    # separate plot
    fig = plot_separate(df_pea, df_bern, station_code)
    save_and_upload(fig, f"{ROOT}/plots/{output_folder}/{station_code}-separate.png")

    # together plot
    fig = plot_together(df_pea, df_bern, station_code)
    save_and_upload(fig, f"{ROOT}/plots/{output_folder}/{station_code}-together.png")

    # compare plot
    df_diff = diff_pea_vs_bernese(df_pea, df_bern)
    fig = plot_compare(df_pea, df_bern, df_diff, station_code)
    save_and_upload(fig, f"{ROOT}/plots/{output_folder}/{station_code}-comp.png")

    # upload csv
    file_name = f"{ROOT}/plots/{output_folder}/{station_code}-diff-stats.csv"
    stats = pd.DataFrame(
        data=[
            [
                station_code,
                df_diff["Mean diff"][0],
                df_diff["Std diff"][0],
                df_diff["RMS diff"][0],
            ]
        ],
        columns=["station", "mean_diff_in_mm", "std_diff_in_mm", "rms_diff_in_mm"],
    )
    stats.to_csv(file_name, index=False)
    s3_client.upload_file(file_name, bucket_name, file_name[len(ROOT + "/") :])


def plot_all_together(df_pea_rapid, df_pea_nrt, df_bern_rapid, df_bern_nrt, station_code):
    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    ax.plot(pydate(df_pea_rapid["datetime"]), df_pea_rapid["trop_value"], label="PEA rapid")
    plot_with_errors(ax, pydate(df_pea_rapid["datetime"]), df_pea_rapid["trop_value"], df_pea_rapid["trop_std"])
    ax.plot(pydate(df_bern_rapid["datetime"]), df_bern_rapid["ZTD"], label="Bernese rapid")
    plot_with_errors(ax, pydate(df_bern_rapid["datetime"]), df_bern_rapid["ZTD"], df_bern_rapid["std"])


    ax.plot(pydate(df_pea_nrt["datetime"]), df_pea_nrt["trop_value"], label="PEA NRT")
    plot_with_errors(ax, pydate(df_pea_nrt["datetime"]), df_pea_nrt["trop_value"], df_pea_nrt["trop_std"])
    ax.plot(pydate(df_bern_nrt["datetime"]), df_bern_nrt["ZTD"], label="Bernese NRT")
    plot_with_errors(ax, pydate(df_bern_nrt["datetime"]), df_bern_nrt["ZTD"], df_bern_nrt["std"])

    ax.set_xlabel(f"Epoch")
    ax.set_ylabel("ZTD (m)")
    ax.title.set_text(f"Zenith Tropospheric Delay - {station_code} - rapid(PEA_30s vs. Bernese_hourly NRT(PEA_10s vs. Bernese_hourly)")
    ax.grid(True)
    ax.legend()

    return fig


def plot_station_4(station_code, year, doy_pair, output_folder_all, smooth_input_folder_rapid, smooth_input_folder_nrt):
    # Load Data
    start_doy, end_doy = doy_pair

    trace_file_nrt = f"{ROOT}/{smooth_input_folder_nrt}/NRT_{station_code}.rts_smoothed"
    trace_file_rapid = f"{ROOT}/{smooth_input_folder_rapid}/PPP-rapid-{station_code}.rts_smoothed"

    df_bern_rapid = bernese_timeseries(f"{ROOT}/bernese_time_series/{station_code}_time_series.ztd")
    df_bern_rapid = df_bern_rapid[(df_bern_rapid["Year"] == year) & (df_bern_rapid["DOY"] >= start_doy) & (df_bern_rapid["DOY"] <= end_doy)].copy()
    df_bern_rapid["epoch"] = (df_bern_rapid["DOY"] - start_doy) * 360 * 24 + df_bern_rapid["Hour"] * 360


    df_bern_nrt = bernese_timeseries(f"{ROOT}/bernese_nrt/{station_code}_time_series.ztd")
    df_bern_nrt = df_bern_nrt[(df_bern_nrt["Year"] == year) & (df_bern_nrt["DOY"] >= start_doy) & (df_bern_nrt["DOY"] <= end_doy)].copy()
    df_bern_nrt["epoch"] = (df_bern_nrt["DOY"] - start_doy) * 360 * 24 + df_bern_nrt["Hour"] * 360

    df_pea_nrt = parse_rts_smoothed_file(trace_file_nrt)
    df_pea_nrt = df_pea_nrt[
        (df_pea_nrt["datetime"] >= (datetime(year, 1, 1) + timedelta(days=start_doy - 1)))
        & (df_pea_nrt["datetime"] <= (datetime(year, 1, 1) + timedelta(days=end_doy)))
    ]
    df_pea_nrt["epoch"] = list(range(len(df_pea_nrt)))

    df_pea_rapid = parse_rts_smoothed_file(trace_file_rapid)
    df_pea_rapid = df_pea_rapid[
        (df_pea_rapid["datetime"] >= (datetime(year, 1, 1) + timedelta(days=start_doy - 1)))
        & (df_pea_rapid["datetime"] <= (datetime(year, 1, 1) + timedelta(days=end_doy)))
    ]
    df_pea_rapid["epoch"] = list(range(len(df_pea_rapid)))
    
    bucket_name = "jamiacstest"
    s3_client = boto3.client("s3")

    def save_and_upload(fig, file_name):
        fig.savefig(file_name)
        fig.clear()
        s3_client.upload_file(file_name, bucket_name, file_name[len(ROOT + "/") :])

    # plot_all_together
    fig = plot_all_together(df_pea_rapid, df_pea_nrt, df_bern_rapid, df_bern_nrt, station_code)
    save_and_upload(fig, f"{ROOT}/plots/{output_folder_all}/{station_code}-all_together.png")


def decode_doy(doy):
    """ Parse string doy to start_doy, end_doy pair. """
    if "-" not in doy:
        start_doy, end_doy = doy, doy
    else:
        start_doy, end_doy = doy.split("-")

    start_doy = int(start_doy)
    end_doy = int(end_doy)

    return start_doy, end_doy


@click.group()
def cli():
    pass


@cli.command()
@click.option("--station", required=True)
@click.option("--year", type=int, required=True)
@click.option("--doy", required=True)
@click.option("--output-folder", required=True)
@click.option("--smooth-input-folder", required=True)
def t2(station, year, doy, output_folder, smooth_input_folder):
    """ Compare PEA vs Bernese (rapid or NRT). """
    plot_station_2(station, year, decode_doy(doy), output_folder, smooth_input_folder)


@cli.command()
@click.option("--station", required=True)
@click.option("--year", type=int, required=True)
@click.option("--doy", required=True)
@click.option("--output-folder", required=True)
@click.option("--smooth-input-folder-rapid", required=True)
@click.option("--smooth-input-folder-nrt", required=True)
def t4(station, year, doy, output_folder, smooth_input_folder_rapid, smooth_input_folder_nrt):
    """ Compare PEA vs Bernese (rapid and NRT both). """
    plot_station_4(station, year, decode_doy(doy), output_folder, smooth_input_folder_rapid, smooth_input_folder_nrt)


if __name__ == "__main__":
    cli()
