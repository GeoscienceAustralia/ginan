import argparse
from io import BytesIO as _BytesIO
from pathlib import Path
import numpy as _np
import pandas as _pd
from urllib import request as _rqs

from datetime import datetime
from scipy.interpolate import InterpolatedUnivariateSpline

from gn_lib.gn_io.common import path2bytes
from gn_lib.gn_datetime import gpsweekD


def extrap_df_data(df, column_list=["LOD", "LODsig"], order=2):
    """
    Extrapolate data from ERP-like structured dataframe
    """
    for para in column_list:

        xs = df[para][~df[para].isna()].index.values
        ys = df[para][~df[para].isna()].values

        s = InterpolatedUnivariateSpline(xs, ys, k=order)
        x_fill = df[para][df[para].isna()].index.values

        fill_dict = {k: v for k, v in zip(x_fill, s(x_fill))}
        df.loc[:, para] = df[para].fillna(value=fill_dict)


def mjd_convert(dt):
    """Convert datetime dt to the corresponding MJD
    51544.00 == 2000-01-01 00:00:00"""
    st_dt = datetime(2000, 1, 1)
    return 51544.00 + (dt - st_dt).days + (dt - st_dt).seconds / 86400


def erp_outfile(datetime_epoch: datetime, output_dir: Path):
    """
    Input datetime string of format "YY-MM-DD hh:mm:ss"
    """
    mjd = mjd_convert(datetime_epoch)

    if Path("finals.daily.iau2000.txt").is_file():
        Path("finals.daily.iau2000.txt").unlink()
    iers_url = "https://datacenter.iers.org/data/latestVersion/finals.daily.iau2000.txt"
    iau2000_daily_file = Path.cwd() / "finals.daily.iau2000.txt"
    _rqs.urlretrieve(iers_url, filename=iau2000_daily_file)
    byte_file = path2bytes(str(iau2000_daily_file))

    iers_df = _pd.read_fwf(
        _BytesIO(byte_file),
        widths=[2, 2, 2, 9, 3, 9, 9, 10, 9, 3, 10, 10, 8, 7, 7, 6, 9, 10, 9, 10, 10, 11, 10, 10],
        usecols=[3, 5, 6, 7, 8, 10, 11, 12, 13] + list(range(15, 19)),
        header=None,
        dtype=float,
    )
    iau2000_daily_file.unlink()

    cols = [
        "MJD",
        "Xpole",
        "Xsig",
        "Ypole",
        "Ysig",
        "UT1-UTC",
        "UTsig",
        "LOD",
        "LODsig",
        "Xrt",
        "Xrtsig",
        "Yrt",
        "Yrtsig",
    ]
    iers_df.columns = cols

    erp_df = iers_df[(iers_df["MJD"] > mjd - 10) & (iers_df["MJD"] < mjd + 3.1)]

    erp_df.loc[:, "Xpole"] = erp_df.loc[:, "Xpole"] * 10 ** 6
    erp_df.loc[:, "Xsig"] = erp_df.loc[:, "Xsig"] * 10 ** 6

    erp_df.loc[:, "Ypole"] = erp_df.loc[:, "Ypole"] * 10 ** 6
    erp_df.loc[:, "Ysig"] = erp_df.loc[:, "Ysig"] * 10 ** 6

    erp_df.loc[:, "UT1-UTC"] = erp_df.loc[:, "UT1-UTC"] * 10 ** 7
    erp_df.loc[:, "UTsig"] = erp_df.loc[:, "UTsig"] * 10 ** 7

    erp_df.loc[:, "Xrt"] = erp_df.loc[:, "Xrt"] * 10 ** 3
    erp_df.loc[:, "Xrtsig"] = erp_df.loc[:, "Xrtsig"] * 10 ** 3

    erp_df.loc[:, "Yrt"] = erp_df.loc[:, "Yrt"] * 10 ** 3
    erp_df.loc[:, "Yrtsig"] = erp_df.loc[:, "Yrtsig"] * 10 ** 3

    erp_df.loc[:, "LOD"] = erp_df.loc[:, "LOD"] * 10 ** 4
    erp_df.loc[:, "LODsig"] = erp_df.loc[:, "LODsig"] * 10 ** 4

    days = erp_df["MJD"].values
    erp_df = erp_df.set_index("MJD")
    ndf = _pd.DataFrame(index=_np.arange(start=days[0], stop=days[-1] + 1, step=0.25))
    ndf.index.name = "MJD"
    edf = ndf.merge(erp_df, left_index=True, right_index=True, how="outer").interpolate(limit_area="inside")
    extrap_df_data(edf, column_list=["LOD", "LODsig"])
    edf = edf.reset_index()
    edf = edf.dropna()

    cols_order = [
        "MJD",
        "Xpole",
        "Ypole",
        "UT1-UTC",
        "LOD",
        "Xsig",
        "Ysig",
        "UTsig",
        "LODsig",
        "Xrt",
        "Yrt",
        "Xrtsig",
        "Yrtsig",
    ]

    erp_out = edf[cols_order]
    erp_out.insert(loc=9, column="Nt", value=_np.zeros(len(edf)))
    erp_out.insert(loc=9, column="Nf", value=_np.zeros(len(edf)))
    erp_out.insert(loc=9, column="Nr", value=_np.zeros(len(edf)))
    erp_out = erp_out[erp_out["MJD"] > mjd - 3]
    out_vals = erp_out[erp_out["MJD"].apply(str).str.endswith("." + str(int(str(mjd + 0.5).split(".")[1])))].values

    # Write file out, with template header of IGU format
    template = [
        "version 2\n",
        "Source: Xpole,Ypole,Xrt,Yrt,LOD: weighted average of centres;\n",
        "        UT1-UTC: integrated from the 5th day prior to Bull. A\n",
        "                 last non-predicted value.\n",
        "\n",
        "Orbits: to be used with the IGS Ultra Rapid Orbits (IGU)\n",
        "\n",
        "  MJD      Xpole   Ypole  UT1-UTC    LOD  Xsig  Ysig   UTsig  LODsig  Nr Nf Nt    Xrt    Yrt  Xrtsig  Yrtsig\n",
        '             (10**-6")       (0.1 usec)    (10**-6")     (0.1 usec)              (10**-6"/d)    (10**-6"/d)\n',
    ]

    for row in out_vals:
        temp_row = f"{row[0]:.02f}{row[1].astype(int):8}{row[2].astype(int):8}{row[3].astype(int):9}{row[4].astype(int):7}{row[5].astype(int):6}{row[6].astype(int):6}{row[7].astype(int):8}{row[8].astype(int):8}{row[9].astype(int):4}{row[10].astype(int):3}{row[11].astype(int):3}{row[12].astype(int):7}{row[13].astype(int):7}{row[14].astype(int):7}{row[15].astype(int):7}\n"
        template += [temp_row]

    gps_date = gpsweekD(datetime_epoch.strftime("%Y"), datetime_epoch.strftime("%j"), wkday_suff=True)
    file_suffix = f'_{int(int(str(mjd).split(".")[1].ljust(2,"0"))*0.24):02}'
    file_name = f"igu{gps_date}{file_suffix}.erp"

    with open(output_dir / file_name, "w") as out_file:
        out = out_file.writelines(template)
        print(out)


if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(description="Create an EPR file based on IERS daily data")

    # Command line function arguments
    parser.add_argument(
        "datetime_string",
        help="""
        DateTime string of format: 
        'YYYY-MM-DD hh:mm:ss'.
        
        Pass argument in brackets: 
        e.g. create_erp_file.py "2021-04-17 00:00:00"
        
        At the moment time must be passed as 00:00:00
        """,
    )

    parser.add_argument("-file_suff", "--file_suff", help="Change filename suffix. Default: '_12' ")

    args = parser.parse_args()
    dt_str = args.datetime_string
    f_suff = args.file_suff

    if f_suff:
        erp_outfile(dt_str, file_suffix=f_suff)
    else:
        erp_outfile(dt_str, file_suffix="_12")
