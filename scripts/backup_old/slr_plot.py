import numpy as np
import pandas as pd
import plotly.express as px

import os
import sys

"""
This script is used to generate plots of the slr pea output data.
NOTE: This script only supports Linux distributions and may have problems on Windows (include WSL)

To use this script you must have the following python packages installed:
 - kaleido==0.2.1
 - pandas==1.1.0
 - plotly==5.3.1

Usage:
This script accepts 6 arguments, described as follows
`python3 slr_plot.py <file_name> <sat_name> <rec_name> <x_data> <y_data>`

 - file_name:   This is the location of the csv file that holds the output of the slr pea, must have appropriate headers
 - sat_name:    This is the name of the satellite that you want to generate plots for (use "all" to generate plots for all satellites in the data)
 - rec_name:    This is the 4 letter code of the reciever station that you want to generate plots for (use "all" to generate plots for all stations)
 - x_data:      This is the column header of the output data that you want to have on the x axis (use "sequential" for sequential x-axis plots)
 - y_data:      This is the column header of the output data that you want to have on the y axis 

"""

WRITE_FILE_PATH = "plots"

def generate_plot(df, x_name, y_name, sat_name, rec_name):
    if x_name == "sequential":
        fig = px.scatter(df, x=list(range(len(df[y_name]))), y=y_name, title=f"{y_name} against {x_name} for {sat_name} and {rec_name}")
    else:
        fig = px.scatter(df, x=x_name, y=y_name, title=f"{y_name} against {x_name} for {sat_name} and {rec_name}")

    os.makedirs(WRITE_FILE_PATH, exist_ok=True)

    fig.write_image(f"{WRITE_FILE_PATH}/{sat_name}_{rec_name}_{x_name}_{y_name}.png", engine="auto")

def generate_all_in_one(df, x_name, y_name):
    if x_name == "sequential":
        fig = px.scatter(df, x=list(range(len(df[y_name]))), y=y_name, title=f"{y_name} against {x_name} for all satellites and recievers")
    else:
        fig = px.scatter(df, x=x_name, y=y_name, title=f"{y_name} against {x_name} for all satellites and recievers")
    
    os.makedirs(WRITE_FILE_PATH, exist_ok=True)

    fig.write_image(f"{WRITE_FILE_PATH}/all_{x_name}_{y_name}.png", engine="auto")


def main():

    if (len(sys.argv) != 6):
        print("ERROR: Incorrect number of arguments. \n \
        Correct usage is: python3 slr_plot.py <file_name> <sat_name> <rec_name> <x_data> <y_data>\n \
        Example: python3 slr_plot.py data.csv lageos1 YARL unix value \
        ")
        return

    # Reading in command line arguments
    file_name = sys.argv[1]
    sat_name = sys.argv[2]
    rec_name = sys.argv[3]
    x_name = sys.argv[4]
    y_name = sys.argv[5]

    df = pd.read_csv(file_name, skipinitialspace=True)

    if sat_name == "all":
        sat_list = df["sat"].unique()
    else:
        sat_list = [sat_name]

    if rec_name == "all":
        rec_list = df["rec"].unique()
    else:
        rec_list = [rec_name]


    for sat in sat_list:
        sat_df = df[df["sat"] == sat]
        for rec in rec_list:
            sat_rec_df = sat_df[sat_df["rec"] == rec]
            generate_plot(sat_rec_df, x_name, y_name, sat, rec)


if __name__ == "__main__":
    main()