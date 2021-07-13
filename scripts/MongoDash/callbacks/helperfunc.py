##Creatingg a RangeSlider for y axis:  The x axis rangeslider is available by default in plotly , but not the y axis.


import dash
import dash_html_components as html
import dash_core_components as dcc
import plotly.graph_objects as go
import pandas               as pd
import pdb
import pymongo
import sys
"""
TODO: Implement the range slider to select bin size for Histograms.
"""
def get_range_slider():
    pass


"""
 * @brief Returns the Graph object.
 * @param data       :          Dataframe containing the values to be plotted.
 * @param xaxis_title:          X Axis title name
 * @param yaxis_title:          Y Axis title name
 * @return Graph     :          go.Figure() object
"""
def get_figure(data,xaxis_title="", yaxis_title=""):
    fig = go.Figure(data=data)
    fig.update_layout(
        xaxis=dict(rangeslider=dict(visible=True)),
        yaxis=dict(autorange = True,fixedrange= False ),
        xaxis_title = xaxis_title,
        yaxis_title = yaxis_title
    )

    fig.layout.autosize = True

    return fig


"""
 * @brief reates a dataframe with columns x and y. It then sorts the dataframe based on values in Column x. The value in column x is generally the DateTime field.
 * @param x          :          X Axis values.
 * @param y          :          Y Axis values.
 * @return Dataframe :          Sorted Data Fame
"""
def sort_df(x,y):
    temp = pd.DataFrame({'x':x,'y':y})
    sorted_df = temp.sort_values(by="x",ascending=True)

    return sorted_df


def get_empty_graph(message):
    emptiness = {
        "layout":
        {
            "xaxis": { "visible": False  },
            "yaxis": { "visible": False  },
            "annotations":
            [{
                "text": message,
                "xref": "paper",
                "yref": "paper",
                "showarrow": False,
                "font": { "size": 28 }
            }]
        }
    }
    return emptiness


"""
 * @brief            : This is get the units of scale(Used primarily for X Axis)
 * @param attribute  : The attribute used in some computation.
 * @return unit      : Units of the attribute used in `that` computation.
"""
def get_units(attribute):
    if (attribute == "Epochs"):
        return "Epochs "

    if (attribute == "Elevation"):
        return attribute +" (deg)"

    if (attribute == "NADIR_ANG"):
        return attribute +" (deg)"

    if (attribute == "Azimuth"):
        return attribute +" (deg)"

    if (attribute == "FLOAT_AMB"):
        return attribute +" (m)"

    if (attribute == "OBS_PHASE_RESID"):
        return attribute +" (deg)"

    if (attribute == "OBS_RANGE_RESID"):
        return attribute +" (deg)"

    if (attribute =="EPOCH_NUM_SITES" or attribute =="EPOCH_NUM_SVS" or attribute =="EPOCH_NUM_OBS" or attribute =="EPOCH_NUM_PARAMS" or  attribute =="EPOCH_NUM_AMB" or
     attribute == "EPOCH_POSTFIT_CHISQR_DOF" or attribute == "EPOCH_PRETFIT_CHISQR_INC" or  attribute == "EPOCH_NUM_WL_AMB_FIXED" or  attribute == "EPOCH_NUM_NL_AMB_FIXED"):
        return attribute + ""


    else:
        return attribute + " (m)"





"""
    Helper function
"""
def satsVisibleToRec_res(rec):

    myclient        = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist          = myclient.list_database_names()
    pea             = myclient[sys.argv[1]]
    measurements    = pea["Measurements"]

    rows    = measurements.find({"Site" : rec})
    sats    = list(set([row['Sat'] for row in rows]))
    sats.sort()
    # print(sats)
    return sats

def getAllSites_res():

    myclient        = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist          = myclient.list_database_names()
    pea             = myclient[sys.argv[1]]
    measurements    = pea["Measurements"]

    rows    = measurements.find()
    sites   = list(set([row['Site'] for row in rows]))
    sites.sort()
    # print(sites)
    return sites


def satsVisibleToRec_sol(rec):

    myclient        = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist          = myclient.list_database_names()
    pea             = myclient[sys.argv[1]]
    measurements    = pea["States"]

    rows    = measurements.find({"Site" : rec})
    sats    = list(set([row['Sat'] for row in rows]))
    sats.sort()
    # print(sats)
    return sats

def getAllSites_sol():

    myclient        = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist          = myclient.list_database_names()
    pea             = myclient[sys.argv[1]]
    measurements    = pea["States"]

    rows    = measurements.find()
    sites   = list(set([row['Site'] for row in rows]))
    sites.sort()
    # print(sites)
    return sites

