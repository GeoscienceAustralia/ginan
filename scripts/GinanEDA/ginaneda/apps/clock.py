"""_summary_
"""

import logging
from itertools import cycle
import itertools 

import numpy as np
import plotly.graph_objs as go
from dash import dcc, html
from dash.dependencies import Input, Output, State

from app import app
from ginaneda.datasets import db
import ginaneda.apps.utilities as util

logger = logging.getLogger(__name__)

colorcycle = cycle([
    "rgba( 99, 110, 250, 0.8)",
    "rgba(171,  99, 250, 0.8)",
    "rgba(239,  85,  59, 0.8)",
    "rgba(255, 161,  90, 0.8)",
    "rgba( 25, 211, 243, 0.8)",
    "rgba(255, 102, 146, 0.8)",
    "rgba(182, 232, 128, 0.8)",
    "rgba(255, 151, 255, 0.8)",
    "rgba(254, 203, 82, 0.8)"])
colorcycleal = cycle([
    "rgba( 99, 110, 250, 0.1)",
    "rgba(171,  99, 250, 0.1)",
    "rgba(239,  85,  59, 0.1)",
    "rgba(255, 161,  90, 0.1)",
    "rgba( 25, 211, 243, 0.1)",
    "rgba(255, 102, 146, 0.1)",
    "rgba(182, 232, 128, 0.1)",
    "rgba(255, 151, 255, 0.1)",
    "rgba(254, 203,  82, 0.1)"])

possible_plot = ["SITE", "SAT"]
dropdown_type = html.Div(
    [
        dcc.Dropdown(
            id="cl_dropdown_type",
            options=[{"label": i, "value": i} for i in possible_plot],
            placeholder="Graph Type",
            value=None,
        )
    ],
    style={"width": "10%", "display": "inline-block"},
)



def dropdown_serie1(sim_list):
    """_summary_
    """
    simList = list(["ALL"])
    simList.extend(sim_list)
    p = util.named_dropdown(
        None,
        id="cl_dropdown_serie1",
        options=[{"label": i, "value": i} for i in simList],
        placeholder="Series",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


def dropdown_serie(sim_list):
    """_summary_
    """
    simList = list(["ALL"])
    simList.extend(sim_list)
    p = util.named_dropdown(
        None,
        id="cl_dropdown_serie",
        options=[{"label": i, "value": i} for i in simList],
        placeholder="Series",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


update_button = html.Div(
    [html.Button("update graph", id="update_graph", n_clicks=0)],
    style={"width": "5%", "display": "inline-block"},
)

def dropdown_satsys(sat_list):
    # syschar = list(['ALL'])
    syschar = ['ALL'] + list(set(s[0] for s in sat_list if len(s) > 1))
    p = util.named_dropdown(
        None,
        id="cl_dropdown_sys",
        options=[{"label": i, "value": i} for i in syschar],
        placeholder="SysChar",
        value='ALL',
        multi=False,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p

@app.callback(
    Output("plot4", "figure"),
    Input("update_graph", "n_clicks"),
    State("session-store", "data"),
    State("cl_dropdown_type", "value"),
    State("cl_dropdown_serie1", "value"),
    State("cl_dropdown_serie", "value"),
    State("cl_dropdown_sys", "value")
    # State("checklist", "value"),
    # State("exclude_npt", "value")
)
def update_graph_pos(click, data_dict, graph_type, serie, serie1, syssat):
    """Update the new graph
    """
    if graph_type is None:
        return util.get_empty_graph(
            "Make sure a value for all the Dropdown Menu is selected")
    else:
        fig = go.Figure()
        trace = []
        logger.info("Request done")
        if graph_type == "SAT":
            if syssat == "ALL":
                list_serie = data_dict['DB_SAT']#[1:]
            else:
                list_serie = []
                for sat in data_dict['DB_SAT']:
                    if len(sat) > 1 and sat[0] == syssat:
                        list_serie.append(sat)
            dd = db.get_series2(data_dict, "States", "SAT_CLOCK", [""], list_serie, serie, "Epoch", "x", x3=None)
            dd1 = db.get_series2(data_dict, "States", "SAT_CLOCK", [""], list_serie, serie1, "Epoch", "x", x3=None)
            key = "sat"
        elif graph_type == "SITE":
            list_serie = data_dict['DB_SITE']#[1:]
            dd = db.get_series2(data_dict, "States", "REC_CLOCK", list_serie, ["G--"], serie, "Epoch", "x", x3=None)
            dd1 = db.get_series2(data_dict, "States", "REC_CLOCK", list_serie, [""], serie1, "Epoch", "x", x3=None)
            key = "site"
        
        data_ =[]
        data_single = []
        label = []
        for i in list_serie[1:]:
            dev = None
            ref = None
            for s in dd:
                if dd[s]['_id'][key] == i:
                    dev  = dd[s] 
            for s2 in dd1:
                if dd1[s2]['_id'][key] == i :
                    ref = dd1[s2]
            if dev and ref:
                if ref['_id'][key] == dev['_id'][key]:
                    dev2 = np.asarray(dev['y']).flatten()#[:-1501]
                    ref2 = np.asarray(ref['y']).flatten()#[:-1500]
                    if len(dev2) != len(ref2):
                        time1 =  np.asarray(dev['x']).flatten()
                        time2 =  np.asarray(ref['x']).flatten()
                        intersect, c1, c2 = np.intersect1d(time1, time2, return_indices = True)
                        dev2 = dev2[c1]
                        ref2 = ref2[c2]

                    data_single.append(dev2 - ref2)
                    data_.append( dev2 - np.mean(dev2) - (ref2 - np.mean(ref2)))
                    label.append(dev['_id'][key])

        data_ = np.asarray(data_)
        data_single = np.asarray(data_single)
        data2 = data_ - data_.mean(axis=0)
        trace =[]
        for i in range(data2.shape[0]):
            trace.append(
                        go.Scatter(
                            # x=time,
                            y=data2[i,:],
                            mode='lines',
                            name=label[i],
                        ))
               
        fig = go.Figure(data=trace)
        fig.update_layout(
            xaxis=dict(rangeslider=dict(visible=True)),
            yaxis=dict(fixedrange=False, tickformat=".3f"),
            height=800,
        )
        fig.layout.autosize = True
        return fig


def layout(data_dict):
    """_summary_
    """
    if data_dict is None:
        return html.Div(
            [html.P("First you will need to select a DB in the Db Info menu")]
        )
    elif data_dict['STATE_DB']:
        return html.Div(
            [
                dropdown_type,
                dropdown_serie(data_dict['Series']),
                dropdown_serie1(data_dict['Series']),
                dropdown_satsys(data_dict['DB_SAT']),
                # exclude_start(),
                update_button,
                dcc.Graph(
                    id="plot4", figure=util.get_empty_graph("select information first")
                ),
            ]
        )
    else:
        return html.Div(
        [html.P("No KF State in the DB, can't do anything on this page")]
        )
