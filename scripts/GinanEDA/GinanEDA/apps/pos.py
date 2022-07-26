import logging
from itertools import cycle
import GinanEDA.apps.utilities as util
import numpy as np
import plotly.express as px

# Imports
import plotly.graph_objs as go
from app import app
from dash import dash_table, dcc, html
from dash.dependencies import Input, Output, State
from GinanEDA.datasets import db
from scipy.stats import normaltest, probplot
from statsmodels.graphics.gofplots import qqplot

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
    "rgba(254, 203, 82, 0.1)"])

possible_plot = ["XYZ", "NEU"]
dropdown_type = html.Div(
    [
        dcc.Dropdown(
            id="mes_dropdown_type",
            options=[{"label": i, "value": i} for i in possible_plot],
            placeholder="Graph Type",
            value=None,
        )
    ],
    style={"width": "10%", "display": "inline-block"},
)


def xyz2blh(x, y, z):
    A = 6378137.0
    B = 6356752.314245
    e = np.sqrt(1 - (B ** 2) / (A ** 2))
    # calculate longitude, in radians
    longitude = np.arctan2(y, x)
    # calculate latitude, in radians
    xy_hypot = np.hypot(x, y)
    lat0 = 0
    latitude = np.arctan(z / xy_hypot)
    while abs(latitude - lat0) > 1e-9:
        lat0 = latitude
        N = A / np.sqrt(1 - e ** 2 * np.sin(lat0) ** 2)
        latitude = np.arctan((z + e ** 2 * N * np.sin(lat0)) / xy_hypot)
    # calculate height, in meters
    N = A / np.sqrt(1 - e ** 2 * np.sin(latitude) ** 2)
    if abs(latitude) < np.pi / 4:
        R, phi = np.hypot(xy_hypot, z), np.arctan(z / xy_hypot)
        height = R * np.cos(phi) / np.cos(latitude) - N
    else:
        height = z / np.sin(latitude) - N * (1 - e ** 2)
    # convert angle unit to degrees
    longitude = np.degrees(longitude)
    latitude = np.degrees(latitude)

    return latitude, longitude, height


def xyz2neu(x0, y0, z0, x, y, z):
    A = 6378137.0
    E2 = 0.00669438
    E4 = 0.0067394968
    B = 0.1
    lat, lon, _ = xyz2blh(x0, y0, z0)
    # convert angle unit to radians
    lat, lon = np.radians(lat), np.radians(lon)
    # calculate NEU
    north = (
            -np.sin(lat) * np.cos(lon) * (x - x0)
            - np.sin(lat) * np.sin(lon) * (y - y0)
            + np.cos(lat) * (z - z0)
    )
    east = -np.sin(lon) * (x - x0) + np.cos(lon) * (y - y0)
    up = (
            np.cos(lat) * np.cos(lon) * (x - x0)
            + np.cos(lat) * np.sin(lon) * (y - y0)
            + np.sin(lat) * (z - z0)
    )

    return north, east, up


def exclude_start():
    return html.Div(
        [
            html.P(
                "Exclude the first points",
                style={"display": "inline-block", "margin-right": 5},
            ),
            dcc.Input(id="exclude_npt", value="0", type="text", size="2"),
            html.P(" points", style={"display": "inline-block", "margin-right": 5}),
        ]
    )


def dropdown_site(site_list):
    site_list2 = list(["ALL"])
    site_list2.extend(site_list)
    p = util.namedDropdown(
        None,
        id="mes_dropdown_site",
        options=[{"label": i, "value": i} for i in site_list2],
        placeholder="Site",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


update_button = html.Div(
    [html.Button("update graph", id="update_graph", n_clicks=0)],
    style={"width": "5%", "display": "inline-block"},
)


def get_empty_graph(message):
    emptiness = {
        "layout": {
            "xaxis": {"visible": False},
            "yaxis": {"visible": False},
            "annotations": [
                {
                    "text": message,
                    "xref": "paper",
                    "yref": "paper",
                    "showarrow": False,
                    "font": {"size": 28},
                }
            ],
        }
    }
    return emptiness


def generate_trace(graph_type, x, y, label=None, error_x=None, error_y=None):
    if graph_type == "Line" or graph_type == "Scatter":
        if graph_type == "Line":
            mode = "lines"
        else:
            mode = "markers"
        trace = go.Scatter(x=x, y=y, mode=mode, name=label, error_x=error_x, error_y=error_y)
    elif graph_type == "Polar":
        trace = go.Scatterpolar(r=y, theta=x, mode="markers", name=label)
    elif graph_type == "HistogramX":
        trace = go.Histogram(x=y, name=label)
    elif graph_type == "HistogramY":
        trace = go.Histogram(y=y, name=label)
    elif graph_type == "QQ":
        trace = go.Scatter(x=x, y=y, mode="Scatter")
    return trace


@app.callback(
    Output("plot3", "figure"),
    Input("update_graph", "n_clicks"),
    State("session-store", "data"),
    State("mes_dropdown_type", "value"),
    State("mes_dropdown_site", "value"),
    State("checklist", "value"),
    State("exclude_npt", "value")
)
def update_graph_measurements(click, data_dict, graph_type, site, opt, exclude):
    try:
        exclude = int(exclude)
    except:
        exclude = 0
    if exclude < 0:
        exclude = 0
    if graph_type is None or site is None:
        return get_empty_graph(
            "Make sure a value for all the Dropdown Menu is selected"
        )
    else:
        fig = go.Figure()
        trace = []
        for st in data_dict['DB_STATE_DIC']:
            if st["_id"] == "REC_POS":
                break
        l_site = [i for i in st["Site"] if i[-2:] != "_0"]
        site = [i for i in l_site] if "ALL" in site else site
        logger.info("Request done")
        req = {}
        for site_ in site:
            pipeline = [
                {"$match": {"State": "REC_POS", "Site": site_}},
                {"$sort": {"Epoch": 1}},
                {
                    "$group": {
                        "_id": "$Site",
                        "Epoch": {"$push": "$Epoch"},
                        "x0": {"$push": "$x0"},
                        "x1": {"$push": "$x1"},
                        "x2": {"$push": "$x2"},
                    }
                }
            ]
            if "unc" in opt:
                pipeline[2]["$group"]["P0"] = {"$push": "$P0"}
                pipeline[2]["$group"]["P1"] = {"$push": "$P1"}
                pipeline[2]["$group"]["P2"] = {"$push": "$P2"}
            req[site_] = pipeline
        if graph_type == "NEU":
            for st in data_dict['DB_STATE_DIC']:
                if st["_id"] == "REC_POS":
                    break
            l_site = [i for i in st["Site"] if i[-2:] == "_0"]
            for site_ in site:
                for l in l_site:
                    if l.split("_")[0] == site_.split("_")[0]:
                        pipeline = [
                            {"$match": {"State": "REC_POS", "Site": l}},
                            {"$sort": {"Epoch": 1}},
                            {
                                "$group": {
                                    "_id": "$Site",
                                    "Epoch": {"$push": "$Epoch"},
                                    "x0": {"$push": "$x0"},
                                    "x1": {"$push": "$x1"},
                                    "x2": {"$push": "$x2"},
                                }
                            }
                        ]
                        req[l.split("_")[0]] = pipeline
        MONGO_CL = db.connect_client(data_dict['MONGO_URL'])[data_dict['MONGO_DB']]
        answer = list(MONGO_CL["States"].aggregate([{"$facet": req}]))[0]
        logger.info("Request done")

        for site_ in site:
            print(site_)
            data = answer[site_]
            print(data)
            # print(data)
            time = np.array(data[0]["Epoch"])[exclude:]
            x = np.array(data[0]["x0"])[exclude:]
            y = np.array(data[0]["x1"])[exclude:]
            z = np.array(data[0]["x2"])[exclude:]
            if "unc" in opt:
                dx0 = (np.array(data[0]["P0"]))[exclude:]
                dx1 = (np.array(data[0]["P1"]))[exclude:]
                dx2 = (np.array(data[0]["P2"]))[exclude:]
            else:
                dx0 = None
                dx1 = None
                dx2 = None
            if graph_type == "XYZ":
                if "plan" in opt:
                    if "unc" in opt:
                        data = go.Scatter3d(x=x, y=y, z=z, \
                                            marker=dict(
                                                size=6,
                                                color=np.sqrt(dx0  + dx1  + dx2),
                                                colorscale=[(0, "green"), (0.03, "green"), (0.05, "purple"),
                                                            (0.10, "orange"), (1, "red")],
                                                showscale=True,
                                                cmin=0,
                                                cmax=1
                                            ),
                                            name=site_)
                    else:
                        data = go.Scatter3d(x=x, y=y, z=z, name=site_)
                    trace.append(data)
                else:
                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(go.Scatter(x=time, y=x, mode='lines', line=dict(color=c1),
                                            name=f"{site_}-X", ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                x=np.concatenate([time, time[::-1]], axis=None),  # x, then x reversed
                                y=np.concatenate([x + np.sqrt(dx0), x[::-1] - np.sqrt(dx0[::-1])], axis=None),
                                # upper, then lower reversed
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )

                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(go.Scatter(x=time, y=y, mode='lines', line=dict(color=c1),
                                            name=f"{site_}-Y", ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                x=np.concatenate([time, time[::-1]], axis=None),  # x, then x reversed
                                y=np.concatenate([y + np.sqrt(dx1), y[::-1] - np.sqrt(dx1[::-1])], axis=None),
                                # upper, then lower reversed
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )

                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(go.Scatter(x=time, y=z, mode='lines', line=dict(color=c1),
                                            name=f"{site_}-Z", ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                x=np.concatenate([time, time[::-1]], axis=None),  # x, then x reversed
                                y=np.concatenate([z + np.sqrt(dx2), z[::-1] - np.sqrt(dx2[::-1])], axis=None),
                                # upper, then lower reversed
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )
            else:
                base = answer[site_.split('_')[0]]
                # print(base)
                # print(data)
                # time = np.array(base[0]["Epoch"])
                x0 = np.array(base[0]["x0"])[0]
                y0 = np.array(base[0]["x1"])[0]
                z0 = np.array(base[0]["x2"])[0]

                lat, lon, h = xyz2blh(x0, y0, z0)
                #using formula from https://gssc.esa.int/navipedia/index.php/Positioning_Error
                lat, lon = np.radians(lat), np.radians(lon)
                rot = np.array([[-np.sin(lon) , - np.cos(lon)*np.sin(lat), np.cos(lat) * np.cos(lon)],
                [ np.cos(lon) , - np.sin(lat)*np.sin(lon), np.cos(lat) * np.sin(lon)],
                [ 0           ,   np.cos(lat)            , np.sin(lat) ]
                ])
                # north, east, up = xyz2neu(x0[0], y0[0], z0[0], x + x0[0], y + y0[0], z + z0[0])
                enu = []
                for i in range(len(x)):
                    enu.append(rot.transpose().dot(np.array([x[i],y[i],z[i]]).transpose()))
                enu  = np.asarray(enu)
                north = enu[:,1]
                east = enu[:,0]
                up = enu[:,2]
                if "unc" in opt:
                    denu = []
                    for i in range(len(dx0)):
                        P = np.diag([dx0[i],dx1[i],dx2[i]])
                        denu.append(np.sqrt(np.diag(rot.transpose().dot(P).dot(rot))))

                    denu  = np.asarray(denu)
                    delta_north = denu[:,1]
                    delta_east = denu[:,0]
                    delta_up = denu[:,2]
                else:
                    delta_north = None
                    delta_east = None
                    delta_up = None
                if "plan" in opt:
                    trace.append(go.Scatter(x=east, y=north, mode='lines+markers', name=f"{site_}-N", ))
                    if "unc" in opt:
                        marker = dict(size=6,
                                      color=np.sqrt(dx0 ** 2 + dx1 ** 2 + dx2 ** 2),
                                      colorscale=[(0, "green"), (0.03, "green"), (0.05, "purple"), (0.10, "orange"),
                                                  (1, "red")],
                                      showscale=True,
                                      cmin=0,
                                      cmax=1
                                      )
                        trace.append(go.Scatter(x=east, y=north, mode='lines+markers', name=f"{site_}-N", marker=marker))

                else:
                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(go.Scatter(x=time, y=north, mode='lines', line=dict(color=c1),
                                            name=f"{site_}-N", ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                x=np.concatenate([time, time[::-1]], axis=None),  # x, then x reversed
                                y=np.concatenate([north + delta_north, north[::-1] - delta_north[::-1]], axis=None),  # upper, then lower reversed
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )

                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(go.Scatter(x=time, y=east, mode='lines', line=dict(color=c1),
                                            name=f"{site_}-E", ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                x=np.concatenate([time, time[::-1]], axis=None),
                                y=np.concatenate([east + delta_east, east[::-1] - delta_east[::-1]], axis=None),
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )

                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(go.Scatter(x=time, y=up, mode='lines', line=dict(color=c1),
                                            name=f"{site_}-U", ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                x=np.concatenate([time, time[::-1]], axis=None),
                                y=np.concatenate([up + delta_up, up[::-1] - delta_up[::-1]], axis=None),
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )
    logger.info("PLOT done")
    fig = go.Figure(data=trace)
    fig.update_layout(
        xaxis=dict(rangeslider=dict(visible=True)),
        yaxis=dict(fixedrange=False, tickformat=".3f"),
        height=800,
    )
    fig.layout.autosize = True
    return fig


def layout(data_dict):
    if data_dict is None:
        return html.Div(
            [html.P("First you will need to select a DB in the Db Info menu")]
        )
    else:
        for st in data_dict['DB_STATE_DIC']:
            if st["_id"] == "REC_POS":
                break
        l_site = [i for i in st["Site"] if i[-2:] != "_0"]
        return html.Div(
            [
                dropdown_type,
                dropdown_site(l_site),
                dcc.Checklist(id="checklist",
                              options=[
                                  {'label': "unc", "value": "unc"},
                                  {'label': "plan view", "value": "plan"}],
                              value=['']

                              ),
                exclude_start(),
                update_button,
                dcc.Graph(
                    id="plot3", figure=get_empty_graph("select information first")
                ),
            ]
        )
