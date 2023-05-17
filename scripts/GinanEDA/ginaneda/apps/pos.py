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
    """_summary_
    """
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
    """_summary_
    """
    lat, lon, _ = xyz2blh(x0, y0, z0)
    lat, lon = np.radians(lat), np.radians(lon)
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
    """_summary_
    """
    return html.Div(
        [
            html.P(
                "Exclude the first points",
                style={
                    "display": "inline-block",
                    "margin-right": 5},
            ),
            dcc.Input(
                id="exclude_npt",
                value="0",
                type="text",
                size="2"),
            html.P(
                " points",
                style={
                    "display": "inline-block",
                    "margin-right": 5}),
        ])


def dropdown_site(site_list):
    """_summary_
    """
    site_list2 = list(["ALL"])
    site_list2.extend(site_list)
    p = util.named_dropdown(
        None,
        id="mes_dropdown_site",
        options=[{"label": i, "value": i} for i in site_list2],
        placeholder="Site",
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
        id="pos_dropdown_serie",
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


@app.callback(
    Output("plot3", "figure"),
    Input("update_graph", "n_clicks"),
    State("session-store", "data"),
    State("mes_dropdown_type", "value"),
    State("mes_dropdown_site", "value"),
    State("pos_dropdown_serie", "value"),
    State("checklist", "value"),
    State("exclude_npt", "value")
)
def update_graph_pos(click, data_dict, graph_type, site, serie, opt, exclude):
    """Update the new graph
    """
    try:
        exclude = int(exclude)
    except BaseException:
        exclude = 0
    if exclude < 0:
        exclude = 0
    if graph_type is None or site is None:
        return util.get_empty_graph(
            "Make sure a value for all the Dropdown Menu is selected")
    else:
        fig = go.Figure()
        trace = []
        site = [i for i in data_dict["DB_SITE"]] if "ALL" in site else site
        if "" in site:
            site.remove("")
        logger.info("Request done")
        answer = {}
        for site_, serie_ in itertools.product(site, serie):
            req = {}
            database , subseries = serie_.split("\\")
            pipeline = [
                {"$match": {"State": "REC_POS", "Site": site_, "Series": subseries}},
                {"$sort": {"Epoch": 1}},
                {
                    "$group": {
                        "_id": "$Site",
                        "Epoch": {"$push": "$Epoch"},
                        "x": {"$push": "$x"},
                    }
                }
            ]
            if "unc" in opt:
                pipeline[2]["$group"]["P"] = {"$push": "$P"}
            req[site_+'_'+serie_] = pipeline
            # if graph_type == "NEU":
            for site_ in site:
                Serie_0 = subseries.split("_")[0]+"_apriori"
                pipeline = [
                    {
                        "$match": {
                            "State": "REC_POS", "Site": site_, "Series": Serie_0}}, {
                        "$sort": {
                            "Epoch": 1}}, {
                        "$group": {
                            "_id": "$Site", "Epoch": {
                                "$push": "$Epoch"}, "x": {
                                    "$push": "$x"}, }}]
                req[site_ + '_' + serie_ + "_apriori"] = pipeline
            mongoClient = db.connect_client(data_dict['MONGO_URL'])[database]
            for cursor in mongoClient["States"].aggregate([{"$facet": req}]):
                answer.update(cursor)

        logger.info("Request done")

        for site_, serie_ in itertools.product(site, serie):
            data = answer[site_ + '_' + serie_]
            data_ap = answer [ site_ + '_' + serie_ + "_apriori"]
            time = np.array(data[0]["Epoch"])[exclude:]
            data2 = np.array(data[0]["x"])
            x = data2[exclude:, 0]
            y = data2[exclude:, 1]
            z = data2[exclude:, 2]
            
            data_ap_arr= np.array(data_ap[0]["x"])
            time_ap = np.array(data_ap[0]["Epoch"])[exclude:] 
            x_ap = data_ap_arr[exclude:, 0]
            y_ap = data_ap_arr[exclude:, 1]
            z_ap = data_ap_arr[exclude:, 2]
            resize = False
            if len(time_ap) != len(time):
                common, idx1, idx2 = np.intersect1d(time, time_ap, return_indices=True)
                time = time[idx1]
                x = x[idx1]
                y = y[idx1]
                z = z[idx1]
                time_ap = time_ap[idx2]
                x_ap = x_ap[idx2]
                y_ap = y_ap[idx2]
                z_ap = z_ap[idx2]
                resize = True
            if "unc" in opt:
                data2 = np.array(data[0]["P"])
                dx0 = data2[exclude:, 0]
                dx1 = data2[exclude:, 1]
                dx2 = data2[exclude:, 2]
                print(len(dx0))
                if resize:
                    common, idx1, idx2 = np.intersect1d(time, time_ap, return_indices=True)
                    dx0 = dx0[idx1]
                    dx1 = dx1[idx1]
                    dx2 = dx2[idx1]
                    print(len(dx0))
            else:
                dx0 = None
                dx1 = None
                dx2 = None
            if (x[0]**2+y[0]**2+z[0]**2 > 6000000**2):
                x = x - x_ap
                y = y - y_ap
                z = z - z_ap
            if graph_type == "XYZ":
                if "plan" in opt:
                    if "unc" in opt:
                        data = go.Scatter3d(
                            x=x,
                            y=y,
                            z=z,
                            marker=dict(
                                size=6,
                                color=np.sqrt(dx0 + dx1 + dx2),
                                colorscale=[(0,"green"), (0.03,"green"), (0.05,"purple"), (0.10,"orange"), (1,"red")],
                                showscale=True,
                                cmin=0,
                                cmax=1),
                                name=site_+serie_)
                    else:
                        data = go.Scatter3d(x=x, y=y, z=z, name=site_+serie_)
                    trace.append(data)
                else:
                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(
                        go.Scatter(
                            x=time,
                            y=x,
                            mode='lines',
                            line=dict(
                                color=c1),
                            name=f"{site_}-{serie_}-X",
                        ))
                    if "unc" in opt:
                        print(len(x), len(dx0))
                        trace.append(
                            go.Scatter(
                                # x, then x reversed
                                x=np.concatenate(
                                    [time, time[::-1]], axis=None),
                                y=np.concatenate(
                                    [x + np.sqrt(dx0), x[::-1] - np.sqrt(dx0[::-1])], axis=None),
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
                    trace.append(
                        go.Scatter(
                            x=time,
                            y=y,
                            mode='lines',
                            line=dict(
                                color=c1),
                            name=f"{site_}-{serie_}-Y",
                        ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                # x, then x reversed
                                x=np.concatenate(
                                    [time, time[::-1]], axis=None),
                                y=np.concatenate(
                                    [y + np.sqrt(dx1), y[::-1] - np.sqrt(dx1[::-1])], axis=None),
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
                    trace.append(
                        go.Scatter(
                            x=time,
                            y=z,
                            mode='lines',
                            line=dict(
                                color=c1),
                            name=f"{site_}-{serie_}-Z",
                        ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                # x, then x reversed
                                x=np.concatenate(
                                    [time, time[::-1]], axis=None),
                                y=np.concatenate(
                                    [z + np.sqrt(dx2), z[::-1] - np.sqrt(dx2[::-1])], axis=None),
                                # upper, then lower reversed
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )
            else:
                base = answer[f"{site_}_{serie_}_apriori"]
                data2 = np.array(base[0]["x"])
                x0 = data2[0, 0]
                y0 = data2[0, 1]
                z0 = data2[0, 2]
                lat, lon, _h = xyz2blh(x0, y0, z0)
                # using formula from
                # https://gssc.esa.int/navipedia/index.php/Positioning_Error
                lat, lon = np.radians(lat), np.radians(lon)
                rot = np.array([[-np.sin(lon), - np.cos(lon) * np.sin(lat), np.cos(lat) * np.cos(lon)],
                                [np.cos(lon), - np.sin(lat) * np.sin(lon), np.cos(lat) * np.sin(lon)],
                                [0, np.cos(lat), np.sin(lat)]
                                ])
                # north, east, up = xyz2neu(x0[0], y0[0], z0[0], x + x0[0], y + y0[0], z + z0[0])
                enu = []
                for x_, y_, z_ in zip(x,y,z):
                    if (x_**2+y_**2+z_**2 > 6000000**2):
                        x_ -= x0
                        y_ -= y0
                        z_ -= z0
                    enu.append(rot.transpose().dot(np.array([x_ , y_ , z_ ]).transpose()))
                enu = np.asarray(enu)
                north = enu[:, 1]
                east = enu[:, 0]
                up = enu[:, 2]
                if "unc" in opt:
                    denu = []
                    for i in range(len(dx0)):
                        P = np.diag([dx0[i], dx1[i], dx2[i]])
                        denu.append(
                            np.sqrt(
                                np.diag(
                                    rot.transpose().dot(P).dot(rot))))

                    denu = np.asarray(denu)
                    delta_north = denu[:, 1]
                    delta_east = denu[:, 0]
                    delta_up = denu[:, 2]
                else:
                    delta_north = None
                    delta_east = None
                    delta_up = None
                if "plan" in opt:
                    trace.append(
                        go.Scatter(
                            x=east,
                            y=north,
                            mode='lines+markers',
                            name=f"{site_}_{serie_}-N",
                        ))
                    if "unc" in opt:
                        marker = dict(
                            size=6,
                            color=np.sqrt(dx0 ** 2 + dx1 ** 2 + dx2 ** 2),
                            colorscale=[(0,"green"), (0.03,"green"), (0.05,"purple"),(0.10, "orange"),(1,"red")],
                            showscale=True,
                            cmin=0,
                            cmax=1)
                        trace.append(
                            go.Scatter(
                                x=east,
                                y=north,
                                mode='lines+markers',
                                name=f"{site_}_{serie_}-N",
                                marker=marker))

                else:
                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(
                        go.Scatter(
                            x=time,
                            y=north,
                            mode='lines',
                            line=dict(
                                color=c1),
                            name=f"{site_}_{serie_}-N",
                        ))
                    if "unc" in opt:
                        trace.append(
                            go.Scatter(
                                # x, then x reversed
                                x=np.concatenate(
                                    [time, time[::-1]], axis=None),
                                # upper, then lower reversed
                                y=np.concatenate(
                                    [north + delta_north, north[::-1] - delta_north[::-1]], axis=None),
                                fill='toself',
                                fillcolor=c2,
                                line=dict(color=c2),
                                hoverinfo="skip",
                                showlegend=True
                            )
                        )

                    c1 = next(colorcycle)
                    c2 = next(colorcycleal)
                    trace.append(
                        go.Scatter(
                            x=time,
                            y=east,
                            mode='lines',
                            line=dict(
                                color=c1),
                            name=f"{site_}_{serie_}-E",
                        ))
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
                    trace.append(
                        go.Scatter(
                            x=time,
                            y=up,
                            mode='lines',
                            line=dict(
                                color=c1),
                            name=f"{site_}_{serie_}-U",
                        ))
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
                dropdown_site(data_dict["DB_SITE"]),
                dropdown_serie(data_dict['Series']),
                dcc.Checklist(id="checklist",
                            options=[
                                {'label': "unc", "value": "unc"},
                                {'label': "plan view", "value": "plan"}],
                            value=['']

                            ),
                exclude_start(),
                update_button,
                dcc.Graph(
                    id="plot3", figure=util.get_empty_graph("select information first")
                ),
            ]
        )
    else:
        return html.Div(
        [html.P("No KF State in the DB, can't do anything on this page")]
        )
