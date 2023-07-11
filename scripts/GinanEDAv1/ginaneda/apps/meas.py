"""_summary_
"""
import logging

import numpy as np
import plotly.graph_objs as go

from dash import dash_table, dcc, html
from dash.dependencies import Input, Output, State

from app import app
import ginaneda.apps.utilities as util
from ginaneda.datasets import db

logger = logging.getLogger(__name__)


def qq_plot(data, sample_size):
    """_summary_
    """
    d = (data - np.mean(data)) / np.std(data)
    qq = np.ones([sample_size, 2])
    np.random.shuffle(d)
    qq[:, 0] = np.sort(d[0:sample_size])
    qq[:, 1] = np.sort(np.random.normal(size=sample_size))
    return qq


possible_plot = ["Line", "Scatter", "Polar", "HistogramX", "HistogramY", "QQ"]
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


def check_list():
    """_summary_
    """
    return dcc.Checklist(
        id="aggregate",
        options=[
            {"label": "Aggregate (hist/stats)", "value": "agg"},
        ],
        value=[""],
    )


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
    site_list2 = ["ALL"] + site_list
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


def dropdown_sat(sat_list):
    """_summary_
    """
    sat_list2 = ["ALL"] + sat_list
    p = util.named_dropdown(
        None,
        id="mes_dropdown_sat",
        options=[{"label": i, "value": i} for i in sat_list2],
        placeholder="Sat",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


def dropdown_serie(sim_list):
    """_summary_
    """
    simList = ["ALL"] + sim_list
    p = util.named_dropdown(
        None,
        id="mes_dropdown_serie",
        options=[{"label": i, "value": i} for i in simList],
        placeholder="Series",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


def keys(name):
    """_summary_
    """
    return [{"label": i, "value": i}
            for i in name if i not in db.exclude_measurements]


def dropdown_key_y(name):
    """_summary_
    """
    p = util.named_dropdown(
        None,
        id="mes_dropdown_key_y",
        options=[i for i in keys(name)],
        placeholder="Y Axis",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


def dropdown_key_x(name):
    """_summary_
    """
    p = util.named_dropdown(
        None,
        id="mes_dropdown_key_x",
        options=[i for i in keys(name)],
        placeholder="X Axis",
        value=None,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p

def generates_statistics(label, time_serie):
    """_summary_

    Args:
        label (_type_): _description_
        time_serie (_type_): _description_

    Returns:
        _type_: _description_
    """
    a = {}
    a["ID"] = label
    a["RMS"] = np.sqrt(np.mean(np.square(time_serie)))
    a["Mean"] = np.mean(time_serie)
    a["Std"] = np.std(time_serie)
    return a
                    
update_button = html.Div(
    [html.Button("update graph", id="update_graph", n_clicks=0)],
    style={"width": "5%", "display": "inline-block"},
)


@app.callback(
    Output("plot1", "figure"),
    Output("tableDiv1", "children"),
    inputs=[Input("update_graph", "n_clicks")],
    state=[
        State("session-store", "data"),
        State("mes_dropdown_type", "value"),
        State("mes_dropdown_site", "value"),
        State("mes_dropdown_sat", "value"),
        State("mes_dropdown_key_x", "value"),
        State("mes_dropdown_key_y", "value"),
        State("mes_dropdown_serie", "value"),
        State("mes_dropdown_site", "options"),
        State("mes_dropdown_sat", "options"),
        State("mes_dropdown_serie", "options"),
        State("exclude_npt", "value"),
        State("aggregate", "value"),
    ],
)
def update_meas_graph(
        click,
        data_dict,
        graph_type,
        site,
        sat,
        xaxis,
        yaxis,
        serie,
        list_site,
        list_sat,
        list_series,
        exclude,
        aggr):
    """_summary_
    """
    try:
        exclude = int(exclude)
    except BaseException:
        exclude = 0
    table = []
    agg_y = []
    if exclude < 0:
        exclude = 0
    if (
        graph_type is None
        or site is None
        or sat is None
        or xaxis is None
        or yaxis is None
    ):
        return (util.get_empty_graph(
            "Make sure a value for all the Dropdown Menu is selected"), [], )
    else:
        fig = go.Figure()
        site = [i["value"] for i in list_site] if "ALL" in site else site
        sat = [i["value"] for i in list_sat] if "ALL" in sat else sat
        serie = [i["value"] for i in list_series] if "ALL" in serie else serie
        if "ALL" in site:
            site.remove("ALL")
        if "ALL" in sat:
            sat.remove("ALL")
        if "ALL" in serie:
            serie.remove("ALL")
        logger.debug(
            f"\n Plotting request type {graph_type} \n\t\t sat {' ,'.join(sat)} \n\t\t site {' ,'.join(site)} \n\t {xaxis} \n\t {' ,'.join(yaxis)}"
        )
        trace = []
        tab = []
        for yaxis_ in yaxis:
            logger.info("entering req")
            queryResult = db.get_series2(
                data_dict,
                "Measurements",
                None,
                site,
                sat,
                serie,
                xaxis,
                yaxis_,
                x3=None)
            logger.info("exiting req")
            if len(queryResult) == 0 :
                return (util.get_empty_graph(
                    "Nothing to plot"), [], )
            x = []
            y = []
            label = []
            for query in sorted(queryResult):
                data = queryResult[query]
                try:
                    if len(np.array(data['y'])) != 0:
                        y.append(np.array(data['y']))
                        x.append(np.array(data['x']))
                        label.append(query)
                except BaseException:
                    pass
            if graph_type == "QQ":
                if "agg" in aggr:
                    _y = []
                    for x_, y_, label_ in zip(x, y, label):
                        _y.append(y_[exclude:])  # .ravel()
                    _y = np.concatenate(_y, axis=0)
                    qqplot_data = qq_plot(_y, len(_y))

                    trace.append(
                        util.generate_trace(
                            "Scatter", qqplot_data[:, 0], qqplot_data[:, 1], "Agg"
                        )
                    )
                    trace.append(
                        util.generate_trace(
                            "Line",
                            [qqplot_data[0, 0], qqplot_data[-1, 0]],
                            [qqplot_data[0, 0], qqplot_data[-1, 0]],
                            "Agg",
                        )
                    )
                else:
                    for x_, y_, label_ in zip(x, y, label):
                        qqplot_data = qq_plot(y_[exclude:], len(y_[exclude:]))
                        trace.append(
                            util.generate_trace(
                                "Scatter",
                                qqplot_data[:, 0],
                                qqplot_data[:, 1],
                                f"{label_}",
                            )
                        )
            elif "agg" in aggr and graph_type in ["HistogramX", "HistogramY"]:
                _y = []
                for y_ in y:
                    _y.append(y_[exclude:])  # .ravel()
                _y = np.concatenate(_y, axis=0)
                trace.append(
                    util.generate_trace(
                        graph_type, x_[exclude:], _y, f"{yaxis_}"))
                table.append(generates_statistics(yaxis_, _y))

            else:
                for x_, y_, label_ in zip(x, y, label):
                    _y = y_[exclude:]
                    _x = x_[exclude:]
                    if len(yaxis) != 1:
                        label.append(yaxis_)
                    logger.debug("Generating 1 trace")
                    trace.append(
                        util.generate_trace(graph_type, _x, _y, label_, yaxis_)
                    )
                    logger.debug("Generating 1 trace done")
                    if "agg" not in aggr:
                        if np.issubdtype(_y.dtype, np.float_):
                            table.append(generates_statistics(label_, _y))
                    else:
                        if np.issubdtype(_y.dtype, np.float_):
                            agg_y.append(_y)
                if "agg" in aggr:
                    _y = np.concatenate(agg_y, axis=0)
                    table.append(generates_statistics(yaxis_, _y))

        fig = go.Figure(data=trace)
        fig.update_layout(showlegend=True)
        if len(table) != 0:
            tab = [
                dash_table.DataTable(
                    id="table",
                    columns=[{"name": i, "id": i} for i in [key for key in table[0].keys()]],
                    data=table,
                )
            ]
        else:
            tab = []
        if graph_type == "QQ":
            fig.update_layout(
                xaxis=dict(
                    rangeslider=dict(
                        visible=True),
                    scaleanchor="y",
                    scaleratio=1),
                yaxis=dict(
                    fixedrange=False,
                    tickformat=".3f"),
                height=700,
                width=700,
            )
        else:
            fig.update_layout(
                xaxis=dict(rangeslider=dict(visible=True)),
                yaxis=dict(fixedrange=False, tickformat=".3f"),
                height=600,
            )
        fig.layout.autosize = True
        if graph_type == "Polar":
            if yaxis == "Elevation":
                fig.update_polars(radialaxis_range=[90, 0])
            if xaxis == "Azimuth":
                fig.update_polars(angularaxis_direction="clockwise")
    return fig, tab


def layout(data_dict):
    """_summary_
    """
    if data_dict is None:
        return html.Div(
            [html.P("First you will need to select a DB in the Db Info menu")]
        )
    elif data_dict["MEAS_DB"]:
        dropd = sorted(data_dict['Geom'] + data_dict['DB_MEAS_KEY'])
        return html.Div([dropdown_type,
                         dropdown_site(data_dict['DB_SITE']),
                         dropdown_sat(data_dict['DB_SAT']),
                         dropdown_serie(data_dict['Series']),
                         dropdown_key_x(dropd),
                         dropdown_key_y(dropd),
                         exclude_start(),
                         check_list(),
                         update_button,
                         dcc.Graph(id="plot1",
                                   figure=util.get_empty_graph("select information first")),
                         html.Div(id="tableDiv1",
                                  className="tableDiv"),
                         ])
    else:
        return html.Div(
            [html.P("No measurements collection in the DB can't do anything on this page")]
        )
