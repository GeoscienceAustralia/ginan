"""_summary_
"""
import numpy as np
import plotly.express as px
import plotly.graph_objs as go

from dash import dcc, html
from dash.dependencies import Input, Output, State

from app import app
from ginaneda.datasets import db
import ginaneda.apps.utilities as util

possible_plot = ["Line", "Scatter"]
dropdown_type = html.Div(
    [
        dcc.Dropdown(
            id="mespolar_dropdown_type",
            options=[{"label": i, "value": i} for i in possible_plot],
            placeholder="Graph Type",
            value=None,
        )
    ],
    style={"width": "10%", "display": "inline-block"},
)

colorscales = px.colors.named_colorscales()


def dropdown_site(site_list):
    """_summary_
    """
    site_list2 = list(["ALL"])
    site_list2.extend(site_list)
    return html.Div(
        [
            dcc.Dropdown(
                id="mespolar_dropdown_site",
                options=[{"label": i, "value": i} for i in site_list2],
                placeholder="Site",
                value=None,
            )
        ],
        style={"width": "20%", "display": "inline-block"},
    )


def dropdown_sat(sat_list):
    """_summary_
    """
    sat_list2 = list(["ALL"])
    sat_list2.extend(sat_list)
    return html.Div(
        [
            dcc.Dropdown(
                id="mespolar_dropdown_sat",
                options=[{"label": i, "value": i} for i in sat_list2],
                placeholder="Sat",
                value=None,
                multi=True
            )
        ],
        style={"width": "20%", "display": "inline-block"},
    )


def dropdown_serie(sim_list):
    """_summary_
    """
    simList = list(["ALL"])
    simList.extend(sim_list)
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


def keys(data):
    """_summary_
    """
    # a = db.mongo_cl["Measurements"].find_one()
    return [{"label": i, "value": i}
            for i in data if i not in db.exclude_measurements]


def dropdown_key_y(data):
    """_summary_
    """
    return html.Div(
        [
            dcc.Dropdown(
                id="mespolar_dropdown_key_y",
                options=[i for i in keys(data)],
                placeholder="Y axis",
                value=None,
            )
        ],
        style={"width": "20%", "display": "inline-block"},
    )


def dropdown_key_x(data):
    """_summary_
    """
    return html.Div(
        [
            dcc.Dropdown(
                id="mespolar_dropdown_key_C",
                options=[i for i in keys(data)],
                placeholder="C Axis",
                value=None,
            )
        ],
        style={"width": "20%", "display": "inline-block"},
    )


update_button = html.Div(
    [html.Button("update", id="update_graph_measP", n_clicks=0)],
    style={"width": "5%", "display": "inline-block"},
)


@app.callback(
    Output("plot_polar", "figure"),
    inputs=[Input("update_graph_measP", "n_clicks")],
    state=[
        State("session-store", "data"),
        State("mespolar_dropdown_site", "value"),
        State("mespolar_dropdown_sat", "value"),
        State("mes_dropdown_serie", "value"),
        State("mespolar_dropdown_key_C", "value"),
        State("mespolar_dropdown_site", "options"),
        State("mespolar_dropdown_sat", "options"),
        State("symbol_size", "value"),
        State("input_min", "value"),
        State("input_max", "value"),
        State("exclude_npt", "value"),
        State("colorscale", "value"),
    ],
)
def update_graph_measurements(
    _click,
    data_dict,
    site,
    sat,
    serie,
    caxis,
    list_site,
    list_sat,
    sym_size,
    input_min,
    input_max,
    exclude,
    cmap,
):
    """_summary_
    """
    try:
        exclude = int(exclude)
    except BaseException:
        exclude = 0

    if exclude < 0:
        exclude = 0
    fig = go.Figure()
    if site is None or sat is None or caxis is None:
        return util.get_empty_graph(
            "Make sure a value for all the Dropdown Menu is selected"
        )
    else:

        site = [i["value"] for i in list_site] if site == "ALL" else [site]
        sat = [i["value"] for i in list_sat] if "ALL" in sat else sat
        queryResult = db.get_series2(
            data_dict,
            "Measurements",
            None,
            site,
            sat,
            serie,
            "Azimuth",
            "Elevation",
            x3=caxis)
        trace = []
        min_ = 0
        max_ = 0
        for lab in sorted(queryResult):
            d = queryResult[lab]
            z = np.asarray(d["z"])
            if len(z) > exclude:
                min_ = min(min_, z[exclude:].min())
                max_ = max(max_, z[exclude:].max())
        min_ = np.floor(min_)
        max_ = np.ceil(max_)
        if input_min is not None:
            min_ = input_min
        if input_max is not None:
            max_ = input_max
        for lab in sorted(queryResult):
            d = queryResult[lab]
            if len(np.asarray(d["x"])) == 0:
                continue
            _x = np.asarray(d["x"])[exclude:]
            _y = np.asarray(d["y"])[exclude:]
            _z = np.asarray(d["z"])[exclude:]
            trace.append(
                go.Scatterpolar(
                    r=_y,
                    theta=_x,
                    mode="markers",
                    marker=dict(
                        size=sym_size,
                        colorscale=cmap,
                        color=_z,
                        showscale=True,
                        cmin=min_,
                        cmax=max_,
                    ),
                    name='_'.join(map(str, d['_id'].values())),
                    hovertemplate="r:%{r:.3f} <br>theta:%{theta:.3f}<br>c: %{marker.color:.3f} ",
                )
            )
        fig = go.Figure(data=trace)
        fig.update_layout(
            # xaxis={"rangeslider": {"visible": True}},
            # yaxis={"rangeslider": {"visible": False}},
            height=600,
            coloraxis_colorbar_x=-1.0,
            polar=dict(
                radialaxis_tickfont_size=8,
                angularaxis=dict(
                    tickfont_size=8,
                    rotation=90,
                    direction="clockwise"),
                radialaxis=dict(
                    range=[
                        90,
                        0]),
            ),
            legend=dict(
                yanchor="top",
                y=0.99,
                xanchor="left",
                x=0.85),
        )
    fig.layout.autosize = True
    return fig


def exclude_start():
    """Create a box to exclude the first n points
    """
    return html.Div(
        [
            html.P("Exclude the first points", style={"margin-left": "3px"}),
            dcc.Input(id="exclude_npt", value="0", type="text", size="2")
            # html.P(' points',style={'display':'inline-block','margin-right':5}),
        ],
        style={"width": "15%", "display": "inline-block"},
    )


def layout(data_dict):
    """_summary_
    """
    if data_dict is None:
        return html.Div(
            [html.P("First you will need to select a DB in the Db Info menu")]
        )
    elif data_dict["MEAS_DB"]:
        return html.Div(
            [
                dropdown_site(data_dict['DB_SITE']),
                dropdown_sat(data_dict['DB_SAT']),
                dropdown_serie(data_dict['Series']),
                dropdown_key_x(data_dict['DB_MEAS_KEY']),
                html.Div(
                    children=[
                        util.named_slider(
                            name="Symbol size",
                            id="symbol_size",
                            min=1,
                            max=8,
                            step=1,
                            value=3,
                            marks={i: str(i) for i in range(1, 9)},
                        )
                    ],
                    style={"width": "20%", "display": "inline-block"},
                ),
                # dropdown_key_y(),
                dcc.Input(
                    id="input_min",
                    type="number",
                    placeholder="Min Value",
                ),
                dcc.Input(
                    id="input_max",
                    type="number",
                    placeholder="Max value",
                ),
                exclude_start(),
                html.P("Color Scale"),
                dcc.Dropdown(
                    id="colorscale",
                    options=[{"value": x, "label": x} for x in colorscales],
                    value="viridis",
                ),
                update_button,
                dcc.Graph(
                    id="plot_polar", figure=util.get_empty_graph("select information first")
                ),
            ]
        )
    else:
        return html.Div(
            [html.P("No measurements collection in the DB can't do anything on this page")]
        )
