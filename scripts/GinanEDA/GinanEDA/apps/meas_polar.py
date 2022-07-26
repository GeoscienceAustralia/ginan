import GinanEDA.apps.utilities as util
import numpy as np
import plotly.express as px
import plotly.graph_objs as go
from app import app
from dash import dcc, html
from dash.dependencies import Input, Output, State
from GinanEDA.datasets import db

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


import plotly.express as px

colorscales = px.colors.named_colorscales()


def exclude_start():
    return html.Div(
        [
            html.P("Exclude the first points", style={"margin-left": "3px"}),
            dcc.Input(id="exclude_npt", value="0", type="text", size="2")
            # html.P(' points',style={'display':'inline-block','margin-right':5}),
        ],
        style={"width": "15%", "display": "inline-block"},
    )


def dropdown_site(site_list):
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


def keys(data):
    # a = db.MONGO_CL["Measurements"].find_one()
    return [{"label": i, "value": i} for i in data if i not in db.exclude_measurements]


def dropdown_key_y():
    return html.Div(
        [
            dcc.Dropdown(
                id="mespolar_dropdown_key_y",
                options=[i for i in keys()],
                placeholder="Y axis",
                value=None,
            )
        ],
        style={"width": "20%", "display": "inline-block"},
    )


def dropdown_key_x(data):
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


def generate_trace(graph_type, x, y, label):
    if graph_type == "Line" or graph_type == "Scatter":
        if graph_type == "Line":
            mode = "lines"
        else:
            mode = "markers"
        trace = go.Scatter(x=x, y=y, mode=mode, name=label)
    elif graph_type == "POLAR":
        trace = go.Scatterpolar(r=y, theta=x, mode="markers")  # , name=label)
    return trace


@app.callback(
    Output("plot_polar", "figure"),
    inputs=[Input("update_graph_measP", "n_clicks")],
    state=[
        State("session-store", "data"),
        State("mespolar_dropdown_site", "value"),
        State("mespolar_dropdown_sat", "value"),
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
    click,
    data_dict,
    site,
    sat,
    caxis,
    list_site,
    list_sat,
    sym_size,
    input_min,
    input_max,
    exclude,
    cmap,
):
    try:
        exclude = int(exclude)
    except:
        exclude = 0

    if exclude < 0:
        exclude = 0
    fig = go.Figure()
    if site is None or sat is None or caxis is None:
        return get_empty_graph(
            "Make sure a value for all the Dropdown Menu is selected"
        )
    else:

        site = [i["value"] for i in list_site] if site == "ALL" else [site]
        sat = [i["value"] for i in list_sat] if "ALL" in sat else sat
        site_, sat_, x_, y_, z_ = db.get_series_xyz(data_dict,
            "Measurements", None, site, sat, "Azimuth", "Elevation", caxis
        )
        trace = []
        min_ = 0
        max_ = 0
        for z in z_:
            min_ = min(min_, z[exclude:].min())
            max_ = max(max_, z[exclude:].max())
        min_ = np.floor(min_)
        max_ = np.ceil(max_)
        if input_min is not None:
            min_ = input_min
        if input_max is not None:
            max_ = input_max

        for i in range(len(x_)):
            _x, _y, _z = x_[i][exclude:], y_[i][exclude:], z_[i][exclude:]
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
                    name=f"{site_[i]}-{sat_[i]}",
                    hovertemplate="r:%{r:.3f} <br>theta:%{theta:.3f}<br>c: %{marker.color:.3f} ",
                )
            )
        fig = go.Figure(data=trace)
        fig.update_layout(
            xaxis=dict(rangeslider=dict(visible=True)),
            yaxis=dict(fixedrange=False),
            height=600,
            coloraxis_colorbar_x=-1.0,
            polar=dict(
                radialaxis_tickfont_size=8,
                angularaxis=dict(tickfont_size=8, rotation=90, direction="clockwise"),
                radialaxis=dict(range=[90, 0]),
            ),
            legend=dict(yanchor="top", y=0.99, xanchor="left", x=0.85),
        )
    fig.layout.autosize = True

    return fig


def layout(data_dict):
    if data_dict== None:
        return html.Div(
            [html.P("First you will need to select a DB in the Db Info menu")]
        )
    else:
        return html.Div(
            [
                dropdown_site(data_dict['DB_SITE']),
                dropdown_sat(data_dict['DB_SAT']),
                dropdown_key_x(data_dict['DB_MEAS_KEY']),
                html.Div(
                    children=[
                        util.namedSlider(
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
                    id="plot_polar", figure=get_empty_graph("select information first")
                ),
            ]
        )
