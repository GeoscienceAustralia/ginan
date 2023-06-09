"""_summary_
"""

import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import PolynomialFeatures
from dash import dash_table, dcc, html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go

from app import app
from ginaneda.datasets import db
import ginaneda.apps.utilities as util


def exclude_start():
    """_summary_
    """
    return html.Div(
        [
            html.P(
                "Exclude the first points",
                style={
                    "display": "inline-block",
                    "margin-right": 20},
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
        ],
        style={
            "width": "10%",
            "display": "inline-block"},
    )


possible_plot = ["Line", "Scatter", "HistogramX", "HistogramY", "Fourier"]


def dropdown_type():
    """_summary_
    """
    return html.Div(
        [
            util.named_dropdown(
                "Type",
                id="state_dropdown_type",
                options=[{"label": i, "value": i} for i in possible_plot],
                placeholder="Graph Type",
                value=None,
            )
        ],
        style={"width": "10%", "display": "inline-block"},
    )


def dropdown_serie(sim_list):
    """_summary_
    """
    simList = list(["ALL"])
    simList.extend(sim_list)
    p = util.named_dropdown(
        "Series",
        id="sate_dropdown_serie",
        options=[{"label": i, "value": i} for i in simList],
        placeholder="Series",
        value=None,
        multi=True,
    )
    p.style = {"width": "20%", "display": "inline-block"}
    return p


possible_trend = ["None", "Fit", "Detrend"]
poly_dropdown = html.Div(
    [
        util.named_dropdown(
            "Trend correction",
            id="poly_dropdown",
            options=[{"label": i, "value": i} for i in possible_trend],
            placeholder="Fit Type",
            value="None",
            multi=False
        )
    ],
    style={"width": "10%", "display": "inline-block"},
)

#     dcc.Dropdown(
#         id='poly_dropdown',
#         options=[{'label': i, 'value': i} for i in possible_plot],
#         placeholder="Fit Type",
#         value='None',
# )
# ],
#     style={'width': '10%', 'display': 'inline-block'}
# )


def check_list():
    """_summary_
    """
    return dcc.Checklist(
        options=[
            {"label": "Aggregate (hist/stats)", "value": "agg"},
        ],
    )


def dropdown_site(site_list):
    """_summary_
    """
    site_list2 = list(["ALL"])
    site_list2.extend(site_list)
    return html.Div(
        [
            util.named_dropdown(
                "Site",
                id="state_dropdown_site",
                options=[{"label": i, "value": i} for i in site_list2],
                placeholder="Site",
                value=None,
                multi=True,
            )
        ],
        style={"width": "15%", "display": "inline-block"},
    )

    # return html.Div([
    #     dcc.Dropdown(
    #         id='state_dropdown_site',
    #         options=[{'label': i, 'value': i} for i in site_list2],
    #         placeholder = "Site",
    #         value=None,
    #         multi=True
    #     )
    # ],
    #     style={'width': '20%', 'display': 'inline-block'}
    # )


def dropdown_sat(sat_list):
    """_summary_
    """
    sat_list2 = list(["ALL"])
    sat_list2.extend(sat_list)
    return html.Div(
        [
            util.named_dropdown(
                "Sat",
                id="state_dropdown_sat",
                options=[{"label": i, "value": i} for i in sat_list2],
                placeholder="Sat",
                value=None,
                multi=True,
            )
        ],
        style={"width": "15%", "display": "inline-block"},
    )


def dropdown_state(state_list):
    """_summary_
    """
    return html.Div(
        [
            util.named_dropdown(
                "State",
                id="state_dropdown_state",
                options=[{"label": i, "value": i} for i in state_list],
                placeholder="State",
                value=None,
                multi=False,
            )
        ],
        style={"width": "15%", "display": "inline-block"},
    )

    # return html.Div([
    #     dcc.Dropdown(
    #         id='state_dropdown_state',
    #         options=[{'label': i, 'value': i} for i in state_list],
    #         placeholder="State",
    #         value=None
    #     )
    # ],
    #     style={'width': '20%', 'display': 'inline-block'}
    # )


def keys(data):
    """_summary_
    """
    return [{"label": i, "value": i}
            for i in data if i not in db.exclude_state]


def dropdown_key_y(data):
    """_summary_
    """
    return html.Div(
        [
            util.named_dropdown(
                "Y axis",
                id="state_dropdown_key_y",
                options=[i for i in keys(data)],
                placeholder="Y axis",
                value=None,
                multi=True,
            )
        ],
        style={"width": "15%", "display": "inline-block"},
    )

    # return html.Div([
    #     dcc.Dropdown(
    #         id='state_dropdown_key_y',
    #         options=[i for i in keys()],
    #         placeholder="Y axis",
    #         value=None
    #     )
    # ],
    #     style={'width': '20%', 'display': 'inline-block'}
    # )


def dropdown_key_x(data):
    """_summary_
    """
    return html.Div(
        [
            util.named_dropdown(
                "X axis",
                id="state_dropdown_key_x",
                options=[i for i in keys(data)],
                placeholder="X axis",
                value=None,
            )
        ],
        style={"width": "15%", "display": "inline-block"},
    )


update_button = html.Div(
    [html.Button("update graph", id="update_graph", n_clicks=0)],
    style={"width": "5%", "display": "inline-block"},
)


# @app.callback(
#         Output("state_dropdown_site", "options"),
#         Output("state_dropdown_sat", "options"),
#     # Output("state_dropdown_key_x", "options"),
#     Output("state_dropdown_key_y", "options"),
#    Input("state_dropdown_state", "value"),
#    State("session-store", "data")
# )
# def update_dropdown(state, data_dict):
#     if not state:
#         return [], [], [], []
#     else:
#         for st in data_dict['DB_STATE_DIC']:
#             if st["_id"] == state:
#                 break
#     site_list = list(["ALL"])
#     site_list.extend(st["Site"])
#     sat_list = list(["ALL"])
#     sat_list.extend(st["Sat"])
#     keys = st["keys"]
#     keys_sat = [{"label": i, "value": i} for i in sat_list]
#     keys_site = [{"label": i, "value": i} for i in site_list]
#     keys_d = [{"label": i, "value": i} for i in keys if i[0] != "_"]
#     return keys_site, keys_sat, keys_d


@app.callback(
    Output("plot2", "figure"),
    Output("tableDiv", "children"),
    Input("update_graph", "n_clicks"),
    State("session-store", "data"),
    State("state_dropdown_type", "value"),
    State("state_dropdown_state", "value"),
    State("state_dropdown_site", "value"),
    State("state_dropdown_sat", "value"),
    State("sate_dropdown_serie", "value"),
    State("state_dropdown_key_y", "value"),
    State("state_dropdown_site", "options"),
    State("state_dropdown_sat", "options"),
    State("sate_dropdown_serie", "options"),
    State("slider-polynomial-degree", "value"),
    State("poly_dropdown", "value"),
    State("exclude_npt", "value"),

)
def update_graph_state(
    _click,
    data_dict,
    graph_type,
    state,
    site,
    sat,
    serie,
    yaxis,
    list_site,
    list_sat,
    list_series,
    poly_deg,
    fit_type,
    exclude,
):
    """_summary_
    """
    try:
        exclude = int(exclude)
    except BaseException:
        exclude = 0

    if exclude < 0:
        exclude = 0
    xaxis = "Epoch"
    table = []
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
        site = [i["value"] for i in list_site] if "ALL" in site else site
        sat = [i["value"] for i in list_sat] if "ALL" in sat else sat
        serie = [i["value"] for i in list_series] if "ALL" in serie else serie
        if "ALL" in site:
            site.remove("ALL")
        if "ALL" in sat:
            sat.remove("ALL")
        if "ALL" in serie:
            serie.remove("ALL")
        fig = go.Figure()
        if fit_type != "None":
            pol = PolynomialFeatures(poly_deg)
            model = make_pipeline(pol, LinearRegression())
        trace = []
        for yaxis_ in yaxis:
            query = db.get_series2(
                data_dict, "States", state, site, sat, serie, xaxis, yaxis_, "Num")
            if len(query) == 0 :
                return (util.get_empty_graph(
                    "Nothing to plot"), [], )
            for q in sorted(query):
                record = query[q]
                x_ = np.asarray(record['x'], dtype=np.datetime64)
                y__ = record['y']
                num = record['z']
                unique_data = np.unique([item for sublist in num for item in sublist])
                d = {k: v for v, k in enumerate(unique_data)}
                nx = len(x_)
                ny = len(unique_data)
                y_ = np.empty((nx,ny))
                y_.fill(np.nan)
                # num__ = np.empty((nx,ny)).fill(np.nan)
                for ix, a in enumerate(y__):
                    for iy, v in enumerate(a):
                        y_[ix, d[num[ix][iy]] ] = v

                label_base = f"{q}_{state}"
                for i, v in enumerate(unique_data):
                    _x, _y = x_[:][exclude:], y_[:, i][exclude:]
                    if fit_type != "None":
                        nonans = ~np.isnan(_y)
                        x2 = (_x - _x[0])
                        x2 = (x2 / np.timedelta64(1, 's')).astype(float)
                        x2 = x2[:, np.newaxis]
                        # print(len(x2), )
                        model.fit(x2[nonans], _y[nonans])
                        coeff = model.steps[1][1].coef_.copy()
                        coeff[0] = model.steps[1][1].intercept_
                        _yf = model.predict(x2)
                        a = {}
                        a["ID"] = f"{label_base}_{str(unique_data[i])}"
                        a["Intercept"] = coeff[0]
                        for ideg in range(len(coeff[1:])):
                            a["Deg %i" % (ideg + 1)] = coeff[ideg + 1]
                        a["RMS"] = np.sqrt(np.mean(np.square(_y - _yf)))
                        table.append(a)
                        if fit_type == "Detrend":
                            _y = _y - _yf
                    label = label_base + "_" + str(unique_data[i])

                    trace.append(
                        util.generate_trace(
                            graph_type,
                            _x,
                            _y,
                            f'{label}'))
                    if fit_type == "Fit":
                        trace.append(
                            util.generate_trace(
                                graph_type,
                                _x,
                                _yf,
                                f'{label}_deg{poly_deg}'))

        fig = go.Figure(data=trace)
        fig.update_layout(
            xaxis=dict(rangeslider=dict(visible=True)),
            yaxis=dict(fixedrange=False, tickformat=".3e"),
            height=800,
        )
        fig.layout.autosize = True
        fig.update_layout(showlegend=True)
    tab = []
    if len(table) != 0 and fit_type != "None":
        cols = []
        cols.append("ID")
        cols.append("Intercept")
        for i in range(1, poly_deg + 1):
            cols.append("Deg %i" % i)
        cols.append("RMS")
        tab = [
            dash_table.DataTable(
                id="table", columns=[{"name": i, "id": i} for i in cols], data=table
            )
        ]
    return fig, tab


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
                dropdown_type(),
                dropdown_state(data_dict['DB_STATES']),
                dropdown_site(data_dict['DB_SITE']),
                dropdown_sat(data_dict['DB_SAT']),
                # dropdown_key_x(data_dict['DB_STATE_DIC'][0]["keys"]),
                dropdown_serie(data_dict['Series']),
                dropdown_key_y(["x", "P", "dx"]),  # , "x+P"]),
                html.Div(
                    children=util.named_slider(
                        name="Polynomial Degree",
                        id="slider-polynomial-degree",
                        min=0,
                        max=2,
                        step=1,
                        value=0,
                        marks={i: str(i) for i in range(0, 3)},
                    ),
                    style={"width": "20%", "display": "inline-block"},
                ),
                poly_dropdown,
                exclude_start(),
                # check_list(),
                update_button,
                dcc.Graph(
                    id="plot2", figure=util.get_empty_graph("select information first")
                ),
                html.Div(id="tableDiv", className="tableDiv"),
            ]
        )
    else:
        return html.Div(
        [html.P("No KF State in the DB, can't do anything on this page")]
        )
