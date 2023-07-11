#!/usr/bin/env python3

"""_summary_
"""
import logging

from dash import dcc, html
from dash.dependencies import Input, Output, State

import ginaneda
from app import app, server
from ginaneda.apps import dbinfo, meas, meas_polar, state, pos, clock
from ginaneda.datasets import db


CONTENT_STYLE = {
    "margin-left": "18rem",
    "margin-right": "2rem",
    "padding": "2rem 1rem",
}
SIDEBAR_STYLE = {
    "position": "fixed",
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "16rem",
    "padding": "2rem 1rem",
    "background-color": "#f8f9fa",
}
sidebar = html.Div(
    [
        dcc.Store(id='session-store', storage_type='session', data={}),

        html.H2("GINAN", className="display-4"),
        html.P("EDA (MongoDB) ", className="lead"),
        html.Hr(),
        html.P(f"None / None", className="lead", id="db-info-side"),
        # dcc.NavLink("Change it", href="/dbselect", id="page-dbselect-link"),
        # html.P(f"{PEA_DB_NAME}", className="lead"),
        html.Hr(),
        html.Ul(
            children=[
                html.Li(
                    dcc.Link(
                        "Db Info",
                        href="/dbinfo",
                        id="page-dbinfo-link")),
                html.Li(
                    dcc.Link(
                        "States",
                        href="/states",
                        id="page-states-link")),
                html.Li(
                    dcc.Link(
                        "Measurements",
                        href="/measurements",
                        id="page-measurements-link",
                    )
                ),
                html.Li(
                    dcc.Link(
                        "Measurements Polar",
                        href="/measurements-polar",
                        id="page-measurements-polar-link",
                    )
                ),
                html.Li(
                    dcc.Link(
                        "Position Analysis",
                        href="/position-analysis",
                        id="page-position-link",
                    )
                ),
                html.Li(
                    dcc.Link(
                        "Clock Analysis",
                        href="/clock-analysis",
                        id="page-clock-link",
                    )
                ),
            ]
        ),
        html.Div(id='dummy1'),

    ],
    style=SIDEBAR_STYLE,
)
content = html.Div(id="page-content", children=[], style=CONTENT_STYLE)

app.layout = html.Div(
    [dcc.Location(id="url", refresh=False), sidebar, content])


@app.callback(Output("page-content", "children"), Input("url",
              "pathname"), State("session-store", "data"), )
def display_page(pathname, datastore):
    """_summary_
    """
    if pathname == "/measurements":
        return meas.layout(datastore)
    elif pathname == "/states":
        return state.layout(datastore)
    elif pathname == "/measurements-polar":
        return meas_polar.layout(datastore)
    elif pathname == "/position-analysis":
        return pos.layout(datastore)
    elif pathname == "/clock-analysis":
        return clock.layout(datastore)
    elif pathname == "/dbinfo":
        return dbinfo.layout
    else:
        return "404 Page Error! Please choose a link"


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--debug", action="store_true", default=False)
    args = parser.parse_args()
    # print (args)
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
    else:
        logging.getLogger().setLevel(logging.INFO)

    app.run_server(debug=args.debug, port=8080, host="0.0.0.0")
