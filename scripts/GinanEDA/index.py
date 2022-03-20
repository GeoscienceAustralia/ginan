from dash import dcc
from dash import html
from dash.dependencies import Input, Output

# Connect to main app.py file
from app import app
from app import server

# Connect to your app pages
from apps import meas, state, dbinfo, meas_polar

from datasets import db

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
        html.H2("GINAN", className="display-4"),
        html.P("EDA (MongoDB) ", className="lead"),
        html.Hr(),
        html.P(f"{db.MONGO_URL} / {db.MONGO_DB}", className="lead", id="db-info-side"),
        # dcc.NavLink("Change it", href="/dbselect", id="page-dbselect-link"),
        # html.P(f"{PEA_DB_NAME}", className="lead"),
        html.Hr(),
        html.Ul(children=[html.Li(dcc.Link("Db Info",           href="/dbinfo", id="page-dbinfo-link")),
                          html.Li(dcc.Link("States",           href="/states", id="page-states-link")),
                          html.Li(dcc.Link("Measurements",     href="/measurements", id="page-measurements-link")),
                          html.Li(dcc.Link("Measurements Polar",     href="/measurements-polar", id="page-measurements-polar-link"))
                          ]),
    ],
    style=SIDEBAR_STYLE,
)
content = html.Div(id='page-content', children=[], style=CONTENT_STYLE)

app.layout = html.Div([dcc.Location(id='url', refresh=False), sidebar, content ])

@app.callback(Output('page-content', 'children'),
              [Input('url', 'pathname')])
def display_page(pathname):
    if pathname == '/measurements':
        return meas.layout()
    if pathname == '/states':
        return state.layout()
    if pathname == '/measurements-polar':
        return meas_polar.layout()
    if pathname == '/dbinfo':
        return dbinfo.layout
    else:
        return "404 Page Error! Please choose a link"


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--debug', action='store_true', default=False)
    args = parser.parse_args()
    # print (args)
    app.run_server(debug=args.debug, port=8080, host='0.0.0.0')