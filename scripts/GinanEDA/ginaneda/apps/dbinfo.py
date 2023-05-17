"""
Page for the information on the DB
"""
import logging

from dash import dcc, html
from dash.dependencies import Input, Output, State

from app import app
from ginaneda.datasets import db

logger = logging.getLogger(__name__)

layout = html.Div(
    [
        html.H1("Information on the database", style={"textAlign": "center"}),
        html.Div(
            id="db-info",
            children=[
                db.check_db(),
            ],
            className="row",
        ),
        html.Div(
            [
                html.P("Database to connect to:"),
                dcc.Input(id="databaseName", value="127.0.0.1", type="text"),
                html.Button(
                    id="connect-to-mongo", type="Connect", children="Connect to mongo"
                ),
            ],
            className="row",
        ),
        # html
        html.Div(
            [
                html.P("List of DB:"),
                dcc.Dropdown(
                    id="dropdown-db",
                    # optionss=[],
                    placeholder="Databases",
                    value=None,
                    multi=True
                ),
                html.Button(
                    id="connect-to-db",
                    type="Connect",
                    children="Load DB"),
            ],
            className="row",
        ),
        html.Div(children=[], id="db-infobox"),
    ]
)


@app.callback(
    Output("dropdown-db", "options"),

    Input("connect-to-mongo", "n_clicks"),
    State("databaseName", "value"),
)
def update_connection_list(_n, database_name):
    """Update the connection list of the database
    """
    client = db.connect_client(database_name)
    if client is not None:
        dropdowns = [
            {"label": i["name"], "value": i["name"]}
            for i in client.list_databases()
        ]
    else:
        dropdowns = [{"label": " ", "value": 0}]
    return dropdowns


@app.callback(
    Output("db-info", "children"),
    Output("db-info-side", "children"),
    Output("db-infobox", "children"),
    Output("session-store", "data"),

    Input("connect-to-db", "n_clicks"),
    State("dropdown-db", "value"),
    State("databaseName", "value"),
)
def update_connection(_n, database_name, database_url):
    """Update the connection
    """
    data_dict = {}
    data_dict['MONGO_URL'] = database_url
    client = db.connect_client(data_dict['MONGO_URL'])
    data_dict['MONGO_DB'] = database_name
    db.connect_db(data_dict)
    sidebar_log = html.P(
        f"{data_dict['MONGO_URL']} / ({data_dict['MONGO_DB']})")
    infobox = []
    if client is not None and data_dict['MONGO_DB'] is not None:
        infobox.append(html.H2("Connection successful"))
        client = db.connect_client(data_dict['MONGO_URL'])[
            data_dict['MONGO_DB'][0]]
        list_cl = list(client.list_collection_names())
        if "Measurements" in list_cl:
            infobox.append(html.P("Measurements present"))
            if data_dict['DB_SAT']:
                infobox.append(
                    html.P(f"Num satelites {len(data_dict['DB_SAT'])}"))
                infobox.append(
                    html.P(f"Num sites     {len(data_dict['DB_SITE'])}"))
        else:
            infobox.append(html.P("Measurements NOT present"))
            infobox.append(html.P("Num satelites N/A"))
            infobox.append(html.P("Num sites     N/A"))
        if "States" in list_cl:
            infobox.append(html.P("State present"))
        else:
            infobox.append(html.P("States NOT present"))
    return db.check_db(), sidebar_log, infobox, data_dict
