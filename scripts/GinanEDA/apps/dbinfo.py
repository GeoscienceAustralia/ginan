from dash import html, dcc
from dash.dependencies import Input, Output, State
import plotly.express as px
from app import app
from apps import meas
from datasets import db

layout = html.Div([
    html.H1('Information on the database', style={"textAlign": "center"}),
    html.Div(id='db-info', children=[
        db.check_db(),
        ], className='row'),
    html.Div([
        html.P("Database to connect to:"),
        dcc.Input(id='databaseName', value='127.0.0.1', type='text'),
        html.Button(id='connect-to-mongo', type='Connect', children='Connect to mongo')
        ], className='row'),
    # html
    html.Div([
        html.P("List of DB:"),
        dcc.Dropdown(
            id='dropdown-db',
            # optionss=[],
            placeholder="Databases",
            value=None
        ),
        html.Button(id='connect-to-db', type='Connect', children='Load DB')
    ], className='row'),
    html.Div(children=[], id='db-infobox'),
    ])


@app.callback(Output('dropdown-db', 'options'),
              [Input('connect-to-mongo', 'n_clicks')],
              [State('databaseName', 'value')],
              )
def update_connection(n, v):
    db.MONGO_URL = v
    # print("db url", db.MONGO_URL)
    db.connect_client()
    # print('dbclient', db.MONGO_CL)
    if db.MONGO_CL is not None:
        dropdowns = [{'label': i['name'], 'value': i['name']} for i in db.MONGO_CL.list_databases()]
    else:
        dropdowns = [{'label': " ", 'value': 0}]
    # print(dropdowns)
    return dropdowns


@app.callback([Output('db-info', 'children'),
               Output('db-info-side', "children"),
               Output('db-infobox', "children")],
              [Input('connect-to-db', 'n_clicks')],
              [State('dropdown-db', 'value')],
              )
def update_connection_db(n, v):
    db.connect_client()
    db.MONGO_DB = v
    db.connect_db()
    # print("value", v)
    sidebar_log = html.P(f"{db.MONGO_DB} ({db.MONGO_URL})")
    infobox = []
    if db.MONGO_CL is not None and db.MONGO_DB is not None:
        infobox.append(html.H2("Connection successful"))
        list_cl = list(db.MONGO_CL.list_collection_names())
        if "Measurements" in list_cl:
            infobox.append(html.P(f"Measurements present"))
            if db.DB_SAT:
                infobox.append(html.P(f"Num satelites {len(db.DB_SAT)}"))
                infobox.append(html.P(f"Num sites     {len(db.DB_SITE)}"))
        else :
            infobox.append(html.P(f"Measurements NOT present"))
            infobox.append(html.P(f"Num satelites N/A"))
            infobox.append(html.P(f"Num sites     N/A"))

        if "States" in list_cl:
            infobox.append(html.P(f"State present"))
        else :
            infobox.append(html.P(f"States NOT present"))
    return db.check_db(), sidebar_log, infobox