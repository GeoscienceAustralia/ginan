import numpy as np
from dash import html, dcc
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd
import pathlib
from app import app
from sklearn.metrics import mean_squared_error
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import  make_pipeline

from dash import html, dcc, dash_table
from dash.dependencies import Input, Output, State
import plotly.express as px
import plotly.graph_objs as go

from app import app
import apps.utilities as util
from datasets import db


def exclude_start():
    return html.Div([
        html.P('Exclude the first points',style={'display':'inline-block','margin-right':20}),
        dcc.Input(id='exclude_npt', value='0', type='text', size='2'),
        html.P(' points',style={'display':'inline-block','margin-right':5}),
    ],
    style={'width': '10%', 'display': 'inline-block'}
    )


possible_plot = ['Line', 'Scatter','HistogramX','HistogramY']
dropdown_type = html.Div([
    dcc.Dropdown(
        id='state_dropdown_type',
        options=[{'label': i, 'value': i} for i in possible_plot],
        placeholder="Graph Type",
        value=None
    )
],
    style={'width': '10%', 'display': 'inline-block'}
)

possible_plot = ['None', 'Fit', 'Detrend']
poly_dropdown = html.Div([
    dcc.Dropdown(
        id='poly_dropdown',
        options=[{'label': i, 'value': i} for i in possible_plot],
        placeholder="Fit Type",
        value='None',
)
],
    style={'width': '10%', 'display': 'inline-block'}
)

def check_list():
    return dcc.Checklist(
        options=[
            {'label': 'Aggregate (hist/stats)', 'value': 'agg'},
        ],
        # value=['NYC', 'MTL']
    )


def dropdown_site(site_list):
    site_list2 = list(['ALL'])
    site_list2.extend(site_list)
    # print(site_list2)
    return html.Div([
        dcc.Dropdown(
            id='state_dropdown_site',
            options=[{'label': i, 'value': i} for i in site_list2],
            placeholder = "Site",
            value=None,
            multi=True
        )
    ],
        style={'width': '20%', 'display': 'inline-block'}
    )


def dropdown_sat(sat_list):
    sat_list2 = list(['ALL'])
    sat_list2.extend(sat_list)
    return html.Div([
        dcc.Dropdown(
            id='state_dropdown_sat',
            options=[{'label': i, 'value': i} for i in sat_list2],
            placeholder="Sat",
            value=None,
            multi=True
        )
    ],
        style={'width': '20%', 'display': 'inline-block'}
    )


def dropdown_state(state_list):
    # print(state_list)
    return html.Div([
        dcc.Dropdown(
            id='state_dropdown_state',
            options=[{'label': i, 'value': i} for i in state_list],
            placeholder="State",
            value=None
        )
    ],
        style={'width': '20%', 'display': 'inline-block'}
    )


def keys():
    a = db.MONGO_CL["States"].find_one()
    temp = list(a.keys())
    return [{'label': i, 'value': i} for i in temp if i[0] != '_']


def dropdown_key_y():
    return html.Div([
        dcc.Dropdown(
            id='state_dropdown_key_y',
            options=[i for i in keys()],
            placeholder="Y axis",
            value=None
        )
    ],
        style={'width': '20%', 'display': 'inline-block'}
    )


def dropdown_key_x():
    # print(keys())
    return html.Div([
        dcc.Dropdown(
            id='state_dropdown_key_x',
            options=[i for i in keys()],
            placeholder = "X Axis",
            value=None
        )
    ],
        style={'width': '20%', 'display': 'inline-block'}
    )

update_button = html.Div([
    html.Button("update graph", id='update_graph', n_clicks=0)], style={'width': '5%', 'display': 'inline-block'}
)


def get_empty_graph(message):
    emptiness = {
        "layout": {
            "xaxis": {
                "visible": False
            },
            "yaxis": {
                "visible": False
            },
            "annotations": [
                {
                    "text": message,
                    "xref": "paper",
                    "yref": "paper",
                    "showarrow": False,
                    "font": {
                        "size": 28
                    }
                }
            ]
        }
    }
    return emptiness

def generate_trace(graph_type, x, y, label):
    if graph_type =="Line" or graph_type =="Scatter":
        if (graph_type == "Line"):
            mode = "lines"
        else:
            mode = "markers"
        # print(label, mode, graph_type)
        trace = go.Scatter(x=x,  y=y, mode=mode, name=label)
    elif(graph_type =="POLAR"):
        trace = go.Scatterpolar(r=y, theta=x, mode="markers", name=label)
    elif(graph_type=='HistogramX'):
        trace = go.Histogram(x=y, name=label)
    elif(graph_type=='HistogramY'):
        trace = go.Histogram(y=y, name=label)
    return trace


@app.callback(
    [
        Output('state_dropdown_site',  'options'),
        Output('state_dropdown_sat',   'options'),
        Output('state_dropdown_key_x', 'options'),
        Output('state_dropdown_key_y', 'options'),
    ],
    inputs=[Input('state_dropdown_state',  'value')]
)
def update_dropdown(state):
    # print(state)
    if not state:
        return [],[],[],[]
    else:
        for st in db.DB_STATE_DIC:
            if st['_id'] == state:
                # print (state)
                break
    # print(st)
    site_list = list(['ALL'])
    site_list.extend(st['Site'])
    sat_list = list(['ALL'])
    sat_list.extend(st['Sat'])
    keys = st['keys']
    # print(site_list, sat_list, keys)
    # for i in sat_list:
    #     print(i)
    keys_sat = [{'label': i, 'value': i} for i in sat_list ]
    keys_site = [{'label': i, 'value': i} for i in site_list ]
    keys_d = [{'label': i, 'value': i} for i in keys if i[0] != '_']
    return keys_site, keys_sat, keys_d, keys_d


@app.callback(
    Output('plot2', 'figure'),
    Output('tableDiv', 'children'),
    inputs=[Input('update_graph', 'n_clicks')],
    state=[
        State('state_dropdown_type',  'value'),
        State('state_dropdown_state',  'value'),
        State('state_dropdown_site',  'value'),
        State('state_dropdown_sat',   'value'),
        State('state_dropdown_key_x', 'value'),
        State('state_dropdown_key_y', 'value'),
        State('state_dropdown_site', 'options'),
        State('state_dropdown_sat', 'options'),
        State('slider-polynomial-degree', 'value'),
        State('poly_dropdown', 'value'),
        State('exclude_npt', 'value')
    ])
def update_graph_measurements(click, graph_type, state, site, sat, xaxis, yaxis, list_site, list_sat, poly_deg, fit_type, exclude):
    # print("HELLO", graph_type, state, "/",site,"/", sat,"/", xaxis,'/', yaxis)
    try:
        exclude = int(exclude)
    except:
        exclude = 0

    if exclude < 0:
        exclude = 0
    table = []
    if graph_type is None or site is None or sat is None or xaxis is None or yaxis is None:
        return get_empty_graph("Make sure a value for all the Dropdown Menu is selected"), []
    else:
        print(sat)
        site = [i['value'] for i in list_site] if 'ALL' in site else site
        sat = [i['value'] for i in list_sat] if 'ALL' in sat else sat
        fig = go.Figure()
        if fit_type != "" :
            pol = PolynomialFeatures(poly_deg)
            model = make_pipeline(pol, LinearRegression())
        site_, sat_, x_, y_ = db.get_series('States', state, site, sat, xaxis, yaxis)
        trace = []
        for i in range(len(x_)):
            _x, _y = x_[i][exclude:], y_[i][exclude:]
            if fit_type != "":
                x2 = _x.astype(float)
                x2 = x2 - x2[0]
                x2 = x2[:, np.newaxis]
                model.fit(x2, _y)
                coeff = model.steps[1][1].coef_.copy()
                coeff[0] = model.steps[1][1].intercept_
                _yf = model.predict(x2)
                print(f"{site_[i]}-{sat_[i]} Coeff: {np.array2string(coeff)}")
                print(f"                    RMS: {np.sqrt(np.mean(np.square(_y-_yf)))}")
                a = dict()
                a['ID']= f"{site_[i]}-{sat_[i]}"
                a['Intercept'] = coeff[0]
                for ideg in range(len(coeff[1:])):
                    a['Deg %i'%(ideg+1)] = coeff[ideg+1]
                a['RMS'] = np.sqrt(np.mean(np.square(_y-_yf)))
                table.append(a)
                if fit_type =="Detrend":
                        _y = _y - _yf
            # print(_x.dtype)
            # print(f'{site_[i]}-{sat_[i]}')
            trace.append(generate_trace(graph_type, _x, _y, f'{site_[i]}-{sat_[i]}'))
            if fit_type == "Fit":
                trace.append(generate_trace(graph_type, _x, _yf, f'{site_[i]}-{sat_[i]}_deg{poly_deg}'))

        fig = go.Figure(data=trace)
        fig.update_layout(xaxis=dict(rangeslider=dict(visible=True)),  yaxis=dict(fixedrange=False, tickformat='.3f'), height=800)
        fig.layout.autosize = True
    tab = []
    if len(table) != 0 and fit_type != "None":
        cols = []
        cols.append('ID')
        cols.append('Intercept')
        for i in range(1, poly_deg+1):
            cols.append("Deg %i" % i)
        cols.append("RMS")
        tab = [
            dash_table.DataTable(
                id='table',
                columns=[{"name": i, "id": i} for i in cols],
                data=table
            )]
        print(tab)
        print(cols)
    return fig, tab




def layout():
    if db.MONGO_CL == None:
        return html.Div([html.P("First you will need to select a DB in the Db Info menu")])
    else:
        return html.Div(
            [
                dropdown_type,
                dropdown_state(db.DB_STATES),
                dropdown_site(db.DB_SITE),
                dropdown_sat(db.DB_SAT),
                dropdown_key_x(),
                dropdown_key_y(),

                    html.Div( children=util.namedSlider(
                        name='Polynomial Degree',
                        id='slider-polynomial-degree',
                        min=0,
                        max=2,
                        step=1,
                        value=0,
                        marks={i: str(i) for i in range(0, 3)},
                    ),
                        style={'width': '20%', 'display': 'inline-block'}
                ),

                poly_dropdown,
                exclude_start(),
                # check_list(),
                update_button,
                dcc.Graph(
                    id='plot2',
                    figure=get_empty_graph("select information first")
                ),
                html.Div(
                    id='tableDiv',
                    className='tableDiv'
                )
            ]
        )