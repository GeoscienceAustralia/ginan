

import dash
import dash_bootstrap_components as dbc
import dash_core_components as dcc
import dash_html_components as html


import pandas as pd
import pdb


from layouts.drowdown_comp import *


res_graph_layout = html.Div([
    html.Div([
        residuals_pg_dropdown_type,
        residuals_pg_dropdown_site,
        residuals_pg_dropdown_sat,
        residuals_pg_dropdown_xaxis,
        residuals_pg_dropdown_yaxis
    ]),

    dcc.Graph(id='res_graph',style ={'height':'80vh'}),

])

sol_graph_layout = html.Div([
    html.Div([
        sol_pg_dropdown_type,
        sol_pg_dropdown_state,
        sol_pg_dropdown_site,
        sol_pg_dropdown_sat,
        sol_pg_dropdown_xaxis,
        sol_pg_dropdown_yaxis
    ]),

    dcc.Graph(id='sol_graph',style ={'height':'80vh'}),

])
