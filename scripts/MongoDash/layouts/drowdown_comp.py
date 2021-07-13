import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import dash_bootstrap_components as dbc

import pdb


#*****************************DROPDOWN  for /page-1*************************************************************************#:
#Todo: Make it dynamic, just like fields_set_2
fields_set_1 = ["EPOCH_MEAN_PHASE_RESID","EPOCH_PHASE_RESID_RMS","EPOCH_MEAN_RANGE_RESID","EPOCH_RANGE_RESID_RMS",
         "EPOCH_POSTFIT_CHISQR_DOF","EPOCH_PREFIT_CHISQR_INC","EPOCH_NUM_SITES","EPOCH_NUM_SVS","EPOCH_NUM_OBS",
         "EPOCH_NUM_PARAMS","EPOCH_NUM_AMB","EPOCH_NUM_WL_AMB_FIXED","EPOCH_NUM_NL_AMB_FIXED"]

summary_pg_dropdown_1=  html.Div([
            dcc.Dropdown(
                id='summary_pg_dropdown_1',
                options=[{'label': i, 'value': i} for i in fields_set_1],
                value='EPOCH_MEAN_PHASE_RESID'
            ),

        ],
        style={'width': '33%', 'display': 'inline-block'})

summary_pg_dropdown_2 = html.Div([
            dcc.Dropdown(
                id='summary_pg_dropdown_2',
                options=[{'label': i, 'value': i} for i in fields_set_1],
                value='EPOCH_PHASE_RESID_RMS'
            ),

        ],style={'width': '33%', 'float': 'right', 'display': 'inline-block'})

#*****************************DROPDOWN  for /page-1 ENDS********************************************************************#:
#************************************************************************************************************************#:




fields_set_5_flat =["LINE","SCATTER","POLAR","HISTOGRAM"]


residuals_pg_dropdown_type=  html.Div([
            dcc.Dropdown(
                id='residuals_pg_dropdown_type',
                options=[{'label': i, 'value': i} for i in fields_set_5_flat],
                placeholder = "Graph Type",
                value = "LINE"
            )
        ],
        style={'width': '20%', 'display': 'inline-block'})


residuals_pg_dropdown_site=  html.Div([
            dcc.Dropdown(
                id='residuals_pg_dropdown_site',
                #options=[{'label': i, 'value': i} for i in fields_set_5],
                placeholder ="Receiver",
                value="ALL"
            )
        ],
        style={'width': '20%', 'display': 'inline-block'})
# pdb.set_trace()

residuals_pg_dropdown_sat=  html.Div([
            dcc.Dropdown(
                id='residuals_pg_dropdown_sat',
                # options=[{'label': i, 'value': i} for i in fields_set_6],
                placeholder = "Satellite",
                value="ALL"
            )
        ],
        style={'width': '20%', 'display': 'inline-block'})
residuals_pg_dropdown_xaxis=  html.Div([
            dcc.Dropdown(
                id='residuals_pg_dropdown_xaxis',
                # options=[{'label': i, 'value': i} for i in fields_set_7],
                placeholder = "X axis",
                value="Epoch"
            )
        ],
        style={'width': '20%', 'display': 'inline-block'})

residuals_pg_dropdown_yaxis=  html.Div([
            dcc.Dropdown(
                id='residuals_pg_dropdown_yaxis',
                #options=[{'label': i, 'value': i} for i in fields_set_8],
                placeholder = "Y axis",
                value= None
            )
        ],
        style={'width': '20%', 'display': 'inline-block'})


#*****************************DROPDOWN  for /page-4 ENDS********************************************************************#:
#***************************************************************************************************************************#:



sol_pg_dropdown_type=  html.Div([
            dcc.Dropdown(
                id='sol_pg_dropdown_type',
                options=[{'label': i, 'value': i} for i in fields_set_5_flat],
                placeholder = "Graph Type",
                value = "LINE"
            )
        ],
        style={'width': '16%', 'display': 'inline-block'})


sol_pg_dropdown_state=  html.Div([
            dcc.Dropdown(
                id='sol_pg_dropdown_state',
                #options=[{'label': i, 'value': i} for i in fields_set_5],
                placeholder ="State",
                value=None
            )
        ],
        style={'width': '16%', 'display': 'inline-block'})


sol_pg_dropdown_site=  html.Div([
            dcc.Dropdown(
                id='sol_pg_dropdown_site',
                #options=[{'label': i, 'value': i} for i in fields_set_5],
                placeholder ="Receiver",
                value="ALL"
            )
        ],
        style={'width': '16%', 'display': 'inline-block'})

sol_pg_dropdown_sat=  html.Div([
            dcc.Dropdown(
                id='sol_pg_dropdown_sat',
                # options=[{'label': i, 'value': i} for i in fields_set_6],
                placeholder = "Satellite",
                value="ALL"
            )
        ],
        style={'width': '16%', 'display': 'inline-block'})

sol_pg_dropdown_xaxis=  html.Div([
            dcc.Dropdown(
                id='sol_pg_dropdown_xaxis',
                # options=[{'label': i, 'value': i} for i in fields_set_7],
                placeholder = "X axis",
                value="Epoch"
            )
        ],
        style={'width': '16%', 'display': 'inline-block'})

sol_pg_dropdown_yaxis=  html.Div([
            dcc.Dropdown(
                id='sol_pg_dropdown_yaxis',
                #options=[{'label': i, 'value': i} for i in fields_set_8],
                placeholder = "Y axis",
                value= None
            )
        ],
        style={'width': '16%', 'display': 'inline-block'})







