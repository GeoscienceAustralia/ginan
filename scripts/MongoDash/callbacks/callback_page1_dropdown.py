import operator

from dash.dependencies import Input, Output

import dash_html_components as html
import plotly.express       as px
import plotly.graph_objects as go
import pdb
import numpy                as np
import re
import pymongo

from app                    import app
from callbacks.helperfunc   import *
from callbacks.empty_graph  import get_empty_graph



def get_trace_sol(graph_type, State, Site, Sat, xaxis, yaxis):

    traceList = []
    mode = ""

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    measurements = pea["States"]

    # rows = db.search((Row.Site == Site) & (Row.Sat == Sat))
    rows = measurements.find({"Site": Site, "Sat" : Sat, "State" : State}, {"_id":0, xaxis:1, yaxis:1})

    x = [row[xaxis] for row in rows.clone() if (xaxis in row and yaxis in row)]
    y = [row[yaxis] for row in rows         if (xaxis in row and yaxis in row)]

    label = str(Site + " " + Sat)

    if (graph_type == "LINE" or graph_type == "SCATTER"):

        if (graph_type == "LINE"):
            mode = "lines"
        else:
            mode = "markers"


        trace =go.Scatter(
        x= x, #sortd_df['x'],
        y= y, #sortd_df['y'] ,
        mode=mode,
        name=label)

    elif(graph_type =="POLAR"):
        trace = go.Scatterpolar(
            r = y,
            theta = x,
            mode = "markers" ,
            name = label )

    elif(graph_type =="HISTOGRAM"):
        trace = go.Histogram(x= res[Sat][Site][attribute])
        return trace




    traceList.append(trace)


    return traceList




@app.callback(
    Output('sol_pg_dropdown_sat', 'options'),
    [Input('sol_pg_dropdown_site', 'value')]
)
def update_date_dropdown(site):

    sats = ["ALL"]

    if (site):
        if (site == "ALL"):
            for s in getAllSites_sol():
                sats.extend(satsVisibleToRec_sol(site))
        else:
            sats.extend(satsVisibleToRec_sol(site))


    sats = list(set(sats))
    return [{'label': sat, 'value': sat} for sat in sats]




@app.callback(
    Output('sol_pg_dropdown_site', 'options'),
    [Input('sol_pg_dropdown_type', 'value')]
)
def update_date_dropdown2(graph_type):

    list = ["ALL"]
    list.extend(getAllSites_res())

    return [{'label': sat, 'value': sat} for sat in list]



@app.callback(
    Output('sol_pg_dropdown_state', 'options'),
    [Input('sol_pg_dropdown_type', 'value')]
)
def update_date_dropdownx(graph_type):

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    stateentries = pea["States"]

    rows    = stateentries.find()
    # for row in rows.clone():
        # print(row)
    states   = list(set([row['State'] for row in rows]))
    states.sort()
    # print(sites)
    return [{'label': state, 'value': state} for state in states]


@app.callback(
    Output('sol_pg_dropdown_xaxis', 'options'),
    [Input('sol_pg_dropdown_type', 'value')]
)
def update_date_dropdown2(graph_type):

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    measurements = pea["States"]

    row = measurements.find_one()
    temp = [a for a in row.keys()]
    temp.sort()
    return [{'label': i, 'value': i} for i in temp if i[0] != '_']




@app.callback(
    Output('sol_pg_dropdown_yaxis', 'options'),
    [Input('sol_pg_dropdown_type', 'value')]
)
def update_date_dropdown3(graph_type):

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    measurements = pea["States"]

    # for x in measurements.find():
    row = measurements.find_one()
    temp = [a for a in row.keys()]
    temp.sort()
    return [{'label': i, 'value': i} for i in temp if i[0] != '_']




def add_site_sol(graph_type, state, site, sat, groupby, attribute):

    tempListTrace = []
    satVisible = satsVisibleToRec_sol(site)

    if (sat == "ALL"):
        for satellite in satVisible:
            tempListTrace = tempListTrace   + get_trace_sol(graph_type, state, site, str(satellite), groupby, attribute)
    else:
        if (sat not in satVisible):
            return get_empty_graph("Select  Satellite from the Dropdown")

        tempListTrace = tempListTrace       + get_trace_sol(graph_type, state, site, sat, groupby, attribute)

    return tempListTrace




@app.callback(
    Output('sol_graph', 'figure'),
    [
     Input('sol_pg_dropdown_type',  'value'),
     Input('sol_pg_dropdown_state', 'value'),
     Input('sol_pg_dropdown_site',  'value'),
     Input('sol_pg_dropdown_sat',   'value'),
     Input('sol_pg_dropdown_xaxis', 'value'),
     Input('sol_pg_dropdown_yaxis', 'value'),
    ])
def update_graph1(graph_type, state, site, sat, xaxis, yaxis):

    fig = go.Figure()

    if (graph_type is None or site is None or state is None or sat is None or xaxis is None or yaxis is None):

         return get_empty_graph("Make sure a value for all the Dropdown Menu is selected")

    if (graph_type =="LINE" or graph_type=="SCATTER" or graph_type=="POLAR"):

        mode = ""
        if (graph_type == "LINE"):
            mode = "lines"
        else:
            mode = "markers"

        tempListTrace = []

        if (site == "ALL"):
            for rec in getAllSites_sol():
                tempListTrace = tempListTrace + add_site_sol(graph_type, state, rec, sat, xaxis, yaxis)

        else:
            if site not in getAllSites_sol():
                return get_empty_graph("Select Site from the Dropdown")
            tempListTrace = tempListTrace + add_site_sol(graph_type, state, site, sat, xaxis, yaxis)

        fig = go.Figure(data = tempListTrace)
        fig.update_layout(xaxis = dict(rangeslider = dict(visible = True)))
        fig.layout.autosize = True
        if (yaxis == "Elevation"):
            fig.update_polars(radialaxis_range=[90,0])
        if (xaxis == "Azimuth"):
            fig.update_polars(angularaxis_direction="clockwise")



        return fig


    elif (graph_type =="HISTOGRAM"):

        if (xaxis != "EPOCHS"):
            return get_empty_graph("Not implmented yet")

        else:

            if(sat == "ALL"):
                return get_empty_graph(" ALL- FUNCTIONALITY for histograms has not been implemented.")

            else:
                if (sat not in satVisible):
                    return get_empty_graph("Select  Satellite from the Dropdown")

                trace = get_trace_sol(graph_type, state, site, sat, xaxis, yaxis)
                sliders = [dict(
                                active = 4,
                                currentvalue = {"prefix": "bin size: "},
                                pad = {"t": 20},
                                steps = [dict(label = i, method = 'restyle',  args = ['xbins.size', i]) for i in np.arange(0.25,15)]
                            )]
                fig = go.Figure()
                fig.add_site_sol(trace)

                fig.update_layout(
                    sliders=sliders
                )

                return fig

    else:
        return get_empty_graph("Select the GRAPH Type from the Dropdown")


