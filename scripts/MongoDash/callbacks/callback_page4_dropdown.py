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



def get_trace_res(graph_type, Site, Sat, xaxis, yaxis):

    traceList = []
    mode = ""

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    measurements = pea["Measurements"]

    # rows = db.search((Row.Site == Site) & (Row.Sat == Sat))
    rows = measurements.find({"Site": Site, "Sat" : Sat}, {"_id":0, xaxis:1, yaxis:1})

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
    Output('residuals_pg_dropdown_sat', 'options'),
    [Input('residuals_pg_dropdown_site', 'value')]
)
def update_date_dropdown(site):

    sats = ["ALL"]

    if (site):
        if (site == "ALL"):
            for s in getAllSites_res():
                sats.extend(satsVisibleToRec_res(site))
        else:
            sats.extend(satsVisibleToRec_res(site))


    sats = list(set(sats))
    return [{'label': sat, 'value': sat} for sat in sats]




@app.callback(
    Output('residuals_pg_dropdown_site', 'options'),
    [Input('residuals_pg_dropdown_type', 'value')]
)
def update_date_dropdown2(graph_type):

    list = ["ALL"]
    list.extend(getAllSites_res())

    return [{'label': sat, 'value': sat} for sat in list]




@app.callback(
    Output('residuals_pg_dropdown_xaxis', 'options'),
    [Input('residuals_pg_dropdown_type', 'value')]
)
def update_date_dropdown2(graph_type):

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    measurements = pea["Measurements"]

    print(measurements)
    row = measurements.find_one()
    print(row)
    
    temp = [a for a in row.keys()]
    temp.sort()
    return [{'label': i, 'value': i} for i in temp if i[0] != '_']




@app.callback(
    Output('residuals_pg_dropdown_yaxis', 'options'),
    [Input('residuals_pg_dropdown_type', 'value')]
)
def update_date_dropdown3(graph_type):

    myclient = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist = myclient.list_database_names()
    pea = myclient[sys.argv[1]]
    measurements = pea["Measurements"]

    # for x in measurements.find():
    row = measurements.find_one()
    temp = [a for a in row.keys()]
    temp.sort()
    return [{'label': i, 'value': i} for i in temp if i[0] != '_']




def add_site_res(graph_type, site, sat, groupby, attribute):

    tempListTrace = []
    satVisible = satsVisibleToRec_res(site)

    if (sat == "ALL"):
        for satellite in satVisible:
            tempListTrace = tempListTrace   + get_trace_res(graph_type, site, str(satellite), groupby, attribute)
    else:
        if (sat not in satVisible):
            return get_empty_graph("Select  Satellite from the Dropdown")

        tempListTrace = tempListTrace       + get_trace_res(graph_type, site, sat, groupby, attribute)

    return tempListTrace




@app.callback(
    Output('res_graph', 'figure'),
    [
     Input('residuals_pg_dropdown_type',  'value'),
     Input('residuals_pg_dropdown_site',  'value'),
     Input('residuals_pg_dropdown_sat',   'value'),
     Input('residuals_pg_dropdown_xaxis', 'value'),
     Input('residuals_pg_dropdown_yaxis', 'value'),
    ])
def update_graph4(graph_type, site, sat, xaxis, yaxis):

    fig = go.Figure()

    if (graph_type is None or site is None or sat is None or xaxis is None or yaxis is None):

         return get_empty_graph("Make sure a value for all the Dropdown Menu is selected")

    if (graph_type =="LINE" or graph_type=="SCATTER" or graph_type=="POLAR"):

        mode = ""
        if (graph_type == "LINE"):
            mode = "lines"
        else:
            mode = "markers"

        tempListTrace = []

        if (site == "ALL"):
            for rec in getAllSites_res():
                tempListTrace = tempListTrace + add_site_res(graph_type, rec, sat, xaxis, yaxis)

        else:
            if site not in getAllSites_res():
                return get_empty_graph("Select  Site from the Dropdown")
            tempListTrace = tempListTrace + add_site_res(graph_type, site, sat, xaxis, yaxis)

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
                    return get_empty_graph("Select Satellite from the Dropdown")

                trace = get_trace(graph_type, site, sat, xaxis, yaxis)
                sliders = [dict(
                                active = 4,
                                currentvalue = {"prefix": "bin size: "},
                                pad = {"t": 20},
                                steps = [dict(label = i, method = 'restyle',  args = ['xbins.size', i]) for i in np.arange(0.25,15)]
                            )]
                fig = go.Figure()
                fig.add_trace_res(trace)

                fig.update_layout(
                    sliders=sliders
                )

                return fig

    else:
        return get_empty_graph("Select the GRAPH Type from the Dropdown")


