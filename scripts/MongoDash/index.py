"""
Entry point of this application, specificall __name__==main() gets executed
"""
import dash
import dash_bootstrap_components    as dbc
import dash_core_components         as dcc
import dash_html_components         as html
import pdb
from layouts.sidebar_comp   import sidebar, content
from callbacks              import all_callbacks
from app                    import app
import argparse
import logging
import pymongo


"""
Entry page layout
TODO: Move to some othe layout ?
"""
app.layout = html.Div([
    html.Div([
        dcc.Location(id="url"),
        content,
        sidebar
    ]),
])


"""
 * @brief            : Function to parse the command line arguments. At present this function is called in data/helperfunc2.py
 * @return           : void
"""
def get_params():
    class MyArgumentParser(argparse.ArgumentParser):
        def convert_arg_line_to_args(self, arg_line):
            return arg_line.split()

    parser = MyArgumentParser(fromfile_prefix_chars='@')

    # Dataset parameters
    parser.add_argument('-data', type = str,help = "Give the data path" )
    

   

    return  parser.parse_args()

#Making it a global variable



if __name__ == '__main__':
    # For Development only, otherwise use gunicorn or uwsgi to launch, e.g.
    # gunicorn -b 0.0.0.0:8050 index:app.server
    prod = False



    myclient        = pymongo.MongoClient("mongodb://localhost:27017/")
    dblist          = myclient.list_database_names()

    print(dblist)

    if(prod):
        app.run_server(
        port=8050,
        host='0.0.0.0'
    )
    else:
        app.run_server(debug=True)
    