
import operator
from app import app
from layouts.different_graphs   import res_graph_layout
from layouts.different_graphs   import sol_graph_layout

from dash.dependencies          import Input, Output

import dash_bootstrap_components    as dbc
import dash_html_components         as html
import plotly.express               as px
import plotly.graph_objects         as go
import pdb
import numpy                        as np

"""
@ app.callback()
    ************************************************************************************************************************************
    * @brief CALLBACK :It deals with updating the sidebar. This callback defines the logic to highlight the sidebar which is selected  *
                       by the user. This is a common callback for all the pages.                                                       *
    * @param Output (Equivalent to Return statement):                                                                                  *
    *        "active" : Which of the sidebar options in active(selected).                                                              *
    *                                                                                                                                  *
    * @param Input:                                                                                                                    *
    *        url      :Url of the current page entered in the browser.                                                                 *
    *        pathname :Value contained in the url.                                                                                     *
    *                                                                                                                                  *
*   ************************************************************************************************************************************
@ toggle_active_links()
    ************************************************************************************************************
    * @brief Updates the graph on the page(pathname == "/page-2"), based on the values of DropDown attributes. *
    * @ pathname: Alias for the the `pathname` in  reciever_pg_dropdown_1                                      *
    * @ attribute: Alias for the `value` in reciever_pg_dropdown_2                                             *
    * @return Sidebar: Showing the active sidebar option.                                                      *                                                                                                                          *
    ************************************************************************************************************
"""
@app.callback(
    [Output(f"page-{i}-link", "active") for i in [1,4]],
    [Input("url", "pathname")],
)
def toggle_active_links(pathname):
    if pathname == "/":
        # Treat page 1 as the homepage / index
        return True, False
    return [pathname == f"/page-{i}" for i in [1,4]]


@app.callback(
    Output("page-content", "children"), 
    [Input("url", "pathname")])
def render_page_content(pathname):

    if pathname in ["/", "/page-1"]:
        return sol_graph_layout

    if pathname == "/page-4":
        return res_graph_layout

    # If the user tries to reach a different page, return a 404 message
    return dbc.Jumbotron(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognised..."),
        ])

