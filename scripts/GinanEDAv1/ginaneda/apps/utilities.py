"""Generic utilites for the EDA
"""
import numpy as np
from dash import dcc
from dash import html
import plotly.graph_objs as go


def _merge(origin, append):
    return dict(origin, **append)


def _omit(omitted_keys, dictionary):
    return {k: v for k, v in dictionary.items() if k not in omitted_keys}


def named_slider(name, **kwargs):
    """Generate name slider
    """
    return html.Div(
        # style={'padding': '10px 10px 15px 4px'},
        children=[
            html.P(f"{name}:"),
            html.Div(dcc.Slider(**kwargs), style={"margin-left": "3px"}),
        ]
    )


def named_dropdown(name, **kwargs):
    """Generate a Dropdown list
    """
    dropdown = []
    if name is not None:
        dropdown.append(html.P(f"{name}:", style={"margin-left": "3px"}))
    dropdown.append(dcc.Dropdown(**kwargs))
    return html.Div(dropdown)


def get_empty_graph(message=""):
    """Return an empty placeholder for the graphe with a message
    """
    emptiness = {
        "layout": {
            "xaxis": {"visible": False},
            "yaxis": {"visible": False},
            "annotations": [
                {
                    "text": message,
                    "xref": "paper",
                    "yref": "paper",
                    "showarrow": False,
                    "font": {"size": 28},
                }
            ],
        }
    }
    return emptiness


def generate_trace(graph_type, x_array, y_array, label, sublabel = ""):
    """Generate plotly traces
    """
    if graph_type == "Line" or graph_type == "Scatter":
        mode = "lines" if graph_type == "Line" else "markers"
        trace = go.Scatter(x=x_array, y=y_array, mode=mode, name=label,
                           hovertemplate =
                           '<i>y</i>: %{y:.4f}'+
                           '<br><b>x</b>: %{x}<br>'+
                           f'<b>{label}</b><br>'+
                           f'{sublabel}'
                           )
    elif graph_type == "POLAR":
        trace = go.Scatterpolar(r=y_array, theta=x_array, mode="markers", name=label)
    elif graph_type == "Fourier":
        transformed = np.absolute(np.fft.fft(y_array))
        trace = go.Scatter(y=transformed, mode="lines", name=label,
                           hovertemplate =
                           '<i>y</i>: %{y:.2f}'+
                           '<br><b>x</b>: %{x}<br>'+
                           f'<b>{label}</b>'
                           )
    elif graph_type == "HistogramX":
        trace = go.Histogram(x=y_array, name=label)
    elif graph_type == "HistogramY":
        trace = go.Histogram(y=y_array, name=label)
    return trace
