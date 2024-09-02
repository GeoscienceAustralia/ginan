from flask import render_template, current_app, session

import numpy as np
from plotly.subplots import make_subplots
import plotly.graph_objs as go
import plotly.io as pio

from backend.data.measurements import MeasurementArray, Measurements
from backend.dbconnector.mongo import MongoDB

extra = {}
extra["plotType"] = ["Line", "Scatter", "QQ"]
extra["filterType"] = ["None", "LPF", "HPF", "DIFF", "DIFF2"]
extra["posMode"] = ["XYZ", "ENU"]
extra["clockType"] = ["Site", "Satellite"]
extra["stateField"] = ["x", "dx", "sigma"]
extra["stateField"] = ["x", "dx", "sigma"]
extra["preprocess"] = ["None", "Fit", "Detrend"]
extra["degree"] = ["0", "1", "2"]
extra["clockModes"] = ["Series", "Epoch"]
extra["orbitType"] = ["Residual RTN", "Residual ECI"]
extra["positionRef"] = ["All", "1st Epoch", "WMean"]


def init_page(template: str) -> str:
    """
    init Generate the empty page

    :return str: HTML Code
    """
    content = []
    return render_template(template, content=content, extra=extra, exlcude=0, selection=session[template.split(".")[0]])

def initialize_session():
    if not session.get('session_initialized'):
        session['measurements'] = {
            "plot": "Line",
            "series": [],
            "site": [],
            "sat": [],
            "xaxis": "Epoch"
        }
        session["states"] = {
            "type": "Line",
            "series": [],
            "site": [],
            "sat": [],
            "xaxis": "Epoch",
            "yaxis": "x",
            "degree": "0",
            "process": "None",
        }
        session["position"] = {
            "type": "Line",
            "series": [],
            "series_base": "",
        }
        session["clocks"] = {
            "series": "",
            "series_base": "",
            "subset": [],
            "modes": [],
            "clockType": ""
        }
        session["orbits"] = {
            "orbitType": "",
            "series": [],
            "sat": [],
        }
        session['session_initialized'] = True

def generate_fig(trace):
    fig = go.Figure(data=trace)
    fig.update_layout(
        xaxis={"rangeslider": {"visible": True}, "showgrid": current_app.config["EDA_GRID"]},
        yaxis={"fixedrange": False, "tickformat": ".3e", "showgrid": current_app.config["EDA_GRID"]},
        height=800,
        template=current_app.config["EDA_THEME"],
    )
    fig.layout.autosize = True
    return pio.to_html(fig)


def generate_figs(traces):
    fig = make_subplots(rows=max(1,len(traces)), cols=1,
                    shared_xaxes=True,
                    vertical_spacing=0.2
                    )
    for i in range( len(traces)):
        for trace in traces[i]:
            fig.add_trace(trace, row=i + 1, col=1)

    fig.update_layout(
        xaxis={"rangeslider":{"visible":True}, "showgrid":current_app.config["EDA_GRID"]},
        yaxis={"fixedrange":False, "tickformat":".3e", "showgrid":current_app.config["EDA_GRID"]},
        height=1200,
        # template=current_app.config["EDA_THEME"],
    )
    fig.layout.autosize = True
    return pio.to_html(fig)


def aggregate_stats(data: dict) -> dict:
    table_agg = {}
    try:
        for _data in data:
            series_ = _data.id["series"]
            db_ = _data.id["db"]
            for _yaxis in _data.data:
                name = f"{db_} {series_} {_yaxis}"
                if name not in table_agg:
                    table_agg[name] = {"mean": 0, "RMS": 0, "len": 0, "count": 0}
                table_agg[name]["mean"] += _data.info[_yaxis]["mean"]
                table_agg[name]["RMS"] += _data.info[_yaxis]["sumsqr"]
                table_agg[name]["len"] += _data.info[_yaxis]["len"]
                table_agg[name]["count"] += 1

        for _name, _tab in table_agg.items():
            _tab["mean"] /= _tab["count"]
            _tab["RMS"] = np.sqrt(_tab["RMS"] / _tab["len"])
    except:
        current_app.logger.debug("not number operations")
        pass
    return table_agg


def get_data(db, collection, state, site, sat, series, yaxis, data, reshape_on=None, exclude=""):
    """
    Get data from the database

    :param db: Database name
    :param collection: Collection name
    :param state: State name
    :param site: Site name
    :param sat: Satellite name
    :param series: Series name
    :param yaxis: Y axis name
    :return MeasurementArray: MeasurementArray object
    """
    # print (session)
    with MongoDB(session["mongo_ip"], data_base=db, port=session["mongo_port"]) as client:
        try:
            for req in client.get_data(
                collection,
                state,
                site,
                sat,
                series,
                yaxis,
            ):
                try:
                    data.append(Measurements.from_dictionary(req, reshape_on=reshape_on, database=db, exclude=exclude))
                except ValueError as err:
                    current_app.logger.warning(err)
                    continue
        except ValueError as err:
            current_app.logger.warning(err)
            pass


def get_keys_from_sub(ip, port, db, coll, element):
    with MongoDB(ip, data_base=db, port=port) as client:
        try:
            return client.get_keys_from_sub(coll, element)
        except ValueError as err:
            print("thing1")
            current_app.logger.warning(err)
            pass


def get_arbitrary(ip, port, db, coll, match, group, datay, reshape_on=None):
    with MongoDB(ip, data_base=db, port=port) as client:
        try:
            return client.get_arbitrary(coll, match, group, datay)
        except ValueError as err:
            print("thing2")
            print(match)
            print(group)
            current_app.logger.warning(err)
            pass


def get_distinct_vals(ip, port, db, coll, element, reshape_on=None):
    with MongoDB(ip, data_base=db, port=port) as client:
        try:
            return client.get_distinct_vals(coll, element)
        except ValueError as err:
            print("thing3")
            current_app.logger.warning(err)
            pass

def extract_database_series(series):
    db_, series_ = series.split("\\")
    return db_,series_