from flask import render_template, current_app, session

import numpy as np
import plotly.graph_objs as go
import plotly.io as pio

from backend.data.measurements import MeasurementArray, Measurements
from backend.dbconnector.mongo import MongoDB

extra = {}
extra["plotType"] = ["Line", "Scatter", "QQ"]
extra["posMode"] = ["XYZ", "ENU"]
extra["clockType"] = ["Site", "Satellite"]
extra["stateField"] = ["x", "dx", "P"]
extra["preprocess"] = ["None", "Fit", "Detrend"]
extra['degree'] = ["0", "1", "2"]

def init_page(template: str) -> str:
    """
    init Generate the empty page

    :return str: HTML Code
    """
    content = []
    return render_template(template, content=content, extra=extra, exlcude=0)



def generate_fig(trace):
    fig = go.Figure(data=trace)
    fig.update_layout(
        xaxis={"rangeslider":{"visible":True}, "showgrid":current_app.config["EDA_GRID"]},
        yaxis={"fixedrange":False, "tickformat":".3e", "showgrid":current_app.config["EDA_GRID"]},
        height=800,
        template=current_app.config["EDA_THEME"],
    )
    fig.layout.autosize = True
    return pio.to_html(fig)


def aggregate_stats(data: dict) -> dict:
    table_agg = {}
    try:
        for _data in data :
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
        current_app.logger.debug('not number operations')
        pass
    return table_agg


def get_data(db, collection, state, site, sat, series, yaxis, data, reshape_on=None):
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
                    data.append(Measurements.from_dictionary(req, reshape_on=reshape_on, database=db))
                except ValueError as err:
                    current_app.logger.warning(err)
                    continue   
        except ValueError as err:
            current_app.logger.warning(err)
            pass