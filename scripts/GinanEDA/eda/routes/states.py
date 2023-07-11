import numpy as np
import plotly.graph_objs as go
from flask import Blueprint, current_app, render_template, request, session

from backend.data.measurements import MeasurementArray, Measurements
from backend.dbconnector.mongo import MongoDB

from ..utilities import extra, init_page, generate_fig, aggregate_stats, get_data
from . import eda_bp


@eda_bp.route("/states", methods=["GET", "POST"])
def states():
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="states.jinja")


def handle_post_request() -> str:
    """
    handle_post_request Code to process the POST request and generate the HTML code

    :return str: webpage code
    """
    current_app.logger.info("Entering request")
    form_data = request.form
    form = {}
    form["type"] = form_data.get("type")
    form["series"] = form_data.getlist("series")
    form["sat"] = form_data.getlist("sat")
    form["site"] = form_data.getlist("site")
    form["state"] = form_data.getlist("state")
    form["xaxis"] = "Epoch"
    form["yaxis"] = form_data.getlist("yaxis")
    form["exclude"] = form_data.get("exclude")
    form["process"] = form_data.get("process")
    form["degree"] = form_data.get("degree")
    if form["exclude"] == "":
        form["exclude"] = 0
    else:
        form["exclude"] = int(form["exclude"])

    current_app.logger.info(
        f"GET {form['type']}, {form['series']}, {form['sat']}, {form['site']}, {form['state']}, {form['xaxis']}, {form['yaxis']}, "
        f"{form['yaxis']+[form['xaxis']]}, exclude {form['exclude']} mintues"
    )
    
    data = MeasurementArray()

    for series in form["series"] :
        db_, series_ = series.split("\\")
        get_data(db_, "States", form["state"], form["site"], form["sat"], [series_], form["yaxis"] + [form["xaxis"]] + ["Num"], data, reshape_on="Num")
        # with MongoDB(session["mongo_ip"], data_base=db_, port=session["mongo_port"]) as client:
        #     try:
        #         for req in client.get_data(
        #         "States",
        #         form["state"],
        #         form["site"],
        #         form["sat"],
        #         [series_],
        #         form["yaxis"] + [form["xaxis"]] + ["Num"],
        #         ): 
        #             try:
        #                 data.append(Measurements.from_dictionary(req, reshape_on="Num", database=db_))     
        #             except ValueError as err:
        #                 current_app.logger.warning(err)
        #                 continue   
        #     except ValueError as err:
        #         current_app.logger.error(err)
        #         continue
    if len(data.arr) == 0:
        return render_template(
            "states.jinja",
            # content=client.mongo_content,
            extra=extra,
            message="Error getting data: No data",
        )
    data.sort()
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=None)
    trace = []
    mode = "markers" if form["type"] == "Scatter" else "lines"
    table = {}
    if form["process"] == "Detrend":
        for _data in data:
            _data.detrend(degree=int(form["degree"]))
    if form["process"] == "Fit":
        for _data in data:
            _data.polyfit(degree=int(form["degree"]))
    
    data.get_stats()
    for _data in data:
        for _yaxis in _data.data:
            _data.id["state"] = _yaxis
            if np.isnan(_data.data[_yaxis][_data.subset]).any():
                current_app.logger.warning(f"Nan detected for {_data.id}")
                current_app.logger.warning(np.argwhere(np.isnan(_data.data[_yaxis][_data.subset])))
            trace.append(
                go.Scatter(
                    x=_data.epoch[_data.subset],
                    y=_data.data[_yaxis][_data.subset],
                    mode=mode,
                    name=f"{_data.id}",
                    hovertemplate="%{x|%Y-%m-%d %H:%M:%S}<br>" + "%{y:.4e%}<br>" + f"{_data.id}",
                )
            )
            table[f"{_data.id}"] =  {"mean": _data.info[_yaxis]["mean"],
                                        "RMS": _data.info[_yaxis]["rms"]}
            if any(keyword in form["process"] for keyword in ["Detrend", "Fit"]):
                table[f"{_data.id}"]["Fit"] = np.array2string(_data.info["Fit"][_yaxis][::-1], precision=2, separator=", ")
                
    table_agg = aggregate_stats(data)
     
    return render_template(
        "states.jinja",
        # content=client.mongo_content,
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=form,
        table_data=table,
        table_headers=["RMS", "mean", "Fit"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
