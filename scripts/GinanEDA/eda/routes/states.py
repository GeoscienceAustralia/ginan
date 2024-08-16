import numpy as np
import plotly.graph_objs as go
from flask import Blueprint, current_app, render_template, request, session

from backend.data.measurements import MeasurementArray, Measurements
from backend.dbconnector.mongo import MongoDB

from ..utilities import extra, init_page, generate_fig, aggregate_stats, get_data, extract_database_series
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
    form = {
        "type": form_data.get("type"),
        "series": form_data.getlist("series"),
        "sat": form_data.getlist("sat"),
        "site": form_data.getlist("site"),
        "state": form_data.getlist("state"),
        "xaxis": form_data.get("xaxis"),
        "yaxis": form_data.getlist("yaxis"),
        "exclude": form_data.get("exclude"),
        "exclude_tail": form_data.get("exclude_tail"),
        "process": form_data.get("process"),
        "degree": form_data.get("degree"),
    }

    if form["exclude"] == "":
        form["exclude"] = 0
    else:
        form["exclude"] = int(form["exclude"])

    if form["exclude_tail"] == "":
        form["exclude_tail"] = 0
    else:
        form["exclude_tail"] = int(form["exclude_tail"])

    current_app.logger.info(
        f"GET {form['type']}, {form['series']}, {form['sat']}, {form['site']}, {form['state']}, {form['xaxis']}, {form['yaxis']}, "
        f"{form['yaxis']+[form['xaxis']]}, exclude {form['exclude']} minutes"
    )
    session["states"] = form
    data = MeasurementArray()
    data2 = MeasurementArray()
    for series in form["series"]:
        db_, series_ = extract_database_series(series)
        get_data(
            db_,
            "States",
            form["state"],
            form["site"],
            form["sat"],
            [series_],
            form["yaxis"] + [form["xaxis"]] + ["Num"],
            data,
            reshape_on="Num",
            exclude=form["xaxis"],
        )
        if any([yaxis in session["list_geometry"] for yaxis in form["yaxis"] + [form["xaxis"]]]):
            get_data(db_, "Geometry", None, form["site"], form["sat"], [""], [form["xaxis"]], data2)

    if len(data.arr) == 0:
        return render_template(
            "states.jinja",
            # content=client.mongo_content,
            selection=session["states"],
            extra=extra,
            message="Error getting data: No data",
        )

    data.merge(data2)
    data.sort()
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"])
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
            if _yaxis != form["xaxis"]:
                _data.id["state"] = _yaxis
                if form["xaxis"] == "Epoch":
                    _x = _data.epoch[_data.subset]
                    x_hover_template = "%{x|%Y-%m-%d %H:%M:%S}<br>%{y:.9e%}<br>"
                else:
                    _x = _data.data[form["xaxis"]][_data.subset]
                    x_hover_template = "%{x}<br>"
                if np.isnan(_data.data[_yaxis][_data.subset]).any():
                    current_app.logger.warning(f"Nan detected for {_data.id}")
                    current_app.logger.warning(np.argwhere(np.isnan(_data.data[_yaxis][_data.subset])))
                smallLegend = [_data.id[a] for a in _data.id]
                trace.append(
                    go.Scatter(
                        x=_x,
                        y=_data.data[_yaxis][_data.subset],
                        mode=mode,
                        name=f"{smallLegend}",
                        hovertemplate=x_hover_template + "%{y:.4e%}<br>" + f"{smallLegend}",
                    )
                )
                table[f"{_data.id}"] = {"mean": _data.info[_yaxis]["mean"], "RMS": _data.info[_yaxis]["rms"]}
                if any(keyword in form["process"] for keyword in ["Detrend", "Fit"]):
                    table[f"{_data.id}"]["Fit"] = np.array2string(
                        _data.info["Fit"][_yaxis][::-1], precision=2, separator=", "
                    )

    table_agg = aggregate_stats(data)

    return render_template(
        "states.jinja",
        # content=client.mongo_content,
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=session["states"],
        table_data=table,
        table_headers=["RMS", "mean", "Fit"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
