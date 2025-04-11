import numpy as np
import plotly.graph_objs as go
from flask import Blueprint, current_app, render_template, request, session

from backend.data.measurements import MeasurementArray, Measurements
from backend.dbconnector.mongo import MongoDB

from ..utilities import extra, init_page, generate_fig, aggregate_stats, get_data, extract_database_series
from . import eda_bp


@eda_bp.route("/states_diff", methods=["GET", "POST"])
def states_diff():
    if request.method == "POST":
        return handle_post_request()
    else:
        template = "states_diff.jinja"
        return init_page(template=template)


@eda_bp.route("/states", methods=["GET", "POST"])
def states():
    if request.method == "POST":
        return handle_post_request()
    else:
        template = "states.jinja"
        return init_page(template=template)


def log_and_set_session(form, session_key):
    current_app.logger.info(
        f"GET {form['type']}, {form['series']}, {form['sat']}, {form['site']}, {form['state']}, {form['xaxis']}, {form['yaxis']}, "
        f"{form['yaxis']+[form['xaxis']]}, exclude {form['exclude']} minutes"
    )
    session[session_key] = form
    current_app.logger.info("Getting Connection")


def retrieve_data(form):
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
    return data, data2


def process_data(data, data2, form):
    if len(data.arr) == 0:
        return None, "Error getting data: No data"

    data.merge(data2)
    data.sort()
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"])
    for data_ in data:
        data_.find_gaps()
    data.get_stats()
    return data, None


def generate_plots(data, form):
    trace = []
    mode = "markers" if form["type"] == "Scatter" else "lines"
    table = {}
    if form["process"] == "Detrend":
        for _data in data:
            _data.detrend(degree=int(form["degree"]))
    if form["process"] == "Fit":
        for _data in data:
            _data.polyfit(degree=int(form["degree"]))

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
    return trace, table


def handle_post_request():
    form_data = request.form
    form = {
        "type": form_data.get("type"),
        "series": form_data.getlist("series"),
        "sat": form_data.getlist("sat"),
        "site": form_data.getlist("site"),
        "state": form_data.getlist("state"),
        "xaxis": form_data.get("xaxis"),
        "yaxis": form_data.getlist("yaxis"),
        "exclude": form_data.get("exclude", "0"),
        "exclude_tail": form_data.get("exclude_tail", "0"),
        "process": form_data.get("process"),
        "degree": form_data.get("degree"),
    }
    for label in ["exclude", "exclude_tail"]:
        form[label] = int(form[label]) if form[label] else 0

    session_key = "states_diff" if "series_base" in form_data else "states"
    template_name = "states_diff.jinja" if session_key == "states_diff" else "states.jinja"
    log_and_set_session(form, session_key)
    data, data2 = retrieve_data(form)
    data, error_message = process_data(data, data2, form)
    if error_message:
        return render_template(
            template_name,
            selection=session[session_key],
            extra=extra,
            message=error_message,
        )

    if session_key == "states_diff":
        form["series_base"] = [form_data.get("series_base")]
        req = form.copy()
        req["series"] = [form_data.get("series_base")]
        data_base, data2_base = retrieve_data(req)
        data_base, error_message = process_data(data_base, data2_base, form)
        list_keys = list(set(key for data_base_ in data_base.arr for key in data_base_.data.keys()))
        data.yaxis = list_keys
        data = data - data_base
        session[session_key] = form
        data.get_stats()
    else:
        session[session_key] = form

    trace, table = generate_plots(data, form)
    table_agg = aggregate_stats(data)

    return render_template(
        template_name,
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=session[session_key],
        table_data=table,
        table_headers=["RMS", "mean", "Fit"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
