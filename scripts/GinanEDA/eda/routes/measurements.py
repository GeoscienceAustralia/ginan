import numpy as np

from flask import Blueprint, current_app, render_template, request, session
import plotly.graph_objs as go


from backend.dbconnector.mongo import MongoDB
from backend.data.measurements import MeasurementArray, Measurements
from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data, extract_database_series
from . import eda_bp


@eda_bp.route("/measurements_diff", methods=["GET", "POST"])
def measurements_diff():
    if request.method == "POST":
        return handle_post_request()
    else:
        template = "measurements_diff.jinja"
        return init_page(template=template)


@eda_bp.route("/measurements", methods=["GET", "POST"])
def measurements():
    if request.method == "POST":
        return handle_post_request()
    else:
        template = "measurements.jinja"
        return init_page(template=template)


def log_and_set_session(form, session_key):
    current_app.logger.info(
        f"GET {form['plot']}, {form['series']}, {form['sat']}, {form['site']}, {form['xaxis']}, {form['yaxis']}, {form['yaxis']+[form['xaxis']]}, exclude {form['exclude']} minutes"
    )
    session[session_key] = form
    current_app.logger.info("Getting Connection")


def retrieve_data(form):
    data = MeasurementArray()
    data2 = MeasurementArray()
    for series in form["series"]:
        db_, series_ = extract_database_series(series)
        get_data(db_, "Measurements", None, form["site"], form["sat"], [series_], form["yaxis"] + [form["xaxis"]], data)
        if any([yaxis in session["list_geometry"] for yaxis in form["yaxis"] + [form["xaxis"]]]):
            get_data(db_, "Geometry", None, form["site"], form["sat"], [""], form["yaxis"] + [form["xaxis"]], data2)
    return data, data2


def process_data(data, data2, form):
    if len(data.arr) + len(data2.arr) == 0:
        return None, "Error getting data: No data"

    if len(data.arr) == 0:
        data = data2
    else:
        data.merge(data2)

    data.sort()
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"])
    for data_ in data:
        data_.find_gaps()
    data.get_stats()
    return data, None


def generate_plots(data, form):
    mode = "markers" if form["plot"] == "Scatter" else "lines"
    if form["plot"] == "QQ":
        data.compute_qq()
        mode = "markers"
    trace = []
    table = {}
    current_app.logger.debug("starting plots")
    for _data in data:
        for _yaxis in form["yaxis"]:
            try:
                if form["xaxis"] == "Epoch":
                    _x = _data.epoch[_data.subset]
                    x_hover_template = "%{x|%Y-%m-%d %H:%M:%S}<br>"
                else:
                    _x = _data.data[form["xaxis"]][_data.subset]
                    x_hover_template = "%{x}<br>"
                if _yaxis in _data.data:
                    if form["plot"] == "QQ":
                        _x = _data.info[_yaxis]["qq"][1]
                        _y = _data.info[_yaxis]["qq"][0]
                        x_hover_template = "%{x}<br>"
                    else:
                        _y = _data.data[_yaxis][_data.subset]
                    if isinstance(_y[0], float) and np.sum(~np.isnan(_y)) == 0:
                        continue
                    legend = _data.id
                    legend["yaxis"] = _yaxis
                    smallLegend = [legend[a] for a in legend]
                    trace.append(
                        go.Scatter(
                            x=_x,
                            y=_y,
                            mode=mode,
                            name=f"{smallLegend}",
                            hovertemplate=x_hover_template + "%{y:.4e%}<br>" + f"{smallLegend}",
                        )
                    )
                    try:
                        table[f"{legend}"] = {"mean": _data.info[_yaxis]["mean"], "RMS": _data.info[_yaxis]["rms"]}
                    except:
                        pass
            except Exception as e:
                current_app.logger.warning(f"Error plotting {_data.id} {form['xaxis']}{_yaxis} {e}")
    current_app.logger.warning("end plots")
    return trace, table


def handle_post_request():
    form_data = request.form
    form = {
        "plot": form_data.get("type"),
        "series": form_data.getlist("series"),
        "sat": form_data.getlist("sat"),
        "site": form_data.getlist("site"),
        "xaxis": form_data.get("xaxis"),
        "yaxis": form_data.getlist("yaxis"),
        "exclude": form_data.get("exclude", "0"),
        "exclude_tail": form_data.get("exclude_tail", "0"),
    }
    for label in ["exclude", "exclude_tail"]:
        form[label] = int(form[label]) if form[label] else 0

    session_key = "measurements_diff" if "series_base" in form_data else "measurements"
    template_name = "measurements_diff.jinja" if session_key == "measurements_diff" else "measurements.jinja"
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

    if session_key == "measurements_diff":
        form["series_base"] = [form_data.get("series_base")]
        req = form.copy()
        req["series"] = [form_data.get("series_base")]
        data_base, data2_base = retrieve_data(req)
        data_base, error_message = process_data(data_base, data2_base, form)
        print("getting base data", form_data.get("series_base"))
        print({"series": [form_data.get("series_base")], **form})
        data.yaxis = form["yaxis"]
        data = data - data_base
        session[session_key] = form
        data.get_stats()
    else:
        session[session_key] = form
    print(session[session_key])
    trace, table = generate_plots(data, form)
    table_agg = aggregate_stats(data)

    return render_template(
        template_name,
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=session[session_key],
        table_data=table,
        table_headers=["RMS", "mean"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
