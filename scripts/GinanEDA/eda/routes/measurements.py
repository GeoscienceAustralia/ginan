import numpy as np

from flask import Blueprint, current_app, render_template, request, session
import plotly.graph_objs as go


from backend.dbconnector.mongo import MongoDB
from backend.data.measurements import MeasurementArray, Measurements
from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data, extract_database_series
from . import eda_bp


@eda_bp.route("/measurements", methods=["GET", "POST"])
def measurements():
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="measurements.jinja")


def handle_post_request():
    form_data = request.form
    form = {}
    form["plot"] = form_data.get("type")
    form["series"] = form_data.getlist("series")
    form["sat"] = form_data.getlist("sat")
    form["site"] = form_data.getlist("site")
    form["xaxis"] = form_data.get("xaxis")
    form["yaxis"] = form_data.getlist("yaxis")
    form["exclude"] = form_data.get("exclude")
    if form["exclude"] == "":
        form["exclude"] = 0
    else:
        form["exclude"] = int(form["exclude"])
    form["exclude_tail"] = form_data.get("exclude_tail")
    if form["exclude_tail"] == "":
        form["exclude_tail"] = 0
    else:
        form["exclude_tail"] = int(form["exclude_tail"])

    current_app.logger.info(
        f"GET {form['plot']}, {form['series']}, {form['sat']}, {form['site']}, {form['xaxis']}, {form['yaxis']}, {form['yaxis']+[form['xaxis']]}, exclude {form['exclude']} mintues"
    )
    session["measurements"] = form
    current_app.logger.info("Getting Connection")
    data = MeasurementArray()
    data2 = MeasurementArray()
    for series in form["series"]:
        db_, series_ = extract_database_series(series)
        get_data(db_, "Measurements", None, form["site"], form["sat"], [series_], form["yaxis"] + [form["xaxis"]], data)
        if any([yaxis in session["list_geometry"] for yaxis in form["yaxis"] + [form["xaxis"]]]):
            get_data(db_, "Geometry", None, form["site"], form["sat"], [""], form["yaxis"] + [form["xaxis"]], data2)

    if len(data.arr) + len(data2.arr) == 0:
        return render_template(
            "measurements.jinja",
            # content=client.mongo_content,
            selection=session["measurements"],
            extra=extra,
            message="Error getting data: No data",
        )

    # try:
    if len(data.arr) == 0 :
        data = data2
    else:
        data.merge(data2)
    # except Exception as e:
    # current_app.logger.warning(f"Merging error data {e}")
    # pass

    # exit(0)

    data.sort()
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"])
    for data_ in data:
        data_.find_gaps()
    data.get_stats()
    mode = "markers" if form["plot"] == "Scatter" else "lines"
    if form["plot"] == "QQ":
        data.compute_qq()
        mode = "markers"
    trace = []
    table = {}
    current_app.logger.warning("starting plots")
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

    table_agg = aggregate_stats(data)

    return render_template(
        "measurements.jinja",
        # content=client.mongo_content,
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=session["measurements"],
        table_data=table,
        table_headers=["RMS", "mean"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
