import numpy as np
import plotly.graph_objs as go
from flask import current_app, render_template, request, session

from backend.dbconnector.mongo import MongoDB
from backend.data.clocks import Clocks
from backend.data.measurements import MeasurementArray, Measurements
from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data
from . import eda_bp


@eda_bp.route("/clocks", methods=["GET", "POST"])
def clocks():
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="clocks.jinja")


def handle_post_request():
    current_app.logger.info("Entering request")
    form_data = request.form
    form = {}
    form["series"] = form_data.get("series")
    form["series_base"] = form_data.get("series_base")
    form["subset"] = form_data.getlist("subset")
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

    form["clockType"] = form_data.get("clockType")

    db_, series_ = form["series"].split("\\")
    db_2, series_2 = form["series_base"].split("\\")
    if db_ != db_2:
        return render_template(
            "clocks.jinja",
            # content=client.mongo_content,
            extra=extra,
            message=f"Error getting data: Can only compare series from the same database",
        )
    if form["clockType"] == "Satellite":
        state = ["SAT_CLOCK"]
        site_list = [""]
        sat_list = form["subset"]
    elif form["clockType"] == "Site":
        state = ["REC_CLOCK"]
        sat_list = ["", "G--"]
        site_list = form["subset"]
    data = MeasurementArray()
    try:
        get_data(db_, "States", state, site_list, sat_list, [series_] + [series_2], ["x"], data)
    except Exception as err:
        current_app.logger.error(err)
        return render_template(
            "clocks.jinja",
            extra=extra,
            message=f"Error getting data: {str(err)}",
        )
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"], trim=True)

    if form["clockType"] == "Satellite":
        clocks = Clocks(data, satlist=sat_list, series=series_, series_base=series_2)
    else:
        clocks = Clocks(data, sitelist=site_list, series=series_, series_base=series_2)
    trace = []
    result = clocks.process()
    result.sort()
    result.find_minmax()
    # result.adjust_slice(minutes_min=form["exclude"], minutes_max=None)
    result.get_stats()
    table = {}
    for _clock in result:
        trace.append(
            go.Scatter(
                x=_clock.epoch[_clock.subset],
                y=_clock.data["x"][_clock.subset],
                mode="lines",
                name=f"{_clock.id}",
                hovertemplate="%{x|%Y-%m-%d %H:%M:%S}<br>" + "%{y:.4e%}<br>" + f"{_clock.id}",
            )
        )
        current_app.logger.debug(_clock.info)
        table[f"{_clock.id}"] = {"mean": _clock.info["x"]["mean"], "RMS": _clock.info["x"]["rms"]}

    table_agg = aggregate_stats(result)

    return render_template(
        "clocks.jinja",
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=form,
        table_data=table,
        table_headers=["RMS", "mean"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
