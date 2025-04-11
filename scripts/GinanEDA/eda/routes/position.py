import numpy as np
import plotly.graph_objs as go
from flask import Blueprint, current_app, render_template, request, session

from backend.data.measurements import MeasurementArray, Measurements
from backend.data.position import Position
from backend.dbconnector.mongo import MongoDB

from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data, extract_database_series
from . import eda_bp


@eda_bp.route("/position", methods=["GET", "POST"])
def position() -> str:
    """
    Overall handeling of the page.

    :return str: HTML code
    """
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="position.jinja")


def handle_post_request() -> str:
    current_app.logger.info("Entering request")
    form_data = request.form
    form = {
        "type": form_data.get("type"),
        "series": form_data.getlist("series"),
        "series_base": form_data.get("series_base"),
        "exclude": form_data.get("exclude"),
    }
    if form["exclude"] == "":
        form["exclude"] = 0
    else:
        form["exclude"] = int(form["exclude"])

    form["exclude_tail"] = form_data.get("exclude_tail")
    if form["exclude_tail"] == "":
        form["exclude_tail"] = 0
    else:
        form["exclude_tail"] = int(form["exclude_tail"])

    form["mode"] = form_data.get("mode")
    form["ref"] = form_data.get("ref")
    form["site"] = form_data.getlist("site")
    session['position'] = form
    data = MeasurementArray()
    base = MeasurementArray()
    for series in form["series"]:
        series_base = form["series_base"]
        db_, series_ = extract_database_series(series)
        db_base_, series_base_ = extract_database_series(series_base)
        try:
            get_data(
                db_,
                "States",
                ["REC_POS"],
                form["site"],
                [""],
                [series_],
                ["x"] + ["Epoch", "Num"],
                data,
                reshape_on="Num",
            )
            get_data(
                db_base_,
                "States",
                ["REC_POS"],
                form["site"],
                [""],
                [series_base_],
                ["x", "sigma"] + ["Epoch", "Num"],
                base,
                reshape_on="Num",
            )
        except Exception as err:
            current_app.logger.error(err)
            return render_template(
                "position.jinja",
                selection=session['position'],
                # content=client.mongo_content,
                extra=extra,
                message=f"Error getting data: {str(err)}",
            )
    data.difference_check = True
    position_vector = Position(data, base, ref=form["ref"])
    position_vector.data.find_minmax()
    position_vector.base.find_minmax()
    position_vector.data.adjust_slice(
        minutes_min=form["exclude"], minutes_max=form["exclude_tail"]
    )
    position_vector.base.adjust_slice(
        minutes_min=form["exclude"], minutes_max=form["exclude_tail"]
    )
    position_vector.calculate()
    if form["mode"] == "ENU":
        position_vector.rotate_enu()
    position_vector.data.sort()

    position_vector.data.get_stats()
    trace = []
    table = {}
    type = "markers+lines" if form["type"] == "Scatter" else "lines"
    for _data in position_vector:
        for _yaxis in _data.data:
            _data.id["state"] = _yaxis
            smallLegend = [_data.id[a] for a in _data.id]
            trace.append(
                go.Scatter(
                    x=_data.epoch[_data.subset],
                    y=_data.data[_yaxis][_data.subset],
                    mode=type,
                    name=f"{smallLegend}",
                    hovertemplate="%{x|%Y-%m-%d %H:%M:%S}<br>"
                    + "%{y:.4e%}<br>"
                    + f"{_data.id}",
                )
            )
            table[f"{_data.id}"] = {
                "mean": _data.info[_yaxis]["mean"],
                "RMS": _data.info[_yaxis]["rms"],
            }
    fig = go.Figure(data=trace)
    fig.update_layout(
        xaxis=dict(rangeslider={"visible": True}),
        yaxis=dict(fixedrange=False, tickformat=".3e"),
        height=600,
    )
    table_agg = aggregate_stats(position_vector)

    return render_template(
        "position.jinja",
        # content=client.mongo_content,
        extra=extra,
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=session['position'],
        table_data=table,
        table_headers=["RMS", "mean"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )


