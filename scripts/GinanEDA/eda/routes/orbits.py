import numpy as np

from flask import Blueprint, current_app, render_template, request, session
import plotly.graph_objs as go


from backend.dbconnector.mongo import MongoDB
from backend.data.measurements import MeasurementArray, Measurements
from backend.data.satellite import Satellite
from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data, extract_database_series
from . import eda_bp


@eda_bp.route("/orbits", methods=["GET", "POST"])
def orbits():
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="orbits.jinja")


def handle_post_request():
    form_data = request.form
    form = {}
    form["orbitType"] = form_data.get("orbitType")
    form["series"] = form_data.getlist("series")
    form["sat"] = form_data.getlist("sat")
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
    session["orbits"] = form
    current_app.logger.info(
        f"GET {form['orbitType']}, {form['series']}, {form['sat']},exclude {form['exclude']} minutes"
    )
    data = MeasurementArray()
    for series in form["series"]:
        db_, series_ = extract_database_series(series)
        with MongoDB(session["mongo_ip"], data_base=db_, port=session["mongo_port"]) as client:
            for sat in form["sat"]:
                try:
                    s = Satellite.process(client, sat=sat, series=series_, mode=form["orbitType"])
                    data.append(s.to_measurement())
                except Exception as e:
                    current_app.logger.warning("something wrong...", str(e))
    type = "lines"
    trace = []
    data.sort()
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"])
    data.get_stats()
    table = {}
    for meas in data.arr:
        print(meas)
        for _data in meas.data:
            legend = meas.id
            legend["yaxis"] = _data
            smallLegend = [legend[a] for a in legend]
            trace.append(
                go.Scatter(
                    x=meas.epoch[meas.subset],
                    y=meas.data[_data][meas.subset],
                    mode=type,
                    name=f"{smallLegend}",
                    hovertemplate="%{x|%Y-%m-%d %H:%M:%S}<br>" + "%{y:.4e%}<br>",
                )
            )
            try:
                table[f"{legend}"] = {"mean": meas.info[_data]["mean"], "RMS": meas.info[_data]["rms"]}
            except Exception as e:
                current_app.logger.warning("Exception has occured", str(e))
                pass
        # for i in range(3):
        #     print(s.rac[:, i])
        #     trace.append(
        #         go.Scatter(
        #             x= s.time,
        #             y= s.rac[:, i],
        #             mode=type,
        #             name=f"{i}",
        #             hovertemplate="%{x|%Y-%m-%d %H:%M:%S}<br>" + "%{y:.4e%}<br>" ,
        #         )
        #     )
        # table[f"{_data.id}"] =  {"mean": _data.info[_yaxis]["mean"],
        #                          "RMS": _data.info[_yaxis]["rms"]}
    table_agg = aggregate_stats(data)
    fig = go.Figure(data=trace)
    fig.update_layout(
        xaxis=dict(rangeslider={"visible": True}),
        yaxis=dict(fixedrange=False, tickformat=".3e"),
        height=600,
    )
    return render_template(
        "orbits.jinja",
        graphJSON=generate_fig(trace),
        mode="plotly",
        selection=session["orbits"],
        # content=client.mongo_content,
        extra=extra,
        table_data=table,
        table_headers=["RMS", "mean"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
