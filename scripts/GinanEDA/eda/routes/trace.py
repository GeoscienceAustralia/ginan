import numpy as np

from flask import Blueprint, current_app, render_template, request, session
import plotly.graph_objs as go


from backend.dbconnector.mongo import MongoDB
from backend.data.measurements import MeasurementArray, Measurements
from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data, get_arbitrary
from . import eda_bp



@eda_bp.route("/advanced", methods=["GET", "POST"])
def trace():
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="trace.jinja")


def handle_post_request():
    form_data = request.form
    form = {}
    form["plot"]    = form_data.get("type")
    form["match"]   = form_data.get("match")
    form["group"]   = form_data.get("group")
    form["datax"]   = form_data.get("datax")
    form["datay"]   = form_data.get("datay")

    # current_app.logger.info(f"GET {form['plot']}, {form["selector1"]}, {form['selector2']}, {form['data1']}, {form['data2']}, exclude {form['exclude']} mintues" )
    
    current_app.logger.info("Getting Connection")
    data = MeasurementArray()
    # for series in form["series"] :
        # db_, series_ = series.split("\\")
    db="ex201"
    data = get_arbitrary(db, "Trace", form["match"], form["group"], form["datax"], form["datay"])

    if len(data) == 0:
        return render_template(
            "trace.jinja",
            # content=client.mongo_content,
            extra=extra,
            message="Error getting data: No data")

    if "excessFields" in data:
        return render_template(
            "trace.jinja",
            # content=client.mongo_content,
            extra=extra,
            message="Excess data received, try grouping by one or more of: " + str(data["excessFields"]))

    mode = "markers" if form["plot"] == "Scatter" else "lines"
    if form["plot"] == "QQ":
        data.compute_qq()
        mode = "markers"
    traces = []
    table = {}
    current_app.logger.warning("starting plots")


    for label, traceData in data.items():
        print("trace:" + label)
        traceX = []
        traceY = []
        # print("data: " + str(traceData))
        for element in traceData:
            traceX.append(element["x"][0])
            traceY.append(element["y"][0])

        x_hover_template = "%{x}<br>"
        traces.append(
            go.Scatter(
                x=traceX,
                y=traceY,
                mode=mode,
                name=f"{label}",
                hovertemplate=x_hover_template + "%{y:.4e%}<br>" + f"{label}",
            )
        )

    current_app.logger.warning("end plots")       

    return render_template(
        "trace.jinja",
        # content=client.mongo_content,
        extra=extra,
        graphJSON=generate_fig(traces),
        mode="plotly",
        selection=form,
        # table_data=table,
        # table_headers=["RMS", "mean"],
        # tableagg_data=table_agg,
        # tableagg_headers=["RMS", "mean"],
    )
