import numpy as np

from flask import Blueprint, current_app, render_template, request, session
import plotly.graph_objs as go


from backend.dbconnector.mongo import MongoDB
from backend.data.measurements import MeasurementArray, Measurements
from ..utilities import init_page, extra, generate_fig, generate_figs, aggregate_stats, get_data, get_arbitrary
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
    form["type"]    = form_data.get("type")
    form["fCoeff"]  = form_data.get("fCoeff")
    form["filter"]  = form_data.get("filter")
    form["match"]   = form_data.get("match")
    form["group"]   = form_data.get("group")
    form["datax"]   = form_data.get("datax")
    form["datay"]   = form_data.get("datay")

    # current_app.logger.info(f"GET {form['plot']}, {form["selector1"]}, {form['selector2']}, {form['data1']}, {form['data2']}, exclude {form['exclude']} mintues" )

    current_app.logger.info("Getting Connection")
    data = MeasurementArray()
    # for series in form["series"] :
        # db_, series_ = series.split("\\")
    db="ex202"
    data1 = get_arbitrary(db, "Trace", form["match"], form["group"], form["datax"])
    data2 = get_arbitrary(db, "Trace", form["match"], form["group"], form["datay"])

    if len(data1) == 0:
        return render_template(
            "trace.jinja",
            selection=form,
            # content=client.mongo_content,
            extra=extra,
            message="Error getting data: No data")

    if "excessFields" in data1:
        return render_template(
            "trace.jinja",
            selection=form,
            # content=client.mongo_content,
            extra=extra,
            message="Excess data received, try grouping by one or more of: " + str(data1["excessFields"]))
    if "excessFields" in data2:
        return render_template(
            "trace.jinja",
            selection=form,
            # content=client.mongo_content,
            extra=extra,
            message="Excess data received, try grouping by one or more of: " + str(data2["excessFields"]))

    mode = "markers" if form["type"] == "Scatter" else "lines"
    if form["type"] == "QQ":
        data.compute_qq()
        mode = "markers"
    traces1 = []
    traces2 = []
    table = {}
    current_app.logger.warning("starting plots")

    for label, traceData in data1.items():
        # print("trace:" + label)
        traceX = []
        traceY = []
        lpf = traceData[0]["y"][0]
        last = traceData[0]["y"][0]
        lastLast = last

        # print("data: " + str(traceData))
        for element in traceData:
            traceX.append(element["Epoch"])


        for element in traceData:
            val = element["y"][0]
            lpf = lpf + (val - lpf) * float(form["fCoeff"])
            hpf = val - lpf
            if (form["filter"] == "HPF"):
                val = hpf
            if (form["filter"] == "LPF"):
                val = lpf
            if (form["filter"] == "DIFF"):
                val = val - last
            if (form["filter"] == "DIFF2"):
                val = val - 2 * last + lastLast
            lastLast = last
            last = element["y"][0]
            traceY.append(val)

        x_hover_template = "%{x}<br>"
        traces1.append(
            go.Scatter(
                x=traceX,
                y=traceY,
                mode=mode,
                name=f"{label}",
                hovertemplate=x_hover_template + "%{y:.4e%}<br>" + f"{label}",
                legendgroup='group1'
            )
        )
    for label, traceData in data2.items():
        # print("trace:" + label)
        traceX = []
        traceY = []
        # print("data: " + str(traceData))
        for element in traceData:
            traceX.append(element["Epoch"])
        for element in traceData:
            traceY.append(element["y"][0])

        x_hover_template = "%{x}<br>"
        traces2.append(
            go.Scatter(
                x=traceX,
                y=traceY,
                mode=mode,
                name=f"{label}",
                hovertemplate=x_hover_template + "%{y:.4e%}<br>" + f"{label}",
                legendgroup='group1', showlegend=False
            )
        )

    current_app.logger.warning("end plots")

    return render_template(
        "trace.jinja",
        # content=client.mongo_content,
        extra=extra,
        graphJSON=generate_figs(traces1, traces2),
        mode="plotly2",
        selection=form,
        # table_data=table,
        # table_headers=["RMS", "mean"],
        # tableagg_data=table_agg,
        # tableagg_headers=["RMS", "mean"],
    )
