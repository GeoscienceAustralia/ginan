import plotly.graph_objs as go
from flask import current_app, render_template, request, session

from backend.data.clocks import Clocks
from backend.data.measurements import MeasurementArray
from ..utilities import init_page, extra, generate_fig, aggregate_stats, get_data, extract_database_series
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
    form = {
        "series": form_data.get("series"),
        "series_base": form_data.get("series_base"),
        "subset": form_data.getlist("subset"),
        "modes": form_data.getlist("modes"),
        "exclude_tail": form_data.get("exclude_tail"),
        "exclude": form_data.get("exclude"),
        "clockType": form_data.get("clockType")
    }

    if form["exclude"] == "":
        form["exclude"] = 0
    else:
        form["exclude"] = int(form["exclude"])

    if form["exclude_tail"] == "":
        form["exclude_tail"] = 0
    else:
        form["exclude_tail"] = int(form["exclude_tail"])
    session['clocks'] = form
    db_, series_ = extract_database_series(form["series"])
    db_2, series_2 = extract_database_series(form["series_base"])
    if db_ != db_2:
        return render_template(
            "clocks.jinja",
            # content=client.mongo_content,
            extra=extra,
            selection=session['clocks'] ,
            message="Error getting data: Can only compare series from the same database",
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
            selection=session['clocks'] ,
            message=f"Error getting data: {str(err)}",
        )
    data.find_minmax()
    data.adjust_slice(minutes_min=form["exclude"], minutes_max=form["exclude_tail"], trim=True)

    if form["clockType"] == "Satellite":
        clocks_processor = Clocks(data, satlist=sat_list, series=series_, series_base=series_2)
    else:
        clocks_processor = Clocks(data, sitelist=site_list, series=series_, series_base=series_2)
    trace = []
    result = clocks_processor.process(mode=form["modes"])
    result.sort()
    result.find_minmax()
    result.get_stats()
    table = {}
    for _clock in result:
        smallLegend = [_clock.id[a] for a in _clock.id]
        trace.append(
            go.Scatter(
                x=_clock.epoch[_clock.subset],
                y=_clock.data["x"][_clock.subset],
                mode="lines",
                name=f"{smallLegend}",
                hovertemplate="%{x|%Y-%m-%d %H:%M:%S}<br>" + "%{y:.4e%}<br>" + f"{smallLegend}",
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
        selection=session['clocks'] ,
        table_data=table,
        table_headers=["RMS", "mean"],
        tableagg_data=table_agg,
        tableagg_headers=["RMS", "mean"],
    )
