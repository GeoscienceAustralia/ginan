from flask import render_template, request, session

from backend.dbconnector.mongo import MongoDB

from . import eda_bp
from ..utilities import init_page, extra


@eda_bp.route("/config", methods=["GET", "POST"])
def config():
    if request.method == "POST":
        return handle_post_request()
    else:
        return init_page(template="config.jinja")


def handle_post_request():
    form = request.form
    database = form.get("database")
    with MongoDB(session["mongo_ip"], data_base=database, port=session["mongo_port"]) as client:
        configuration = client.get_config()
    if configuration is None:
        return render_template("config.jinja", message="No configuration found")
    configuration.pop("_id")
    return render_template("config.jinja", configuration=configuration, selection=form)
