import json
import secrets
import os
from flask import Flask, g, session

from eda.routes import register_routes

from eda.utilities import initialize_session

def create_app():
    app = Flask(__name__, template_folder="templates/")
    app.secret_key = secrets.token_hex(16)

    def dict_filter(value):
        return json.dumps(value)

    theme = os.getenv("EDA_THEME", "plotly")
    grid = os.getenv("EDA_GRID", "True")
    app.config["EDA_THEME"] = theme
    app.config["EDA_GRID"] = True if grid == "True" else False

    app.jinja_env.filters["dict"] = dict_filter

    register_routes(app)

    @app.template_filter("to_dict")
    def to_dict_filter(value):
        return dict(value)

    @app.before_request
    def before_request():
        initialize_session()
    return app


app = create_app()

if __name__ == "__main__":
    app.run()
