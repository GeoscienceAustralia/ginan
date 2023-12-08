from flask import Blueprint

eda_bp = Blueprint("eda", __name__, url_prefix="/")


# Register the blueprint
def register_routes(app):
    app.register_blueprint(eda_bp)


from . import states, measurements, config, errorPages, position, clocks, dbConnection, orbits, trace
