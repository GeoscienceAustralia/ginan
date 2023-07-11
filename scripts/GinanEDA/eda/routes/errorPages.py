# error_pages.py

from flask import Blueprint, render_template
from . import eda_bp

# error_bp = Blueprint("error_pages", __name__)


@eda_bp.app_errorhandler(404)
def page_not_found(e):
    return render_template("404.jinja"), 404


@eda_bp.app_errorhandler(405)
def method_not_allowed(e):
    return render_template("405.jinja"), 405
