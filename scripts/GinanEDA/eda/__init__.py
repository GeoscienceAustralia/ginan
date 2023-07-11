import os

from flask import Flask, render_template

from .routes import register_routes

app = Flask(__name__, template_folder="templates")  # Specify the templates folder

register_routes(app)

if __name__ == "__main__":
    app.run()


# def create_app(test_config=None):
#     app = Flask(__name__, instance_relative_config=True)
#     app.config.from_mapping(
#         SECRET_KEY='dev',
#     )

#     if test_config:
#         app.config.from_mapping(test_config)
#     else:
#         app.config.from_pyfile('config.py', silent=True)

#     @app.route('/hello')
#     def hello():
#         return render_template("base.html")

#     @app.route('/')
#     def select_db():
#         return
#     return app
