from pathlib import Path
import subprocess
import uuid
import random

from flask import Flask
import flask
from flask_cors import CORS
import ruamel.yaml

app = Flask(__name__)
# This sets up CORS to allow browsers to actually get this data?
# The default is pretty permissive but I think that's fine?
# Look into this further at some point.
CORS(app)

# This probably works while running the dev server in a single-threaded nature
# but it certainly doesn't work if you deploy it properly. Need a better
# solution to this in future, but given we want to store subprocess.Popen
# objects, this will have to be in-memory and can probably just be a
# synchronised object managing access to this map.
jobs = {}

def build_job_details(jobid, job):
    returncode = job["process"].poll()
    if returncode is None:
        status = "Running"
    elif returncode == 0:
        status = "Done"
    else:
        status = f"Failed ({returncode})"
    return {"id": jobid, "status": status, "name": job["name"], "command": job["process"].args[0]}

@app.route("/")
@app.route("/status")
def status():
    return flask.render_template("status.html")

@app.route("/status/<uuid:jobid>")
def jobstatus(jobid):
    return f"<h1>More detail for {jobid} goes here</h1>"

@app.route("/map")
def map():
    return "<h1>Map goes here</h1>"

@app.route("/plots")
def plots():
    return "<h1>EDA goes here</h1>"

@app.route("/newjob")
def newjob():
    return flask.render_template("newjob.html")

@app.route("/api/jobs", methods=["GET"])
def api_get_job_data():
    if flask.request.args.get("details", default=False, type=bool):
        return flask.jsonify([build_job_details(jobid, job) for jobid, job in jobs.items()])
    return flask.jsonify(list(jobs))

@app.route("/api/jobs", methods=["POST"])
def api_submit_job():
    jobjson = flask.request.get_json()
    jobid = uuid.uuid4()
    app.logger.warning(jobjson)
    if jobjson["type"] == "sleep":
        sleep_duration = random.randint(20,60)
        jobproc = subprocess.Popen(["sleep", str(sleep_duration)])
    elif jobjson["type"] == "pea":
        # This needs to go to a proper place, run directories need to be sorted out
        configfile = Path(str(jobid) + ".yaml")
        with open(configfile, "w", encoding="utf-8") as f:
            f.write(jobjson["configYAML"])
        jobproc = subprocess.Popen(["pea", "-y", str(configfile)])
    elif jobjson["type"] == "pod":
        # This needs to go to a proper place, run directories need to be sorted out
        configfile = Path(str(jobid) + ".yaml")
        with open(configfile, "w", encoding="utf-8") as f:
            f.write(jobjson["configYAML"])
        jobproc = subprocess.Popen(["pod", "-y", str(configfile)])
    jobs[jobid] = {"name": jobjson["name"], "process": jobproc}
    return {"id": jobid, "statusURL": flask.url_for("status") }

@app.route("/api/jobs/<uuid:jobid>", methods=["GET"])
def api_get_job_details(jobid):
    if jobid in jobs:
        job = jobs[jobid]
        return build_job_details(jobid, job)
    return {}

if __name__ == "__main__":
    app.run()