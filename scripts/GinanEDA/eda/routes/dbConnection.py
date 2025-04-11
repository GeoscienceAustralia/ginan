from flask import render_template, request, session
from flask import current_app

from pymongo.errors import ServerSelectionTimeoutError

from backend.dbconnector.mongo import MongoDB
from . import eda_bp


@eda_bp.route("/", methods=["GET", "POST"])
def connect():
    """
    Render the index page with the database connection form.

    This function handles both GET and POST requests. For GET requests, it simply renders the 'connect.html' template.
    For POST requests, it calls the appropriate handler based on the form data.

    Returns:
        The rendered template 'connect.html' for GET requests, or the result from the corresponding handler for POST requests.
    """
    if request.method == "POST":
        return handle_post_request()
    db_ip = getattr(session, "mongo_ip", "127.0.0.1")
    db_port = getattr(session, "mongo_port", "27017")
    return render_template("connect.jinja", db_ip=db_ip, db_port=db_port)


def handle_post_request():
    """
    Handle the POST request from the database connection form.

    Based on the form data, this function calls the appropriate handler function.

    Returns:
        The result from the corresponding handler function.
    """
    form_data = request.form
    db_ip = getattr(session, "mongo_ip", "127.0.0.1")
    db_port = getattr(session, "mongo_port", "27017")
    if "connect" in form_data:
        return handle_connect_request(form_data)
    elif "load" in form_data:
        return handle_load_request(form_data)
    else:
        return render_template("connect.jinja", db_ip=db_ip, db_port=db_port)


def handle_connect_request(form_data):
    """
    Handle the 'connect' request from the database connection form.

    This function connects to the specified database using the provided IP address and port.
    It retrieves the list of databases and renders the 'connect.html' template with the necessary data.

    Args:
        form_data: The form data containing the IP address and port.

    Returns:
        The rendered template 'connect.html' with the retrieved data.
    """
    connect_db_ip = form_data.get("db_ip", "")
    db_port = int(form_data.get("db_port", 27017))

    current_app.logger.info(f"connecting to {connect_db_ip}")
    try:
        client = MongoDB(url=connect_db_ip, port=db_port)
        client.connect()
        databases = client.get_list_db()
        print(databases)
        to_remove = []
        for database in databases:
            try:
                with MongoDB(connect_db_ip, port=db_port, data_base=database) as client2:
                    current_app.logger.debug(f"looking for Content in {database}")
                    client2.get_content()
            except Exception as e:
                current_app.logger.info(f"Error in {database}: {e}")
                to_remove.append(database)
                # databases.remove(database)
        for database in to_remove:
            databases.remove(database)
        try:
            databases.remove("config")
        except:
            pass
        return render_template("connect.jinja", db_ip=connect_db_ip, db_port=db_port, databases=databases)
    except ServerSelectionTimeoutError:
        error_message = (
            f"Connection failed: MongoDB server doesn't exist or is not reachable. {connect_db_ip}:{db_port}"
        )
        return render_template("connect.jinja", db_ip=connect_db_ip, db_port=db_port, message=error_message)


def handle_load_request(form_data):
    """
    Handle the 'load' request from the database connection form.

    This function connects to the specified database and retrieves its content.
    It stores the connection information in the session and renders the 'connect.html' template with the retrieved data.

    Args:
        form_data: The form data containing the IP address, port, and database name.

    Returns:
        The rendered template 'connect.html' with the retrieved data.
    """
    connect_db_ip = form_data.get("db_ip", "")
    db_name = form_data.getlist("dataset")
    db_port = int(form_data.get("db_port", 27017))
    current_app.logger.info(f"connection to {connect_db_ip}, {db_name}")
    message = []
    session["mongo_ip"] = connect_db_ip
    session["mongo_db"] = db_name
    session["mongo_port"] = db_port
    site = []
    sat = []
    states_series = []
    measurements_series = []
    mesurements = []
    geometry = []
    state = []
    for database in db_name:
        with MongoDB(connect_db_ip, port=db_port, data_base=database) as client:
            databases = client.get_list_db()
            client.get_content()
            nsat = len(client.mongo_content["Sat"])
            nsite = len(client.mongo_content["Site"])
            message.append(f"connected to {database}:  has {nsat} satellites and {nsite} sites")
            site += client.mongo_content["Site"]
            sat += client.mongo_content["Sat"]
            if "Series" in client.mongo_content:
                states_series       += [f"{database}\{series}" for series in client.mongo_content["Series"]]
                measurements_series += [f"{database}\{series}" for series in client.mongo_content["Series"]]
            else:
                states_series       += [f"{database}\{series}" for series in client.mongo_content["StateSeries"]]
                measurements_series += [f"{database}\{series}" for series in client.mongo_content["MeasurementsSeries"]]
            if client.mongo_content["Has_measurements"]:
                mesurements += client.mongo_content["Measurements"]
            geometry += client.mongo_content["Geometry"]
            state += client.mongo_content["State"]
    current_app.logger.debug(site, sat, states_series)
    current_app.logger.debug("\n".join(message))
    session["list_sat"] = sorted(set(sat))
    session["list_site"] = sorted(set(site))
    session["list_measurements_series"] = sorted(set(measurements_series))
    session["list_states_series"] = sorted(set(states_series))
    session["list_generic"] = ["Epoch", "Sat", "Site", "Series"]
    session["list_geometry"] = sorted(set(geometry))
    # remove list_generic from list_geometry
    session["list_geometry"] = [item for item in session["list_geometry"] if item not in session["list_generic"]]

    if client.mongo_content["Has_measurements"]:
        session["list_measurements"] = sorted(set(mesurements))

    session["list_state"] = sorted(set(state))

    return render_template(
        "connect.jinja",
        db_ip=connect_db_ip,
        db_port=db_port,
        databases=databases,
        message="<br>".join(message),
    )
