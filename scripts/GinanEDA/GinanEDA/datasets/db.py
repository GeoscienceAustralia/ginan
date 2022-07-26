import logging

import numpy as np
import pymongo
from dash import html
from pymongo import MongoClient

logger = logging.getLogger(__name__)
#MONGO_DB = None
#MONGO_CL = None
DB_SITE = None
DB_SAT = None
DB_STATES = None
DB_STATE_DIC = None
DB_MEAS_KEY = None
DB_STATE_KEY = None


# exclusion list for dropdown x and y
exclude_measurements = [
    "_id",
    "REF0-Prefit",
    "REF1-Prefit",
    "REF0-Postfit",
    "REF1-Postfit",
    "REF1-Prefit",
    "REF0-Variance",
    "REF1-Variance",
    "0-Prefit",
    "0-Postfit",
    "0-Variance",
    "1-Variance",
    "1-Prefit",
    "0-Prefit",
]

exclude_state = ["_id"]


def check_db(url=None, db=None):
    if url is None:
        return html.P(f"Not connected")
    else:
        return html.P(f"Connected to dataserver {url}, dataset {db}")


def connect_client(url):
    logger.debug(f"CONNECING TO {url}")
    return MongoClient(host=url)


pipeline_state = [
    {
        "$group": {
            "_id": "$State",
            "Site": {"$addToSet": "$Site"},
            "Sat": {"$addToSet": "$Sat"},
        }
    }
]


def connect_db(data_dict):
    print(f" **** {data_dict}")
    if data_dict['MONGO_URL'] is None or data_dict['MONGO_DB'] is None:
        return
    logger.info(f"Connecting to DB {data_dict['MONGO_URL']} / {data_dict['MONGO_DB']}")
    MONGO_CL = connect_client(data_dict['MONGO_URL'])[data_dict['MONGO_DB']]
    list_cl = list(MONGO_CL.list_collection_names())
    if "Measurements" in list_cl:
        data_dict['DB_SITE'] = get_stations_list(MONGO_CL, "Measurements")
        data_dict['DB_SAT'] = get_sats_list(MONGO_CL, "Measurements")
        data_dict['DB_MEAS_KEY'] = get_keys(MONGO_CL, "Measurements")
        logger.info(
            f" => Measurements contains {len(data_dict['DB_SITE'])} sites and {len(data_dict['DB_SAT'])} satellites"
        )
        logger.info(f"    List of Keys : {', '.join( data_dict['DB_MEAS_KEY'])}")
    else:
        logger.warning(f" => Measurements collection not present")
    # to do if states exist
    if "States" in list_cl :  #temporary locked
        data_dict['DB_STATES'] = get_states_list(MONGO_CL, "States")
        data_dict['DB_STATE_DIC'] = list(MONGO_CL["States"].aggregate(pipeline_state))
        req = {}
        for l in data_dict['DB_STATE_DIC']:
            pipeline = [
                {"$match": {"State": l["_id"]}},
                {"$project": {"keyvalue": {"$objectToArray": "$$ROOT"}}},
                {"$unwind": {"path": "$keyvalue"}},
                {"$group": {"_id": None, "allkeys": {"$addToSet": "$keyvalue.k"}}},
            ]
            req[l["_id"]] = pipeline
        answer = list(MONGO_CL["States"].aggregate([{"$facet": req}]))[0]
        for l in data_dict['DB_STATE_DIC']:
            l["Site"].sort()
            l["Sat"].sort()
            l["keys"] = sorted(answer[l["_id"]][0]["allkeys"])

    else:
        logger.warning(f" => States collection not present")


def get_keys(MONGO_CL, collection):
    pipeline = [
        {"$project": {"keyvalue": {"$objectToArray": "$$ROOT"}}},
        {"$unwind": {"path": "$keyvalue"}},
        {"$group": {"_id": None, "allkeys": {"$addToSet": "$keyvalue.k"}}},
    ]
    if MONGO_CL is not None:# and MONGO_DB is not None:
        l = list(MONGO_CL[collection].aggregate(pipeline))
    else:
        return list([None])
    return sorted(l[0]["allkeys"])


def get_stations_list(MONGO_CL, collection):
    pipeline = [{"$group": {"_id": None, "sites": {"$addToSet": "$Site"}}}]
    if MONGO_CL is not None :#and MONGO_DB is not None:
        l = list(MONGO_CL[collection].aggregate(pipeline))
    else:
        return list([None])
    return sorted(l[0]["sites"])


def get_sats_list(MONGO_CL, collection):
    pipeline = [{"$group": {"_id": None, "sats": {"$addToSet": "$Sat"}}}]
    if MONGO_CL is not None:# and MONGO_DB is not None:
        l = list(MONGO_CL[collection].aggregate(pipeline))
    else:
        l = list([None])
    return sorted(l[0]["sats"])


def get_states_list(MONGO_CL, collection):
    pipeline = [{"$group": {"_id": None, "State": {"$addToSet": "$State"}}}]
    if MONGO_CL is not None:# and MONGO_DB is not None:
        l = list(MONGO_CL[collection].aggregate(pipeline))
    else:
        l = list([None])
    return sorted(l[0]["State"])


def get_series(data, collection, state, site, sat, x1, x2):
    req_match_1 = {"$match": {"Sat": {"$in": sat}, "Site": {"$in": site}}}
    req_match_2 = {"$match": {x1: {"$ne": None}, x2: {"$ne": None}}}

    req_match_state1 = {
        "$match": {
            "Sat": {"$in": sat},
            "Site": {"$in": site},
            "State": state,
        }
    }
    req_match_state2 = {"$match": {x1: {"$ne": None}, x2: {"$ne": None}}}
    req_sort = {"$sort": {"Epoch": 1}}
    req_group = {
        "$group": {
            "_id": {"site": "$Site", "sat": "$Sat"},
            "x": {"$push": "$" + x1},
            "y": {"$push": "$" + x2},
        }
    }
    req_sort2 = {"$sort": {"_id.site": 1, "_id.sat": 1}}
    pipeline = []
    if state is not None:
        pipeline.append(req_match_state1)
        pipeline.append(req_match_state2)
    else:
        pipeline.append(req_match_1)
        pipeline.append(req_match_2)

    pipeline.append(req_sort)
    if x1 == "Epoch":
        pipeline.append({"$project": {x1: 1, x2: 1, "Sat": 1, "Site": 1}})
    pipeline.append(req_group)
    pipeline.append(req_sort2)
    MONGO_CL = connect_client(data["MONGO_URL"])[data["MONGO_DB"]]
    req = MONGO_CL[collection].aggregate(pipeline)
    x_ = []
    y_ = []
    sat_ = []
    site_ = []
    for i in req:
        x1_ = np.array(i["x"])
        x2_ = np.array(i["y"])
        if x1 == "Epoch":
            x_.append(x1_.astype("datetime64[s]"))
        else:
            x_.append(x1_)
        y_.append(x2_)
        sat_.append(i["_id"]["sat"])
        site_.append(i["_id"]["site"])
    return site_, sat_, x_, y_


def get_series_xyz(data, collection, state, site, sat, x1, x2, x3):
    req_match = {
        "$match": {
            "Sat": {"$in": sat},
            "Site": {"$in": site},
            x1: {"$ne": None},
            x2: {"$ne": None},
            x3: {"$ne": None},
        }
    }
    req_match_state = {
        "$match": {
            "Sat": {"$in": sat},
            "Site": {"$in": site},
            "State": state,
            x1: {"$ne": None},
            x2: {"$ne": None},
            x3: {"$ne": None},
        }
    }
    req_sort = {"$sort": {"Epoch": 1}}
    req_group = {
        "$group": {
            "_id": {"site": "$Site", "sat": "$Sat"},
            "x": {"$push": "$" + x1},
            "y": {"$push": "$" + x2},
            "z": {"$push": "$" + x3},
        }
    }
    req_sort2 = {"$sort": {"_id.site": 1, "_id.sat": 1}}
    pipeline = []
    if state is not None:
        pipeline.append(req_match_state)
    else:
        pipeline.append(req_match)

    pipeline.append(req_sort)
    if x1 == "Epoch":
        pipeline.append({"$project": {x1: 1, x2: 1, x3: 1, "Sat": 1, "Site": 1}})
    pipeline.append(req_sort)
    pipeline.append(req_group)
    pipeline.append(req_sort2)
    MONGO_CL = connect_client(data["MONGO_URL"])[data["MONGO_DB"]]
    req = MONGO_CL[collection].aggregate(pipeline)
    x_ = []
    y_ = []
    z_ = []
    sat_ = []
    site_ = []
    for i in req:
        x1 = np.array(i["x"])
        x2 = np.array(i["y"])
        x3 = np.array(i["z"])
        x_.append(x1)
        y_.append(x2)
        z_.append(x3)
        sat_.append(i["_id"]["sat"])
        site_.append(i["_id"]["site"])
    return site_, sat_, x_, y_, z_
