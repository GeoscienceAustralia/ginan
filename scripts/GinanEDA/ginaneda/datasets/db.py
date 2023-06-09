"""_summary_
"""
import logging

import numpy as np
from dash import html
from pymongo import MongoClient


logger = logging.getLogger(__name__)


# exclusion list for dropdown x and y
exclude_measurements = [
    "_id"
]

exclude_state = ["_id"]


def check_db(url=None, db=None):
    """_summary_
    """
    if url is None:
        return html.P(f"Not connected")
    else:
        return html.P(f"Connected to dataserver {url}, dataset {db}")


def connect_client(url):
    """_summary_
    """
    cl = MongoClient(host=url)
    return cl


pipeline_state = [
    {
        "$group": {
            "_id": "$State",
            "Site": {"$addToSet": "$Site"},
            "Sat": {"$addToSet": "$Sat"},
        }
    }
]

def reshape_list(l):
    return list(set([item for sublist in l for item in sublist]))

def connect_db(data_dict):
    """_summary_
    """
    if data_dict['MONGO_URL'] is None or data_dict['MONGO_DB'] is None:
        return
    data_dict['DB_MEAS_KEY'] = []
    data_dict['DB_SITE'] = []
    data_dict['DB_SAT'] = []
    data_dict['DB_STATES'] = []
    data_dict['Geom']=[]
    data_dict['Series']=[]
    for db_name in data_dict['MONGO_DB']:
        mongo_cl = connect_client(data_dict['MONGO_URL'])[db_name]
        list_cl = list(mongo_cl.list_collection_names())
        content = mongo_cl["Content"].find({})
        for doc in content:
            if doc['type'] == "Site":
                data_dict['DB_SITE'].append(doc['Values'])
            if doc['type'] == "Sat":
                data_dict['DB_SAT'].append(doc['Values'])
            if doc['type'] == 'Series':
                for s in doc['Values']:
                    data_dict['Series'].append(f"{db_name}\{s}")
            if doc['type'] == 'Measurements':
                data_dict['DB_MEAS_KEY'].append('Epoch')
                data_dict['DB_MEAS_KEY'].append('Site')
                data_dict['DB_MEAS_KEY'].append('Sat')
                for rec in doc['Values']:
                    data_dict['DB_MEAS_KEY'].append(rec)
            if doc['type'] == "State":
                data_dict['DB_STATES'].append(doc['Values'])
        if "Geometry" in list_cl:
            geom  = mongo_cl["Geometry"].find_one({})
            for i in geom:
                if i not in ['Epoch', 'Site', 'Sat', 'Series']:
                    data_dict['Geom'].append(i)
    for l in  [ 'DB_SITE', 'DB_SAT', 'DB_STATES']:
        data_dict[l] = reshape_list(data_dict[l])
        data_dict[l].sort()

    data_dict['DB_MEAS_KEY'] = sorted(set(data_dict['DB_MEAS_KEY']))
    data_dict['Geom'] = sorted(set(data_dict['Geom']))
    if "Measurements" in list_cl:
        logger.info(
            f" => Measurements contains {len(data_dict['DB_SITE'])} sites and {len(data_dict['DB_SAT'])} satellites"
        )
        logger.info(f"    List of Keys : {', '.join( data_dict['DB_MEAS_KEY'])}")
        data_dict['MEAS_DB'] = True
    else:
        logger.warning(" => Measurements collection not present")
        data_dict["MEAS_DB"] = False
    if "States" in list_cl :
        logger.info("States are present")
        data_dict['STATE_DB'] = True
    else:
        logger.warning(" => States collection not present")
        data_dict['STATE_DB'] = False


def get_keys(mongo_cl, collection):
    """_summary_
    """
    pipeline = [
        {"$project": {"keyvalue": {"$objectToArray": "$$ROOT"}}},
        {"$unwind": {"path": "$keyvalue"}},
        {"$group": {"_id": None, "allkeys": {"$addToSet": "$keyvalue.k"}}},
    ]
    if mongo_cl is not None:# and MONGO_DB is not None:
        l = list(mongo_cl[collection].aggregate(pipeline))
    else:
        return list([None])
    return sorted(l[0]["allkeys"])


def get_series2(data, collection, state, site, sat, serie, x1, x2, x3=None):
    results = {}
    for s in serie:
        db_, s_ = s.split('\\')
        results.update(get_series(data, collection, state, site, sat, db_, s_, x1, x2, x3))
    return results



def get_series(data, collection, state, site, sat, db, serie, x1, x2, x3):
    """_summary_
    """
    logger.info("getting data")
    pipeline = []
    pipeline.append(
        {"$match": {"Sat": {"$in" : sat},
                    "Site": {"$in": site},
                    "Series": {"$in": [serie]}}
        })
    if state is not None:
        pipeline[-1]["$match"]["State"] = state
    pipeline.append( {"$sort": {"Epoch": 1}})
    pipeline.append( {
        "$group": {
            "_id": {"site": "$Site", "sat": "$Sat", "series":"$Series"},
            "t" : {"$push": "$Epoch"},
            "x": {"$push": "$" + x1},
            "y": {"$push": "$" + x2},
        }
    })
    if x3 is not None:
        pipeline[-1]["$group"]["z"] =  {"$push": "$" + x3}
    logger.info("connection to db")
    logger.info("getting data")
    mongo_cl = connect_client(data["MONGO_URL"])[db]
    req = {}
    req2 = {}
    for cursor in mongo_cl[collection].aggregate(pipeline):
        req[db+"_"+cursor['_id']['series']+"_"+cursor['_id']['site']+"_"+cursor['_id']['sat']] = cursor

    suffix = ""
    for i, v  in enumerate(pipeline[0]["$match"]["Series"]["$in"]):
        pipeline[0]["$match"]["Series"]["$in"][i] = v.split("_")[0]
        if len(v.split("_")) > 1:
            suffix = "_"+v.split("_")[1]

    for cursor in mongo_cl["Geometry"].aggregate(pipeline):
        req2[db+"_"+cursor['_id']['series']+suffix+"_"+cursor['_id']['site']+"_"+cursor['_id']['sat']] = cursor
    

    if len(req2)!=0:
        for r in req:
            if r in req2:
                r2  = req2[r]
                if len(req[r]["x"]) == 0 and len(r2["x"])!=0:
                    intersect, c1, c2 = np.intersect1d(req[r]["t"] , r2["t"], return_indices = True)
                    req[r]["x"] = np.asarray(r2["x"])[c2]
                if len(req[r]["y"]) == 0 and len(r2["y"])!=0:
                    intersect, c1, c2 = np.intersect1d(req[r]["t"] , r2["t"], return_indices = True)
                    req[r]["y"] = np.asarray(r2["y"])[c2]
                if x3 is not None and len(req[r]["z"]) == 0  and len(r2["z"])!=0:
                    intersect, c1, c2 = np.intersect1d(req[r]["x"] , r2["x"], return_indices = True)
                    req[r]["z"] = np.asarray(r2["z"])[c2]
                # continue
    logger.info("get data")
    return req


##def get_series(data, collection, state, site, sat, serie, x1, x2):
##    """_summary_
##    """
##    req_match_1 = {"$match": {"Sat": {"$in": sat}, "Site": {"$in": site}}}
##    req_match_2 = {"$match": {x1: {"$ne": None}, x2: {"$ne": None}}}
##
##    req_match_state1 = {
##        "$match": {
##            "Sat": {"$in": sat},
##            "Site": {"$in": site},
##            "State": state,
##            "Series":{"$in": serie}
##        }
##    }
##    req_match_state2 = {"$match": {x1: {"$ne": None}, x2: {"$ne": None}}}
##    req_sort = {"$sort": {"Epoch": 1}}
##    req_group = {
##        "$group": {
##            "_id": {"site": "$Site", "sat": "$Sat", "series":"$Series"},
##            "x": {"$push": "$" + x1},
##            "y": {"$push": "$" + x2},
##        }
##    }
##    req_sort2 = {"$sort": {"_id.site": 1, "_id.sat": 1, "_id.series": 1}}
##    pipeline = []
##    if state is not None:
##        pipeline.append(req_match_state1)
##        pipeline.append(req_match_state2)
##    else:
##        pipeline.append(req_match_1)
##        pipeline.append(req_match_2)
##
##    pipeline.append(req_sort)
##    if x1 == "Epoch":
##        pipeline.append({"$project": {x1: 1, x2: 1, "Sat": 1, "Site": 1, "Series": 1}})
##    pipeline.append(req_group)
##    pipeline.append(req_sort2)
##    mongo_cl = connect_client(data["MONGO_URL"])[data["MONGO_DB"]]
##    req = mongo_cl[collection].aggregate(pipeline)
##    x_ = []
##    y_ = []
##    sat_ = []
##    site_ = []
##    serie_ =[]
##    for i in req:
##        x1_ = np.array(i["x"])
##        x2_ = np.array(i["y"])
##        if x1 == "Epoch":
##            x_.append(x1_.astype("datetime64[s]"))
##        else:
##            x_.append(x1_)
##        y_.append(x2_)
##        sat_.append(i["_id"]["sat"])
##        site_.append(i["_id"]["site"])
##        serie_.append(i["_id"]["serie"])
##    return site_, sat_, serie_,  x_, y_
