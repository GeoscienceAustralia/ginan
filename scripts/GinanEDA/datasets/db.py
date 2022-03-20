from dash import html
from pymongo import MongoClient
import numpy as np
import pymongo
MONGO_URL = None
MONGO_DB = None
MONGO_CL = None
DB_SITE = None
DB_SAT = None
DB_STATES = None
DB_STATE_DIC = None


def check_db():
    # print(MONGO_URL, MONGO_DB)
    if MONGO_URL is None:
        return html.P(f"Not connected")
    else:
        return html.P(f"Connected to dataserver {MONGO_URL}, dataset {MONGO_DB}")


def connect_client():
    global MONGO_CL
    MONGO_CL = MongoClient(host=MONGO_URL)
    # print(MONGO_CL, MONGO_URL)
    # for i in MONGO_CL.list_databases():
    #     print(i['name'])

pipeline_state = [
    {
        '$group': {
            '_id': '$State',
            'Site': {
                '$addToSet': '$Site'
            },
            'Sat': {
                '$addToSet': '$Sat'
            }
        }
    }
]


def connect_db():
    global DB_SITE
    global DB_SAT
    global DB_STATES
    global DB_STATE_DIC
    global MONGO_CL
    if MONGO_DB is not None and MONGO_CL is not None:
        MONGO_CL = MONGO_CL[MONGO_DB]
        list_cl = list(MONGO_CL.list_collection_names())
        if "Measurements" in list_cl:
            DB_SITE = get_stations_list("Measurements")
            DB_SAT = get_sats_list("Measurements")
        # else:
        #     DB_SAT = ["None"]
        #     DB_SITE = ["None"]
        # to do if states exist
        if "States" in list_cl:
            DB_STATES = get_states_list("States")
            DB_STATE_DIC = list(MONGO_CL["States"].aggregate(pipeline_state))
            for l in DB_STATE_DIC:
                l['Site'].sort()
                l['Sat'].sort()
                a = MONGO_CL["States"].find_one({"State":l['_id']})
                # temp = list(a.keys())
                l['keys'] = list(a.keys())
    #     print(DB_STATE_DIC)
    # print(DB_SITE)


def get_stations_list(collection):
    pipeline = [
        {
            '$group': {
                '_id': None,
                'sites': { '$addToSet': '$Site'}
            }
        }
    ]
    # print(pipeline)
    if MONGO_CL is not None and MONGO_DB is not None:
        l = list(MONGO_CL[collection].aggregate(pipeline))
    else:
        return list([None])
    return sorted(l[0]['sites'])


def get_sats_list(collection):
    pipeline = [
        {
            '$group': {
                '_id': None,
                'sats': {'$addToSet': '$Sat'}
            }
        }
    ]
    # print("*** ", MONGO_DB, MONGO_CL )
    if MONGO_CL is not None and MONGO_DB is not None:
        l = list(MONGO_CL[collection].aggregate(pipeline))
    else:
        l = list([None])
    return sorted(l[0]['sats'])


def get_states_list(collection):
    pipeline = [
        {
            '$group': {
                '_id': None,
                'State': {'$addToSet': '$State'}
            }
        }
    ]
    # print("*** ", MONGO_DB, MONGO_CL )
    if MONGO_CL is not None and MONGO_DB is not None:

        l = list(MONGO_CL[collection].aggregate(pipeline))
        # print(l)
    else:
        l = list([None])
    return sorted(l[0]['State'])



def get_series(collection, state, site, sat, x1, x2):
    req_match_1 ={
        '$match': {
            'Sat': {'$in': sat},
            'Site':{'$in': site}
        }
    }
    req_match_2 = {
        '$match':{
            x1: { '$ne': None},
            x2: {'$ne': None }
        }
    }

    req_match_state1 = {
        '$match': {
            'Sat': {'$in': sat},
            'Site':{'$in': site},
            'State': state,
        }
    }
    req_match_state2 = {
        '$match': {
            x1: { '$ne': None},
            x2: {'$ne': None }
        }
    }
    req_sort =  {
        '$sort': {
            'Epoch': 1
        }
    }
    req_group = {
        '$group': {
            '_id': {'site':"$Site", 'sat':"$Sat"},
            'x': {
                '$push': '$'+x1
            },
            'y': {
                '$push': '$'+x2
            }
        }}
    req_sort2 = {
        '$sort': {'_id.site':1, '_id.sat':1}
    }
    pipeline = []
    if state is not None:
        pipeline.append(req_match_state1)
        pipeline.append(req_match_state2)
    else:
        pipeline.append(req_match_1)
        pipeline.append(req_match_2)

    pipeline.append(req_sort)
    if (x1 == "Epoch"):
        pipeline.append(     {
            '$project': {
                x1 :1,
                x2 : 1,
                'Sat':1,
                'Site':1
            }}
        )
    pipeline.append(req_group)
    pipeline.append(req_sort2)
    req = MONGO_CL[collection].aggregate(pipeline)
    x_ = []
    y_ =[]
    sat_=[]
    site_=[]
    # print(pipeline)
    for i in req:
        x1_ = np.array(i['x'])
        x2_ = np.array(i['y'])
        # print(len(x1), len(x2))
        # print(x1)
        # idx = np.argsort(x1)
        # print(i['_id']['sat'])
        if (x1 == "Epoch"):
            x_.append(x1_.astype('datetime64[s]'))
        else:
            x_.append(x1_)
        y_.append(x2_)
        sat_.append(i['_id']['sat'])
        site_.append(i['_id']['site'])
    return site_, sat_, x_, y_


def get_series_xyz(collection, state, site, sat, x1, x2, x3):
    req_match ={
        '$match': {
            'Sat': {'$in': sat},
            'Site':{'$in': site},
            x1: { '$ne': None},
            x2: {'$ne': None },
            x3: {'$ne': None}
        }
    }
    req_match_state = {
        '$match': {
            'Sat': {'$in': sat},
            'Site':{'$in': site},
            'State': state,
            x1: { '$ne': None},
            x2: {'$ne': None },
            x3: {'$ne': None}
        }
    }
    req_sort = {
        '$sort': {
            'Epoch': 1
        }
    }
    req_group = {
        '$group': {
            '_id': {'site':"$Site", 'sat':"$Sat"},
            'x': {
                '$push': '$'+x1
            },
            'y': {
                '$push': '$'+x2
            },
            'z':{
                '$push': '$'+x3
            }
        }}
    req_sort2 = {
        '$sort': {'_id.site':1, '_id.sat':1}
    }
    pipeline = []
    if state is not None:
        pipeline.append(req_match_state)
    else:
        pipeline.append(req_match)

    pipeline.append(req_sort)
    if (x1 == "Epoch"):
        pipeline.append(     {
            '$project': {
                x1 :1,
                x2 : 1,
                x3 : 1,
                'Sat':1,
                'Site':1
            }}
        )
    pipeline.append(req_sort)
    pipeline.append(req_group)
    pipeline.append(req_sort2)
    req = MONGO_CL[collection].aggregate(pipeline)
    x_ = []
    y_ = []
    z_ = []
    sat_=[]
    site_=[]
    for i in req:
        x1 = np.array(i['x'])
        x2 = np.array(i['y'])
        x3 = np.array(i['z'])
        # print(len(x1), len(x2))
        # print(x1)
        # idx = np.argsort(x1)
        # print(i['_id']['sat'])
        x_.append(x1)
        y_.append(x2)
        z_.append(x3)
        sat_.append(i['_id']['sat'])
        site_.append(i['_id']['site'])
    return site_, sat_, x_, y_, z_
