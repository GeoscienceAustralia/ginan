import logging

import numpy as np
import numpy.typing as npt

from backend.dbconnector import mongo
from backend.data.measurements import Measurements

logger = logging.getLogger(__name__)


class Satellite:
    def __init__(self, mongodb: mongo.MongoDB, sat: str = "", series: str = "") -> None:
        self.sat: str = sat
        self.series: str = series
        self.mongodb: mongo.MongoDB = mongodb

        self.time: npt.ArrayLike = np.empty(0, dtype="datetime64[us]")
        self.statetime: npt.ArrayLike = np.empty(0, dtype="datetime64[us]")
        self.pos: npt.ArrayLike = np.empty(0)
        self.vel: npt.ArrayLike = np.empty(0)
        self.residual: npt.ArrayLike = np.empty(0)
        self.rac: npt.ArrayLike = np.empty(0)
        self.mode: str = ""
        self.db: str = mongodb.mongo_db

    @classmethod
    def process(cls, mongodb: mongo.MongoDB, sat: str = "", series: str = "", mode: str = "") -> "Satellite":
        s = cls(mongodb, sat, series)
        s.get_postfit()
        s.get_state()
        s.match_time()
        if mode == "Residual RTN":
            s.get_rac()
            s.mode = mode
        return s

    def to_measurement(self) -> Measurements:
        meas = Measurements()
        meas.sat = self.sat
        meas.id["series"] = self.series
        meas.id["sat"] = self.sat
        meas.id["db"] = self.db
        meas.epoch = self.time
        if self.mode == "Residual RTN":
            meas.data["R"] = self.rac[:, 0]
            meas.data["T"] = self.rac[:, 1]
            meas.data["N"] = self.rac[:, 2]
        else:
            meas.data["X"] = self.residual[:, 0]
            meas.data["Y"] = self.residual[:, 1]
            meas.data["Z"] = self.residual[:, 2]
        return meas

    def get_postfit(self):
        data = self.mongodb.get_data(
            collection="Measurements",
            state=None,
            sat=[self.sat],
            site=[""],
            series=[self.series],
            keys=["ECI PseudoPos-0-Postfit", "ECI PseudoPos-1-Postfit", "ECI PseudoPos-2-Postfit"],
        )
        self.time = np.asarray(data[0]["t"], dtype="datetime64[us]")
        self.residual = np.empty((len(data[0]["t"]), 3))
        self.residual[:, 0] = data[0]["ECI PseudoPos-0-Postfit"]
        self.residual[:, 1] = data[0]["ECI PseudoPos-1-Postfit"]
        self.residual[:, 2] = data[0]["ECI PseudoPos-2-Postfit"]

    def get_state(self):
        data = self.mongodb.get_data(
            collection="States",
            state=["ORBIT"],
            sat=[self.sat],
            site=[""],
            series=[self.series],
            keys=["x"],
        )
        self.pos = np.empty((3, len(data[0]["t"])))
        self.vel = np.empty((3, len(data[0]["t"])))
        self.statetime = np.asarray(data[0]["t"], dtype="datetime64[us]")
        data = np.asarray(data[0]["x"])
        self.pos = data[:, :3]
        self.vel = data[:, 3:]

    def get_rms(self, use_rac=False):
        data = self.residual if not use_rac else self.rac
        rms = np.zeros(4)
        rms[:3] = np.sqrt(np.mean(data**2, axis=0))
        res3d = np.sqrt(np.sum(data**2, axis=1))
        rms[3] = np.sqrt(np.mean(res3d**2))
        return rms

    def get_rac(self):
        r = self.pos / np.linalg.norm(self.pos, axis=1)[:, np.newaxis]
        c = np.cross(self.pos, self.vel)
        c = c / np.linalg.norm(c, axis=1)[:, np.newaxis]
        a = np.cross(c, self.pos)
        a = a / np.linalg.norm(a, axis=1)[:, np.newaxis]
        self.rac = np.empty_like(self.residual)
        self.rac[:, 0] = (r * self.residual).sum(axis=1)
        self.rac[:, 1] = (a * self.residual).sum(axis=1)
        self.rac[:, 2] = (c * self.residual).sum(axis=1)
        return self.get_rms(use_rac=True)

    def match_time(self):
        _, in_time, in_state = np.intersect1d(self.time, self.statetime, return_indices=True)
        self.time = self.time[in_time]
        self.residual = self.residual[in_time]
        self.pos = self.pos[in_state]
        self.vel = self.vel[in_state]
        self.statetime = self.statetime[in_state]
