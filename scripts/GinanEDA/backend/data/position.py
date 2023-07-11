import datetime
import logging

import matplotlib.pyplot as plt
import numpy as np
import numpy.typing as npt

from backend.data.measurements import MeasurementArray, Measurements

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

def xyz2blh(x, y, z):
    """_summary_
    Angle returned will be in radians
    """
    A = 6378137.0
    B = 6356752.314245
    e = np.sqrt(1 - (B ** 2) / (A ** 2))
    # calculate longitude, in radians
    longitude = np.arctan2(y, x)
    # calculate latitude, in radians
    xy_hypot = np.hypot(x, y)
    lat0 = np.zeros_like(x)
    latitude = np.arctan(z / xy_hypot)
    while np.any(np.abs(latitude - lat0) > 1e-9):
        lat0 = latitude
        N = A / np.sqrt(1 - e ** 2 * np.sin(lat0) ** 2)
        latitude = np.arctan((z + e ** 2 * N * np.sin(lat0)) / xy_hypot)
    # calculate height, in meters
    N = A / np.sqrt(1 - e ** 2 * np.sin(latitude) ** 2)
    small_angle_indices = np.abs(latitude) < np.pi / 4
    R, phi = np.hypot(xy_hypot[small_angle_indices], z[small_angle_indices]), np.arctan(z[small_angle_indices] / xy_hypot[small_angle_indices])
    height = np.zeros_like(x)
    height[small_angle_indices] = R * np.cos(phi) / np.cos(latitude[small_angle_indices]) - N[small_angle_indices]
    height[~small_angle_indices] = z[~small_angle_indices] / np.sin(latitude[~small_angle_indices]) - N[~small_angle_indices] * (1 - e ** 2)
    return latitude, longitude, height

class Position:
    """
    Position class to handle position analysis
    """

    def __init__(
        self,
        data: MeasurementArray = None,
        base: MeasurementArray = None,
        sitelist: list = None,
    ) -> None:
        self.data = data
        self.base = base
        self.sitelist = sitelist
        if self.base is not None:
            self.data = self.data - self.base
            
    def __iter__(self):
        return iter(self.data)

        
    def rotate_enu(self) -> None:
        """
        rotate Rotate the position to the ENU frame from the base 
        """
        for data in self.data:
            for k in data.data:
                print(k)
            #locate the base with the same station id
            base = self.base.locate(site=data.id['site'])
            lat, lon, height = xyz2blh(base.data['x_0'], base.data['x_1'], base.data['x_2'])
            rot = np.zeros((3,3, len(lat)))
            rot[0,0] = -np.sin(lon)
            rot[0,1] = -np.sin(lat)*np.cos(lon)
            rot[0,2] = np.cos(lat)*np.cos(lon)
            rot[1,0] = np.cos(lon)
            rot[1,1] = -np.sin(lat)*np.sin(lon)
            rot[1,2] = np.cos(lat)*np.sin(lon)
            rot[2,0] = 0
            rot[2,1] = np.cos(lat)
            rot[2,2] = np.sin(lat)
            project = np.empty((len(data.data['x_0']),3))
            for i in range(3):
                project[:, i] = data.data[f'x_{i}']
            enu = np.matmul(rot.transpose(), project[:,:,np.newaxis])[:,:,0]
            for i in range(3):
                data.data[f'x_{i}'] = enu[:, i]
        