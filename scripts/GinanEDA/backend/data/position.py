import logging

import numpy as np

from backend.data.measurements import MeasurementArray

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


def xyz2blh(x, y, z):
    """_summary_
    Angle returned will be in radians
    """
    A = 6378137.0
    B = 6356752.314245
    e = np.sqrt(1 - (B**2) / (A**2))
    longitude = np.arctan2(y, x)
    xy_hypot = np.hypot(x, y)
    latitude0 = np.zeros_like(x)
    latitude = np.arctan(z / xy_hypot)
    while np.any(np.abs(latitude - latitude0) > 1e-9):
        latitude0 = latitude
        N = A / np.sqrt(1 - e**2 * np.sin(latitude0) ** 2)
        latitude = np.arctan((z + e**2 * N * np.sin(latitude0)) / xy_hypot)

    N = A / np.sqrt(1 - e**2 * np.sin(latitude) ** 2)
    small_angle_indices = np.abs(latitude) < np.pi / 4
    R = np.hypot(xy_hypot[small_angle_indices], z[small_angle_indices])
    phi = np.arctan(z[small_angle_indices] / xy_hypot[small_angle_indices])
    height = np.zeros_like(x)
    height[small_angle_indices] = R * np.cos(phi) / np.cos(latitude[small_angle_indices]) - N[small_angle_indices]
    height[~small_angle_indices] = z[~small_angle_indices] / np.sin(latitude[~small_angle_indices]) - N[
        ~small_angle_indices
    ] * (1 - e**2)
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
        Rotate the position to the ENU frame from the base
        """
        for data in self.data:
            # locate the base with the same station id
            base = self.base.locate(site=data.id["site"])
            _common, in_base, in_data = np.intersect1d(base.epoch, data.epoch, return_indices=True)
            data.epoch = _common
            data_matrix = np.column_stack([data.data[f"REC_POS_x_{i}"][in_data] for i in range(3)])
            base_matrix = np.column_stack([base.data[f"REC_POS_x_{i}"][in_base] for i in range(3)])
            lat, lon, _height = xyz2blh(base_matrix[:, 0], base_matrix[:, 1], base_matrix[:, 2])
            rot = np.zeros((3, 3, len(lat)))
            rot[0, 0] = -np.sin(lon)
            rot[0, 1] = -np.sin(lat) * np.cos(lon)
            rot[0, 2] = np.cos(lat) * np.cos(lon)
            rot[1, 0] = np.cos(lon)
            rot[1, 1] = -np.sin(lat) * np.sin(lon)
            rot[1, 2] = np.cos(lat) * np.sin(lon)
            rot[2, 0] = 0
            rot[2, 1] = np.cos(lat)
            rot[2, 2] = np.sin(lat)
            project = np.matmul(rot.transpose(), data_matrix[:, :, np.newaxis])[:, :, 0]
            for i in range(3):
                data.data[f"REC_POS_x_{i}"] = project[:, i]
