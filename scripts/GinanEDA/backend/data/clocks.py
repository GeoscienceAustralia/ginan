import datetime
import logging

import matplotlib.pyplot as plt
import numpy as np
import numpy.typing as npt

from backend.data.measurements import MeasurementArray, Measurements

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


class Clocks:
    """
    Clock class to handle clock data
    """

    def __init__(
        self,
        data: MeasurementArray = None,
        satlist: list = None,
        sitelist: list = None,
        series: str = None,
        series_base: str = None,
    ) -> None:
        self.data = data
        self.satlist = satlist
        self.sitelist = sitelist
        self.series = series
        self.series_base = series_base

    def process(self) -> MeasurementArray:
        """
        Process the data. process in 2 steps.
        1. locate inside the data vector the two element with the same "sat" name in the self.identifier field.
        2. create a common time vector for the two elements, filling the missing data with Nan.
        """
        result = MeasurementArray()
        iterate_list = self.sitelist if self.satlist is None else self.satlist
        key = "site" if self.satlist is None else "sat"

        for sat in iterate_list:
            reference, comparison = self._find_reference_and_comparison(sat, key)

            if reference is not None and comparison is not None:
                common_time = np.union1d(reference.epoch, comparison.epoch)
                common_data1 = np.full_like(common_time, np.nan, dtype="float64")
                common_data2 = np.full_like(common_time, np.nan, dtype="float64")
                # check if there is a dupllicated epoch in ref.epoch, and remove it, as well as in ref.data['x']
                # Comes from the PEA writting duplicates (last epochs) and inaptitude to deal with it.
                for series in [reference, comparison]:
                    _, unique_indices = np.unique(series.epoch, return_index=True)
                    series.epoch = series.epoch[unique_indices]
                    series.data["x"] = series.data["x"][unique_indices]
                common_data1[np.isin(common_time, reference.epoch)] = reference.data["x"][:, 0]
                common_data2[np.isin(common_time, comparison.epoch)] = comparison.data["x"][:, 0]
                data = {}
                # was initially common_data1 - np.nanmean(common_data1) - common_data2 + np.nanmean(common_data2).
                # issues with means as Nans are not necessary at the same place.ß
                data["x"] = common_data1 - common_data2
                data["x"] -= np.nanmean(data["x"])
                datats = Measurements(
                    epoch=common_time,
                    data=data,
                    identifier=comparison.id,
                )
                if datats.mask_outliers():
                    datats.data["x"] -= np.nanmean(datats.data["x"])
                result.append(datats)

        common_time = np.unique(np.concatenate([_result.epoch for _result in result]))
        data = np.full((len(common_time), len(result.arr)), np.nan, dtype="float64")
        for i, _result in enumerate(result):
            data[np.isin(common_time, _result.epoch), i] = _result.data["x"]
        data = np.nanmean(data, axis=1)
        for _result in result:
            _result.data["x"] -= data[np.isin(common_time, _result.epoch)]

        return result

    def _find_reference_and_comparison(self, sat, key):
        reference = None
        comparison = None
        for data in self.data:
            if data.id[key] == sat and data.id["series"] == self.series:
                comparison = data
            if data.id[key] == sat and data.id["series"] == self.series_base:
                reference = data
        return reference, comparison
