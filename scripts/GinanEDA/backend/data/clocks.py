import logging
from typing import List, Union
import numpy as np

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

    def process(self, mode: Union[List[str], None] = None) -> MeasurementArray:
        """
        Process the data. process in 2 steps.
        1. locate inside the data vector the two element with the same "sat" name in the self.identifier field.
        2. create a common time vector for the two elements, filling the missing data with Nan.
        """
        if mode is None:
            mode = ["Series", "Epoch"]

        result = MeasurementArray()
        iterate_list = self.sitelist if self.satlist is None else self.satlist
        key = "site" if self.satlist is None else "sat"

        for sat in iterate_list:
            reference, comparison = self._find_reference_and_comparison(sat, key)

            if reference is not None and comparison is not None:
                datats = self._combine_clocks(reference, comparison)
                result.append(datats)

        for _result in result:
            _result.mask_outliers()

        if "Series" in mode:
            self._demean(result)
        if "Epoch" in mode:
            self._demean_by_epoch(result) #demean by epoch
        return result

    def _combine_clocks(self, reference: Measurements, comparison: Measurements) -> Measurements:
        common_time = np.union1d(reference.epoch, comparison.epoch)
        for series in [reference, comparison]:
            _, unique_indices = np.unique(series.epoch, return_index=True)
            series.epoch = series.epoch[unique_indices]
            series.data["x"] = series.data["x"][unique_indices]
        common_data1 = np.full_like(common_time, np.nan, dtype="float64")
        common_data1[np.isin(common_time, reference.epoch)] = reference.data["x"][:, 0]
        common_data2 = np.full_like(common_time, np.nan, dtype="float64")
        common_data2[np.isin(common_time, comparison.epoch)] = comparison.data["x"][:, 0]
        datats = Measurements(
            epoch=common_time,
            data={"x": common_data1 - common_data2},
            identifier=comparison.id,
        )
        return datats

    def _demean(self, result: MeasurementArray) -> MeasurementArray:
        for _result in result:
            _result.data["x"] -= np.nanmean(_result.data["x"]) #demean 
        return result

    def _demean_by_epoch(self, result: MeasurementArray) -> MeasurementArray:
        #demean by epoch
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
