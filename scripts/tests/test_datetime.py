from gn_lib.gn_datetime import GPSDate
import numpy as np

def test_gpsdate():
    date = GPSDate(np.datetime64('2021-08-31'))
    assert int(date.yr) == 2021
    assert int(date.dy) == 243
    assert int(date.gpswk) == 2173
    assert int(date.gpswkD[-1]) == 2
    assert str(date) == '2021-08-31'

    date = date.next
    assert int(date.yr) == 2021
    assert int(date.dy) == 244
    assert int(date.gpswk) == 2173
    assert int(date.gpswkD[-1]) == 3
    assert str(date) == '2021-09-01'
