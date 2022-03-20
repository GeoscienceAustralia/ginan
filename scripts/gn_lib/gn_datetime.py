'''Base time conversion functions'''
from datetime import datetime as _datetime

import numpy as _np
import pandas as _pd

from .gn_const import GPS_ORIGIN as _GPS_ORIGIN
from .gn_const import J2000_ORIGIN as _J2000_ORIGIN
from .gn_const import SEC_IN_DAY as _SEC_IN_DAY
from .gn_const import SECS_IN_WEEK as _SECS_IN_WEEK


def gpsweekD(yr, doy, wkday_suff=False):
    """
    Convert year, day-of-year to GPS week format: WWWWD or WWWW
    Based on code from Kristine Larson's gps.py
    https://github.com/kristinemlarson/gnssIR_python/gps.py

    Input:
    yr - year (int)
    doy - day-of-year (int)

    Output:
    GPS Week in WWWWD format - weeks since 7 Jan 1980 + day of week number (str)
    """

    # Set up the date and time variables
    yr = int(yr)
    doy = int(doy)
    dt = _datetime.strptime(f"{yr}-{doy:03d} 01","%Y-%j %H")

    wkday = dt.weekday() + 1

    if wkday == 7:
        wkday = 0

    mn, dy, hr = dt.month, dt.day, dt.hour

    if mn <= 2:
        yr = yr-1
        mn = mn+12

    JD = _np.floor(365.25*yr) + _np.floor(30.6001*(mn+1)) + dy + hr/24.0 + 1720981.5
    GPS_wk = _np.int(_np.floor((JD-2444244.5)/7.0))

    if wkday_suff:
        return str(GPS_wk)+str(wkday)
    else:
        return str(GPS_wk)


class GPSDate:
    """
    Representation of datetime that provides easy access to
    useful properties.

    Usage:
    today = GPSDate("today")
    tomorrow = today.next
    print(f"today year: {today.year}, doy: {today.dy}, GPS week and weekday: {today.gpswkD}")
    print(f"tomorrow year: {tomorrow.year}, doy: {tomorrow.dy}, GPS week and weekday: {tomorrow.gpswkD}")
    """

    def __init__(self, ts: _np.datetime64):
        if isinstance(ts, str):
            ts = _np.datetime64(ts)

        self.ts = ts

    @property
    def as_datetime(self):
        """Convert to Python `datetime` object."""
        return self.ts.astype(_datetime)

    @property
    def yr(self):
        """Year"""
        return self.as_datetime.strftime("%Y")

    @property
    def dy(self):
        """Day of year"""
        return self.as_datetime.strftime("%j")

    @property
    def gpswk(self):
        """GPS week"""
        return gpsweekD(self.yr, self.dy, wkday_suff=False)

    @property
    def gpswkD(self):
        """GPS week with weekday suffix"""
        return gpsweekD(self.yr, self.dy, wkday_suff=True)

    @property
    def next(self):
        """The following day"""
        return GPSDate(self.ts + 1)

    @property
    def prev(self):
        """The previous day"""
        return GPSDate(self.ts - 1)

    def __str__(self):
        """Same string representation as the underlying numpy datetime64 object"""
        return str(self.ts)


def dt2gpswk(dt,wkday_suff=False,both=False):
    '''
    Convert the given datetime object to a GPS week (option to include day suffix)
    '''
    yr = dt.strftime('%Y')
    doy = dt.strftime('%j')
    if not both:
        return gpsweekD(yr,doy,wkday_suff=wkday_suff)
    else:
        return gpsweekD(yr,doy,wkday_suff=False),gpsweekD(yr,doy,wkday_suff=True)



def gpswkD2dt(gpswkD):
    '''
    Convert from GPS-Week-Day (WWWWDD) format to datetime object
    '''
    if type(gpswkD) != str:
        gpswkD = str(gpswkD)
    dt_64 = _GPS_ORIGIN + _np.timedelta64(int(gpswkD[:-1]),'W') + _np.timedelta64(int(gpswkD[-1]),'D')
    return dt_64.astype(_datetime)


def yydoysec2datetime(
    snxdate :_np.ndarray or _pd.Series,
    recenter:bool=True,
    as_j2000:bool=True)->_np.ndarray:

    '''Converts snx YY.DOY.SSSSS object Series to datetime64.
    recenter overrirdes day seconds value to midday
    as_j2000 outputs int seconds after 2000-01-01 12:00:00, datetime64 otherwise'''
    if not isinstance(snxdate,_np.ndarray):
        snxdate = snxdate.values #if Series to ndarray

    snxdate = list(_np.char.split(a=snxdate.astype('<U12'),sep=':'))
    snxdate = _np.asarray(snxdate,dtype=_np.int_)

    delta_years = snxdate[:,0]
    day_of_year = snxdate[:,1]
    secs_day    = snxdate[:,2]

    delta_years -= 100*(delta_years>50)
    years = _np.datetime64('2000-01-01').astype('datetime64[Y]') + delta_years

    date  = years.astype('datetime64[D]') + (day_of_year - 1) #as years start with day no. 1
    time  = _SEC_IN_DAY//2 if recenter else secs_day
    datetime = date.astype('datetime64[s]') + time

    if as_j2000:
        return datetime2j2000(datetime)
    return  datetime


def datetime2yydoysec(datetime):
    '''datetime64[s] -> yydoysecond
    NaNs become ""
    year 2100 become 00:000:00000'''
    nan_mask = _np.isnan(datetime)
    datetime_no_nans = datetime[~nan_mask]
    date_years = datetime_no_nans.astype('datetime64[Y]')
    date_days  = datetime_no_nans.astype('datetime64[D]')
    doy =  _pd.Series((date_days - date_years + 1).astype(int).astype(str))
    seconds = _pd.Series((datetime_no_nans - date_days).astype('timedelta64[s]').astype(int).astype(str))
    yydoysec_no_nans = (_pd.Series(date_years.astype(str)).str.slice(2).values #need U4+1 for [Y] so str
                            + ':' + doy.str.zfill(3).values
                            + ':' + seconds.str.zfill(5).values)
    yydoysec_no_nans[date_years == _np.datetime64('2100')] = '00:000:00000'

    yydoysec = _np.empty_like(datetime,dtype=object)
    yydoysec[~nan_mask] = yydoysec_no_nans
    return yydoysec

def gpsweeksec2datetime(gps_week:_np.ndarray, tow:_np.ndarray, as_j2000:bool=True)->_np.ndarray:
    '''trace file date (gps week, time_of_week) to datetime64 conversion'''
    ORIGIN = (_GPS_ORIGIN - _J2000_ORIGIN).astype(int) if as_j2000 else _GPS_ORIGIN
    datetime = ORIGIN + (gps_week*_SECS_IN_WEEK + tow)
    return datetime


def datetime2gpsweeksec(array:_np.ndarray, as_decimal = False)->tuple or _np.ndarray:
    if array.dtype == int:
        ORIGIN = _J2000_ORIGIN.astype(int) - _GPS_ORIGIN.astype(int)
        gps_time = (array + ORIGIN) # need int conversion for the case of datetime64
    else:
        ORIGIN = _GPS_ORIGIN.astype(int)
        gps_time = array.astype('datetime64[s]').astype(int) - ORIGIN #datetime64 converted to int seconds

    weeks_int = (gps_time/_SECS_IN_WEEK).astype(int)
    tow = gps_time - weeks_int * _SECS_IN_WEEK # this eliminates rounding error problem
    return weeks_int + (tow / 1000000) if as_decimal else (weeks_int, tow)


def datetime2mjd(array):
    if array.dtype == int:
        array= array + _J2000_ORIGIN
    gps_datetime = array - _GPS_ORIGIN
    mjd_days = gps_datetime.astype('timedelta64[D]')
    seconds_frac = (gps_datetime - mjd_days).astype(float)/86400
    return mjd_days.astype(int) + 44244, seconds_frac


def datetime2j2000(datetime:_np.ndarray)->_np.ndarray:
    '''datetime64 conversion to int seconds after J2000 (2000-01-01 12:00:00)'''
    if not isinstance(datetime,_np.ndarray):
        raise TypeError("input should be numpy ndarray")
    if datetime.dtype != '<M8[s]':
        return (datetime.astype('datetime64[s]') - _J2000_ORIGIN).astype(int) # this will break on pandas dataframe
    return (datetime.astype('datetime64[s]') - _J2000_ORIGIN).astype(int)


def j20002datetime(j2000secs:_np.ndarray)->_np.ndarray:
    '''int64 seconds after J2000 (2000-01-01 12:00:00) conversion to datetime64'''
    if j2000secs.dtype == int:
        return _J2000_ORIGIN + j2000secs
    return _J2000_ORIGIN + j2000secs.astype(int)


def j20002rnxdt(j2000secs:_np.ndarray)->_np.ndarray:
    '''
    Converts j2000 array to rinex format string representation
    674913600 -> '2021-05-22T00:00:00' -> '*  2021  5 22  0  0 0.00000000\n'
    '''
    datetime = j20002datetime(j2000secs)
    year = datetime.astype('datetime64[Y]')
    month = datetime.astype('datetime64[M]')
    day = datetime.astype('datetime64[D]')
    hour = datetime.astype('datetime64[h]')
    minute = datetime.astype('datetime64[m]')

    date_y = '*' + _pd.Series(year.astype(str)).str.rjust(6).values
    date_m = _pd.Series(((month - year).astype(int)+1).astype(str)).str.rjust(3).values
    date_d = _pd.Series(((day - month).astype(int) + 1).astype(str)).str.rjust(3).values

    time_h = _pd.Series((hour - day).astype(int).astype(str)).str.rjust(3).values
    time_m = _pd.Series((minute - hour).astype(int).astype(str)).str.rjust(3).values
    time_s = ((_pd.Series((datetime - minute)).view(int)/1e9)
                        .apply('{:.8f}\n'.format).str.rjust(13).values)
    return date_y + date_m + date_d + time_h + time_m + time_s
