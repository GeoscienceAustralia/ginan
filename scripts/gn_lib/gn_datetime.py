'''Base time conversion functions'''
import numpy as _np
import pandas as _pd

from .gn_const import J2000_ORIGIN as _J2000_ORIGIN
from .gn_const import SEC_IN_DAY as _SEC_IN_DAY
from .gn_const import GPS_ORIGIN as _GPS_ORIGIN

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
    t_days = gps_week.astype('timedelta64[W]') if gps_week.dtype != 'timedelta64[W]' else gps_week
    t_tow  = tow.astype('timedelta64[s]') if tow.dtype != 'timedelta64[s]' else tow

    datetime = _GPS_ORIGIN + t_days + t_tow
    if as_j2000:
        return datetime2j2000(datetime)
    return datetime

def datetime2gpsweeksec(array:_np.ndarray)->tuple:
    if array.dtype == int:
        array= array + _J2000_ORIGIN
    gps_time = array - _GPS_ORIGIN
    weeks = (gps_time).astype('timedelta64[W]')
    seconds = gps_time - weeks
    return weeks.astype(int),seconds.astype(int)

def datetime2mjd(array):
    if array.dtype == int:
        array= array + _J2000_ORIGIN
    gps_datetime = array - _GPS_ORIGIN
    mjd_days = gps_datetime.astype('timedelta64[D]') 
    seconds_frac = (gps_datetime - mjd_days).astype(float)/86400
    return mjd_days.astype(int) + 44244, seconds_frac

def datetime2j2000(datetime:_np.ndarray)->_np.ndarray:
    '''datetime64 conversion to int seconds after J2000 (2000-01-01 12:00:00)'''
    return (datetime - _J2000_ORIGIN).astype('timedelta64[s]').astype(_np.int_)

def j20002datetime(j2000secs:_np.ndarray)->_np.ndarray:
    '''int64 seconds after J2000 (2000-01-01 12:00:00) conversion to datetime64'''
    if j2000secs.dtype == _np.int_:
        return _J2000_ORIGIN + j2000secs
    return _J2000_ORIGIN + j2000secs.astype(_np.int_)

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
    time_s = ((_pd.Series((datetime - minute)).astype(int)/1e10)
                        .apply('{:.8f}\n'.format).str.rjust(13).values)
    return date_y + date_m + date_d + time_h + time_m + time_s
