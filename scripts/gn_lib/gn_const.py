'''Constants to be declared here'''
from numpy import datetime64 as _datetime64
from pandas import CategoricalDtype as _CategoricalDtype

GPS_ORIGIN   = _datetime64('1980-01-06 00:00:00')
J2000_ORIGIN = _datetime64('2000-01-01 12:00:00')

SECS_IN_WEEK = 604800
SEC_IN_DAY   = 86400
SEC_IN_YEAR  = 31557600

# https://www.iers.org/SharedDocs/Publikationen/EN/IERS/Documents/ac/sinex/sinex_v201_appendix1_pdf.pdf
TYPE_CATEGORY       = _CategoricalDtype(categories = [
        'STAX','STAY','STAZ','VELX','VELY','VELZ','XGC','YGC','ZGC','RS_RA','RS_DE','RS_RAR',
        'RS_DER','RS_PL','LOD','UT','XPO','YPO','XPOR','YPOR','NUT_LN','NUT_OB','NUTRLN','NUTROB',
        'SAT__X','SAT__Y','SAT__Z','SAT_VX','SAT_VY','SAT_VZ','SAT_RP','SAT_GX','SAT_GZ','SATYBI',
        'TROTOT','TRODRY','TROWET','TGNTOT','TGNWET','TGNDRY','TGETOT','TGEWET','TGEDRY','RBIAS',
        'TBIAS','SBIAS','ZBIAS','AXI_OF','SATA_Z','SATA_X','SATA_Y',
        'ALOG_E','TLOG_E','ALOG_N','TLOG_N','ALOG_H','TLOG_H',
        'AEXP_E','TEXP_E','AEXP_N','TEXP_N','AEXP_H','TEXP_H'])

TECHNIQUE_CATEGORY  = _CategoricalDtype(categories = ['C','D','L','M','P','R'])

UNIT_CATEGORY       = _CategoricalDtype(categories = ['m','m/y','m/s2','ppb','ms','msd2',
                                               'mas','ma/d','rad','rd/y','rd/d'])

PT_CATEGORY         = _CategoricalDtype(categories = ['A', 'B','C','D'])
# ILRS 7307 site PT is D
#GeodePy
class Ellipsoid:
    __doc__ = 'ellipsoid class doc placeholder'
    def __init__(self, semimaj, inversef):
        self.semimaj    = float(semimaj) # a
        self.semimajsq  = semimaj * semimaj # a**2
        self.inversef   = inversef # inverse of the first flattening factor
        self.flatten    = 1 / self.inversef # first flattening factor
        self.semimin    = self.semimaj * (1 - self.flatten) # b
        self.semiminsq  = self.semimin * self.semimin # b**2
        self.ecc1sq     = self.flatten * (2 - self.flatten)
        self.ecc2sq     = self.ecc1sq  / (1 - self.ecc1sq)
#         self.ecc1       = sqrt(self.ecc1sq)
#         self.n          = float(self.f / (2 - self.f))
#         self.n2         = self.n ** 2
#         self.meanradius = (2 * self.semimaj + self.semimin)/3

# Geodetic Reference System 1980
# www.epsg-registry.org/export.htm?gml=urn:ogc:def:ellipsoid:EPSG::7019
GRS80 = Ellipsoid(6378137, 298.257222101)

# World Geodetic System 1984
# www.epsg-registry.org/export.htm?gml=urn:ogc:def:ellipsoid:EPSG::7030
WGS84 = Ellipsoid(6378137, 298.257223563)

# Australian National Spheroid
# www.epsg-registry.org/export.htm?gml=urn:ogc:def:ellipsoid:EPSG::7003
ANS = Ellipsoid(6378160, 298.25)

# International (Hayford) 1924
# www.epsg-registry.org/export.htm?gml=urn:ogc:def:ellipsoid:EPSG::7022
INTL24 = Ellipsoid(6378388, 297)
