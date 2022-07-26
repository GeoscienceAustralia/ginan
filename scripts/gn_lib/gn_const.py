'''Constants to be declared here'''
from numpy import datetime64 as _datetime64
from pandas import CategoricalDtype as _CategoricalDtype, DataFrame as _DataFrame

MJD_ORIGIN   = _datetime64('1858-11-17 00:00:00')
GPS_ORIGIN   = _datetime64('1980-01-06 00:00:00')
J2000_ORIGIN = _datetime64('2000-01-01 12:00:00')

SEC_IN_WEEK  = 604800
SEC_IN_DAY   = 86400
SEC_IN_YEAR  = 31557600

C_LIGHT = 299792458.0     # speed of light (m/s)
OMEGA_E = 7.2921151467E-5 # rad/sec WGS84 value of earth's rotation rate

# https://www.iers.org/SharedDocs/Publikationen/EN/IERS/Documents/ac/sinex/sinex_v201_appendix1_pdf.pdf
TYPE_CATEGORY = object # tmp fix to get rid of Categorical in the future
# TYPE_CATEGORY       = _CategoricalDtype(categories = [
#         'STAX','STAY','STAZ','VELX','VELY','VELZ','XGC','YGC','ZGC','RS_RA','RS_DE','RS_RAR',
#         'RS_DER','RS_PL','LOD','UT','XPO','YPO','XPOR','YPOR','NUT_LN','NUT_OB','NUTRLN','NUTROB',
#         'SAT__X','SAT__Y','SAT__Z','SAT_VX','SAT_VY','SAT_VZ','SAT_RP','SAT_GX','SAT_GZ','SATYBI',
#         'TROTOT','TRODRY','TROWET','TGNTOT','TGNWET','TGNDRY','TGETOT','TGEWET','TGEDRY','RBIAS',
#         'TBIAS','SBIAS','ZBIAS','AXI_OF','SATA_Z','SATA_X','SATA_Y',
#         'ALOG_E','TLOG_E','ALOG_N','TLOG_N','ALOG_H','TLOG_H',
#         'AEXP_E','TEXP_E','AEXP_N','TEXP_N','AEXP_H','TEXP_H'])

TECHNIQUE_CATEGORY  = _CategoricalDtype(categories = ['C','D','L','M','P','R'])

UNIT_CATEGORY       = _CategoricalDtype(categories = ['m','m/y','m/s2','ppb','ms','msd2',
                                                      'mas','ma/d','rad','rd/y','rd/d'])

PT_CATEGORY         = _CategoricalDtype(categories = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                                                      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'])

PRN_CATEGORY = _CategoricalDtype(categories = [
        'G01', 'G02', 'G03', 'G04', 'G05', 'G06', 'G07', 'G08', 'G09',
        'G10', 'G11', 'G12', 'G13', 'G14', 'G15', 'G16', 'G17', 'G18',
        'G19', 'G20', 'G21', 'G22', 'G23', 'G24', 'G25', 'G26', 'G27',
        'G28', 'G29', 'G30', 'G31', 'G32', 'G33', 'G34', 'G35', 'G36',
        'R01', 'R02', 'R03', 'R04', 'R05', 'R06', 'R07', 'R08', 'R09',
        'R10', 'R11', 'R12', 'R13', 'R14', 'R15', 'R16', 'R17', 'R18',
        'R19', 'R20', 'R21', 'R22', 'R23', 'R24',
        'E01', 'E02', 'E03', 'E04', 'E05', 'E06', 'E07', 'E08', 'E09',
        'E10', 'E11', 'E12', 'E13', 'E14', 'E15', 'E16', 'E17', 'E18',
        'E19', 'E20', 'E21', 'E22', 'E23', 'E24', 'E25', 'E26', 'E27',
        'E28', 'E29', 'E30', 'E31', 'E32', 'E33', 'E34', 'E35', 'E36',
        'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09',
        'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 'C16', 'C17', 'C18',
        'C19', 'C20', 'C21', 'C22', 'C23', 'C24', 'C25', 'C26', 'C27',
        'C28', 'C29', 'C30', 'C31', 'C32', 'C33', 'C34', 'C35', 'C36',
        'C37', 'C38', 'C39', 'C40', 'C41', 'C42', 'C43', 'C44', 'C45',
        'C46',
        'C57', 'C58', 'C59', 'C60', 'C61'])

CLK_TYPE_CATEGORY = _CategoricalDtype(categories=['CR','DR','AR','AS','MS'])

STATE_TYPES_CATEGORY = _CategoricalDtype(categories =[
         'PHASE_BIAS', 'REC_POS', 'REC_CLOCK', 'REC_SYS_BIAS', 'TROP', 'TROP_GM', 'SAT_CLOCK', 'AMBIGUITY','EOP','EOP_RATE','DCB','IONOSPHERIC']) # 'ONE' removed

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

SISRE_COEF_DF = _DataFrame(     data = [[0.99,	0.98,	0.98,	0.98,	0.98],
                                        [127,	54,	49,	45,	61  ]],
                                columns=['C_IGSO','C','G','R','E'],
                                index = ['alpha','beta'])