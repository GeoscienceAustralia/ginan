SUBROUTINE attitude (mjd, rsat, vsat, rSun, PRNsat, BLKsat, & 
                     eclipsf, beta, Mangle, Yangle, eBX_nom, eBX_ecl)

! ----------------------------------------------------------------------
! SUBROUTINE: attitude.f03 
! ----------------------------------------------------------------------
! Purpose:
!  GNSS yaw-attitude modelling during nominal and eclipse seasons.
!  This subroutine "attitude.f03" has been obtained by the partial conversion
! of the GNSS Yaw-attitude Tool (GYT) standalone program to subroutine  
! ----------------------------------------------------------------------
! Input arguments:
! ----------------------------------------------------------------------
! - mjd:			Modified Julian Day number (including the fraction of the day) in Terrestrial Time (TT)
! - rsat:			Satellite Position vector (m)   in inertial frame (ICRF)
! - vsat:			Satellite Velocity vector (m/s) in inertial frame (ICRF)
! - rSun:			Sun Position vector (m)   in inertial frame (ICRF)
! - PRNsat:		GNSS PRN number (Constellation ID + Number) e.g. G03
! - BLKsat:			GNSS satellites block type
! - satblk:			GPS (only) Satellite Block ID according to the numbering adopted 
!					in eclips.f:     1=I, 2=II, 3=IIA, IIR=(4, 5), IIF=6
! - BDSorbtype:		Beidou (only) satellite orbit type:  MEO, IGSO, GEO
!
! Output arguments:
! - eclipsf:		Satellite attitude status flag:
!					0 = Nominal
!					1 = Non-nominal, midnight
!					2 = Non-nominal, noon
! - beta:			Sun angle with the orbital plane in degrees (see Note 1)
! - Yangle: 		Yaw angle array (in degrees) with 2 values: Yaw nominal and Yaw modelled
!					During nominal periods, the two values are equal
! - Mangle:			Orbit angle between satellite and orbit midnight (degrees)
! - eBX_nom:		Body-X axis unit vector based on noninal yaw-attitude
! - eBX_ecl:		Body-X axis unit vector based on modelled yaw-attitude during eclipse seasons (Set to eBX_nom when out of eclipse season) 
!
! ----------------------------------------------------------------------
! Note 1:
!  Beta angle is computed based on the input vectors in inertial system
!  Sign conventions for beta angle:
!   Postiive: above orbital plane
!   Negative: below orbital plane  	
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	July 2016
! ----------------------------------------------------------------------
! Last modified
! - Dr. Thomas Papanikolaou, 21 November 2018:
!	GYT program upgrade from Fortran 90 to Fortran 2003 for calling the modified 
!   subroutines (upgraded to F03) of the POD code package 
! - Dr. Thomas Papanikolaou, 25 March 2019:
!   The major part of the GYT program has been converted into subroutine 
!   for the integration to the Precise Orbit Determination (POD) package  
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      !USE mdl_planets
      !USE mdl_write
      !USE mdl_arr
      !USE mdl_eop
      !USE m_eop_data
      !USE m_eop_cor
      !USE m_eop_igu
      !USE m_interporb
      !USE m_keplerorb
      !USE m_writearray
      !USE m_sp3
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: rsat, vsat
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: rSun
      CHARACTER (LEN=3) , INTENT(IN) :: PRNsat
      CHARACTER (LEN=20), INTENT(IN) :: BLKsat
!      INTEGER (KIND = 4) , INTENT(IN) :: satblk
!      CHARACTER (LEN=5), INTENT(IN) :: BDSorbtype
	  
! OUT
      INTEGER (KIND = 4), INTENT(OUT) :: eclipsf
      REAL (KIND = prec_d), INTENT(OUT) :: beta, Mangle, Yangle(2)
	  REAL (KIND = prec_d), Dimension(3), INTENT(OUT) :: eBX_nom, eBX_ecl
! ----------------------------------------------------------------------
 
! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: yaw_mod
      CHARACTER (LEN=1) :: GNSSid
      CHARACTER (LEN=2) :: PRN_charnum
      INTEGER (KIND = 4) :: PRN_num, PRN_eclips
      INTEGER (KIND = 4) :: satbf
      INTEGER (KIND = 4) :: orbdir
      INTEGER (KIND = prec_int2) :: ios  
      CHARACTER (LEN=100) :: fmt_line
      CHARACTER (LEN=100) :: realine
      REAL (KIND = prec_d) :: r_CRS(3), v_CRS(3)
      REAL (KIND = prec_d) :: r_sun_crs(3)
      INTEGER (KIND = prec_int4) :: time_in
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
! ----------------------------------------------------------------------
! yaw_attitude.f90
      REAL (KIND = prec_d) :: Mangle_e, Mrate_e, Ynom_e
! ----------------------------------------------------------------------
! yawdyn.f90
      REAL (KIND = prec_d) :: beta0
      INTEGER (KIND = prec_int8) :: dparam
! BDS
      !CHARACTER (LEN=5) :: BDSorbtype
      REAL (KIND = prec_d) :: beta_t0 
      INTEGER (KIND = prec_int2) :: BetaP
      INTEGER (KIND = prec_int8) :: NPint	  
! ----------------------------------------------------------------------
! Phase Wind-up effect 
      !REAL (KIND = prec_d) :: r_station(3), rstat_crs(3), rstat_trs(3), rstat_ref(3)
	  !REAL (KIND = prec_d) :: dphi_wup_nom, dphi_wup_ecl
	  !REAL (KIND = prec_d) :: deg2rad_mult, rad2cycles_mult
! ----------------------------------------------------------------------
      INTEGER (KIND = 4) :: satblk
      CHARACTER (LEN=5)  :: BDSorbtype


! ----------------------------------------------------------------------
! GNSS Satellite Block Type
! ----------------------------------------------------------------------
! BLK_TYP :: Global variable in mdl_param
! GPS case: Satellite Block ID:        1=I, 2=II, 3=IIA, IIR=(4, 5), IIF=6
!BLKTYP = BLKsat

satblk = 6
IF(BLKsat=='GPS-I')			  THEN
	satblk = 1
ELSE IF(BLKsat=='GPS-II')	  THEN
	satblk = 2
ELSE IF(BLKsat=='GPS-IIA') 	  THEN
	satblk = 3
ELSE IF(BLKsat=='GPS-IIR')	  THEN
	satblk = 4
ELSE IF(BLKsat=='GPS-IIR-A')  THEN
	satblk = 5
ELSE IF(BLKsat=='GPS-IIR-B')  THEN
	satblk = 5
ELSE IF(BLKsat=='GPS-IIR-M')  THEN
	satblk = 5
ELSE IF(BLKsat=='GPS-IIF')    THEN
	satblk = 6
END IF
! ----------------------------------------------------------------------
! Beidou case: 'IGSO', 'MEO'
! 1. BDSorbtype = 'IGSO'
! 2. BDSorbtype = 'MEO'
! 3. BDSorbtype = 'IGSO'
BDSorbtype = 'MEO'
IF(BLKsat=='BDS-2G'.or.BLKsat == 'BDS-3G')            BDSorbtype = 'GEO'  
IF(BLKsat=='BDS-2I'.or.BLKsat == 'BDS-3I'.or.&
   BLKsat=='BDS-3SI-SECM'.or.BLKsat =='BDS-3SI-CAST') BDSorbtype = 'IGSO' 
IF(BLKsat=='BDS-2M'.or.BLKsat == 'BDS-3M'.or.&
   BLKsat=='BDS-3M-SECM'.or.BLKsat =='BDS-3M-CAST')   BDSorbtype = 'MEO'
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Satellite Position & Velocity vector in ICRF
r_CRS = rsat
v_CRS = vsat
! Sun position vector in ICRF
r_sun_crs = rSun
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! GNSS Yaw-attitude Tool (GYT) program configuration
! ----------------------------------------------------------------------
! Start of INPUT Configuration parameters:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Yaw attitude model/method
! ----------------------------------------------------------------------	    
! 1. Yaw attitude modelling applied based on the models adopted for each GNSS constellation
!    Set yaw_mod variable to the GNSSid (GNSS constellation id letter)
!    GNSS constellation id letters
!    G: GPS
!    R: GLONASS
!    E: Galileo
!	 C: BDS (BeiDou)
! ----------------------------------------------------------------------
! 2. Dynamic Yaw-steering method 
!    The yaw_mod variable should set to 'D' for special cases of  
!    Galileo and Beidou satellites that apply the dynamic yaw-steering method
! ----------------------------------------------------------------------
!yaw_mod = 'G'
!yaw_mod = 'R'
!yaw_mod = 'E'
!yaw_mod = 'C'
!yaw_mod = 'D'

READ (PRNsat, FMT='(A1)' , IOSTAT=ios) GNSSid
yaw_mod = GNSSid
!print *,"yaw_mod", yaw_mod
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Beidou case only:
! ----------------------------------------------------------------------
! Approach for the beta angle computation at the latest epoch that the orbital angle M is equal to 90 degrees
! ----------------------------------------------------------------------
! BetaP = 1 : Orbit backward prediction based on Keplerian elements  
! BetaP = 2 : Orbit backward computation based on numerical interpolation of the precise orbit data sp3 
BetaP = 1
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------	  
! Satellite Body-fixed frame definition type
! ----------------------------------------------------------------------	  
! satbf = 1 : Body-fixed frame according to the IGS Conventions; Cases: GPS Block II,IIA,IIF, GLONASS, BeiDou  
! satbf = 2 : Body-fixed frame X,Y axes reversal; Cases: Galileo, GPS Block IIR 
satbf = 1
! ----------------------------------------------------------------------	


! ----------------------------------------------------------------------
! Dynamic yaw-steering method
! ----------------------------------------------------------------------
! yawdyn.f90 subroutine input arguments to be configured
! ----------------------------------------------------------------------	  
! Beta angle condition limit for the "dynamic yaw steering" method
beta0 = 2.0D0
! Design parameter
dparam = 258
! ----------------------------------------------------------------------	  


! ----------------------------------------------------------------------
! End of INPUT Configuration
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Satellite PRN number 
! ----------------------------------------------------------------------
! Consisted by the GNSS constellation id letter (GNSSid) and the number (PRN_num)  
! ----------------------------------------------------------------------
! GNSSid:		GNSS constellation id letter
! 				G: GPS
! 				R: GLONASS
! 				E: Galileo
! 				C: BDS (BeiDou)
! 				J: QZSS
! 				S: SBAS
! ----------------------------------------------------------------------
! PRN_num:		PRN numbering as adopted in the sp3 format (Numerical value after the GNSS constellation id letter)
! ----------------------------------------------------------------------
!fmt_line = '(A1,I2.2)'
READ (PRNsat, FMT='(A1,I2.2)' , IOSTAT=ios) GNSSid, PRN_num
!print *, "PRNsat", PRNsat
!print *, "GNSSid  ", GNSSid
!print *, "PRN_num ", PRN_num


! ----------------------------------------------------------------------
! PRN_eclips:	PRN number as adopted in the eclips.f subroutine numbering
!			 	GPS: 		1-32 
! 				GLONASS:	33-64
!				Galileo:	65-100
!				BeiDou:		101-136
! TODO:                         QZSS: ???
! ----------------------------------------------------------------------
If (GNSSid == 'G') then
    PRN_eclips = PRN_num
Else if (GNSSid == 'R') then
    PRN_eclips = PRN_num + 32
Else if (GNSSid == 'E') then
    PRN_eclips = PRN_num + 64
Else if (GNSSid == 'C') then
    PRN_eclips = PRN_num + 100
End If
! ----------------------------------------------------------------------

!write (*,*) "r_CRS:", r_CRS(1), r_CRS(2), r_CRS(3), "v_CRS: ", v_CRS(1), v_CRS(2), v_CRS(3), &
!        "r_sun_crs: ", r_sun_crs(1), r_sun_crs(2), r_sun_crs(3)
! ----------------------------------------------------------------------
! Beta angle
CALL beta_angle (r_CRS, v_CRS, r_sun_crs, beta)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Time System of input epoch:
! ----------------------------------------------------------------------
! 1. TT
! 2. GPS time
! 3. UTC
! 4. TAI
time_in = TT_time
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! "Time Systems" transformation											 
! ----------------------------------------------------------------------
      IF (time_in == TT_time) THEN
	     CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
      ELSE IF (time_in == GPS_time) THEN 
	     CALL time_GPS (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
      ELSE IF (time_in == UTC_time) THEN 
	     CALL time_UTC (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
      ELSE IF (time_in == TAI_time) THEN 
         CALL time_TAI (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
      END IF 
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Yaw angle computation based on the configured attitude model
! ----------------------------------------------------------------------	    

! ----------------------------------------------------------------------	    
! GPS and GLONASS yaw-attitude model
! ----------------------------------------------------------------------	    
If (yaw_mod == 'G' .or. yaw_mod == 'R') Then

! Eclipsing attitude modelling is computed based on the modified version of the eclips.f
! orbdir: Direction of processing (1=FORWARD, -1=BACKWARD); eclips.f input argument
orbdir = 1 

! Nominal and Eclipsing Yaw-attitude
CALL yaw_attitude (mjd_GPS , r_CRS, v_CRS, r_sun_crs, beta, PRN_eclips, satblk, orbdir, & 
				   eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle, Mangle_e, Mrate_e, Ynom_e, satbf)
				   
!PRINT *,"Yangle", Yangle(1), Yangle(2)
if (eclipsf == 0) then
    Yangle(2) = Yangle(1)
end if
! ----------------------------------------------------------------------	    

! ----------------------------------------------------------------------	    
! Galileo attitude law
! ----------------------------------------------------------------------	    
Else if (yaw_mod == 'E') Then

! Yaw-steering as provided by the European GNSS Service Centre 
!CALL yaw_gal (mjd_GPS, r_CRS, v_CRS, r_sun_crs, beta, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle, satbf)
CALL yaw_gal (mjd_GPS, r_CRS, v_CRS, r_sun_crs, beta, PRNsat, BLKsat, satbf, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle)
!PRINT *,"Yangle", Yangle(1), Yangle(2)

! ----------------------------------------------------------------------	    

! ----------------------------------------------------------------------	    
! BeiDou attitude law
! ----------------------------------------------------------------------	    
Else if (yaw_mod == 'C') Then

! Beta angle based on interpolation is cancelled
NPint = 0

CALL yaw_bds (mjd_GPS, r_CRS, v_CRS, r_sun_crs, BDSorbtype, satbf, BetaP, NPint, & 
			  beta, beta_t0, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle)
! ----------------------------------------------------------------------	    

! ----------------------------------------------------------------------	    
! Dynamic Yaw-steering method
! ----------------------------------------------------------------------	    
Else if (yaw_mod == 'D') Then

CALL yawdyn (mjd_GPS, r_CRS, v_CRS, r_sun_crs, satbf, beta0, dparam, beta, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle)
! ----------------------------------------------------------------------	    

End If
! ----------------------------------------------------------------------

if (1<0) then
print *,"yaw_mod", yaw_mod
print *,"Yangle ", Yangle, Mangle
print *,"eBX_nom", eBX_nom
print *,"eBX_ecl", eBX_ecl
end if


End 

