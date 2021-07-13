SUBROUTINE yaw_gal (mjd, r_sat, v_sat, r_sun, beta_angle, PRNsat, BLKsat, satbf, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_gal.f90
! ----------------------------------------------------------------------
! Purpose:
!  Galileo yaw-attitude based on the yaw-steering law provided by the 
!  European GNSS Service Center  
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - r_sun:		Sun position vector (m) in ITRF
! - beta_angle:	Sun angle with respect to the orbital plane (degrees)
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
! - PRNsat:		PRN number of GNSS satellites (Constellation ID + Number) e.g. G03
! - BLKsat:		GNSS satellites Block
!
! Output arguments:
! - eclipsf:	non-nominal attitude status flag:
!				0 = Nominal
!				1 = Non-nominal, midnight
!				2 = Non-nominal, noon
! - eBX_nom:	Body-X axis unit vector during noninal yaw-attitude
! - eBX_ecl:	Body-X axis unit vector during eclipsng yaw-attitude (Set to eBX_nom when out of eclipse season) 
! - Yangle: 	Yaw angle array (in degrees) with 2 values: Yaw nominal and Yaw modelled
!				During nominal periods, the two values are equal
! - Mangle:		Orbit angle between satellite and orbit midnight (deg)
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia           29 May 2017
! ----------------------------------------------------------------------
! Last modified:
! 15/04/2020	Thomas Papanikolaou
!				Modified for supporting the new Galileo attitude law for FOC and IOV satellites
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd, r_sat(3), v_sat(3), r_sun(3), beta_angle
      CHARACTER (LEN=3) , INTENT(IN) :: PRNsat
      CHARACTER (LEN=20), INTENT(IN) :: BLKsat
      INTEGER (KIND = 4), INTENT(IN) :: satbf 
	  
! OUT
      REAL (KIND = prec_d) :: Yangle(2)
	  REAL (KIND = prec_d) :: eBX_nom(3), eBX_ecl(3)
      REAL (KIND = prec_d) :: Mangle
      INTEGER (KIND = 4) :: eclipsf
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = 4) :: BLKsat_GAL
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Galileo Satellite Block Type
! ----------------------------------------------------------------------
BLKsat_GAL = 1
IF(BLKsat=='GAL-1')			  THEN
	BLKsat_GAL = 1
ELSE IF(BLKsat=='GAL-2')	  THEN
	BLKsat_GAL = 2
END IF

! Galileo attitude law
IF (BLKsat_GAL == 1 ) THEN
! Galileo IOV satellites attitude law
	CALL yaw_gal_iov (mjd, r_sat, v_sat, r_sun, beta_angle, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle, satbf)

ELSEIF (BLKsat_GAL == 2) THEN
! Galileo FOC satellites attitude law
	CALL yaw_gal_foc (mjd, r_sat, v_sat, r_sun, beta_angle, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle, satbf)

END IF


END
