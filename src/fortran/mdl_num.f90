MODULE mdl_num

! ----------------------------------------------------------------------
! Purpose:
!  Module for setting adopted "Numerical Standards" as global variables
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia             July 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			
	  
! ----------------------------------------------------------------------
! Constants declaration
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GM: Earth Gravity Constant (m^3/sec^2)
! ----------------------------------------------------------------------
! GM value provided by the latest Gravity Field Models
       REAL (KIND = prec_q), PARAMETER :: GM_gfm = 0.39860044150D+15
! GM value adopted by the IERS Conventions 2010
       REAL (KIND = prec_q), PARAMETER :: GM_iers = 0.39860044180D+15
! GM value to be used as global variable here
       REAL (KIND = prec_q), PARAMETER :: GM_global = GM_gfm
!       REAL (KIND = prec_q) :: GM_global
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GMsun: Sun Gravity Constant (m^3/sec^2)
      REAL (KIND = prec_q)  :: GMsun_glb 
! GMmoon: Moon Gravity Constant (m^3/sec^2)
      REAL (KIND = prec_q) :: GMmoon_glb
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth Radius (meters):
! radius value provided by the latest Gravity Field Models
       REAL (KIND = prec_q), PARAMETER :: Earth_radius = 0.6378136460D+07
       REAL (KIND = prec_q), PARAMETER :: Sun_radius = 696.0d+06
       REAL (KIND = prec_q), PARAMETER :: Moon_radius = 1738.0d+03
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! PI
! ----------------------------------------------------------------------
      REAL (KIND=prec_q), PARAMETER :: PI_global = 4D0 * ATAN(1D0)
      !REAL (KIND=prec_q), PARAMETER :: PI_global = 4Q0 * ATAN(1Q0)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Speed of light (m/sec)
      REAL (KIND = prec_q), PARAMETER :: cslight = 299792458.0D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Relativity parameters: beta and gama
! PPN (parameterized post-Newtonian) parameters  
! PPN equal to 1 in General Relativity
      REAL (KIND = prec_q), PARAMETER :: ppn_beta = 1.0D0
      REAL (KIND = prec_q), PARAMETER :: ppn_gama = 1.0D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Other constants
! ---------------------------------------------------------------------
      REAL (KIND = prec_q), PARAMETER :: S0 =  1367.0D0 !solar constant
      REAL (KIND = prec_q), PARAMETER :: AU =  149597870691.D0 !earth-sun dist
      REAL (KIND = prec_q), PARAMETER :: TOA = 6371000.D0 + 30000.D0 !Top of troposphere
      REAL (KIND = prec_q), PARAMETER :: ALB = 0.3 ! Earth albedo
      REAL (KIND = prec_q), PARAMETER :: Ps = S0/cslight !solar radiation pressure
END
