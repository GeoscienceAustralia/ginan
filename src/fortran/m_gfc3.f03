MODULE m_gfc3


! ----------------------------------------------------------------------
! MODULE: m_gfc3.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for the subroutines that read the gravity field model data 
! (parameters and spherical harmonic coefficients)
! 
! Subroutines contained within this module:
! - gfc3_iers.f90: IERS Conventional geoptential model
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	24 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains



SUBROUTINE gfc3_iers (gfmfilename, n_trunc, sigma_shc, mjd_t, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)

! ----------------------------------------------------------------------
! SUBROUTINE: gfc3_iers.f90
! ----------------------------------------------------------------------
! Purpose:
!  Geopotential modelling according to the IERS Conventions 2010 (Petit and Luzum 2010)
! ----------------------------------------------------------------------
! Input arguments:
! - gfmfilename:        Data file name of the global Earth gravity field model in ICGEM format
!						This gravity model is only used as the static model.
! - n_trunc:            Degree truncation limit for the Spherical Harmonic Coefficients (SHC)
!                       (set to negative values for reading all SHC)
! - sigma_shc:          Set to any value or 0 for reading or not the
!                       coefficients errors (sigmaC and sigmaS)
! - mjd_t:				Modified Julian Day Number of the input epoch
!						including the fraction of the day
!
! Output arguments:
! - GM_gfc:       	Earth gravity constant (m^3/sec^2)
! - ae_gfc:    		Radius  (meters)
! - Nmax_gfc:  		Gravity model's maximum degree
! - tide_gfc:		Gravity model's tide system: zero_tide or tide_free
! - Cnm:        	Normalized spherical harmonics coefficients C (dynamic allocatable array)
! - Snm:        	Normalized spherical harmonics coefficients S (dynamic allocatable array)
! - sCnm:     		Errors (variances) of C spherical harmonics coefficients (dynamic allocatable array)
! - sSnm:     		Errors (variances) of S spherical harmonics coefficients (dynamic allocatable array)
! ----------------------------------------------------------------------
! Remark:
!  Cnm and Snm arrays are formed into lower triangular matrices.
!  Coefficient Cnm corresponds to the matrix element Cnm(n+1,m+1)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	January 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 25 October 2017:
!   The subroutine has been modified in order to use advantages of Fortran 2003 in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
!      USE mdl_gfc
      USE m_gfc
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int2), INTENT(IN) :: n_trunc
      INTEGER (KIND = prec_int2), INTENT(IN) :: sigma_shc
      CHARACTER (*), INTENT(IN) :: gfmfilename
      REAL (KIND = prec_q), INTENT(IN) :: mjd_t
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: GM_gfc, ae_gfc
      INTEGER (KIND = prec_int8), INTENT(OUT) :: Nmax_gfc 
      CHARACTER (LEN=50), INTENT(OUT) :: tide_gfc  
	  ! Dynamic Allocatable arrays
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Cnm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Snm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: sCnm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: sSnm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: Ntv_trunc
      INTEGER IY, IM, ID, J_flag
      DOUBLE PRECISION DJ1, DJ2, FD, DJM0, DJM
      REAL (KIND = prec_d) :: dt, mjd_2000
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: epoch, xp_mean, yp_mean, arcsec2rad
      !REAL*8 epoch, xp_mean,yp_mean
      INTEGER error,version
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: C20, C30, C40, C22, S22, dC20, dC30, dC40
      REAL (KIND = prec_d) :: C20_t, C30_t, C40_t
      REAL (KIND = prec_d) :: C21_t, S21_t
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Factor for converting arcsec to radians
      arcsec2rad = PI_global / (3600.0D0 * 180.0D0)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! MJD
      CALL iau_CAL2JD ( 2000, 1, 1, DJM0, DJM, J_flag )
      mjd_2000 = DJM + 0.5
      	  
      DJ1 = 2400000.5D0
      DJ2 = mjd_t
      CALL iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J_flag )
      epoch = IY*1D0 + IM*1D0 / 12D0 + ID*1D0 / 365.25D0 + FD / 365.25D0 														
! ----------------------------------------------------------------------
      !PRINT *,"MJD2000.0, DJM", mjd_2000, DJM
      !PRINT *,"epoch", epoch


! ----------------------------------------------------------------------
! Static gravity field model	  
! ----------------------------------------------------------------------
!CALL gfc1 (gfmfilename, n_trunc, sigma_shc, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)
!CALL gfc2 (gfmfilename, n_trunc, sigma_shc, mjd_t, 0, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)
Ntv_trunc = 0
CALL gfc2 (gfmfilename, n_trunc, sigma_shc, mjd_t, Ntv_trunc, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)
! ----------------------------------------------------------------------
print *,"GM_gfc2",GM_gfc

! ----------------------------------------------------------------------
! Static gravity model coefficients
! ----------------------------------------------------------------------
! C30, C40
      C30 = Cnm(3 +1 , 0 +1)
      C40 = Cnm(4 +1 , 0 +1)
! C22, S22
      C22 = Cnm(2 +1 , 2 +1)
      S22 = Snm(2 +1 , 2 +1)
! ----------------------------------------------------------------------

 
 
! ----------------------------------------------------------------------
! IERS Conv. 2010 : Table 6.2
! ----------------------------------------------------------------------
! C20 zero-tide coefficient
      C20 = -0.48416948D-3
      tide_gfc = 'zero_tide' ! Global variable in module "mdl_gfc.f90"
! ----------------------------------------------------------------------
! Rates
	  dC20 = 11.6D-12
	  dC30 =  4.9D-12
	  dC40 =  4.7D-12
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! IERS Conv. 2010 : Eq. 6.4
! ----------------------------------------------------------------------
      dt = (mjd_t - mjd_2000) / 365.25D0			! dt in years since 2000
      !dt = epoch - (2000D0 + 0.5D0 / 365.25D0) 	! dt in years since 2000
      !PRINT *, "dt in ys", dt
	  
      C20_t = C20 + dC20 * dt
      C30_t = C30 + dC30 * dt
      C40_t = C40 + dC40 * dt
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Conventional Mean Pole
! ----------------------------------------------------------------------
! Angular coordinates of conventional mean pole (in seconds of arc)
	  version = 2015
      CALL IERS_CMP_2015 (version, epoch, xp_mean, yp_mean, error)
! Conversion in radians
      xp_mean = xp_mean * arcsec2rad
      yp_mean = yp_mean * arcsec2rad
! ----------------------------------------------------------------------
      !PRINT *,"xp_mean,yp_mean,error", xp_mean,yp_mean,error


! ----------------------------------------------------------------------
! IERS Conv. 2010 : Eq. 6.5
! ----------------------------------------------------------------------
! C21
      C21_t = sqrt(3D0) * xp_mean * C20 - xp_mean * C22 + yp_mean * S22
! S21
      S21_t = -1.0D0 * sqrt(3D0) * yp_mean * C20 - yp_mean * C22 - xp_mean * S22
! ----------------------------------------------------------------------

      !PRINT *, "----------------------- gfc3_iers.f90 --------------------------------"
      !PRINT *, "delta C20", Cnm(2 +1 , 0 +1) - C20_t
      !PRINT *, "delta C30", Cnm(3 +1 , 0 +1) - C30_t
      !PRINT *, "delta C40", Cnm(4 +1 , 0 +1) - C40_t
      !PRINT *, "delta C21", Cnm(2 +1 , 1 +1) - C21_t
      !PRINT *, "delta S21", Snm(2 +1 , 1 +1) - S21_t
      !PRINT *, "C20, C30, C40", C20_t, C30_t, C40_t
      !PRINT *, "C21_t, S21_t", C21_t, S21_t 
      !PRINT *, "----------------------------------------------------------------------"
	  
! ----------------------------------------------------------------------
! Cnm, Snm matrices : Update variables values through module "mdl_gfc.f90"
! ----------------------------------------------------------------------
      Cnm(2 +1 , 0 +1) = C20_t
      Cnm(3 +1 , 0 +1) = C30_t
      Cnm(4 +1 , 0 +1) = C40_t
      Cnm(2 +1 , 1 +1) = C21_t
      Snm(2 +1 , 1 +1) = S21_t
! ----------------------------------------------------------------------


END SUBROUTINE




End

