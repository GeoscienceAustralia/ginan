MODULE mdl_param

! ---------------------------------------------------------------------------
! Purpose:
!  Module for setting orbit modelling parameters as global variables 
!  to be used from various part of the source code 
! ---------------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	November 2017
! Changes:      12-12-2018 Dr. Tzupang Tseng : added specific global parameters
! for ECOM-based SRP model
!                5-03-2020 John Donovan: make sinex file a global array
! 			17 August 2020 Dr. Thomas Papanikolaou: pseudo-stachastic pulses added  
! ----------------------------------------------------------------------

      USE mdl_precision
! Remove bind(c) bits for now - can't make them work. (SCM 20200430)
!      use iso_c_binding, only : c_int, c_long, c_double, c_short
      IMPLICIT NONE
      SAVE 			

! ---------------------------------------------------------------------------
      REAL (KIND = prec_d) :: ORBPRED_ARC_glb

! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Time System of orbit ouput files defined by the input configuration file
!      CHARACTER (LEN=3) :: TIME_SCALE
      INTEGER :: DOY, YR ! Day of year

! ---------------------------------------------------------------------------
! Parameter corrleation matrix
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: corrl
! ---------------------------------------------------------------------------
! Satellite information file
      CHARACTER(LEN=20) :: BLKTYP     ! block type
      INTEGER(KIND = prec_int2)    :: SVNID      ! SVN 
      INTEGER(KIND = prec_int2)    :: BLKID      ! block type number
      INTEGER(KIND = prec_int4)    :: POWER      ! transmitted power in watts
      REAL(KIND = prec_d)          :: MASS       ! S/C mass in kg
! --------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Orbit arc length (sec)
      REAL (KIND = prec_d) :: orbarc
      REAL (KIND = prec_d) :: orb_est_arc
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Numerical integration method ID
      INTEGER (KIND = prec_int2) :: integmeth	  
! Numerical integration step size (sec)
      REAL (KIND = prec_d) :: integstep
! ---------------------------------------------------------------------------

      INTEGER (KIND=prec_int2) :: mVEQ, mEQM
      parameter (mEQM = 0, mVEQ = 1)

! ---------------------------------------------------------------------------
! Initial Conditions
! Initial Epoch: MJD and Seconds since 00h in Terrestrial Time (TT)
      REAL (KIND = prec_d) :: MJD_to
      REAL (KIND = prec_d) :: SEC_to
! Initial State Vector in ICRF
      REAL (KIND = prec_d) :: SVEC_Zo(6)
      REAL (KIND = prec_d) :: SVEC_Zo_ESTIM(6)
! Initial Conditions' satellite matrix 
      REAL (KIND = prec_d), DIMENSION(:), ALLOCATABLE :: IC_sat_glb	 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: IC_sat_pulse_glb 
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! GNSS satellite PRN number e.g. G03 	  
      CHARACTER (LEN=3) :: PRN
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! External Orbit arrays (prm_orbext.f03, prm_pseudobs.f03)
! Allocatable Arrays	  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbext_kepler, orbext_ICRF, orbext_ITRF
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbext_kepler2,orbext_ICRF2, orbext_ITRF2
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: pseudobs_ITRF, pseudobs_ICRF
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: pseudobs_ITRF2, pseudobs_ICRF2
!      INTEGER (KIND = prec_int2) :: ORBEXT_glb
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Leap Second TAI-UTC Arrays 
      INTEGER   (KIND = prec_int4),DIMENSION(:,:),ALLOCATABLE :: IDAT
      INTEGER   (KIND = prec_int4)                            :: NDAT
      REAL      (KIND = prec_q),DIMENSION(:),ALLOCATABLE      :: DATS

! ---------------------------------------------------------------------------
! Force Model
! ---------------------------------------------------------------------------

! ----------------------------------------------------------------------
! Gravitational forces modelling considered (on/off): defined in prm_grav.f03 
!INTEGER (KIND = prec_int2) :: FMOD_GRAV(4)
INTEGER (KIND = prec_int2) :: FMOD_GRAVFIELD
!INTEGER (KIND = prec_int2) :: FMOD_PLANETS(11)
!INTEGER (KIND = prec_int2) :: FMOD_TIDES(5)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Gravity Field Model parameters: setting via module m_gfc.f03
! ----------------------------------------------------------------------
! - GFM_GM:			Earth gravity constant (m^3/sec^2)
! - GFM_ae: 		radius (meters)
! - GFM_degmax:		Maximum degree of the gravity model
! - GFM_tide:		Tide System
! - Cnm, Snm:		Normalized Spherical Harmonic coefficients
! - sCnm, sSnm:		Errors, coefficients variances  
REAL (KIND = prec_q) :: 		GFM_GM
REAL (KIND = prec_q) :: 		GFM_ae
INTEGER (KIND = prec_int8) ::	GFM_degmax 
CHARACTER (LEN=50) ::			GFM_tide 
	  
! Truncation maximum degree and order: user defined (via input parameterization) 
INTEGER (KIND = prec_int8) :: GFM_Nmax, GFM_Mmax

! Spherical Harmonic Coefficients (SHC) lower triangular matrices (Dynamic Allocatable arrays)
REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: GFM_Cnm
REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: GFM_Snm
! Errors/Variances of SHC (Dynamic Allocatable arrays)
REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: GFM_sCnm
REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: GFM_sSnm
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Ocean Tides model
! Truncation maximum degree and order: user defined (via input parameterization) 
INTEGER (KIND = prec_int8) :: OCEAN_Nmax, OCEAN_Mmax
! ---------------------------------------------------------------------------

! ----------------------------------------------------------------------
! Non-gravitational forces modelling considered (on/off): defined in prm_nongrav.f03 
!INTEGER (KIND = prec_int2) :: FMOD_NONGRAV(3)
! ----------------------------------------------------------------------

! ECOM-based solar radiation pressure model
! ----------------------------------------------------------------------
INTEGER (KIND = prec_int8) :: ECOMNUM
!REAL (KIND = prec_q) :: ECOM_accel_glb(9)
!REAL (KIND = prec_q) :: ECOM_accel_aposteriori(9)
REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: ECOM_accel_glb

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Empirical forces
INTEGER (KIND = prec_int8) :: EMPNUM
REAL (KIND = prec_q) :: Bias_accel_glb(3), Bias_accel_aposteriori(3)

REAL (KIND = prec_q) :: CPR_CS_glb(3,2), CPR_CS_aposteriori(3,2)

INTEGER (KIND = prec_int2) :: Frame_EmpiricalForces_glb
REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: EMP_accel_glb
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Number of parameters of Empirical models (Empirical Forces and/or ECOM models)
INTEGER (KIND = prec_int8) :: NPARAM_EMP_ECOM_glb
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Variational Equations
! ----------------------------------------------------------------------
! Numerical Integration of Variational Equations
!      INTEGER (KIND = prec_int2) :: VEQ_integration_glb 
! Number of parameters to be estimated
!INTEGER (KIND = prec_int8) ::	N_PARAM 
INTEGER (KIND = prec_int8) :: NPARAM_glb
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Estimator method
!      INTEGER (KIND = prec_int2) :: ESTIM_mode_glb 
!      INTEGER (KIND = prec_int2) :: ESTIM_iter_glb 
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Attitude models
      INTEGER (KIND = 4) :: SATblock_glb
      CHARACTER (LEN=5)  :: BDSorbtype_glb
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Boxwing properties
! ----------------------------------------------------------------------

      integer (kind=4) :: max_blk   ! Max number of block types indices
                          ! 1-10 GPS, 11-20 Glonass, 21-30 Galileo, 31-40 BDS, 41-50 QZSS
      parameter ( max_blk = 50 ) 
!
!     EARTH AND SATELLITE PROPERTIES
      REAL (kind=prec_q) :: AREA(4,2,max_blk),REFL(4,2,max_blk)
      REAL (kind=prec_q) :: DIFU(4,2,max_blk),ABSP(4,2,max_blk)
      REAL (kind=prec_q) :: AREA2(4,2,max_blk),REFL2(4,2,max_blk)
      REAL (KIND=prec_q) :: DIFU2(4,2,max_blk),ABSP2(4,2,max_blk)
      REAL (kind=prec_q) :: REFLIR(4,2,max_blk)
      REAL (kind=prec_q) :: DIFUIR(4,2,max_blk),ABSPIR(4,2,max_blk)
!
      REAL (kind=prec_q) :: CERES_R(72,144),CERES_E(72,144)
      REAL (kind=prec_q) :: CERGRE(72,144),CERGEM(72,144)
      REAL (kind=prec_q) :: D_AREA_ALL(72,144),V_NS_ALL(72,144,3)

      ! size of the above arrays
      INTEGER (kind = 4) :: LATKMX, LONKMX
      REAL (kind=prec_q) :: GRDNUM

      CHARACTER (LEN=2) :: F_MONTH(12)

      integer (kind=4) known_blkids(max_blk)  ! Block numbers by the type
                  ! we known and have coded.  0 value unknown

! ----------------------------------------------------------------------
! Sinex data
! IMPORTANT: put bigger members in front of smaller members. Otherwise
! you may have access issues because 8-byte quantities must start on an 8-byte
! boundary.  'Bus' errors may occur otherwise.
! NB Satellites can have different PRNs for different time windows
! ----------------------------------------------------------------------
!!!! Remove bind(c) bits for now - can't make them work. (SCM 20200430) !!!
!TYPE, bind(c) :: sinex
!        REAL (c_double)      :: MASS
!        REAL (c_double)      :: E_PX, E_PY, E_PZ ! P-eccentricity (not epoch bound)
!        REAL (c_double)      :: E_LX, E_LY, E_LZ ! L-eccentricity (not epoch bound)
!        REAL (c_double)      :: COM_X, COM_Y, COM_Z ! center-of-mass
!        REAL (c_double)      :: TSTART, TSTOP
!        INTEGER (c_int)      :: startyr, startdoy, startsod
!        INTEGER (c_int)      :: stopyr, stopdoy, stopsod
!        INTEGER (c_int)      :: POWER
!        INTEGER (c_int)      :: FRQCHN  ! only for GLONASS
!        INTEGER (c_short)    :: BLKID   ! see above
!        CHARACTER (len=20)   :: BLKTYP  ! character definition of above
!        CHARACTER (len=10)   :: COSPAR  ! ???
!        CHARACTER (len=5)    :: SVN
!        CHARACTER (len=4)    :: PRN
!END TYPE 
!!!!
       
TYPE :: sinex
        REAL (kind=prec_q)       :: MASS
        REAL (kind=prec_q)       :: E_PX, E_PY, E_PZ ! P-eccentricity (not epoch bound)
        REAL (kind=prec_q)       :: E_LX, E_LY, E_LZ ! L-eccentricity (not epoch bound)
        REAL (kind=prec_q)       :: COM_X, COM_Y, COM_Z ! center-of-mass
        REAL (kind=prec_q)       :: TSTART, TSTOP
        INTEGER (kind=prec_int4) :: startyr, startdoy, startsod
        INTEGER (kind=prec_int4) :: stopyr, stopdoy, stopsod
        INTEGER (kind=prec_int4) :: POWER
        INTEGER (kind=prec_int4) :: FRQCHN  ! only for GLONASS
        INTEGER (kind=prec_int2) :: BLKID   ! see above
        CHARACTER (len=20)       :: BLKTYP  ! SVN Block Type Identifier
        CHARACTER (len=10)       :: COSPAR  ! COSPAR ID
        CHARACTER (len=4)        :: SVN     ! Satellite SVN Identifier
        CHARACTER (len=3)        :: PRN     ! Satellite PRN Identifier
END TYPE        

        INTEGER (Kind=4)          MAX_SAT ! Maximum we can handle
        parameter (MAX_SAT = 2048)

TYPE (sinex) satellites(MAX_SAT)
        INTEGER (Kind=8)          SAT_COUNT ! actual number of satellites
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Pseudo-stochastic pulses
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: PULSE_dir_glb
      REAL (KIND = prec_q) :: MJD_DELTA_V_glb
      REAL (KIND = prec_q) :: DELTA_V_apriori_glb(3)
      REAL (KIND = prec_q) :: DELTA_V_aposteriori_glb(3)
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: PULSES_Array_apriori_glb
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: PULSES_Array_aposteriori_glb
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: PULSES_Array_sat_glb
! ----------------------------------------------------------------------

END
