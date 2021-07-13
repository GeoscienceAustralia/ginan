      program main_crs2trs


! ----------------------------------------------------------------------
! Program:	main_crs2trs
! ----------------------------------------------------------------------
! Purpose:  
!  Computation of the transformation matrix between inertial and terrestrial 
!  (ICRF-ITRF) reference frames. 
!  The transformation matrix (direct/inverse) is provided for position/velocity vectors
!  The Earth Orientation Matrix is computed based on the EOP data and 
!  the consideration of the EOP corrections due to tidal variations
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, CRC-SI
! Created:	7 August 2018
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_eop
      USE m_eop_data
      USE m_eop_cor
      USE m_eop_igu
      USE mdl_param
      USE m_writearray
      use pod_yaml
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: filename, EQMfname, VEQfname				
! ----------------------------------------------------------------------
      DOUBLE PRECISION arcsec2rad
      INTEGER (KIND = prec_int4) :: time_in
      INTEGER IY, IM, ID, J_flag
      DOUBLE PRECISION DJM0, sec, FD
      REAL (KIND = prec_d) :: mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1, mjd_0
      CHARACTER (LEN=512) :: EOP_fname
      CHARACTER (LEN=512) :: ERP_fname
      REAL (KIND = prec_d) :: mjd_igu
      INTEGER (KIND = prec_int4) :: n_interp
      INTEGER (KIND = prec_int8) :: mjd_UTC_day
      INTEGER (KIND = prec_int1) :: EOP_sol
      INTEGER (KIND = prec_int2) :: iau_model
      DOUBLE PRECISION EOP_cr(7), EOP_cr2(7)
! ----------------------------------------------------------------------
      DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: R_Matrix  
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: EOP_days	  


Print *,"ICRF-ITRF transformation matrix and EOP Data processing"


! ----------------------------------------------------------------------
! INPUT:

! ----------------------------------------------------------------------
! Time System of input epoch:
! 1. TT
! 2. GPS time
! 3. UTC
! 4. TAI
time_in = UTC_time
! ----------------------------------------------------------------------
! Date
IY = 2011
IM = 6
ID = 15
sec = 0.0D0  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! EOP data
! ----------------------------------------------------------------------
! Select EOP solution
! 1. IERS C04
! 2. IERS RS/PC Daily (finals2000A.daily)
! 3. IGS ultra-rapid ERP + IERS RS/PC Daily (dX,dY)
EOP_sol = 1
n_interp = 4   	! number of points for EOP interpolation
! ----------------------------------------------------------------------
! IERS C04
if (EOP_sol == 1) then 
	EOP_fname = 'eopc04_08_IAU2000.62-now'
	  
! IERS RS/PC Daily
else if (EOP_sol == 2)  then
	EOP_fname = 'finals2000A.daily'
	  
! IGS ultra-rapid ERP
else if (EOP_sol == 3) then
	ERP_fname = 'igu18543_12.erp'
	EOP_fname = 'finals2000A.daily'
end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! IAU Precession-Nutation model:
! 2. IAU2000A:			iau_model = 2000
! 3. IAU2006/2000A:		iau_model = 2006
iau_model = IAU2000
! ----------------------------------------------------------------------

! End of Input
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! MJD 
CALL iau_CAL2JD ( IY, IM, ID, DJM0, mjd, J_flag )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Time Systems transformation											 
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
! Case 1. EOP by IERS data: EOP reading, interpolation and corrections
! ----------------------------------------------------------------------
! - IERS Earth Orientation Center:			C04 solution
! - IERS Rapid Service/Prediction Center:	finals2000A.daily solution
! ----------------------------------------------------------------------
IF (EOP_sol == 1 .OR. EOP_sol == 2) THEN  

! ----------------------------------------------------------------------
! EOP data reading
CALL eop_data (mjd_TT, EOP_fname, EOP_sol, n_interp , EOP_days)
!print *,"EOP_days", EOP_days
! ----------------------------------------------------------------------
! EOP data corrections due to tidal variations
CALL eop_cor (mjd_TT, EOP_days , EOP_sol, n_interp, EOP_cr)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Case 2. ERP by IGS ultra-rapid data: EOP reading and interpolation
! ----------------------------------------------------------------------
ELSEIF (EOP_sol == 3)  THEN 
! EOP ultra-rapid: ERP by IGS and dX,dY (IAU 2000A) by IERS RS/PC (finals2000A.daily)
	CALL eop_igu (mjd_TT, ERP_fname, EOP_days, EOP_cr)
END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! ICRF-ITRF transformation matrix (including derivatives)
! ----------------------------------------------------------------------
! CRS-TRS transformation matrix (r,v)
! - CRS2TRS:		GCRS to ITRS transformation matrix (position vector)
! - TRS2CRS:		ITRS to GCRS transformation matrix (position vector)
! - d_CRS2TRS:		Derivative of GCRS to ITRS transformation matrix (velocity vector)
!					v_TRS = TRS2CRS * v_CRS + d_CRS2TRS * r_CRS
! - d_TRS2CRS:		ITRS to GCRS transformation matrix (velocity vector)
!					v_CRS = TRS2CRS * v_TRS + d_TRS2CRS * r_TRS
! ----------------------------------------------------------------------
Print *,"ICRF-ITRF transformation matrix"
CALL crs_trs (mjd_TT, EOP_cr, iau_model, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)
! ----------------------------------------------------------------------
Print *,"CRS2TRS", CRS2TRS
Print *,"TRS2CRS", TRS2CRS
Print *,"d_CRS2TRS", d_CRS2TRS 
Print *,"d_TRS2CRS", d_TRS2CRS


! ----------------------------------------------------------------------
! Write matrices to ouput files (ascii)
PRINT *,"Write ICRF-ITRF transformation matrices to output files: CRS2TRS.out, TRS2CRS.out, d_CRS2TRS.out, d_TRS2CRS.out"
! ----------------------------------------------------------------------
! Allocatable arrays
ALLOCATE (R_Matrix(3,3), STAT = AllocateStatus)

filename = "CRS2TRS.out"
R_Matrix = CRS2TRS
Call writearray (R_Matrix, filename)

filename = "TRS2CRS.out"
R_Matrix = TRS2CRS
Call writearray (R_Matrix, filename)

filename = "d_CRS2TRS.out"
R_Matrix = d_CRS2TRS
Call writearray (R_Matrix, filename)

filename = "d_TRS2CRS.out"
R_Matrix = d_TRS2CRS
Call writearray (R_Matrix, filename)
! ----------------------------------------------------------------------


End Program
