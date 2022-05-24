MODULE m_eop_data


! ----------------------------------------------------------------------
! MODULE: m_eop_data.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for callnig the eop_data subroutine 
! 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia 
! Created:	23 July 2018
! ----------------------------------------------------------------------

      use pod_yaml
      IMPLICIT NONE
      !SAVE 			
 
Contains


SUBROUTINE eop_data (mjd, EOP_fname, EOP_sol, orbit_arc_length, n_interp , EOP_days)


! ----------------------------------------------------------------------
! Subroutine:  eop_data.f90
! ----------------------------------------------------------------------
! Purpose:
!  IERS Earth Orientation Parameters (EOP) data reading and processing.
!  Corrections computing due to tidal variations
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number of the epoch (in TT)
! - EOP_fname:		EOP data file name  e.g. 'eopc04_IAU2000.62-now'
! - EOP_sol:		EOP solution type (see Note 1)
! - orbit_arc_length    arc_length to be integrated over (in hours)
! - n_interp:		number of points to be used for EOP data interpolation 
!
! Output arguments:
! - EOP_cr:		Array of the EOP data after applying the tidal corrections
!   EOP_cr = [MJD xp_cor yp_cor UT1-UTC_cor LOD_cor dX dY]  1x6 matrix
!   MJD:     		MJD of the input epoch (in TT)
!   xp_cor,yp_cor:	Polar motion coordinates (arcsec) at input epoch considering 
!					corrections due to tidal variations (ocean tidal and libration effects)  	
!   UT1UTC_cor:		UT1-UTC difference (sec) at input epoch considering corrections
!					due to tidal variations	(ocean tidal and libration effects)  
!   LOD:			Length of Day (LOD)
!   dX, dY:			Corrections to the Precession-Nutation model (arcsec)
! ----------------------------------------------------------------------
! Note 1:
! Values of the EOP_sol input argument:
! 1. EOP_sol = 1 : refers to EOP data format of the C04 solution
!    			   provided by the IERS Earth Orientation Center 
! 2. EOP_sol = 2 : refers to EOP data format of the daily solution (finals2000A.daily)
!    			   provided by the IERS Rapid Service/Prediction Center
! 3. EOP_sol = 3 : refers to EOP data format of the ultra-rapid products
!    			   provided by the IGS (per every 6 hours)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia 
! Created:	23 July 2018
! ----------------------------------------------------------------------
!
! Changes: 21-07-2021 Tzupang Tseng: read ERP values from IC file
!
! ---------------------------------------------------------------------

      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE pod_yaml
      USE mdl_eop, ONLY: ERP_day_glb,ERP_day_IC
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
      CHARACTER (LEN=512), INTENT(IN) :: EOP_fname
      INTEGER (KIND = prec_int4), INTENT(IN) :: orbit_arc_length, n_interp
      INTEGER (KIND = prec_int1), INTENT(IN) :: EOP_sol
! OUT
      !REAL (KIND = prec_d), INTENT(OUT) :: EOP_data(n_interp,7)
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: EOP_days  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
      DOUBLE PRECISION TT1, TT2, TT1_UT1, TT2_UT1, TAI1, TAI2
      INTEGER (KIND = prec_int8) :: mjd_UTC_day, mjd_day_int
      REAL (KIND = prec_d) :: EOP_day(EOP_MAX_ARRAY)
      DOUBLE PRECISION xp, yp, xp_int, yp_int, xp_cor, yp_cor
      DOUBLE PRECISION dUT1_UTC, ut1utc_int, UT1UTC_cor
      DOUBLE PRECISION dX_eop, dY_eop, LOD 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: MJD_i, n_cent, half_interp, orbit_days
      INTEGER (KIND = prec_int4) :: i, j, k, next, rowcount, sz1, sz2
      REAL (KIND = prec_d) :: EOP_i(EOP_MAX_ARRAY)
      REAL (KIND = prec_d), DIMENSION (:,:), ALLOCATABLE :: EOP_days_data
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
      LOGICAL found, updated

! ----------------------------------------------------------------------
! Dynamic allocatable arrays
half_interp = n_interp/2
orbit_days = orbit_arc_length / 24
if (MOD(n_interp, 2) == 1) half_interp = half_interp+1
if (orbit_arc_length - (24 * orbit_days) > 0) orbit_days = orbit_days + 1
ALLOCATE (EOP_days_data(n_interp + orbit_days, EOP_MAX_ARRAY), STAT = AllocateStatus)

if (AllocateStatus /= 0) then
        print *,'ERROR: eop_data - allocating EOP_days n_interp:',half_interp * 2 + orbit_days
        stop
end if

updated = .false.
! ----------------------------------------------------------------------
! Mod - Initialise the EOP interpolation data array (SCM 04022020)
EOP_days_data = 0.0d0

! ----------------------------------------------------------------------
! Time Systems transformation											 
! ----------------------------------------------------------------------
      CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
! ----------------------------------------------------------------------
! TT
      TT1 = 2400000.5D0
      TT2 = mjd_TT
! ----------------------------------------------------------------------
! TAI
      TAI1 = 2400000.5D0
      TAI2 = mjd_TAI
! ----------------------------------------------------------------------
! UTC
      mjd_UTC_day = INT (mjd_UTC)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! EOP data reading
! ----------------------------------------------------------------------
!      CALL eop_rd (EOP_fname, EOP_sol, mjd_UTC_day , EOP_day)
! xp,yp (arcsec)
!      xp = EOP_day(2)
!      yp = EOP_day(3)
! UT1-UTC (sec)
!      dUT1_UTC = EOP_day(4)
! LOD (sec)
!	  LOD = EOP_day(5)
! dX,dY (arcsec)													
!      dX_eop = EOP_day(6)
!      dY_eop = EOP_day(7)
! ----------------------------------------------------------------------

mjd_day_int = INT(mjd)
!mjd_day_int = mjd_UTC_day
sz1 = SIZE(ERP_day_glb, DIM = 1)

do i = 1, sz1
    if (ERP_day_glb(i, EOP_MJD) /= 0.d0) then
        eop_days_data(i, :) = ERP_day_glb(i, :)
    else
        exit
    end if
end do

! ----------------------------------------------------------------------
! EOP data reading
! ----------------------------------------------------------------------
DO i = 1 , n_interp + orbit_days
   MJD_i = mjd_day_int + i - half_interp
   found = .false.
   do j = 1, sz1
       if (j .le. SIZE (EOP_days_data, DIM = 1)) then
       if (EOP_days_data(j, EOP_MJD) == MJD_i) then
           found = .true.
           exit
       end if
       end if
   end do
   if (.not. found) then
       CALL eop_rd (EOP_fname, EOP_sol, MJD_i , EOP_i)

! LOD (sec)
   LOD = EOP_i(EOP_LOD)
! dX,dY (arcsec)													
   dX_eop = EOP_i(EOP_DX)
   dY_eop = EOP_i(EOP_DY)

   next = 1
   do j = 1, SIZE(EOP_days_data, DIM=1)
       if (EOP_days_data(j, EOP_MJD) == 0.0) then
               next = j
               exit
       end if
   end do
   if (mjd_i == EOP_i (EOP_MJD)) then
       do k = 1, next - 1
       if (EOP_days_data(k, EOP_MJD) > mjd_i) then
           do j = next - k, 1, -1
               EOP_days_data(j+1, :) = EOP_days_data(j, :)
           end do
           next = k
           exit
       end if
       end do
        EOP_days_data(next,EOP_MJD) = MJD_i
        EOP_days_data(next,EOP_X) = EOP_i(EOP_X)
        EOP_days_data(next,EOP_Y) = EOP_i(EOP_Y)
        EOP_days_data(next,EOP_UT1) = EOP_i(EOP_UT1)
        EOP_days_data(next,EOP_LOD) = EOP_i(EOP_LOD)
        EOP_days_data(next,EOP_DX) = EOP_i(EOP_DX)
        EOP_days_data(next,EOP_DY) = EOP_i(EOP_DY)
        EOP_days_data(next,EOP_X_ERR) = EOP_i(EOP_X_ERR)
        EOP_days_data(next,EOP_Y_ERR) = EOP_i(EOP_Y_ERR)
        EOP_days_data(next,EOP_UT1_ERR) = EOP_i(EOP_UT1_ERR)
        EOP_days_data(next,EOP_LOD_ERR) = EOP_i(EOP_LOD_ERR)
        updated = .true.
   end if

   end if

END DO
! ----------------------------------------------------------------------

sz1 = SIZE(ERP_day_IC, DIM=1)
sz2 = SIZE(EOP_days_data, DIM=1)
!Only an IC file has ERP data. Pod_data section of yaml does not
IF (yml_pod_mode == MODE_IC_INT) THEN
        IF(ERP_day_IC(1,1) /= 0.d0 ) THEN
                DO i = 1, sz2
                    if (EOP_days_data(i, EOP_MJD) == ERP_day_IC(1, EOP_MJD)) then
                        sz2 = i
                        exit
                    end if
                end do
                if (sz2 < SIZE(EOP_days_data, DIM=1)) then
                updated = .true.
                DO i = 1 , sz1
                EOP_days_data(sz2 + i - 1,EOP_MJD) = ERP_day_IC(i,EOP_MJD)
                EOP_days_data(sz2 + i - 1,EOP_X) = ERP_day_IC(i,EOP_X)
                EOP_days_data(sz2 + i - 1,EOP_Y) = ERP_day_IC(i,EOP_Y)
                EOP_days_data(sz2 + i - 1,EOP_UT1) = ERP_day_IC(i,EOP_UT1)
                if (sz2 + i > SIZE(EOP_days_data, DIM=1)) exit
                !print*,'ERP_day_IC =', ERP_day_IC(i,:)
                END DO
                end if
        END IF
END IF

! now go through the array and remove rows where mjd is 0
sz1 = SIZE (EOP_days_data, DIM=1)
rowcount = 0
do i = 1, sz1
    if (EOP_days_data(i, EOP_MJD) /= 0.d0) then
        rowcount = rowcount + 1
    end if 
end do
ALLOCATE (EOP_days(rowcount, EOP_MAX_ARRAY), STAT = AllocateStatus)

if (AllocateStatus /= 0) then
        print *,'ERROR: eop_data - allocating EOP_days n_interp:',half_interp * 2 + orbit_days
        stop
end if

j = 0
do i = 1, sz1
    if (EOP_days_data(i, EOP_MJD) /= 0.d0) then
         j = j+1
         EOP_days(j,:) = EOP_days_data(i,:)
    end if
end do
if (updated) then
ERP_day_glb(1:rowcount,:) = EOP_days
end if

if (.false.) then
do i = 1 , rowcount
print*,'ERP_day_glb(', i, ') =', ERP_day_glb(i,1:4)
end do
end if

END SUBROUTINE


END
