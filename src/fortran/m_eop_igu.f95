MODULE m_eop_igu


! ----------------------------------------------------------------------
! MODULE: m_eop_igu.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the modified eop_igu subroutine 
! 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia 
! Created:	28 August 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
 
	  
Contains

SUBROUTINE eop_igu (mjd, ERP_fname, ERP_days, EOP_days, EOP_Nint, EOP_int)


! ----------------------------------------------------------------------
! Subroutine:  eop_igu.f90
! ----------------------------------------------------------------------
! Purpose:
!  EOP data reading and processing by using:
!  - ERP (Earth Rotation Parameters) data from the ultra-rapid products 
!    provided by the IGS (International GNSS Service).
!  - Corrections to the precession-nutation model are obtained from the 
!    daily solutions (finals2000A.daily) provided by the International 
!    Earth Rotation Service and Reference Systems (IERS) 
!    Rapid Service/Prediction Center (RS/PC) 
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number at the required epoch
!					(including fraction of the day)
! - ERP_fname:		IGS ultra-rapid ERP data file name e.g. igu18861_00.erp
! - EOP_days:		EOP data array of the days (data points aplied for interpolation) based on IERS RS/PC EOP data
! - EOP_Nint:           Number of points to use for interpolation
! - arc_length:         Number of hours of arc to read from file
!
! Output arguments:
! - eop_int:		EOP data array at the input epoch
!   				eop_int = [MJD xp yp UT1_UTC LOD dX dY] 
!   				MJD:     MJD at the input epoch (including fraction of the day)
!   				x,y:     Polar motion coordinates (arcsec) 
!   				UT1_UTC: Difference between UT1 and UTC (sec)
!					dX,dY:   Corrections to Precession-Nutation model (arcsec)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia               March 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
      CHARACTER (LEN=512), INTENT(IN) :: ERP_fname
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: ERP_days, EOP_days
      INTEGER, INTENT (IN) :: EOP_Nint
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: EOP_int(EOP_MAX_ARRAY)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: ERP_igu_data(EOP_Nint,EOP_MAX_ARRAY), ERP_int(EOP_MAX_ARRAY), mjd_t
      LOGICAL :: igu_flag
      INTEGER (KIND = prec_int8) :: mjd_UTC_day
      REAL (KIND = prec_d) :: mjd_ar(EOP_Nint), Xpole_ar(EOP_Nint), Ypole_ar(EOP_Nint), UT1UTC_ar(EOP_Nint), LOD_ar(EOP_Nint)
      REAL (KIND = prec_d) :: Xerr_ar(EOP_Nint), Yerr_ar(EOP_Nint), UT1err_ar(EOP_Nint), LODerr_ar(EOP_Nint)
      REAL (KIND = prec_d) :: mjd_int, Xpole_int, Ypole_int, UT1UTC_int, LOD_int
      REAL (KIND = prec_d) :: Xerr_int, Yerr_int, UT1err_int, LODerr_int
      REAL (KIND = prec_d) :: dX_eop, dY_eop
      INTEGER (KIND = prec_int4) :: i, j, sz1_EOP, sz2_EOP, EOP_nint_used, saved_row_count = 0
      INTEGER (KIND = prec_int4) :: lo, hi, offset
      INTEGER MAX_EOP_DATA
      parameter (MAX_EOP_DATA = 30)
      REAL (KIND = prec_d) :: ERP_read_data(MAX_EOP_DATA, EOP_MAX_ARRAY)

      igu_flag = .false.
      ERP_igu_data = 0.d0

! ----------------------------------------------------------------------
! check we don't have the required data already
      if (allocated(ERP_days)) then
          saved_row_count = SIZE(ERP_days, DIM=1)
          do i = 1, saved_row_count
              if (ERP_days(i,EOP_MJD) == 0.d0) then
                   saved_row_count = i-1
                   exit
              end if
              ERP_read_data(i,:) = ERP_days(i, :)
          end do
         DO i = 1, saved_row_count - 1
         if ((ERP_read_data(i, EOP_MJD) <= mjd) .and. (mjd <= ERP_read_data(i+1, EOP_MJD))) then
            igu_flag = .true.
            if (i > (EOP_Nint / 2) .and. ((i + 1) <= (saved_row_count - EOP_Nint / 2))) then
               if (MOD(EOP_Nint, 2) == 1) then
                   lo = i-EOP_Nint/2
                   hi = i+1+EOP_Nint/2
                   if (lo == 0) then
                       lo = lo + 1
                       hi = hi + 1
                   end if
               else
                   lo = i-EOP_Nint/2+1
                   hi = i+EOP_Nint/2
               end if
               ERP_igu_data = ERP_read_data(lo:hi, :)
               EOP_Nint_used = EOP_Nint
            else if (i <= EOP_nint/2) then
                ERP_igu_data = ERP_read_data(1:EOP_Nint,:)
                EOP_Nint_used = EOP_nint
            else if (i >= (saved_row_count - EOP_Nint/2)) then
                ERP_igu_data = ERP_read_data(saved_row_count - EOP_Nint + 1:saved_row_count, :)
                EOP_nint_used = EOP_Nint
            end if
            exit
         end if
         end do
         if (.not. igu_flag) then
             !print *, "mjd=", mjd, ", last row of data is mjd ", ERP_read_data(saved_row_count, EOP_MJD)
             if (mjd < ERP_read_data(saved_row_count, EOP_MJD) + 1.0d0) then
                 igu_flag = .true.
                 ERP_igu_data = ERP_read_data(saved_row_count - EOP_Nint + 1:saved_row_count, :)
                 EOP_Nint_used = EOP_Nint
             else if (mjd < ERP_read_data(1, EOP_MJD)) then
                 igu_flag = .true.
                 ERP_igu_data = ERP_read_data(1:EOP_nint,:)
                 EOP_Nint_used = EOP_Nint
             end if
         end if
     end if

! ----------------------------------------------------------------------
! ERP data reading 
      if (.not. igu_flag) then
          if (MOD(EOP_Nint, 2) == 1) then
              !EOP_Nint is odd, use Nint/2 points below if mjd is above 0.5, else Nint/2 + 1 points below
              mjd_t = mjd - int(mjd)
              if (mjd_t > 0.5) then
                  lo = int(mjd) - EOP_Nint/2 + 1
                  hi = INT(mjd) + EOP_Nint/2 + 1
              else
                  lo = int(mjd) - EOP_Nint/2
                  hi = int(mjd) + EOP_Nint/2
              end if
          else
              !EOP_nint is even use half above and half below precisely
              lo = int(mjd) - EOP_Nint/2 + 1
              hi = int(mjd) + EOP_Nint/2
          end if
          offset = 1
          EOP_nint_used = 0
          do i = hi, lo, -1
              mjd_t = float(i)
              CALL erp_igu (ERP_fname, mjd_t, ERP_int, igu_flag)
              if (igu_flag) then
                 erp_igu_data (i - lo + offset, :) = ERP_int
                 if (EOP_Nint_used == 0) then
                    EOP_Nint_used = i - lo + offset
                 end if
              else
                offset = offset + 1
              end if
          end do
      end if
      if (.not. igu_flag .and. offset > 2 ) then
         PRINT *,"--------------------------------------------------------"
         PRINT *, "Warning error: Subroutine erp_igu.f90"
         PRINT *, "Input epoch is out of the range covered by the IGS ultra-rapid ERP file" 
         PRINT *, "Check the input ", ERP_fname
         PRINT *,"--------------------------------------------------------"
         STOP  ! END PROGRAM
      end if		 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! ERP interpolation
      ERP_int = 0.d0

      mjd_int = mjd
      mjd_ar    = ERP_igu_data(1:EOP_Nint_used,EOP_MJD)
      Xpole_ar  = ERP_igu_data(1:EOP_Nint_used,EOP_X)
      Ypole_ar  = ERP_igu_data(1:EOP_Nint_used,EOP_Y)
      UT1UTC_ar = ERP_igu_data(1:EOP_Nint_used,EOP_UT1)
      LOD_ar    = ERP_igu_data(1:EOP_Nint_used,EOP_LOD)
      Xerr_ar = ERP_igu_data(1:EOP_Nint_used,EOP_X_ERR)
      Yerr_ar = ERP_igu_data(1:EOP_Nint_used,EOP_Y_ERR)
      UT1err_ar = ERP_igu_data(1:EOP_Nint_used,EOP_UT1_ERR)
      LODerr_ar = ERP_igu_data(1:EOP_Nint_used,EOP_LOD_ERR)
      ! do we need to correct, or should that have already been applied?
      call interp_iers(mjd_ar, Xpole_ar, Ypole_ar, UT1UTC_ar, EOP_Nint_used, EOP_Nint_used, &
              mjd_int, Xpole_int, Ypole_int, UT1UTC_int)
      !CALL LAGINT(mjd_ar, Xpole_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, Xpole_int)
      !CALL LAGINT(mjd_ar, Ypole_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, Ypole_int)
      !CALL LAGINT(mjd_ar, UT1UTC_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, UT1UTC_int)
      CALL LAGINT(mjd_ar, LOD_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, LOD_int)
      CALL LAGINT(mjd_ar, Xerr_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, Xerr_int)
      CALL LAGINT(mjd_ar, Yerr_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, Yerr_int)
      CALL LAGINT(mjd_ar, UT1err_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, UT1err_int)
      CALL LAGINT(mjd_ar, LODerr_ar, EOP_Nint_used, EOP_Nint_used, mjd_int, LODerr_int)

      ERP_int (EOP_MJD) = mjd_int
      ERP_int (EOP_X) = Xpole_int
      ERP_int (EOP_Y) = Ypole_int
      ERP_int (EOP_UT1) = UT1UTC_int
      ERP_int (EOP_LOD) = LOD_int
      ERP_int (EOP_X_ERR) = Xerr_int
      ERP_int (EOP_Y_ERR) = Yerr_int
      ERP_int (EOP_UT1_ERR) = ut1err_int
      ERP_int (EOP_LOD_ERR) = LODerr_int
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! dX,dY : Corrections w.r.t Precession-Nutation model
      !mjd_UTC_day = INT (mjd)
      !CALL eop_finals2000A (EOP_fname, mjd_UTC_day , EOP_data)
      !dX = EOP_data(6) 
      !dY = EOP_data(7) 
      !----------------------------------------------------------
      ! init next two var to something sensible
dX_eop = 0.d0
dY_eop = 0.d0
sz1_EOP = SIZE (EOP_days,DIM=1)
sz2_EOP = SIZE (EOP_days,DIM=2)
DO i = 1 , sz1_EOP  
If (mjd_int == EOP_days(i,EOP_MJD) ) then
! dX,dY (arcsec)													
      dX_eop = EOP_days(i,EOP_DX)
      dY_eop = EOP_days(i,EOP_DY)
End If
END DO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      EOP_int (EOP_MJD) = mjd_int
      EOP_int (EOP_X) = Xpole_int
      EOP_int (EOP_Y) = Ypole_int
      EOP_int (EOP_UT1) = UT1UTC_int
      EOP_int (EOP_LOD) = LOD_int
	  ! dX,dY : Precession-Nutation model corrections
      EOP_int (EOP_DX) = dX_eop !EOP_data (6) 
      EOP_int (EOP_DY) = dY_eop !EOP_data (7) 
! ----------------------------------------------------------------------
      EOP_int (EOP_X_ERR) = Xerr_int
      EOP_int (EOP_Y_ERR) = Yerr_int
      EOP_int (EOP_UT1_ERR) = ut1err_int
      EOP_int (EOP_LOD_ERR) = LODerr_int


!      PRINT *,"--------------------------------------------------------"
!      PRINT *, "ERP_igu_data"
!      PRINT *, ERP_igu_data
!      PRINT *, "ERP_int"
!      PRINT *, ERP_int
!      PRINT *,"--------------------------------------------------------"


	  
END SUBROUTINE


END
