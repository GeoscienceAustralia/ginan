SUBROUTINE eop_interp (n_interp, filename, EOP_sol, mjd , x_int,y_int,ut1_int)


! ----------------------------------------------------------------------
! Subroutine:	eop_interp.f90
! ----------------------------------------------------------------------
! Purpose:
!  Form arrays of EOP data which are to be used for EOP data interpolation
!  Corrections due to ocean tidal and libration effects
! ----------------------------------------------------------------------
! Input arguments:
! - n_interp:		number of points to be used for interpolation 
! - filename:		EOP data file name e.g. 'eopc04_IAU2000.62-now'
! - mjd:			Modified Julian Day number of the input epoch (in UTC)
!
! Output arguments:
! - x_int,y_int:	Polar motion coordinates (arcsec) at input epoch considering 
!					corrections due to ocean tidal and libration effects  	
! - ut1_int:		UT1-UTC difference (sec) at input epoch considering 
!					corrections due to ocean tidal and libration effects
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            December 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int4), INTENT(IN) :: n_interp
      INTEGER (KIND = prec_int1), INTENT(IN) :: EOP_sol
      CHARACTER (LEN=512), INTENT(IN) :: filename
      REAL (KIND = prec_d), INTENT(IN) :: mjd
! OUT
      DOUBLE PRECISION x_int, y_int, ut1_int
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: MJD_day, MJD_i, n_cent
      INTEGER (KIND = prec_int4) :: i
      REAL (KIND = prec_d) :: EOP_i(7)
      REAL (KIND = prec_d) :: MJDint_ar(n_interp), xint_ar(n_interp), yint_ar(n_interp), UT1int_ar(n_interp)
! ----------------------------------------------------------------------


      MJD_day = INT (mjd)


! ----------------------------------------------------------------------
! Epochs center definition
      IF (n_interp/2 - INT(n_interp/2) == 0) THEN
	     n_cent = n_interp/2
      ELSE
         n_cent = INT(n_interp/2) + 1
      END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      DO i = 1 , n_interp
         MJD_i = MJD_day - n_cent + i
         CALL eop_rd (filename, EOP_sol, MJD_i , EOP_i)
         MJDint_ar(i) = MJD_i
         xint_ar(i) = EOP_i(2)
         yint_ar(i) = EOP_i(3)
         UT1int_ar(i) = EOP_i(4)
      END DO
! ----------------------------------------------------------------------
 

! ----------------------------------------------------------------------
! interp.f: Ocean tidal and libration effects at interpolation epoch
      CALL INTERP_iers (MJDint_ar, xint_ar, yint_ar, UT1int_ar, n_interp, mjd, x_int,y_int,ut1_int)
! ----------------------------------------------------------------------


END
