SUBROUTINE tide_pole_oc (mjd, xp, yp , dC21,dS21)

! ----------------------------------------------------------------------
! SUBROUTINE: tide_pole_oc.f90
! ----------------------------------------------------------------------
! Purpose:
!  Ocean pole tide effect to C21 and S21 spherical harmonic coefficients
!  of the geopotential harmonics expansion
! ----------------------------------------------------------------------
! Input arguments:
! - mjd   :             Modified Julian Day number in Terrestrial Time (TT) 
!                             including the fraction of the day 
! Output arguments:
! - dC21  :             C21 coefficient correction due to Ocean Pole Tide
! - dS21  :             S21 coefficient correction due to Ocean Pole Tide
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  November 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 30 October 2017:
!   The subroutine has been slightly modified in order to call the subroutine iau_JD2CAL.for from the IAU SOFA package 
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: mjd, xp, yp
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: dC21,dS21
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: m1,m2
! ----------------------------------------------------------------------
! Remark:
!  Variables "epoch, xp_mean, yp_mean" enter F77 code and thus, these are declared as DP.
      REAL (KIND = prec_d) :: epoch, xp_mean,yp_mean
      !REAL*8 epoch, xp_mean,yp_mean
      INTEGER error,version
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: jd0
      INTEGER IY, IM, ID, J_flag
      DOUBLE PRECISION FD  
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
jd0 = 2400000.5D0
CALL iau_JD2CAL ( jd0, mjd, IY, IM, ID, FD, J_flag )    

epoch = IY*1.D0 + IM/12.D0 + ID/365.25D0 + FD/365.25D0                                                                                    
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Conventional Mean Pole

!      epoch_int = INT (epoch)
!      IF (epoch_int >= 2010) THEN
!         version = 2015
!      ELSE IF (epoch_int < 2010 .AND. epoch_int >= 2003) THEN
!          version = 2010
!      ELSE IF (epoch_int < 2003 .AND. epoch_int >= 2010)          
  
      version = 2015
      CALL IERS_CMP_2015   (version,epoch , xp_mean,yp_mean,error)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Wobble variables m1, m2 (in seconds of arc)
      m1 = xp - xp_mean
      m2 = -1.0D0 * (yp - yp_mean)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      dC21 = -2.1778D-10 * (m1 - 0.01724D0 * m2)
      dS21 = -1.7232D-10 * (m2 - 0.03365D0 * m1)
! ----------------------------------------------------------------------


END
