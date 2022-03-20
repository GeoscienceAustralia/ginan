SUBROUTINE integr_rk4(mjdo, yo, step, mjdn, yn)


! ----------------------------------------------------------------------
! SUBROUTINE: integr_rk4.f03
! ----------------------------------------------------------------------
! Purpose:
!  Numerical integration method: Runge-Kutta 4th order by Runge and Kutta
!  RK4 method is implemented here for satellite orbit integration
! ----------------------------------------------------------------------
! Input arguments:
! - MJDo: 		Initial epoch's (to) Modified Julian Day (MJD) number (including the fraction of the day)
! - ro:			Position vector at initial epoch (m)
! - vo:			Velocity vector at initial epoch (m/sec)
! - step:		Numerical Integration stepsize (sec)
!
! Output arguments
! - MJD: 		Next epoch's (to+h) Modified Julian Day (MJD) number (including the fraction of the day)
! - r:			Position vector at next epoch (m)
! - v:			Velocity vector at next epoch (m/sec)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	19 October 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(6) :: yo
      REAL (KIND = prec_d), INTENT(IN) :: mjdo
      REAL (KIND = prec_d), INTENT(IN) :: step
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(6) :: yn
      REAL (KIND = prec_d), INTENT(OUT) :: mjdn
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: h, to, mjd_to
      REAL (KIND = prec_d), DIMENSION(3) :: ro , vo	  
      REAL (KIND = prec_d) :: ti, mjd_t
      REAL (KIND = prec_d), DIMENSION(3) :: ri, vi
      REAL (KIND = prec_d), DIMENSION(6) :: yi
      REAL (KIND = prec_d) :: fx, fy, fz
      REAL (KIND = prec_d), DIMENSION(6) :: k1, k2, k3, k4, FRK4
!       , y
     INTEGER (KIND = prec_int4) :: i_stage
! ----------------------------------------------------------------------	  



! ----------------------------------------------------------------------	  
! Integration step
h = step
! ----------------------------------------------------------------------	  


! ----------------------------------------------------------------------	  
! Initial Conditions
! ----------------------------------------------------------------------	  
! Initial epoch
! MJD
mjd_to = mjdo
! Seconds at MJDo since 00h
to = (mjd_to - INT(mjd_to)) * (24.0D0 * 3600.D0)

! Initial State vector at epoch to
ro = yo(1:3)
vo = yo(4:6)
! ----------------------------------------------------------------------	  


! ----------------------------------------------------------------------	  
! Computation of integration coefficients (k)
! ----------------------------------------------------------------------	  

! ----------------------------------------------------------------------	  
! k1 = f(to,yo)
! ----------------------------------------------------------------------	  
ri = ro
vi = vo

! MJD (in days) including the fraction of the day
mjd_t = mjd_to

! Force model acceleration component
i_stage=0
Call force_sum(mjd_to, to, ri, vi, fx, fy, fz, i_stage)

k1(1:3) = vi
k1(4:6) = (/ fx, fy, fz /)
! ----------------------------------------------------------------------	  

! ----------------------------------------------------------------------	  
! k2 = f(to+h/2,yo+h*k1/2)
! ----------------------------------------------------------------------	  
ti = to + h/2.0D0
yi = yo + (h/2.0D0) * k1

ri = yi(1:3)
vi = yi(4:6)

! MJD (in days) at ti including the fraction of the day
mjd_t = INT(mjd_to) + ti / (24.0D0 * 3600.D0)

! Force model acceleration components
i_stage = 2-1
Call force_sum(mjd_t, ti, ri, vi, fx, fy, fz, i_stage)

k2(1:3) = vi
k2(4:6) = (/ fx, fy, fz /)
! ----------------------------------------------------------------------	  

! ----------------------------------------------------------------------	  
! k3 = f(to+h/2,yo+h*k2/2)
! ----------------------------------------------------------------------	  
ti = to + h/2.0D0
yi = yo + (h/2.0D0) * k2

ri = yi(1:3)
vi = yi(4:6)

! MJD (in days) at ti including the fraction of the day
mjd_t = INT(mjd_to) + ti / (24.0D0 * 3600.D0)

! Force model acceleration components
i_stage = 3-1
Call force_sum(mjd_t, ti, ri, vi, fx, fy, fz, i_stage)

k3(1:3) = vi
k3(4:6) = (/ fx, fy, fz /)
! ----------------------------------------------------------------------	

! ----------------------------------------------------------------------	  
! k4 = f(to+h,yo+h*k3)
! ----------------------------------------------------------------------	  
ti = to + h 
yi = yo + h * k3

ri = yi(1:3)
vi = yi(4:6)

! MJD (in days) at ti including the fraction of the day
mjd_t = INT(mjd_to) + ti / (24.0D0 * 3600.D0)

! Force model acceleration components
i_stage = 4-1
Call force_sum(mjd_t, ti, ri, vi, fx, fy, fz, i_stage)

k4(1:3) = vi
k4(4:6) = (/ fx, fy, fz /)
! ----------------------------------------------------------------------	

! ----------------------------------------------------------------------	


! ----------------------------------------------------------------------	
! Increment Function
FRK4 = (1.0D0/6.0D0) * (k1 + 2*k2 + 2*k3 + k4)
! ----------------------------------------------------------------------	


! ----------------------------------------------------------------------	
! Solution of the numerical integration
! ----------------------------------------------------------------------	
! State vector at the next epoch
yn = yo + h * FRK4
! ----------------------------------------------------------------------	

! Next Epoch
! MJD (in days) including the fraction of the day
mjdn = mjdo + step / (24.0D0 * 3600.D0) 



END SUBROUTINE
