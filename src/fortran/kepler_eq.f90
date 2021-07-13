SUBROUTINE kepler_eq (M, ec, E)


! ----------------------------------------------------------------------
! SUBROUTINE: kepler_eq.f90
! ----------------------------------------------------------------------
! Purpose:
!  Kepler's Equation solution based on Newton method
! ----------------------------------------------------------------------
! Input arguments:
! - M:			Mean amomaly (degrees)
! - ec:			Eccentricity
!
! Output arguments:
! - E:      	Eccentric anomaly (degrees)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia           29 August 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: M, ec
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: E
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: Mrad, Eo, f_E
      REAL (KIND = prec_q) :: E_arr(8)
      INTEGER (KIND = prec_int8) :: Niter, i, sz1	  
! ----------------------------------------------------------------------



! converse in radians
Mrad = M * (PI_global /180.D0)

! ----------------------------------------------------------------------
! Initial approximate value of Eccentric anomaly (E)
! - small eccentricity:			Eo = M
! - high eccentricity (e>0.8):	Eo = pi
if (e < 0.8D0) then     
    if (Mrad == 0.D0) then 
        Eo = Mrad + 0.001D0
    else
        Eo = Mrad
    end if   
else    
    Eo = PI_global    
end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! number of iterations
Niter = 7
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Newton method: Iterative solution
Do i = 1 , Niter

    if (i == 1) then 
        E_arr(i) = Eo
    end if    
	
    ! auxiliary function
    f_E = E_arr(i) - ec * sin( E_arr(i) ) - Mrad
 
    E_arr(i+1) = E_arr(i) - f_E / ( 1.D0 - ec * cos( E_arr(i) ) )
	
End Do
! ----------------------------------------------------------------------


! Converse in degrees
E_arr = E_arr * (180.D0 / PI_global)


! Solution from the final iteration value
sz1 = size(E_arr, DIM = 1)

E = E_arr(sz1)



End


