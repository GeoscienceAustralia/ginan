MODULE m_veq_rkn768


! ----------------------------------------------------------------------
! MODULE: m_veq_rkn768.f95
! ----------------------------------------------------------------------
! Purpose:
!  Module for numerical integration of the Variational Equations 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou 
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	29 January 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
	  


Contains


SUBROUTINE veq_rkn768(zo, veqZo, veqPo, step, lamda_h, Np, z_q, e_r, veqZ, veqP)


! ----------------------------------------------------------------------
! SUBROUTINE: veq_rkn768.f95
! ----------------------------------------------------------------------
! Purpose:
!  Variational Equations solution based on RKN7(6)-8 numerical integration method by Dormand & Prince(1978)
! ----------------------------------------------------------------------
! Input arguments:
! - zo:  		Initial epoch (to) and state vector in Celestial Reference System GCRS
!        		zo = [MJDo to ro vo]  
!   			MJDo: initial epoch's Modified Julian Day (MJD) number (including the fraction of the day)
!   			ro:   Position vector at initial epoch in GCRS (m)
!   			vo:   Velocity vector at initial epoch in GCRS (m/sec)
! - veqZo:		State Transition matrix at initial epoch (6x6 matrix)
! - veqPo:		Sensitivity matrix at initial epoch (6xNp matrix)
! - step:		Integration stepsize (sec)
! - lamda_h:	RKN7(6)-8 specific parameter for stepsize control
! - Np:			Number of parameters to be estimated through the sensitivity matrix
!
! Output arguments
! - z_q:		State vector at the next epoch t=to+h in GCRS
! - e_r:   		Local truncation error of position vector
! - veqZ:		State transition matrix at next epoch (6x6 matrix)
! - veqP:		Sensitivity matrix at next epoch (6xNp matrix)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	29 January 2018
!
! Changes:  18-03-2019 Simon McClusky: Initialise the three matrixes of sumkP, sum_agP and sum_ag with zero values
!                                      to make the system stable in estimating the SRP parameters
!
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE m_matrixRxR
      USE m_pd_force
      USE mdl_config
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(8) :: zo
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(6,6) :: veqZo
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: veqPo
      REAL (KIND = prec_d), INTENT(IN) :: step
      REAL (KIND = prec_d), INTENT(IN) :: lamda_h
	  INTEGER (KIND = prec_int8), INTENT(IN) :: Np
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(6) :: z_q
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(3) :: e_r
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(6,6) :: veqZ  
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: veqP 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: h, to, mjd_t, mjd_to, t_sec
      INTEGER (KIND = prec_int8) :: Np2, N0
      REAL (KIND = prec_d) :: cf
      REAL (KIND = prec_d), DIMENSION(9) :: t
      REAL (KIND = prec_d), DIMENSION(3) :: ro, vo
      REAL (KIND = prec_d), DIMENSION(3) :: ri1, vi1
      REAL (KIND = prec_d), DIMENSION(3) :: r_q, v_q, r_p, er, ev
      REAL (KIND = prec_d), DIMENSION(3,9) :: r, k
      REAL (KIND = prec_d), DIMENSION(3) :: sum_ak 
      REAL (KIND = prec_d), DIMENSION(9) :: c, b, b1, b2 
      REAL (KIND = prec_d), DIMENSION(9,8) :: a 
      REAL (KIND = prec_d), DIMENSION(8,9) :: a_transp 
      INTEGER (KIND = prec_int4) :: s, i, j
      REAL (KIND = prec_d) :: fx, fy, fz
	  REAL (KIND = prec_d), DIMENSION(3) :: sum_bk, sum_b1k, sum_b2k
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus	  
      INTEGER (KIND = prec_int4) :: i1, j1
      REAL (KIND = prec_d), DIMENSION(3,6) :: veqZ_ro, veqZ_vo, veqZ_r, veqZ_v
      REAL (KIND = prec_d) :: Fi(3), pd_Fr(3,3), pd_Fv(3,3)
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: pd_Fp 
      REAL (KIND = prec_d) :: k_Z(3,6,9), kZ_i(3,6), G_kZ(3,6)
      REAL (KIND = prec_d), DIMENSION(3,6) :: sum_ag, sum_bq_r_g, sum_bq_v_g, sumkZ 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: veqP_ro, veqP_vo, veqP_r, veqP_v
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE :: k_P 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: kP_i, G_kP, sum_agP, sum_bq_r_gP, sum_bq_v_gP, sumkP 
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: U33, U36, Z36
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: U3n, P3n		
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      CHARACTER (len=100) :: mesg
! ----------------------------------------------------------------------
! initialise output var
e_r = 0.d0

! Integration step
h = step

! ----------------------------------------------------------------------	  
! Initial Conditions
! ----------------------------------------------------------------------	  
! Initial epoch
! MJD
mjd_to = zo(1)
! Seconds at MJDo since 00h
to = zo(2) !to = (mjd_to - INT(mjd_to)) * (24.D0 * 3600.D0)

! Initial position and velocity vectors
ro = zo(3:5)
vo = zo(6:8)

! State Transition Matrix Initial values
veqZ_ro(1,:) = veqZo(1,:)
veqZ_ro(2,:) = veqZo(2,:)
veqZ_ro(3,:) = veqZo(3,:)
veqZ_vo(1,:) = veqZo(4,:)
veqZ_vo(2,:) = veqZo(5,:)
veqZ_vo(3,:) = veqZo(6,:)

! Sensitivity Matrix Initial values: veqPo (6xNp)
sz1 = size(veqPo, DIM = 1)
sz2 = size(veqPo, DIM = 2)
!print *,"sz1, sz2", sz1,sz2

ALLOCATE (veqP(6,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated veqP, dimensions (6,", &
                sz2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (veqP_ro(3,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated veqP_ro, dimensions (3,", &
                sz2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (veqP_vo(3,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated veqP_vo, dimensions (3,", &
                sz2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (veqP_r (3,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated veqP_r, dimensions (3,", &
                sz2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (veqP_v (3,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated veqP_v, dimensions (3,", &
                sz2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if

veqP_ro(1,:) = veqPo(1,:)
veqP_ro(2,:) = veqPo(2,:)
veqP_ro(3,:) = veqPo(3,:)
veqP_vo(1,:) = veqPo(4,:)
veqP_vo(2,:) = veqPo(5,:)
veqP_vo(3,:) = veqPo(6,:)
! ----------------------------------------------------------------------
! initialise
veqP = 0.d0
veqP_r = 0.d0
veqP_v = 0.d0


! ----------------------------------------------------------------------	  
Np2 = Np
If (Np == 0) Then
Np2 = 1
End IF

! Allocatable arrays
If (Np2 /= 0) Then

ALLOCATE (kP_i(3,Np2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated kP_i, dimensions (3,", &
                Np2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (k_P(3,Np2,9), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated k_P, dimensions (3,", &
                Np2, ",9)"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (G_kP(3,Np2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated G_kP, dimensions (3,", &
                Np2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (sum_agP(3,Np2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated sum_agP, dimensions (3,", &
                Np2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (sum_bq_r_gP(3,Np2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated sum_bq_r_gP, dimensions (3,", &
                Np2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (sum_bq_v_gP(3,Np2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated sum_bq_v_gP, dimensions (3,", &
                Np2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if
ALLOCATE (sumkP(3,Np2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory -failed to allocated sumkP, dimensions (3,", &
                Np2, ")"
        call report ('FATAL', pgrm_name, 'veq_rkn768', mesg, 'src/fortran/m_veq_rkn768.f95', 1)
end if

sumkP   = 0.d0
sum_agP = 0.d0
sum_ag  = 0.d0

kP_i = 0.d0
G_kP = 0.d0
k_P = 0.d0

sum_bq_r_gP = 0.d0
sum_bq_v_gP = 0.d0

End IF


! ----------------------------------------------------------------------	  
! Coefficients ci, aij, bi 
! ----------------------------------------------------------------------	  
! s-stage Function evaluations k(s), s=0..to...
s = 8
!% order p=7

! ci coefficients, i=0,....,s
c(1:9) = (/ 0.D0, 1.D0/10.D0, 1.D0/5.D0, 3.D0/8.D0, 1.D0/2.D0, (7.D0-sqrt(21.D0))/14.D0, (7.D0+sqrt(21.D0))/14.D0, 1.D0, 1.D0 /)
	
! ----------------------------------------------------------------------
! aij coefficients, i=1,....,s, j=0,...,s-1
! ----------------------------------------------------------------------
a(1,1:8) = (/ 0.0D0, 0.0D0, 0.0D0, 0.0D0,  0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
a(2,1:8) = (/ 1.D0/200.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /)       
a(3,1:8) = (/ 1.D0/150.D0,   1.D0/75.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /) 
a(4,1:8) = (/ 171.D0/8192.D0,   45.D0/4096.D0,   315.D0/8192.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /)     
a(5,1:8) = (/ 5.D0/288.D0,   25.D0/528.D0,   25.D0/672.D0,   16.D0/693.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /)

!!a(6,1:5) = (/ (1003.D0-205.D0*sqrt(21.D0))/12348.D0,   -25.D0*(751.D0-173.D0*sqrt(21.D0))/90552.D0,   25.D0*(624.D0-137.D0*sqrt(21.D0))/43218.D0,   -128.D0*(361.D0-79.D0*sqrt(21.D0))/237699.D0,   (3411.D0-745.D0*sqrt(21.D0))/24696.D0, 	 0.0D0,   0.0D0,   0.0D0 /)
a(6,1) = (1003.D0-205.D0*sqrt(21.D0))/12348.D0
a(6,2) = -25.D0*(751.D0-173.D0*sqrt(21.D0))/90552.D0
a(6,3) = 25.D0*(624.D0-137.D0*sqrt(21.D0))/43218.D0
a(6,4) = -128.D0*(361.D0-79.D0*sqrt(21.D0))/237699.D0   
a(6,5) = (3411.D0-745.D0*sqrt(21.D0))/24696.D0
a(6,6:8) = (/ 0.0D0,   0.0D0,   0.0D0 /)

!a(7,1:8) = (/ (793.D0+187.D0*sqrt(21.D0))/12348.D0,   -25.D0*(331.D0+113.D0*sqrt(21.D0))/90552.D0,   25.D0*(1044.D0+247.D0*sqrt(21.D0))/43218.D0,   -128.D0*(14885.D0+3779.D0*sqrt(21.D0))/9745659.D0,   (3327.D0+797.D0*sqrt(21.D0))/24696.D0,   -(581.D0+127.D0*sqrt(21.D0))/1722.D0,   0.0D0,   0.0D0 /)  
a(7,1) = (793.D0+187.D0*sqrt(21.D0))/12348.D0
a(7,2) = -25.D0*(331.D0+113.D0*sqrt(21.D0))/90552.D0
a(7,3) = 25.D0*(1044.D0+247.D0*sqrt(21.D0))/43218.D0
a(7,4) = -128.D0*(14885.D0+3779.D0*sqrt(21.D0))/9745659.D0
a(7,5) = (3327.D0+797.D0*sqrt(21.D0))/24696.D0
a(7,6) = -(581.D0+127.D0*sqrt(21.D0))/1722.D0
a(7,7) = 0.0D0
a(7,8) = 0.0D0  

!a(8,1:8) = (/ -(157.D0-3.D0*sqrt(21.D0))/378.D0,   25.D0*(143.D0-10.D0*sqrt(21.D0))/2772.D0,   -25.D0*(876.D0+55.D0*sqrt(21.D0))/3969.D0,  	1280.D0*(913.D0+18.D0*sqrt(21.D0))/596673.D0,   -(1353.D0+26.D0*sqrt(21.D0))/2268.D0,  	7.D0*(1777.D0+377.D0*sqrt(21.D0))/4428.D0,   7.D0*(5.D0-sqrt(21.D0))/36.D0,   0.0D0 /)
a(8,1) = -(157.D0-3.D0*sqrt(21.D0))/378.D0 
a(8,2) = 25.D0*(143.D0-10.D0*sqrt(21.D0))/2772.D0 
a(8,3) = -25.D0*(876.D0+55.D0*sqrt(21.D0))/3969.D0 
a(8,4) = 1280.D0*(913.D0+18.D0*sqrt(21.D0))/596673.D0 
a(8,5) = -(1353.D0+26.D0*sqrt(21.D0))/2268.D0 
a(8,6) = 7.D0*(1777.D0+377.D0*sqrt(21.D0))/4428.D0 
a(8,7) = 7.D0*(5.D0-sqrt(21.D0))/36.D0 
a(8,8) = 0.0D0 

!a(9,1:8) = (/ 1.D0/20.D0,   0.D0,   0.D0,   0.D0,   8.D0/45.D0,   7.D0*(7.D0+sqrt(21.D0))/360.D0,   7.D0*(7.D0-sqrt(21.D0))/360.D0,   0.D0 /)   
a(9,1:5) = (/ 1.D0/20.D0,   0.D0,   0.D0,   0.D0,   8.D0/45.D0 /)
a(9,6) = 7.D0*(7.D0+sqrt(21.D0))/360.D0
a(9,7) = 7.D0*(7.D0-sqrt(21.D0))/360.D0 
a(9,8) = 0.D0      
! ----------------------------------------------------------------------
  
! position coefficients bi, order p+1,  i=0,....,s
b1(1:9) = (/ 1.D0/20.D0, 0.D0, 0.D0, 0.D0, 8.D0/45.D0, 7.D0*(7.D0+sqrt(21.D0))/360.D0, 7.D0*(7.D0-sqrt(21.D0))/360.D0, 0.D0, 0.D0 /)

! position coefficients bi, order p,  i=0,....,s
b(1:9) = (/ 1.D0/20.D0, 0.D0, 0.D0, 0.D0, 8.D0/45.D0, 7.D0*(7.D0+sqrt(21.D0))/360.D0, 7.D0*(7.D0-sqrt(21.D0))/360.D0,  & 
           -lamda_h, lamda_h /)

! velocity coefficients bi, order p+1(p),  i=0,....,s
b2(1:9) = (/ 1.D0/20.D0, 0.D0, 0.D0, 0.D0, 16.D0/45.D0, 49.D0/180.D0, 49.D0/180.D0, 1.D0/20.D0, 0.D0 /)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Function evaluations K(i), i = 0,...,s
! ----------------------------------------------------------------------
sum_ak(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /) 

DO i = 0 , s
    If (i == 0) Then
        t(i+1) = to        
        r(1:3,i+1) = ro
		! MJD (in days) at ti including the fraction of the day
		mjd_t = INT(mjd_to) + t(i+1) / (24.D0 * 3600.D0)
		ri1(1:3) = ro
		vi1 = vo
! ----------------------------------------------------------------------
		! Force model acceleration components & Partial derivatives w.r.t. state vector & Parameters 
		Call pd_force (mjd_t, to, ri1, vi1, Fi, pd_Fr, pd_Fv, pd_Fp, i) 
! ----------------------------------------------------------------------	
!      Function evaluations (ki) for State vector (z = [r v]')       
		fx = Fi(1)
		fy = Fi(2)
		fz = Fi(3)
        k(1:3,i+1) = (/ fx, fy, fz /)
! ----------------------------------------------------------------------
!      Function evaluations for the State Transition matrix (k_Z)	
		! Position transition matrix : k_Z 3x6 matrix
		kZ_i = MATMUL(pd_Fr, veqZ_ro) 		
		!print *,"kZ_i matmul", kZ_i				
        ! Function evaluation as a 3x6xi matrix
		k_Z(:,:,i+1) = kZ_i(:,:)  										 
! ----------------------------------------------------------------------
!       Function evaluations for the Sensitivity matrix (k_P)
If (Np /= 0) Then
        !gp = pd_Fr * veqP_ro + pd_Fp  ! 3xNp matrix
		kP_i = MATMUL(pd_Fr, veqP_ro) 			
		kP_i = kP_i + pd_Fp
        ! Function evaluation as a 3xPxi matrix
		k_P(:,:,i+1) = kP_i(:,:) 									
End IF 
! ----------------------------------------------------------------------
    Else        
! ----------------------------------------------------------------------
!      Function evaluations (ki) for State vector        
        Do j = 0 , i-1    
           sum_ak = sum_ak + a(i+1,j+1) * k(1:3,j+1)
        end do        	        
		t(i+1) = to + c(i+1) * h
        r(1:3,i+1) = ro + c(i+1) * h * vo + h**2 * sum_ak	
        sum_ak(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /) 

		ri1(1:3) = r(1:3,i+1)       
		! MJD (in days) at ti including the fraction of the day
		mjd_t = INT(mjd_to) + t(i+1) / (24.D0 * 3600.D0)
        ! Compute velocity at intermediate epoch ti
        vi1 = vo + (h / c(i+1)) * sum_ak 	
! ----------------------------------------------------------------------
		! Force model acceleration components & Partial derivatives w.r.t. state vector & Parameters 
                t_sec = t(i+1)
		Call pd_force (mjd_t, t_sec, ri1, vi1, Fi, pd_Fr, pd_Fv, pd_Fp, i) 	
! ----------------------------------------------------------------------
		fx = Fi(1)
		fy = Fi(2)
		fz = Fi(3)
        k(1:3,i+1) = (/ fx, fy, fz /)
! ----------------------------------------------------------------------
!      Function evaluations for the State Transition matrix
       Do j = 0 , i-1		
          G_kZ(1,1:6) = (/ k_Z(1,1,j+1), k_Z(1,2,j+1), k_Z(1,3,j+1), & 
		                   k_Z(1,4,j+1), k_Z(1,5,j+1), k_Z(1,6,j+1) /) 
          G_kZ(2,1:6) = (/ k_Z(2,1,j+1), k_Z(2,2,j+1), k_Z(2,3,j+1), & 
		                   k_Z(2,4,j+1), k_Z(2,5,j+1), k_Z(2,6,j+1) /) 
          G_kZ(3,1:6) = (/ k_Z(3,1,j+1), k_Z(3,2,j+1), k_Z(3,3,j+1), & 
		                   k_Z(3,4,j+1), k_Z(3,5,j+1), k_Z(3,6,j+1) /) 
          sum_ag = sum_ag + a(i+1,j+1) * G_kZ
       End Do
       veqZ_r = veqZ_ro + c(i+1) * h * veqZ_vo + h**2 * sum_ag
   		!print *," "  										 
   		!print *,"i", i  										 
		!print *,"c*h*veqZ_vo", c(i+1) * h * veqZ_vo 										 
		!print *,"h**2*sum_ag", h**2 * sum_ag 										 

	   ! Re-Initialize to zero
           sum_ag = 0.0D0
		
		! Position transition matrix : k_Z 3x6 matrix
		kZ_i = MATMUL(pd_Fr, veqZ_r) 		
        ! Function evaluation as a 3x6xi matrix
		k_Z(:,:,i+1) = kZ_i(:,:) 
		!print *,"pd_Fr", pd_Fr  										 
		!print *,"kZ_i", kZ_i  										 	
! ----------------------------------------------------------------------
!       Function evaluations for the Sensitivity matrix
If (Np /= 0) Then
        Do j = 0 , i-1	
			Do i1 = 1 , 3
			   Do j1 = 1 , Np
                  G_kP(i1,j1) = k_P(i1,j1,j+1)
			   End Do	  
			End Do
            sum_agP = sum_agP + a(i+1,j+1) * G_kP
        End Do
        veqP_r = veqP_ro + c(i+1) * h * veqP_vo + h**2 * sum_agP
		! Re-Initialize to zero
              sum_agP = 0.0D0
		
        !gp = pd_Fr * veqP_ro + pd_Fp  ! 3xNp matrix
		kP_i = MATMUL(pd_Fr, veqP_r) 		
		kP_i = kP_i + pd_Fp
        ! Function evaluation as a 3xPxi matrix
		k_P(:,:,i+1) = kP_i(:,:) 									
End IF
! ----------------------------------------------------------------------
    End If
End Do


! ----------------------------------------------------------------------
! Solution
! ----------------------------------------------------------------------
! Increment function for State vector
sum_bk(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /)
sum_b1k(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /)
sum_b2k(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /)

Do i = 0 , s        
    sum_bk = sum_bk + b(i+1) * k(:,i+1)
    sum_b1k = sum_b1k + b1(i+1) * k(:,i+1)
    sum_b2k = sum_b2k + b2(i+1) * k(:,i+1)
end do

! Position and velocity approximations of order q(p+1)
r_q = ro + h * vo + h**2 * sum_b1k
v_q = vo + h * sum_b2k

z_q(1:3) = r_q 
z_q(4:6) = v_q

! Position approximation of lower order p
r_p = ro + h * vo + h**2 * sum_bk
! Local truncation error - Stepsize control
er = abs(r_q - r_p)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! State Transition matrix
! ----------------------------------------------------------------------
! Increment function for State Transition matrix
	sum_bq_r_g = 0.0D0
	sum_bq_v_g = 0.0D0 

Do i = 0 , s
    sumkZ = k_Z(:,:,i+1)
	cf = b1(i+1)
    sum_bq_r_g = sum_bq_r_g + cf * sumkZ
	cf = b2(i+1)
    sum_bq_v_g = sum_bq_v_g + b2(i+1) * sumkZ
End Do

! approximation of State transition matrix of order q
veqZ_r = veqZ_ro + h * veqZ_vo + h**2 * sum_bq_r_g
veqZ_v = veqZ_vo + h * sum_bq_v_g
veqZ(1:3,:) = veqZ_r
veqZ(4:6,:) = veqZ_v
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Sensitivity matrix
! ----------------------------------------------------------------------
If (Np /= 0) Then

! Increment function for Sensitivity matrix
Do i1 = 1 , 3
   Do j1 = 1 , Np
	sum_bq_r_gP(i1,j1) = 0.0D0
	sum_bq_v_gP(i1,j1) = 0.0D0
   End Do	  
End Do

Do i = 0 , s
	sumkP = k_P(:,:,i+1)
	cf = b1(i+1)
    sum_bq_r_gP = sum_bq_r_gP + cf * sumkP
	cf = b2(i+1)
    sum_bq_v_gP = sum_bq_v_gP + cf * sumkP
End Do

! approximation of Sensitivity matrix of order q
veqP_r = veqP_ro + h * veqP_vo + h**2 * sum_bq_r_gP
veqP_v = veqP_vo + h * sum_bq_v_gP
veqP(1:3,:) = veqP_r
veqP(4:6,:) = veqP_v

End IF
! ----------------------------------------------------------------------


 100 END SUBROUTINE


End Module
