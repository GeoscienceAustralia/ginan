SUBROUTINE integr_rkn768(zo, step, lamda_h, z_q, e_r)


! ----------------------------------------------------------------------
! SUBROUTINE: integr_rkn768.f03
! ----------------------------------------------------------------------
! Purpose:
!  Runge-Kutta-Nystrom numerical integration methods
!  RKN7(6)-8 method by Dormand & Prince(1978) is implemented for satellite orbit integration
! ----------------------------------------------------------------------
! Input arguments:
! - zo:           Initial epoch (to) and state vector in Celestial Reference System GCRS
!                 zo = [MJDo to ro vo]  
!                 MJDo: initial epoch's Modified Julian Day (MJD) number (including the fraction of the day)
!                 ro:   Position vector at initial epoch in GCRS (m)
!                 vo:   Velocity vector at initial epoch in GCRS (m/sec)
! - step:         Integration stepsize (sec)
! - lamda_h:      RKN7(6)-8 specific parameter for stepsize control
!
! Output arguments
! - z_q:          State vector at the next epoch t=to+h in GCRS
! - e_r:          Local truncation error of position vector
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  6 October 2017
! ----------------------------------------------------------------------
        
        
      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(8) :: zo
      REAL (KIND = prec_d), INTENT(IN) :: step
      REAL (KIND = prec_d), INTENT(IN) :: lamda_h
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(6) :: z_q
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(3) :: e_r
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: h, to, mjd_t, mjd_to, t_sec
      REAL (KIND = prec_d), DIMENSION(9) :: t
      REAL (KIND = prec_d), DIMENSION(3) :: ro, vo
      REAL (KIND = prec_d), DIMENSION(3) :: ri1, vi1
      REAL (KIND = prec_d), DIMENSION(3) :: r_q, v_q, r_p, er
!       , ev
      REAL (KIND = prec_d), DIMENSION(3,9) :: r, k
      REAL (KIND = prec_d), DIMENSION(3) :: sum_ak 
      REAL (KIND = prec_d), DIMENSION(9) :: c, b, b1, b2 
      REAL (KIND = prec_d), DIMENSION(9,8) :: a 
!       REAL (KIND = prec_d), DIMENSION(8,9) :: a_transp 
      INTEGER (KIND = prec_int4) :: s, i, j
      REAL (KIND = prec_d) :: fx, fy, fz
        REAL (KIND = prec_d), DIMENSION(3) :: sum_bk, sum_b1k, sum_b2k
! ----------------------------------------------------------------------        



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Integration step
h = step
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% Initial Conditions
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% Initial epoch
! MJD
mjd_to = zo(1)
! Seconds at MJDo since 00h
to = zo(2) !to = (mjd_to - INT(mjd_to)) * (24.D0 * 3600.D0)

!% Initial position and velocity vectors
ro = zo(3:5)
vo = zo(6:8)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Coefficients ci, aij, bi 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! s-stage Function evaluations k(s), s=0..to...
s = 8

!% order p=7


! ci coefficients, i=0,....,s
!c=[ 0
!    1/10
!    1/5
!    3/8
!    1/2
!    (7-sqrt(21))/14
!    (7+sqrt(21))/14
!    1
!    1];
c(1:9) = (/ 0.D0, 1.D0/10.D0, 1.D0/5.D0, 3.D0/8.D0, 1.D0/2.D0, (7.D0-sqrt(21.D0))/14.D0, (7.D0+sqrt(21.D0))/14.D0, 1.D0, 1.D0 /)


      
! ----------------------------------------------------------------------
! aij coefficients, i=1,....,s, j=0,...,s-1
! ----------------------------------------------------------------------
!a=[  NaN                         NaN                            NaN                             NaN                                 NaN                          NaN                          NaN                  NaN
!    1/200                        NaN                            NaN                             NaN                                 NaN                          NaN                          NaN                  NaN      
!    1/150                        1/75                           NaN                             NaN                                 NaN                          NaN                          NaN                  NaN 
!    171/8192                     45/4096                        315/8192                        NaN                                 NaN                          NaN                          NaN                  NaN      
!    5/288                        25/528                         25/672                          16/693                              NaN                          NaN                          NaN                  NaN 
!    (1003-205*sqrt(21))/12348   -25*(751-173*sqrt(21))/90552    25*(624-137*sqrt(21))/43218    -128*(361-79*sqrt(21))/237699        (3411-745*sqrt(21))/24696    NaN                          NaN                  NaN 
!    (793+187*sqrt(21))/12348    -25*(331+113*sqrt(21))/90552    25*(1044+247*sqrt(21))/43218   -128*(14885+3779*sqrt(21))/9745659   (3327+797*sqrt(21))/24696   -(581+127*sqrt(21))/1722      NaN                  NaN  
!    -(157-3*sqrt(21))/378        25*(143-10*sqrt(21))/2772     -25*(876+55*sqrt(21))/3969       1280*(913+18*sqrt(21))/596673      -(1353+26*sqrt(21))/2268      7*(1777+377*sqrt(21))/4428   7*(5-sqrt(21))/36    NaN
!    1/20                         0                              0                               0                                   8/45                         7*(7+sqrt(21))/360           7*(7-sqrt(21))/360   0   ];

a(1,1:8) = (/ 0.0D0, 0.0D0, 0.0D0, 0.0D0,  0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
a(2,1:8) = (/ 1.D0/200.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /)       
a(3,1:8) = (/ 1.D0/150.D0,   1.D0/75.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /) 
a(4,1:8) = (/ 171.D0/8192.D0,   45.D0/4096.D0,   315.D0/8192.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /)     
a(5,1:8) = (/ 5.D0/288.D0,   25.D0/528.D0,   25.D0/672.D0,   16.D0/693.D0,   0.0D0,   0.0D0,   0.0D0,   0.0D0 /)

!!a(6,1:5) = (/ (1003.D0-205.D0*sqrt(21.D0))/12348.D0,   -25.D0*(751.D0-173.D0*sqrt(21.D0))/90552.D0,   25.D0*(624.D0-137.D0*sqrt(21.D0))/43218.D0,   -128.D0*(361.D0-79.D0*sqrt(21.D0))/237699.D0,   (3411.D0-745.D0*sqrt(21.D0))/24696.D0,     0.0D0,   0.0D0,   0.0D0 /)
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

!a(8,1:8) = (/ -(157.D0-3.D0*sqrt(21.D0))/378.D0,   25.D0*(143.D0-10.D0*sqrt(21.D0))/2772.D0,   -25.D0*(876.D0+55.D0*sqrt(21.D0))/3969.D0,      1280.D0*(913.D0+18.D0*sqrt(21.D0))/596673.D0,   -(1353.D0+26.D0*sqrt(21.D0))/2268.D0,     7.D0*(1777.D0+377.D0*sqrt(21.D0))/4428.D0,   7.D0*(5.D0-sqrt(21.D0))/36.D0,   0.0D0 /)
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
    
  
!% position coefficients bi, order p+1,  i=0,....,s
!b1 = [ 1/20 0 0 0 8/45 7*(7+sqrt(21))/360 7*(7-sqrt(21))/360 0 0 ]';
b1(1:9) = (/ 1.D0/20.D0, 0.D0, 0.D0, 0.D0, 8.D0/45.D0, 7.D0*(7.D0+sqrt(21.D0))/360.D0, 7.D0*(7.D0-sqrt(21.D0))/360.D0, 0.D0, 0.D0 /)


!% position coefficients bi, order p,  i=0,....,s
!b = [ 1/20 0 0 0 8/45 7*(7+sqrt(21))/360 7*(7-sqrt(21))/360 -lamda_h lamda_h ]';
b(1:9) = (/ 1.D0/20.D0, 0.D0, 0.D0, 0.D0, 8.D0/45.D0, 7.D0*(7.D0+sqrt(21.D0))/360.D0, 7.D0*(7.D0-sqrt(21.D0))/360.D0,  & 
           -lamda_h, lamda_h /)


!% velocity coefficients bi, order p+1(p),  i=0,....,s
!b2 = [ 1/20 0 0 0 16/45 49/180 49/180 1/20 0 ]';
b2(1:9) = (/ 1.D0/20.D0, 0.D0, 0.D0, 0.D0, 16.D0/45.D0, 49.D0/180.D0, 49.D0/180.D0, 1.D0/20.D0, 0.D0 /)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% Equations for the description of the Runge-Kutta-Nystrom method
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% Function evaluations K(i), i = 0,...,s
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! K(i) is expressed here as matrix K(:,i) = [Kx(i); Ky(i); Kz(i)]
sum_ak(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /) 


DO i = 0 , s
    If (i == 0) Then
      
!%%      Function evaluations (ki) for State vector (z = [r v]')       
        t(i+1) = to        
        r(1:3,i+1) = ro
            
        !% random value for velocity v = vo:
        !% Force model is indepedent from velocity vector
            vi1 = vo

            ri1(1:3) = ro
        
            ! MJD (in days) at ti including the fraction of the day
            mjd_t = INT(mjd_to) + t(i+1) / (24.D0 * 3600.D0)

            ! Force model acceleration components
                t_sec = t(i+1)
            Call force_sum(mjd_t, t_sec, ri1, vi1, fx, fy, fz, i)
            
        k(1:3,i+1) = (/ fx, fy, fz /)

    Else        
      
!%%      Function evaluations (ki) for State vector        
        Do j = 0 , i-1    
           !sum_ak = sum_ak + a(i+1,j+1) * k(:,j+1);
           sum_ak = sum_ak + a(i+1,j+1) * k(1:3,j+1)
        end do        
                    
            t(i+1) = to + c(i+1) * h
        r(1:3,i+1) = ro + c(i+1) * h * vo + h**2 * sum_ak
            
        sum_ak(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /) 
        
            !% random value for velocity v = vo:
        !% Force model is indepedent from velocity vector
            vi1 = vo

            ri1(1:3) = r(1:3,i+1)
        
            ! MJD (in days) at ti including the fraction of the day
            mjd_t = INT(mjd_to) + t(i+1) / (24.D0 * 3600.D0)

            ! Force model acceleration components
                t_sec = t(i+1)
            Call force_sum(mjd_t, t_sec, ri1, vi1, fx, fy, fz, i)

        k(1:3,i+1) = (/ fx, fy, fz /)
            
    End If
End Do
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!% Increment function for State vector
sum_bk(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /)
sum_b1k(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /)
sum_b2k(1:3) = (/ 0.0D0, 0.0D0, 0.0D0 /)


Do i = 0 , s        
    sum_bk = sum_bk + b(i+1) * k(:,i+1)
    sum_b1k = sum_b1k + b1(i+1) * k(:,i+1)
    sum_b2k = sum_b2k + b2(i+1) * k(:,i+1)
end do



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% Solution
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%  Embedded Runge-Kutta method
!%  Position and velocity approximations of order q(p+1)
r_q = ro + h * vo + h**2 * sum_b1k
v_q = vo + h * sum_b2k

z_q(1:3) = r_q 
z_q(4:6) = v_q
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!% Local truncation error
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%  Position approximation of lower order p
r_p = ro + h * vo + h**2 * sum_bk

!%  Local truncation error - Stepsize control
er = abs(r_q - r_p)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e_r = er

END SUBROUTINE
