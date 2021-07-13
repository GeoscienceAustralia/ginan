SUBROUTINE glnacc (idir,rx,ry,rz,vx,vy,ACC)

! ---------------------------------------------------------------------------
! Purpose: Compute the glonass accelerations for the poistion propagation in
!          ECEF system.
!
! INPUT :
!        idir   : =1 X direction
!               : =2 Y direction
!               : =3 Z direction
!   (rx,ry,rz)  : satellite coordinates
!    vx, vy     : velocity in x and y dirctions 
!    
! OUTPUT :
!        ACC    : acceleration
!
!
! ---------------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng, Geoscience Australia
!
! Created:      17-07-2019
! ----------------------------------------------------------------------


USE mdl_precision
IMPLICIT NONE
REAL (KIND = prec_d) ::  rx,ry,rz
REAL (KIND = prec_d) ::  vx,vy
REAL (KIND = prec_d) ::  GM,RE,WE,C20,ACC
REAL (KIND = prec_d) ::  A1,A2,A3,A4,D
INTEGER (KIND = prec_int4) :: idir

GM  = 398.60044D12
RE  = 6378136.D0
WE  = 7292115.D-11
C20 = -1082.63D-6

D=SQRT(rx**2+ry**2+rz**2)

IF(idir.EQ.1)THEN
A1=-GM/(D*D*D)*rx
A2=3.D0/2.D0*C20*(GM*RE**2)/(D*D*D*D*D)*rx*(1-(5*rz**2)/(D**2))
A3=WE**2*rx
A4=2*WE*vy
ACC=A1+A2+A3+A4
ELSEIF(idir.EQ.2)THEN
A1=-GM/(D*D*D)*ry
A2=3.D0/2.D0*C20*(GM*RE**2)/(D*D*D*D*D)*ry*(1-(5*rz**2)/(D**2))
A3=WE**2*ry
A4=-2*WE*vx
ACC=A1+A2+A3+A4
ELSEIF(idir.EQ.3)THEN
A1=-GM/(D*D*D)*rz
A2=3.D0/2.D0*C20*(GM*RE**2)/(D*D*D*D*D)*rz*(3-(5*rz**2)/(D**2))
A3=0.D0
A4=0.D0
ACC=A1+A2+A3+A4
END IF

END
                                                                           