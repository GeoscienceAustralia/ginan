SUBROUTINE chkbrdc(ISAT,EPH,AVE,STD)

! NAME       :  chkbrdc.f90
!
! PURPOSE    :  Check the broadcast reference epoch by using 3 to 4 sigma critierion
!
!
! PARAMETERS :
!         IN :  ISAT : Satellite number
!               EPH: Broadcast elements
!
!        OUT :  Print out the status of dynamic elements
!
! AUTHOR     :  Tzupang Tseng
!
!
! CREATED    :  02-08-2019
!
! CHANGES    :  02-08-2019  T. Tseng: Introduce the STD values for quality control

USE mdl_precision
IMPLICIT NONE

INTEGER*4  ::N
INTEGER (KIND = prec_int4) :: ISAT
REAL(KIND = prec_q)::PI
REAL(KIND = prec_q)::axis,cmm,ecc,ron,per,inc,ma,node
REAL(KIND = prec_q)::EPH(20),AVE(8),STD(8)

PI = 4*ATAN(1.D0)
N  = 3 ! This variable can be adjusted.

axis  =EPH(3)
ecc   =EPH(4)
inc   =EPH(5)*180/PI
node  =EPH(6)*180/PI
per   =EPH(7)*180/PI
ma    =EPH(8)*180/PI
cmm   =EPH(9)
ron   =EPH(10)*180/PI

!CHECK SEMI-MAJOR AXIS
!---------------------
IF(ABS(axis-AVE(1))>N*STD(1))PRINT*,'BAD SEMI-MAJOR AXIS, ABS(OBSERVED-MEAN)',ABS(axis-AVE(1)),'Satellite =', ISAT

! CHECK ECCENTRICITY
! ------------------
IF(ABS(ecc-AVE(2))>N*STD(2)) PRINT*,'BAD ECCENTRICITY, ABS(OBSERVED-MEAN)',ABS(ecc-AVE(2)),'Satellite =', ISAT

! CHECK INCLINATION
! ------------------
IF(ABS(inc-AVE(3)*180/PI)>N*STD(3)*180/PI) PRINT*,'BAD INCLINATION, ABS(OBSERVED-MEAN)', ABS(inc-AVE(3)*180/PI),'Satellite =', ISAT

! CHECK NODE
! ----------
IF(ABS(node-AVE(4)*180/PI)>N*STD(4)*180/PI) PRINT*,'BAD NODE, ABS(OBSERVED-MEAN)', ABS(node-AVE(4)*180/PI),'Satellite =', ISAT

! CHECK PERIGEE
! -------------
IF(ABS(per-AVE(5)*180/PI)>N*STD(5)*180/PI) PRINT*,'BAD PERIGEE, ABS(OBSERVED-MEAN)', ABS(per-AVE(5)*180/PI),'Satellite =', ISAT

! CHECK MEAN ANOMALY
! ------------------
IF(ABS(ma-AVE(6)*180/PI)>N*STD(6)*180/PI) PRINT*,'BAD MEAN ANOMALY, ABS(OBSERVED-MEAN)', ABS(ma-AVE(6)*180/PI),'Satellite =', ISAT

! CHECK CORRECTION TO MEAN MOTION
! -------------------------------
IF(ABS(cmm-AVE(7))>N*STD(7)) PRINT*,'BAD CORRECTION TO MEAN MOTION, ABS(OBSERVED-MEAN)', ABS(cmm-AVE(7)),'Satellite =', ISAT

! CHECK RATE OF NODE
! ------------------
IF(ABS(ron-AVE(8)*180/PI)>N*STD(8)*180/PI) PRINT*,'BAD RATE OF NODE, ABS(OBSERVED-MEAN)', ABS(ron-AVE(8)*180/PI),'Satellite =', ISAT

END 
