SUBROUTINE brdc2ecef(GPST,EPH,CLK,ECEFPOS,SATCLK)

! NAME       :  brdc2ecef.f90
!
! PURPOSE    :  Convert the broadcast elements to ECEF coordinates
!
! PARAMETERS :
!         IN :  GPST     :  GPS time
!               EPH      :  broadcast parameters
!               EPH(2)   :  toe
!               EPH(3)   :  semi-major axis
!               EPH(4)   :  eccentricity
!               EPH(5)   :  inclination
!               EPH(6)   :  right ascension of ascending node
!               EPH(7)   :  perigee
!               EPH(8)   :  mean anomaly
!               EPH(9)   :  correction to mean motion
!               EPH(10)  :  rate of node
!               EPH(11)  :  cus
!               EPH(12)  :  cuc
!               EPH(13)  :  crs
!               EPH(14)  :  crc
!               EPH(15)  :  cis
!               EPH(16)  :  cic
!               EPH(18)  :  idot
!
!        OUT :  ECEFPOS  :  ECEF coordinates
!
! Author: Tzupang Tseng
!
! Create: 18-09-2019
!
! Corpyright: Geoscience Australia

USE mdl_precision
IMPLICIT NONE

INTEGER :: I
REAL (KIND = prec_d) :: GPST
REAL (KIND = prec_d) :: CLK(3),EPH(20),ECEFPOS(3),SATCLK
REAL (KIND = prec_d) :: axis, toe, DT
REAL (KIND = prec_d) :: u0, u
REAL (KIND = prec_d) :: Et, e
REAL (KIND = prec_d) :: v
REAL (KIND = prec_d) :: cus,cuc,crs,crc,cis,cic
REAL (KIND = prec_d) :: w0, inc0, idot
REAL (KIND = prec_d) :: w, it, r
REAL (KIND = prec_d) :: ascnode0, ascnode,dot_ascnode
REAL (KIND = prec_d) :: GM, OMEGA

OMEGA  = 7292115.1467D-11 ! RAD/SEC
GM     = 398.6004415D12   ! M**3/SEC**

!
! Convert the broadcast to ECEF coordinates
! Time elapse since toe
! --------------------------
toe = EPH(2)
DT=GPST-toe

IF(DT.GT. 302400.D0)DT=DT-604800.D0
IF(DT.LT.-302400.D0)DT=DT+604800.D0
!print*,'DT =',DT
! Mean anomaly at GPST
! --------------------
axis = EPH(3)
u0   = EPH(8)
u    =u0+(sqrt(GM/axis**3)+EPH(9))*DT
!print*,'mean anomaly', u
! Iteration solution of eccentricity anomaly
! ---------------------------------------
Et=u
e=EPH(4)
DO I=1,10
Et=u+e*sin(Et)
ENDDO
!print*,'eccentricity anomaly', Et
! True anomaly
! -------------
v=2.D0*DATAN(DSQRT((1.D0+e)/(1.D0-e))*DTAN(Et/2.D0))
!print*,'true anomaly',v
! Correct for orbital perturbations
! ----------------------------------
cus =  EPH(11)
cuc =  EPH(12)
crs =  EPH(13)
crc =  EPH(14)
cis =  EPH(15)
cic =  EPH(16)

w0   = EPH(7) + v
inc0 = EPH(5)
idot = EPH(18)
! Argument of perigee
w  = w0                 + cuc*cos(2.D0*w0) + cus*sin(2.D0*w0)
! Radial distance
r  = axis*(1-e*cos(Et)) + crc*cos(2.D0*w0) + crs*sin(2.D0*w0)
! Inclination
it = inc0 + idot*DT     + cic*cos(2.D0*w0) + cis*sin(2.D0*w0)
!print*,'Argument of perigee, Radial distance, Inclination',w,r,it
! Compute the right ascension of ascending node
! ---------------------------------------------
ascnode0     = EPH(6)
dot_ascnode  = EPH(10)
ascnode=ascnode0+(dot_ascnode-OMEGA)*DT-OMEGA*toe
!print*,'right ascension of ascending node',ascnode

! Convert the Keplerian parameters to ECEF coordinates
! ------------------------------------------------------
ECEFPOS(1)=r*cos(w)*cos(ascnode) - r*sin(w)*cos(it)*sin(ascnode)
ECEFPOS(2)=r*cos(w)*sin(ascnode) + r*sin(w)*cos(it)*cos(ascnode)
ECEFPOS(3)=r*sin(w)*sin(it)
!print*,'ECEFPOS =', ECEFPOS
! Convert to the unit required in SP3 file
SATCLK = 1.D6*(CLK(1) + CLK(2)*DT + CLK(3)*DT**2) 

END 

	  
	  
