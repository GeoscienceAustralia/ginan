MODULE m_antoffset

      IMPLICIT NONE
      !SAVE
Contains


SUBROUTINE antoffset(mjd, R1, R2)


! ----------------------------------------------------------------------
! SUBROUTINE: antoffset.f90
! ----------------------------------------------------------------------
! Purpose:
!  Correct the position of transmitting antenna to the center of mass of satellite
!
! NOTE: If the orbital information from SP3 file is based on the center of mass
! of satellite, then this routine cannot be used for the position correction.
! Conversely, if the satellite position is given in the phase center of transmitting antenna, 
! then this routine is used to correct the poistion from antenna to the center of mass.
! ----------------------------------------------------------------------
! Input:
! - mjd: mjd in terrestrial time system
! - R1 : The position of the transmitting antenna 
!
! Output: 
! - R2 : The position of center of mass of satellite 
!
! ----------------------------------------------------------------------
! Author : Dr. Tzupang Tseng, Geoscience Australia         
!
! Create: 21-08-2019
!
! ----------------------------------------------------------------------

USE mdl_precision
IMPLICIT NONE

! ---------------------------------------------------------------------------
REAL (KIND = prec_q), DIMENSION(3) :: R1, R2
REAL (KIND = prec_q), DIMENSION(3) :: yy 
REAL (KIND = prec_q), DIMENSION(3) :: ANTOFF
REAL (KIND = prec_q), DIMENSION(3) :: ed, ey, ex, ez
REAL (KIND = prec_q), DIMENSION(3) :: rbody
REAL (KIND = prec_q), DIMENSION(3) :: rSun 
DOUBLE PRECISION  JD, Zbody(6)
REAL (KIND = prec_q) :: Ds
REAL (KIND = prec_d) :: mjd
INTEGER (KIND = 4)   :: K
INTEGER NTARG_SUN, NCTR
! ---------------------------------------------------------------------------


! Julian Day Number of the input epoch
JD = mjd + 2400000.5D0

! Center celestial body: Earth (NCTR = 3)
NCTR = 3
NTARG_SUN = 11

! Get SUN position
CALL  PLEPH ( JD, NTARG_SUN, NCTR, Zbody )
 

rbody(1) = Zbody(1) * 1000.D0
rbody(2) = Zbody(2) * 1000.D0
rbody(3) = Zbody(3) * 1000.D0
rSun = rbody

! The unit vector ez SAT->EARTH
ez(1) = -R1(1)/SQRT(R1(1)**2+R1(2)**2+R1(3)**2)
ez(2) = -R1(2)/SQRT(R1(1)**2+R1(2)**2+R1(3)**2)
ez(3) = -R1(3)/SQRT(R1(1)**2+R1(2)**2+R1(3)**2)

! The unit vector ed SAT->SUN
Ds=sqrt((rSun(1)-R1(1))**2+(rSun(2)-R1(2))**2+(rSun(3)-R1(3))**2)
ed(1)=((rSun(1)-R1(1))/Ds)
ed(2)=((rSun(2)-R1(2))/Ds)
ed(3)=((rSun(3)-R1(3))/Ds)

! The unit vector ey = ez x ed/|ez x ed|
CALL productcross (ez,ed,yy)
ey(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
ey(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
ey(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)

! The unit vector of x surface
CALL productcross (ey,ez,ex)


! Get antenna offset (the following information is used for GLN testing)
! For more detailed information, the SINEX file is required.
! ----------------------------------------------------------------------
ANTOFF(1) = -0.545d0
ANTOFF(2) =  0.d0
ANTOFF(3) =  2.5d0

DO K=1,3
R2(K)=R1(K)-ex(K)*ANTOFF(1) &
           -ey(K)*ANTOFF(2) & 
           -ez(K)*ANTOFF(3)
END DO


END
END
