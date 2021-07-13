SUBROUTINE reformbrdc(MAXNPAR,eph1,eph2,clk)

! NAME       :  reformbrdc.f90
!
! PURPOSE    :  Rearrange the broadcast information into orbit-related
!               and clock related groups.
!
! PARAMETERS :
!         IN :  eph1          : Broadcast dynamic elements
!
!        OUT :  eph2          : the rearranged array
!
!
! Author:  Tzupang Tseng
!
!


USE mdl_precision
IMPLICIT NONE

INTEGER (KIND = prec_int4) :: MAXNPAR
REAL    (KIND = prec_q)    :: eph2(MAXNPAR)
REAL    (KIND = prec_q)    :: eph1(MAXNPAR)
!REAL    (KIND = prec_q)    :: clk(MAXNPAR)
REAL    (KIND = prec_q)    :: clk(3)

! Rearrange EPH and CLOCK
! ------------------------
eph2(1)  = eph1(23)                ! GPS week
eph2(2)  = DNINT(eph1(13)*1D6)/1D6 ! toe
eph2(3)  = eph1(12)**2             ! semi-major axis
eph2(4)  = eph1(10)                ! eccentricity
eph2(5)  = eph1(17)                ! inclination
eph2(6)  = eph1(15)                ! right ascension of ascending node
eph2(7)  = eph1(19)                ! perigee
eph2(8)  = eph1(8)                 ! mean anomaly
eph2(9)  = eph1(7)                 ! correction to mean motion
eph2(10) = eph1(20)                ! rate of node
eph2(11) = eph1(11)                ! cus
eph2(12) = eph1(9)                 ! cuc
eph2(13) = eph1(6)                 ! crs
eph2(14) = eph1(18)                ! crc
eph2(15) = eph1(16)                ! cis
eph2(16) = eph1(14)                ! cic
eph2(17) = eph1(5)                 ! aode
eph2(18) = eph1(21)                ! idot
eph2(19) = 0.d0
eph2(20) = 0.d0


!clk(1)  = eph1(23)                 ! GPS week
!clk(2)  = eph1(22)                 ! L2 code indicator  
!clk(3)  = eph1(25)                 ! user ranging error (m)
!clk(4)  = eph1(26)                 ! satellite health message
!clk(6)  = eph1(24)                 ! P2 data flag
!clk(9)  = eph1(27)                 ! 
!clk(10) = eph1(28)
!clk(11) = eph1(1)                  ! toc
!IF(clk(11)-eph2(2).GT.+302400.D0) clk(1)=clk(1)-1
!IF(clk(11)-eph2(2).LT.-302400.D0) clk(1)=clk(1)+1
!clk(12) = eph1(4)                  ! A2
!clk(13) = eph1(3)                  ! A1
!clk(14) = eph1(2)                  ! A0

clk(1) = eph1(2)                  ! A0
clk(2) = eph1(3)                  ! A1
clk(3) = eph1(4)                  ! A2


END
