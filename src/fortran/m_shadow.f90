MODULE m_shadow

! ----------------------------------------------------------------------
! MODULE: m_shadow
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the m_shadow subroutine
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng, Geosceince Australia, Australia 
!               
!               
! Created:      16-04-2019
! ----------------------------------------------------------------------

      IMPLICIT NONE
      !SAVE

Contains

SUBROUTINE shadow ( r_sat, r_sun, r_moon, lambda, ECLTYP )

! ----------------------------------------------------------------------
! SUBROUTINE: shadow.f90
! ----------------------------------------------------------------------
! Purpose:
! This subrutine is used to compute the coefficient for scaling the solar radiation pressure 
! from penumbra to umbra. The concept of this model is to use the ratio of the celestial radius 
! over the distance (the celestial to the satellite) as a threshold for the eclipsing judgement. 
! Such a ratio forms an apparent angle (in radian) viewed from the satellite. 
! By comparing with different apparent angles as viewed from the satellite, the satellite can be 
! judged in the sun light, penumbra or umbra.
!
! This concept can be easily understood through three illustrations: a sideview,
! a look-down view from the satellite and a sideview of the vector yy.    
! ----------------------------------------------------------------------
! Input arguments:
! - r_sat        : satellite position vector (m)
! - r_sun        : Sun position vector wrt the earth
! - r_moon       : Moon position vector wrt the earth
! 
! Output arguments:
! - lambda       : Coefficient for scaling the SRP-induced acceleration acting
!                  on the eclipsed GNSS satellites
!                  
!                  lambda = 1     : In SUN LIGHT AREA
!                         = 0     : In UMBRA AREA (full eclipse)
!                  0 < lambda < 1 : In PENUMBRA AREA 
!- ECLTYP        : ECLIPSE TYPE = 'E' : eclipsed by the earth shadow
!                                 'M' : eclipsed by the moon  shadow 
!                                 ' ' : no eclipse
!
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
!
! Created:	16-04-2019
!
! Changes:      Simon McClusky 19/06/2019 (Fixed bug in lunar eclipse logic)
! 
! Copyright:  GEOSCIENCE AUSTRALIA
! ----------------------------------------------------------------------
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE m_get_lambda
      IMPLICIT NONE

! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3),INTENT(IN) :: r_sat,r_moon
      REAL (KIND = prec_q), DIMENSION(3),INTENT(IN) :: r_sun
      REAL (KIND = prec_q), INTENT(OUT) :: lambda
      
! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      CHARACTER (KIND = 1) :: ECLTYP
      REAL (KIND = prec_q) :: sunrad, moonrad, ertrad
      REAL (KIND = prec_q) :: Dsun, Dmoonsun
      REAL (KIND = prec_q) :: Ds,sclfa
      REAL (KIND = prec_q) :: AB, CD
      REAL (KIND = prec_q) :: rp, rs, xx
      REAL (KIND = prec_q), DIMENSION(3) :: ud 
      REAL (KIND = prec_q), DIMENSION(3) :: yy
      REAL (KIND = prec_q), DIMENSION(3) :: Rmoonsun, Rsatmoon
      INTEGER              :: i,j,k
      INTEGER (KIND = 4) :: idbprt,idebug
      
! ----------------------------------------------------------------------
! Numerical Constants
! ----------------------------------------------------------------------
sunrad = Sun_radius
moonrad = Moon_radius
ertrad = Earth_Radius
lambda = 1.0d0
ECLTYP = ' '

! ---------------------------------------------------------------------
! The distance between the Earth and Sun
Dsun = norm2(r_sun)

! The distance between the Satellite and the Sun
Ds = norm2(r_sun - r_sat)

! The unit vector of the satellite wrt the sun (SUN->SAT)
ud=(r_sat-r_sun)/Ds

! Check if an Earth  Eclipse is possible. Ie satellite is in the sunlit area?
IF (Ds .gt. Dsun) THEN

! Project the satellite position vector onto the unit vector SUN->SAT
   AB = dot_product(r_sat,ud)
   
! A vector resulted from the cross product of the position vector EARTH->SAT and the unit vector SUN->SAT 

   CALL productcross(r_sat,ud,yy)

! rs  : apparent angle of sun as viewed from satellite (radians)
! rp  : apparent angle of eclipsing body as viewed from satellite (radians)
! xx  : apparent separation of the center of the Sun and eclipsing body (radians)
!     rs+rp <= xx : no eclipse
!    
!     rp-rs >= xx : full eclipse

   xx = norm2(yy)/AB
   rs=sunrad/Ds 
   rp=ertrad/AB 

! Check the earth shadow  

   CALL get_lambda(rs, rp, xx, lambda, idbprt, idebug) 

   IF (lambda .lt. 1.0d0) THEN
      ECLTYP = 'E'
!     PRINT*,' ECLTYP =  ', ECLTYP
      RETURN
   ENDIF

ENDIF

! Now check if a lunar eclipse is possible? Ie is the moon closer to the Sun than the Satellite?
Rmoonsun = r_moon - r_sun
Rsatmoon = r_sat - r_moon

!Dmoonsun = dsqrt(Rmoonsun(1)**2+Rmoonsun(2)**2+Rmoonsun(3)**2)
Dmoonsun = norm2(Rmoonsun)
    
IF (Ds .gt. Dmoonsun) THEN

   CD = dot_product(Rsatmoon,ud)
   CALL productcross (Rsatmoon,ud,yy)
   xx=norm2(yy)/CD

   rs=sunrad/Ds 
   rp=moonrad/CD 

   CALL get_lambda(rs, rp, xx, lambda, idbprt, idebug)

   IF( lambda.lt.1.d0 ) THEN
      ECLTYP = 'M'
!     PRINT*,' ECLTYP =  ', ECLTYP
      RETURN
   ENDIF

END IF

END SUBROUTINE

END MODULE 
