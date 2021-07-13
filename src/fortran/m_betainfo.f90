MODULE m_betainfo


! ----------------------------------------------------------------------
! MODULE: m_betainfo.f90
! ----------------------------------------------------------------------
! Purpose:
!  Module for computing the beta anle 
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng
!
! Copyright: GEOSCIENCE AUSTRALIA, AUSTRALIA
!
! Created:      21-05-2019
! ----------------------------------------------------------------------
      IMPLICIT NONE


      Contains

SUBROUTINE betainfo (mjd, rsat, vsat, beta )
! ----------------------------------------------------------------------
! SUBROUTINE: orbinfo.f90
! ----------------------------------------------------------------------
! Purpose:
! Compute the beta angle for determining the integration step size
! ----------------------------------------------------------------------
! Input parameters:
! - mjd:               Time in MJD
! - rsat:              Satellite position vector in ICRF
! - vsat:              Satellite velocity vector in ICRF
!
! Output parameters:
! - beta (deg) :       BETA angle 
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng
!
! Copyright: GEOSCIENCE AUSTRALIA,AUSTRALIA
!
! Created:      21-05-2019
! ----------------------------------------------------------------------
      USE mdl_precision
      USE mdl_num
      USE mdl_planets
      USE m_shadow
      USE mdl_param
      IMPLICIT NONE


!-------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: prnnum
      INTEGER (KIND = 4)         :: satsvn
      REAL (KIND = prec_d), INTENT(IN) :: mjd
      REAL (KIND = prec_d), DIMENSION(3), INTENT(IN) :: rsat, vsat
      REAL (KIND = prec_q), INTENT(OUT) :: beta
      REAL (KIND = prec_q) :: lambda, del_u, yaw
      CHARACTER (KIND = 1) :: ECLTYP

!--------------------------------------------------------------------
      DOUBLE PRECISION  JD, Zbody(6)
      INTEGER (KIND = 4) :: i, j
      REAL (KIND = prec_q), DIMENSION(3) :: rbody
      REAL (KIND = prec_q), DIMENSION(3) :: rSun, rMoon
      REAL (KIND = prec_q), DIMENSION(3) :: r_sun1, r_sun2
      REAL (KIND = prec_q) :: u_sun, yaw2
      INTEGER  NTARG_SUN, NTARG_MOON, NCTR
      REAL (KIND = prec_q), DIMENSION(3) :: ed, ey, eb, ex, en, ev, ez, et
      REAL (KIND = prec_q), DIMENSION(3) :: yy
      REAL (KIND = prec_q), DIMENSION(9) :: kepler
      REAL (KIND = prec_q), DIMENSION(4) :: cosang
      REAL (KIND = prec_q) :: R11(3,3),R33(3,3)
      REAL (KIND = prec_q) :: sclfa
      REAL (KIND = prec_d) :: GM, Ds
      REAL (KIND = prec_d) :: u_sat, i_sat, omega_sat
      INTEGER (KIND = prec_int2) :: AllocateStatus
      INTEGER              :: PD_Param_ID

! Numerical Constants

      GM = GM_gfm

! Julian Day Number of the input epoch
      JD = mjd + 2400000.5D0

! Center celestial body: Earth (NCTR = 3)
      NCTR = 3
      NTARG_MOON= 10
      NTARG_SUN = 11
! MOON
! Celestial body's (NTARG) Cartesian coordinates w.r.t. Center body
! (NCTR)
      CALL  PLEPH ( JD, NTARG_MOON, NCTR, Zbody )
! Cartesian coordinates of the celestial body in meters: KM to M
      rbody(1) = Zbody(1) * 1000.D0
      rbody(2) = Zbody(2) * 1000.D0
      rbody(3) = Zbody(3) * 1000.D0
      rMoon = rbody
! SUN
      CALL  PLEPH ( JD, NTARG_SUN, NCTR, Zbody )
      rbody(1) = Zbody(1) * 1000.D0
      rbody(2) = Zbody(2) * 1000.D0
      rbody(3) = Zbody(3) * 1000.D0
      rSun = rbody

      CALL shadow (rsat, rSun, rMoon, lambda, ECLTYP )

!! The unit vector ez SAT->EARTH
      ez(1)=-rsat(1)/sqrt(rsat(1)**2+rsat(2)**2+rsat(3)**2)
      ez(2)=-rsat(2)/sqrt(rsat(1)**2+rsat(2)**2+rsat(3)**2)
      ez(3)=-rsat(3)/sqrt(rsat(1)**2+rsat(2)**2+rsat(3)**2)

! The unit vector ed SAT->SUN
! vector)
      Ds=sqrt((rSun(1)-rsat(1))**2+(rSun(2)-rsat(2))**2+(rSun(3)-rsat(3))**2)
      ed(1)=((rSun(1)-rsat(1))/Ds)
      ed(2)=((rSun(2)-rsat(2))/Ds)
      ed(3)=((rSun(3)-rsat(3))/Ds)

! The unit vector ey = ez x ed/|ez x ed|
      CALL productcross (ez,ed,yy)
      ey(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
      ey(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
      ey(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)

! The unit vector of x surface
      CALL productcross (ey,ez,ex)

! The unit vector eb = ed x ey
      CALL productcross (ed,ey,eb)

! the orbit normal vector
!------------------------
      ev(1)=vsat(1)/sqrt(vsat(1)**2+vsat(2)**2+vsat(3)**2)
      ev(2)=vsat(2)/sqrt(vsat(1)**2+vsat(2)**2+vsat(3)**2)
      ev(3)=vsat(3)/sqrt(vsat(1)**2+vsat(2)**2+vsat(3)**2)

      CALL productcross (-ez,ev,en)

! the along track direction
  CALL productcross (en,-ez,et)

! computation of the satellite argument of latitude and orbit inclination
      CALL kepler_z2k (rsat, vsat, GM, kepler)
      u_sat = kepler(9)*Pi_global/180.d0
      i_sat = kepler(3)*Pi_global/180.d0
      omega_sat = kepler(4)*Pi_global/180.d0

! compute the sun position in the satellite orbit plane by rotating omega_sat and i_sat,
! allowing us for the computation of u_sun and sun elevation angles (beta)
      do i=1,3
         do j=1,3
          R33(i,j)=1.0d0
         end do
      end do
      R33(1,3)=0.0d0
      R33(2,3)=0.0d0
      R33(3,1)=0.0d0
      R33(3,2)=0.0d0
      R33(3,3)=1.0d0

      do i=1,3
         do j=1,3
          R11(i,j)=1.0d0
         end do
      end do
      R11(1,1)=1.0d0
      R11(1,2)=0.0d0
      R11(1,3)=0.0d0
      R11(2,1)=0.0d0
      R11(3,1)=0.0d0

! rotate big omega to make the X-axis of the celestial frame consistent
! with the
! direction of right ascension of ascending node
      CALL R3(omega_sat,R33)
! rotate inclination to make the XY plane of the celestial frame
! consistent with
! the orbit plane
      CALL R1(i_sat,R11)
! convert the sun position in the celestial frame to the orbit plane
      CALL matrix_Rr(R33,rSun,r_sun1)
      CALL matrix_Rr(R11,r_sun1,r_sun2)

      beta  = atan2(r_sun2(3),sqrt(r_sun2(1)**2+r_sun2(2)**2))*180.d0/Pi_global ! in deg
!write (*,*) beta*180.0d0/Pi_global

      u_sun = atan2(r_sun2(2),r_sun2(1)) ! in rad
      del_u = u_sat - u_sun
      if (del_u*180/Pi_global .gt.360.0d0) then
      del_u=del_u-2*Pi_global
      else if(del_u*180/Pi_global .lt.0.0d0) then
      del_u=del_u+2*Pi_global
      end if

! yaw angle
      yaw= acos(ex(1)*et(1)+ex(2)*et(2)+ex(3)*et(3))

IF (beta*180/Pi_global .gt. 0.d0) yaw= -yaw ! In accordance with the IGS convention


       END SUBROUTINE
        END MODULE



