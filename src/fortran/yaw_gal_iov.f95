SUBROUTINE yaw_gal_iov (mjd, r_sat, v_sat, r_sun, beta_angle, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle, satbf)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_gal_iov.f03
! ----------------------------------------------------------------------
! Purpose:
!  Galileo yaw-attitude based on the yaw-steering law provided by the 
!  European GNSS Service Center  
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - r_sun:		Sun position vector (m) in ITRF
! - beta_angle:	Sun angle with respect to the orbital plane (degrees)
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
!
! Output arguments:
! - eclipsf:	non-nominal attitude status flag:
!				0 = Nominal
!				1 = Non-nominal, midnight
!				2 = Non-nominal, noon
! - eBX_nom:	Body-X axis unit vector during noninal yaw-attitude
! - eBX_ecl:	Body-X axis unit vector during eclipsng yaw-attitude (Set to eBX_nom when out of eclipse season) 
! - Yangle: 	Yaw angle array (in degrees) with 2 values: Yaw nominal and Yaw modelled
!				During nominal periods, the two values are equal
! - Mangle:		Orbit angle between satellite and orbit midnight (deg)
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia           29 May 2017
! ----------------------------------------------------------------------
! Last modified:
! 15/04/2020	Thomas Papanikolaou, 
! 				The subroutine was renamed from yaw_gal.f90 to yaw_gal_iov.f03 
! 				in order to be called by the new yaw_gal.f03
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd, r_sat(3), v_sat(3), r_sun(3), beta_angle
      INTEGER (KIND = 4), INTENT(IN) :: satbf 
	  
! OUT
      REAL (KIND = prec_d) :: Yangle(2)
	  REAL (KIND = prec_d) :: eBX_nom(3), eBX_ecl(3)
      REAL (KIND = prec_d) :: Mangle
      INTEGER (KIND = 4) :: eclipsf
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), Xsun(3)
      REAL (KIND = prec_d) :: pcross(3), pcross2(3), pdot
      REAL (KIND = prec_d) :: SVBCOS
      INTEGER (KIND = 4) :: satblk
	  REAL (KIND = prec_d) :: Yangle_nom, Yangle_ecl
      REAL (KIND = prec_d) :: eta
      REAL (KIND = prec_d) :: So(3), Sh(3)
      REAL (KIND = prec_d) :: betaX, betaY
      REAL (KIND = prec_d) :: Gama, Soy_to, ft0, fti, ftE, beta_to, beta_te
      LOGICAL GALecl
      LOGICAL NOON, NIGHT
      REAL (KIND = prec_d) :: ANOON, ANIGHT
      REAL (KIND = prec_d) :: delta_Yaw, delta_Yaw_rad
      REAL (KIND = prec_d) :: Rz_yaw(3,3)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Body-frame definition type
if (satbf == 1) then
! IGS Conventions definition
   satblk = 6 
elseif (satbf == 2) then
! Galileo body-fixed frame has same orientation with the GPS Block IIR satellites
   satblk = 5 
Else
   Print *,"Error in input argument 'satbf': Galileo Body-frame type"
end if
! ----------------------------------------------------------------------
 
! ----------------------------------------------------------------------
! Unit Vectors
! ----------------------------------------------------------------------
! Xsat:		Satellite position unit vector
      Xsat = (1D0 / sqrt(r_sat(1)**2 + r_sat(2)**2 + r_sat(3)**2) ) * r_sat
	  
! Xsun:		Sun position unit vector
      Xsun = (1D0 / sqrt(r_sun(1)**2 + r_sun(2)**2 + r_sun(3)**2) ) * r_sun
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! SVBCOS:	Cosine of the angle E
! E:		Angle between the satellite position vector and the sun position vector
! cos(E) is computed as the dot product between the unit vectors of those position vectors 
! ----------------------------------------------------------------------
      CALL productdot (Xsat, Xsun , pdot)
      SVBCOS = pdot
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Body-frame X-axis unit vector
! eBX_nom = - ( (Xsun x Xsat) x Xsat )  							
! ----------------------------------------------------------------------
      CALL productcross (Xsun,Xsat , pcross)
      CALL productcross (pcross,Xsat , pcross2 )
      eBX_nom = -1.0D0 * pcross2	  
  
if (satbf == 2) then
! Galileo body-fixed frame has same orientation with the GPS Block IIR satellites
! Galileo: Body-X unit vector reversal
      eBX_nom = -1.0D0 * eBX_nom
end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Satellite orbital angle (m)
! ----------------------------------------------------------------------
! Arguments to be removed from the yaw_nom.f90 subroutine
ANOON  = 0.0D0
ANIGHT = 0.0D0

! Mangle (in degrees)
Call yaw_nom (eBX_nom, v_sat, beta_angle, satblk, SVBCOS, ANOON, ANIGHT, Mangle, Yangle_nom) 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Galileo Attitude Law as provided by the European GNSS Service Centre (GSC) web page 
! https://www.gsc-europa.eu/support-to-developers/galileo-iov-satellite-metadata#top
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Galileo yaw steering law 
!Call gal_yawnom (Mangle, beta_angle, Yangle_nom)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Galileo non-nominal yaw attitude
!Call gal_yawecl (Mangle, beta_angle, Yangle_ecl, eclipsf)
! ----------------------------------------------------------------------


! eta: Orbital angle between satellite position vector and orbit noon (in degrees)
eta = Mangle - 180.0D0

! Sun reference vector in orbital frame according to the GSC conventions (radial direction reversal) 
So(1) = -sin(eta * (PI_global / 180.0D0) ) * cos(beta_angle * (PI_global / 180.0D0) )
So(2) = -sin(beta_angle * (PI_global / 180.0D0) )
So(3) = -cos(eta * (PI_global / 180.0D0) ) * cos(beta_angle * (PI_global / 180.0D0) )


! ----------------------------------------------------------------------
! Galileo yaw steering law for IOV satellites
! ----------------------------------------------------------------------
! Nominal yaw angle (in degrees)
!Yangle_nom = ATAN2( -So(2) , -So(1) ) * (180.0D0 / PI_global)
Yangle_nom = ATAN2( -So(2) / sqrt(1.0D0 - So(3)**2) , -So(1) / sqrt(1.0D0 - So(3)**2) ) * (180.0D0 / PI_global)

! IGS Conventions
IF (satbf == 1) then
Yangle_nom = ATAN2( So(2) / sqrt(1.0D0 - So(3)**2) , So(1) / sqrt(1.0D0 - So(3)**2) ) * (180.0D0 / PI_global)
END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Galileo non-nominal yaw attitude
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Conditions limits for modified yaw-steering attitude
betaX = 15.0D0
betaY =  2.0D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Conditions 
GALecl = .FALSE. 	 
If ( ( abs(So(1)) < sin(betaX*(PI_global / 180.0D0)) ) .and. &
     ( abs(So(2)) < sin(betaY*(PI_global / 180.0D0)) ) ) THEN

     GALecl = .TRUE. 	 

END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
IF (GALecl .EQV. .TRUE.) THEN

! ----------------------------------------------------------------------
! Gama parameter computation
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Orbit midnight or noon turn
! ----------------------------------------------------------------------
      NOON=.FALSE.
      NIGHT=.FALSE.

      If (SVBCOS < 0.0D0) Then
        NIGHT=.TRUE.
      Else If (SVBCOS > 0.0D0) then
        NOON=.TRUE.	
      Else If (SVBCOS == 0.0D0) then
	  ! Special case: E = 90deg      	  
        NOON=.TRUE.	  
      END IF
! ----------------------------------------------------------------------
! Towards night
If (NIGHT) Then 

    ft0 = - betaX
    !ft0 = 360.0D0 - betaX

    ftE = + betaX
	
! Towards noon
Else If (NOON) Then

    ft0 = 180.0D0 - betaX
    ftE = 180.0D0 + betaX
	
End If
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
fti = Mangle
If (NOON) Then 
    If (Mangle < 0.0D0)  fti = 360.0D0 + Mangle
End If
! ----------------------------------------------------------------------

! Beta angle prediction at epoch to, te
! - to: Start of the auxiliary region (modified yaw-steering attitude) 
! - te: End of the auxiliary region 
CALL beta_pred (mjd, r_sat, v_sat, fti, ft0, beta_to)
CALL beta_pred (mjd, r_sat, v_sat, fti, ftE, beta_te)

! Soy_to = -sin(beta_to * (PI_global / 180.0D0) )

! Beta sign change test
if (abs(beta_to) >= abs(beta_te) )  Soy_to = -sin(beta_to * (PI_global / 180.0D0) )
if (abs(beta_to) <  abs(beta_te) )  Soy_to = -sin(beta_te * (PI_global / 180.0D0) )

! ----------------------------------------------------------------------
! Gama parameter sign
Gama = sign(1.D0 , Soy_to)
! ----------------------------------------------------------------------

!Print *,"M,ft0,fti,ftE", Mangle, ft0, fti, ftE
!Print *,"beta_to, beta_te, Soy_to, Gama", beta_to, beta_te, Soy_to, Gama 


! ----------------------------------------------------------------------
! Auxiliary Sun reference vector as defined by the GSC 
! ----------------------------------------------------------------------
Sh(1) = So(1) 

Sh(2) = 0.5D0 * ( sin(betaY * (PI_global / 180.0D0)) * Gama + So(2) )  &
      + 0.5D0 * ( sin(betaY * (PI_global / 180.0D0)) * Gama - So(2) )  & 
	  * cos( PI_global * abs(So(1)) / sin(betaX * (PI_global / 180.0D0)) )

Sh(3) = sqrt(1.D0 - So(1)**2 - Sh(2)**2) * SIGN(1.D0,So(3))    
! ----------------------------------------------------------------------

if (1<0) then
print *,"So(2),Sh(2)", So(2), Sh(2)
print *,"So(3),Sh(3)", So(3), Sh(3)
print *,"sin(betaY)", sin(betaY * (PI_global / 180.0D0))
print *,"Shy1", 0.5D0 * ( sin(betaY * (PI_global / 180.0D0)) * Gama + So(2) )
print *,"Shy2", 0.5D0 * ( sin(betaY * (PI_global / 180.0D0)) * Gama - So(2) )
print *,"Shy3", cos( PI_global * abs(So(1)) / sin(betaX * (PI_global / 180.0D0)) )
end if

! ----------------------------------------------------------------------
! Yaw angle (in degrees)
Yangle_ecl = atan2(-Sh(2) / sqrt(1.0D0 - Sh(3)**2) , -Sh(1) / sqrt(1.0D0 - Sh(3)**2) ) * (180.0D0 / PI_global)

! IGS Conventions
IF (satbf == 1) then
Yangle_ecl = atan2(Sh(2) / sqrt(1.0D0 - Sh(3)**2) , Sh(1) / sqrt(1.0D0 - Sh(3)**2) ) * (180.0D0 / PI_global)
END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Eclipse status flag 
! Midnight
   if ( abs(Mangle) <= 90.0D0 ) eclipsf = 1
! Noon
   if ( abs(Mangle) >  90.0D0 ) eclipsf = 2     
! ----------------------------------------------------------------------
   

ELSE

! Eclipse status flag
eclipsf = 0  

! Return Yangle nominal when the eclipse conditions are not fullfilled
Yangle_ecl = Yangle_nom

END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Yaw angle values: Nominal and Modified (eclipsing)
Yangle(1) = Yangle_nom
Yangle(2) = Yangle_ecl
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Body-fixed X-axis in ICRF based on the eclipsing yaw angle 
! ----------------------------------------------------------------------
If (eclipsf > 0) THEN

! Body-fixed X-axis rotation about body-fixed frame Z-axis

! Rotation matrix: Inertial/Orbital frame to Body-fixed frame
!CALL crf_bff (r_sat, v_sat, Yangle_ecl, Rcrf_bff, Rrtn_bff)

delta_Yaw = Yangle_ecl - Yangle_nom

! Rz(delta-yaw) rotation matrix: 
delta_Yaw_rad = delta_Yaw * (PI_global / 180.0D0)	  
Rz_yaw(1,1:3) = (/  cos(delta_Yaw_rad),  sin(delta_Yaw_rad),  0.0D0 /)
Rz_yaw(2,1:3) = (/ -sin(delta_Yaw_rad),  cos(delta_Yaw_rad),  0.0D0 /)
Rz_yaw(3,1:3) = (/        0.0D0,            0.0D0,  			1.0D0 /)

! Apply rotation matrix to the Body-fixed X-axis unit vector
eBX_ecl = MATMUL(Rz_yaw, eBX_nom)

 
Else

eBX_ecl = eBX_nom

End If
! ----------------------------------------------------------------------




END
