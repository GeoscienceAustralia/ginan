SUBROUTINE yawdyn (mjd, r_sat, v_sat, r_sun, satbf, beta0, dparam, beta, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle)


! ----------------------------------------------------------------------
! SUBROUTINE: yawdyn.f90
! ----------------------------------------------------------------------
! Purpose:
!  Dynamic yaw steering method based on the patent:
!  "Ebert K., Oesterlin W., 2008, Dynamic yaw steering method for spacecraft; European patent specification EP 1526072B1."
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - r_sun:		Sun position vector (m) in ITRF
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
! - beta0:		Limit of the beta angle (Sun elevation angle w.r.t. orbital plane) in degrees
! 				Dynamic yaw sterring is applied when |beta|<= beta0 
! - dparam:		Design parameter required in the smoothing function of the dynamic yaw steering
! 				Design parameter's optimum value has been found as 258 by Ebert and Oesterlin (2008)
!
! Output arguments:
! - beta:		Sun elevation angle with respect to the orbital plane (degrees) 
! - eclipsf:	Status flag of yaw steering solution:
!				0 = Kinematic Yaw steering solution, Nominal yaw-attitude
!				1 = Dynamic Yaw steering solution, Midnight
!				2 = Non-nominal, noon
! - eBX_nom:	Body-X axis unit vector during noninal yaw-attitude (Kinematic yaw-steering)
! - eBX_ecl:	Body-X axis unit vector during eclipsng yaw-attitude (Dyanmic yaw-steering)
! - Yangle: 	Yaw angle array (in degrees):
!				Yangle(1) = Kinematic Yaw steering (nominal)
!				Yangle(2) = Dynamic Yaw steering (for |beta|<=beta0)
! - Mangle:		Orbit angle between satellite and orbit midnight (degrees)
! ----------------------------------------------------------------------
! Note1:
!  eBX_ecl is set equal to eBX_nom when |beta|>beta0 
!
! Note2:
!  Yangle(2) is set equal to Yangle(1) when |beta|>beta0 
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia           6 June 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd, r_sat(3), v_sat(3), r_sun(3), beta0 
      INTEGER (KIND = prec_int8) :: dparam
      INTEGER (KIND = 4)   :: satbf
	  
! OUT
      REAL (KIND = prec_d) :: Yangle(2)
	  REAL (KIND = prec_d) :: eBX_nom(3), eBX_ecl(3)
      REAL (KIND = prec_d) :: beta, Mangle
      INTEGER (KIND = 4)   :: eclipsf
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), Xsun(3)
      REAL (KIND = prec_d) :: pcross(3), pcross2(3), pdot
      REAL (KIND = prec_d) :: SVBCOS
      REAL (KIND = prec_d) :: eta, fsmooth, ft1, ft2, fti, beta1, beta2, delta, betaD
	  REAL (KIND = prec_d) :: Yangle_nom, Yangle_ecl
	  REAL (KIND = prec_d) :: Yangle_kin, Yangle_dyn
      INTEGER (KIND = 4)   :: satblk
      REAL (KIND = prec_d) :: delta_Yaw, delta_Yaw_rad
      REAL (KIND = prec_d) :: Rz_yaw(3,3)
! ----------------------------------------------------------------------
! ANOON,ANIGHT variables to be removed
      REAL (KIND = prec_d) :: ANOON, ANIGHT
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Beta angle (in degrees)
      CALL beta_angle (r_sat, v_sat, r_sun, beta)
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
! E:	Angle between the satellite position vector and the sun position vector
! cos(E) is computed as the dot product between the unit vectors of those position vectors 
! ----------------------------------------------------------------------
! SVBCOS:	Cosine of the angle E
      CALL productdot (Xsat, Xsun , pdot)
      SVBCOS = pdot
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Body-frame X-axis unit vector (IGS Conventions)
! ----------------------------------------------------------------------
! eBX_nom = - ( (Xsun x Xsat) x Xsat )  							
      CALL productcross (Xsun,Xsat , pcross)
      CALL productcross (pcross,Xsat , pcross2 )
      eBX_nom = -1.0D0 * pcross2	  
  
! Body-X unit vector points opposite to the Sun; Cases: Galileo and GPS IIR
if (satbf == 2) then
      eBX_nom = -1.0D0 * eBX_nom
end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Satellite orbital angle (mi and eta)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Arguments to be removed from the yaw_nom.f90 subroutine
ANOON  = 0.0D0
ANIGHT = 0.0D0
! ----------------------------------------------------------------------

! Body-frame definition type
if (satbf == 1) then
   satblk = 2 
elseif (satbf == 2) then
   satblk = 5 
end if


! Mangle: Orbital angle between satellite position vector and orbit midnight (in degrees)
Call yaw_nom (eBX_nom, v_sat, beta, satblk, SVBCOS, ANOON, ANIGHT, Mangle, Yangle_nom)


! eta: Orbital angle between satellite position vector and orbit noon (in degrees)
eta = Mangle - 180.0D0

! Set Orbital angle in the range {-180,+180}
eta = MOD (eta, 360.D0) 

If (ABS(eta) > 180.D0) Then
	eta = eta - 360.D0 * (eta / ABS(eta) )
End If
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Kinematic yaw steering
! ----------------------------------------------------------------------
! Nominal yaw angle (in degrees)
Yangle_kin = 0.d0 ! dummy initialise

if (satbf == 1) then
! Body-fixed frame following the IGS Conventions
Yangle_kin = ATAN2( -tan(beta * (PI_global/180.0D0)) , -sin(eta * (PI_global/180.0D0)) ) * (180.0D0 / PI_global)

elseif (satbf == 2) then
! Body-fixed frame following reversed X,Y axes of body-fixed frame w.r.t. IGS Conventions
Yangle_kin = ATAN2( tan(beta * (PI_global/180.0D0)) , sin(eta * (PI_global/180.0D0)) ) * (180.0D0 / PI_global)

end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Dynamic yaw steering
! ----------------------------------------------------------------------
If (abs(beta) <= beta0) Then

! Smoothing function
fsmooth = (cos(eta * (PI_global/180.0D0)))**2 / (1.0D0 + 1.0D0* dparam * (sin(eta * (PI_global/180.0D0)))**2 )


! ----------------------------------------------------------------------
! Delta parameter sign; Delta=+1 or -1
! ----------------------------------------------------------------------
! Test the beta sign at the folllowing two epochs
! - t1: The latest epoch with 'eta' angle equal to 90 degrees 
! - t2: The epoch t2=t1+360deg; approximately after one orbital period since t1 
ft1 = 90.0D0
ft2 = 90.0D0 + 360.0D0


! Current orbit angle (deg)
fti = eta

! Reduction if eta<90
if (fti < ft1) then
	fti = fti + 360.0D0	
end if




! Compute beta angle at t1 and t2
call beta_pred (mjd, r_sat, v_sat, fti, ft1, beta1)
call beta_pred (mjd, r_sat, v_sat, fti, ft2, beta2)

If (beta1 * beta2 > 0.0D0) Then
   ! No sign change of beta
   delta = sign(1.D0,beta1)
Else
   ! Beta sign change
   if (abs(beta1) >= abs(beta2) )  delta = sign(1.D0,beta1)
   if (abs(beta1) <  abs(beta2) )  delta = sign(1.D0,beta2)
End If
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Smoothed beta angle
betaD = beta + fsmooth * (beta0 * delta - beta)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Yaw angle (in degrees) obtained from dynamic yaw steering solution
Yangle_dyn = 0.d0 ! dummy initialise

if (satbf == 1) then
! Body-fixed frame following the IGS Conventions
Yangle_dyn = ATAN2( -tan(betaD * (PI_global/180.0D0)) , -sin(eta * (PI_global/180.0D0)) ) * (180.0D0 / PI_global)

elseif (satbf == 2) then
! Body-fixed frame following reversed X,Y axes of body-fixed frame w.r.t. IGS Conventions
Yangle_dyn = ATAN2( tan(betaD * (PI_global/180.0D0)) , sin(eta * (PI_global/180.0D0)) ) * (180.0D0 / PI_global)

end if
! ----------------------------------------------------------------------


! Status flag: Dynamic yaw steering solution
! Midnight
   if ( abs(Mangle) <= 90.0D0 ) eclipsf = 1
! Noon
   if ( abs(Mangle) >  90.0D0 ) eclipsf = 2  
   
else

! Status flag: Kinematic yaw steering
eclipsf = 0
Yangle_dyn = Yangle_kin

End If
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Output: Yaw angle
Yangle(1) = Yangle_kin
Yangle(2) = Yangle_dyn
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! init Yangle_ecl
Yangle_ecl = 0.d0

! ----------------------------------------------------------------------
! Body-fixed X-axis in ICRF based on the eclipsing yaw angle 
! ----------------------------------------------------------------------
If (eclipsf > 0) THEN

! Body-fixed X-axis rotation about body-fixed frame Z-axis

! Rotation matrix: Inertial/Orbital frame to Body-fixed frame
!CALL crf_bff (r_sat, v_sat, Yangle_on, Rcrf_bff, Rrtn_bff)

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



if (1>0) then
PRINT *, "Yangle_nom", Yangle_nom
PRINT *, "Yangle_kin", Yangle_kin
PRINT *, "Yangle_dyn", Yangle_dyn
PRINT *, "Mangle", Mangle
PRINT *, "eta   ", eta
PRINT *, "beta0", beta0
PRINT *, "beta", beta
PRINT *, "betaD", betaD
PRINT *, "fsmooth", fsmooth
PRINT *, "delta", delta
PRINT *, "beta1", beta1
PRINT *, "beta2", beta2
end if



END
