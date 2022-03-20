SUBROUTINE yaw_bds (mjd, r_sat, v_sat, r_sun, orbtype, satbf, BetaP, NPint, & 
					beta, beta_t0, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle)

! ----------------------------------------------------------------------
! SUBROUTINE: yaw_bds.f90
! ----------------------------------------------------------------------
! Purpose:
!  BeiDou yaw-attitude law 
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - r_sun:		Sun position vector (m) in ITRF
! - orbtype:	Orbit type
!				orbtype = MEO  
!				orbtype = IGSO
!				orbtype = GEO
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
! - BetaP:		Approach for the beta angle computation at the latest epoch that the orbital angle M is equal to 90 degrees
!				BetaP=1 : Orbit backward prediction based on Keplerian elements  
!				BetaP=2 : Orbit backward computation based on numerical interpolation of the precise orbit data sp3 
! - NPint:		Number of data points to be used for numerical interpolation (defines the order of the polynomial)
!
! Output arguments:
! - beta:		Sun elevation angle with respect to the orbital plane (degrees) 
! - beta_t0:	Beta value at the latest epoch that orbital angle M is equal to 90 degrees
!               Beta prediction backward in time is performed through beta_pred.f90 considering the Keplerian orbit approach
! - eclipsf:	Status flag of attitude mode solution:
!				0 = Yaw-steering mode (Nominal yaw-attitude)
!				3 = Orbit-normal mode (Yaw fixed to opposite of the beta angle) 
! - eBX_nom:	Body-X axis unit vector during yaw-steering mode
! - eBX_ecl:	Body-X axis unit vector during orbit-normal mode
! - Yangle: 	Yaw angle array (in degrees):
!				Yangle(1) = Nominal Yaw angle during the yaw-steering mode
!				Yangle(2) = Yaw fixed to -beta during the orbit-normal mode
! - Mangle:		Orbit angle between satellite and orbit midnight (degrees)
! ----------------------------------------------------------------------
! Note1:
!  eBX_ecl is set equal to eBX_nom during the yaw-steering mode
!
! Note2:
!  Yangle(2) is set equal to Yangle(1) during the yaw-steering mode 
! ----------------------------------------------------------------------
! Authors:	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
!			Dr. Tzupang Tseng, Cooperative Research Centre for Spatial Information, Australia
! Created:	20 June 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd, r_sat(3), v_sat(3), r_sun(3) 
      CHARACTER (LEN=5), INTENT(IN) :: orbtype
      INTEGER (KIND = 4)   :: satbf 
      INTEGER (KIND = prec_int2) :: BetaP
      INTEGER (KIND = prec_int8), INTENT(IN) :: NPint

	  
! OUT
      REAL (KIND = prec_d) :: Yangle(2)
	  REAL (KIND = prec_d) :: eBX_nom(3), eBX_ecl(3)
      REAL (KIND = prec_d) :: beta, beta_t0, Mangle
      INTEGER (KIND = 4)   :: eclipsf
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), Xsun(3)
      REAL (KIND = prec_d) :: pcross(3), pcross2(3), pdot
      REAL (KIND = prec_d) :: SVBCOS
      REAL (KIND = prec_d) :: beta0, dbeta, beta_entry, beta_exit
      REAL (KIND = prec_d) :: ft0, fti
	  REAL (KIND = prec_d) :: Yangle_nom, Yangle_on
      INTEGER (KIND = 4)   :: satblk
	  LOGICAL              :: ON, YS, ON_Entry, ON_Exit
      REAL (KIND = prec_d) :: delta_Yaw, delta_Yaw_rad
      REAL (KIND = prec_d) :: Rz_yaw(3,3)
! ----------------------------------------------------------------------
!       REAL (KIND = prec_d) :: mjdtest

! ANOON,ANIGHT variables to be removed
      REAL (KIND = prec_d) :: ANOON, ANIGHT

	
 


! ----------------------------------------------------------------------
! Beta angle (in degrees) at the current epoch (input)
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
! Yaw steering mode 
! ----------------------------------------------------------------------
! Body-frame definition type
if (satbf == 1) then
   satblk = 2 
elseif (satbf == 2) then
   satblk = 5 
Else
   Print *,"Error in input argument 'satbf': BeiDou Body-frame type"
end if


! Nominal yaw angle (in degrees)
! Mangle: Orbital angle between satellite position vector and orbit midnight (in degrees)
! Arguments to be removed from the yaw_nom.f90 subroutine
ANOON  = 0.0D0
ANIGHT = 0.0D0

Call yaw_nom (eBX_nom, v_sat, beta, satblk, SVBCOS, ANOON, ANIGHT, Mangle, Yangle_nom)
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! BDS attitude regimes test: Orbit-Normal (ON) mode or Yaw-Steering (YS)
! ----------------------------------------------------------------------
! Orbit-Normal (ON) and Yaw-steering (YS) modes logical values initialization 
ON = .FALSE.
YS = .FALSE.


! The attitude regimes test is applied for beta angle within the range {-5,+5}
beta0 = 5.D0


If (abs(beta) > beta0) Then

! Yaw-steering mode
YS = .TRUE.


Else if (abs(beta) <= beta0) Then

! ----------------------------------------------------------------------
! Test the beta angle at the latest epoch that the orbital angle is equal to 90deg
! ----------------------------------------------------------------------
! t0: latest epoch that Mangle=90
ft0 = 90.0D0

! Orbit angle (deg) at the current epoch ti
fti = Mangle

! Reduction if Mangle<90
if (fti < ft0) then
	fti = fti + 360.0D0	
end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Compute the beta angle at the epochs t0 (orbit points that Mangle=90)
! ----------------------------------------------------------------------
If (BetaP == 1) Then

Call beta_pred (mjd, r_sat, v_sat, fti, ft0, beta_t0)

Else If (BetaP == 2) Then
 
!Call beta_interp (mjd, r_sat, v_sat, fti, ft0, NPint, beta_t0)

End If 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Entry and Exit orbit phases of the Orbit-Normal mode
! ----------------------------------------------------------------------
! ON_Entry : orbit phase of entering the Orbit-normal mode (entry point to the middle of the mode)
! ON_Exit  : orbit phase of exiting the Orbit-normal mode (middle to the exit point of the mode)   

! Initialization: set to .FALSE.
ON_Entry = .FALSE.   
ON_Exit  = .FALSE.  

! Compute the difference dbeta = β(ti) - β(t0)
dbeta = beta - beta_t0

! Cases scenarios
If (dbeta>0  .and. beta_t0<0)    ON_Entry =.TRUE.
If (dbeta<=0 .and. beta_t0<0)    ON_Exit  =.TRUE.
If (dbeta>=0 .and. beta_t0>0)    ON_Exit  =.TRUE.
If (dbeta<0  .and. beta_t0>0)    ON_Entry =.TRUE.
! Identical case
If (beta_t0 == 0)                ON_Entry =.TRUE.
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Detect the critical orbital periods that satellite is entering/exiting the orbit normal mode
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Conditions thresholds (in degrees) for entry and exit orbital periods
! Controler empirical values
beta_entry = 4.10D0
beta_exit  = 3.90D0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Entry orbital period
If (ON_Entry) Then
   If ( abs(beta_t0) < beta_entry ) Then
       ON = .TRUE.
   Else
       YS = .TRUE.
   END IF
END IF    
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Exit orbital period
If (ON_Exit) Then
   If ( abs(beta_t0) > beta_exit ) Then
       YS = .TRUE.
   Else
       ON = .TRUE.
   END IF
END IF    
! ----------------------------------------------------------------------

END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Init Yangle_on
Yangle_on = 0.d0

! ----------------------------------------------------------------------
! Yaw angle
! ----------------------------------------------------------------------
If (YS) Then

! Yaw angle is equal to the Nominal Yaw angle 
Yangle_on = Yangle_nom

ELSE IF (ON) Then

! Yaw fixed to opposite of the beta angle value at the current epoch
Yangle_on = -1.D0 * beta
  
END IF  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Output: Yaw angle
Yangle(1) = Yangle_nom
Yangle(2) = Yangle_on
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Status flag for attitude mode (YS or ON)
! ----------------------------------------------------------------------
If (YS) Then

eclipsf = 0

Else IF (ON) Then

eclipsf = 3

End If
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Body-fixed X-axis in ICRF based on the eclipsing yaw angle 
! ----------------------------------------------------------------------
If (eclipsf > 0) THEN

! Body-fixed X-axis rotation about body-fixed frame Z-axis

! Rotation matrix: Inertial/Orbital frame to Body-fixed frame
!CALL crf_bff (r_sat, v_sat, Yangle_on, Rcrf_bff, Rrtn_bff)

delta_Yaw = Yangle_on - Yangle_nom

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

!PRINT *, "eclipsf  ", eclipsf
!PRINT *, "Yangle(1)", Yangle(1)
!PRINT *, "Yangle(2)", Yangle(2)
!PRINT *, "Mangle", Mangle
!PRINT *, "beta0", beta0
!PRINT *, "beta", beta
!PRINT *, "beta_t0", beta_t0

end if


if (1<0) then

print *,"ON_Entry", ON_Entry
print *,"ON_Exit", ON_Exit
print *,"ON", ON
print *,"YS", YS
print *,"beta_entry", beta_entry

end if


END
