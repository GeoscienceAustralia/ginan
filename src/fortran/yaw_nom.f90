SUBROUTINE yaw_nom (e_BX, v_sat, beta, satblk, cosE, ANOON, ANIGHT, Mangle, Yangle)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_nom.f90
! ----------------------------------------------------------------------
! Purpose:
!  Nominal yaw-attitude
!
!  The sign of the nominal yaw angle follows the convections of the eclips.f 
! ----------------------------------------------------------------------
! Input arguments:
! - e_BX: 			Satellite Body-X unit vector (m) 
! - v_sat: 			Satellite velocity vector (m/sec)
! - r_sun:			Sun position vector (m)
! - beta:			Sun angle with the orbital plane in degrees (see Note 1)
! - satblk:			Satellite Block series number 1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6
! - cosE:			Cosine of the angle (E) between the sv radius vector and
!					the sun radius vector (see Note 2)
! - ANOON			Beta angle limit (deg) for noon  (see Note 3)
! - ANIGHT			Beta angle limit (deg) for night (see Note 3)
!
! Output arguments:
! - Yangle :		Yaw angle (in degrees)
! - Mangle : 		Orbit angle (M) between satellite position vector and orbit midnight (in degrees)
! ----------------------------------------------------------------------
! Note 1:
!  Beta angle is computed by the beta_angle.f90 subroutine 
!
! Note 2:
!  cosE (SVBCOS) is computed as the dot product of the two unit vectors 
!  within the attitude_yaw.f90 subroutine 
! 
! Note 3:
!  ANOON and ANIGHT arguments are no longer necessary and should be removed 
!  from the yaw_nom.f90 subroutine
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia             June 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: e_BX(3), v_sat(3), beta, cosE
      REAL (KIND = prec_d), INTENT(IN) :: ANOON, ANIGHT
      INTEGER (KIND = 4), INTENT(IN) :: satblk

! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Yangle, Mangle
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
      INTEGER :: yaw_opt
      REAL (KIND = prec_d) :: beta_rad, Mangle_rad, Mangle_deg, cosM, yaw, Yangle_rad
      REAL (KIND = prec_d) :: phi_sun, lamda_sun, radius_sun, phi_sat, lamda_sat, radius_sat
      REAL (KIND = prec_d) :: PHI, BETADG, SVBCOS, PI, DTR, CNOON, CNIGHT, DET
      REAL (KIND = prec_d) :: YawBias
      LOGICAL NOON, NIGHT
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Yaw angle computation (in degrees)
! ----------------------------------------------------------------------
      svbcos = cosE
      BETADG = beta 
      PI = 3.1415926536D0
      DTR = PI/180.D0

! ----------------------------------------------------------------------
! Yaw angle (deg) as dot product of Body-X unit vector and intertial velocity unit vector   
      yaw = acos( (e_BX(1) * v_sat(1) + e_BX(2) * v_sat(2) + e_BX(3) * v_sat(3)) &
	                  / sqrt(v_sat(1)**2 + v_sat(2)**2 + v_sat(3)**2) ) &
					 * (180D0 / PI_global)

! IIR YANGLE has the same sign as beta, II/IIA has the opposite sign
      if (beta .LT. 0.d0 .AND. satblk .GE. 4 .AND. satblk .LE. 5) then
          yaw = -yaw
      end if
	 
      if (beta .GT. 0.d0 .AND. satblk .NE. 4 .AND. satblk .NE. 5) then
          yaw = -yaw
      end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Towards Night or Noon semicycle
! ----------------------------------------------------------------------
      CNOON  = DCOS(ANOON*DTR)
      CNIGHT = DCOS(ANIGHT*DTR)

	  
! 18 August 2016: IIR night analogous to noon turn has been deactivated
! GPS Block IIR:
!      IF(satblk.EQ.4 .OR. satblk.EQ.5) THEN
!         CNIGHT = DCOS( (ANOON + 180.d0)*DTR )
!      END IF
	  
	  
      NOON=.FALSE.
      NIGHT=.FALSE.

      If (SVBCOS < 0.0D0) Then
        NIGHT=.TRUE.
      Else If (SVBCOS > 0.0D0) then
        NOON=.TRUE.
		
! ----------------------------------------------------------------------
      Else If (SVBCOS == 0.0D0) then
	  ! Special case: E = 90deg      	  
        NOON=.TRUE.
! ----------------------------------------------------------------------
	  
      END IF
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Yaw angle (Phi) and Orbital angle (Mangle)
! ----------------------------------------------------------------------
! init PHI
PHI = PI/2.d0
IF ( (NIGHT .OR. NOON) ) THEN
      DET = SQRT( (180.d0-acos(svbcos)/DTR)**2 - BETADG**2)
      PHI = PI/2.d0
      Mangle = DET
		 
		 
! Towards night
       IF (NIGHT) THEN
! GPS IIR
         IF (satblk.EQ.4 .OR. satblk.EQ.5) THEN
          IF (DABS(yaw).GT.90.d0) DET=-DET
          If (DET.NE.0.d0) Then
			PHI = ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))/DTR
			Mangle = -DET
		  End If
         ELSE
! GPS IIA/IIF & GLONASS
          IF (DABS(yaw).LT.90.d0) DET=-DET
          If (DET.NE.0.d0) Then 
			PHI = ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
			Mangle = -DET
		  End If
         END IF
       END IF 
	   
	   
! Towards noon
       IF (NOON) THEN
         DET = SQRT( (acos(svbcos) * 180.0D0 /pi)**2 - BETADG**2)
! GPS IIR
         IF (satblk.EQ.4 .OR. satblk.EQ.5) THEN
          IF (DABS(yaw).LT.90.d0) DET=-DET
          IF (DET.NE.0.d0) Then 
			PHI = ATAN2(TAN(BETADG*DTR),-SIN(PI-DET*DTR))/DTR
			Mangle = 180.D0 - DET
		  End If			
         ELSE
! GPS IIA/IIF & GLONASS
          IF (DABS(yaw).GT.90.d0) DET=-DET
          If (DET.NE.0.d0) Then 
			PHI = ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
			Mangle = 180.D0 - DET
		  End If						
         END IF
       END IF 
End If	   
! ----------------------------------------------------------------------
      !PRINT *,"DET", DET
      !PRINT *,"PHI", PHI
  
	  
      Yangle = PHI

! ----------------------------------------------------------------------
! Yaw Bias: Applied for IIA only	 
if (satblk == 3) then
    YawBias = 0.5D0
    !YawBias = -0.5D0 ! GPS23 (PRN23 t<=13/02/2004, PRN32 t>=26/02/2008)
	Yangle = Yangle + YawBias
end if
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Set Orbital angle in the range {-180,+180}
Mangle = MOD (Mangle, 360.D0) 

If (ABS(Mangle) > 180.D0) Then
	Mangle = Mangle - 360.D0 * (Mangle / ABS(Mangle) )
End If
! ----------------------------------------------------------------------


  
END
