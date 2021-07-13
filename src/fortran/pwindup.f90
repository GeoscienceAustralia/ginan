SUBROUTINE pwindup (r_sat, eBodyX, r_rec, dphi)


! ----------------------------------------------------------------------
! SUBROUTINE: pwindup.f90
! ----------------------------------------------------------------------
! Purpose:
!  Phase wind-up effect
! ----------------------------------------------------------------------
! Input arguments:
! - r_sat: 			Satellite geocentric position vector (m) 
! - eBF:			Satellite position unit vector in Body-Fixed frame
! - r_rec: 			Station (receiver) geocentric position vector (m) 
!
! Output arguments:
! - dphi:			Phase wind-up correction (degrees) 
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia        10 August 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: r_sat(3), eBodyX(3), r_rec(3)
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: dphi
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
	  REAL (KIND = prec_d) :: Rrec_ENU(3), e_ENU(3)
	  REAL (KIND = prec_d) :: r_satrec(3), e_satrec(3)
	  REAL (KIND = prec_d) :: e_rSat(3)
	  REAL (KIND = prec_d) :: eBx(3), eBy(3), eBz(3)
	  REAL (KIND = prec_d), Dimension(3) :: eE, eN, eU
	  REAL (KIND = prec_d), Dimension(3) :: Dsat, Drec
	  REAL (KIND = prec_d) :: pdot, pcross(3)
	  REAL (KIND = prec_d) :: zeta, Dsat_norm, Drec_norm, cos_dphi, dphi_rad
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Satellite position unit vector
      e_rSat = (1.D0 / sqrt(r_sat(1)**2 + r_sat(2)**2 + r_sat(3)**2) ) * r_sat
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Satellite Body axes unit vectors ()	  
	  !Body-X unit vector : input argument (computed by yaw_attitude.f90)
	  eBx = eBodyX
	  
	  ! Body-Z unit vector
	  eBz = - e_rSat

	  ! Body-Y unit vector
      CALL productcross(eBz,eBx , eBy)
	  !eBy = cp  
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Station (receiver) position vector and unit vectors in local ENU system (East, North, Up) 
CALL  ENU(r_rec, Rrec_ENU, eE, eN, eU)	        
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Satellite to station (receiver) unit vector
	  r_satrec = - r_sat + r_rec
! Unit vector (k)
      e_satrec = (1.D0 / sqrt(r_satrec(1)**2 + r_satrec(2)**2 + r_satrec(3)**2) ) * r_satrec
! ----------------------------------------------------------------------
 
 
! ----------------------------------------------------------------------
! Satellite dipole unit vector 
! Dsat = eBF_x - e_satrec * ( dot_k_eXsat ) - cross_k_eYsat
      CALL productdot  (e_satrec, eBx, pdot)
	  CALL productcross(e_satrec, eBy, pcross) 
      Dsat = eBx - e_satrec * pdot - pcross

! Receiver dipole unit vector 
! Drec = eN - e_satrec * ( dot_k_eN ) - cross_k_eE
      CALL productdot  (e_satrec, eN, pdot)
	  CALL productcross(e_satrec, eE, pcross) 
      Drec = eN - e_satrec * pdot + pcross
! ----------------------------------------------------------------------
 
 
! ----------------------------------------------------------------------
! Zeta 
	  CALL productcross(Dsat, Drec, pcross) 
      CALL productdot  (e_satrec, pcross, pdot)
      zeta = pdot
! ----------------------------------------------------------------------
 

! ----------------------------------------------------------------------
! Phase wind-up correction (dphi)
      CALL productdot  (Dsat, Drec, pdot)
	  Dsat_norm = sqrt( Dsat(1)**2 + Dsat(2)**2 + Dsat(3)**2 )
	  Drec_norm = sqrt( Drec(1)**2 + Drec(2)**2 + Drec(3)**2 )
	  
      cos_dphi = pdot / ( Dsat_norm * Drec_norm )
 
      If (zeta > 0) then
	     dphi_rad = acos(cos_dphi)
	  else if (zeta < 0) then
	     dphi_rad = -1.0D0 * acos(cos_dphi)
      End If  

! Output in radians	  
!dphi = dphi_rad
	  
! convert in degrees 
      dphi = dphi_rad * (180.D0 / PI_global )
! ----------------------------------------------------------------------
 



End subroutine

