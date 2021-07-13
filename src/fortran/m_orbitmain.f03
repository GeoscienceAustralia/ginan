MODULE m_orbitmain


! ----------------------------------------------------------------------
! MODULE: m_orbitmain.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for Precise Orbit Determination
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	21 March 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
!SUBROUTINE orbitmain (EQMfname, VEQfname, orb_icrf, orb_itrf, veqSmatrix, veqPmatrix, Vres, Vrms)
SUBROUTINE orbitmain (EQMfname, VEQfname, orb_icrf, orb_itrf, veqSmatrix, veqPmatrix, Vres, Vrms, Xsigma, &
					  dorb_icrf, dorb_RTN, dorb_Kepler, dorb_itrf,orbdiff)

! ----------------------------------------------------------------------
! SUBROUTINE:	orbitmain.f03
! ----------------------------------------------------------------------
! Purpose:
!  Precise Orbit Determination 
! ----------------------------------------------------------------------
! Input arguments:
! - EQMfname: 	Input cofiguration file name for the orbit parameterization 
! - VEQfname: 	Input cofiguration file name for the orbit parameterization 
!
! Output arguments:
! - orb_icrf: 	Satellite orbit array in ICRF including the following per epoch:
!               - Modified Julian Day number (including the fraction of the day) 
!				- Seconds since 00h 
!				- Position vector (m)
!				- Velocity vector (m/sec)
! - orb_itrf: 	Satellite orbit array in ITRF including the following per epoch:
!               - Modified Julian Day number (including the fraction of the day) 
!				- Seconds since 00h 
!				- Position vector (m)
!				- Velocity vector (m/sec)
! - veqSmatrix:	State trasnition matrix obtained from the Variational Equations solution based on numerical integration methods
! - veqPmatrix: Sensitivity matrix obtained from the Variational Equations solution based on numerical integration methods
! - Vres:               Orbit residuals matrix in ICRF
! - Vrms:               RMS (in XYZ) of orbit residuals in ICRF
! - dorb_icrf:          Orbit differences in the intertial frame (position and
! velocity vector differences)
!                               dorb_icrf = [Time_MJD Time_Seconds00h r(X) r(Y)
!                               r(Z) v(X) v(Y) v(Z)] 
! - dorb_itrf:          Orbit differences in the terrestrial frame (position and
! velocity vector differences)
!                               dorb_itrf = [Time_MJD Time_Seconds00h r(X) r(Y)
!                               r(Z) v(X) v(Y) v(Z)] 
! - dorb_RTN:           Orbit differences in the orbital frame (RTN: radial,
! along-track and cross-track differences)
!                               dorb_RTN = [Time_MJD Time_Seconds00h R(X) T(Y)
!                               N(Z) R(vX) T(vY) N(vZ)]
! - dorb_Kepler:        Orbit differences for the Keplerian elements
!                               dorb_Kepler = [Time_MJD Time_Seconds00h a e i
!                               Omega omega E]
!                               a:              semi-major axis  (m)
!                               e:              eccentricity
!                               i:              inclination (degrees)
!                               Omega:  right ascension of the ascending node
!                               (degrees)
!                               omega:  argument of perigee (degrees)
!                               E:              eccentric anomaly (degrees)
! - orbdiff   : [MJD PRN BLOCKTYPE lambda beta(deg) del_u(deg) yaw(deg) ANGX(deg) ANGY(deg) ANGZ(deg) dR(m) dT(m) dN(m) FR(m^2/s) FT(m^2/s) FN(m^2/s)] 
! ----------------------------------------------------------------------
! Note 1:
! The time scale of the 2 first collumns of the orbit arrays (MJD and Seoncds since 00h) 
! refer to the time system defined by the global variable TIME_SCALE in the module mdl_param.f03
! according to the input parameterization file 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	21 March 2019
!
! Changes:      20-05-2019 Tzupang Tseng: output the orbital information for data analysis
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE m_orbdet
      USE m_orbext
      USE m_orbext2
      USE m_writearray
      USE m_writeorbit
      use pod_yaml
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: CPU_t0, CPU_t1
      CHARACTER (LEN=512) :: filename, EQMfname, VEQfname				
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orb_icrf, orb_itrf  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: veqSmatrix, veqPmatrix
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: Vres, Xsigma 
      REAL (KIND = prec_d), DIMENSION(3) :: Vrms 	    
	  REAL (KIND = prec_d), DIMENSION(5,6) :: stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC, stat_XYZ_extT
! ----------------------------------------------------------------------
      CHARACTER (LEN=2) :: GNSS_id
	  INTEGER (KIND = prec_int2) :: ORB_mode
! ----------------------------------------------------------------------
	  INTEGER (KIND = prec_int8) :: Nsat, isat
	  INTEGER (KIND = prec_int8) :: iepoch, iparam
	  INTEGER (KIND = prec_int8) :: i
	  INTEGER (KIND = prec_int8) :: sz1, sz2, Nepochs, N2_orb, N2_veqSmatrix, N2_veqPmatrix, N2sum  
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE :: orbit_veq  
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
	  CHARACTER (LEN=3), ALLOCATABLE :: PRN_array(:)
	  CHARACTER (LEN=3) :: PRN_isat
	  INTEGER :: ios
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: dorb_icrf, dorb_itrf 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: dorb_RTN, dorb_Kepler
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbdiff
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Precise Orbit Determination or Orbit Prediction
CALL orbdet (EQMfname, VEQfname, orb_icrf, orb_itrf, veqSmatrix, veqPmatrix, Vres, Vrms, Xsigma)
if (.not.Allocated(orb_icrf)) then
        print *,"Error from orbdet orb_icrf not allocated"
end if
! ----------------------------------------------------------------------
print *,"Orbit residuals: ICRF" 
WRITE (*,FMT='(A17, A4, 3F14.4)') "RMS-XYZ ICRF FIT", PRN, Vrms
!PRINT *,"Orbit Determination: Completed"
!CALL cpu_time (CPU_t1)
!PRINT *,"CPU Time (sec)", CPU_t1-CPU_t0


If (yml_ext_orbit_opt > TYPE_NONE) Then
! ----------------------------------------------------------------------
! External Orbit Comparison (optional)
Call orbext(EQMfname, orb_icrf, orb_itrf, stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC, stat_XYZ_extT, orbdiff)
!Call orbext (EQMfname, orb_icrf, orb_itrf, stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC, stat_XYZ_extT)
CALL orbext2(EQMfname, orb_icrf, orb_itrf, stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC, stat_XYZ_extT, &
              dorb_icrf, dorb_RTN, dorb_Kepler, dorb_itrf)
! ----------------------------------------------------------------------
PRINT *,"External Orbit comparison"
print *,"Orbit comparison: ICRF"
WRITE (*,FMT='(A17, A4, 3F14.4)') "RMS-RTN ICRF CMP", PRN, stat_RTN_extC(1, 1:3)
WRITE (*,FMT='(A17, A4, 3F14.4)') "RMS-XYZ ICRF CMP", PRN, stat_XYZ_extC(1, 1:3)
!WRITE (*,FMT='(A9, 3F17.9)'),"RMS Vxyz", stat_XYZ_extC(1, 4:6)

print *,"Orbit comparison: ITRF"
WRITE (*,FMT='(A17, A4, 3F14.4)') "RMS-XYZ ITRF CMP", PRN, stat_XYZ_extT(1, 1:3)
!WRITE (*,FMT='(A9, 3F17.9)'),"RMS Vxyz", stat_XYZ_extT(1,4:6)
End If


! ----------------------------------------------------------------------
! Write orbit matrices to output files (ascii)
!PRINT *,"Write orbit matrices to output files"
! ----------------------------------------------------------------------
! Estimated Orbit or Predicted Orbit
write(filename, '(AAA)')  trim(yml_output_dir), "/" ,"orb_icrf.out"
!Call writearray (orb_icrf, filename)
!Call writeorbit (orb_icrf, filename)
write(filename, '(AAA)')  trim(yml_output_dir), "/" ,"orb_itrf.out"
!Call writearray (orb_itrf, filename)
!Call writeorbit (orb_itrf, filename)

! Variational Equations matrices
If (yml_estimator_procedure > 0) then
write(filename, '(AAA)')  trim(yml_output_dir), "/" ,"VEQ_Smatrix.out"
!Call writearray (veqSmatrix, filename)
write(filename, '(AAA)')  trim(yml_output_dir), "/" ,"VEQ_Pmatrix.out"
!Call writearray (veqPmatrix, filename)
End IF

!IF (PRN == 'G02') THEN
!print *,"extorbcomp prn", PRN
write(filename, '(AAA)')  trim(yml_output_dir), "/" ,"dorb_icrf_ext.out"
Call writearray (dorb_icrf, filename)

write(filename, '(AAA)')  trim(yml_output_dir), "/" ,"dorb_rtn_ext.out"
!Call writearray (dorb_RTN, filename)
!END IF
! ----------------------------------------------------------------------



End SUBROUTINE


End MODULE
