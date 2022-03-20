SUBROUTINE eclipse_integstep (EQMfname, VEQfname, mjd, r_sat, v_sat, integstep_flag, integstep_initial, integstep_reduced)


! ----------------------------------------------------------------------
! SUBROUTINE: eclipse_integstep.f03
! ----------------------------------------------------------------------
! Purpose:
!  Reduce the step of the numerical integration method applied for orbit 
!  determination during eclipse seasons 
! ----------------------------------------------------------------------
! Input arguments:
! - EQMfname: 	Input configuration file name for the orbit parameterization 
! - VEQfname: 	Input configuration file name for the orbit parameterization 
! - mjd:		Modified Julian Day (MJD) in Terrestrial Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ICRF
! - v_sat: 		Satellite velocity vector (m/sec) in ICRF
!
! Output arguments:
! - integstep_flag: 	Flag regarding the change of the orbit integration step 
! - integstep_initial:	Initial value of the orbit integration method 
! - integstep_reduced:	Reduced value of the orbit integration method 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia, Frontier-SI
! Created:	11 June 2019
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      use pod_yaml
      use mdl_config
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN)  :: EQMfname, VEQfname				
      REAL (KIND = prec_d), INTENT(IN) :: mjd, r_sat(3), v_sat(3)!, r_sun(3)
! ----------------------------------------------------------------------
! OUT
      LOGICAL, INTENT(OUT) :: integstep_flag
      REAL (KIND = prec_d), INTENT(OUT) :: integstep_initial, integstep_reduced
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: rbody(3)
      REAL (KIND = prec_d) :: r_sun(3)
      REAL (KIND = prec_d) :: beta
      real (kind = prec_d) :: frac
      DOUBLE PRECISION  JD, Zbody(6)
      INTEGER  NTARG, NCTR, NTARG_body, i, j
      CHARACTER (LEN=50) :: fname_id				
      CHARACTER (LEN=100) :: param_id				
      CHARACTER (LEN=500) :: param_value				
      logical found
      integer  reductions(6), integstep_int, integstep_int2
      character (len=128) :: line
! ----------------------------------------------------------------------


integstep_flag = .FALSE.
found = .false.

reductions(1) = 100
reductions(2) = 75
reductions(3) = 50
reductions(4) = 25
reductions(5) = 10
reductions(6) = 5

! ----------------------------------------------------------------------
! Julian Day Number of the input epoch
JD = mjd + 2400000.5D0
! Center celestial body: Earth
NCTR = 3 
! Sun (NTARG) Cartesian coordinates w.r.t. Earth (NCTR)
NTARG = 11
CALL  PLEPH ( JD, NTARG, NCTR, Zbody )
! Cartesian coordinates of the celestial body in meters: KM to M
rbody(1) = Zbody(1) * 1000.D0
rbody(2) = Zbody(2) * 1000.D0
rbody(3) = Zbody(3) * 1000.D0
r_sun = rbody
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Beta angle (degrees)
!print *, "r_sat=", r_sat
!print *, "v_sat=", v_sat
!print *, "r_sun=", r_sun

do i=1,prn_override_count
    if (yml_prn_overrides(i)%name == TRIM(PRN)) then
         found = .true.
         exit
    end if
end do

if (.not. found) then
    i = new_prn_override(PRN)
end if
CALL beta_angle (r_sat, v_sat, r_sun, beta)
! ----------------------------------------------------------------------
write(line, *) 'day of year',YR,DOY,PRN,'  ',"beta", beta
yml_prn_overrides(i)%integ%integ_header = trim(yml_prn_overrides(i)%integ%integ_header) // NEW_LINE('A') // line

! ----------------------------------------------------------------------
! criteria for changing orbit integration stepsize
IF ( abs(beta) <= 14) THEN
	integstep_initial = integstep
        integstep_int = INT(integstep)
IF (integstep_initial >= 100.D0) THEN
        do j = 1,6
	integstep_reduced = 1.0d0 * reductions(j)
        integstep_int2 = integstep_int/reductions(j)
        integstep_int2 = integstep_int2*reductions(j)
        if (integstep_int == integstep_int2) then
	    integstep_flag = .TRUE.
            write (line,*) 'eclipsing satellite ', PRN, ', reduced stepsize of ', reductions(j), ' chosen'
            yml_prn_overrides(i)%integ%integ_header = trim(yml_prn_overrides(i)%integ%integ_header) &
                    // NEW_LINE('A') // line
            exit
        end if
        end do
END IF

if (integstep_flag .and. .not. yaml_found) then
write (fname_id, *) '_INT'
param_id = 'integrator_step'
write (param_value, *) integstep_reduced
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
end if
END IF 

if (integstep_flag) then
    yml_prn_overrides(i)%integ%veq_enabled = .true.
    yml_prn_overrides(i)%integ%eqm_enabled = .true.
    yml_prn_overrides(i)%integ%veq_stepsize = integstep_reduced
    yml_prn_overrides(i)%integ%eqm_stepsize = integstep_reduced
else
    yml_prn_overrides(i)%integ%veq_enabled = .false.
    yml_prn_overrides(i)%integ%eqm_enabled = .false.
end if

! ----------------------------------------------------------------------

END SUBROUTINE
