SUBROUTINE force_sum (mjd, t_sec, rsat, vsat, SFx, SFy, SFz, integr_stage)


! ----------------------------------------------------------------------
! SUBROUTINE: force_sum.f03
! ----------------------------------------------------------------------
! Purpose:
!  Satellite acceleration components based on the considered force model 
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:            Modified Julian Day number (including the fraction of the day) in Terrestrial Time (TT)
! - t_sec:          Seconds since the start of the day 00h 
! - rsat:           Satellite Position vector (m)   in inertial frame (ICRF)
! - vsat:           Satellite Velocity vector (m/s) in inertial frame (ICRF)
! 
! Output arguments:
! - fx,fy,fz:       Acceleration's cartesian components (m)
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  9 October 2017
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 3 October 2018:
!   Empirical forces of bias & cycle per revolution accelerations have been added 
!
! Changes: 03-12-2018 Dr. Tzupang Tseng: Added the models of solar radiation
!                                        pressure, earth radiation pressure 
!                                        and antenna thrust
!          02-05-2019 Dr. Tzupang Tseng: added shadow model (shadow.f90) for scaling the SRP effect
!
! - Dr. Thomas Papanikolaou, 25 March 2019:
!      The satellite attitude models have now been integrated into POD through
!   the "attitude.f03 subroutine" obtained from the conversion of the 
!   "GNSS yaw-attitude program" to subroutine 
! - Last modified:  17 August 2020, Dr. Thomas Papanikolaou, Velocity pulses added  
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_eop
      USE mdl_planets
      USE mdl_tides
      USE m_force_gfm
      USE m_force_tides
      USE m_tides_ocean
      USE m_pd_empirical
      USE m_writedata
      USE m_shadow
      USE mdl_config
      use pod_yaml
      USE m_pd_pulses
      USE m_pulses_force
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd, t_sec
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: rsat, vsat
      INTEGER (KIND = prec_int4), INTENT(IN) :: integr_stage
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: SFx, SFy, SFz

! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: lambda
      CHARACTER (KIND = 1) :: ECLTYP
      
! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: rsat_icrf, vsat_icrf, rsat_itrf, vsat_itrf, v_TRS_1, v_TRS_2
      DOUBLE PRECISION EOP_cr(7)
      DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)     
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: SF, SFgrav, SFnongrav, SFemp
      REAL (KIND = prec_q), DIMENSION(3) :: Fgrav_itrf , Fgrav_icrf, Fplanets_icrf, Ftides_icrf, Frelativity_icrf
      REAL (KIND = prec_d), DIMENSION(3) :: Fsrp_icrf
      REAL (KIND = prec_d), DIMENSION(3) :: Ferp_icrf
      REAL (KIND = prec_d), DIMENSION(3) :: Fant_icrf
      REAL (KIND = prec_d) :: fx, fy, fz
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: GMearth, aEarth
      INTEGER (KIND = prec_int8) :: n_max, m_max
!       INTEGER(KIND = 4)          :: satsvn
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: aPlanets_icrf, a_perturb, a_iJ2, a_iJ2_icrf
      DOUBLE PRECISION  JD, Zbody(6)
      INTEGER  NTARG, NCTR, NTARG_body
      REAL (KIND = prec_q), DIMENSION(3) :: rbody
      REAL (KIND = prec_q) :: GMbody
      REAL (KIND = prec_q), DIMENSION(3) :: rSun, rMoon, rMoon_ITRS, rSun_ITRS      
      REAL (KIND = prec_q) :: GM_moon, GM_sun
      REAL (KIND = prec_q) :: C20, Re
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: ut1_utc
      REAL (KIND = prec_d) :: xp, yp
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(5,5) :: dCnm_solid1, dSnm_solid1
      REAL (KIND = prec_q), DIMENSION(3,3) :: dCnm_solid2, dSnm_solid2  
      REAL (KIND = prec_q) :: dC20_perm
      REAL (KIND = prec_q) :: dC21_pse, dS21_pse, dC21_poc, dS21_poc
      INTEGER (KIND = prec_int8) :: sz_tides  
      INTEGER (KIND = prec_int8) :: i , j 
      REAL (KIND = prec_q) :: ax, ay, az
      REAL (KIND = prec_q), DIMENSION(3) :: a_tides, a_solidtides, a_ocean 
!       REAL (KIND = prec_q), DIMENSION(3) :: a_solid1, a_solid2, a_pse, a_poc
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCnm_tides, dSnm_tides       
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCnm_ocean, dSnm_ocean       
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(6) :: Zsat_GCRS, Zearth_GCRS
      REAL (KIND = prec_q), DIMENSION(3) :: vSun
      REAL (KIND = prec_q), DIMENSION(3) :: a_Schwarzschild, a_LenseThirring, a_deSitter
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: AllocateStatus
!       , DeAllocateStatus
      CHARACTER (LEN=30) :: fmt_line
      INTEGER (KIND = prec_int2) :: ios
      CHARACTER (LEN=1) :: GNSSid
      INTEGER (KIND = prec_int4) :: PRN_no
      INTEGER ::  srpid
!       eclpf,
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: beta_ppn, gama_ppn
      REAL (KIND = prec_q) :: c_light
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: PD_EMP_r(3,3), PD_EMP_v(3,3)
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: PD_EMP_param
! ----------------------------------------------------------------------
!       REAL (KIND = prec_q) :: phi,radius
!       ,lamda
!       REAL (KIND = prec_q), DIMENSION(3) :: a_solidtides_icrf, a_ocean_icrf 
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: filename
      CHARACTER (LEN=1000) :: writeline
! ----------------------------------------------------------------------
      CHARACTER (LEN=3)  :: PRN_GNSS
      INTEGER (KIND = 4) :: satblk
      CHARACTER (LEN=5)  :: BDSorbtype
      INTEGER (KIND = 4) :: eclipsf
      REAL (KIND = prec_d) :: beta, Mangle, Yangle(2)
      REAL (KIND = prec_d) , Dimension(3) :: eBX_nom, eBX_ecl
      INTEGER (KIND = prec_int2) :: Frame_EmpiricalForces
      REAL (KIND = prec_d) :: Yawangle
! ----------------------------------------------------------------------
      CHARACTER (LEN=20) :: BLKsat
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: SFpulses
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: PD_pulses_param

!       REAL (KIND = prec_d) :: Fpulse (3)
      REAL (KIND = prec_d) :: PD_pulse_r(3,3), PD_pulse_v(3,3)
      !REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: PD_pulse_param
!       REAL (KIND = prec_d), DIMENSION(3,1) :: PD_pulse_param
!       INTEGER (KIND = prec_int8) :: PULSE_param, N_PULSE_param
!       INTEGER (KIND = prec_int2) :: dir_pulse
!       REAL (KIND = prec_d), DIMENSION(3) :: delta_v
!       REAL (KIND = prec_d) :: mjd_ti_pulse
!       , ti_sec_pulse
!       INTEGER (KIND = prec_int8) :: i1, i2, i_dir
!       INTEGER (KIND = prec_int8) :: N_PULSE_epochs
! ----------------------------------------------------------------------

    
! ----------------------------------------------------------------------
! Global variables used
! ----------------------------------------------------------------------
! Module mdl_param.f03:
! FMOD_GRAV, FMOD_NONGRAV
! GFM_Cnm, GFM_Snm
! GFM_Nmax, GFM_Mmax
! OCEAN_Nmax, OCEAN_Mmax
GMearth = GFM_GM
aEarth = GFM_ae
! ----------------------------------------------------------------------
! Module mdl_num.f90:
! Relativistic parameters
beta_ppn = ppn_beta
gama_ppn = ppn_gama
c_light = cslight
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
If (FMOD_GRAVFIELD == 0) THEN
! Module mdl_num
GMearth = GM_global
aEarth = Earth_radius
END IF
! ----------------------------------------------------------------------

! State Vector in ICRF
rsat_icrf = rsat
vsat_icrf = vsat
rsat_itrf = 0.d0

! Init var
vSun = 0.d0

! ----------------------------------------------------------------------
! EOP data and Tranformation matrices between ITRF and ICRF
! ----------------------------------------------------------------------
If (FMOD_GRAVFIELD > 0 .OR. yml_planetary_perturbations_enabled .OR. yml_tidal_effects_enabled .OR. yml_rel_effects_enabled ) Then

CALL EOP (mjd, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)    

! EOP data corrected at computation epoch 
! Polar motion coordinates
xp = EOP_cr(2) 
yp = EOP_cr(3)
! UT1-UTC
ut1_utc = EOP_cr(4) 

! State Vector in ITRF
! r in ITRF
CALL matrix_Rr (CRS2TRS, rsat_icrf , rsat_itrf)
! v in ITRF 
! v_TRS = TRS2CRS * v_CRS + d_CRS2TRS * r_CRS
CALL matrix_Rr (CRS2TRS, vsat_icrf , v_TRS_1)
CALL matrix_Rr (d_CRS2TRS, rsat_icrf , v_TRS_2)
vsat_itrf = v_TRS_1 + v_TRS_2

End IF 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Gravitational Effects
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Gravity Field
Fgrav_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
Fgrav_itrf = (/ 0.D0, 0.0D0, 0.0D0 /)
If (.not. yml_gravity_enabled) then

! Setting to Zero
fx = 0.0D0
fy = 0.0D0
fz = 0.0D0    
Fgrav_icrf = (/ fx, fy, fz /)

Else      

If (FMOD_GRAVFIELD == 0) Then

! Central term of the Earth Gravity Field 
!Call force_gm (rsat_icrf, fx, fy, fz)
Call force_gm (GMearth, rsat_icrf, fx,fy,fz )
Fgrav_icrf = (/ fx, fy, fz /)

Else 

! Earth Gravity Field model acceleration components (spherical harmonics expansion) 
n_max = GFM_Nmax
m_max = GFM_Mmax
CALL force_gfm (GMearth, aEarth, rsat_itrf, n_max, m_max, GFM_Cnm, GFM_Snm , fx,fy,fz)
Fgrav_itrf = (/ fx, fy, fz /)

! Acceleration vector transformation from terrestrial to inertial frame
! Fgrav_icrf = TRS2CRS * Fgrav_itrf
CALL matrix_Rr (TRS2CRS, Fgrav_itrf , Fgrav_icrf)

End IF

End If
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Planetary/Lunar orbital perturbations
! we need sun and moon positions anyway so test is lower down
! ----------------------------------------------------------------------

! Julian Day Number of the input epoch
JD = mjd + 2400000.5D0

! Center celestial body: Earth
NCTR = 3 

! Planets perturbation loop
aPlanets_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
DO NTARG_body = 1 , 11
   IF (NTARG_body /= 3) THEN
   !IF (NTARG_body /= 3 .and. &
   !(NTARG_body==2 .or. NTARG_body==5 .or. NTARG_body==10 .or. NTARG_body==11) ) THEN 
   !print *,"NTARG_body", NTARG_body !IF (NTARG_body/=3 .and. NTARG_body/=1 .and. ) THEN 
        
! Celestial body's (NTARG) Cartesian coordinates w.r.t. Center body (NCTR)
      NTARG = NTARG_body
      CALL  PLEPH ( JD, NTARG, NCTR, Zbody )
      
! Cartesian coordinates of the celestial body in meters: KM to M
      rbody(1) = Zbody(1) * 1000.D0
      rbody(2) = Zbody(2) * 1000.D0
      rbody(3) = Zbody(3) * 1000.D0

! GM gravity constants of the solar system bodies
      GMbody = GMconst(NTARG)
 
If (yml_planetary_perturbations_enabled) Then
! ----------------------------------------------------------------------
! Point-mass perturbations vector due to the selected celestial body
      CALL force_gm3rd (rbody, rsat_icrf, GMbody , a_perturb)
! Overall (sum) planetary pertrubation acceleration vector
      aPlanets_icrf = aPlanets_icrf + a_perturb
! ----------------------------------------------------------------------
end if

! ----------------------------------------------------------------------
! Sun
      if (NTARG == 11) then
      rSun = rbody
      vSun = (/ Zbody(4), Zbody(5), Zbody(6) /) * 1000.D0 ! KM/sec to m/sec   
      GM_sun = GMbody     
! ----------------------------------------------------------------------
! Moon
      else if (NTARG == 10) then
      rMoon = rbody
      GM_moon = GMbody
      end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
if (1<0) then
! ----------------------------------------------------------------------
! Venus
If (NTARG == 2) then
!print *,"Venus", mjd, JD, rbody(1:3) 
WRITE (writeline,FMT='(2F25.15, 3F25.5)') mjd, JD, rbody(1:3)
filename = 'Venus.out'
Call write_data (filename, writeline)
End IF
! ----------------------------------------------------------------------
! Jupiter
If (NTARG == 5) then
!print *,"Jupiter", mjd, JD, rbody(1:3) 
WRITE (writeline,FMT='(2F25.15, 3F25.5)') mjd, JD, rbody(1:3)
filename = 'Jupiter.out'
Call write_data (filename, writeline)
End IF
! ----------------------------------------------------------------------
! Sun
If (NTARG == 11) then
!print *,"Sun", mjd, JD, rbody(1:3) 
WRITE (writeline,FMT='(2F25.15, 3F25.5)') mjd, JD, rbody(1:3)
filename = 'Sun.out'
Call write_data (filename, writeline)
End IF
! ----------------------------------------------------------------------
! Moon
If (NTARG == 10) then
!print *,"Moon", mjd, JD, rbody(1:3) 
WRITE (writeline,FMT='(2F25.15, 3F25.9)') mjd, JD, rbody(1:3)
filename = 'Moon.out'
Call write_data (filename, writeline)
End IF
! ----------------------------------------------------------------------
end if
! ----------------------------------------------------------------------
  
   END IF
END DO
 
If (yml_planetary_perturbations_enabled) Then
  
! ----------------------------------------------------------------------
! Indirect J2 effect
! ----------------------------------------------------------------------
! Sun and Moon position vectors in terrestrial reference frame 
! Transformation GCRS to ITRS 
CALL matrix_Rr (CRS2TRS,rMoon , rMoon_ITRS)
CALL matrix_Rr (CRS2TRS,rSun  , rSun_ITRS)

If (FMOD_GRAVFIELD > 0) Then
! C20 spherical harmonic coefficient of geopotential 
C20 = GFM_Cnm(2+1,0+1)
!Re = GFM_ae

Else If (FMOD_GRAVFIELD == 0) then
C20 = -4.841694552725D-04
!Re = Earth_radius

End IF

! Earth Radius
Re = aEarth
  
! Indirect J2 effect of Sun and Moon
CALL indirectJ2(C20,Re,GM_moon,rMoon_ITRS,GM_sun,rSun_ITRS , a_iJ2)

! Acceleration vector transformation from terrestrial to inertial frame
a_iJ2_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
CALL matrix_Rr (TRS2CRS, a_iJ2 , a_iJ2_icrf)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Planetary/Lunar perturbation accleration
Fplanets_icrf = aPlanets_icrf + a_iJ2_icrf
! ----------------------------------------------------------------------

ELSE If (.not. yml_planetary_perturbations_enabled) then

Fplanets_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)

End IF    
! ----------------------------------------------------------------------

a_solidtides = 0.d0
! ----------------------------------------------------------------------
! Tidal effects
! ----------------------------------------------------------------------
If (yml_tidal_effects_enabled) Then

! ----------------------------------------------------------------------
! Solid Earth Tides
! ----------------------------------------------------------------------
! Frequency-independent : Step 1 (IERS Conventions 2010)
      dCnm_solid1 = 0.0D0
      dSnm_solid1 = 0.0D0
If (BTEST(yml_tidal_effects, solid_nonfreq - one)) Then 
      CALL tides_solid1(rMoon_ITRS, rSun_ITRS, GMearth, aEarth, GM_moon, GM_sun, dCnm_solid1, dSnm_solid1)
End if

! Frequency-dependent : Step 2 (IERS Conventions 2010)
      dCnm_solid2 = 0.0D0
      dSnm_solid2 = 0.0D0
If (BTEST(yml_tidal_effects, solid_freq - one)) Then 
      CALL tides_solid2(mjd, ut1_utc , dCnm_solid2, dSnm_solid2)
End if
 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Zero Tide term
CALL tide_perm (dC20_perm)

! Tide system of the input gravity field model is set in the GFM_tide global variable in the module mdl_param.f03
if (GFM_tide == 'zero_tide') then
   dCnm_solid1(2+1,0+1) = dCnm_solid1(2+1,0+1) - dC20_perm
   !dCnm_solid2(2+1,0+1) = dCnm_solid2(2+1,0+1) - dC20_perm
end if    
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Pole Tide
! ----------------------------------------------------------------------
! Solid Earth pole tide
dC21_pse = 0.0D0
dS21_pse = 0.0D0
If (BTEST(yml_tidal_effects, solid_pole - one)) Then 
      CALL tide_pole_se (mjd,xp,yp , dC21_pse, dS21_pse)  
End if
 
! Ocean pole tide
dC21_poc = 0.0D0
dS21_poc = 0.0D0
If (BTEST(yml_tidal_effects, ocean_pole - one)) Then 
      CALL tide_pole_oc (mjd,xp,yp , dC21_poc, dS21_poc)
end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! dCnm, dSnm arrays sum
sz_tides = SIZE (dCnm_solid1,DIM=1)
ALLOCATE (dCnm_tides(sz_tides,sz_tides), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE force_sum.f03"
         PRINT *, "Error: Allocatable Array: dCnm_tides | Nmax:", sz_tides
!         STOP "*** Not enough memory ***"
      END IF  

ALLOCATE (dSnm_tides(sz_tides,sz_tides), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE force_sum.f03"
         PRINT *, "Error: Allocatable Array: dSnm_tides | Nmax:", sz_tides
!         STOP "*** Not enough memory ***"
      END IF  
      
dCnm_tides = dCnm_solid1
dSnm_tides = dSnm_solid1

Do i = 1 , 3
Do j = 1 , 3
   dCnm_tides(i,j) = dCnm_tides(i,j) + dCnm_solid2(i,j) 
   dSnm_tides(i,j) = dSnm_tides(i,j) + dSnm_solid2(i,j) 
End Do
End Do

dCnm_tides(2+1,1+1) = dCnm_tides(2+1,1+1) + dC21_pse + dC21_poc 
dSnm_tides(2+1,1+1) = dSnm_tides(2+1,1+1) + dS21_pse + dS21_poc

! Acceleration cartesian components
a_solidtides = (/ 0.D0, 0.0D0, 0.0D0 /)
CALL force_tides(rsat_itrf, GMearth, aEarth, sz_tides-1, sz_tides-1, dCnm_tides, dSnm_tides, ax,ay,az)
a_solidtides(1) = ax
a_solidtides(2) = ay
a_solidtides(3) = az

!DEALLOCATE (dCnm_tides,   STAT = DeAllocateStatus)
!DEALLOCATE (dSnm_tides,   STAT = DeAllocateStatus)
sz_tides = 0
! ----------------------------------------------------------------------
!print *,"Pole Tide dC,dS", dC21_pse, dC21_poc, dS21_pse, dS21_poc

! ----------------------------------------------------------------------
! Ocean Tides
a_ocean = (/ 0.D0, 0.0D0, 0.0D0 /)
If (BTEST(yml_tidal_effects, ocean - one)) Then 
      CALL tides_ocean(OCEAN_Nmax, OCEAN_Mmax, mjd, ut1_utc, dCnm_ocean, dSnm_ocean)
      ! Acceleration cartesian components
      CALL force_tides(rsat_itrf, GMearth, aEarth, OCEAN_Nmax, OCEAN_Mmax, dCnm_ocean, dSnm_ocean, ax,ay,az)      
      a_ocean (1) = ax
      a_ocean (2) = ay
      a_ocean (3) = az
      !DEALLOCATE (dCnm_ocean,   STAT = DeAllocateStatus)
      !DEALLOCATE (dSnm_ocean,   STAT = DeAllocateStatus)
      sz_tides = 0
End if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Tides acceleration cartesian components : Overall effects
! ----------------------------------------------------------------------
!a_tides = a_solid1 + a_solid2 + a_ocean + a_pse + a_poc
a_tides = a_solidtides + a_ocean 
 
! Acceleration vector transformation from terrestrial to inertial frame
CALL matrix_Rr (TRS2CRS, a_tides , Ftides_icrf)
! ----------------------------------------------------------------------
  
Else If (.not. yml_tidal_effects_enabled) Then

Ftides_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)

End IF
! End of Tidal effects
! ----------------------------------------------------------------------
if (yml_tidal_effects_enabled) then
IF (abs(a_solidtides(1))>1.D0 .OR. abs(a_solidtides(2))>1.D0 .OR. abs(a_solidtides(3))>1.D0) THEN
print *,"bad solidtides! "
print *,"PRN", PRN
print *,"mjd ", mjd
print *,"a_solidtides ", a_solidtides
PRINT *,"dCnm_tides",dCnm_tides
print *," "
PRINT *,"dSnm_tides",dSnm_tides
print *," "
PRINT *,"dCnm_solid1",dCnm_solid1
print *," "
PRINT *,"dSnm_solid1",dSnm_solid1
print *," "
PRINT *,"dCnm_solid2",dCnm_solid2
print *," "
PRINT *,"dSnm_solid2",dSnm_solid2
print *," "
print *,"Pole Tide dC,dS", dC21_pse, dC21_poc, dS21_pse, dS21_poc
print *," "
print *,"PRN", PRN
print *,"mjd ", mjd
STOP
END IF 
!print *,"a_ocean      ", a_ocean
end if

! ----------------------------------------------------------------------
! Relativistic effects
! ----------------------------------------------------------------------
If (yml_rel_effects_enabled) Then

! Satellite State Vector in GCRS
Zsat_GCRS = (/rsat_icrf(1), rsat_icrf(2), rsat_icrf(3), vsat_icrf(1), vsat_icrf(2), vsat_icrf(3) /) 

! Earth state vector w.r.t. Sun
Zearth_GCRS = -1.0D0 * (/ rSun(1),rSun(2),rSun(3) , vSun(1),vSun(2),vSun(3) /)
!PRINT *,"Zearth_GCRS",Zearth_GCRS

! Schwarzschild terms
CALL rel_schwarzschild (Zsat_GCRS, Zearth_GCRS, GMearth, beta_ppn, gama_ppn, c_light, a_Schwarzschild)

! Lense-Thirring effects      
CALL rel_LenseThirring (Zsat_GCRS, Zearth_GCRS,  GMearth, gama_ppn, c_light , a_LenseThirring)
!FIXME: why call function if you are going to ignore the result?
a_LenseThirring = (/ 0.D0, 0.0D0, 0.0D0 /)

! de Sitter effect or geodesic precesssion 
CALL rel_deSitter (Zsat_GCRS, Zearth_GCRS, GMearth, GM_sun, gama_ppn, c_light , a_deSitter)

Frelativity_icrf = a_Schwarzschild + a_LenseThirring + a_deSitter

Else If (.not. yml_rel_effects_enabled) Then

Frelativity_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)

End IF
! ----------------------------------------------------------------------

! Summary of gravitational effects 
SFgrav = Fgrav_icrf + Fplanets_icrf + Ftides_icrf + Frelativity_icrf 
! End of Gravitational Effects
! ----------------------------------------------------------------------
!print *,"Fgrav_icrf       ", Fgrav_icrf
!print *,"Fplanets_icrf    ", Fplanets_icrf
!print *,"Ftides_icrf      ", Ftides_icrf
!print *,"Frelativity_icrf ", Frelativity_icrf


! ----------------------------------------------------------------------
! Attitude model :: GNSS Yaw-attitude models
! ----------------------------------------------------------------------
! Variables "satblk" and "BDSorbtype" are temporary manually configured 
! in the main program file (main_pod.f03) through setting the global variables: 

! GPS case: Satellite Block ID:        1=I, 2=II, 3=IIA, IIR=(4, 5), IIF=6
satblk = SATblock_glb

! Beidou case: 'IGSO', 'MEO'
BDSorbtype = BDSorbtype_glb

fmt_line = '(A1,I2.2)'
READ (PRN, fmt_line , IOSTAT=ios) GNSSid, PRN_no

! Yaw-attitude model
PRN_GNSS = PRN
!CALL attitude (mjd, rsat_icrf, vsat_icrf, rSun, PRN_GNSS, satblk, BDSorbtype, &
!                     eclipsf, beta, Mangle, Yangle, eBX_nom, eBX_ecl)
BLKsat = BLKTYP                  
CALL attitude (mjd, rsat_icrf, vsat_icrf, rSun, PRN_GNSS, BLKsat, & 
                     eclipsf, beta, Mangle, Yangle, eBX_nom, eBX_ecl)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! shadow model :: A conical model is applied.
!                 lambda = 1     : In SUN LIGHT AREA
!                        = 0     : In UMBRA AREA (full eclipse)
!                 0 < lambda < 1 : In PENUMBRA AREA
! ----------------------------------------------------------------------
CALL shadow (rsat_icrf, rSun, rMoon, lambda, ECLTYP )
!print*,'lambda =', lambda


! ----------------------------------------------------------------------
! Non-Gravitational Effects
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Solar Radiation
! ----------------------------------------------------------------------
if (BTEST(yml_non_grav_effects, SOLARNG - one)) Then


! SRP model
srpid =  yml_apriori_srp


CALL force_srp (lambda, eBX_ecl, eclipsf, GMearth, GNSSid, srpid, rsat_icrf, vsat_icrf, rSun, fx, fy, fz )
Fsrp_icrf = (/ fx, fy, fz /)

Else

    Fsrp_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
End IF

! ----------------------------------------------------------------------
! Earth radiation pressure
! ----------------------------------------------------------------------
if (BTEST(yml_non_grav_effects, EARTHNG - one)) Then

CALL force_erp (mjd, rsat_icrf, vsat_icrf, rSun, fx, fy, fz)
Ferp_icrf = (/ fx, fy, fz /)

Else

        Ferp_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
End IF

! ----------------------------------------------------------------------
! Antenna thrust effect 
! ----------------------------------------------------------------------
if (BTEST(yml_non_grav_effects, ANTENNANG - one)) Then

CALL force_ant (rsat_icrf, fx, fy, fz)
Fant_icrf = (/ fx, fy, fz /)

Else

        Fant_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
End IF
! ----------------------------------------------------------------------

! Summary of non-gravitational effects
SFnongrav = Fsrp_icrf + Ferp_icrf + Fant_icrf

! End of non-Gravitational Effects
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Empirical forces
! ----------------------------------------------------------------------
IF (yml_EMP_mode) Then

!Call pd_empirical (rsat_icrf, vsat_icrf, GMearth, SFemp, PD_EMP_r, PD_EMP_v, PD_EMP_param)
Frame_EmpiricalForces = Frame_EmpiricalForces_glb      
Yawangle = Yangle(2)
CALL pd_empirical (rsat_icrf, vsat_icrf, GMearth, Yawangle,Frame_EmpiricalForces, & 
                   SFemp, PD_EMP_r, PD_EMP_v, PD_EMP_param)
       
Else
    SFemp = (/ 0.0D0, 0.0D0, 0.0D0 /)
End IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Pseudo-stochastic pulses
! ----------------------------------------------------------------------
IF (yml_pulses) Then
CALL pulses_force (rsat_icrf, vsat_icrf, mjd, t_sec, integr_stage, SFpulses, PD_pulse_r, PD_pulse_v, PD_pulses_param) 
Else 
    SFpulses = (/ 0.0D0, 0.0D0, 0.0D0 /)
End IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Acceleration sum of the force model
! ----------------------------------------------------------------------
!SF = Fgrav_icrf + Fplanets_icrf + Ftides_icrf + Frelativity_icrf + Fsrp_icrf
SF = SFgrav + SFnongrav + SFemp + SFpulses      
SFx = SF(1) 
SFy = SF(2)
SFz = SF(3)
! ----------------------------------------------------------------------
!print *,"SFgrav   ", SFgrav
!print *,"SFnongrav", SFnongrav
!print *,"SFemp    ", SFemp



END
