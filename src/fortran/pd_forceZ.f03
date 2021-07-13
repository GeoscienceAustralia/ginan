SUBROUTINE pd_forceZ (mjd, rsat, vsat, Fvec, PDr, PDv)


! ----------------------------------------------------------------------
! SUBROUTINE: pd_forceZ.f03
! ----------------------------------------------------------------------
! Purpose:
!  Satellite acceleration components based on the considered force model 
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number (including the fraction of the day) in Terrestrial Time (TT)
! - rsat:			Satellite Position vector (m)   in inertial frame (ICRF)
! - vsat:			Satellite Velocity vector (m/s) in inertial frame (ICRF)
! 
! Output arguments:
! - Fvec:			Acceleration vector cartesian components in inertial frame (ICRF)
! - PDr: 			Partial derivatives matrix of the acceleration w.r.t. the position vector in ICRF
! - PDv: 			Partial derivatives matrix of the acceleration w.r.t. the velocity vector in ICRF
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Cooperative Research Centre for Spatial Information, Australia
! Created:	February 2018
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
      USE m_pd_geopotential
      USE m_matrixRxR
      USE mdl_config
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: rsat, vsat
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Fvec(3)
      REAL (KIND = prec_d), INTENT(OUT) :: PDr(3,3), PDv(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: rsat_icrf, vsat_icrf, rsat_itrf, vsat_itrf, v_TRS_1, v_TRS_2
	  DOUBLE PRECISION EOP_cr(7)
      DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)	  
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: SF
      REAL (KIND = prec_q), DIMENSION(3) :: Fgrav_itrf , Fgrav_icrf, Fplanets_icrf, Ftides_icrf, Frelativity_icrf
      REAL (KIND = prec_d), DIMENSION(3) :: Fsrp_icrf
      REAL (KIND = prec_d) :: fx, fy, fz
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: GMearth, aEarth
      INTEGER (KIND = prec_int8) :: n_max, m_max
      INTEGER(KIND = 4)          :: satsvn
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
      REAL (KIND = prec_q), DIMENSION(3) :: a_solid1, a_solid2, a_pse, a_poc
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCnm_tides, dSnm_tides		
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCnm_ocean, dSnm_ocean		
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(6) :: Zsat_GCRS, Zearth_GCRS
      REAL (KIND = prec_q), DIMENSION(3) :: vSun
      REAL (KIND = prec_q), DIMENSION(3) :: a_Schwarzschild, a_LenseThirring, a_deSitter
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=30) :: fmt_line
      INTEGER (KIND = prec_int2) :: ios
      CHARACTER (LEN=1) :: GNSSid
      INTEGER (KIND = prec_int4) :: PRN_no
      INTEGER :: eclpf, srpid
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: beta_ppn, gama_ppn
      REAL (KIND = prec_q) :: c_light
! ----------------------------------------------------------------------
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: U1, U2, U3
      REAL (KIND = prec_d), DIMENSION(3,3) :: Ugrav_icrf, Ugrav_itrf, Ugrav




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
If (.not. yml_gravity_enabled) Then

! Setting to Zero
fx = 0.0D0
fy = 0.0D0
fz = 0.0D0	  
Fgrav_icrf = (/ fx, fy, fz /)

Ugrav_icrf(1,:) = (/0.0D0, 0.0D0, 0.0D0 /)
Ugrav_icrf(2,:) = (/0.0D0, 0.0D0, 0.0D0 /)
Ugrav_icrf(3,:) = (/0.0D0, 0.0D0, 0.0D0 /)

Else 	  

If (FMOD_GRAVFIELD == 0) Then

! Central term of the Earth Gravity Field 
Call force_gm (GMearth, rsat_icrf, fx,fy,fz )
Fgrav_icrf = (/ fx, fy, fz /)
! Partial derivatives
Call pd_gm (GMearth, rsat_icrf, Ugrav_icrf)


Else 

! Earth Gravity Field model acceleration components (spherical harmonics expansion) 
n_max = GFM_Nmax
m_max = GFM_Mmax
CALL force_gfm (GMearth, aEarth, rsat_itrf, n_max, m_max, GFM_Cnm, GFM_Snm , fx,fy,fz)
Fgrav_itrf = (/ fx, fy, fz /)

! Acceleration vector transformation from terrestrial to inertial frame
! Fgrav_icrf = TRS2CRS * Fgrav_itrf
CALL matrix_Rr (TRS2CRS, Fgrav_itrf , Fgrav_icrf)


! Partial derivatives
!Call pd_geopotential(GMearth, aEarth, rsat_itrf, n_max, m_max, GFM_Cnm, GFM_Snm, dCnm, dSnm , fx,fy,fz, Ugrav_itrf)
Call pd_geopotential(GMearth, aEarth, rsat_itrf, n_max, m_max, GFM_Cnm, GFM_Snm, Fgrav_itrf, Ugrav_itrf)

! Acceleration vector transformation from terrestrial to inertial frame
CALL matrix_Rr (TRS2CRS, Fgrav_itrf , Fgrav_icrf)


! PD matrix transformation ITRF to ICRF
! (df/dr)_Inertial = EOP(t) * (df/dr)_Terrestrial * inv( EOP(t) )

!! Allocatable arrays
!ALLOCATE (U1(3,3), STAT = AllocateStatus)
!ALLOCATE (U2(3,3), STAT = AllocateStatus)
!ALLOCATE (U3(3,3), STAT = AllocateStatus)

!!Call matrixRxR(TRS2CRS, Ugrav_itrf, Ugrav)
!U1 = TRS2CRS
!U2 = Ugrav_itrf
!Call matrixRxR (U1, U2, U3)								
!Ugrav = U3

!!Call matrixRxR(Ugrav, TRANSPOSE(TRS2CRS) , Ugrav_icrf)
!!Call matrixRxR(Ugrav, CRS2TRS ,Ugrav_icrf)
!U1 = Ugrav
!U2 = TRANSPOSE(TRS2CRS)
!Call matrixRxR (U1, U2, U3)								
!Ugrav_icrf = U3

Ugrav = MATMUL(TRS2CRS, Ugrav_itrf) 		
Ugrav_icrf = MATMUL(Ugrav, TRANSPOSE(TRS2CRS)) 		

End IF

End If
! ----------------------------------------------------------------------

! variable initialisation (vSun)
vSun = 0.d0

! ----------------------------------------------------------------------
! Planetary/Lunar orbital perturbations
! ----------------------------------------------------------------------
If (yml_planetary_perturbations_enabled) Then

! Julian Day Number of the input epoch
JD = mjd + 2400000.5D0

! Center celestial body: Earth
NCTR = 3 

! Planets perturbation loop
aPlanets_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)
DO NTARG_body = 1 , 11
   IF (NTARG_body /= 3) THEN 
		
! Celestial body's (NTARG) Cartesian coordinates w.r.t. Center body (NCTR)
      NTARG = NTARG_body
      CALL  PLEPH ( JD, NTARG, NCTR, Zbody )
	  
! Cartesian coordinates of the celestial body in meters: KM to M
	  rbody(1) = Zbody(1) * 1000.D0
	  rbody(2) = Zbody(2) * 1000.D0
	  rbody(3) = Zbody(3) * 1000.D0

! GM gravity constants of the solar system bodies
      GMbody = GMconst(NTARG)
 
 
! ----------------------------------------------------------------------
! Point-mass perturbations vector due to the selected celestial body
      CALL force_gm3rd (rbody, rsat_icrf, GMbody , a_perturb)
! Overall (sum) planetary pertrubation acceleration vector
	  aPlanets_icrf = aPlanets_icrf + a_perturb
! ----------------------------------------------------------------------


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

   END IF
END DO
	  
	  
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
CALL matrix_Rr (TRS2CRS, a_iJ2 , a_iJ2_icrf)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Planetary/Lunar perturbation accleration
Fplanets_icrf = aPlanets_icrf + a_iJ2_icrf
! ----------------------------------------------------------------------


ELSE If (.not. yml_planetary_perturbations_enabled) Then

Fplanets_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)

End IF	  
! ----------------------------------------------------------------------




! ----------------------------------------------------------------------
! Tidal effects
! ----------------------------------------------------------------------
If (yml_tidal_effects_enabled) Then


! ----------------------------------------------------------------------
! Solid Earth Tides
! ----------------------------------------------------------------------
! Frequency-independent : Step 1 (IERS Conventions 2010)
If (BTEST(yml_tidal_effects, solid_nonfreq - one)) Then 

      CALL tides_solid1(rMoon_ITRS, rSun_ITRS, GMearth, aEarth, GM_moon, GM_sun, dCnm_solid1, dSnm_solid1)
	  
Else
      dCnm_solid1 = 0.0D0
      dSnm_solid1 = 0.0D0
End if

! Frequency-dependent : Step 2 (IERS Conventions 2010)
If (BTEST(yml_tidal_effects, solid_freq - one)) Then 

      CALL tides_solid2(mjd, ut1_utc , dCnm_solid2, dSnm_solid2)
	  
Else
      dCnm_solid2 = 0.0D0
      dSnm_solid2 = 0.0D0
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
CALL force_tides(rsat_itrf, GMearth, aEarth, sz_tides-1, sz_tides-1, dCnm_tides, dSnm_tides, ax,ay,az)
a_solidtides(1) = ax
a_solidtides(2) = ay
a_solidtides(3) = az

!PRINT *,"dCnm_tides",dCnm_tides
!PRINT *,"dSnm_tides",dSnm_tides

DEALLOCATE (dCnm_tides,   STAT = DeAllocateStatus)
DEALLOCATE (dSnm_tides,   STAT = DeAllocateStatus)
sz_tides = 0
! ----------------------------------------------------------------------
!PRINT *,"dCnm_solid1",dCnm_solid1
!PRINT *,"dSnm_solid1",dSnm_solid1
!PRINT *,"dCnm_solid2",dCnm_solid2
!PRINT *,"dSnm_solid2",dSnm_solid2
!PRINT *,"dC21_pse, dS21_pse", dC21_pse, dS21_pse
!PRINT *,"dC21_poc, dS21_poc", dC21_poc, dS21_poc
!PRINT *, "dC20_perm"  , dC20_perm



! ----------------------------------------------------------------------
! Ocean Tides
a_ocean = (/ 0.D0, 0.0D0, 0.0D0 /)
If (BTEST(yml_tidal_effects, ocean - one)) Then 

      CALL tides_ocean(OCEAN_Nmax, OCEAN_Mmax, mjd, ut1_utc, dCnm_ocean, dSnm_ocean)
  
      ! Acceleration cartesian components
	  !CALL force_tides(rsat_itrf, GMearth, aEarth, sz_tides-1, sz_tides-1, dCnm_ocean, dSnm_ocean, ax,ay,az)
	  CALL force_tides(rsat_itrf, GMearth, aEarth, OCEAN_Nmax, OCEAN_Mmax, dCnm_ocean, dSnm_ocean, ax,ay,az)
      a_ocean (1) = ax
      a_ocean (2) = ay
      a_ocean (3) = az
	  
      DEALLOCATE (dCnm_ocean,   STAT = DeAllocateStatus)
      DEALLOCATE (dSnm_ocean,   STAT = DeAllocateStatus)
      sz_tides = 0
End if
! ----------------------------------------------------------------------
!PRINT *,"dCnm_ocean",dCnm_ocean
!PRINT *,"dSnm_ocean",dSnm_ocean
!PRINT *,"FMOD_TIDES",FMOD_TIDES



! ----------------------------------------------------------------------
! Tides acceleration cartesian components : Overall effects
! ----------------------------------------------------------------------
!a_tides = a_solid1 + a_solid2 + a_ocean + a_pse + a_poc
a_tides = a_solidtides + a_ocean 
 
! Acceleration vector transformation from terrestrial to inertial frame
CALL matrix_Rr (TRS2CRS, a_tides , Ftides_icrf)
! ----------------------------------------------------------------------
!PRINT *,"a_tides", a_tides
!PRINT *,"a_solid", a_solidtides
!PRINT *,"a_ocean", a_ocean
	  
	  
Else If (.not. yml_tidal_effects_enabled) Then

Ftides_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)


End IF
! End of Tidal effects
! ----------------------------------------------------------------------





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
! FIXME: why call function if we are going to ignore the result?
CALL rel_LenseThirring (Zsat_GCRS, Zearth_GCRS,  GMearth, gama_ppn, c_light , a_LenseThirring)
a_LenseThirring = (/ 0.D0, 0.0D0, 0.0D0 /)

! de Sitter effect or geodesic precesssion 
CALL rel_deSitter (Zsat_GCRS, Zearth_GCRS, GMearth, GM_sun, gama_ppn, c_light , a_deSitter)


Frelativity_icrf = a_Schwarzschild + a_LenseThirring + a_deSitter


Else If (.not. yml_rel_effects_enabled) Then

Frelativity_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)

End IF
! ----------------------------------------------------------------------
!print *,"a_Schwarzschild ", a_Schwarzschild
!print *,"a_LenseThirring ", a_LenseThirring
!print *,"a_deSitter      ", a_deSitter
!print *,"Frelativity_icrf", Frelativity_icrf



 
! ----------------------------------------------------------------------
! Non-Gravitational Effects
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Solar Radiation
! ----------------------------------------------------------------------
if (BTEST(yml_non_grav_effects, SOLARNG - one)) Then

! PRN: GNSS constellation ID letter + Satellite number
fmt_line = '(A1,I2.2)'
READ (PRN, fmt_line , IOSTAT=ios) GNSSid, PRN_no

! Eclipse status		!! Temporary !!  Upgrade for calling the Yaw attitude library
eclpf = 0

! SRP model
srpid =  yml_apriori_srp

CALL force_srp (GMearth, PRN_no, satsvn, eclpf, srpid, rsat_icrf, vsat_icrf, rSun, fx,fy,fz )
Fsrp_icrf = (/ fx, fy, fz /)

Else

	Fsrp_icrf = (/ 0.D0, 0.0D0, 0.0D0 /)

End IF
! ----------------------------------------------------------------------
!print *,"SRP model values per epoch"
!print *,"FMOD_NONGRAV(1)", FMOD_NONGRAV(1)
!print *,"Fsrp_icrf      ", Fsrp_icrf
!print *,"rsat_icrf", rsat_icrf
!print *,"vsat_icrf", vsat_icrf
!print *,"rSun     ", rSun
!print *,"GFM_GM   ", GFM_GM





! ----------------------------------------------------------------------
! Acceleration sum of the force model
! ----------------------------------------------------------------------
SF = Fgrav_icrf + Fplanets_icrf + Ftides_icrf + Frelativity_icrf + Fsrp_icrf
!SFx = SF(1) 
!SFy = SF(2)
!SFz = SF(3)
Fvec = SF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Partial derivatives overall matrix
! ----------------------------------------------------------------------
! PD w.r.t position vector
PDr = Ugrav_icrf
! PD  w.r.t velocity vector
!PDv - Initialise to zero for now
! ----------------------------------------------------------------------
PDv = 0.d0

!print *,"Fgrav_icrf      ", Fgrav_icrf
!print *,"Fplanets_icrf   ", Fplanets_icrf
!print *,"Ftides_icrf     ", Ftides_icrf
!print *,"Frelativity_icrf", Frelativity_icrf




END
