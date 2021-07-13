MODULE m_keplerorb


! ----------------------------------------------------------------------
! MODULE: m_keplerorb.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the keplerorb subroutine used for computating Kepelrian orbit
! 
! Subroutines contained within the module:
! - keplerorb
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	14 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE keplerorb (GM, MJDo, Sec0, Zo, Zel, Ndays, interv, orb1, orb2)


! ----------------------------------------------------------------------
! SUBROUTINE: keplerorb.f90
! ----------------------------------------------------------------------
! Purpose:
!  Kepler orbit based on the initial conditions as input
! ----------------------------------------------------------------------
! Input arguments:
! - GM:				Earth Gravity Constant (m^3/sec^2)
! - MJDo: 			Modified Julian Day number at the initial epoch (including the fraction of the day)
! - Sec0:			Seconds of Day at the initial epoch
! - Zo:				Initial state vector 
! - Zel:			Coordinates type of the initial state vector
!   Zel = 1			Keplerian Elements 
!   Zel = 2			State vector (Position & Velocity) cartesian coordinates in m & m/sec
! - Ndays:			Number of days of the keplerian orbit arc length to be computed
! - interv: 		Interval of the orbit epochs (in seconds)
!
! Output arguments:
! - orb1:			Orbit array of Keplerian elements per epoch
! - orb2: 			Orbit array of state vector's cartesian coordinates per epoch
! ----------------------------------------------------------------------
! Note 1:
! - kepler:		Kepler elements array (6)
!   a:       	semi-major axis  (m)
!   e:      	eccentricity
!   i:      	inclination (degrees)
!   Omega:  	right ascension of the ascending node (degrees)
!   omega:  	argument of perigee (degrees)
!   E:      	eccentric anomaly (degrees)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	25 August 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 14 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 adavantages in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_arr
	  !USE mdl_write
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: GM
      REAL (KIND = prec_q), INTENT(IN) :: MJDo, Sec0, Zo(6)
      INTEGER (KIND = prec_int4), INTENT(IN) :: Zel
      INTEGER (KIND = prec_int8), INTENT(IN) :: Ndays, interv 
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orb1
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orb2
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Norb, i, sz1, sz2
      REAL (KIND = prec_q) :: kepler_0(6), kepler_9(9), a_semiaxis, ec, Eo_rad, Mo_rad, n_motion
      REAL (KIND = prec_q) :: MJD_i, SecDay_i, SecDay_0, dt, Sec_arc
      REAL (KIND = prec_q) :: Mi_rad, Mi_deg, Ei
      REAL (KIND = prec_q) :: kepler_i(6), r_i(3), v_i(3)
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Gravitational constant
!GM = 0.39860044150D+15
! ----------------------------------------------------------------------


! Orbit arc in sec
Sec_arc = Ndays * 86400.D0

Norb = INT( Sec_arc / interv) 


! orb1: Orbit array of Keplerian elements per epoch
ALLOCATE ( orb1(Norb, 8), STAT = AllocateStatus)	  

! orb2: Orbit array of state vector's cartesian coordinates per epoch
ALLOCATE ( orb2(Norb, 8), STAT = AllocateStatus)	  



! ----------------------------------------------------------------------
! Intial Conditions
If (Zel == 1) then

kepler_0 = Zo

Else if (Zel == 2) then

r_i = Zo(1:3)
v_i = Zo(4:6)
!CALL kepler_z2k (r_i, v_i, GM, kepler_0)
CALL kepler_z2k (r_i, v_i, GM, kepler_9)

End If

SecDay_0 = Sec0
!MJDo = MJDo


!a_semiaxis = kepler_0(1)
a_semiaxis = kepler_9(1)

!ec = kepler_0(2)
ec = kepler_9(2)

! Eccentric anomaly at to (radians)
!Eo_rad = kepler_0(7) * (PI_global / 180.D0)
Eo_rad = kepler_9(7) * (PI_global / 180.D0)

! Mean anomaly  at to (radians)
Mo_rad = Eo_rad - ec * sin(Eo_rad)
!Mo_rad = kepler_9(8) * (PI_global / 180.D0)


! ----------------------------------------------------------------------
! mean motion
n_motion = sqrt(GM / a_semiaxis**3)
! ----------------------------------------------------------------------


kepler_0(1:5) = kepler_9(1:5)
kepler_0(6) = kepler_9(7)



! ----------------------------------------------------------------------
! Computing Keplerian elements and State vector's cartesian coordinates per epoch
Do i = 1 , Norb


If (i == 1) then

! Initial Epoch
MJD_i    = MJDo
SecDay_i = SecDay_0


! Cartesian coordinates of the state vector (Inertial Frame)
kepler_i = kepler_0
CALL kepler_k2z (kepler_i, GM, r_i, v_i) 


Else

! Epoch i
SecDay_i = SecDay_0 + interv * (i - 1)
!SecDay_i = SecDay_i + interv

!MJD_i = MJDo + SecDay_i / 86400.D0
MJD_i = INT(MJDo) + SecDay_i / 86400.D0


! ----------------------------------------------------------------------
! Time difference since initial epoch
dt = MJD_i - MJDo
dt = SecDay_i - SecDay_0
! ----------------------------------------------------------------------


! Mean anomaly at epoch i (radians)
Mi_rad = Mo_rad + n_motion * dt

! Mean anomaly in degrees 
Mi_deg = Mi_rad * (180.D0/PI_global)

! Angle within the range {0 - 360}
If (Mi_deg >= 360.D0) then
	Mi_deg = Mi_deg - INT(Mi_deg / 360.D0) * 360.D0
End If


! Eccentric anomaly at ti (degrees) obtained by solving Kepler's Equation
CALL kepler_eq ( Mi_deg , ec , Ei)

!kepler_i = (/ a, e, i, Omega_asc, omega_per, Ei /)
kepler_i = (/ kepler_0(1), kepler_0(2), kepler_0(3), kepler_0(4), kepler_0(5), Ei /)

! Cartesian coordinates of the state vector (Inertial Frame)
CALL kepler_k2z (kepler_i, GM, r_i, v_i) 


End If 


! Orbit arrays
If (SecDay_i >= 86400.D0) Then
	SecDay_i = SecDay_i - INT(SecDay_i / 86400.D0) * 86400.D0
End If
orb1(i,:) = (/ MJD_i, SecDay_i, kepler_i /) 
orb2(i,:) = (/ MJD_i, SecDay_i, r_i, v_i /) 


End Do



End SUBROUTINE



END Module
