SUBROUTINE prm_ocean (PRMfname)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_ocean.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read the Ocean Tides model 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	5 April 2018
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
      CHARACTER (LEN=100), INTENT(IN) :: PRMfname				
! OUT

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: ForceMod 
      logical ForceMod_Tides, ForceMod_Ocean  

! ----------------------------------------------------------------------
! Tides
      CHARACTER (LEN=100) :: FESxxfname				
      INTEGER (KIND = prec_int8) :: Nmax 
!      INTEGER (KIND = prec_int8) :: ocean_Nmax, ocean_Mmax
!      INTEGER (KIND = prec_int2) :: tide_system
! ----------------------------------------------------------------------
      CHARACTER (LEN=300) :: filename	   
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
! ----------------------------------------------------------------------

if (.not. yaml_found) then

! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! Init ForceMod_Tides and ForceMod_Ocean to nonsense values
ForceMod_Tides = .false.
ForceMod_Ocean = .false.

! ----------------------------------------------------------------------
! Read input file
i = 0
DO

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1
!PRINT *, "READ Line (i,ios):", i, ios_line

! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
!READ (line_ith, * , IOSTAT=ios_data) word1_ln, charN 
! ----------------------------------------------------------------------
!PRINT *, "word1_ln: ", word1_ln



! ----------------------------------------------------------------------
! Parameters Keywords read 
! ----------------------------------------------------------------------
! Tidal effects
! FMOD_GRAV(3) : Tides
IF (word1_ln == "Tidal_effects") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ForceMod 
	!FMOD_GRAV(3) = ForceMod
	ForceMod_Tides = (ForceMod > 0)
        !yml_tidal_effects_enabled = .false.
        !if (ForceMod > 0) yml_tidal_effects_enabled = .true.
	!print *,"ForceMod", ForceMod
END IF

! FMOD_TIDES(3) : Ocean Tides
! 1. FMOD_TIDES(i)=1 : Effect is considered
! 2. FMOD_TIDES(i)=0 : Effect is neglected 
IF (word1_ln == "ocean_tides") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ForceMod 
	!FMOD_TIDES(3) = ForceMod
	ForceMod_Ocean = (ForceMod > 0)
        !if (ForceMod > 0 .and. .not. BTEST(yml_tidal_effects, ocean - one)) then
        !    yml_tidal_effects = yml_tidal_effects + pow(2, ocean - one)
        !end if
END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Ocean Tides model
! ----------------------------------------------------------------------
! Model file name
IF (word1_ln == "ocean_tides_model_file") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, filename 
	FESxxfname = TRIM(filename)
        yml_ocean_tides_file = FESxxfname
END IF

! Maximum degree and order of ocean tides model (spherical harmonic coefficients corrections)	  
IF (word1_ln == "ocean_tides_model_deg") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Nmax 
	OCEAN_Nmax = Nmax
	OCEAN_Mmax = Nmax
        yml_tides_max_degree = Nmax
END IF
! ----------------------------------------------------------------------


END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------

else
        OCEAN_Nmax = yml_tides_max_degree
        OCEAN_Mmax = yml_tides_max_degree
        ForceMod_Tides = yml_tidal_effects_enabled
        ForceMod_Ocean = BTEST(yml_tidal_effects, ocean - one)
end if ! not yaml_found

! ----------------------------------------------------------------------
!  Tidal effects to orbits: Solid Earth Tides, Ocean Tides and Pole Tide
! ----------------------------------------------------------------------
!If (FMOD_GRAV(3) == 1) Then
If (ForceMod_Tides) Then

! ----------------------------------------------------------------------
! Ocean Tides
!IF (FMOD_TIDES(3) == 1) Then
IF (ForceMod_Ocean) Then

! Ocean Tides model
! Read ocean tides model data: Spherical harmonic coefficients corrections
! The spherical harmoncis corrections are stored in dynamic allocatable arrays through the module mdl_tides.f90
if (.false.) print *, "calling tides on file ", yml_ocean_tides_file

CALL tides_fes2004(yml_ocean_tides_file)

End IF 
! ----------------------------------------------------------------------

End IF
! End of parameters setting for Tidal effects
! ----------------------------------------------------------------------



END
