SUBROUTINE prm_planets (PRMfname)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_planets.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read the Planetary/Lunar Ephemeris (DE) data 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Cooperative Research Centre for Spatial Information, Australia
! Created:	5 April 2018
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      use mdl_config
      use pod_yaml
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
! Planetary/Lunar orbits variables
      CHARACTER (LEN=100) :: fname_header,fname_data,fname_out
! ----------------------------------------------------------------------
      CHARACTER (LEN=300) :: filename	   
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
      LOGICAL saved
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------

if (.not. yaml_found) then
      saved = .false.
! ----------------------------------------------------------------------
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

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
! FMOD_GRAV(2) : Planetary/Lunar orbital perturbations

! Planetary/Lunar Perturbations
IF (word1_ln == "Planets_perturbations") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ForceMod 
	!FMOD_GRAV(2) = ForceMod
	!print *,"ForceMod", ForceMod
        yml_planetary_perturbations_enabled = .true.
        if (ForceMod == 0) yml_planetary_perturbations_enabled = .false.
END IF

! ----------------------------------------------------------------------	
! Planetary/Lunar precise ephemeris DE data filenames
! ----------------------------------------------------------------------	
! DE Header data file
IF (word1_ln == "DE_fname_header") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_header 
   yml_ephemeris_header = fname_header
END IF

! DE Header data file
IF (word1_ln == "DE_fname_data") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_data 
   yml_ephemeris_data_file = fname_data
END IF
! ----------------------------------------------------------------------	



END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------
else
     ! just force the creation of the ephemeris
     saved = yml_planetary_perturbations_enabled
     yml_planetary_perturbations_enabled = .true.
end if


if (.false.) then
print *, "planetary perturbations enabled = ", yml_planetary_perturbations_enabled
print *, "DE header = ", yml_ephemeris_header
print *, "DE data = ", yml_ephemeris_data_file
end if

! ----------------------------------------------------------------------
! Planetary/Lunar orbit data
! ---------------------------------------------------------------------- 
!If (FMOD_GRAV(2) == 1) Then
If (yml_planetary_perturbations_enabled) Then

! Planetary/Lunar precise ephemeris DE: Data files merging
fname_out = 'DE.430' 
CALL CATfile (yml_ephemeris_header,yml_ephemeris_data_file,fname_out)

! DE ephemeris data processing
! Store selected DE data to global variables via the module mdl_planets.f90 
CALL asc2eph (fname_out)

! Set the GM gravity constants of the solar system bodies as global variables via the module mdl_planets.f90
CALL GM_de

End If
! End of parameters setting for Planetary/Lunar orbit data
! ----------------------------------------------------------------------

if (yaml_found) yml_planetary_perturbations_enabled = saved


END
