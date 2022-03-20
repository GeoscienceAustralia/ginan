SUBROUTINE prm_gravity (PRMfname)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_gravity.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read/Set the parameterization regarding the Earth gravity field
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Cooperative Research Centre for Spatial Information, Australia
! Created:  5 April 2018
! ----------------------------------------------------------------------
        
        
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      !USE mdl_gfc
      USE m_gfc
      USE m_gfc3
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
! Gravity Field model variables
      INTEGER (KIND = prec_int8) :: Ntrunc, Ntrunc_tv 
      INTEGER (KIND = prec_int8) :: Nmax, Mmax 
      !INTEGER (KIND = prec_int8) :: n_max, m_max
      INTEGER (KIND = prec_int2) :: sigma_shc, gfc_opt
      CHARACTER (LEN=300) :: gfmfilename
      REAL (KIND = prec_q) :: mjd_t
      REAL (KIND = prec_q) :: GM_gfc, ae_gfc
      INTEGER (KIND = prec_int8) :: Nmax_gfc 
      CHARACTER (LEN=50) :: tide_gfc  
! Spherical Harmonic Coefficients (SHC) lower triangular matrices (Dynamic Allocatable arrays)
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Cnm
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Snm
! Errors/Variances of SHC (Dynamic Allocatable arrays)
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: sCnm
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: sSnm  
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

! ----------------------------------------------------------------------
! Gravitational forces considered: 
! ----------------------------------------------------------------------
! FMOD_GRAV setting (global variable in the module mdl_param.f03)
! FMOD_GRAV(1) : Earth gravity Field
! 1. FMOD_GRAV(i)=1 : Effect is considered
! 2. FMOD_GRAV(i)=0 : Effect is neglected 
!
! Gravity Field
!IF (word1_ln == "Gravity_field") THEN
!   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ForceMod 
      !FMOD_GRAV(1) = ForceMod
      !print *,"ForceMod", ForceMod
!END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth Gravity Field
! ----------------------------------------------------------------------
! Gravity Field model options:
! 0. Central gravity field (Keplerian orbit only)     : gravity_model = 0
! 1. Static global gravity field model                    : gravity_model = 1
! 2. Time-variable global gravity field model         : gravity_model = 2
! 3. IERS conventional geopotential model                   : gravity_model = 3
IF (word1_ln == "gravity_model") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, gfc_opt 
   FMOD_GRAVFIELD = 1
   yml_gravity_model = gfc_opt
   if (gfc_opt == CENTRAL_MODEL) then
      FMOD_GRAVFIELD = 0
   end if
END IF

! FMOD_GRAVFIELD global variable in mdl_param.f03


! Gravity Field model
IF (word1_ln == "gravity_model_filename") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, gfmfilename 
   yml_gravity_filename = gfmfilename
END IF

! Degree truncation limit (Spherical harmonics expansion series)
IF (word1_ln == "degree_max") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Nmax 
      Ntrunc = Nmax
   yml_gravity_max_degree = Nmax
END IF

! Degree truncation limit for the time-variable coefficients (case: gfc_opt=2)
IF (word1_ln == "degree_max_timevar") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Nmax 
      Ntrunc_tv = Nmax
   yml_gravity_time_max_degree = Nmax
END IF
        
! ----------------------------------------------------------------------      



END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------
else
    FMOD_GRAVFIELD = 1
    if (yml_gravity_model == CENTRAL_MODEL) FMOD_GRAVFIELD = 0
    yml_gravity_time_max_degree = yml_eqm_gravity_time_max_degree
    yml_gravity_max_degree = yml_eqm_gravity_max_degree
    Ntrunc = yml_gravity_max_degree
    Ntrunc_tv = yml_gravity_time_max_degree
end if

! Errors of spherical harmonic coefficients: Store errors in arrays or Not
sigma_shc = 0  

! ----------------------------------------------------------------------
! Earth Gravity Field
! ----------------------------------------------------------------------
IF (FMOD_GRAVFIELD == 0) then

GFM_GM = GM_global
GFM_ae = Earth_radius
GFM_tide = 'tide_free'

Else 

if (.false.) then
print *, "gravity model = ", yml_gravity_model
print *, "gravity file = ", yml_gravity_filename
print *, "max degree = ", yml_gravity_max_degree
print *, "max time degree = ", yml_gravity_time_max_degree
print *, "non central gravity field = ", FMOD_GRAVFIELD
end if

! ----------------------------------------------------------------------
! Gravity model's data: Spherical Harmonic Coefficients (icgem format)
! ----------------------------------------------------------------------
      if (yml_gravity_model == STATIC_MODEL) then 
          CALL gfc1 (yml_gravity_filename, yml_gravity_max_degree, sigma_shc, GM_gfc,&
                  ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)
              
      else if (yml_gravity_model == TIME_MODEL) then    
              mjd_t = MJD_to ! Module mdl_param.f03
          CALL gfc2 (yml_gravity_filename,yml_gravity_max_degree,sigma_shc, mjd_t, &
                  yml_gravity_time_max_degree, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)
              
      else if (yml_gravity_model == IERS_MODEL) then    
              mjd_t = MJD_to ! Module mdl_param.f03
          CALL gfc3_iers (yml_gravity_filename, yml_gravity_max_degree, sigma_shc,&
                  mjd_t, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)
              
      end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Gravity model's global variables set in mdl_param.f03
GFM_GM = GM_gfc
GFM_ae = ae_gfc
GFM_degmax = Nmax_gfc
GFM_tide = tide_gfc

! Truncation maximum degree and order       
! Nmax
IF (yml_gravity_max_degree == -1) THEN
   GFM_Nmax = Nmax_gfc
ELSE
   GFM_Nmax = yml_gravity_max_degree
END IF  
! Mmax
GFM_Mmax = GFM_Nmax

ALLOCATE (GFM_Cnm(GFM_Nmax+1,GFM_Nmax+1), STAT = AllocateStatus)
ALLOCATE (GFM_Snm(GFM_Nmax+1,GFM_Nmax+1), STAT = AllocateStatus)
GFM_Cnm  = Cnm
GFM_Snm  = Snm

IF (sigma_shc /= 0) THEN        
      ALLOCATE (GFM_sCnm(GFM_Nmax+1,GFM_Nmax+1), STAT = AllocateStatus)
      ALLOCATE (GFM_sSnm(GFM_Nmax+1,GFM_Nmax+1), STAT = AllocateStatus)
      GFM_sCnm = sCnm
      GFM_sSnm = sSnm
END IF

END IF
! End of parameters setting for Gravity Field
! ----------------------------------------------------------------------
!print *,"gfmfilename: ",gfmfilename





END
