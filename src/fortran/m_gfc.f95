MODULE m_gfc


! ----------------------------------------------------------------------
! MODULE: m_gfc.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for the subroutines that read the gravity field model data 
! (parameters and spherical harmonic coefficients)
! 
! Subroutines contained within this module:
! - gfc1.f90: Static gravity field models
! - gfc2.f90: Time-variable gravity field models
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  24 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE                   
  
        
Contains


   

SUBROUTINE gfc1 (gfmfilename, n_trunc, sigma_shc, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)

! ----------------------------------------------------------------------
! SUBROUTINE: gfc1.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read gravity field model's .gfc data file in the ICGEM (International
!  Centre for Global Earth Models) format
! ----------------------------------------------------------------------
! Input arguments:
! - gfmfilename:  Data file name of the global Earth gravity field
!                             model in ICGEM format
! - n_trunc:      Degree truncation limit in reading the Spherical
!                       Harmonic Coefficients (SHC)
!                 Set equal to -1 for reading all the SHC
! - sigma_shc:          Set to any value or 0 for reading or not the
!                       coefficients errors (sigmaC and sigmaS)
! 
! Output arguments:
! - GM_gfc:             Earth gravity constant (m^3/sec^2)
! - ae_gfc:             Radius  (meters)
! - Nmax_gfc:           Gravity model's maximum degree
! - tide_gfc:           Gravity model's tide system: zero_tide or tide_free
! - Cnm:          Normalized spherical harmonics coefficients C (dynamic allocatable array)
! - Snm:          Normalized spherical harmonics coefficients S (dynamic allocatable array)
! - sCnm:               Errors (variances) of C spherical harmonics coefficients (dynamic allocatable array)
! - sSnm:               Errors (variances) of S spherical harmonics coefficients (dynamic allocatable array)
! ----------------------------------------------------------------------
! Remark:
!  Cnm and Snm arrays are formed into lower triangular matrices.
!  Coefficient Cnm corresponds to the matrix element Cnm(n+1,m+1)
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  September 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 24 October 2017:
!   The subroutine has been modified in order to use advantages of Fortran 2003 in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
!      USE mdl_gfc
      IMPLICIT NONE
        
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int2), INTENT(IN) :: n_trunc
      INTEGER (KIND = prec_int2), INTENT(IN) :: sigma_shc
      CHARACTER (*), INTENT(IN) :: gfmfilename
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: GM_gfc, ae_gfc
      INTEGER (KIND = prec_int8), INTENT(OUT) :: Nmax_gfc 
      CHARACTER (LEN=50), INTENT(OUT) :: tide_gfc  
        ! Dynamic Allocatable arrays
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Cnm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Snm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: sCnm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: sSnm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i
!       , read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
!       INTEGER (KIND = prec_int2) :: space_i
      INTEGER (KIND = prec_int2) :: AllocateStatus
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith       
      CHARACTER (LEN=150) :: word1_ln, word_i     
      INTEGER (KIND = prec_int8) :: n_ith, m_ith, n_limit
      REAL (KIND = prec_q) :: Cnm_i, Snm_i, sCnm_i, sSnm_i
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      UNIT_IN = 9                                                                         
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open .gfc file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (gfmfilename), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", gfmfilename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Read Head of .gfc file
! ----------------------------------------------------------------------
      i = 0
      DO
           READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
           i = i + 1
!          PRINT *, "READ Line (i,ios):", i, ios_line
! ----------------------------------------------------------------------
! 1st Word of Line ith
           READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
! ----------------------------------------------------------------------
! Header Start
         IF (word1_ln == "begin_of_head") THEN
!            PRINT *, "Begin of Head"            
         END IF
! Header End
         IF (word1_ln == "end_of_head") THEN
!            PRINT *, "End of Head"
            EXIT        
         END IF
! ----------------------------------------------------------------------
! Keywords for Gravity Field Model parameters
         IF (word1_ln == "earth_gravity_constant") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, GM_gfc 
         END IF
         IF (word1_ln == "radius") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ae_gfc 
         END IF
         IF (word1_ln == "max_degree") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Nmax_gfc 
         END IF
         IF (word1_ln == "tide_system") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, tide_gfc 
         END IF
! ----------------------------------------------------------------------
      END DO
! ----------------------------------------------------------------------
!       PRINT *, "GM_gfc =", GM_gfc        
!       PRINT *, "ae_gfc =", ae_gfc        
!       PRINT *, "Nmax_gfc =", Nmax_gfc          
!       PRINT *, "tide_gfc :", tide_gfc


! ----------------------------------------------------------------------
      IF (n_trunc < 0) THEN
         n_limit = Nmax_gfc
      ELSE
         n_limit = n_trunc
      END IF
! ----------------------------------------------------------------------
!      PRINT *, "n_limit=", n_limit

! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------
      ALLOCATE (Cnm(n_limit+1,n_limit+1), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: Cnm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF  
      Cnm = 0.0D0

      ALLOCATE (Snm(n_limit+1,n_limit+1), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: Snm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF  
      Snm = 0.0D0

      IF (sigma_shc /= 0) THEN   
        
      ALLOCATE (sCnm(n_limit+1,n_limit+1), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: sCnm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF 
      sCnm = 0.0D0

      ALLOCATE (sSnm(n_limit+1,n_limit+1), STAT = AllocateStatus) 
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: sSnm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF  
      sSnm = 0.0D0

      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read Spherical Harmonic Coefficients of .gfc file
! ----------------------------------------------------------------------
!      i = 0
      DO
           READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
           i = i + 1
!          PRINT *, "READ Line (i,ios):", i, ios_line
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
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Data
         IF (word1_ln == "gfc" .OR. word1_ln == "gfct") THEN
              READ (line_ith, * , IOSTAT=ios_data) word_i, n_ith, m_ith, Cnm_i, Snm_i, sCnm_i, sSnm_i    
!             PRINT *, "n", n_ith          
!             PRINT *, "m", m_ith          
!             PRINT *, "Cnm", Cnm_i        
!             PRINT *, "Snm", Snm_i        
!             PRINT *, "sCnm", sCnm_i            
!             PRINT *, "sSnm", sSnm_i           
            IF (n_ith <= n_limit) THEN
               Cnm(n_ith+1,m_ith+1) = Cnm_i
               Snm(n_ith+1,m_ith+1) = Snm_i
               IF (sigma_shc /= 0) THEN
                  sCnm(n_ith+1,m_ith+1) = sCnm_i
                  sSnm(n_ith+1,m_ith+1) = sSnm_i
               END IF
            END IF
         END IF
! ----------------------------------------------------------------------
      END DO
! ----------------------------------------------------------------------

      CLOSE (UNIT=UNIT_IN)
        
END SUBROUTINE





SUBROUTINE gfc2 (gfmfilename,n_trunc,sigma_shc, mjd_t, Ntv_trunc, GM_gfc, ae_gfc, Nmax_gfc, tide_gfc, Cnm, Snm, sCnm, sSnm)

! ----------------------------------------------------------------------
! SUBROUTINE: gfc2.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read gravity field model's .gfc data file including the time-variable
!  coefficients (icgem format)
! ----------------------------------------------------------------------
! Input arguments:
! - gfmfilename:        Data file name of the global Earth gravity field model in ICGEM format
! - n_trunc:            Degree truncation limit for the Spherical Harmonic Coefficients (SHC)
!                       (set to negative values for reading all SHC)
! - sigma_shc:          Set to any value or 0 for reading or not the
!                       coefficients errors (sigmaC and sigmaS)
! - mjd_t:                    Modified Julian Day Number of the input epoch
!                                   including the fraction of the day. Time-variable 
!                                   coefficients are computed at this epoch.
! - Ntv_trunc > 0       Degree truncation limit for the time-variable SHC 
!   Ntv_trunc = 0       Read the static part only ignoring the time-variable SHC
!   Ntv_trunc < 0       Read the time-variable SHC up to the maximum degree 
! 
! Output arguments:
! - GM_gfc:             Earth gravity constant (m^3/sec^2)
! - ae_gfc:             Radius  (meters)
! - Nmax_gfc:           Gravity model's maximum degree
! - tide_gfc:           Gravity model's tide system: zero_tide or tide_free
! - Cnm:          Normalized spherical harmonics coefficients C (dynamic allocatable array)
! - Snm:          Normalized spherical harmonics coefficients S (dynamic allocatable array)
! - sCnm:               Errors (variances) of C spherical harmonics coefficients (dynamic allocatable array)
! - sSnm:               Errors (variances) of S spherical harmonics coefficients (dynamic allocatable array)
! ----------------------------------------------------------------------
! Remark:
!  Cnm and Snm arrays are formed into lower triangular matrices.
!  Coefficient Cnm corresponds to the matrix element Cnm(n+1,m+1)
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  January 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 25 October 2017:
!   The subroutine has been modified in order to use advantages of Fortran 2003 in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
!      USE mdl_gfc
      IMPLICIT NONE
        
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int2), INTENT(IN) :: n_trunc, Ntv_trunc
      INTEGER (KIND = prec_int2), INTENT(IN) :: sigma_shc
      CHARACTER (*), INTENT(IN) :: gfmfilename
      REAL (KIND = prec_q), INTENT(IN) :: mjd_t
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: GM_gfc, ae_gfc
      INTEGER (KIND = prec_int8), INTENT(OUT) :: Nmax_gfc 
      CHARACTER (LEN=50), INTENT(OUT) :: tide_gfc  
        ! Dynamic Allocatable arrays
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Cnm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Snm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: sCnm
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: sSnm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i
!       , read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
!       INTEGER (KIND = prec_int2) :: space_i
      INTEGER (KIND = prec_int2) :: AllocateStatus
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith       
      CHARACTER (LEN=150) :: word1_ln, word_i, t0       
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: n_ith, m_ith, n_limit, Nmax_tv, n_i,m_i
      REAL (KIND = prec_q) :: Cnm_i, Snm_i, sCnm_i, sSnm_i
      REAL (KIND = prec_q) :: Cnm_t0, Snm_t0,sCnm_t0, sSnm_t0
      REAL (KIND = prec_q) :: Cnm_trend, Snm_trend, sCnm_trend, sSnm_trend
      REAL (KIND = prec_q) :: Cnm_acos, Snm_acos, sCnm_acos, sSnm_acos
      REAL (KIND = prec_q) :: Cnm_asin, Snm_asin, sCnm_asin, sSnm_asin
      REAL (KIND = prec_q) :: T_days, period, delta_t
! ----------------------------------------------------------------------
      INTEGER Y_t0, M_t0, D_t0, J_flag
      DOUBLE PRECISION DJM0, mjd_t0
! ----------------------------------------------------------------------

        
! ----------------------------------------------------------------------
      UNIT_IN = 9                                                                         
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open .gfc file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (gfmfilename), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", gfmfilename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Read Head of .gfc file
! ----------------------------------------------------------------------
      i = 0
      DO
           READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
           i = i + 1
!          PRINT *, "READ Line (i,ios):", i, ios_line
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
           READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Header Start
         IF (word1_ln == "begin_of_head") THEN
!            PRINT *, "Begin of Head"            
         END IF
! Header End
         IF (word1_ln == "end_of_head") THEN
!            PRINT *, "End of Head"
            EXIT        
         END IF
! ----------------------------------------------------------------------
! Keywords for Gravity Field Model parameters
         IF (word1_ln == "earth_gravity_constant") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, GM_gfc 
         END IF
         IF (word1_ln == "radius") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ae_gfc 
         END IF
         IF (word1_ln == "max_degree") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Nmax_gfc 
         END IF
         IF (word1_ln == "tide_system") THEN
              READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, tide_gfc 
         END IF
! ----------------------------------------------------------------------
      END DO
! ----------------------------------------------------------------------
       !PRINT *, "GM_gfc =", GM_gfc        
       !PRINT *, "ae_gfc =", ae_gfc        
       !PRINT *, "Nmax_gfc =", Nmax_gfc          
       !PRINT *, "tide_gfc : ", tide_gfc


! ----------------------------------------------------------------------
      IF (n_trunc < 0) THEN
         n_limit = Nmax_gfc
      ELSE
         n_limit = n_trunc
      END IF
        
      IF (Ntv_trunc < 0) THEN
         Nmax_tv = n_limit
      ELSE
         Nmax_tv = Ntv_trunc
      END IF
        
      IF (Ntv_trunc > n_limit) THEN
         Nmax_tv = n_limit
      END IF
! ----------------------------------------------------------------------
!      PRINT *, "n_limit, Nmax_tv", n_limit, Nmax_tv


! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------  
      ALLOCATE (Cnm(n_limit+1,n_limit+1), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: Cnm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF  
        
      ALLOCATE (Snm(n_limit+1,n_limit+1), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: Snm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF  
        
      IF (sigma_shc /= 0) THEN        
        
      ALLOCATE (sCnm(n_limit+1,n_limit+1), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: sCnm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF      
            
      ALLOCATE (sSnm(n_limit+1,n_limit+1), STAT = AllocateStatus) 
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE gfc1 in m_gfc.f03"
         PRINT *, "Error: Allocatable Array: sSnm, Nmax =", n_limit
!         STOP "*** Not enough memory ***"
      END IF  
            
      sCnm = 0.0D0
      sSnm = 0.0D0
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Set the Cnm,Snm triangular matrices to zero matrices
! ----------------------------------------------------------------------
      DO n_i = 0 , n_limit
            DO m_i = 0 , n_limit
                  Cnm (n_i+1,m_i+1) = 0D0
                  Snm (n_i+1,m_i+1) = 0D0
            END DO
      END DO        
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read Spherical Harmonic Coefficients of .gfc file
! ----------------------------------------------------------------------
!      i = 0
      DO
           READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
           i = i + 1
!          PRINT *, "READ Line (i,ios):", i, ios_line
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
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Data
! ----------------------------------------------------------------------
! Static gravity field coefficients
         IF (word1_ln == "gfc") THEN
              READ (line_ith, * , IOSTAT=ios_data) word_i, n_ith, m_ith, Cnm_i, Snm_i, sCnm_i, sSnm_i    
            IF (n_ith <= n_limit) THEN
               Cnm(n_ith+1,m_ith+1) = Cnm_i
               Snm(n_ith+1,m_ith+1) = Snm_i
               IF (sigma_shc /= 0) THEN
                  sCnm(n_ith+1,m_ith+1) = sCnm_i
                  sSnm(n_ith+1,m_ith+1) = sSnm_i
               END IF
            END IF
! ----------------------------------------------------------------------
! Time-variable gravity field coefficients                  
         ELSE IF (word1_ln == "gfct") THEN
                  ! gfct
              READ (line_ith,FMT= *, IOSTAT=ios_data) word_i, n_ith, m_ith, Cnm_t0, Snm_t0, sCnm_t0, sSnm_t0, t0
      !PRINT *, "Line:", i , TRIM (word_i), n_ith, m_ith
      !PRINT *, "Line:", i, TRIM (t0)
                  ! Read next (3) lines for "trend,acos,asin" coefficients  
                  ! Trend
              READ (UNIT=UNIT_IN,FMT= *,IOSTAT=ios_line) word_i, n_ith, m_ith, Cnm_trend, Snm_trend, sCnm_trend, sSnm_trend
              i = i + 1
      !PRINT *, "Line:", i, TRIM (word_i), n_ith, m_ith
                  ! acos
              READ (UNIT=UNIT_IN,FMT= *,IOSTAT=ios_line) word_i, n_ith, m_ith, Cnm_acos, Snm_acos, sCnm_acos, sSnm_acos, period
              i = i + 1
      !PRINT *, "Line:", i, TRIM (word_i), n_ith, m_ith
                  ! asin
              READ (UNIT=UNIT_IN,FMT= *,IOSTAT=ios_line) word_i, n_ith, m_ith, Cnm_asin, Snm_asin, sCnm_asin, sSnm_asin, period
              i = i + 1
      !PRINT *, "Line:", i, TRIM (word_i), n_ith, m_ith
! ----------------------------------------------------------------------
! Compute Cnm,Snm at the input epoch
            IF (n_ith <= Nmax_tv) THEN
! ----------------------------------------------------------------------
                        !delta_t = (t_ys - t0_ys) / period
                        T_days = period * 365.25D0
                        read( t0(1:4), * ) Y_t0  
                        read( t0(5:6), * ) M_t0  
                        read( t0(7:8), * ) D_t0  
                        CALL iau_CAL2JD ( Y_t0, M_t0, D_t0, DJM0, mjd_t0, J_flag )
                        !PRINT *, "MJD t0", mjd_t0, TRIM (t0)
                        delta_t = (mjd_t - mjd_t0) / T_days             
! ----------------------------------------------------------------------
! V(t) = gfct + trnd * (t-t0)/T + acos*cos( 2pi * (t-t0)/T ) + asin*sin( 2pi * (t-t0)/T )
                        Cnm_i = Cnm_t0 + Cnm_trend * delta_t &
                                       + Cnm_acos * cos(2.0D0 * PI_global * delta_t) &
                                       + Cnm_asin * sin(2.0D0 * PI_global * delta_t)
                        Snm_i = Snm_t0 + Snm_trend * delta_t &
                                       + Snm_acos * cos(2.0D0 * PI_global * delta_t) &
                                             + Snm_asin * sin(2.0D0 * PI_global * delta_t)
! ----------------------------------------------------------------------
            ELSE IF (n_ith > Nmax_tv .AND. n_ith <= n_limit) THEN
                        Cnm_i = Cnm_t0
                        Snm_i = Snm_t0
            END IF                  
! ----------------------------------------------------------------------      
            IF (n_ith <= n_limit) THEN
               Cnm(n_ith+1,m_ith+1) = Cnm_i
               Snm(n_ith+1,m_ith+1) = Snm_i
               IF (sigma_shc /= 0) THEN
                  sCnm(n_ith+1,m_ith+1) = sCnm_i
                  sSnm(n_ith+1,m_ith+1) = sSnm_i
               END IF
               !PRINT *,"n,m", n_ith, m_ith            
               !PRINT *,"Cnm,Snm", Cnm_i, Snm_i              
            END IF                  
! ----------------------------------------------------------------------
         END IF
! ----------------------------------------------------------------------
!             PRINT *, "n", n_ith          
!             PRINT *, "m", m_ith          
!             PRINT *, "Cnm", Cnm_i        
!             PRINT *, "Snm", Snm_i        
!             PRINT *, "sCnm", sCnm_i            
!             PRINT *, "sSnm", sSnm_i           
! ----------------------------------------------------------------------
      END DO
! ----------------------------------------------------------------------

      CLOSE (UNIT=UNIT_IN)

END SUBROUTINE




End

