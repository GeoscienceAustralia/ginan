SUBROUTINE tides_fes2004(FESxxfname)


! ----------------------------------------------------------------------
! SUBROUTINE: tides_fes2004.f90
! ----------------------------------------------------------------------
! Purpose:
! Ocean Tide Model: FES2004
!  Read data from the FES2004 ocean tide model and obtain the Delaunay variables
!  multipliers for each tide wave via the Doodson number
! ----------------------------------------------------------------------
! Input arguments
! - FESxxfname      : FES2004 data file name (IERS file: fes2004_Cnm-Snm.dat) 
!
! Output arguments:
! Stored as allocatable arrays via module : mdl_tides.f90
! - Delaunay_FES 	: Delaunay variables multipliers for FES2004 tidal waves
! - dCnm_p          : Spherical harmonic coefficients of ocean tide model to
!   dSnm_p            be used in Eq.6.15 of IERS Conventions 2010
!   dCnm_m            Coefficients are stored in four 3D arrays
!   dSnm_m
! ----------------------------------------------------------------------
! Remark:
! FES2004 Tidal constituents (waves) that are excluded: 
! Doodson Darw
! 455.555 M4   
!         T2   (coefficients are missing from the IERS data file)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            November 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 30 January 2019:
!   Modified in order to replace the fundamental arguments used in the 
!   basic formula of the tidal corrections. 
!   The Doodson arguments and multipliers are now applied instead of the Delaunay arguments.
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_tides
      USE mdl_param
      USE m_writearray
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=512), INTENT(IN) :: FESxxfname
! OUT
! mdl_tides.f90 : Allocatable arrays : Delaunay_FES, dCnm_p,dSnm_p, dCnm_m,dSnm_m
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: fes2004_cfunit
      REAL (KIND = prec_d) :: Ndelaunay_f_transp(8,71),Ndelaunay_f(71,8)
      REAL (KIND = prec_d), DIMENSION(8,18) :: Delaunay_FES_t
      REAL (KIND = prec_d), DIMENSION(7,18) :: Doodson_mult_T
      INTEGER (KIND = prec_int4) :: N_Delaunay, Nfrq, N_Doodson
      INTEGER (KIND = prec_int4) :: UNIT_IN,ios,ios_line,ios_data,AllocateStatus, i,space_i,i_Delaunay_FES, i_3Darray
      CHARACTER (LEN=150) :: line_ith, word1_ln  
      REAL (KIND = prec_q) :: doodson_no, dCp, dSp, dCm, dSm
      INTEGER (KIND = prec_int8) :: Nread, Mread, nmax
!       , iNmax, iMmax
      CHARACTER (LEN=5) :: Darw  
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCSnm_wrt
      CHARACTER (LEN=100) :: fname				
      CHARACTER (LEN=50) :: fname_id				
!       INTEGER (KIND = prec_int4) :: ifreq  


! ----------------------------------------------------------------------
! Ocean Tides model truncated maximum degree
! ----------------------------------------------------------------------
      nmax = 100
!nmax = OCEAN_Nmax
! Remark: Allocatable arrays for the model coefficients are formed based on the nmax value
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Coefficients Unit : 10^-12 (IERS Conventions 2010)
! Coefficients Unit : 10^-11 (IERS Conventions 2010, update 23/09/11)
      fes2004_cfunit = 1.0D-11
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! N : Five multipliers of the Delaunay variables (l l' F D Omega)
! N : Nj, j = 1:5
! Values are taken from Tables 6.5b, 6.5a, 6.5c
! Matrix format : Columns : Doodson_No. l l' F D Omega   Amp.(ip) Amp.(op)
! Last two columns are not required

Ndelaunay_f_transp  = RESHAPE ( (/                                      &
55.565D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,  16.6D0,  -6.7D0,  &
55.575D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,  -0.1D0,   0.1D0,  &
56.554D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  -1.2D0,   0.8D0,  &
57.555D0,  0.0D0,  0.0D0, -2.0D0,  2.0D0, -2.0D0,  -5.5D0,   4.3D0,  &
57.565D0,  0.0D0,  0.0D0, -2.0D0,  2.0D0, -1.0D0,   0.1D0,  -0.1D0,  &
58.554D0,  0.0D0, -1.0D0, -2.0D0,  2.0D0, -2.0D0,  -0.3D0,   0.2D0,  &
63.655D0,  1.0D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,  -0.3D0,   0.7D0,  &
65.445D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0, -1.0D0,   0.1D0,  -0.2D0,  &
65.455D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  -1.2D0,   3.7D0,  &
65.465D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,   0.1D0,  -0.2D0,  &
65.655D0,  1.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   0.1D0,  -0.2D0,  &
73.555D0,  0.0D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,   0.0D0,   0.6D0,  &
75.355D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   0.0D0,   0.3D0,  &
75.555D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   0.6D0,   6.3D0,  &
75.565D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0, -1.0D0,   0.2D0,   2.6D0,  &
75.575D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,  0.0D0,   0.0D0,   0.2D0,  &
83.655D0,  1.0D0,  0.0D0, -2.0D0, -2.0D0, -2.0D0,   0.1D0,   0.2D0,  &
85.455D0, -1.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   0.4D0,   1.1D0,  &
85.465D0, -1.0D0,  0.0D0, -2.0D0,  0.0D0, -1.0D0,   0.2D0,   0.5D0,  &
93.555D0,  0.0D0,  0.0D0, -2.0D0, -2.0D0, -2.0D0,   0.1D0,   0.2D0,  &
95.355D0, -2.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   0.1D0,   0.1D0,  &  
125.755D0,  2.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -0.1D0,    0.0D0,  &
127.555D0,  0.0D0,  0.0D0,  2.0D0,  2.0D0,  2.0D0,   -0.1D0,    0.0D0,  &
135.645D0,  1.0D0,  0.0D0,  2.0D0,  0.0D0,  1.0D0,   -0.1D0,    0.0D0,  &
135.655D0,  1.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -0.7D0,    0.1D0,  &
137.455D0, -1.0D0,  0.0D0,  2.0D0,  2.0D0,  2.0D0,   -0.1D0,    0.0D0,  &
145.545D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  1.0D0,   -1.3D0,    0.1D0,  &
145.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -6.8D0,    0.6D0,  &
147.555D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,    0.1D0,    0.0D0,  &
153.655D0,  1.0D0,  0.0D0,  2.0D0, -2.0D0,  2.0D0,    0.1D0,    0.0D0,  &
155.445D0, -1.0D0,  0.0D0,  2.0D0,  0.0D0,  1.0D0,    0.1D0,    0.0D0,  &
155.455D0, -1.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,    0.4D0,    0.0D0,  &
155.655D0,  1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,    1.3D0,   -0.1D0,  &
155.665D0,  1.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,    0.3D0,    0.0D0,  &
157.455D0, -1.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,    0.3D0,    0.0D0,  &
157.465D0, -1.0D0,  0.0D0,  0.0D0,  2.0D0,  1.0D0,    0.1D0,    0.0D0,  &
162.556D0,  0.0D0,  1.0D0,  2.0D0, -2.0D0,  2.0D0,   -1.9D0,    0.1D0,  &
163.545D0,  0.0D0,  0.0D0,  2.0D0, -2.0D0,  1.0D0,    0.5D0,    0.0D0,  &
163.555D0,  0.0D0,  0.0D0,  2.0D0, -2.0D0,  2.0D0,  -43.4D0,    2.9D0,  &
164.554D0,  0.0D0, -1.0D0,  2.0D0, -2.0D0,  2.0D0,    0.6D0,    0.0D0,  &
164.556D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0,  0.0D0,    1.6D0,   -0.1D0,  &
165.345D0, -2.0D0,  0.0D0,  2.0D0,  0.0D0,  1.0D0,    0.1D0,    0.0D0,  &
165.535D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, -2.0D0,    0.1D0,    0.0D0,  &
165.545D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, -1.0D0,   -8.8D0,    0.5D0,  &
165.555D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  470.9D0,  -30.2D0,  &
165.565D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,   68.1D0,   -4.6D0,  &
165.575D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,   -1.6D0,    0.1D0,  &
166.455D0, -1.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,    0.1D0,    0.0D0,  &
166.544D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0, -1.0D0,   -0.1D0,    0.0D0,  &
166.554D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  -20.6D0,   -0.3D0,  &
166.556D0,  0.0D0,  1.0D0, -2.0D0,  2.0D0, -2.0D0,    0.3D0,    0.0D0,  &
166.564D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  1.0D0,   -0.3D0,    0.0D0,  &
167.355D0, -2.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,   -0.2D0,    0.0D0,  &
167.365D0, -2.0D0,  0.0D0,  0.0D0,  2.0D0,  1.0D0,   -0.1D0,    0.0D0,  &
167.555D0,  0.0D0,  0.0D0, -2.0D0,  2.0D0, -2.0D0,   -5.0D0,    0.3D0,  &
167.565D0,  0.0D0,  0.0D0, -2.0D0,  2.0D0, -1.0D0,    0.2D0,    0.0D0,  &
168.554D0,  0.0D0, -1.0D0, -2.0D0,  2.0D0, -2.0D0,   -0.2D0,    0.0D0,  &
173.655D0,  1.0D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,   -0.5D0,    0.0D0,  &
173.665D0,  1.0D0,  0.0D0,  0.0D0, -2.0D0,  1.0D0,   -0.1D0,    0.0D0,  &
175.445D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0, -1.0D0,    0.1D0,    0.0D0,  &
175.455D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   -2.1D0,    0.1D0,  &
175.465D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,   -0.4D0,    0.0D0,  &
183.555D0,  0.0D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,   -0.2D0,    0.0D0,  &
185.355D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
185.555D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   -0.6D0,    0.0D0,  &
185.565D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0, -1.0D0,   -0.4D0,    0.0D0,  &
185.575D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
195.455D0, -1.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   -0.1D0,    0.0D0,  &
195.465D0, -1.0D0,  0.0D0, -2.0D0,  0.0D0, -1.0D0,   -0.1D0,    0.0D0,  &
245.655D0,  1.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -0.3D0,    0.0D0,  & 
255.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -1.2D0,    0.0D0  &
/) , (/ 8 , 71 /) )  

Ndelaunay_f = TRANSPOSE (Ndelaunay_f_transp)



! ---------------------------------------------------------------------------
Delaunay_FES_t = RESHAPE ( (/                                        &
55.565D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,  16.6D0,  -6.7D0,  &
55.575D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,  -0.1D0,   0.1D0,  &
56.554D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  -1.2D0,   0.8D0,  &
57.555D0,  0.0D0,  0.0D0, -2.0D0,  2.0D0, -2.0D0,  -5.5D0,   4.3D0,  &
65.455D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  -1.2D0,   3.7D0,  &
75.555D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   0.6D0,   6.3D0,  &
85.455D0, -1.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   0.4D0,   1.1D0,  &
93.555D0,  0.0D0,  0.0D0, -2.0D0, -2.0D0, -2.0D0,   0.1D0,   0.2D0,  &
135.655D0,  1.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -0.7D0,    0.1D0,  &
145.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -6.8D0,    0.6D0,  &
163.555D0,  0.0D0,  0.0D0,  2.0D0, -2.0D0,  2.0D0,  -43.4D0,    2.9D0,  &
165.555D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  470.9D0,  -30.2D0,  &
235.755D0, -2.0D0,  0.0D0, -2.0D0,  0.0D0, -2.0D0,   -0.3D0,    0.0D0,  & 
245.655D0,  1.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -0.3D0,    0.0D0,  & 
255.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -1.2D0,    0.0D0,  &
273.555D0,  0.0D0,  0.0D0,  2.0D0, -2.0D0,  2.0D0,    0.0D0,    0.0D0,  &
275.555D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,    0.0D0,    0.0D0,  &
455.555D0,  0.0D0,  0.0D0,  4.0D0,  0.0D0,  4.0D0,    0.0D0,    0.0D0  &
/) , (/ 8 , 18 /) )  

! 255.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  2.0D0,   -1.2D0,    0.0D0,  &

! ---------------------------------------------------------------------------
! Allocate array
! ---------------------------------------------------------------------------
      N_Delaunay = SIZE (Delaunay_FES_t,DIM=1)
      Nfrq = SIZE (Delaunay_FES_t,DIM=2)
      !PRINT *,"tides_fes2004.f90 Nfrq", Nfrq
	  
      ALLOCATE (Delaunay_FES(Nfrq,N_Delaunay), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE tides_fes2004.f90"
         PRINT *, "Error: Allocatable Array: Delaunay_FES"
!         STOP "*** Not enough memory ***"
      END IF  
      Delaunay_FES = TRANSPOSE (Delaunay_FES_t)
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
Doodson_mult_T = RESHAPE ( (/                              &
55.565D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,  &
55.575D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  &
56.554D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0, -1.0D0,  &
57.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  &
65.455D0,  0.0D0,  1.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  &
75.555D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  &
85.455D0,  0.0D0,  3.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  &
93.555D0,  0.0D0,  4.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,  &
135.655D0,  1.0D0, -2.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0, &
145.555D0,  1.0D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, &
163.555D0,  1.0D0,  1.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0, &
165.555D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, &
235.755D0,  2.0D0, -2.0D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0, & 
245.655D0,  2.0D0, -1.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0, & 
255.555D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, &
273.555D0,  2.0D0,  2.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0, &
275.555D0,  2.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0, &
455.555D0,  4.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0 &
/) , (/ 7 , 18 /) )  

!55.565D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,  &
!55.575D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  &
!93.555D0,  0.0D0,  0.0D0, -2.0D0, -2.0D0, -2.0D0,   0.1D0,  &


! ---------------------------------------------------------------------------
! Allocate array
! ---------------------------------------------------------------------------
      N_Doodson = SIZE (Doodson_mult_T,DIM=1)
      Nfrq = SIZE (Doodson_mult_T,DIM=2)
      !PRINT *,"tides_fes2004.f90 Nfrq", Nfrq
	  
      ALLOCATE (Doodson_mult_glb(Nfrq,N_Doodson), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE tides_fes2004.f90"
         PRINT *, "Error: Allocatable Array: Doodson_mult_glb"
!         STOP "*** Not enough memory ***"
      END IF  
      Doodson_mult_glb = TRANSPOSE (Doodson_mult_T)
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
! Allocate Arrays	  
! ---------------------------------------------------------------------------	  
      ALLOCATE (dCnm_p(nmax+1,nmax+1,Nfrq), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE tides_fes2004.f90"
         PRINT *, "Error: Allocatable Array: dCnm_p | Nmax:", nmax
!         STOP "*** Not enough memory ***"
      END IF  
      dCnm_p = 0.d0

      ALLOCATE (dSnm_p(nmax+1,nmax+1,Nfrq), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE tides_fes2004.f90"
         PRINT *, "Error: Allocatable Array: dSnm_p | Nmax:", nmax
!         STOP "*** Not enough memory ***"
      END IF  
      dSnm_p = 0.d0  
      ALLOCATE (dCnm_m(nmax+1,nmax+1,Nfrq), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE tides_fes2004.f90"
         PRINT *, "Error: Allocatable Array: dCnm_m | Nmax:", nmax
!         STOP "*** Not enough memory ***"
      END IF  
      dCnm_m = 0.d0
      ALLOCATE (dSnm_m(nmax+1,nmax+1,Nfrq), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE tides_fes2004.f90"
         PRINT *, "Error: Allocatable Array: dSnm_m | Nmax:", nmax
!         STOP "*** Not enough memory ***"
      END IF  
      dSnm_m =0.d0
! ---------------------------------------------------------------------------



! ----------------------------------------------------------------------
! Read FES2004 data file
! ----------------------------------------------------------------------
! Open file
      UNIT_IN = 9  												
      OPEN (UNIT = UNIT_IN, FILE = TRIM (FESxxfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", FESxxfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------
! Read Header
      i = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
	     i = i + 1
  	     !PRINT *, "READ Line (i,ios):", i, ios_line
! 1st Word of Line ith
	     space_i = SCAN ( line_ith, " ")
         word1_ln = line_ith (1 : space_i-1)
         !PRINT *, "Line", i," space char i", space_i
         !PRINT *, " Word1: ", word1_ln, "Length", LEN(word1_ln)
! Header End
         IF (word1_ln == "Doodson") THEN
            !PRINT *, "End of Head"
            EXIT		
         END IF
      END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read Data
! ----------------------------------------------------------------------
!      i = 0
      DO
!	     READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
!  	     PRINT *, "READ Line (i,ios):", i, ios_line

!Doodson Darw  l   m    DelC+     DelS+       DelC-     DelS- 
! 55.565 Om1   2   0  -6.58128   0.00000    -0.00000  -0.00000
	     READ (UNIT=UNIT_IN, FMT=* , IOSTAT=ios_data) doodson_no, Darw, Nread, Mread, dCp, dSp, dCm, dSm

!Doodson Darw  n   m    Csin+     Ccos+       Csin-     Ccos-       C+   eps+      C-   eps-  
! 55.565 Om1   2   0  0.540594  0.000000    0.000000  0.000000   0.5406  90.000 0.0000   0.000
!	     READ (UNIT=UNIT_IN, FMT=* , IOSTAT=ios_data) doodson_no, Darw, Nread, Mread, Csin_p, Ccos_p, Csin_m, Ccos_m, C_p, eps_p, C_m, eps_m

	     i = i + 1
! ----------------------------------------------------------------------
! End of file
         IF (ios_data < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Tidal Wave (frequency): ID (1-18)
         DO i_Delaunay_FES = 1 , Nfrq
            IF ( abs(doodson_no - Delaunay_FES(i_Delaunay_FES,1)) < 10D-8 ) THEN			!999
               i_3Darray = i_Delaunay_FES
!               PRINT *, "i_3Darray=", i_3Darray
			   EXIT																			
            ELSE
               i_3Darray = -1 
            END IF
         END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Data
! 3D arrays: ixjxk : n x m x frq
         IF (i_3Darray > 0) THEN
            dCnm_p  (Nread+1, Mread+1, i_3Darray) = fes2004_cfunit * dCp
            dSnm_p  (Nread+1, Mread+1, i_3Darray) = fes2004_cfunit * dSp
            dCnm_m (Nread+1, Mread+1, i_3Darray) = fes2004_cfunit * dCm
            dSnm_m (Nread+1, Mread+1, i_3Darray) = fes2004_cfunit * dSm			
!print *," "
!print *,"Nread, Mread, i_3Darray, dCp, dSp, dCm, dSm", Nread, Mread, i_3Darray, dCp, dSp, dCm, dSm
!print *,"CS_p ", dCnm_p(Nread+1,Mread+1,i_3Darray), dSnm_p(Nread+1,Mread+1,i_3Darray)
!print *,"CS_m ", dCnm_m(Nread+1,Mread+1,i_3Darray), dSnm_m(Nread+1,Mread+1,i_3Darray)
!Call SLEEP (1)
         END IF
! ----------------------------------------------------------------------
      END DO
      CLOSE (UNIT=UNIT_IN)
 
		
END
