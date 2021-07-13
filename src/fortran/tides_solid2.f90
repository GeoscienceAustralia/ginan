SUBROUTINE tides_solid2(mjd, ut1_utc , dCnm,dSnm)


! ----------------------------------------------------------------------
! SUBROUTINE: tides_solid2.f90
! ----------------------------------------------------------------------
! Purpose:
! Solid Earth Tides (frequency dependent)
!  Solid Earth Tides (Step 2) based on IERS Conventions 2010.
!  tides_solid2 implements the Step2 that refers to the computation of the
!  frequency dependent corrections to the geopotential's spherical harmonic
!  coefficients.
! ----------------------------------------------------------------------
! Input arguments
! - mjd:			MJD in Terrestrial Time (TT) scale including the fraction 
!             		of the day 
! - ut1_utc:   		Time difference between UT1 and UTC (seconds)
!					See Remark 3
!
! Output arguments:
! - dCnm :			Cnm corrections matrix
! - dSnm :			Snm corrections matrix
! ----------------------------------------------------------------------
! Remark 1:
!  Computed dCnm and dSnm are formed into lower triangular matrices.
!  Coefficient dCnm corresponds to the matrix element dCnm(n+1,m+1).
!
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	9 November 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 27 October 2017:
!   The subroutine has been modified in order to call the subroutine gmst_iers.f03 
!   for the computation of the Greenwich mean sidereal time
! - Dr. Thomas Papanikolaou, 30 January 2019:
!   Modified in order to replace the fundamental arguments used in the 
!   basic formula of the tidal corrections. 
!   The Doodson arguments and multipliers are now applied instead of the Delaunay arguments.
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: mjd, ut1_utc

! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3,3) :: dCnm, dSnm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------  
      REAL (KIND = prec_q) :: thetag, thetaf
      REAL (KIND = prec_q) :: F1,F2,F3,F4,F5
      REAL (KIND = prec_d), DIMENSION(5) :: delaunay_arr
      REAL (KIND = prec_q) :: beta1, beta2, beta3, beta4, beta5, beta6
      REAL (KIND = prec_d), DIMENSION(6) :: doodson_arr
      REAL (KIND = prec_d) :: table65b(21,9) , table65b_transp(9,21)
      REAL (KIND = prec_d) :: table65a(48,9) , table65a_transp(9,48)
      REAL (KIND = prec_d), DIMENSION(2,8) :: table65c
      INTEGER (KIND = prec_int2) :: sz1, sz2, i, j
      REAL (KIND = prec_q) :: dC20, dC21,dS21, dC22,dS22
      REAL (KIND = prec_q) :: matrix_mult
      REAL (KIND = prec_q) :: pi
! ----------------------------------------------------------------------  

dCnm = 0.0D0
dCnm = 0.0D0
pi = PI_global																	

! ----------------------------------------------------------------------  
! Delaunay variables (in radians)
CALL delaunay (mjd , F1,F2,F3,F4,F5)
delaunay_arr = (/ F1 , F2 , F3 , F4 , F5 /)									
! ----------------------------------------------------------------------  

! ----------------------------------------------------------------------  
! Greenwich Mean Sidereal Time (GMST) in radians
Call gmst_iers(mjd, ut1_utc , thetag)
! ----------------------------------------------------------------------  

! ----------------------------------------------------------------------  
! Doodson arguments in radians
! ----------------------------------------------------------------------  
! N' = -Omega : Negative of the Mean Longitude of the Ascending Node of the Moon
beta5 = -1.0D0 * F5
! s = F+Omega : Mean Longitude of the Moon
beta2 = F3 + F5
! taph = thetag+p - s : Mean Lunar Time
beta1 = (thetag + pi) -1.0D0 * beta2 
! h = s - D : Mean Longitude of the Sun
beta3 = beta2 - F4
! p = s - l : Longitude of the Moon's mean perigee 
beta4 = beta2 - F1
! ps = s - D - l' : Longitude of the Sun's mean perigee 
beta6 = beta2 - F4 -F2

doodson_arr = (/ beta1 , beta2 , beta3 , beta4 , beta5, beta6 /)							
! ---------------------------------------------------------------------- 

! ----------------------------------------------------------------------  
! dC20 correction
! Table 6.5b
! Columns : Doodson_No. Doodson arguments multipliers (Coll.2-7)  Amp.(ip) Amp.(op)
table65b_transp  = RESHAPE ( (/                                      &
55.565D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,  16.6D0,  -6.7D0,  &
55.575D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  -0.1D0,   0.1D0,  &
56.554D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0, -1.0D0,  -1.2D0,   0.8D0,  &
57.555D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  -5.5D0,   4.3D0,  &
57.565D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,  1.0D0,  0.0D0,   0.1D0,  -0.1D0,  &
58.554D0,  0.0D0,  0.0D0,  3.0D0,  0.0D0,  0.0D0, -1.0D0,  -0.3D0,   0.2D0,  &
63.655D0,  0.0D0,  1.0D0, -2.0D0,  1.0D0,  0.0D0,  0.0D0,  -0.3D0,   0.7D0,  &
65.445D0,  0.0D0,  1.0D0,  0.0D0, -1.0D0, -1.0D0,  0.0D0,   0.1D0,  -0.2D0,  &
65.455D0,  0.0D0,  1.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,  -1.2D0,   3.7D0,  &
65.465D0,  0.0D0,  1.0D0,  0.0D0, -1.0D0,  1.0D0,  0.0D0,   0.1D0,  -0.2D0,  &
65.655D0,  0.0D0,  1.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0,   0.1D0,  -0.2D0,  &
73.555D0,  0.0D0,  2.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,   0.0D0,   0.6D0,  &
75.355D0,  0.0D0,  2.0D0,  0.0D0, -2.0D0,  0.0D0,  0.0D0,   0.0D0,   0.3D0,  &
75.555D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   0.6D0,   6.3D0,  &
75.565D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,   0.2D0,   2.6D0,  &
75.575D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,   0.0D0,   0.2D0,  &
83.655D0,  0.0D0,  3.0D0, -2.0D0,  1.0D0,  0.0D0,  0.0D0,   0.1D0,   0.2D0,  &
85.455D0,  0.0D0,  3.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,   0.4D0,   1.1D0,  &
85.465D0,  0.0D0,  3.0D0,  0.0D0, -1.0D0,  1.0D0,  0.0D0,   0.2D0,   0.5D0,  &
93.555D0,  0.0D0,  4.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,   0.1D0,   0.2D0,  &
95.355D0,  0.0D0,  4.0D0,  0.0D0, -2.0D0,  0.0D0,  0.0D0,   0.1D0,   0.1D0  /) , (/ 9 , 21 /) )  

table65b = TRANSPOSE (table65b_transp)

! dC20 computation
sz1 = SIZE (table65b,DIM=1)
sz2 = SIZE (table65b,DIM=2)
!PRINT *,"sz1",sz1

dC20 = 0.0D0
DO i = 1 , sz1
    ! thetaf (in radians)
      matrix_mult = 0.0D0
      DO j = 1 , 6
	     matrix_mult = matrix_mult + ( table65b(i,j+1) * doodson_arr(j) )
      END DO
    !thetaf = -1.0D0 * matrix_mult   						          			
    thetaf = matrix_mult   						          			

    dC20 = dC20 + table65b(i,8) * 1.0D-12 * cos(thetaf) - table65b(i,9) * 1.0D-12 * sin(thetaf)
END DO
dCnm(2+1,0+1) = dC20
!PRINT *,"dC20",dC20
! ----------------------------------------------------------------------  

! ----------------------------------------------------------------------  
! dC21,dS21 corrections 
! Table 6.5a
! Columns : Doodson_No. Doodson arguments multipliers (Coll.2-7)  Amp.(ip) Amp.(op)
table65a_transp  = RESHAPE ( (/                                         &
125.755D0,  1.0D0, -3.0D0,  0.0D0,  2.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
127.555D0,  1.0D0, -3.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
135.645D0,  1.0D0, -2.0D0,  0.0D0,  1.0D0, -1.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
135.655D0,  1.0D0, -2.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0,   -0.7D0,    0.1D0,  &
137.455D0,  1.0D0, -2.0D0,  2.0D0, -1.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
145.545D0,  1.0D0, -1.0D0,  0.0D0,  0.0D0, -1.0D0,  0.0D0,   -1.3D0,    0.1D0,  &
145.555D0,  1.0D0, -1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   -6.8D0,    0.6D0,  &
147.555D0,  1.0D0, -1.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,    0.1D0,    0.0D0,  &
153.655D0,  1.0D0,  0.0D0, -2.0D0,  1.0D0,  0.0D0,  0.0D0,    0.1D0,    0.0D0,  &
155.445D0,  1.0D0,  0.0D0,  0.0D0, -1.0D0, -1.0D0,  0.0D0,    0.1D0,    0.0D0,  &
155.455D0,  1.0D0,  0.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,    0.4D0,    0.0D0,  &
155.655D0,  1.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0,    1.3D0,   -0.1D0,  &
155.665D0,  1.0D0,  0.0D0,  0.0D0,  1.0D0,  1.0D0,  0.0D0,    0.3D0,    0.0D0,  &
157.455D0,  1.0D0,  0.0D0,  2.0D0, -1.0D0,  0.0D0,  0.0D0,    0.3D0,    0.0D0,  &
157.465D0,  1.0D0,  0.0D0,  2.0D0, -1.0D0,  1.0D0,  0.0D0,    0.1D0,    0.0D0,  &
162.556D0,  1.0D0,  1.0D0, -3.0D0,  0.0D0,  0.0D0,  1.0D0,   -1.9D0,    0.1D0,  &
163.545D0,  1.0D0,  1.0D0, -2.0D0,  0.0D0, -1.0D0,  0.0D0,    0.5D0,    0.0D0,  &
163.555D0,  1.0D0,  1.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,  -43.4D0,    2.9D0,  &
164.554D0,  1.0D0,  1.0D0, -1.0D0,  0.0D0,  0.0D0, -1.0D0,    0.6D0,    0.0D0,  &
164.556D0,  1.0D0,  1.0D0, -1.0D0,  0.0D0,  0.0D0,  1.0D0,    1.6D0,   -0.1D0,  &
165.345D0,  1.0D0,  1.0D0,  0.0D0, -2.0D0, -1.0D0,  0.0D0,    0.1D0,    0.0D0,  &
165.535D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0, -2.0D0,  0.0D0,    0.1D0,    0.0D0,  &
165.545D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0, -1.0D0,  0.0D0,   -8.8D0,    0.5D0,  &
165.555D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  470.9D0,  -30.2D0,  &
165.565D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,   68.1D0,   -4.6D0,  &
165.575D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,   -1.6D0,    0.1D0,  &
166.455D0,  1.0D0,  1.0D0,  1.0D0, -1.0D0,  0.0D0,  0.0D0,    0.1D0,    0.0D0,  &
166.544D0,  1.0D0,  1.0D0,  1.0D0,  0.0D0, -1.0D0, -1.0D0,   -0.1D0,    0.0D0,  &
166.554D0,  1.0D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0, -1.0D0,  -20.6D0,   -0.3D0,  &
166.556D0,  1.0D0,  1.0D0,  1.0D0,  0.0D0,  0.0D0,  1.0D0,    0.3D0,    0.0D0,  &
166.564D0,  1.0D0,  1.0D0,  1.0D0,  0.0D0,  1.0D0, -1.0D0,   -0.3D0,    0.0D0,  &
167.355D0,  1.0D0,  1.0D0,  2.0D0, -2.0D0,  0.0D0,  0.0D0,   -0.2D0,    0.0D0,  &
167.365D0,  1.0D0,  1.0D0,  2.0D0, -2.0D0,  1.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
167.555D0,  1.0D0,  1.0D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,   -5.0D0,    0.3D0,  &
167.565D0,  1.0D0,  1.0D0,  2.0D0,  0.0D0,  1.0D0,  0.0D0,    0.2D0,    0.0D0,  &
168.554D0,  1.0D0,  1.0D0,  3.0D0,  0.0D0,  0.0D0, -1.0D0,   -0.2D0,    0.0D0,  &
173.655D0,  1.0D0,  2.0D0, -2.0D0,  1.0D0,  0.0D0,  0.0D0,   -0.5D0,    0.0D0,  &
173.665D0,  1.0D0,  2.0D0, -2.0D0,  1.0D0,  1.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
175.445D0,  1.0D0,  2.0D0,  0.0D0, -1.0D0, -1.0D0,  0.0D0,    0.1D0,    0.0D0,  &
175.455D0,  1.0D0,  2.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,   -2.1D0,    0.1D0,  &
175.465D0,  1.0D0,  2.0D0,  0.0D0, -1.0D0,  1.0D0,  0.0D0,   -0.4D0,    0.0D0,  &
183.555D0,  1.0D0,  3.0D0, -2.0D0,  0.0D0,  0.0D0,  0.0D0,   -0.2D0,    0.0D0,  &
185.355D0,  1.0D0,  3.0D0,  0.0D0, -2.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
185.555D0,  1.0D0,  3.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   -0.6D0,    0.0D0,  &
185.565D0,  1.0D0,  3.0D0,  0.0D0,  0.0D0,  1.0D0,  0.0D0,   -0.4D0,    0.0D0,  &
185.575D0,  1.0D0,  3.0D0,  0.0D0,  0.0D0,  2.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
195.455D0,  1.0D0,  4.0D0,  0.0D0, -1.0D0,  0.0D0,  0.0D0,   -0.1D0,    0.0D0,  &
195.465D0,  1.0D0,  4.0D0,  0.0D0, -1.0D0,  1.0D0,  0.0D0,   -0.1D0,    0.0D0   &
/) , (/ 9 , 48 /) )  

table65a = TRANSPOSE (table65a_transp)

! dC21 dS21 computations
sz1 = SIZE (table65a,DIM=1)
sz2 = SIZE (table65a,DIM=2)
!PRINT *,"sz1", sz1

dC21 = 0.0D0
dS21 = 0.0D0
DO i = 1 , sz1
    ! thetaf (in radians)
      matrix_mult = 0.0D0
      DO j = 1 , 6
	     matrix_mult = matrix_mult + ( table65a(i,j+1) * doodson_arr(j) )
      END DO
      !thetaf = 1.0D0 * (thetag + pi) -1.0D0 * matrix_mult   						          			
      thetaf = matrix_mult   						          			
    dC21 = dC21 + table65a(i,8) * 1.0D-12 * sin(thetaf) + table65a(i,9) * 1.0D-12 * cos(thetaf)
    dS21 = dS21 + table65a(i,8) * 1.0D-12 * cos(thetaf) - table65a(i,9) * 1.0D-12 * sin(thetaf)
END DO
dCnm(2+1,1+1) = dC21
dSnm(2+1,1+1) = dS21
! ----------------------------------------------------------------------  

! ----------------------------------------------------------------------  
! dC22,dS22 corrections 
! Table 6.5c
! Columns :  Doodson_No. Doodson arguments multipliers (2-7) Amp.
table65c(1,1:8) = (/ 245.655D0,  2.0D0, -1.0D0,  0.0D0,  1.0D0,  0.0D0,  0.0D0,   -0.3D0 /)
table65c(2,1:8) = (/ 255.555D0,  2.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,   -1.2D0 /)

! dC22 dS22 computations
dC22 = 0.0D0
dS22 = 0.0D0
      DO i = 1 , 2
         ! thetaf (in radians)
         matrix_mult = 0.0D0
         DO j = 1 , 6
	        matrix_mult = matrix_mult + ( table65c(i,j+1) * doodson_arr(j) )
         END DO
         !thetaf = 2.0D0 * (thetag + pi) -1.0D0 * matrix_mult														 
         thetaf = matrix_mult														 
         dC22 = dC22 + table65c(i,8) * 1.0D-12 * cos(thetaf) ! -1.0D * table65c(i,8) * 1.0D-12 * sin(thetaf) 
         dS22 = dS22 - table65c(i,8) * 1.0D-12 * sin(thetaf) ! -1.0D * table65c(i,8) * 1.0D-12 * cos(thetaf) 
      END DO
dCnm(2+1,2+1) = dC22
dSnm(2+1,2+1) = dS22


END
