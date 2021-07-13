SUBROUTINE GM_de


! ----------------------------------------------------------------------
! SUBROUTINE: GM_de.f90
! ----------------------------------------------------------------------
! Purpose:
!  Reading and storing the GM gravity constant of Sun, Moon and Planets 
!  from the JPL/NASA DE (Development Ephemeris) data
! ----------------------------------------------------------------------
! Input arguments:
!
! Output arguments:
!
! Input/Output arguments are defined via the module mdl_planets.f90
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou                               October  2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_planets
      USE mdl_num	  
      IMPLICIT NONE

	  
! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q) :: GMearth  
      INTEGER (KIND = prec_int4) :: N_CVAL, I  
! ---------------------------------------------------------------------------


! ----------------------------------------------------------------------
! GMconstant = [GM1 GM2 GMB GM4 GM5 GM6 GM7 GM8 GM9 GMmoon GMS]'  
! ----------------------------------------------------------------------
      N_CVAL = size(CVAL_2)
      DO  I = 1 , N_CVAL
          IF ( CNAM_2(I) .EQ. 'GM1   ' )  GMconst_au(1) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM2   ' )  GMconst_au(2) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GMB   ' )  GMconst_au(3) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM4   ' )  GMconst_au(4) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM5   ' )  GMconst_au(5) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM6   ' )  GMconst_au(6) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM7   ' )  GMconst_au(7) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM8   ' )  GMconst_au(8) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GM9   ' )  GMconst_au(9) = CVAL_2(I)
          IF ( CNAM_2(I) .EQ. 'GMS   ' )  GMconst_au(11) = CVAL_2(I)		  
      END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GM units conversion from au^3/day^2 to m^3/sec^2
! ----------------------------------------------------------------------
DO I = 1 , 11
   GMconst(I) = GMconst_au(I) * (AU_2**3 * 1D9) / ( (24D0 * 3600D0)**2 )
END DO   
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GMmoon = GMearth / EMRAT;
!	  GMearth = 0.39860044150D+15
	  GMearth = GM_gfm
      GMconst(10) = GMearth / EMRAT_2	  
! ----------------------------------------------------------------------


END
 