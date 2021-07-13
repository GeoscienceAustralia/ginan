      SUBROUTINE FSIZER3(NRECL,KSIZE,NRFILE,NAMFIL)
C
C++++++++++++++++++++++++
C
C  THE SUBROUTINE SETS THE VALUES OF  NRECL, KSIZE, NRFILE, AND NAMFIL.

      SAVE

      CHARACTER*80 NAMFIL

C  *****************************************************************
C  *****************************************************************
C
C  THE PARAMETERS NRECL, NRFILE, AND NAMFIL ARE TO BE SET BY THE USER

C  *****************************************************************

C  NRECL=1 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN S.P. WORDS
C  NRECL=4 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN BYTES

       NRECL=4

C  *****************************************************************

C  NRFILE IS THE INTERNAL UNIT NUMBER USED FOR THE EPHEMERIS FILE (DEFAULT: 12)

      NRFILE=12

C  *****************************************************************

C  NAMFIL IS THE EXTERNAL NAME OF THE BINARY EPHEMERIS FILE

      NAMFIL='JPLEPH'

C  *****************************************************************

C  KSIZE must be set by the user according to the ephemeris to be read

C  For  de200, set KSIZE to 1652
C  For  de405, set KSIZE to 2036
C  For  de406, set KSIZE to 1456
C  For  de414, set KSIZE to 2036
C  For  de418, set KSIZE to 2036
C  For  de421, set KSIZE to 2036
C  For  de422, set KSIZE to 2036
C  For  de423, set KSIZE to 2036
C  For  de424, set KSIZE to 2036
C  For  de430, set KSIZE to 2036

      KSIZE = 2036

C  *******************************************************************

      RETURN

      END
C++++++++++++++++++++++++++
C