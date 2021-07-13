      SUBROUTINE taiutc ( IY, IM, ID, FD, DELTAT, J )
*
*  - - - - - - - -
*   taiutc
*  - - - - - - - -
*
*  This routine is a MODIFIED version of the  the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  user-replaceable support routine.
*
*  Given:
*     IY       i     UTC:  year (Notes 1 and 2)
*     IM       i           month (Note 2)
*     ID       i           day (Notes 2 and 3)
*     FD       d           fraction of day (Note 4)
*
*  Returned:
*     DELTAT   d     TAI minus UTC, seconds
*     J        i     status (Note 5):
*                       1 = dubious year (Note 1)
*                       0 = OK
*                      -1 = bad year
*                      -2 = bad month
*                      -3 = bad day (Note 3)
*                      -4 = bad fraction (Note 4)
*                      -5 = internal error (Note 5)
*
*  Notes:
*
*  1) UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
*     to call the routine with an earlier date.  If this is attempted,
*     zero is returned together with a warning status.
*
*  1a)In this verion of the routine, dates before the first entry in the 
*     leap.seconds file (January 1st 1972) cannot be used.
*
*     Because leap seconds cannot, in principle, be predicted in
*     advance, a reliable check for dates beyond the valid range is
*     impossible.  To guard against gross errors, a year five or more
*     after the release year of the present routine (see parameter IYV)
*     is considered dubious.  In this case a warning status is returned
*     but the result is computed in the normal way.
*
*     For both too-early and too-late years, the warning status is J=+1.
*     This is distinct from the error status J=-1, which signifies a
*     year so early that JD could not be computed.
*
*  2) If the specified date is for a day which ends with a leap second,
*     the TAI-UTC value returned is for the period leading up to the
*     leap second.  If the date is for a day which begins as a leap
*     second ends, the TAI-UTC returned is for the period following the
*     leap second.
*
*  3) The day number must be in the normal calendar range, for example
*     1 through 30 for April.  The "almanac" convention of allowing
*     such dates as January 0 and December 32 is not supported in this
*     routine, in order to avoid confusion near leap seconds.
*
*  4) The fraction of day is used only for dates before the introduction
*     of leap seconds, the first of which occurred at the end of 1971.
*     It is tested for validity (0 to 1 is the valid range) even if not
*     used;  if invalid, zero is used and status J=-4 is returned.  For
*     many applications, setting FD to zero is acceptable;  the
*     resulting error is always less than 3 ms (and occurs only
*     pre-1972).
*
*  5) The status value returned in the case where there are multiple
*     errors refers to the first error detected.  For example, if the
*     month and day are 13 and 32 respectively, J=-2 (bad month) will be
*     returned.  The "internal error" status refers to a case that is
*     impossible but causes some compilers to issue a warning.
*
*  6) In cases where a valid result is not available, zero is returned.
*
*  Called:
*     iau_CAL2JD   Gregorian calendar to JD
*
*  This revision:  2019 July 9th
*
*-----------------------------------------------------------------------

      USE mdl_param
      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION FD, DELTAT
      INTEGER J

*  Miscellaneous local variables
      LOGICAL MORE
      INTEGER JS, M, N, IS
      DOUBLE PRECISION DA, DJM0, DJM

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Initialize the result to zero and the status to OK.
      DA = 0D0
      JS = 0

*  If invalid fraction of a day, set error status and give up.
      IF ( FD.LT.0D0 .OR. FD.GT.1D0 ) THEN
         JS = -4
         GO TO 9000
      END IF

*  Convert the date into an MJD.
      CALL iau_CAL2JD ( IY, IM, ID, DJM0, DJM, JS )

*  If invalid year, month, or day, give up.
      IF ( JS .LT. 0 ) GO TO 9000

*  If pre-UTC year, set warning status and give up.
      IF ( IY .LT. IDAT(1,1) ) THEN
         JS = 1
         GO TO 9000
      END IF

*  If suspiciously late year, set warning status but proceed.
      IF ( IY .GT. IDAT(1,NDAT)+3 ) JS = 1

*  Combine year and month.
      M = 12*IY+IM

*  Find the most recent table entry.
      IS = 0
      MORE = .TRUE.
      DO 1 N=NDAT,1,-1
         IF ( MORE ) THEN
            IS = N
            MORE = M .LT. ( 12*IDAT(1,N) + IDAT(2,N) )
         END IF
 1    CONTINUE

*  Prevent underflow warnings.
      IF ( IS .LT. 1 ) THEN
         JS = -5
         GO TO 9000
      END IF

*  Get the Delta(AT).
      DA = DATS(IS)

*  Return the Delta(AT) value and the status.
 9000 CONTINUE
      DELTAT = DA
      J = JS
!      PRINT *,'TAI-UTC: ',DELTAT
      
*  Finished.

      END
