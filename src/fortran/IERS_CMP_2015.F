      SUBROUTINE IERS_CMP_2015   (version, epoch,x,y,error)
*+
*
*  This routine is part of the International Earth Rotation and
*  Reference Systems Service (IERS) Conventions software collection.
*
* This subroutine provides the angular coordinates of the IERS Conventional Mean Pole (CMP)
* to be used in the analysis of space geodesy data after 1970.
* Starting with the version CMP(2015), the coordinates are
* based on the table of values from ftp://hpiers.obspm.fr/iers/eop/eopc01/mean-pole.tab
* See IERS Conventions Section 7.1.4 at http://tai.bipm.org/iers/convupdt/convupdt.html for details.
* The subroutine also provides previous versions of the CMP in the IERS Conventions (2003) and (2010) 
*
*
*  In general, Class 1, 2, and 3 models represent physical effects that
*  act on geodetic parameters while canonical models provide lower-level
*  representations or basic computations that are used by Class 1, 2, or
*  3 models.
* 
*  Status: Class 1
*
*     Class 1 models are those recommended to be used a priori in the
*     reduction of raw space geodetic data in order to determine
*     geodetic parameter estimates.
*     Class 2 models are those that eliminate an observational
*     singularity and are purely conventional in nature.
*     Class 3 models are those that are not required as either Class
*     1 or 2.
*     Canonical models are accepted as is and cannot be classified as a
*     Class 1, 2, or 3 model.
*
*   INPUT
*   version   i     Year of the conventional model.  Limited to integer values of 2003, 2010, 2015.  
*                   For all versions (more specifically for 2003 or 2010) significant departures from the observed values 
*                   of the mean pole coordinates  result for those epochs which lie outside of the 
*                   range of years for which those models were designed and an error code will be issued, as specified below.
*                   See section 7.1.4 of the IERS Conventions for details.
*
*   epoch     d     Date for which the angular coordinates of the Conventional Mean Pole are 
*                   desired. Units are decimal years and fraction, e.g.1942.65.
*
*   OUTPUT:
*   x         d     Angular coordinate x of conventional mean pole. Units are seconds of arc. (Note 1)
*   y         d     Angular coordinate y of conventional mean pole. Units are seconds of arc. (Note 1)
*
*   error     i     flag indicating possible error in requested epoch or version.  
*                     Requesting an invalid version or an epoch before 1970 returns error code -1 and mean pole coordinates (0,0)
*                     Requesting an epoch out of the range 1975.0-2003.0 in version 2003 returns error code 1   
*                     Requesting an epoch out of the range 1975.0-2010.0 in version 2010 returns error code 2  
*                     Requesting an epoch out of the range 1970.0-2016.2 in version 2015 returns error code 3 
*                     Requesting a valid version and an epoch in the recommended range returns error code 0
*
*  Notes: 
*
*  1) Returned value will be zero if invalid version number or epoch requested before 1970. 
*     In other cases, users should be aware that the returned value may depart from the observed mean pole coordinate
*     if the requested epoch lies outside the range of years for which the model was designed, as indicated by the error code.
*
*  Called: None
*
*  Test cases:
*    1)  INPUT:    2015   2015.35D0
*        OUTPUT:     0.12523135017430187       0.34623285979490870                0 
*
*    2)  INPUT:    2003   2002.1D0 
*        OUTPUT:     5.57429999999999246E-002  0.36529499999999965                0 
*
*    3)  INPUT:    2010   2002.1D0
*        OUTPUT:     6.06820922651758257E-002  0.34962260201001832                0
*
*    4)  INPUT:    2015   2002.1D0
*        OUTPUT:     6.07152994763746900E-002  0.34770609736572355                0
*    
*    5)  INPUT:    2003   2003.1D0
*        OUTPUT:     5.65729999999999220E-002  0.36924499999999960                1

*
*
*  References:
*
*     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
*     IERS Technical Note No. 36, BKG (2010)
*
*  Revisions:
*  2015 May-June    D. McCarthy    Original code and test case
*  2015 15 June     G. Petit       a) truncated table(3,n) to include only 1970.00 to 2015.00 epochs;
*                                  b) added code: if epoch is previous to 1970, return x=y=0.d0;
*                                  c) x and y output to be non-zero when epoch is outside recommended MJD range
*                                     for version 2003, 2010, and 2015 models. (Original McCarthy code
*                                     returned values of x=y=0 for these cases.)
*  2015 16 June     N. Stamatakos at direction of G. Petit   Test cases 1) through 5)
*            
*                                  
*
*-----------------------------------------------------------------------

      INTEGER i,j,k,error,version
      REAL*8 epoch, x, y
!**********************************************************************
!  The following table is taken from the IERS file  polemean1900-2015 which is found 
!   at http://hpiers.obspm.fr/eoppc/eop/eopc01/mean-pole.tab.  Although the IERS table 
!   provides data with a spacing of 0.05 years, only the data for the beginning of
!   each calendar year i.e. YYYY.000) are taken from that table and reproduced in 
!   the DATA statement  below.  The data should be replaced  when a new version is made 
!   available by the IERS at the beginning of each year. The parameter n should be adjusted 
!   at that time also.
!**********************************************************************
      PARAMETER (n=46)             
      REAL*8  table(3,n)
      data table/ 1970.00,0.002117,0.219438,
     .1971.00,	0.005376,	0.225514,
     .1972.00,	0.008752,	0.231446,
     .1973.00,	0.012200,	0.237249,
     .1974.00,	0.015675,	0.242939,
     .1975.00,	0.019134,	0.248524,
     .1976.00,	0.022536,	0.254008,
     .1977.00,	0.025841,	0.25939,
     .1978.00,	0.029016,	0.264667,
     .1979.00,	0.032029,	0.269833,
     .1980.00,	0.034850,	0.274883,
     .1981.00,	0.037453,	0.279812,
     .1982.00,	0.039815,	0.284619,
     .1983.00,	0.041917,	0.289304,
     .1984.00,	0.043746,	0.293872,
     .1985.00,	0.045297,	0.298326,
     .1986.00,	0.046571,	0.302667,
     .1987.00,	0.047582,	0.306895,
     .1988.00,	0.048352,	0.311002,
     .1989.00,	0.048912,	0.314978,
     .1990.00,	0.049304,	0.318804,
     .1991.00,	0.049576,	0.322462,
     .1992.00,	0.049783,	0.32593,
     .1993.00,	0.049983,	0.329189,
     .1994.00,	0.050238,	0.332221,
     .1995.00,	0.050608,	0.335012,
     .1996.00,	0.051151,	0.337554,
     .1997.00,	0.051918,	0.339844,
     .1998.00,	0.052955,	0.341881,
     .1999.00,	0.054299,	0.34367,
     .2000.00,	0.055981,	0.345218,
     .2001.00,	0.058022,	0.346532,
     .2002.00,	0.060436,	0.347619,
     .2003.00,	0.063229,	0.34849,
     .2004.00,	0.066403,	0.349153,
     .2005.00,	0.069953,	0.349618,
     .2006.00,	0.073868,	0.349899,
     .2007.00,	0.078133,	0.350008,
     .2008.00,	0.082731,	0.349963,
     .2009.00,	0.087643,	0.349779,
     .2010.00,	0.092851,	0.349474,
     .2011.00,	0.098339,	0.349062,
     .2012.00,	0.104098,	0.348553,
     .2013.00,	0.110120,	0.347955,
     .2014.00,	0.116401,	0.347271,
     .2015.00,	0.122942,	0.346502/

      x=0.0d0
      y=0.0d0
      error=-1
      if(epoch<1970.00)return
 
!***********************************************************************
!   Conventional mean pole model in IERS Conventions 2003.  
!
!   Because this model would return values that depart significantly from observations for 
!   epochs before 1975.00 and after 2003.00, an error code of 1 is returned for epochs outside of that
!    interval.
!***********************************************************************
      IF(version.eq.2003) then 
         x=0.054d0+0.00083d0*(epoch-2000.0d0)
         y=0.357d0+0.00395d0*(epoch-2000.0d0)
         error=0
         IF(epoch<1975.00.or.epoch>2003.00) error=1
      ENDIF

!***********************************************************************
!   Conventional mean pole model in IERS Conventions 2010.  
!
!   Because this model would return values that depart significantly from observations for 
!   epochs before 1975.00 and after 2010.00, an error code of 2 is returned for epochs outside of that
!   interval.          
!***********************************************************************
      IF(version.eq.2010) then 
         dx=epoch-2000.0d0
               IF (dx<10.0) then
                    x=0.055974d0 + 0.0018243d0 *dx + 0.00018413d0 *dx**2
     *                                      +0.000007024d0 *dx**3
                    y=0.346346d0 + 0.0017896d0*dx  - 0.00010729d0*dx**2
     *                                      - 0.000000908d0*dx**3
                ELSE
         
               x=0.023513d0 + 0.0076141d0*dx
               y=0.358891d0 - 0.0006287d0*dx

                ENDIF
         error=0
         IF(epoch<1975.00.or.epoch>2010.00) error=2

      ENDIF         

!***********************************************************************
!   CMP(2015) If requested date is after 2016.2 the error code is set to 3
!***********************************************************************

      IF(version.eq.2015) then
      
      i=1  
      j=n+1
       
      DO
             k=(i+j)/2

             IF (epoch < table(1,k)) THEN
               j=k
             ELSE
               i=k
             END IF

             IF (j <= i+1) EXIT
      END DO

      dx=epoch-table(1,i)   
      error=0
       if(i.eq.n)then

        x = table(2,i)+dx*(table(2,n)-table(2,n-1))
        y = table(3,i)+dx*(table(3,n)-table(3,n-1))
        if(dx>1.2)error=3
       else
        x = table(2,i)+dx*(table(2,i+1)-table(2,i))
        y = table(3,i)+dx*(table(3,i+1)-table(3,i))
       ENDIF  

      ENDIF
      

      RETURN
*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  IERS Conventions Center
*
*  ==================================
*  IERS Conventions Software License
*  ==================================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is provided by the IERS Conventions Center ("the
*     Center").
*
*  2. Permission is granted to anyone to use the Software for any
*     purpose, including commercial applications, free of charge,
*     subject to the conditions and restrictions listed below.
*
*  3. You (the user) may adapt the Software and its algorithms for your
*     own purposes and you may distribute the resulting "derived work"
*     to others, provided that the derived work complies with the
*     following requirements:
*
*     a) Your work shall be clearly identified so that it cannot be
*        mistaken for IERS Conventions software and that it has been
*        neither distributed by nor endorsed by the Center.
*
*     b) Your work (including source code) must contain descriptions of
*        how the derived work is based upon and/or differs from the
*        original Software.
*
*     c) The name(s) of all modified routine(s) that you distribute
*        shall be changed.
* 
*     d) The origin of the IERS Conventions components of your derived
*        work must not be misrepresented; you must not claim that you
*        wrote the original Software.
*
*     e) The source code must be included for all routine(s) that you
*        distribute.  This notice must be reproduced intact in any
*        source distribution. 
*
*  4. In any published work produced by the user and which includes
*     results achieved by using the Software, you shall acknowledge
*     that the Software was used in obtaining those results.
*
*  5. The Software is provided to the user "as is" and the Center makes
*     no warranty as to its use or performance.   The Center does not
*     and cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Center makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Center be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Center representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning IERS Conventions software should be
*  addressed as follows:
*
*                     Gerard Petit
*     Internet email: gpetit[at]bipm.org
*     Postal address: IERS Conventions Center
*                     Time, frequency and gravimetry section, BIPM
*                     Pavillon de Breteuil
*                     92312 Sevres  FRANCE
*
*     or
*
*                     Brian Luzum
*     Internet email: brian.luzum[at]usno.navy.mil
*     Postal address: IERS Conventions Center
*                     Earth Orientation Department
*                     3450 Massachusetts Ave, NW
*                     Washington, DC 20392
*
*
*------------------------------------------------------------------
      END
