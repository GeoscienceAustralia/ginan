      SUBROUTINE iau_BP00 ( DATE1, DATE2, RB, RP, RBP )
*+
*  - - - - - - - - -
*   i a u _ B P 0 0
*  - - - - - - - - -
*
*  Frame bias and precession, IAU 2000.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2    d       TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     RB           d(3,3)    frame bias matrix (Note 2)
*     RP           d(3,3)    precession matrix (Note 3)
*     RBP          d(3,3)    bias-precession matrix (Note 4)
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The matrix RB transforms vectors from GCRS to mean J2000.0 by
*     applying frame bias.
*
*  3) The matrix RP transforms vectors from J2000.0 mean equator and
*     equinox to mean equator and equinox of date by applying
*     precession.
*
*  4) The matrix RBP transforms vectors from GCRS to mean equator and
*     equinox of date by applying frame bias then precession.  It is the
*     product RP x RB.
*
*  Called:
*     iau_BI00     frame bias components, IAU 2000
*     iau_PR00     IAU 2000 precession adjustments
*     iau_IR       initialize r-matrix to identity
*     iau_RX       rotate around X-axis
*     iau_RY       rotate around Y-axis
*     iau_RZ       rotate around Z-axis
*     iau_RXR      product of two r-matrices
*     iau_CR       copy r-matrix
*
*  Reference:
*
*     Capitaine, N., Chapront, J., Lambert, S. and Wallace, P.,
*     "Expressions for the Celestial Intermediate Pole and Celestial
*     Ephemeris Origin consistent with the IAU 2000A precession-nutation
*     model", Astron.Astrophys. 400, 1145-1154 (2003)
*
*     n.b. The celestial ephemeris origin (CEO) was renamed "celestial
*          intermediate origin" (CIO) by IAU 2006 Resolution 2.
*
*  This revision:  2013 August 21
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, RB(3,3), RP(3,3), RBP(3,3)

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

*  J2000.0 obliquity (Lieske et al. 1977)
      DOUBLE PRECISION EPS0
      PARAMETER ( EPS0 = 84381.448D0 * DAS2R )

      DOUBLE PRECISION T, DPSIBI, DEPSBI, DRA0, PSIA77, OMA77, CHIA,
     :                 DPSIPR, DEPSPR, PSIA, OMA, RBW(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and current date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

*  Frame bias.
      CALL iau_BI00 ( DPSIBI, DEPSBI, DRA0 )

*  Precession angles (Lieske et al. 1977)
      PSIA77 =        ( 5038.7784D0 +
     :                (   -1.07259D0 +
     :                (   -0.001147D0 ) * T ) * T ) * T * DAS2R
      OMA77  = EPS0 + (
     :                (    0.05127D0 +
     :                (   -0.007726D0 ) * T ) * T ) * T * DAS2R
      CHIA   =        (   10.5526D0 +
     :                (   -2.38064D0 +
     :                (   -0.001125D0 ) * T ) * T ) * T * DAS2R

*  Apply IAU 2000 precession corrections.
      CALL iau_PR00 ( DATE1, DATE2, DPSIPR, DEPSPR )
      PSIA = PSIA77 + DPSIPR
      OMA  = OMA77  + DEPSPR

*  Frame bias matrix: GCRS to J2000.0.
      CALL iau_IR ( RBW )
      CALL iau_RZ ( DRA0, RBW )
      CALL iau_RY ( DPSIBI*SIN(EPS0), RBW )
      CALL iau_RX ( -DEPSBI, RBW )
      CALL iau_CR ( RBW, RB )

*  Precession matrix: J2000.0 to mean of date.
      CALL iau_IR ( RP )
      CALL iau_RX ( EPS0, RP )
      CALL iau_RZ ( -PSIA, RP )
      CALL iau_RX ( -OMA, RP )
      CALL iau_RZ ( CHIA, RP )

*  Bias-precession matrix: GCRS to mean of date.
      CALL iau_RXR ( RP, RBW, RBP )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
