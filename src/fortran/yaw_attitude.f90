SUBROUTINE yaw_attitude (mjd, r_sat, v_sat, r_sun, beta_angle, PRN, satblk, orbdir, eclipsf, & 
						 eBX_nom, eBX_ecl, Yangle, Mangle, Mangle_e, Mrate_e, Ynom_e, satbf)

! ----------------------------------------------------------------------
! SUBROUTINE: yaw_attitude.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS yaw-attitude modelling during eclipse seasons and nominal
!  yaw-attitude computation
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - r_sun:		Sun position vector (m) in ITRF
! - beta_angle:	Sun angle with the orbital plane (computed by beta_angle.f90)
! - PRN:		SV PRN NUMBER (.LE.32 FOR GPS, .GT.32 FOR GLONASS)
! - satblk:		SV BLOCK  1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6
! - orbdir:		DIRECTION OF PROCESSING (1=FORWARD, -1=BACKWARD)
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
!
! Output arguments:
! - eclipsf:	Status of eclipse: 0=NO  |  1,2=YES (1=night, 2=noon)
!				Equal to the 'IECLIPS' output argument of eclips.f;
! - eBX_nom:	Body-X axis unit vector during noninal yaw-attitude
! - eBX_ecl:	Body-X axis unit vector during eclipsng yaw-attitude (as computed by eclips.f); Equal to eBX_nom when out of eclipse season 
! - Yangle: 	Yaw angle array (in degrees) with 2 values: Yaw nominal and Yaw modeled (eclips.f)
!				In case that there is no eclipse effect (IECLIPS=0), the two values are equal
! - Mangle:		Orbit angle between satellite and orbit midnight (deg)
! - Mangle_e:	Orbit angle as obtained from eclips.f subroutine (additional ouput to eclips.f)
! - Mrate_e:	Rate of the orbit angle as obtained from eclips.f subroutine (additional ouput to eclips.f)
! - Ynom_e:		Yaw angle nominal as obtained from eclips.f subroutine (additional ouput to eclips.f)
! ----------------------------------------------------------------------
! Called/Required subroutines:
! eclips.f (Kouba 2009)
! yaw_angle.f90
! beta_angle.f90 (required for the computation of the input 'beta_angle')
! ----------------------------------------------------------------------
!
! Chnages:  02-05-2019 Tzupang Tseng : The unit vector SANTXYZ from Kouba is not correctly defined.
!                                      The unit vector SANTXYZ is re-defined by using the nominal yaw attitude. 
!
!
! Dr. Thomas D. Papanikolaou, Geoscience Australia             June 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd, r_sat(3), v_sat(3), r_sun(3), beta_angle
!      INTEGER (KIND = prec_int8) :: PRN, orbdir
!      INTEGER (KIND = prec_int4) :: satblk
      INTEGER (KIND = 4) :: PRN, orbdir, satblk
      INTEGER (KIND = 4), INTENT(IN) :: satbf 
	  
! OUT
      REAL (KIND = prec_d) :: Yangle(2)
	  REAL (KIND = prec_d) :: eBX_nom(3), eBX_ecl(3)
      REAL (KIND = prec_d) :: Mangle
      REAL (KIND = prec_d) :: Mangle_e, Mrate_e, Ynom_e
      !INTEGER (KIND = prec_int8) :: eclipsf
      INTEGER (KIND = 4) :: eclipsf
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), Xsun(3)
      REAL (KIND = prec_d) :: pcross(3), pcross2(3), pdot
	  REAL (KIND = prec_d) :: yaw_model, yaw_nominal
	  REAL (KIND = prec_d) :: Yangle_prior, Yangle_post
! ----------------------------------------------------------------------
      INTEGER i, Pflag
      INTEGER (KIND = prec_int8) :: GPS_week, GPSweek_mod1024, eclipsV
      REAL (KIND = prec_d) :: GPS_wsec
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! eclips.f arguments
! ----------------------------------------------------------------------
      INTEGER NECLIPS_max
      PARAMETER (NECLIPS_max = 2)
! ----------------------------------------------------------------------
      INTEGER MAXSAT
      !PARAMETER (MAXSAT=64)												
      PARAMETER (MAXSAT=136)	  
! ----------------------------------------------------------------------
!      INTEGER*4 IDIR, IPRN
!      INTEGER*4 IECLIPS, NECLIPS(*)
!      INTEGER*4 IBLK(*)

!      INTEGER (KIND = prec_int8) :: IDIR
!      INTEGER (KIND = prec_int4) :: iblk(MAXSAT)
!      INTEGER (KIND = prec_int8) :: IPRN, IECLIPS, NECLIPS(MAXSAT)

      INTEGER (KIND = 4) :: IDIR, iblk(MAXSAT), IPRN, IECLIPS, NECLIPS(MAXSAT)
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: TTAG, SVBCOS, PI
	  
!      REAL*8    ECLSTM(MAXSAT,*)
!      REAL*8    ECLETM(MAXSAT,*)
	  !REAL (KIND = prec_d), DIMENSION(:,:) :: ECLSTM
      !REAL (KIND = prec_d) :: ECLSTM
      REAL (KIND = prec_d) :: ECLSTM (MAXSAT , NECLIPS_max)
      REAL (KIND = prec_d) :: ECLETM (MAXSAT , NECLIPS_max)
	  
      REAL (KIND = prec_d) :: ANOON, ANIGHT
      REAL (KIND = prec_d) :: beta_eclips 
	  
	  !REAL*8    XSV(*), SANTXYZ(*), VSVC(*)
      REAL (KIND = prec_d) :: xsv(3), santxyz(3), VSVC(3)
      
      REAL (KIND = prec_d) :: yaw(2), MU, MURATE
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Yangle_nom

      !REAL*8    BETAINI(*)
      REAL (KIND = prec_d) :: BETAINI(MAXSAT) 
	  
	  INTEGER (KIND = 4) :: Nsat
      
      REAL (KIND = prec_q), DIMENSION(3) :: ed, ey
      REAL (KIND = prec_q) :: Ds
! ----------------------------------------------------------------------
	 
! init variables
ECLSTM=0.d0
ECLETM=0.d0
MU = 0.d0
MURATE = 0.d0
	 
	 
	 
      Nsat = MAXSAT
	 
! PI input of eclips.f
      PI = 3.1415926536D0
  

! ----------------------------------------------------------------------
! Unit Vectors
! ----------------------------------------------------------------------
! Xsat:		Satellite position unit vector
      Xsat = (1D0 / sqrt(r_sat(1)**2 + r_sat(2)**2 + r_sat(3)**2) ) * r_sat
	  
! Xsun:		Sun position unit vector
      Xsun = (1D0 / sqrt(r_sun(1)**2 + r_sun(2)**2 + r_sun(3)**2) ) * r_sun
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! eclips.f (Kouba 2009) input arguments:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! IPRN:		SV PRN NUMBER (.LE.32 FOR GPS, .GT.32 FOR GLONASS)
      IPRN = PRN	
! IBLK:		SV BLOCK  1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6
      iblk(PRN) = satblk
! IDIR:		DIRECTION OF PROCESSING (1=FORWARD, -1=BACKWARD)
      IDIR = orbdir 
! ----------------------------------------------------------------------

	  
! ----------------------------------------------------------------------
! TTAG : Seconds of GPS week
! ----------------------------------------------------------------------
! mjd_GPS to GPS_week & GPS_wsec
      CALL time_GPSweek (mjd , GPS_week, GPS_wsec, GPSweek_mod1024)
      TTAG = GPS_wsec 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! SVBCOS:	COS of the angle between the sv radius vector and the sun 
!			radius vector (the dot product of the two unit vectors) 
! ----------------------------------------------------------------------
!      SVBCOS = cos(E)													! see Kouba (2009), Eq.11-12
      CALL productdot (Xsat, Xsun , pdot)
! noon turns: Eq.11
      SVBCOS = pdot
! midnight turns: Eq.12
      !SVBCOS = -1.0D0 * pdot 				 
! ----------------------------------------------------------------------


! ANOON:	computed within eclips.f subroutine 


! ----------------------------------------------------------------------
! ANIGHT: shadow limit as "input" in eclips.f subroutine 
! ----------------------------------------------------------------------
! 180.D0+-13.25D0 for GPS (IPRN.GE. 32)
! 180.D0+-14.20D0 for GLONASS (IPRN.GT.32) 
      if (IPRN <= 32) then
          !ANIGHT = 180.D0 + 13.25D0
          ANIGHT = 180.D0 + 13.5D0		! Kouba (2009), pp.6, Eq.17
      elseif (IPRN > 32) then
          ANIGHT = 180.D0 + 14.20D0
      end if         	  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! NECLIPS:        NUMBER OF ECLIPSING FOR THE PRN SATELLITE 
      do i = 1 , Nsat
	     NECLIPS (i) = 0 
		 
         BETAINI (i) = 0.0D0   
      end do
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! XSV: 		SV X, Y, Z (m)
      xsv = r_sat
! VSVC:		SV INERTIAL VELOCIRY VECTOR	
      vsvc = v_sat
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! SANTXYZ:	Body-X unit vector
! ----------------------------------------------------------------------
!C eclips.f WARNING: THE IIA BODY-X ORIENTATION EXPECTED FOR THE IIR
!C                   THE  BODY-X REVERSED FOR IIR (SEE BELOW) & RETURNED
! ----------------------------------------------------------------------
!      SANTXYZ = - ( (Xsun x Xsat) x Xsat )  							! see Kouba (2009), Eq.1
!      CALL productcross (Xsun,Xsat , pcross)
!      CALL productcross (pcross,Xsat , pcross2 )
!      santxyz = -1.0D0 * pcross2
      Ds=sqrt((r_sun(1)-r_sat(1))**2+(r_sun(2)-r_sat(2))**2+(r_sun(3)-r_sat(3))**2)
      ed(1)=((r_sun(1)-r_sat(1))/Ds)
      ed(2)=((r_sun(2)-r_sat(2))/Ds)
      ed(3)=((r_sun(3)-r_sat(3))/Ds)

      CALL productcross (-Xsat,ed, pcross)
      ey(1)=pcross(1)/sqrt(pcross(1)**2+pcross(2)**2+pcross(3)**2)
      ey(2)=pcross(2)/sqrt(pcross(1)**2+pcross(2)**2+pcross(3)**2)
      ey(3)=pcross(3)/sqrt(pcross(1)**2+pcross(2)**2+pcross(3)**2)

      CALL productcross (ey,-Xsat,pcross2)
	   santxyz = pcross2/sqrt(pcross2(1)**2+pcross2(2)**2+pcross(3)**2)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! eBX_nom:	Body-X axis unit vector (nominal)
! ----------------------------------------------------------------------
      eBX_nom = santxyz
	  !print *,"eBX_nom", eBX_nom
	  
! GPS Block IIR: Body-X unit vector reversal
!if (satblk == 4 .or. satblk==5) then
!      eBX_nom = -1.0D0 * eBX_nom
	  !print *,"eBX_nom", eBX_nom

      !DO i = 1 , 3
	  !	eBX_nom(i) = -1.0D0 * eBX_nom(i)
      !END DO
	  !print *,"eBX_nom", eBX_nom
!end if

if (satblk == 4 .or. satblk==5) then
if (satbf == 2) then
! GPS Block IIR: Body-X unit vector reversal
      eBX_nom = -1.0D0 * eBX_nom
end if
end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! beta input argument of eclips.f (in rad): beta angle (Sun angle with orbital plane) + 90 degrees
          beta_eclips = (beta_angle + 90.0D0) * (PI_global / 180.0D0) 	  
! ----------------------------------------------------------------------

yaw = (/ 0.D0, 0.D0 /)
!print *,"yaw prior",yaw

! ----------------------------------------------------------------------
! Modelling of yaw-attitude during eclipse seasons based on eclips.f
! ----------------------------------------------------------------------
if (Nsat == 64) then

! Modified Version (2016) of the eclips.f v.2013
      CALL ECLIPS ( IDIR, IPRN, TTAG, SVBCOS, ANOON, ANIGHT,            &
                    NECLIPS, ECLSTM, ECLETM, IECLIPS, PI ,              &
                    xsv, santxyz, vsvc, beta_eclips, iblk,              &
                    yaw, MU, MURATE ) 

					
else if (Nsat == 136) then

! ----------------------------------------------------------------------
! 
eclipsV = 201707
 
If (eclipsV == 2017) then
! eclips.f Version Feb. 2017
      CALL ECLIPS2017 ( IDIR, IPRN, TTAG, SVBCOS, ANOON, ANIGHT,         &
						NECLIPS, ECLSTM, ECLETM, IECLIPS, PI ,           &  
						xsv, santxyz, vsvc, beta_eclips, iblk, BETAINI, yaw)

Else If (eclipsV == 201707) then  
! Modified Version (July 2017) of the eclips.f v.2017 based on the 
! modifications performed in 2016 to the eclips.f v.2013

santxyz = eBX_nom

      CALL ECLIPS201707 ( IDIR, IPRN, TTAG, SVBCOS, ANOON, ANIGHT,         &
						NECLIPS, ECLSTM, ECLETM, IECLIPS, PI ,           &  
						xsv, santxyz, vsvc, beta_eclips, iblk, BETAINI, yaw)			

End IF
! ---------------------------------------------------------------------- 


end if


      eclipsf = IECLIPS
	  
      Yangle = yaw
	  
	  ! Yangle_nom as obtained from eclips.f
      Ynom_e = yaw(1)
	  
	  ! Mangle as obtained from eclips.f
      if ( abs(MU) < 1.0D-30) then
	     MU = 0.0D0
      end if
	  Mangle_e = MU
	  Mrate_e = MURATE
	
	
! ----------------------------------------------------------------------
! eBX_ecl:	Body-X axis unit vector as computed by eclips.f
! ----------------------------------------------------------------------
! The vector remains the same with the nominal (input) out of eclipse seasons
      !eBX = santxyz
      eBX_ecl = santxyz
      !eBX = santxyz_0 ! eBX input

  
	  
! ----------------------------------------------------------------------
! Nominal Yaw angle and Orbital angle (M)
! ----------------------------------------------------------------------
! Body-frame definition type
if (satblk == 4 .or. satblk==5) then
if (satbf == 1) then
! IGS Conventions definition
   satblk = 6 
end if
end if

      Call yaw_nom (eBX_nom, v_sat, beta_angle, satblk, SVBCOS, ANOON, ANIGHT, Mangle, Yangle_nom)  
      !CALL yaw_nom2 (r_sat, v_sat, r_sun, eBX_nom, beta_angle, satblk, SVBCOS, ANOON, ANIGHT, Mangle, Yangle_nom)	
	  
! Nominal yaw angle
	  !PRINT *,"Yangle",Yangle
      Yangle(1) = Yangle_nom
	  !PRINT *,"Yangle",Yangle
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      Pflag = 0
	  if (Pflag > 0) then
      PRINT *, "e_B-X 0",eBX_nom
      PRINT *, "e_B-X  ",eBX_ecl
      PRINT *, "TTAG   ",TTAG
      Print *, 'SVBCOS', SVBCOS 
      PRINT *, "ANOON",  ANOON 
      PRINT *, "ANIGHT", ANIGHT
      PRINT *, "ECLSTM", ECLSTM(IPRN,1)
      PRINT *, "ECLETM", ECLETM(IPRN,1)
      PRINT *, "NECLIPS",NECLIPS(IPRN)
      PRINT *, "IECLIPS",IECLIPS
      !PRINT *, "yaw ", yaw
	  end if
! ----------------------------------------------------------------------



END
