C----------------------------------------------------------------
      SUBROUTINE INTERP_iers (RJD,X,Y,UT1,N,rjd_int,x_int,y_int,ut1_int)
C
C     This subroutine takes a series of x, y, and UT1-UTC values
C     and interpolates them to an epoch of choice. This routine
C     assumes that the values of x and y are in seconds of
C     arc and that UT1-UTC is in seconds of time. At least
C     one point before and one point after the epoch of the
C     interpolation point are necessary in order for the
C     interpolation scheme to work. 
C
C     parameters are :
C     RJD     - array of the epochs of data (given in mjd)
C     X       - array of x polar motion (arcsec)
C     Y       - array of y polar motion (arcsec)
C     UT1     - array of UT1-UTC (sec)
C     n       - number of points in arrays
C     rjd_int - epoch for the interpolated value
C     x_int   - interpolated value of x
C     y_int   - interpolated value of y
C     ut1_int - interpolated value of ut1-utc
C
C     CALLED SUBROUTINE : LAGINT (Lagrange interpolation)
C                         PMUT1_OCEANS (Diurnal and semidiurnal oceanic effects)
C                         PM_GRAVI (Diurnal and semidiurnal lunisolar effects)
C
C      coded by Ch. BIZOUARD (Observatoire de Paris) : November 2002
C                                          Corrected : September 2007   
  
      implicit none
      integer i,n
      DOUBLE PRECISION RJD(N), X(N), Y(N), UT1(N),
     . rjd_int, x_int, y_int, ut1_int, 
     . cor_x, cor_y, cor_ut1, cor_lod
      

      CALL LAGINT (RJD,X,n,rjd_int,x_int)
  
      CALL LAGINT (RJD,Y,n,rjd_int,y_int)
      
      CALL LAGINT (RJD,UT1,n,rjd_int,ut1_int)
      !PRINT *,"LAGINT", x_int, y_int, ut1_int  

c --------------
c Oceanic effect      
c --------------
   
      CALL PMUT1_OCEANS (rjd_int,cor_x,cor_y,cor_ut1,cor_lod)
      !PRINT *,"PMUT1_OCEANS cor", cor_x, cor_y, cor_ut1, cor_lod
	  
      x_int = x_int + cor_x
      y_int = y_int + cor_y
      ut1_int = ut1_int + cor_ut1

C Lunisolar effect 
      CALL PM_GRAVI (rjd_int,cor_x,cor_y)
      !PRINT *,"PM_GRAVI cor", cor_x, cor_y
      
      x_int   = x_int + cor_x
      y_int   = y_int + cor_y

      ! SCM 20200805 added IERS subdiurnal libration correction in 
      ! the axial component of rotation, expressed by UT1 and LOD.
      ! not yet tested, currently commented out.
      !CALL UTLIBR (RJD_int, cor_ut1, cor_lod)
      !PRINT*,"UTLIBR: rjd_int,cor_ut1,cor_lod:",rjd_int,cor_ut1,cor_lod
      !ut1_int = ut1_int + cor_ut1*1.0d-6

     
      RETURN

      END
C
C----------------------------------------------------------------
C
      SUBROUTINE LAGINT (X,Y,n,xint,yout)
C 
C     This subroutine performs lagrangian interpolation
C     within a set of (X,Y) pairs to give the y
C     value corresponding to xint. This program uses a
C     window of 4 data points to perform the interpolation.
C     if the window size needs to be changed, this can be
C     done by changing the indices in the do loops for
C     variables m and j.
C
C     PARAMETERS ARE :
C     X     - array of values of the independent variable
C     Y     - array of function values corresponding to x
C     n     - number of points
C     xint  - the x-value for which estimate of y is desired
C     yout  - the y value returned to caller

      implicit none
      REAL*8 X(n),Y(n),xint,yout,term
      INTEGER i,j,k,m,n

      yout = 0.d0
      k = 0
      do  i = 1,n-1
        if ( xint .ge. X(i) .and. xint .lt. X(i+1) ) k = i
      enddo
    
      if ( k .lt. 2 ) k = 2
      if ( k .gt. n-2 ) k = n-2
   
      do m = k-1,k+2
        term = y(m)
        do  j = k-1,k+2
          if ( m .ne. j ) then
            term = term * (xint - X(j))/(X(m) - X(j))
          end if
        enddo 
        yout = yout + term
      enddo
   
      return
      end

C----------------------------------------------------------------
      SUBROUTINE PMUT1_OCEANS (rjd,cor_x,cor_y,cor_ut1,cor_lod)
C
C    This subroutine provides, in time domain, the diurnal/subdiurnal
C    tidal effets on polar motion ("), UT1 (s) and LOD (s). The tidal terms,
C    listed in the program above, have been extracted from the procedure   
C    ortho_eop.f coed by Eanes in 1997.
C    
C    N.B.:  The fundamental lunisolar arguments are those of Simon et al.  
C
C    These corrections should be added to "average"
C    EOP values to get estimates of the instantaneous values.
C
C     PARAMETERS ARE :
C     rjd      - epoch of interest given in mjd
C     cor_x    - tidal correction in x (sec. of arc)
C     cor_y    - tidal correction in y (sec. of arc)
C     cor_ut1  - tidal correction in UT1-UTC (sec. of time)
C     cor_lod  - tidal correction in length of day (sec. of time)
C
C     coded by Ch. Bizouard (2002), initially coded by McCarthy and 
C     D.Gambis(1997) for the 8 prominent tidal waves.  
      
      IMPLICIT NONE
      
      INTEGER nlines
      PARAMETER(nlines=71)
      DOUBLE PRECISION ARG(6),    ! Array of the tidal arguments   
     .                 DARG(6)    ! Array of their time derivative 
      
      REAL*4 XCOS(nlines),XSIN(nlines),
     .YCOS(nlines),YSIN(nlines),UTCOS(nlines),UTSIN(nlines)
     
      REAL*8 t,ag,dag,rjd,halfpi,secrad,
     .       cor_x,cor_y,cor_ut1,cor_lod
      INTEGER NARG(nlines,6),i,j
      
      halfpi = 1.5707963267948966d0
      secrad=2.d0*halfpi/(180.d0*3600.d0)	

c  Oceanic tidal terms present in x (microas),y(microas),ut1(microseconds)       
c  NARG(j,6) : Multipliers of GMST+pi and Delaunay arguments. 
	
       data( 
     & NARG(j,1),NARG(j,2),NARG(j,3),NARG(j,4),NARG(j,5),NARG(j,6),
     & XSIN(j),XCOS(j),YSIN(j),YCOS(j),UTSIN(j),UTCOS(j),j=1,nlines)/
     &1,-1, 0,-2,-2,-2,  -0.05,   0.94,  -0.94,  -0.05,  0.396, -0.078,
     &1,-2, 0,-2, 0,-1,   0.06,   0.64,  -0.64,   0.06,  0.195, -0.059,
     &1,-2, 0,-2, 0,-2,   0.30,   3.42,  -3.42,   0.30,  1.034, -0.314,
     &1, 0, 0,-2,-2,-1,   0.08,   0.78,  -0.78,   0.08,  0.224, -0.073,
     &1, 0, 0,-2,-2,-2,   0.46,   4.15,  -4.15,   0.45,  1.187, -0.387,
     &1,-1, 0,-2, 0,-1,   1.19,   4.96,  -4.96,   1.19,  0.966, -0.474,
     &1,-1, 0,-2, 0,-2,   6.24,  26.31, -26.31,   6.23,  5.118, -2.499,
     &1, 1, 0,-2,-2,-1,   0.24,   0.94,  -0.94,   0.24,  0.172, -0.090,
     &1, 1, 0,-2,-2,-2,   1.28,   4.99,  -4.99,   1.28,  0.911, -0.475,
     &1, 0, 0,-2, 0, 0,  -0.28,  -0.77,   0.77,  -0.28, -0.093,  0.070,
     &1, 0, 0,-2, 0,-1,   9.22,  25.06, -25.06,   9.22,  3.025, -2.280,
     &1, 0, 0,-2, 0,-2,  48.82, 132.91,-132.90,  48.82, 16.020,-12.069,
     &1,-2, 0, 0, 0, 0,  -0.32,  -0.86,   0.86,  -0.32, -0.103,  0.078,
     &1, 0, 0, 0,-2, 0,  -0.66,  -1.72,   1.72,  -0.66, -0.194,  0.154,
     &1,-1, 0,-2, 2,-2,  -0.42,  -0.92,   0.92,  -0.42, -0.083,  0.074,
     &1, 1, 0,-2, 0,-1,  -0.30,  -0.64,   0.64,  -0.30, -0.057,  0.050,
     &1, 1, 0,-2, 0,-2,  -1.61,  -3.46,   3.46,  -1.61, -0.308,  0.271,
     &1,-1, 0, 0, 0, 0,  -4.48,  -9.61,   9.61,  -4.48, -0.856,  0.751,
     &1,-1, 0, 0, 0,-1,  -0.90,  -1.93,   1.93,  -0.90, -0.172,  0.151,
     &1, 1, 0, 0,-2, 0,  -0.86,  -1.81,   1.81,  -0.86, -0.161,  0.137,
     &1, 0,-1,-2, 2,-2,   1.54,   3.03,  -3.03,   1.54,  0.315, -0.189,
     &1, 0, 0,-2, 2,-1,  -0.29,  -0.58,   0.58,  -0.29, -0.062,  0.035,
     &1, 0, 0,-2, 2,-2,  26.13,  51.25, -51.25,  26.13,  5.512, -3.095,
     &1, 0, 1,-2, 2,-2,  -0.22,  -0.42,   0.42,  -0.22, -0.047,  0.025,
     &1, 0,-1, 0, 0, 0,  -0.61,  -1.20,   1.20,  -0.61, -0.134,  0.070,
     &1, 0, 0, 0, 0, 1,   1.54,   3.00,  -3.00,   1.54,  0.348, -0.171,
     &1, 0, 0, 0, 0, 0, -77.48,-151.74, 151.74, -77.48,-17.620,  8.548,
     &1, 0, 0, 0, 0,-1, -10.52, -20.56,  20.56, -10.52, -2.392,  1.159,
     &1, 0, 0, 0, 0,-2,   0.23,   0.44,  -0.44,   0.23,  0.052, -0.025,
     &1, 0, 1, 0, 0, 0,  -0.61,  -1.19,   1.19,  -0.61, -0.144,  0.065,
     &1, 0, 0, 2,-2, 2,  -1.09,  -2.11,   2.11,  -1.09, -0.267,  0.111,
     &1,-1, 0, 0, 2, 0,  -0.69,  -1.43,   1.43,  -0.69, -0.288,  0.043,
     &1, 1, 0, 0, 0, 0,  -3.46,  -7.28,   7.28,  -3.46, -1.610,  0.187,
     &1, 1, 0, 0, 0,-1,  -0.69,  -1.44,   1.44,  -0.69, -0.320,  0.037,
     &1, 0, 0, 0, 2, 0,  -0.37,  -1.06,   1.06,  -0.37, -0.407, -0.005,
     &1, 2, 0, 0, 0, 0,  -0.17,  -0.51,   0.51,  -0.17, -0.213, -0.005,
     &1, 0, 0, 2, 0, 2,  -1.10,  -3.42,   3.42,  -1.09, -1.436, -0.037,
     &1, 0, 0, 2, 0, 1,  -0.70,  -2.19,   2.19,  -0.70, -0.921, -0.023,
     &1, 0, 0, 2, 0, 0,  -0.15,  -0.46,   0.46,  -0.15, -0.193, -0.005,
     &1, 1, 0, 2, 0, 2,  -0.03,  -0.59,   0.59,  -0.03, -0.396, -0.024,
     &1, 1, 0, 2, 0, 1,  -0.02,  -0.38,   0.38,  -0.02, -0.253, -0.015,
     &2,-3, 0,-2, 0,-2,  -0.49,  -0.04,   0.63,   0.24, -0.089, -0.011,
     &2,-1, 0,-2,-2,-2,  -1.33,  -0.17,   1.53,   0.68, -0.224, -0.032,
     &2,-2, 0,-2, 0,-2,  -6.08,  -1.61,   3.13,   3.35, -0.637, -0.177,
     &2, 0, 0,-2,-2,-2,  -7.59,  -2.05,   3.44,   4.23, -0.745, -0.222,
     &2, 0, 1,-2,-2,-2,  -0.52,  -0.14,   0.22,   0.29, -0.049, -0.015,
     &2,-1,-1,-2, 0,-2,   0.47,   0.11,  -0.10,  -0.27,  0.033,  0.013,
     &2,-1, 0,-2, 0,-1,   2.12,   0.49,  -0.41,  -1.23,  0.141,  0.058,
     &2,-1, 0,-2, 0,-2, -56.87, -12.93,  11.15,  32.88, -3.795, -1.556,
     &2,-1, 1,-2, 0,-2,  -0.54,  -0.12,   0.10,   0.31, -0.035, -0.015,
     &2, 1, 0,-2,-2,-2, -11.01,  -2.40,   1.89,   6.41, -0.698, -0.298,
     &2, 1, 1,-2,-2,-2,  -0.51,  -0.11,   0.08,   0.30, -0.032, -0.014,
     &2,-2, 0,-2, 2,-2,   0.98,   0.11,  -0.11,  -0.58,  0.050,  0.022,
     &2, 0,-1,-2, 0,-2,   1.13,   0.11,  -0.13,  -0.67,  0.056,  0.025,
     &2, 0, 0,-2, 0,-1,  12.32,   1.00,  -1.41,  -7.31,  0.605,  0.266,
     &2, 0, 0,-2, 0,-2,-330.15, -26.96,  37.58, 195.92,-16.195, -7.140,
     &2, 0, 1,-2, 0,-2,  -1.01,  -0.07,   0.11,   0.60, -0.049, -0.021,
     &2,-1, 0,-2, 2,-2,   2.47,  -0.28,  -0.44,  -1.48,  0.111,  0.034,
     &2, 1, 0,-2, 0,-2,   9.40,  -1.44,  -1.88,  -5.65,  0.425,  0.117,
     &2,-1, 0, 0, 0, 0,  -2.35,   0.37,   0.47,   1.41, -0.106, -0.029,
     &2,-1, 0, 0, 0,-1,  -1.04,   0.17,   0.21,   0.62, -0.047, -0.013,
     &2, 0,-1,-2, 2,-2,  -8.51,   3.50,   3.29,   5.11, -0.437, -0.019,
     &2, 0, 0,-2, 2,-2,-144.13,  63.56,  59.23,  86.56, -7.547, -0.159,
     &2, 0, 1,-2, 2,-2,   1.19,  -0.56,  -0.52,  -0.72,  0.064,  0.000,
     &2, 0, 0, 0, 0, 1,   0.49,  -0.25,  -0.23,  -0.29,  0.027, -0.001,
     &2, 0, 0, 0, 0, 0, -38.48,  19.14,  17.72,  23.11, -2.104,  0.041,
     &2, 0, 0, 0, 0,-1, -11.44,   5.75,   5.32,   6.87, -0.627,  0.015,
     &2, 0, 0, 0, 0,-2,  -1.24,   0.63,   0.58,   0.75, -0.068,  0.002,
     &2, 1, 0, 0, 0, 0,  -1.77,   1.79,   1.71,   1.04, -0.146,  0.037,
     &2, 1, 0, 0, 0,-1,  -0.77,   0.78,   0.75,   0.45, -0.064,  0.017,
     &2, 0, 0, 2, 0, 2,  -0.33,   0.62,   0.65,   0.19, -0.049,  0.018/

      T = (rjd - 51544.5D0)/36525.0D0  ! julian century

C Arguments in the following order : chi=GMST+pi,l,lp,F,D,Omega
C et leur derivee temporelle 

      ARG(1) = (67310.54841d0 +
     .        (876600d0*3600d0 + 8640184.812866d0)*T +
     .         0.093104d0*T**2 -
     .         6.2d-6*T**3)*15.0d0 + 648000.0d0
      ARG(1)=dmod(ARG(1),1296000d0)*secrad 
   
      DARG(1) = (876600d0*3600d0 + 8640184.812866d0 
     .         + 2.d0 * 0.093104d0 * T - 3.d0 * 6.2d-6*T**2)*15.d0
      DARG(1) = DARG(1)* secrad / 36525.0D0   ! rad/day


      ARG(2) = -0.00024470d0*T**4 + 0.051635d0*T**3 + 31.8792d0*T**2
     .  + 1717915923.2178d0*T + 485868.249036d0
      ARG(2) = DMOD(ARG(2),1296000d0)*secrad
      
      DARG(2) = -4.d0*0.00024470d0*T**3 + 3.d0*0.051635d0*T**2 
     .  + 2.d0*31.8792d0*T + 1717915923.2178d0 
      DARG(2) = DARG(2)* secrad / 36525.0D0   ! rad/day

      ARG(3) = -0.00001149d0*T**4 - 0.000136d0*T**3
     .  -  0.5532d0*T**2
     .  + 129596581.0481d0*T + 1287104.79305d0
      ARG(3) = DMOD(ARG(3),1296000d0)*secrad

      DARG(3) = -4.D0*0.00001149d0*T**3 - 3.d0*0.000136d0*T**2
     .  -  2.D0*0.5532d0*T + 129596581.0481d0
      DARG(3) = DARG(3)* secrad / 36525.0D0   ! rad/day
          
      ARG(4) = 0.00000417d0*T**4 - 0.001037d0*T**3 - 12.7512d0*T**2
     .  + 1739527262.8478d0*T + 335779.526232d0
      ARG(4) = DMOD(ARG(4),1296000d0)*secrad

      DARG(4) = 4.d0*0.00000417d0*T**3 - 3.d0*0.001037d0*T**2 
     .- 2.d0 * 12.7512d0*T + 1739527262.8478d0 
      DARG(4) = DARG(4)* secrad / 36525.0D0   ! rad/day
    
      ARG(5) = -0.00003169d0*T**4 + 0.006593d0*T**3 - 6.3706d0*T**2
     .  + 1602961601.2090d0*T + 1072260.70369d0
      ARG(5) = DMOD(ARG(5),1296000d0)*secrad

      DARG(5) = -4.d0*0.00003169d0*T**3 + 3.d0*0.006593d0*T**2
     . - 2.d0 * 6.3706d0*T + 1602961601.2090d0
      DARG(5) = DARG(5)* secrad / 36525.0D0   ! rad/day

      ARG(6) = -0.00005939d0*T**4 + 0.007702d0*T**3
     .  + 7.4722d0*T**2
     .  - 6962890.2665d0*T + 450160.398036d0
      ARG(6) = DMOD(ARG(6),1296000d0)*secrad

      DARG(6) = -4.d0*0.00005939d0*T**3 + 3.d0 * 0.007702d0*T**2
     .  + 2.d0 * 7.4722d0*T - 6962890.2665d0
      DARG(6) = DARG(6)* secrad / 36525.0D0   ! rad/day

C CORRECTIONS

	cor_x  = 0.d0
	cor_y  = 0.d0
	cor_ut1= 0.d0
	cor_lod= 0.d0

 	do j=1,nlines
 	
	ag  = 0.d0
 	dag = 0.d0
		do i=1,6
 		ag  = ag  + dble(narg(j,i))*ARG(i)
 		dag = dag + dble(narg(j,i))*DARG(i)
		enddo
	ag=dmod(ag,4.d0*halfpi)

      cor_x = cor_x + dble(XCOS(j)) *dcos(ag) + dble(XSIN(j)) * dsin(ag)
      cor_y = cor_y + dble(YCOS(j)) *dcos(ag) + dble(YSIN(j)) * dsin(ag)
        cor_ut1= cor_ut1 + dble(UTCOS(j))*dcos(ag)
     &                   + dble(UTSIN(j))* dsin(ag)
        cor_lod= cor_lod -(-dble(UTCOS(j)) * dsin(ag) 
     &                    + dble(UTSIN(j)) * dcos(ag) ) * dag   	 

        enddo
  
       cor_x   = cor_x * 1.0d-6   ! arcsecond (")
       cor_y   = cor_y * 1.0d-6   ! arcsecond (")
       cor_ut1 = cor_ut1 * 1.0d-6 ! second (s)
       cor_lod = cor_lod * 1.0d-6 ! second (s)
 
      RETURN
      END
      	
C----------------------------------------------------------------
      SUBROUTINE PM_GRAVI (rjd,cor_x,cor_y)
C
C    This subroutine provides, in time domain, the diurnal
C    lunisolar effet on polar motion (")
C    
C    N.B.:  The fundamental lunisolar arguments are those of Simon et al.  
C
C    These corrections should be added to "average"
C    EOP values to get estimates of the instantaneous values.
C
C     PARAMETERS ARE :
C     rjd      - epoch of interest given in mjd
C     cor_x    - tidal correction in x (sec. of arc)
C     cor_y    - tidal correction in y (sec. of arc)
C
C     coded by Ch. Bizouard (2002)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      INTEGER nlines
      PARAMETER(nlines=10)
      DOUBLE PRECISION ARG(6)    ! Array of the tidal arguments   
      REAL*4 XCOS(nlines),XSIN(nlines),YCOS(nlines),YSIN(nlines)
      INTEGER NARG(nlines,6)
      
      halfpi = 1.5707963267948966d0
      secrad=2.d0*halfpi/(180.d0*3600.d0)	

c  Diurnal lunisolar tidal terms present in x (microas),y(microas)      
c  NARG(j,6) : Multipliers of GMST+pi and Delaunay arguments. 
	
       data( 
     & NARG(j,1),NARG(j,2),NARG(j,3),NARG(j,4),NARG(j,5),NARG(j,6),
     & XSIN(j),XCOS(j),YSIN(j),YCOS(j),j=1,nlines)/    
     & 1,-1, 0,-2, 0,-1,    -.44,   .25,   -.25,  -.44,
     & 1,-1, 0,-2, 0,-2,   -2.31,  1.32,  -1.32, -2.31,
     & 1, 1, 0,-2,-2,-2,    -.44,   .25,   -.25,  -.44,
     & 1, 0, 0,-2, 0,-1,   -2.14,  1.23,  -1.23, -2.14,
     & 1, 0, 0,-2, 0,-2,  -11.36,  6.52,  -6.52,-11.36,
     & 1,-1, 0, 0, 0, 0,     .84,  -.48,    .48,   .84,
     & 1, 0, 0,-2, 2,-2,   -4.76,  2.73,  -2.73, -4.76,
     & 1, 0, 0, 0, 0, 0,   14.27, -8.19,   8.19, 14.27,
     & 1, 0, 0, 0, 0,-1,    1.93, -1.11,   1.11,  1.93,
     & 1, 1, 0, 0, 0, 0,     .76,  -.43,    .43,   .76/
 
      T = (rjd - 51544.5D0)/36525.0D0  ! julian century

C Arguments in the following order : chi=GMST+pi,l,lp,F,D,Omega
C et leur derivee temporelle 

      ARG(1) = (67310.54841d0 +
     .        (876600d0*3600d0 + 8640184.812866d0)*T +
     .         0.093104d0*T**2 -
     .         6.2d-6*T**3)*15.0d0 + 648000.0d0
      ARG(1)=dmod(ARG(1),1296000d0)*secrad 
   

      ARG(2) = -0.00024470d0*T**4 + 0.051635d0*T**3 + 31.8792d0*T**2
     .  + 1717915923.2178d0*T + 485868.249036d0
      ARG(2) = DMOD(ARG(2),1296000d0)*secrad
      

      ARG(3) = -0.00001149d0*T**4 - 0.000136d0*T**3
     .  -  0.5532d0*T**2
     .  + 129596581.0481d0*T + 1287104.79305d0
      ARG(3) = DMOD(ARG(3),1296000d0)*secrad

          
      ARG(4) = 0.00000417d0*T**4 - 0.001037d0*T**3 - 12.7512d0*T**2
     .  + 1739527262.8478d0*T + 335779.526232d0
      ARG(4) = DMOD(ARG(4),1296000d0)*secrad

    
      ARG(5) = -0.00003169d0*T**4 + 0.006593d0*T**3 - 6.3706d0*T**2
     .  + 1602961601.2090d0*T + 1072260.70369d0
      ARG(5) = DMOD(ARG(5),1296000d0)*secrad

  
      ARG(6) = -0.00005939d0*T**4 + 0.007702d0*T**3
     .  + 7.4722d0*T**2
     .  - 6962890.2665d0*T + 450160.398036d0
      ARG(6) = DMOD(ARG(6),1296000d0)*secrad


C CORRECTIONS

	cor_x  = 0.d0
	cor_y  = 0.d0

 	do j=1,nlines
 	
	ag  = 0.d0
		do i=1,6
 		ag  = ag  + dble(narg(j,i))*ARG(i)
		enddo
	ag=dmod(ag,4.d0*halfpi)

        cor_x =cor_x+dble(XCOS(j))*dcos(ag)+dble(XSIN(j))*dsin(ag)
        cor_y =cor_y+dble(YCOS(j))*dcos(ag)+dble(YSIN(j))*dsin(ag) 

        enddo
  
      cor_x = cor_x * 1.0d-6   ! arcsecond (")
      cor_y = cor_y * 1.0d-6   ! arcsecond (")
 
      RETURN

      END

      SUBROUTINE UTLIBR (RMJD, DUT1, DLOD)
*+
*  - - - - - - - - 
*   U T L I B R
*  - - - - - - - -
*
*  This routine is part of the International Earth Rotation and
*  Reference Systems Service (IERS) Conventions software collection.
**
** Downloaded from ftp:/tai.bipm.org/iers/conventions2010/chapter5
** and added to GAMIT by R. King 1 July 2013.  The only change is
** to append the subroutine FUNDARG.F from convetions/2010/chapter8
** to the bottom of this file.  This is nearly duplicative with 
** gamit/lib/funarg.f and kf/gen_util/fund_arg.f, which have 
** different calling arguments, but to stay consistent with the IERS 
** and to avoid some extra checking (needed eventually), we'll not 
** now unify these routines.
** 
*  This subroutine evaluates the model of subdiurnal libration
*  in the axial component of rotation, expressed by UT1 and LOD.
*  This effect is due to the influence of tidal gravitation on the
*  departures of the Earth's mass distribution from the rotational
*  symmetry, expressed by the non-zonal components of geopotential.
*  The amplitudes have been computed for an elastic Earth with liquid
*  core. The adopted truncation level is 0.033 microseconds in UT1
*  corresponding to the angular displacement of 0.5 microarcseconds
*  or to 0.015 mm at the planet surface. With this truncation level
*  the model contains 11 semidiurnal terms. The coefficients of
*  the model are given in Table 5.1b of the IERS Conventions (2010).
*
*  In general, Class 1, 2, and 3 models represent physical effects that
*  act on geodetic parameters while canonical models provide lower-level
*  representations or basic computations that are used by Class 1, 2, or
*  3 models.
* 
*  Status:  Class 3 model
*
*     Class 1 models are those recommended to be used a priori in the
*     reduction of raw space geodetic data in order to determine
*     geodetic parameter estimates.
*     Class 2 models are those that eliminate an observational
*     singularity and are purely conventional in nature.
*     Class 3 models are those that are not required as either Class
*     1 or 2.
*     Canonical models are accepted as is and cannot be classified as
*     a Class 1, 2, or 3 model.
*
*  Given:
*     rmjd        d      Time expressed as modified Julian date
*
*  Returned:
*     dUT1        d      Incremental UT1 in microseconds
*     dLOD        d      Incremental LOD in microseconds per day
*
*  Notes:
*  1) The procedure FUNDARG.F is the same as used by the program PMSDNUT2.F
*     which implements the corresponding model of the lunisolar libration in
*     polar motion.
*
*  Called:
*     FUNDARG             Compute the angular fundamental arguments
*
*  Test cases:
*     given input:  rmjd_a = 44239.1 ( January 1, 1980 2:24.00 )
*                   rmjd_b = 55227.4 ( January 31, 2010 9:35.59 )
*
*     expected output: dUT1_a =   2.441143834386761746D0 mus;
*                      dLOD_a = -14.78971247349449492D0 mus / day
*                      dUT1_b = - 2.655705844335680244D0 mus;
*                      dLOD_b =  27.39445826599846967D0 mus / day
*
*  References:
*
*     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
*     IERS Technical Note No. 36, BKG (2010)
*
*  Revisions:
*  2010 May       A.Brzezinski   Original code
*  2010 June  1   B.E.Stetzler   Initial changes to code
*  2010 June  2   B.E.Stetzler   Provided test case
*  2010 June  2   B.E.Stetzler   Capitalized all variables for FORTRAN
*                                77 compatibility
*  2010 June  2   B.E.Stetzler   Replaced call to PMARGS to FUNDARG
*                                for universal fundamental argument
*                                subroutine
*  2010 June  2   B.E.Stetzler   Validated test case using internally
*                                computed GMST and call to FUNDARG
*                                matched previous external call to
*                                PMARGS for all six parameters
*  2010 June  23  B.E.Stetzler   Modified coefficients of semi-diurnal
*                                variations in UT1 and LOD due to
*                                libration for a non-rigid Earth to
*                                coincide with Table 5.1b
*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION RMJD, DUT1, DLOD

*         ----------------------------
*           D E F I N I T I O N S
*         ----------------------------
*  iarg   - array defining for each of the 11 trigonometric terms a set
*           of 6 integer multipliers of the fundamental angular arguments
*  arg    - vector of the following 6 fundamental arguments used to
*           compute the angular argument of the trigonometric functions
*           arg(1:6) = [ GMST+pi, el, elp, f, d, om ]; this vector is
*           evaluated by the subroutine FUNDARG which is called as an 
*           external subroutine.  Originally evaluated by the subroutine
*           PMARGS. 
*  period - array of periods of the trigonometric terms of expansion, in
*           mean solar days; only for a check - not used in computations
*  dUT1s, dUT1c - sine and cosine coefficients of dUT1, in microseconds
*  dLODs, dLODc - sine and cosine coefficients of dLOD, in microseconds
*                 per day
*  angle  - angular argument of the trigonometric functions
*           angle = Sum(i=1:6) iarg(i,j)*arg(i), for j=1,11

      INTEGER I, J
      INTEGER IARG(6,11)
      DOUBLE PRECISION T, GMST, L, LP, F, D, OM
      DOUBLE PRECISION ARG(6)
      DOUBLE PRECISION PER(11), DUT1S(11), DUT1C(11), DLODS(11),
     .                 DLODC(11) 
      DOUBLE PRECISION ANGLE

* Set constants

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Arcseconds in a full circle
      DOUBLE PRECISION TURNAS
      PARAMETER ( TURNAS = 1296000D0 )

*  rmjd0   - modified Julian date of J2000
*  twopi   - 2*pi

      DOUBLE PRECISION RMJD0, PI, TWOPI
      PARAMETER ( RMJD0   = 51544.5D0                )
      PARAMETER ( PI      = 3.141592653589793238462643D0 )
      PARAMETER ( TWOPI   = 6.283185307179586476925287D0 )

*  Radians to seconds
      DOUBLE PRECISION RAD2SEC
      PARAMETER ( RAD2SEC = 86400D0/TWOPI            )

* Coefficients of the quasi semidiurnal terms in dUT1, dLOD 
* Source: IERS Conventions (2010), Table 5.1b

      DATA 
     .((IARG(I,J),I=1,6), PER(J), DUT1S(J),DUT1C(J), DLODS(J), DLODC(J),
     .                                                           J=1,11)
     ./2, -2,  0, -2,  0, -2, 0.5377239,  0.05, -0.03,  -0.3,  -0.6,
     . 2,  0,  0, -2, -2, -2, 0.5363232,  0.06, -0.03,  -0.4,  -0.7,
     . 2, -1,  0, -2,  0, -2, 0.5274312,  0.35, -0.20,  -2.4,  -4.1,
     . 2,  1,  0, -2, -2, -2, 0.5260835,  0.07, -0.04,  -0.5,  -0.8,
     . 2,  0,  0, -2,  0, -1, 0.5175645, -0.07,  0.04,   0.5,   0.8,
     . 2,  0,  0, -2,  0, -2, 0.5175251,  1.75, -1.01, -12.2, -21.3,
     . 2,  1,  0, -2,  0, -2, 0.5079842, -0.05,  0.03,   0.3,   0.6,
     . 2,  0, -1, -2,  2, -2, 0.5006854,  0.04, -0.03,  -0.3,  -0.6,
     . 2,  0,  0, -2,  2, -2, 0.5000000,  0.76, -0.44,  -5.5,  -9.6,
     . 2,  0,  0,  0,  0,  0, 0.4986348,  0.21, -0.12,  -1.5,  -2.6,
     . 2,  0,  0,  0,  0, -1, 0.4985982,  0.06, -0.04,  -0.4,  -0.8/

* Compute the harmonic model of dUT1 and dLOD 
* dUT1 and dLOD are set to zero first 
      DUT1 = 0D0
      DLOD = 0D0

* Evaluate the vector of the fundamental arguments
* arg(1:6) = [ GMST+pi, el, elp, f, d, om ] at t = rmjd

*  Convert the input epoch to Julian centuries of TDB since J2000
      T = (RMJD-RMJD0)/36525D0

*  Compute GMST + pi
      GMST = MOD (   67310.54841D0 +
     .               T*( (8640184.812866D0 + 3155760000D0) +
     .               T*( 0.093104D0 +
     .               T*( -0.0000062 ))), 86400D0 )

      CALL FUNDARG ( T, L, LP, F, D, OM )

      ARG(1) = GMST / RAD2SEC + PI
      ARG(1) = DMOD( ARG(1), TWOPI )
      ARG(2) = L
      ARG(3) = LP
      ARG(4) = F
      ARG(5) = D
      ARG(6) = OM 

      DO 20 J=1,11

* For the j-th term of the trigonometric expansion, compute the angular
* argument angle of sine and cosine functions as a linear integer
* combination of the 6 fundamental arguments
        ANGLE = 0D0
        DO 10 I=1,6
          ANGLE = ANGLE + IARG(I,J) * ARG(I)
   10   CONTINUE
        ANGLE = DMOD( ANGLE, TWOPI )

* Compute contribution from the j-th term of expansion to dUT1 and dLOD 
        DUT1 = DUT1 + DUT1S(J)*DSIN(ANGLE) + DUT1C(J)*DCOS(ANGLE)
        DLOD = DLOD + DLODS(J)*DSIN(ANGLE) + DLODC(J)*DCOS(ANGLE)
   20 CONTINUE
      RETURN

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2008
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
*-----------------------------------------------------------------------
      END
      
*************************************************************************

      SUBROUTINE FUNDARG ( T, L, LP, F, D, OM )
*+
*  - - - - - - - - - - -
*   F U N D A R G 
*  - - - - - - - - - - -
*
*  This routine is part of the International Earth Rotation and
*  Reference Systems Service (IERS) Conventions software collection.
*
*  This subroutine computes the lunisolar fundamental arguments.
*  The model used is from Simon et al. (1994) as recommended by the IERS
*  Conventions (2010).  Refer to IERS Conventions (2010) Chapter 5 
*  Sections 5.7.1 - 5.7.2 (pp. 57 - 59).
*
*  In general, Class 1, 2, and 3 models represent physical effects that
*  act on geodetic parameters while canonical models provide lower-level
*  representations or basic computations that are used by Class 1, 2, or
*  3 models.
* 
*  Status: Canonical model
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
*  Given:
*     T           d      TT, Julian centuries since J2000 (Note 1)
*
*  Returned:
*     L           d      Mean anomaly of the Moon (Note 2)
*     LP          d      Mean anomaly of the Sun (Note 2)
*     F           d      L - OM (Notes 2 and 3)
*     D           d      Mean elongation of the Moon from the Sun
*                                                         (Note 2)
*     OM          d      Mean longitude of the ascending node of
*                                                the Moon (Note 2)
*
*  Notes:
*
*  1) Though T is strictly TDB, it is usually more convenient to use
*     TT, which makes no significant difference.  Julian centuries since
*     J2000 is (JD - 2451545.0)/36525.
*
*  2) The expression used is as adopted in IERS Conventions (2010) and
*     is from Simon et al. (1994).  Arguments are in radians.
*
*  3) L in this instance is the Mean Longitude of the Moon. OM is the 
*     Mean longitude of the ascending node of the Moon.
*
*  Test case:
*     given input: T = 0.07995893223819302 Julian centuries since J2000
*                  (MJD = 54465)
*     expected output:  L = 2.291187512612069099 radians
*                       LP = 6.212931111003726414 radians
*                       F = 3.658025792050572989 radians
*                       D = 4.554139562402433228 radians
*                       OM = -0.5167379217231804489 radians
*
*  References:
*
*     Simon, J.-L., Bretagnon, P., Chapront, J., Chapront-Touze, M.,
*     Francou, G., Laskar, J., 1994, Astron.Astrophys. 282, 663-683
*
*     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
*     IERS Technical Note No. 36, BKG (2010)
*
*  Revisions:
*  2008 January 18 B.E.Stetzler  Initial changes to header
*               and used 2PI instead of PI as parameter
*  2008 January 25 B.E. Stetzler Additional changes to header
*               and defined fundamental arguments
*  2008 January 28 B.E. Stetzler Additional changes to header
*  2008 March   12 B.E. Stetzler Applied changes to wording of notes.
*  2008 April   03 B.E. Stetzler Provided example test case.
*  2009 February 11 B.E. Stetzler Corrected term in OM from 6962890.2665
*                                 to 6962890.5431 and updated test case
*  2009 May     07 B.E. Stetzler Code formatting changes based on 
*                                client recommendations
*  2009 May     07 B.E. Stetzler Updated test case due to above changes
*  2010 February 25 B.E. Stetzler Recalculation of fundamental arguments
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION T, L, LP, F, D, OM

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Arcseconds in a full circle
      DOUBLE PRECISION TURNAS
      PARAMETER ( TURNAS = 1296000D0 )

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Compute the fundamental argument L.
      L = MOD (       485868.249036D0 +
     .                  T*( 1717915923.2178D0 +
     .                  T*(         31.8792D0 +
     .                  T*(          0.051635D0 +
     .                  T*(        - 0.00024470D0 )))), TURNAS ) * DAS2R

*  Compute the fundamental argument LP.
      LP = MOD (       1287104.79305D0 +
     .            T*( 129596581.0481D0 +
     .            T*(       - 0.5532D0 +
     .            T*(         0.000136D0 +
     .            T*(       - 0.00001149D0 )))), TURNAS ) * DAS2R

*  Compute the fundamental argument F.
      F  = MOD (       335779.526232D0 +
     .                  T*( 1739527262.8478D0 +
     .                  T*(       - 12.7512D0 +
     .                  T*(       -  0.001037D0 +
     .                  T*(          0.00000417D0 )))), TURNAS ) * DAS2R

*  Compute the fundamental argument D.
      D = MOD (        1072260.70369D0 +
     .          T*( 1602961601.2090D0 +
     .          T*(        - 6.3706D0 +
     .          T*(          0.006593D0 +
     .          T*(        - 0.00003169D0 )))), TURNAS ) * DAS2R

*  Compute the fundamental argument OM.
      OM = MOD (       450160.398036D0 +
     .             T*( - 6962890.5431D0 +
     .             T*(         7.4722D0 +
     .             T*(         0.007702D0 +
     .             T*(       - 0.00005939D0 )))), TURNAS ) * DAS2R

*  Finished.

      END
