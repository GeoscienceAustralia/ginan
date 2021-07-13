MODULE m_pd_ECOM


! ----------------------------------------------------------------------
! MODULE: m_pd_ECOM
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the m_pd_ECOM subroutine
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng, Geosceince Australia, Australia 
!               
!               
! Created:      05-12-2018
! ----------------------------------------------------------------------

      IMPLICIT NONE
      !SAVE


Contains


SUBROUTINE pd_ECOM (lambda, eBX_ecl, eclipsf, GM, GNSSid, r, v, r_sun, Asrp)


! ----------------------------------------------------------------------
! SUBROUTINE: pd_ECOM.f90
! ----------------------------------------------------------------------
! Purpose:
! This subrutine is used to compute the partial derivative of force
! w.r.t. ECOM SRP parameters.  
! ----------------------------------------------------------------------
! Input arguments:
! - GNSSid       : id of satellite constellation  
! - r            : satellite position vector (m)
! - v            : satellite velocity vector
! - r_sun        : Sun position vector
! - lambda       : shadow coefficient
! - eBX_ecl      : dynamic ex of satellite body frame
! 
! Output arguments:
! - Asrp         : Partial derivative output 
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
!
! Created:	08-Jan-2018
!
! Changes:  11-12-2018 Tzupang Tseng: make the PD matrix dynamic
!           31-01-2019 Tzupang Tseng: change the definition of ey by
!                                     dividing the length of ey
!           20-02-2019 Tzupang Tseng: create a function for switching on and
!                                     off some particular coefficients in ECOM models
!           22-03-2019 Tzupang Tseng: set a simple condition for the eclipsed satellites
!                                     where only D0 partials are setup to zero 
!           02-05-2019 Tzupang Tseng: use the coefficient from the shadow.f90 for scaling
!                                     the SRP effect
!           30-08-2019 Tzupang Tseng: implement the orbit-normal attitude for the BDS SRP estimation (still need to be tested)
!           03-09-2019 Tzupang Tseng: use a simple box-wing model as a priori SRP value where the ECOM is
!                                     used to adjust the box-wing model(BOX-WING + ECOM)
!           03-12-2019 Tzupang Tseng: add a function of estimating the parameters in the simple box wing model
!           20-07-2020 Tzupang Tseng: enable a hybrid ECOM1+ECOM2 model estimation
! 
! Copyright:  GEOSCIENCE AUSTRALIA
! ----------------------------------------------------------------------

      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: GNSSid
      REAL (KIND = prec_d) , Dimension(3), INTENT(IN) :: eBX_ecl
      REAL (KIND = prec_q), DIMENSION(3),INTENT(IN) :: r,v
      REAL (KIND = prec_q), DIMENSION(3),INTENT(IN) :: r_sun
      REAL (KIND = prec_q),INTENT(IN) :: GM
      REAL (KIND = prec_q),INTENT(IN) :: lambda
      INTEGER (KIND = 4):: eclipsf
      
! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: Cr,ab,Lab
      REAL (KIND = prec_q) :: ANG,E,Edot
      REAL (KIND = prec_q) :: Ds,sclfa
      REAL (KIND = prec_q) :: fxo,fyo,fzo
      REAL (KIND = prec_q), DIMENSION(4) :: cosang
! ----------------------------------------------------------------------
! Satellite physical informaiton
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: X_SIDE,Z_SIDE
      REAL (KIND = prec_q) :: A_SOLAR
      REAL (KIND = prec_q) :: F0,alpha
! ---------------------------------------------------------------------
      REAL (KIND = prec_q) :: R11(3,3),R33(3,3)
      REAL (KIND = prec_q), DIMENSION(3) :: er,ed,ey,eb,ex,ez,ev,en
      REAL (KIND = prec_q), DIMENSION(3) :: yy,et
      REAL (KIND = prec_q), DIMENSION(3) :: FXX(3),FZZ(3),FSP(3)
      REAL (KIND = prec_q), DIMENSION(9) :: kepler
      INTEGER              :: i,j,k,ECOM
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: u_sat,i_sat,omega_sat
      INTEGER              :: ex_i
      INTEGER              :: att_ON
! ----------------------------------------------------------------------
! Sun-related variables
! ----------------------------------------------------------------------
       REAL (KIND = prec_q) :: u_sun,beta,del_u, dang
       REAL (KIND = prec_q) :: xmul, zmul, solarmul
       REAL (KIND = prec_q), DIMENSION(3) :: r_sun1,r_sun2

      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: N_param, PD_Param_ID
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE,INTENT(OUT) :: Asrp
! ----------------------------------------------------------------------
      REAL (KIND = 8)      :: II, KN, U
      INTEGER*4 SVN,REFF,ERM,ANT,GRD,MONTH
      REAL*8  ACCEL(3),SUN(3)
      REAL*8  YSAT(6)

! ---------------------------------------------------------------------
    ex_i = 0 ! change the definition of the unit vector ex
             ! ex_i = 0 (default)
             !      = 1 (using dynamic ex vector from attitude routine)
  att_ON = 1 ! att_ON = 1 : use the orbit-normal attitude for BDS satellite
             !              when the beta < 4 deg
             !        = 0 : use the yaw-steering attitude for BDS satellite
             !              for all beta angles
! ---------------------------------------------------------------------

! default init
Z_SIDE = 0.d0
X_SIDE = 0.d0
A_SOLAR = 0.d0
F0 = 0.d0

call apr_srp(GNSSid, BLKTYP, X_SIDE, Z_SIDE, A_SOLAR, F0)


! The unit vector ez SAT->EARTH
      er(1)=r(1)/sqrt(r(1)**2+r(2)**2+r(3)**2)
      er(2)=r(2)/sqrt(r(1)**2+r(2)**2+r(3)**2)
      er(3)=r(3)/sqrt(r(1)**2+r(2)**2+r(3)**2)
      ez(1)=-er(1)
      ez(2)=-er(2)
      ez(3)=-er(3)

! The unit vector ed SAT->SUN (where is just opposite to the solar radiation vector)
      Ds=sqrt((r_sun(1)-r(1))**2+(r_sun(2)-r(2))**2+(r_sun(3)-r(3))**2)
      ed(1)=((r_sun(1)-r(1))/Ds)
      ed(2)=((r_sun(2)-r(2))/Ds)
      ed(3)=((r_sun(3)-r(3))/Ds)
! The unit vector ey = ez x ed/|ez x ed|, parallel to the rotation axis of solar panel
! If the Y-axis is reversed for IIR satellites, the result is identical to the
! IGS-defined. We have done the test (14/10/2020, Tzupang Tseng)
      CALL productcross (ez,ed,yy)
      ey(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
      ey(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
      ey(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)

! The unit vector of x side, which is always illuminated by the sun.

      CALL productcross (ey,ez,ex)

! Using the BODY-X univector from Kouba routine to redefine the ey for the satellite eclipsed
IF(ex_i .gt. 0) THEN
      ex(1)=eBX_ecl(1)/sqrt(eBX_ecl(1)**2+eBX_ecl(2)**2+eBX_ecl(3)**2)
      ex(2)=eBX_ecl(2)/sqrt(eBX_ecl(1)**2+eBX_ecl(2)**2+eBX_ecl(3)**2)
      ex(3)=eBX_ecl(3)/sqrt(eBX_ecl(1)**2+eBX_ecl(2)**2+eBX_ecl(3)**2)

      CALL productcross (ez,ex,ey) 
END IF
! The unit vector eb = ed x ey

      CALL productcross (ed,ey,eb)

! the orbit normal vector
!------------------------

      ev(1)=v(1)/sqrt(v(1)**2+v(2)**2+v(3)**2)
      ev(2)=v(2)/sqrt(v(1)**2+v(2)**2+v(3)**2)
      ev(3)=v(3)/sqrt(v(1)**2+v(2)**2+v(3)**2)

     CALL productcross (er,ev,en)

! computation of the satellite argument of latitude and orbit inclination
      CALL kepler_z2k (r,v,GM,kepler)
      u_sat = kepler(9)*Pi_global/180.d0
      i_sat = kepler(3)*Pi_global/180.d0
      omega_sat = kepler(4)*Pi_global/180.d0

! compute the sun position in the satellite orbit plane by rotating big Omega_sat and i_sat,
! allowing us for the computation of u_sun and sun elevation angles (beta)
!   sun                 sun
!  r    = R(big_w,i) x r
!   kep                 ICF
!
      DO i=1,3
         DO j=1,3
          R33(i,j)=1.0d0
         END DO
      END DO 

      R33(1,3)=0.0d0
      R33(2,3)=0.0d0
      R33(3,1)=0.0d0
      R33(3,2)=0.0d0

      DO i=1,3
         DO j=1,3
          R11(i,j)=1.0d0
         END DO 
      END DO 

      R11(1,2)=0.0d0
      R11(1,3)=0.0d0
      R11(2,1)=0.0d0
      R11(3,1)=0.0d0

! rotate big omega to make the X-axis of the celestial frame consistent with the
! direction of right ascension of ascending node
      CALL R3(omega_sat,R33)

! rotate inclination to make the XY plane of the celestial frame consistent with
! the orbit plane
      CALL R1(i_sat,R11)

! convert the sun position from the celestial frame to the orbit plane
      CALL matrix_Rr(R33,r_sun,r_sun1)
      CALL matrix_Rr(R11,r_sun1,r_sun2)

!compute the sun argument of lattitude and beta angle
      u_sun = atan2(r_sun2(2),r_sun2(1)) ! in rad
      beta  = atan2(r_sun2(3),sqrt(r_sun2(1)**2+r_sun2(2)**2)) ! in rad

      del_u = u_sat - u_sun ! in rad
      IF (del_u*180/Pi_global .GT.360.0d0) THEN
      del_u=del_u-2*Pi_global
      ELSE IF(del_u*180/Pi_global .LT.0.0d0) THEN
      del_u=del_u+2*Pi_global
      END IF 

! Implement the orbit-normal attitude for BDS satellites when the beat < 4 deg
! ----------------------------------------------------------------------------
     if (att_ON == 1) then
     if(BLKID == 301 ) then
         CALL productcross (ez,ev,yy)
        ey(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ey(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ey(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)

        CALL productcross (ed,ey,yy)
        eb(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        eb(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        eb(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        
        CALL productcross (ey,eb,yy)
        ed(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ed(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ed(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)

     elseif(BLKID == 302 .or.BLKID == 303) then
!        if (abs(beta*180.0d0/Pi_global) < 4.d0) then
        if (eclipsf == 3) then
        CALL productcross (ez,ev,yy)
        ey(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ey(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ey(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)

        CALL productcross (ed,ey,yy)
        eb(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        eb(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        eb(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)  

! Switch on the (ed,on) does not improve the orbit accuracy!!
! Only for testing purpose!
        CALL productcross (ey,eb,yy)
        ed(1)=yy(1)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ed(2)=yy(2)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        ed(3)=yy(3)/sqrt(yy(1)**2+yy(2)**2+yy(3)**2)
        end if
     end if
     end if
! ----------------------------------------------------------------------

!========================================
! angles between ed and each surface(k)
! k=1: +X
!   2: +Y
!   3: +Z
!   4: solar panels
!=======================================

     cosang(1)=ed(1)*ex(1)+ed(2)*ex(2)+ed(3)*ex(3)
     cosang(2)=ed(1)*ey(1)+ed(2)*ey(2)+ed(3)*ey(3)
     cosang(3)=ed(1)*ez(1)+ed(2)*ez(2)+ed(3)*ez(3)
     cosang(4)=ed(1)*ed(1)+ed(2)*ed(2)+ed(3)*ed(3)

xmul = 0.02
zmul = 0.02
solarmul = 1.7

! A scaling factor is applied to ECOM model
!******************************************************************
      sclfa=(AU/Ds)**2
! A priori SRP model
      alpha = 1.d0  ! yml_apriori_srp == SRP_NONE
      if (yml_apriori_srp == SRP_CANNONBALL) then
         alpha = F0/MASS

      else if (yml_apriori_srp == SRP_SIMPLE_BW) then
         xmul = 0.02
         zmul = 0.02
         solarmul = 1.7

         if(cosang(3)<0.d0) ez = ez*(-1.d0)
         fxo=Ps/MASS*(xmul*X_SIDE*cosang(1)*ex(1)+zmul*Z_SIDE*cosang(3)*ez(1)+solarmul*A_SOLAR*cosang(4)*ed(1))
         fyo=Ps/MASS*(xmul*X_SIDE*cosang(1)*ex(2)+zmul*Z_SIDE*cosang(3)*ez(2)+solarmul*A_SOLAR*cosang(4)*ed(2))
         fzo=Ps/MASS*(xmul*X_SIDE*cosang(1)*ex(3)+zmul*Z_SIDE*cosang(3)*ez(3)+solarmul*A_SOLAR*cosang(4)*ed(3))
         alpha = sqrt(fxo**2+fyo**2+fzo**2)

      else if (yml_apriori_srp == SRP_FULL_BW) then
         REFF = 0
         YSAT(1:3) = r
         YSAT(4:6) = v
         CALL SRPFBOXW(REFF,YSAT,R_SUN,SVNID,ACCEL)
         alpha = sqrt(ACCEL(1)**2+ACCEL(2)**2+ACCEL(3)**2)

      end if

! Partial derivatives w.r.t. unknown parameters

ALLOCATE (Asrp(3,ECOMNUM), STAT = AllocateStatus)
Asrp(:,:) = 0.d0


IF(yml_ECOM_mode/=ECOM_NONE .and. yml_ECOM_mode/=SBOXW) THEN
!print*,'ECOM1 partials'
        PD_Param_ID = 0
        IF (BTEST(yml_srp_parameters, ECOM_D_BIAS - one)) then
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*1.0d0*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*1.0d0*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*1.0d0*ed(3)*alpha
        IF (lambda .lt. 1) Asrp(1:3,PD_Param_ID) = lambda*Asrp(1:3,PD_Param_ID)
!print*,'D0=',PD_Param_ID
        End IF
        IF (BTEST(yml_srp_parameters, ECOM_Y_BIAS - one)) then
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*1.0d0*ey(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*1.0d0*ey(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*1.0d0*ey(3)*alpha
!print*,'Y0=',PD_Param_ID
        End IF
        IF (BTEST(yml_srp_parameters, ECOM_B_BIAS - one)) then
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*1.0d0*eb(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*1.0d0*eb(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*1.0d0*eb(3)*alpha
!print*,'B0=',PD_Param_ID
        End IF
        IF (BTEST(yml_srp_parameters, ECOM_D_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DCOS(del_u)*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DCOS(del_u)*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DCOS(del_u)*ed(3)*alpha
!print*,'DC=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DSIN(del_u)*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DSIN(del_u)*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DSIN(del_u)*ed(3)*alpha
!print*,'DS=',PD_Param_ID
        End IF
        IF (BTEST(yml_srp_parameters, ECOM_Y_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DCOS(del_u)*ey(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DCOS(del_u)*ey(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DCOS(del_u)*ey(3)*alpha
!print*,'YC=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DSIN(del_u)*ey(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DSIN(del_u)*ey(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DSIN(del_u)*ey(3)*alpha
!print*,'YS=',PD_Param_ID
        End IF
        IF (BTEST(yml_srp_parameters, ECOM_B_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        Asrp(1,PD_Param_ID) = -sclfa*DCOS(del_u)*eb(1)*alpha
        Asrp(2,PD_Param_ID) = -sclfa*DCOS(del_u)*eb(2)*alpha
        Asrp(3,PD_Param_ID) = -sclfa*DCOS(del_u)*eb(3)*alpha
!print*,'BC=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        Asrp(1,PD_Param_ID) = -sclfa*DSIN(del_u)*eb(1)*alpha
        Asrp(2,PD_Param_ID) = -sclfa*DSIN(del_u)*eb(2)*alpha
        Asrp(3,PD_Param_ID) = -sclfa*DSIN(del_u)*eb(3)*alpha
!print*,'BS=',PD_Param_ID
        End If

        IF (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DCOS(2*del_u)*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DCOS(2*del_u)*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DCOS(2*del_u)*ed(3)*alpha
! S term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DSIN(2*del_u)*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DSIN(2*del_u)*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DSIN(2*del_u)*ed(3)*alpha
        End IF
        IF (BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DCOS(4*del_u)*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DCOS(4*del_u)*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DCOS(4*del_u)*ed(3)*alpha
! S term
        PD_Param_ID = PD_Param_ID + 1
        Asrp (1,PD_Param_ID) = -sclfa*DSIN(4*del_u)*ed(1)*alpha
        Asrp (2,PD_Param_ID) = -sclfa*DSIN(4*del_u)*ed(2)*alpha
        Asrp (3,PD_Param_ID) = -sclfa*DSIN(4*del_u)*ed(3)*alpha
        End IF

        IF (ECOMNUM /= PD_Param_ID) THEN
            PRINT*, 'THE NUMBER OF FORCE PARAMETERS IS NOT CONSISTENT'
            PRINT*,           'ECOMNUM     =', ECOMNUM
            PRINT*,           'PD_Param_ID =', PD_Param_ID
            PRINT*, 'PROGRAM STOP AT m_pd_ECOM.f90'
            STOP
        END IF



ELSE IF (yml_ECOM_mode == SBOXW) THEN
     DO PD_Param_ID = 1, 7
        IF (PD_Param_ID == 1) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*ex(i)
           END DO
           IF (lambda .lt. 1) Asrp(1:3,PD_Param_ID) = lambda*Asrp(1:3,PD_Param_ID)
        ELSE IF (PD_Param_ID == 2) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*ez(i)
           END DO
           IF (lambda .lt. 1) Asrp(1:3,PD_Param_ID) = lambda*Asrp(1:3,PD_Param_ID)
        ELSE IF (PD_Param_ID == 3) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*ed(i)
           END DO
           IF (lambda .lt. 1) Asrp(1:3,PD_Param_ID) = lambda*Asrp(1:3,PD_Param_ID)
        ELSE IF (PD_Param_ID == 4) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*ey(i)
           END DO
        ELSE IF (PD_Param_ID == 5) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*eb(i)
           END DO
        ELSE IF (PD_Param_ID == 6) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*DCOS(del_u)*eb(i)
           END DO
        ELSE IF (PD_Param_ID == 7) THEN
           DO i = 1, 3
           Asrp (i,PD_Param_ID) = -sclfa*DSIN(del_u)*eb(i)
           END DO
        END IF
     END DO
END IF 
! ==================================================================
! use the shadow coefficient for scaling the SRP effect
!-------------------------------------------------------
!IF (lambda .lt. 1) THEN
!   IF(yml_ECOM_mode/=ECOM_NONE .and. yml_ECOM_mode/=SBOXW) Asrp(1:3,1) = lambda*Asrp(1:3,1)
!   IF(yml_ECOM_mode==SBOXW) THEN
!   Asrp(1:3,1) = lambda*Asrp(1:3,1)
!   Asrp(1:3,2) = lambda*Asrp(1:3,2)
!   Asrp(1:3,3) = lambda*Asrp(1:3,3)
!   END IF
!END IF
!-------------------------------------------------------
END SUBROUTINE

END MODULE 
