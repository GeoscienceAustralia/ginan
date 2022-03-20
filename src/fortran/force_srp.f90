
SUBROUTINE force_srp (lambda, eBX_ecl, eclipsf, GM, GNSSid, srpid, r, v, r_sun, fx, fy, fz)


! ----------------------------------------------------------------------
! SUBROUTINE: force_srp.f90
! ----------------------------------------------------------------------
! Purpose:
! Acceleration due to the solar radiation pressure 
! Computation of SRP acceleration using various SRP models, 
! such as a simply cannonball model (srpid=1),a box-wing model (srpid=2) 
! and a ECOM model (srpid=3) 
! ----------------------------------------------------------------------
! Input arguments:
! - GM           : the earth gravitational constant  
! - srpid        : =1: a simply cannonball model; 
!                  =2: box-wing model;
!                  =3: ECOM model
! - GNSSid       : id of satellite constellation
! - r            : satellite position vector (m)
! - v            : satellite velocity vector
! - r_sun        : Sun position vector
! - lambda       : shadow coefficient
! - eBX_ecl      : dynamic ex of satellite body frame
! 
! Output arguments:
! - fx,fy,fz:		Acceleration's cartesian components (m)
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
!
! Created:	01-10-2017
! 
! Changes:      10-10-2018 Tzupang Tseng: modify the Cannonball model to be 
!                                         similar to the box-wing model in
!                                         preparation for BDS with the ON
!                                         attitude mode.
!               11-12-2018 Tzupang Tseng: make the force matrix dynamic
!               23-01-2019 Tzupang Tseng: add the ECOM2 model
!               31-01-2019 Tzupang Tseng: change the definition of ey by
!                                         dividing the length of ey
!               20-02-2019 Tzupang Tseng: create a functionality for switching
!                                         on and off some particular coefficients in ECOM models
!               22-03-2019 Tzupang Tseng: set a simple condition for the eclipsed satellites
!                                         where only the D0 accelerations are setup to zero 
!               02-05-2019 Tzupang Tseng: use the coefficient from the shadow.f90 for scaling the SRP effect
!               30-08-2019 Tzupang Tseng: implement the orbit-normal attitude for the BDS SRP estimation (still need to be tested)
!               03-09-2019 Tzupang Tseng: use a simple box-wing model as a  priori SRP value where the ECOM is 
!                                         used to adjust the box-wing model (BOX-WING + ECOM)
!               03-12-2019 Tzupang Tseng: add a function of estimating the  parameters in the simple box wing model
!               20-07-2020 Tzupang Tseng: enable a hybrid ECOM1+ECOM2 modle estimation
!
! Copyright:  GEOSCIENCE AUSTRALIA, AUSTRALIA
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
      REAL (KIND = prec_q), INTENT (IN) :: lambda
      REAL (KIND = prec_d) , Dimension(3), INTENT(IN) :: eBX_ecl
      INTEGER                           :: srpid,ECOM
      INTEGER (KIND = 4):: eclipsf
      REAL (KIND = prec_q), DIMENSION(3) :: r,v,r_sun
      REAL (KIND = prec_q), INTENT(OUT)  :: fx,fy,fz
      CHARACTER (LEN=1) :: GNSSid
      CHARACTER (LEN=256) :: mesg

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: Cr
      REAL (KIND = prec_q) :: GM
!       ANG,
      REAL (KIND = prec_q) :: Ds,sclfa
      REAL (KIND = prec_q) :: fxo,fyo,fzo

      REAL (KIND = prec_q) :: R11(3,3),R33(3,3)
!       REAL (KIND = prec_q) :: surforce(4,3)
!       REAL (KIND = prec_q), DIMENSION(3) :: nz, nx, nd
      REAL (KIND = prec_q), DIMENSION(3) :: er,ed,ey,eb,ex,en,ev,ez,ECOM_ey
      REAL (KIND = prec_q), DIMENSION(3) :: yy,et
!       REAL (KIND = prec_q), DIMENSION(3) :: FXX(3),FZZ(3),FSP(3)
      REAL (KIND = prec_q), DIMENSION(3) :: fsrp
      REAL (KIND = prec_q), DIMENSION(9) :: kepler
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: srpcoef 
      REAL (KIND = prec_q), DIMENSION(4) :: cosang
      REAL (KIND = prec_q), DIMENSION(4) :: AREA1,REFL1,DIFU1
!       ,ABSP1
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
      INTEGER              :: ex_i
      INTEGER              :: i,j
!       ,k,m
      INTEGER              :: N_param, PD_Param_ID
      INTEGER              :: att_ON
! ----------------------------------------------------------------------
! Satellite physical informaiton
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: X_SIDE,Z_SIDE
      REAL (KIND = prec_q) :: A_SOLAR
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: u_sat,i_sat,omega_sat
      REAL (KIND = 8)      :: II, KN
!       , U
      REAL (KIND = prec_q) :: F0,alpha
! ----------------------------------------------------------------------
! Sun-related variables
! ----------------------------------------------------------------------
       REAL (KIND = prec_q) :: xmul, zmul, solarmul
       REAL (KIND = prec_q) :: u_sun
       REAL (KIND = prec_q) :: beta,del_u
       REAL (KIND = prec_q), DIMENSION(3) :: r_sun1,r_sun2
! ----------------------------------------------------------------------
       INTEGER*4 SVN,REFF,ERM,ANT,GRD,MONTH
       REAL*8  ACCEL(3),SUN(3)
       REAL*8  YSAT(6)
! ---------------------------------------------------------------------- 
! Numerical Constants
      Cr = 1.5 ! SRP coefficient ranges from 1.3 to 1.5
! ---------------------------------------------------------------------
    ex_i = 0 ! change the definition of the unit vector ex 
             ! ex_i = 0 (default)
             !      = 1 (using dynamic ex vector from attitude routine)
  att_ON = 1 ! att_ON = 1 : use the orbit-normal attitude for BDS satellite
             !              when the beta < 4 deg
             !        = 0 : use the yaw-steering attitude for BDS satellite 
             !              for all beta angles 
! ---------------------------------------------------------------------
     
! initialize the SRP force
! -------------------------
     DO i=1,3
         fsrp(i)=0.0d0
     END DO

! Initialise
Z_SIDE = 0.d0
X_SIDE = 0.d0
A_SOLAR = 0.d0
F0 = 0.d0

fx = 0.d0
fy = 0.d0
fz = 0.d0
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

! compute the sun position in the satellite orbit plane by rotating omega_sat and i_sat,
! allowing us for the computation of u_sun and sun elevation angles (beta)
      do i=1,3
         do j=1,3
          R33(i,j)=1.0d0
         end do
      end do
      R33(1,3)=0.0d0
      R33(2,3)=0.0d0
      R33(3,1)=0.0d0
      R33(3,2)=0.0d0
      R33(3,3)=1.0d0

      do i=1,3
         do j=1,3
          R11(i,j)=1.0d0
         end do
      end do
      R11(1,1)=1.0d0
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

! convert the sun position in the celestial frame to the orbit plane
      CALL matrix_Rr(R33,r_sun,r_sun1)
      CALL matrix_Rr(R11,r_sun1,r_sun2)

      beta  = atan2(r_sun2(3),sqrt(r_sun2(1)**2+r_sun2(2)**2)) ! in rad
!write (*,*) beta*180.0d0/Pi_global

      u_sun = atan2(r_sun2(2),r_sun2(1)) ! in rad

      del_u = u_sat - u_sun

      if (del_u*180/Pi_global .gt.360.0d0) then
      del_u=del_u-2*Pi_global
      else if(del_u*180/Pi_global .lt.0.0d0) then
      del_u=del_u+2*Pi_global
      end if

!print*,'del_u=, lambda=',del_u*180/Pi_global, lambda

! Implement the orbit-normal attitude for BDS satellites when the beat < 4 deg
! ----------------------------------------------------------------------------
     if (att_ON == 1) then
!     PRINT*,'The orbit-normal attitude is applied.'

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
        if (eclipsf == 3) then 
!        if (abs(beta*180.0d0/Pi_global) < 4.d0) then
!        PRINT*,'ed_YS =',ed, sqrt(ed(1)**2+ed(2)**2+ed(3)**2)
!        print*,'ey_YS =',ey, sqrt(ey(1)**2+ey(2)**2+ey(3)**2)
!        print*,'eb_YS =',eb, sqrt(eb(1)**2+eb(2)**2+eb(3)**2)

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
!        PRINT*,'ed_ON =',ed, sqrt(ed(1)**2+ed(2)**2+ed(3)**2)
!        print*,'ey_ON =',ey, sqrt(ey(1)**2+ey(2)**2+ey(3)**2)
!        print*,'eb_ON =',eb, sqrt(eb(1)**2+eb(2)**2+eb(3)**2)
!
        end if
     end if
     end if


!========================================
! angles between ed and each surface(k)
! k=1: +X
!   2: +Yi
!   3: +Z
!   4: solar panels
!=======================================

     cosang(1)=ed(1)*ex(1)+ed(2)*ex(2)+ed(3)*ex(3)
     cosang(2)=ed(1)*ey(1)+ed(2)*ey(2)+ed(3)*ey(3)
     cosang(3)=ed(1)*ez(1)+ed(2)*ez(2)+ed(3)*ez(3)
     cosang(4)=ed(1)*ed(1)+ed(2)*ed(2)+ed(3)*ed(3)


! A scaling factor is applied to ECOM model
!******************************************************************
      sclfa=(AU/Ds)**2
      alpha = 1.0d0 ! srpid == SRP_NONE

      if (srpid == SRP_CANNONBALL) then
         fx=-F0/MASS*ed(1)*lambda
         fy=-F0/MASS*ed(2)*lambda
         fz=-F0/MASS*ed(3)*lambda
         alpha = F0/MASS
      else if (srpid == SRP_SIMPLE_BW) then
         xmul = 0.02
         zmul = 0.02
         solarmul = 1.7

         if(cosang(3)<0.d0) ez = ez*(-1.d0)
         fxo=Ps/MASS*(xmul*X_SIDE*cosang(1)*ex(1)+zmul*Z_SIDE*cosang(3)*ez(1)+solarmul*A_SOLAR*cosang(4)*ed(1))
         fyo=Ps/MASS*(xmul*X_SIDE*cosang(1)*ex(2)+zmul*Z_SIDE*cosang(3)*ez(2)+solarmul*A_SOLAR*cosang(4)*ed(2))
         fzo=Ps/MASS*(xmul*X_SIDE*cosang(1)*ex(3)+zmul*Z_SIDE*cosang(3)*ez(3)+solarmul*A_SOLAR*cosang(4)*ed(3))
         alpha = sqrt(fxo**2+fyo**2+fzo**2)
         fx=-fxo*lambda
         fy=-fyo*lambda
         fz=-fzo*lambda

      else if (srpid == SRP_FULL_BW) then
         REFF = 0    ! 0: inertial frame,  1: satellite body-fixed frame, 
                     ! 2: sun-fixed frame, 3: orbital frame 
         YSAT(1:3) = r
         YSAT(4:6) = v
         CALL SRPFBOXW(REFF,YSAT,R_SUN,SVNID,ACCEL)
         alpha = sqrt(ACCEL(1)**2+ACCEL(2)**2+ACCEL(3)**2)
         fx=ACCEL(1)*lambda
         fy=ACCEL(2)*lambda
         fz=ACCEL(3)*lambda

      end if


ALLOCATE (srpcoef(ECOMNUM), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to allocate srpfcoef array, dimension=", ECOMNUM
        call report('FATAL', pgrm_name, 'force_srp', mesg, 'src/fortran/force_srp.f90', 1)
endif

!ALLOCATE (srpcoef(NPARAM_EMP_ECOM_glb), STAT = AllocateStatus)

srpcoef = 0.d0

! ECOM model
! ***********************************************************************
IF (yml_ECOM_mode /= ECOM_NONE .AND. yml_ECOM_mode /= SBOXW) then
        ! ie ECOM1, ECOM2 or ECOM_HYBRID
        PD_Param_ID = 0
        If (BTEST(yml_srp_parameters, ECOM_D_Bias - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        IF (lambda .lt. 1) srpcoef(PD_Param_ID) = lambda*srpcoef(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*ed(i)*alpha
        END DO
!print*,'ECOM1-caused accelerations'
!print*,'D0'
        End IF
        If (BTEST(yml_srp_parameters, ECOM_Y_Bias - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*ey(i)*alpha
        END DO
!print*,'Y0'
        End IF
        If (BTEST(yml_srp_parameters, ECOM_B_Bias - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*eb(i)*alpha
        END DO
!print*,'B0'
        End IF
        If (BTEST(yml_srp_parameters, ECOM_D_CPR - one)) THEN
! C term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DCOS(del_u)*ed(i)*alpha
        END DO
!print*,'DC'
! S term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DSIN(del_u)*ed(i)*alpha
        END DO
!print*,'DS'
        End IF
        If (BTEST(yml_srp_parameters, ECOM_Y_CPR - one)) THEN
! C term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DCOS(del_u)*ey(i)*alpha
        END DO
!print*,'YC'
! S term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DSIN(del_u)*ey(i)*alpha
        END DO
!print*,'YS'
        End IF
        If (BTEST(yml_srp_parameters, ECOM_B_CPR - one)) THEN
! C term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DCOS(del_u)*eb(i)*alpha
        END DO
!print*,'BC'
! S term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DSIN(del_u)*eb(i)*alpha
        END DO
!print*,'BS'
        End If

        If (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)) THEN
! C term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DCOS(2*del_u)*ed(i)*alpha
        END DO
! S term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DSIN(2*del_u)*ed(i)*alpha
        END DO
        End IF

        If (BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)) THEN
! C term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DCOS(4*del_u)*ed(i)*alpha
        END DO
! S term
        PD_Param_ID = PD_Param_ID + 1
        srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
        DO i=1,3
        fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DSIN(4*del_u)*ed(i)*alpha
        END DO
        End IF


        IF (ECOMNUM /= PD_Param_ID) THEN
        PRINT*, 'THE NUMBER OF FORCE PARAMETERS IS NOT CONSISTENT'
        PRINT*,           'ECOMNUM     =', ECOMNUM
        PRINT*,           'PD_Param_ID =', PD_Param_ID
        PRINT*,'PROGRAM STOP AT force_srp.f90'
        STOP
        END IF



! SIMPLE BOX WING
! *******************************************************************
ELSE IF (yml_ECOM_mode == SBOXW) THEN
        
      DO PD_Param_ID = 1, 7
         IF (PD_Param_ID == 1) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         IF (lambda .lt. 1) srpcoef(PD_Param_ID) = lambda*srpcoef(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*ex(i)
         END DO
         ELSE IF (PD_Param_ID == 2) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         IF (lambda .lt. 1) srpcoef(PD_Param_ID) = lambda*srpcoef(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*ez(i)
         END DO
         ELSE IF (PD_Param_ID == 3) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         IF (lambda .lt. 1) srpcoef(PD_Param_ID) = lambda*srpcoef(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*ed(i)
         END DO
         ELSE IF (PD_Param_ID == 4) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*ey(i)
         END DO
         ELSE IF (PD_Param_ID == 5) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*eb(i)
         END DO
         ELSE IF (PD_Param_ID == 6) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DCOS(del_u)*eb(i)
         END DO
         ELSE IF (PD_Param_ID == 7) THEN
         srpcoef (PD_Param_ID) = ECOM_accel_glb(PD_Param_ID)
         DO i=1,3
         fsrp(i) = fsrp(i) + srpcoef(PD_Param_ID)*sclfa*DSIN(del_u)*eb(i)
         END DO
         END IF
      END DO

END IF 


     fx=-fsrp(1)
     fy=-fsrp(2)
     fz=-fsrp(3)


!     END IF 

END
