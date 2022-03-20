MODULE m_statorbit2


! ----------------------------------------------------------------------
! MODULE: m_statorbit2.f95
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling statorbit2 subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	6 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			


Contains
	  
	  
SUBROUTINE statorbit2 (ds1, ds2, orbdiff)


! ----------------------------------------------------------------------
! SUBROUTINE: statorbit2.f95
! ----------------------------------------------------------------------
! Purpose:
!  Prepare the related orbital inforation for data analysis 
! ----------------------------------------------------------------------
! Input arguments:
! - orb_ref:		1st data series matrix (used as reference)
! - orb_det: 		2nd data series matrix
!
! Output arguments:
! - orbdiff:		Orbit differences in the orbital frame (RTN: radial, along-track and cross-track differences)
!   orbdiff = [MJD PRN BLOCKTYPE lambda beta(deg) del_u(deg) yaw(deg) ANGX(deg) ANGY(deg) ANGZ(deg) dR(m) dT(m) dN(m) FR(m^2/s) FT(m^2/s)  FN(m^2/s) ]
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Tzupang Tseng
!
! Created:	11 October 2017
!
! Changes:      20-05-2019   Tzupang Tseng : output the orbital information for data analysis
!
! Copyright: GEOSCIENCE AUSTRALIA, AUSTRALIA
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE m_statist
      USE mdl_planets
      USE m_orbinfo
      USE mdl_config
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), ALLOCATABLE, DIMENSION(:,:) :: ds1, ds2
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbdiff  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: delta_t, dt_limit
      INTEGER (KIND = prec_int8) :: Nepochs, Nepochs2, Nelements, Nepochs_delta 
      INTEGER (KIND = prec_int8) :: i, j, j1, k, looptest 
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3, sz4
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------	  
      REAL (KIND = prec_d), DIMENSION(3) :: r1i, v1i, r2i, v2i
      REAL (KIND = prec_d), DIMENSION(3) :: dr, dv, dr_RTN, dv_RTN
      REAL (KIND = prec_d) :: Rrtn(3,3)
      REAL (KIND = prec_d) :: GM
! ----------------------------------------------------------------------	  
      DOUBLE PRECISION  JD, Zbody(6)
      REAL (KIND = prec_d) :: mjd
      REAL (KIND = prec_d), DIMENSION(3) :: rsat, vsat
      REAL (KIND = prec_q), DIMENSION(3) :: rSun, rMoon
      REAL (KIND = prec_q), DIMENSION(3) :: rbody
      INTEGER  NTARG_SUN, NTARG_MOON, NCTR
      REAL (KIND = prec_q) :: lambda
      REAL (KIND = prec_q) :: angX0, angY0, angZ0
      REAL (KIND = prec_q) :: fr0, ft0, fn0
      REAL (KIND = prec_q) :: beta0, del_u0, yaw0, lambda0
      REAL (KIND = prec_q), DIMENSION(3) :: rsat0
      REAL (KIND = prec_q), DIMENSION(3) :: ed, ey, eb, ex, en, ev, ez
      REAL (KIND = prec_q), DIMENSION(3) :: yy
      REAL (KIND = prec_q), DIMENSION(9) :: kepler
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: angX1, angY1, angZ1
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: angX2, angY2, angZ2
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: fr1, ft1, fn1
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: fr2, ft2, fn2
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: beta1, beta2
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: del_u1, del_u2
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: yaw1, yaw2
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: lambda1, lambda2
      CHARACTER (LEN=1) :: GNSSid
      CHARACTER (LEN=30) :: fmt_line
      CHARACTER (LEN=200) :: mesg
      INTEGER (KIND = prec_int2) :: ios
      INTEGER (KIND = prec_int4) :: PRN_no
      REAL    (KIND = 4)         :: prnnum


! Set the numerical differences digits limit for the common epochs
dt_limit = 1.D-08


! Gravitational constant
GM = 0.39860044150D+15


sz1 = size(ds1, DIM = 1)
sz2 = size(ds1, DIM = 2)
Nepochs = sz1
!print*,'sz1, sz2 = ', sz1, sz2
sz3 = size(ds2, DIM = 1)
sz4 = size(ds2, DIM = 2)
Nepochs2 = sz3
!print*,'sz3, sz4 = ', sz3, sz4

! ----------------------------------------------------------------------
! Test columns dimension
If (sz2 .NE. sz4) Then
write(mesg, *) "Input matrices dimension(DIM=2) do not agree, DIM=2 (", sz2, ",", sz4, ")"
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
End If
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Find the number of the common epochs
Nepochs_delta = 0
i = 0
j = 0
Do i = 1 , Nepochs
   ! Test the time argument: 
   Do j = 1 , Nepochs2 
      delta_t = ABS(ds2(j,1) - ds1(i,1))
      IF (delta_t < dt_limit) then
         ! Counter of the common epochs
         Nepochs_delta = Nepochs_delta + 1 
      End if
   End Do
End Do
! ----------------------------------------------------------------------
!print *,"Nepochs_delta", Nepochs_delta


! Dynamic allocatable array
! Allocate the array of the numerical orbit comparison
ALLOCATE (beta1(Nepochs_delta),beta2(Nepochs_delta), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate beta1 or beta2, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (del_u1(Nepochs_delta),del_u2(Nepochs_delta), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate del_u1 or del_u2, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (lambda1(Nepochs_delta),lambda2(Nepochs_delta), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate lambda1 or lambda2, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (orbdiff(Nepochs_delta,sz2+8), STAT = AllocateStatus)
!print *, "orbdiff allocated to size: ", Nepochs_delta, ",",sz2 + 8
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate orbdiff, dimensions (", &
                Nepochs_delta, ",", sz2+8, ")"
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (yaw1(Nepochs_delta),yaw2(Nepochs_delta), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate yaw1 or yaw2, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (angX1(Nepochs_delta), angY1(Nepochs_delta), angZ1(Nepochs_delta), STAT= AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate angX1, angY1 or angZ1, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (angX2(Nepochs_delta), angY2(Nepochs_delta), angZ2(Nepochs_delta), STAT= AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate angX2, angY2 or angZ2, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (fr1(Nepochs_delta), ft1(Nepochs_delta), fn1(Nepochs_delta), STAT= AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate fr1, ft1 or fn1, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
ALLOCATE (fr2(Nepochs_delta), ft2(Nepochs_delta), fn2(Nepochs_delta), STAT= AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate fr2, ft2 or fn2, dimension ", &
                Nepochs_delta
        call report('FATAL', pgrm_name, 'statorbit2', mesg, 'src/fortran/m_statorbit2.f95', 1)
end if
beta1 =0.d0
beta2 =0.d0
del_u1=0.d0
del_u2=0.d0
lambda1=0.d0
lambda2=0.d0
orbdiff=0.d0
yaw1 =0.d0
yaw2 =0.d0
angX1=0.d0
angY1=0.d0
angZ1=0.d0
angX2=0.d0
angY2=0.d0
angZ2=0.d0
fr1=0.d0
ft1=0.d0
fn1=0.d0
fr2=0.d0
ft2=0.d0
fn2=0.d0
! Identify PRN, SVN and BLOCK TYPE
!----------------------------------------------------------------------
fmt_line = '(A1,I2.2)'
READ (PRN, fmt_line , IOSTAT=ios) GNSSid, PRN_no

CALL prn_shift (GNSSid, PRN_no, PRN_no)
!print*,GNSSid, PRN_no

! ----------------------------------------------------------------------
! Compute the numerical differences
i = 0
j = 0
k = 0
Do i = 1 , Nepochs
   ! Test the time argument: 
   Do j = 1 , Nepochs2   
      delta_t = ABS(ds2(j,1) - ds1(i,1))
      IF (delta_t < dt_limit) then
         k = k + 1
rsat = ds1(i,3:5)
vsat = ds1(i,6:8)
	     ! State vector numerical differences at common epochs
CALL orbinfo (ds1(i,1), PRN_no, rsat, vsat, beta0, del_u0, yaw0, lambda0, & 
              angX0, angY0, angZ0, fr0, ft0, fn0)
beta1(k)  = beta0
del_u1(k) = del_u0
yaw1(k)   = yaw0
lambda1(k)= lambda0
angX1(k)  = angX0
angY1(k)  = angY0
angZ1(k)  = angZ0
fr1(k)  = fr0
ft1(k)  = ft0
fn1(k)  = fn0
!print*,diel_u0*180/Pi_global, fr0, ft0, fn0
rsat = ds2(j,3:5)
vsat = ds2(j,6:8)
CALL orbinfo (ds2(j,1), PRN_no, rsat, vsat, beta0, del_u0, yaw0, lambda0, & 
             angX0, angY0, angZ0, fr0, ft0, fn0)
beta2(k)  = beta0
del_u2(k) = del_u0
yaw2(k)   = yaw0
lambda2(k)= lambda0
angX2(k)  = angX0
angY2(k)  = angY0
angZ2(k)  = angZ0
fr2(k)  = fr0
ft2(k)  = ft0
fn2(k)  = fn0



	     ! Orbital frame numerical differences at common epochs
		 r1i = ds1(i,3:5)
		 v1i = ds1(i,6:8)
		 dr = ds2(j,3:5) - ds1(i,3:5)
		 CALL orb_frame(r1i, v1i, Rrtn)
		 CALL matrix_Rr(Rrtn, dr, dr_RTN)
	 orbdiff(k,1)   = ds2(j,1)
         orbdiff(k,2)   = PRN_no 
         orbdiff(k,3)   = BLKID
         orbdiff(k,4)   = lambda2(k)
         orbdiff(k,5)   = beta2(k)  *180/Pi_global
         orbdiff(k,6)   = del_u2(k) *180/Pi_global
         orbdiff(k,7)   = yaw2(k)   *180/Pi_global
         orbdiff(k,8)   = angX2(k) *180/Pi_global
         orbdiff(k,9)   = angY2(k) *180/Pi_global
         orbdiff(k,10)   = angZ2(k) *180/Pi_global
         orbdiff(k,11:13) = dr_RTN
         orbdiff(k,14)   = fr2(k)
         orbdiff(k,15)   = ft2(k)
         orbdiff(k,16)   = fn2(k)


	  End IF
   End Do
End Do



 100 END SUBROUTINE


End Module

