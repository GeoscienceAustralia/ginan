SUBROUTINE scan0orb 

! ----------------------------------------------------------------------
! SUBROUTINE: scan0orb.f95
! ----------------------------------------------------------------------
! Purpose:
!  To skip the bad orbit with zero value  in SP3 file
! ----------------------------------------------------------------------
! Input arguments:
! 
! - pseudobs_ICRF: 	Orbit array (Nx5) in inertial frame (ICRF) including the state vector per epoch
! 					Collumns desciption per epoch:
!               	- Modified Julian Day number (including the fraction of the day) 
!					- Seconds since 00h 
!					- Position vector (m)
! - pseudobs_ITRF:	Orbit array (Nx5) in terrestrial frame (ITRF)
! 					Collumns desciption per epoch: similar to the orbobs_ICRF
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng,  Geoscience Australia
!
! Created:	07-08-2019
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config

      IMPLICIT NONE

	  
 
! ----------------------------------------------------------------------
! Local Variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3 ,sz4 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, read_i, k 
      INTEGER (KIND = prec_int8) :: ic, it, j
      INTEGER (KIND = prec_int8) :: INEW1, INEW2
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: pseudobs_ITRF2,pseudobs_ICRF2
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: pseudobs_ITRF3,pseudobs_ICRF3
      CHARACTER (LEN=100) :: mesg
! ----------------------------------------------------------------------

sz1 = SIZE (pseudobs_ICRF, DIM =1)
sz2 = SIZE (pseudobs_ICRF, DIM =2)

sz3 = SIZE (pseudobs_ITRF, DIM =1)
sz4 = SIZE (pseudobs_ITRF, DIM =2)

!print*,'sz1, sz2 =', sz1, sz2, 'sz3, sz4 =',  sz3, sz4

ALLOCATE (pseudobs_ICRF2(sz1,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to allocate pseudo_ICRF2, dimensions = (,", &
                sz1, ",", sz2, ")"
        call report('FATAL', pgrm_name, 'scan0orb', mesg, 'src/fortran/scan0orb.f95', 1)
end if
ALLOCATE (pseudobs_ITRF2(sz3,sz4), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to allocate pseudo_ITRF2, dimensions = (,", &
                sz3, ",", sz4, ")"
        call report('FATAL', pgrm_name, 'scan0orb', mesg, 'src/fortran/scan0orb.f95', 1)
end if

pseudobs_ICRF2 = 0.d0
pseudobs_ITRF2 = 0.d0

ic = 0
DO k=1, sz1
IF (pseudobs_ICRF(k,3) /= 0.d0) THEN
ic = ic +1
pseudobs_ICRF2(ic,1:8) = pseudobs_ICRF(k,1:8)
END IF
END DO


it = 0
DO k=1, sz3
IF (pseudobs_ITRF(k,3) /= 0.d0) THEN
it = it +1
pseudobs_ITRF2(it,1:8) = pseudobs_ITRF(k,1:8)
END IF
END DO

ALLOCATE (pseudobs_ICRF3(ic,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to allocate pseudo_ICRF3, dimensions = (,", &
                ic, ",", sz2, ")"
        call report('FATAL', pgrm_name, 'scan0orb', mesg, 'src/fortran/scan0orb.f95', 1)
end if
pseudobs_ICRF3 = 0.d0
ALLOCATE (pseudobs_ITRF3(it,sz3), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to allocate pseudo_ITRF3, dimensions = (,", &
                it, ",", sz3, ")"
        call report('FATAL', pgrm_name, 'scan0orb', mesg, 'src/fortran/scan0orb.f95', 1)
end if
pseudobs_ITRF3 = 0.d0

INEW1 = 0
DO k=1, sz1
   DO j=1, ic
   IF (pseudobs_ICRF2(j,3) == pseudobs_ICRF(k,3) .AND. pseudobs_ICRF2(j,4) == pseudobs_ICRF(k,4))THEN

   INEW1 = INEW1 + 1
   pseudobs_ICRF3(INEW1,1:8)= pseudobs_ICRF(k,1:8)

   END IF
   END DO
END DO


INEW2 = 0
DO k=1, sz3
   DO j=1, it
   IF (pseudobs_ITRF2(j,3) == pseudobs_ITRF(k,3) .AND. pseudobs_ITRF2(j,4) == pseudobs_ITRF(k,4))THEN

   INEW2 = INEW2 + 1
   pseudobs_ITRF3(INEW2,1:8)= pseudobs_ITRF(k,1:8)

   END IF
   END DO
END DO

DEALLOCATE (pseudobs_ICRF, STAT = DeAllocateStatus)
DEALLOCATE (pseudobs_ITRF, STAT = DeAllocateStatus)

ALLOCATE (pseudobs_ICRF(ic,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to re-allocate pseudo_ICRF, dimensions = (,", &
                ic, ",", sz2, ")"
        call report('FATAL', pgrm_name, 'scan0orb', mesg, 'src/fortran/scan0orb.f95', 1)
end if
ALLOCATE (pseudobs_ITRF(it,sz3), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "failed to re-allocate pseudo_ITRF, dimensions = (,", &
                it, ",", sz3, ")"
        call report('FATAL', pgrm_name, 'scan0orb', mesg, 'src/fortran/scan0orb.f95', 1)
end if
!print*,'Number of Epochs or Positions used for POD =', ic
pseudobs_ICRF = 0.d0
pseudobs_ITRF = 0.d0

pseudobs_ICRF = pseudobs_ICRF3
pseudobs_ITRF = pseudobs_ITRF3

 100 END
