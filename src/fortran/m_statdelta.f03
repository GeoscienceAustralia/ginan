MODULE m_statdelta


! ----------------------------------------------------------------------
! MODULE: m_statdelta.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for computing statistics of two data series comparison
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	11 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			


Contains
	  
	  
SUBROUTINE statdelta (ds1, ds2, dsr, RMSdsr, Sigmadsr, MEANdsr, MINdsr, MAXdsr)


! ----------------------------------------------------------------------
! SUBROUTINE: statdelta.f03
! ----------------------------------------------------------------------
! Purpose:
!  Numerical comparison between two data series matrices. 
!  Numerical differences are computed at the common IDs (e.g. epochs)
! ----------------------------------------------------------------------
! Input arguments:
! - ds1:		1st data series matrix (used as reference)
! - ds2: 		2nd data series matrix
!
! Output arguments:
! - dsr: 		Differences between the two input matrices at the common IDs (epochs) 
!        		dsr = [id(common) id2(common) sr2-sr1(:,3:end)]
! - RMSdsr: 	Root Mean Square of each variable's differences
! - Sigmadsr	Standard Deviation of each variable's differences
! - MEANdsr		Mean value of each variable's differences
! - MINdsr:		Minimum value of each variable's differences
! - MAXdsr: 	Maximum value of each variable's differences
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	11 October 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_statist
      USE mdl_config
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), ALLOCATABLE, DIMENSION(:,:) :: ds1, ds2
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: dsr  
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:)  , ALLOCATABLE :: RMSdsr, Sigmadsr, MEANdsr, MINdsr, MAXdsr   
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), ALLOCATABLE, DIMENSION(:) :: dx
      REAL (KIND = prec_d) :: RMSdx, Sigmadx, MEANdx, MINdx, MAXdx  
      REAL (KIND = prec_d) :: delta_t, dt_limit, smallest_delta, real_smallest_delta
      INTEGER (KIND = prec_int8) :: Nepochs, Nepochs2, Nelements, Nepochs_delta 
      INTEGER (KIND = prec_int8) :: i, j, j1, k, looptest 
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3, sz4
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      INTEGER (KIND = prec_int8) :: Nparam 
      REAL (KIND = prec_d), ALLOCATABLE, DIMENSION(:,:) :: ds_temp, ds1_2, ds2_2
      CHARACTER (LEN=100) mesg
! ----------------------------------------------------------------------	  



dt_limit = 1.D-8
smallest_delta = 1.D+09
real_smallest_delta = 1.D+09


sz1 = size(ds1, DIM = 1)
sz2 = size(ds1, DIM = 2)
Nepochs = sz1

sz3 = size(ds2, DIM = 1)
sz4 = size(ds2, DIM = 2)
Nepochs2 = sz3

! default initialisation
Nparam = min0(sz2, sz4)

! ----------------------------------------------------------------------
! Test collumns dimension
If (sz2 .NE. sz4) Then
!print *,"Subroutine statdelta.f03 within Module mdl_statdelta.f03: Input matrices dimension(DIM=2) do not agree"
!print *,"DIM=2", sz2, sz4   
!         STOP "*** - ***"
end if
ALLOCATE (ds1_2(Nepochs,Nparam), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate ds1_2, dimensions (", &
              Nepochs, ",", Nparam, ")"
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
end if
ds1_2 = ds1(:,1:NParam)
ALLOCATE (ds2_2(Nepochs2,Nparam), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate ds2_2, dimensions (", &
              Nepochs2, ",", Nparam, ")"
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
end if
ds2_2 = ds2(:,1:NParam)

! ----------------------------------------------------------------------
! Find the number of the common epochs
Nepochs_delta = 0
Do i = 1 , Nepochs
   ! Test the time argument: 
   Do j = 1 , Nepochs2 
      delta_t = ABS(ds2_2(j,1) - ds1_2(i,1))
      if (delta_t < real_smallest_delta) real_smallest_delta = delta_t
      IF (delta_t < dt_limit) then
         ! Counter of the common epochs
         Nepochs_delta = Nepochs_delta + 1 
      else if (delta_t < smallest_delta) then
         smallest_delta = delta_t
      End if
   End Do
End Do
! ----------------------------------------------------------------------
if (1 < 0) then
print *,"Nepochs_delta ", Nepochs_delta
print *,"Nepochs ", Nepochs
print *,"Nepochs2 ", Nepochs2
print *,"Smallest delta ",smallest_delta
print *,"Real Smallest delta ",real_smallest_delta
end if
!print *,"delta_t", delta_t


! Dynamic allocatable array
! Allocate the array of the numerical orbit comparison
ALLOCATE (dsr(Nepochs_delta,Nparam), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate dsr, dimensions (", &
              Nepochs_delta, ",", Nparam, ")"
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if


! ----------------------------------------------------------------------
! Compute the numerical differences
k = 0
Do i = 1 , Nepochs
   ! Test the time argument: 
   Do j = 1 , Nepochs2   
      delta_t = ABS(ds2_2(j,1) - ds1_2(i,1))
      IF (delta_t < dt_limit) then
	  ! Compute the numerical differences of the state vector
         k = k + 1
		 dsr(k,1:2) = ds1_2(i,1:2)
         dsr(k,3:sz2) = ds2_2(j,3:sz2) - ds1_2(i,3:sz2)
      end if
   End Do
End Do
! ----------------------------------------------------------------------
!print *,"k", k



! ----------------------------------------------------------------------
! Statistics

! First two collumns refer to Time (MJD, Sec since 00h)
Nelements = sz2 - 2

! Allocate arrays
ALLOCATE (dx(Nepochs_delta), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate dx, dimension ", &
              Nepochs_delta
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if
ALLOCATE (RMSdsr(Nelements), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate RMSdsr, dimension ", &
              Nelements
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if
ALLOCATE (Sigmadsr(Nelements), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate Sigmadsr, dimension ", &
              Nelements
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if
ALLOCATE (MEANdsr(Nelements), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate MEANdsr, dimension ", &
              Nelements
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if
ALLOCATE (MINdsr(Nelements), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate MINdsr, dimension ", &
              Nelements
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if
ALLOCATE (MAXdsr(Nelements), STAT = AllocateStatus)
        if (AllocateStatus .ne. 0) then
      write(mesg, *) "Not enough memory - failed to allocate MAXdsr, dimension ", &
              Nelements
      call report('FATAL', pgrm_name, 'statdelta', mesg, 'src/m_statdelta.f03', 1)
        end if

j = 0
Do i = 3 , sz2
   dx(:) = dsr(:,i)
   if (size(dx) == 0) then
       print *,"error, size dx is 0, i = ", i
       STOP
   end if
   Call statist(dx, RMSdx, Sigmadx, MEANdx, MINdx, MAXdx)
   j = j + 1
   RMSdsr(j) = RMSdx
   Sigmadsr(j) = Sigmadx
   MEANdsr(j) = MEANdx
   MINdsr(j) = MINdx
   MAXdsr(j) = MAXdx
End Do
! ----------------------------------------------------------------------


 100 END SUBROUTINE


End Module

