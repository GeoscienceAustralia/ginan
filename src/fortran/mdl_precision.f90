MODULE mdl_precision

! ---------------------------------------------------------------------------
! Purpose:
!  Module for setting the 'Precision Parameters' as global variables
! ---------------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou                                        July 2015
! ---------------------------------------------------------------------------


      IMPLICIT NONE
      SAVE 			! Attention

! ---------------------------------------------------------------------------
! Real numbers precision
! ---------------------------------------------------------------------------
! 1.
      INTEGER, PARAMETER :: prec_s = selected_real_kind(6,37)
      INTEGER, PARAMETER :: prec_d = selected_real_kind(15,307)
      INTEGER, PARAMETER :: prec_q = selected_real_kind(15,307)
!      INTEGER, PARAMETER :: prec_q = selected_real_kind(33,4931)
!      INTEGER, PARAMETER :: prec_quadruple = selected_real_kind(33,4931)
! ---------------------------------------------------------------------------
! 2. 
!  REAL (KIND=4) :: X ! SP
!  REAL (KIND=8) :: X ! DP
!  REAL (KIND=16) :: X ! QP
! ---------------------------------------------------------------------------
! 3.
!  integer, parameter :: SINGLE = REAL64
!  integer, parameter :: DOUBLE = REAL128
! ---------------------------------------------------------------------------
! 4. Attention: Old approach, to be avoided
!       REAL*4 :: pi
!       REAL*8 :: pi
!       REAL*16 :: pi
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Integer numbers precision
! ---------------------------------------------------------------------------
      INTEGER, PARAMETER :: prec_int1 = selected_int_kind(2)
      INTEGER, PARAMETER :: prec_int2 = selected_int_kind(4)
      INTEGER, PARAMETER :: prec_int4 = selected_int_kind(8)
      INTEGER, PARAMETER :: prec_int8 = selected_int_kind(18)

!   integer, parameter :: INT8    = selected_int_kind(2)
!   integer, parameter :: INT16   = selected_int_kind(4)
!   integer, parameter :: INT32   = selected_int_kind(18)
!   !  integer, parameter :: INT64   = selected_int_kind(18)
! ---------------------------------------------------------------------------


END
