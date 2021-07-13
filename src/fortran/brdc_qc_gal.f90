SUBROUTINE brdc_qc_gal(EPHNEW, AVE_A, STD_A) 

! ----------------------------------------------------------------------
! PROGRAM: brdc_qc_gal.f90
! ----------------------------------------------------------------------
! Purpose: Compute statistic information of broadcast parameters for quality control  
!       
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng
!
! Copyright: GEOSCIENCE AUSTRALIA, AUSTRALIA
!
! Created:   13-08-2019
!
! ----------------------------------------------------------------------
      USE mdl_precision
      USE m_meanstd
      IMPLICIT NONE
      REAL (KIND = prec_q) :: EPHNEW(150)
      REAL (KIND = prec_q), ALLOCATABLE, DIMENSION(:) :: EPHQC
      REAL (KIND = prec_q) :: A
      REAL (KIND = prec_q) :: A_SumSQR
      REAL (KIND = prec_q) :: AVE_A
      REAL (KIND = prec_q) :: STD_A
      INTEGER (KIND=4) :: SZ1, NUM, I, JJ
      INTEGER (KIND = prec_int2) :: AllocateStatus

SZ1 = SIZE(EPHNEW,DIM =1)
NUM = 0
DO I= 1, SZ1
IF(EPHNEW(3) /= 0.d0)THEN
NUM = NUM + 1
END IF 
END DO
ALLOCATE (EPHQC(NUM), STAT=AllocateStatus)

EPHQC = 0.d0
NUM = 0
DO I= 1, SZ1
IF(EPHNEW(3) /= 0.d0)THEN
NUM = NUM + 1
EPHQC(NUM) = EPHNEW(I)
END IF
END DO

A = 0.d0
A_SumSQR    = 0.d0 

DO JJ=1,NUM
A = A     + EPHQC(JJ)
A_SumSQR    = A_SumSQR     + EPHQC(JJ)*EPHQC(JJ)
END DO 

CALL meanstd(A, A_SumSQR, NUM, AVE_A, STD_A)

END SUBROUTINE

