SUBROUTINE empirical_cor (Xmatrix, Bias_0, CPR_0, Bias_apos, CPR_apos)


! ----------------------------------------------------------------------
! SUBROUTINE: empirical_cor.f03
! ----------------------------------------------------------------------
! Purpose:
!  Orbit empirical parameters initialization 
! ----------------------------------------------------------------------
! Input arguments:
! - Bias_0: 	Bias parameters acceleration apriori values 
! - CPR_0: 		Cycle per revolution parameters (C and S coefficients) apriori values 
!
! Output arguments:
! - Bias_apos: 	Bias parameters acceleration aposteriori values 
! - CPR_apos: 	Cycle per revolution parameters (C and S coefficients) aposteriori values 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, CRC-SI
! Created:	30 November 2018
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      use pod_yaml
      IMPLICIT NONE
	  
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: Xmatrix  
	  REAL (KIND = prec_q), INTENT(IN) :: Bias_0(3)
	  REAL (KIND = prec_q), INTENT(IN) :: CPR_0(3,2)
! ----------------------------------------------------------------------
! OUT
	  REAL (KIND = prec_q), INTENT(OUT) :: Bias_apos(3)
	  REAL (KIND = prec_q), INTENT(OUT) :: CPR_apos(3,2)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------  
      REAL (KIND = prec_d) :: Bias_corr(3), CPR_corr(3,2)
! ----------------------------------------------------------------------  

! ----------------------------------------------------------------------
! FIXME: variable initialisation
Bias_corr = 0.d0
CPR_corr = 0.d0

! ----------------------------------------------------------------------  
If (BTEST(yml_srp_parameters, EMP_R_Bias - one) .and. BTEST(yml_srp_parameters, EMP_R_CPR - one)) Then
! Bias & CPR
Bias_corr = Xmatrix(7:9,1)
CPR_corr(1,1) = Xmatrix(10,1)
CPR_corr(1,2) = Xmatrix(11,1)
CPR_corr(2,1) = Xmatrix(12,1)
CPR_corr(2,2) = Xmatrix(13,1)
CPR_corr(3,1) = Xmatrix(14,1)
CPR_corr(3,2) = Xmatrix(15,1)

else If (.not. BTEST(yml_srp_parameters, EMP_R_Bias - one) .and. BTEST(yml_srp_parameters, EMP_R_CPR - one)) Then
! CPR
CPR_corr(1,1) = Xmatrix(7,1)
CPR_corr(1,2) = Xmatrix(8,1)
CPR_corr(2,1) = Xmatrix(9,1)
CPR_corr(2,2) = Xmatrix(10,1)
CPR_corr(3,1) = Xmatrix(11,1)
CPR_corr(3,2) = Xmatrix(12,1)

else If (BTEST(yml_srp_parameters, EMP_R_Bias - one) .and. .not. BTEST(yml_srp_parameters, EMP_R_CPR - one)) Then
! Bias 
Bias_corr = Xmatrix(7:9,1)

End If
! ----------------------------------------------------------------------  

Bias_apos = Bias_0 + Bias_corr
!print *, "Bias_accel_aposteriori", Bias_accel_aposteriori
!print *, "Bias_accel_glb", Bias_accel_glb
!print *, "Bias_corr", Bias_corr

CPR_apos = CPR_0 + CPR_corr
!print *, "CPR_CS_aposteriori", CPR_CS_aposteriori
!print *, "CPR_CS_glb", CPR_CS_glb
!print *, "CPR_corr", CPR_corr


END SUBROUTINE

