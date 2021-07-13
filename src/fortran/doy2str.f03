SUBROUTINE doy2str (DOYSTR)


! ----------------------------------------------------------------------
! SUBROUTINE: doy2str.f03
! ----------------------------------------------------------------------
! Purpose:
!  This subroutine is used to export the processed date to string format.  
! ----------------------------------------------------------------------
! Input arguments:
! - YR : year with 4 digits 
! - DOY: day of year
! - PRN: PRN number 
! 
! Output arguments:
! - DOYSTR:		
!
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
! 
! Copyright: Geoscience Australia, Australia
!
! Created:	06-12-2019
! ----------------------------------------------------------------------


      USE mdl_param
      IMPLICIT NONE

      CHARACTER (LEN=10) :: DOYSTR


      WRITE(DOYSTR(1:10),'(I4,I3.3,3A)') YR,DOY,PRN
      !print*,'DOYSTR=',DOYSTR
End
