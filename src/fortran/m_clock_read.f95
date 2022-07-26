MODULE m_clock_read


! ----------------------------------------------------------------------
! MODULE: m_clock_read.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the subroutine 'clock_read' for reading GNSS orbits in sp3 format
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	18 October 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
	  


Contains


SUBROUTINE clock_read (CLKfname,CLKformat, PRNmatrix, ORB_matrix, CLKmatrix)

! ----------------------------------------------------------------------
! SUBROUTINE: clock_read
! ----------------------------------------------------------------------
! Purpose:
!  Read clock data file format for forming a 3-dimensions clocks matrix for all satellites
! ----------------------------------------------------------------------
! Input arguments:
! - CLKfname:		GNSS satellite clocks file name 
! - CLKformat:		GNSS satellite clocks file format
! 					0. Return zero matrix for clocks output array (CLKmatrix)
! 					1. sp3 format
! 					2. clk format 
! - PRNmatrix:      Satellites' PRN numbers allocatable array
!
! Output allocatable arrays:
! - CLKmatrix:		Clocks dynamic allocatable array (3-dimensional)
!					CLKmatrix(i,j,isat) = [MJD Sec_of_day clock]
!					If clock error (sdev) is available (when Position and Velocity vector are written in orbit sp3 format)
!					CLKmatrix(i,j,isat) = [MJD Sec_of_day clock clock_sdev]
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	18 October 2018
! ----------------------------------------------------------------------

      USE mdl_precision
      USE mdl_num
      USE m_sp3
      use pod_yaml
      use mdl_config
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: CLKfname
      INTEGER (KIND = prec_int2), INTENT(IN) :: CLKformat
      CHARACTER (LEN=3), INTENT(IN), ALLOCATABLE :: PRNmatrix(:)
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: ORB_matrix	  

! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:,:), ALLOCATABLE :: CLKmatrix
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nsat, isat, Nepochs, Nel, Nepochs_orb, i_epochs
      INTEGER (KIND = prec_int8) :: Cepochs, Cel, i, j 
      CHARACTER (LEN=3) :: PRNid
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3, clock_matrix
      INTEGER (KIND = prec_int2) :: AllocateStatus
      INTEGER (KIND=prec_int2) :: time_system
      CHARACTER (LEN = 500) :: mesg
      LOGICAL found, first, nodata
! ----------------------------------------------------------------------


! PRN array dimensions
Nsat = SIZE (PRNmatrix,DIM=1)

IF (CLKformat == 0) THEN
	!CLKmatrix = 0.0D0  
	! Set Clocks matrix' values to "999999.999999D0" and dimesnions follow the dimensions of Orbit matrix
	Nepochs_orb = size(ORB_matrix, DIM = 1)
	ALLOCATE (CLKmatrix(Nepochs_orb,4,Nsat), STAT = AllocateStatus)
        CLKmatrix = 0.d0
	DO isat = 1 , Nsat
               if (yml_satellites(isat)) then
		DO i_epochs = 1 , Nepochs_orb
			CLKmatrix (i_epochs,1,isat) = ORB_matrix (i_epochs,1,isat)
			CLKmatrix (i_epochs,2,isat) = ORB_matrix (i_epochs,2,isat)
			CLKmatrix (i_epochs,3,isat) = 999999.999999D0
			CLKmatrix (i_epochs,4,isat) = 999999.999999D0
		END DO
                end if
	END DO
! ----------------------------------------------------------------------
ELSE IF (CLKformat == 1) THEN	  
        first = .true.
	DO isat = 1 , Nsat
		PRNid = PRNmatrix(isat)
		CALL sp3(CLKfname,PRNid,orbsp3,yml_interpolate_start,clock_matrix,time_system,found,.false., nodata)
                write (mesg, *) "PRN ", PRNid, " not found in sp3 file ", CLKfname
                if (.not. found) then
                        call report('WARNING', pgrm_name, 'clock_read', trim(mesg), 'src/fortran/m_clock_read.f95', 1)
                        continue
                end if
		IF (first) THEN
                        first = .false.
		! Clock array dimensions
		Nepochs = SIZE (clock_matrix,DIM=1)
		Nel     = SIZE (clock_matrix,DIM=2)
		! Allocate array
		ALLOCATE (CLKmatrix(Nepochs,Nel,Nsat), STAT = AllocateStatus)
                CLKmatrix = 0.d0
		END IF
                if (yml_satellites(isat)) then
                    cepochs = SIZE(clock_matrix, DIM=1)
                    cel = SIZE(clock_matrix, DIM=2)
                    ! because some sats may have been unhealthy in the middle, sizes could be diff
                    do i=1,min(cepochs, Nepochs)
                        do j=1,min(cel, Nel)
                            CLKmatrix(i,j,isat) = clock_matrix(i,j)
                        end do
                    end do
                end if
	END DO
! ----------------------------------------------------------------------
!ELSE IF (CLKformat == 2) THEN	  
!
END IF	  
! ----------------------------------------------------------------------

END SUBROUTINE

End Module


