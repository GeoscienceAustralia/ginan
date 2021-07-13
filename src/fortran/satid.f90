SUBROUTINE satid


! ---------------------------------------------------------------------------
! Purpose:
!  Module used for forming an allocatable array with the PRNs, Blocks and SVNs
!  of GNSS satellites
! ---------------------------------------------------------------------------
! Note: 
!  This module is used in subroutine attitude_yaw.f90
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Subroutine:  satid.f90
! ---------------------------------------------------------------------------
! Purpose:
!  Read the history file with the PRNs, Blocks and SVNs of the GNSS satellites
!  and form an allocatable array with the IDs (PRN,BLK,SVN) at the input epoch
! ---------------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number in UTC time (including fraction of the day)
! 
! Output declaration to module:
! - mdl_satid.f90:	Module used for the definition of the output arguments
!					as dynamic allocatable arrays
! Output arguments:
! - GNSS_id:		Allocotable array with GNSS IDs at the input epoch
! ---------------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia                  June 2016
! ---------------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			


! ---------------------------------------------------------------------------
      INTEGER :: GNSS_maxsat = 64
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! GNSS_id format (temporary)
! ---------------------------------------------------------------------------
! Col.1: PRN number
! Col.2: Block number
! Col.3: SVN number
! ---------------------------------------------------------------------------

! Block number:		SV BLOCK  1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6


! ---------------------------------------------------------------------------
! GNSS_id array should be constructed for every epoch by reading the history file (PRN,Block,SVN)
! ---------------------------------------------------------------------------


! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------
      ALLOCATE (GNSS_id(GNSS_maxsat,3), STAT = AllocateStatus)
!      print *,"GNSS_id AllocateStatus=", AllocateStatus
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: MODULE mdl_satid.f90"
!         STOP "*** Not enough memory ***"
      END IF  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Temporary:
! ----------------------------------------------------------------------
! 25 Dec. 2010: PRN 30, GPS Block IIA
      GNSS_id (30,1) = 30
      GNSS_id (30,2) = 3
      GNSS_id (30,1) = 30
! ----------------------------------------------------------------------


END
