MODULE mdl_brdconfig

! ---------------------------------------------------------------------------
! Purpose:
!  Parametes in commandline source code 
! ---------------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng, Geoscience Australia
!
! Created:	17-07-2019 
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			
	  
! ----------------------------------------------------------------------
! Input and output file names 
! ----------------------------------------------------------------------
	  CHARACTER (LEN=100) :: INFILENAME, OUTFILENAME
          CHARACTER (LEN=2)   :: GNSSTYPE
!          CHARACTER (LEN=100) :: leapsec_filename_cfg
!          CHARACTER (LEN=50)  :: EOP_fname_cfg
! ----------------------------------------------------------------------


END
