MODULE m_writearray


! ----------------------------------------------------------------------
! MODULE: m_writearray.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for write array data to output (ascii) files 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	17 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE writearray (wrtArray, filename)

! ----------------------------------------------------------------------
! SUBROUTINE: write_array.f90 >> writearray.f03
! ----------------------------------------------------------------------
! Purpose:
!  Write array data to an ascii file
! ----------------------------------------------------------------------
! Input arguments:
! - wrtArray:       Input allocatable array
! - filename:       file name to be used for writing array data
!
! Output arguments:
!
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia           September 2015
! ----------------------------------------------------------------------
! Last modified:
! - Thomas Papanikolaou, 17 October 2017:
!   write_array.f90 subroutine has been modified in order to use advantages of Fortran 2003 (in dynamic memory allocation) and 
!   to avoid compiling errors based on the gfortran compiler (variable format expressions are not recognized)   
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: wrtArray 
      CHARACTER (*), INTENT(IN) :: filename
! OUT
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, i_write
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_ith
      INTEGER (KIND = prec_int8) :: sz1, sz2
      INTEGER (KIND = prec_int2) :: wrt_opt
      INTEGER (KIND = prec_int2) :: FMT_opt
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: RealT
      INTEGER (KIND = prec_int2) :: RealW, RealD
      CHARACTER (LEN=70) :: fmt_wrt, fmt_wrt0, fmt_sz2
      REAL (KIND = prec_q) :: wrtArrayLN 
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Format definition option
! ----------------------------------------------------------------------
! 1. Format defined by the 100 Format (not recognized by gfortran due to variable format expressions)
! 2. General format (FMT=*) approach (ok) (default)
! 3. Write per each individual data record based on the 200 Format (if option to limit sats is chosen)
! 4. Based on the format definition by fmt_wrt 
! 5. E27.17 (see below)
FMT_opt = 2
sz2 = SIZE(yml_satellites)
do i = 1, sz2
    if (.not. yml_satellites(i)) FMT_opt = 3
end do
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      UNIT_IN = 7  												
      wrt_opt = 2  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Array dimensions
      sz1 = SIZE (wrtArray,DIM=1) ! sz1 is the epochs
      sz2 = SIZE (wrtArray,DIM=2) ! sz2 is the satellites + 2
      ! if sizes don't match properly revert to default
      if ((FMT_opt == 3) .and. (sz2 .ne. (SIZE(yml_satellites, DIM=1) - 2))) then
          FMT_opt = 2
      end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Format definition
! ----------------------------------------------------------------------
!100   FORMAT (<sz2>E27.17)   ! Deactivated: Variable format expressions (<>) are not recognized by gfortran compiler

200   FORMAT (E27.17)                                      			
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Format alternative approach
! ----------------------------------------------------------------------
RealT = 'E'
RealW = 27
RealD = 17
      
!WRITE (fmt_wrt,FMT='(A1,A1,I3,A1, A1,I2,A1,I2,A1)') "(", "<",sz2,">", RealT, RealW, ".", RealD, ")" 
!PRINT *,"fmt_wrt", fmt_wrt

If (sz2 < 100) Then
   If (sz2 < 10) Then
 	  WRITE (fmt_wrt,FMT='(A1,I1,A1,I2,A1,I2,A1)') "(",sz2, RealT, RealW, ".", RealD,")" 
   Else 
 	  WRITE (fmt_wrt,FMT='(A1,I2,A1,I2,A1,I2,A1)') "(",sz2, RealT, RealW, ".", RealD,")" 
   End IF    
Else 
 	  WRITE (fmt_wrt,FMT='(A1,I3,A1,I2,A1,I2,A1)') "(",sz2, RealT, RealW, ".", RealD,")" 
End If
! ----------------------------------------------------------------------
!PRINT *,"fmt_wrt", fmt_wrt



! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT=UNIT_IN,FILE=filename,ACTION="WRITE",POSITION="REWIND", IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", filename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------
	 
	  
      IF (wrt_opt == 1)	THEN
! ----------------------------------------------------------------------
! Write data to file 
      !WRITE (UNIT=UNIT_IN, FMT=100, IOSTAT=ios_ith) TRANSPOSE(wrtArray)
	  
      IF (ios_ith /= 0) THEN
         PRINT *, "Error in writing to file: ", TRIM(filename)
         PRINT *, "WRITE IOSTAT=", ios_ith
      END IF
      ENDFILE (UNIT = UNIT_IN) 
      CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------

      ELSE  
	  
! ----------------------------------------------------------------------
! Write data to file | Line by line	  
      DO i_write = 1 , sz1
         
		If (FMT_opt == 1) Then
		
		 ! 1. Format defined by 100 Format
		 !    intel Fortran: Ok
		 !    gfortran: Not; Variable format expressions are not recognized by gfortran)
	     !WRITE (UNIT=UNIT_IN,FMT=100,IOSTAT=ios_ith) wrtArray(i_write,:)
		 
		Else If (FMT_opt == 2) Then 
		
		 ! 2. General format (FMT=*) approach (ok)
	     WRITE (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_ith) wrtArray(i_write,:)
		 
		Else If (FMT_opt == 3) Then 
		
		 ! 3. Write per each individual data record based on the 2000 Format
	     Do i = 1 , sz2
                    if (i>2) then
                            if (.not. yml_satellites(i-2)) cycle
                    end if
		    WRITE (UNIT=UNIT_IN,FMT=200,IOSTAT=ios_ith,ADVANCE='NO') wrtArray(i_write,i)
	     End Do
		 ! Move to the next line
		 WRITE (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_ith)  

		Else If (FMT_opt == 4) Then 

         ! 4. Based on the format definition by fmt_wrt 
	     WRITE (UNIT=UNIT_IN,FMT=fmt_wrt,IOSTAT=ios_ith) wrtArray(i_write,:)
		
		Else If (FMT_opt == 5) Then 
		
         ! 5. 
         WRITE (fmt_sz2,FMT=*) sz2
	     WRITE (UNIT=UNIT_IN,FMT="(" // TRIM(fmt_sz2) // "E27.17)",IOSTAT=ios_ith) wrtArray(i_write,:)	 	
		 
		 End If
 
		 
         IF (ios_ith /= 0) THEN
            PRINT *, "Error in writing to file: ", TRIM (filename)
            PRINT *, "WRITE IOSTAT=", ios_ith
         END IF
		 
      END DO
	  
      ENDFILE (UNIT = UNIT_IN) 
      CLOSE (UNIT = UNIT_IN)
	  
! ----------------------------------------------------------------------
      END IF

	  


END SUBROUTINE



End Module

