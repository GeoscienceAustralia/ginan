SUBROUTINE att_matrix(mjd, rsat_icrf, vsat_icrf, PRNsat, BLKsat, & 
                        &       eclipse_status, Yangle_array, Rtrf2bff, Quaternions_trf2bff)


! ----------------------------------------------------------------------
! SUBROUTINE: att_matrix.f03
! ----------------------------------------------------------------------
! Purpose:
!  Compute satellite attitude and transformation matrix between 
!  terrestrial reference frame and satellite body-fixed frame
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:                Modified Julian Day number (including fraction of the day)
! - rsat_icrf:          Satellie position vector (m) 
! - vsat_icrf:          Satellie velocity vector (m/sec)
! - PRNsat:             PRN number of GNSS satellites
!
! Output arguments:
! - eclipse_status:     Satellite attitude' flag according to eclipse status:
!                             0 = Nominal attitude
!                             1 = Non-nominal, midnight turn due to eclipse
!                             2 = Non-nominal, noon turn due to eclipse
! - Yangle:             Yaw angle array (in degrees) with 2 values: Yaw nominal and Yaw modelled
!                             During nominal periods, the two values are equal
! - Yangle_array: Yaw angle (in degrees) array based on nominal attitude and eclipsing model (during eclipse season)
!                             Yaw_angle(1) = Yaw nominal based on nominal attitude 
!                             Yaw_angle(2) = Yaw eclipsing based on attitude model applied during eclipse period 
!                             In case of out of eclipse season, the two values are same and equal to Yaw angle of nominal attitude  
! - Rtrf2bff :          Transformation matrix: Terrestrial reference frame to satellite body-fixed frame 
! - Quaternions_trf2bff: Quaternions of the transformation matrix between terrestrial reference frame and satellite body-fixed frame 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou at Geoscience Australia
! Created:  10 December 2019
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE m_matrixinv
      !USE m_read_satsnx
      IMPLICIT NONE
        
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd
      REAL (KIND = prec_q), INTENT(IN) :: rsat_icrf(3), vsat_icrf(3)
      CHARACTER (LEN=3) , INTENT(IN) :: PRNsat
      !CHARACTER (LEN=100), INTENT(IN) :: satsinex_filename
      CHARACTER (LEN=20), INTENT(IN) :: BLKsat
! OUT
      INTEGER (KIND = 4) :: eclipse_status
      REAL (KIND = prec_d) :: Yangle_array(2)     
      REAL (KIND = prec_q), INTENT(OUT) :: Rtrf2bff(3,3)
      REAL (KIND = prec_q), INTENT(OUT) :: Quaternions_trf2bff(4)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = 4) :: satblk
      CHARACTER (LEN=5)  :: BDSorbtype
      REAL (KIND = prec_d) :: Sec_00, jd0, mjd_1
      !INTEGER (KIND = prec_int4) :: IY, IM, ID
      INTEGER Iyear, Imonth, Iday, J_flag
      DOUBLE PRECISION FD  
      !INTEGER (KIND = prec_int4) :: DOY
      DOUBLE PRECISION  JD, Zbody(6)
      INTEGER  NTARG, NCTR, NTARG_body
      REAL (KIND = prec_q), DIMENSION(3) :: rbody
      REAL (KIND = prec_q), DIMENSION(3) :: rSun            
      REAL (KIND = prec_d) :: beta, Mangle, Yaw_angle
        REAL (KIND = prec_d) , Dimension(3) :: eBX_nom, eBX_ecl

      REAL (KIND = prec_d) :: Rcrf_bff(3,3), Rrtn_bff(3,3)
      REAL (KIND = prec_d) :: Rbff2crf(3,3)
      REAL (KIND = prec_d) :: Rbff2trf(3,3)
      INTEGER (KIND = prec_int8) :: An
      REAL (KIND = prec_d) :: EOP_cr(7)
      REAL (KIND = prec_d) :: CRS2TRS(3,3), TRS2CRS(3,3)
      REAL (KIND = prec_d) :: d_CRS2TRS(3,3), d_TRS2CRS(3,3)
        DOUBLE PRECISION, Dimension(4) :: quater 
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Rz_yaw(3,3)
      REAL (KIND = prec_d) :: Yangle_rad


! ----------------------------------------------------------------------
! GNSS Satellite Block Type
! ----------------------------------------------------------------------
! BLK_TYP :: Global variable in mdl_param
! GPS case: Satellite Block ID:        1=I, 2=II, 3=IIA, IIR=(4, 5), IIF=6
!BLKTYP = BLKsat

satblk = 6
IF(BLKsat=='GPS-I')                   THEN
      satblk = 1
ELSE IF(BLKsat=='GPS-II')       THEN
      satblk = 2
ELSE IF(BLKsat=='GPS-IIA')      THEN
      satblk = 3
ELSE IF(BLKsat=='GPS-IIR')      THEN
      satblk = 4
ELSE IF(BLKsat=='GPS-IIR-A')  THEN
      satblk = 5
ELSE IF(BLKsat=='GPS-IIR-B')  THEN
      satblk = 5
ELSE IF(BLKsat=='GPS-IIR-M')  THEN
      satblk = 5
ELSE IF(BLKsat=='GPS-IIF')    THEN
      satblk = 6
END IF
! ----------------------------------------------------------------------
! Beidou case: 'IGSO', 'MEO'
! 1. BDSorbtype = 'IGSO'
! 2. BDSorbtype = 'MEO'
! 3. BDSorbtype = 'IGSO'
IF(BLKsat=='BDS-2G'.or.BLKsat == 'BDS-3G')            BDSorbtype = 'GEO'  
IF(BLKsat=='BDS-2I'.or.BLKsat == 'BDS-3I'.or.&
   BLKsat=='BDS-3SI-SECM'.or.BLKsat =='BDS-3SI-CAST') BDSorbtype = 'IGSO' 
IF(BLKsat=='BDS-2M'.or.BLKsat == 'BDS-3M'.or.&
   BLKsat=='BDS-3M-SECM'.or.BLKsat =='BDS-3M-CAST')   BDSorbtype = 'MEO'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Sun position vector computation
! ----------------------------------------------------------------------
! Julian Day Number of the input epoch
JD = mjd + 2400000.5D0
! Center celestial body: Earth
NCTR = 3 
! Celestial body's (NTARG) Cartesian coordinates w.r.t. Center body (NCTR)
! Sun: 11 
NTARG = 11

      CALL  PLEPH ( JD, NTARG, NCTR, Zbody )
        
! Cartesian coordinates of the celestial body in meters: KM to M
        rbody(1) = Zbody(1) * 1000.D0
        rbody(2) = Zbody(2) * 1000.D0
        rbody(3) = Zbody(3) * 1000.D0

! Sun
        rSun = rbody
        !vSun = (/ Zbody(4), Zbody(5), Zbody(6) /) * 1000.D0 ! KM/sec to m/sec        
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Satellite Attitude computation 
! ----------------------------------------------------------------------
! Yaw-attitude model
!CALL attitude (mjd, rsat_icrf, vsat_icrf, rSun, PRNsat, satblk, BDSorbtype, &
!                     eclipse_status, beta, Mangle, Yangle_array, eBX_nom, eBX_ecl)                          
CALL attitude (mjd, rsat_icrf, vsat_icrf, rSun, PRNsat, BLKsat, & 
                     eclipse_status, beta, Mangle, Yangle_array, eBX_nom, eBX_ecl)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Rotation matrices
! ----------------------------------------------------------------------
! Rotation matrix Intertial to Body-fixed frame
Yaw_angle = Yangle_array(2)
CALL crf_bff (rsat_icrf, vsat_icrf, Yaw_angle, Rcrf_bff, Rrtn_bff)

IF (1<0) THEN
! Inverse matrix
!Rbff2crf = inv(Rcrf_bff)
!An = size(Rcrf_bff, DIM = 2)
!Call matrixinv (Rcrf_bff, Rbff2crf, An)
CALL matrix_inv3 (Rcrf_bff, Rbff2crf)
 
! Rotation matrix: Intertial to Terrestrial frame
!CALL crs_trs (mjd, EOP_ar, iau_model, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)
CALL EOP (mjd, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)      

! Body-fixed frame to Terrestrial frame
!Rbff2trf = Rbff2crf * CRS2TRS
Rbff2trf = MATMUL(Rbff2crf,CRS2TRS)

! Rotation matrix: Terrestrial reference frame to body-fixed frame
!An = size(Rbff2trf, DIM = 2)
!Call matrixinv (Rbff2trf, Rtrf2bff, An)
CALL matrix_inv3 (Rbff2trf, Rtrf2bff)

END IF 
! ----------------------------------------------------------------------

CALL EOP (mjd, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)      

!Rtrf2bff = MATMUL(TRS2CRS,Rcrf_bff)

Rtrf2bff = MATMUL(Rcrf_bff,TRS2CRS)
!Rtrf2bff = Rcrf_bff
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Quaternions computation based on rotation matrix
! ----------------------------------------------------------------------
! Rotation matrix to quaternions
CALL mat2quater(Rtrf2bff,quater)
Quaternions_trf2bff = quater
! ----------------------------------------------------------------------

end
