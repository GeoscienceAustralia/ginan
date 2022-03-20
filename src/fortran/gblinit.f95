! ----------------------------------------------------------------------
! SUBROUTINE: globals_init
! ----------------------------------------------------------------------
! Purpose:
!  initialise global variables
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------
SUBROUTINE globals_init()
use mdl_param
use mdl_eop
use mdl_num
use mdl_config
use pod_yaml

        ! from module pod_yaml
        yml_pod_mode = NO_MODE
        soption_on_command_line = .false.
        ooption_on_command_line = .false.
        roption_on_command_line = .false.
        moption_on_command_line = .false.
        noption_on_command_line = .false.
        toption_on_command_line = .false.
        yml_output_dir = "."

        ! from module config
        gbl_debug = 0
        yaml_config = ""
        yaml_found = .false.

        ! from module EOP
        EOP_MJD0_glb = 0.d0

        ! from module NUM
        GMsun_glb = 0.d0
        GMmoon_glb = 0.d0

        ! from module param
        ORBPRED_ARC_glb = 0.d0
!        ORBEXT_glb = 0
        yml_ext_orbit_opt = TYPE_NONE
        SRP_MOD_arp = SRP_MISSING
        yml_ECOM_mode = ECOM_NONE
        yml_EMP_mode = .false.
        yml_srp_parameters = 0
        Bias_accel_glb = 0.d0
        CPR_CS_glb = 0.d0
        Frame_EmpiricalForces_glb = 0
        VEQ_integration_glb = 0
        NPARAM_glb = 0
        yml_estimator_procedure = 0
        yml_estimator_iterations = 0
        yml_veq_refsys = NO_REFSYS
        SATblock_glb = 0
        BDSorbtype_glb = "12345"
        yml_pulses = .false.
        yml_pulse_interval = -1
        yml_pulse_offset = -1
        yml_pulse_epoch_number = -1
        yml_pulse_ref_frame = NO_REFSYS
        yml_pulse_parameter_count = 0
        yml_pulse_parameters = 0
        MJD_DELTA_V_glb = 0.d0
        DELTA_V_apriori_glb = 0.d0
        DELTA_V_aposteriori_glb = 0.d0

        ! only require month parameter (3rd arg) when ERM (1st arg) is 2)
        call BOXWINGINIT(1, 1, 1)
        return
end

! ----------------------------------------------------------------------
! SUBROUTINE: globals_fini
! ----------------------------------------------------------------------
! Purpose:
!  finalise (release memory) global variables
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------
subroutine globals_fini()
use mdl_param
use mdl_tides
use mdl_planets
use mdl_eop
use pod_yaml

        integer(kind = prec_int2) DeallocateStatus

        ! from module param
        if (allocated(ECOM_accel_glb)) Deallocate(ECOM_accel_glb, Stat=DeallocateStatus)
        if (allocated(IDAT)) Deallocate(IDAT, Stat=DeallocateStatus)
        if (allocated(DATS)) Deallocate(DATS, Stat=DeallocateStatus)
        if (allocated(GFM_Cnm)) Deallocate(GFM_Cnm, Stat=DeallocateStatus)
        if (allocated(GFM_Snm)) Deallocate(GFM_Snm, Stat=DeallocateStatus)
        if (allocated(pseudobs_ICRF)) Deallocate(pseudobs_ICRF, Stat=DeallocateStatus)
        if (allocated(pseudobs_ITRF)) Deallocate(pseudobs_ITRF, Stat=DeallocateStatus)
        if (allocated(orbext_ICRF)) Deallocate(orbext_ICRF, Stat=DeallocateStatus)
        if (allocated(orbext_ITRF)) Deallocate(orbext_ITRF, Stat=DeallocateStatus)

        ! from module tides
        if (allocated(Doodson_mult_glb)) Deallocate(Doodson_mult_glb, Stat=DeallocateStatus)
        if (allocated(Delaunay_FES)) Deallocate(Delaunay_FES, Stat=DeallocateStatus)
        if (allocated(dCnm_p)) Deallocate(dCnm_p, Stat=DeallocateStatus)
        if (allocated(dSnm_p)) Deallocate(dSnm_p, Stat=DeallocateStatus)
        if (allocated(dCnm_m)) Deallocate(dCnm_m, Stat=DeallocateStatus)
        if (allocated(dSnm_m)) Deallocate(dSnm_m, Stat=DeallocateStatus)

        ! from module planets
        if (allocated(CVAL_2)) Deallocate(CVAL_2, Stat=DeallocateStatus)
        if (allocated(DB_array)) Deallocate(DB_array, Stat=DeallocateStatus)
        
        ! from module eop
        if (allocated(EOP_day_glb)) Deallocate(EOP_day_glb, Stat=DeallocateStatus)

        ! pulse parameters
!        if (allocated(PULSES_Array_sat_glb)) Deallocate(PULSES_Array_sat_glb, Stat=DeallocateStatus)
        if (allocated(PULSES_Array_aposteriori_glb)) Deallocate(PULSES_Array_aposteriori_glb, Stat=DeallocateStatus)
        if (allocated(PULSES_Array_apriori_glb)) Deallocate(PULSES_Array_apriori_glb, Stat=DeallocateStatus)
        if (allocated(yml_satellites)) Deallocate(yml_satellites, Stat=DeallocateStatus)
        return
end

