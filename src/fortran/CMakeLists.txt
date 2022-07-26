

add_library(pod_lib
	mdl_param.f95
	mdl_precision.f90
	mdl_arr.f90
	mdl_num.f90
	mdl_config.f95
	mdl_eop.f90
	mdl_planets.f90
	mdl_tides.f90
	m_matrixRxR.f95
	m_matrixinv.f95
	m_matrixreverse.f95
	m_matrixmerge.f95
	m_integrEQM.f95
	m_force_tides.f95
	m_tides_ocean.f95
	m_pd_geopotential.f95
	m_legendre.f95
	m_legendre1.f95
	m_legendre2.f95
	m_force_gfm.f95
#	pd_forceZ.f95
	m_get_lambda.f
	m_shadow.f90
	m_pd_empirical.f95
	m_emp_init.f95
	m_pd_ECOM.f90
	m_pulses_init.f95
	m_pd_pulses.f95
	m_pulses_force.f95
	m_satinfo.f90
	m_pd_force.f95
	m_veq_rkn768.f95
	m_veqC2T.f95
	m_integrVEQ.f95
	m_betainfo.f90
	m_orbinteg.f95
	m_orb_estimator.f95
	m_orbC2T.f95
	m_orbT2C.f95
	m_statist.f95
	m_statdelta.f95
	m_orbinfo.f90
	m_statorbit.f95
	m_statorbit2.f95
	m_writearray.f95
	m_writearray2.f95
	m_write_orbres.f95
	m_orbresize.f95
	m_orbext.f95
	m_orbext2.f95
	m_gfc.f95
	m_gfc3.f95
	m_obsorbT2C.f95
	m_writedata.f95
	m_writeorbit.f95
	m_read_svsinex.f95
	m_read_satsnx.f95
	m_writeorbit_multi.f95
	m_write_prmfile_init.f95
	m_rso.f95
	m_sp3.f95
	m_sp3_PRN.f95
	m_lagrange.f95
	m_interporb.f95
	m_interporb_nom.f95
	m_interporb_filt.f95
	m_interpclocks.f95
	m_interpclock_nom.f95
	m_orb_outlier.f95
	m_eop_data.f95
	m_keplerorb.f95
	m_ecom_init.f95
	m_orbdet.f95
	m_orbitmain.f95
	m_orbitIC.f95
	m_attitude_orb.f95
	m_write_orbex.f95
	m_satmetadata.f95
	)

add_library(iau_math
	R3.for
	R1.for
	jd2cal.for
	cal2jd.for
	fad03.for
	faf03.for
	fal03.for
	fama03.for
	fane03.for 
	fapa03.for
	faur03.for
	fae03.for 
	faju03.for
	falp03.for
	fame03.for
	faom03.for
	fasa03.for
	fave03.for
	xy06.for
	s06.for
	c2ixys.for
	xys00a.for
	pnm00a.for
	bpn2xy.for
	s00.for
	pn00a.for
	nut00a.for
	pn00.for
	pr00.for
	obl80.for
	bp00.for
	numat.for
	bi00.for
	ir.for
	rz.for
	ry.for
	rx.for
	anp.for
	era00.for
	gmst00.for
	gmst06.for
	sp00.for
	pom00.for
	taiutc.for
	rxr.for
	tr.for
	cr.for
	cp.for
	)
   
add_library(iers
	ORTHO_EOP.F
	CNMTX.F
	UTLIBR.F
	PMSDNUT2.F
	FUNDARG.F
	RG_ZONT2.F
	)

#ga_math library requires LAPACK as m_matrixinv uses it
add_library(ga_math
	m_matrixRxR.f95
	m_matrixinv.f95
	m_matrixreverse.f95
	m_matrixmerge.f95
	matrix_inv3.f90
	matrix_Rr.f90
	matrix_RxR.f90
	productdot.f90
	productcross.f90
	arctan.f90
	STATE.f90
	PLEPH.f
	CONST.f
	SPLIT.f
	INTERP.f
	FSIZER3.f
	interp_lin.f90
	)

add_library(ga_tz
	time_TT.f90
	time_GPS.f90
	time_UTC.f90
	time_TAI.f90
	time_GPSweek.f90
	time_GPSweek2.f90
	time_GPSweek3.f90
	time_TT_sec.f90
	)

add_library(boxwing
	BOXWINIT.f90
	ERPFBOXW.f90
	SURFBOXW.f90
	PROPBOXW.f90
	SRPFBOXW.f90
	)

add_library(tides
	tides_solid1.f90
	tides_solid2.f90
	tide_perm.f90
	tides_fes2004.f90
	tide_pole_se.f90
	tide_pole_oc.f90
	)

add_library(ga_gen
	doy2str.f95
	f90getopt.F90
	read_cmdline.f95
	)

add_library(rw_param
	write_prmfile_init0.f95
	writeparam.f95
	writeparam1.f95
	write_prmfile.f95
	write_prmfile2.f95
	readparam.f95 
	prm_main.f95
	prm_emp.f95
	prm_srp.f95
	prm_grav.f95
	prm_gravity.f95
	prm_nongrav.f95
	prm_ocean.f95
	prm_orbext.f95
	prm_planets.f95
	prm_pseudobs.f95
	)

add_library(force
	force_gm.f90
	force_gm3rd.f90
	force_srp.f90
	force_erp.f90
	force_ant.f90
	force_sum.f95
	apr_srp.f90
	)

add_library(rel
	rel_schwarzschild.f90
	rel_LenseThirring.f90
	rel_deSitter.f90
	)

add_library(rk
	integr_rkn768.f95
	integr_rk87.f95
	integr_rk4.f95
	)

add_library(yaw
	yaw_nom.f90
	yaw_angle.f90
	yaw_attitude.f90
	yaw_bds.f90
	yaw_gal.f90
	yaw_gal_iov.f95
	yaw_gal_foc.f95
	yawdyn.f90
	att_matrix.f95
	mat2quater.f95
	yaw_gal_foc_nom.f95
	yaw_pred.f95
	colinearity_angle.f95
	colinear_pred.f95
	)

add_library(eop
	EOP.f90
	eop_rd.f90
	eop_c04.f90
	m_eop_cor.f95
	m_eop_igu.f95
	eop_finals2000A.f90
	erp_igu.f90
	)

add_library(kepler
	kepler_eq.f90
	kepler_k2z.f90
	kepler_z2k.f90
	)

add_library(eclipse
	eclips.f
	eclips2017.f
	eclips201707.f
	eclipse_integstep.f95
	)

add_library(yaml
	yaml_types.F90
	yaml_version.F90
	yaml_settings.F90
	yaml.F90
	m_pod_yaml.F90
	fthread.c
	)

add_executable(pod
	report.f90
	gmst_iers.f95
	interp_iers.f
	IERS_CMP_2015.F
	m_read_leapsec.f95
	m_clock_read.f95
	m_write_orb2sp3.f95
	m_pod_gnss.f95
	gblinit.f95
	coord_r2sph.f90
	crs_trs.f90
	era_matrix.f90
	orb_frame.f90
	orb_frame2_unit.f90
	crf_bff.f90
	GM_de.f90
	CATfile.f90
	asc2eph.f90
	indirectJ2.f90
	delaunay.f90
	prn_shift.f95
	surfprop.f90
	scan0orb.f95
	empirical_init.f95
	empirical_cor.f95
	beta_angle.f90
	beta_pred.f90
	attitude.f95
	pd_gm.f95
	pd_gm3rd.f95
#	pd_forceZ.f95
	main_pod.f95
	)

add_executable(crs2trs
	report.f90
	coord_r2sph.f90
	interp_iers.f
	crs_trs.f90
	main_crs2trs.f95
	)

add_executable(brdc2ecef
	report.f90
	m_read_leapsec.f95
	asc2eph.f90
	brdc2ecef.f90
	brdc_cmdline.f95
	brdc_qc_gal.f90
	brdc_qc_gps.f90
	CATfile.f90
	chkbrdc.f90
	crs_trs.f90
	reformbrdc.f90
	glnacc.f90
	glnorbint.f90
	m_antoffset.f90
	mdl_brdconfig.f95
	m_meanstd.f90
	m_write_brd2sp3.f95
	m_write_orb2sp3.f95
	prn2str.f95
	prn_shift_brdc.f95
	readbrdcheader.f95
	readbrdcmessg.f90
	main_brdcorbit.f90
	)

add_executable(timesystem
	mdl_timeconfig.f95
	time_cmdline.f95
	main_time.f95
	)

add_executable(test_yaml
	test_yaml.F90
	)

	
target_include_directories(pod PUBLIC
	.
	${LAPACK_INCLUDE_DIRS}
	${BLAS_INCLUDE_DIRS})

#must get all the modules built first to build all the other libraries - so make everything depend on pod_lib & yaml ...
target_link_libraries(rw_param 	PUBLIC pod_lib	yaml)
target_link_libraries(force 	PUBLIC pod_lib	yaml)
target_link_libraries(eop 		PUBLIC pod_lib	yaml)
target_link_libraries(yaw 		PUBLIC pod_lib	yaml)
target_link_libraries(tides 	PUBLIC pod_lib	yaml)
target_link_libraries(ga_tz 	PUBLIC pod_lib	yaml)
target_link_libraries(rk 		PUBLIC pod_lib	yaml)
target_link_libraries(ga_math 	PUBLIC pod_lib	yaml)
target_link_libraries(iau_math 	PUBLIC pod_lib	yaml)
target_link_libraries(ga_gen 	PUBLIC pod_lib	yaml)
target_link_libraries(boxwing 	PUBLIC pod_lib	yaml)
target_link_libraries(kepler 	PUBLIC pod_lib	yaml)
target_link_libraries(rel 		PUBLIC pod_lib	yaml)
target_link_libraries(eclipse 	PUBLIC pod_lib	yaml)
target_link_libraries(pod_lib 	PUBLIC 			yaml)

# must have pod_lib first to get all the modules. As rw_param requires pod_lib it must be listed again after that. 
# rk and force depend on eop and boxwing and must precede them in the list, likewise yaw must precede eclipse
target_link_libraries(pod 			PUBLIC pod_lib rw_param pod_lib rel kepler yaw eclipse tides ga_tz rk force eop boxwing ga_math ga_gen iers iau_math yaml ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(crs2trs 		PUBLIC pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math	ga_gen iers iau_math yaml 	${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(brdc2ecef 	PUBLIC pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math	ga_gen iers iau_math yaml 	${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(timesystem 	PUBLIC pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math	ga_gen iers iau_math 	${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(test_yaml     PUBLIC																ga_gen		iau_math yaml iau_math	${CMAKE_THREAD_LIBS_INIT}) 

set_target_properties(pod 			PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(crs2trs 		PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(brdc2ecef 	PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(timesystem 	PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(test_yaml 	PROPERTIES LINKER_LANGUAGE Fortran)

if (OpenMP_Fortran_FOUND)
	target_link_libraries(pod PUBLIC OpenMP::OpenMP_Fortran)
endif()

target_compile_definitions(pod 			PRIVATE LAPACK BLAS)
target_compile_definitions(crs2trs 		PRIVATE)
target_compile_definitions(brdc2ecef 	PRIVATE)
target_compile_definitions(timesystem 	PRIVATE)
target_compile_definitions(test_yaml 	PRIVATE)
