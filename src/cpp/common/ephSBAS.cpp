
#if (0)
/* satellite position and clock with sbas correction -------------------------*/
// int satpos_sbas(gtime_t time, gtime_t teph, SatSys Sat, const nav_t* nav,
// 				double* rs, double* dtSat, double* var, int* svh)
// {
// 	const sbssatp_t* sbs;
// 	int i;
// 
// 	trace(4, __FUNCTION__ ": time=%s sat=%2d\n", time.to_string(3).c_str(), Sat);
// 
// 	/* search sbas satellite correciton */
// 	for (i = 0; i < nav->sbssat.nsat; i++)
// 	{
// 		sbs = nav->sbssat.sat + i;
// 
// 		if (sbs->Sat == Sat)
// 			break;
// 	}
// 
// 	if (i >= nav->sbssat.nsat)
// 	{
// 		trace(2, "no sbas correction for orbit: %s sat=%2d\n", time.to_string(0).c_str(), Sat);
// 		ephpos(time, teph, Sat, nav, -1, rs, dts, var, svh);
// 		*svh = -1;
// 
// 		return 0;
// 	}
// 
// 	/* satellite postion and clock by broadcast ephemeris */
// 	if (!ephpos(time, teph, Sat, nav, sbs->lcorr.iode, rs, dts, var, svh))
// 		return 0;
// 
// 	/* sbas satellite correction (long term and fast) */
// 	if (sbssatcorr(time, Sat, nav, rs, dts, var))
// 		return 1;
// 
// 	*svh = -1;
// 
// 	return 0;
// }
#endif
