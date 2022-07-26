
/** \file

Descriptions of use-cases and data flows (work in progress)


*/


/** Iterative process to estimate many products and corrections for distribution to end-users.
 */
void	Operational_Service()
{
	Configure_Orbit_Propagation_Pod();
	Propagate_Orbits_With_Pod();
	Configure_Orbit_Corrections_Pea();
	Correct_Orbits_With_Pea();
	Configure_Orbit_Corrections_Pod();
	Propagate_Corrected_Orbits_With_Pod();
	Configure_Clock_Corrections_Pea();
	Estimate_Clocks_With_Pea();
}


/** Processing scheme that allows optimal processing of many parameters
 */
void Pea_Network_Mode()
{
	Ex17();
	Ex18();
}

/** End-user processing scheme that provides precise results with intermediate receiver hardware and correction products.
 */
void Pea_IF_GPS_User()
{
	Ex11();
// 	Download_ANTEX_Files();
// 	Download_SP3_Files();
// 	Configure_IF_GPS_User_Pea();
// 	Pea();
}

/** End-user processing scheme that provides precise results with low-cost receiver hardware and correction products.
 */
void PEA_SF_GPS_User()
{
	Ex13();
// 	Download_ANTEX_Files();
// 	Download_SP3_Files();
// 	Pea_SF_GPS_User_Configuration();
// 	Pea();
}
