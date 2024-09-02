
// #pragma GCC optimize ("O0")

#include <iostream>
#include <mutex>  

using std::lock_guard;

#include "coordinates.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "jpl_eph.hpp"
#include "planets.hpp"
#include "enums.h"
#include "erp.hpp"
#include "sofa.h"


std::mutex jplEphMutex;


bool jplEphPos(
	struct jpl_eph_data*	jplEph_ptr,			///< Pointer to jpl binary data
	MjDateTT				mjdTT,				///< Julian_TT
	E_ThirdBody     		thirdBody,			///< Star to Calculate the Velocity and Position
	Vector3d&				pos,				///< Unit: m
	Vector3d*				vel_ptr)			///< vel (m/s)
{
	if (jplEph_ptr == nullptr)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No JPL ephemeris file found.";
		
		return false;
	}
	
	int result;
	double r_p[6];
	{
		lock_guard<mutex> guard(jplEphMutex);
		
		result = jpl_pleph(jplEph_ptr, mjdTT.to_double() + JD2MJD, thirdBody, eEarth, r_p, !!vel_ptr);
	}

	switch (result)
	{
		case 0:
			pos(0) = r_p[0] * AU;
			pos(1) = r_p[1] * AU;
			pos(2) = r_p[2] * AU;

			if (vel_ptr)
			{
				auto& vel = *vel_ptr;
				vel(0) = r_p[3] * AUPerDay;
				vel(1) = r_p[4] * AUPerDay;
				vel(2) = r_p[5] * AUPerDay;
			}
			return true;
		case -1:		std::cout << "JPL_EPH_OUTSIDE_RANGE"				<< "\n";	break;
		case -2:		std::cout << "JPL_EPH_READ_ERROR"					<< "\n";	break;
		case -3:		std::cout << "JPL_EPH_QUANTITY_NOT_IN_EPHEMERIS"	<< "\n";	break;
		case -5:		std::cout << "JPL_EPH_INVALID_INDEX"				<< "\n";	break;
		case -6:		std::cout << "JPL_EPH_FSEEK_ERROR"					<< "\n";	break;
		default:		std::cout << "Result is out of Known Situation"		<< "\n";	break;
	}
	
	return false;
}

bool planetPosEcef(
	GTime		time,
	E_ThirdBody	thirdBody,
	VectorEcef&	rBody,
	ERPValues	erpv)
{
	VectorEci rBodyEci;
	
	bool pass = jplEphPos(nav.jplEph_ptr, time, thirdBody, rBodyEci);
	if (pass == false)
	{
		double pvh[2][3];
		double pvb[2][3];
		
		if		(thirdBody == +E_ThirdBody::SUN)	{	Sofa::iauEpv	(time, pvh, pvb);		for (int i = 0; i < 3; i++)	{	rBodyEci(i) = -pvh[0][i] * AU;	}	}
		else if (thirdBody == +E_ThirdBody::MOON)	{	Sofa::iauMoon	(time, pvh);			for (int i = 0; i < 3; i++)	{	rBodyEci(i) = +pvh[0][i] * AU;	}	}
		else
			return false;
	}
	
	FrameSwapper frameSwapper(time, erpv);
	
	rBody = frameSwapper(rBodyEci);
	
	return true;
}
