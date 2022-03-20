
// #pragma GCC optimize ("O0")

#include <iostream>

#include "instrument.hpp"
#include "constants.hpp"
#include "jplEph.hpp"
#include "jpl_eph.hpp"



bool jplEphPos(
	struct jpl_eph_data*	jplEph_ptr,
	double					jd,					///< Julian_TT
	E_ThirdBody				thirdBody,			///< Star Needs to Calculate the Velocity and Position
	Vector3d&				pos,				///< Unit: m
	Vector3d*				vel_ptr)			///< vel (m/s)
{
	Instrument instrument(__FUNCTION__);
	
	if (jplEph_ptr == nullptr)
	{
		return false;
	}
	
	double r_p[6];
	int result = jpl_pleph(jplEph_ptr, jd, thirdBody, eEarth, r_p, !!vel_ptr);
	
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
		case -1:		std::cout << "JPL_EPH_OUTSIDE_RANGE"				<< std::endl;	break;
		case -2:		std::cout << "JPL_EPH_READ_ERROR"					<< std::endl;	break;
		case -3:		std::cout << "JPL_EPH_QUANTITY_NOT_IN_EPHEMERIS"	<< std::endl;	break;
		case -5:		std::cout << "JPL_EPH_INVALID_INDEX"				<< std::endl;	break;
		case -6:		std::cout << "JPL_EPH_FSEEK_ERROR"					<< std::endl;	break;
		default:		std::cout << "Result is out of Known Situation"		<< std::endl;	break;
	}
	
	return false;
}

