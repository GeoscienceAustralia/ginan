//
// Created by Sébastien Allgeyer on 24/2/22.
//

#ifndef GINAN_ORBIT_HPP
#define GINAN_ORBIT_HPP

#include "staticField.hpp"
#include "oceanTide.hpp"
#include "gTime.hpp"
#include "erp.hpp"
/** structure of data to be loaded just once.
 *
 */
struct orb_t {
	StaticField egm;
	OceanTide tide;
	struct jpl_eph_data *jplEph_ptr = nullptr;
	GTime startTime;
	ERP erpData;

};

extern orb_t orbit_data;



#endif //GINAN_ORBIT_HPP
