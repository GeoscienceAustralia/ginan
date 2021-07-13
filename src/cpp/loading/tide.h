/*!
 *
 * @author Sébastien Allgeyer
 * @date 26/2/21
 *
 */

#ifndef TEST_CODE_TIDE_H
#define TEST_CODE_TIDE_H
#include <string>
#include <boost/multi_array.hpp>
#include "boost_ma_type.h"

/**
 * Implementation of the class  to manage the tides.
 */
class tide {
public:
	tide(){};
	tide(std::string name);
	~tide();
	void set_name(std::string name);
	void read();
	size_t get_nlon(){return nLon;};
	size_t get_nlat(){return nLat;};
	void fill_ReIm();
	float get_lat(size_t i){return lat[i];};
	float get_lon(size_t i){return lon[i];};

	float * get_lat_ptr(){return lat.origin();};
	float * get_lon_ptr(){return lon.origin();};

	float * get_lat_ptr_end(){return lat.origin()+lat.num_elements();};
	float * get_lon_ptr_end(){return lon.origin()+lon.num_elements();};

	double * get_in_ptr(){return in_phase.origin();};
	double * get_out_ptr(){return out_phase.origin();};

	double * get_in_ptr_end(){return in_phase.origin()+in_phase.num_elements();};
	double * get_out_ptr_end(){return out_phase.origin()+out_phase.num_elements();};

private:
	std::string fileName;
	size_t nLon;
	size_t nLat;
	MA1f lat;
	MA1f lon;
	MA2f amplitude;
	MA2f phase;
	MA2d in_phase;
	MA2d out_phase;
	float fillNan;
	std::string wave_name;


};


#endif //TEST_CODE_TIDE_H
