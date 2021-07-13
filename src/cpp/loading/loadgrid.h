/*!
 *
 * @author Sébastien Allgeyer
 * @date 8/4/21
 *
 */

#ifndef TEST_CODE_LOADGRID_H
#define TEST_CODE_LOADGRID_H
#include <string>
#include <boost/multi_array.hpp>
#include "boost_ma_type.h"

/**
 * Implementation of the class  to manage the tides.
 */
class loadGrid {
public:
	loadGrid(){};
	loadGrid(std::string name);
	~loadGrid();
	void set_name(std::string name);
	void read();
	size_t get_nlon(){return nLon;};
	size_t get_nlat(){return nLat;};
	size_t get_nwave(){return static_cast<size_t> (nWave/6) ;};


	float get_lat(size_t i){return lat[i];};
	float get_lon(size_t i){return lon[i];};

	float * get_lat_ptr(){return lat.origin();};
	float * get_lon_ptr(){return lon.origin();};

	float * get_lat_ptr_end(){return lat.origin()+lat.num_elements();};
	float * get_lon_ptr_end(){return lon.origin()+lon.num_elements();};

	float interpolate(int, float, float);

	std::vector < std::string > get_wave_names(){return wave_names; };
private:
	std::string fileName;
	std::vector <std::string> wave_names;
	size_t nLon;
	size_t nLat;
	size_t nWave;
	MA1f lat;
	MA1f lon;
	MA3f load;
	float fillNan;


};


#endif //TEST_CODE_LOADGRID_H
