/*!
 *
 * @author Sébastien Allgeyer
 * @date 8/4/21
 *
 */

#include "loadgrid.h"
#include <iostream>
#include <netcdf>
#include <boost/math/interpolators/cardinal_cubic_b_spline.hpp>
#include <boost/algorithm/string/classification.hpp> 
#include <boost/algorithm/string/split.hpp> 


using namespace std;
using namespace netCDF;
using namespace netCDF::exceptions;
using namespace boost::math::interpolators;

loadGrid::loadGrid(std::string name) :  fileName(name ){
	return;
};

loadGrid::~loadGrid()
{
//	amplitude.resize(boost::extents[0][0]);
//	phase.resize(boost::extents[0][0]);
//	return ;
};



void loadGrid::set_name(std::string name) {
	fileName = name;
	return ;
};


void loadGrid::read(){
	try{
		//std::cout << fileName << "\n";
		NcFile datafile(fileName, NcFile::read);
		nWave = datafile.getDim("nwaves").getSize();
	    //std::vector<std::string> test;
		//NcGroupAtt attr = datafile.getAtt("wave_name");
		NcGroupAtt attr = datafile.getAtt("wave_name");
		cout << attr.getAttLength() <<"\n";
		cout << attr.getType().getTypeClassName() << "\n";
		//std::string test;
		//test = new char[5];
		std::string test;
		//test.resize(2);
		attr.getValues(test);
		
		std::vector<std::string> words;
		boost::split(wave_names, test, boost::is_any_of(", "), boost::token_compress_on);

		NcVar lat_var = datafile.getVar("lat");
		nLat = lat_var.getDim(0).getSize() ;

		NcVar lon_var = datafile.getVar("lon");
		nLon = lon_var.getDim(0).getSize() ;

		lat.resize(boost::extents[nLat]);
		lon.resize(boost::extents[nLon]);

		lat_var.getVar(lat.origin());
		lon_var.getVar(lon.origin());
		load.resize(boost::extents[nWave][nLat][nLon]);
		NcVar amp_var = datafile.getVar("waves");
		amp_var.getVar(load.origin());

		datafile.close();
		//cout << "1600,400 => " << amplitude[1600][400] << "  n n  " << amplitude[400][1600] << "\n";
	}catch(NcException &e)
	{
		throw e;
	}
	return ;
};

float loadGrid::interpolate(int itide, float lon_, float lat_)
{
	MA1f lon_val;
	lon_val.resize(boost::extents[nLat]);
//	std::vector< cubic_b_spline<float> > splines;
//	splines.resize(nLat);
	for (int iLat = 0; iLat < nLat; iLat ++ )
	{
		cardinal_cubic_b_spline<float> splines(load[itide][iLat].origin(),nLon, lon[0], static_cast<float>(1.0));
		lon_val[iLat] = splines( lon_ );
//		cout << "for lat " << lat[iLat] << "\t" << lon_val[iLat] <<"\n";
	}

	cardinal_cubic_b_spline<float> splines2(lon_val.origin(),nLat, lat[0], static_cast<float>(1.0));
	return splines2(lat_);
//	for (int iWave = 0; iWave < nWave ; iWave++)
//	{
//		lon_slice =
//	}
	return 0;
}