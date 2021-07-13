/*!
 *
 * @author Sébastien Allgeyer
 * @date 26/2/21
 *
 */

#include "tide.h"
#include <iostream>
#include <netcdf>
using namespace std;
using namespace netCDF;
using namespace netCDF::exceptions;

tide::tide(std::string name) :  fileName(name ){
	return;
};

tide::~tide()
{
	amplitude.resize(boost::extents[0][0]);
	phase.resize(boost::extents[0][0]);
	return ;
};



void tide::set_name(std::string name) {
	fileName = name;
	return ;
};

void tide::fill_ReIm(){
	double scale = (double)1030./100.0 * (double)6371e3 * (double)6371e3 *
				   (0.0625 * M_PI / 180) * (0.0625 * M_PI / 180);
	auto phase_ptr = phase.origin();
	auto& ma_shape = reinterpret_cast<boost::array<size_t, MA2f::dimensionality> const&>(*amplitude.shape());

	in_phase.resize(ma_shape);
	out_phase.resize(ma_shape);

	auto in_phase_ptr = in_phase.origin();
	auto out_phase_ptr = out_phase.origin();
	auto lat_ptr = lat.origin();
	auto lon_ptr = lon.origin();
	//cout << amplitude.num_elements() << " " << phase.num_elements() << " " << Re.num_elements() << "\n";
	for (auto amp_ptr = amplitude.origin();
		 amp_ptr != amplitude.origin() + amplitude.num_elements() ;
		 ++amp_ptr, ++phase_ptr, ++in_phase_ptr, ++out_phase_ptr, ++lon_ptr)
	{
		if ( *amp_ptr != fillNan)
		{
			*in_phase_ptr = static_cast<double>( *amp_ptr * cos( *phase_ptr * M_PI / 180.0)*scale);
			*out_phase_ptr = static_cast<double>( *amp_ptr * sin( *phase_ptr * M_PI / 180.0)*scale);
			//cout << Re[ilat][ilon] << "\n";
			*in_phase_ptr *= sin(M_PI / 2 - *lat_ptr * M_PI / 180);
			*out_phase_ptr *= sin(M_PI / 2 - *lat_ptr * M_PI / 180);
		} else {
			*in_phase_ptr  = 0.0;
			*out_phase_ptr  = 0.0 ;
		}
		if (lon_ptr == lon.origin()+lon.num_elements() ) { lat_ptr ++; lon_ptr = lon.origin(); };
	}


//    cout << *std::max_element( Re.origin(), Re.origin() + Re.num_elements()) << " " << *std::min_element( Re.origin(), Re.origin() + Re.num_elements())  << "\n";
//    cout << *std::max_element( Im.origin(), Im.origin() + Im.num_elements()) << " " << *std::min_element( Im.origin(), Im.origin() + Im.num_elements())  << "\n";

	return ;
};

void tide::read(){
	try{
		//std::cout << fileName << "\n";
		NcFile datafile(fileName, NcFile::read);
		NcVar lat_var = datafile.getVar("lat");
		nLat = lat_var.getDim(0).getSize() ;
		NcVar lon_var = datafile.getVar("lon");
		nLon = lon_var.getDim(0).getSize() ;
		lat.resize(boost::extents[nLat]);
		lon.resize(boost::extents[nLon]);
		lat_var.getVar(lat.origin());
		lon_var.getVar(lon.origin());
		amplitude.resize(boost::extents[nLat][nLon]);
		phase.resize(boost::extents[nLat][nLon]);
		NcVar amp_var = datafile.getVar("amplitude");
		NcVar phase_var = datafile.getVar("phase");
		amp_var.getVar(&amplitude[0][0]);
		phase_var.getVar(&phase[0][0]);
		bool test;
		phase_var.getFillModeParameters(test,fillNan);

		datafile.close();
		//cout << "1600,400 => " << amplitude[1600][400] << "  n n  " << amplitude[400][1600] << "\n";
	}catch(NcException &e)
	{
		throw e;
	}
	return ;
};
