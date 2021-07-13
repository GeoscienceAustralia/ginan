/*!
 * Functions to compute the load
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */

#include "load_functions.h"

#include <boost/multi_array.hpp>
#include <iostream>
#include <fstream>
#include <iomanip>

#include "loading.h"
#include "tide.h"
#include "input_otl.h"
#include "boost_ma_type.h"
#include "utils.h"

/**
 * Compute the loading of a single point
 * @param tide_info [in] vector of classes containing the tide grids
 * @param input [in,out] class containing the coordinates information and also the loading vector [output]
 * @param load [in] class containing the Green's function
 * @param idx [in] index of the point in the list
 */
void load_1_point(tide *tide_info, otl_input *input, loading load,  int idx)
{

	MA2d greenZ;
	MA2d greenNS;
	MA2d greenEW;

	greenZ.resize(boost::extents[tide_info[0].get_nlat()][tide_info[0].get_nlon()]);
	greenNS.resize(boost::extents[tide_info[0].get_nlat()][tide_info[0].get_nlon()]);
	greenEW.resize(boost::extents[tide_info[0].get_nlat()][tide_info[0].get_nlon()]);

	auto greenZ_it = greenZ.origin();
	auto greenEW_it = greenEW.origin();
	auto greenNS_it = greenNS.origin();
	float lat0 = input->lat[idx];
	float lon0 = input->lon[idx];

	for (float *lat_ptr = tide_info[0].get_lat_ptr(); lat_ptr != tide_info[0].get_lat_ptr_end(); lat_ptr++) {
		for (float *lon_ptr = tide_info[0].get_lon_ptr(); lon_ptr < tide_info[0].get_lon_ptr_end(); lon_ptr++) {
			double dist, azimuth;
			calcDistanceBearing(&lat0, &lon0, lat_ptr, lon_ptr, &dist, &azimuth);

			*greenZ_it = load.interpolate_gz(dist);
			*greenNS_it = load.interpolate_gh(dist) * cos(azimuth);
			*greenEW_it = load.interpolate_gh(dist) * sin(azimuth);

			// *greenNS_it *= cos(azimuth);
			// *greenEW_it *= sin(azimuth);
			if (*greenZ_it != *greenZ_it)
			{
				std::cout << " nan detected for " << *lat_ptr << " " <<*lon_ptr << std::endl;
				std::cout << dist << "  " << azimuth << std::endl;
				exit(0);
			}
			greenZ_it++;
			greenNS_it++;
			greenEW_it++;
		}
	}



	// Computing load;
	for (int it = 0 ; it < input->tide_file.size() ; it++ ) {
		auto tideim_it = tide_info[it].get_out_ptr();
		auto gz_it = greenZ.origin();
		auto gNS_it = greenNS.origin();
		auto gEW_it = greenEW.origin();
		for (auto tidere_it = tide_info[it].get_in_ptr();
			 tidere_it != tide_info[it].get_in_ptr_end();
			 tidere_it++, tideim_it++, gz_it++, gNS_it++, gEW_it++) {
			
			input->dispEW_in[idx][it] += *gEW_it * *tidere_it;
			input->dispEW_out[idx][it]+= *gEW_it * *tideim_it;
			input->dispZ_in[idx][it]  += *gz_it  * *tidere_it;
			input->dispZ_out[idx][it] += *gz_it  * *tideim_it;
			input->dispNS_in[idx][it] += *gNS_it * *tidere_it;
			input->dispNS_out[idx][it]+= *gNS_it * *tideim_it;

		}
	}
}

void write_BLQ(otl_input *input, int mode)
{
	std::ofstream out ;
	out.open(input->output_blq_file);
	out << "$$ OUTPUT OF CODENAME\n";
	out << "$$ Processed " << input->tide_file.size() << " tides\n";
	for (unsigned int i=0; i< input->tide_file.size(); i++ )
		out << "$$	TF: " << input->tide_file[i] <<"\n";
	out << "$$ Wave Names:\n";
	for (unsigned int i=0; i< input->wave_names.size(); i++ )
		out << "$$	WN: " << input->wave_names[i] <<"\n";
	for (unsigned int i=0; i< input->code.size(); i++) {
		out <<"  " << input->code[i] <<"\n";
		out << "$$ " <<input->code[i] <<"                    RADI TANG  lon/lat: " <<input->lon[i] << " " << input->lat[i] << "\n";
		out.setf(std::ios::fixed);
		out << std::setprecision(5) ;
		// write Amplitudes
		for (int i_dir = 0; i_dir< 3; i_dir++)
		{
			for (int it = 0; it < input->wave_names.size() ; it++)  // Will need to write nwaves
				out << " " << std::setprecision(5)<< std::setw(8) <<  std::abs(input->out_disp[i][it][i_dir]) << " ";
			out << "\n";
		}
		// Now write the angles. 
		for (int i_dir = 0; i_dir< 3; i_dir++)
		{
			for (int it = 0; it < input->wave_names.size() ; it++)  // Will need to write nwaves
				out << " " << std::setprecision(1) << std::setw(8)  << std::arg(input->out_disp[i][it][i_dir]) * 180 / M_PI << " ";
			out << "\n";
		}	
	}
	out << "$$ END TABLE\n";
	out.close();
}

void write_BLQ(otl_input *input)
{
	FILE * fp = fopen(input->output_blq_file.c_str(),"w");
	fprintf(fp,"$$ OUTPUT OF make_otl_blq\n");
	fprintf(fp,"$$ Processed %li tides\n",input->tide_file.size());
	for (unsigned int i=0; i < input->tide_file.size(); i++)
		fprintf(fp,"$$    - %s\n",input->tide_file[i].c_str());
	fprintf(fp,"$$ Green function used is %s\n",input->green.c_str());

	for (unsigned int i=0; i< input->code.size(); i++) {
		fprintf(fp, "  %s\n", input->code[i].c_str());
		fprintf(fp, "$$ %s                    RADI TANG  lon/lat: %f %f\n", input->code[i].c_str(), input->lon[i],
				input->lat[i]);
		for (int it = 0; it < input->tide_file.size(); it++)
			fprintf(fp, " % 1.5f ", sqrt(input->dispZ_out[i][it] * input->dispZ_out[i][it] +
										 input->dispZ_in[i][it] * input->dispZ_in[i][it]));
		fprintf(fp, "\n");
		for (int it = 0; it < input->tide_file.size(); it++)
			fprintf(fp, " % 1.5f ", sqrt(input->dispEW_out[i][it] * input->dispEW_out[i][it] +
										 input->dispEW_in[i][it] * input->dispEW_in[i][it]));
		fprintf(fp, "\n");
		for (int it = 0; it < input->tide_file.size(); it++)
			fprintf(fp, " % 1.5f ", sqrt(input->dispNS_out[i][it] * input->dispNS_out[i][it] +
										 input->dispNS_in[i][it] * input->dispNS_in[i][it]));
		fprintf(fp, "\n");


		for (int it = 0; it < input->tide_file.size(); it++)
			fprintf(fp, " %8.1f ", rad_to_deg(atan2(input->dispZ_out[i][it], input->dispZ_in[i][it])));
		fprintf(fp, "\n");
		for (int it = 0; it < input->tide_file.size(); it++)
			fprintf(fp, " %8.1f ", rad_to_deg(atan2(input->dispEW_out[i][it], input->dispEW_in[i][it])));
		fprintf(fp, "\n");
		for (int it = 0; it < input->tide_file.size(); it++)
			fprintf(fp, " %8.1f ", rad_to_deg(atan2(input->dispNS_out[i][it], input->dispNS_in[i][it])));
		fprintf(fp, "\n");

	}
	fprintf(fp, "$$ END TABLE\n");
	fclose(fp);
}


