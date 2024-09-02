/*!
 * Functions to compute the load
 * @author S�bastien Allgeyer
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

/** Compute the loading of a single point
 */
void load_1_point(
	tide*		tide_info,		///< vector of classes containing the tide grids
	otl_input*	input,			///< class containing the coordinates information and also the loading vector
	loading		load,			///< class containing the Green's function
	int			idx)			///< index of the point in the list
{
	MA2d greenZ;
	MA2d greenNS;
	MA2d greenEW;

	greenZ.	resize(boost::extents[tide_info[0].get_nlat()][tide_info[0].get_nlon()]);
	greenNS.resize(boost::extents[tide_info[0].get_nlat()][tide_info[0].get_nlon()]);
	greenEW.resize(boost::extents[tide_info[0].get_nlat()][tide_info[0].get_nlon()]);

	auto greenZ_it = greenZ.origin();
	auto greenEW_it = greenEW.origin();
	auto greenNS_it = greenNS.origin();
	float lat0 = input->lat[idx];
	float lon0 = input->lon[idx];

	for (float *lat_ptr = tide_info[0].get_lat_ptr(); lat_ptr != tide_info[0].get_lat_ptr_end(); lat_ptr++) 
	for (float *lon_ptr = tide_info[0].get_lon_ptr(); lon_ptr < tide_info[0].get_lon_ptr_end(); lon_ptr++) 
	{
		double dist, azimuth;
		calcDistanceBearing(&lat0, &lon0, lat_ptr, lon_ptr, &dist, &azimuth);

		*greenZ_it = load.interpolate_gz(dist);
		*greenNS_it = load.interpolate_gh(dist) * cos(azimuth);
		*greenEW_it = load.interpolate_gh(dist) * sin(azimuth);

		// *greenNS_it *= cos(azimuth);
		// *greenEW_it *= sin(azimuth);
		if (*greenZ_it != *greenZ_it)
		{
			std::cout << " nan detected for " << *lat_ptr << " " <<*lon_ptr << "\n";
			std::cout << dist << "  " << azimuth << "\n";
			exit(0);
		}
		greenZ_it++;
		greenNS_it++;
		greenEW_it++;
	}
	
	// Computing load
	for (int it = 0 ; it < input->tide_file.size() ; it++ ) 
	{
		auto tideim_it = tide_info[it].get_out_ptr();
		auto gz_it = greenZ.origin();
		auto gNS_it = greenNS.origin();
		auto gEW_it = greenEW.origin();
		for (auto tidere_it = tide_info[it].get_in_ptr();
			 tidere_it != tide_info[it].get_in_ptr_end();
			 tidere_it++, tideim_it++, gz_it++, gNS_it++, gEW_it++) 
		{
			
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

	out << "$$ " << input->type << " loading displacement\n";
	out << "$$\n";
	out << "$$ OUTPUT OF interpolate_loading\n";
	out << "$$ Processed " << input->tide_file.size() << " tides\n";
	for (unsigned int i = 0; i < input->tide_file.size(); i++ )
		out << "$$    - " << input->tide_file[i] << "\n";
	out << "$$\n";
	out << "$$ COLUMN ORDER:";
	for (unsigned int i = 0; i < input->wave_names.size(); i++ )
		out << " " << std::setw(3) << input->wave_names[i];
	out << "\n";
	out << "$$\n";
	out << "$$ ROW ORDER:\n";
	out << "$$ AMPLITUDES (m)\n";
	out << "$$   RADIAL\n";
	out << "$$   TANGENTL    EW\n";
	out << "$$   TANGENTL    NS\n";
	out << "$$ PHASES (degrees)\n";
	out << "$$   RADIAL\n";
	out << "$$   TANGENTL    EW\n";
	out << "$$   TANGENTL    NS\n";
	out << "$$\n";
	out << "$$ CMC:  NO   (corr.tide centre of mass)\n";
	out << "$$\n";
	out << "$$ END HEADER\n";
	out << "$$\n";

	for (unsigned int i = 0; i < input->code.size(); i++)
	{
		out << std::fixed << std::setprecision(4);
		out << "  " << input->code[i] << "\n";
		out << "$$ " << input->code[i] << "                    RADI TANG  lon/lat: " << std::setw(9) << input->lon[i] << " " << std::setw(9) << input->lat[i] << "\n";

		// write Amplitudes
		for (int i_dir = 0; i_dir < 3; i_dir++)
		{
			for (int it = 0; it < input->wave_names.size(); it++)  // Will need to write nwaves
				out << " " << std::setprecision(7) << std::setw(10) << std::abs(input->out_disp[i][it][i_dir]) << " ";
			out << "\n";
		}

		// Now write the phases
		for (int i_dir = 0; i_dir < 3; i_dir++)
		{
			for (int it = 0; it < input->wave_names.size(); it++)  // Will need to write nwaves
				out << " " << std::setprecision(4) << std::setw(10) << std::arg(input->out_disp[i][it][i_dir]) * R2D << " ";
			out << "\n";
		}
	}

	out << "$$ END TABLE\n";
	out.close();
}

void write_BLQ(otl_input *input)
{
	std::ofstream out ;
	out.open(input->output_blq_file);

	out << "$$ " << input->type << " loading displacement\n";
	out << "$$\n";
	out << "$$ OUTPUT OF make_otl_blq\n";
	out << "$$ Processed " << input->tide_file.size() << " tides\n";
	for (unsigned int i = 0; i < input->tide_file.size(); i++ )
		out << "$$    - " << input->tide_file[i] << "\n";
	out << "$$\n";
	out << "$$ Green function used is " << input->green << "\n";
	out << "$$\n";
	out << "$$ COLUMN ORDER:";
	for (unsigned int i = 0; i < input->wave_names.size(); i++ )
		out << " " << std::setw(3) << input->wave_names[i];
	out << "\n";
	out << "$$\n";
	out << "$$ ROW ORDER:\n";
	out << "$$ AMPLITUDES (m)\n";
	out << "$$   RADIAL\n";
	out << "$$   TANGENTL    EW\n";
	out << "$$   TANGENTL    NS\n";
	out << "$$ PHASES (degrees)\n";
	out << "$$   RADIAL\n";
	out << "$$   TANGENTL    EW\n";
	out << "$$   TANGENTL    NS\n";
	out << "$$\n";
	out << "$$ CMC:  NO   (corr.tide centre of mass)\n";
	out << "$$\n";
	out << "$$ END HEADER\n";
	out << "$$\n";

	for (unsigned int i = 0; i < input->code.size(); i++)
	{
		out << std::fixed << std::setprecision(4);
		out << "  " << input->code[i] << "\n";
		out << "$$ " << input->code[i] << "                    RADI TANG  lon/lat: " << std::setw(9) << input->lon[i] << " " << std::setw(9) << input->lat[i] << "\n";

		// write Amplitudes
		for (int it = 0; it < input->tide_file.size(); it++)
			out << " " << std::setprecision(5) << std::setw(8) << sqrt(SQR(input->dispZ_out [i][it]) + SQR(input->dispZ_in [i][it])) << " ";
		out << "\n";
		for (int it = 0; it < input->tide_file.size(); it++)
			out << " " << std::setprecision(5) << std::setw(8) << sqrt(SQR(input->dispEW_out[i][it]) + SQR(input->dispEW_in[i][it])) << " ";
		out << "\n";
		for (int it = 0; it < input->tide_file.size(); it++)
			out << " " << std::setprecision(5) << std::setw(8) << sqrt(SQR(input->dispNS_out[i][it]) + SQR(input->dispNS_in[i][it])) << " ";
		out << "\n";

		for (int it = 0; it < input->tide_file.size(); it++)
			out << " " << std::setprecision(1) << std::setw(8) << atan2(input->dispZ_out [i][it], input->dispZ_in [i][it]) * R2D << " ";
		out << "\n";
		for (int it = 0; it < input->tide_file.size(); it++)
			out << " " << std::setprecision(1) << std::setw(8) << atan2(input->dispEW_out[i][it], input->dispEW_in[i][it]) * R2D << " ";
		out << "\n";
		for (int it = 0; it < input->tide_file.size(); it++)
			out << " " << std::setprecision(1) << std::setw(8) << atan2(input->dispNS_out[i][it], input->dispNS_in[i][it]) * R2D << " ";
		out << "\n";
	}

	out << "$$ END TABLE\n";
	out.close();
}


