/*!
 * Definition of the input structure for the OTL
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */

#ifndef PEA_INPUT_OTL_H
#define PEA_INPUT_OTL_H

#include  "boost_ma_type.h"
struct otl_input {
	std::string green;
	std::string output_blq_file;
	std::vector<std::string> tide_file;
	std::vector< std::vector<float>> xyz_coords;
	std::vector<float> lon;
	std::vector<float> lat;
	std::vector<std::string>  code;
	std::vector< std::vector<float> > dispZ_in;
	std::vector< std::vector<float> > dispEW_in;
	std::vector< std::vector<float> > dispNS_in;
	std::vector< std::vector<float> > dispZ_out;
	std::vector< std::vector<float> > dispEW_out;
	std::vector< std::vector<float> > dispNS_out;
	std::vector< std::string > wave_names;
	MA3cf out_disp; //  nstation, nphase, naxis  
};


#endif //PEA_INPUT_OTL_H
