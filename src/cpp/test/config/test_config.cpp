#include <memory>
#include <iostream>

#include "minunit.h"
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include <boost/program_options.hpp>
#include <boost/range/adaptor/indexed.hpp>
#include <boost/thread.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "config.hpp"

using namespace std;

std::unique_ptr<ACSConfig> config;
//=============================================================================
// Read in the configuration file from example 03 and make sure it parses correctly
//=============================================================================
MU_TEST(test_example03) {
    std::string configFile = "../src/test/config/data/testConfig.yaml";
    config = std::unique_ptr<ACSConfig>{ACSConfig::parse(configFile)};
    cout << "Loading Configuration file: " << configFile << "\n\n";
    cout << "\tRunning: Configuration file parsing test for Example 3: ";

    mu_assert_string_eq("EX03/general/igs14.atx",config->antenna.c_str());
    mu_assert_std_string_eq("EX03/general/igs14.atx",config->antenna);
    mu_assert_std_string_eq("EX03/products/brdc0880.17n",config->navigation);
    mu_assert_std_string_eq("EX03/general/igs17P19423.snx",config->sinex);
    mu_assert_std_string_eq("EX03/general/gpt_25.grd",config->grid);
    mu_assert_std_string_eq("EX03/general/OLOAD_GO.BLQ",config->blqfile);

    // Product files
    mu_assert_std_string_eq("EX03/products/igs19427.erp",config->erpfile);
    mu_assert_std_string_eq("EX03/products/igs19423a.sp3",config->sp3);
    mu_assert_std_string_eq("EX03/products/P1C11703_RINEX.DCB",config->dcbfile);
    mu_assert_std_string_eq("EX03/products/grid5/",config->vmf3dir);
    mu_assert_std_string_eq("EX03/products/orography_ell_5x5",config->orography);

    // Options
    mu_assert_std_string_eq("/data/acs/output/",config->output_directory);
    mu_assert_double_eq(7.5,config->elevation_mask);
    mu_assert_double_eq(30.0,config->epoch_interval);

    mu_assert_int_eq(0,config->trace);

    // Process noise
    mu_assert_double_eq(0.2,config->pnoise_satellite_clock);
    mu_assert_double_eq(0.01,config->pnoise_troposphere);

    //================================================================================
    //pnoise_station_coords: 0.001,0.001,0.003
    double ans[3]; ans[0] = 0.001; ans[1] = 0.001; ans[2]=0.003;
    int i = 0;
    for (double const& pnoise : config->pnoise_station_coords) {
        mu_assert_double_eq(ans[i],pnoise);
	i++;
    }

    /*# ECOM-1 D, Y B cosd sind cosY sinY cosb sinB
    # nm/s**2
    pnoise_satellite_srp_ecom1:
    - 1.0   # D
    - 0.001 # Y
    - 0.001 # B
    - 0.001 # cos(D)
    - 0.001 # sind(D)
    - 0.001 # cos(Y)
    - 0.001 # sin(Y)
    - 0.001 # cos(B)
    - 0.001 # sin(B)
    */

    /*double ecom1_ans[9]; ecom1_ans[0]=1.0; ecom1_ans[1]=0.001; ecom1_ans[2]=0.001; ecom1_ans[3]=0.001; ecom1_ans[4]=0.001; ecom1_ans[5]=0.001; ecom1_ans[6]=0.001; ecom1_ans[7]=0.001; ecom1_ans[8]=0.001;
    i = 0;
    for (double const& pnoise : config->pnoise_satellite_srp_ecom1) {
        mu_assert_double_eq(ecom1_ans[i],pnoise);
	i++;
    }
    */
    //==========================================================================
    //# process noise on satellite position in XYZ and velocity XYZ
    //==========================================================================
    /*double satsigma_ans[9]; satsigma_ans[0]=0.02; satsigma_ans[1]=0.02; satsigma_ans[2]=0.02; satsigma_ans[3]=0.001; satsigma_ans[4]=0.001; satsigma_ans[5]=0.001;
    i = 0;
    for (double const& pnoise : config->sigma_satellite_xyz) {
        mu_assert_double_eq(satsigma_ans[i],pnoise);
	i++;
    }
    */

    //==========================================================================
    // Ionosphere model values
    //==========================================================================
    mu_assert_std_string_eq("spherical_caps",config->ion_model);
/*
satellites:
- type: GPS
  satellite_srp_model: ecom1
  satellite_xyz: 0.02,0.02,0.02,0.001,0.001,0.001
        #   -noise_satellite_srp_ecom1: 1.0, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001
  subtype:
  - type: BLOCKIIR
    satellite_srp_model: ecom2
        #       -srp_model: ecom2
        #        -noise_satellite_srp_ecom2:
        #                satellites:
        #                -{SVN: 378, srp_model: hybrid, noise_satellite_srp_hybrid: 0.001}
        #-subtype: BLOCKIII
        #-srp_model: jplsm
        #-noise_satellite_srp_ecom2:
        #satellites:
  - {TYPE: GPS,
    satellite_srp_model:
    ecom1,noise_satellite_srp_ecom1: 1.0,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,satellite_xyz: 0.02,0.02,0.02,0.001,0.001,0.001}
  - {SVN: 399,satellite_srp_model: boxwing,noise_satellite_srp_boxwing: 0.001}
  - {SUBTYPE: BLOCKIIF, satellite_srp_model: hybrid, noise_satellite_srp_hybrid: 0.001}
  - {SUBTYPE: BLOCKIIR, satellite_srp_model: ecom2, noise_satellite_srp_ecom2: 1.0, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001}

- type: GALILEO
  subtype:
  - type: IOC
    satellite_srp_model: boxwing
*/

    // Observation - not sure what this is actally referring to..
    mu_assert_int_eq(-999,config->count);

    // boost::posix_time::ptime start{boost::posix_time::not_a_date_time};
    // mu_assert_std_string_eq("",config->start)
    // mu_assert_std_string_eq("",config->end)
    mu_assert_int_eq(67,config->streams.size());

    for (auto const & item : config->streams|boost::adaptors::indexed(0)) {

//for (auto const & item : config->streams| boost::adaptors::indexed(0)) {
//725             // TODO for now all streams will be RINEX file streams
//726             BOOST_LOG_TRIVIAL(info) << "In stream loop" << std::endl;
//727             auto *s = reinterpret_cast<RinexFileStream*>(item.value());

// 	 auto *s = reinterpret_cast<RinexFileStream*>(item.value());
	 // Not of stye string, is a ‘StreamType’
	 //cout << "\nTYPE: " << stream->type;
	 //mu_assert_std_string_eq("FILE",stream->type);
// 	 cout << " FORMAT: " << s->format;
	 //mu_assert_std_string_eq("RINEXv2",stream->format);
	 //cout << "ID: " << stream->id;
	 //cout << " ID: " << stream->id;
	 //cout << " Path: " << stream->path;
             //auto s = item->value();
             //cout << "\tStream " << index << " = " << s->info() << "\n";
         //cout << "Root: " << stream->root << "\n";
             //cout << "Path: " << s->path << "\n";
             //cout << "ID: " << s->id << "\n";
             //cout << "Count: " << s->count << "\n";
             //cout << "rec_clock_noise: " << s->receiver_clock_noise << "\n";
         //}

    }



}


MU_TEST(test_IonSphericalCaps) {
    std::string configFile = "../src/test/config/data/IonSphericalCaps.yaml";
    config = std::unique_ptr<ACSConfig>{ACSConfig::parse(configFile)};
    cout << "\n\nLoading Configuration file: " << configFile << "\n\n";
    cout << "\tRunning: Test for minimum Ionosphere Spherical Cap configuration file: ";

    mu_assert_string_eq("EX03/general/igs14.atx",config->antenna.c_str());
    mu_assert_std_string_eq("EX03/general/igs14.atx",config->antenna);
    mu_assert_std_string_eq("EX03/products/brdc0880.17n",config->navigation);
    mu_assert_std_string_eq("EX03/general/igs17P19423.snx",config->sinex);
    mu_assert_std_string_eq("EX03/general/gpt_25.grd",config->grid);
    mu_assert_std_string_eq("EX03/general/OLOAD_GO.BLQ",config->blqfile);

    // Product files
    mu_assert_std_string_eq("EX03/products/igs19427.erp",config->erpfile);
    mu_assert_std_string_eq("EX03/products/igs19423a.sp3",config->sp3);
    mu_assert_std_string_eq("EX03/products/P1C11703_RINEX.DCB",config->dcbfile);

    // Options
    mu_assert_std_string_eq("/data/acs/output/",config->output_directory);
    mu_assert_double_eq(7.5,config->elevation_mask);
    mu_assert_double_eq(30.0,config->epoch_interval);

    mu_assert_int_eq(0,config->trace);

    // Process noise
    mu_assert_double_eq(0.2,config->pnoise_satellite_clock);
    mu_assert_double_eq(0.01,config->pnoise_troposphere);

    //================================================================================
    //pnoise_station_coords: 0.001,0.001,0.003
    double ans[3]; ans[0] = 0.001; ans[1] = 0.001; ans[2]=0.003;
    int i = 0;
    for (double const& pnoise : config->pnoise_station_coords) {
        mu_assert_double_eq(ans[i],pnoise);
	i++;
    }

    // Ionosphere model values
    //==========================================================================
    mu_assert_std_string_eq("spherical_caps",config->ion_model);
    // Observation - not sure what this is actally referring to..
    mu_assert_int_eq(-999,config->count);

    // boost::posix_time::ptime start{boost::posix_time::not_a_date_time};
    // mu_assert_std_string_eq("",config->start)
    // mu_assert_std_string_eq("",config->end)
    mu_assert_int_eq(67,config->streams.size());

    for (auto const& stream : config->streams) {
	 //auto *s = reinterpret_cast<RinexFileStream*>(stream->value());
	 // Not of stye string, is a ‘StreamType’
	 //cout << "\nTYPE: " << stream->type;
	 //mu_assert_std_string_eq("FILE",stream->type);
	 //cout << " FORMAT: " << stream->format;
	 //mu_assert_std_string_eq("RINEXv2",stream->format);
	 //cout << "ID: " << stream->id;
	 //cout << " ID: " << stream->id;
	 //cout << " Path: " << stream->path;
             //auto s = item->value();
             //cout << "\tStream " << index << " = " << s->info() << "\n";
             //cout << "Root: " << stream->root << "\n";
             //cout << "Path: " << s->path << "\n";
             //cout << "ID: " << s->id << "\n";
             //cout << "Count: " << s->count << "\n";
             //cout << "rec_clock_noise: " << s->receiver_clock_noise << "\n";
         //}
    }
}



MU_TEST_SUITE(test_suite) {
    MU_RUN_TEST(test_example03);
    //MU_RUN_TEST(test_IonSphericalCaps);
}

int main(int argc, char *argv[]) {
    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    return 0;
}

