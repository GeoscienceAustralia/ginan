
#pragma once

#include <boost/program_options.hpp>

#include <yaml-cpp/yaml.h>

#include "eigenIncluder.hpp"

#include <unordered_map>
#include <filesystem>
#include <chrono>
#include <memory>
#include <limits>
#include <vector>
#include <mutex>
#include <tuple>
#include <array>
#include <map>
#include <set>

using std::unordered_map;
using std::vector;
using std::string;
using std::tuple;
using std::mutex;
using std::array;
using std::map;
using std::set;

#include "satSys.hpp"
#include "trace.hpp"
#include "enums.h"


#define PI          3.141592653589793238462643383279502884197169399375105820974
#define D2R         (PI/180.0)          ///< deg to rad
#define R2D         (180.0/PI)          ///< rad to deg

template<typename BASE, typename COMP>
bool isInited(
	const	BASE&	base,
	const	COMP&	comp)
{
	int offset = (char*)(&comp) - (char*)(&base);

	auto it = base.initialisedMap.find(offset);
	if (it == base.initialisedMap.end())
	{
		return false;
	}

	auto [dummy, inited] = *it;

	return inited;
}

/** Input source filenames and directories
*/
struct InputOptions
{
	string			inputs_root		= ".";

	string			gnss_obs_root			= "<INPUTS_ROOT>";
	string			pseudo_obs_root			= "<INPUTS_ROOT>";
	string			custom_pseudo_obs_root	= "<INPUTS_ROOT>";
	string			sat_data_root			= "<INPUTS_ROOT>";
	string			rtcm_inputs_root		= "<SAT_DATA_ROOT>";
	string			sisnet_inputs_root		= "<SAT_DATA_ROOT>";

	vector<string>	atx_files;
	vector<string>	snx_files;
	vector<string>	nav_files;
	vector<string>	sp3_files;
	vector<string>	clk_files;
	vector<string>	obx_files;
	vector<string>	sid_files;
	vector<string>	com_files;
	vector<string>	crd_files;
	vector<string>	vmf_files;
	vector<string>	erp_files;
	vector<string>	dcb_files;
	vector<string>	bsx_files;
	vector<string>	ion_files;
	vector<string>	igrf_files;
	vector<string>	egm_files;
	vector<string>	cmc_files;
	vector<string>	hfeop_files;
	vector<string>	gpt2grid_files;
	vector<string>	orography_files;
	vector<string>	pseudo_filter_files;
	vector<string>	atm_reg_definitions;
	vector<string>	planetary_ephemeris_files;
	vector<string>	ocean_tide_potential_files;
	vector<string>	atmos_tide_potential_files;
	vector<string>	ocean_tide_loading_blq_files;
	vector<string>	atmos_tide_loading_blq_files;
	vector<string>	ocean_pole_tide_loading_files;
	vector<string>	atmos_oceean_dealiasing_files;
	vector<string>	ocean_pole_tide_potential_files;

	vector<string>	sisnet_inputs;
	vector<string>	nav_rtcm_inputs;
	vector<string>	qzs_rtcm_inputs;

	map<string, vector<string>>	rnx_inputs;
	map<string, vector<string>>	ubx_inputs;
	map<string, vector<string>>	custom_inputs;
	map<string, vector<string>>	obs_rtcm_inputs;
	map<string, vector<string>>	pseudo_sp3_inputs;
	map<string, vector<string>>	pseudo_snx_inputs;


	vector<E_TidalComponent>	atl_blq_row_order	= {E_TidalComponent::UP,	E_TidalComponent::EAST,		E_TidalComponent::NORTH};
	vector<E_TidalComponent>	otl_blq_row_order	= {E_TidalComponent::UP,	E_TidalComponent::WEST,		E_TidalComponent::SOUTH};

	vector<E_TidalConstituent>	atl_blq_col_order =
	{
		E_TidalConstituent::S1,
		E_TidalConstituent::S2
	};

	vector<E_TidalConstituent>	otl_blq_col_order =
	{
		E_TidalConstituent::M2,
		E_TidalConstituent::S2,
		E_TidalConstituent::N2,
		E_TidalConstituent::K2,
		E_TidalConstituent::K1,
		E_TidalConstituent::O1,
		E_TidalConstituent::P1,
		E_TidalConstituent::Q1,
		E_TidalConstituent::MF,
		E_TidalConstituent::MM,
		E_TidalConstituent::SSA
	};


	bool						eci_pseudoobs		= false;

	string stream_user;
	string stream_pass;
};

struct IonexOptions
{
	double			lat_centre	= 0;
	double			lon_centre	= 0;
	double			lat_width	= 90;
	double			lon_width	= 90;
	double			lat_res		= 10;
	double			lon_res		= 10;
	double			time_res	= 900;
};

/** Enabling and setting destiations of program outputs.
*/
struct OutputOptions
{
	string	outputs_root				= ".";

	int		fatal_level					= 0;
	double	rotate_period				= 60*60*24;

	int		trace_level					= 0;
	bool	output_receiver_trace		= false;
	bool	output_network_trace		= false;
	bool	output_ionosphere_trace		= false;
	bool	output_satellite_trace		= false;
	bool	output_json_trace			= false;
	string	trace_directory				= "<OUTPUTS_ROOT>";
	string	receiver_trace_filename		= "<TRACE_DIRECTORY>/<RECEIVER>-<LOGTIME>.trace";
	string	network_trace_filename		= "<TRACE_DIRECTORY>/<RECEIVER>-<LOGTIME>.trace";
	string	ionosphere_trace_filename	= "<TRACE_DIRECTORY>/<RECEIVER>-<LOGTIME>.trace";
	string	satellite_trace_filename	= "<TRACE_DIRECTORY>/<RECEIVER>-<LOGTIME>.trace";

	bool	record_raw_ubx				= false;
	string	raw_ubx_directory			= "<OUTPUTS_ROOT>";
	string	raw_ubx_filename			= "<UBX_DIRECTORY>/<RECEIVER>-<LOGTIME>-OBS.rtcm";

	bool	record_raw_custom			= false;
	string	raw_custom_directory		= "<OUTPUTS_ROOT>";
	string	raw_custom_filename			= "<CUSTOM_DIRECTORY>/<RECEIVER>-<LOGTIME>-OBS.custom";

	bool	record_rtcm_obs				= false;
	bool	record_rtcm_nav				= false;
	string	rtcm_obs_directory			= "<OUTPUTS_ROOT>";
	string	rtcm_nav_directory			= "<OUTPUTS_ROOT>";
	string  rtcm_obs_filename			= "<RTCM_OBS_DIRECTORY>/<RECEIVER>-<LOGTIME>-OBS.rtcm";
	string  rtcm_nav_filename			= "<RTCM_NAV_DIRECTORY>/<STREAM>-<LOGTIME>-NAV.rtcm";

	bool	output_log					= false;
	string	log_directory				= "<OUTPUTS_ROOT>";
	string  log_filename				= "<LOG_DIRECTORY>/log-<LOGTIME>.json";

	bool	output_ntrip_log			= false;
	string	ntrip_log_directory			= "<OUTPUTS_ROOT>";
	string  ntrip_log_filename			= "<NTRIP_LOG_DIRECTORY>/ntrip_log-<LOGTIME>.json";

	bool	output_gpx					= false;
	string	gpx_directory				= "<OUTPUTS_ROOT>";
	string  gpx_filename				= "<GPX_DIRECTORY>/<RECEIVER>-<LOGTIME>.gpx";

	bool	output_pos					= false;
	string	pos_directory				= "<OUTPUTS_ROOT>";
	string  pos_filename				= "<POS_DIRECTORY>/<RECEIVER>-<LOGTIME>.pos";

	string	root_stream_url				= "";

	bool	output_predicted_states		= false;
	bool	output_initialised_states	= false;
	bool	output_residuals 			= false;
	bool	output_residual_chain		= true;

	bool	output_config	 			= false;
	bool	colourise_terminal			= true;
	bool	warn_once					= true;

	bool					output_clocks 				= false;
	vector<E_Source>		clocks_receiver_sources  	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>		clocks_satellite_sources 	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	double					clocks_output_interval		= 1;
	string					clocks_directory			= "<OUTPUTS_ROOT>";
	string					clocks_filename				= "<CLOCKS_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>.clk";

	bool					output_sp3 					= false;
	bool					output_inertial_orbits		= false;
	bool					output_sp3_velocities		= false;
	vector<E_Source>		sp3_orbit_sources 			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>		sp3_clock_sources 			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	double					sp3_output_interval			= 1;
	string					sp3_directory				= "<OUTPUTS_ROOT>";
	string					sp3_filename				= "<SP3_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>-Filt.sp3";
	string					predicted_sp3_filename		= "<SP3_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>-Prop.sp3";

	bool					output_orbex 				= false;
	vector<E_Source>		orbex_orbit_sources			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>		orbex_clock_sources			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>		orbex_attitude_sources 		= {E_Source::NOMINAL};
	double					orbex_output_interval		= 1;
	string					orbex_directory				= "<OUTPUTS_ROOT>";
	string					orbex_filename				= "<ORBEX_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>.obx";
	vector<E_OrbexRecord>	orbex_record_types			= {E_OrbexRecord::ATT};


	bool	split_sys					= false;

	bool	output_rinex_obs			= false;
	string	rinex_obs_directory			= "<OUTPUTS_ROOT>";
	string	rinex_obs_filename			= "<RINEX_OBS_DIRECTORY>/<RECEIVER>-<LOGTIME>_<SYS>.<YY>O";
	double	rinex_obs_version			= 3.05;
	bool	rinex_obs_print_C_code		= true;
	bool	rinex_obs_print_L_code		= true;
	bool	rinex_obs_print_D_code	 	= true;
	bool	rinex_obs_print_S_code 		= true;

	bool	output_ionex 				= false;
	string	ionex_directory				= "<OUTPUTS_ROOT>";
	string	ionex_filename				= "<IONEX_DIRECTORY>/<CONFIG>-<LOGTIME>.INX";
	IonexOptions	ionexGrid;

	bool	output_rinex_nav			= false;
	string	rinex_nav_directory			= "<OUTPUTS_ROOT>";
	string	rinex_nav_filename			= "<RINEX_NAV_DIRECTORY>/<CONFIG>-<LOGTIME>_nav_<SYS>.rnx";
	double	rinex_nav_version			= 3.05;

	bool	output_ionstec				= false;
	string	ionstec_directory			= "<OUTPUTS_ROOT>";
	string	ionstec_filename			= "<IONSTEC_DIRECTORY>/<CONFIG>-<LOGTIME>.STEC";

	bool	output_erp						= false;
	string	erp_directory					= "<OUTPUTS_ROOT>";
	string	erp_filename					= "<ERP_DIRECTORY>/<CONFIG>-<LOGTIME>.ERP";

	bool				output_bias_sinex		= false;
	string				bias_sinex_directory	= "<OUTPUTS_ROOT>";
	string				bias_sinex_filename		= "<BIAS_SINEX_DIRECTORY>/<CONFIG>-<LOGTIME>.BIA";
	string				bias_time_system		= "G";

	bool				output_sinex			= false;
	string 				sinex_directory			= "<OUTPUTS_ROOT>";
	string				sinex_filename			= "<SINEX_DIRECTORY>/<CONFIG>-<LOGTIME>.snx";

	bool				output_trop_sinex		= false;
	vector<E_Source>	trop_sinex_data_sources	= {E_Source::KALMAN};
	string				trop_sinex_directory	= "<OUTPUTS_ROOT>";
	string				trop_sinex_filename		= "<TROP_SINEX_DIRECTORY>/<CONFIG>-<LOGTIME>.tro";
	string				trop_sinex_sol_type		= "Solution parameters";
	char				trop_sinex_obs_code		= 'P';
	char				trop_sinex_const_code	= ' ';
	double				trop_sinex_version		= 2.0;


	bool				output_cost			= false;
	vector<E_Source>	cost_data_sources	= {E_Source::KALMAN};
	string				cost_directory		= "<OUTPUTS_ROOT>";
	string				cost_filename		= "<COST_DIRECTORY>/cost_s_t_<LOGTIME>_<RECEIVER>_ga__.dat";
	int					cost_time_interval	= 900;
	string				cost_format			= "COST-716 V2.2";
	string				cost_project		= "GA-NRT";
	string				cost_status			= "TEST";
	string				cost_centre			= "GA__ Geoscience Aus";
	string				cost_method			= "GINAN V2";
	string				cost_orbit_type		= "IGSPRE";
	string				cost_met_source		= "NONE";

	bool	output_slr_obs				= false;
	string 	slr_obs_directory			= "<OUTPUTS_ROOT>";
	string 	slr_obs_filename			= "<SLR_OBS_DIRECTORY>/<RECEIVER>.slr_obs";

	bool	output_orbit_ics			= false;
	string	orbit_ics_directory			= "<OUTPUTS_ROOT>";
	string	orbit_ics_filename			= "<ORBIT_ICS_DIRECTORY>/<CONFIG>-<LOGTIME>-orbits.yaml";

	bool	output_sbas_ems				= false;
	string	ems_directory				= "<OUTPUTS_ROOT>";
	string	ems_filename				= "<EMS_DIRECTORY>/y<YYYY>/d<DDD>/h<HH>.ems";

	void defaultOutputOptions()
	{
		*this = OutputOptions();
	}

	bool	output_decoded_rtcm_json	= false;
	string 	decoded_rtcm_json_directory	= "<OUTPUTS_ROOT>";
	string	decoded_rtcm_json_filename	= "<DECODED_RTCM_DIRECTORY>/<CONFIG>-<LOGTIME>_rtcm_decoded.json";

	bool	output_encoded_rtcm_json	= false;
	string	encoded_rtcm_json_directory	= "<OUTPUTS_ROOT>";
	string	encoded_rtcm_json_filename	= "<ENCODED_RTCM_DIRECTORY>/<CONFIG>-<LOGTIME>_rtcm_encoded.json";

	bool	output_network_statistics_json		= false;
	string	network_statistics_json_directory	= "<OUTPUTS_ROOT>";
	string	network_statistics_json_filename	= "<NETWORK_STATISTICS_DIRECTORY>/<CONFIG>-<LOGTIME>_network_statistics.json";
};

/** Options to be used only for debugging new features
*/
struct DebugOptions
{
	bool	mincon_only					= false;
	bool	output_mincon				= false;
	string	mincon_filename				= "preMinconState.bin";
	bool	check_plumbing				= false;
	bool	retain_rts_files			= false;
	bool	rts_only					= false;
	bool	explain_measurements		= false;
	bool	compare_orbits				= false;
	bool	compare_clocks				= false;
	bool	compare_attitudes			= false;

	bool	check_broadcast_differences	= false;
};

/** Options for processing SLR observations
*/
struct SlrOptions
{
	bool	process_slr					= false;
};



struct PreprocOptions
{
	double	slip_threshold	= 0.05;
	double	mw_proc_noise	= 0;

	bool	preprocess_all_data			= true;
};

struct SlipOptions
{
	bool LLI		= true;
	bool GF			= true;
	bool MW			= true;
	bool SCDIA		= true;
};

struct ExcludeOptions : SlipOptions
{
	bool bad_spp	= true;
	bool config		= true;
	bool eclipse	= true;
	bool elevation	= true;
	bool outlier	= true;
	bool system		= true;
	bool svh		= true;
};

struct AmbiguityErrorHandler
{
	int			phase_reject_limit		= 10;
	int			outage_reset_limit		= 300;

	SlipOptions	resetOnSlip;
};

struct IonoErrorHandler
{
	int			outage_reset_limit		= 300;
};

struct OrbitErrorHandler
{
	bool	enable						= false;
	double	pos_proc_noise				= 10;
	double	vel_proc_noise				= 5;
	double	vel_proc_noise_trail		= 1;
	double	vel_proc_noise_trail_tau	= 0.05;
};

struct StateErrorHandler
{
	bool	enable			= true;
	double	deweight_factor	= 1000;
};

struct MeasErrorHandler
{
	bool	enable			= true;
	double	deweight_factor	= 1000;
};

struct ErrorAccumulationHandler
{
	bool	enable							= true;
	int		receiver_error_count_threshold	= 4;
	int		receiver_error_epochs_threshold	= 4;
};


/** Options for the general operation of the software
*/
struct GlobalOptions
{
	int		sleep_milliseconds		= 50;
	double	epoch_interval			= 1;
	double	epoch_tolerance			= 0.5;
	int		max_epochs				= 0;
	int		leap_seconds			= -1;

	boost::posix_time::ptime start_epoch	{ boost::posix_time::not_a_date_time };
	boost::posix_time::ptime end_epoch		{ boost::posix_time::not_a_date_time };

	string	config_description				= "Pea";
	string	config_details;
	string	analysis_agency					= "GAA";
	string	analysis_centre					= "Geoscience Australia";
	string	analysis_software				= "Ginan";
	string	analysis_software_version		= "3.0";
	string	ac_contact						= "clientservices@ga.gov.au";
	string	rinex_comment					= "Daily 30-sec observations from IGS stations";
	string	reference_system				= "igb14";
	string	time_system						= "G";
	string	ocean_tide_loading_model		= "FES2004";
	string	atmospheric_tide_loading_model	= "---";
	string	geoid_model						= "EGM96";
	string	gradient_mapping_function		= "Chen & Herring, 1992";

	bool	simulate_real_time			= false;

	bool	process_preprocessor		= true;
	bool	process_spp					= true;
	bool	process_minimum_constraints	= false;
	bool	process_ionosphere			= false;
	bool	process_rts					= false;
	bool	process_ppp					= false;
	bool	process_orbits				= false;

	map<E_Sys,	bool>				process_sys;
	map<E_Sys,	bool>				solve_amb_for;
	bool							process_meas[NUM_MEAS_TYPES] = {true, true};
	map<E_Sys,	bool>				reject_eclipse;


	string	pivot_receiver	= "NO_PIVOT";
	string	pivot_satellite	= "NO_PIVOT";

	bool	interpolate_rec_pco			= true;
	bool	auto_fill_pco				= true;
	bool	require_apriori_positions	= false;
	bool	require_site_eccentricity	= false;
	bool	require_sinex_data			= false;
	bool	require_antenna_details		= false;
	bool	require_reflector_com		= false;


	bool	use_tgd_bias	= false;

	double	wait_next_epoch				= 0;
	double	max_rec_latency				= 0;
	bool	require_obs					= true;
	bool	assign_closest_epoch		= false;

	bool	delete_old_ephemerides  	= true;

// 	bool	reinit_on_clock_error		= false;
	double	validity_interval_factor	= 10;

	E_OffsetType ssr_input_antenna_offset = E_OffsetType::UNSPECIFIED;

	map<E_Sys, vector<E_ObsCode>>	code_priorities;
	map<E_Sys, E_NavMsgType>		used_nav_types =				///< Default observation codes on L1 for IF combination based satellite clocks
	{
		{E_Sys::GPS, E_NavMsgType::LNAV},
		{E_Sys::GLO, E_NavMsgType::FDMA},
		{E_Sys::GAL, E_NavMsgType::INAV},
		{E_Sys::BDS, E_NavMsgType::D1},
		{E_Sys::QZS, E_NavMsgType::LNAV}
	};

	vector<E_ObsCode> code_priorities_default =
	{
		E_ObsCode::L1C,
		E_ObsCode::L1P,
		E_ObsCode::L1Y,
		E_ObsCode::L1W,
		E_ObsCode::L1M,
		E_ObsCode::L1N,
		E_ObsCode::L1S,
		E_ObsCode::L1L,
		E_ObsCode::L1X,

		E_ObsCode::L2W,
		E_ObsCode::L2P,
		E_ObsCode::L2Y,
		E_ObsCode::L2C,
		E_ObsCode::L2M,
		E_ObsCode::L2N,
		E_ObsCode::L2D,
		E_ObsCode::L2S,
		E_ObsCode::L2L,
		E_ObsCode::L2X,

		E_ObsCode::L5I,
		E_ObsCode::L5Q,
		E_ObsCode::L5X
	};

	double fixed_phase_bias_var   = 0.01;

	bool	adjust_rec_clocks_by_spp		= true;
	bool	adjust_clocks_for_jumps_only	= false;
	bool	minimise_sat_clock_offsets		= false;
	bool	minimise_sat_orbit_offsets		= false;
	bool	minimise_ionosphere_offsets		= false;

	map<E_Sys, bool> receiver_amb_pivot;		///< fix one ambiguity to eliminate rank deficiency
	map<E_Sys, bool> network_amb_pivot;			///< fix ambiguities to eliminate network rank deficiencies
	map<E_Sys, bool> use_for_iono_model;		///< use system for ionospheric modelling
	map<E_Sys, bool> use_iono_corrections;		///< use system for ionospheric modelling

	bool common_sat_pco			= false;
	bool common_rec_pco			= false;
	bool use_trop_corrections	= false;

	//to be removed?

	double			predefined_fail		= 0.001;	/* pre-defined fail-rate (0.01,0.001) */
};

/** Options associated with kalman filter states
*/
struct KalmanModel
{
	vector<double>	sigma				= {-1};	//{0} is very necessary
	vector<double>	apriori_value		= {0};
	vector<double>	process_noise		= {0};
	vector<double>	tau					= {-1};	//tau<0 (inf): Random Walk model; tau>0: First Order Gauss Markov model
	vector<double>	mu					= {0};
	vector<bool>	estimate			= {false};
	vector<bool>	use_remote_sigma	= {false};
	vector<string>	comment				= {""};

	KalmanModel& operator+=(
		const KalmanModel& rhs);

	map<int, bool>	initialisedMap;
};

struct PrefitOptions
{
	int			max_iterations	 		= 2;
	bool		sigma_check				= true;
	double		state_sigma_threshold	= 4;
	double		meas_sigma_threshold	= 4;
	bool		omega_test				= false;
};

struct PostfitOptions
{
	int			max_iterations	 		= 2;
	bool		sigma_check				= true;
	double		state_sigma_threshold	= 4;
	double		meas_sigma_threshold	= 4;
};

struct ChiSquareOptions
{
	bool		enable					= false;
	E_ChiSqMode	mode					= E_ChiSqMode::INNOVATION;
	double		sigma_threshold			= 4;
};

struct RtsOptions
{
	string		rts_filename			= "Filter-<CONFIG>-<RECEIVER>.rts";
	string		rts_directory			= "<OUTPUTS_ROOT>";
	int			rts_lag					= -1;
	int			rts_interval			= 0;
	string		rts_smoothed_suffix		= "_smoothed";
	bool		output_intermediate_rts	= false;

	bool		queue_rts_outputs		= false;

	E_Inverter	rts_inverter			= E_Inverter::LDLT;
};

struct FilterOptions : RtsOptions
{
	bool		simulate_filter_only	= false;
	bool		assume_linearity		= false;
	bool		advanced_postfits		= false;

	bool		joseph_stabilisation	= false;

	E_Inverter	inverter				= E_Inverter::LDLT;

	PrefitOptions		prefitOpts;
	PostfitOptions		postfitOpts;
	ChiSquareOptions	chiSquareTest;
};

/** Options associated with the ionospheric modelling processing mode of operation
*/
struct IonosphericOptions
{
	E_IonoMode 		corr_mode  		= E_IonoMode::BROADCAST;

	bool			common_ionosphere				= true;
	bool			use_if_combo					= false;
	bool			use_gf_combo					= false;

};

/** Options associated with the ppp processing mode of operation
*/
struct PppOptions : FilterOptions
{
	KalmanModel				eop;
	KalmanModel				eop_rates;

	IonosphericOptions		ionoOpts;

	bool			equate_ionospheres		= false;
	bool			equate_tropospheres		= false;
	bool			use_rtk_combo			= false;

	bool			add_eop_component		= false;

	bool			satellite_chunking		= false;
	bool			receiver_chunking		= false;
	int				chunk_size				= 0;

	bool			nuke_enable				= false;
	int				nuke_interval			= 86400;
	vector<KF>		nuke_states				= {KF::ALL};
};

struct SppOptions : FilterOptions
{
	bool		always_reinitialise	= false;
	int			max_lsq_iterations	= 12;
	double		max_gdop			= 30;
	double		sigma_scaling		= 1;
	bool		raim				= true;

	E_IonoMode 	iono_mode  			= E_IonoMode::IONO_FREE_LINEAR_COMBO;
};

struct IonModelOptions : FilterOptions
{
	E_IonoModel		model			= E_IonoModel::NONE;
	int				numBasis;		///< not a directly configurable parameter
	int				function_order;
	int				function_degree;

	vector<double>	layer_heights;
	bool			estimate_sat_dcb	= true;
	bool			use_rotation_mtx	= false;
	double			basis_sigma_limit	= 1000;

	KalmanModel	ion;
};

struct AmbROptions
{
	E_ARmode	mode				= E_ARmode::OFF;
	int			lambda_set			= 2;
	int			AR_max_itr			= 1;

	double		elevation_mask_deg	= 15;

	double	succsThres = 0.9999;	///< Thresholds for ambiguity validation: succsess rate NL
	double	ratioThres = 3;			///< Thresholds for ambiguity validation: succsess rate NL

	double	code_output_interval	= 0;		///< Update interval for code  biases, 0: no output
	double	phase_output_interval	= 0;		///< Update interval for phase biases, 0: no output
	bool	output_rec_bias			= false;	///< Output receivr bias

	bool	once_per_epoch			= true;
	bool	fix_and_hold			= false;
};

/** Rinex 2 conversions for individual receivers
*/
struct Rinex23Conversion
{
	map<E_ObsCode2, E_ObsCode> codeConv =
	{
		{E_ObsCode2::P1, E_ObsCode::L1C},
		{E_ObsCode2::P2, E_ObsCode::L2W}
	};

	map<E_ObsCode2, E_ObsCode> phasConv =
	{
		{E_ObsCode2::L1, E_ObsCode::L1C},
		{E_ObsCode2::L2, E_ObsCode::L2W}
	};

	Rinex23Conversion& operator +=(const Rinex23Conversion& rhs)
	{
		for (auto& [code2, code3] : rhs.codeConv)		{	if (code3 != +E_ObsCode::NONE)		codeConv[code2] = code3;		}
		for (auto& [code2, code3] : rhs.phasConv)		{	if (code3 != +E_ObsCode::NONE)		phasConv[code2] = code3;		}

		return *this;
	}
};







struct InertialKalmans
{
	KalmanModel			accelerometer_scale;
	KalmanModel			accelerometer_bias;
	KalmanModel			orientation;
	KalmanModel			imu_offset;
	KalmanModel			gyro_scale;
	KalmanModel			gyro_bias;

	InertialKalmans& operator+=(
		const InertialKalmans& rhs)
	{
		accelerometer_scale	+= rhs.accelerometer_scale;
		accelerometer_bias	+= rhs.accelerometer_bias;
		orientation			+= rhs.orientation;
		imu_offset			+= rhs.imu_offset;
		gyro_scale			+= rhs.gyro_scale;
		gyro_bias			+= rhs.gyro_bias;

		return *this;
	}
};

struct EmpKalmans
{
	KalmanModel			emp_d_0;
	KalmanModel			emp_d_1;
	KalmanModel			emp_d_2;
	KalmanModel			emp_d_3;
	KalmanModel			emp_d_4;

	KalmanModel			emp_y_0;
	KalmanModel			emp_y_1;
	KalmanModel			emp_y_2;
	KalmanModel			emp_y_3;
	KalmanModel			emp_y_4;

	KalmanModel			emp_b_0;
	KalmanModel			emp_b_1;
	KalmanModel			emp_b_2;
	KalmanModel			emp_b_3;
	KalmanModel			emp_b_4;

	KalmanModel			emp_r_0;
	KalmanModel			emp_r_1;
	KalmanModel			emp_r_2;
	KalmanModel			emp_r_3;
	KalmanModel			emp_r_4;

	KalmanModel			emp_t_0;
	KalmanModel			emp_t_1;
	KalmanModel			emp_t_2;
	KalmanModel			emp_t_3;
	KalmanModel			emp_t_4;

	KalmanModel			emp_n_0;
	KalmanModel			emp_n_1;
	KalmanModel			emp_n_2;
	KalmanModel			emp_n_3;
	KalmanModel			emp_n_4;

	KalmanModel			emp_p_0;
	KalmanModel			emp_p_1;
	KalmanModel			emp_p_2;
	KalmanModel			emp_p_3;
	KalmanModel			emp_p_4;

	KalmanModel			emp_q_0;
	KalmanModel			emp_q_1;
	KalmanModel			emp_q_2;
	KalmanModel			emp_q_3;
	KalmanModel			emp_q_4;

	EmpKalmans& operator+=(
		const EmpKalmans& rhs)
	{
		emp_d_0	+= rhs.emp_d_0;
		emp_d_1	+= rhs.emp_d_1;
		emp_d_2	+= rhs.emp_d_2;
		emp_d_3	+= rhs.emp_d_3;
		emp_d_4	+= rhs.emp_d_4;

		emp_y_0	+= rhs.emp_y_0;
		emp_y_1	+= rhs.emp_y_1;
		emp_y_2	+= rhs.emp_y_2;
		emp_y_3	+= rhs.emp_y_3;
		emp_y_4	+= rhs.emp_y_4;

		emp_b_0	+= rhs.emp_b_0;
		emp_b_1	+= rhs.emp_b_1;
		emp_b_2	+= rhs.emp_b_2;
		emp_b_3	+= rhs.emp_b_3;
		emp_b_4	+= rhs.emp_b_4;

		emp_r_0	+= rhs.emp_r_0;
		emp_r_1	+= rhs.emp_r_1;
		emp_r_2	+= rhs.emp_r_2;
		emp_r_3	+= rhs.emp_r_3;
		emp_r_4	+= rhs.emp_r_4;

		emp_t_0	+= rhs.emp_t_0;
		emp_t_1	+= rhs.emp_t_1;
		emp_t_2	+= rhs.emp_t_2;
		emp_t_3	+= rhs.emp_t_3;
		emp_t_4	+= rhs.emp_t_4;

		emp_n_0	+= rhs.emp_n_0;
		emp_n_1	+= rhs.emp_n_1;
		emp_n_2	+= rhs.emp_n_2;
		emp_n_3	+= rhs.emp_n_3;
		emp_n_4	+= rhs.emp_n_4;

		emp_p_0	+= rhs.emp_p_0;
		emp_p_1	+= rhs.emp_p_1;
		emp_p_2	+= rhs.emp_p_2;
		emp_p_3	+= rhs.emp_p_3;
		emp_p_4	+= rhs.emp_p_4;

		emp_q_0	+= rhs.emp_q_0;
		emp_q_1	+= rhs.emp_q_1;
		emp_q_2	+= rhs.emp_q_2;
		emp_q_3	+= rhs.emp_q_3;
		emp_q_4	+= rhs.emp_q_4;

		return *this;
	}
};

struct CommonKalmans
{
	KalmanModel			pos;
	KalmanModel			pos_rate;
	KalmanModel			orbit;
	KalmanModel			clk;
	KalmanModel			clk_rate;
	KalmanModel			code_bias;
	KalmanModel			phase_bias;
	KalmanModel			pco;
	KalmanModel			ant_delta;

	CommonKalmans& operator+=(
		const CommonKalmans& rhs)
	{
		pos				+= rhs.pos;
		pos_rate		+= rhs.pos_rate;
		orbit			+= rhs.orbit;
		clk				+= rhs.clk;
		clk_rate		+= rhs.clk_rate;
		code_bias		+= rhs.code_bias;
		phase_bias		+= rhs.phase_bias;
		pco				+= rhs.pco;
		ant_delta		+= rhs.ant_delta;

		return *this;
	}
};

struct SatelliteKalmans : CommonKalmans, InertialKalmans, EmpKalmans
{
	//nothing here

	SatelliteKalmans& operator+=(
		const SatelliteKalmans& rhs)
	{
		CommonKalmans	::operator+=(rhs);
		InertialKalmans	::operator+=(rhs);
		EmpKalmans		::operator+=(rhs);

		return *this;
	}
};

struct ReceiverKalmans : CommonKalmans, InertialKalmans, EmpKalmans
{
	KalmanModel			ambiguity;
	KalmanModel			strain_rate;
	KalmanModel			slr_range_bias;
	KalmanModel			slr_time_bias;
	KalmanModel			pcv;
	KalmanModel			ion_stec;
	KalmanModel			ion_model;
	KalmanModel			trop;
	KalmanModel			trop_grads;
	KalmanModel			trop_maps;

	ReceiverKalmans& operator+=(
		const ReceiverKalmans& rhs)
	{
		CommonKalmans	::operator+=(rhs);
		InertialKalmans	::operator+=(rhs);
		EmpKalmans		::operator+=(rhs);

		ambiguity		+= rhs.ambiguity;
		strain_rate		+= rhs.strain_rate;
		slr_range_bias	+= rhs.slr_range_bias;
		slr_time_bias	+= rhs.slr_time_bias;
		pcv				+= rhs.pcv;
		ion_stec		+= rhs.ion_stec;
		ion_model		+= rhs.ion_model;
		trop			+= rhs.trop;
		trop_grads		+= rhs.trop_grads;
		trop_maps		+= rhs.trop_maps;

		return *this;
	}
};












struct CommonOptions
{
	bool					exclude						= false;
	double					pseudo_sigma				= 100000;
	double					laser_sigma					= 0.5;

	vector<E_ObsCode>		clock_codes					= {};
	vector<double>			apriori_sigma_enu			= {};
	double					mincon_scale_apriori_sigma	= 1;
	double					mincon_scale_filter_sigma	= 0;

	Vector3d				antenna_boresight			= { 0,  0, +1};
	Vector3d				antenna_azimuth				= { 0, +1,  0};

	double					ellipse_propagation_time_tolerance	= 30;

	struct
	{
		bool				enable			= true;
		vector<E_Source>	sources			= {E_Source::KALMAN, E_Source::CONFIG, E_Source::PRECISE, E_Source::SPP, E_Source::BROADCAST};
	} posModel;

	struct
	{
		bool				enable			= true;
		vector<E_Source>	sources			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	} clockModel;

	struct
	{
		bool				enable			= true;
		vector<E_Source>	sources			= {E_Source::PRECISE, E_Source::MODEL, E_Source::NOMINAL};
		double				model_dt		= 1;
	} attitudeModel;

	struct
	{
		bool				enable			= true;
		double				default_bias	= 0;
		double				undefined_sigma	= 0;
	} codeBiasModel;

	struct
	{
		bool				enable			= false;
		double				default_bias	= 0;
		double				undefined_sigma	= 0;
	} phaseBiasModel;

	struct
	{
		bool 				enable			= true;
	} pcoModel;

	struct
	{
		bool 				enable			= true;
	} pcvModel;

	struct
	{
		bool 				enable			= true;
	} phaseWindupModel;

	CommonOptions& operator+=(
		const CommonOptions& rhs);

	map<int, bool>	initialisedMap;
};


struct PropagationOptions
{
	int		egm_degree					= 12;
	double	integrator_time_step		= 60;
	bool	egm_field					= true;
	bool	solid_earth_tide			= true;
	bool	pole_tide_ocean				= true;
	bool	pole_tide_solid				= true;
	bool	ocean_tide					= true;
	bool	indirect_J2					= true;
	bool	aod							= false;
	bool	atm_tide					= false;
	bool	central_force				= true;
	bool	general_relativity			= true;
};

struct SurfaceDetails
{
	vector<double>	normal					= {0,0,0};
	vector<double>	rotation_axis			= {}; // Optional field
	double			shape					= 0;
	double			area					= 0;
	double			reflection_visible		= 0;
	double			diffusion_visible		= 0;
	double			absorption_visible		= 0;
	double			thermal_reemission		= 0;
	double 			reflection_infrared		= 0;
	double 			diffusion_infrared		= 0;
	double 			absorption_infrared		= 0;
};

/** Options associated with orbital force models
*/
struct OrbitOptions
{
	double					mass		= 1000;
	double					area		= 20;
	double					power		= 20;
	double					srp_cr		= 1.25;

	vector<E_ThirdBody>		planetary_perturbations			= {E_ThirdBody::SUN, E_ThirdBody::MOON, E_ThirdBody::JUPITER };
	bool					empirical						= true;
	bool					antenna_thrust					= true;
	E_SRPModel				albedo							= E_SRPModel::NONE;
	E_SRPModel				solar_radiation_pressure		= E_SRPModel::NONE;

	vector<bool>			empirical_dyb_eclipse			= {true};
	vector<bool> 			empirical_rtn_eclipse			= {false};

	vector<SurfaceDetails>	surface_details;

	struct
	{
		bool	enable			= false;
		int		interval		= 1;

		double	pos_proc_noise	= 10;
		double	vel_proc_noise	= 5;
	} pseudoPulses;

	OrbitOptions& operator+=(
		const OrbitOptions& rhs);

	map<int, bool>	initialisedMap;
};







/** Options to be applied to kalman filter states for individual satellites
*/
struct SatelliteOptions : SatelliteKalmans, CommonOptions, OrbitOptions
{
	bool				_initialised	= false;
	string				id;

	E_NoiseModel			error_model			= E_NoiseModel::UNIFORM;
	double					code_sigma			= 0;
	double					phase_sigma			= 0;

	SatelliteOptions& operator+=(
		const SatelliteOptions& rhs);

	void uninitialiseInheritors();

	map<string, SatelliteOptions*>		inheritors;
	map<string, SatelliteOptions*>		inheritedFrom;

	map<int, bool>	initialisedMap;
};

/** Options to be applied to kalman filter states for individual receivers
*/
struct ReceiverOptions : ReceiverKalmans, CommonOptions
{
	bool				_initialised	= false;
	string				id;

	Rinex23Conversion	rinex23Conv;

	bool				kill						= false;
	vector<E_ObsCode>	zero_dcb_codes				= {};
	Vector3d			apriori_pos					= Vector3d::Zero();
	string				antenna_type				;
	string				receiver_type				;
	string				sat_id						;
	double				elevation_mask_deg			= 10;
	E_Sys				receiver_reference_system	= E_Sys::NONE;

	struct
	{
		bool				enable			= true;
		Vector3d			eccentricity	= Vector3d::Zero();
	} eccentricityModel;

	struct
	{
		bool				enable			= true;
		vector<E_TropModel>	models			= {E_TropModel::VMF3, E_TropModel::GPT2, E_TropModel::STANDARD};
	} tropModel;

	struct
	{
		bool	enable		= true;
		bool	solid		= true;
		bool	otl			= true;
		bool	atl			= true;
		bool	spole		= true;
		bool	opole		= true;
	} tideModels;

	bool range					= true;
	bool relativity				= true;
	bool relativity2			= true;
	bool sagnac					= true;
	bool integer_ambiguity		= true;
	bool ionospheric_component	= true;
	bool ionospheric_component2	= false;
	bool ionospheric_component3	= false;
	bool ionospheric_model		= false;
	bool tropospheric_map		= false;
	bool eop					= false;

	E_IonoMapFn		mapping_function				= E_IonoMapFn::MSLM;
	double			geomagnetic_field_height		= 450;
	double			mapping_function_layer_height	= 506.7;
	double			iono_sigma_limit				= 1000;

	E_NoiseModel			error_model			= E_NoiseModel::ELEVATION_DEPENDENT;
	double					code_sigma			= 1;
	double					phase_sigma			= 0.0015;

	ReceiverOptions& operator+=(
		const ReceiverOptions& rhs);

	map<string, ReceiverOptions*>	inheritors;
	map<string, ReceiverOptions*>	inheritedFrom;

	map<int, bool>	initialisedMap;
};











/** Options associated with the minimum constraints mode of operation
*/
struct MinimumConstraintOptions : FilterOptions
{
	KalmanModel		delay;
	KalmanModel		scale;
	KalmanModel		rotation;
	KalmanModel		translation;

	bool			once_per_epoch			= false;
	bool			full_vcv				= false;
	bool			constrain_orbits		= true;
	bool			transform_unweighted	= true;

	E_Mincon		application_mode	= E_Mincon::COVARIANCE_INVERSE;
};

struct MongoInstanceOptions
{
	string	uri			= "mongodb://localhost:27017";
	string	suffix;
	string	database	= "<CONFIG>";
};

struct MongoOptions : array<MongoInstanceOptions, 3>
{
	E_Mongo	enable					= E_Mongo::NONE;
	E_Mongo	output_measurements		= E_Mongo::NONE;
	E_Mongo	output_components		= E_Mongo::NONE;
	E_Mongo	output_cumulative		= E_Mongo::NONE;
	E_Mongo	output_states			= E_Mongo::NONE;
	E_Mongo	output_state_covars		= E_Mongo::NONE;
	E_Mongo	output_trace			= E_Mongo::NONE;
	E_Mongo	output_config			= E_Mongo::NONE;
	E_Mongo	output_test_stats		= E_Mongo::NONE;
	E_Mongo	output_logs				= E_Mongo::NONE;
	E_Mongo	output_ssr_precursors	= E_Mongo::NONE;
	E_Mongo	delete_history			= E_Mongo::NONE;
	E_Mongo	cull_history			= E_Mongo::NONE;
	E_Mongo	use_predictions			= E_Mongo::NONE;
	E_Mongo	output_predictions		= E_Mongo::NONE;

	bool	queue_outputs					= false;

	vector<KF>	used_predictions	= {KF::ORBIT, KF::REC_POS, KF::SAT_CLOCK, KF::CODE_BIAS, KF::PHASE_BIAS, KF::EOP, KF::EOP_RATE};
	vector<KF>	sent_predictions	= {KF::ORBIT, KF::REC_POS, KF::SAT_CLOCK, KF::CODE_BIAS, KF::PHASE_BIAS, KF::EOP, KF::EOP_RATE};

	double	prediction_offset				= 0;
	double	prediction_interval				= 30;
	double	forward_prediction_duration		= 300;
	double	reverse_prediction_duration		= -1;

	double	min_cull_age					= 300;
};

/** Options associated with SSR corrections and exporting RTCM messages
*/
struct SsrOptions
{
	bool				extrapolate_corrections	= false;
	double				prediction_interval		= 30;
	double				prediction_duration		= 0;
	vector<E_Source>	ephemeris_sources		= {E_Source::PRECISE};
	vector<E_Source>	clock_sources			= {E_Source::KALMAN};
	vector<E_Source>	code_bias_sources		= {E_Source::PRECISE};
	vector<E_Source>	phase_bias_sources		= {E_Source::NONE};
	vector<E_Source>	atmosphere_sources		= {E_Source::NONE};
	bool cmpssr_cell_mask						= false;
	int  cmpssr_stec_format						= 3;
	int  cmpssr_trop_format						= 1;
	double				max_stec_sigma			= 1.0;

	int				region_id		= -1;
	int				region_iod		= -1;
	int 			npoly_trop		= -1;
	int 			npoly_iono		= -1;
	int 			grid_type		= -1;
	bool			use_grid_iono	= true;
	bool			use_grid_trop	= true;
	double			lat_max			= 0;
	double			lat_min			= 0;
	double			lat_int			= 0;
	double			lon_max			= 0;
	double			lon_min			= 0;
	double			lon_int			= 0;

	int				ngrid			= 0;	//not configs?
	int				nbasis			= 0;	//not configs?
};

struct SsrInOptions
{
	double			code_bias_valid_time	= 3600;		///< Valid time period of SSR code biases
	double			phase_bias_valid_time	= 300;		///< Valid time period of SSR phase biases
	double			global_vtec_valid_time	= 300;		///< Valid time period of SSR global Ionospheres
	double			local_stec_valid_time	= 120;		///< Valid time period of SSR local Ionospheres
	double			local_trop_valid_time	= 120;		///< Valid time period of SSR local Tropospheres
	bool			one_freq_phase_bias		= false;
};

struct SbsInOptions
{
	string	host;		///< hostname is passed as acsConfig.sisnet_inputs
	string 	port;		///< port of SISNet steam
	string	user;		///< Username for SISnet stream access
	string	pass;		///< Password for SISnet stream access
	int 	prn;		///< prn of SBAS satellite
	int		freq;		///< freq (L1 or L5) of SBAS channel
};

struct SSRMetaOpts
{
	bool	itrf_datum			= true;
	int		provider_id			= 0;
	int		solution_id			= 0;
};

struct RtcmMsgTypeOpts
{
	int		udi		= 0;		///< Update interval (0 = don't upload message)

	map<CompactSSRSubtype,	int>	comp_udi;
	map<IgsSSRSubtype,		int>	igs_udi;

	// space for multiple UDI's here
};

struct SsrBroadcast : SSRMetaOpts
{
	string						url;

	map<RtcmMessageType, RtcmMsgTypeOpts>	rtcmMsgOptsMap;	///< RTCM message type options
};

struct NetworkOptions
{
	map<string, SsrBroadcast>	uploadingStreamData;
};




struct YamlDefault
{
	string	defaultValue;
	string	comment;
	string	typeName;
	bool	found;
	string	foundValue;
	string	enumName;
	int		configLevel;	//yet unused
};

/** General options object to be used throughout the software
*/
struct ACSConfig : GlobalOptions, InputOptions, OutputOptions, DebugOptions
{
	vector<YAML::Node>				yamls;
	map<string, YamlDefault>		yamlDefaults;
	map<string, bool>				availableOptions	=	{
																{"yaml_filename:",	true},
																{"yaml_number:",	true},
															};
	map<string, map<string, bool>>	foundOptions;

	mutex							configMutex;


	map<string, set<string>>		customAliasesMap;

	vector<string>										configFilenames;
	vector<string>										includedFilenames;
	map<string, std::filesystem::file_time_type>		configModifyTimeMap;
	boost::program_options::variables_map				commandOpts;

	static map<string, string>			docs;

	void	recurseYaml(
		const string&		file,
		YAML::Node			node,
		const string&		stack		= "",
		const string&		aliasStack	= "");

	bool	parse(
		const vector<string>&					filenames,
		boost::program_options::variables_map&	vm);

	bool	parse();

	void	info(
		Trace& trace);

	void	sanityChecks();

	void	outputDefaultConfiguration(
		int level);


	SatelliteOptions&			getSatOpts(SatSys	Sat,	const vector<string>& suffixes = {});
	ReceiverOptions&			getRecOpts(string	id,		const vector<string>& suffixes = {});

	unordered_map<string,		SatelliteOptions>	satOptsMap;
	unordered_map<string,		ReceiverOptions>	recOptsMap;

	PropagationOptions			propagationOptions;
	PreprocOptions				preprocOpts;
	MinimumConstraintOptions	minconOpts;
	SsrInOptions				ssrInOpts;
	SbsInOptions				sbsInOpts;
	AmbROptions					ambrOpts;
	SsrOptions					ssrOpts;
	PppOptions					pppOpts;
	SppOptions					sppOpts;
	SlrOptions					slrOpts;
	ExcludeOptions				exclude;

	StateErrorHandler			stateErrors;
	MeasErrorHandler			measErrors;
	AmbiguityErrorHandler		ambErrors;
	OrbitErrorHandler			orbErrors;
	IonoErrorHandler			ionErrors;
	ErrorAccumulationHandler	errorAccumulation;

	IonModelOptions				ionModelOpts;
	MongoOptions				mongoOpts;
	NetworkOptions				netOpts;
};

bool replaceString(
	string&	str,
	string	subStr,
	string	replacement,
	bool	warn = true);

bool configure(
	int		argc,
	char**	argv);

void dumpConfig(
	Trace& trace);

extern ACSConfig acsConfig;		///< Global variable housing all options to be used throughout the software

