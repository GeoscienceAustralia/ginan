
#pragma once

#include <boost/filesystem.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/program_options.hpp>

#include <yaml-cpp/yaml.h>

#include "eigenIncluder.hpp"

#include <chrono>
#include <memory>
#include <limits>
#include <vector>
#include <mutex>
#include <tuple>
#include <array>
#include <map>
#include <set>

using std::vector;
using std::string;
using std::tuple;
using std::mutex;
using std::array;
using std::map;
using std::set;

#include "instrument.hpp"
#include "satSys.hpp"
#include "trace.hpp"
#include "enums.h"


#define PI          3.141592653589793238462643383279502884197169399375105820974
#define D2R         (PI/180.0)          ///< deg to rad
#define R2D         (180.0/PI)          ///< rad to deg

template<typename BASE, typename COMP>
void setInited(
	BASE&	base,
	COMP&	comp,
	bool	init = true)
{
	if (init == false)
	{
		return;
	}

	int offset = (char*)(&comp) - (char*)(&base);

	base.initialisedMap[offset] = true;
}

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
	string			gnss_obs_root	= "<INPUTS_ROOT>";
	string			pseudo_obs_root	= "<INPUTS_ROOT>";
	string			sat_data_root	= "<INPUTS_ROOT>";

	vector<string>	atx_files;
	vector<string>	snx_files;
	vector<string>	otl_blq_files;
	vector<string>	atl_blq_files;
	vector<string>	opole_files;
                       
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

	vector<string>	nav_rtcm_inputs;
	vector<string>	qzs_rtcm_inputs;

	map<string, vector<string>>	rnx_inputs;
	map<string, vector<string>>	ubx_inputs;
	map<string, vector<string>>	custom_inputs;
	map<string, vector<string>>	obs_rtcm_inputs;
	map<string, vector<string>>	pseudo_sp3_inputs;
	map<string, vector<string>>	pseudo_snx_inputs;

	vector<string> atm_reg_definitions;

	vector<string> egm_files;
	vector<string> boxwing_files;
	vector<string> jpl_files;
	vector<string> ocetide_files;
	vector<string> atmtide_files;
	vector<string> cmc_files;
	vector<string> hfeop_files;
	vector<string> aod1b_files;
	vector<string> poleocean_files;

	string stream_user			= "";
	string stream_pass			= "";
};

struct IonexOptions
{
	double			lat_center	= 0;
	double			lon_center	= 0;
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
	bool	output_station_trace		= false;
	bool	output_network_trace		= false;
	bool	output_ionosphere_trace		= false;
	bool	output_satellite_trace		= false;
	bool	output_json_trace			= false;
	string	trace_directory				= "<OUTPUTS_ROOT>";
	string	station_trace_filename		= "<TRACE_DIRECTORY>/<STATION>-<LOGTIME>.trace";
	string	network_trace_filename		= "<TRACE_DIRECTORY>/<STATION>-<LOGTIME>.trace";
	string	ionosphere_trace_filename	= "<TRACE_DIRECTORY>/<STATION>-<LOGTIME>.trace";
	string	satellite_trace_filename	= "<TRACE_DIRECTORY>/<STATION>-<LOGTIME>.trace";

	bool	record_raw_ubx				= false;
	string	raw_ubx_directory			= "<OUTPUTS_ROOT>";
	string	raw_ubx_filename			= "<UBX_DIRECTORY>/<STATION>-<LOGTIME>-OBS.rtcm";

	bool	record_raw_custom			= false;
	string	raw_custom_directory		= "<OUTPUTS_ROOT>";
	string	raw_custom_filename			= "<CUSTOM_DIRECTORY>/<STATION>-<LOGTIME>-OBS.custom";

	bool	record_rtcm_obs				= false;
	bool	record_rtcm_nav				= false;
	string	rtcm_obs_directory			= "<OUTPUTS_ROOT>";
	string	rtcm_nav_directory			= "<OUTPUTS_ROOT>";
	string  rtcm_obs_filename			= "<RTCM_OBS_DIRECTORY>/<STATION>-<LOGTIME>-OBS.rtcm";
	string  rtcm_nav_filename			= "<RTCM_NAV_DIRECTORY>/<STREAM>-<LOGTIME>-NAV.rtcm";

	bool	output_log					= false;
	string	log_directory				= "<OUTPUTS_ROOT>";
	string  log_filename				= "<LOG_DIRECTORY>/log-<LOGTIME>.json";

	bool	output_ntrip_log			= false;
	string	ntrip_log_directory			= "<OUTPUTS_ROOT>";
	string  ntrip_log_filename			= "<NTRIP_LOG_DIRECTORY>/ntrip_log-<LOGTIME>.json";

	bool	output_gpx					= false;
	string	gpx_directory				= "<OUTPUTS_ROOT>";
	string  gpx_filename				= "<GPX_DIRECTORY>/<STATION>-<LOGTIME>.gpx";

	bool	output_residuals 			= false;
	bool	output_residual_chain		= true;

	bool	output_config				= false;
	bool	colorize_terminal			= true;
	bool	warn_once					= true;

	bool				output_clocks 				= false;
	vector<E_Source>	clocks_receiver_sources  	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>	clocks_satellite_sources 	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	string				clocks_directory			= "<OUTPUTS_ROOT>";
	string				clocks_filename				= "<CLOCKS_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>.clk";

	bool				output_sp3 					= false;
	bool				output_predicted_orbits 	= false;
	bool				output_inertial_orbits		= false;
	bool				output_sp3_velocities		= false;
	vector<E_Source>	sp3_orbit_sources 			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>	sp3_clock_sources 			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	int					sp3_output_interval			= 900;
	string				sp3_directory				= "<OUTPUTS_ROOT>";
	string				sp3_filename				= "<SP3_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>-Filt.sp3";
	string				predicted_sp3_filename		= "<SP3_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>-Prop.sp3";

	bool				output_orbex 				= false;
	vector<E_Source>	orbex_orbit_sources			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>	orbex_clock_sources			= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	vector<E_Source>	orbex_attitude_sources 		= {E_Source::NOMINAL};
	string				orbex_directory				= "<OUTPUTS_ROOT>";
	string				orbex_filename				= "<ORBEX_DIRECTORY>/<CONFIG>-<LOGTIME>_<SYS>.obx";
	vector<string>		orbex_record_types			= { "ATT" };

	bool	split_sys					= false;

	bool	output_rinex_obs			= false;
	string	rinex_obs_directory			= "<OUTPUTS_ROOT>";
	string	rinex_obs_filename			= "<RINEX_OBS_DIRECTORY>/<STATION>-<LOGTIME>_<SYS>.<YY>O";
	double	rinex_obs_version			= 3.05;
	bool	rinex_obs_print_C_code		= true;
	bool	rinex_obs_print_L_code		= true;
	bool	rinex_obs_print_D_code	 	= true;
	bool	rinex_obs_print_S_code 		= true;

	bool	output_ppp_sol 				= false;
	string	ppp_sol_directory			= "<OUTPUTS_ROOT>";
	string	ppp_sol_filename			= "<PPP_SOL_DIRECTORY>/<STATION><YYYY><DDD><HH>.pppsol";

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
	string				cost_filename		= "<COST_DIRECTORY>/cost_s_t_<LOGTIME>_<STATION>_ga__.dat";
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
	string 	slr_obs_filename			= "<SLR_OBS_DIRECTORY>/<STATION>.slr_obs";

	bool	output_orbit_ics			= false;
	string	orbit_ics_directory			= "<OUTPUTS_ROOT>";
	string	orbit_ics_filename			= "<ORBIT_ICS_DIRECTORY>/<CONFIG>-<LOGTIME>-orbits.yaml";


	bool	enable_binary_store			= false;
	bool	store_binary_states			= false;
	bool	store_binary_measurements	= false;

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
	int		csfreq = 3;         /* cycle slip detection and repair frequency */

	bool	instrument					= false;
	bool    instrument_once_per_epoch	= false;
	bool	mincon_only					= false;
	bool	check_plumbing				= false;
	bool	retain_rts_files			= false;
	bool	rts_only					= false;
	bool	explain_measurements		= false;
};

/** Options for processing SLR observations
*/
struct SlrOptions
{
	bool	process_slr					= false;
};


struct ModelTides
{
	bool	enable		= true;
	bool	solid		= true;
	bool	otl			= true;
	bool	atl			= true;
	bool	spole		= true;
	bool	opole		= true;

	vector<E_TidalComponent>	otl_blq_row_order	= {E_TidalComponent::UP, E_TidalComponent::WEST, E_TidalComponent::SOUTH};
	vector<E_TidalComponent>	atl_blq_row_order	= {E_TidalComponent::UP, E_TidalComponent::NORTH, E_TidalComponent::EAST};
};


/** Options to set the tropospheric model used in calculations
*/
struct ModelTrop
{
	E_TropModel		model		= E_TropModel::STANDARD;
	bool			enable		= true;
	string			orography;
	string			gpt2grid;
};

struct Model
{
	ModelTrop	trop;
	ModelTides	tides;

	bool range					= true;

	bool relativity				= true;
	bool relativity2			= true;
	bool sagnac					= true;
	bool phase_windup			= true;
	bool heading				= true;
	bool integer_ambiguity		= true;
	bool ionospheric_component	= true;
	bool ionospheric_component2	= false;
	bool ionospheric_component3	= false;
	bool eop					= false;
	bool ionospheric_model		= false;
	bool tropospheric_map		= false;
};

/** Options for the general operation of the software
*/
struct GlobalOptions
{
	Model	model;

	int		sleep_milliseconds	= 50;
	double	epoch_interval		= 1;
	double	epoch_tolerance		= 0.5;
	int		max_epochs			= 0;
	int		leap_seconds		= -1;

	boost::posix_time::ptime start_epoch	{ boost::posix_time::not_a_date_time };
	boost::posix_time::ptime end_epoch		{ boost::posix_time::not_a_date_time };

	string	config_description				= "Pea";
	string	analysis_agency					= "GAA";
	string	analysis_center					= "Geoscience Australia";
	string	analysis_program				= "Ginan";
	string	analysis_program_version		= "2.0";
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

	double	elevation_mask	= 10 * D2R;

	string	pivot_station	= "NO_PIVOT";
	string	pivot_satellite	= "NO_PIVOT";

	bool	raim						= true;
	bool	interpolate_rec_pco			= true;
	bool	auto_fill_pco				= true;
	bool	require_apriori_positions	= false;
	bool	require_antenna_details		= false;

	double	thres_slip   	= 0.05;
	double	mw_proc_noise	= 0;
	double	max_gdop     	= 30;
	double	deweight_factor	= 100;

	bool	use_tgd_bias	= false;


	bool	preprocess_all_data			= true;

	double	wait_next_epoch				= 0;
	double	wait_all_stations			= 0;
	bool	require_obs					= true;
	bool	assign_closest_epoch		= false;

	bool	delete_old_ephemerides  	= true;

	bool	reinit_on_all_slips			= false;
	bool	reinit_on_clock_error		= false;
	bool	reject_on_state_error		= false;

	bool	joseph_stabilisation		= false;
	double	validity_interval_factor	= 10;

	E_OffsetType ssr_input_antenna_offset = E_OffsetType::UNSPECIFIED;

	map<E_Sys, vector<E_ObsCode>>	code_priorities;
	map<E_Sys, vector<E_ObsCode>>	zero_code_average;
	map<E_Sys, vector<E_ObsCode>>	zero_phase_average;
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

	map<E_Sys, E_ObsCode> clock_codesL1 =	///< Default observation codes on first frequency for IF combination based satellite clocks
	{
		{E_Sys::GPS, E_ObsCode::L1W},
		{E_Sys::GLO, E_ObsCode::L1P},
		{E_Sys::GAL, E_ObsCode::L1C},
		{E_Sys::BDS, E_ObsCode::L2I},
		{E_Sys::QZS, E_ObsCode::L1C},
		{E_Sys::SBS, E_ObsCode::L1C}
	};

	map<E_Sys, E_ObsCode> clock_codesL2 =	///< Default observation codes on second frequency for IF combination based satellite clocks
	{
		{E_Sys::GPS, E_ObsCode::L2W},
		{E_Sys::GLO, E_ObsCode::L2P},
		{E_Sys::GAL, E_ObsCode::L5Q},
		{E_Sys::BDS, E_ObsCode::L6I},
		{E_Sys::QZS, E_ObsCode::L2L},
		{E_Sys::SBS, E_ObsCode::L5I}
	};

	E_Sys  receiver_reference_clk = E_Sys::NONE;
	double fixed_phase_bias_var   = 0.01;

	bool	sat_clk_definition			= false;
	bool	minimise_sat_clock_offsets	= false;

	map<E_Sys, bool> zero_receiver_dcb;			///< set receiver DCBs to 0
	map<E_Sys, bool> zero_satellite_dcb;		///< set satellite DCBs to 0
	map<E_Sys, bool> one_phase_bias;			///< use common phase bias for frequency
	map<E_Sys, bool> receiver_amb_pivot;		///< fix one ambiguity to eliminate rank deficiency
	map<E_Sys, bool> network_amb_pivot;			///< fix ambiguities to eliminate network rank deficiencies
	map<E_Sys, bool> use_for_iono_model;		///< use system for ionospheric modelling
	map<E_Sys, bool> use_iono_corrections;		///< use system for ionospheric modelling

	bool common_sat_pco			= false;
	bool common_rec_pco			= false;
	bool use_trop_corrections	= false;

	double clock_wrap_threshold = 0.05e-3;

	//to be removed?

	double			predefined_fail		= 0.001;	/* pre-defined fail-rate (0.01,0.001) */
};

/** Options associated with kalman filter states
*/
struct KalmanModel
{
	vector<double>	sigma				= {0};	//{0} is very necessary
	vector<double>	apriori_val			= {0};
	vector<double>	proc_noise			= {0};
	vector<double>	tau					= {-1};	//tau<0 (inf): Random Walk model; tau>0: First Order Gauss Markov model
	vector<double>	mu					= {0};
	vector<bool>	estimate			= {false};
	vector<string>	comment				= {""};

	KalmanModel& operator+=(
		const KalmanModel& rhs)
	{
		if (isInited(rhs, rhs.sigma			))	{ sigma			= rhs.sigma			;	setInited(*this, sigma		);	}
		if (isInited(rhs, rhs.apriori_val	))	{ apriori_val	= rhs.apriori_val	;	setInited(*this, apriori_val);	}
		if (isInited(rhs, rhs.proc_noise	))	{ proc_noise	= rhs.proc_noise	;	setInited(*this, proc_noise	);	}
		if (isInited(rhs, rhs.tau			))	{ tau			= rhs.tau			;	setInited(*this, tau		);	}
		if (isInited(rhs, rhs.mu			))	{ mu			= rhs.mu			;	setInited(*this, mu			);	}
		if (isInited(rhs, rhs.estimate		))	{ estimate		= rhs.estimate		;	setInited(*this, estimate	);	}
		if (isInited(rhs, rhs.comment		))	{ comment		= rhs.comment		;	setInited(*this, comment	);	}

		return *this;
	}

	map<int, bool>	initialisedMap;
};

struct FilterOptions
{
	int			phase_reject_limit		= 10;
	int			outage_reset_limit		= 10;

	bool		sigma_check				= true;
	bool		w_test					= false;
	bool		chi_square_test			= false;
	bool		simulate_filter_only	= false;
	E_ChiSqMode	chi_square_mode			= E_ChiSqMode::NONE;
	double		sigma_threshold			= 4;

	bool		assume_linearity		= false;

	int			max_filter_iter 		= 2;
	int			max_prefit_remv 		= 2;

	string		rts_filename			= "Filter-<CONFIG>-<STATION>.rts";
	string		rts_directory			= "<OUTPUTS_ROOT>";
	int			rts_lag					= -1;
	string		rts_smoothed_suffix		= "_smoothed";
	bool		output_intermediate_rts	= false;
	bool		queue_rts_outputs		= false;

	E_Inverter	rts_inverter			= E_Inverter::LDLT;
	E_Inverter	inverter				= E_Inverter::LDLT;
};

/** Options associated with the ppp processing mode of operation
*/
struct PPPOptions : FilterOptions
{
	KalmanModel		eop;
	KalmanModel		eop_rates;

	bool			common_atmosphere	= false;
	bool			use_rtk_combo		= false;

	bool			satellite_chunking	= false;
	bool			station_chunking	= false;
	int				chunk_size			= 0;
};

struct SPPOptions
{
	bool	always_reinitialize	= true;
	int		max_lsq_iterations	= 12;
	bool	sigma_check			= true;
	int		sigma_threshold		= 4;
	int		max_removals		= 2;
};

/** Options associated with the ionospheric modelling processing mode of operation
*/
struct IonosphericOptions
{
	E_IonoMode 		corr_mode  			= E_IonoMode::IONO_FREE_LINEAR_COMBO;
	E_IonoMapFn		mapping_function	= E_IonoMapFn::MSLM;

	double			pierce_point_layer_height		= 450;
	double			mapping_function_layer_height	= 506.7;
	bool			common_ionosphere				= true;
	bool			use_if_combo					= false;
	bool			use_gf_combo					= false;

	double			iono_sigma_limit				= 1000;
};

struct SlipOptions
{
	bool LLI	= true;
	bool GF		= true;
	bool MW		= true;
	bool SCDIA	= true;
};

struct IonModelOptions
{
	E_IonoModel		model			= E_IonoModel::NONE;
	int				numBasis;		///< not a directly configurable parameter
	int				function_order;
	int				function_degree;

	vector<double>	layer_heights;
	bool			estimate_sat_dcb  = true;
	bool			use_rotation_mtx  = false;
	double			basis_sigma_limit = 1000;

	KalmanModel	ion;
};

struct AmbROptions
{
	E_ARmode	mode			= E_ARmode::OFF;
	int			lambda_set		= 2;
	int			AR_max_itr		= 1;
	double		min_el_AR		= 15;

	double	succsThres = 0.9999;	///< Thresholds for ambiguity validation: succsess rate NL
	double	ratioThres = 3;		///< Thresholds for ambiguity validation: succsess rate NL

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
	map<E_Sys, map<E_ObsCode2, E_ObsCode>> codeConv;
	map<E_Sys, map<E_ObsCode2, E_ObsCode>> phasConv;

	Rinex23Conversion& operator +=(const Rinex23Conversion& rhs)
	{
		for (auto& [sys, map23]		: rhs.codeConv)
		for (auto& [code2, code3]	: map23)
		{
			if (code3 != +E_ObsCode::NONE)
				codeConv[sys][code2] = code3;
		}

		for (auto& [sys, map23]		: rhs.phasConv)
		for (auto& [code2, code3]	: map23)
		{
			if (code3 != +E_ObsCode::NONE)
				phasConv[sys][code2] = code3;
		}

		return *this;
	}
};

struct SrpOptions
{
	double			mass		= 1000;
	double			area		= 20;
	double			power		= 20;
	double			srp_cr		= 1.25;

	SrpOptions& operator+=(
		const SrpOptions& rhs)
	{
		if (isInited(rhs, rhs.mass				))	{ mass			= rhs.mass			;	setInited(*this, mass			);	}
		if (isInited(rhs, rhs.area				))	{ area			= rhs.area			;	setInited(*this, area			);	}
		if (isInited(rhs, rhs.power				))	{ power			= rhs.power			;	setInited(*this, power			);	}
		if (isInited(rhs, rhs.srp_cr			))	{ srp_cr		= rhs.srp_cr		;	setInited(*this, srp_cr			);	}

		return *this;
	}

	map<int, bool>	initialisedMap;
};

struct EmpOptions
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

	EmpOptions& operator+=(
		const EmpOptions& rhs)
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

/** Options to be applied to kalman filter states for individual satellites
*/
struct SatelliteOptions : EmpOptions, SrpOptions
{
	bool				_initialised	= false;
	string				id;

	KalmanModel			clk;
	KalmanModel			clk_rate;
	KalmanModel			orbit;
	KalmanModel			pos;
	KalmanModel			pos_rate;
	KalmanModel			pco;
	KalmanModel			ant;
	KalmanModel			code_bias;
	KalmanModel			phase_bias;

	bool				exclude				= false;
	vector<double>		code_sigmas			= {0};
	vector<double>		phase_sigmas		= {0};
	vector<double>		pseudo_sigmas		= {100000};
	vector<double>		laser_sigmas		= {0};
	vector<double>		minConNoise			= {-1};

	Vector3d			antenna_boresight	= { 0,  0, +1};
	Vector3d			antenna_azimuth		= { 0, +1,  0};

	struct
	{
		bool				enable				= true;
		vector<E_Source>	ephemeris_sources	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	} sat_pos;

	struct
	{
		bool				enable				= true;
		vector<E_Source>	ephemeris_sources	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	} sat_clock;

	struct
	{
		bool				enable				= true;
		vector<E_Source>	sources				= {E_Source::PRECISE, E_Source::MODEL, E_Source::NOMINAL};
		double				model_dt			= 1;
	} sat_attitude;

	struct
	{
		bool 	enable			= true;
		double	default_bias	= 0;
		double	undefined_sigma = 0;
	} sat_code_bias;

	struct
	{
		bool	enable			= false;
		double	default_bias	= 0;
		double	undefined_sigma = 0;
	} sat_phase_bias;

	bool		sat_pco				= true;
	bool		sat_pcv				= true;

	SatelliteOptions& operator+=(
		const SatelliteOptions& rhs)
	{
		Instrument	instrument(__FUNCTION__);

		clk			+= rhs.clk;
		clk_rate	+= rhs.clk_rate;
		orbit		+= rhs.orbit;
		pos			+= rhs.pos;
		pos_rate	+= rhs.pos_rate;
		pco			+= rhs.pco;
		ant			+= rhs.ant;
		code_bias	+= rhs.code_bias;
		phase_bias	+= rhs.phase_bias;

		EmpOptions::operator+=(rhs);
		SrpOptions::operator+=(rhs);

		if (isInited(rhs, rhs.exclude				))	{ exclude			= rhs.exclude			;	setInited(*this, exclude			);	}
		if (isInited(rhs, rhs.code_sigmas			))	{ code_sigmas		= rhs.code_sigmas		;	setInited(*this, code_sigmas		);	}
		if (isInited(rhs, rhs.phase_sigmas			))	{ phase_sigmas		= rhs.phase_sigmas		;	setInited(*this, phase_sigmas		);	}
		if (isInited(rhs, rhs.pseudo_sigmas			))	{ pseudo_sigmas		= rhs.pseudo_sigmas		;	setInited(*this, pseudo_sigmas		);	}
		if (isInited(rhs, rhs.laser_sigmas			))	{ laser_sigmas		= rhs.laser_sigmas		;	setInited(*this, laser_sigmas		);	}

		if (isInited(rhs, rhs.antenna_boresight		))	{ antenna_boresight	= rhs.antenna_boresight	;	setInited(*this, antenna_boresight	);	}
		if (isInited(rhs, rhs.antenna_azimuth		))	{ antenna_azimuth	= rhs.antenna_azimuth	;	setInited(*this, antenna_azimuth	);	}


		if (isInited(rhs, rhs.sat_pos.enable				))	{ sat_pos.enable				= rhs.sat_pos.enable				;	setInited(*this, sat_pos.enable					);	}
		if (isInited(rhs, rhs.sat_pos.ephemeris_sources		))	{ sat_pos.ephemeris_sources		= rhs.sat_pos.ephemeris_sources		;	setInited(*this, sat_pos.ephemeris_sources		);	}
		if (isInited(rhs, rhs.sat_clock.enable				))	{ sat_clock.enable				= rhs.sat_clock.enable				;	setInited(*this, sat_clock.enable				);	}
		if (isInited(rhs, rhs.sat_clock.ephemeris_sources	))	{ sat_clock.ephemeris_sources	= rhs.sat_clock.ephemeris_sources	;	setInited(*this, sat_clock.ephemeris_sources	);	}
		if (isInited(rhs, rhs.sat_attitude.enable			))	{ sat_attitude.enable			= rhs.sat_attitude.enable			;	setInited(*this, sat_attitude.enable			);	}
		if (isInited(rhs, rhs.sat_attitude.sources			))	{ sat_attitude.sources			= rhs.sat_attitude.sources			;	setInited(*this, sat_attitude.sources			);	}
		if (isInited(rhs, rhs.sat_attitude.model_dt			))	{ sat_attitude.model_dt			= rhs.sat_attitude.model_dt			;	setInited(*this, sat_attitude.model_dt			);	}
		if (isInited(rhs, rhs.sat_code_bias.enable			))	{ sat_code_bias.enable			= rhs.sat_code_bias.enable			;	setInited(*this, sat_code_bias.enable			);	}
		if (isInited(rhs, rhs.sat_code_bias.default_bias	))	{ sat_code_bias.default_bias	= rhs.sat_code_bias.default_bias	;	setInited(*this, sat_code_bias.default_bias		);	}
		if (isInited(rhs, rhs.sat_code_bias.undefined_sigma	))	{ sat_code_bias.undefined_sigma	= rhs.sat_code_bias.undefined_sigma	;	setInited(*this, sat_code_bias.undefined_sigma	);	}
		if (isInited(rhs, rhs.sat_phase_bias.enable			))	{ sat_phase_bias.enable			= rhs.sat_phase_bias.enable			;	setInited(*this, sat_phase_bias.enable			);	}
		if (isInited(rhs, rhs.sat_phase_bias.default_bias	))	{ sat_phase_bias.default_bias	= rhs.sat_phase_bias.default_bias	;	setInited(*this, sat_phase_bias.default_bias	);	}
		if (isInited(rhs, rhs.sat_phase_bias.undefined_sigma))	{ sat_phase_bias.undefined_sigma= rhs.sat_phase_bias.undefined_sigma;	setInited(*this, sat_phase_bias.undefined_sigma	);	}
		if (isInited(rhs, rhs.sat_pco						))	{ sat_pco						= rhs.sat_pco						;	setInited(*this, sat_pco						);	}
		if (isInited(rhs, rhs.sat_pcv						))	{ sat_pcv						= rhs.sat_pcv						;	setInited(*this, sat_pcv						);	}

		return *this;
	}

	map<int, bool>	initialisedMap;
};

/** Options to be applied to kalman filter states for individual receivers
*/
struct ReceiverOptions : EmpOptions, SrpOptions
{
	bool				_initialised	= false;
	string				id;

	KalmanModel			amb;
	KalmanModel			pos;
	KalmanModel			pos_rate;
	KalmanModel			heading;
	KalmanModel			orbit;
	KalmanModel			strain_rate;
	KalmanModel			clk;
	KalmanModel			clk_rate;
	KalmanModel			slr_range_bias;
	KalmanModel			slr_time_bias;
	KalmanModel			dcb;
	KalmanModel			pco;
	KalmanModel			ant;
	KalmanModel			ion_stec;
	KalmanModel			ion_model;
	KalmanModel			trop;
	KalmanModel			trop_grads;
	KalmanModel			trop_maps;
	KalmanModel			code_bias;
	KalmanModel			phase_bias;

	KalmanModel			quat;
	KalmanModel			gyro_bias;
	KalmanModel			accl_bias;
	KalmanModel			gyro_scale;
	KalmanModel			accl_scale;
	KalmanModel			imu_offset;

	bool				kill				= false;
	bool				exclude				= false;
	E_NoiseModel		error_model			= E_NoiseModel::ELEVATION_DEPENDENT;
	vector<double>		code_sigmas			= {1};
	vector<double>		phase_sigmas		= {0.0015};
	vector<double>		laser_sigmas		= {0.5};
	vector<double>		minConNoise			= {-1};
	double				spp_sigma_scaling	= 1;

	Rinex23Conversion	rinex23Conv;

	Vector3d			apriori_pos			= Vector3d::Zero();
	Vector3d			eccentricity		= Vector3d::Zero();
	Vector3d			antenna_boresight	= { 0,  0, +1};
	Vector3d			antenna_azimuth		= { 0, +1,  0};
	string				antenna_type		;
	string				receiver_type		;
	string				sat_id				;




	struct
	{
		bool				enable				= true;
		vector<E_Source>	ephemeris_sources	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	} rec_pos;

	struct
	{
		bool				enable				= true;
		vector<E_Source>	ephemeris_sources	= {E_Source::KALMAN, E_Source::PRECISE, E_Source::BROADCAST};
	} rec_clock;

	struct
	{
		bool				enable				= true;
		vector<E_Source>	sources				= {E_Source::PRECISE, E_Source::MODEL, E_Source::NOMINAL};
	} rec_attitude;

	struct
	{
		bool	enable			= true;
		double	default_bias	= 0;
		double	undefined_sigma	= 0;
	} rec_code_bias;

	struct
	{
		bool	enable			= false;
		double	default_bias	= 0;
		double	undefined_sigma	= 0;
	} rec_phase_bias;

	struct
	{
		bool				enable			= true;
		vector<E_TropModel>	models			= {E_TropModel::VMF3, E_TropModel::GPT2, E_TropModel::STANDARD};
	} rec_trop;


	bool rec_ant_delta			= true;
	bool rec_pco				= true;
	bool rec_pcv				= true;

	ReceiverOptions& operator+=(
		const ReceiverOptions& rhs)
	{
		Instrument	instrument(__FUNCTION__);

		amb				+= rhs.amb;
		pos				+= rhs.pos;
		pos_rate		+= rhs.pos_rate;
		heading			+= rhs.heading;
		orbit			+= rhs.orbit;
		strain_rate		+= rhs.strain_rate;
		clk				+= rhs.clk;
		clk_rate		+= rhs.clk_rate;
		slr_range_bias	+= rhs.slr_range_bias;
		slr_time_bias	+= rhs.slr_time_bias;
		dcb				+= rhs.dcb;
		pco				+= rhs.pco;
		ant				+= rhs.ant;
		ion_stec		+= rhs.ion_stec;
		ion_model		+= rhs.ion_model;
		trop			+= rhs.trop;
		trop_grads		+= rhs.trop_grads;
		trop_maps		+= rhs.trop_maps;
		code_bias		+= rhs.code_bias;
		phase_bias		+= rhs.phase_bias;

		quat			+= rhs.quat;
		gyro_bias		+= rhs.gyro_bias;
		accl_bias		+= rhs.accl_bias;
		gyro_scale		+= rhs.gyro_scale;
		accl_scale		+= rhs.accl_scale;
		imu_offset		+= rhs.imu_offset;

		EmpOptions::operator+=(rhs);

		rinex23Conv		+= rhs.rinex23Conv;

		if (isInited(rhs, rhs.kill					))	{ kill				= rhs.kill				;	setInited(*this, kill				);	}
		if (isInited(rhs, rhs.exclude				))	{ exclude			= rhs.exclude			;	setInited(*this, exclude			);	}
		if (isInited(rhs, rhs.error_model			))	{ error_model		= rhs.error_model		;	setInited(*this, error_model		);	}
		if (isInited(rhs, rhs.code_sigmas			))	{ code_sigmas		= rhs.code_sigmas		;	setInited(*this, code_sigmas		);	}
		if (isInited(rhs, rhs.phase_sigmas			))	{ phase_sigmas		= rhs.phase_sigmas		;	setInited(*this, phase_sigmas		);	}
		if (isInited(rhs, rhs.laser_sigmas			))	{ laser_sigmas		= rhs.laser_sigmas		;	setInited(*this, laser_sigmas		);	}
		if (isInited(rhs, rhs.minConNoise			))	{ minConNoise		= rhs.minConNoise		;	setInited(*this, minConNoise		);	}
		if (isInited(rhs, rhs.spp_sigma_scaling		))	{ spp_sigma_scaling	= rhs.spp_sigma_scaling	;	setInited(*this, spp_sigma_scaling	);	}

		if (isInited(rhs, rhs.apriori_pos			))	{ apriori_pos		= rhs.apriori_pos		;	setInited(*this, apriori_pos		);	}
		if (isInited(rhs, rhs.eccentricity			))	{ eccentricity		= rhs.eccentricity		;	setInited(*this, eccentricity		);	}
		if (isInited(rhs, rhs.antenna_boresight		))	{ antenna_boresight	= rhs.antenna_boresight	;	setInited(*this, antenna_boresight	);	}
		if (isInited(rhs, rhs.antenna_azimuth		))	{ antenna_azimuth	= rhs.antenna_azimuth	;	setInited(*this, antenna_azimuth	);	}
		if (isInited(rhs, rhs.antenna_type			))	{ antenna_type		= rhs.antenna_type		;	setInited(*this, antenna_type		);	}
		if (isInited(rhs, rhs.receiver_type			))	{ receiver_type		= rhs.receiver_type		;	setInited(*this, receiver_type		);	}
		if (isInited(rhs, rhs.sat_id				))	{ sat_id			= rhs.sat_id			;	setInited(*this, sat_id				);	}


		if (isInited(rhs, rhs.rec_pos.enable				))	{ rec_pos.enable				= rhs.rec_pos.enable				;	setInited(*this, rec_pos.enable					);	}
		if (isInited(rhs, rhs.rec_pos.ephemeris_sources		))	{ rec_pos.ephemeris_sources		= rhs.rec_pos.ephemeris_sources		;	setInited(*this, rec_pos.ephemeris_sources		);	}
		if (isInited(rhs, rhs.rec_clock.enable				))	{ rec_clock.enable				= rhs.rec_clock.enable				;	setInited(*this, rec_clock.enable				);	}
		if (isInited(rhs, rhs.rec_clock.ephemeris_sources	))	{ rec_clock.ephemeris_sources	= rhs.rec_clock.ephemeris_sources	;	setInited(*this, rec_clock.ephemeris_sources	);	}
		if (isInited(rhs, rhs.rec_attitude.enable			))	{ rec_attitude.enable			= rhs.rec_attitude.enable			;	setInited(*this, rec_attitude.enable			);	}
		if (isInited(rhs, rhs.rec_attitude.sources			))	{ rec_attitude.sources			= rhs.rec_attitude.sources			;	setInited(*this, rec_attitude.sources			);	}
		if (isInited(rhs, rhs.rec_code_bias.enable			))	{ rec_code_bias.enable			= rhs.rec_code_bias.enable			;	setInited(*this, rec_code_bias.enable			);	}
		if (isInited(rhs, rhs.rec_code_bias.default_bias	))	{ rec_code_bias.default_bias	= rhs.rec_code_bias.default_bias	;	setInited(*this, rec_code_bias.default_bias		);	}
		if (isInited(rhs, rhs.rec_code_bias.undefined_sigma	))	{ rec_code_bias.undefined_sigma	= rhs.rec_code_bias.undefined_sigma	;	setInited(*this, rec_code_bias.undefined_sigma	);	}
		if (isInited(rhs, rhs.rec_phase_bias.enable			))	{ rec_phase_bias.enable			= rhs.rec_phase_bias.enable			;	setInited(*this, rec_phase_bias.enable			);	}
		if (isInited(rhs, rhs.rec_phase_bias.default_bias	))	{ rec_phase_bias.default_bias	= rhs.rec_phase_bias.default_bias	;	setInited(*this, rec_phase_bias.default_bias	);	}
		if (isInited(rhs, rhs.rec_phase_bias.undefined_sigma))	{ rec_phase_bias.undefined_sigma= rhs.rec_phase_bias.undefined_sigma;	setInited(*this, rec_phase_bias.undefined_sigma	);	}
		if (isInited(rhs, rhs.rec_ant_delta					))	{ rec_ant_delta					= rhs.rec_ant_delta					;	setInited(*this, rec_ant_delta					);	}
		if (isInited(rhs, rhs.rec_pco						))	{ rec_pco						= rhs.rec_pco						;	setInited(*this, rec_pco						);	}
		if (isInited(rhs, rhs.rec_pcv						))	{ rec_pcv						= rhs.rec_pcv						;	setInited(*this, rec_pcv						);	}
		if (isInited(rhs, rhs.rec_trop.enable				))	{ rec_trop.enable				= rhs.rec_trop.enable				;	setInited(*this, rec_trop.enable				);	}
		if (isInited(rhs, rhs.rec_trop.models				))	{ rec_trop.models				= rhs.rec_trop.models				;	setInited(*this, rec_trop.models				);	}

		return *this;
	}

	map<int, bool>	initialisedMap;
};

/** Options associated with the minimum constraints mode of operation
*/
struct MinimumConstraintOptions : FilterOptions
{
	KalmanModel		scale;
	KalmanModel		rotation;
	KalmanModel		translation;

	bool			once_per_epoch	= false;
	bool			full_vcv		= false;
	bool			scale_by_vcv	= false;
};

struct MongoOptions
{
	bool	enable							= false;
	bool	output_measurements				= false;
	bool	output_components				= false;
	bool	output_states					= false;
	bool	output_trace					= false;
	bool	output_test_stats				= false;
	bool	output_logs						= false;
	bool	output_ssr_precursors			= false;
	bool	delete_history					= false;
	bool	cull_history					= false;
	bool	local							= false;
	string	uri								= "mongodb://localhost:27017";
	string	suffix							= "";
	string	database						= "<CONFIG>";

	bool	predict_states					= false;
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
	E_SSROutTiming		output_timing			= E_SSROutTiming::GPS_TIME;
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
	double			max_lat			= 0;
	double			min_lat			= 0;
	double			int_lat			= 0;
	double			max_lon			= 0;
	double			min_lon			= 0;
	double			int_lon			= 0;

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

struct OrbitErrors
{
	bool	enable						= false;
	double	pos_proc_noise				= 10;
	double	vel_proc_noise				= 5;
	double	vel_proc_noise_trail		= 1;
	double	vel_proc_noise_trail_tau	= 0.05;
};

struct PseudoPulses
{
	bool	enable			= false;
	int		num_per_day		= 1;

	double	pos_proc_noise	= 10;
	double	vel_proc_noise	= 5;
};

/** Options associated with orbital force models
*/
struct OrbitPropagation
{
	bool			central_force					= true;
	bool			planetary_perturbation			= true;
	bool			indirect_J2						= true;
	bool			egm_field						= true;
	bool			solid_earth_tide				= true;
	bool			ocean_tide						= true;
	bool			atm_tide						= false;
	bool			general_relativity				= true;
	bool			pole_tide_ocean					= true;
	bool			pole_tide_solid					= true;
	E_SRPModels		solar_radiation_pressure		= E_SRPModels::NONE;
	bool			empirical						= false;
	bool			antenna_thrust					= false;
	bool			albedo							= false;
	bool			aod								= false;

	vector<bool>	empirical_dyb_eclipse			= {true};
	vector<bool> 	empirical_rtn_eclipse			= {false};

	int				degree_max						= 12;
	double			integrator_time_step			= 60;
	bool			itrf_pseudoobs					= true;

};

struct YamlDefault
{
	string	defaultValue;
	string	comment;
	bool	found;
	string	foundValue;
	string	enumName;
};

/** General options object to be used throughout the software
*/
struct ACSConfig : GlobalOptions, InputOptions, OutputOptions, DebugOptions
{
	vector<YAML::Node>				yamls;
	map<string, YamlDefault>		yamlDefaults;
	map<string, bool>				availableOptions;
	map<string, map<string, bool>>	foundOptions;

	mutex								configMutex;

	vector<string>										configFilenames;
	vector<string>										includedFilenames;
	map<string, time_t>									configModifyTimeMap;
	boost::program_options::variables_map				commandOpts;

	static map<string, string>			docs;

	void	recurseYaml(
		YAML::Node	node,
		const string&		stack		= "",
		const string&		aliasStack	= "");

	bool	parse(
		const vector<string>&					filenames,
		boost::program_options::variables_map&	vm);

	bool	parse();

	void	info(
		Trace& trace);

	void	outputDefaultConfiguration(
		int level);

	SatelliteOptions&			getSatOpts				(SatSys	Sat,	const vector<string>& suffixes = {});
	ReceiverOptions&			getRecOpts				(string	id,		const vector<string>& suffixes = {});

	map<string,		SatelliteOptions>	satOptsMap;
	map<string,		ReceiverOptions>	recOptsMap;

	IonosphericOptions			ionoOpts;
	PPPOptions					pppOpts;
	SPPOptions					sppOpts;
	MinimumConstraintOptions	minCOpts;
	SsrOptions					ssrOpts;
	SsrInOptions				ssrInOpts;
	AmbROptions					ambrOpts;
	SlipOptions					excludeSlip;
	SlipOptions					resetOnSlip;
	OrbitPropagation			orbitPropagation;
	PseudoPulses				pseudoPulses;
	OrbitErrors					orbitErrors;
	IonModelOptions				ionModelOpts;
	NetworkOptions				netOpts;
	SlrOptions					slrOpts;
	MongoOptions				localMongo	= {.local	= true};
	MongoOptions				remoteMongo	= {.local	= false};
};

bool replaceString(
	string&	str,
	string	subStr,
	string	replacement,
	bool	warn = true);

void removePath(
	string& filepath); 	// fully qualified file

void tryAddRootToPath(
	string& root,		///< Root path
	string& path);		///< Filename to prepend root path to

bool configure(
	int		argc,
	char**	argv);

bool checkValidFile(
	const string&	path,				///< Filename to check
	const string&	description = "");	///< Description for error messages

void dumpConfig(
	Trace& trace);

extern ACSConfig acsConfig;		///< Global variable housing all options to be used throughout the software

