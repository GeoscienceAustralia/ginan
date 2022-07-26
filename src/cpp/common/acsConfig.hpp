
#ifndef ACS_CONFIG_H
#define ACS_CONFIG_H

#include <boost/filesystem.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/program_options.hpp>

#include <yaml-cpp/yaml.h>

#include "eigenIncluder.hpp"

#include <chrono>
#include <memory>
#include <limits>
#include <vector>
#include <tuple>
#include <array>
#include <list>
#include <map>
#include <set>

using std::vector;
using std::string;
using std::tuple;
using std::array;
using std::list;
using std::map;
using std::set;

#include "streamTrace.hpp"
#include "satSys.hpp"
#include "enums.h"


#define PI          3.141592653589793238462643383279502884197169399375105820974
#define D2R         (PI/180.0)          ///< deg to rad
#define R2D         (180.0/PI)          ///< rad to deg

/** Input source filenames and directories
*/
struct InputOptions
{
	vector<string>	atx_files;
	vector<string>	snx_files;
	vector<string>	blq_files;
                       
	vector<string>	nav_files;
	vector<string>	orb_files;
	vector<string>	sp3_files;
	vector<string>	clk_files;
                       
	vector<string>	erp_files;
	vector<string>	dcb_files;
	vector<string>	bsx_files;
	vector<string>	ion_files;

	vector<string> rnx_files;
	vector<string> obs_rtcm_files;
	vector<string> nav_rtcm_files;
	vector<string> pseudoobs_files;
	
	vector<string>	egm_files;
	vector<string>	jpl_files;

	string	root_stations_dir	= "./";
	
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
	int		trace_level					= 0;
	int		fatal_level					= 0;
	bool	output_station_trace		= false;
	bool	output_network_trace		= false;
	string	trace_directory				= "./";
	string	station_trace_filename		= "<STATION><YYYY><DDD><HH>.trace";
	string	network_trace_filename		= "<STATION><YYYY><DDD><HH>.trace";
	double	rotate_period				= 60*60*24;

	bool	record_rtcm_obs				= false;
	bool	record_rtcm_nav				= false;
	string	rtcm_obs_directory			= "./";
	string	rtcm_nav_directory			= "./";
	string  rtcm_obs_filename			= "<STATION><YYYY><DDD><HH>-OBS.rtcm3";
	string  rtcm_nav_filename			= "<STREAM><YYYY><DDD><HH>-NAV.rtcm3";

	bool	output_log					= false;
	string	log_directory	         	= "./";
	string  log_filename				= "log<LOGTIME>.json";

	bool	output_gpx					= false;
	string	gpx_directory	         	= "./";
	string  gpx_filename				= "<STATION>.gpx";

	bool	output_residuals 			= false;
	bool	output_residual_chain		= true;

	bool	output_config	 			= false;

	bool			output_clocks 				= false;
	E_Ephemeris 	clocks_receiver_source  	= E_Ephemeris::KALMAN;
	E_Ephemeris		clocks_satellite_source 	= E_Ephemeris::KALMAN;
	string			clocks_directory			= "./";
	string			clocks_filename				= "pea<YYYY><DDD><HH>_<SYS>.clk";
	bool			output_ar_clocks			= false;		//todo aaron this config sucks

	bool			output_orbits 					 = false;
	bool			output_orbit_velocities			 = false;
	E_Ephemeris		orbits_data_source 				 = E_Ephemeris::BROADCAST;
	string			orbits_directory				 = "./";
	string			orbits_filename					 = "pea<YYYY><DDD>_<SYS>.sp3";
	
	bool	split_sys					= false;

	bool	output_rinex_obs			= false;
	string	rinex_obs_directory			= "./";
	string	rinex_obs_filename			= "<STATION>_<YYYY><DDD>_<SYS>.<YY>O";
	double	rinex_obs_version			= 3.05;
	bool	rinex_obs_print_C_code		= true;
	bool	rinex_obs_print_L_code		= true;
	bool	rinex_obs_print_D_code	 	= true;
	bool	rinex_obs_print_S_code 		= true;
	
	bool	output_ppp_sol 				= false;
	string	ppp_sol_directory			= "./";
	string	ppp_sol_filename			= "pea<YYYY><DDD><HH>.pppsol";

	bool	output_ionex 				= false;
	string	ionex_directory				= "./";
	string	ionex_filename				= "pea<YYYY><DDD><HH>.ionex";
	IonexOptions	ionexGrid;

	bool	output_rinex_nav			= false;
	string	rinex_nav_directory			= "./";
	string	rinex_nav_filename			= "<YYYY><DDD>_nav_<SYS>.rnx";
	double	rinex_nav_version			= 3.05;

	bool	output_ionstec				= false;
	string	ionstec_directory			= "./";
	string	ionstec_filename			= "pea<YYYY><DDD><HH>.STEC";

	bool	output_erp						= false;
	string	erp_directory					= "./";
	string	erp_filename					= "pea.ERP";

	bool			output_bias_sinex		= false;
	string			bias_sinex_directory	= "./";
	string			bias_sinex_filename		= "AUS0ACSRAP_<YYYY><DDD><HH>00_01D_30S_ABS.BIA";

	bool			output_sinex			= false;
	string 			sinex_directory			= "./";
	string			sinex_filename			= "<CONFIG><WWWW><D>.snx";

	bool			output_trop_sinex		= false;
	E_Ephemeris 	trop_data_source  		= E_Ephemeris::KALMAN;
	string			trop_sinex_directory	= "./";
	string			trop_sinex_filename		= "<CONFIG><WWWW><D>.tro";

	bool	output_persistance			= false;
	bool	input_persistance			= false;
	string 	persistance_directory		= "./";
	string	persistance_filename		= "<CONFIG><WWWW><D>.persist";

	bool	enable_mongo				= false;
	bool	output_mongo_rtcm_messages	= false;
	bool	output_mongo_measurements	= false;
	bool	output_mongo_states			= false;
	bool	output_mongo_test_stats		= false;
	bool	output_intermediate_rts		= false;
	bool	output_mongo_logs			= false;
	bool	delete_mongo_history		= false;
	string	mongo_rts_suffix			= "~";
	string	mongo_uri					= "mongodb://localhost:27017";
	string	mongo_suffix				= "";
	string	mongo_database				= "<CONFIG>";
	
	void defaultOutputOptions()
	{
		*this = OutputOptions();
	}

	bool	output_decoded_rtcm_json	= false;
	string 	decoded_rtcm_json_directory	= "./";
	string	decoded_rtcm_json_filename	= "<CONFIG>_<WWWW>_<D>_decoded_rtcm.json";

	bool	output_encoded_rtcm_json	= false;
	string	encoded_rtcm_json_directory	= "./";
	string	encoded_rtcm_json_filename	= "<CONFIG>_<WWWW>_<D>_encoded_rtcm.json";
	
	bool	output_network_statistics_json	= false;
	string	network_statistics_json_directory	= "./";
	string	network_statistics_json_filename	= "<CONFIG>_network_statistics.json";
	
	string test_filename	= "testData";
	string test_directory	= "./";
};

/** Options to be used only for debugging new features
*/
struct DebugOptions
{
	int		csfreq = 3;         /* cycle slip detection and repair frequency */
	
	bool	mincon_only			= false;
	bool	check_plumbing		= false;
	bool	retain_rts_files	= false;
	bool	rts_only			= false;
};

/** Options for unit testing
*/
struct TestOptions
{
	bool	enable			= false;
	bool	output_pass		= true;
	bool	stop_on_fail	= false;
	bool	stop_on_done	= false;
	bool	output_errors	= false;
	bool	absorb_errors	= false;
};

struct Enable
{
	bool enable = true;
};

struct ModelSatPos : Enable
{
	E_Ephemeris	ephemeris_source	= E_Ephemeris::PRECISE;
};

struct ModelClk : Enable
{
	E_Ephemeris	ephemeris_source	= E_Ephemeris::PRECISE;	
};

struct ModelRecPos : Enable{};

struct ModelTides : Enable
{
	bool	solid		= true;
	bool	otl			= true;
	bool	pole		= true;
};


/** Options to set the tropospheric model used in calculations
*/
struct ModelTrop : Enable
{
	E_TropModel		model		= E_TropModel::VMF3;
	
	string			vmf3dir;
	string			orography;
	string			gpt2grid;
};	

struct Model
{
	ModelRecPos	rec_pos;
	ModelSatPos	sat_pos;
	ModelClk	sat_clock;
	ModelClk	rec_clock;
	ModelTrop	trop;
	ModelTides	tides;
	
	bool range					= true;
	
	bool sat_code_bias			= true;
	bool rec_code_bias			= true;
	bool sat_phase_bias			= true;
	bool rec_phase_bias			= true;
	
	bool rec_ant_delta			= true;
	bool sat_pco				= true;
	bool rec_pco				= true;
	bool sat_pcv				= true;
	bool rec_pcv				= true;
	
	bool relativity				= true;
	bool relativity2			= true;
	bool sagnac					= true;
	bool phase_windup			= true;
	bool integer_ambiguity		= true;
	bool ionospheric_component	= true;
	
	bool clock_definitions		= true;
	bool ionospheric_model		= false;
};

/** Options for the general operation of the software
*/
struct GlobalOptions
{
	Model	model;
	
	double	epoch_interval	= 1;
	int		max_epochs		= 0;

	boost::posix_time::ptime start_epoch	{ boost::posix_time::not_a_date_time };
	boost::posix_time::ptime end_epoch		{ boost::posix_time::not_a_date_time };

	string	config_description				= "Pea";
	string	analysis_agency					= "GAA";
	string	analysis_center					= "Geoscience Australia";
	string	analysis_program				= "AUSACS";
	string	rinex_comment					= "AUSNETWORK1";
	string	reference_system				= "igb14";
	string	time_system						= "G";
	string	ocean_tide_loading_model		= "FES2004";
	string	atmospheric_tide_loading_model	= "---";
	string	geoid_model						= "EGM96";
	string	gradient_mapping_function		= "Chen & Herring, 1992";

	bool	simulate_real_time			= false;

	bool	process_preprocessor		= true;
	bool	process_user				= false;
	bool	process_network 			= false;
	bool	process_minimum_constraints	= false;
	bool	process_ionosphere			= false;
	bool	process_rts					= false;
	bool	process_ppp					= false;
	bool	process_orbits				= false;

	map<E_Sys,	bool>				process_sys;
	map<E_Sys,	bool>				solve_amb_for;
	map<E_Sys,	map<int,	bool>>	process_freq;	
	bool							process_meas[NUM_MEAS_TYPES] = {true, true};

	double	elevation_mask	= 10 * D2R;

	string	pivot_station	= "<AUTO>";


	bool	reject_eclipse	= true;
	bool	raim			= true;
	bool	interpolate_rec_pco = true;

	double	thres_slip   	= 0.05;
	double	mw_proc_noise	= 0;
	double	max_gdop     	= 30;
	double	deweight_factor	= 100;

	double	wait_next_epoch		= 0;
	double	wait_all_stations	= 0;
	bool	require_obs			= true;
	
	bool	delete_old_ephemerides = false;
	
	bool	reinit_on_all_slips		= false;
	bool	reinit_on_clock_error	= false;
	bool	deweight_on_state_error	= false;

	bool	joseph_stabilisation		= false;
	double	validity_interval_factor	= 100;

	list<string>							station_files;

	E_OffsetType ssr_input_antenna_offset = E_OffsetType::UNSPECIFIED;
	bool	     if_antenna_phase_centre  = true;

	map<E_Sys, vector<E_ObsCode>>	code_priorities;
	
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

	map<E_Sys, E_ObsCode> clock_codesL1 =	///< Default observation codes on L1 for IF combination based satellite clocks
	{
		{E_Sys::GPS, E_ObsCode::L1W},
		{E_Sys::GLO, E_ObsCode::L1P},
		{E_Sys::GAL, E_ObsCode::L1C},
		{E_Sys::BDS, E_ObsCode::L2I},
		{E_Sys::QZS, E_ObsCode::L1C}
	};

	map<E_Sys, E_ObsCode> clock_codesL2 =	///< Default observation codes on L2 for IF combination based satellite clocks
	{
		{E_Sys::GPS, E_ObsCode::L2W},
		{E_Sys::GLO, E_ObsCode::L2P},
		{E_Sys::GAL, E_ObsCode::L5Q},
		{E_Sys::BDS, E_ObsCode::L7I},
		{E_Sys::QZS, E_ObsCode::L2L}
	};
	
	bool dumb_sat_pco	= true;
	bool dumb_rec_pco	= false;
	
	
	double clock_wrap_threshold = 0.05e-3;

	//to be removed?

	double proc_noise_iono	= 0.001;		//todo aaron, these need values, or move to other type
	
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
};

struct FilterOptions
{
	int			phase_reject_limit	= 10;
	int			outage_reset_limit	= 10;
	
	bool		sigma_check			= true;
	bool		w_test				= false;
	bool		chi_square_test		= false;
	E_ChiSqMode	chi_square_mode		= E_ChiSqMode::NONE;
	double		sigma_threshold		= 4;

	int			max_filter_iter 	= 2;
	int			max_prefit_remv 	= 2;
	
	string		rts_filename	= "Filter-<STATION>.rts";
	string		rts_directory	= "./";
	int			rts_lag			= -1;
	
	E_Inverter	inverter			= E_Inverter::LDLT;
};

/** Options associated with the network processing mode of operation
*/
struct NetworkOptions : FilterOptions
{
	NetworkOptions()
	{
		rts_filename = "Network.rts";
	}

	KalmanModel		eop;
	KalmanModel		eop_rates;

	set<string>		download_hosts;
	set<string>		nav_mount_url;
	set<string>		obs_mount_url;	
	
	bool			chunk_stations		= false;
	int				chunk_size			= 0;
};

/** Options associated with the ionospheric modelling processing mode of operation
*/
struct IonosphericOptions
{
	E_IonoMode 		corr_mode  			= E_IonoMode::IONO_FREE_LINEAR_COMBO;
	E_LinearCombo	iflc_freqs			= E_LinearCombo::ANY;
	
	bool			common_ionosphere			= false;
	bool			use_if_combo				= false;
	bool			auto_select_default_code	= false;
};

struct SlipOptions
{
	bool LLI	= true;
	bool GF		= true;
	bool MW		= true;
	bool EMW	= true;
	bool CJ		= true;
	bool SCDIA	= true;
};

struct IonModelOptions
{
	E_IonoModel		model			= E_IonoModel::NONE;
	int				NBasis;
	int				function_order;
	double			model_noise;
	vector<double>	layer_heights;

	KalmanModel	ion;
};

struct AmbROptions
{
	E_ARmode	WLmode			= E_ARmode::OFF;		///< Ambiguity resolution mode: OFF, ROUND, ITER_RND, BOOTST, LAMBDA
	E_ARmode	NLmode			= E_ARmode::OFF; 		///< Ambiguity resolution mode: OFF, ROUND, ITER_RND, BOOTST, LAMBDA
	int			lambda_set		= 2;
	int			AR_max_itr		= 1;
	double		min_el_AR		= 15;					///< minimum elevation to attempt ambigity resolution (degrees)
	int			Max_Hold_epoc	= 0;
	double		Max_Hold_time	= 1200;

	double	WLsuccsThres = 0.9999;	///< Thresholds for ambiguity validation: succsess rate WL
	double	WLratioThres = 3;		///< Thresholds for ambiguity validation: succsess rate WL
	int		WL_filter_iter = 2;
	int		WL_prefit_remv = 2;
	
	double	WLSatPrcNois = 0.0001;	///< Process noise for WL satellite biases
	double	WLRecPrcNois = 0.001;	///< Process noise for WL station biases

	double	NLsuccsThres = 0.9999;	///< Thresholds for ambiguity validation: succsess rate NL
	double	NLratioThres = 3;		///< Thresholds for ambiguity validation: succsess rate NL
	double	NLstarttime = 3600;		///< Time before starting to calculate (and output NL zmbiguities/biases)

	double	biasOutrate  = 0;		///< Update interval for clock update 0: no output
	
	int reduction_limit = 5000;
};

/** Options to be applied to kalman filter states for individual satellites
*/
struct SatelliteOptions
{
	bool			_initialised	= false;
	bool			exclude			= false;

	KalmanModel		clk;
	KalmanModel		clk_rate;
	KalmanModel		clk_rate_gauss_markov;
	KalmanModel		orbit;
	KalmanModel		srp;
	KalmanModel		pos;
	KalmanModel		pos_rate;
	KalmanModel		orb;
	KalmanModel		pco;
	KalmanModel		ant;
	KalmanModel		code_bias;
	KalmanModel		phase_bias;
};

/** Options to be applied to kalman filter states for individual receivers
*/
struct ReceiverOptions
{
	bool			_initialised	= false;
	bool			exclude			= false;

	KalmanModel		amb;
	KalmanModel		pos;
	KalmanModel		pos_rate;
	KalmanModel		clk;
	KalmanModel		clk_rate;
	KalmanModel		clk_rate_gauss_markov;
	KalmanModel		dcb;
	KalmanModel		pco;
	KalmanModel		ant;
	KalmanModel		ion;
	KalmanModel		trop;
	KalmanModel		trop_gauss_markov;
	KalmanModel		trop_grads;
	KalmanModel		trop_grads_gauss_markov;
	KalmanModel		code_bias;
	KalmanModel		phase_bias;

	E_NoiseModel	error_model	= E_NoiseModel::ELEVATION_DEPENDENT;
	vector<double>	code_sigmas	= {1};
	vector<double>	phas_sigmas	= {0.0015};
};

/** Minimum constraint options for individual receivers
*/
struct MinimumStationOptions		//todo aaron, move to stations?
{
	bool			_initialised	= false;

	vector<double>	noise			= {-1};
};

/** Options associated with the minimum constraints mode of operation
*/
struct MinimumConstraintOptions : FilterOptions
{
	KalmanModel		scale;
	KalmanModel		rotation;
	KalmanModel		translation;
	
	bool			full_vcv		= false;
	bool			scale_by_vcv	= false;

	map<string,	MinimumStationOptions>		stationMap;
};

/** Options associated with SSR corrections and exporting RTCM messages
*/
struct SsrOptions
{
	bool			calculate_ssr			= false;
	bool			extrapolate_corrections	= false;
	int				prediction_interval		= 30;
	int				prediction_duration		= 0;
	E_Ephemeris 	ephemeris_source		= E_Ephemeris::PRECISE;
	E_Ephemeris 	clock_source			= E_Ephemeris::KALMAN;
	E_Ephemeris 	code_bias_source		= E_Ephemeris::PRECISE;
	E_Ephemeris 	phase_bias_source		= E_Ephemeris::NONE;
};

struct SsrInOptions
{
	double			code_bias_valid_time	= 3600;		///< Valid time period of SSR code biases
	double			phase_bias_valid_time	= 30;		///< Valid time period of SSR phase biases
};

struct SSRMetaOpts
{
	bool	itrf_datum			= true;
	int		provider_id			= 0;
	int		solution_id			= 0;
	int 	update_interval		= 10;
	int 	master_iod			= 10;
};

struct SsrBroadcast : SSRMetaOpts
{
	int						message_timeout		= INT_MAX;
	string					url;
	
	set<RtcmMessageType, std::greater<RtcmMessageType>>     rtcmMessagesTypes;
};

/** Options associated with orbital force models
*/
struct ForceModels
{  
	bool			earth_gravity					= true;
	E_GravMdl		gravity_model					= E_GravMdl::GGM03S;	
	bool			solid_earth_tides				= false;
	bool			ocean_tide_loading				= false;
	bool			relativity_effect				= false;
	bool			solar_radiation_pressure		= false;
	bool			thermal_emission				= false;
	bool			earth_albedo					= false;
	bool			infrared_radiation				= false;
	bool			antenna_thrust					= false;
	bool			empirical_acceleration			= false;
	bool			satellite_manoeuvre				= false;
	double			sat_mass						= 100;
	E_SRPModels		srp_model						= E_SRPModels::CANNONBALL;
	double			srp_area						= 5;
	double			srp_coef						= 1;
	int				egmAccDeg						= 12;
	int				egmAccOrd						= 12;	                
	int				egmSTMDeg						= 4;
	int				egmSTMOrd						= 4;	
	E_Integrator	ode_integrator					= E_Integrator::RKF78;     
	E_TidesMdl		earth_tide_Model				= E_TidesMdl::ELASTIC;  
	
	map<E_ThirdBody,	bool>			process_third_body;
};

/** General options object to be used throughout the software
*/
struct ACSConfig : GlobalOptions, InputOptions, OutputOptions, DebugOptions
{
	YAML::Node	yaml;
	map<string, tuple<string, string>>	yamlDefaults;
	map<string, bool>					availableOptions;
	

	string												configFilename;
	map<string, time_t>									configModifyTimeMap;
	boost::program_options::variables_map				commandOpts;
	
	void	recurseYaml(
		YAML::Node	node, 
		string		stack = "");

	bool	parse(string filename, boost::program_options::variables_map& vm);
	bool	parse();
	void	info(Trace& trace);
	
	void	outputDefaultConfiguration();

	SatelliteOptions&			getSatOpts		(SatSys&	Sat);
	ReceiverOptions&			getRecOpts		(string		id);
	MinimumStationOptions&		getMinConOpts	(string 	id);

	map<string,		SatelliteOptions>	satOptsMap;
	map<string,		ReceiverOptions>	recOptsMap;

	IonosphericOptions			ionoOpts;
	NetworkOptions				pppOpts;
	MinimumConstraintOptions	minCOpts;
	TestOptions					testOpts;
	SsrOptions					ssrOpts;
	SsrInOptions				ssrInOpts;
	AmbROptions					ambrOpts;
	SlipOptions					excludeSlip;
	ForceModels					forceModels;
	IonModelOptions			ionModelOpts;

	KalmanModel	ion;
	list<string>	rinexFiles;
	
	bool		common_atmosphere	= false;

	map<string, SsrBroadcast>	uploadingStreamData;
};






bool replaceString(
	string&	str,
	string	subStr,
	string	replacement,
	bool	warn = true);

void removePath(
	string &filepath); 	// fully qualified file

void tryAddRootToPath(
	string& root,		///< Root path
	string& path);		///< Filename to prepend root path to

void replaceTags(
	string&						str);		///< String to replace macros within

bool configure(
	int		argc,
	char**	argv);

bool checkValidFile(
	string&	path,				///< Filename to check
	string	description = "");	///< Description for error messages

void dumpConfig(
	Trace& trace);

extern ACSConfig acsConfig;		///< Global variable housing all options to be used throughout the software



#endif
