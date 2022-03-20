
#ifndef _KALMAN_HPP_
#define _KALMAN_HPP_

#include "eigenIncluder.hpp"


#include <iostream>
#include <string>
#include <vector>
#include <limits>
#include <tuple>
#include <list>
#include <map>

using std::string;
using std::vector;
using std::tuple;
using std::list;
using std::hash;
using std::map;

#include "streamTrace.hpp"
#include "satSys.hpp"
#include "gTime.hpp"


//forward declaration
struct Station;

/** Keys used to interface with Kalman filter objects.
* These have parameters to separate states of different 'type', for different 'Sat's, with different receiver id 'str's and may have a different 'num' (eg xyz->0,1,2)
*
* Keys should be used rather than indices for accessing kalman filter state parameters.
*/
struct KFKey
{
	short int	type	= 0;			///< Key type (From enum)
	SatSys		Sat		= {};			///< Satellite
	string		str		= "";			///< String (receiver ID)
	short int 	num		= 0;			///< Subkey number (eg xyz => 0,1,2)
	Station*	rec_ptr	= 0;		///< Pointer to station object for dereferencing

	bool operator ==	(const KFKey& b) const;
	bool operator <		(const KFKey& b) const;

	friend ostream& operator<<(ostream& os, const KFKey& kfKey)
	{
		tracepdeex(0, os, "%10s %4s %4s %3d", KF::_from_integral(kfKey.type)._to_string(), kfKey.Sat.id().c_str(), kfKey.str.c_str(), kfKey.num); 
// 		os << KF::_from_integral(kfKey.type)._to_string() << ' ' << kfKey.Sat.id() << ' ' << kfKey.str << ' ' << kfKey.num;
		return os;
	}
};

struct ObsKey
{
	SatSys		Sat		= {};			///< Satellite
	string		str		= "";			///< String (receiver ID)
	string	 	type	= "";			///< Substring (eg LC12-P)
	int 		num		= 0;

	friend ostream& operator<<(ostream& os, const ObsKey& obsKey)
	{
		os 
		<< obsKey.Sat.id()	<< '\t' 
		<< obsKey.str		<< '\t' 
		<< obsKey.type		<< '\t' 
		<< obsKey.num;
		
		return os;
	}

	bool operator ==	(const ObsKey& b) const;
	bool operator <		(const ObsKey& b) const;

	operator string()
	{
		char buff[100];
		snprintf(buff, sizeof(buff), "%5s\t%3s\t%5s", str.c_str(), Sat.id().c_str(), (type + std::to_string(num) ).c_str() );
		string str = buff;

		return str;
	}
};

namespace std
{
	template<> struct hash<ObsKey>
	{
		size_t operator()(ObsKey const& key) const
		{
			//create hashes of all parts and XOR them to get a complete hash
// 			string str;
// 			if (key.station_ptr == nullptr)	str = "NO STREAM";
// 			else 							str = key.station_ptr->id;

			size_t hashval	= hash<string>	{}(key.str)		<< 0
							^ hash<size_t>	{}(key.Sat) 	<< 1
							^ hash<string>	{}(key.type) 	<< 2
							^ hash<int>		{}(key.num) 	<< 3;
			return hashval;
		}
	};
}

namespace std
{
	template<> struct hash<KFKey>
	{
		size_t operator()(KFKey const& key) const
		{
			//create hashes of all parts and XOR them to get a complete hash

			size_t hashval	= hash<string>	{}(key.str)		<< 0
							^ hash<size_t>	{}(key.Sat) 	<< 1
							^ hash<int>		{}(key.type) 	<< 2
							^ hash<short>   {}(key.num) 	<< 3;
			return hashval;
		}
	};
}

struct FilterChunk
{
	Trace*	trace_ptr = nullptr;
	int		begX		=  0;
	int		numX		= -1;
	int		begH		=  0;
	int		numH		= -1;
};

/** Minimum viable kfState element object.
* Used in binary io to save space, and where other functions are not required.
*/
struct SubState
{
	KFKey				kfKey;		///< Key for this state
	double				x;			///< Value for this state
};

/** Object to hold measurements, design matrices, and residuals for multiple observations
*/
struct KFMeas
{
	GTime		time = GTime::noTime();		///< Epoch these measurements were recorded
	VectorXd	Y;							///< Value of the observations (for linear systems)
	VectorXd	V;							///< Residual of the observations (for non-linear systems)
	MatrixXd	R;							///< Measurement noise for these observations
	VectorXd	W;							///< Weight (inverse of noise) used in least squares
	MatrixXd	A;							///< Design matrix between measurements and state

	vector<ObsKey>				obsKeys;					///< Optional labels for reporting when measurements are removed etc.
	vector<map<string, void*>>	metaDataMaps;
	
	void removeMeas(int index)
	{
		vector<int> keepIndices;

		for (int i = 0; i < Y.rows(); i++)
		{
			if (i != index)
			{
				keepIndices.push_back(i);
			}
		}

		obsKeys.erase(obsKeys.begin() + index);

		Y = ( Y(keepIndices)			).eval();
		V = ( V(keepIndices)			).eval();
		R = ( R(keepIndices, keepIndices)	).eval();
		A = ( A(keepIndices, all)		).eval();
	}
};

/** Object to hold the values used to initialise new states when adding to the kalman filter object
*/
struct InitialState
{
	double	x	= 0;											///< State value
	double	P	= 0;											///< State Covariance
	double	Q	= 0;											///< Process Noise
	double	tau	= -1;											///< Correlation Time, default to -1 (inf) (Random Walk)
	double	mu	= 0;											///< Desired Mean Value
};

struct KalmanModel;

InitialState initialStateFromConfig(
	KalmanModel&	kalmanModel,
	int				index);

typedef list<KFMeas>		KFMeasList;

typedef std::ostream		Trace;

struct KFMeasEntry;
typedef list<KFMeasEntry>	KFMeasEntryList;

struct KFState;

typedef bool (*StateRejectCallback)	(Trace& trace, KFState& kfState, KFMeas& meas, KFKey&	key);
typedef bool (*MeasRejectCallback)	(Trace& trace, KFState& kfState, KFMeas& meas, int		index);


/** Kalman filter object.
*
* Contains most persistant parameters and values of state. Includes state vector, covariance, and process noise.
*
* This object performs all operations on the kalman filter to ensure that edge cases are included and state kept in a valid configuration.
*/
struct KFState
{
	bool		lsqRequired		= false;				///< Uninitialised parameters require least squares calculation

	GTime		time = {};
	VectorXd	x;										///< State
	MatrixXd	Z;										///< Permutation Matrix
	MatrixXd	P;										///< State Covariance
	VectorXd	dx;										///< Last filter update

	map<KFKey, short int>						kfIndexMap;			///< Map from key to indexes of parameters in the state vector

	map<KFKey, map<KFKey, double>>				ZTransitionMap;
	map<KFKey, double>							ZAdditionMap;
	map<KFKey, map<KFKey, tuple<double, int>>>	stateTransitionMap;
	map<KFKey, double>							gaussMarkovTauMap;
	map<KFKey, double>							gaussMarkovMuMap;
	map<KFKey, double>							procNoiseMap;
	map<KFKey, double>							initNoiseMap;
	map<ObsKey, double>							noiseElementMap;

	list<StateRejectCallback> 					stateRejectCallbacks;
	list<MeasRejectCallback> 					measRejectCallbacks;

	bool		chiQCPass				= false;

	bool		sigma_check				= true;
	bool		w_test					= false;
	bool		chi_square_test			= false;
	E_ChiSqMode	chi_square_mode			= E_ChiSqMode::NONE;
	double		sigma_threshold			= 4;

	string		rts_filename			= "";
	string		rts_forward_filename	= "";
	int			rts_lag					= 0;

	int			max_filter_iter			= 1;
	int			max_prefit_remv			= 0;

	bool		output_residuals		= false;

	int			inverter				= E_Inverter::INV;

	KFState()
	{
		//initialise all filter state objects with a ONE element for later use.
		x			= VectorXd	::Ones(1);
		Z			= MatrixXd	::Ones(1,1);
		P			= MatrixXd	::Zero(1,1);
		dx			= VectorXd	::Zero(1);

		KFKey oneKey = {KF::ONE};
		kfIndexMap[oneKey]	= 0;
		
		initFilterEpoch();
	}


	void	initFilterEpoch();

	int		getKFIndex(
		KFKey		key);

	bool	getKFValue(
		KFKey		key,
		double&		value,
		double*		variance = nullptr);

	bool	getKFSigma(
		KFKey		key,
		double&		sigma);

	bool	setKFValue(
		KFKey		key,
		double		value);

	bool	setKFNoise(
		KFKey		key,
		double		value);

	void	setKFTransRate(
		KFKey			dest,
		KFKey			source,
		double			value,
		InitialState	initialState = {});

	void	setKFTrans(
		KFKey			dest,
		KFKey			source,
		double			value,
		InitialState	initialState = {});

	void	addKFState(
		KFKey			kfKey,
		InitialState	initialState = {});

	void	addNoiseElement(
		ObsKey			obsKey,
		double			variance);

	void	removeState(
		KFKey kfKey);

	void	stateTransition(
		Trace&		trace,
		GTime		newTime);

	void	preFitSigmaCheck(
		Trace&		trace,	
		KFMeas&		kfMeas,	
		KFKey&		badStateKey,
		int&		badMeasIndex,	
		double&		sumOfSqTestStat,
		int			begX,
		int			numX,
		int			begH,
		int			numH);

	void	postFitSigmaChecks(
		Trace&		trace,
		KFMeas&		kfMeas,
		VectorXd&	xp,
		VectorXd&	dx,
		int			iteration,
		KFKey&		badStateKey,	
		int&		badMeasIndex,	
		double&		sumOfSqTestStat,
		int			begX,
		int			numX,
		int			begH,
		int			numH);

	double stateChiSquare(
		Trace&		trace,
		MatrixXd&	Pp,
		VectorXd&	dx,
		int			begX,
		int			numX,
		int			begH,
		int			numH);

	double measChiSquare(
		Trace&		trace,
		KFMeas&		kfMeas,
		VectorXd&	dx,
		int			begX,
		int			numX,
		int			begH,
		int			numH);

	double innovChiSquare(
		Trace&		trace,	
		KFMeas&		kfMeas,	
		int			begX,
		int			numX,
		int			begH,
		int			numH);

	int	kFilter(
		Trace&			trace,	
		KFMeas&			kfMeas,	
		VectorXd&		xp,   	
		MatrixXd&		Pp,   	
		VectorXd&		dx,		
		int				begX	=  0,
		int				numX	= -1,
		int				begH	=  0,
		int				numH	= -1);

	bool		chiQC(
		Trace&		trace,
		KFMeas&		kfMeas,
		VectorXd&	xp);

	void 	outputStates(
		Trace&			trace,			///< Trace to output to
		int				begX	=  0,
		int				numX 	= -1);
	
	void	outputCorrelations(
		Trace&		trace);

	bool	doStateRejectCallbacks(
		Trace&			trace,
		KFMeas&			kfMeas,
		KFKey&			badKey);

	bool	doMeasRejectCallbacks(
		Trace&			trace,
		KFMeas&			kfMeas,
		int				badIndex);

	int		filterKalman(
		Trace&				trace,
		KFMeas&				kfMeas,
		bool				innovReady			= false,
		list<FilterChunk>*	filterChunkList_ptr	= nullptr);

	int		filterLeastSquares(
		Trace&			trace,
		KFMeas&			kfMeas);

	void	leastSquareInitStatesA(
		Trace&			trace,
		KFMeas&			kfMeas,
		bool			initCovars	= false,
		VectorXd*		dx			= nullptr,
		bool			innovReady	= false);

	void	leastSquareInitStates(
		Trace&			trace,
		KFMeas&			kfMeas,
		bool			initCovars	= false,
		VectorXd*		dx			= nullptr,
		bool			innovReady	= false);

	KFMeas	combineKFMeasList(
		KFMeasEntryList&	kfEntryList,
		GTime				measTime = GTime::noTime());

	VectorXd getSubState(
		map<KFKey, int>&	kfKeyMap,
		MatrixXd*			covarMat = nullptr);

	KFMeasEntryList calcPrefitResids(
		Trace&			trace,
		KFMeas&			kfMeas);
};

/** Object to hold an individual measurement.
* Includes the measurement itself, (or its innovation) and design matrix entries
* Adding design matrix entries for states that do not yet exist will create and add new states to the measurement's kalman filter object.
*/
struct KFMeasEntry
{
	KFState* kfState_ptr;			///< Pointer to filter object that measurements are referencing

	double value	= 0;			///< Value of measurement (for linear systems)
	double noise	= 0;			///< Noise of measurement
	double innov	= 0;			///< Innovation of measurement (for non-linear systems)
	ObsKey obsKey	= {};			///< Optional labels to be used in output traces



	map<KFKey,	double>		designEntryMap;
	map<ObsKey,	double>		noiseEntryMap;
	map<string,	void*>		metaDataMap;

	KFMeasEntry(
		KFState*	kfState_ptr = nullptr,
		ObsKey		obsKey		= {})
	:	kfState_ptr	(kfState_ptr),
		obsKey		(obsKey)
	{

	}

	/** Adds a noise element for this measurement
	*/
	void addNoiseEntry(
		ObsKey			obsKey,				///< [in]	Key to determine the origin of the noise
		double			value,				///< [in]	Noise entry matrix entry value
		double			variance)			///< [in]	Variance of noise element
	{
		if	( value		== 0
			||variance	== 0)
		{
			return;
		}

		if (kfState_ptr)
		{
			kfState_ptr->addNoiseElement(obsKey, variance);
		}

		noiseEntryMap[obsKey] = value;
	}

	/** Adds a design matrix entry for this measurement
	*/
	void addDsgnEntry(
		KFKey			kfKey,				///< [in]	Key to determine which state parameter is affected
		double			value,				///< [in]	Design matrix entry value
		InitialState	initialState = {})	///< [in]	Initial conditions for new states
	{
		if (value == 0)
		{
			return;
		}

		if (kfState_ptr)
		{
			kfState_ptr->addKFState(kfKey, initialState);
		}

		designEntryMap[kfKey] = value;
	}

	/** Adds the measurement noise entry for this measurement
	*/
	void setNoise(
		double value)		///< [in]	Measurement noise matrix entry value
	{
		if (value == 0)
		{
			std::cout << "Zero noise encountered"	<< std::endl;
// 			return;
		}
		if 		(std::isinf(value))
		{
			std::cout << "Inf noise encountered"	<< std::endl;
			return;
		}
		else if (std::isnan(value))
		{
			std::cout << "Nan noise encountered"	<< std::endl;
			return;
		}

		this->noise = value;
	}

	/** Adds the actual measurement value for this measurement
	*/
	void setValue(
		double value)		///< [in]	Actual measurement entry value
	{
		this->value = value;
	}

	/** Adds the innovation value for this measurement
	*/
	void setInnov(
		double value)		///< [in]	Innovation entry value
	{
		this->innov = value;
	}
};

KFState mergeFilters(
	list<KFState*>& kfStatePointerList,
	bool			includeTrop = false);

int filter_(const double *x, const double *P, const double *H,
				const double *v, const double *R, int n, int m,
				double *xp, double *Pp);

/* matrix and vector functions -----------------------------------------------*/
double *mat  (int n, int m);
int    *imat (int n, int m);
double *zeros(int n, int m);
double *eye  (int n);
double dot (const double *a, const double *b, int n);
double norm(const double *a, int n);
void matcpy(double *A, const double *B, int n, int m);
void matmul(const char *tr, int n, int k, int m, double alpha, const double *A, const double *B, double beta, double *C);
int  matinv(double *A, int n);
int  solve (const char *tr, const double *A, const double *Y, int n,
				int m, double *X);


extern double chisqr_arr[];

#endif
