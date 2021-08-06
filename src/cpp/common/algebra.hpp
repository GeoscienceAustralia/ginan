
#ifndef _KALMAN_HPP_
#define _KALMAN_HPP_

#include "eigenIncluder.hpp"


#include <iostream>
#include <string>
#include <vector>
#include <limits>
#include <list>
#include <map>

using std::string;
using std::vector;
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
	Station*	station_ptr = 0;		///< Pointer to station object for dereferencing

	bool operator ==	(const KFKey& b) const;
	bool operator <		(const KFKey& b) const;

	friend ostream& operator<<(ostream& os, const KFKey& kfKey)
	{
		os << KF::_from_integral(kfKey.type)._to_string() << ' ' << kfKey.Sat.id() << '\t' << kfKey.str << '\t' << kfKey.num;
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
		os << obsKey.Sat.id() << '\t' << obsKey.str << '\t' << obsKey.type;
		return os;
	}

	bool operator ==	(const ObsKey& b) const;
	bool operator <		(const ObsKey& b) const;

	operator string()
	{
		char buff[100];
		snprintf(buff, sizeof(buff), "%5s %5s %5s %d", str.c_str(), Sat.id().c_str(), type.c_str(), num);
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
							^ hash<string>	{}(key.type) 	<< 2;
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
	VectorXd	R;							///< Measurement noise for these observations
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

		Y = ( Y(keepIndices)             ).eval();
		V = ( V(keepIndices)             ).eval();
		R = ( R(keepIndices)             ).eval();
		A = ( A(keepIndices, Eigen::all) ).eval();
	}
};

/** Object to hold the values used to initialise new states when adding to the kalman filter object
*/
struct InitialState
{
	double	x	= 0;											///< State value
	double	P	= 0;											///< State Covariance
	double	Q	= 0;											///< Process Noise
	double	clampMax	= std::numeric_limits<double>::max();	///< Maximum state value
	double	clampMin	= std::numeric_limits<double>::lowest();///< Minimum state value
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

typedef bool (*RejectCallback)(Trace& trace, KFState& kfState, KFMeas& meas, int index);
typedef bool (*AcceptCallback)(Trace& trace, KFState& kfState, KFMeas& meas, int index);


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
	MatrixXd	P;										///< State Covariance
	VectorXd	dx;										///< Last filter update

	map<KFKey, short int>			kfIndexMap;			///< Map from key to indexes of parameters in the state vector

	map<KFKey, map<KFKey, double>>	stateTransitionMap;
	map<KFKey, map<KFKey, double>>	rateTransitionMap;
	map<KFKey, double>				gaussMarkovTauMap;
	map<KFKey, double>				procNoiseMap;
	map<KFKey, double>				initNoiseMap;
	map<KFKey, double>				stateClampMaxMap;
	map<KFKey, double>				stateClampMinMap;

	list<RejectCallback> 			rejectCallbacks;

	bool		chiQCPass				= false;

	string		rts_filename			= "";
	string		rts_forward_filename	= "";
	int			rts_lag					= 0;

	bool		rtsFilterInProgress		= false;

	int			max_filter_iter			= 1;
	int			max_prefit_remv			= 0;

	bool		output_residuals		= false;

	int			inverter				= E_Inverter::INV;

	KFState()
	{
		//initialise all filter state objects with a ONE element for later use.
		x			= VectorXd	::Ones(1);
		P			= MatrixXd	::Zero(1,1);
		dx			= VectorXd	::Zero(1);

		KFKey oneKey = {KF::ONE};
		kfIndexMap[oneKey]	= 0;
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

	void 	setKFGaussMarkovTau(
		KFKey			kfKey,
		double			tau);

	void	resetKFValue(
		KFKey			kfKey,
		InitialState	initialState = {});

	void	addKFState(
		KFKey			kfKey,
		InitialState	initialState = {});

	void	removeState(
		KFKey kfKey);

	void	clampStateValues();

	void	stateTransition(
		Trace&		trace,
		GTime		newTime);

	int	postFitSigmaCheck(
		Trace&		trace,
		KFMeas&		kfMeas,
		VectorXd&	xp,
		VectorXd&	dx);

	int	preFitSigmaCheck(
		Trace&		trace,
		KFMeas&		kfMeas);

	int 	kFilter(
		Trace&			trace,
		KFMeas&			kfMeas,
		VectorXd&		xp,
		MatrixXd&		Pp,
		VectorXd&		dx);

	bool		chiQC(
		Trace&		trace,
		KFMeas&		kfMeas,
		VectorXd&	xp);

	void 	outputStates(
		Trace&		trace);

	void	doRejectCallbacks(
		Trace&			trace,
		KFMeas&			kfMeas,
		int				badIndex);

	int		filterKalman(
		Trace&			trace,
		KFMeas&			kfMeas,
		bool			innovReady = false);

	int		filterLeastSquares(
		Trace&			trace,
		KFMeas&			kfMeas);

	void	leastSquareInitStates(
		Trace&			trace,
		KFMeas&			kfMeas,
		bool			initCovars	= false,
		VectorXd*		dx			= nullptr);

	KFMeas	combineKFMeasList(
		KFMeasEntryList&	kfEntryList);

	VectorXd getSubState(
		list<KFKey>&	kfKeyList,
		MatrixXd*		covarMat = nullptr);

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



	map<KFKey, double>		designEntryMap;
	map<string, void*>		metaDataMap;

	KFMeasEntry(
		KFState*	kfState_ptr = nullptr,
		ObsKey		obsKey		= {})
	: kfState_ptr	(kfState_ptr),
	obsKey		(obsKey)
	{

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
	list<KFState*>& kfStatePointerList);

KFMeas combineKFMeasList(
	KFMeasEntryList&	kfEntryList,
	KFState&			kfState);

extern int filter_(const double *x, const double *P, const double *H,
				const double *v, const double *R, int n, int m,
				double *xp, double *Pp);

extern int filter(double *x, double *P, const double *H, const double *v,
				const double *R, int n, int m);

/* matrix and vector functions -----------------------------------------------*/
extern double *mat  (int n, int m);
extern int    *imat (int n, int m);
extern double *zeros(int n, int m);
extern double *eye  (int n);
// extern double *vector(int n);
extern double dot (const double *a, const double *b, int n);
extern double norm(const double *a, int n);
extern void matcpy(double *A, const double *B, int n, int m);
extern void matmul(const char *tr, int n, int k, int m, double alpha,
				const double *A, const double *B, double beta, double *C);
extern int  matinv(double *A, int n);
extern int  solve (const char *tr, const double *A, const double *Y, int n,
				int m, double *X);
extern int  lsq   (const double *A, const double *y, int n, int m, double *x,
				double *Q);



extern int lsqqc(
	Trace&	trace,
	const double *H,
	const double *P,
	const double *Z,
	double *v,
	double *xo,
	double *Po,
	int m,
	int n,
	int ind,
	int norb);

#endif
