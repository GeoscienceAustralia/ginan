
#pragma once

#include "eigenIncluder.hpp"
#include <boost/algorithm/string.hpp>

#include <iostream>
#include <string>
#include <vector>
#include <limits>
#include <math.h>
#include <mutex>
#include <tuple>
#include <map>

using boost::algorithm::to_lower;
using std::lock_guard;
using std::string;
using std::vector;
using std::mutex;
using std::tuple;
using std::hash;
using std::pair;
using std::map;

#include "acsConfig.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "trace.hpp"


//forward declaration
struct Receiver;

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
	int 		num		= 0;			///< Subkey number (eg xyz => 0,1,2)
	string		comment	= "";			///< Optional comment
	Receiver*	rec_ptr	= 0;			///< Pointer to station object for dereferencing

	bool operator ==	(const KFKey& b) const;
	bool operator <		(const KFKey& b) const;

	static string emptyString()
	{
		KFKey key;
		string str = key;
		for (auto& c : str)
		{
			if (c != '\t')
				c = ' ';
		}

		return str;
	}

	operator string() const
	{
		char buff[100];
		snprintf(buff, sizeof(buff), "%10s\t%4s\t%4s\t%5d", KF::_from_integral(type)._to_string(), Sat.id().c_str(), str.c_str(), num);
		string str = buff;

		return str;
	}

	string commaString() const
	{
		char buff[100];
		snprintf(buff, sizeof(buff), "%s,%s,%s,%d", KF::_from_integral(type)._to_string(), Sat.id().c_str(), str.c_str(), num);
		string str = buff;
		to_lower(str);

		return str;
	}

	friend ostream& operator<<(ostream& os, const KFKey& kfKey)
	{
		string str = kfKey;
		os << str;

		return os;
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & Sat;
		ar & str;
		ar & num;
		ar & type;
		ar & comment;
	}

};

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


struct ComponentsDetails
{
	double	value	= 0;
	string	eq;					//not valid after combinations
	double	var		= 0;


	ComponentsDetails& operator+=(const ComponentsDetails& rhs)
	{
		value	+=	rhs.value;
		var		+=	rhs.var;
		return *this;
	}

	ComponentsDetails operator*(double rhs)
	{
		ComponentsDetails newDetails = *this;
		newDetails.value	*= rhs;
		newDetails.var		*= rhs;
		newDetails.var		*= rhs;
		return newDetails;
	}
};

/** Object to hold measurements, design matrices, and residuals for multiple observations
*/
struct KFMeas
{
	GTime		time = GTime::noTime();		///< Epoch these measurements were recorded
	VectorXd	Y;							///< Value of the observations (for linear systems)
	VectorXd	V;							///< Prefit Residual of the observations (for non-linear systems)
	VectorXd	VV;							///< Postfit Residual of the observations (for non-linear systems)
	MatrixXd	R;							///< Measurement noise for these observations
	VectorXd	W;							///< Weight (inverse of noise) used in least squares
	MatrixXd	H;							///< Design matrix between measurements and state
	MatrixXd	H_star;						///< Design matrix between measurements and noise states

	vector<KFKey>									obsKeys;					///< Optional labels for reporting when measurements are removed etc.
	vector<map<string, void*>>						metaDataMaps;
	vector<map<E_Component, ComponentsDetails>>		componentsMaps;

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

		Y		= ( Y		(keepIndices)				).eval();
		V		= ( V		(keepIndices)				).eval();
		VV		= ( VV		(keepIndices)				).eval();
		R		= ( R		(keepIndices, keepIndices)	).eval();
		H		= ( H		(keepIndices, all)			).eval();
		H_star	= ( H_star	(keepIndices, all)			).eval();
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		int rows = H.rows();
		int cols = H.cols();
		ar & rows;
		ar & cols;

		if (ARCHIVE::is_saving::value)
		{
			//just wrote this, we are writing
			map<pair<int, int>, double>	H2;

			ar & obsKeys;
			ar & time;
			ar & VV;

			for (int i = 0; i < rows; i++)
			for (int j = 0; j < cols; j++)
			{
				double value = H(i,j);
				if (value)
				{
					H2[{i,j}] = value;
				}
			}

			ar & H2;
		}
		else
		{
			//we're reading
			map<pair<int, int>, double>	H2;

			ar & obsKeys;
			ar & time;
			ar & VV;
			ar & H2;

			H = MatrixXd::Zero(rows,cols);
			R = MatrixXd::Zero(rows,rows);
			V = VectorXd::Zero(rows);

			for (auto & [index, value] : H2)
			{
				H(index.first, index.second) = value;
			}
		}
	}
};

/** Object to hold the values used to initialise new states when adding to the kalman filter object
*/
struct InitialState
{
	double	estimate	= false;
	double	x			= 0;	///< State value
	double	P			= -1;	///< State Covariance
	double	Q			= 0;	///< Process Noise, -ve indicates infinite (throw away state)
	double	tau			= -1;	///< Correlation Time, default to -1 (inf) (Random Walk)
	double	mu			= 0;	///< Desired Mean Value
	string	comment;
};

struct KFStatistics
{
	double averageRatio	= 0;
	double sumOfSquares	= 0;
};

struct KFState;
struct KalmanModel;
struct KFMeasEntry;

InitialState initialStateFromConfig(
	const KalmanModel&	kalmanModel,
	int					index = 0);

typedef std::ostream		Trace;

struct KFMeasList : vector<KFMeas>
{

};

struct KFMeasEntryList : vector<KFMeasEntry>
{

};

typedef bool (*StateRejectCallback)	(Trace& trace, KFState& kfState, KFMeas& meas, const	KFKey&	key,	bool postFit);
typedef bool (*MeasRejectCallback)	(Trace& trace, KFState& kfState, KFMeas& meas, 			int		index,	bool postFit);

struct Exponential
{
	double value	= 0;
	double tau		= 0;
};

/** Kalman filter object.
*
* Contains most persistant parameters and values of state. Includes state vector, covariance, and process noise.
*
* This object performs all operations on the kalman filter to ensure that edge cases are included and state kept in a valid configuration.
*/
struct KFState_ : FilterOptions
{
	bool		lsqRequired		= false;				///< Uninitialised parameters require least squares calculation

	GTime		time = {};
	VectorXd	x;										///< State
	MatrixXd	P;										///< State Covariance
	MatrixXd	Pp;										///< State Covariance update
	VectorXd	dx;										///< Last filter update

	map<KFKey, short int>								kfIndexMap;			///< Map from key to indexes of parameters in the state vector
	map<KFKey, short int>								noiseIndexMap;		///< Map from key to indexes of parameters in the noise vector

	map<KFKey, map<KFKey, map<int, double>>>			stateTransitionMap;
	map<KFKey, double>									gaussMarkovTauMap;
	map<KFKey, double>									gaussMarkovMuMap;
	map<KFKey, double>									procNoiseMap;
	map<KFKey, double>									initNoiseMap;
	map<KFKey, double>									noiseElementMap;
	map<KFKey, Exponential>								exponentialNoiseMap;

	vector<StateRejectCallback> 						stateRejectCallbacks;
	vector<MeasRejectCallback> 							measRejectCallbacks;

	map<string, string>									metaDataMap;

	bool		chiQCPass				= false;
	double		chi						= 0;
	int			dof						= 0;

	string		id						= "KFState";

	string		suffix					= "";

	string		rts_basename			= "";
	int			rts_lag					= 0;

	bool		output_residuals		= false;
	bool		outputMongoMeasurements	= false;

	map<string, int>	statisticsMap;
	map<string, int>	statisticsMapSum;
};


struct KFState : KFState_
{
	mutex kfStateMutex;

	static const KFKey oneKey;

	KFState(
		const KFState &kfState)
	:	KFState_		(kfState),
		kfStateMutex	()
	{
		//dont use same rts file unless explicitly copied
		rts_basename.clear();
	}

	KFState()
	{
		//initialise all filter state objects with a ONE element for later use.
		x			= VectorXd	::Ones(1);
// 		Z			= MatrixXd	::Ones(1,1);
		P			= MatrixXd	::Zero(1,1);
		dx			= VectorXd	::Zero(1);

		kfIndexMap[oneKey]	= 0;

		initFilterEpoch();
	}

	KFState& operator=(
		const KFState& kfState)
	{
		KFState_* thisKfState_ = (KFState_*)this;
		KFState_* thatKfState_ = (KFState_*)&kfState;

		*thisKfState_ = *thatKfState_;

		//dont use same rts file unless explicitly copied
		rts_basename.clear();

		return *this;
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & kfIndexMap;
		ar & time;
		ar & x;
		ar & dx;

		double num;
		int rows = P.rows();
		ar & rows;

		if (ARCHIVE::is_saving::value)
		{
			for (int i = 0; i <  P.rows();	i++)
			for (int j = 0; j <= i;			j++)
			{
				num = P(i,j);

				ar & num;
			}
		}
		else
		{
			P = MatrixXd(rows,rows);

			for (int i = 0; i <  P.rows();	i++)
			for (int j = 0; j <= i;			j++)
			{
				ar & num;

				P(i,j) = num;
				P(j,i) = num;
			}
		}
	}

	void	initFilterEpoch();

	int		getKFIndex(
		const	KFKey&		key)
	const;

	int		getNoiseIndex(
		const	KFKey&		key)
	const;

	bool	getKFValue(
		const	KFKey&		key,
				double&		value,
				double*		variance		= nullptr,
				double*		adjustment_ptr	= nullptr)
	const;

	bool	getKFSigma(
		const	KFKey&		key,
				double&		sigma);

	bool	addKFState(
		const	KFKey&			kfKey,
		const	InitialState&	initialState = {});

	void	setExponentialNoise(
		const	KFKey&			kfKey,
		const	Exponential		exponential);

	void setAccelerator(
		const	KFKey&			element,
		const	KFKey&			dotElement,
		const	KFKey&			dotDotElement,
		const	double			value,
		const	InitialState&	initialState = {});

	void	setKFTrans(
		const	KFKey&			dest,
		const	KFKey&			source,
		const	double			value,
		const	InitialState&	initialState = {});

	void	setKFTransRate(
		const	KFKey&			integral,
		const	KFKey&			rate,
		const	double			value,
		const	InitialState&	initialRateState		= {},
		const	InitialState&	initialIntegralState	= {});

	void	addNoiseElement(
		const	KFKey&			obsKey,
		const	double			variance);

	void	removeState(
		const	KFKey&			kfKey);

	void	noiseElementStateTransition();

	void	stateTransition(
		Trace&		trace,
		GTime		newTime,
		MatrixXd*	stm_ptr = nullptr);

	void	manualStateTransition(
		Trace&		trace,
		GTime		newTime,
		MatrixXd&	stm,
		MatrixXd&	procNoise);

	void	preFitSigmaCheck(
		Trace&			trace,
		KFMeas&			kfMeas,
		KFKey&			badStateKey,
		int&			badMeasIndex,
		KFStatistics&	statistics,
		int				begX,
		int				numX,
		int				begH,
		int				numH);

	void	postFitSigmaChecks(
		Trace&			trace,
		KFMeas&			kfMeas,
		VectorXd&		dx,
		int				iteration,
		KFKey&			badStateKey,
		int&			badMeasIndex,
		KFStatistics&	statistics,
		int				begX,
		int				numX,
		int				begH,
		int				numH);

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

	bool kFilter(
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
		Trace&			trace,
		string			suffix	= "",
		int				begX	=  0,
		int				numX 	= -1);

	void outputConditionNumber(
		Trace&		trace);

	void	outputCorrelations(
		Trace&		trace);

	void	outputMeasurements(
		Trace&		trace,
		KFMeas&		meas);

	bool	doStateRejectCallbacks(
		Trace&			trace,
		KFMeas&			kfMeas,
		KFKey&			badKey,
		bool			postFit);

	bool	doMeasRejectCallbacks(
		Trace&			trace,
		KFMeas&			kfMeas,
		int				badIndex,
		bool			postFit);

	void	filterKalman(
		Trace&					trace,
		KFMeas&					kfMeas,
		bool					innovReady			= false,
		vector<FilterChunk>*	filterChunkList_ptr	= nullptr);

	void	leastSquareInitStates(
		Trace&			trace,
		KFMeas&			kfMeas,
		bool			initCovars	= false,
		VectorXd*		dx			= nullptr,
		bool			innovReady	= false);

	KFMeas	combineKFMeasList(
		KFMeasEntryList&	kfEntryList,
		GTime				measTime = GTime::noTime(),
		MatrixXd*			noiseMatrix_ptr = nullptr);

	VectorXd getSubState(
		map<KFKey, int>&	kfKeyMap,
		MatrixXd*			covarMat_ptr	= nullptr,
		VectorXd*			adjustVec_ptr	= nullptr)
	const;

	void getSubState(
		map<KFKey, int>&	kfKeyMap,
		KFState&			kfState)
	const;

	KFState getSubState(
		vector<KF>)
	const;

	void	removeState(
		const	KFKey&			kfKey)
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);			kfState.removeState		(kfKey);
	}

	void setExponentialNoise(
		const	KFKey&			kfKey,
		const	Exponential		exponential)
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);			kfState.setExponentialNoise	(kfKey, exponential);
	}

	void addNoiseElement(
		const	KFKey&			kfKey,
		const	double			variance)
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);			kfState.addNoiseElement	(kfKey, variance);
	}

	void addNoiseEntry(
		const	KFKey&			kfKey,
		const	double			value,
		const	double			variance)
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);			kfState.addNoiseEntry	(kfKey, value, variance);
	}

	bool 	addKFState(
		const	KFKey&			kfKey,
		const	InitialState&	initialState = {})
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);	return	kfState.addKFState		(kfKey, initialState);
	}

	void	setKFTrans(
		const	KFKey&			dest,
		const	KFKey&			source,
		const	double			value,
		const	InitialState&	initialState = {})
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);			kfState.setKFTrans		(dest, source, value, initialState);
	}

	void	setKFTransRate(
		const	KFKey&			integral,
		const	KFKey&			rate,
		const	double			value,
		const	InitialState&	initialRateState		= {},
		const	InitialState&	initialIntegralState	= {})
	const
	{
		auto& kfState = *const_cast<KFState*>(this);	lock_guard<mutex> guard(kfState.kfStateMutex);	kfState.setKFTransRate	(integral, rate, value, initialRateState, initialIntegralState);
	}
};

/** Object to hold an individual measurement.
* Includes the measurement itself, (or its innovation) and design matrix entries
* Adding design matrix entries for states that do not yet exist will create and add new states to the measurement's kalman filter object.
*/
struct KFMeasEntry
{
			KFState*					kfState_ptr			= nullptr;			///< Pointer to filter object that measurements are referencing
	const	KFState*					constKfState_ptr	= nullptr;			///< Pointer to filter object that measurements are referencing

	double valid	= true;			///< Optional parameter to invalidate a measurement (to avoid needing to delete it and reshuffle a vector)
	double value	= 0;			///< Value of measurement (for linear systems)
	double noise	= 0;			///< Noise of measurement
	double innov	= 0;			///< Innovation of measurement (for non-linear systems)
	KFKey obsKey	= {};			///< Optional labels to be used in output traces

	map<E_Component, ComponentsDetails> componentsMap;

	map<KFKey,	double>		designEntryMap;
	map<KFKey,	double>		usedValueMap;
	map<KFKey,	double>		noiseEntryMap;
	map<string,	void*>		metaDataMap;

	KFMeasEntry(
				KFState*	kfState_ptr,
				KFKey		obsKey		= {})
	:	kfState_ptr			(kfState_ptr),
		obsKey				(obsKey)
	{

	}

	KFMeasEntry(
		const	KFState*	constKfState_ptr,
				KFKey		obsKey		= {})
	:	constKfState_ptr	(constKfState_ptr),
		obsKey				(obsKey)
	{

	}

	KFMeasEntry()
	{

	}

	/** Adds a noise element for this measurement
	*/
	void addNoiseEntry(
		const	KFKey			kfKey,				///< Key to determine the origin of the noise
		const	double			value,				///< Noise entry matrix entry value
		const	double			variance)			///< Variance of noise element
	{
		if	( value		== 0
			||variance	<= 0)
		{
			return;
		}
		if (kfState_ptr)		{	kfState_ptr		->addNoiseElement(kfKey, variance);		}
		if (constKfState_ptr)	{	constKfState_ptr->addNoiseElement(kfKey, variance);		}

		noiseEntryMap[kfKey] += value;
	}

	/** Adds a design matrix entry for this measurement
	*/
	void addDsgnEntry(
		const	KFKey&					kfKey,						///< Key to determine which state parameter is affected
		const	double					value,						///< Design matrix entry value
		const	InitialState&			initialState	= {})		///< Initial conditions for new states
	{
		if (value == 0)
		{
			return;
		}

		if (initialState.Q < 0)
		{
			addNoiseEntry(kfKey, value, initialState.P);
			return;
		}

		bool retval = false;
		if (kfState_ptr)		{	kfState_ptr		->addKFState(kfKey, initialState);		}
		if (constKfState_ptr)	{	constKfState_ptr->addKFState(kfKey, initialState);		}

		usedValueMap[kfKey] = initialState.x;
		designEntryMap[kfKey] += value;
	}

	/** Adds the measurement noise entry for this measurement
	*/
	void setNoise(
		const	double value)		///< Measurement noise matrix entry value
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
		const	double value)		///< Actual measurement entry value
	{
		this->value = value;
	}

	/** Adds the innovation value for this measurement
	*/
	void setInnov(
		const	double value)		///< Innovation entry value
	{
		this->innov = value;
	}
};

KFState mergeFilters(
	const vector<KFState*>&	kfStatePointerList,
	const vector<KF>&		stateList);

MatrixXi correlationMatrix(
	MatrixXd& P);

void outputResiduals(
	Trace&			trace,
	KFMeas&			kfMeas,
	int				iteration,
	string			suffix,
	int				begH,
	int				numH);


bool isPositiveSemiDefinite(
	MatrixXd& mat);

int filter_(const double *x, const double *P, const double *H,
				const double *v, const double *R, int n, int m,
				double *xp, double *Pp);

// matrix and vector functions
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


