
#include <functional>
#include <utility>
#include <list>

using std::list;
using std::pair;


#include "eigenIncluder.hpp"
#include "algebraTrace.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "mongo.hpp"


/** \file
* This software uses specialised kalman filter classes to perform filtering.
* Using a classes such as KFState, KFMeas, etc, prevents duplication of code, and ensures that many edge cases are taken care of
* without the need for the developer to consider them explicitly.
*
* The basic workflow for using the filter is:
* create filter object,
* initialise the state transition matrix at the beginning of each epoch
* create a list of measurements (only adding entries for required states, using KFKeys to reference the state element)
* combining measurements that were in a list into a single matrix corresponding to the new state,
* and filtering - filtering internally saves the states for RTS code, using a single sequential file that has some headers added so that it can be traversed backwards
*
* The filter has some pre/post fit checks that remove measurements that are out of expected ranges,
* and there are functions provided for setting, resetting, and getting values, noises, and covariance values.
*
* KFKeys are used to identify states. They may have a type, SatStat value, string, and number associated with them, and can be set and read from filter objects as required.
*
* KFMeasEntrys are used for an individual measurement, before being combined into KFMeas objects that contain all of the measurements for a filter iteration.
*
* Internally, the data is stored in maps and Eigen matrices/vectors, but the accessors should be used rather than the vectors themselves to ensure that states have been initialised and are in the expected order.
*/


bool KFKey::operator ==(const KFKey& b) const
{
	if (str.compare(b.str)	!= 0)		return false;
	if (Sat					!= b.Sat)	return false;
	if (type				!= b.type)	return false;
	if (num					!= b.num)	return false;
	else								return true;
}

bool KFKey::operator <(const KFKey& b) const
{
	int strCompare = str.compare(b.str);
	if (type < b.type)		return true;
	if (type > b.type)		return false;

	if (strCompare < 0)		return true;
	if (strCompare > 0)		return false;

	if (Sat < b.Sat)		return true;
	if (Sat > b.Sat)		return false;

	if (num < b.num)		return true;
	else					return false;
}


/** Clears and initialises the state transition matrix to identity at the beginning of an epoch.
* Also clears any noise that was being added for the initialisation of a new state.
*/
void KFState::initFilterEpoch()
{
	stateTransitionMap.	clear();
	initNoiseMap.		clear();

	for (auto& [kfKey, index] : kfIndexMap)
	{
		stateTransitionMap[kfKey][kfKey] = 1;
	}
}

/** Finds the position in the KF state vector of particular states.
*/
int KFState::getKFIndex(
	KFKey		key)		///< [in]	Key to search for in state
{
	auto index = kfIndexMap.find(key);
	if (index == kfIndexMap.end())
	{
		return -1;
	}
	return index->second;
}

/** Returns the value and variance of a state within the kalman filter object
*/
bool KFState::getKFValue(
	KFKey		key,			///< [in]	Key to search for in state
	double&		value,			///< [out]	Output value
	double*		variance)		///< [out]	Output variance
{
	auto a = kfIndexMap.find(key);
	if (a == kfIndexMap.end())
	{
//		std::cout << std::endl << "Warning: State not found in filter: " << key << std::endl;
		return false;
	}
	int index = a->second;
	if (index >= x.size())
	{
		return false;
	}
	value = x(index);

	if (variance)
	{
		*variance = P(index,index);
	}

	return true;
}

/** Returns the standard deviation of a state within the kalman filter object
*/
bool KFState::getKFSigma(
	KFKey		key,		///< [in]	Key to search for in state
	double&		sigma)		///< [out]	Output value
{
	auto a = kfIndexMap.find(key);
	if (a == kfIndexMap.end())
	{
		return false;
	}
	int index = a->second;
	if (index >= x.size())
	{
		return false;
	}

	sigma = sqrt(P(index,index));
	return true;
}

/** Sets the value of a state within the kalman filter object
*/
bool KFState::setKFValue(
	KFKey		key,		///< [in]	Key to search for in state
	double		value)		///< [in]	Input value
{
	auto a = kfIndexMap.find(key);
	if (a == kfIndexMap.end())
	{
		return false;
	}

	int index = a->second;
	if (index >= x.size())
	{
		return false;
	}

	x(index) = value;

	return true;
}

/** Sets the process noise of a state within the kalman filter object
*/
bool KFState::setKFNoise(
	KFKey		kfKey,		///< [in]	Key to search for in state
	double		value)		///< [in]	Input value
{
	procNoiseMap[kfKey] = value;

	return true;
}

/** Adds dynamics to a filter state by inserting off-diagonal, time dependent elements to transition matrix
*/
void KFState::setKFTransRate(
	KFKey			dest,	   		///< [in]	Key to search for in state to change in transition
	KFKey			source,	   		///< [in]	Key to search for in state as source
	double			value,   		///< [in]	Input value
	InitialState	initialState)	///< [in]	Initial state for rate state.
{
	addKFState(source, initialState);

	rateTransitionMap[dest][source] = value;
}

/** Adds dynamics to a filter state by inserting off-diagonal, non-time dependent elements to transition matrix
*/
void KFState::setKFTrans(
	KFKey			dest,	   		///< [in]	Key to search for in state to change in transition
	KFKey			source,	   		///< [in]	Key to search for in state as source
	double			value,   		///< [in]	Input value
	InitialState	initialState)	///< [in]	Initial state for rate state.
{
	addKFState(source, initialState);

	stateTransitionMap[dest][source] = value;
}
/** Sets time constant Tau used in first-order Gauss-Markov state transition & process noise models
*/
void KFState::setKFGaussMarkovTau(
	KFKey			kfKey,	   		///< [in]	Key to search for in state
	double			tau)   			///< [in]	Input value
{
	gaussMarkovTauMap[kfKey] = tau;
}

/** Remove a state from a kalman filter object.
*/
void KFState::removeState(
	KFKey			kfKey)				///< [in]	Key to search for in state
{
	stateTransitionMap.	erase(kfKey);
	rateTransitionMap.	erase(kfKey);
	procNoiseMap.		erase(kfKey);
	stateClampMaxMap.	erase(kfKey);
	stateClampMinMap.	erase(kfKey);
	gaussMarkovTauMap.	erase(kfKey);
}

/** Reinitialise a state within a kalman filter object.
*/
void KFState::resetKFValue(
	KFKey			kfKey,				///< [in]	Key to search for in state
	InitialState	initialState)       ///< [in]	Initial values to reset the state with
{
	removeState	(kfKey);
	addKFState	(kfKey, initialState);
}

/** Tries to add a state to the filter object.
*  If the state already exists, it simply returns the index that refers to it.
*  If it does not exist, it adds it to a list of states to be added.
*  Call consolidateKFState() to apply the list to the filter object
*/
void KFState::addKFState(
	KFKey			kfKey,			///< [in]	The key to add to the state
	InitialState	initialState)	///< [in]	The initial conditions to add to the state
{
	auto iter = stateTransitionMap.find(kfKey);
	if (iter != stateTransitionMap.end())
	{
		//is an existing state, just update process noise value
		procNoiseMap[kfKey]	= initialState.Q;

		return;
	}

	//this is a new state, add to the state transition matrix to create a new state.

	KFKey ONE = {KF::ONE};
	stateTransitionMap	[kfKey][ONE]	= initialState.x;
	initNoiseMap		[kfKey]			= initialState.P;
	procNoiseMap		[kfKey]			= initialState.Q;

	if (initialState.clampMax != std::numeric_limits<double>::max())
	{
		stateClampMaxMap[kfKey]			= initialState.clampMax;
	}
	if (initialState.clampMin != std::numeric_limits<double>::lowest())
	{
		stateClampMinMap[kfKey]			= initialState.clampMin;
	}

	if (initialState.P == 0)
	{
		//will be an uninitialised variable, do a least squares solution
		lsqRequired = true;
	}
}

/** Limit state values according to configured parameters
*/
void KFState::clampStateValues()
{
	VectorXd clampMaxVec = VectorXd::Ones(x.rows()) * std::numeric_limits<double>::max();
	VectorXd clampMinVec = VectorXd::Ones(x.rows()) * std::numeric_limits<double>::lowest();

	for (auto& [kfKey, clampMax] : stateClampMaxMap)
	{
		int index = getKFIndex(kfKey);
		if (index < 0)
		{
			continue;
		}

		clampMaxVec(index) = clampMax;
	}

	for (auto& [kfKey, clampMin] : stateClampMinMap)
	{
		int index = getKFIndex(kfKey);
		if (index < 0)
		{
			continue;
		}

		clampMinVec(index) = clampMin;
	}

	x = x.cwiseMax(clampMinVec).cwiseMin(clampMaxVec);
}

/** Add process noise and dynamics to filter object according to time gap.
 * This will also sort states according to their kfKey as a result of the way the state transition matrix is generated.
 */
void KFState::stateTransition(
	Trace&		trace,		///< [out]	Trace file for output
	GTime		newTime)	///< [in]	Time of update for process noise and dynamics (s)
{
	KFState& kfState = *this;
	
	if (time == GTime::noTime())
	{
		time = newTime;
	}
	
	double tgap = newTime.time - time.time;
	time = newTime;
	
//	TestStack ts(__FUNCTION__);

	int newStateCount = stateTransitionMap.size();
	if (newStateCount == 0)
	{
		return;
	}

	//if we're part way through a filter iteration, make sure we finish the old one before starting the new one, or it will crash rts.
	if	(rtsFilterInProgress)
	{
		//add some states to prvent bad things happening.
		spitFilterToFile(kfState, E_SerialObject::FILTER_MINUS,	kfState.rts_forward_filename);
		spitFilterToFile(kfState, E_SerialObject::FILTER_PLUS,	kfState.rts_forward_filename);
		rtsFilterInProgress = false;
	}

	//Initialise and populate a state transition matrix
	MatrixXd F = MatrixXd::Zero(newStateCount, x.rows());

	//add transitions for any states (usually close to identity)
	int row = 0;
	map<KFKey, short int> newKFIndexMap;
	for (auto& [kfKey1, map] : stateTransitionMap)
	{
		newKFIndexMap[kfKey1] = row;

		for (auto& [kfKey2, value] : map)
		{
			int index2	= getKFIndex(kfKey2);

			if	( (index2 < 0)
				||(index2 >= F.cols()))
			{
				continue;
			}

			F(row, index2) = value;
		}
		row++;
	}

	//add transitions for states using a first-order Gauss-Markov model 
	for (auto& [kfKey, tau] : gaussMarkovTauMap)
	{
		auto iter = newKFIndexMap.find(kfKey);
		if (iter == newKFIndexMap.end())
		{
			std::cout << kfKey << " broke" << std::endl;        //todo aaron, this might have been unmeasured, need to add
			continue;
		}
		int row    = iter->second;

		int index2	= getKFIndex(kfKey);
		if	( (index2 < 0)
			||(index2 >= F.cols()))
		{
			continue;
		}
		F(row, index2) = exp(-tgap/tau); // Ref: El-Mowafy (2011) - Dynamic Modelling of Zenith Wet Delay in GNSS Measurements - https://ro.ecu.edu.au/cgi/viewcontent.cgi?referer=&httpsredir=1&article=1789&context=ecuworks2011
	}

	//add transitions for any dynamics that are scaled by time
	for (auto& [dest,	map] : rateTransitionMap)
	for (auto& [source,	val] : map)
	{
		auto iter = newKFIndexMap.find(dest);
		if (iter == newKFIndexMap.end())
		{
			std::cout << dest << " broke" << std::endl;		//todo aaron, this might have been unmeasured, need to add
			continue;
		}

		int index1	= iter->second;
		int index2	= getKFIndex(source);

		if	( (index1 < 0)
			||(index2 < 0)
			||(index1 >= F.rows())
			||(index2 >= F.cols()))
		{
			continue;
		}

		F(index1, index2) = val * tgap;
	}

	//scale and add process noise
	MatrixXd Q0 = MatrixXd::Zero(newStateCount, newStateCount);
	tgap = fabs(tgap);

	//add noise as 'process noise' as the method of initialising a state's variance
	for (auto& [kfKey, value]	: initNoiseMap)
	{
		auto iter = newKFIndexMap.find(kfKey);
		if (iter == newKFIndexMap.end())
		{
			std::cout << kfKey << " broke" << std::endl;
			continue;
		}
		int index	= iter->second;

		if	( (index < 0)
			||(index >= Q0.rows()))
		{
			continue;
		}

		Q0(index, index) = value;
	}

	//add time dependent process noise
	for (auto& [kfKey, value]	: procNoiseMap)
	{
		auto initIter = initNoiseMap.find(kfKey);
		if (initIter != initNoiseMap.end())
		{
			//this was initialised this epoch
			double init = initIter->second;

			if (init == 0)
			{
				//this was initialised with no noise, do lsq, dont add process noise
				continue;
			}
		}

		auto iter = newKFIndexMap.find(kfKey);
		if (iter == newKFIndexMap.end())
		{
			std::cout << kfKey << " broke" << std::endl;
			continue;
		}
		int index	= iter->second;

		if	( (index < 0)
			||(index >= Q0.rows()))
		{
			continue;
		}

		// add noise for states using first-order Gauss-Markov model (overrides regular time dependent process noise)
		auto gmIter = gaussMarkovTauMap.find(kfKey);
		if (gmIter != gaussMarkovTauMap.end())
		{
			double tau = gmIter->second;
			Q0(index, index) += value * (1-exp(-2*tgap/tau)) * tgap;  // Ref: El-Mowafy (2011) - Dynamic Modelling of Zenith Wet Delay in GNSS Measurements - https://ro.ecu.edu.au/cgi/viewcontent.cgi?referer=&httpsredir=1&article=1789&context=ecuworks2011
			continue;
		}

		Q0(index, index) += value * tgap;
	}

	//add process noise according to the uncertainty in dynamical states
	for (auto& [dest,	map] : rateTransitionMap)
	for (auto& [source,	val] : map)
	{
		auto initIter = initNoiseMap.find(dest);
		if (initIter != initNoiseMap.end())
		{
			//this was initialised this epoch
			double init = initIter->second;

			if (init == 0)
			{
				//this was initialised with no noise, do lsq, dont add process noise
				continue;
			}
		}

		auto iter1 = newKFIndexMap.find(dest);
		if (iter1 == newKFIndexMap.end())
		{
			std::cout << dest << " broke" << std::endl;
			continue;
		}

		auto iter2 = newKFIndexMap.find(source);
		if (iter2 == newKFIndexMap.end())
		{
			std::cout << source << " broke" << std::endl;
			continue;
		}

		int index1	= iter1->second;
		int index2	= iter2->second;

		if	( (index1 < 0)
			||(index2 < 0)
			||(index1 >= Q0.rows())
			||(index2 >= Q0.cols()))
		{
			continue;
		}

		double sourceNoise = Q0(index2, index2) * val;
// 		Q0(index1, index2) += sourceNoise / 2 * tgap * tgap;
// 		Q0(index2, index1) += sourceNoise / 2 * tgap * tgap;
		Q0(index1, index1) += sourceNoise / 3 * tgap * tgap * tgap;
	}

	//output the state transition matrix to a trace file (used by RTS smoother)
	if (rts_filename.empty() == false)
	{
		TransitionMatrixObject transitionMatrixObject;
		transitionMatrixObject.rows = F.rows();
		transitionMatrixObject.cols = F.cols();

		for (auto& [kfKey1, newIndex] : newKFIndexMap)
		for (auto& [kfKey2, oldIndex] : kfIndexMap)
		{
			double transition	= F(newIndex, oldIndex);

			//only output non zero elements to save space
			if	(transition	!= 0)
			{
				transitionMatrixObject.forwardTransitionMap[{newIndex, oldIndex}] = transition;
			}
		}
		spitFilterToFile(transitionMatrixObject,	E_SerialObject::TRANSITION_MATRIX,	rts_forward_filename);
		rtsFilterInProgress = true;
	}

	//compute the updated states and covariance matrices
	x = (F * x							).eval();
	P = (F * P * F.transpose()	+ Q0	).eval();

	//replace the index map with the updated version that corresponds to the updated state
	kfIndexMap = newKFIndexMap;
}

/** Compare variances of measurements and filtered states to detect unreasonable values
*/
int KFState::postFitSigmaCheck(
	Trace&		trace,      ///< Trace file to output to
	KFMeas&		kfMeas,		///< Measurements, noise, and design matrix
	VectorXd&	xp,         ///< The post-filter state vector to compare with measurements
	VectorXd&	dx)			///< The innovations from filtering to recalculate the deltas.
{
	MatrixXd&	H = kfMeas.A;
	VectorXd	v = kfMeas.V	- H * dx;

	//use 'array' for component-wise calculations
	auto		variations		= v.array().square();	//delta squared
	auto		variances		= kfMeas.R.array();
	auto		ratios			= variations / variances;
	auto		outsideExp		= ratios > SQR(4);
// 	double		meanVariations	= variations.mean();

	if (output_residuals)
	{
		trace << std::endl << "+ Residuals" << std::endl;
		tracepdeex(2, trace, "* %20s %13s %13s %16s\n", "Obs Name", "Prefit Res", "Postfit Res", "Meas Variance");

		for (int i = 0; i < kfMeas.V.rows(); i++)
		{
			tracepdeex(2, trace, "* %20s %13.4f %13.4f %16.9f\n", ((string)kfMeas.obsKeys[i]).c_str(), kfMeas.V(i), v(i), kfMeas.R(i));
		}
		trace << "- Residuals" << std::endl;
	}

#	ifdef ENABLE_MONGODB
	if (acsConfig.output_mongo_measurements)
	{
		mongoMeasResiduals(kfMeas.obsKeys, kfMeas.V, v, kfMeas.R);
	}
#	endif

	trace << std::endl << "DOING SIGMACHECK: ";
// 	std::cout << std::endl << "meanVariations+: " << meanVariations << std::endl;

	//if any are outside the expected value, flag an error
	if (outsideExp.any())
	{
		Eigen::ArrayXd::Index index;

		trace << "LARGE ERROR OF " << ratios.maxCoeff(&index) << " AT " << index << " : " << kfMeas.obsKeys[index];

		return index;
	}
	return -1;
}

/** Compare variances of measurements and pre-filtered states to detect unreasonable values
*/
int KFState::preFitSigmaCheck(
	Trace&		trace,		///< Trace to output to
	KFMeas&		kfMeas)		///< Measurements, noise, and design matrix
{
	auto	v = kfMeas.V;
	auto	R = kfMeas.R;
	auto	H = kfMeas.A;
	auto	P = this->P;

	//use 'array' for component-wise calculations
	MatrixXd	HPHt		= H * P * H.transpose();
	auto		variations	= v.array().square();	//delta squared
	auto		variances	= (R + (H * P * H.transpose()).diagonal()).array();

	auto		ratios		= variations / variances;
	auto		outsideExp	= ratios > SQR(4);

// 	double		meanRatios		= ratios.mean();
// 	double		meanVariations	= variations.mean();

	trace << std::endl << "DOING PRE SIGMA CHECK: ";
// 	std::cout << std::endl << "meanVariations-: " << meanVariations << std::endl;

	//if any are outside the expected value, flag an error
	if (outsideExp.any())
	{
		Eigen::ArrayXd::Index index;

		trace << "LARGE ERROR OF " << ratios.maxCoeff(&index) << " AT " << index << " : " << kfMeas.obsKeys[index];

		return index;
	}

	return -1;
}

/** Kalman filter.
*/
int KFState::kFilter(
	Trace&			trace,		///< Trace to output to
	KFMeas&			kfMeas,		///< Measurements, noise, and design matrices
	VectorXd&		xp,   		///< Post-update state vector
	MatrixXd&		Pp,   		///< Post-update covariance of states
	VectorXd&		dx)			///< Post-update state innovation
{
	auto& H = kfMeas.A;
	auto& R = kfMeas.R;
	auto& v = kfMeas.V;

	MatrixXd HP	= H		* P;
	MatrixXd Q	= HP	* H.transpose();

	Q += R.asDiagonal();

	MatrixXd K;

	bool repeat = true;
	while (repeat)
	{
		switch (inverter)
		{
			default:
			case E_Inverter::LDLT:
			{
				auto QQ = Q.triangularView<Eigen::Upper>().adjoint();
				LDLT<MatrixXd> solver;
				solver.compute(QQ);
				if (solver.info() != Eigen::ComputationInfo::Success)
				{
					xp = x;
					Pp = P;
					dx = VectorXd::Zero(xp.rows());

					return 1;
				}

				auto Kt = solver.solve(HP);
				if (solver.info() != Eigen::ComputationInfo::Success)
				{
					tracepdeex(1, trace, "Warning: kalman filter error2\n");
					xp = x;
					Pp = P;
					dx = VectorXd::Zero(xp.rows());

					return 1;
				}

				K = Kt.transpose();

				break;
			}
			case E_Inverter::LLT:
			{
				auto QQ = Q.triangularView<Eigen::Upper>().adjoint();
				LDLT<MatrixXd> solver;
				solver.compute(QQ);
				if (solver.info() != Eigen::ComputationInfo::Success)
				{
					inverter = E_Inverter::LDLT;
					continue;
				}

				auto Kt = solver.solve(HP);
				if (solver.info() != Eigen::ComputationInfo::Success)
				{
					inverter = E_Inverter::LDLT;
					continue;
				}

				K = Kt.transpose();

				break;
			}
			case E_Inverter::INV:
			{
				MatrixXd Qinv = Q.inverse();
				K = P * H.transpose() * Qinv;

				break;
			}
		}
		repeat = false;
	}

	dx = K * v;
	xp = x + dx;
	
	if (acsConfig.joseph_stabilisation)
	{
		MatrixXd IKH = MatrixXd::Identity(P.rows(), P.cols()) - K * H;
		Pp = IKH * P * IKH.transpose() + K * R.asDiagonal() * K.transpose();
	}
	else
	{
		Pp = P - K * HP;
		Pp = (Pp + Pp.transpose()).eval() / 2;
	}


	bool error = xp.array().isNaN().any();
	if (error)
	{
		std::cout << std::endl << "xp:" << std::endl << xp << std::endl;
		std::cout << std::endl << "R :" << std::endl << R << std::endl;
		std::cout << std::endl << "v :" << std::endl << v << std::endl;
		std::cout << std::endl << "K :" << std::endl << K << std::endl;
		std::cout << std::endl << "P :" << std::endl << P << std::endl;
		std::cout << std::endl;
		std::cout << "NAN found. Exiting...";
		std::cout << std::endl;

		exit(0);
	}

	bool pass = true;
	return pass;
}

/** Perform chi squared quality control.
*/
bool KFState::chiQC(
	Trace&		trace,      ///< Trace to output to
	KFMeas&		kfMeas,		///< Measurements, noise, and design matrix
	VectorXd&	xp)         ///< Post filtered state vector
{
	auto& H = kfMeas.A;
	auto W = kfMeas.W(Eigen::all, 0);

	VectorXd&	y		= kfMeas.Y;
	VectorXd	v		= y - H * xp;
	double		v_Wv	= v.transpose() * W.asDiagonal() * v;

	//trace << std::endl << "chiqcV" << v.rows() << std::endl;	tracematpde(5, trace, v, 15, 5);

	int obsNumber = v.rows() - x.rows() + 1;
	double val = v_Wv / (obsNumber);

	double thres;
	if (obsNumber <= 100) 	thres = chisqr[obsNumber - 1] / (obsNumber);
	else					thres = 3;

	/* chi-square validation */
	if (val > thres)
	{
		tracepdeex(1, trace, "\n ChiSquare error detected");

		auto variations	= v.array().square();
// 		Eigen::MatrixXf::Index index;
// 		trace << " -> LARGEe ERROR OF " << sqrt(variations.maxCoeff(&index)) << " AT " << index;

		return false;
	}

	return true;
}

/** Combine a list of KFMeasEntrys into a single KFMeas object for used in the filter
*/
KFMeas KFState::combineKFMeasList(
	KFMeasEntryList&	kfEntryList)	///< [in]	List of input measurements as lists of entries
{
	int numMeas = kfEntryList.size();

	KFMeas kfMeas;

	kfMeas.R.resize(numMeas);
	kfMeas.V.resize(numMeas);
	kfMeas.Y.resize(numMeas);

	kfMeas.A = MatrixXd::Zero(numMeas, x.rows());

	kfMeas.obsKeys		.resize(numMeas);
	kfMeas.metaDataMaps	.resize(numMeas);

	int meas = 0;
	for (auto& entry: kfEntryList)
	{
		kfMeas.R(meas) = entry.noise;
		kfMeas.Y(meas) = entry.value;
		kfMeas.V(meas) = entry.innov;

		for (auto& [kfKey, value] : entry.designEntryMap)
		{
			int index = getKFIndex(kfKey);
			if (index < 0)
			{
				std::cout << "hhuh?" << kfKey << std::endl;
				return KFMeas();
			}
			kfMeas.A(meas, index) = value;
		}

		kfMeas.obsKeys		[meas] = std::move(entry.obsKey);
		kfMeas.metaDataMaps	[meas] = std::move(entry.metaDataMap);
		meas++;
	}

	return kfMeas;
}

void KFState::doRejectCallbacks(
	Trace&		trace,				///< Trace file for output
	KFMeas&		kfMeas,				///< Measurements that were passed to the filter
	int			badIndex)			///< Index in measurement list that was unsatisfactory
{
	for (auto& callback : rejectCallbacks)
	{
		bool keepGoing = callback(trace, *this, kfMeas, badIndex);

		if (keepGoing == false)
		{
			return;
		}
	}
}

bool ObsKey::operator ==(const ObsKey& b) const
{
	if (str.compare(b.str)	!= 0)		return false;
	if (Sat					!= b.Sat)	return false;
	if (type.compare(b.type)!= 0)		return false;
	else								return true;
}

bool ObsKey::operator <(const ObsKey& b) const
{
	int typeCompare = type.compare(b.type);
	if (typeCompare < 0)		return true;
	if (typeCompare > 0)		return false;

	int strCompare = str.compare(b.str);
	if (strCompare < 0)		return true;
	if (strCompare > 0)		return false;

	if (Sat < b.Sat)		return true;
	if (Sat > b.Sat)		return false;

	else					return false;
}

/** Kalman filter operation
*/
int KFState::filterKalman(
	Trace&			trace,				///< [out]	Trace file for output
	KFMeas&			kfMeas,				///< [in]	Measurement object
	bool			innovReady)			///< [in]	Innovation already constructed
{
	KFState& kfState = *this;

	kfState.time = kfMeas.time;

	if (kfState.rts_filename.empty() == false)
	{
		spitFilterToFile(kfState, E_SerialObject::FILTER_MINUS, kfState.rts_forward_filename);
	}

	if (kfMeas.A.rows() == 0)
	{
		//nothing to be done, clean up and return early

		if (kfState.rts_filename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_PLUS, kfState.rts_forward_filename);
			rtsFilterInProgress = false;
		}
		return 1;
	}

	/* kalman filter measurement update */
	if (innovReady == false)
	{
		kfMeas.V = kfMeas.Y - kfMeas.A * kfState.x;
	}

	for (int i = 0; i < max_prefit_remv; i++)
	{
//		cout << "A" << endl;
//		cout << kfState.x.size() << endl;
//		cout << kfState.P.size() << endl;
//		cout << kfMeas.Y.size() << endl;
//		cout << kfMeas.V.size() << endl;
		int badIndex = kfState.preFitSigmaCheck(trace, kfMeas);
		if (badIndex < 0)	{	trace << std::endl << "PreSigma check passed" << std::endl;									break;		}
		else				{	trace << std::endl << "PreSigma check failed.";	doRejectCallbacks(trace, kfMeas, badIndex);	continue;	}
	}

	MatrixXd Pp;
	VectorXd xp;

	for (int i = 0; i < max_filter_iter; i++)
	{
		bool pass = kfState.kFilter(trace, kfMeas, xp, Pp, dx);

		if (pass == false)
		{
			trace << "FILTER FAILED" << std::endl;
			return 0;
		}

		int badIndex = kfState.postFitSigmaCheck(trace, kfMeas, xp, dx);
		if (badIndex < 0)	{	trace << std::endl << "Sigma check passed" << std::endl;									break;		}
		else				{	trace << std::endl << "Sigma check failed.";	doRejectCallbacks(trace, kfMeas, badIndex);	continue;	}
	}

// 	if (pass)
	{
		kfState.x = std::move(xp);
		kfState.P = std::move(Pp);
	}

	clampStateValues();

	if (kfState.rts_filename.empty() == false)
	{
		spitFilterToFile(kfState, E_SerialObject::FILTER_PLUS, kfState.rts_forward_filename);
		rtsFilterInProgress = false;
	}
	return 1;
}

/** Perform least squares using the measurements in the KFMeas object
*/
int KFState::filterLeastSquares(
	Trace&			trace,		///< [in]		Trace to output to
	KFMeas&			kfMeas)		///< [in]		Measurements, noise, and design matrix
{
	KFState& kfState = *this;

	trace << std::endl << " -------STARTING LS --------" << std::endl;

	//invert measurement noise matrix to get a weight matrix
	kfMeas.W = (1 / kfMeas.R.array()).matrix();

	if (kfMeas.R.rows() < kfState.x.rows())
	{
		trace << std::endl << "INSUFFICIENT MEASUREMENTS FOR LEAST SQUARES " << kfMeas.R.rows() <<  " " << kfState.x.rows();
		return 0;
	}

	auto& H = kfMeas.A;
	auto& W = kfMeas.W;
	auto& Z = kfMeas.Y;

	//calculate least squares solution
	MatrixXd H_W	= H.transpose() * W.asDiagonal();
	MatrixXd Q		= H_W * H;

	trace << std::endl << " -------DOING LS --------"<< std::endl;

	MatrixXd Qinv = Q.inverse();
	VectorXd x1 = Qinv * H_W * Z;

	bool error = x1.array().isNaN().any();
	if (error)
	{
		std::cout << "NAN found. Exiting...";
		std::cout	<< std::endl
		<< x1		<< std::endl
		<< Qinv		<< std::endl
		<< Q		<< std::endl;

		exit(0);
	}

// 	std::cout << std::endl << "postLSQ" << std::endl;

	chiQCPass = kfState.chiQC(trace, kfMeas, x1);
	if (chiQCPass == false)
	{
// 		return 0;	//todo aaron
	}

	//copy results to outputs
	for (int i = 0; i < kfState.x.rows(); i++)
	{
		if (kfState.P(i,i) == 0)
		{
			kfState.P(i,i)	= Qinv(i,i);
			kfState.x(i)	= x1(i);
		}
	}

	trace << std::endl << " -------STOPPING LS --------"<< std::endl;
	return 1;
}

//todo aaron, remove tgap, add time to filter

/** Least squares estimator for new kalman filter states.
* If new states have been added that do not contain variance values, the filter will assume that these states values and covariances should be
* estimated using least squares.
*
* This function will extract the minimum required states from the existing state vector,
* and the minimum required measurements in order to perform least squares for the uninitialised states.
*/
void KFState::leastSquareInitStates(
	Trace&			trace,				///< [in]		Trace file for output
	KFMeas&			kfMeas,				///< [in]		Measurement object
	bool			initCovars,			///< [in]		Option to also initialise off-diagonal covariance values
	VectorXd*		dx)					///< [out]		Optional output of state deltas
{
	chiQCPass = false;

	vector<int> newStateIndicies;

	//find all the states that aren't initialised, they need least squaring.
	for (auto& [key, i] : kfIndexMap)
	{
		if	( (key.type != KF::ONE)
			&&(P(i,i) == 0))
		{
			//this is a new state and needs to be evaluated using least squares
			newStateIndicies.push_back(i);
		}
	}

	//get the subset of the measurement matrix that applies to the uninitialised states
	auto subsetA = kfMeas.A(Eigen::all, newStateIndicies);

	//find the subset of measurements that are required for the initialisation
	auto usedMeas = subsetA.rowwise().any();

	unordered_map<int, bool> pseudoMeasStates;
	vector<int> leastSquareMeasIndicies;

	for (int meas = 0; meas < usedMeas.rows(); meas++)
	{
		//if not used, dont worry about it
		if (usedMeas(meas) == 0)
		{
			continue;
		}

		//this measurement is used to calculate a new state.
		//copy it to a new design matrix
		leastSquareMeasIndicies.push_back(meas);

		//make a pseudo measurement of anything it references that is already set
		for (int state = 0; state < kfMeas.A.cols(); state++)
		{
			if	( (kfMeas.A(meas, state)	!= 0)
				&&(P(state,state)			!= 0))
			{
				pseudoMeasStates[state] = true;
			}
		}
	}

	int newMeasCount = leastSquareMeasIndicies.size() + pseudoMeasStates.size();

	//Create new measurement objects with larger size, (using all states for now)
	KFMeas	leastSquareMeas;

	leastSquareMeas.Y = VectorXd::Zero(newMeasCount);
	leastSquareMeas.R = VectorXd::Zero(newMeasCount);
	leastSquareMeas.A = MatrixXd::Zero(newMeasCount, kfMeas.A.cols());

	int measCount = leastSquareMeasIndicies.size();

	//copy in the required measurements from the old set
	leastSquareMeas.Y.head		(measCount) = kfMeas.Y(leastSquareMeasIndicies);
	leastSquareMeas.R.head		(measCount) = kfMeas.R(leastSquareMeasIndicies);
	leastSquareMeas.A.topRows	(measCount) = kfMeas.A(leastSquareMeasIndicies, Eigen::all);

	//append any new pseudo measurements to the end
	for (auto& [state, boool] : pseudoMeasStates)
	{
		leastSquareMeas.Y(measCount)		= x(state);
		leastSquareMeas.R(measCount)		= P(state,state);
		leastSquareMeas.A(measCount, state)	= 1;
		measCount++;
	}

	//find the subset of states required for these measurements
	vector<int> usedCols;
	auto usedStates = leastSquareMeas.A.colwise().any();
	for (int i = 0; i < usedStates.cols(); i++)
	{
		if (usedStates(i) != 0)
		{
			usedCols.push_back(i);
		}
	}

	//create a new meaurement object using only the required states.
	KFMeas	leastSquareMeasSubs;
	leastSquareMeasSubs.Y = leastSquareMeas.Y;
	leastSquareMeasSubs.R = leastSquareMeas.R;
	leastSquareMeasSubs.A = leastSquareMeas.A(Eigen::all, usedCols);

	//invert measurement noise matrix to get a weight matrix
	leastSquareMeasSubs.W = (1 / leastSquareMeasSubs.R.array()).matrix();

	VectorXd w = (1 / leastSquareMeasSubs.R.array()).matrix().col(0);

	for (int i = 0; i < w.rows(); i++)
	{
		if (std::isinf(w(i)))
		{
			w(i) = 0;
		}
	}

	if (leastSquareMeasSubs.R.rows() < leastSquareMeasSubs.A.cols())
	{
		trace << std::endl << "INSUFFICIENT MEASUREMENTS FOR LEAST SQUARES " << leastSquareMeasSubs.R.rows() <<  " " << x.rows();
		return;
	}
	auto& H = leastSquareMeasSubs.A;
	auto& Z = leastSquareMeasSubs.Y;

	//calculate least squares solution
	MatrixXd W		= w.asDiagonal();
	MatrixXd H_W	= H.transpose() * W;
	MatrixXd Q		= H_W * H;

	MatrixXd Qinv	= Q.inverse();
	VectorXd x1		= Qinv * H_W * Z;

// 	std::cout << "Q : " << std::endl << Q;
	bool error = x1.array().isNaN().any();
	if (error)
	{
		std::cout << std::endl << "x1:" << std::endl << x1 << std::endl;
		std::cout << std::endl << "w :" << std::endl << w << std::endl;
		std::cout << std::endl << "H :" << std::endl << H << std::endl;
		std::cout << std::endl << "P :" << std::endl << P << std::endl;
		std::cout << std::endl;
		std::cout << "NAN found. Exiting....";
		std::cout	<< std::endl;

		exit(-1);
	}

// 	std::cout << std::endl << "postLSQ" << std::endl;
	chiQCPass = chiQC(trace, leastSquareMeasSubs, x1);
	if (chiQCPass == false)
	{
// 		return 1;	//todo aaron
	}

	if (dx)
	{
		(*dx) = x1;
	}

	for (int i = 0; i < usedCols.size(); i++)
	{
		int stateRowIndex = usedCols[i];

		if (P(stateRowIndex, stateRowIndex) != 0)
		{
			continue;
		}

		double newStateVal = x1(i);
		double newStateCov = Qinv(i,i);

		if (dx)
		{
			x(stateRowIndex)				+=	newStateVal;
			P(stateRowIndex,stateRowIndex)	=	newStateCov;
		}
		else
		{
			x(stateRowIndex)				= newStateVal;
			P(stateRowIndex,stateRowIndex)	= newStateCov;
		}


		if (initCovars)
		{
			for (int j = 0; j < i; j++)
			{
				int stateColIndex = usedCols[j];

				newStateCov = Qinv(i,j);

				P(stateRowIndex,stateColIndex)	= newStateCov;
				P(stateColIndex,stateRowIndex)	= newStateCov;
			}
		}
	}
}

/** Get a portion of the state vector by passing a list of keys
*/
VectorXd KFState::getSubState(
	list<KFKey>&	kfKeyList,	///< List of keys to return within substate
	MatrixXd*		covarMat)	///< Optional pointer to a matrix for output of covariance submatrix

{
	vector<int> indices;
	indices.reserve(kfKeyList.size());

	for (auto& kfKey : kfKeyList)
	{
		int index = getKFIndex(kfKey);
		if (index >= 0)
		{
			indices.push_back(index);
		}
	}

	VectorXd subState = x(indices);
	if (covarMat)
	{
		*covarMat = P(indices, indices);
	}
	return subState;
}

/** Output keys and states in human readable format
*/
void KFState::outputStates(
			Trace&		trace)   	///< Trace to output to
{
	tracepdeex(2, trace, "\n\n");

	trace << std::endl << "+ States" << std::endl;
	tracepdeex(2, trace, "* %20s %5s %3s %3s %13s %16s %10s\n", "Type", "Str", "Sat", "Num", "State", "Variance", "Adjust");

	for (auto& [key, index] : kfIndexMap)
	{
		if (index >= x.rows())
		{
			continue;
		}

		double _x	= x(index);
		double _dx = 0;
		if (index < dx.rows())
			_dx = dx(index);
		double _p	= P(index, index);
		string type	= KF::_from_integral(key.type)._to_string();

		tracepdeex(2, trace, "* %20s %5s %3s %3d %13.4f %16.9f %10.3f\n", type.c_str(), key.str.c_str(), key.Sat.id().c_str(), key.num, _x, _p, _dx);
	}
	trace << "- States" << std::endl;
}

InitialState initialStateFromConfig(
	KalmanModel&	kalmanModel,
	int				index)
{
	InitialState init = {};

	if (index < kalmanModel.apriori_val	.size())	init.x			= SQR(kalmanModel.apriori_val	[index]);
	else											init.x			= SQR(kalmanModel.apriori_val	.back());
	if (index < kalmanModel.sigma		.size())	init.P			= SQR(kalmanModel.sigma			[index]);
	else											init.P			= SQR(kalmanModel.sigma			.back());
	if (index < kalmanModel.proc_noise	.size())	init.Q			= SQR(kalmanModel.proc_noise	[index]);
	else											init.Q			= SQR(kalmanModel.proc_noise	.back());
	if (index < kalmanModel.clamp_max	.size())	init.clampMax	=    (kalmanModel.clamp_max		[index]);
	else											init.clampMax	=    (kalmanModel.clamp_max		.back());
	if (index < kalmanModel.clamp_min	.size())	init.clampMin	=    (kalmanModel.clamp_min		[index]);
	else											init.clampMin	=    (kalmanModel.clamp_min		.back());

	return init;
}

KFState mergeFilters(
	list<KFState*>& kfStatePointerList)
{
	map<KFKey, double>				stateValueMap;
	map<KFKey, map<KFKey, double>>	stateCovarMap;

	for (auto& statePointer : kfStatePointerList)
	{
		KFState& kfState = *statePointer;

		for (auto& [key1, index1] : kfState.kfIndexMap)
		{
			if	( key1.type != KF::REC_POS
				&&key1.type != KF::ONE)
			{
				continue;
			}

			stateValueMap[key1] = kfState.x(index1);

			for (auto& [key2, index2] : kfState.kfIndexMap)
			{
				if (key2.type != KF::REC_POS)
				{
					continue;
				}

				if (kfState.P(index1, index2) != 0)
				{
					stateCovarMap[key1][key2] = kfState.P(index1, index2);
				}
			}
		}
	}

	KFState mergedKFState;

	mergedKFState.x		= VectorXd::Zero(stateValueMap.size());
	mergedKFState.dx	= VectorXd::Zero(stateValueMap.size());
	mergedKFState.P		= MatrixXd::Zero(stateValueMap.size(), stateValueMap.size());

	int i = 0;
	for (auto& [key, value] : stateValueMap)
	{
		mergedKFState.kfIndexMap[key]	= i;
		mergedKFState.x(i)				= value;

		i++;
	}

	for (auto& [key1, map2]		: stateCovarMap)
	for (auto& [key2, value]	: map2)
	{
		int index1 = mergedKFState.kfIndexMap[key1];
		int index2 = mergedKFState.kfIndexMap[key2];

		mergedKFState.P(index1, index2) = value;
	}

	return mergedKFState;
}

/** Calculates prefit residual, returning results as a KFMeasEntryList
* KFMeasEntryMap[ObsKey].value = prefit residual value
* KFMeasEntryMap[ObsKey].noise = prefit residual uncertainty
* KFMeasEntryMap[ObsKey].innov = kfMeas.V(i)
*/
KFMeasEntryList KFState::calcPrefitResids(
	Trace&			trace,				///< [out]	Trace file for output
	KFMeas&			kfMeas)				///< [in]	Measurement object
{
	KFState& kfState = *this;

	// Calculate prefit resid & uncertainty S
	Eigen::VectorXd PrefitResid = kfMeas.Y - kfMeas.A * kfState.x;
	Eigen::MatrixXd S = kfMeas.A * kfState.P * kfMeas.A.transpose() + Eigen::MatrixXd(kfMeas.R.asDiagonal());
	int numMeas = PrefitResid.size();
	//assert(kfMeas.R.size() == numMeas);
	assert(S.rows() == numMeas);
	assert(S.cols() == numMeas);
	assert(kfMeas.V.size() == numMeas);

	// Return result within a KFMeasEntryList
	KFMeasEntryList kfMeasEntryList;
	for (int meas=0; meas<numMeas; ++meas)
	{
		KFMeasEntry entry;
		entry.value = PrefitResid(meas);
		//entry.noise = kfMeas.R(meas);
		entry.noise = S(meas,meas);
		entry.innov = kfMeas.V(meas);
		entry.obsKey = kfMeas.obsKeys.at(meas);
		kfMeasEntryList.push_back(entry);
	}
	return kfMeasEntryList;
}
