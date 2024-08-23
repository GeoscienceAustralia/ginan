
#include "architectureDocs.hpp"

/** Kalman Filter.
 *
 * This software uses specialised kalman filter classes to perform filtering.
 * Using classes such as KFState, KFMeas, etc, prevents duplication of code, and ensures that many edge cases are taken care of
 * without the need for the developer to consider them explicitly.
 *
 * The basic workflow for using the filter is:
 * create filter object,
 * create a list of measurements (only adding entries for required states, using KFKeys to reference the state element)
 * combine measurements that in a list into a single matrix corresponding to the new state,
 * and filtering - filtering internally saves the states for RTS code, using a single sequential file that has some headers added so that it can be traversed backwards
 *
 * The filter has some pre/post fit checks that remove measurements that are out of expected ranges,
 * and there are functions provided for setting, resetting, and getting values, noises, and covariance values.
 *
 * KFKey objects are used to identify states. They may have a KF type, SatSys value, string (usually used for receiver id), and number associated with them, and can be set and read from filter objects as required.
 *
 * KFMeasEntry objects are used for an individual measurement, before being combined into KFMeas objects that contain all of the measurements for a filter iteration.
 *
 * Internally, the data is stored in maps and Eigen matrices/vectors, but the accessors should be used rather than the vectors themselves to ensure that states have been initialised and are in the expected order.
 *
 * InitialState objects are created directly from yaml configurations, and contain the detailis about state transitions, including process noise, which are automatically added to the filter
 * when a stateTransition() call is used, scaling any process noise according to the time gap since the last stateTransition.
 *
 * $$ K = HPH^\intercal + R $$ fgdfg
 */
ParallelArchitecture Kalman_Filter__()
{
	DOCS_REFERENCE(Binary_Archive__);
}



#include <utility>
#include <sstream>

using std::ostringstream;
using std::pair;

#include <boost/math/distributions/chi_squared.hpp>
#include <boost/math/distributions/normal.hpp>

#include "interactiveTerminal.hpp"
#include "eigenIncluder.hpp"
#include "algebraTrace.hpp"
#include "mongoWrite.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "mongo.hpp"
#include "trace.hpp"

// #pragma GCC optimize ("O0")

const KFKey KFState::oneKey = {.type = KF::ONE};


bool KFKey::operator ==(const KFKey& b) const
{
	if (str		!= b.str)	return false;
	if (Sat		!= b.Sat)	return false;
	if (type	!= b.type)	return false;
	if (num		!= b.num)	return false;
	else					return true;
}

bool KFKey::operator <(const KFKey& b) const
{
	if (str < b.str)		return true;
	if (str > b.str)		return false;

	if (Sat < b.Sat)		return true;
	if (Sat > b.Sat)		return false;

	if (type < b.type)		return true;
	if (type > b.type)		return false;

	if (num < b.num)		return true;
	else					return false;
}


/** Finds the position in the noise vector of particular noise elements.
*/
int KFMeas::getNoiseIndex(
	const	KFKey&		key)		///< Key to search for in noise vector
const
{
	auto index = noiseIndexMap.find(key);
	if (index == noiseIndexMap.end())
	{
		return -1;
	}

	return index->second;
}

/** Clears and initialises the state transition matrix to identity at the beginning of an epoch.
* Also clears any noise that was being added for the initialisation of a new state.
*/
void KFState::initFilterEpoch()
{
	initNoiseMap.clear();

	for (auto& [key1, mapp]	: stateTransitionMap)
	{
		if (key1 == oneKey)
		{
			continue;
		}

		//remove initialisation elements for subsequent epochs
		mapp.erase(oneKey);
	}

	stateTransitionMap[oneKey][oneKey][0]	= 1;
}

/** Finds the position in the KF state vector of particular states.
*/
int KFState::getKFIndex(
	const	KFKey&		key)		///< Key to search for in state
const
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
E_Source KFState::getKFValue(
	const	KFKey&		key,			///< Key to search for in state
			double&		value,			///< Output value
			double*		variance_ptr,	///< Optional variance output
			double*		adjustment_ptr,	///< Optional adjustment output
			bool		allowAlternate)	///< Optional flag to disable alternate filter
const
{
	auto a = kfIndexMap.find(key);
	if (a == kfIndexMap.end())
	{
		if	( allowAlternate	== false
			||alternate_ptr		== nullptr)
		{
			return E_Source::NONE;
		}

		E_Source found = alternate_ptr->getKFValue(key, value, variance_ptr, adjustment_ptr);
		if (found)
			return E_Source::REMOTE;

		return  E_Source::NONE;
	}

	int index = a->second;
	if (index >= x.size())
	{
		return E_Source::NONE;
	}
	value = x(index);

	if (variance_ptr)
	{
		*variance_ptr	= P(index,index);
	}

	if (adjustment_ptr)
	{
		*adjustment_ptr	= dx(index);
	}

	return E_Source::KALMAN;
}

/** Returns the standard deviation of a state within the kalman filter object
*/
bool KFState::getKFSigma(
	const	KFKey&		key,		///< Key to search for in state
			double&		sigma)		///< Output value
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

void KFState::setAccelerator(
	const	KFKey&			element,
	const	KFKey&			dotElement,
	const	KFKey&			dotDotElement,
	const	double			value,
	const	InitialState&	initialState)
{
	addKFState(dotDotElement, initialState);

	//t^2 term
	stateTransitionMap[element]		[dotDotElement][2] = value;

	//t terms
	stateTransitionMap[dotElement]	[dotDotElement][1] = value;
}

/** Adds dynamics to a filter state by inserting off-diagonal, non-time dependent elements to transition matrix
*/
void KFState::setKFTrans(
	const	KFKey&			dest,			///< Key to search for in state to change in transition
	const	KFKey&			source,			///< Key to search for in state as source
	const	double			value,			///< Input value
	const	InitialState&	initialState)	///< Initial state.
{
	addKFState(dest, initialState);

	auto& transition = stateTransitionMap[dest][source][0];

	transition += value;
}

/** Adds dynamics to a filter state by inserting off-diagonal, time dependent elements to transition matrix
*/
void KFState::setKFTransRate(
	const	KFKey&			integralKey,			///< Key to search for in state to change in transition
	const	KFKey&			rateKey,				///< Key to search for in state as source
	const	double			value,					///< Input value
	const	InitialState&	initialRateState,		///< Initial state for rate state.
	const	InitialState&	initialIntegralState)	///< Initial state for the thing that is modified by the rate
{
	addKFState(rateKey,		initialRateState);
	addKFState(integralKey,	initialIntegralState);

	stateTransitionMap[integralKey][rateKey][1] = value;
}

/** Remove a state from a kalman filter object.
*/
void KFState::removeState(
	const	KFKey&			kfKey)				///< Key to search for in state
{
	stateTransitionMap.		erase(kfKey);
	procNoiseMap.			erase(kfKey);
	gaussMarkovTauMap.		erase(kfKey);
	gaussMarkovMuMap.		erase(kfKey);
	exponentialNoiseMap.	erase(kfKey);
}

/** Tries to add a state to the filter object.
*  If it does not exist, it adds it to a list of states to be added.
*  Call consolidateKFState() to apply the list to the filter object
*/
bool KFState::addKFState(
	const	KFKey&			kfKey,			///< The key to add to the state
	const	InitialState&	initialState)	///< The initial conditions to add to the state
{
	auto iter = stateTransitionMap.find(kfKey);
	if (iter != stateTransitionMap.end())
	{
		//is an existing state, just update values
		if (initialState.Q		!= 0)		{	procNoiseMap		[kfKey]	= initialState.Q;		}
		if (initialState.mu		!= 0)		{	gaussMarkovMuMap	[kfKey]	= initialState.mu;		}
		if (initialState.tau	!= 0)		{	gaussMarkovTauMap	[kfKey] = initialState.tau;		}

		return false;
	}

	//this should be a new state, add to the state transition matrix to create a new state.

	//check if it exists in the state though, identity STM causes double ups if its reinitialised too quickly
	double currentX = 0;
	auto it = kfIndexMap.find(kfKey);
	if (it != kfIndexMap.end())
	{
		auto& [dummy, index] = *it;
		currentX = x[index];
	}

	stateTransitionMap	[kfKey][kfKey]	[0]	= 1;
	stateTransitionMap	[kfKey][oneKey]	[0]	= initialState.x - currentX;
	initNoiseMap		[kfKey]				= initialState.P;
	procNoiseMap		[kfKey]				= initialState.Q;
	gaussMarkovTauMap	[kfKey]				= initialState.tau;
	gaussMarkovMuMap	[kfKey]				= initialState.mu;

	if (initialState.P < 0)
	{
		//will be an uninitialised variable, do a least squares solution
		lsqRequired = true;
	}

	return true;
}

void KFState::setExponentialNoise(
	const	KFKey&			kfKey,
	const	Exponential		exponential)
{
	exponentialNoiseMap[kfKey] = exponential;
}

/** Add process noise and dynamics to filter object manually. BEWARE!
 * Not recommended for ordinary use, likely to break things. Dont touch unless you really know what you're doing.
 * Hint - you dont really know what you're doing
 */
void KFState::manualStateTransition(
		Trace&		trace,
		GTime		newTime,
		MatrixXd&	F,
		MatrixXd&	Q0)
{
	if	(newTime != GTime::noTime())
	{
		time = newTime;
	}

	//output the state transition matrix to a trace file (used by RTS smoother)
	if (rts_basename.empty() == false)
	{
		TransitionMatrixObject transitionMatrixObject = F;

		spitFilterToFile(transitionMatrixObject,	E_SerialObject::TRANSITION_MATRIX,	rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
	}

	//compute the updated states and permutation and covariance matrices
	VectorXd Fx = F * x;
	if (simulate_filter_only == false)
	{
		dx = Fx - x;
		x = (Fx										).eval();
		P = (F		* P * F.transpose()		+ Q0	).eval();
	}

	initFilterEpoch();
}


/** Add process noise and dynamics to filter object according to time gap.
 * This will also sort states according to their kfKey as a result of the way the state transition matrix is generated.
 */
void KFState::stateTransition(
	Trace&		trace,		///< Trace file for output
	GTime		newTime,	///< Time of update for process noise and dynamics (s)
	MatrixXd*	stm_ptr)	///< Optional pointer to output state transition matrix
{
	double tgap = 0;
	if	( newTime	!= GTime::noTime()
		&&time		!= GTime::noTime())
	{
		tgap = (newTime - time).to_double();
	}

	if	( newTime	!= GTime::noTime())
	{
		time = newTime;
	}

	int newStateCount = stateTransitionMap.size();
	if (newStateCount == 0)
	{
		std::cout << "THIS IS WEIRD" << "\n";
		return;
	}

	//Initialise and populate a state transition and Z transition matrix
	SparseMatrix<double>	F		= SparseMatrix<double>	(newStateCount, x.rows());

	//add transitions for any states (usually close to identity)
	int row = 0;
	map<KFKey, int> newKFIndexMap;
	for (auto& [newStateKey, newStateMap] : stateTransitionMap)
	{
		newKFIndexMap[newStateKey] = row;

		for (auto& [sourceStateKey, values] : newStateMap)
		{
			int sourceIndex	= getKFIndex(sourceStateKey);

			if	( (sourceIndex < 0)
				||(sourceIndex >= F.cols()))
			{
				continue;
			}

			for (auto& [tExp, value] : values)
			{
				double tau = -1;

				auto gmIter = gaussMarkovTauMap.find(sourceStateKey);
				if (gmIter != gaussMarkovTauMap.end())
				{
					auto& [dummy, sourceTau] = *gmIter;

					tau = sourceTau;
				}

				double scalar = 1;

				if (tau < 0)
				{
					//Random Walk model (special case for First Order Gauss Markov model when tau == inf)

					for (int i = 0; i < tExp; i++)
					{
						scalar *= tgap / (i+1);
					}

		// 				F(row, sourceIndex) = value * scalar;
					F.coeffRef(row, sourceIndex) += value * scalar;

					continue;
				}

				//First Order Gauss Markov model, Ref: Carpenter and Lee (2008) - A Stable Clock Error Model Using Coupled First- and Second-Order Gauss-Markov Processes - https://ntrs.nasa.gov/api/citations/20080044877/downloads/20080044877.pdf

				double tempTerm = 1;
				scalar = exp(-tgap/tau);

				for (int i = 0; i < tExp; i++)
				{
					scalar = tau * (tempTerm - scalar);	//recursive formula derived according to Ref: Carpenter and Lee (2008)
					tempTerm *= tgap / (i+1);
				}

				double transition = value * scalar;

		// 			F(row, sourceIndex) = transition;
				F.coeffRef(row, sourceIndex) += transition;


				//Add state transitions to ONE element, to allow for tiedown to average value mu
				//derived from integrating and distributing terms for v = (v0 - mu) * exp(-t/tau) + mu;
				//tempTerm calculated above appears to be same as required for these terms too, (at least for tExp = 0,1)

				auto muIter = gaussMarkovMuMap.find(sourceStateKey);
				if (muIter != gaussMarkovMuMap.end())
				{
					auto& [dummy2, mu] = *muIter;

		// 				F(row, 0) = mu * (tempTerm - transition);
					F.coeffRef(row, 0) += mu * (tempTerm - transition);
				}
			}
		}

		row++;
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
// 			std::cout << kfKey << " broke" << "\n";
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

	//add time dependent exponential process noise)
	if	(tgap)
	for (auto& [dest, exponential] : exponentialNoiseMap)
	{
		auto destIter = newKFIndexMap.find(dest);
		if (destIter == newKFIndexMap.end())
		{
			std::cout << dest << " broke" << "\n";
			continue;
		}

		auto& expNoise = exponential.value;

		if (expNoise > 0.01)
		{
			trace
			<< "\n" << "Adding : " << expNoise << " to process noise for " << dest << " \n";
		}

		int destIndex	= destIter->second;

		if	( (destIndex < 0)
			||(destIndex >= Q0.rows()))
		{
			continue;
		}

		Q0(destIndex, destIndex) += expNoise * tgap;
	}

	//shrink time dependent exponential process noise
	if (tgap)
	for (auto& [dest, exponential] : exponentialNoiseMap)
	{
		auto& expNoise	= exponential.value;
		auto& expTau	= exponential.tau;

		//shrink the exponential process noise the next time around
		if (expTau)		expNoise *= exp(-tgap / expTau);
		else			expNoise  = 0;
	}

	//add time dependent process noise
	if	(tgap)
	for (auto& [dest,	map]	: stateTransitionMap)
	for (auto& [source,	vals]	: map)
	for (auto& [tExp,	val]	: vals)
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

		auto destIter = newKFIndexMap.find(dest);
		if (destIter == newKFIndexMap.end())
		{
			std::cout << dest << " broke" << "\n";
			continue;
		}

		int destIndex	= destIter->second;

		if	( (destIndex < 0)
			||(destIndex >= Q0.rows()))
		{
			continue;
		}

		auto sourceIter = newKFIndexMap.find(source);
		if (sourceIter == newKFIndexMap.end())
		{
			std::cout << dest << " broKe" << "\n";
			continue;
		}

		int sourceIndex	= sourceIter->second;

		if	( (sourceIndex < 0)
			||(sourceIndex >= Q0.rows()))
		{
			continue;
		}

		auto iter2 = procNoiseMap.find(source);
		if (iter2 == procNoiseMap.end())
		{
// 			std::cout << dest << " brOke" << "\n";
			continue;
		}

		auto [dummy, sourceProcessNoise] = *iter2;

		auto gmIter = gaussMarkovTauMap.find(source);
		if (gmIter != gaussMarkovTauMap.end())
		{
			auto& [dummy, tau] = *gmIter;

			if (tau < 0)
			{
				//Random Walk model (special case for First Order Gauss Markov model when tau == inf)

				if		(tExp == 0)	{	Q0(destIndex,	destIndex) += sourceProcessNoise / 1	* tgap;}
// 				else if	(tExp == 1)	{	Q0(destIndex,	destIndex) += sourceProcessNoise / 3	* tgap * tgap * tgap;
		// 								Q0(sourceIndex, destIndex) += sourceProcessNoise / 2	* tgap * tgap;
		// 								Q0(destIndex, sourceIndex) += sourceProcessNoise / 2	* tgap * tgap;
// 									}
// 				else if (tExp == 2)	{	Q0(destIndex,	destIndex) += sourceProcessNoise / 20	* tgap * tgap * tgap * tgap * tgap;}
			}
			else
			{
				//First Order Gauss Markov model, Ref: Carpenter and Lee (2008) - A Stable Clock Error Model Using Coupled First- and Second-Order Gauss-Markov Processes - https://ntrs.nasa.gov/api/citations/20080044877/downloads/20080044877.pdf

				if		(tExp == 0)	{	Q0(destIndex,	destIndex) += sourceProcessNoise / 2	* tau * (1 - exp(-2*tgap/tau));		}
				else if	(tExp == 1)	{	Q0(destIndex,	destIndex) += sourceProcessNoise / 2	* tau * tau * (	+ 2 * tgap 								//one tau from front tau3 distributed to prevent divide by zero
																												- 4 * tau * (1 - exp(-1*tgap/tau))
																												+ 1 * tau * (1 - exp(-2*tgap/tau)));	//correct formula re-derived according to Ref: Carpenter and Lee (2008)
		// 								Q0(sourceIndex, destIndex) += sourceProcessNoise / 2	* tau * tau * (1-exp(-tgap/tau)) * (1-exp(-tgap/tau));
		// 								Q0(destIndex, sourceIndex) += sourceProcessNoise / 2	* tau * tau * (1-exp(-tgap/tau)) * (1-exp(-tgap/tau));
									}
				else if (tExp == 2)	{	std::cout << "FOGM model is not applied to acceleration term at the moment" << "\n";	}
			}
		}
		else
		{
			std::cout << "Tau value not found in filter: " << source << "\n";
			continue;
		}
	}

	//output the state transition matrix to a trace file (used by RTS smoother)
	if (rts_basename.empty() == false)
	{
		TransitionMatrixObject transitionMatrixObject = F;

		spitFilterToFile(transitionMatrixObject,	E_SerialObject::TRANSITION_MATRIX,	rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
	}

	if (stm_ptr)
	{
		*stm_ptr = F;
	}

// 	std::cout << "x" << "\n" << x << "\n";
	//compute the updated states and permutation and covariance matrices
	VectorXd Fx = F * x;
	if (F.rows() == F.cols())
	{
		dx = Fx - x;
	}
	else
	{
		dx = VectorXd::Zero(F.rows());
	}

	{
		x = (Fx										).eval();
	}
	if (simulate_filter_only)
	{
		Q0.setZero();
	}
	{
		P = (F		* P * F.transpose()		+ Q0	).eval();
	}
	// std::cout << "F" << "\n" << MatrixXd(F).format(heavyFmt) << "\n";
// 	std::cout << "x1" << "\n" << MatrixXd(x).transpose().format(HeavyFmt) << "\n";
// 	std::cout << "Q0" << "\n" << Q0 << "\n";
// 	std::cout << "P" << "\n" << P << "\n";

	//replace the index map with the updated version that corresponds to the updated state
	kfIndexMap = std::move(newKFIndexMap);

	initFilterEpoch();
}

/** Compare variances of measurements and pre-filtered states to detect unreasonable values
* Ref: Wang et al. (1997) - On Quality Control in Hydrographic GPS Surveying
* &  Wieser et al. (2004) - Failure Scenarios to be Considered with Kinematic High Precision Relative GNSS Positioning - http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.573.9628&rep=rep1&type=pdf
*/
void KFState::preFitSigmaCheck(
	Trace&			trace,			///< Trace to output to
	KFMeas&			kfMeas,			///< Measurements, noise, and design matrix
	KFKey&			badStateKey,	///< Key to the state that has worst ratio (only if worse than badMeasIndex)
	int&			badMeasIndex,	///< Index of the measurement that has the worst ratio
	KFStatistics&	statistics,		///< Test statistics
	int				begX,			///< Index of first state element to process
	int				numX,			///< Number of states elements to process
	int				begH,			///< Index of first measurement to process
	int				numH)			///< Number of measurements to process
{
	auto		v = kfMeas.V.segment(begH, numH);
	auto		R = kfMeas.R.block(begH, begH, numH, numH);
	auto		H = kfMeas.H.block(begH, begX, numH, numX);
	auto		P = this-> P.block(begX, begX, numX, numX);

	ArrayXd		measRatios	= ArrayXd::Zero(numH);
	ArrayXd		stateRatios	= ArrayXd::Zero(numX);

	if (prefitOpts.sigma_check)
	{
		//use 'array' for component-wise calculations
		auto		measVariations	= v.array().square();	//delta squared
		auto		measVariances	= ((H*P*H.transpose()).diagonal() + R.diagonal()).array();

		measRatios	= measVariations	/ measVariances;
		measRatios	= measRatios.isFinite()	.select(measRatios,		0);

// 		trace << "\n" << "DOING PRE SIGMA CHECK: ";
	}
	else if (prefitOpts.omega_test)
	{
		MatrixXd	Qinv	= (H*P*H.transpose() + R).inverse();
		MatrixXd	H_Qinv	= H.transpose() * Qinv;

		//use 'array' for component-wise calculations
		auto		measNumerator		= (Qinv * v)	.array().square();		//weighted residuals squared
		auto		stateNumerator		= (H_Qinv * v)	.array().square();

		auto		measDenominator		=  Qinv			.diagonal().array();	//weights
		auto		stateDenominator	= (H_Qinv * H)	.diagonal().array();

		measRatios	= measNumerator		/ measDenominator;
		measRatios	= measRatios.isFinite()	.select(measRatios,		0);	//set ratio to 0 if corresponding variance is 0, e.g. ONE state, clk rate states
		stateRatios	= stateNumerator	/ stateDenominator;
		stateRatios	= stateRatios.isFinite().select(stateRatios,	0);

// 		trace << "\n" << "DOING W-test: ";
	}

	statistics.sumOfSquares	= measRatios.sum();
	statistics.averageRatio	= measRatios.mean();

	//if any are outside the expected value, flag an error

	Eigen::ArrayXd::Index stateIndex;
	Eigen::ArrayXd::Index measIndex;

	double maxStateRatio	= stateRatios	.maxCoeff(&stateIndex);
	double maxMeasRatio		= measRatios	.maxCoeff(&measIndex);

	//if any are outside the expected values, flag an error
	if	( maxStateRatio > maxMeasRatio * 0.95
		&&maxStateRatio > SQR(prefitOpts.state_sigma_threshold))
	{
		int chunkIndex = stateIndex + begX;

		auto it = kfIndexMap.begin();
		std::advance(it, stateIndex);

		auto& [key, dummy] = *it;

		trace << "\n" << "LARGE STATE ERROR OF " << maxStateRatio	<< " AT " << chunkIndex << " : " << key;

		badStateKey = key;
	}

	if	(maxMeasRatio > SQR(prefitOpts.meas_sigma_threshold))
	{
		int chunkIndex = measIndex + begH;

		trace << "\n" << "LARGE MEAS  ERROR OF " << maxMeasRatio	<< " AT " << chunkIndex << " : " << kfMeas.obsKeys[chunkIndex];

		badMeasIndex = measIndex + begH;
	}
}

void outputResiduals(
	Trace&			trace,      	///< Trace file to output to
	KFMeas&			kfMeas,			///< Measurements, noise, and design matrix
	int				iteration,		///< Number of iterations prior to this check
	string			suffix,			///< Suffix to use in header
	int				begH,			///< Index of first measurement to process
	int				numH)			///< Number of measurements to process
{
	string name = "RESIDUALS";
	name += suffix;
	Block block(trace, name);

	tracepdeex(0, trace, "#\t%2s\t%22s\t%10s\t%4s\t%4s\t%5s\t%13s\t%13s\t%16s\t %s\n", "It", "Time", "Type", "Sat", "Str", "Num", "Prefit Res", "Postfit Res", "Meas Sigma", "Comments");
	for (int i = begH; i < begH + numH; i++)
	{
		char var[32];

		double sigma = sqrt(kfMeas.R(i,i));

		if		(sigma	== 0 || (fabs(sigma)	> 0.0001	&& fabs(sigma)	< 1e7))		snprintf(var,	sizeof(var),	"%16.7f",	sigma);
		else																			snprintf(var,	sizeof(var),	"%16.3e",	sigma);

		tracepdeex(0, trace, "%%\t%2d\t%21s\t%20s\t%13.8f\t%13.8f\t%s\t %s\n", iteration, kfMeas.time.to_string(2).c_str(), ((string)kfMeas.obsKeys[i]).c_str(), kfMeas.V(i), kfMeas.VV(i), var, kfMeas.obsKeys[i].comment.c_str());
	}
}

/** Compare variances of measurements and filtered states to detect unreasonable values
*/
void KFState::postFitSigmaChecks(
	Trace&			trace,      	///< Trace file to output to
	KFMeas&			kfMeas,			///< Measurements, noise, and design matrix
	VectorXd&		dx,				///< The innovations from filtering to recalculate the deltas.
	int				iteration,		///< Number of iterations prior to this check
	KFKey&			badStateKey,	///< Key to the state that has worst ratio (only if worse than badMeasIndex)
	int&			badMeasIndex,	///< Index of the measurement that has the worst ratio
	KFStatistics&	statistics,		///< Test statistics
	int				begX,			///< Index of first state element to process
	int				numX,			///< Number of state elements to process
	int				begH,			///< Index of first measurement to process
	int				numH)			///< Number of measurements to process
{
	auto						H	= kfMeas.H.block(begH, begX, numH, numX);

	//use 'array' for component-wise calculations
	auto		measVariations		= kfMeas.VV	.segment(begH, numH).array().square();	//delta squared
	auto		stateVariations		= dx		.segment(begX, numX).array().square();

	auto		measVariances		= (kfMeas.	R.block(begH, begH, numH, numH)).diagonal().array();
	auto		stateVariances		= 			P.block(begX, begX, numX, numX)	.diagonal().array();

	ArrayXd		measRatios			= measVariations	/ measVariances;
				measRatios			= measRatios.isFinite()	.select(measRatios,		0);
	ArrayXd		stateRatios			= stateVariations	/ stateVariances;
				stateRatios			= stateRatios.isFinite().select(stateRatios,	0);

// 	trace << "\n" << "DOING SIGMACHECK: ";

	statistics.sumOfSquares	= measRatios.sum();
	statistics.averageRatio	= measRatios.mean();

	//if any are outside the expected values, flag an error

	Eigen::ArrayXd::Index stateIndex;
	Eigen::ArrayXd::Index measIndex;

// 	std::cout << "\nStateRatios\n"	<< stateRatios;
// 	std::cout << "\nmeasRatios\n"	<< measRatios;

	double maxStateRatio	= stateRatios	.maxCoeff(&stateIndex);
	double maxMeasRatio		= measRatios	.maxCoeff(&measIndex);

	//if any are outside the expected values, flag an error
	if	( maxStateRatio > maxMeasRatio
		&&maxStateRatio > SQR(postfitOpts.state_sigma_threshold))
	{
		int chunkIndex = stateIndex + begX;

		auto it = kfIndexMap.begin();
		std::advance(it, stateIndex);

		auto& [key, dummy] = *it;

		trace << "\n" << "LARGE STATE ERROR OF " << maxStateRatio	<< " AT " << chunkIndex << " : " << key;

		badStateKey = key;
	}

	if	(maxMeasRatio > SQR(postfitOpts.meas_sigma_threshold))
	{
		int chunkIndex = measIndex + begH;

		trace << "\n" << "LARGE MEAS  ERROR OF " << maxMeasRatio	<< " AT " << chunkIndex << " : " << kfMeas.obsKeys[chunkIndex];

		badMeasIndex = measIndex + begH;

// 		std::cout << "\n" << "P" << "\n" << P.diagonal() << "\n";
// 		std::cout << "\n" << "H" << "\n" << H << "\n";
// 		std::cout << "\n" << "dx" << "\n" << dx << "\n";
// 		std::cout << "\n" << "kfMeas.VV" << "\n" << kfMeas.VV << "\n";
// 		std::cout << "\n" << stateRatios << "\n" << "\n" << measRatios << "\n";

	}
}

/** Compute Chi-square increment based on the change of fitting solution
*/
double KFState::stateChiSquare(
	Trace&		trace,      ///< Trace file to output to
	MatrixXd&	Pp,   		///< Post-update covariance of states
	VectorXd&	dx,			///< The innovations from filtering to recalculate the deltas.
	int			begX,		///< Index of first state element to process
	int			numX,		///< Number of states elements to process
	int			begH,		///< Index of first measurement to process
	int			numH)		///< Number of measurements to process
{
	if (begX == 0)	//exclude the One state
	{
		begX  = 1;
		numX -= 1;
	}

	auto		w  = dx.segment(begX, numX);
	MatrixXd	P  = this->P.block(begX, begX, numX, numX);
	// MatrixXd	dP = this->P.block(begX, begX, numX, numX) - Pp.block(begX, begX, numX, numX);	//Ref: Li et al. (2020) - Robust Kalman Filtering Based on Chi-square Increment and Its Application - https://www.mdpi.com/2072-4292/12/4/732/pdf

	double		chiSq = w.transpose() * P.inverse() * w;
	// double		chiSq = w.transpose() * dP.inverse() * w;	//Ref: Li et al. (2020) - Robust Kalman Filtering Based on Chi-square Increment and Its Application - https://www.mdpi.com/2072-4292/12/4/732/pdf
																//numerical instability problem exists for dP.inverse()

	trace << "\n" << "DOING STATE CHI-SQUARE TEST:";
	// for (int i = 0; i < numX; i++)	trace << "dx: " 	<< w(i) << "\tdP: "	<< dP(i, i) << "\n";

	return chiSq;
}

/** Compute Chi-square increment based on post-fit residuals
*/
double KFState::measChiSquare(
	Trace&		trace,      ///< Trace file to output to
	KFMeas&		kfMeas,		///< Measurements, noise, and design matrix
	VectorXd&	dx,			///< The innovations from filtering to recalculate the deltas.
	int			begX,		///< Index of first state element to process
	int			numX,		///< Number of states elements to process
	int			begH,		///< Index of first measurement to process
	int			numH)		///< Number of measurements to process
{
	auto		w = dx.segment(begX, numX);
	auto		H = kfMeas.H.block(begH, begX, numH, numX);
	VectorXd	v = kfMeas.V.segment(begH, numH) - H * w;
	auto		R = kfMeas.R.block(begH, begH, numH, numH);

	double		chiSq = (v.array().square() / R.diagonal().array()).sum();

	trace << "\n" << "DOING MEASUREMENT CHI-SQUARE TEST:";
	// for (int i = 0; i < numH; i++)	trace << "v(+): "	<< v(i) << "\tR: "		<< R(i, i) << "\n";

	return chiSq;
}

/** Compute Chi-square increment based on pre-fit residuals (innovations)
*/
double KFState::innovChiSquare(
	Trace&		trace,		///< Trace to output to
	KFMeas&		kfMeas,		///< Measurements, noise, and design matrix
	int			begX,		///< Index of first state element to process
	int			numX,		///< Number of states elements to process
	int			begH,		///< Index of first measurement to process
	int			numH)		///< Number of measurements to process
{
	auto		H = kfMeas.H.block(begH, begX, numH, numX);
	auto		v = kfMeas.V.segment(begH, numH);
	auto		R = kfMeas.R.block(begH, begH, numH, numH);
	auto		P = this->P.block(begX, begX, numX, numX);
	MatrixXd	Q = R + H * P * H.transpose();

	double		chiSq = v.transpose() * Q.inverse() * v;

	trace << "\n" << "DOING INNOVATION CHI-SQUARE TEST:";
	// for (int i = 0; i < numH; i++)	trace << "v(-): "	<< v(i) << "\tS: "		<< Q(i, i) << "\n";

	return chiSq;
}

/** Kalman filter.
*/
bool KFState::kFilter(
	Trace&			trace,		///< Trace to output to
	KFMeas&			kfMeas,		///< Measurements, noise, and design matrices
	VectorXd&		xp,   		///< Post-update state vector
	MatrixXd&		Pp,   		///< Post-update covariance of states
	VectorXd&		dx,			///< Post-update state innovation
	int				begX,		///< Index of first state element to process
	int				numX,		///< Number of state elements to process
	int				begH,		///< Index of first measurement to process
	int				numH)		///< Number of measurements to process
{
	auto& R			= kfMeas.R;
	auto& v			= kfMeas.V;
	auto& H			= kfMeas.H;
	auto& H_star	= kfMeas.H_star;
	auto& noise		= kfMeas.uncorrelatedNoise.asDiagonal();

	auto subH = H.block(begH, begX, numH, numX);

	auto		HRH_star	= H_star	* noise * H_star.transpose();
	auto		HP			= subH		* P.block(begX, begX, numX, numX);
	MatrixXd	Q			= HP		* subH.transpose()
							+ R.block(begH, begH, numH, numH);

	MatrixXd K;
	MatrixXd HRHQ_star;

	bool repeat = true;
	while (repeat)
	{
		switch (inverter)
		{
			default:
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning: kalman filter inverter type " << inverter << " not supported, reverting";
				inverter = E_Inverter::LDLT;
				continue;
			}
			case E_Inverter::LDLT:
			{
				auto QQ = Q.triangularView<Eigen::Upper>().transpose();
				LDLT<MatrixXd> solver;
				if	( 						(				solver.compute(QQ),						solver.info() != Eigen::ComputationInfo::Success)
					||						(K			=	solver.solve(HP)		.transpose(),	solver.info() != Eigen::ComputationInfo::Success)
					||(advanced_postfits &&	(HRHQ_star	=	solver.solve(HRH_star)	.transpose(),	solver.info() != Eigen::ComputationInfo::Success)))
				{
					xp = x;
					Pp = P;
					dx = VectorXd::Zero(xp.rows());

					BOOST_LOG_TRIVIAL(error) << "Error: Failed to calculate kalman gain, see trace file for matrices";

					trace << "\n" << "Kalman Filter Error1";
					trace << "\n" << "Q:" << "\n" << Q;
					trace << "\n" << "H:" << "\n" << H;
					trace << "\n" << "R:" << "\n" << R;
					trace << "\n" << "P:" << "\n" << P;

					return false;
				}

				break;
			}
			case E_Inverter::LLT:
			{
				auto QQ = Q.triangularView<Eigen::Upper>().transpose();
				LLT<MatrixXd> solver;
				if	( 						(				solver.compute(QQ),						solver.info() != Eigen::ComputationInfo::Success)
					||						(K			=	solver.solve(HP)		.transpose(),	solver.info() != Eigen::ComputationInfo::Success)
					||(advanced_postfits &&	(HRHQ_star	=	solver.solve(HRH_star)	.transpose(),	solver.info() != Eigen::ComputationInfo::Success)))
				{
					inverter = E_Inverter::LDLT;
					continue;
				}

				break;
			}
			case E_Inverter::INV:
			{
				MatrixXd Qinv = Q.inverse();
				K = P * H.transpose() * Qinv;

				if (advanced_postfits)
				{
					HRHQ_star = HRH_star * Qinv;
				}
				break;
			}
		}
		repeat = false;
	}

	if (advanced_postfits)
	{
		kfMeas.VV = HRHQ_star * v;
	}

	dx.segment(begX, numX)	= K * v.segment(begH, numH);
	xp.segment(begX, numX)	= x. segment(begX, numX)
							+ dx.segment(begX, numX);

// 	trace << "\n" << "h "	<< "\n" << subH;
// 	trace << "\n" << "hp "	<< "\n" << HP;
// 	trace << "\n" << "Q "	<< "\n" << Q;
// 	trace << "\n" << "K "	<< "\n" << K;
// 	trace << "\n" << "X "	<< "\n" << x. segment(begX, numX);
// 	trace << "\n" << "DX"	<< "\n" << dx.segment(begX, numX);
// 	trace << "\n" << "xp"	<< "\n" << xp.segment(begX, numX);

	if (joseph_stabilisation)
	{
		MatrixXd IKH = MatrixXd::Identity(P.rows(), P.cols()) - K * H;
		Pp = IKH * P * IKH.transpose() + K * R * K.transpose();
	}
	else
	{
		Pp.block(begX, begX, numX, numX) = P.block(begX, begX, numX, numX) - K * HP;

		Pp.block(begX, begX, numX, numX) = (	  Pp.block(begX, begX, numX, numX)
												+ Pp.block(begX, begX, numX, numX).transpose()	).eval() / 2;
	}

	bool error = xp.segment(begX, numX).array().isNaN().any();
	if (error)
	{
		std::cout << "\n" << "x:" << "\n" << x << "\n";
		std::cout << "\n" << "xp:" << "\n" << xp << "\n";
		std::cout << "\n" << "R :" << "\n" << R << "\n";
		std::cout << "\n" << "K :" << "\n" << K << "\n";
		std::cout << "\n" << "P :" << "\n" << P << "\n";
		std::cout << "\n" << "v :" << "\n" << v << "\n";
		std::cout << "\n";
		std::cout << "NAN found. Exiting...";
		std::cout << "\n";

		exit(0);
	}

	return true;
}

/** Perform chi squared quality control.
*/
bool KFState::chiQC(
	Trace&		trace,      ///< Trace to output to
	KFMeas&		kfMeas,		///< Measurements, noise, and design matrix
	VectorXd&	xp)         ///< Post filtered state vector
{
	auto& H = kfMeas.H;
	auto W = kfMeas.W(all, 0);

	VectorXd&	y		= kfMeas.Y;
	VectorXd	v		= y - H * xp;
	double		v_Wv	= v.transpose() * W.asDiagonal() * v;

	//trace << "\n" << "chiqcV" << v.rows() << "\n";

	dof = v.rows() - (x.rows() - 1);	//ignore KF::ONE element -> -1
	chi = v_Wv;

	if (dof < 1)
	{
		chiQCPass = true;
		return true;
	}

	boost::math::normal normDist;

	double	alpha = cdf(complement(normDist, chiSquareTest.sigma_threshold)) * 2;	//two-tailed

	boost::math::chi_squared chiSqDist(dof);

	double thres = quantile(complement(chiSqDist, alpha));

	/* chi-square validation */
	if (chi > thres)
	{
		tracepdeex(5, trace, "\nChiSquare error detected: dof:%d chi:%f thres:%f", dof, chi, thres);

// 		auto variations	= v.array().square();
// 		Eigen::MatrixXf::Index index;
// 		trace << " -> LARGEe ERROR OF " << sqrt(variations.maxCoeff(&index)) << " AT " << index;

		chiQCPass = false;
		return false;
	}

	chiQCPass = true;
	return true;
}

/** Combine a list of KFMeasEntrys into a single KFMeas object for used in the filter
*/
KFMeas::KFMeas(
	KFState&			kfState,			///< Filter state to correspond to
	KFMeasEntryList&	kfEntryList,		///< List of input measurements as lists of entries
	GTime				measTime,			///< Time to use for measurements and hence state transitions
	MatrixXd*			noiseMatrix_ptr)	///< Optional pointer to use custom noise matrix
{
	int numMeas = kfEntryList.size();

	if (measTime == GTime::noTime())
	{
		measTime = time;
	}

	time = measTime;

	V	.resize(numMeas);
	VV	.resize(numMeas);
	Y	.resize(numMeas);

	//merge all individual noise elements into the a new map and then vector
	{
		map<KFKey,	double> noiseElementMap;

		for (auto& entry			: kfEntryList)
		for (auto& [kfKey, value]	: entry.noiseElementMap)
		{
			noiseElementMap[kfKey] = value;
		}

		uncorrelatedNoise = VectorXd::Zero(noiseElementMap.size());

		int noises = 0;
		for (auto& [key, value] : noiseElementMap)
		{
			uncorrelatedNoise(noises) = value;

			noiseIndexMap[key] = noises;
			noises++;
		}
	}

	R		= MatrixXd::Zero(numMeas, numMeas);
	H		= MatrixXd::Zero(numMeas, kfState.x			.rows());
	H_star	= MatrixXd::Zero(numMeas, uncorrelatedNoise	.rows());

	obsKeys			.resize(numMeas);
	metaDataMaps	.resize(numMeas);
	componentsMaps	.resize(numMeas);

	bool error = false;
#	ifdef ENABLE_PARALLELISATION
		Eigen::setNbThreads(1);
#		pragma omp parallel for
#	endif
	for (int meas = 0; meas < kfEntryList.size(); meas++)
	{
		auto it = kfEntryList.begin();
		std::advance(it, meas);

		auto& entry = *it;

		R(meas, meas)	= entry.noise;

		auto& value = Y(meas);
		auto& innov = V(meas);

		value			= entry.value;
		innov			= entry.innov;

		for (auto& [kfKey, coeff] : entry.designEntryMap)
		{
			if (coeff == 0)
			{
				continue;
			}

			int index = kfState.getKFIndex(kfKey);
			if (index < 0)
			{
				std::cout << "Code error: Trying to create measurement for undefined key, check stateTransition() is called first: " << kfKey << "\n";
				error = true;
			}
			H(meas, index) = coeff;

			if (kfState.assume_linearity)
			{
				double xVal = kfState.x[index];
				double uVal = entry.usedValueMap[kfKey];

				double deltaX = xVal - uVal;
				if (deltaX)
				{
					BOOST_LOG_TRIVIAL(debug) << std::fixed
					<< "Adjusting meas '" << entry.obsKey << "' as '" << kfKey << "' changed " << deltaX
					<< "\tfrom "	<< uVal
					<< "\tto "		<< xVal
					<< "\t : "		<< innov
					<< "\t-> "		<< innov - deltaX * coeff;

					value -= deltaX * coeff;
					innov -= deltaX * coeff;
				}
			}
		}

		for (auto& [kfKey, coeff] : entry.noiseEntryMap)
		{
			int index = getNoiseIndex(kfKey);
			if (index < 0)
			{
				std::cout << "Code error: Trying to create measurement for undefined key :" << kfKey << "\n";
				error = true;
			}
			H_star(meas, index) = coeff;
		}

		obsKeys			[meas] = std::move(entry.obsKey);
		metaDataMaps	[meas] = std::move(entry.metaDataMap);
		componentsMaps	[meas] = std::move(entry.componentsMap);
	}
	Eigen::setNbThreads(0);

	if (error)
	{
		return;
	}

	if (noiseMatrix_ptr)
	{
		R = *noiseMatrix_ptr;
	}

	if (uncorrelatedNoise.rows() != 0)
	{
		SparseMatrix<double> R_A = SparseMatrix<double>(numMeas, uncorrelatedNoise.rows());

		int meas = 0;
		for (auto& entry: kfEntryList)
		{
			for (auto& [kfKey, value] : entry.noiseEntryMap)
			{
				int noiseIndex = noiseIndexMap[kfKey];

				R_A.insert(meas, noiseIndex) = value;
			}

			meas++;
		}

		R = R_A * uncorrelatedNoise.asDiagonal() * R_A.transpose();
	}
}

bool KFState::doStateRejectCallbacks(
	Trace&		trace,				///< Trace file for output
	KFMeas&		kfMeas,				///< Measurements that were passed to the filter
	KFKey&		badKey,				///< Key in state that was unsatisfactory
	bool		postFit)			///< Rejection occured during post-filtering checks
{
	for (auto& callback : stateRejectCallbacks)
	{
		bool keepGoing = callback(trace, *this, kfMeas, badKey, postFit);

		if (keepGoing == false)
		{
			return false;
		}
	}

	return true;
}

bool KFState::doMeasRejectCallbacks(
	Trace&		trace,				///< Trace file for output
	KFMeas&		kfMeas,				///< Measurements that were passed to the filter
	int			badIndex,			///< Index in measurement list that was unsatisfactory
	bool		postFit)			///< Rejection occured during post-filtering checks
{
	for (auto& callback : measRejectCallbacks)
	{
		bool keepGoing = callback(trace, *this, kfMeas, badIndex, postFit);

		if (keepGoing == false)
		{
			return false;
		}
	}

	return true;
}


/** Kalman filter operation
*/
void KFState::filterKalman(
	Trace&						trace,				///< Trace file for output
	KFMeas&						kfMeas,				///< Measurement object
	const string&				suffix,				///< Suffix to append to residuals block
	bool						innovReady,			///< Innovation already constructed
	map<string, FilterChunk>*	filterChunkMap_ptr)	///< Optional map of chunks for parallel processing of sub filters
{
	DOCS_REFERENCE(Kalman_Filter__);

	if (kfMeas.time != GTime::noTime())
	{
		time = kfMeas.time;
	}

	map<string, FilterChunk> dummyFilterChunkMap;
	if (filterChunkMap_ptr == nullptr)
	{
		filterChunkMap_ptr = &dummyFilterChunkMap;
	}

	filterChunkMap = *filterChunkMap_ptr;

	if (filterChunkMap.empty())
	{
		FilterChunk filterChunk;

		filterChunk.trace_ptr	= &trace;
		filterChunk.numX		= x.rows();

		filterChunkMap[""] = filterChunk;
	}

	auto returnEarlyPrep = [&]()
	{
		if (rts_basename.empty() == false)
		{
			spitFilterToFile(*this,			E_SerialObject::FILTER_MINUS,	rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
			spitFilterToFile(*this,			E_SerialObject::FILTER_PLUS,	rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
			spitFilterToFile(kfMeas,		E_SerialObject::MEASUREMENT,	rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
		}
	};

	if (kfMeas.H.rows() == 0)
	{
		//nothing to be done, clean up and return early
		returnEarlyPrep();
		return;
	}

	/* kalman filter measurement update */
	if (innovReady == false)
	{
		kfMeas.V	= kfMeas.Y - kfMeas.H * x;
		kfMeas.VV	= kfMeas.V;
	}

	TestStatistics testStatistics;

	for (auto& [id, filterChunk] : filterChunkMap)
	{
		if (filterChunk.numH == 0)
		{
			continue;
		}

		if (filterChunk.numX < 0)	filterChunk.numX = x.rows();
		if (filterChunk.numH < 0)	filterChunk.numH = kfMeas.H.rows();

		KFStatistics statistics;
		for (int i = 0; i < prefitOpts.max_iterations; i++)
		{
			auto& chunkTrace = *filterChunk.trace_ptr;

			if	(  prefitOpts.sigma_check	== false
				&& prefitOpts.omega_test	== false)
			{
				continue;
			}

			KFKey	badState;
			int		badMeasIndex = -1;

			preFitSigmaCheck(chunkTrace, kfMeas, badState, badMeasIndex, statistics, filterChunk.begX, filterChunk.numX, filterChunk.begH, filterChunk.numH);

			if (badState.type)		{	chunkTrace << "\n" << "Prefit check failed state test";		bool keepGoing = doStateRejectCallbacks	(chunkTrace, kfMeas, badState,		false);		/*continue;*/	}	//always fallthrough
			if (badMeasIndex >= 0)	{	chunkTrace << "\n" << "Prefit check failed measurement test";	bool keepGoing = doMeasRejectCallbacks	(chunkTrace, kfMeas, badMeasIndex,	false);		continue;		}	//retry next iteration
			else					{	chunkTrace << "\n" << "Prefit check passed";																											break;			}
		}

		testStatistics.sumOfSquaresPre	+= statistics.sumOfSquares;
		testStatistics.averageRatioPre	+= statistics.averageRatio / filterChunkMap.size();
	}

	if	(  prefitOpts.sigma_check
		|| prefitOpts.omega_test)
	{
		trace << "\n" << "Sum-of-squared test statistics (prefit): "	<< testStatistics.sumOfSquaresPre	<< "\n";
	}

	VectorXd	xp = x;
	MatrixXd	Pp = P;
				dx = VectorXd::Zero(x.rows());


	statisticsMap["States"] = x.rows();

	for (auto& [id, fc] : filterChunkMap)
	{
		if (fc.numH == 0)
		{
			continue;
		}

		if (fc.id.empty() == false)
		{
			BOOST_LOG_TRIVIAL(info) << " ------- FILTERING CHUNK " << fc.id << "         --------\n";
		}

		statisticsMap["Observations"] += fc.numH;

		KFStatistics statistics;
		for (int i = 0; i < postfitOpts.max_iterations; i++)
		{
			auto& chunkTrace = *fc.trace_ptr;

			bool pass = kFilter(chunkTrace, kfMeas, xp, Pp, dx, fc.begX, fc.numX, fc.begH, fc.numH);

			if (pass == false)
			{
				chunkTrace << "FILTER FAILED" << "\n";
				returnEarlyPrep();
				return;
			}

			if (advanced_postfits == false)
			{
				kfMeas.VV.segment(fc.begH, fc.numH) = kfMeas.V.segment(fc.begH,fc.numH)
													- kfMeas.H.block(fc.begH, fc.begX, fc.numH, fc.numX) * dx.segment(fc.begX, fc.numX);
			}

			if (output_residuals)
			{
				InteractiveTerminal ss("Residuals" + suffix, trace);

				outputResiduals(ss, kfMeas, i, suffix, fc.begH, fc.numH);
			}

			if (postfitOpts.sigma_check == false)
			{
				break;
			}

			KFKey	badState;
			int		badMeasIndex = -1;

			postFitSigmaChecks(chunkTrace, kfMeas, dx, i, badState, badMeasIndex, statistics, fc.begX, fc.numX, fc.begH, fc.numH);
			bool stopIterating = true;
			if (badState.type)		{	chunkTrace << "\n" << "Postfit check failed state test";		bool keepGoing = doStateRejectCallbacks	(chunkTrace, kfMeas, badState,		true);		stopIterating = false;	}
			if (badMeasIndex >= 0)	{	chunkTrace << "\n" << "Postfit check failed measurement test";	bool keepGoing = doMeasRejectCallbacks	(chunkTrace, kfMeas, badMeasIndex,	true);		stopIterating = false;	}

			if (stopIterating)		{	chunkTrace << "\n" << "Postfit check passed";																																	}

			if	( stopIterating
				||i == postfitOpts.max_iterations - 1)
			{
				statisticsMap["Filter iterations " + std::to_string(i+1)]++;

				break;
			}
		}

		if (outputMongoMeasurements)
		{
			mongoMeasResiduals(kfMeas.time, kfMeas, acsConfig.mongoOpts.queue_outputs, suffix, fc.begH, fc.numH);
		}

		testStatistics.sumOfSquaresPost	+= statistics.sumOfSquares;
		testStatistics.averageRatioPost	+= statistics.averageRatio / filterChunkMap.size();
	}

	if (postfitOpts.sigma_check)
		trace << "\n" << "Sum-of-squared test statistics (postfit): " << testStatistics.sumOfSquaresPost << "\n";

	if (chiSquareTest.enable)
	{
		for (auto& [id, fc] : filterChunkMap)
		{
			if (fc.numH == 0)
			{
				continue;
			}

			auto& chunkTrace = *fc.trace_ptr;

			switch (chiSquareTest.mode)	// todo Eugene: rethink Chi-Square test modes, consider keep only INNOVATION and determine DOF automatically based on process noises
			{
				case E_ChiSqMode::INNOVATION:	{	testStatistics.chiSq += innovChiSquare	(chunkTrace, kfMeas,		fc.begX, fc.numX, fc.begH, fc.numH);	break;	}
				case E_ChiSqMode::MEASUREMENT:	{	testStatistics.chiSq += measChiSquare	(chunkTrace, kfMeas,	dx,	fc.begX, fc.numX, fc.begH, fc.numH);	break;	}
				case E_ChiSqMode::STATE:		{	testStatistics.chiSq += stateChiSquare	(chunkTrace, Pp,		dx,	fc.begX, fc.numX, fc.begH, fc.numH);	break;	}
				default:						{	BOOST_LOG_TRIVIAL(error) << "Error: Unknown Chi-square test mode";											break;	}
			}
		}

		if (chiSquareTest.mode == +E_ChiSqMode::STATE)	testStatistics.dof	= 			x.rows() - 1;
		else											testStatistics.dof	= kfMeas.	H.rows();	// todo Eugene: revisit DOF in the future for MEASUREMENT mode

		testStatistics.chiSqPerDof	= testStatistics.chiSq / testStatistics.dof;

		// check against threshold
		boost::math::normal normDist;
		double	alpha = cdf(complement(normDist, chiSquareTest.sigma_threshold)) * 2;	//two-tailed

		boost::math::chi_squared chiSqDist(testStatistics.dof);
		testStatistics.qc = quantile(complement(chiSqDist, alpha));
		if (testStatistics.chiSq <= testStatistics.qc)		trace << "\n" << "Chi-square test passed";
		else												trace << "\n" << "Chi-square test failed";

		trace << "\n"
		<< "Chi-square increment: "		<< testStatistics.chiSq
		<< "\tThreshold: "				<< testStatistics.qc
		<< "\tNumber of measurements:"	<< kfMeas.H.rows()
		<< "\tNumber of states:"		<< x.rows() - 1
		<< "\tDegree of freedom: "		<< testStatistics.dof
		<< "\tChi-square per DOF: "		<< testStatistics.chiSqPerDof << "\n";
	}

	if (acsConfig.mongoOpts.output_test_stats)
	{
		mongoTestStat(*this, testStatistics);
	}

	if (rts_basename.empty() == false)
	{
		spitFilterToFile(*this,		E_SerialObject::FILTER_MINUS, rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
	}

	if (simulate_filter_only)
	{
		dx = VectorXd::Zero(x.rows());
	}
	else
	{
		x = std::move(xp);
		P = std::move(Pp);
	}

	if (rts_basename.empty() == false)
	{
		spitFilterToFile(*this,		E_SerialObject::FILTER_PLUS, rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
		spitFilterToFile(kfMeas,	E_SerialObject::MEASUREMENT, rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
	}

	initFilterEpoch();
}

/** Least squares estimator for new kalman filter states.
* If new states have been added that do not contain variance values, the filter will assume that these states values and covariances should be
* estimated using least squares.
*
* This function will extract the minimum required states from the existing state vector,
* and the minimum required measurements in order to perform least squares for the uninitialised states.
*/
void KFState::leastSquareInitStates(
	Trace&			trace,				///< Trace file for output
	KFMeas&			kfMeas,				///< Measurement object
	bool			initCovars,			///< Option to also initialise off-diagonal covariance values
	VectorXd*		dx,					///< Optional output of state deltas
	bool			innovReady)			///< Perform conversion between V & Y
{
	lsqRequired = false;

	chiQCPass = false;

	if (innovReady)
	{
		kfMeas.Y = kfMeas.V;
	}

	vector<int> newStateIndicies;

	//find all the states that aren't initialised, they need least squaring.
	for (auto& [key, i] : kfIndexMap)
	{
		if	( (key.type != KF::ONE)
			&&(P(i,i) < 0))
		{
			//this is a new state and needs to be evaluated using least squares
			newStateIndicies.push_back(i);
		}
	}

	//get the subset of the measurement matrix that applies to the uninitialised states
	auto subsetA = kfMeas.H(all, newStateIndicies);

	//find the subset of measurements that are required for the initialisation
	auto usedMeas = subsetA.rowwise().any();

	map<int, bool> pseudoMeasStates;
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

		//remember make a pseudo measurement of anything it references that is already set
		for (int state = 0; state < kfMeas.H.cols(); state++)
		{
			if	( (kfMeas.H(meas, state)	!=	0)
				&&(P(state,state)			>=	0))
			{
				pseudoMeasStates[state] = true;
			}
		}
	}

	int newMeasCount = leastSquareMeasIndicies.size() + pseudoMeasStates.size();

	//Create new measurement objects with larger size, (using all states for now)
	KFMeas	leastSquareMeas;

	leastSquareMeas.Y = VectorXd::Zero(newMeasCount);
	leastSquareMeas.R = MatrixXd::Zero(newMeasCount, newMeasCount);
	leastSquareMeas.H = MatrixXd::Zero(newMeasCount, kfMeas.H.cols());
	//VV
	//V

	int measCount = leastSquareMeasIndicies.size();

	//copy in the required measurements from the old set
	leastSquareMeas.Y.head			(measCount)				= kfMeas.Y(leastSquareMeasIndicies);
	leastSquareMeas.R.topLeftCorner	(measCount, measCount)	= kfMeas.R(leastSquareMeasIndicies, leastSquareMeasIndicies);
	leastSquareMeas.H.topRows		(measCount)				= kfMeas.H(leastSquareMeasIndicies, all);

	//append any new pseudo measurements to the end
	for (auto& [state, boool] : pseudoMeasStates)
	{
		leastSquareMeas.Y(measCount)				= x(state);
		leastSquareMeas.R(measCount, measCount)		= P(state, state);
		leastSquareMeas.H(measCount, state)			= 1;
		measCount++;
	}

	//find the subset of states required for these measurements
	vector<int> usedCols;
	auto usedStates = leastSquareMeas.H.colwise().any();
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
	leastSquareMeasSubs.H = leastSquareMeas.H(all, usedCols);

	//invert measurement noise matrix to get a weight matrix
	leastSquareMeasSubs.W = (1 / leastSquareMeasSubs.R.diagonal().array()).matrix();

	VectorXd w = (1 / leastSquareMeasSubs.R.diagonal().array()).matrix().col(0);

	for (int i = 0; i < w.rows(); i++)
	{
		if (std::isinf(w(i)))
		{
			w(i) = 0;
		}
	}

	if	( leastSquareMeasSubs.H.cols() == 0
		||leastSquareMeasSubs.H.rows() == 0)
	{
		trace << "\n" << "EMPTY DESIGN MATRIX DURING LEAST SQUARES";
		return;
	}

	if (leastSquareMeasSubs.R.rows() < leastSquareMeasSubs.H.cols())
	{
		trace << "\n" << "Insufficient measurements for least squares " << leastSquareMeasSubs.R.rows() <<  " " << x.rows();
		return;
	}
	auto& H = leastSquareMeasSubs.H;
	auto& Y = leastSquareMeasSubs.Y;

// 	std::cout << "\nkfmeasY\n" << kfMeas.Y << "\n";
// 	std::cout << "\nkfmeasV\n" << kfMeas.V << "\n";
// 	std::cout << "\nY\n" << Y << "\n";

	//calculate least squares solution
	MatrixXd W		= w.asDiagonal();
	MatrixXd H_W	= H.transpose() * W;
	MatrixXd Q		= H_W * H;

	MatrixXd Qinv	= Q.inverse();
	VectorXd x1		= Qinv * H_W * Y;

// 	std::cout << "Q : " << "\n" << Q;
	bool error = x1.array().isNaN().any();
	if (error)
	{
		std::cout << "\n" << "P :" << "\n" << P << "\n";
		std::cout << "\n" << "x1:" << "\n" << x1 << "\n";
		std::cout << "\n" << "w :" << "\n" << w << "\n";
		std::cout << "\n" << "H :" << "\n" << H << "\n";
		std::cout << "\n";
		std::cout << "NAN found. Exiting....";
		std::cout	<< "\n";

		exit(-1);
	}

	if (chiSquareTest.enable)
		chiQC(trace, leastSquareMeasSubs, x1);

	if (dx)
	{
		(*dx) = x1;
	}

	for (int i = 0; i < usedCols.size(); i++)
	{
		int stateRowIndex = usedCols[i];

		if (P(stateRowIndex, stateRowIndex) >= 0)
		{
			continue;
		}

		double newStateVal = x1(i);
		double newStateCov = Qinv(i,i);

		if (dx)
		{
			x(stateRowIndex)				+=	newStateVal;
			P(stateRowIndex,stateRowIndex)	=	newStateCov;
			kfMeas.VV						=	kfMeas.Y - H * *dx;
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
	map<KFKey, int>&	kfKeyMap,		///< List of keys to return within substate
	MatrixXd*			covarMat_ptr,	///< Optional pointer to a matrix for output of covariance submatrix
	VectorXd*			adjustVec_ptr)	///< Optional pointer to a vector for output of last adjustments
const
{
	vector<int> indices;
	indices.resize(kfKeyMap.size());

	for (auto& [kfKey, mapIndex] : kfKeyMap)
	{
		int stateIndex = getKFIndex(kfKey);
		if (stateIndex >= 0)
		{
			indices[mapIndex] = stateIndex;
		}
	}

	VectorXd					subState		= x (indices);
	if (covarMat_ptr)		{	*covarMat_ptr	= P (indices, indices);		}
	if (adjustVec_ptr)		{	*adjustVec_ptr	= dx(indices);				}

	return subState;
}

/** Get a portion of a state by passing in a list of keys.
 *  Only gets some aspects, as most aren't required
 */
void KFState::getSubState(
	map<KFKey, int>&	kfKeyMap,	///< List of keys to return within substate
	KFState&			subState)	///< Output state
const
{
	vector<int> indices;
	indices.resize(kfKeyMap.size());

	subState.kfIndexMap.clear();
	for (auto& [kfKey, mapIndex] : kfKeyMap)
	{
		int stateIndex = getKFIndex(kfKey);
		if (stateIndex >= 0)
		{
			indices[mapIndex] = stateIndex;
		}

		subState.kfIndexMap[kfKey] = mapIndex;
	}

	subState.time	= time;
	subState.x		= x	(indices);
	subState.dx		= dx(indices);
	subState.P		= P	(indices, indices);

	subState.stateTransitionMap.clear();

	for (auto& [keyA, stmMap] : stateTransitionMap)
	{
		auto itA = kfKeyMap.find(keyA);
		if (itA == kfKeyMap.end())
		{
			continue;
		}

		for (auto& [keyB, st] : stmMap)
		{
			auto itB = kfKeyMap.find(keyB);
			if (itB == kfKeyMap.end())
			{
				continue;
			}

			subState.stateTransitionMap[keyA][keyB] = st;
		}
	}
}

KFState KFState::getSubState(
	vector<KF>	types)
const
{
	if (std::find(types.begin(), types.end(), +KF::ALL) != types.end())
	{
		return *this;
	}

	KFState subState;

	vector<int> indices;

	int index = 0;
	for (auto& [kfKey, mapIndex] : kfIndexMap)
	{
		if (std::find(types.begin(), types.end(), kfKey.type) == types.end())
		{
			continue;
		}

		indices.push_back(mapIndex);
		subState.kfIndexMap[kfKey]	= index;

		index++;
	}

	subState.time	= time;
	subState.x		= x	(indices);
	subState.dx		= dx(indices);
	subState.P		= P	(indices, indices);

	// for (auto& [kfKey,	thing] : stateTransitionMap)
	// for (auto& [other,	entry] : thing)					try	{	subState.kfIndexMap.at(kfKey);
	// 															subState.kfIndexMap.at(other);		subState.stateTransitionMap	[kfKey][other]	= entry;	}	catch (...){}
	// for (auto& [kfKey,	entry] : gaussMarkovTauMap)		try	{	subState.kfIndexMap.at(kfKey);		subState.gaussMarkovTauMap	[kfKey]			= entry;	}	catch (...){}
	// for (auto& [kfKey,	entry] : gaussMarkovMuMap)		try	{	subState.kfIndexMap.at(kfKey);		subState.gaussMarkovMuMap	[kfKey]			= entry;	}	catch (...){}
	// for (auto& [kfKey,	entry] : procNoiseMap)			try	{	subState.kfIndexMap.at(kfKey);		subState.procNoiseMap		[kfKey]			= entry;	}	catch (...){}
	// for (auto& [kfKey,	entry] : exponentialNoiseMap)	try	{	subState.kfIndexMap.at(kfKey);		subState.exponentialNoiseMap[kfKey]			= entry;	}	catch (...){}

	return subState;
}

/** Output keys and states in human readable format
*/
void KFState::outputStates(
		Trace&		output,	///< Trace to output to
		string		suffix,	///< Suffix to append to state block info tag in trace files
		int			begX,	///< Index of first state element to process
		int			numX)   ///< Number of state elements to process
{
	tracepdeex(2, output, "\n\n");

	string name = "STATES";
	name += suffix;

	InteractiveTerminal trace(name, output);
	Block block(trace, name);

	tracepdeex(1, trace, "#\t%20s\t%20s\t%5s\t%3s\t%7s\t%17s\t%17s\t%15s", "Time", "Type", "Str", "Sat", "Num", "State", "Sigma", "Adjust");
	tracepdeex(5, trace, "\t%17s",	"Mu");
	tracepdeex(2, trace, "\t%s",	"Comments");
	tracepdeex(1, trace, "\n");

	int endX;
	if (numX < 0)	endX = x.rows();
	else			endX = begX + numX;

	bool noAdjust = dx.isZero();

	for (auto& [key, index] : kfIndexMap)
	{
		if (index >= x.rows())
		{
			continue;
		}
		if	( index <  begX
			||index >= endX)
		{
			continue;
		}

		double _x	= x(index);
		double _dx = 0;
		if (index < dx.rows())
			_dx = dx(index);
		double _sigma	= sqrt(P(index, index));
		string type	= KF::_from_integral(key.type)._to_string();

		char dStr[20];
		char xStr[20];
		char pStr[20];
		char muStr[20];
		if (noAdjust)																snprintf(dStr, sizeof(dStr), "%15.0s", "");
		else if (_dx	== 0 || (fabs(_dx)	> 0.0001	&& fabs(_dx)	< 1e5))		snprintf(dStr, sizeof(dStr), "%15.8f",	_dx);
		else																		snprintf(dStr, sizeof(dStr), "%15.4e",	_dx);

		if		(_x		== 0 || (fabs(_x)	> 0.0001	&& fabs(_x)		< 1e8))		snprintf(xStr, sizeof(xStr), "%17.7f",	_x);
		else																		snprintf(xStr, sizeof(xStr), "%17.3e",	_x);

		if		(_sigma	== 0 || (fabs(_sigma)> 0.0001	&& fabs(_sigma)	< 1e8))		snprintf(pStr, sizeof(pStr), "%17.8f",	_sigma);
		else																		snprintf(pStr, sizeof(pStr), "%17.4e",	_sigma);

		double mu = 0;
		auto it = gaussMarkovMuMap.find(key);
		if (it != gaussMarkovMuMap.end())
			mu = it->second;

		if		(mu	== 0)															snprintf(muStr, sizeof(muStr), "");
		else if (fabs(mu)> 0.0001	&& fabs(mu)	< 1e8)								snprintf(muStr, sizeof(muStr), "%17.8f",	mu);
		else																		snprintf(muStr, sizeof(muStr), "%17.4e",	mu);


		tracepdeex(1, trace, "*\t%20s\t%20s\t%5s\t%3s\t%7d\t%s\t%s\t%s",
			time.to_string(0).c_str(),
			type.c_str(),
			key.str.c_str(),
			key.Sat.id().c_str(),
			key.num,
			xStr,
			pStr,
			dStr);
		tracepdeex(5, trace, "\t%17s",	muStr);
		tracepdeex(6, trace, "\t%x",	key.rec_ptr);
		tracepdeex(2, trace, "\t%-40s",	key.comment.c_str());
		tracepdeex(1, trace, "\n");
	}
}

MatrixXi correlationMatrix(
	MatrixXd& P)
{
	MatrixXi correlations = MatrixXi(P.rows(), P.cols());

	for (int i = 0; i <  P.rows();	i++)
	for (int j = 0; j <= i;			j++)
	{
		double v1	= P(i, i);
		double v2	= P(j, j);
		double v12	= P(i, j);

		double correlation = v12 / sqrt(v1 * v2) * 100;
		correlations(i, j) = correlation;
		correlations(j, i) = correlation;
	}

	return correlations;
}

void KFState::outputConditionNumber(
			Trace&		trace)
{
	Eigen::JacobiSVD<MatrixXd> svd(P.bottomRightCorner(P.rows()-1, P.cols()-1));
	double conditionNumber = svd.singularValues()(0) / svd.singularValues()(svd.singularValues().size()-1);

	tracepdeex(0, trace, "\n\n Condition number: %f", conditionNumber);
}

void KFState::outputCorrelations(
			Trace&		trace)
{
	tracepdeex(2, trace, "\n\n");

	Block block(trace, "CORRELATIONS");

	int skip	= 0;
	int total	= kfIndexMap.size();
	for (auto& [key, index] : kfIndexMap)
	{
		if (key.type == KF::ONE)
		{
			continue;
		}

		tracepdeex(2, trace, "%s      ", KFKey::emptyString().c_str());
		for (int i = 0; i < skip; i++)
		{
			tracepdeex(2, trace, "|    ");
		}

		trace << "> ";

		for (int i = 0; i < total - skip; i++)
		{
			tracepdeex(2, trace, "-----");
		}

		trace << key << "\n";

		skip++;
	}

	MatrixXi correlations = correlationMatrix(P);

	for (auto& [key, index] : kfIndexMap)
	{
		if (key.type == KF::ONE)
		{
			continue;
		}

		trace << key << " : ";

		for (auto& [key2, index2] : kfIndexMap)
		{
			if (key2.type == KF::ONE)
			{
				continue;
			}

			int correlation = correlations(index, index2);

			if		(index == index2)				tracepdeex(2, trace, "%4s ", "100");
			else if	(fabs(correlation) > 100)		tracepdeex(2, trace, "%4s ", "----");
			else if	(fabs(correlation) < 1)			tracepdeex(2, trace, "%4s ", "");
			else									tracepdeex(2, trace, "%4.0f ", correlation);
		}

		trace << "\n";
	}
}

void KFState::outputMeasurements(
		Trace&		trace,
		KFMeas&		meas)
{
	tracepdeex(2, trace, "\n\n");

	Block block(trace, "MEASUREMENTS");

	int total = kfIndexMap.size();
	int skip = 0;
	for (auto& [key, index] : kfIndexMap)
	{
		if (key.type == KF::ONE)
		{
			continue;
		}

		tracepdeex(0, trace, "%s        ", KFKey::emptyString().c_str());

		for (int i = 0; i < skip; i++)
		{
			tracepdeex(2, trace, "|      ");
		}

		trace << "> ";

		for (int i = 0; i < total - skip; i++)
		{
			tracepdeex(2, trace, "-------");
		}

		trace << key << "\n";

		skip++;
	}

	for (int i = 0; i < meas.obsKeys.size(); i++)
	{
		auto& key = meas.obsKeys[i];

		trace << key << " : ";

		for (int j = 1; j < meas.H.cols(); j++)
		{
			double a = meas.H(i,j);

			if (fabs(a) > 0.001)		tracepdeex(2, trace, "%6.2f ", a);
			else						tracepdeex(2, trace, "%6.2s ", "");
		}
		tracepdeex(2, trace, "\t   : %16.4f\n", meas.V(i));
	}
}

InitialState initialStateFromConfig(
	const KalmanModel&	kalmanModel,
	int					index)
{
	InitialState init = {};

	if (index < kalmanModel.estimate		.size())		init.estimate			= 		kalmanModel.estimate		[index];
	else													init.estimate			= 		kalmanModel.estimate		.back();
	if (index < kalmanModel.use_remote_sigma.size())		init.use_remote_sigma	= 		kalmanModel.use_remote_sigma[index];
	else													init.use_remote_sigma	= 		kalmanModel.use_remote_sigma.back();
	if (index < kalmanModel.apriori_value	.size())		init.x					= 		kalmanModel.apriori_value	[index];
	else													init.x					= 		kalmanModel.apriori_value	.back();
	if (index < kalmanModel.sigma			.size())		init.P					= SQR(	kalmanModel.sigma			[index])	* SGN(kalmanModel.sigma		[index]);
	else													init.P					= SQR(	kalmanModel.sigma			.back())	* SGN(kalmanModel.sigma		.back());
	if (index < kalmanModel.tau				.size())		init.tau				= 		kalmanModel.tau				[index];
	else													init.tau				= 		kalmanModel.tau				.back();
	if (index < kalmanModel.mu				.size())		init.mu					= 		kalmanModel.mu				[index];
	else													init.mu					= 		kalmanModel.mu				.back();
	if (index < kalmanModel.process_noise	.size())		init.Q					= SQR(	kalmanModel.process_noise	[index])	* SGN(kalmanModel.process_noise[index]);
	else													init.Q					= SQR(	kalmanModel.process_noise	.back())	* SGN(kalmanModel.process_noise.back());
	if (index < kalmanModel.comment			.size())		init.comment			= 		kalmanModel.comment			[index];
	else													init.comment			= 		kalmanModel.comment			.back();

	return init;
}

KFState mergeFilters(
	const vector<KFState*>&	kfStatePointerList,
	const vector<KF>&		stateList)
{
	map<KFKey, double>				stateValueMap;
	map<KFKey, map<KFKey, double>>	stateCovarMap;

	for (auto& statePointer : kfStatePointerList)
	{
		KFState& kfState = *statePointer;

		for (auto& [key1, index1] : kfState.kfIndexMap)
		for (auto state1 : stateList)
		{
			if (key1.type == +state1)
			{
				stateValueMap[key1] = kfState.x(index1);

				for (auto& [key2, index2] : kfState.kfIndexMap)
				for (auto state2 : stateList)
				{
					if (key2.type == +state2)
					{
						double val = kfState.P(index1, index2);
						if (val != 0)
						{
							stateCovarMap[key1][key2] = val;
						}

						break;
					}
				}
				break;
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

bool isPositiveSemiDefinite(
	MatrixXd& mat)
{
	for (int i = 0; i < mat.rows(); i++)
	for (int j = 0; j < i; j++)
	{
		double a	= mat(i, i);
		double ab	= mat(i, j);
		double b	= mat(j, j);

		if (ab * ab > a * b)
		{
// 			std::cout << "large off diagonals " << "\n";
// 			return false;
			if (ab > 0) ab = +sqrt(0.99 * a * b);
			else		ab = -sqrt(0.99 * a * b);
			mat(i, j) = ab;
			mat(j, i) = ab;
		}
	}
	return true;
}
