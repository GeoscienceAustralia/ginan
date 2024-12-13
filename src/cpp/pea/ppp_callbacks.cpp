
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include <sstream>

using std::ostringstream;

#include "interactiveTerminal.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "ppp.hpp"

/** Deweight worst measurement
 */
bool deweightMeas(
	RejectCallbackDetails	rejectDetails)
{
	auto& trace			= rejectDetails.trace;
	auto& kfMeas		= rejectDetails.kfMeas;
	auto& kfState		= rejectDetails.kfState;
	auto& measIndex		= rejectDetails.measIndex;
	auto& postFit		= rejectDetails.postFit;

	if (acsConfig.measErrors.enable == false)
	{
		return true;
	}

	double deweightFactor;

	if (rejectDetails.scalar)		deweightFactor = acsConfig.stateErrors.	deweight_factor * rejectDetails.scalar;
	else							deweightFactor = acsConfig.measErrors.	deweight_factor;

	auto& key = kfMeas.obsKeys[measIndex];

	InteractiveTerminal ss("Deweights", trace, false);

	double preSigma = sqrt(kfMeas.R(measIndex,measIndex));
	double residual;

	string description;
	if (postFit)	{	description = "postfit";	residual = kfMeas.VV(measIndex);	}
	else			{	description = "prefit";		residual = kfMeas.V	(measIndex);	}

	addRejectDetails(kfState.time, trace, kfState, key, "Measurement Deweighted", description,
					 {
						 {"preDeweightSigma",			preSigma},
						 {description + "Residual",		residual},
						 {description + "Ratio",	abs(residual / preSigma)}
					});

	kfMeas.R.row(measIndex) *= deweightFactor;
	kfMeas.R.col(measIndex) *= deweightFactor;

	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

	MatrixXd*	otherNoiseMatrix_ptr	= (MatrixXd*)	metaDataMap["otherNoiseMatrix_ptr"];
	long int	otherIndex				= (long int)	metaDataMap["otherIndex"];

	if (otherNoiseMatrix_ptr)
	{
		//some measurements have noise matrices in 2 places (mincon) - update the other one too if its available.
		auto& otherNoiseMatrix = *otherNoiseMatrix_ptr;

		otherNoiseMatrix.row(otherIndex) *= deweightFactor;
		otherNoiseMatrix.col(otherIndex) *= deweightFactor;
	}

	return true;
}

/** Call state rejection functions when a measurement is a pseudo observation
 */
bool pseudoMeasTest(
	RejectCallbackDetails	rejectDetails)
{
	auto& kfMeas	= rejectDetails.kfMeas;
	auto& kfState	= rejectDetails.kfState;
	auto& measIndex	= rejectDetails.measIndex;

	if (kfMeas.metaDataMaps[measIndex]["pseudoObs"] == (void*) false)
	{
		return true;
	}

	for (auto& [key, state] : kfState.kfIndexMap)
	{
		if	( kfMeas.H(measIndex, state)
			&&key.type == KF::ORBIT)
		{
			satelliteGlitchReaction(rejectDetails);
		}
	}

	return true;
}

/** Deweight measurement and its relatives
 */
bool deweightStationMeas(
	RejectCallbackDetails	rejectDetails)
{
	auto& trace		= rejectDetails.trace;
	auto& kfMeas	= rejectDetails.kfMeas;
	auto& kfState	= rejectDetails.kfState;
	auto& measIndex	= rejectDetails.measIndex;
	auto& postFit	= rejectDetails.postFit;

	string id = kfMeas.obsKeys[measIndex].str;

	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		auto& key = kfMeas.obsKeys[i];

		if (key.str != id)
		{
			continue;
		}

		double deweightFactor = acsConfig.stateErrors.deweight_factor;

		addRejectDetails(kfState.time, trace, kfState, key, "Station Meas Deweighted", postFit ? "Postfit" : "Prefit");

		kfMeas.R.row(i) *= deweightFactor;
		kfMeas.R.col(i) *= deweightFactor;

		map<string, void*>& metaDataMap = kfMeas.metaDataMaps[i];

		bool* used_ptr = (bool*) metaDataMap["used_ptr"];

		if (used_ptr)
		{
			*used_ptr = false;
		}

		MatrixXd*	otherNoiseMatrix_ptr	= (MatrixXd*)	metaDataMap["otherNoiseMatrix_ptr"];
		long int	otherIndex				= (long int)	metaDataMap["otherIndex"];

		if (otherNoiseMatrix_ptr)
		{
			//some measurements have noise matrices in 2 places (mincon) - update the other one too if its available.
			auto& otherNoiseMatrix = *otherNoiseMatrix_ptr;

			otherNoiseMatrix.row(otherIndex) *= deweightFactor;
			otherNoiseMatrix.col(otherIndex) *= deweightFactor;
		}
	}
	return true;
}

/** Count worst measurement
 */
bool incrementPhaseSignalError(
	RejectCallbackDetails	rejectDetails)
{
	auto& trace		= rejectDetails.trace;
	auto& kfMeas	= rejectDetails.kfMeas;
	auto& measIndex	= rejectDetails.measIndex;

	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

	for (auto suffix : {"", "_alt"})
	{
		string metaData = "phaseRejectCount";
		metaData += suffix;

		unsigned int* phaseRejectCount_ptr = (unsigned int*) metaDataMap[metaData];

		if (phaseRejectCount_ptr == nullptr)
		{
			continue;
		}

		unsigned int&	phaseRejectCount	= *phaseRejectCount_ptr;

		//increment counter, and clear the pointer so it cant be reset to zero in subsequent operations (because this is a failure)
		phaseRejectCount++;
		metaDataMap[metaData] = nullptr;

		trace << "\n" << "Incrementing phaseRejectCount on " << kfMeas.obsKeys[measIndex].Sat.id() << " to " << phaseRejectCount;
	}

	return true;
}

/** Count all errors on receiver
 */
bool incrementReceiverErrors(
	RejectCallbackDetails	rejectDetails)
{
	auto& trace		= rejectDetails.trace;
	auto& kfMeas	= rejectDetails.kfMeas;
	auto& measIndex	= rejectDetails.measIndex;

	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

	unsigned int* receiverErrorCount_ptr	= (unsigned int*) metaDataMap["receiverErrorCount"];

	if (receiverErrorCount_ptr	== nullptr)
	{
		return true;
	}

	unsigned int&	receiverErrorCount	= *receiverErrorCount_ptr;

	//increment counters,
	receiverErrorCount++;

	trace << "\n" << "Incrementing receiverErrorCount on " << kfMeas.obsKeys[measIndex] << " to " << receiverErrorCount;

	return true;
}
/** Count all errors on satellite
 */
bool incrementSatelliteErrors(
	RejectCallbackDetails	rejectDetails)
{
	if (acsConfig.errorAccumulation.enable == false)
	{
		return true;
	}

	auto& trace		= rejectDetails.trace;
	auto& kfMeas	= rejectDetails.kfMeas;
	auto& kfState	= rejectDetails.kfState;
	auto& measIndex	= rejectDetails.measIndex;

	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

	unsigned int* satelliteErrorCount_ptr	= (unsigned int*) metaDataMap["satelliteErrorCount"];
	unsigned int* satelliteErrorEpochs_ptr	= (unsigned int*) metaDataMap["satelliteErrorEpochs"];

	if	( satelliteErrorCount_ptr	== nullptr
		||satelliteErrorEpochs_ptr	== nullptr)
	{
		return true;
	}

	unsigned int&	satelliteErrorCount		= *satelliteErrorCount_ptr;
	unsigned int&	satelliteErrorEpochs	= *satelliteErrorEpochs_ptr;

	satelliteErrorCount++;

	trace << "\n" << "Incrementing satelliteErrorCount on " << kfMeas.obsKeys[measIndex] << " to " << satelliteErrorCount;

	if (satelliteErrorCount < acsConfig.errorAccumulation.satellite_error_count_threshold)
	{
		return true;
	}

	satelliteErrorEpochs++;

	if (satelliteErrorEpochs < acsConfig.errorAccumulation.satellite_error_epochs_threshold)
	{
		return true;
	}

	KFKey kfKey;
	kfKey.Sat	= kfMeas.obsKeys[measIndex].Sat;

	trace << "\n" << "Satellite relaxed due to high satellite error counts: " << kfKey;

	satelliteGlitchReaction(rejectDetails);

	kfState.statisticsMap["Sat error resets"]++;

	return false;
}

bool resetPhaseSignalError(
	const	GTime&		time,
			KFMeas&		kfMeas,
			int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	//these will have been set to null if there was an error after adding the measurement to the list
	for (auto suffix : {"", "_alt"})
	{
		unsigned int* phaseRejectCount_ptr = (unsigned int*) metaDataMap[(string)"phaseRejectCount" + suffix];

		if (phaseRejectCount_ptr == nullptr)
		{
			return true;
		}

		unsigned int&	phaseRejectCount	= *phaseRejectCount_ptr;

		phaseRejectCount = 0;
	}

	return true;
}

bool resetIonoSignalOutage(
	const	GTime&		time,
			KFMeas&		kfMeas,
			int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	for (auto suffix : {"", "_alt"})
	{
		GTime* lastIonTime_ptr = (GTime*) metaDataMap[(string)"lastIonTime" + suffix];

		if (lastIonTime_ptr == nullptr)
		{
			return true;
		}

		GTime& lastIonTime = *lastIonTime_ptr;

		lastIonTime = time;
	}

	return true;
}

/** Reject measurements attached to worst state using measurement reject callback list
 */
bool rejectByState(
	RejectCallbackDetails	rejectDetails)
{
	auto& trace		= rejectDetails.trace;
	auto& kfKey		= rejectDetails.kfKey;
	auto& kfMeas	= rejectDetails.kfMeas;
	auto& kfState	= rejectDetails.kfState;

	if (acsConfig.stateErrors.enable == false)
	{
		return true;
	}

	trace << "\n" << "Bad state detected " << kfKey << " - rejecting all referencing measurements" << "\n";

	kfState.statisticsMap["State rejection"]++;

	int stateIndex = kfState.getKFIndex(kfKey);

	for (int meas = 0; meas < kfMeas.H.rows(); meas++)
	{
		rejectDetails.measIndex	= meas;
		rejectDetails.scalar	= abs(kfMeas.H(meas, stateIndex));

		if (rejectDetails.scalar == 0)
		{
			continue;
		}

		if (acsConfig.stateErrors.scale_by_design_entry == false)
		{
			rejectDetails.scalar = 1;
		}

		kfState.doMeasRejectCallbacks(rejectDetails);
	}

	return true;
}

/** Immediately executed reaction to orbital state errors.
 * Note there is also a 1 epoch delayed reaction function
 */
bool satelliteGlitchReaction(
	RejectCallbackDetails	rejectDetails)
{
	if (acsConfig.satelliteErrors.enable == false)
	{
		return true;
	}

	auto& trace		= rejectDetails.trace;
	auto& kfKey		= rejectDetails.kfKey;
	auto& kfState	= rejectDetails.kfState;

	trace << "\n" << "Bad satellite state detected " << kfKey;

	kfState.statisticsMap["Satellite state reject"]++;

	Exponential exponentialNoise;
	exponentialNoise.tau	=		acsConfig.satelliteErrors.vel_proc_noise_trail_tau;
	exponentialNoise.value	= SQR(	acsConfig.satelliteErrors.vel_proc_noise_trail);

	MatrixXd F = MatrixXd::Identity	(kfState.x.rows(), kfState.x.rows());
	MatrixXd Q = MatrixXd::Zero		(kfState.x.rows(), kfState.x.rows());

	bool transitionRequired = false;

	if	( kfKey.type == KF::NONE
		||kfKey.type == KF::ORBIT)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	(  key.type	!= KF::ORBIT
			|| key.str	!= kfKey.str
			|| key.Sat	!= kfKey.Sat)
		{
			continue;
		}

		if	( key.num < 3
			&&acsConfig.satelliteErrors.pos_proc_noise)
		{
			trace << "\n - Adding " << acsConfig.satelliteErrors.pos_proc_noise << " to sigma of " << key;

			Q(index, index) = SQR(acsConfig.satelliteErrors.pos_proc_noise);

			transitionRequired = true;
		}

		if	( key.num >= 3
			&&acsConfig.satelliteErrors.vel_proc_noise)
		{
			trace << "\n - Adding " << acsConfig.satelliteErrors.vel_proc_noise << " to sigma of " << key;

			Q(index, index) = SQR(acsConfig.satelliteErrors.vel_proc_noise);

			kfState.setExponentialNoise(key, exponentialNoise);

			transitionRequired = true;
		}
	}

	if	( kfKey.type == KF::NONE
		||kfKey.type == KF::SAT_CLOCK)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	(  key.type	!= KF::SAT_CLOCK
			|| key.str	!= kfKey.str
			|| key.Sat	!= kfKey.Sat)
		{
			continue;
		}

		if (acsConfig.satelliteErrors.clk_proc_noise)
		{
			trace << "\n - Adding " << acsConfig.satelliteErrors.clk_proc_noise << " to sigma of " << key;

			Q(index, index) = SQR(acsConfig.satelliteErrors.clk_proc_noise);

			transitionRequired = true;
		}
	}

	if (transitionRequired)
	{
		int index = -1;

		auto it = kfState.kfIndexMap.find(kfKey);
		if (it != kfState.kfIndexMap.end())
		{
			index = it->second;
		}

		if (index >= 0)
		{
			trace << "\n - Pre-transition state sigma: " << kfState.P(index, index);
		}

		kfState.manualStateTransition(trace, kfState.time, F, Q);

		if (index >= 0)
		{
			trace << "\n - Post-transition state sigma: " << kfState.P(index, index);
		}

		return false;
	}

	return true;
}

