
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
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit)
{
	if (acsConfig.measErrors.enable == false)
	{
		return true;
	}

	double deweightFactor = acsConfig.measErrors.deweight_factor;

	auto& key = kfMeas.obsKeys[index];

	InteractiveTerminal ss("Deweights", trace, false);

	ss << "\n" << kfState.time.to_string() << "\tDeweighting " << key << " - " << key.comment;

	kfState.statisticsMap["Meas deweight"]++;

	char buff[64];
	snprintf(buff, sizeof(buff), "Meas Deweight-%4s-%s-%sfit",		key.str.c_str(),		KF::_from_integral(key.type)._to_string(), postFit ? "Post" : "Pre");			kfState.statisticsMap[buff]++;
	snprintf(buff, sizeof(buff), "Meas Deweight-%4s-%s-%sfit",		key.Sat.id().c_str(),	KF::_from_integral(key.type)._to_string(), postFit ? "Post" : "Pre");			kfState.statisticsMap[buff]++;

	kfMeas.R.row(index) *= deweightFactor;
	kfMeas.R.col(index) *= deweightFactor;

	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

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
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit)
{
	if (kfMeas.metaDataMaps[index]["pseudoObs"] == (void*) false)
	{
		return true;
	}

	for (auto& [key, state] : kfState.kfIndexMap)
	{
		if	( kfMeas.H(index, state)
			&&key.type == KF::ORBIT)
		{
			orbitGlitchReaction(trace, kfState, kfMeas, key, postFit);
		}
	}

	return true;
}

/** Deweight measurement and its relatives
 */
bool deweightStationMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit)
{
	string id = kfMeas.obsKeys[index].str;

	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		auto& key = kfMeas.obsKeys[i];

		if (key.str != id)
		{
			continue;
		}

		double deweightFactor = acsConfig.stateErrors.deweight_factor;

		trace << "\n" << "Deweighting " << key << " - " << key.comment << "\n";

		kfState.statisticsMap["Receiver deweight"]++;

		char buff[64];
		snprintf(buff, sizeof(buff), "Receiver Deweight-%4s-%sfit", key.str.c_str(),	postFit ? "Post" : "Pre");			kfState.statisticsMap[buff]++;

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
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	unsigned int* phaseRejectCount_ptr = (unsigned int*) metaDataMap["phaseRejectCount"];

	if (phaseRejectCount_ptr == nullptr)
	{
		return true;
	}

	unsigned int&	phaseRejectCount	= *phaseRejectCount_ptr;

	//increment counter, and clear the pointer so it cant be reset to zero in subsequent operations (because this is a failure)
	phaseRejectCount++;
	metaDataMap["phaseRejectCount"] = nullptr;

	trace << "\n" << "Incrementing phaseRejectCount on " << kfMeas.obsKeys[index].Sat.id() << " to " << phaseRejectCount;

	return true;
}

/** Count all errors on receiver
 */
bool incrementReceiverError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	unsigned int* receiverErrorCount_ptr = (unsigned int*) metaDataMap["receiverErrorCount"];

	if (receiverErrorCount_ptr == nullptr)
	{
		return true;
	}

	unsigned int&	receiverErrorCount	= *receiverErrorCount_ptr;

	//increment counter, and clear the pointer so it cant be reset to zero in subsequent operations (because this is a failure)
	receiverErrorCount++;
	metaDataMap["receiverErrorFlag"] = nullptr;

	trace << "\n" << "Incrementing receiverErrorCount on " << kfMeas.obsKeys[index] << " to " << receiverErrorCount;

	return true;
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


bool resetPhaseSignalOutage(
	const	GTime&		time,
			KFMeas&		kfMeas,
			int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	for (auto suffix : {"", "_alt"})
	{
		GTime* lastPhaseTime_ptr = (GTime*) metaDataMap[(string)"lastPhaseTime" + suffix];

		if (lastPhaseTime_ptr == nullptr)
		{
			return true;
		}

		GTime& lastPhaseTime = *lastPhaseTime_ptr;

		lastPhaseTime = time;
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
			Trace&		trace,
			KFState&	kfState,
			KFMeas&		kfMeas,
	const	KFKey&		kfKey,
			bool		postFit)
{
	if (acsConfig.stateErrors.enable == false)
	{
		return true;
	}

	trace << "\n" << "Bad state detected " << kfKey << " - rejecting all referencing measurements" << "\n";

	kfState.statisticsMap["State rejection"]++;

	int stateIndex = kfState.getKFIndex(kfKey);

	for (int meas = 0; meas < kfMeas.H.rows(); meas++)
	{
		if (kfMeas.H(meas, stateIndex))
		{
			kfState.doMeasRejectCallbacks(trace, kfMeas, meas, postFit);
		}
	}

	return true;
}

/** Remove any states connected to a bad clock if it glitches
 */
// bool clockGlitchReaction(		//todo aaron orphan
// 			Trace&		trace,
// 			KFState&	kfState,
// 			KFMeas&		kfMeas,
// 	const	KFKey&		kfKey)
// {
// 	if	(  kfKey.type != KF::SAT_CLOCK
// 		&& kfKey.type != KF::REC_SYS_BIAS)
// 	{
// 		return true;
// 	}
//
// 	if (acsConfig.reinit_on_clock_error == false)
// 	{
// 		return true;
// 	}
//
// 	trace << "\n" << "Bad clock detected " << kfKey << " - resetting linked states" << "\n";
//
// 	kfState.statisticsMap["Clock glitch"]++;
//
// 	for (auto& [key, index] : kfState.kfIndexMap)
// 	{
// 		if	(  kfKey.type	== KF::SAT_CLOCK
// 			&& kfKey.Sat	== key.Sat
// 			&&( key	.type	== KF::AMBIGUITY
// 			  ||key	.type	== KF::SAT_CLOCK))
// 		{
// 			//remove the satellite clock, and any ambiguities that are connected to it.
// 			trace << "- Removing " << key << "\n";
//
// 			kfState.removeState(key);
// 		}
//
// 		if	(  kfKey.type	== KF::REC_SYS_BIAS
// 			&& kfKey.str	== key.str
// 			&&( key	.type	== KF::AMBIGUITY
// 			  ||key	.type	== KF::REC_SYS_BIAS))
// 		{
// 			//remove the satellite clock, and any ambiguities that are connected to it.
// 			trace << "- Removing " << key << "\n";
//
// 			kfState.removeState(key);
//
// 			if (kfKey.rec_ptr)
// 			{
// 				//make sure receiver clock corrections get reset too.
// 				trace << "- Resetting clock adjustment" << "\n";
//
// 				auto& rec = *kfKey.rec_ptr;
//
// 				rec.sol.deltaDt_net_old[E_Sys::GPS] = 0;
// 			}
// 		}
// 	}
//
// 	return true;
// }


bool orbitGlitchReaction(
			Trace&		trace,
			KFState&	kfState,
			KFMeas&		kfMeas,
	const	KFKey&		kfKey,
			bool		postFit)
{
	if (kfKey.type != KF::ORBIT)
	{
		return true;
	}

	if (acsConfig.orbErrors.enable == false)
	{
		return true;
	}

	trace << "\n" << "Bad orbit state detected " << kfKey;

	kfState.statisticsMap["Orbit state reject"]++;

	Exponential exponentialNoise;
	exponentialNoise.tau	=		acsConfig.orbErrors.vel_proc_noise_trail_tau;
	exponentialNoise.value	= SQR(	acsConfig.orbErrors.vel_proc_noise_trail);

	MatrixXd F = MatrixXd::Identity	(kfState.x.rows(), kfState.x.rows());
	MatrixXd Q = MatrixXd::Zero		(kfState.x.rows(), kfState.x.rows());

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	(  key.type	!= KF::ORBIT
			|| key.str	!= kfKey.str
			|| key.Sat	!= kfKey.Sat)
		{
			continue;
		}

		if (key.num	< 3)
		{
			Q(index, index) = SQR(acsConfig.orbErrors.pos_proc_noise);
		}
		else
		{
			Q(index, index) = SQR(acsConfig.orbErrors.vel_proc_noise);

			kfState.setExponentialNoise(key, exponentialNoise);
		}
	}

	kfState.manualStateTransition(trace, kfState.time, F, Q);

	return false;
}

