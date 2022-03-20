#include <iostream>
#include <vector>
#include <list>
#include <algorithm>
#include <chrono>

using std::cout;
using std::endl;
using std::vector;
using std::list;

#include <boost/math/distributions/students_t.hpp>
#include <boost/math/distributions/normal.hpp>

using boost::math::students_t;
using boost::math::cdf;
using boost::math::complement;
using boost::math::normal;
using boost::math::quantile;

#include "cycleSlip.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "ppp.hpp"

static CSAnalyserSingle	csAnalyserL1;			///< Cycle slip analysis object for L1. Contains time-series data from multiple epochs
static CSAnalyserSingle	csAnalyserL2;			///< Cycle slip analysis object for L2. Contains time-series data from multiple epochs
static CSAnalyserCombo	csAnalyserLc(calcLc);	///< Cycle slip analysis object for Lc. Contains time-series data from multiple epochs
typedef std::chrono::steady_clock Clock; 		///< For measuring program runtime durations


/** \file
* This software performs cycle slip detection and repair, given prefit residual data.
*
* The basic workflow for performing this analysis is:
* create CSAnalyser objects
* in epoch 1, initialise internal param's + call init()
* every epoch, call cycleSlipDetectRepair()
*/


// External functions


/** All-in-one function for cycle slip detection + repair
*/
bool	cycleSlipDetectRepair(
	CSAnalyserSingle&		csAnalyserL1,	///< Object containing cycle-slip-related data for L1 frequency
	CSAnalyserSingle&		csAnalyserL2,	///< Object containing cycle-slip-related data for L2 frequency
	CSAnalyserCombo&		csAnalyserLc,	///< Object containing cycle-slip-related data for Lc frequency
	const KFMeasEntryList&	kfPrefitL1,		///< L1 prefit residuals
	const KFMeasEntryList&	kfPrefitL2,		///< L2 prefit residuals
	const KFMeasEntryList&	kfPrefitLc,		///< Lc prefit residuals
	int						epochNum,		///< Epoch number
	KFState&				kfState,		///< Kalman filter object containing the network state parameters
	KFMeasEntryList&		kfMeasEntryList,///< List of input measurements as lists of entries
	map<KFKey, bool>&		measuredStates)	///< Map of measured states in this epoch
{

	// Clean prefit data
	ObsValMap prefitL1 = kfMeasEntryListToObsValMap(kfPrefitL1);	// Convert KFMeasEntryList to ObsValMap
	ObsValMap prefitL2 = kfMeasEntryListToObsValMap(kfPrefitL2);
	ObsValMap prefitLc = kfMeasEntryListToObsValMap(kfPrefitLc);
	csAnalyserL1.adjustAmb(prefitL1, kfState, epochNum);
	csAnalyserL2.adjustAmb(prefitL2, kfState, epochNum);
	csAnalyserL1.cleanData(prefitL1, epochNum);
	csAnalyserL2.cleanData(prefitL2, epochNum);

	// Detect outliers in L1/L2 that are large enough to be potential cycle slips
	csAnalyserL1.detectOutliers(epochNum);
	csAnalyserL2.detectOutliers(epochNum);

	// Classify previously marked outliers as either outliers or cycle slips, then estimate + validate int solution
	csAnalyserL1.classifySlips(epochNum);
	csAnalyserL2.classifySlips(epochNum);

	// Verify L1/L2 int solutions with Lc
	csAnalyserLc.cleanData(prefitLc, epochNum);
	ObsBoolMap lcValidated = csAnalyserLc.validateSingleSols(csAnalyserL1.slipIntSols, csAnalyserL2.slipIntSols);
	csAnalyserL1.applyExternalSlipValidation(lcValidated);
	csAnalyserL2.applyExternalSlipValidation(lcValidated);

	// Combine L1/L2 outlier/slip analysis into Lc and apply to KF
	csAnalyserLc.combineSingleSols(csAnalyserL1.slipIntSols, csAnalyserL2.slipIntSols, csAnalyserL1.ambToReinit, csAnalyserL2.ambToReinit, csAnalyserL1.deweightLog, csAnalyserL2.deweightLog);
	bool stateModified = csAnalyserLc.applyAdjustmentsToKF(kfState, kfMeasEntryList, measuredStates);

	// Repair timeseries's within each analyser object for future analysis
	csAnalyserL1.repairInternalTS(); // Adjust internal time-series to account for repaired CS's / reinitialised ambiguities
	csAnalyserL2.repairInternalTS();
	csAnalyserLc.repairInternalTS();

	// Decrement outlier deweight durations + clear ambiguity repair/reinit queue
	csAnalyserL1.incrementTime();
	csAnalyserL2.incrementTime();
	csAnalyserLc.incrementTime();

	return stateModified; // If true, KFState.stateTransition() needs to be rerun to apply changes made to state vector
}


/** Prints a KFMeasList to file
*/
void	printToFile(
	const KFMeasEntryList& kfMeasEntryList,		///< KFMeasEntryList to print
	string					filename,			///< File to print to
	int						epochNum)			///< Epoch number
{
	std::ofstream out;
	out.open(filename, std::ios::app);
	for (auto& meas : kfMeasEntryList)
	{
		out << epochNum << " " << meas.obsKey << " " << meas.value << " " << meas.noise << " " << meas.innov << std::endl;
	}
	out.close();
}



// CSAnalyserBase functions

/** Resets debug files
*/
void	CSAnalyserBase::resetDebug()
{
	std::ofstream out;
	out.open(freqName + "-prefit.dbg", std::ios::out);			out << "Start" << std::endl;	out.close();
	out.open(freqName + "-dPrefit.dbg", std::ios::out);			out << "Start" << std::endl;	out.close();
	out.open(freqName + "-dPrefitIqm.dbg", std::ios::out);		out << "Start" << std::endl;	out.close();
	out.open(freqName + "-dPrefitLessCom.dbg", std::ios::out);	out << "Start" << std::endl;	out.close();
	out.open(freqName + "-dPrefitClean.dbg", std::ios::out);	out << "Start" << std::endl;	out.close();
	out.open(freqName + "-prefitClean.dbg", std::ios::out);		out << "Start" << std::endl;	out.close();
	out.open(freqName + "-csTSData.dbg", std::ios::out);		out << "Start" << std::endl;	out.close();
}


/** Clean prefit resid data
*/
void	CSAnalyserBase::cleanData(
	const ObsValMap&		prefit,		///< [in]	Prefit residuals for each channel
	int						epochNum)	///< [in]	Epoch number, starting from 1
{
	initClearTimeseries(prefit, prefitTSMap);	// Prep timeseries for the current epoch - create new timeseries if missing, and clear obsolete
	ObsValMap dPrefit;
	for (auto& [obsKey, val] : prefit)
	{
		// Add current prefit value to timeseries
		assert(prefitTSMap.find(obsKey) != prefitTSMap.end()); // Note - entry should be added by initClearTimeseries()
		prefitTSMap.at(obsKey).push_back(val);

		// Time-difference last 2 time-series values (for each channel)
		int numPts = prefitTSMap.at(obsKey).size();
		if (numPts >= 2 + newChannelBreakInDuration)
		{
			dPrefit[obsKey] = prefitTSMap.at(obsKey).at(numPts - 1) - prefitTSMap.at(obsKey).at(numPts - 2); // diff between last & 2nd-last timeseries value
		}
	}

	// Calculate common mode
	ObsValMap dPrefitCom = calcStationCommonModes(dPrefit, commonModeMethod);
	initClearTimeseries(dPrefit, dPrefitLessComTSMap);
	initClearTimeseries(dPrefit, prefitCleanTSMap);

	// Debugging objects
	ObsValMap dPrefitLessComMap;
	ObsValMap dPrefitCleanMap;
	ObsValMap prefitCleanMap;

	for (auto& [obsKey, val] : dPrefit)
	{
		// Subtract common mode + append result to timeseries
		ObsKey recOnlyObsKey = obsKey;
		recOnlyObsKey.Sat = {};
		assert(dPrefitCom.find(recOnlyObsKey) != dPrefitCom.end());
		double dPrefitLessCom = val - dPrefitCom.at(recOnlyObsKey);
		dPrefitLessComMap[obsKey] = dPrefitLessCom;
		assert(dPrefitLessComTSMap.find(obsKey) != dPrefitLessComTSMap.end());
		dPrefitLessComTSMap.at(obsKey).push_back(dPrefitLessCom);


		// Check that there are sufficient data points to calculate moving average & continue data cleaning
		if (dPrefitLessComTSMap.at(obsKey).size() >= dMovingAveWin + slipClassifyPostOutlierPts)
		{

			// Calculate moving average
			vector<double> movingAveSample = subVecExclLastN(subVecLastN(dPrefitLessComTSMap.at(obsKey), dMovingAveWin + slipClassifyPostOutlierPts), slipClassifyPostOutlierPts);
			double ave = calcMean(movingAveSample);


			// Subtract moving average + append result to timeseries
			double dPrefitClean = dPrefitLessCom - ave;
			dPrefitCleanMap[obsKey] = dPrefitClean; // (for debugging)

			// Integrate + append result to timeseries
			assert(prefitCleanTSMap.find(obsKey) != prefitCleanTSMap.end());
			if (prefitCleanTSMap.at(obsKey).empty())
			{
				prefitCleanTSMap.at(obsKey).push_back(0);
			}
			double prefitClean = prefitCleanTSMap.at(obsKey).back() + dPrefitClean;
			prefitCleanMap[obsKey] = prefitClean;
			prefitCleanTSMap.at(obsKey).push_back(prefitClean);
		}
	}


	// Debugging
	if (debug)
	{
		printToFile(prefit, freqName + "-prefit.dbg", epochNum);
		printToFile(dPrefit, freqName + "-dPrefit.dbg", epochNum);
		printToFile(dPrefitCom, freqName + "-dPrefitCom.dbg", epochNum);
		printToFile(dPrefitLessComMap, freqName + "-dPrefitLessCom.dbg", epochNum);
		printToFile(dPrefitCleanMap, freqName + "-dPrefitClean.dbg", epochNum);
		printToFile(prefitCleanMap, freqName + "-prefitClean.dbg", epochNum);
	}
}


/** Repair internal timeseries' data to account for slip repair + ambiguity reinitialisation
*/
void	CSAnalyserBase::repairInternalTS()
{
	list<ObsKey> slipObsKeys = getSlipSolObsKeys();

	// Apply cycle slip solution to ambiguity via state transition matrix
	for (auto& obsKey : slipObsKeys)
	{
		double ambCorrection = getAmbCorrection(obsKey);

		// prefitTSMap - correct data from outlier to present
		int tsSize = prefitTSMap.at(obsKey).size();
		assert(slipClassifyPostOutlierPts <= tsSize);
		for (int i = tsSize - slipClassifyPostOutlierPts; i < tsSize; ++i)
		{
			prefitTSMap.at(obsKey).at(i) -= ambCorrection;
		}

		// dPrefitLessComTSMap - correct data point at outlier
		tsSize = dPrefitLessComTSMap.at(obsKey).size();
		assert(slipClassifyPostOutlierPts <= tsSize);
		dPrefitLessComTSMap.at(obsKey).at(tsSize - slipClassifyPostOutlierPts) -= ambCorrection;

		// prefitCleanTSMap - correct data from outlier to present
		tsSize = prefitCleanTSMap.at(obsKey).size();
		assert(slipClassifyPostOutlierPts <= tsSize);
		for (int i = tsSize - slipClassifyPostOutlierPts; i < tsSize; ++i)
		{
			prefitCleanTSMap.at(obsKey).at(i) -= ambCorrection;
		}
	}


	// Clear timeseries if ambiguity is to be reinitialised
	for (auto& obsKey : ambToReinit)
	{
		assert(prefitTSMap.find(obsKey) != prefitTSMap.end());
		prefitTSMap.at(obsKey).clear();
		assert(dPrefitLessComTSMap.find(obsKey) != dPrefitLessComTSMap.end());
		dPrefitLessComTSMap.at(obsKey).clear();
		assert(prefitCleanTSMap.find(obsKey) != prefitCleanTSMap.end());
		prefitCleanTSMap.at(obsKey).clear();
	}
}



// CSAnalyserSingle functions

/** Set internal config values
*/
void	CSAnalyserSingle::init()
{
	assert(isInit == false);
	isInit = true;
	if (freq == 0)
	{
		std::cout << "Error - CSAnalyserSingle.freq = 0. Set to non-zero value before calling init()!" << std::endl;
		assert(freq != 0);
	}
	wavelength = 1 / freq * CLIGHT;

	// Reset debug files
	if (debug)	resetDebug();
};


/** Adjust incoming prefit data by subtracting the accumulated deteted slips on this channel, and adding back the Lc ambiguity (previously subtracted in the prefit calculation)
*/
void	CSAnalyserSingle::adjustAmb(
	ObsValMap&				prefit,
	KFState&				kfState,
	int						epochNum)
{
	initZeroObsIntMap(prefit, accumSlips);
	for (auto& [obsKey, val] : prefit)
	{
		// Subtract previously detected slips on this frequency
		assert(accumSlips.find(obsKey) != accumSlips.end());
		val -= accumSlips.at(obsKey) * wavelength;

		// Add back Lc amb
		KFKey ambiguityKey = { KF::AMBIGUITY, obsKey.Sat,	obsKey.str, 0, nullptr };
		double ambVal = 0;
		kfState.getKFValue(ambiguityKey, ambVal);
		val += ambVal;
	}
}


/** Detect outliers in cleaned prefit data
*/
void	CSAnalyserSingle::detectOutliers(
	int 					epochNum)	///< [in]	Epoch number, starting from 1
{
	// Perform outlier analysis on cleaned data.
	for (auto& [obsKey, prefitCleanTS] : prefitCleanTSMap)
	{
		// Check that there are sufficient data points to perform outlier analysis
		if (prefitCleanTS.size() >= outlierDetectMinPts)
		{
			vector<double> tsData = subVecLastN(prefitCleanTS, outlierDetectMinPts);

			// Detect if current data point is an outlier
			vector<double> baseline = subVecExclLastN(tsData, 1);
			double curr = tsData.back();
			bool outlierDetected = calcIfOutlier(curr, baseline, outlierPAlpha);

			if (outlierDetected)
			{
				double mean = calcMean(baseline);
				double stdDev = calcStdDev(baseline);
				bool isOutlierLargeEnough = rightTailedTest(wavelength, fabs(curr - mean), stdDev, minSizePAlpha); // If false, an outlier has been detected but it is v. unlikely that it is >= the minimum cycle slip size - ignore these.
				if (isOutlierLargeEnough)
				{
					// Mark as outlier to deweight
					int outlierDeweightDuration = slipClassifyPostOutlierPts - 1;
					deweightLog[obsKey] = outlierDeweightDuration;

					if (printActivity)
					{
						cout << "Outlier detected -";
						cout << " Rec: " << obsKey.str;
						cout << " Sat: " << (string)obsKey.Sat;
						cout << endl;
					}
				}
			}
		}
	}
}


/** Classify previously detected potential slips as either slips or outliers
*/
void	CSAnalyserSingle::classifySlips(
	int 					epochNum)	///< [in]	Epoch number, starting from 1
{

	// Perform CS analysis on previously marked outliers.
	for (auto& [obsKey, prefitCleanTS] : prefitCleanTSMap)
	{
		// If this channel has just reached the end of its outlier deweighting period, check it for a potential cycle slip
		// Also check that there are sufficient data points to perform cycle-slip classification
		auto index = deweightLog.find(obsKey);
		if (index != deweightLog.end() && index->second == 0 && prefitCleanTS.size() >= slipClassifyMinPts)
		{
			bool slipFound = false;
			vector<double> tsData = subVecLastN(prefitCleanTS, slipClassifyMinPts);

			// Check if pre-split and post-split means are statistically different using Student's t-test
			// Split the dataset into 2 vectors, one excluding the last outlierPtsFromEnd pts, the second including the last outlierPtsFromEnd pts
			int outlierPtsFromEnd = slipClassifyPostOutlierPts;
			int eventEpoch = epochNum - outlierPtsFromEnd + 1;
			vector<double> samplePre = subVecExclLastN(tsData, outlierPtsFromEnd);
			vector<double> samplePost = subVecLastN(tsData, outlierPtsFromEnd);

			// Perform Student's t-test on vectors - is there a statistically significant difference in their means?
			bool stepFound = performStudentsTTest(samplePre, samplePost, tAlpha);
			bool isStepLargeEnough = false;
			if (stepFound)
			{
				double stepEst = calcMean(samplePost) - calcMean(samplePre);
				double stepEstStdDev = sqrt(pow(calcStdDev(samplePre), 2) / samplePre.size() + pow(calcStdDev(samplePost), 2) / samplePost.size()); //Ref: http://onlinestatbook.com/2/sampling_distributions/samplingdist_diff_means.html

				// Verify stepEst is large enough to be a cycle slip (e.g. >= 19cm for L1)
				isStepLargeEnough = rightTailedTest(wavelength, fabs(stepEst), stepEstStdDev, minSizePAlpha); // If false, a step has been detected but it is v. unlikely that it is >= the minimum cycle slip size - ignore these.
				if (isStepLargeEnough)
				{
					// Calculate and validate integer cycle slip solution
					double factor = stepEst / wavelength;
					double factorStdDev = stepEstStdDev / wavelength;
					int intSol = round(factor);
					bool isRoundingValid = validateIntSolution(factor, factorStdDev, intSol);
					slipFound = true;
					if (isRoundingValid)
					{
						// Mark as cycle slip to repair
						slipIntSols[obsKey] = intSol;
					}
					else
					{
						// Large step change detected but unable to find valid int solution - reinitialise ambiguity
						ambToReinit.push_back(obsKey);
					}

					if (printActivity)
					{
						cout << "Previous event classified as cycle slip -";
						cout << " Rec: " << obsKey.str;
						cout << " Sat: " << (string)obsKey.Sat;
						cout << " Slip epoch: " << eventEpoch;
						cout << " Factor: " << factor;
						cout << " Int soln: " << intSol;
						cout << " Rounding valid: " << isRoundingValid;
						cout << endl;
					}
				}
			}
			//if (printActivity && !slipFound)
			//{
			//	cout << "Previous event classified as outlier -";
			//	cout << " Rec: " << obsKey.str;
			//	cout << " Sat: " << (string)obsKey.Sat;
			//	cout << " Outlier epoch: " << eventEpoch;
			//	cout << " Step found: " << stepFound;
			//	cout << " Step large enough: " << isStepLargeEnough;
			//	cout << endl;
			//}
		}
	}
}


/** Applies external integer solution validation to solutions. Paired with validateSingleSols()
*/
void	CSAnalyserSingle::applyExternalSlipValidation(
	const ObsBoolMap&		validMap)	///< External integer solution validation, produced by validateSingleSols()
{
	for (auto& [obsKey, isValid] : validMap)
	{
		if (!isValid)
		{
			slipIntSols.erase(obsKey); // Side-note: map.erase(key) does nothing if map.at(key) does not exist
			ambToReinit.push_back(obsKey); // Invalid CS solutions get ambiguities reinitialised instead of being repaired
		}
	}
}


/** Gets list of keys from slipIntSols map
*/
list<ObsKey>	CSAnalyserSingle::getSlipSolObsKeys()
{
	list<ObsKey> slipObsKeys;
	for (auto& [obsKey, val] : slipIntSols)	slipObsKeys.push_back(obsKey);
	return slipObsKeys;
}


/** Calculates ambiguity correction from slipIntSols map
*/
double	CSAnalyserSingle::getAmbCorrection(
	const ObsKey&			obsKey)
{
	return -slipIntSols[obsKey] * wavelength;
}


/** Update deweightings + clear logs to be ready for the next epoch
*/
void	CSAnalyserSingle::incrementTime()
{
	// Decrement deweight duration of all entries in deweightLog, remove entries with zero duration remaining
	for (auto it = deweightLog.begin(); it != deweightLog.end(); )
	{
		auto obsKey = it->first;
		auto& duration = it->second;

		if (duration <= 0)
		{
			it = deweightLog.erase(it);
		}
		else
		{
			--duration;
			++it;
		}
	}

	for (auto& [obsKey, slipInt] : slipIntSols)
	{
		assert(accumSlips.find(obsKey) != accumSlips.end());
		accumSlips.at(obsKey) += slipInt;
	}
	slipIntSols.clear();
	ambToReinit.clear();
}



// CSAnalyserCombo functions

/** Set internal config values
*/
void	CSAnalyserCombo::init()
{
	assert(isInit == false);
	isInit = true;
	if (freq1 == 0)
	{
		std::cout << "Error - CSAnalyserCombo.freq1 == 0. Set to a non-zero value before calling init()!" << std::endl;
		assert(freq1 != 0);
	}
	if (freq2 == 0)
	{
		std::cout << "Error - CSAnalyserCombo.freq2 == 0. Set to a non-zero value before calling init()!" << std::endl;
		assert(freq2 != 0);
	}
	wavelength1 = 1 / freq1 * CLIGHT;
	wavelength2 = 1 / freq2 * CLIGHT;

	// Reset debug files
	if (debug)	resetDebug();
};


/** Validate single frequency (e.g. L1 & L2) integer solutions by comparing against the combination (e.g. Lc) to see if a corresponding jump can be found
*/
ObsBoolMap	CSAnalyserCombo::validateSingleSols(
	ObsIntMap&				slipIntSols1In,
	ObsIntMap&				slipIntSols2In)
{
	// Get list of candidate solution ObsKeys
	list<ObsKey> solObsKeys = getUniqueKeys(slipIntSols1In, slipIntSols2In);

	// Get expected Lc jumps for each solution
	ObsValMap expectedLcJumps;
	for (auto& obsKey : solObsKeys)
	{
		expectedLcJumps[obsKey] = getComboVal(slipIntSols1In[obsKey] * wavelength1, freq1, slipIntSols2In[obsKey] * wavelength2, freq2); // Side-note: map[key] creates an entry with value = 0 if entry does not exist
	}

	// Validate L1/L2 candidate solutions against Lc - if
	ObsBoolMap lcValidated;
	for (auto& [obsKey, expectedLcJump] : expectedLcJumps)
	{
		// Calc jump in Lc
		vector<double> tsData = subVecLastN(prefitCleanTSMap.at(obsKey), slipClassifyMinPts);

		// Split the dataset into 2 vectors, one excluding the last slipClassifyPostOutlierPts pts, the second including the last slipClassifyPostOutlierPts pts
		vector<double> samplePre = subVecExclLastN(tsData, slipClassifyPostOutlierPts);
		vector<double> samplePost = subVecLastN(tsData, slipClassifyPostOutlierPts);
		double actualLcJump = calcMean(samplePost) - calcMean(samplePre);
		double stdDev = sqrt(pow(calcStdDev(samplePre), 2) / samplePre.size() + pow(calcStdDev(samplePost), 2) / samplePost.size()); //Ref: http://onlinestatbook.com/2/sampling_distributions/samplingdist_diff_means.html


		// Compare this jump to expected jump
		bool isOutlier = twoSidedHypothesisTest(expectedLcJump, actualLcJump, stdDev, intValidComboJumpAlpha);
		lcValidated[obsKey] = !isOutlier; // If not an outlier, Lc validates L1/L2 int solutions

		if (printActivity)
		{
			cout << "Lc validation results -";
			cout << " Rec: " << obsKey.str;
			cout << " Sat: " << (string)obsKey.Sat;
			cout << " Lc validated: " << lcValidated.at(obsKey);
			cout << endl;
		}
	}

	return lcValidated;
}


/** Combine single freq (e.g. L1/L2) integer solutions, ambiguities to reinitialise & channels to deweight into the combination (e.g. Lc) equivalents
*/
void	CSAnalyserCombo::combineSingleSols(
	const ObsIntMap&		slipIntSols1In,
	const ObsIntMap&		slipIntSols2In,
	const list<ObsKey>&		ambToReinit1,
	const list<ObsKey>&		ambToReinit2,
	ObsIntMap				deweightLog1,
	ObsIntMap				deweightLog2)
{
	slipIntSols1 = slipIntSols1In;
	slipIntSols2 = slipIntSols2In;
	ambToReinit = ambToReinit1;
	ambToReinit.insert(ambToReinit.end(), ambToReinit2.begin(), ambToReinit2.end());
	ambToReinit.sort();
	ambToReinit.unique();
	list<ObsKey> deweightLogObsKeys = getUniqueKeys(deweightLog1, deweightLog2);
	for (auto& obsKey : deweightLogObsKeys)
	{
		deweightLog[obsKey] = std::max(deweightLog1[obsKey], deweightLog2[obsKey]); //Side-note: map[key] defaults to 0 if entry doesn't exist
	}
}


/** Gets list of keys from slipIntSols1 & slipIntSols2 maps
*/
list<ObsKey>	CSAnalyserCombo::getSlipSolObsKeys()
{
	return getUniqueKeys(slipIntSols1, slipIntSols2);
}


/** Calculates ambiguity correction from slipIntSols1 & slipIntSols2 maps
*/
double	CSAnalyserCombo::getAmbCorrection(
	const ObsKey&			obsKey)
{
	return -getComboVal(slipIntSols1[obsKey] * wavelength1, freq1, slipIntSols2[obsKey] * wavelength2, freq2); // Side-note: map[key] creates an entry with value = 0 if entry does not exist
}


/** Apply ambiguity adjustment / reinitialisation & channel deweighting to the Kalman filter & measurement weightings
* Returns true if state vector is adjusted - if so, KFState.stateTransition() needs to be rerun to apply changes made to state vector
*/
bool	CSAnalyserCombo::applyAdjustmentsToKF(
	KFState&				kfState,			///< [in/out]	Kalman filter object containing the network state parameters
	KFMeasEntryList&		kfMeasEntryList,	///< [in/out]	List of input measurements as lists of entries
	map<KFKey, bool>&		measuredStates)		///< [in/out]	Map of measured states in this epoch to compare against. Used to reinitialise ambiguities using removeUnmeasuredAmbiguities() (called separately after calling this function).
{
	bool stateModified = false;
	for (auto it = kfMeasEntryList.begin(); it != kfMeasEntryList.end();  )
	{
		KFMeasEntry& kfMeasEntry = (*it);
		bool toDelete = false;

		// Deweight outliers
		auto index = deweightLog.find(kfMeasEntry.obsKey);
		if (index != deweightLog.end())
		{
			int epochsRemainingToDeweight = index->second;
			if (epochsRemainingToDeweight > 0)
			{
				kfMeasEntry.noise = deweightNoise;
			}
		}

		
		// Reinitialise ambiguities (remove amb this epoch, add amb back in next epoch)
		if (std::find(ambToReinit.begin(), ambToReinit.end(), kfMeasEntry.obsKey) != ambToReinit.end())
		{
			KFKey ambiguityKey = { KF::AMBIGUITY, kfMeasEntry.obsKey.Sat,	kfMeasEntry.obsKey.str, 0, nullptr };
			assert(measuredStates.find(ambiguityKey) != measuredStates.end());
			measuredStates.at(ambiguityKey) = false;
			stateModified = true;

			// Remove corresponding phase measurement from kfMeasList
			toDelete = true;

			if (printActivity)
			{
				cout << "Reinitialising ambiguity -";
				cout << " Rec: " << kfMeasEntry.obsKey.str;
				cout << " Sat: " << (string)kfMeasEntry.obsKey.Sat;
				cout << endl;
			}
		}

		if (toDelete)
		{
			it = kfMeasEntryList.erase(it);
		}
		else
		{
			++it;
		}
	}
	removeUnmeasuredAmbiguities(cout, kfState, measuredStates);

	// Apply cycle slip solution to ambiguity via state transition matrix
	list<ObsKey> slipObsKeysToRepair = getSlipSolObsKeys();
	for (auto& obsKey : slipObsKeysToRepair)
	{
		double ambCorrection = getAmbCorrection(obsKey);
		KFKey ambiguityKey = {KF::AMBIGUITY, obsKey.Sat,	obsKey.str, 0, nullptr};
		kfState.setKFTrans(ambiguityKey, {KF::ONE}, ambCorrection);
		stateModified = true;
		if (printActivity)
		{
			cout << "Adjusting ambiguity -";
			cout << " Rec: " << obsKey.str;
			cout << " Sat: " << (string)obsKey.Sat;
			cout << " Adjustment: " << ambCorrection;
			cout << endl;
		}
	}

	return stateModified;
}


/** Prepare internal data for the following epoch
*/
void	CSAnalyserCombo::incrementTime()
{
	slipIntSols1.clear();
	slipIntSols2.clear();
	ambToReinit.clear();
	deweightLog.clear(); // seeded by L1/L2 deweightLog every epoch in combineSingleSols()
}



// Statistical functions

/** Calculates mean of sample
*/
double	calcMean(
	const vector<double>&	sample)	///< Sample to take mean of
{
	if (sample.empty())
	{
		cout << "Warning in calcMean(): sample.empty()!" << endl;
		return 0;
	}
	double accum = 0;
	for (auto val : sample)
	{
		accum += val;
	}
	return accum / sample.size();
}


/** Calculates the median for the given vector
*/
double	calcMed(
	vector<double>			vec)
{
	double med = 0;
	if (vec.empty())
	{
		std::cout << "Warning: calcMed() given vector of size 0!" << std::endl;
		med = 0;
	}
	else {
		sort(vec.begin(), vec.end());

		int size = vec.size();
		if (size % 2 == 0)
		{
			med = (vec.at(size / 2) + vec.at(size / 2 - 1)) / 2.0;
		}
		else {
			med = vec.at((size - 1) / 2);
		}
	}
	return med;
}


/** Calculates the interquartile mean for the given vector
*  https://en.wikipedia.org/wiki/Interquartile_mean
*/
double	calcIqm(
	vector<double>		vec)
{
	double iqm = 0;
	if (vec.empty())
	{
		std::cout << "Warning: calcIqm() given vector of size 0!" << std::endl;
		iqm = 0;
	}
	else if (vec.size() == 1)
	{
		iqm = vec.front(); // note - method below is not suitable for samples of size 1
	}
	else {
		sort(vec.begin(), vec.end());
		double quartileSize = vec.size() / 4.0;
		int cutSize = floor(quartileSize);
		double edgeWeighting = 1 - (quartileSize - cutSize);  // See https://en.wikipedia.org/wiki/Interquartile_mean#Dataset_size_not_divisible_by_four

		double accum = 0;
		for (int i = cutSize; i < vec.size() - cutSize; ++i)
		{
			double weight = 1;
			if (i == cutSize || i == vec.size() - cutSize - 1)
			{
				weight = edgeWeighting;
			}
			accum += weight * vec.at(i);
		}
		iqm = accum / (quartileSize * 2);
	}
	return iqm;
}


/** Calculates standard deviation of sample
*/
double	calcStdDev(
	const vector<double>&	sample)	///< Sample to take std dev of
{
	double mean = calcMean(sample);
	if (sample.size() < 2)
	{
		cout << "Warning in calcStdDev(): vector of size " << sample.size() << " received!" << endl;
		return -1;
	}
	double var = 0;
	for (auto val : sample)
	{
		var += pow(val - mean, 2);
	}
	var /= sample.size() - 1;
	return sqrt(var);
}


/** Calculate left-tailed p-value of z using a normal distribution N(mean,stdDev)
*/
double	calcLPVal(
	double					z,
	double					mean,
	double					stdDev)
{
	boost::math::normal dist(mean, stdDev);
	return cdf(dist, z);
}


/** Calculate right-tailed p-value of z using a normal distribution N(mean,stdDev)
*/
double	calcRPVal(
	double					z,
	double					mean,
	double					stdDev)
{
	boost::math::normal dist(mean, stdDev);
	return 1 - cdf(dist, z);
}


/** Determines if it is plausible for z to be greater than the mean, using the right-tailed hypothesis test
* Returns true if z is likely to be greater than the mean; else false.
*/
bool	rightTailedTest(
	double					z,
	double					mean,
	double					stdDev,
	double					alpha)
{
	double p = calcRPVal(z, mean, stdDev);
	bool isGreaterThanMean = (p >= alpha);
	return isGreaterThanMean;
}


/** Perform two-tailed hypothesis test of z using a normal distribution N(mean,stdDev)
* H0: z == mean
* Ha: z is significantly != mean
* Returns true if there is enough support for Ha, i.e. if z is significantly != mean
*/
bool	twoSidedHypothesisTest(
	double					z,
	double					mean,
	double					stdDev,
	double					alpha)
{
	double p = calcLPVal(z, mean, stdDev);
	bool isHaSupported = (p < alpha / 2 || p > 1 - alpha / 2);
	return isHaSupported;
}


/** Determines if current value is an outlier compared to baseline values, using a normal p-value test
*/
bool	calcIfOutlier(
	double					curr,
	const vector<double>&	baseline,
	double					alpha)
{
	double mean = calcMean(baseline);
	double stdDev = calcStdDev(baseline);
	bool isOutlier = twoSidedHypothesisTest(curr, mean, stdDev, alpha);
	return isOutlier;
}


/**
* Perform Student's t-test (equal variances).
* H0 - the two samples have the same mean, any difference is due to chance.
* Ha - the two samples have different means.
* Returns true if Ha is supported (i.e. means are different); false if H0 is not rejected (i.e. means are equal)
* Ref: http://www.itl.nist.gov/div898/handbook/eda/section3/eda353.htm
*/
bool	performStudentsTTest(
	const vector<double>&	sample1,	///< Sample 1
	const vector<double>&	sample2,	///< Sample 2
	double					alpha)			///< Alpha to use in Student's t-test
{
	double mean1 = calcMean(sample1);
	double mean2 = calcMean(sample2);
	double std1 = calcStdDev(sample1);
	double std2 = calcStdDev(sample2);
	int n1 = sample1.size();
	int n2 = sample2.size();
	double df = n1 + n2 - 2; // Degrees of freedom
	double stdPooled = sqrt(((n1 - 1) * std1 * std1 + (n2 - 1) * std2 * std2) / df); // Pooled std dev
	double tStat = (mean1 - mean2) / (stdPooled * sqrt(1.0 / n1 + 1.0 / n2)); // t-statistic
	students_t dist(df);
	double q = cdf(complement(dist, fabs(tStat)));
	bool notEqual = (q < alpha / 2); // true if support is found for Ha
	return notEqual;
}


/** Given some measurement of unknown value with given mean and stdDev, calculate how likely the true value == candidate, vs. all other alternative values
*/
double	calcCandidateSuccessVsAlternatives(
	double					mean,
	double					stdDev,
	double					candidate,
	const vector<double>& alternatives)
{
	normal s(mean, stdDev);
	double candidatePd = pdf(s, candidate);
	double nonCandidatePdSum = 0;
	for (auto val : alternatives)
	{
		nonCandidatePdSum += pdf(s, val);
	}
	return candidatePd / (candidatePd + nonCandidatePdSum);
}


/** Validate int solution by considering the factor (unrounded solution) and its stdDev
*/
bool	CSAnalyserSingle::validateIntSolution(
	double					factor,
	double					stdDev,
	int						intEst)
{
	// Put together list of alternative integer solutions (i.e. all integers != intEst, within a reasonable range)
	int halfTestRange = ceil(stdDev * 5); //pdf 5x sigmas away from mean = 1.5E-06 - further out than this is unnecessary
	int testMin = intEst - halfTestRange;
	int testMax = intEst + halfTestRange;
	vector<double> alternatives;
	for (int i = testMin; i < testMax; ++i)
	{
		if (i != intEst)
		{
			alternatives.push_back(i);
		}
	}

	// Determine if candidate integer is significantly more likely to be the correct integer solution than all other integers
	double successPc = calcCandidateSuccessVsAlternatives(factor, stdDev, intEst, alternatives);
	bool isOutlier = twoSidedHypothesisTest(intEst, factor, stdDev, intValidOutlierAlpha);
	//if (printActivity)
	//{
	//	cout << "Rounding verification -";
	//	cout << " factor: " << factor;
	//	cout << " stdDev: " << stdDev;
	//	cout << " successPc: " << successPc;
	//	cout << " (successPc > intValidPdfThresh): " << (successPc > intValidPdfThresh);
	//	cout << " !isOutlier: " << !isOutlier;
	//	cout << endl;
	//}
	bool isRoundingValid = (successPc > intValidPdfThresh) && !isOutlier;	
	return isRoundingValid;
}



// GNSS-related functions

/** Calculate the ionosphere-free combination Lc.
* Ref: https://gssc.esa.int/navipedia/index.php/Combination_of_GNSS_Measurements
*/
double	calcLc(
	double					val1,	///< L1 range value (m)
	double					freq1,	///< L1 freq (Hz)
	double					val2,	///< L2 range value (m)
	double					freq2)	///< L2 freq (Hz)
{
	double freqSqL1 = pow(freq1, 2);
	double freqSqL2 = pow(freq2, 2);
	return (freqSqL1 * val1 - freqSqL2 * val2) / (freqSqL1 - freqSqL2);
}


/** Derivative of ObsKey::operator ==(), except it compares satellite strings to compare satellites.
* Used in insertArtificialSlip() for debugging only.
*/
bool	obsKeysAreEqual (const ObsKey& obsKey1, const ObsKey& obsKey2)
{
	if (obsKey1.str.compare(obsKey2.str)	!= 0)					return false;
	if ((string)obsKey1.Sat					!= (string)obsKey2.Sat)	return false;
	if (obsKey1.type.compare(obsKey2.type)	!= 0)					return false;
	else															return true;
}


/** Insert an artificial cycle slip at a given epoch in a given channel
*/
void	insertArtificialSlip(
	const ObsKey&			obsKeyToInsertSlip,
	double					slipSize,
	int						epochToInsertSlip,
	KFMeasEntryList&		kfMeasEntryList,
	int						currEpoch)
{
	if (currEpoch >= epochToInsertSlip)
	{
		bool found = false;
		for (auto& meas : kfMeasEntryList)
		{
			bool obsKeysEqual = obsKeysAreEqual(meas.obsKey, obsKeyToInsertSlip);
			if (obsKeysEqual)
			{
				meas.value += slipSize;
				found = true;
			}
		}
		if (!found) cout << "Warning in insertArtificialSlip(): ObsKey " << obsKeyToInsertSlip.str << "-" << (string)obsKeyToInsertSlip.Sat << " not found at epoch " << currEpoch << endl;
	}
}


/** Returns the interquartile mean of all channels for each station.
*/
ObsValMap	calcStationCommonModes(
	const ObsValMap&		chVals,				///< Values for each channel
	string					commonModeMethod)	///< Method for calculating common mode
{
	// Gather common-rec values together
	ObsVecMap recChannels;
	for (auto& [obsKey, val] : chVals)
	{
		ObsKey obsKeyRecOnly = obsKey;
		obsKeyRecOnly.Sat = {};
		auto ind = recChannels.find(obsKeyRecOnly);
		if (ind != recChannels.end())
		{
			ind->second.push_back(val);
		}
		else {
			vector<double> newEntry;
			newEntry.push_back(val);
			recChannels[obsKeyRecOnly] = newEntry;
		}
	}

	// Calc common mode for each station
	ObsValMap commonModes;
	for (auto& [obsKey, vec] : recChannels)
	{
		assert(vec.size() != 0);
		if 		(commonModeMethod == "iqm")		commonModes[obsKey] = calcIqm(vec);
		else if	(commonModeMethod == "median")	commonModes[obsKey] = calcMed(vec);
		else if	(commonModeMethod == "mean")	commonModes[obsKey] = calcMean(vec);
		else 									cout << "Error: invalid method under cycle_slip_filter_parameters.common_mode_method in YAML file!" << endl;
	}
	return commonModes;
}



// Other helper functions

/** Convert a KFMeasEntryList values to an ObsValMap.
* KFMeasEntryList.noise/innov's are discarded.
*/
ObsValMap	kfMeasEntryListToObsValMap(
	const KFMeasEntryList&	kfMeasEntryList)
{
	ObsValMap obsValMap;
	for (auto& entry : kfMeasEntryList)
	{
		obsValMap[entry.obsKey] = entry.value;
	}
	return obsValMap;
}


/** Preps a ObsVecMap with empty vectors for later use. Also clears vectors not in use.
*/
void	initClearTimeseries(
	const ObsValMap&		currEntries,	///< Data from the current epoch
	ObsVecMap&				timeseriesMap)		///< Timeseries of previous epochs' data
{
	// Initialise timeseries (if missing) for each entry in the current epoch
	for (const auto& [obsKey, meas] : currEntries)
	{
		if (timeseriesMap.find(obsKey) == timeseriesMap.end())
		{
			vector<double> newVec;
			timeseriesMap[obsKey] = newVec;
		}
	}

	// Clear timeseries that don't have entries in the current epoch
	for (auto& [obsKey, vec] : timeseriesMap)
	{
		if (currEntries.find(obsKey) == currEntries.end())
		{
			vec.clear(); // note - can delete vector entirely, but easier to just clear vector
		}
	}
}


/** Preps a ObsIntMap with zeros for later use. Also zeroes entries not in use.
*/
void	initZeroObsIntMap(
	const ObsValMap&		currEntries,	///< Data from the current epoch
	ObsIntMap&				obsIntMap)			///< ObsIntMap to initialise/zero
{
	// Initialise + zero ObsIntMap entries (if missing) for each entry in the current epoch
	for (const auto& [obsKey, meas] : currEntries)
	{
		if (obsIntMap.find(obsKey) == obsIntMap.end())
		{
			obsIntMap[obsKey] = 0;
		}
	}

	// Zero ObsIntMap entries that don't have corresponding entries in the current epoch
	for (auto& [obsKey, vec] : obsIntMap)
	{
		if (currEntries.find(obsKey) == currEntries.end())
		{
			obsIntMap.at(obsKey) = 0;
		}
	}
}


/** Returns subvector made up of the last n points.
* Returns empty vector if n <= 0.
* Returns original vector if n >= original vector size.
*/
vector<double>	subVecLastN(
	vector<double>			vec,
	int						n)
{
	if (n < 0)
	{
		cout << "Warning: n < 0 in subVecLastN()" << endl;
		return vector<double>();
	}
	else if (n > vec.size())
	{
		cout << "Warning: n > vec.size() in subVecLastN()" << endl;
		return vec;
	}
	auto first = vec.begin() + vec.size() - n;
	auto last = vec.end();
	return vector<double>(first, last);
}


/** Returns subvector that excludes the last n points
* Returns original vector if n <= 0.
* Returns empty vector if n >= original vector size.
*/
vector<double>	subVecExclLastN(
	vector<double>			vec,
	int						n)
{
	if (n < 0)
	{
		cout << "Warning: n < 0 in subVecExclLastN()" << endl;
		return vec;

	}
	else if (n > vec.size())
	{
		cout << "Warning: n > vec.size() in subVecExclLastN()" << endl;
		return vector<double>();
	}

	auto first = vec.begin();
	auto last = vec.end() - n;
	return vector<double>(first, last);
}


/** Prints an ObsValMap to file
*/
void	printToFile(
	const ObsValMap&		obsValMap,	///< ObsValMap to print
	string					filename,		///< File to print to
	int						epochNum)			///< Epoch number
{
	std::ofstream out;
	out.open(filename, std::ios::app);
	for (auto& [obsKey, val] : obsValMap)
	{
		out << epochNum << " " << obsKey << " " << val << endl;
	}
	out.close();
}


/** Get unique ObsKey's from two given maps
*/
list<ObsKey>	getUniqueKeys(
	const ObsIntMap&		obsIntMap1,
	const ObsIntMap&		obsIntMap2)
{
	list<ObsKey> obsKeys;
	for (auto& [obsKey, val] : obsIntMap1)	obsKeys.push_back(obsKey);
	for (auto& [obsKey, val] : obsIntMap2)	obsKeys.push_back(obsKey);
	obsKeys.sort();
	obsKeys.unique();
	return obsKeys;
}


void networkEstimatorCSStuff()
{
#if 0
	// Cycle slip detection + repair
	bool anotherStateTransitionCalcRequired = true;
	if (acsConfig.csOpts.enable)
	{
		auto timer = Clock::now();

		// Replicate flow of networkEstimator() with L1/L2/Lc phase measurements, up to the point where prefit residuals are calculated
		KFMeasEntryList kfMeasEntryListCopy = kfMeasEntryList;
		KFState kfStatePrior = kfState;
		//add process noise to existing states as per their initialisations.
		kfStatePrior.stateTransition(trace, tgap);
		if (acsConfig.csOpts.timer_debug)
		{
			if (sEpochNum == 1)
			{
				std::ofstream out("csTimer.dbg", std::ios::out);	
				out << "Start" << std::endl;
			}
			
			std::ofstream out("csTimer.dbg", std::ios::app);
			out << "Epoch: " << sEpochNum << " Section: stateTransition Runtime (s): " << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - timer).count() / 1000.0 << std::endl;
			timer = Clock::now();
		}
		//combine the measurement list into a single matrix
		KFMeas combinedMeasCopy = kfStatePrior.combineKFMeasList(kfMeasEntryListCopy);
		KFMeas combinedMeasL1 	= kfStatePrior.combineKFMeasList(kfMeasEntryListL1);
		KFMeas combinedMeasL2 	= kfStatePrior.combineKFMeasList(kfMeasEntryListL2);
		KFMeas combinedMeasLc 	= kfStatePrior.combineKFMeasList(kfMeasEntryListLc);
		auto time = stations.front()->obsList.front().time;
		combinedMeasCopy.time 	= time;
		combinedMeasL1.time 	= time;
		combinedMeasL2.time 	= time;
		combinedMeasLc.time 	= time;
		correctRecClocks(trace, kfStatePrior, refRec);
		
		//if there are uninitialised state values, estimate them using least squares
		if (acsConfig.csOpts.timer_debug)
		{
			std::ofstream out;
			out.open("csTimer.dbg", std::ios::app);
			out << "Epoch: " << sEpochNum << " Section: combineKFMeasList Runtime (s): " << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - timer).count() / 1000.0 << std::endl;
			timer = Clock::now();
			out.close();
		}
		
		KFState kfStatePriorLssq = kfStatePrior;
		if (kfStatePriorLssq.lsqRequired)
		{
			kfStatePriorLssq.lsqRequired = false;
			kfStatePriorLssq.leastSquareInitStates(trace, combinedMeasCopy);
		}
		
		if (acsConfig.csOpts.timer_debug)
		{
			std::ofstream out("csTimer.dbg", std::ios::app);
			out << "Epoch: " << sEpochNum << " Section: lssq Runtime (s): " << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - timer).count() / 1000.0 << std::endl;
			timer = Clock::now();
		}

		// Calculate prefit residuals & store in a KFMeasEntryList (where .value = prefit residual value)
		KFMeasEntryList prefitL1 = kfStatePriorLssq.calcPrefitResids(trace, combinedMeasL1);
		KFMeasEntryList prefitL2 = kfStatePriorLssq.calcPrefitResids(trace, combinedMeasL2);
		KFMeasEntryList prefitLc = kfStatePriorLssq.calcPrefitResids(trace, combinedMeasLc);
		
		if (acsConfig.csOpts.timer_debug)
		{
			std::ofstream out("csTimer.dbg", std::ios::app);
			out << "Epoch: " << sEpochNum << " Section: calcPrefitResids Runtime (s): " << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - timer).count() / 1000.0 << std::endl;
			timer = Clock::now();
		}
		
		// Insert artificial cycle slips (for debugging/testing)
		bool insertSlip = false;
		if (insertSlip)
		{
			std::string sat = "G01";
			std::string rec = "DAV1";
			int slipL1 = 1; // Number of cycles to slip in L1
			int slipL2 = 1;
			int epoch = 30;

			double wavelengthL1 = 1 / acsConfig.csOpts.freq_l1 * CLIGHT;
			double wavelengthL2 = 1 / acsConfig.csOpts.freq_l2 * CLIGHT;
			double jumpL1 = slipL1 * wavelengthL1;
			double jumpL2 = slipL2 * wavelengthL2;
			double jumpLc = calcLc(jumpL1, acsConfig.csOpts.freq_l1, jumpL2, acsConfig.csOpts.freq_l2);
			ObsKey obsKeyToInsertSlipL1 = { sat.c_str(), rec, "L" };
			ObsKey obsKeyToInsertSlipL2 = { sat.c_str(), rec, "L" };
			ObsKey obsKeyToInsertSlipLc = { sat.c_str(), rec, "L" };
			insertArtificialSlip(obsKeyToInsertSlipL1, jumpL1,	epoch, prefitL1, sEpochNum);
			insertArtificialSlip(obsKeyToInsertSlipL2, jumpL2,	epoch, prefitL2, sEpochNum);
			insertArtificialSlip(obsKeyToInsertSlipLc, jumpLc,	epoch, prefitLc, sEpochNum);
		}
		
		if (insertSlip)
		{
			std::string sat = "G10";
			std::string rec = "QUIN";
			int slipL1 = 1; // Number of cycles to slip in L1
			int slipL2 = 1;
			int epoch = 40;

			double wavelengthL1 = 1 / acsConfig.csOpts.freq_l1 * CLIGHT;
			double wavelengthL2 = 1 / acsConfig.csOpts.freq_l2 * CLIGHT;
			double jumpL1 = slipL1 * wavelengthL1;
			double jumpL2 = slipL2 * wavelengthL2;
			double jumpLc = calcLc(jumpL1, acsConfig.csOpts.freq_l1, jumpL2, acsConfig.csOpts.freq_l2);
			ObsKey obsKeyToInsertSlipL1 = { sat.c_str(), rec, "L" };
			ObsKey obsKeyToInsertSlipL2 = { sat.c_str(), rec, "L" };
			ObsKey obsKeyToInsertSlipLc = { sat.c_str(), rec, "L" };
			insertArtificialSlip(obsKeyToInsertSlipL1, jumpL1,	epoch, prefitL1, sEpochNum);
			insertArtificialSlip(obsKeyToInsertSlipL2, jumpL2,	epoch, prefitL2, sEpochNum);
			insertArtificialSlip(obsKeyToInsertSlipLc, jumpLc,	epoch, prefitLc, sEpochNum);
		}

		// Set internal parameters & reset debug files
		if (sEpochNum == 1)
		{
			csAnalyserL1.freqName					= "L1";
			csAnalyserL1.freq						= acsConfig.csOpts.freq_l1;
			csAnalyserL1.printActivity				= acsConfig.csOpts.print_activity;
			csAnalyserL1.debug						= acsConfig.csOpts.debug;
			csAnalyserL1.newChannelBreakInDuration	= acsConfig.csOpts.new_channel_break_in_duration;
			csAnalyserL1.dMovingAveWin				= acsConfig.csOpts.d_moving_ave_win;
			csAnalyserL1.commonModeMethod			= acsConfig.csOpts.common_mode_method;
			csAnalyserL1.slipClassifyMinPts			= acsConfig.csOpts.slip_classify_min_pts;
			csAnalyserL1.slipClassifyPostOutlierPts	= acsConfig.csOpts.slip_classify_post_outlier_pts;
			csAnalyserL1.outlierPAlpha				= acsConfig.csOpts.outlier_p_alpha;
			csAnalyserL1.tAlpha						= acsConfig.csOpts.t_alpha;
			csAnalyserL1.minSizePAlpha				= acsConfig.csOpts.min_size_p_alpha;
			csAnalyserL1.outlierDetectMinPts		= acsConfig.csOpts.outlier_detect_min_pts;
			csAnalyserL1.intValidPdfThresh			= acsConfig.csOpts.int_valid_pdf_thresh;
			csAnalyserL1.intValidOutlierAlpha		= acsConfig.csOpts.int_valid_outlier_alpha;
			csAnalyserL1.init();

			csAnalyserL2.freqName					= "L2";
			csAnalyserL2.freq						= acsConfig.csOpts.freq_l2;
			csAnalyserL2.printActivity				= acsConfig.csOpts.print_activity;
			csAnalyserL2.debug						= acsConfig.csOpts.debug;
			csAnalyserL2.newChannelBreakInDuration	= acsConfig.csOpts.new_channel_break_in_duration;
			csAnalyserL2.dMovingAveWin				= acsConfig.csOpts.d_moving_ave_win;
			csAnalyserL2.commonModeMethod			= acsConfig.csOpts.common_mode_method;
			csAnalyserL2.slipClassifyMinPts			= acsConfig.csOpts.slip_classify_min_pts;
			csAnalyserL2.slipClassifyPostOutlierPts	= acsConfig.csOpts.slip_classify_post_outlier_pts;
			csAnalyserL2.outlierPAlpha				= acsConfig.csOpts.outlier_p_alpha;
			csAnalyserL2.tAlpha						= acsConfig.csOpts.t_alpha;
			csAnalyserL2.minSizePAlpha				= acsConfig.csOpts.min_size_p_alpha;
			csAnalyserL2.outlierDetectMinPts		= acsConfig.csOpts.outlier_detect_min_pts;
			csAnalyserL2.intValidPdfThresh			= acsConfig.csOpts.int_valid_pdf_thresh;
			csAnalyserL2.intValidOutlierAlpha		= acsConfig.csOpts.int_valid_outlier_alpha;
			csAnalyserL2.init();

			csAnalyserLc.freqName					= "Lc";
			csAnalyserLc.freq1						= acsConfig.csOpts.freq_l1;
			csAnalyserLc.freq2						= acsConfig.csOpts.freq_l2;
			csAnalyserLc.printActivity				= acsConfig.csOpts.print_activity;
			csAnalyserLc.debug						= acsConfig.csOpts.debug;
			csAnalyserLc.newChannelBreakInDuration	= acsConfig.csOpts.new_channel_break_in_duration;
			csAnalyserLc.dMovingAveWin				= acsConfig.csOpts.d_moving_ave_win;
			csAnalyserLc.commonModeMethod			= acsConfig.csOpts.common_mode_method;
			csAnalyserLc.slipClassifyMinPts			= acsConfig.csOpts.slip_classify_min_pts;
			csAnalyserLc.slipClassifyPostOutlierPts	= acsConfig.csOpts.slip_classify_post_outlier_pts;
			csAnalyserLc.deweightNoise				= acsConfig.csOpts.deweight_noise;
			csAnalyserLc.intValidComboJumpAlpha		= acsConfig.csOpts.int_valid_combo_jump_alpha;
			csAnalyserLc.init();
		}

		// Perform cycle slip detection + repair
		anotherStateTransitionCalcRequired = cycleSlipDetectRepair(csAnalyserL1, csAnalyserL2, csAnalyserLc, prefitL1, prefitL2, prefitLc, sEpochNum, kfState, kfMeasEntryList, measuredStates);
		
		// Skip state transition recalculation if no changes to state vector are required
		if (!anotherStateTransitionCalcRequired)
		{
			kfState = kfStatePrior;
		}

		// Debug
		if (acsConfig.csOpts.timer_debug)
		{
			std::ofstream out("csTimer.dbg", std::ios::app);
			// Record slip analysis run-time
			out << "Epoch: " << sEpochNum << " Section: cycleSlipDetectRepair Runtime (s): " << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - timer).count() / 1000.0 << std::endl;
			timer = Clock::now();
		}
		
		if (acsConfig.csOpts.debug)
		{
			// Reset debug files
			if (sEpochNum == 1)
			{
				for (string file : 	{
										"Visibilities.dbg",
										"KFLcAll.dbg",
										"KFL1.dbg",
										"KFL2.dbg",
										"KFLc.dbg",
										"KFL1Prefit.dbg",
										"KFL2Prefit.dbg",
										"KFLcPrefit.dbg"
									})
				{
					std::ofstream out(file, std::ios::out);
					
					out << "Start" << std::endl;
				}
			}

			// Record rec-sat visibilities, OmC's & prefit residuals for L1/L2/Lc
			std::ofstream out("Visibilities.dbg", std::ios::app);
			for (auto& meas : kfMeasEntryListL1)	
				out << sEpochNum << " " << meas.obsKey.str << " " << meas.obsKey.Sat.id() << std::endl;
			
			printToFile(kfMeasEntryList, 	"KFLcAll.dbg",		sEpochNum);
			printToFile(kfMeasEntryListL1, 	"KFL1.dbg",			sEpochNum);
			printToFile(kfMeasEntryListL2, 	"KFL2.dbg",			sEpochNum);
			printToFile(kfMeasEntryListLc, 	"KFLc.dbg",			sEpochNum);
			printToFile(prefitL1, 			"KFL1Prefit.dbg",	sEpochNum);
			printToFile(prefitL2, 			"KFL2Prefit.dbg",	sEpochNum);
			printToFile(prefitLc, 			"KFLcPrefit.dbg",	sEpochNum);
		}
		++sEpochNum;
	}

	//add process noise to existing states as per their initialisations.
	if (anotherStateTransitionCalcRequired)
	{
		kfState.stateTransition(trace, tgap);
	}

// 	auto cycle_slip = stringsToYamlObject({yaml, ""}, {"cycle_slip_filter_parameters"});
// 	{
// 		trySetFromYaml(csOpts.enable,							cycle_slip, {"enable"							});
// 		trySetFromYaml(csOpts.freq_l1,							cycle_slip, {"freq_l1"							});
// 		trySetFromYaml(csOpts.freq_l2,							cycle_slip, {"freq_l2"							});
// 		trySetFromYaml(csOpts.print_activity,					cycle_slip, {"print_activity"					});
// 		trySetFromYaml(csOpts.debug,							cycle_slip, {"debug"							});
// 		trySetFromYaml(csOpts.timer_debug,						cycle_slip, {"timer_debug"						});
// 		trySetFromYaml(csOpts.new_channel_break_in_duration,	cycle_slip, {"new_channel_break_in_duration"	});
// 		trySetFromYaml(csOpts.d_moving_ave_win,					cycle_slip, {"d_moving_ave_win"					});
// 		trySetFromYaml(csOpts.common_mode_method,				cycle_slip, {"common_mode_method"				});
// 		trySetFromYaml(csOpts.slip_classify_min_pts,			cycle_slip, {"slip_classify_min_pts"			});
// 		trySetFromYaml(csOpts.slip_classify_post_outlier_pts,	cycle_slip, {"slip_classify_post_outlier_pts"	});
// 		trySetFromYaml(csOpts.outlier_p_alpha,					cycle_slip, {"outlier_p_alpha"					});
// 		trySetFromYaml(csOpts.t_alpha,							cycle_slip, {"t_alpha"							});
// 		trySetFromYaml(csOpts.min_size_p_alpha,					cycle_slip, {"min_size_p_alpha"					});
// 		trySetFromYaml(csOpts.outlier_detect_min_pts,			cycle_slip, {"outlier_detect_min_pts"			});
// 		trySetFromYaml(csOpts.int_valid_pdf_thresh,				cycle_slip, {"int_valid_pdf_thresh"				});
// 		trySetFromYaml(csOpts.int_valid_outlier_alpha,			cycle_slip, {"int_valid_outlier_alpha"			});
// 		trySetFromYaml(csOpts.deweight_noise,					cycle_slip, {"deweight_noise"					});
// 		trySetFromYaml(csOpts.int_valid_combo_jump_alpha,		cycle_slip, {"int_valid_combo_jump_alpha"		});
// 	}

/** Options associated with cycle slip detection and repair within the network filter
*/
struct CycleSlipOptions
{
	bool	enable							= false;
	bool	print_activity					= false;
	bool	debug							= false;
	bool	timer_debug						= false;
	double	freq_l1							= 0;
	double	freq_l2							= 0;
	int		new_channel_break_in_duration	= 0;
	int		d_moving_ave_win				= 0;
	string	common_mode_method				= "";
	int		slip_classify_min_pts			= 0;
	int		slip_classify_post_outlier_pts	= 0;
	double	outlier_p_alpha					= 0;
	double	t_alpha							= 0;
	double	min_size_p_alpha				= 0;
	int		outlier_detect_min_pts			= 0;
	double	int_valid_pdf_thresh			= 0;
	double	int_valid_outlier_alpha			= 0;
	double	deweight_noise					= 0;
	double	int_valid_combo_jump_alpha		= 0;
};

#endif
}
