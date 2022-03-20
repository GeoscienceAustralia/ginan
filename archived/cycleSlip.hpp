#pragma once
#ifndef __CYCLESLIP_HPP__
#define __CYCLESLIP_HPP__

#include <iostream>
#include <fstream>
#include <map>
#include <assert.h>

using std::string;
using std::map;

#include "algebra.hpp"



typedef map<ObsKey, bool>			ObsBoolMap;
typedef map<ObsKey, int>			ObsIntMap;
typedef map<ObsKey, double>			ObsValMap;
typedef map<ObsKey, vector<double>>	ObsVecMap;



/** Abstract class for cycle slip analysis classes (single and combination frequencies).
*
* Contains persistant parameters and timeseries data used for cycle slip detection and repair.
* This class contains operations to clean prefit residual data, used for detecting and repairing cycle slips within the network estimator.
*/
struct CSAnalyserBase
{
	// Config values
	bool			isInit						= false;	///< If internal params have been initialised (using setParams())
	bool			printActivity				= false;	///< Print out analysis to terminal
	bool			debug						= false;	///< Print debug files
	int				newChannelBreakInDuration	= 0;		///< Number of epochs to wait before starting data cleaning
	int				dMovingAveWin				= 0;		///< Differenced prefit moving average window length
	string			commonModeMethod			= "";		///< Method used to calculate common mode. Options: "iqm" (inter-quartile mean), "median" (median), "mean" (mean)
	int				slipClassifyMinPts			= 0;		///< Number of points to perform slip/outlier classification on
	int				slipClassifyPostOutlierPts	= 0;		///< Number of post-outlier epochs to perform slip/outlier classification on (inclusive of the outlier itself). E.g. if outlier detected in epoch 4 and slipClassifyWait										= 3, then classification happens in epoch 6. Channel is deweighted in the interim

	// Internal data
	string 			freqName					= "";		///< Frequency name
	ObsVecMap		prefitTSMap;							///< Prefit time-series
	ObsVecMap		dPrefitLessComTSMap;					///< Differenced prefit minus common mode time-series
	ObsVecMap		prefitCleanTSMap;						///< Prefit time-series (cleaned)
	ObsIntMap		deweightLog;							///< Log of channels to deweight. <Channel ID, # epochs remaining to deweight (inclusive of current epoch)>
	list<ObsKey>	ambToReinit;							///< Ambiguities to reinitialise.

	// Member functions
	void	resetDebug();

	void	cleanData(
		const ObsValMap&		prefit,	
		int						epochNum);

	void	repairInternalTS();

	virtual list<ObsKey>	getSlipSolObsKeys()						= 0;

	virtual double			getAmbCorrection(const ObsKey& obsKey)	= 0;
};



struct CSAnalyserSingle : CSAnalyserBase
{
	// Config values
	double		outlierPAlpha					= 0;		///< Alpha value used in outlier p--test
	double		tAlpha							= 0;		///< Alpha value used in Student's t-test
	double		minSizePAlpha					= 0;		///< Alpha value used to verify (potential) slips are large enough (i.e. >= 1 wavelength)
	int			outlierDetectMinPts				= 0;		///< Number of points to perform outlier analysis on
	double		intValidPdfThresh				= 0;		///< Threshold for integer validation PDF test
	double		intValidOutlierAlpha			= 0;		///< Alpha for integer validation outlier test
	double		freq							= 0;		///< Frequency (Hz)
	double		wavelength						= 0;		///< Size of wavelength (m)

	// Internal data
	ObsIntMap	slipIntSols;								///< Cycle slip integer solutions to apply. <Channel ID, integer cycle slip solution to apply>.
	ObsIntMap	accumSlips;									///< Accumulated slips for each channel (m)

	// Member functions
	void	init();

	void	adjustAmb(
		ObsValMap&			prefit,
		KFState&			kfState,
		int					epochNum);

	void	detectOutliers(
		int					epochNum);

	void	classifySlips(
		int					epochNum);

	bool	validateIntSolution(
		double				floatEst,
		double				stdDev,
		int					intEst);

	void	applyExternalSlipValidation(
		const ObsBoolMap&	validMap);

	list<ObsKey> getSlipSolObsKeys();

	double	getAmbCorrection(
		const ObsKey&		obsKey);

	void	incrementTime();
};



struct CSAnalyserCombo : CSAnalyserBase
{
	// Config values
	double		deweightNoise					= 0;		///< 'Noise' given to deweighted measurements (higher than normal)
	double		intValidComboJumpAlpha			= 0;		///< Alpha for integer validation outlier test using combined data

	// Internal data
	double		freq1							= 0;
	double		freq2							= 0;
	double		wavelength1						= 0;
	double		wavelength2						= 0;
	double		(*getComboVal) (double val1, double freq1, double val2, double freq2);
	ObsIntMap	slipIntSols1;								///< Frequency 1 cycle slip integer solutions to apply. <Channel ID, integer cycle slip solution to apply>.
	ObsIntMap	slipIntSols2;								///< Frequency 2 cycle slip integer solutions to apply. <Channel ID, integer cycle slip solution to apply>.

	CSAnalyserCombo(double (*getComboValIn)(double val1, double freq1, double val2, double freq2)) : getComboVal(getComboValIn)
	{
	};
	~CSAnalyserCombo() {};

	void	init();

	ObsBoolMap	validateSingleSols(
		ObsIntMap&				slipIntSols1,
		ObsIntMap&				slipIntSols2);

	void		combineSingleSols(
		const ObsIntMap& 		slipIntSols1In,
		const ObsIntMap& 		slipIntSols2In,
		const list<ObsKey>&		ambToReinit1,
		const list<ObsKey>&		ambToReinit2,
		ObsIntMap				deweightLog1,
		ObsIntMap				deweightLog2);

	list<ObsKey> getSlipSolObsKeys();
	
	double		getAmbCorrection(
		const ObsKey&			obsKey);

	bool	applyAdjustmentsToKF(
		KFState&				kfState,
		KFMeasEntryList&		kfMeasEntryList,
		map<KFKey, bool>&		measuredStates);

	void		incrementTime();
};



// External functions
bool	cycleSlipDetectRepair(
	CSAnalyserSingle&		csAnalyserL1,
	CSAnalyserSingle&		csAnalyserL2,
	CSAnalyserCombo&		csAnalyserLc,
	const KFMeasEntryList&	kfPrefitL1,	
	const KFMeasEntryList&	kfPrefitL2,	
	const KFMeasEntryList&	kfPrefitLc,	
	int						epochNum,
	KFState&				kfState,
	KFMeasEntryList&		kfMeasEntryList,
	map<KFKey, bool>&		measuredStates);


void	printToFile(
	const KFMeasEntryList&	kfMeasEntryList,	
	string					filename,			
	int						epochNum);



// Statistical functions

double	calcMean(
	const vector<double>&	sample);

double	calcMed(
	vector<double>			vec);

double	calcIqm(
	vector<double>			vec);

double	calcStdDev(
	const vector<double>&	sample);

double	calcLPVal(
	double					z,
	double					mean,
	double					stdDev);

double	calcRPVal(
	double					z,
	double					mean,
	double					stdDev);

bool	rightTailedTest(
	double					z,
	double					mean,
	double					stdDev,
	double					alpha);

bool	twoSidedHypothesisTest(
	double					z,
	double					mean,
	double					stdDev,
	double					alpha);

bool	calcIfOutlier(
	double					curr,
	const vector<double>&	baseline,
	double					alpha);

bool	performStudentsTTest(
	const vector<double>&	sample1,	
	const vector<double>&	sample2,	
	double					alpha);

double	calcCandidateSuccessVsAlternatives(
	double					mean,
	double					stdDev,
	double					candidate,
	const vector<double>& alternatives);

double	calcClosenessToPeak(
	double					mean,
	double					stdDev,
	double					candidate);

bool	validateIntSolution(
	double					floatEst,
	double					stdDev,
	int						intEst);



// GNSS-related functions

double	calcLc(
	double					val1,	
	double					freq1,	
	double					val2,	
	double					freq2);

void	insertArtificialSlip(
	const ObsKey&			obsKeyToInsert,
	double					slipSize,
	int						epochToInsert,
	KFMeasEntryList&		kfMeasEntryList,
	int						currEpoch);

ObsValMap	calcStationCommonModes(
	const ObsValMap&		chVals,
	std::string				commonModeMethod);



// Other helper functions

ObsValMap	kfMeasEntryListToObsValMap(
	const KFMeasEntryList&	kfMeasEntryList);

void	initClearTimeseries(
	const ObsValMap&		currEntries,	
	ObsVecMap&				timeseriesMap);

void	initZeroObsIntMap(
	const ObsValMap&		currEntries,	
	ObsIntMap&				obsIntMap);

vector<double>	subVecLastN(
	vector<double>			vec,
	int						n);

vector<double>	subVecExclLastN(
	vector<double>			vec,
	int						n);

void	printToFile(
	const ObsValMap&		obsValMap,
	string					filename,
	int						epochNum);

list<ObsKey>	getUniqueKeys(
	const ObsIntMap&		obsIntMap1,
	const ObsIntMap&		obsIntMap2);

#endif
