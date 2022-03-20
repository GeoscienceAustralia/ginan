
// #pragma GCC optimize ("O0")

#include <string>
#include <tuple>
#include <list>
#include <map>

using std::string;
using std::tuple;
using std::list;
using std::map;

#include "observations.hpp"
#include "streamTrace.hpp"
#include "corrections.hpp"
#include "forceModels.hpp"
#include "navigation.hpp"
#include "acsStream.hpp"

#include "eigenIncluder.hpp"
#include "forceModels.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "lambda.h"
#include "ppp.hpp"

#include <iostream>
#include <fstream>

KFMeas makeLCs(
	KFMeas&		combinedMeas,
	KFState&	kfState)
{
// 	Instrument instrument(__FUNCTION__);
	
	auto& recOpts = acsConfig.getRecOpts("");
	
	// Replace individual measurements with linear combinations
	if (recOpts.ion.estimate)
	{
		return combinedMeas;
	}
	
	KFMeas newMeas;

	vector<Triplet<double>> tripletList;
	int meas = 0;

	for (int i = 1; i < combinedMeas.obsKeys.size(); i++)
	{
		int j = i - 1;
		auto& obsKeyi = combinedMeas.obsKeys[i];
		auto& obsKeyj = combinedMeas.obsKeys[j];
		if (obsKeyi.num != F2)
		{
			continue;
		}
		if (obsKeyj.num != F1)
		{
			continue;
		}
		if (obsKeyi.Sat != obsKeyj.Sat)
		{
			continue;
		}
		
		Obs& obs = *(Obs*)combinedMeas.metaDataMaps[i]["obs_ptr"];
		
		double lami = obs.satNav_ptr->lamMap[F2];
		double lamj = obs.satNav_ptr->lamMap[F1];
		
		double fi = CLIGHT / lami;
		double fj = CLIGHT / lamj;
		
		double coeffi = SQR(fi) / (SQR(fi) - SQR(fj));
		double coeffj = SQR(fj) / (SQR(fj) - SQR(fi));
		
		tripletList.push_back({meas, i, coeffi});
		tripletList.push_back({meas, j, coeffj});
		meas++;
		
		obsKeyj.num		= 12;
		newMeas.obsKeys.push_back(obsKeyj);
	}
	
	SparseMatrix<double> F = SparseMatrix<double>(meas, combinedMeas.V.rows());
	F.setFromTriplets(tripletList.begin(), tripletList.end());
	
	newMeas.V = F * combinedMeas.V;
	newMeas.A = F * combinedMeas.A;
	newMeas.R = F * combinedMeas.R * F.transpose();
	
	newMeas.metaDataMaps	= std::move(combinedMeas.metaDataMaps);
	newMeas.time			= std::move(combinedMeas.time);
	
	return newMeas;
}



/** Update the accelerations applied to orbital elements during the interval between epochs.
*/
// void updateOrbits(
// 	Trace&			trace,
// 	KFState&		kfState)
// {
// 	auto trace = getTraceFile(rec);
// 	
// 	const bool outputResidualChain = true;
// 	
// 	if (rec.obsList.empty())
// 	{
// 		return;
// 	}
// 	
// 	GTime time = rec.obsList.front().time;
// 	
// 	satposs(trace, time, rec.obsList, nav, E_Ephemeris::PRECISE, E_OffsetType::COM, false);
// 	
// 	double erpv[5] = {};
// 	geterp(&nav.erp, time, erpv);
// 	
// 	Matrix3d i2tMatrix;
// 	eci2ecef(time, erpv, i2tMatrix);
// 	KFKey oneKey;
// 	oneKey.type = KF::ONE;
// 	
// 	for (auto& [kfKey, index] : kfState.kfIndexMap)
// 	{
// 		if	( kfKey.type	!= KF::SAT_POS
// 			||kfKey.num		!= 0)
// 		{
// 			continue;
// 		}
// 		
// 		//this is the first orbital component for a satellite, accumulate it and all others.
// 		
// 		map<KFKey, int> keyMap;
// 		int keyCount = 0;
// 		for (auto& [satKey, satIndex] : kfState.kfIndexMap)
// 		{
// 			if	(  satKey.type == KF::SAT_POS
// 				|| satKey.type == KF::SAT_POS_RATE
// 				|| satKey.type == KF::SRP)
// 			{
// 				keyMap[satKey] = keyCount;
// 				keyCount++;
// 			}
// 		}
// 		
// 		double interval = tsync - kfState.time;
// 		
// 		Matrix3d mECI2ECEF		= Matrix3d::Zero();
// 		
// 		MatrixXd covariances;
// 		VectorXd oldStates		= kfState.getSubState(keyMap, &covariances);
// 		VectorXd newStates		= oldStates;
// 		
// 		propagateOrbit(std::cout, keyMap, newStates, covariances, mECI2ECEF, interval);
// 		
// 		MatrixXd F = MatrixXd::Identity(oldStates.rows(), oldStates.rows());
// 		
// 		//create a dummy state transition matrix
// 		for (int i = 0; i < 3; i++)
// 		{
// 			KFKey posKey = kfKey;
// 			posKey.num = i;
// 			
// 			KFKey velKey = posKey;
// 			velKey.type = KF::SAT_POS_RATE;
// 			
// 			int posIndex = keyMap[posKey];
// 			int velIndex = keyMap[velKey];
// 			
// 			F(posIndex, velIndex) = interval;
// 		}
// 		
// 		std::cout << std::endl << "F\n" << F << std::endl;
// 		VectorXd U = newStates - F * oldStates;
// 		
// 		std::cout << std::endl << "U " << U.transpose() << std::endl;
// 		
// 		for (auto& [key, index] : keyMap)
// 		{
// 			kfState.setKFTrans(key, oneKey, U[index]);
// 		}
// 
// 		if (0)
// 		{
// 			kfState.stateTransition(std::cout, tsync);
// 		
// 			Vector3d rSat	= Vector3d::Zero();
// 			Vector3d vSat	= Vector3d::Zero();
// 			
// 			for (short int i = 0; i < 3; i++)
// 			{
// 				kfState.getKFValue({.type = KF::SAT_POS,		.Sat = SatSys(E_Sys::GPS, 1), .num = i}, rSat[i]);
// 				kfState.getKFValue({.type = KF::SAT_POS_RATE,	.Sat = SatSys(E_Sys::GPS, 1), .num = i}, vSat[i]);
// 			}
// 			
// 			componentList.push_back({"Sat Phase Bias", satPhasBias});
// 		}
// 		
// 		
// 		//Calculate residuals and form up the measurement
// 		
// 		componentList.push_back({"Net Residual", 0});
// 		double residual = 0;
// 		for (auto& [componentName, componentVal] : componentList)
// 		{
// 			residual -= componentVal;
// 			
// 			if (outputResidualChain)
// 			{
// 				tracepdeex(0, trace, "\n%-20s %+14.4f -> %13.4f", componentName.c_str(), -componentVal, residual);
// 			}
// 		}
// 		measEntry.setInnov(residual);
// 		
// 	
// 		ObsKey obsKey;
// 		obsKey.str	= obs.mount;
// 		obsKey.Sat	= obs.Sat;
// 		
// 		//add ephemeris noise - common for all in obs
// 		if (satNoise)				{	obsKey.type = "satEph";		measEntry.addNoiseEntry(obsKey, 1, obs.ephVar);		}
// 		if (recClockNoise)			{	obsKey.type = "recClk";		measEntry.addNoiseEntry(obsKey, 1, precDtRecVar);	}
// 		
// 		//add signal noise - one per signal
// 		obsKey.type	= std::to_string(measType);
// 		obsKey.num	= ft;
// // 		std::cout << obsKey << std::endl;
// 		if		(measType == CODE)	{								measEntry.addNoiseEntry(obsKey, 1, sig.codeVar);	}
// 		else if	(measType == PHAS)	{								measEntry.addNoiseEntry(obsKey, 1, sig.phasVar);	}
// 		
// // 		tracepdeex(0, std::cout, "%14.6f %14.6f %14.6f %14.6f %14.6f\n", sig.codeVar, sig.phasVar, obs.ephVar, varTrop, 0);
// 
// 		kfMeasEntryList.push_back(measEntry);
// 	}
// }


void updateRecClocks(
	Trace&			trace,			///< Trace to output to
	StationMap&		stations,		///< List of stations containing observations for this epoch
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	for (auto& [id, rec] : stations)
	{
		auto	trace	= getTraceFile(rec);
		auto&	recOpts	= acsConfig.getRecOpts(id);
		
		if (recOpts.clk.estimate == false)
		{
			continue;
		}
		
		KFKey clkKey;
		clkKey.type		= KF::REC_CLOCK; 
		clkKey.str		= id;
		clkKey.rec_ptr	= &rec;
		
		KFKey oneKey;
		oneKey.type	= KF::ONE;
		
		double C_dtRecAdj	= rec.rtk.sol.dtRec_m[0]
							- rec.rtk.sol.dtRec_m_pppp_old[0];
							
		trace << std::endl
		<< "Adjusting " << clkKey.str
		<< " clock by " << C_dtRecAdj;
		
		rec.rtk.sol.dtRec_m_pppp_old[0] = rec.rtk.sol.dtRec_m[0];
		
		InitialState init		= initialStateFromConfig(recOpts.clk);
		
		kfState.setKFTrans(clkKey, oneKey, C_dtRecAdj, init);
	}
}

void propagateUncertainty(
	KFState&		kfState)
{
	MatrixXd F1;
	
	KFMeasEntryList kfMeasEntryList;
	
	KFKey pivotKey;
	
	if	(  acsConfig.pivot_station.empty()	== false
		&& acsConfig.pivot_station			!= "<AUTO>")
	{
		pivotKey.type	= KF::REC_CLOCK;
		pivotKey.str	= acsConfig.pivot_station;
	}
		
	if (0)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::REC_CLOCK)
		{
			continue;
		}
		
		if (pivotKey.str.empty())
		{
			pivotKey = key;
		}
		
		KFMeasEntry kfMeasEntry;
		if (pivotKey.str != key.str)
		{
			kfMeasEntry.addDsgnEntry(key,		+1 / CLIGHT * 1000);
			kfMeasEntry.addDsgnEntry(pivotKey,	-1 / CLIGHT * 1000);
		}
		
		kfMeasEntry.obsKey.str = key.str + "-" + pivotKey.str;
		
		kfMeasEntryList.push_back(kfMeasEntry);
	}
	
	if (0)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::SAT_CLOCK)
		{
			continue;
		}
		
		KFMeasEntry kfMeasEntry;
		kfMeasEntry.addDsgnEntry(key,		+1 / CLIGHT * 1000);
		kfMeasEntry.addDsgnEntry(pivotKey,	-1 / CLIGHT * 1000);
		
		kfMeasEntry.obsKey.str = " " + key.Sat.id() + "-" + pivotKey.str;
		
		kfMeasEntryList.push_back(kfMeasEntry);
	}
	
	for (int i = 1; i < kfState.kfIndexMap.size();	i++)
	for (int j = 0; j < i;							j++)
	{
		auto iti = kfState.kfIndexMap.begin();
		auto itj = kfState.kfIndexMap.begin();
		
		std::advance(iti, i);
		std::advance(itj, j);
		
		auto& [keyi, indexi] = *iti;
		auto& [keyj, indexj] = *itj;
		
		if (keyi.type != KF::AMBIGUITY)
		{
			continue;
		}
		
		if (keyj.type != KF::AMBIGUITY)
		{
			continue;
		}
		
		if (keyi.Sat != keyj.Sat)
		{
			continue;
		}
		
		if (keyi.str != keyj.str)
		{
			continue;
		}
		
		{
			KFMeasEntry kfMeasEntry;
			kfMeasEntry.addDsgnEntry(keyi,	-60.0/600);
			kfMeasEntry.addDsgnEntry(keyj,	+77.0/600);
			
			kfMeasEntry.obsKey.str = keyi.str + " " + keyi.Sat.id() + "C";
			
			kfMeasEntryList.push_back(kfMeasEntry);
		}
		{
			KFMeasEntry kfMeasEntry;
			kfMeasEntry.addDsgnEntry(keyi,	+60);
			kfMeasEntry.addDsgnEntry(keyj,	-77);
			
			kfMeasEntry.obsKey.str = keyi.str + " " + keyi.Sat.id() + "D";
			
			kfMeasEntryList.push_back(kfMeasEntry);
		}
	}
	
	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

	KFState propagatedState;
	
	propagatedState.P = combinedMeas.A * kfState.P * combinedMeas.A.transpose();
	propagatedState.x = combinedMeas.A * kfState.x;
	
	propagatedState.kfIndexMap.clear();
	
	int i = 0;
	for (auto& obsKey : combinedMeas.obsKeys)
	{
		KFKey kfKey;
		kfKey.type	= KF::CALC;
		kfKey.str	= obsKey.str;
		
		propagatedState.kfIndexMap[kfKey] = i;
		i++;
	}

	propagatedState.outputStates(std::cout);
}


void removeBadAmbiguities(
	Trace&				trace,
	KFState&			kfState); 

void removeBadIonospheres(
	Trace&				trace,
	KFState&			kfState); 

void incrementOutageCount(
	StationMap&		stations);

void PPP(
	Trace&			trace,			///< Trace to output to
	StationMap&		stations,		///< List of stations containing observations for this epoch
	KFState&		kfState,		///< Kalman filter object containing the network state parameters
	gptgrid_t&		gptg,			///< [in/out]
// 	vmf3_t*			vmf3,			///< [in/out]
	double*			orography)		///< Pointer to orography maps
{
	Instrument	instrument(__FUNCTION__);
	
	removeBadAmbiguities(trace, kfState);
	removeBadIonospheres(trace, kfState);
	
	incrementOutageCount(stations);
	
	//prepare receiver clocks using spp values to minimise pre-fit residuals
	updateRecClocks(trace, stations, kfState);
	
	updateOrbits(trace, kfState, tsync);
	
	//add process noise and dynamics to existing states as a prediction of current state
	{
// 		Instrument instrument("PPP state1");
		
		kfState.stateTransition(trace, tsync);
	}
	
	trace << std::endl << "Predicted states";
	
	{
// 		Instrument instrument("PPP output1");
		
		kfState.outputStates(trace);
	}
	
	//prepare a map of lists of measurements for use below
	map<string, KFMeasEntryList> stationKFEntryListMap;
	for (auto& [id, rec] : stations)
	{
		stationKFEntryListMap[rec.id] = KFMeasEntryList();
	}
	
	{		
// 		Instrument instrument("PPP obsOMC");
	
		//calculate the measurements for each station
	// #	ifdef ENABLE_PARALLELISATION
	// #	ifndef ENABLE_UNIT_TESTS
	// 			Eigen::setNbThreads(1);
	// #		pragma omp parallel for
	// #	endif
	// #	endif
		for (int i = 0; i < stations.size(); i++)
		{
			auto rec_iterator = stations.begin();
			std::advance(rec_iterator, i);
			
			auto& [id, rec] = *rec_iterator;
			
			stationPPP		(std::cout, rec, kfState, stationKFEntryListMap[rec.id], nav.gptg, nav.orography);
			stationPseudo	(std::cout, rec, kfState, stationKFEntryListMap[rec.id]);
		}
	}

	//combine all lists of measurements into a single list
	KFMeasEntryList kfMeasEntryList;
	for (auto& [rec, stationKFEntryList]	: stationKFEntryListMap)
	for (auto& kfMeasEntry					: stationKFEntryList)
	{
		kfMeasEntryList.push_back(std::move(kfMeasEntry));
	}
	
	//use state transition to initialise new state elements
	{
// 		Instrument instrument("PPP state2");
		
		kfState.stateTransition(trace, tsync);
	}
	
	for (auto& [key, value] : kfState.noiseElementMap)
	{
// 		std::cout << std::endl << key << "\t" << value;
	}
	
	KFMeas combinedMeas;
	{
// 		Instrument instrument("PPP combine");
		
		combinedMeas = kfState.combineKFMeasList(kfMeasEntryList, tsync);
	}
	
	combinedMeas = makeLCs(combinedMeas, kfState);
	
	if (kfState.lsqRequired)
	{
		kfState.lsqRequired = false;
		std::cout << std::endl << " -------INITIALISING PPPPP USING LEAST SQUARES--------" << std::endl;

		VectorXd dx;
 		kfState.leastSquareInitStatesA(trace, combinedMeas, false, &dx, true);
		
		trace << std::endl << "Least Squares Initialised States";
		kfState.outputStates(trace);
	}
	
	list<FilterChunk>	filterChunkList;
	map<string, std::ofstream>	traceList;	//keep in large scope
	
	if (acsConfig.netwOpts.chunk_stations)
	{
		map<string, int>	begH;
		map<string, int>	endH;
		map<string, int>	begX;
		map<string, int>	endX;
		
		for (int i = 0; i < combinedMeas.obsKeys.size(); i++)
		{
			auto& obsKey = combinedMeas.obsKeys[i];
			
			if (begH.find(obsKey.str) == begH.end())	{	begH[obsKey.str] = i;		}
														{	endH[obsKey.str] = i;		}
		}
		
		for (auto& [kfKey, index] : kfState.kfIndexMap)
		{
			if (begX.find(kfKey.str) == begX.end())		{	begX[kfKey.str] = index;	}
														{	endX[kfKey.str] = index;	}
		}
		
		for (auto& [str, dummy] : begH)
		{
			FilterChunk filterChunk;
			
			auto& rec = stations[str];
			
			traceList[str] = getTraceFile(rec);
			
			filterChunk.trace_ptr	= &traceList[str];
			
			filterChunk.begH = begH[str];			filterChunk.numH = endH[str] - begH[str] + 1;
			filterChunk.begX = begX[str];			filterChunk.numX = endX[str] - begX[str] + 1;
			
			filterChunkList.push_back(filterChunk);
		}
	}
	
	if (acsConfig.netwOpts.chunk_size)
	{
		list<FilterChunk> newFilterChunkList;
		FilterChunk	bigFilterChunk;
		
		int chunks		= filterChunkList.size() / acsConfig.netwOpts.chunk_size	+ 0.5;
		int chunkTarget = -1;
		if (chunks)			
			chunkTarget = filterChunkList.size() / chunks							+ 0.5;
		
		int count = 0;
		for (auto& filterChunk : filterChunkList)
		{
			if (count == 0)
			{
				bigFilterChunk = filterChunk;
				bigFilterChunk.trace_ptr = &trace;
			}
			else
			{
				bigFilterChunk.numX += filterChunk.numX;
				bigFilterChunk.numH += filterChunk.numH;
			}
			
			count++;
			
			if (count == chunkTarget)
			{
				newFilterChunkList.push_back(bigFilterChunk);
				count = 0;
			}
		}
		
		if (count)
		{
			newFilterChunkList.push_back(bigFilterChunk);
		}
		
		filterChunkList = std::move(newFilterChunkList);
	}

	
	{
// 		Instrument	instrument("PPP filter");
		
		std::cout << std::endl << " -------DOING PPPPP KALMAN FILTER --------" << std::endl;
	
		kfState.filterKalman(trace, combinedMeas, true, &filterChunkList);
	}
	
	postFilterChecks(combinedMeas);
	
	for (auto& filterChunk : filterChunkList)
	{
// 		kfState.outputStates(*filterChunk.trace_ptr, filterChunk.begX, filterChunk.numX);
	}
	
// 	lambdacalcs(kfState);
	
// 	propagateUncertainty(kfState);
}




