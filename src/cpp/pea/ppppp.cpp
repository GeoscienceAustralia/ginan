
// #pragma GCC optimize ("O0")

#include <iostream>
#include <fstream>
#include <string>
#include <tuple>
#include <map>

#ifdef ENABLE_PARALLELISATION
	#include "omp.h"
#endif

using std::string;
using std::tuple;
using std::map;


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "instrument.hpp"
#include "mongoWrite.hpp"
#include "navigation.hpp"
#include "orbitProp.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "metaData.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "lambda.h"
#include "ppp.hpp"

	
struct Duo
{
	map<KFKey, short int>*	indexMap_ptr;
	MatrixXd*				designMatrix_ptr;
};

void explainMeasurements(
	Trace&		trace,
	KFMeas&		meas,
	KFState&	kfState)
{
	for (int i = 0; i < meas.obsKeys.size(); i++)
	{
		auto& obsKey		= meas.obsKeys		[i];
		auto& metaDataMap	= meas.metaDataMaps	[i];
		
		if (metaDataMap["explain"] == nullptr)
		{
			continue;
		}
		
		trace << std::endl << ">>>>>>>>>>>>>>>>>>>>>>>>";
		trace << std::endl << "Explaining " << obsKey << " : " << obsKey.comment;
		
		
		for (int col = 0; col < meas.H.cols(); col++)
		{
			double entry = meas.H(i, col);
			
			if (entry == 0)
			{
				continue;
			}
			
			for (auto& [kfKey, index] : kfState.kfIndexMap)
			{
				if (index == col)
				{
					trace << std::endl << kfKey << " : " << entry;
					break;
				}
			}
		}
		trace << std::endl;
	}
}

/** Replace individual measurements with linear combinations
 */
KFMeas makeIFLCs(
	KFMeas&		combinedMeas,
	KFState&	kfState)
{
	Instrument instrument(__FUNCTION__);
	
	KFMeas newMeas;

	vector<Triplet<double>> tripletList;
	int meas = 0;
	
	vector<Duo> duos = 
	{
		{&kfState.kfIndexMap,		&combinedMeas.H},
		{&kfState.noiseIndexMap,	&combinedMeas.H_star}
	};
	
	for (auto duo : duos)
	for (auto& [kfKey, index] : *duo.indexMap_ptr)						{																if (kfKey.type != KF::IONO_STEC)	continue;
	for (int i_2 = 0; i_2 < combinedMeas.obsKeys.size();	i_2++)		{	double coeff_2 = (*duo.designMatrix_ptr)(i_2, index);		if (coeff_2 == 0)					continue;	
	for (int i_1 = 0; i_1 < i_2;							i_1++)		{	double coeff_1 = (*duo.designMatrix_ptr)(i_1, index);		if (coeff_1 == 0)					continue;
	{
		if (coeff_1 * coeff_2 < 0)								{	continue;	}	//only combine similarly signed (code/phase) components
		if (coeff_1	== coeff_2)									{	continue;	}	//dont combine if it will eliminate the entire measurement
		
		if (combinedMeas.metaDataMaps[i_1]["IFLCcombined"])		{	continue;	}
		if (combinedMeas.metaDataMaps[i_2]["IFLCcombined"]) 	{	continue;	}
		
		
		//these measurements both share a common ionosphere, remove it.
		
		double scalar = sqrt(  (SQR(coeff_1) + SQR(coeff_2)) / SQR(coeff_1 - coeff_2)  );
		
		tripletList.push_back({meas, i_1, +coeff_2 * scalar});
		tripletList.push_back({meas, i_2, -coeff_1 * scalar});
		meas++;
		
		auto& obsKey_1 = combinedMeas.obsKeys[i_1];
		auto& obsKey_2 = combinedMeas.obsKeys[i_2];
		
		auto newObsKey = obsKey_2;
		
		newObsKey.num += 100 * obsKey_1.num;			//100(1) + 1(2)
		
		newObsKey.comment = obsKey_1.comment + "-" + obsKey_2.comment;
		
		newMeas.obsKeys			.push_back(newObsKey);
		
		//copy metadata from the second to the first, then copy into the new measurement
		for (auto& [id, value] : combinedMeas.metaDataMaps[i_2])
		{
			combinedMeas.metaDataMaps[i_1][id + "_alt"] = value;
		}
		
		for (auto& component : combinedMeas.componentLists[i_2])
		{
			combinedMeas.componentLists[i_1].push_back(component);
		}
		
		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i_1]);
		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i_1]);
		
		combinedMeas.metaDataMaps[i_1]["IFLCcombined"] = (void*) true;
		combinedMeas.metaDataMaps[i_2]["IFLCcombined"] = (void*) true;
	}}}}
	
	if (meas == 0)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No IONO_STEC measurements found - 'use_if_combo' requires 'ion_stec' estimation to be enabled in the config file.";
	}
	
	for (int i = 0; i < combinedMeas.obsKeys.size(); i++)
	{
		if (combinedMeas.metaDataMaps[i]["pseudoObs"] == (void*) false)
		{
			continue;
		}
		
		//need to keep this measurement even if its not a valid ionospheric one, copy it over
		
		tripletList.push_back({meas, i, 1});
		meas++;
		
		newMeas.obsKeys			.push_back(combinedMeas.obsKeys			[i]);
		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i]);
		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i]);
	}
	
	SparseMatrix<double> F;
	F = SparseMatrix<double>(meas, combinedMeas.V.rows());
	F.setFromTriplets(tripletList.begin(), tripletList.end());
	
	newMeas.V		= F * combinedMeas.V;
	newMeas.VV		= newMeas.V;
	newMeas.H		= F * combinedMeas.H;
	newMeas.R		= F * combinedMeas.R * F.transpose();
	newMeas.time	= std::move(combinedMeas.time);
	
	return newMeas;
}

/** Replace individual measurements with linear combinations
 */
KFMeas makeGFLCs(
	KFMeas&		combinedMeas,
	KFState&	kfState)
{
	Instrument instrument(__FUNCTION__);
	
	KFMeas newMeas;

	vector<Triplet<double>> tripletList;
	int meas = 0;
	
	vector<Duo> duos = 
	{
		{&kfState.kfIndexMap,		&combinedMeas.H},
		{&kfState.noiseIndexMap,	&combinedMeas.H_star}
	};
	
	for (auto duo : duos)
	for (auto& [kfKey, index] : *duo.indexMap_ptr)						{																if (kfKey.type != KF::IONO_STEC)	continue;
	for (int i_2 = 0; i_2 < combinedMeas.obsKeys.size();	i_2++)		{	double coeff_2 = (*duo.designMatrix_ptr)(i_2, index);		if (coeff_2 == 0)					continue;	
	for (int i_1 = 0; i_1 < i_2;							i_1++)		{	double coeff_1 = (*duo.designMatrix_ptr)(i_1, index);		if (coeff_1 == 0)					continue;
	{
		if (coeff_1 * coeff_2 < 0)								{	continue;	}
		if (coeff_1	== coeff_2)									{	continue;	}	//dont combine if it will eliminate the entire measurement
		
		if (combinedMeas.metaDataMaps[i_1]["GFLCcombined"])		{	continue;	}
		if (combinedMeas.metaDataMaps[i_2]["GFLCcombined"]) 	{	continue;	}
		
		//these measurements probably both share a common geometry, remove it.
		
		double scalar = 0.5;
		
		tripletList.push_back({meas, i_1, +1 * scalar});
		tripletList.push_back({meas, i_2, -1 * scalar});
		meas++;
		
		auto& obsKey_1 = combinedMeas.obsKeys[i_1];
		auto& obsKey_2 = combinedMeas.obsKeys[i_2];
		
		auto newObsKey = obsKey_2;
		
		newObsKey.num += 100 * obsKey_1.num;			//100(1) + 1(2)
		
		newObsKey.comment = obsKey_1.comment + "-" + obsKey_2.comment;
		
		newMeas.obsKeys			.push_back(newObsKey);
		
		//copy metadata from the second to the first, then copy into the new measurement
		for (auto& [id, value] : combinedMeas.metaDataMaps[i_2])
		{
			combinedMeas.metaDataMaps[i_1][id + "_alt"] = value;
		}
		
		for (auto& component : combinedMeas.componentLists[i_2])
		{
			combinedMeas.componentLists[i_1].push_back(component);
		}

		combinedMeas.metaDataMaps[i_1]["explain"] = (void*) true;

		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i_1]);
		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i_1]);
		
		combinedMeas.metaDataMaps[i_1]["GFLCcombined"] = (void*) true;
		combinedMeas.metaDataMaps[i_2]["GFLCcombined"] = (void*) true;
	}}}}
	
	if (meas == 0)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No IONO_STEC measurements found - 'use_gf_combo' requires 'iono_stec' estimation to be enabled in the config file.";
	}
	
	for (int i = 0; i < combinedMeas.obsKeys.size(); i++)
	{
		if (combinedMeas.metaDataMaps[i]["pseudoObs"] == (void*) false)
		{
			continue;
		}
		
		//need to keep this measurement even if its not a valid ionospheric one, copy it over
		
		tripletList.push_back({meas, i, 1});
		meas++;
		
		newMeas.obsKeys			.push_back(combinedMeas.obsKeys			[i]);
		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i]);
		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i]);
	}
	
	SparseMatrix<double> F;
	F = SparseMatrix<double>(meas, combinedMeas.V.rows());
	F.setFromTriplets(tripletList.begin(), tripletList.end());
	
	newMeas.V		= F * combinedMeas.V;
	newMeas.VV		= newMeas.V;
	newMeas.H		= F * combinedMeas.H;
	newMeas.R		= F * combinedMeas.R * F.transpose();
	newMeas.time	= std::move(combinedMeas.time);
	
	return newMeas;
}


/** Replace individual measurements with linear combinations
 */
KFMeas makeRTKLCs(
	KFMeas&		combinedMeas,
	KFState&	kfState)
{
	Instrument instrument(__FUNCTION__);
	
	auto& recOpts = acsConfig.getRecOpts("");
	
	KFMeas newMeas;

	vector<Triplet<double>> tripletList;
	int meas = 0;
	
	vector<Duo> duos = 
	{
		{&kfState.kfIndexMap,		&combinedMeas.H},
		{&kfState.noiseIndexMap,	&combinedMeas.H_star}
	};
	
	for (auto duo : duos)
	for (auto& [kfKey, index] : *duo.indexMap_ptr)						{																if (kfKey.type != KF::SAT_CLOCK)	continue;
	for (int i_2 = 0; i_2 < combinedMeas.obsKeys.size();	i_2++)		{	double coeff_2 = (*duo.designMatrix_ptr)(i_2, index);		if (coeff_2 == 0)					continue;	
	for (int i_1 = 0; i_1 < i_2;							i_1++)		{	double coeff_1 = (*duo.designMatrix_ptr)(i_1, index);		if (coeff_1 == 0)					continue;
	{
		if (combinedMeas.metaDataMaps[i_1]["RTKcombined"])		{	continue;	}
		if (combinedMeas.metaDataMaps[i_2]["RTKcombined"])		{	continue;	}
		
		//these measurements both share a common satellite clock, remove it.
	
		tripletList.push_back({meas, i_1, +coeff_2});
		tripletList.push_back({meas, i_2, -coeff_1});
		meas++;
		
		auto& obsKey_1 = combinedMeas.obsKeys[i_1];
		auto& obsKey_2 = combinedMeas.obsKeys[i_2];
		
		auto newObsKey = obsKey_2;
		
// 		newObsKey.num += 100 * obsKey_1.num;			//100(1) + 1(2)		//todo aaron
		
		newObsKey.comment = obsKey_1.comment + "-" + obsKey_2.comment;
		
		newMeas.obsKeys			.push_back(newObsKey);
		
		//copy metadata from the second to the first, then copy into the new measurement
		for (auto& [id, value] : combinedMeas.metaDataMaps[i_2])
		{
			combinedMeas.metaDataMaps[i_1][id + "_alt"] = value;
		}
		
		for (auto& component : combinedMeas.componentLists[i_2])
		{
			combinedMeas.componentLists[i_1].push_back(component);
		}

		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i_1]);
		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i_1]);
		
		combinedMeas.metaDataMaps[i_1]["RTKcombined"] = (void*) true;
		combinedMeas.metaDataMaps[i_2]["RTKcombined"] = (void*) true;
	}}}}
	
	
	for (int i = 0; i < combinedMeas.obsKeys.size(); i++)
	{
		if (combinedMeas.metaDataMaps[i]["pseudoObs"] == (void*) false)
		{
			continue;
		}
		
		//need to keep this measurement even if its not a valid rtk one, copy it over
		
		tripletList.push_back({meas, i, 1});
		meas++;
		
		newMeas.obsKeys			.push_back(combinedMeas.obsKeys			[i]);
		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i]);
		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i]);
	}
	
	SparseMatrix<double> F;
	F = SparseMatrix<double>(meas, combinedMeas.V.rows());
	F.setFromTriplets(tripletList.begin(), tripletList.end());
	
	newMeas.V		= F * combinedMeas.V;
	newMeas.VV		= newMeas.V;
	newMeas.H		= F * combinedMeas.H;
	newMeas.R		= F * combinedMeas.R * F.transpose();
	newMeas.time	= std::move(combinedMeas.time);
	
	return newMeas;
}

/** Prepare receiver clocks using spp values to minimise pre-fit residuals
 */
void updateRecClocks(
	Trace&			trace,			///< Trace to output to
	StationMap&		stations,		///< List of stations containing observations for this epoch
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	for (auto& [id, rec] : stations)
	{
		auto	trace	= getTraceFile(rec);
		auto&	recOpts	= acsConfig.getRecOpts(id);
		
		InitialState init		= initialStateFromConfig(recOpts.clk);
		
		if (init.estimate == false)
		{
			continue;
		}
		
		KFKey clkKey;
		clkKey.type		= KF::REC_CLOCK; 
		clkKey.str		= id;
		clkKey.rec_ptr	= &rec;
		
		double C_dtRecAdj	= rec.sol.dtRec_m[E_Sys::GPS]
							- rec.sol.dtRec_m_pppp_old[E_Sys::GPS];
							
		trace << std::endl
		<< "Adjusting " << clkKey.str
		<< " clock by " << C_dtRecAdj;
		
		rec.sol.dtRec_m_pppp_old[E_Sys::GPS] = rec.sol.dtRec_m[E_Sys::GPS];
		
		kfState.setKFTrans(clkKey, KFState::oneKey, C_dtRecAdj, init);		//todo aaron, change to rate?
	}
}

/** Prepare Satellite clocks to minimise residuals to broadcast clocks
 */
void updateAvgClocks(
	Trace&			trace,			///< Trace to output to
	GTime			time,			///< Time
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	double sum = 0;
	double num = 0;
	
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::SAT_CLOCK)
		{
			continue;
		}
		
		SatPos satPos;
		satPos.Sat			= key.Sat;
		satPos.satNav_ptr	= &nav.satNavMap[key.Sat];
		
		bool pass = satClkBroadcast(trace, time, time, satPos, nav);
		if (pass == false)
		{
			continue;
		}
		
		double brdcClk = satPos.satClk;
		
		double stateClk = kfState.x(index);
	
		double delta = brdcClk - stateClk;
		
		sum += delta;
		num++;
	}
	
	if (num == 0)
	{
		return;
	}
	
	double avg = sum / num;
	
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type != KF::SAT_CLOCK
			&&key.type != KF::REC_CLOCK)
		{
			continue;
		}
		
		trace << std::endl
		<< "Adjusting " << key.Sat.id() << key.str
		<< " clock by " << avg;
		
		kfState.setKFTrans(key, KFState::oneKey, avg);	//todo aaron, this will interact poorly with receiver one?
	}
}

KFState propagateUncertainty(
	Trace&			trace,
	KFState&		kfState)
{
	MatrixXd F1;
	
	KFMeasEntryList kfMeasEntryList;
	
	KFKey pivotKey;
	
	if	(  acsConfig.pivot_station.empty()	== false
		&& acsConfig.pivot_station			!= "<AUTO>"
		&& acsConfig.pivot_station			!= "NO_PIVOT")
	{
		pivotKey.type	= KF::REC_CLOCK;
		pivotKey.str	= acsConfig.pivot_station;
	}
		
// 	if (0)
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
			kfMeasEntry.addDsgnEntry(key,		+1);
			kfMeasEntry.addDsgnEntry(pivotKey,	-1);
		}
		
		kfMeasEntry.obsKey.str = key.str + "-" + pivotKey.str;
		
		kfMeasEntryList.push_back(kfMeasEntry);
	}
	
// 	if (0)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::SAT_CLOCK)
		{
			continue;
		}
		
		KFMeasEntry kfMeasEntry;
		kfMeasEntry.addDsgnEntry(key,		+1);
		kfMeasEntry.addDsgnEntry(pivotKey,	-1);
		
		kfMeasEntry.obsKey.str = " " + key.Sat.id() + "-" + pivotKey.str;
		
		kfMeasEntryList.push_back(kfMeasEntry);
	}
	
	if (0)
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
	
	propagatedState.time	= kfState.time;
	propagatedState.P		= combinedMeas.H * kfState.P * combinedMeas.H.transpose();
	propagatedState.x		= combinedMeas.H * kfState.x;
	propagatedState.dx		= combinedMeas.H * kfState.dx;
	
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
	
	return propagatedState;
}

void chunkFilter(
	Trace&						trace,
	KFState&					kfState,
	KFMeas&						combinedMeas,
	StationMap&					stationMap,
	vector<FilterChunk>&		filterChunkList,
	map<string, std::ofstream>&	traceList)	
{	
	if (acsConfig.pppOpts.station_chunking == false)
	{
		return;
	}
	
	map<string, int>	begH;
	map<string, int>	endH;
	map<string, int>	begX;
	map<string, int>	endX;
	
	//get all meas begs/ends for this type
	for (int i = 0; i < combinedMeas.obsKeys.size(); i++)
	{
		auto& obsKey = combinedMeas.obsKeys[i];
		
		string chunkId = obsKey.str;
		
		if (begH.find(chunkId) == begH.end())			{	begH[chunkId] = i;		}
														{	endH[chunkId] = i;		}
	}
	
	//get all state begs/ends for this type
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		string chunkId = kfKey.str;
		
		if (begX.find(chunkId) == begX.end())			{	begX[chunkId] = index;	}
														{	endX[chunkId] = index;	}
	}
	
	tuple<map<string, int>&, map<string, int>&> measTuple	= {begH, endH};
	tuple<map<string, int>&, map<string, int>&> stateTuple	= {begX, endX};
	
	//check for overlapping chunks for both matrices
	for (auto& duo : {measTuple, stateTuple})
	{
		auto& [begMap, endMap] = duo;
		
		for (auto& [str, beg] : begMap)
		{
			auto& end = endMap[str];
			
			for (auto& [str2, beg2] : begMap)
			{
				if (str == str2)
				{
					continue;
				}
				
				auto& end2 = endMap[str2];
				
				if	( (beg2 > beg && beg2 < end)		// 2 starts in the middle of beg,end
					||(end2 > beg && end2 < end))		// 2 ends   in the middle of beg,end
				{
					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: Cannot chunk filter with current configuration - disabling.";
					
					acsConfig.pppOpts.station_chunking		= false;
					return;
				}
			}
		}
	}
	
	
	for (auto& [str, dummy] : begH)
	{
		FilterChunk filterChunk;
		
		auto& rec = stationMap[str];
		
		traceList[str] = getTraceFile(rec);
		
		filterChunk.trace_ptr	= &traceList[str];
		
		filterChunk.begH = begH[str];			filterChunk.numH = endH[str] - begH[str] + 1;
		filterChunk.begX = begX[str];			filterChunk.numX = endX[str] - begX[str] + 1;
		
		filterChunkList.push_back(filterChunk);
	}
	
	if (acsConfig.pppOpts.chunk_size)
	{
		Instrument	instrument("PPP chunksize");
		
		vector<FilterChunk> newFilterChunkList;
		FilterChunk	bigFilterChunk;
		
		int chunks		= filterChunkList.size() / acsConfig.pppOpts.chunk_size	+ 0.5;
		int chunkTarget = -1;
		if (chunks)			
			chunkTarget = filterChunkList.size() / chunks						+ 0.5;
		
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
}

void removeBadAmbiguities(
	Trace&			trace,
	KFState&		kfState,
	StationMap&		stationMap);

void removeBadIonospheres(
	Trace&			trace,
	KFState&		kfState); 

void incrementOutageCount(
	StationMap&		stations);

void checkOrbits(
	Trace&			trace,
	KFState&		kfState);




void PPP(
	Trace&			trace,			///< Trace to output to
	StationMap&		stationMap,		///< List of stations containing observations for this epoch
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	Instrument	instrument(__FUNCTION__);
	
	{
		Instrument instrument("PPP pppre");
		
		removeBadAmbiguities(trace, kfState, stationMap);
		removeBadIonospheres(trace, kfState);
		
		incrementOutageCount(stationMap);
		
		updateRecClocks(trace, stationMap,	kfState);
// 		updateAvgClocks(trace, tsync,		kfState);
		
		//integrate orbits
		predictOrbits(trace, kfState, tsync);
	}
	
	//add process noise and dynamics to existing states as a prediction of current state
	{
		Instrument instrument("PPP stateTransition1");
		
		kfState.stateTransition(trace, tsync);
	
		kfState.outputStates(trace, "/PREDICTED");
	}
	
	//prepare a map of lists of measurements for use below
	map<string, KFMeasEntryList> stationKFEntryListMap;
	for (auto& [id, rec] : stationMap)
	{
		stationKFEntryListMap[rec.id] = KFMeasEntryList();
	}
	
	MatrixXd	R;
	MatrixXd*	R_ptr = nullptr;
// 	if (kfMeasEntryList.empty())
	{
// 		R_ptr = &R;
	}
	
	map<SatSys,int> activeSatMap;
	{	
		Instrument instrument("PPP obsOMC");
	
		BOOST_LOG_TRIVIAL(info) << " ------- CALCULATING PPP MEASUREMENTS --------" << std::endl;
	
		//calculate the measurements for each station
#		ifdef ENABLE_PARALLELISATION
			Eigen::setNbThreads(1);
#			pragma omp parallel for
#		endif
		for (int i = 0; i < stationMap.size(); i++)
		{
			auto rec_iterator = stationMap.begin();
			std::advance(rec_iterator, i);
			
			auto& [id, rec] = *rec_iterator;
			
			if (rec.obsList.empty())
			{
				continue;
			}
			
			auto& kfMeasEntryList = stationKFEntryListMap[rec.id];
			
			orbitPseudoObs	(trace,		rec, kfState, kfMeasEntryList);
			stationPPP		(std::cout,	rec, kfState, kfMeasEntryList);
			stationSlr		(std::cout, rec, kfState, kfMeasEntryList);
			stationPseudoObs(std::cout,	rec, kfState, kfMeasEntryList, stationMap, R_ptr);
		}
		Eigen::setNbThreads(0);
	}
	
	//combine all lists of measurements into a single list
	KFMeasEntryList kfMeasEntryList;
	for (auto& [rec, stationKFEntryList]	: stationKFEntryListMap)
	for (auto& kfMeasEntry					: stationKFEntryList)
	{
		kfMeasEntryList.push_back(std::move(kfMeasEntry));
	}
	
	// apply external Ionosphere estimates
	ionoPseudoObs(trace, stationMap, kfState, kfMeasEntryList);
	
	//apply pseudoobs to states available from before
	biasPseudoObs(trace, kfState, kfMeasEntryList);
	ambgPseudoObs(trace, kfState, kfMeasEntryList);

	initPseudoObs(trace, kfState, kfMeasEntryList);
	
	//use state transition to initialise new state elements
	{
		Instrument	instrument("PPP stateTransition2");
		
		kfState.stateTransition(trace, tsync);
		
// 		kfState.outputStates(trace, "/INITIALISED");
	}

	
	KFMeas combinedMeas;
	{
		combinedMeas = kfState.combineKFMeasList(kfMeasEntryList, tsync, R_ptr);
	}
	
	if (acsConfig.ionoOpts.use_gf_combo)	{	combinedMeas = makeGFLCs	(combinedMeas, kfState);	}
	if (acsConfig.ionoOpts.use_if_combo)	{	combinedMeas = makeIFLCs	(combinedMeas, kfState);	}
	if (acsConfig.pppOpts.use_rtk_combo)	{	combinedMeas = makeRTKLCs	(combinedMeas, kfState);	}
	
	if (acsConfig.explain_measurements)
	{
		explainMeasurements(trace, combinedMeas, kfState);
	}
	
	if (kfState.lsqRequired)
	{
		kfState.lsqRequired = false;
		BOOST_LOG_TRIVIAL(info) << "-------INITIALISING PPPPP USING LEAST SQUARES--------" << std::endl;

		VectorXd dx;
 		kfState.leastSquareInitStatesA(trace, combinedMeas, false, &dx, true);
		
		kfState.outputStates(trace, "/LSQ");
	}
	
	vector<FilterChunk>	filterChunkList;
	map<string, std::ofstream>	traceList;	//keep in large scope as we're using pointers
	
	chunkFilter(trace, kfState, combinedMeas, stationMap, filterChunkList, traceList);
	
	BOOST_LOG_TRIVIAL(info) << " ------- DOING PPPPP KALMAN FILTER    --------" << std::endl;

	kfState.filterKalman(trace, combinedMeas, true, &filterChunkList);
	
	{
		Instrument	instrument("PPP postFilterChecks");
		
		postFilterChecks(combinedMeas);
	}
	
	//output chunks if we are actually chunking still
	if	( acsConfig.pppOpts.station_chunking
		||acsConfig.pppOpts.satellite_chunking)
	for (auto& filterChunk : filterChunkList)
	{
		kfState.outputStates(*filterChunk.trace_ptr, "/PPP", filterChunk.begX, filterChunk.numX);
	}
	
	kfState.outputStates(trace, "/PPP");
}




