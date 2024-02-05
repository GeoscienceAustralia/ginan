
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
#include "mongoRead.hpp"
#include "orbitProp.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "metaData.hpp"
#include "receiver.hpp"
#include "posProp.hpp"
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

		trace << std::endl << "============================";
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

void makeIFLCs(
	Trace&				trace,
	const KFState&		kfState,
	KFMeasEntryList&	kfMeasEntryList)
{
	Instrument instrument(__FUNCTION__);

	bool iflcMade = false;

	for (int i = 0; i < kfMeasEntryList.size(); i++)
	{
		auto& kfMeasEntryI = kfMeasEntryList[i];

		if (kfMeasEntryI.valid == false)
		{
			continue;
		}

		double	coeff_i = 0;
		KFKey	ionKey_i;

		for (auto& [key, value] : kfMeasEntryI.designEntryMap)
		{
			if (key.type == KF::IONO_STEC)
			{
				ionKey_i	= key;
				coeff_i		= value;
				break;
			}
		}

		if (coeff_i == 0)
		{
			//no ionosphere reference
			continue;
		}

		//either this will be combined with something below, or it wont be, in either case this one is not needed and invalid now
		kfMeasEntryI.valid = false;

		for (int j = i + 1; j < kfMeasEntryList.size(); j++)
		{
			auto& kfMeasEntryJ = kfMeasEntryList[j];

			if (kfMeasEntryI.metaDataMap["IFLCcombined"])			{	continue;	}
			if (kfMeasEntryJ.metaDataMap["IFLCcombined"])			{	continue;	}

			auto it = kfMeasEntryJ.designEntryMap.find(ionKey_i);
			if (it == kfMeasEntryJ.designEntryMap.end())
			{
				continue;
			}

			auto& [ionKey_j, coeff_j] = *it;

			double coefj = coeff_j;

			if (coeff_i * coeff_j < 0)								{	continue;	}	//only combine similarly signed (code/phase) components
			if (coeff_i == coeff_j)									{	continue;	}	//dont combine if it will eliminate the entire measurement

			//these measurements both share a common ionosphere, remove it.

			iflcMade = true;

			double scalar = sqrt(  (SQR(coeff_i) + SQR(coeff_j)) / SQR(coeff_i - coeff_j)  );

			kfMeasEntryJ.obsKey.num		= 100 * kfMeasEntryI.obsKey.num
										+		kfMeasEntryJ.obsKey.num;

			kfMeasEntryJ.obsKey.comment	=		kfMeasEntryI.obsKey.comment
										+ "-" +	kfMeasEntryJ.obsKey.comment;

			kfMeasEntryJ.innov	= coeff_j * scalar * kfMeasEntryI.innov
								- coeff_i * scalar * kfMeasEntryJ.innov;

			map<KFKey,			double>				newDesignEntryMap;
			map<KFKey,			double>				newNoiseEntryMap;
			map<E_Component,	ComponentsDetails>	newComponentsMap;

			for (auto& [key, valueI]	: kfMeasEntryI.usedValueMap)		kfMeasEntryJ.usedValueMap	[key] = valueI;

			for (auto& [key, valueI]	: kfMeasEntryI.designEntryMap)		newDesignEntryMap			[key] += valueI * coeff_j * scalar * +1;
			for (auto& [key, valueJ]	: kfMeasEntryJ.designEntryMap)		newDesignEntryMap			[key] += valueJ * coeff_i * scalar * -1;

			for (auto& [key, valueI]	: kfMeasEntryI.noiseEntryMap)		newNoiseEntryMap			[key] += valueI * coeff_j * scalar * +1;
			for (auto& [key, valueJ]	: kfMeasEntryJ.noiseEntryMap)		newNoiseEntryMap			[key] += valueJ * coeff_i * scalar * -1;

			for (auto& [key, valueI]	: kfMeasEntryI.componentsMap)		newComponentsMap			[key] += valueI * coeff_j * scalar * +1;
			for (auto& [key, valueJ]	: kfMeasEntryJ.componentsMap)		newComponentsMap			[key] += valueJ * coeff_i * scalar * -1;

			for (auto& [id, value] : kfMeasEntryI.metaDataMap)
			{
				kfMeasEntryJ.metaDataMap[id + "_alt"] = value;
			}

			kfMeasEntryI.metaDataMap["IFLCcombined"]	= (void*) true;
			kfMeasEntryJ.metaDataMap["IFLCcombined"]	= (void*) true;
// 			kfMeasEntryJ.metaDataMap["explain"]			= (void*) true;

			newDesignEntryMap[ionKey_j] = 0;
			kfMeasEntryJ.designEntryMap	= std::move(newDesignEntryMap);
			kfMeasEntryJ.noiseEntryMap	= std::move(newNoiseEntryMap);

			kfState.removeState(ionKey_i);
			kfState.removeState(ionKey_j);
			break;
		}
	}

	if	( kfMeasEntryList.empty()	== false
		&&iflcMade					== false)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No IONO_STEC measurements found - 'use_if_combo' requires 'ion_stec' estimation to be enabled in the config file.";
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

// 		for (auto& component : combinedMeas.componentLists[i_2])
// 		{
// 			combinedMeas.componentLists[i_1].push_back(component);
// 		}

		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i_1]);
// 		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i_1]);

		combinedMeas.metaDataMaps[i_1]["IFLCcombined"] = (void*) true;
		combinedMeas.metaDataMaps[i_2]["IFLCcombined"] = (void*) true;

		combinedMeas.metaDataMaps[i_1]["explain"]			= (void*) true;
		combinedMeas.metaDataMaps[i_2]["explain"]			= (void*) true;
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
// 		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i]);
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

// 		for (auto& component : combinedMeas.componentLists[i_2])
// 		{
// 			combinedMeas.componentLists[i_1].push_back(component);
// 		}

		combinedMeas.metaDataMaps[i_1]["explain"] = (void*) true;

		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i_1]);
// 		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i_1]);

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
// 		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i]);
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

// 		for (auto& component : combinedMeas.componentLists[i_2])
// 		{
// 			combinedMeas.componentLists[i_1].push_back(component);
// 		}

		newMeas.metaDataMaps	.push_back(combinedMeas.metaDataMaps	[i_1]);
// 		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i_1]);=

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
// 		newMeas.componentLists	.push_back(combinedMeas.componentLists	[i]);
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
	ReceiverMap&	receiverMap,	///< List of stations containing observations for this epoch
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	for (auto& [id, rec] : receiverMap)
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

/** Prepare stec values clocks to minimise residuals to klobuchar model
 */
void updateAvgIonosphere(
	Trace&			trace,			///< Trace to output to
	GTime			time,			///< Time
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	if (acsConfig.minimise_ionosphere_offsets == false)
	{
		return;
	}

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONO_STEC)
		{
			continue;
		}

		if (key.rec_ptr == nullptr)
		{
			continue;
		}

		auto& rec = *key.rec_ptr;

		auto& satStat	= rec.satStatMap[key.Sat];
		auto& satNav	= nav.satNavMap[key.Sat];
		auto& recOpts	= acsConfig.getRecOpts(rec.id);

		double diono	= 0;
		double dummy	= 0;
		double dummy2	= 0;
		bool pass = ionoModel(time, rec.pos, satStat, E_IonoMapFn::KLOBUCHAR, E_IonoMode::BROADCAST, 0, dummy, diono, dummy2);
		if (pass == false)
		{
			continue;
		}

		double alpha = 40.3e16 / SQR(CLIGHT / satNav.lamMap[F1]);

		double ionosphereStec = diono / alpha;

		//update the mu value but dont use the state thing - it will re-add it after its deleted
		// kfState.addKFState(key, init);
		kfState.gaussMarkovMuMap[key] = ionosphereStec;
	}
}

/** Prepare Satellite clocks to minimise residuals to broadcast clocks
 */
void updateAvgClocks(
	Trace&			trace,			///< Trace to output to
	GTime			time,			///< Time
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	if (acsConfig.minimise_sat_clock_offsets == false)
	{
		return;
	}

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

		auto& satOpts = acsConfig.getSatOpts(key.Sat);

		InitialState init = initialStateFromConfig(satOpts.clk);

		init.mu = satPos.satClk * CLIGHT;

		//update the mu value
		kfState.addKFState(key, init);
	}
}

KFState propagateUncertainty(
	Trace&			trace,
	KFState&		kfState)
{
	MatrixXd F1;

	KFMeasEntryList kfMeasEntryList;

	map<KFKey, KFMeasEntry> kfMeasEntryMap;

	string pivotRec = acsConfig.pivot_receiver;

// 	if (0)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type != KF::REC_CLOCK
			&&key.type != KF::SAT_CLOCK
			&&key.type != KF::REC_CLOCK_RATE
			&&key.type != KF::SAT_CLOCK_RATE)
		{
			continue;
		}

		if	( pivotRec	== "<AUTO>"
			&&key.type 		== KF::REC_CLOCK
			&&key.num		== 0)
		{
			pivotRec = key;
		}

		auto subKey = key;
		subKey.num = 0;

		auto& kfMeasEntry = kfMeasEntryMap[subKey];

		kfMeasEntry.addDsgnEntry(key, +1);

		string newComment = kfMeasEntry.obsKey.comment;
		if (newComment.empty())
		{
			newComment += "=>";
		}
		newComment += " + [" + key.commaString() + "]";

		kfMeasEntry.obsKey = subKey;

		kfMeasEntry.obsKey.comment = newComment;
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
		// 	auto& kfMeasEntry = kfMeasEntryMap[key.str + key.Sat.id()];
		// 	kfMeasEntry.addDsgnEntry(keyi,	-60.0/600);
		// 	kfMeasEntry.addDsgnEntry(keyj,	+77.0/600);
  //
		// 	kfMeasEntry.obsKey.str = keyi.str + " " + keyi.Sat.id() + "C";
		// }
		// {
		// 	auto& kfMeasEntry = kfMeasEntryMap[key.str + key.Sat.id()];
		// 	kfMeasEntry.addDsgnEntry(keyi,	+60);
		// 	kfMeasEntry.addDsgnEntry(keyj,	-77);
  //
		// 	kfMeasEntry.obsKey.str = keyi.str + " " + keyi.Sat.id() + "D";
		}
	}

	for (auto& [id, entry] : kfMeasEntryMap)
	{
		if (pivotRec != "NO_PIVOT")
		for (auto& [pivotKey, pivotEntry] : kfState.kfIndexMap)
		{
			if	( pivotKey.type != KF::REC_CLOCK
				&&pivotKey.type != KF::REC_CLOCK_RATE)
			{
				continue;
			}

			if (pivotKey.str != pivotRec)
			{
				continue;
			}

			if	(  pivotKey.type	== KF::REC_CLOCK
				&& id.type			!= KF::REC_CLOCK
				&& id.type			!= KF::SAT_CLOCK)
			{
				continue;
			}

			entry.addDsgnEntry(pivotKey, -1);

			entry.obsKey.comment += " - [" + pivotKey.commaString() + "]";
		}

		kfMeasEntryList.push_back(entry);
	}

	KFState propagatedState;

	if (kfMeasEntryList.empty())
	{
		return propagatedState;
	}

	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

	propagatedState.time	= kfState.time;
	propagatedState.P		= combinedMeas.H * kfState.P * combinedMeas.H.transpose();
	propagatedState.x		= combinedMeas.H * kfState.x;
	propagatedState.dx		= combinedMeas.H * kfState.dx;

	propagatedState.kfIndexMap.clear();

	int i = 0;
	for (auto& obsKey : combinedMeas.obsKeys)
	{
		propagatedState.kfIndexMap[obsKey] = i;
		i++;
	}

	return propagatedState;
}

void chunkFilter(
	Trace&						trace,
	KFState&					kfState,
	KFMeas&						combinedMeas,
	ReceiverMap&					receiverMap,
	vector<FilterChunk>&		filterChunkList,
	map<string, std::ofstream>&	traceList)
{
	if (acsConfig.pppOpts.receiver_chunking == false)
	{
		return;
	}

	map<string, int>	begH;
	map<string, int>	endH;
	map<string, int>	begX;
	map<string, int>	endX;

	//get all meas begs/ends for this type
	for (int h = 0; h < combinedMeas.H.rows(); h++)
	{
		auto& obsKey = combinedMeas.obsKeys[h];

		string chunkId = obsKey.str;

		if (begH.find(chunkId) == begH.end())			{	begH[chunkId] = h;		}
														{	endH[chunkId] = h;		}

		for (int x = 0; x < kfState.x.rows(); x++)
		if (combinedMeas.H(h, x))
		{
			if (begX.find(chunkId) == begX.end())		{	begX[chunkId] = x;		}
			if (x > endX[chunkId]) 						{	endX[chunkId] = x;		}
		}
	}

	bool chunkX = true;

	//check for overlapping x entries
	for (auto& [str1, beg1]	: begX)
	for (auto& [str2, beg2]	: begX)
	{
		auto& end1 = endX[str1];

		if (str1 == str2)
		{
			continue;
		}

		auto& end2 = endX[str2];

		if	( (beg2 > beg1 && beg2 < end1)		// 2 starts in the middle of beg,end
			||(end2 > beg1 && end2 < end1))		// 2 ends   in the middle of beg,end
		{
			chunkX = false;
		}
	}

	for (auto& [str, dummy] : begH)
	{
		FilterChunk filterChunk;

		if (str.empty() == false)
		{
			auto& rec = receiverMap[str];

			traceList[str] = getTraceFile(rec);

			filterChunk.trace_ptr = &traceList[str];
		}
		else
		{
			filterChunk.trace_ptr = &trace;
		}

// 		std::cout << std::endl << "Chunk : " << str << " " << begH[str] << " " << endH[str] << " " << begX[str] << " " << endX[str];
							filterChunk.begH = begH[str];			filterChunk.numH = endH[str] - begH[str] + 1;
		if (chunkX)		{	filterChunk.begX = begX[str];			filterChunk.numX = endX[str] - begX[str] + 1;		}
		else			{	filterChunk.begX = 0;					filterChunk.numX = kfState.x.rows();				}

		filterChunkList.push_back(filterChunk);
	}

	std::sort(filterChunkList.begin(), filterChunkList.end(), [](FilterChunk& a, FilterChunk& b) {return a.begH < b.begH;});

	if (acsConfig.pppOpts.chunk_size)
	{
		Instrument	instrument("PPP chunksize");

		vector<FilterChunk> newFilterChunkList;
		FilterChunk	bigFilterChunk;

		int chunks		= (double) filterChunkList.size() / acsConfig.pppOpts.chunk_size	+ 0.5;
		int chunkTarget = -1;
		if (chunks)
			chunkTarget = (double) filterChunkList.size() / chunks							+ 0.5;

		int count = 0;
		for (auto& filterChunk : filterChunkList)
		{
			if (count == 0)
			{
				bigFilterChunk = filterChunk;
				bigFilterChunk.trace_ptr = &trace;
			}

			int chunkEndX = filterChunk.begX + filterChunk.numX - 1;
			int chunkEndH = filterChunk.begH + filterChunk.numH - 1;

			if (bigFilterChunk.begX							> filterChunk.begX)						{	bigFilterChunk.begX = filterChunk.begX;	}
			if (bigFilterChunk.begH							> filterChunk.begH)						{	bigFilterChunk.begH = filterChunk.begH;	}

			if (bigFilterChunk.begX + bigFilterChunk.numX	< filterChunk.begX + filterChunk.numX)	{	bigFilterChunk.numX = chunkEndX - bigFilterChunk.begX + 1;	}
			if (bigFilterChunk.begH + bigFilterChunk.numH	< filterChunk.begH + filterChunk.numH)	{	bigFilterChunk.numH = chunkEndH - bigFilterChunk.begH + 1;	}

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

void updatePseudoPulses(
	Trace&			trace,
	KFState&		kfState)
{
	int epochsPerDay = 86400 / acsConfig.epoch_interval;

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::ORBIT)
		{
			continue;
		}

// 	if (acsConfig.pseudoPulses.enable == false)
// 	{
// 		return;
// 	}			//todo aaron, add config


// 		if	((epoch - 1) % (epochsPerDay / acsConfig.pseudoPulses.num_per_day) == 0)
// 		{
// 			if (key.num < 3)	kfState.setExponentialNoise(key, {SQR(acsConfig.pseudoPulses.pos_proc_noise)});
// 			else				kfState.setExponentialNoise(key, {SQR(acsConfig.pseudoPulses.vel_proc_noise)});
// 		}
	}
}

void removeBadAmbiguities(
	Trace&			trace,
	KFState&		kfState,
	ReceiverMap&		receiverMap);

void removeBadIonospheres(
	Trace&			trace,
	KFState&		kfState);

void incrementOutageCount(
	ReceiverMap&		stations);

void checkOrbits(
	Trace&			trace,
	KFState&		kfState);

void PPP(
	Trace&			trace,			///< Trace to output to
	ReceiverMap&	receiverMap,		///< List of stations containing observations for this epoch
	KFState&		kfState,		///< Kalman filter object containing the network state parameters
	KFState&		remoteState)	///< Optional pointer to remote kalman filter
{
	Instrument	instrument(__FUNCTION__);

	{
		Instrument instrument("PPP pppre");

		removeBadAmbiguities(trace, kfState, receiverMap);
		removeBadIonospheres(trace, kfState);

		incrementOutageCount(receiverMap);

		updateRecClocks		(trace, receiverMap,			kfState);
		updateAvgClocks		(trace, 				tsync,	kfState);
		updateAvgIonosphere	(trace,					tsync,	kfState);
		updatePseudoPulses	(trace,							kfState);
	}

	//add process noise and dynamics to existing states as a prediction of current state
	if (kfState.assume_linearity == false)
	{
		Instrument instrument("PPP stateTransition1");

		BOOST_LOG_TRIVIAL(info) << " ------- DOING STATE TRANSITION       --------" << std::endl;

		kfState.stateTransition(trace, tsync);

// 		kfState.outputStates(trace, "/PREDICTED");
	}

	//prepare a map of lists of measurements for use below
	map<string, KFMeasEntryList> stationKFEntryListMap;
	for (auto& [id, rec] : receiverMap)
	{
		stationKFEntryListMap[rec.id] = KFMeasEntryList();
	}

	MatrixXd	R;
	MatrixXd*	R_ptr = nullptr;
// 	if (kfMeasEntryList.empty())
	{
// 		R_ptr = &R;
	}

// 	mongoReadFilter(remoteState, GTime::noTime(), {});

	{
		Instrument instrument("PPP obsOMC");

		BOOST_LOG_TRIVIAL(info) << " ------- CALCULATING PPP MEASUREMENTS --------" << std::endl;

		//calculate the measurements for each station
#		ifdef ENABLE_PARALLELISATION
			Eigen::setNbThreads(1);
#			pragma omp parallel for
#		endif
		for (int i = 0; i < receiverMap.size(); i++)
		{
			const KFState& constKfState = kfState;
			descope kfState;

			auto rec_iterator = receiverMap.begin();
			std::advance(rec_iterator, i);

			auto& [id, rec] = *rec_iterator;

			if (rec.obsList.empty())
			{
				continue;
			}

			auto& kfMeasEntryList = stationKFEntryListMap[rec.id];

			orbitPseudoObs		(trace,		rec,	constKfState, kfMeasEntryList);
			receiverPPP			(std::cout,	rec,	constKfState, kfMeasEntryList,	remoteState);
			receiverSlr			(std::cout, rec,	constKfState, kfMeasEntryList);
			receiverPseudoObs	(std::cout,	rec,	constKfState, kfMeasEntryList, receiverMap, R_ptr);

			if (acsConfig.pppOpts.ionoOpts.use_if_combo)	makeIFLCs(trace, constKfState, kfMeasEntryList);
		}
		Eigen::setNbThreads(0);
	}

	//combine all lists of measurements into a single list
	KFMeasEntryList kfMeasEntryList;
	for (auto& [rec, stationKFEntryList]	: stationKFEntryListMap)
	for (auto& kfMeasEntry					: stationKFEntryList)
	{
		if (kfMeasEntry.valid)
		{
			kfMeasEntryList.push_back(std::move(kfMeasEntry));
		}
	}

	// apply external estimates
	ionoPseudoObs			(trace,	receiverMap,	kfState,	kfMeasEntryList);
	tropPseudoObs			(trace, receiverMap,	kfState,	kfMeasEntryList);

	//apply pseudoobs to states available from before
	pseudoRecDcb			(trace,					kfState,	kfMeasEntryList);
	ambgPseudoObs			(trace,					kfState,	kfMeasEntryList);
	initPseudoObs			(trace,					kfState,	kfMeasEntryList);
	satClockPivotPseudoObs	(trace,					kfState,	kfMeasEntryList);
	//use state transition to initialise new state elements
	{
		Instrument	instrument("PPP stateTransition2");

		BOOST_LOG_TRIVIAL(info) << " ------- DOING STATE TRANSITION       --------" << std::endl;

		kfState.stateTransition(trace, tsync);

// 		kfState.outputStates(trace, "/INITIALISED");
	}


	KFMeas combinedMeas;
	{
		combinedMeas = kfState.combineKFMeasList(kfMeasEntryList, tsync, R_ptr);
	}

	if (acsConfig.pppOpts.ionoOpts	.use_gf_combo)	{	combinedMeas = makeGFLCs	(combinedMeas, kfState);	}
// 	if (acsConfig.pppOpts.ionoOpts	.use_if_combo)	{	combinedMeas = makeIFLCs	(combinedMeas, kfState);	}
	if (acsConfig.pppOpts			.use_rtk_combo)	{	combinedMeas = makeRTKLCs	(combinedMeas, kfState);	}

	if (acsConfig.explain_measurements)
	{
		explainMeasurements(trace, combinedMeas, kfState);
	}

	if (kfState.lsqRequired)
	{
		kfState.lsqRequired = false;
		BOOST_LOG_TRIVIAL(info) << "-------INITIALISING PPPPP USING LEAST SQUARES--------" << std::endl;

		VectorXd dx;
 		kfState.leastSquareInitStates(trace, combinedMeas, false, &dx, true);

		kfState.outputStates(trace, "/LSQ");
	}

	vector<FilterChunk>	filterChunkList;
	map<string, std::ofstream>	traceList;	//keep in large scope as we're using pointers

	chunkFilter(trace, kfState, combinedMeas, receiverMap, filterChunkList, traceList);


	BOOST_LOG_TRIVIAL(info) << " ------- DOING PPPPP KALMAN FILTER    --------" << std::endl;

	kfState.filterKalman(trace, combinedMeas, true, &filterChunkList);

	{
		Instrument	instrument("PPP postFilterChecks");

		postFilterChecks(combinedMeas);
	}

	//output chunks if we are actually chunking still
	if	( acsConfig.pppOpts.receiver_chunking
		||acsConfig.pppOpts.satellite_chunking)
	for (auto& filterChunk : filterChunkList)
	{
		kfState.outputStates(*filterChunk.trace_ptr, "/PPP", filterChunk.begX, filterChunk.numX);
	}

	kfState.outputStates(trace, "/PPP");

// 	propagateUncertainty(trace, kfState).outputStates(trace, "/PIVOT");
}




