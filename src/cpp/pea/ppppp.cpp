
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"


/** Primary estimation and filtering.
 *
 * While there are other auxiliary filters and states used within the Pea, all PPP processing flows through a common filtering stage.
 *
 * The residuals for all observations are first computed in an undifferenced-uncombined state, which leads to the greatest generality and extensibility.
 * As each receiver's observations are then independent of each other, these computations are computed in parallel, using openMP directives, increasing thoughput.
 *
 * Bookkeeping around the initialisation of state elements and their state transitions is taken care of automatically at the point they are first referenced in the observation equations,
 * using values as configured in the yaml file.
 *
 * The distinction between "Network" and "User" positioning modes that may be used in other software packages is not required in Ginan.
 * All receivers are always stored in a large single filter.
 * Depending on the configuration, the state and it's covariance matrix may turn out to be block-diagonal (user-mode), which will automatically be treated optimally upon the estimation stage by applying 'chunking'.
 *
 */
Architecture Main_Filter__()
{
	DOCS_REFERENCE(UDUC_GNSS_Measurements__);
	DOCS_REFERENCE(SLR_Mesaurements__);
	DOCS_REFERENCE(Combinators__);
	DOCS_REFERENCE(Pseudo_Observations__);
	DOCS_REFERENCE(Kalman_Filter__);
	DOCS_REFERENCE(Error_Handling__);
}

/**
 */
Architecture Combinators__()
{

}

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


#include "interactiveTerminal.hpp"
#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "coordinates.hpp"
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
	map<KFKey, int>&	indexMap;
	MatrixXd&			designMatrix;
};

void explainMeasurements(
	Trace&		trace,
	KFMeas&		kfMeas,
	KFState&	kfState)
{
	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		auto& obsKey		= kfMeas.obsKeys		[i];
		auto& metaDataMap	= kfMeas.metaDataMaps	[i];

		if (metaDataMap["explain"] == nullptr)
		{
			continue;
		}

		InteractiveTerminal output((string)"Partials/" + (string)obsKey, trace);

		output << "\n" << "============================";
		output << "\n" << "Explaining " << obsKey << " : " << obsKey.comment;

		for (auto duo :	{
							Duo{kfState	.kfIndexMap,		kfMeas.H},
							Duo{kfMeas	.noiseIndexMap,		kfMeas.H_star}
						})
		for (int col = 0; col < duo.designMatrix.cols(); col++)
		{
			double entry = duo.designMatrix(i, col);

			if (entry == 0)
			{
				continue;
			}

			for (auto& [kfKey, index] : duo.indexMap)
			{
				if (index == col)
				{
					output << "\n";
					if (traceLevel >= 4)
						output << kfState.time << " " << obsKey;

					output << kfKey << " : " << entry;
					break;
				}
			}
		}
		output << "\n";
	}
}

void alternatePostfits(
	Trace&		trace,
	KFMeas&		kfMeas,
	KFState&	kfState)
{
	for (auto& [kfKey, col] : kfMeas.noiseIndexMap)
	{
		if	( kfKey.type > KF::BEGIN_MEAS_STATES
			&&kfKey.type < KF::END_MEAS_STATES)
		{
			continue;
		}

		bool first = true;

		for (int row = 0; row < kfMeas.H_star.rows(); row++)
		{
			double& entry = kfMeas.H_star(row, col);

			if (entry == 0)
			{
				continue;
			}

			if (first)
			{
				first = false;

				trace << "\n" << "Removing " << kfKey << " from postfit residual calculations";
			}

			entry = 0;
		}
	}
}

void makeIFLCs(
	Trace&				trace,
	const KFState&		kfState,
	KFMeasEntryList&	kfMeasEntryList)
{
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

			for (auto& [key, valueI]	: kfMeasEntryI.noiseElementMap)		kfMeasEntryJ.noiseElementMap[key] = valueI;

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
			kfMeasEntryJ.componentsMap	= std::move(newComponentsMap);

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
KFMeas makeGFLCs(
	KFMeas&		kfMeas,
	KFState&	kfState)
{
	int meas = 0;

	vector<Triplet<double>>			tripletList;
	decltype(KFMeas::metaDataMaps)	newMetaDataMaps;
	decltype(KFMeas::obsKeys)		newObsKeys;

	for (auto duo :	{
						Duo{kfState	.kfIndexMap,	kfMeas.H},
						Duo{kfMeas	.noiseIndexMap,	kfMeas.H_star}
					})
	for (auto& [kfKey, index] : duo.indexMap)					{														if (kfKey.type != KF::IONO_STEC)	continue;
	for (int i_2 = 0; i_2 < kfMeas.obsKeys.size();	i_2++)		{	double coeff_2 = duo.designMatrix(i_2, index);		if (coeff_2 == 0)					continue;
	for (int i_1 = 0; i_1 < i_2;					i_1++)		{	double coeff_1 = duo.designMatrix(i_1, index);		if (coeff_1 == 0)					continue;
	{
		if (coeff_1 * coeff_2 < 0)								{	continue;	}
		if (coeff_1	== coeff_2)									{	continue;	}	//dont combine if it will eliminate the entire measurement

		if (kfMeas.metaDataMaps[i_1]["GFLCcombined"])			{	continue;	}
		if (kfMeas.metaDataMaps[i_2]["GFLCcombined"]) 			{	continue;	}

		//these measurements probably both share a common geometry, remove it.

		double scalar = 0.5;

		tripletList.push_back({meas, i_1, +1 * scalar});
		tripletList.push_back({meas, i_2, -1 * scalar});
		meas++;

		auto& obsKey_1 = kfMeas.obsKeys[i_1];
		auto& obsKey_2 = kfMeas.obsKeys[i_2];

		auto newObsKey = obsKey_2;

		newObsKey.num 		= 100 * obsKey_1.num
							+   1 * obsKey_2.num;

		newObsKey.comment	= obsKey_1.comment + "-"
							+ obsKey_2.comment;

		newObsKeys			.push_back(newObsKey);

		//copy metadata into the new measurement
		map<string, void*> newMetaData;
		for (auto& [id, value] : kfMeas.metaDataMaps[i_1])		{	newMetaData[id]				= value;		}
		for (auto& [id, value] : kfMeas.metaDataMaps[i_2])		{	newMetaData[id + "_alt"]	= value;		}
																	newMetaData["explain"]		= (void*) true;

		newMetaDataMaps		.push_back(std::move(newMetaData));

		kfMeas.metaDataMaps[i_1]["GFLCcombined"] = (void*) true;
		kfMeas.metaDataMaps[i_2]["GFLCcombined"] = (void*) true;
	}}}}

	if (meas == 0)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No IONO_STEC measurements found - 'use_gf_combo' requires 'iono_stec' estimation to be enabled in the config file.";
	}

	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		if (kfMeas.metaDataMaps[i]["pseudoObs"] == (void*) false)
		{
			continue;
		}

		//need to keep this measurement even if its not a valid ionospheric one, copy it over

		tripletList.push_back({meas, i, 1});
		meas++;

		newObsKeys		.push_back(kfMeas.obsKeys		[i]);
		newMetaDataMaps	.push_back(kfMeas.metaDataMaps	[i]);
	}

	return KFMeas(kfMeas, std::move(tripletList), std::move(newObsKeys), std::move(newMetaDataMaps));
}


/** Replace individual measurements with linear combinations
 */
KFMeas makeRTKLCs1(
	KFMeas&		kfMeas,
	KFState&	kfState)
{
	int meas = 0;

	vector<Triplet<double>>			tripletList;
	decltype(KFMeas::metaDataMaps)	newMetaDataMaps;
	decltype(KFMeas::obsKeys)		newObsKeys;

	for (auto duo :	{
						Duo{kfState	.kfIndexMap,	kfMeas.H},
						Duo{kfMeas	.noiseIndexMap,	kfMeas.H_star}
					})
	for (auto& [kfKey, index] : duo.indexMap)					{													if (kfKey.type != KF::PHASE_BIAS && kfKey.type != KF::CODE_BIAS)	continue;
	for (int i_2 = 0; i_2 < kfMeas.obsKeys.size();	i_2++)		{	double coeff_2 = duo.designMatrix(i_2, index);	if (coeff_2 == 0)													continue;
	for (int i_1 = 0; i_1 < i_2;					i_1++)		{	double coeff_1 = duo.designMatrix(i_1, index);	if (coeff_1 == 0)													continue;
	{
		if (kfMeas.metaDataMaps[i_1]["RTKcombined1"])			{	continue;	}
		if (kfMeas.metaDataMaps[i_2]["RTKcombined1"])			{	continue;	}

		auto& obsKey1 = kfMeas.obsKeys[i_1];
		auto& obsKey2 = kfMeas.obsKeys[i_2];

		if (kfKey.str.empty() == false)
		{
			continue;
		}

		if (obsKey1.str == obsKey2.str)
		{
			continue;
		}

		//these measurements both share a common satellite bias, remove it.

		// std::cout << kfKey << " " << kfMeas.obsKeys[i_2]  << " " << kfMeas.obsKeys[i_1] << "\n";

		tripletList.push_back({meas, i_1, +coeff_2});
		tripletList.push_back({meas, i_2, -coeff_1});
		meas++;

		auto& obsKey_1 = kfMeas.obsKeys[i_1];
		auto& obsKey_2 = kfMeas.obsKeys[i_2];

		auto newObsKey = obsKey_2;

		newObsKey.str.clear();

		newObsKey.comment	= (string) obsKey_1	+ "-"
							+ (string) obsKey_2	+ " "
							+ obsKey_1.comment	+ "-"
							+ obsKey_2.comment;

		newObsKeys		.push_back(newObsKey);

		//copy metadata into the new measurement
		map<string, void*> newMetaData;
		for (auto& [id, value] : kfMeas.metaDataMaps[i_1])		{	newMetaData[id]				= value;		}
		for (auto& [id, value] : kfMeas.metaDataMaps[i_2])		{	newMetaData[id + "_alt"]	= value;		}
																	newMetaData["explain"]		= (void*) true;

		newMetaDataMaps	.push_back(std::move(newMetaData));

		kfMeas.metaDataMaps[i_1]["RTKcombined"] = (void*) true;
		kfMeas.metaDataMaps[i_2]["RTKcombined"] = (void*) true;
	}}}}

	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		if (kfMeas.metaDataMaps[i]["pseudoObs"] == (void*) false)
		{
			continue;
		}

		//need to keep this measurement even if its not a valid rtk one, copy it over

		tripletList.push_back({meas, i, 1});
		meas++;

		newObsKeys		.push_back(kfMeas.obsKeys		[i]);
		newMetaDataMaps	.push_back(kfMeas.metaDataMaps	[i]);
	}

	return KFMeas(kfMeas, std::move(tripletList), std::move(newObsKeys), std::move(newMetaDataMaps));
}


/** Replace individual measurements with linear combinations
 */
KFMeas makeRTKLCs2(
	KFMeas&		kfMeas,
	KFState&	kfState)
{
	int meas = 0;

	vector<Triplet<double>>			tripletList;
	decltype(KFMeas::metaDataMaps)	newMetaDataMaps;
	decltype(KFMeas::obsKeys)		newObsKeys;

	for (auto duo :	{
						Duo{kfState	.kfIndexMap,	kfMeas.H},
						Duo{kfMeas	.noiseIndexMap,	kfMeas.H_star}
					})
	for (auto& [kfKey, index] : duo.indexMap)					{													if (kfKey.type != KF::PHASE_BIAS && kfKey.type != KF::CODE_BIAS)	continue;
	for (int i_2 = 0; i_2 < kfMeas.obsKeys.size();	i_2++)		{	double coeff_2 = duo.designMatrix(i_2, index);	if (coeff_2 == 0)													continue;
	for (int i_1 = 0; i_1 < i_2;					i_1++)		{	double coeff_1 = duo.designMatrix(i_1, index);	if (coeff_1 == 0)													continue;
	{
		// if (kfMeas.metaDataMaps[i_1]["RTKcombined2"])		{	continue;	}
		if (kfMeas.metaDataMaps[i_2]["RTKcombined2"])			{	continue;	}

		auto& obsKey1 = kfMeas.obsKeys[i_1];
		auto& obsKey2 = kfMeas.obsKeys[i_2];

		if (kfKey.str.empty())
		{
			continue;
		}

		if (obsKey1.Sat == obsKey2.Sat)
		{
			continue;
		}

		//these measurements both share a common receiver bias, remove it.

		tripletList.push_back({meas, i_1, +coeff_2});
		tripletList.push_back({meas, i_2, -coeff_1});
		meas++;

		auto& obsKey_1 = kfMeas.obsKeys[i_1];
		auto& obsKey_2 = kfMeas.obsKeys[i_2];

		auto newObsKey = obsKey_2;
		newObsKey.Sat = SatSys();

		newObsKey.comment	= (string) obsKey_1	+ "-"
							+ (string) obsKey_2	+ " "
							+ obsKey_1.comment	+ "-"
							+ obsKey_2.comment;

		newObsKeys			.push_back(newObsKey);

		//copy metadata into the new measurement
		map<string, void*> newMetaData;
		for (auto& [id, value] : kfMeas.metaDataMaps[i_1])		{	newMetaData[id]				= value;		}
		for (auto& [id, value] : kfMeas.metaDataMaps[i_2])		{	newMetaData[id + "_alt"]	= value;		}
																	newMetaData["explain"]		= (void*) true;

		newMetaDataMaps		.push_back(std::move(newMetaData));

		kfMeas.metaDataMaps[i_1]["RTKcombined2"] = (void*) true;
		kfMeas.metaDataMaps[i_2]["RTKcombined2"] = (void*) true;
	}}}}

	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		if (kfMeas.metaDataMaps[i]["pseudoObs"] == (void*) false)
		{
			continue;
		}

		//need to keep this measurement even if its not a valid rtk one, copy it over

		tripletList.push_back({meas, i, 1});
		meas++;

		newObsKeys			.push_back(kfMeas.obsKeys		[i]);
		newMetaDataMaps		.push_back(kfMeas.metaDataMaps	[i]);
	}

	return KFMeas(kfMeas, std::move(tripletList), std::move(newObsKeys), std::move(newMetaDataMaps));
}

/** Prepare receiver clocks using spp values to minimise pre-fit residuals
 */
void updateRecClocks(
	Trace&			trace,			///< Trace to output to
	ReceiverMap&	receiverMap,	///< List of stations containing observations for this epoch
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	if (acsConfig.adjust_rec_clocks_by_spp == false)
	{
		return;
	}

	for (auto& [id, rec] : receiverMap)
	{
		if (rec.isPseudoRec)
		{
			continue;
		}

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

		//for non-first epochs, and if enabled, do rounding
		if	(  rec.sol.dtRec_m_pppp_old[E_Sys::GPS]
			&& acsConfig.adjust_clocks_for_jumps_only)
		{
			const double scalar = 1000 * 2 / CLIGHT;

			double halfMilliseconds = C_dtRecAdj * scalar;

			C_dtRecAdj = round(halfMilliseconds) / scalar;

			if (C_dtRecAdj)
			{
				trace << "\n"
				<< "Jump of " << halfMilliseconds * 0.5 << "ms found, rounding";
			}
		}

		// if (C_dtRecAdj)
		{
			trace << "\n"
			<< "Adjusting " << clkKey.str
			<< " clock by " << C_dtRecAdj;
		}

		rec.sol.dtRec_m_pppp_old[E_Sys::GPS] = rec.sol.dtRec_m[E_Sys::GPS];

		kfState.setKFTrans(clkKey, KFState::oneKey, C_dtRecAdj, init);
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

		double alpha = 40.3e16 / SQR(CLIGHT / genericWavelength[F1]);

		double ionosphereStec = diono / alpha;

		//update the mu value but dont use the state thing - it will re-add it after its deleted
		// kfState.addKFState(key, init);
		kfState.gaussMarkovMuMap[key] = ionosphereStec;
	}
}

/** Prepare Satellite clocks to minimise residuals to broadcast clocks
 */
void updateAvgOrbits(
	Trace&			trace,			///< Trace to output to
	GTime			time,			///< Time
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	if (acsConfig.minimise_sat_orbit_offsets == false)
	{
		return;
	}

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type	!= KF::ORBIT
			||key.num	!= 0)
		{
			continue;
		}

		SatPos satPos;
		satPos.Sat			= key.Sat;
		satPos.satNav_ptr	= &nav.satNavMap[key.Sat];

		bool pass = satPosBroadcast(trace, time, time, satPos, nav);
		if (pass == false)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(key.Sat);

		satPos.rSatEci0 = frameSwapper(satPos.rSatCom);

		for (int i = 0; i < 3; i++)
		{
			InitialState init = initialStateFromConfig(satOpts.orbit, i);

			init.mu = satPos.rSatEci0(i);

			//update the mu value
			kfState.addKFState(key, init);
		}
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
			pivotRec = key.str;
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

	KFMeas kfMeas(kfState, kfMeasEntryList);

	propagatedState.time	= kfState.time;
	propagatedState.P		= kfMeas.H * kfState.P * kfMeas.H.transpose();
	propagatedState.x		= kfMeas.H * kfState.x;
	propagatedState.dx		= kfMeas.H * kfState.dx;

	propagatedState.kfIndexMap.clear();

	int i = 0;
	for (auto& obsKey : kfMeas.obsKeys)
	{
		propagatedState.kfIndexMap[obsKey] = i;
		i++;
	}

	return propagatedState;
}

void chunkFilter(
	Trace&						trace,
	KFState&					kfState,
	KFMeas&						kfMeas,
	ReceiverMap&				receiverMap,
	map<string, FilterChunk>&	filterChunkMap,
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

	for (auto& [kfKey, x] : kfState.kfIndexMap)
	{
		if (kfKey.type == KF::ONE)
		{
			continue;
		}

		string chunkId = kfKey.str;

		if (begX.find(chunkId) == begX.end())								{	begX[chunkId] = x;		}
																			{	endX[chunkId] = x;		}


		for (int h = 0; h < kfMeas.H.rows(); h++)
		if (kfMeas.H(h, x))
		{
			if (begH.find(chunkId) == begH.end() || h < begH[chunkId])		{	begH[chunkId] = h;		}
			if (									h > endH[chunkId]) 		{	endH[chunkId] = h;		}
		}
	}

	bool chunkX = true;
	bool chunkH = true;

	//check for overlapping entries
	for (auto& [str1, beg1]	: begH)
	for (auto& [str2, beg2]	: begH)
	{
		auto& end1 = endH[str1];

		if (str1 == str2)
		{
			continue;
		}

		auto& end2 = endH[str2];

		if	( (beg2 >= beg1 && beg2 <= end1)		// 2 starts in the middle of beg,end
			||(end2 >= beg1 && end2 <= end1))		// 2 ends   in the middle of beg,end
		{
			chunkH = false;
		}
	}

	if (chunkH == false)
	{
		return;
	}

	for (auto& [str, dummy] : begH)
	{
		FilterChunk filterChunk;

		if (str.empty() == false)
		{
			auto& rec = receiverMap[str];

			traceList[str] = getTraceFile(rec);

			filterChunk.id			= str;
			filterChunk.trace_ptr	= &traceList[str];
		}
		else
		{
			filterChunk.trace_ptr = &trace;
		}

// 		std::cout << "\n" << "Chunk : " << str << " " << begH[str] << " " << endH[str] << " " << begX[str] << " " << endX[str];
		if (chunkH)		{	filterChunk.begH = begH[str];			filterChunk.numH = endH[str] - begH[str] + 1;		}
		else			{	filterChunk.begH = 0;					filterChunk.numH = kfMeas.H.rows();					}
		if (chunkX)		{	filterChunk.begX = begX[str];			filterChunk.numX = endX[str] - begX[str] + 1;		}
		else			{	filterChunk.begX = 0;					filterChunk.numX = kfState.x.rows();				}

		filterChunkMap[str] = filterChunk;
	}

	//check all states are filtered despite not needing to be (required for rts)
	//assume all chunks are in receiver order == state order
	{
		int x = 0;
		map<string, FilterChunk> filterChunkExtras;

		auto addChunk = [&](int lastX, int nextX)
		{
			FilterChunk filterChunk;
			filterChunk.id		= "dummy " + std::to_string(x);
			filterChunk.begX	= lastX + 1;
			filterChunk.numX	= nextX - lastX - 1;
			filterChunk.begH	= 0;
			filterChunk.numH	= 0;

			filterChunkExtras[filterChunk.id] = filterChunk;
		};

		for (auto& [str, filterChunk] : filterChunkMap)
		{
			if (filterChunk.begX != x + 1)
			{
				addChunk(x, filterChunk.begX);
			}

			x = filterChunk.begX + filterChunk.numX - 1;
		}

		if (x != kfState.x.rows() - 1)
		{
			addChunk(x, kfState.x.rows());
		}

		for (auto& [id, fc] : filterChunkExtras)
		{
			filterChunkMap[id] = std::move(fc);
		}
	}

	if (acsConfig.pppOpts.chunk_size)
	{
		map<string, FilterChunk> newFilterChunkMap;

		FilterChunk	bigFilterChunk;

		int chunks		= (double) filterChunkMap.size() / acsConfig.pppOpts.chunk_size	+ 0.5;
		int chunkTarget = -1;
		if (chunks)
			chunkTarget = (double) filterChunkMap.size() / chunks						+ 0.5;

		int count = 0;
		for (auto& [id, filterChunk] : filterChunkMap)
		{
			if (count == 0)
			{
				bigFilterChunk = filterChunk;
				bigFilterChunk.trace_ptr = &trace;
			}
			else
			{
				bigFilterChunk.id += "-";
				bigFilterChunk.id += filterChunk.id;
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
				newFilterChunkMap[bigFilterChunk.id] = bigFilterChunk;
				count = 0;
			}
		}

		if (count)
		{
			newFilterChunkMap[bigFilterChunk.id] = bigFilterChunk;
		}

		filterChunkMap = std::move(newFilterChunkMap);
	}
}

void updatePseudoPulses(
	Trace&			trace,
	KFState&		kfState)
{
	static map<KFKey, double> nextEpochMap;

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::ORBIT)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(key.Sat);

		if	( satOpts.pseudoPulses.enable	== false
			||satOpts.pseudoPulses.interval	== 0)
		{
			continue;
		}

		auto& nextEpoch = nextEpochMap[key];

		if (epoch < nextEpoch)
		{
			continue;
		}

		double epochsPerInterval = satOpts.pseudoPulses.interval / acsConfig.epoch_interval;

		if (nextEpoch == 0)
		{
			nextEpoch = epochsPerInterval + 1;
		}

		while (epoch >= nextEpoch)
		{
			nextEpoch += satOpts.pseudoPulses.interval / acsConfig.epoch_interval;
		}

		if (key.num < 3)	kfState.setExponentialNoise(key, {SQR(satOpts.pseudoPulses.pos_proc_noise)});
		else				kfState.setExponentialNoise(key, {SQR(satOpts.pseudoPulses.vel_proc_noise)});
	}
}

void updateNukeFilter(
	Trace&		trace,
	KFState&	kfState)
{
	if	( acsConfig.pppOpts.nuke_enable		== false
		||acsConfig.pppOpts.nuke_interval	== 0)
	{
		return;
	}

	static double epochsPerInterval	= acsConfig.pppOpts.nuke_interval / acsConfig.epoch_interval;
	static double nukeEpoch			= epochsPerInterval + 1;

	if (epoch < nukeEpoch)
	{
		return;
	}

	while (epoch >= nukeEpoch)
	{
		nukeEpoch += epochsPerInterval;
	}

	auto& nuke_states = acsConfig.pppOpts.nuke_states;

	bool foundAll = (std::find(nuke_states.begin(), nuke_states.end(), +KF::ALL) != nuke_states.end());

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type == KF::ONE)
		{
			continue;
		}

		if	( foundAll
			||std::find(nuke_states.begin(), nuke_states.end(), key.type) != nuke_states.end())
		{
			trace << "\n" << "State removed due to nuclear config: " << key;
			kfState.removeState(key);
		}
	}
}

void removeBadAmbiguities(
	Trace&			trace,
	KFState&		kfState,
	ReceiverMap&	receiverMap);

void removeBadReceivers(
	Trace&			trace,
	KFState&		kfState,
	ReceiverMap&	receiverMap);

void removeBadIonospheres(
	Trace&			trace,
	KFState&		kfState);

void checkOrbits(
	Trace&			trace,
	KFState&		kfState);

void updateFilter(
	Trace&			trace,			///< Trace to output to
	ReceiverMap&	receiverMap,	///< List of receivers containing observations for this epoch
	KFState&		kfState)		///< Kalman filter object containing the network state parameters
{
	removeBadReceivers	(trace, kfState, receiverMap);
	removeBadAmbiguities(trace, kfState, receiverMap);
	removeBadIonospheres(trace, kfState);

	updateNukeFilter	(trace,							kfState);
	updateRecClocks		(trace, receiverMap,			kfState);
	updateAvgClocks		(trace, 				tsync,	kfState);
	updateAvgOrbits		(trace, 				tsync,	kfState);
	updateAvgIonosphere	(trace,					tsync,	kfState);
	updatePseudoPulses	(trace,							kfState);
}

void perRecMeasurements(
	Trace&				trace,
	Receiver&			rec,
	ReceiverMap&		receiverMap,
	KFMeasEntryList&	kfMeasEntryList,
	const KFState&		kfState,
	const KFState&		remoteState)
{
	rec.pppTideCache.uninit();
	rec.pppEopCache	.uninit();

	orbitPseudoObs		(trace,	rec,	kfState, kfMeasEntryList);
	receiverUducGnss	(trace,	rec,	kfState, kfMeasEntryList,	remoteState);
	receiverSlr			(trace, rec,	kfState, kfMeasEntryList);
	receiverPseudoObs	(trace,	rec,	kfState, kfMeasEntryList,	receiverMap);

	if (acsConfig.pppOpts.ionoOpts.use_if_combo)		makeIFLCs(trace, kfState, kfMeasEntryList);
}

void pppLinearCombinations(
	KFMeas&		kfMeas,
	KFState&	kfState)
{
	if (acsConfig.pppOpts.ionoOpts	.use_gf_combo)	{	kfMeas = makeGFLCs	(kfMeas, kfState);	}
	if (acsConfig.pppOpts			.use_rtk_combo)	{	kfMeas = makeRTKLCs1(kfMeas, kfState);	}
	if (acsConfig.pppOpts			.use_rtk_combo)	{	kfMeas = makeRTKLCs2(kfMeas, kfState);	}
}

void pppPseudoObs(
	Trace&				trace,
	ReceiverMap&		receiverMap,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList)
{
	DOCS_REFERENCE(Pseudo_Observations__);

	// apply external estimates
	ionoPseudoObs			(trace,	receiverMap,	kfState,	kfMeasEntryList);
	tropPseudoObs			(trace, receiverMap,	kfState,	kfMeasEntryList);

	//apply pseudoobs to states available from before
	pseudoRecDcb			(trace,					kfState,	kfMeasEntryList);
	ambgPseudoObs			(trace,					kfState,	kfMeasEntryList);
	initPseudoObs			(trace,					kfState,	kfMeasEntryList);
	satClockPivotPseudoObs	(trace,					kfState,	kfMeasEntryList);
	filterPseudoObs			(trace,					kfState,	kfMeasEntryList);
}

void ppp(
	Trace&			trace,			///< Trace to output to
	ReceiverMap&	receiverMap,	///< List of receivers containing observations for this epoch
	KFState&		kfState,		///< Kalman filter object containing the network state parameters
	KFState&		remoteState)	///< Optional pointer to remote kalman filter
{
	DOCS_REFERENCE(Main_Filter__);

	updateFilter(trace, receiverMap, kfState);

	//add process noise and dynamics to existing states as a prediction of current state
	if (kfState.assume_linearity == false)
	{
		InteractiveTerminal::setMode(E_InteractiveMode::StateTransition1);

		BOOST_LOG_TRIVIAL(info) << " ------- DOING STATE TRANSITION       --------" << "\n";

		kfState.stateTransition(trace, tsync);

		if (acsConfig.output_predicted_states)
		{
			kfState.outputStates(trace, "/PREDICTED");

			mongoStates(kfState,
						{
							.suffix		= "/PREDICTED",
							.instances	= acsConfig.mongoOpts.output_states,
							.queue		= acsConfig.mongoOpts.queue_outputs
						});
		}
	}

	//prepare a map of lists of measurements for use below
	map<string, KFMeasEntryList> stationKFEntryListMap;
	for (auto& [id, rec] : receiverMap)
	{
		stationKFEntryListMap[rec.id] = KFMeasEntryList();
	}

	{
		InteractiveTerminal::setMode(E_InteractiveMode::OMCCalculations);

		BOOST_LOG_TRIVIAL(info) << " ------- CALCULATING PPP MEASUREMENTS --------" << "\n";

		//calculate the measurements for each station
#		ifdef ENABLE_PARALLELISATION
			Eigen::setNbThreads(1);
#			pragma omp parallel for
#		endif
		for (int i = 0; i < receiverMap.size(); i++)
		{
			auto rec_iterator = receiverMap.begin();
			std::advance(rec_iterator, i);

			auto& [id, rec] = *rec_iterator;

			if	( 0
				// rec.ready == false
				||rec.obsList.empty())
			{
				continue;
			}

			auto& kfMeasEntryList = stationKFEntryListMap[rec.id];

			perRecMeasurements(trace, rec, receiverMap, kfMeasEntryList, kfState, remoteState);
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

	pppPseudoObs(trace, receiverMap, kfState, kfMeasEntryList);

	//use state transition to initialise new state elements
	InteractiveTerminal::setMode(E_InteractiveMode::StateTransition2);

	BOOST_LOG_TRIVIAL(info) << " ------- DOING STATE TRANSITION       --------" << "\n";

	kfState.stateTransition(trace, tsync);

	if (acsConfig.output_initialised_states)
	{
		kfState.outputStates(trace, "/INITIALISED");

		mongoStates(kfState,
					{
						.suffix		= "/INITIALISED",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	std::sort(kfMeasEntryList.begin(), kfMeasEntryList.end(), [](KFMeasEntry& a, KFMeasEntry& b) {return a.obsKey < b.obsKey;});

	KFMeas kfMeas(kfState, kfMeasEntryList, tsync);

	pppLinearCombinations(kfMeas, kfState);

	if (acsConfig.explain_measurements)
	{
		explainMeasurements(trace, kfMeas, kfState);
	}

	alternatePostfits(trace, kfMeas, kfState);

	if (kfState.lsqRequired)
	{
		BOOST_LOG_TRIVIAL(info) << "-------INITIALISING PPPPP USING LEAST SQUARES--------" << "\n";

		VectorXd dx;
 		kfState.leastSquareInitStates(trace, kfMeas, false, &dx, true);

		kfState.outputStates(trace, "/LSQ");
	}

	map<string, FilterChunk>	filterChunkMap;
	map<string, std::ofstream>	traceList;	//keep in large scope as we're using pointers

	chunkFilter(trace, kfState, kfMeas, receiverMap, filterChunkMap, traceList);


	InteractiveTerminal::setMode(E_InteractiveMode::Filtering);
	BOOST_LOG_TRIVIAL(info) << " ------- DOING PPPPP KALMAN FILTER    --------" << "\n";

	kfState.filterKalman(trace, kfMeas, "/PPP", true, &filterChunkMap);

	postFilterChecks(tsync, kfMeas);

	//output chunks if we are actually chunking still
	if	( acsConfig.pppOpts.receiver_chunking
		||acsConfig.pppOpts.satellite_chunking)
	for (auto& [id, filterChunk] : filterChunkMap)
	{
		if (filterChunk.trace_ptr)
		{
			kfState.outputStates(*filterChunk.trace_ptr, (string)"/PPPChunk/" + id, filterChunk.begX, filterChunk.numX);
		}
	}

	kfState.outputStates(trace, "/PPP");
}




