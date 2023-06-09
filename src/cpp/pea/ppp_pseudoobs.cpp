
// #pragma GCC optimize ("O0")


#include "minimumConstraints.hpp"
#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "instrument.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "ionoModel.hpp"
#include "constants.hpp"
#include "orbitProp.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "sinex.hpp"

#define PIVOT_MEAS_VARIANCE 	SQR(1E-5)

map<SatSys, double> expNoiseMap;

struct AvBiasMonitor
{
	int					numCode = 0;
	int					numPhas = 0;
	double				sumCode = 0;
	double				sumPhas = 0;
	map<SatSys, double>	pastCode;
	map<SatSys, double>	pastPhas;
};

map<E_Sys, map<E_ObsCode, AvBiasMonitor>> avBiasMap;

typedef map<E_Sys,map<int,bool>> RecSetList;

#define COMMON_ARG(type)    type

#define COMMON_PSEUDO_ARGS											\
	COMMON_ARG(		Trace&					)	trace,				\
	COMMON_ARG(		KFState&				)	kfState,			\
	COMMON_ARG(		KFMeasEntryList&		)	kfMeasEntryList,	\
	COMMON_ARG(		RecSetList&				)	recSetList

void initPseudoObs(
			Trace&				trace,				///< Trace to output to
			KFState&			kfState,			///< Kalman filter object containing the network state parameters
			KFMeasEntryList&	kfMeasEntryList)	///< List to append kf measurements to
{	
	if	( kfMeasEntryList.empty() == false
		||epoch != 1)
	{
		return;
	}
	
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (acsConfig.process_sys[Sat.sys] == false)
		{
			continue;
		}
		
		auto& satOpts = acsConfig.getSatOpts(Sat);
		
		if (satOpts.exclude)
		{
			continue;
		}
		
		
		bool newState = false;
		
		for (int i = 0; i < 3; i++)
		{
			InitialState posInit = initialStateFromConfig(satOpts.orbit, i);
			InitialState velInit = initialStateFromConfig(satOpts.orbit, i + 3);
			
			if (posInit.estimate == false)
			{
				continue;
			}
			
			KFKey satPosKey;
			KFKey satVelKey;
			
			satPosKey.type	= KF::ORBIT;
			satPosKey.Sat	= Sat;
			satPosKey.num	= i;
			
			satVelKey.type	= KF::ORBIT;
			satVelKey.Sat	= Sat;
			satVelKey.num	= i + 3;
		
			newState |= kfState.addKFState(satPosKey, posInit);
			newState |= kfState.addKFState(satVelKey, velInit);
			
			kfState.setKFTransRate(satPosKey, satVelKey,	1,	velInit);
		}

		if (newState == false)
		{
			continue;
		}
		
		addKFSatEMPStates(satOpts.emp_dyb_0,	kfState,	KF::EMP_DYB_0,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_1c,	kfState,	KF::EMP_DYB_1C,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_1s,	kfState,	KF::EMP_DYB_1S,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_2c,	kfState,	KF::EMP_DYB_2C,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_2s,	kfState,	KF::EMP_DYB_2S,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_3c,	kfState,	KF::EMP_DYB_3C,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_3s,	kfState,	KF::EMP_DYB_3S,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_4c,	kfState,	KF::EMP_DYB_4C,	Sat);
		addKFSatEMPStates(satOpts.emp_dyb_4s,	kfState,	KF::EMP_DYB_4S,	Sat);
		
		addKFSatEMPStates(satOpts.srp_dyb_0,	kfState,	KF::SRP_DYB_0,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_1c,	kfState,	KF::SRP_DYB_1C,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_1s,	kfState,	KF::SRP_DYB_1S,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_2c,	kfState,	KF::SRP_DYB_2C,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_2s,	kfState,	KF::SRP_DYB_2S,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_3c,	kfState,	KF::SRP_DYB_3C,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_3s,	kfState,	KF::SRP_DYB_3S,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_4c,	kfState,	KF::SRP_DYB_4C,	Sat);
		addKFSatEMPStates(satOpts.srp_dyb_4s,	kfState,	KF::SRP_DYB_4S,	Sat);
	}
}

void orbitPseudoObs(
			Trace&				trace,				///< Trace to output to
			Station&			rec,				///< Receiver to perform calculations for
	const	KFState&			kfState,			///< Kalman filter object containing the network state parameters
			KFMeasEntryList&	kfMeasEntryList)	///< List to append kf measurements to
{	
	GTime time = rec.obsList.front()->time;
	
	ERPValues erpv = geterp(nav.erp, time);
	
	FrameSwapper frameSwapper(time, erpv);
	
	for (auto& obs : only<PObs>(rec.obsList))
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			continue;
		}
		
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		if (satOpts.exclude)
		{
			continue;
		}
		
		VectorEci rSatEci;
		VectorEci vSatEci;
		
		if (acsConfig.orbitPropagation.itrf_pseudoobs)
		{
			if (obs.vel.isZero())
			{
				SatPos satPos;
				satPos.Sat = obs.Sat;
				
				satpos(trace, time, time, satPos, {E_Source::PRECISE}, E_OffsetType::COM, nav);
				
				obs.vel = satPos.satVel;
			}
			
			//Get framed vectors because obs is undefined
			VectorEcef rSat = obs.pos;
			VectorEcef vSat = obs.vel;
			
			rSatEci	= frameSwapper(rSat, &vSat, &vSatEci);
		}
		else
		{
			rSatEci = obs.pos;
			vSatEci = obs.vel;
		}

		KFKey satPosKeys[3];
		KFKey satVelKeys[3];
		for (int i = 0; i < 3; i++)
		{
			satPosKeys[i].type	= KF::ORBIT;
			satPosKeys[i].Sat	= obs.Sat;
			satPosKeys[i].num	= i;
			
			satVelKeys[i].type	= KF::ORBIT;
			satVelKeys[i].Sat	= obs.Sat;
			satVelKeys[i].num	= i + 3;
		}
		
	
		for (int i = 0; i < 3; i++)
		{
			InitialState posInit = initialStateFromConfig(satOpts.orbit, i);
			InitialState velInit = initialStateFromConfig(satOpts.orbit, i + 3);
			if (posInit.estimate)
			{
				VectorEci statePosEci = rSatEci;
				VectorEci stateVelEci = vSatEci;
				
				KFMeasEntry kfMeasEntry(&kfState);
					
				kfState.getKFValue(satPosKeys[i], statePosEci[i]);
				kfState.getKFValue(satVelKeys[i], stateVelEci[i]);
				
				if (posInit.x == 0)		posInit.x = rSatEci[i];
				if (velInit.x == 0)		velInit.x = vSatEci[i];
				
				auto& expNoise = expNoiseMap[obs.Sat];
				if (expNoise)
				{
					trace  
					<< std::endl << "adding : " << expNoise << " to process noise for " << obs.Sat.id() << " \n";
				}
				
				velInit.Q += 1e-20;
				velInit.Q += expNoise;
				
				if (i == 2)
				{
					//shrink the exponential process noise the next time around	//todo aaron, need to copy to real obs
					expNoise *= acsConfig.orbit_vel_proc_noise_trail_tau;
				}
				
				bool newState = false;
				newState |= kfState.addKFState(satPosKeys[i], posInit);
				newState |= kfState.addKFState(satVelKeys[i], velInit);
				
				if (newState)
				{
					statePosEci[i] = posInit.x;
				}
				
				kfMeasEntry.addDsgnEntry(satPosKeys[i], 1, posInit);
				
					
				kfState.setKFTransRate(satPosKeys[i], satVelKeys[i],	1,	velInit);
					
				double omc	= rSatEci[i] 
							- statePosEci[i];

				kfMeasEntry.setInnov(omc);
					
				kfMeasEntry.obsKey.comment	= "ECI PseudoPos";
				kfMeasEntry.obsKey.type		= KF::ORBIT;
				kfMeasEntry.obsKey.Sat		= obs.Sat;
				kfMeasEntry.obsKey.num		= i;
				kfMeasEntry.metaDataMap["pseudoObs"] = (void*) true;
					
				kfMeasEntry.addNoiseEntry(kfMeasEntry.obsKey, 1, SQR(satOpts.pseudo_sigmas[0]));
				
				kfMeasEntryList.push_back(kfMeasEntry);
			}
		}

		addKFSatEMPStates(satOpts.emp_dyb_0,	kfState,	KF::EMP_DYB_0,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_1c,	kfState,	KF::EMP_DYB_1C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_1s,	kfState,	KF::EMP_DYB_1S,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_2c,	kfState,	KF::EMP_DYB_2C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_2s,	kfState,	KF::EMP_DYB_2S,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_3c,	kfState,	KF::EMP_DYB_3C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_3s,	kfState,	KF::EMP_DYB_3S,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_4c,	kfState,	KF::EMP_DYB_4C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_4s,	kfState,	KF::EMP_DYB_4S,	obs.Sat);
		
		addKFSatEMPStates(satOpts.srp_dyb_0,	kfState,	KF::SRP_DYB_0,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_1c,	kfState,	KF::SRP_DYB_1C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_1s,	kfState,	KF::SRP_DYB_1S,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_2c,	kfState,	KF::SRP_DYB_2C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_2s,	kfState,	KF::SRP_DYB_2S,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_3c,	kfState,	KF::SRP_DYB_3C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_3s,	kfState,	KF::SRP_DYB_3S,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_4c,	kfState,	KF::SRP_DYB_4C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_4s,	kfState,	KF::SRP_DYB_4S,	obs.Sat);
	}
}

void stationPseudoObs(
			Trace&				trace,				///< Trace to output to
			Station&			rec,				///< Receiver to perform calculations for                //todo aaron, this isnt a rec anymore
	const	KFState&			kfState,			///< Kalman filter object containing the network state parameters
			KFMeasEntryList&	kfMeasEntryList,	///< List to append kf measurements to
			StationMap&			stationMap,
			MatrixXd*			R_ptr)				///< Optional pointer to measurement noise matrix
{	
	GTime time = rec.obsList.front()->time;
	
	vector<int>	indices;
	
	for (auto& obs			: only<FObs>(rec.obsList))
	{
		for (auto& [key, index]	: obs.obsState.kfIndexMap)
		{
			if	( key.type	!= KF::REC_POS
				||key.num	!= 0)
			{
				continue;
			}
			
			auto& rec = *key.rec_ptr;
			//try to get apriori from the existing state, otherwise use sinex.
			
			Vector3d apriori = Vector3d::Zero();
			
			bool found = true;
			for (int i = 0; i < 3; i++)
			{
				KFKey posKey = key;
				posKey.num = i;
				
				found &= kfState.getKFValue(posKey, apriori(i));
			}
			
			//make sure this receiver is initialised since this might be the first time anyone has seen it
			
// 			if (found == false)
			{
				rec.id = key.str;
				getStnSnx(rec.id, obs.time, rec.snx);
				apriori = rec.snx.pos;
			}
			
			rec.minconApriori = apriori;
		}
		
		obs.obsState.time = time;
		
		mincon(trace, obs.obsState);
		
		for (auto& [key, index]	: obs.obsState.kfIndexMap)
		{
			auto& recOpts = acsConfig.getRecOpts(key.str);
			
			if (key.type != KF::REC_POS)
			{
				continue;
			}
			
			KFKey kfKey = key;
				
			auto& rec = stationMap[key.str];
			kfKey.rec_ptr = &rec;
			
			InitialState posInit = initialStateFromConfig(recOpts.pos, kfKey.num);
			if (posInit.estimate == false)
			{
				continue;
			}
			
			double obsX = obs.obsState.x[index];
			
			KFMeasEntry kfMeasEntry(&kfState, kfKey);
			
			double stateX = obsX;
			kfState.getKFValue(kfKey, stateX);
				
			posInit.x = obsX;
				
			kfMeasEntry.addDsgnEntry(kfKey, 1, posInit);
				
				
			InitialState velInit = initialStateFromConfig(recOpts.strain_rate,	kfKey.num);
			if (velInit.estimate)
			{
				KFKey velKey	= kfKey;
				velKey.type		= KF::STRAIN_RATE;
				velKey.comment	= "mm/year";
				
				kfState.setKFTransRate(kfKey, velKey,	1/(365.25*24*60*60*1e3),	velInit);
			}
				
			double omc	= obsX
						- stateX;
						
			kfMeasEntry.setInnov(omc);
			kfMeasEntry.setNoise(obs.obsState.P(index, index));
			
			indices.push_back(index);
				
			kfMeasEntryList.push_back(kfMeasEntry);

			if (R_ptr)
			{
				auto& R = *R_ptr;
				
				R = obs.obsState.P(indices, indices);
			}
		}
	}
}

void pseudoCommonSatBias(COMMON_PSEUDO_ARGS)
{
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type != KF::PHASE_BIAS
			||key.str.empty())
		{
			continue;
		}
		
		E_Sys		sys		= key.Sat.sys;
		E_ObsCode	code1	= E_ObsCode::_from_integral(key.num);
		E_FType		freq1	= code2Freq[sys][code1];
		E_ObsCode	code0	= freq2CodeHax(sys, freq1);
		
		if	( acsConfig.one_phase_bias[sys] == false
			||code0 == +E_ObsCode::NONE
			||code0 == code1)
		{
			continue;
		}
		
		KFKey key0 = key;
		key0.num = code0;
		
		double	bias0;
		double	bvar0;
		double	bias1;
		double	bvar1;
		bool	found = true;
		found &= kfState.getKFValue(key0, bias0, &bvar0);
		found &= kfState.getKFValue(key,  bias1, &bvar1); 
		
		if (found == false)
			continue;
		
		if	( (bvar0 < acsConfig.fixed_phase_bias_var && bvar1 > 2 * acsConfig.fixed_phase_bias_var)
			||(bvar1 < acsConfig.fixed_phase_bias_var && bvar0 > 2 * acsConfig.fixed_phase_bias_var))
		{
			KFMeasEntry measEntry(&kfState);
			measEntry.obsKey.type		= KF::PHASE_BIAS;
			measEntry.obsKey.Sat		= SatSys(sys);
			measEntry.obsKey.num		= (int) freq1 + 100;
			measEntry.obsKey.comment	= "Common phase bias";
			
			measEntry.metaDataMap["pseudoObs"]	= (void*) true;
//			measEntry.metaDataMap["explain"]	= (void*) true;
			
			measEntry.addDsgnEntry(key,		+1);
			measEntry.addDsgnEntry(key0,	-1);
		
			measEntry.setInnov(bias0 - bias1);
			
			measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
			
			kfMeasEntryList.push_back(measEntry);
		}
	}
}

void pseudoRecDcb(COMMON_PSEUDO_ARGS)
{
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		E_Sys sys = key.Sat.sys;

		if	( key.str.empty()
			||key.type		!= KF::CODE_BIAS
			||key.rec_ptr	== nullptr)
		{
			continue;
		}
		
		auto& rec = *key.rec_ptr;
		
		auto& [code1, code2] = rec.recClockCodes[sys];

		if (key.num != code1)
		{
			continue;
		}
		
		KFKey kfKey1 = key;
		kfKey1.num = code1;
		
		KFKey kfKey2 = key;
		kfKey2.num = code2;

		double bias1 = 0;
		double bias2 = 0;
		bool found = true;
		found &= kfState.getKFValue(kfKey1, bias1);
		found &= kfState.getKFValue(kfKey2, bias2);
		
		if (found == false)
			continue;
		
		int codeSet	= code1 * 100
					+ code2;
		
		recSetList[sys][codeSet] = true;
		
		// Receiver DCB pseudoobs
		if (acsConfig.zero_receiver_dcb[sys])
		{
			KFMeasEntry measEntry(&kfState);
			measEntry.obsKey.type		= KF::DCB;
			measEntry.obsKey.Sat		= key.Sat;
			measEntry.obsKey.str		= key.str;
			measEntry.obsKey.num		= codeSet;
			measEntry.obsKey.comment	= "DCB - Rec";
			
			measEntry.metaDataMap["pseudoObs"]	= (void*) true;
//			measEntry.metaDataMap["explain"]	= (void*) true;
			
			measEntry.addDsgnEntry(kfKey1, +1);
			measEntry.addDsgnEntry(kfKey2, -1);
		
			measEntry.setInnov(bias2 - bias1);
			
			measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
			
			kfMeasEntryList.push_back(measEntry);
		}
		
		// Receiver clock definition
		if (acsConfig.receiver_reference_clk == key.Sat.sys)
		{
			E_FType	ft1		= code2Freq			[sys][code1];
			E_FType	ft2		= code2Freq			[sys][code2];
			double	lam1	= genericWavelength	[ft1];
			double	lam2	= genericWavelength	[ft2];
			
			if	( lam1 == 0
				||lam2 == 0)
			{
				continue;
			}
			
			tracepdeex (4, trace, "\nReceiver clock is %s clock, %.4f %.4f", sys._to_string(), lam1, lam2);
			
			double C1	= SQR(lam2) / (SQR(lam2) - SQR(lam1));
			double C2	= 1 - C1;
			
			KFMeasEntry measEntry(&kfState);
			measEntry.obsKey.type		= KF::CODE_BIAS;
			measEntry.obsKey.Sat		= key.Sat;
			measEntry.obsKey.str		= key.str;
			measEntry.obsKey.num		= codeSet;
			measEntry.obsKey.comment	= "Rec clk definition";
			
			measEntry.metaDataMap["pseudoObs"]	= (void*) true;
			measEntry.metaDataMap["explain"]	= (void*) true;
		
			measEntry.addDsgnEntry(kfKey1, C1);
			measEntry.addDsgnEntry(kfKey2, C2);
		
			measEntry.setInnov(-C1 * bias1 - C2 * bias2);
			
			measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
			
			kfMeasEntryList.push_back(measEntry);
		}
	}
}
	
void pseudoSatClockDefinition(COMMON_PSEUDO_ARGS)
{
	if (acsConfig.sat_clk_definition)
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		E_Sys sys = key.Sat.sys;
	
		// Satellite clock definition 
		E_ObsCode code1 = acsConfig.clock_codesL1[sys];
		E_ObsCode code2 = acsConfig.clock_codesL2[sys];
		
		if	( key.str.empty()	== false
			||key.type			!= KF::CODE_BIAS
			||key.num			!= code1)
		{
			continue;
		}
		
		KFKey satKey;
		satKey.type	= KF::SAT_CLOCK;
		satKey.Sat	= key.Sat;
		
		KFKey kfKey1 = key;
		kfKey1.num = code1;
		
		KFKey kfKey2 = key;
		kfKey2.num = code2;
		
		double satClk	= 0;
		double bias1	= 0;
		double bias2	= 0;
		
		bool found = true;
		found &= kfState.getKFValue(satKey, satClk);
		found &= kfState.getKFValue(kfKey1, bias1);
		found &= kfState.getKFValue(kfKey2, bias2);
		
		if (found == false)
			continue;
		
		E_FType ft1 = code2Freq[sys][code1];
		E_FType ft2 = code2Freq[sys][code2];
			
		double lam1	= nav.satNavMap[key.Sat].lamMap[ft1];
		double lam2	= nav.satNavMap[key.Sat].lamMap[ft2];
			
		if	( lam1 == 0
			||lam2 == 0)
		{
			continue;
		}
			
		double C1	= SQR(lam2) / (SQR(lam2) - SQR(lam1));
		double C2	= 1 - C1;
			
		KFMeasEntry measEntry(&kfState);
		measEntry.obsKey.type		= KF::CODE_BIAS;
		measEntry.obsKey.Sat		= key.Sat;
		measEntry.obsKey.str		= key.str;
		measEntry.obsKey.comment	= "Sat clk definition";
		
		measEntry.metaDataMap["pseudoObs"]	= (void*) true;
//		measEntry.metaDataMap["explain"]	= (void*) true;
		
		measEntry.addDsgnEntry(kfKey1, C1);
		measEntry.addDsgnEntry(kfKey2, C2);
		
		measEntry.setInnov(-C1 * bias1 - C2 * bias2);
			
		measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
			
		kfMeasEntryList.push_back(measEntry);
	}
}
	
void pseudoSatDCBPseudoObs(COMMON_PSEUDO_ARGS)
{
	for (auto& [sys,	process]	: acsConfig.process_sys)
	for (auto& [codi,	set]		: recSetList[sys])
	{
		if	( process == false
			||acsConfig.zero_satellite_dcb[sys] == false)
		{
			continue;
		}
		
		KFKey key1;
		key1.type	= KF::CODE_BIAS;
		key1.num	= codi / 100;
		
		KFKey key2;
		key2.type	= KF::CODE_BIAS;
		key2.num	= codi % 100;
		
		for (auto& Sat : getSysSats(sys))
		{
			key1.Sat = Sat;
			key2.Sat = Sat;
			
			double	bias1 = 0;
			double	bias2 = 0;
			bool	found = true;
			found &= kfState.getKFValue(key1, bias1);
			found &= kfState.getKFValue(key2, bias2);
			
			if (found == false)
				continue;
			
			KFMeasEntry measEntry(&kfState);
			measEntry.obsKey.type		= KF::DCB;
			measEntry.obsKey.Sat		= Sat;
			measEntry.obsKey.num		= codi;
			measEntry.obsKey.comment	= "DCB - Sat";
			
			measEntry.metaDataMap["pseudoObs"]	= (void*) true;
//			measEntry.metaDataMap["explain"]	= (void*) true;
		
			measEntry.addDsgnEntry(key1, +1);
			measEntry.addDsgnEntry(key2, -1);
		
			measEntry.setInnov(bias2 - bias1);
			
			measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
			
			kfMeasEntryList.push_back(measEntry);
		}
	}
}
	
void pseudoAvSatCodeBias(COMMON_PSEUDO_ARGS)
{
	for (auto& [sys, process]	: acsConfig.process_sys)
	for (auto& code				: acsConfig.zero_code_average[sys])
	{
		if (process == false)
		{
			continue;
		}
		
		auto& avBias = avBiasMap[sys][code];
		
		KFKey baseKey;
		baseKey.type	= KF::CODE_BIAS;
		baseKey.num	 	= code;
		
		KFMeasEntry codeEntry(&kfState);
		codeEntry.obsKey			= baseKey;
		codeEntry.obsKey.Sat		= SatSys(sys);
		codeEntry.obsKey.comment	= "Average code bias";
		
		double	sumBias	= 0;
		int		numBias	= 0;
		
		map<SatSys, double> currBiasMap;
		
		tracepdeex(2, trace, "\nFixing average code bias of %s %s bias:  ", sys._to_string(), code._to_string());
		
		for (auto& Sat : getSysSats(sys))
		{
			baseKey.Sat = Sat;
			
			double bias;
			bool found = kfState.getKFValue(baseKey, bias);
			if (found == false)
			{
				avBias.sumCode -= avBias.pastCode[Sat];
				avBias.numCode--;
				
				avBias.pastCode.erase(Sat);
				
				continue;
			}
			
			if	(  avBias.pastCode.empty() == false
				&& avBias.pastCode.find(Sat) == avBias.pastCode.end())
			{
				tracepdeex(2, trace, "+");
				
				avBias.sumCode += bias;
				avBias.numCode++;
			}
			
			tracepdeex(2, trace, "%s: %.4f;   ", Sat.id().c_str(), bias);
			
			sumBias += bias; 
			numBias++;
			
			currBiasMap[Sat] = bias;
			
			codeEntry.addDsgnEntry(baseKey, 1);
		}
		
		tracepdeex(2, trace, "\n%.4f  -> %.4f\n", sumBias, avBias.sumCode);
		
		for (auto& [Sat, bias] : currBiasMap)
		{
			avBias.pastCode[Sat] = currBiasMap[Sat];
		}
		
		if	( numBias			== 0
			||avBias.numCode	== 0)
		{
			continue;
		}
		
		double avBias1 = sumBias		/ numBias;
		double avBias0 = avBias.sumCode	/ avBias.numCode;
		
		codeEntry.setInnov(avBias0 - avBias1);
		
		codeEntry.addNoiseEntry(codeEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
		
		codeEntry.metaDataMap["pseudoObs"]	= (void*) true;
//		codeEntry.metaDataMap["explain"]	= (void*) true;
	
		kfMeasEntryList.push_back(codeEntry);
	}
}

void pseudoAvSatPhaseBias(COMMON_PSEUDO_ARGS)
{
	for (auto& [sys, process]	: acsConfig.process_sys)
	for (auto& code				: acsConfig.zero_phas_average[sys])
	{
		if (process == false)
		{
			continue;
		}
		
		auto& avBias = avBiasMap[sys][code];
		
		KFKey baseKey;
		baseKey.type	= KF::PHASE_BIAS;
		baseKey.num		= code;
		
		KFMeasEntry phasEntry(&kfState);
		phasEntry.obsKey			= baseKey;
		phasEntry.obsKey.Sat		= SatSys(sys);
		phasEntry.obsKey.comment	= "average phas bias";
		
		double	sumBias	= 0;
		int		numBias	= 0;
		map<SatSys, double> currBiasMap;
		
		tracepdeex(2, trace, "\nFixing average phase bias of %s %s bias:  ", sys._to_string(), code._to_string());
		
		for (auto& Sat : getSysSats(sys))
		{
			baseKey.Sat = Sat;
			
			double bias;
			bool found = kfState.getKFValue(baseKey, bias);
			if (found == false)
			{
				avBias.sumPhas -= avBias.pastPhas[Sat];
				avBias.numPhas--;
				
				avBias.pastPhas.erase(Sat);
			
				continue;
			}
			
			if	(  avBias.pastPhas.empty() == false
				&& avBias.pastPhas.find(Sat) == avBias.pastPhas.end())
			{
				tracepdeex(2, trace, "+");
				
				avBias.sumPhas += bias;
				avBias.numPhas++;
			}
			
			tracepdeex(2, trace, "%s: %.4f;   ", Sat.id().c_str(), bias);
			
			sumBias += bias; 
			numBias++;
			
			currBiasMap[Sat] = bias;
			
			phasEntry.addDsgnEntry(baseKey, 1);
		}
		
		tracepdeex(2, trace, "\n%.4f  -> %.4f\n", sumBias, avBias.sumPhas);
		
		for (auto& [Sat, bias] : currBiasMap)
		{
			avBias.pastPhas[Sat] = currBiasMap[Sat];
		}
		
		if	( numBias			== 0
			||avBias.numPhas	== 0)
		{
			continue;
		}
		
		double avBias1 = sumBias		/ numBias;
		double avBias0 = avBias.sumPhas	/ avBias.numPhas;
		
		phasEntry.setInnov(avBias0 - avBias1);
		
		phasEntry.addNoiseEntry(phasEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
	
		phasEntry.metaDataMap["pseudoObs"]	= (void*) true;
// 		phasEntry.metaDataMap["explain"]	= (void*) true;
		
		kfMeasEntryList.push_back(phasEntry);
	}
}










//redefine this to replace with nothing from now on - ie, use the argument name but not its type
#undef	COMMON_ARG
#define	COMMON_ARG(type)










void biasPseudoObs(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList)
{
	Instrument	instrument(__FUNCTION__);
	
	if (acsConfig.receiver_reference_clk == +E_Sys::GLO)
	{
		tracepdeex(0, trace, "GLONASS is not supported as basis for receiver clock\n");
		
		acsConfig.receiver_reference_clk = E_Sys::GPS;
	}
	
	map<E_Sys, map<int, bool>> recSetList;
	
	pseudoCommonSatBias		(COMMON_PSEUDO_ARGS);
	pseudoRecDcb			(COMMON_PSEUDO_ARGS);
	pseudoSatClockDefinition(COMMON_PSEUDO_ARGS);
	pseudoSatDCBPseudoObs	(COMMON_PSEUDO_ARGS);
	pseudoAvSatCodeBias		(COMMON_PSEUDO_ARGS);
	pseudoAvSatPhaseBias	(COMMON_PSEUDO_ARGS);
}


void ambgPseudoObs(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList)
{
	map<string,	map<E_Sys,	map<int, SatSys>>>	recBound;
	map<SatSys,				map<int, string>>	satBound;
	
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::AMBIGUITY)
			continue;
		
		auto& rec		= *key.rec_ptr;
		auto& satStat	= rec.satStatMap[key.Sat];
		
		if (satStat.el < (acsConfig.elevation_mask + 5 * D2R))
			continue;
		
		KFKey satKey	= key;
		satKey.type 	= KF::PHASE_BIAS;
		satKey.str		= "";
		
		KFKey recKey	= key;
		recKey.type 	= KF::PHASE_BIAS;
		recKey.Sat      = SatSys(key.Sat.sys);
		
		double sbias	= 0;
		double svar		= 0;
		double rbias	= 0;
		double rvar		= 0;
		
		kfState.getKFValue(satKey, sbias, &svar);
		kfState.getKFValue(recKey, rbias, &rvar);
		
		bool apply = false;
		if (acsConfig.network_amb_pivot[key.Sat.sys])
		{
			bool recBind =	(  rvar	> acsConfig.fixed_phase_bias_var * 2
							&& svar	< acsConfig.fixed_phase_bias_var
							&& recBound[key.str][key.Sat.sys].find(key.num) == recBound[key.str][key.Sat.sys].end());
			if (recBind)
			{
				recBound[key.str][key.Sat.sys][key.num] = key.Sat;
				apply = true;
			}
			
			bool satBind =	(  svar	> acsConfig.fixed_phase_bias_var * 2
							&& rvar	< acsConfig.fixed_phase_bias_var
							&& satBound[key.Sat].find(key.num) == satBound[key.Sat].end());
			if (satBind)
			{
				satBound[key.Sat][key.num] = key.str;
				apply = true;
			}
		}
		else if (acsConfig.receiver_amb_pivot[key.Sat.sys])
		{
			apply = (  rvar > acsConfig.fixed_phase_bias_var
					&& recBound[key.str][key.Sat.sys].find(key.num) == recBound[key.str][key.Sat.sys].end());
			
			if (apply)
				recBound[key.str][key.Sat.sys][key.num] = key.Sat;
		}
		
		if (apply == false)
		{
			continue;
		}
		
		double floatAmb = 0;
		
		kfState.getKFValue(key, floatAmb);
		
		double fixedAmb = ROUND(floatAmb);
		
		tracepdeex(4, trace, "\nAmbiguity pseudo-obs %s", key);
		
		KFMeasEntry measEntry(&kfState);
		measEntry.obsKey			= key;
		measEntry.obsKey.comment	= "phase binding";
		
		measEntry.addDsgnEntry(key, +1);
		
		measEntry.setInnov(fixedAmb - floatAmb);
		
		measEntry.metaDataMap["pseudoObs"]	= (void*) true;
//		measEntry.metaDataMap["explain"]	= (void*) true;
		
		measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);
	
		kfMeasEntryList.push_back(measEntry);
	}
}

void ionoPseudoObs(				//todo aaron, move to model section
	Trace&				netTrace,
	StationMap&			stations,		
	KFState&			kfState,			
	KFMeasEntryList&	kfMeasEntryList)
{
	for (auto& [id, rec]	: stations)
	for (auto& obs			: only<GObs>(rec.obsList))
	{
		if (acsConfig.use_iono_corrections[obs.Sat.sys] == false)
			continue;
		
		KFKey kfKey;
		kfKey.type	= KF::IONO_STEC;
		kfKey.str	= rec.id;
		kfKey.Sat	= obs.Sat;
		
		double extvar;
		double extion = getSSRIono(obs.time, rec.aprioriPos, obs.rSat, extvar, obs.Sat);
	
		if (extvar <= 0)
			continue;
			
		double kfion;	
		
		bool found = kfState.getKFValue(kfKey, kfion);
		if (found == false)
			continue;
		
		KFMeasEntry measEntry(&kfState);
 		measEntry.obsKey.type	= KF::IONOSPHERIC;
 		measEntry.obsKey.str	= rec.id;
 		measEntry.obsKey.Sat	= obs.Sat;
 		measEntry.obsKey.num	= 1;
		
		measEntry.addDsgnEntry(kfKey, +1);
		
		measEntry.setInnov(extion - kfion);
			
		measEntry.metaDataMap["pseudoObs"]	= (void*) true;
//		measEntry.metaDataMap["explain"]	= (void*) true;
		
		measEntry.addNoiseEntry(measEntry.obsKey, 1, extvar);
		
		kfMeasEntryList.push_back(measEntry);
	}
}
