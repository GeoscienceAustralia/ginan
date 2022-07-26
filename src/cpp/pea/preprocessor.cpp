
// #pragma GCC optimize ("O0")

#include "observations.hpp"
#include "streamTrace.hpp"
#include "instrument.hpp"
#include "GNSSambres.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "station.hpp"
#include "satStat.hpp"
#include "sinex.hpp"
#include "acsQC.hpp"
#include "ppp.hpp"


void outputObservations(
	Trace&		trace,
	ObsList&	obsList)
{
	for (auto& obs : obsList)
	{
		auto& satStat = *obs.satStat_ptr;
		
		trace << std::endl << "-->"
		<< " " << obs.Sat.id() 
		<< " " << obs.time.to_string(3)
		<< " " << satStat.el * R2D 
		<< " " << obs.ephVar 
		<< " " << obs.vsat;
		
		for (auto& [ft, sig] : obs.Sigs)
		{
			trace 
			<< " F" << ft 
			<< " " << sig.vsig
			<< " " << sig.codeVar;
		}
	}
}

void obsVariances(
	ObsList& obsList)
{
	for (auto& obs			: obsList)
	for (auto& [ft, Sig]	: obs.Sigs)
	{
		auto& receiverOpts = acsConfig.getRecOpts(obs.mount);

		//get the sigma for this frequency, (or the last one in the list)
		int freqTypeCode = ft;
		int freqTypePhas = ft;
		if (freqTypeCode >= receiverOpts.code_sigmas.size())		freqTypeCode = receiverOpts.code_sigmas.size() - 1;
		if (freqTypePhas >= receiverOpts.phas_sigmas.size())		freqTypePhas = receiverOpts.phas_sigmas.size() - 1;

		double sigmaCode = receiverOpts.code_sigmas[freqTypeCode];
		double sigmaPhas = receiverOpts.phas_sigmas[freqTypePhas];

		double el = obs.satStat_ptr->el;
		if (el == 0)
			el = PI/8;

		double elevationScaling = 1;
		switch (receiverOpts.error_model)
		{
			case E_NoiseModel::UNIFORM:					{	elevationScaling = 1;				break;	}
			case E_NoiseModel::ELEVATION_DEPENDENT:		{	elevationScaling = 1 / sin(el);		break;	} }

		sigmaCode *= elevationScaling;
		sigmaPhas *= elevationScaling;

		Sig.codeVar = sigmaCode * sigmaCode;
		Sig.phasVar = sigmaPhas * sigmaPhas;
	}
}

void excludeUnprocessed(
	ObsList&	obsList)
{
	for (auto& obs : obsList)
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			obs.excludeSystem = true;
		}
	}
}

void preprocessor(
	Station&	rec)
{
// 	TestClipper tc;
	
	Instrument instrument(__FUNCTION__);
	
	auto trace = getTraceFile(rec);
		
	acsConfig.getRecOpts(rec.id);
	
	auto& obsList = rec.obsList;
	
	rec.sol.time = obsList.front().time;
	
	GTime start_time;
	start_time.time = static_cast<int>(boost::posix_time::to_time_t(acsConfig.start_epoch));
	
	if	(  acsConfig.start_epoch.is_not_a_date_time() == false
		&& rec.sol.time < start_time - 0.5)
	{
		return;
	}
	
	//prepare and connect navigation objects to the observations
	for (auto& obs : obsList)
	{
		obs.satNav_ptr	= &nav.satNavMap[obs.Sat];
		obs.mount 		= rec.id;
		
		if (obs.Sat.sys == +E_Sys::GLO)		obs.satNav_ptr->geph_ptr	= seleph<Geph>	(trace, obs.time, obs.Sat, -1, nav);
		else								obs.satNav_ptr->eph_ptr		= seleph<Eph>	(trace, obs.time, obs.Sat, -1, nav);
		
		updatenav(obs);

		obs.satStat_ptr = &rec.satStatMap[obs.Sat];

		acsConfig.getSatOpts(obs.Sat);
		
		//ar stuff
		{
			if (acsConfig.process_network)	ARstations["NETWORK"].ID	= "NETWORK";
			else							ARstations[rec.id].ID		= rec.id;
			
			sys_activ[rec.id];
		
			ARsatellites[obs.Sat];
			
			for (short int i = 0; i < E_AmbTyp::_size(); i++)
			{
				E_AmbTyp	ambType			= E_AmbTyp::_values()[i];
				
				elev_archive[{KF::AMBIGUITY, obs.Sat, obs.mount, i}];
				slip_archive[{KF::AMBIGUITY, obs.Sat, obs.mount, i}];
			}
		} 
	}
	
	excludeUnprocessed(obsList);
	
	
	//satellite positons, velocities and clocks
	satposs(trace, rec.sol.time, obsList, nav, acsConfig.model.sat_pos.ephemeris_source, E_OffsetType::APC);

	obsVariances(obsList);

	/* linear combinations */
	for (auto& obs : obsList)
	{
		obs.satStat_ptr->lc_pre = obs.satStat_ptr->lc_new;
		obs.satStat_ptr->lc_new = {};
	}
	obs2lcs		(trace,	obsList);

	//cycle slip detection
	detectslips	(trace,	obsList);

	for (auto& obs			: obsList)
	for (auto& [ft, Sig]	: obs.Sigs)
	{
		if (obs.satStat_ptr->sigStatMap[ft].slip.any)
		{
			rec.slipCount++;
			break;
		}
	}

	//do a spp on the observations
	sppos(trace, obsList, rec.sol);
	
// 	outputObservations(trace, obsList);
	
	//recalculate variances now that elevations are known due to satellite postions calculation above
	obsVariances(obsList);
}
