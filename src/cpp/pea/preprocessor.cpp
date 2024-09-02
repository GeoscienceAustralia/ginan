
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/** Use linear combinations of primary signals to detect jumps in carrier phase observations
 */
Architecture Cycle_Slip_Detection__()
{

}

/** Perform basic quality checks on observations
 */
Architecture Preprocessing__()
{
	DOCS_REFERENCE(Cycle_Slip_Detection__);
	DOCS_REFERENCE(SPP__);
}

#include "observations.hpp"
#include "navigation.hpp"
#include "GNSSambres.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "receiver.hpp"
#include "satStat.hpp"
#include "trace.hpp"
#include "sinex.hpp"
#include "acsQC.hpp"
#include "ppp.hpp"


void outputObservations(
	Trace&		trace,
	Trace&		jsonTrace,
	ObsList&	obsList)
{
	for (auto& obs : only<GObs>(obsList))
	for (auto& [ft, sigs] : obs.sigsLists)
	for (auto& sig : sigs)
	{
		if (obs.exclude)
		{
			continue;
		}

		tracepdeex(4, trace, "\n%s %5s %5s %14.4f %14.4f", obs.time.to_string().c_str(), obs.Sat.id().c_str(), sig.code._to_string(), sig.L, sig.P);

		traceJson(4, jsonTrace, obs.time,
		{
			{"data", __FUNCTION__},
			{"Sat", obs.Sat.id()},
			{"Rec", obs.mount},
			{"Sig", sig.code._to_string()}
		},
		{
			{"SNR", sig.snr},
			{"L",	sig.L},
			{"P",	sig.P},
			{"D",	sig.D},
			// {"LLI", sig.lli},
		});
	}
}

void obsVariances(
	ObsList& obsList)
{
	for (auto& obs			: only<GObs>(obsList))
	if (obs.satNav_ptr)
	if (obs.satStat_ptr)
	if (obs.exclude == false)
	if (acsConfig.process_sys[obs.Sat.sys])
	{
		auto& recOpts = acsConfig.getRecOpts(obs.mount);
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		double el = obs.satStat_ptr->el;
		if (el == 0)
			el = PI/8;

		double recElScaling = 1;
		switch (recOpts.error_model)
		{
			case E_NoiseModel::UNIFORM:					{	recElScaling = 1;				break;	}
			case E_NoiseModel::ELEVATION_DEPENDENT:		{	recElScaling = 1 / sin(el);		break;	}
		}

		double satElScaling = 1;
		switch (satOpts.error_model)
		{
			case E_NoiseModel::UNIFORM:					{	satElScaling = 1;				break;	}
			case E_NoiseModel::ELEVATION_DEPENDENT:		{	satElScaling = 1 / sin(el);		break;	}
		}

		for (auto& [ft, sig] : obs.sigs)
		{
			if (sig.P == 0)
				continue;

			string sigName = sig.code._to_string();

			auto& satOpts = acsConfig.getSatOpts(obs.Sat,	{sigName});
			auto& recOpts = acsConfig.getRecOpts(obs.mount,	{obs.Sat.sys._to_string(), sigName});

			sig.codeVar = 0;
			sig.phasVar = 0;

			sig.codeVar += SQR(recElScaling * recOpts.code_sigma);			sig.codeVar += SQR(satElScaling * satOpts.code_sigma);
			sig.phasVar += SQR(recElScaling * recOpts.phase_sigma);			sig.phasVar += SQR(satElScaling * satOpts.phase_sigma);
		}

		for (auto& [ft, sigList] : obs.sigsLists)
		for (auto& sig : sigList)
		{
			string sigName = sig.code._to_string();

			auto& satOpts = acsConfig.getSatOpts(obs.Sat,	{sigName});
			auto& recOpts = acsConfig.getRecOpts(obs.mount,	{obs.Sat.sys._to_string(), sigName});

			sig.codeVar = 0;
			sig.phasVar = 0;

			sig.codeVar += SQR(recElScaling * recOpts.code_sigma);			sig.codeVar += SQR(satElScaling * satOpts.code_sigma);
			sig.phasVar += SQR(recElScaling * recOpts.phase_sigma);			sig.phasVar += SQR(satElScaling * satOpts.phase_sigma);
		}
	}
}

void excludeUnprocessed(
	ObsList&	obsList)
{
	for (auto& obs : only<GObs>(obsList))
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			obs.excludeSystem = true;
		}
	}
}

void recordSlips(
	Receiver&	rec)
{
	for (auto& obs			: only<GObs>(rec.obsList))
	for (auto& [ft, sig]	: obs.sigs)
	if  (obs.satStat_ptr)
	{
		SigStat& sigStat = obs.satStat_ptr->sigStatMap[ft2string(ft)];

		if	(	sigStat.slip.any
			&&( (acsConfig.exclude.LLI		&& sigStat.slip.LLI)
			  ||(acsConfig.exclude.GF		&& sigStat.slip.GF)
			  ||(acsConfig.exclude.MW		&& sigStat.slip.MW)
			  ||(acsConfig.exclude.SCDIA	&& sigStat.slip.SCDIA)))
		{
			rec.savedSlips[obs.Sat] = obs.time;
		}
	}
}

void preprocessor(
	Trace&		trace,
	Receiver&	rec,
	bool		realEpoch)
{
	DOCS_REFERENCE(Preprocessing__);

	if	( (acsConfig.process_preprocessor == false)
		||(acsConfig.preprocOpts.preprocess_all_data == true	&& realEpoch == true)
		||(acsConfig.preprocOpts.preprocess_all_data == false	&& realEpoch == false))
	{
		return;
	}

	auto jsonTrace	= getTraceFile(rec, true);

	acsConfig.getRecOpts(rec.id);

	auto& obsList = rec.obsList;

	if (obsList.empty())
	{
		return;
	}

	PTime start_time;
	start_time.bigTime = boost::posix_time::to_time_t(acsConfig.start_epoch);

	double tol;
	if (acsConfig.assign_closest_epoch)			tol = acsConfig.epoch_interval / 2;		//todo aaron this should be the epoch_tolerance?
	else										tol = 0.5;

	if	(  acsConfig.start_epoch.is_not_a_date_time() == false
		&& obsList.front()->time < (GTime) start_time - tol)
	{
		return;
	}

	//prepare and connect navigation objects to the observations
	for (auto& obs : only<GObs>(obsList))
	{
		obs.mount = rec.id;

		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			obs.excludeSystem = true;

			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		if (satOpts.exclude)
		{
			obs.excludeConfig = true;

			continue;
		}

		auto& satNav = nav.satNavMap[obs.Sat];

		obs.rec_ptr		= &rec;
		obs.satNav_ptr	= &satNav;
		obs.satStat_ptr = &rec.satStatMap[obs.Sat];

		updateLamMap(obs.time, obs);
	}

	for (auto& obs : only<LObs>(obsList))
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			continue;
		}

		obs.satNav_ptr	= &nav.satNavMap[obs.Sat];
		obs.satStat_ptr = &rec.satStatMap[obs.Sat];
	}

	clearSlips(obsList);

	excludeUnprocessed(obsList);

	outputObservations(trace, jsonTrace, obsList);

	/* linear combinations */
	for (auto& obs : only<GObs>(obsList))
	if (obs.satStat_ptr)
	{
		obs.satStat_ptr->lc_pre = obs.satStat_ptr->lc_new;
		obs.satStat_ptr->lc_new = {};
	}
	obs2lcs		(trace,	obsList);

	detectslips	(trace,	obsList);

	recordSlips(rec);

	for (auto& obs			: only<GObs>(obsList))
	for (auto& [ft, Sig]	: obs.sigs)
	if  (obs.satStat_ptr)
	{
		if (obs.satStat_ptr->sigStatMap[ft2string(ft)].slip.any)
		{
			rec.slipCount++;
			break;
		}
	}
}
