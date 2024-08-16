
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include <iostream>
#include <vector>

using std::vector;


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "algebraTrace.hpp"
#include "coordinates.hpp"
#include "ephPrecise.hpp"
#include "navigation.hpp"
#include "mongoWrite.hpp"
#include "ephPrecise.hpp"
#include "testUtils.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "receiver.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "antenna.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "erp.hpp"

void outputApriori(
	ReceiverMap& receiverMap)
{
	if (acsConfig.mongoOpts.output_states == +E_Mongo::NONE)
	{
		return;
	}

	ERPValues erpv = getErp(nav.erp, tsync);

	FrameSwapper frameSwapper(tsync, erpv);

	KFState aprioriState;
	KFState brdcState;

	for (auto& [id, rec] : receiverMap)
	{
		// apriori_positions:
		{
			KFKey kfKey;
			kfKey.str	= id;
			kfKey.type	= KF::REC_POS;

			for (int i = 0; i < 3; i++)
			{
				kfKey.num = i;
				double apriori = rec.aprioriPos[i];

				if (apriori == 0)
				{
					continue;
				}

				aprioriState.addKFState(kfKey, {.x = apriori});
			}
		}

		// apriori_clocks:
		{
			KFKey kfKey;
			kfKey.str	= id;
			kfKey.Sat	= SatSys(E_Sys::GPS);
			kfKey.type	= KF::REC_CLOCK;

			double apriori = rec.aprioriClk;

			if (apriori == 0)
			{
				continue;
			}

			aprioriState.addKFState(kfKey, {.x = apriori});
		}
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

		SatPos satPos;
		satPos.Sat = Sat;

		//apriori_clocks:
		{
			KFKey kfKey;
			kfKey.Sat	= Sat;
			kfKey.type	= KF::SAT_CLOCK;

			aprioriState.addKFState(kfKey, {.x = CLIGHT * satNav.aprioriClk});

			bool brdcPass = satClkBroadcast(nullStream, tsync, tsync, satPos, nav);
			if (brdcPass)
			{
				brdcState.addKFState(kfKey, {.x = CLIGHT * satPos.satClk});

				satPos.rSatEci0 = frameSwapper(satPos.rSatApc);
			}
		}

		//apriori_positions:
		{
			bool brdcPass = satPosBroadcast(nullStream, tsync, tsync, satPos, nav);
			if (brdcPass)
			{
				satPos.rSatEci0 = frameSwapper(satPos.rSatApc);
			}

			for (int i = 0; i < 3; i++)
			{
				KFKey kfKey;
				kfKey.Sat	= Sat;
				kfKey.num	= i;
				kfKey.type	= KF::ORBIT;

									aprioriState.addKFState(kfKey, {.x = satNav.aprioriPos	(i)});
				if (brdcPass)		brdcState	.addKFState(kfKey, {.x = satPos.rSatEci0	(i)});
			}
		}
	}

	aprioriState.stateTransition(nullStream, tsync);
	brdcState	.stateTransition(nullStream, tsync);

	mongoStates(aprioriState,
				{
					.suffix		= "/APRIORI",
					.instances	= acsConfig.mongoOpts.output_states,
					.queue		= acsConfig.mongoOpts.queue_outputs
				});

	mongoStates(brdcState,
				{
					.suffix		= "/BROADCAST",
					.instances	= acsConfig.mongoOpts.output_states,
					.queue		= acsConfig.mongoOpts.queue_outputs
				});
}

/** Compare estimated station position with benchmark in SINEX file
 */
// void outputPPPSolution(
// 	string		filename,
// 	KFState&	kfState,
// 	Receiver&	rec)
// {
// 	VectorEcef&	snxPos		= rec.snx.pos;
//
// 	auto& recOpts = acsConfig.getRecOpts(rec.id);
//
// 	if (recOpts.apriori_pos.isZero() == false)
// 		snxPos	= recOpts.apriori_pos;
//
// 	ERPValues erpv = getErp(nav.erp, kfState.time);
//
// 	FrameSwapper frameSwapper(kfState.time, erpv);
//
// 	VectorEcef pppRRec;
// 	VectorEcef pppVRec;
//
// 	for (short i = 0; i < 3; i++)
// 	{
// 		kfState.getKFValue({KF::REC_POS, 		{}, rec.id,	i},		pppRRec[i]);
// 		kfState.getKFValue({KF::REC_POS_RATE,	{}, rec.id,	i},		pppVRec[i]);
// 	}
//
// 	VectorEci pppVRecEci;
// 	VectorEci pppRRecEci = frameSwapper(pppRRec, &pppVRec, &pppVRecEci);
//
// 	for (short i = 0; i < 3; i++)
// 	{
// 		kfState.getKFValue({KF::ORBIT, 			{}, rec.id,	i},		pppRRecEci[i]);
// 		kfState.getKFValue({KF::ORBIT,			{}, rec.id,	i + 3},	pppVRecEci[i]);
// 	}
//
// 	pppRRec = frameSwapper(pppRRecEci, &pppVRecEci, &pppVRec);
//
// 	VectorEcef	diffEcef	= snxPos - pppRRec;
//
// 	auto& pos = rec.pos;
//
// 	pos = ecef2pos(snxPos);
//
// 	VectorEnu diffEnu = ecef2enu(pos, diffEcef);
//
// 	std::ofstream fout(filename, std::ios::out | std::ios::app);
//
// 	if (!fout)
// 	{
// 		BOOST_LOG_TRIVIAL(warning)
// 		<< "Warning: Could not open trace file for PPP solution at \"" << filename << "\"";
//
// 		return;
// 	}
//
// 	if (fout.tellp() == 0)
// 	{
// 		tracepdeex(1, fout, "  Date       UTC time  Sta.   A priori X    A priori Y    A priori Z    Estimated X   Estimated Y   Estimated Z    Dif. X  Dif. Y  Dif. Z   Dif. E  Dif. N  Dif. U\n");
// 	}
//
// 	fout << kfState.time.to_string() << " ";
// 	fout << rec.id << " ";
// 	fout << std::fixed << std::setprecision(4);
// 	fout << snxPos.transpose() << "  ";
// 	fout << pppRRec.transpose() << "  ";
// 	fout << diffEcef.transpose() << "  ";
// 	fout << diffEnu.transpose() << "  ";
// 	fout << "\n";
// }

/** Output GPGGA or modified GPGGA messages
 */
// void gpggaout(
// 	string		outfile,		///< File name to output GGA message
// 	KFState&	kfState,		///< KF containing the positioning solution
// 	string		recId,			///< Receiver ID
// 	int 		solStat,		///< Solution quiality (2: ambiguity float, 5: ambiguity fix)
// 	int			numSat,			///< Number of satellites
// 	double      hdop,			///< Horizontal DOP
// 	bool		lng)			///< Modified GPGGA format (false: GPGGA format according to NMEA 0183)
// {
// 	GEpoch ep = kfState.time;
//
// 	std::ofstream fpar(outfile, std::ios::out | std::ios::app);
//
// 	if (fpar.tellp() == 0)
// 		tracepdeex(1,fpar,"!GPGGA, UTC time, Latitude, N/S, Longitude, E/W, State, # Sat, HDOP, Height, , Geoid,\n");
//
// 	VectorEcef ecef;
// 	for (short i = 0; i < 3; i++)
// 		kfState.getKFValue({KF::REC_POS,		{}, recId,	i}, ecef[i]);
//
// 	VectorPos pos = ecef2pos(ecef);
//
// 	double latint = floor(fabs(pos.latDeg()));
// 	double lonint = floor(fabs(pos.lonDeg()));
//
// 	double latv = latint*100 + (fabs(pos.latDeg()) - latint)*60;
// 	double lonv = lonint*100 + (fabs(pos.lonDeg()) - lonint)*60;
// 	char   latc = pos.latDeg()>0?'N':'S';
// 	char   lonc = pos.lonDeg()>0?'E':'W';
//
// 	if (lng)
// 	{
// 		tracepdeex(1,fpar,"$GPGGALONG,%02.0f%02.0f%05.2f,",ep[3],ep[4],ep[5]);
// 		tracepdeex(1,fpar,"%012.7f,%c,%013.7f,%c,",	latv,latc, lonv,lonc);
// 		tracepdeex(1,fpar,"%1d,%02d,%3.1f,%09.3f",	solStat, numSat, hdop,pos.hgt());
// 	}
// 	else
// 	{
// 		tracepdeex(1,fpar,"$GPGGA,%02.0f%02.0f%05.2f,",ep[3],ep[4],ep[5]);
// 		tracepdeex(1,fpar,"%09.4f,%c,%010.4f,%c,",	latv,latc, lonv,lonc);
// 		tracepdeex(1,fpar,"%1d,%02d,%3.1f,%08.2f",	solStat, numSat, hdop,pos.hgt());
// 	}
// 	tracepdeex(1,fpar,",M,0.0,M,,,\n");
// }

void selectAprioriSource(
	SatSys&		Sat,
	GTime&		time,
	KFState&	kfState,
	KFState*	remote_ptr)
{
	auto& satOpts	= acsConfig.getSatOpts(Sat);

	auto& satNav 	= nav.satNavMap[Sat];
	auto& satPos0	= satNav.satPos0;

	auto trace = getTraceFile(satNav);

	satPos0.Sat			= Sat;
	satPos0.satNav_ptr	= &satNav;

	auto posModelSources = satOpts.posModel.sources;
	auto clkModelSources = satOpts.clockModel.sources;

	//remove kalman from the list to not corrupt the apriori states
	auto brdcPosIt = std::find(posModelSources.begin(), posModelSources.end(), +E_Source::KALMAN);
	auto brdcClkIt = std::find(clkModelSources.begin(), clkModelSources.end(), +E_Source::KALMAN);
	posModelSources.erase(brdcPosIt);
	clkModelSources.erase(brdcClkIt);

	bool posPass = satpos(trace, time, time, satPos0, posModelSources, E_OffsetType::COM,	nav, &kfState, remote_ptr);
	bool clkPass = satclk(trace, time, time, satPos0, clkModelSources,						nav, &kfState, remote_ptr);

	if (posPass == false)	{	BOOST_LOG_TRIVIAL(warning) << "Warning: No sat pos found for " << satPos0.Sat.id() << ".";	return;	}
	if (clkPass == false)	{	BOOST_LOG_TRIVIAL(warning) << "Warning: No sat clk found for " << satPos0.Sat.id() << ".";	return;	}

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	satNav.aprioriPos = frameSwapper(satPos0.rSatCom);
	satNav.aprioriClk = satPos0.satClk;

	tracepdeex(1, trace, "\n");
	tracepdeex(1, trace, "\nSelecting apriori pos (ECI) at %s, found %-10s: [%f, %f, %f]",	time.to_string(), satPos0.posSource._to_string(), satNav.aprioriPos.x(), satNav.aprioriPos.x(), satNav.aprioriPos.x());
	tracepdeex(1, trace, "\nSelecting apriori clk (m)   at %s, found %-10s: %f",			time.to_string(), satPos0.clkSource._to_string(), satNav.aprioriClk * CLIGHT);

	updateSatAtts(satPos0);
}



void selectAprioriSource(
	Trace&		trace,
	Receiver&	rec,
	GTime&		time,
	bool&		sppUsed,
	KFState&	kfState,
	KFState*	remote_ptr)
{
	sppUsed = false;

	auto& recOpts = acsConfig.getRecOpts(rec.id);

	E_Source foundSource = E_Source::NONE;
	for (auto source : recOpts.posModel.sources)
	{
		switch (source)
		{
			case E_Source::CONFIG:
			{
				if (recOpts.apriori_pos.isZero())
				{
					continue;
				}

				rec.aprioriPos		= recOpts.apriori_pos;

				break;
			}
			case E_Source::PRECISE:
			{
				if (rec.snx.pos.isZero())
				{
					continue;
				}

				rec.aprioriPos		= rec.snx.pos;
				rec.primaryApriori	= rec.snx.primary;
				for (int i = 0; i < 3; i++)
				{
					rec.aprioriTime[i] = rec.snx.start[i];
				}

				break;
			}
			case E_Source::REMOTE:
			{
				if (remote_ptr == nullptr)
				{
					continue;
				}

				bool found = true;
				for (int i = 0; i < 3; i++)
				{
					KFKey kfKey;
					kfKey.type		= KF::REC_POS;
					kfKey.str		= rec.id;
					kfKey.num		= i;

					found &= remote_ptr->getKFValue(kfKey, rec.aprioriPos(i));
				}

				if (found == false)
				{
					continue;
				}

				break;
			}
			case E_Source::SPP:
			{
				rec.aprioriTime 	= rec.sol.sppTime;
				rec.aprioriPos		= rec.sol.sppRRec;

				sppUsed				= true;

				break;
			}
			case E_Source::BROADCAST:
			{
				//todo for satellite receivers
				continue;
			}
			case E_Source::KALMAN:
			{
				//skipping this for receivers because it breaks minimum constraints
				continue;
			}
			default:
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning: Unknown receiver apriori position source found: " << source._to_string();

				continue;
			}
		}

		foundSource = source;
		break;
	}

	if (foundSource == +E_Source::NONE)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No receiver apriori position found for " << rec.id;
	}

	tracepdeex(4, trace, "\nUsing %s as source for receiver apriori position: %f %f %f",
				foundSource._to_string(),
				rec.aprioriPos.x(),
				rec.aprioriPos.y(),
				rec.aprioriPos.z());

	foundSource = E_Source::NONE;

	for (auto source : recOpts.clockModel.sources)
	{
		switch (source)
		{
			case E_Source::PRECISE:
			{
				double	dtRec		= 0;
				double	dtRecVar	= 0;
				bool pass = pephclk(trace, time, rec.id, nav, dtRec, &dtRecVar);
				if (pass == false)
				{
					continue;
				}

				rec.aprioriClk		= dtRec		*		CLIGHT;
				rec.aprioriClkVar	= dtRecVar	* SQR(	CLIGHT);

				break;
			}
			case E_Source::KALMAN:
			case E_Source::REMOTE:
			{
				if (source == +E_Source::REMOTE	&& remote_ptr	== nullptr)	continue;

				E_Source found;
				{
					KFKey kfKey;
					kfKey.type		= KF::REC_CLOCK;
					kfKey.str		= rec.id;

					double dummy;

					if (source == +E_Source::KALMAN)	found = kfState.	getKFValue(kfKey, rec.aprioriClk, &rec.aprioriClkVar, &dummy, false);
					if (source == +E_Source::REMOTE)	found = remote_ptr->getKFValue(kfKey, rec.aprioriClk, &rec.aprioriClkVar, &dummy, false);
				}

				if (found == false)
				{
					continue;
				}

				break;
			}
			case E_Source::SPP:
			{
				rec.aprioriClk		= rec.sol.dtRec_m[E_Sys::GPS];
				rec.aprioriClkVar	= SQR(30);

				break;
			}
			case E_Source::BROADCAST:
			{
				//ignore broadcast thats in the common default list for satellites benefit
				continue;
			}
			default:
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning: Unknown receiver apriori clock source found: " << source._to_string();
				continue;
			}
		}

		foundSource = source;
		break;
	}

	if (foundSource == +E_Source::NONE)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No receiver apriori clock found for " << rec.id;
	}

	tracepdeex(4, trace, "\nUsing %s as source for receiver apriori clock: %f",
				foundSource._to_string(),
				rec.aprioriClk);

	if (recOpts.apriori_sigma_enu.empty() == false)
	{
		Matrix3d enuNoise = Matrix3d::Zero();
		for (int i = 0; i < 3; i++)
		{
			int j = i;

			if (j >= recOpts.apriori_sigma_enu.size())
				j = recOpts.apriori_sigma_enu.size() - 1;

			enuNoise(i,i) = SQR(recOpts.apriori_sigma_enu[j]);
		}

		VectorPos pos = ecef2pos(rec.aprioriPos);

		Matrix3d E;
		pos2enu(pos, E.data());

		Matrix3d varianceXYZ = E.transpose()	* enuNoise * E;


		rec.aprioriVar 		= varianceXYZ;
	}
	else
	{
		rec.aprioriVar		= rec.snx.var.asDiagonal();
	}

	Vector3d delta	= rec.aprioriPos
					- rec.sol.sppRRec;

	double distance = delta.norm();

	if	( distance > 20
		&&rec.sol.sppRRec.norm() > 0)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Apriori for " << rec.id << " is " << distance << "m from SPP estimate";
	}
}

string ft2string(
	E_FType ft)
{
	return "F" + std::to_string(ft);
}

/** Remove ambiguity states from filter when they deemed old or bad
 * This effectively reinitialises them on the following epoch as a new state, and can be used for simple
 * resolution of cycle-slips
 */
void removeBadAmbiguities(
	Trace&			trace,			///< Trace to output to
	KFState&		kfState, 		///< Filter to remove states from
	ReceiverMap&	receiverMap)	///< List of stations containing observations for this epoch
{
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::AMBIGUITY)
		{
			continue;
		}

		if (key.rec_ptr == nullptr)
		{
			continue;
		}

		auto& rec		= *key.rec_ptr;
		auto& satStat	= rec.satStatMap[key.Sat];

		string	preprocSigName;
		string	sigName;

		if	(acsConfig.process_ppp)
		{
			E_ObsCode	obsCode	= E_ObsCode::_from_integral(key.num);
			E_FType		ft		= code2Freq[key.Sat.sys][obsCode];

			preprocSigName	= ft2string(ft);
			sigName			= obsCode._to_string();
		}
		else
		{
			E_FType ft = (E_FType) key.num;

			preprocSigName	= ft2string(ft);		//todo aaron, is this redundant now that network is gone?
			sigName			= preprocSigName;
		}

		auto& sigStat			= satStat.sigStatMap[sigName];
		auto& preprocSigStat	= satStat.sigStatMap[preprocSigName];

		if	( sigStat.lastPhaseTime							!=	GTime::noTime()
			&&(tsync - sigStat.lastPhaseTime).to_double()	>	acsConfig.ambErrors.outage_reset_limit)
		{
			sigStat.lastPhaseTime = GTime::noTime();

			trace << "\n" << "Phase ambiguity removed due to long outage: " <<	sigName	<< " " << key;

			kfState.statisticsMap["Phase outage resets"]++;

			char buff[64];
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-OUTAGE",	key.str.c_str());				kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-OUTAGE",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.str.c_str());				kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;

			kfState.removeState(key);
			continue;
		}

		if (sigStat.phaseRejectCount >= acsConfig.ambErrors.phase_reject_limit)
		{
			sigStat.phaseRejectCount = 0;

			trace << "\n" << "Phase ambiguity removed due to high reject count: " <<	sigName	<< " " 	<< key;

			kfState.statisticsMap["Phase reject resets"]++;

			char buff[64];
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-REJECT",	key.str.c_str());				kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-REJECT",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.str.c_str());				kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;

			kfState.removeState(key);

			InitialState init = initialStateFromConfig(acsConfig.getRecOpts("global").ion_stec);

			if (init.estimate == false)
			{
				KFKey kfKey = key;
				for (kfKey.num = 0; kfKey.num < NUM_FTYPES; kfKey.num++)
				{
					kfState.removeState(kfKey);
					continue;
				}
			}
		}

		//reset slipping signals
		if 	(  preprocSigStat.savedSlip.any
			&& (  (acsConfig.ambErrors.resetOnSlip.LLI		&& preprocSigStat.savedSlip.LLI)
				||(acsConfig.ambErrors.resetOnSlip.GF		&& preprocSigStat.savedSlip.GF)
				||(acsConfig.ambErrors.resetOnSlip.MW		&& preprocSigStat.savedSlip.MW)
				||(acsConfig.ambErrors.resetOnSlip.SCDIA	&& preprocSigStat.savedSlip.SCDIA)))
		{
			trace << "\n" << "Phase ambiguity removed due cycle slip detection: "	<< key;

			if (acsConfig.ambErrors.resetOnSlip.LLI		&& preprocSigStat.savedSlip.LLI)		trace << "\t - LLI";
			if (acsConfig.ambErrors.resetOnSlip.GF		&& preprocSigStat.savedSlip.GF)			trace << "\t - GF";
			if (acsConfig.ambErrors.resetOnSlip.MW		&& preprocSigStat.savedSlip.MW)			trace << "\t - MW";
			if (acsConfig.ambErrors.resetOnSlip.SCDIA	&& preprocSigStat.savedSlip.SCDIA)		trace << "\t - SCDIA";

			kfState.statisticsMap["Cycle slip resets"]++;

			char buff[64];
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-PREPROC",	key.str.c_str());				kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-PREPROC",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.str.c_str());				kfState.statisticsMap[buff]++;
			snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;

			kfState.removeState(key);
		}

		//for ionosphere free, need to reset all connected singals
		if	(acsConfig.pppOpts.ionoOpts.use_if_combo)
		for (auto& [sigNam, sigStat] : satStat.sigStatMap)
		{
			if	(sigStat.savedSlip.any
				&&( (  acsConfig.ambErrors.resetOnSlip.LLI		&& sigStat.savedSlip.LLI)
					||(acsConfig.ambErrors.resetOnSlip.GF		&& sigStat.savedSlip.GF)
					||(acsConfig.ambErrors.resetOnSlip.MW		&& sigStat.savedSlip.MW)
					||(acsConfig.ambErrors.resetOnSlip.SCDIA	&& sigStat.savedSlip.SCDIA)))
			{
				trace << "\n" << "Phase ambiguity removed due cycle slip detection: "	<< key;

				if (acsConfig.ambErrors.resetOnSlip.LLI		&& sigStat.savedSlip.LLI)			trace << "\t - LLI";
				if (acsConfig.ambErrors.resetOnSlip.GF		&& sigStat.savedSlip.GF)			trace << "\t - GF";
				if (acsConfig.ambErrors.resetOnSlip.MW		&& sigStat.savedSlip.MW)			trace << "\t - MW";
				if (acsConfig.ambErrors.resetOnSlip.SCDIA	&& sigStat.savedSlip.SCDIA)			trace << "\t - SCDIA";

				kfState.statisticsMap["Cycle slip resets*"]++;

				char buff[64];
				snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-PREPROC",	key.str.c_str());				kfState.statisticsMap[buff]++;
				snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-PREPROC",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;
				snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.str.c_str());				kfState.statisticsMap[buff]++;
				snprintf(buff, sizeof(buff), "Ambiguity Reset-%4s-TOTAL",	key.Sat.id().c_str());			kfState.statisticsMap[buff]++;

				kfState.removeState(key);
			}
		}
	}

	for (auto& [id,		rec]		: receiverMap)
	for (auto& [sat,	satStat]	: rec.satStatMap)
	for (auto& [sig,	sigStat]	: satStat.sigStatMap)
	{
		sigStat.savedSlip.any = false;
	}
}

void removeBadReceivers(
	Trace&			trace,				///< Trace to output to
	KFState&		kfState, 			///< Filter to remove states from
	ReceiverMap&	receiverMap)	///< List of stations containing observations for this epoch
{
	if (acsConfig.errorAccumulation.enable == false)
	{
		return;
	}

	for (auto& [id, rec] : receiverMap)
	{
		if (rec.receiverErrorCount >= acsConfig.errorAccumulation.receiver_error_count_threshold)		rec.receiverErrorEpochs++;
		else																							rec.receiverErrorEpochs = 0;

		rec.receiverErrorCount	= 0;

		if (rec.receiverErrorEpochs < acsConfig.errorAccumulation.receiver_error_epochs_threshold)
		{
			continue;
		}

		rec.receiverErrorEpochs	= 0;

		for (auto [key, index] : kfState.kfIndexMap)
		if (key.str == rec.id)
		{
			trace << "\n" << "State removed due to high receiver error counts: " << key;

			kfState.removeState(key);
		}

		kfState.statisticsMap["Rec error resets"]++;
	}
}

/** Remove ambiguity states from filter when they deemed old or bad
 * This effectively reinitialises them on the following epoch as a new state, and can be used for simple
 * resolution of cycle-slips
 */
void removeBadIonospheres(
	Trace&				trace,				///< Trace to output to
	KFState&			kfState) 			///< Filter to remove states from
{
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if (key.type == KF::IONOSPHERIC)
		{
			double state;
			double variance;
			kfState.getKFValue(key, state, &variance);

			auto& recOpts = acsConfig.getRecOpts(key.str);

			if (variance > SQR(recOpts.iono_sigma_limit))
			{
				trace << "\n" << "Ionosphere removed due to high variance: " << key;

				kfState.removeState(key);
			}

			if (key.rec_ptr == nullptr)
			{
				auto& rec		= *key.rec_ptr;
				auto& satStat	= rec.satStatMap[key.Sat];

				if	( satStat.lastIonTime						!=	GTime::noTime()
					&&(tsync - satStat.lastIonTime).to_double()	>	acsConfig.ionErrors.outage_reset_limit)
				{
					kfState.removeState(key);
				}
			}

			continue;
		}

		if (key.type != KF::IONO_STEC)
		{
			continue;
		}

		if (key.rec_ptr == nullptr)
		{
			continue;
		}

		auto& rec		= *key.rec_ptr;
		auto& satStat	= rec.satStatMap[key.Sat];
		auto& recOpts	= acsConfig.getRecOpts(key.str);

		if	( satStat.lastIonTime						!=	GTime::noTime()
			&&(tsync - satStat.lastIonTime).to_double()	>	acsConfig.ionErrors.outage_reset_limit)
		{
			satStat.lastIonTime = GTime::noTime();

			trace << "\n" << "Ionosphere removed due to long outage: " << key;

			kfState.statisticsMap["Iono outage resets"]++;

			kfState.removeState(key);
			continue;
		}

		double state;
		double variance;
		kfState.getKFValue(key, state, &variance);

		if (variance > SQR(recOpts.iono_sigma_limit))
		{
			trace << "\n" << "Ionosphere removed due to high variance: " << key;

			kfState.removeState(key);
		}
	}
}

void postFilterChecks(
	const	GTime&	time,
			KFMeas&	kfMeas)
{
	for (int i = 0; i < kfMeas.V.rows(); i++)
	{
		resetPhaseSignalError	(time, kfMeas, i);
		resetPhaseSignalOutage	(time, kfMeas, i);
		resetIonoSignalOutage	(time, kfMeas, i);
	}
}

/* write solution status for PPP
*/
void outputPppNmea(
	Trace&		trace,
	KFState&	kfState,
	string		id)
{
	GWeek	week	= kfState.time;
	GTow	tow		= kfState.time;

	Block block(trace, "NMEA");

	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		KFKey key = kfKey;
		if	( key.type	== KF::REC_POS
			&&key.num	== 0
			&&key.str	== id)
		{
			double x[3]	= {};
			double v[3]	= {};

			for (key.num = 0; key.num < 3; key.num++)
			{
				kfState.getKFValue(key, x[key.num], &v[key.num]);
			}
			tracepdeex(0, trace, "$POS,%d,%.3f,%.4f,%.4f,%.4f,%.7f,%.7f,%.7f\n",
						week,
						tow,
						x[0],
						x[1],
						x[2],
						sqrt(v[0]),
						sqrt(v[1]),
						sqrt(v[2]));
		}

// 		if (key.type == KF::PHASE_BIAS)
// 		{
// 			double phase_bias		= 0;
// 			double phase_biasVar	= 0;
// 			kfState.getKFValue(key, phase_bias, &phase_biasVar);
// 			tracepdeex(1, trace, "$AMB,%d,%.3f,%d,%s,%d,%.4f,%.7f\n",
// 						week,
// 						tow,
// 						solStat,
// 						key.Sat.id().c_str(),
// 						key.num,
// 						phase_bias,
// 						sqrt(phase_biasVar));
// 		}

		if	( key.type	== KF::TROP	//todo aaron needs iteration
			&&key.str	== id)
		{
			string grad;
			double trop		= 0;
			double tropVar	= 0;
			if (key.num == 1)	grad = "_N";
			if (key.num == 2)	grad = "_E";
			kfState.getKFValue(key, trop, &tropVar);
			tracepdeex(0, trace, "$TROP%s,%d,%.3f,%f,%.7f\n",
					grad.c_str(),
					week,
					tow,
					trop,
					sqrt(tropVar));
		}

		if	( key.type	== KF::REC_SYS_BIAS
			&&key.Sat	== SatSys(E_Sys::GPS)
			&&key.str	== id)
		{
			double rClkGPS		= 0;
			double rClkGLO		= 0;
			double rClkGAL		= 0;
			double rClkBDS		= 0;
			double GPSclkVar	= 0;
			double GLOclkVar	= 0;
			double GALclkVar	= 0;
			double BDSclkVar	= 0;

			key.Sat	= SatSys(E_Sys::GPS);			kfState.getKFValue(key, rClkGPS, &GPSclkVar);
			key.Sat = SatSys(E_Sys::GLO);			kfState.getKFValue(key, rClkGLO, &GLOclkVar);
			key.Sat = SatSys(E_Sys::GAL);			kfState.getKFValue(key, rClkGAL, &GALclkVar);
			key.Sat = SatSys(E_Sys::BDS);			kfState.getKFValue(key, rClkBDS, &BDSclkVar);

			tracepdeex(0, trace, "$CLK,%d,%.3f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n",
					week,
					tow,
					rClkGPS			* 1E9 / CLIGHT,
					rClkGLO			* 1E9 / CLIGHT,
					rClkGAL			* 1E9 / CLIGHT,
					rClkBDS			* 1E9 / CLIGHT,
					sqrt(GPSclkVar)	* 1E9 / CLIGHT,
					sqrt(GLOclkVar)	* 1E9 / CLIGHT,
					sqrt(GALclkVar)	* 1E9 / CLIGHT,
					sqrt(BDSclkVar)	* 1E9 / CLIGHT);
		}
	}
//
// 	/* receiver velocity and acceleration */
// 	{
// 		ecef2pos(rtk->sol.rr, pos);
// 		ecef2enu(pos, rtk->xx + 3, vel);
// 		ecef2enu(pos, rtk->xx + 6, acc);
// 		p += sprintf(p, "$VELACC,%d,%.3f,%d,%.4f,%.4f,%.4f,%.5f,%.5f,%.5f,%.4f,%.4f,"
// 		             "%.4f,%.5f,%.5f,%.5f\n", week, tow, rtk->sol.stat, vel[0], vel[1],
// 		             vel[2], acc[0], acc[1], acc[2], 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
// 	}
}


double netResidualAndChainOutputs(
	Trace&			trace,
	Observation&	obs,
	KFMeasEntry&	measEntry)
{
	double residual		= 0;
	double residualVar	= 0;

	for (auto& [component, details] : measEntry.componentsMap)
	{
		auto& [componentVal, eq, var] = details;

		residual -= componentVal;

		if (var > 0)
		{
			residualVar += var;
		}

		if (acsConfig.output_residual_chain)
		{
			tracepdeex(0, trace, "\n");
			tracepdeex(4, trace, "%s",		obs.time.to_string());
			tracepdeex(3, trace, "%30s",	((string)measEntry.obsKey).c_str());
			tracepdeex(0, trace, " %-23s %+14.4f", component._to_string(), -componentVal);

			if		(var >= 0)		tracepdeex(2, trace, " ~ %5.3e", var);
			else if	(var == 0)		tracepdeex(2, trace, " ~ 0        ");
			else					tracepdeex(2, trace, " ~ Estimated");

			tracepdeex(2, trace, " -> %13.4f",	residual);
			tracepdeex(3, trace, " ~ %.2e ",	residualVar);
		}
	}

	for (auto& [component, details] : measEntry.componentsMap)
	{
		auto& [componentVal, eq, var] = details;

		if (var > 100)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Unestimated component '" << component._to_string() << "' for '" << measEntry.obsKey
			<< "' has large variance (" << var << "), valid inputs may not (yet) be available";

			trace << "\n"
			<< "Warning: Unestimated component '" << component._to_string() << "' for '" << measEntry.obsKey
			<< "' has large variance (" << var << "), valid inputs may not (yet) be available";
		}
	}

	if (acsConfig.output_residual_chain)
	{
		trace << "\n" << "\n" << "0 =";

		for (auto& [component, details] : measEntry.componentsMap)
		{
			auto& [componentVal, eq, var] = details;

			tracepdeex(0, trace, " %s", eq.c_str());
		}
	}

	if (abs(residual) > 1e30)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: " << measEntry.obsKey << " has very large residual: " << residual;
	}

	return residual;
}
