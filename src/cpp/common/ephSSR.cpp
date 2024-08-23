
#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "ephemeris.hpp"
#include "gTime.hpp"
#include "ssr.hpp"


#define DEFURASSR		0.03				///< default accurary of ssr corr (m)
#define MAXECORSSR		15					///< max orbit correction of ssr (m)
#define MAXCCORSSR		(1E-6*CLIGHT)		///< max clock correction of ssr (m)

/** variance by ura ssr (ref [4])
*/
double var_urassr(
	int ura)
{
	if (ura <= 0)		return SQR(DEFURASSR);
	if (ura >= 63)		return SQR(5.4665);

	double std = (pow(3, (ura >> 3) & 7) * (1.0 + (ura & 7) / 4.0) - 1.0) * 1E-3;
	return SQR(std);
}

Matrix3d rac2ecef(
	Vector3d& rSat,				// Sat position (ECEF)
	Vector3d& satVel)			// Sat velocity (ECEF)
{
	Matrix3d ecef2racMat = ecef2rac(rSat, satVel);

	return ecef2racMat.transpose();
}

template<typename TYPE>
void cullSSRMap(
	GTime	time,
	TYPE&	map)
{
	for (auto it = map.begin(); it != map.end(); )
	{
		auto& [ssrtime, ssr] = *it;

		if (ssr.t0 < time - ssr.udi * acsConfig.validity_interval_factor)
		{
			it = map.erase(it);
		}
		else
		{
			++it;
		}
	}
}

void cullOldSSRs(
	GTime	time)
{
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		cullSSRMap(time, satNav.receivedSSR.ssrCodeBias_map);
		cullSSRMap(time, satNav.receivedSSR.ssrPhasBias_map);
		cullSSRMap(time, satNav.receivedSSR.ssrClk_map);
		cullSSRMap(time, satNav.receivedSSR.ssrEph_map);
		cullSSRMap(time, satNav.receivedSSR.ssrHRClk_map);
		cullSSRMap(time, satNav.receivedSSR.ssrUra_map);
	}
}


bool ssrPosDelta(
	Trace&			trace,
	GTime			time,
	GTime			ephTime,
	SatPos&			satPos,
	const SSRMaps&	ssrMaps,
	Vector3d&		dPos,
	int&			iodPos,
	int&			iodEph,
	GTime&			validStart,
	GTime&			validStop)
{
	if (ssrMaps.ssrEph_map.empty())
	{
		satPos.failureSsrPosEmpty = true;

		return false;
	}

	//get 'price is right' closest ssr components to ephemeris time.
	auto ephIt = ssrMaps.ssrEph_map.lower_bound(ephTime);
	if (ephIt == ssrMaps.ssrEph_map.end())
	{
		satPos.failureSsrPosTime = true;

		return false;
	}

	auto& [t_e, ssrEph] = *ephIt;

	iodPos = ssrEph.iod;
	iodEph = ssrEph.iode;

	double tEph = (time - ssrEph.t0).to_double();

	validStart	= ssrEph.t0 - ssrEph.udi / 2;
	validStop	= ssrEph.t0 + ssrEph.udi / 2;

	/* ssr orbit and clock correction (ref [4]) */
	if (fabs(tEph) > ssrEph.udi * acsConfig.validity_interval_factor)
	{
		satPos.failureSsrPosUdi = true;

		tracepdeex(2, std::cout, "age of ssr error: %s t=%.0f > %.0f\n", time.to_string().c_str(), tEph, ssrEph.udi * acsConfig.validity_interval_factor);
		return false;
	}

	dPos	= ssrEph.deph
			+ ssrEph.ddeph * tEph;

	if (dPos.norm() > MAXECORSSR)
	{
		satPos.failureSsrPosMag = true;

		tracepdeex(2, std::cout, "SSR pos correction too large : %s %s deph=%.1fm\n", time.to_string().c_str(), satPos.Sat.id().c_str(), dPos.norm());
		return false;
	}

	tracepdeex(5, trace, "\n%s %2d %2d %lf %s %s %lf", __FUNCTION__, iodPos, iodEph, tEph, validStart.to_string().c_str(), validStop.to_string().c_str(), dPos.norm());

	return true;
}

bool ssrClkDelta(
	Trace&			trace,
	GTime			time,
	GTime			ephTime,
	SatPos&			satPos,
	const SSRMaps&	ssrMaps,
	double&			dclk,
	int&			iodClk,
	GTime&			validStart,
	GTime&			validStop)
{
	if (ssrMaps.ssrClk_map.empty())
	{
		satPos.failureSsrClkEmpty = true;
		tracepdeex(4, std::cout, "No SSR corrections for sat=%s\n", satPos.Sat.id().c_str());
		return false;
	}

	//get 'price is right' closest ssr components to ephemeris time
	auto clkIt = ssrMaps.ssrClk_map.lower_bound(ephTime);
	if (clkIt == ssrMaps.ssrClk_map.end())
	{
		satPos.failureSsrClkTime = true;
		tracepdeex(4, std::cout, "No SSR corrections before %s sat=%s\n", ephTime.to_string().c_str(), satPos.Sat.id().c_str());
		return false;
	}

	auto& [t_c, ssrClk] = *clkIt;

	iodClk = ssrClk.iod;

	double tClk = (time - ssrClk.t0).to_double();

	validStart	= ssrClk.t0 - ssrClk.udi / 2;
	validStop	= ssrClk.t0 + ssrClk.udi / 2;

	/* ssr orbit and clock correction (ref [4]) */
	if (fabs(tClk) > ssrClk.udi * acsConfig.validity_interval_factor)
	{
		satPos.failureSsrClkUdi = true;

        tracepdeex(4, std::cout, "age of ssr error: %s sat=%s\n", time.to_string().c_str(), satPos.Sat.id().c_str());
		return false;
	}

	dclk	= ssrClk.dclk[0]
			+ ssrClk.dclk[1] * tClk
			+ ssrClk.dclk[2] * tClk * tClk;

	/* ssr highrate clock correction (ref [4]) */
	auto hrcIt = ssrMaps.ssrHRClk_map.lower_bound(ephTime);
	if (hrcIt != ssrMaps.ssrHRClk_map.end())
	{
		auto& [t_h, ssrHrc] = *hrcIt;

		double tHrc = (time - ssrHrc.t0).to_double();

		if 	(  ssrClk.iod == ssrHrc.iod
			&& fabs(tHrc) < ssrClk.udi * acsConfig.validity_interval_factor)
		{
			dclk += ssrHrc.hrclk;
		}
	}

	if (fabs(dclk) > MAXCCORSSR)
	{
		satPos.failureSsrClkMag = true;

		tracepdeex(2, std::cout,"SSR clk correction too large : %s %s dclk=%.1f\n", time.to_string().c_str(), satPos.Sat.id().c_str(), dclk);
		return 0;
	}

	tracepdeex(5, trace, "\n%s %2d %2d %lf %s  %s %lf", __FUNCTION__, iodClk, 0, tClk, validStart.to_string().c_str(), validStop.to_string().c_str(), dclk);

	return true;
}

/** satellite position and clock with ssr correction
*/
bool satPosSSR(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav)
{
	SSRMaps&		ssrMaps		= satPos.satNav_ptr->receivedSSR;
	SatSys&			Sat			= satPos.Sat;
	Vector3d&		rSat		= satPos.rSatCom;
	Vector3d&		satVel		= satPos.satVel;
	double&			satClk		= satPos.satClk;
	double&			satClkVel	= satPos.satClkVel;
	bool&			ephPosValid	= satPos.ephPosValid;
	bool&			ephClkValid	= satPos.ephClkValid;
	int&			iodeClk		= satPos.iodeClk;
	int&			iodePos		= satPos.iodePos;
	double&			posVar		= satPos.posVar;
	double&			clkVar		= satPos.satClkVar;

//     tracepdeex(4, trace, __FUNCTION__ ": time=%s sat=%2d\n", time.to_string().c_str(), satPos.Sat);
	ephPosValid = false;
	ephClkValid = false;

	int			iodPos;
	int			iodEph;
	int			iodClk;
	GTime		ephValidStart;
	GTime		ephValidStop;
	GTime		clkValidStart;
	GTime		clkValidStop;
	Vector3d	rSat0;
	Vector3d	dPos;
	double		satClk0;
	double		dClk;
	GTime		ephTime = time;

	bool		once = true;

	while (true)
	{
		bool posDeltaPass = ssrPosDelta(trace, time, ephTime, satPos, ssrMaps, dPos, iodPos, iodEph,	ephValidStart, ephValidStop);
		bool clkDeltaPass = ssrClkDelta(trace, time, ephTime, satPos, ssrMaps, dClk, iodClk,			clkValidStart, clkValidStop);

		if	(  posDeltaPass == false
			|| clkDeltaPass == false)
		{
			satPos.failureSSRFail = true;

			BOOST_LOG_TRIVIAL(warning)	<< "Warning: SSR Corrections not found for " << satPos.Sat.id();
			trace << "\n"				<< "Warning: SSR Corrections not found for " << satPos.Sat.id();

			return false;
		}

		if	( ephValidStart >= clkValidStop
			||clkValidStart >= ephValidStop)
		{
			BOOST_LOG_TRIVIAL(warning)	<< "Warning: Timing inconsistent for " << satPos.Sat.id() << "   :   " << ephValidStart.to_string(0) << "  -  " << ephValidStop.to_string(0) << "   " << clkValidStart.to_string(0) << "  -  " << clkValidStop.to_string(0);
			trace << "\n"				<< "Warning: Timing inconsistent for " << satPos.Sat.id() << "   :   " << ephValidStart.to_string(0) << "  -  " << ephValidStop.to_string(0) << "   " << clkValidStart.to_string(0) << "  -  " << clkValidStop.to_string(0);

			if (ephValidStart >= clkValidStop)		ephTime = clkValidStop - 0.5;
			if (clkValidStart >= ephValidStop)		ephTime = ephValidStop - 0.5;
			continue;
		}

		if (iodClk != iodPos)
		{
			satPos.failureIodeConsistency = true;

			BOOST_LOG_TRIVIAL(warning)	<< "Warning: IOD inconsistent for " << satPos.Sat.id() << iodClk << " " << iodPos;
			trace << "\n"				<< "Warning: IOD inconsistent for " << satPos.Sat.id() << iodClk << " " << iodPos;

			return false;
		}

		iodePos = iodEph;
		iodeClk = iodEph;

		bool pass = true;
		pass &= satPosBroadcast(trace, time, teph, Sat, rSat0,		satVel,		posVar, ephPosValid, iodePos, nav);
		pass &= satClkBroadcast(trace, time, teph, Sat, satClk0,	satClkVel,	clkVar, ephClkValid, iodeClk, nav);

		if (pass == false)
		{
			if (once)
			{
				once = false;

				if (clkValidStart < ephValidStart)	ephTime = clkValidStart - 0.5;
				else								ephTime = ephValidStart - 0.5;

				BOOST_LOG_TRIVIAL(warning)	<< "Warning: IODE BRDC not found for " << satPos.Sat.id() << " - adjusting ephTime";
				trace << "\n"				<< "Warning: IODE BRDC not found for " << satPos.Sat.id() << " - adjusting ephTime";

				continue;
			}
			satPos.failureBroadcastEph = true;

			BOOST_LOG_TRIVIAL(warning)	<< "Warning: IODE BRDC not found for " << satPos.Sat.id();
			trace << "\n"				<< "Warning: IODE BRDC not found for " << satPos.Sat.id();

			return false;
		}

		break;
	}

	tracepdeex(4, trace, "\nBRDCEPH %s    %s    %13.3f %13.3f %13.3f %11.3f %2d", time.to_string().c_str(), Sat.id().c_str(), rSat0[0], rSat0[1], rSat0[2], 1e9*satClk0, iodePos);

	Matrix3d rac2ecefMat = rac2ecef(rSat0, satVel);

	Vector3d dPosECEF = rac2ecefMat * dPos;

	rSat = rSat0 - dPosECEF;

	/* t_corr = t_sv - (dtSat(brdc) + dClk(ssr) / CLIGHT) (ref [10] eq.3.12-7) */
	satClk = satClk0 + dClk / CLIGHT;

	/* variance by ssr ura */
	double ura = -1;

	auto uraIt = ssrMaps.ssrUra_map.lower_bound(time);
	if (uraIt != ssrMaps.ssrUra_map.end())
	{
		auto& [t_u, ssrUra] = *uraIt;

		ura = ssrUra.ura;
	}

	clkVar = var_urassr(ura);
	posVar = 0;

	tracepdeex(3, trace, "\nSSR_EPH %s    %s    %13.3f %13.3f %13.3f %11.3f ", time.to_string().c_str(), Sat.id().c_str(), rSat[0], rSat[1], rSat[2], 1e9*satClk);

	tracepdeex(5, trace, "\n%s: %s sat=%s deph=%6.3f %6.3f %6.3f dclk=%6.3f var=%6.3f iode=%d clktimes:%s %s ephtimes%s %s\n",
		__FUNCTION__,
		time.to_string().c_str(),
		Sat.id().c_str(),
		dPos[0],
		dPos[1],
		dPos[2],
		dClk,
		clkVar,
		iodEph,
		clkValidStart	.to_string().c_str(),
		clkValidStop	.to_string().c_str(),
		ephValidStart	.to_string().c_str(),
		ephValidStop	.to_string().c_str());


	if (round(time.bigTime) == time.bigTime)
	{
		traceJson(1, nullStream, time,
			{
				{"data",	__FUNCTION__		},
				{"Sat",		satPos.Sat.id()		}
			},
			{
				{"rSat0[0]",		rSat0[0]},
				{"rSat0[1]",		rSat0[1]},
				{"rSat0[2]",		rSat0[2]},
				{"rSat[0]",			rSat[0]},
				{"rSat[1]",			rSat[1]},
				{"rSat[2]",			rSat[2]},
				{"dPos[0]",			dPos[0]},
				{"dPos[1]",			dPos[1]},
				{"dPos[2]",			dPos[2]},
				{"satClk0",			satClk0},
				{"satClk",			satClk},
				{"dClk",			dClk},
				{"iode",			iodEph},
				{"iodClk",			iodClk},
				{"iodPos",			iodPos},
				{"clkValidStart",	(long int)clkValidStart.bigTime},
				{"ephValidStart",	(long int)ephValidStart.bigTime}
			});
	}

	return true;
}
