
// #pragma GCC optimize ("O0")

#include <iostream>
#include <string>
#include <array>
#include <map>
#include <ctype.h>

using std::string;
using std::array;
using std::map;

#include <boost/log/trivial.hpp>


#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "constants.hpp"
#include "mongoRead.hpp"
#include "ephemeris.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "planets.hpp"
#include "common.hpp"
#include "biases.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"

#define NMAX		10				/* order of polynomial interpolation */
#define MAXDTE		900.0			/* max time difference to ephem time (s) */
#define EXTERR_CLK	1E-3			/* extrapolation error for clock (m/s) */
#define EXTERR_EPH	5E-7			/* extrapolation error for ephem (m/s^2) */

/** read dcb parameters file
*/
int readdcb(
	string	file)
{
	std::ifstream inputStream(file);
	if (!inputStream)
	{
//         trace(2,"dcb parameters file open error: %s\n",file);
		return 0;
	}

	BiasEntry entry;
//     trace(3,"readdcbf: file=%s\n",file);

	entry.tini.bigTime	= 3;
	entry.measType		= CODE;
	entry.source		= "dcb";

	string line;
	string type;
	while (std::getline(inputStream, line))
	{
		char* buff = &line[0];

		if      (strstr(buff,"DIFFERENTIAL (P1-P2) CODE BIASES"))	type = "P1_P2";
		else if (strstr(buff,"DIFFERENTIAL (P1-C1) CODE BIASES"))	type = "P1_C1";
		else if (strstr(buff,"DIFFERENTIAL (P2-C2) CODE BIASES"))	type = "P2_C2";

		char str1[32] = "";
		char str2[32] = "";

		if	( type.empty()
			||sscanf(buff,"%31s %31s", str1, str2) < 1)
			continue;

		double cbias	= str2num(buff,26,9);
		double rms		= str2num(buff,38,9);

		entry.bias = cbias   * 1E-9 * CLIGHT; /* ns -> m */
		entry.var  = SQR(rms * 1E-9 * CLIGHT);

		SatSys Sat(str1);

		if (Sat.sys == +E_Sys::GPS)
		{
			if      (type == "P1_P2")   { entry.cod1 = E_ObsCode::L1W;  entry.cod2 = E_ObsCode::L2W; }
			else if (type == "P1_C1")   { entry.cod1 = E_ObsCode::L1W;  entry.cod2 = E_ObsCode::L1C; }
			else if (type == "P2_C2")   { entry.cod1 = E_ObsCode::L2W;  entry.cod2 = E_ObsCode::L2D; }
			else continue;
		}
		else if (Sat.sys == +E_Sys::GLO)
		{
			if      (type == "P1_P2")   { entry.cod1 = E_ObsCode::L1P;  entry.cod2 = E_ObsCode::L2P; }
			else if (type == "P1_C1")   { entry.cod1 = E_ObsCode::L1P;  entry.cod2 = E_ObsCode::L1C; }
			else if (type == "P2_C2")   { entry.cod1 = E_ObsCode::L2P;  entry.cod2 = E_ObsCode::L2C; }
			else continue;
		}

		string id;
		if  ( !strcmp(str1,"G")
			||!strcmp(str1,"R"))
		{
			/* receiver dcb */
			entry.Sat  = Sat;
			entry.name = str2;
			id = str2;
		}
		else if (Sat)
		{
			/* satellite dcb */
			entry.Sat  = Sat;
			entry.name = "";
			id = str1;
		}

		if	( Sat.sys == +E_Sys::GLO
			&&Sat.prn == 0)
		{
			// this seems to be a receiver
			// for ambiguous GLO receiver bias id (i.e. PRN not specified), duplicate bias entry for each satellite
			for (int prn = 1; prn <= NSATGLO; prn++)
			{
				Sat.prn	= prn;
				id = entry.name + ":" + Sat.id();
				// entry.Sat = Sat;
				pushBiasEntry(id, entry);
			}
		}
		else if	( Sat.sys == +E_Sys::GLO
				&&Sat.prn != 0)
		{
			// this can be a receiver or satellite
			id = id + ":" + Sat.id();
			pushBiasEntry(id, entry);
		}
		else
		{
			// this can be a receiver or satellite
			id = id + ":" + Sat.sysChar();
			pushBiasEntry(id, entry);
		}
	}

	return 1;
}

/** polynomial interpolation by Neville's algorithm
*/
double interpolate(const double *x, double *y, int n)
{
	for (int j=1; j < n;		j++)
	for (int i=0; i < n - j;	i++)
	{
		y[i] = (x[i+j] * y[i] - x[i] * y[i+1]) / (x[i+j] - x[i]);
	}

	return y[0];
}

/** satellite position by precise ephemeris
*/
bool pephpos(
	Trace&		trace,
	GTime		time,
	SatSys		Sat,
	Navigation&	nav,
	Vector3d&	rSat,
	double*		vare)
{
//     trace(4,"%s : time=%s sat=%s\n",__FUNCTION__, time.to_string(3).c_str(),Sat.id().c_str());

	rSat = Vector3d::Zero();

	if (nav.pephMap.empty())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Looking for precise positions, but no precise ephemerides found";

		return false;
	}

	auto it = nav.pephMap.find(Sat.id());
	if (it == nav.pephMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Looking for precise position, but no precise ephemerides found for " << Sat.id();

		return false;
	}

	auto& [id, pephMap] = *it;

	auto firstTime	= pephMap.begin()	->first;
	auto lastTime	= pephMap.rbegin()	->first;

	if	( (pephMap.size()	< NMAX + 1)
		||(time	< firstTime	- MAXDTE)
		||(time	> lastTime	+ MAXDTE))
	{
		tracepdeex(3, std::cout, "\nNo precise ephemeris for %s at %s, ephemerides cover %s to %s",
				Sat.id()					.c_str(),
				time		.to_string()	.c_str(),
				firstTime	.to_string()	.c_str(),
				lastTime	.to_string()	.c_str());

		return false;
	}

// 	//search for the ephemeris in the map

	auto peph_it = pephMap.lower_bound(time);
	if (peph_it == pephMap.end())
	{
		peph_it--;
	}

	auto middle0 = peph_it;

	//go forward a few steps to make sure we're far from the end of the map.
	for (int i = 0; i < NMAX/2; i++)
	{
		peph_it++;
		if (peph_it == pephMap.end())
		{
			break;
		}
	}

	//go backward a few steps to make sure we're far from the beginning of the map
	for (int i = 0; i <= NMAX; i++)
	{
		peph_it--;
		if (peph_it == pephMap.begin())
		{
			break;
		}
	}

	auto begin = peph_it;

	vector<double>		t(NMAX+1);
	vector<Vector3d>	p(NMAX+1);
	double c[2];
	double s[3];

	//get interpolation parameters and check all ephemerides have values.
	peph_it = begin;
	for (int i = 0; i <= NMAX; i++, peph_it++)
	{
		Peph& peph = peph_it->second;
		if (peph.pos.isZero())
		{
//             trace(3,"prec ephem outage %s sat=%s\n",time.to_string().c_str(), Sat.id().c_str());
			return false;
		}

		auto& pos = peph.pos;

		t[i] = (peph.time - time).to_double();
		p[i] = pos;
	}

	rSat = interpolate(t, p);

	if (vare)
	{
		double std = middle0->second.posStd.norm();

		/* extrapolation error for orbit */
		if      (t[0   ] > 0) std += EXTERR_EPH * SQR(t[0   ]) / 2;
		else if (t[NMAX] < 0) std += EXTERR_EPH * SQR(t[NMAX]) / 2;

		*vare = SQR(std);
	}

	return true;
}

template<typename TYPE>
bool pclkMapClk(
	Trace&		trace,
	GTime		time,
	string		id,
	Navigation&	nav,
	double&		clk,
	double*		varc,
	TYPE&		pclkMaps)
{
	auto it = pclkMaps.find(id);
	if (it == pclkMaps.end())
	{
		return false;
	}

	auto& [key, pclkMap] = *it;

	if	( (pclkMap.size() < 2)
		||(time	< pclkMap.begin()	->first - MAXDTE)
		||(time	> pclkMap.rbegin()	->first + MAXDTE))
	{
		BOOST_LOG_TRIVIAL(debug) << "no prec clock " << time.to_string() << " for " << id;

		return false;
	}

	auto pclk_it = pclkMap.lower_bound(time);
	if (pclk_it == pclkMap.end())
	{
		pclk_it--;
	}

	auto middle0_it = pclk_it;

	auto middle1_it = middle0_it;
	if (middle0_it != pclkMap.begin())
	{
		middle0_it--;
	}

	auto& [time0, middle0] = *middle0_it;
	auto& [time1, middle1] = *middle1_it;

	//linear interpolation
	double t[2];
	double c[2];
	t[0] = (time - time0).to_double();
	t[1] = (time - time1).to_double();
	c[0] = middle0.clk;
	c[1] = middle1.clk;

	bool use0 = true;
	bool use1 = true;

	if (c[0] == INVALID_CLOCK_VALUE)		{	use0 = false;	}
	if (c[1] == INVALID_CLOCK_VALUE)		{	use1 = false;	}
	if (t[0] <= 0)							{	use1 = false;	}
	if (t[1] >= 0)							{	use0 = false;	}

	if	(  use0 == false
		&& use1 == false)
	{
		BOOST_LOG_TRIVIAL(debug) << "Precise clock outage " << time.to_string() << " for " << id;

		clk = 0;

		return false;
	}

	double std = 0;

	if		(use0 && use1)	{	clk = (c[1] * t[0] - c[0] * t[1]) / (t[0] - t[1]);		double inv0 = 1 / middle0.clkStd	* CLIGHT + EXTERR_CLK * fabs(t[0]);
																						double inv1 = 1 / middle1.clkStd	* CLIGHT + EXTERR_CLK * fabs(t[1]);
																						std			= 1 / (inv0 + inv1);											}
	else if (use0)			{	clk = c[0];												std			= middle0.clkStd		* CLIGHT + EXTERR_CLK * fabs(t[0]);		}
	else if (use1)			{	clk = c[1];												std			= middle1.clkStd		* CLIGHT + EXTERR_CLK * fabs(t[1]);		}

	if (varc)
		*varc = SQR(std);

	return true;
}

/** clock by precise clock
*/
bool pephclk(
	Trace&		trace,
	GTime		time,
	string		id,
	Navigation&	nav,
	double&		clk,
	double*		varc)
{
//	BOOST_LOG_TRIVIAL(debug) << "pephclk : time=" << time.to_string(3) << " id=" << id;

	bool pass;
	pass = pclkMapClk(trace, time, id, nav, clk, varc, nav.pclkMap);		if (pass)	return true;
	pass = pclkMapClk(trace, time, id, nav, clk, varc, nav.pephMap);		if (pass)	return true;

	return false;
}

/** satellite antenna phase center offset in ecef
*/
VectorEcef satAntOff(
	Trace&				trace,			///< Trace file to output to
	GTime				time,			///< Solution time
	AttStatus&			attStatus,		///< attitude status
	SatSys& 			Sat,			///< Satellite ID
	map<int, double>&	lamMap)			///< Lambda (wavelengths) map
{
	tracepdeex(4, trace, "\n%-10s: time=%s sat=%s", __FUNCTION__, time.to_string().c_str(), Sat.id().c_str());

	VectorEcef dAnt;

	E_FType j;
	E_FType k;
	E_FType l;
	E_Sys sys = Sat.sys;
	if (!satFreqs(sys,j,k,l))
			return dAnt;

	if 	( lamMap[j] == 0
		||lamMap[k] == 0)
	{
		return dAnt;
	}

	double gamma	= SQR(lamMap[k]) / SQR(lamMap[j]);
	double C1		= gamma	/ (gamma - 1);
	double C2		= -1	/ (gamma - 1);

	/* iono-free LC */
	double varDummy = 0;
	Vector3d pcoJ = antPco(Sat.id(), Sat.sys, j, time, varDummy, E_Radio::TRANSMITTER);
	Vector3d pcoK = antPco(Sat.id(), Sat.sys, k, time, varDummy, E_Radio::TRANSMITTER);

	VectorEcef dant1 = body2ecef(attStatus, pcoJ);
	VectorEcef dant2 = body2ecef(attStatus, pcoK);

	dAnt	= C1 * dant1
			+ C2 * dant2;

	return dAnt;
}

bool satClkPrecise(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	double&		clk,
	double&		clkVel,
	double&		clkVar,
	Navigation&	nav)
{
	clk		= 0;
	clkVel	= 0;

	tracepdeex(4, trace, "\n%-10s: time=%s sat=%s", __FUNCTION__, time.to_string().c_str(), Sat.id().c_str());

	double tt = 1E-3;

	double clk2 = 0;

	bool pass	=	pephclk(trace, time,		Sat, nav,	clk,		&clkVar)
				&&	pephclk(trace, time + tt,	Sat, nav,	clk2);

	if 	(  pass	== false
		|| clk	== INVALID_CLOCK_VALUE)
	{
		tracepdeex(4, trace, " - pephclk failed");
		clk = 0;

		return false;
	}

	clkVel	= (clk2 - clk) / tt;

	return true;
}


/** Satellite position/clock by precise ephemeris/clock
*/
bool satPosPrecise(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	Vector3d&	rSat,
	Vector3d&	satVel,
	double&		ephVar,
	Navigation&	nav)
{
	rSat	= Vector3d::Zero();
	satVel	= Vector3d::Zero();

	tracepdeex(4, trace, "\n%-10s: time=%s sat=%s", __FUNCTION__, time.to_string().c_str(), Sat.id().c_str());

	double tt = 10e-3;

	Vector3d rSat1 = Vector3d::Zero();
	Vector3d rSat2 = Vector3d::Zero();

	bool pass	=	pephpos(trace, time - tt,	Sat, nav, rSat1,	&ephVar)
				&&	pephpos(trace, time + tt,	Sat, nav, rSat2);

	if 	(pass == false)
	{
		tracepdeex(4, trace, " - pephpos failed");

		return false;
	}

	rSat	= (rSat2 + rSat1) /  2;
	satVel	= (rSat2 - rSat1) / (2 * tt);

	return true;
}

bool satPosPrecise(
	Trace&		trace,
	GTime		time,
	SatPos&		satPos,
	Navigation&	nav)
{
	return satPosPrecise(
		trace,
		time,
		satPos.Sat,
		satPos.rSatCom,
		satPos.satVel,
		satPos.posVar,
		nav);
}

bool satClkPrecise(
	Trace&		trace,
	GTime		time,
	SatPos&		satPos,
	Navigation&	nav)
{
	return satClkPrecise(
		trace,
		time,
		satPos.Sat,
		satPos.satClk,
		satPos.satClkVel,
		satPos.satClkVar,
		nav);
}
