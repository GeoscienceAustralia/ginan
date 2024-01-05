
#pragma once

#include "rtcmEncoder.hpp"
#include "mongo.hpp"

struct SSRMeta;

GTime mongoReadLastClock();

SsrOutMap mongoReadOrbClk(
	GTime		referenceTime,
	SSRMeta&	ssrMeta,
	int			masterIod,
	E_Sys		targetSys);

SsrCBMap mongoReadCodeBias(
	SSRMeta&	ssrMeta,
	int			masterIod,
	E_Sys		targetSys);

SsrPBMap mongoReadPhaseBias(
	SSRMeta&	ssrMeta,
	int			masterIod,
	E_Sys		targetSys);

Eph mongoReadEphemeris(
	GTime			targetTime,
	SatSys			Sat,
	RtcmMessageType	rtcmMessCode);

Geph mongoReadGloEphemeris(
	GTime			targetTime,
	SatSys			Sat);

SSRAtm mongoReadIGSIonosphere(
			GTime		time,
	const	SSRMeta&	ssrMeta,
			int			masterIod);

SSRAtm mongoReadCmpAtmosphere(
	GTime	time,
	SSRMeta	ssrMeta);

map<SatSys, map<GTime, Vector6d>> mongoReadOrbits(
	GTime	time	= GTime::noTime(),
	SatSys	Sat		= SatSys(),
	bool	remote	= false,
	double*	var_ptr	= nullptr);

map<string, map<GTime, tuple<double, double>>> mongoReadClocks(
	GTime	time	= GTime::noTime(),
	string	str		= "",
	bool	remote	= false);

class KF;
struct KFState;

void mongoReadFilter(
	KFState&				kfState,
	GTime					time	= GTime::noTime(),
	const vector<KF>&		types	= {},
	const string&			Sat		= "",
	const string&			str		= "");
