#ifndef READMONGO_H
#define READMONGO_H

#include <rtcmEncoder.hpp>

#include <bsoncxx/stdx/string_view.hpp>
#include <bsoncxx/types.hpp>

#include "mongo.hpp"
#include "ssr.hpp"


SsrClkOutMap mongoReadClocks(
	GTime	curTime,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys);

SsrCBMap mongoReadCodeBias(
	GTime	curTime,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys);

map<SatSys, SSROut> mongoReadSSRData(
	GTime	targetTime,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys);

SsrPBMap mongoReadPhaseBias(
	GTime	curTime,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys);


#endif 
