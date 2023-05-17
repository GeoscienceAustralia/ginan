/* prototype functions for compact SSR and IGS SSR messages */

#pragma once

#include "rtcmDecoder.hpp"
#include "navigation.hpp"
#include "ntripTrace.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "enums.h"

const int igsUpdateInterval[16] =
{
	1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
};
void decodecompactSSR(
	vector<unsigned char>&	data,
	GTime					now);

void decodeigsSSR(
	vector<unsigned char>&	data,
	GTime					now);

// vector<uint8_t>  encodecompactMSK( 
//     map<SatSys, SSROut>         OrbClkMap,
//     map<SatSys, SSRCodeBias>    CodBiaMap,
// 	map<SatSys, SSRPhasBias>    PhsBiaMap);

// vector<uint8_t>  encodecompactORB( 
//     map<SatSys, SSROut>         OrbClkMap);

// vector<uint8_t>  encodecompactCLK( 
//     map<SatSys, SSROut>         OrbClkMap,
//     bool						last);

// vector<uint8_t>  encodecompactCOD( 
//     map<SatSys, SSRCodeBias>    CodBiaMap,
//     bool						last);

// vector<uint8_t>  encodecompactPHS(
// 	map<SatSys, SSRPhasBias>    PhsBiaMap,
//     bool						last);

// vector<uint8_t>  encodecompactION(
// 	SSRAtm                      ssrAtm,
//     bool						last);

unsigned short IGS_SSR_subtype (
	int typ, 
	E_Sys sys);

int IGS_SSR_group(
	IgsSSRSubtype	subTyp, 
	E_Sys&			sys);

vector<uint8_t>  encodeIGS_ORB( 
	map<SatSys, SSROut>&		OrbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_CLK( 
	map<SatSys, SSROut>&		OrbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_CMB( 
	map<SatSys, SSROut>&		OrbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_HRC( 
	map<SatSys, SSROut>&		OrbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_COD( 
	map<SatSys, SSRCodeBias>&	CodBiaMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_PHS(
	map<SatSys, SSRPhasBias>&	PhsBiaMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_URA( 
	map<SatSys, SSRUra>&		URAMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_ATM(
	SSRAtm						ssrAtm,
	bool						last);

