/* prototype functions for compact SSR and IGS SSR messages */

#pragma once

#include <vector>
#include <map>

using std::vector;
using std::map;

#include "gTime.hpp"
#include "enums.h"

struct SSRUra;
struct SSRAtm;
struct SSROut;
struct SSRPhasBias;
struct SSRCodeBias;

const int ssrUdi[16] =
{
	1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
};

extern map<E_Sys, map<int, E_ObsCode>> cmpSSRIndex2Code;
extern map<E_Sys, map<int, E_ObsCode>> igsSSRIndex2Code;
extern map<E_Sys, map<E_ObsCode, int>> igsSSRCode2Index;

int decodecompactSSR(
	vector<unsigned char>&	data,
	GTime					now);

E_ReturnType decodeigsSSR(
	vector<unsigned char>&	data,
	GTime					now);

vector<uint8_t>  encodecompactMSK(
	map<SatSys, SSROut>&			orbClkMap,
	map<SatSys, SSRCodeBias>&		codBiaMap,
	map<SatSys, SSRPhasBias>&		phsBiaMap,
	SSRAtm							ssrAtm,
	int								updateIntIndex);

vector<uint8_t>  encodecompactORB(
	map<SatSys, SSROut>&			orbClkMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactCLK(
	map<SatSys, SSROut>&			orbClkMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactURA(
	map<SatSys, SSROut>&			orbClkMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactCMB(
	map<SatSys, SSROut>&			orbClkMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactCOD(
	map<SatSys, SSRCodeBias>&		codBiaMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactPHS(
	map<SatSys, SSRPhasBias>&		phsBiaMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactBIA(
	map<SatSys, SSRCodeBias>&		codBiaMap,
	map<SatSys, SSRPhasBias>&		phsBiaMap,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactTEC(
	SSRMeta&						ssrMeta,
	int								regId,
	SSRAtmRegion&					ssrAtmReg,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactGRD(
	SSRMeta&						ssrMeta,
	int								regId,
	SSRAtmRegion&					ssrAtmReg,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactATM(
	SSRMeta&						ssrMeta,
	int								regId,
	SSRAtmRegion&					ssrAtmReg,
	int								updateIntIndex,
	bool							last);

vector<uint8_t>  encodecompactSRV(
	SSRAtm&							ssrAtm);



unsigned short IGS_SSR_subtype(
	IgsSSRSubtype	type,
	E_Sys			sys);

IgsSSRSubtype IGS_SSR_group(
	IgsSSRSubtype	subType,
	E_Sys&			sys);

vector<uint8_t>  encodeIGS_ORB(
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_CLK(
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_CMB(
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_HRC(
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_COD(
	map<SatSys, SSRCodeBias>&	codBiaMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_PHS(
	map<SatSys, SSRPhasBias>&	phsBiaMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_URA(
	map<SatSys, SSRUra>&		uraMap,
	E_Sys						sys,
	bool						last);

vector<uint8_t>  encodeIGS_ATM(
	SSRAtm&						ssrAtm,
	bool						last);

double checkPhaseDisc(
	SatSys		Sat,
	E_ObsCode	code,
	double		bias,
	int			disc,
	int 		regn);
