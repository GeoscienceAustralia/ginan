
#pragma once

#include "biases.hpp"
#include "gTime.hpp"

#define STEC2DELAY  4.48397972589608


#define MIN_NSAT_REC 		3

struct ReceiverMap;

extern bool ionoConfigured;

void obsIonoData(
	Trace&		trace,
	Receiver&	rec);

void obsIonoDataFromFilter(
	Trace&			trace,
	ReceiverMap&	receiverMap,
	KFState&		measKFstate);

void filterIonosphere(
	Trace&			trace,
	KFState&		kfState,
	ReceiverMap&	receiverMap,
	GTime 			time);

void ionosphereSsrUpdate(
	Trace&		trace,
	KFState&	kfState);

bool queryBiasDCB(
	Trace&		trace,
	KFState&	kfState,
	SatSys		Sat,
	string		Rec,
	E_ObsCode	code,
	double&		bias,
	double&		var);

void ionexFileWrite(
	Trace&		trace,
	string		filename,
	GTime		time,
	KFState&	kfState);

void writeIonStec(
	string			filename,
	KFState&		kFstate);

bool configIonModel(	  Trace& trace);
int  configIonModelSphhar(Trace& trace);
int  configIonModelSphcap(Trace& trace);
int  configIonModelBsplin(Trace& trace);
int  configIonModelLocal_(Trace& trace);

bool ippCheckSphhar(GTime time, VectorPos& Ion_pp);
bool ippCheckSphcap(GTime time, VectorPos& Ion_pp);
bool ippCheckBsplin(GTime time, VectorPos& Ion_pp);
bool ippCheckLocal (GTime time, VectorPos& Ion_pp);

double ionModelCoef (Trace& trace, int ind, IonoObs& obs, bool slant = true);
double ionCoefSphhar(Trace& trace, int ind, IonoObs& obs, bool slant = true);
double ionCoefSphcap(Trace& trace, int ind, IonoObs& obs, bool slant = true);
double ionCoefBsplin(Trace& trace, int ind, IonoObs& obs, bool slant = true);
double ionCoefLocal (Trace& trace, int ind, IonoObs& obs);

double ionVtecSphhar(Trace& trace, GTime time, VectorPos& ionPP, int layer, double& var, KFState& kfState);
double ionVtecSphcap(Trace& trace, GTime time, VectorPos& ionPP, int layer, double& var, KFState& kfState);
double ionVtecBsplin(Trace& trace, GTime time, VectorPos& ionPP, int layer, double& var, KFState& kfState);

int checkSSRRegion (VectorPos&	pos);

void ionOutputSphcal(Trace& trace, KFState& kfState);
void ionOutputLocal (Trace& trace, KFState& kfState);

double	getSSRIono(   Trace& trace, GTime time,					Vector3d& rRec, AzEl& azel,					double& var, SatSys& Sat);
bool	getIGSSSRIono(Trace& trace, GTime time, SSRAtm& ssrAtm,	Vector3d& rRec, AzEl& azel,	double& iono,	double& var);
bool	getCmpSSRIono(Trace& trace, GTime time, SSRAtm& ssrAtm,	Vector3d& rRec,				double& iono,	double& var, SatSys  Sat);

bool configAtmosRegions(
	Trace&				trace,
	ReceiverMap&		receiverMap);
