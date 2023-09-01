
#pragma once

#include "station.hpp"
#include "biases.hpp"
#include "gTime.hpp"

#define STEC2DELAY  4.48397972589608


#define MIN_NSAT_STA 		3 


int  configIonModel();

double getSSRIono(GTime time, Vector3d& rRec, Vector3d& rSat, double& variance, SatSys& Sat);

void obsIonoData(
	Trace&		trace, 
	Station&	rec);

void obsIonoDataFromFilter(
	Trace&		trace, 
	StationMap&	stationMap, 
	KFState&	measKFstate);

void filterIonosphere(
	Trace&			trace,		
	KFState&		kfState,
	StationMap&		stations,		
	GTime 			time);			

void ionosphereSsrUpdate(Trace& trace, KFState& kfState);

bool queryBiasDCB(
	Trace&		trace,	
	KFState&	kfState,
	SatSys		Sat,	
	string		Rec,	
	E_FType		freq,	
	double&		bias,	
	double&		var);	

bool ionexFileWrite(
	string		filename, 
	GTime		time, 
	KFState&	kfState);


void writeIONStec(Trace& trace, string filename, map<string, Station>& stations, GTime time);
void writeSTECfromRTS(
	string			filename,
	KFState&		kFstate);
	
int configIonModelSphhar();
int configIonModelSphcap();
int configIonModelBsplin();

bool ippCheckSphhar(GTime time, VectorPos& Ion_pp);
bool ippCheckSphcap(GTime time, VectorPos& Ion_pp);
bool ippCheckBsplin(GTime time, VectorPos& Ion_pp);
bool ippCheckLocal (GTime time, VectorPos& Ion_pp);

double ionModelCoef (Trace& trace, int ind, IonoObs& obs, bool slant = true);
double ionCoefSphhar(int ind, IonoObs& obs, bool slant = true);
double ionCoefSphcap(int ind, IonoObs& obs, bool slant = true);
double ionCoefBsplin(int ind, IonoObs& obs, bool slant = true);
double ionCoefLocal (Trace& trace, int ind, IonoObs& obs);

double ionVtecSphhar(GTime time, VectorPos& ionPP, int layer, double& vari, KFState& kfState);
double ionVtecSphcap(GTime time, VectorPos& ionPP, int layer, double& vari, KFState& kfState);
double ionVtecBsplin(GTime time, VectorPos& ionPP, int layer, double& vari, KFState& kfState);

void ionOutputSphcal(Trace& trace, KFState& kfState);
void ionOutputLocal (Trace& trace, KFState& kfState);

bool getIGSSSRIono(GTime time, SSRAtm& ssrAtm, Vector3d& rSat, Vector3d&	rRec, double& 	iono, double& var);
bool getCmpSSRIono(GTime time, SSRAtm& ssrAtm, Vector3d& rRec,						double& iono, double& var, SatSys Sat);



int defineLocalIonoBasis();
bool configLocalIonoFromFile();
bool configAtmosRegions(
    map<string, Station>&		stationMap);