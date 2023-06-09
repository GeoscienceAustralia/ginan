
#pragma once

#include "observations.hpp"
#include "navigation.hpp"
#include "biasSINEX.hpp"
#include "station.hpp"
#include "gTime.hpp"

#define STEC2DELAY  4.48397972589608


#define MIN_NSAT_STA 		3 


/* Global variables */
extern KFState		iono_KFState;

/* General functions */
int  configIonModel();

double getSSRIono(GTime time, Vector3d& rRec, Vector3d& rSat, double& variance, SatSys& Sat);
void  update_receivr_measr (Trace& trace, Station& rec);
int  ginan2IonoMeas (Trace& trace, StationMap& stationMap, KFState& measKFstate);

void updateIonosphereModel (Trace& trace, string ionstecFilename, string ionexFilename, StationMap& stationMap, GTime time);
void ionosphereSsrUpdate(Trace& trace, KFState& kfState);
bool overwriteIonoKF(KFState& kfState);
bool queryBiasDCB (Trace& trace, SatSys Sat, string Rec, E_FType freq, double& bias, double& vari);
bool ionexFileWrite(
	string		filename, 
	GTime		time, 
	KFState&	kfState);


void writeReceiverMeasurements(Trace& trace, string filename, map<string, Station>& stations, GTime time);

int configIonModelSphhar();
int configIonModelSphcap();
int configIonModelBsplin();

bool ippCheckSphhar(GTime time, VectorPos& Ion_pp);
bool ippCheckSphcap(GTime time, VectorPos& Ion_pp);
bool ippCheckBsplin(GTime time, VectorPos& Ion_pp);
int  ippCheckLocal (GTime time, VectorPos& Ion_pp);

double ionModelCoef (int ind, GObs&	obs, bool slant);
double ionCoefSphhar(int ind, GObs& obs, bool slant = true);
double ionCoefSphcap(int ind, GObs& obs, bool slant = true);
double ionCoefBsplin(int ind, GObs& obs, bool slant = true);
double ionCoefLocal (int ind, GObs& obs);

double ionVtecSphhar(GTime time, VectorPos& ionPP, int layer, double& vari, KFState& kfState);
double ionVtecSphcap(GTime time, VectorPos& ionPP, int layer, double& vari, KFState& kfState);
double ionVtecBsplin(GTime time, VectorPos& ionPP, int layer, double& vari, KFState& kfState);

void ionOutputSphcal(Trace& trace, KFState& kfState);
void ionOutputLocal (Trace& trace, KFState& kfState);

bool getIGSSSRIono(GTime time, SSRAtm& ssrAtm, Vector3d& rSat, Vector3d&	rRec, double& 	iono, double& var);
bool getCmpSSRIono(GTime time, SSRAtm& ssrAtm, Vector3d& rRec,						double& iono, double& var, SatSys Sat);



int defineLocalIonoBasis();
bool configLocalIonoFromFile();

