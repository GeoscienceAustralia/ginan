
#pragma once

#include <map>

using std::map;



#include "algebra.hpp"
#include "satStat.hpp"
#include "gTime.hpp"

//forward declarations
struct Station;
struct Solution;
struct Vmf3;
struct gptgrid_t;
struct AttStatus;
struct PhaseCenterData;
using StationMap = map<string, Station>;




void removeUnmeasuredAmbiguities(
	Trace&				trace,
	KFState&			kfState,
	map<KFKey, bool>	measuredStates);

void pppos(
	Trace&		trace,
	ObsList&	obsList,
	Station&	rec);

void pppoutstat(
	Trace&		trace,
	KFState&	kfState,
	string		id);

void pppomc(
	Trace&			trace,
	ObsList&		obsList,
	gptgrid_t&		gptg,
	Station&		rec,			
	Vmf3&			vmf3);		

void sppos(
	Trace&		trace,
	ObsList&	obsList,
	Solution&	sol,
	string		id);

void testEclipse(
	ObsList&	obsList);

void pppCorrections(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	rRec,
	Station&	rec);
	
void PPP(
	Trace&			trace,
	StationMap&		stations,
	KFState&		kfState);

void corr_meas(
	Trace&		trace,
	GObs&		obs,
	E_FType		ft,
	double		dAntRec,
	double		dAntSat,
	double		phw,
	bool		oldSchool = true);

double sbstropcorr(
	GTime			time,
	VectorEcef&		rRec,
	double			el,
	double*			var = nullptr);

void phaseWindup(
	GObs&		obs,
	Station&	rec,
	double&		phw);

double gradMapFn(
	double		el);

double trop_model_prec(
	GTime		time,
	VectorPos&	pos,
	double*		azel,
	double*		tropStates,
	double*		dTropDx,
	double&		var);

int ionoModel(
	GTime		time,
	VectorPos&	pos,
	double*		azel,
	double		ionoState,
	double&		dion,
	double&		var);

void outputDeltaClocks(
	StationMap& stationMap);

void outputApriori(
	StationMap& stationMap);

void outputPPPSolution(
	string		filename,
	Station&	rec);

void selectAprioriSource(
	Station&	rec,
	bool&		sppUsed);

void postFilterChecks(
	KFMeas&	kfMeas);

bool deweightMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool deweightStationMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool countSignalErrors(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool incrementPhaseSignalError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool resetPhaseSignalError(
	KFMeas&		kfMeas,
	int			index);

bool resetPhaseSignalOutage(
	KFMeas&		kfMeas,
	int			index);

bool deweightByState(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey);

bool clockGlitchReaction(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey);

bool orbitGlitchReaction(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey);

bool orbitMeasReaction(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);



void stationPPP(
			Trace&				netTrace,		
			Station&			rec,			
	const	KFState&			kfState,		
			KFMeasEntryList&	kfMeasEntryList);


void orbitPseudoObs(
			Trace&				netTrace,			
			Station&			rec,				
	const	KFState&			kfState,			
			KFMeasEntryList&	kfMeasEntryList);	

void stationPseudoObs(
			Trace&				netTrace,			
			Station&			rec,				
	const	KFState&			kfState,			
			KFMeasEntryList&	kfMeasEntryList,
			StationMap&			stationMap,
			MatrixXd*			R_ptr = nullptr);

void stationSlr(
			Trace&				netTrace,		
			Station&			rec,			
	const	KFState&			kfState,		
			KFMeasEntryList&	kfMeasEntryList);		

Matrix3d stationEopPartials(
	Vector3d&	rRec);


bool satQuat(
	GObs&				obs,
	vector<E_Source>	attitudeTypes,
	Quaterniond&		quat,
	bool				origGal	= false);

void satYaw(
	GObs&		obs,
	AttStatus&	attStatus);
	
int PPP_AR(
	Trace&		trace,		
	KFState&	kfState);

bool copyFixedKF(
	KFState& fixed);

bool queryBiasUC(
	Trace&		trace,	
	GTime		time,		
	SatSys		sat,		
	string		rec,		
	E_ObsCode	code,	
	double&		bias,	
	double&		vari,	
	E_MeasType	typ);

void biasPseudoObs(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

void ambgPseudoObs(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

void ionoPseudoObs (
	Trace&				trace,
	StationMap&			stations,	
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);
