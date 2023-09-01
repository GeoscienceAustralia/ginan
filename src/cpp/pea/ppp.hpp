
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
struct StationMap;




void removeUnmeasuredAmbiguities(
	Trace&				trace,
	KFState&			kfState,
	map<KFKey, bool>	measuredStates);

void outputPppNmea(
	Trace&		trace,
	KFState&	kfState,
	string		id);

void SPP(
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

void phaseWindup(
	GObs&		obs,
	Station&	rec,
	double&		phw);

int ionoModel(
	GTime		time,
	VectorPos&	pos,
	double*		azel,
	double		ionoState,
	double&		dion,
	double&		var);

void outputApriori(
	StationMap& stationMap);

void outputPPPSolution(
	string		filename,
	KFState&	kfState,
	Station&	rec);

void gpggaout(
	string outfile, 
	KFState& KfState, 
	string recId, 
	int solStat, 
	int numSat, 
	double hdop, 
	bool lng);

void selectAprioriSource(
	Station&	rec,
	bool&		sppUsed);

void postFilterChecks(
	KFMeas&	kfMeas);

bool deweightMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit);

bool pseudoMeasTest(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit);

bool deweightStationMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit);

bool countSignalErrors(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit);

bool incrementPhaseSignalError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit);

bool resetPhaseSignalError(
	KFMeas&		kfMeas,
	int			index);

bool resetPhaseSignalOutage(
	KFMeas&		kfMeas,
	int			index);

bool resetIonoSignalOutage(
	KFMeas&		kfMeas,
	int			index);

bool rejectByState(
			Trace&		trace,
			KFState&	kfState,
			KFMeas&		kfMeas,
	const	KFKey&		kfKey,
			bool		postFit);

bool clockGlitchReaction(
			Trace&		trace,
			KFState&	kfState,
			KFMeas&		kfMeas,
	const	KFKey&		kfKey,
			bool		postFit);

bool orbitGlitchReaction(
			Trace&		trace,
			KFState&	kfState,
			KFMeas&		kfMeas,
	const	KFKey&		kfKey,
			bool		postFit);



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

void initPseudoObs(
			Trace&				netTrace,			
			KFState&			kfState,			
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


bool satQuat(
	GObs&				obs,
	vector<E_Source>	attitudeTypes,
	Quaterniond&		quat,
	bool				origGal	= false);

void fixAndHoldAmbiguities(
	Trace&		trace,		
	KFState&	kfState);

bool queryBiasUC(
	Trace&		trace,	
	GTime		time,	
	KFState&	kfState,	
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
