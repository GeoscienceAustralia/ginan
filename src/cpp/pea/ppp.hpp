
#pragma once

#include <map>

using std::map;



#include "algebra.hpp"
#include "satStat.hpp"

//forward declarations
struct Receiver;
struct Solution;
struct Vmf3;
struct gptgrid_t;
struct AttStatus;
struct PhaseCenterData;
struct ReceiverMap;
struct GTime;




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
	string		id,
	KFState*	kfState_ptr	= nullptr,
	KFState*	remote_ptr	= nullptr);

void testEclipse(
	ObsList&	obsList);

void pppCorrections(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	rRec,
	Receiver&	rec);

void PPP(
	Trace&			trace,
	ReceiverMap&	receiverMap,
	KFState&		kfState,
	KFState&		remoteState);

void phaseWindup(
	GObs&		obs,
	Receiver&	rec,
	double&		phw);

int ionoModel(
	GTime&		time,
	VectorPos&	pos,
	AzEl&		azel,
	E_IonoMapFn	mapFn,
	E_IonoMode	mode,
	double		layerHeight,
	double		ionoState,
	double&		dion,
	double&		var);

void outputApriori(
	ReceiverMap& receiverMap);

// void outputPPPSolution(
// 	string		filename,
// 	KFState&	kfState,
// 	Receiver&	rec);
//
// void gpggaout(
// 	string outfile,
// 	KFState& KfState,
// 	string recId,
// 	int solStat,
// 	int numSat,
// 	double hdop,
// 	bool lng);

void selectAprioriSource(
	Receiver&	rec,
	GTime&		time,
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



void receiverPPP(
			Trace&				pppTrace,
			Receiver&			rec,
	const	KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList,
	const	KFState&			remoteState);


void orbitPseudoObs(
			Trace&				pppTrace,
			Receiver&			rec,
	const	KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList);

void initPseudoObs(
			Trace&				pppTrace,
			KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList);

void receiverPseudoObs(
			Trace&				pppTrace,
			Receiver&			rec,
	const	KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList,
			ReceiverMap&			receiverMap,
			MatrixXd*			R_ptr = nullptr);

void receiverSlr(
			Trace&				pppTrace,
			Receiver&			rec,
	const	KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList);


bool satQuat(
	SatPos&				satPos,
	vector<E_Source>	attitudeTypes,
	Quaterniond&		quat);

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

void pseudoRecDcb(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

void ambgPseudoObs(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

void ionoPseudoObs(
	Trace&				trace,
	ReceiverMap&		receiverMap,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

void tropPseudoObs(
	Trace&				trace,
	ReceiverMap&		receiverMap,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

void satClockPivotPseudoObs(
	Trace&				trace,
	KFState&			kfState,
	KFMeasEntryList&	kfMeasEntryList);

KFState propagateUncertainty(
	Trace&			trace,
	KFState&		kfState);
