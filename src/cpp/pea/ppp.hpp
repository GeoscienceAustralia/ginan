
#pragma once

#include <map>

using std::map;



#include "algebra.hpp"

struct PhaseCenterData;
struct FrameSwapper;
struct Observation;
struct ReceiverMap;
struct AttStatus;
struct Receiver;
struct Solution;
struct KFState;
struct ObsList;
struct GTime;
struct Vmf3;
struct GObs;


double relativity2(
	VectorEcef&		rSat,
	VectorEcef&		rRec);

double recAntDelta(
	VectorEcef&		e,
	Receiver&		rec);

tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d> tideDelta(
	Trace&				trace,
	GTime				time,
	Receiver&			rec,
	VectorEcef&			rRec,
	ReceiverOptions&	recOpts);

void eopAdjustment(
	GTime&			time,
	VectorEcef&		e,
	ERPValues&		erpv,
	FrameSwapper&	frameSwapper,
	Receiver&		rec,
	VectorEcef&		rRec,
	KFMeasEntry&	measEntry,
	const KFState&	kfState);

double netResidualAndChainOutputs(
	Trace&			trace,
	Observation&	obs,
	KFMeasEntry&	measEntry);

void removeUnmeasuredAmbiguities(
	Trace&				trace,
	KFState&			kfState,
	map<KFKey, bool>	measuredStates);

void outputPppNmea(
	Trace&		trace,
	KFState&	kfState,
	string		id);

void spp(
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

void ppp(
	Trace&			trace,
	ReceiverMap&	receiverMap,
	KFState&		kfState,
	KFState&		remoteState);

void phaseWindup(
	GObs&		obs,
	Receiver&	rec,
	double&		phw);

bool ionoModel(
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

void selectAprioriSource(
	Trace&		trace,
	Receiver&	rec,
	GTime&		time,
	bool&		sppUsed,
	KFState&	kfState,
	KFState*	remote_ptr	= nullptr);

void selectAprioriSource(
	SatSys&		Sat,
	GTime&		time,
	KFState&	kfState,
	KFState*	remote_ptr	= nullptr);

void postFilterChecks(
	const	GTime&	time,
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

bool incrementReceiverError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit);

bool resetPhaseSignalError(
	const	GTime&		time,
			KFMeas&		kfMeas,
			int			index);

bool resetPhaseSignalOutage(
	const	GTime&		time,
			KFMeas&		kfMeas,
			int			index);

bool resetIonoSignalOutage(
	const	GTime&		time,
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



void receiverUducGnss(
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

void filterPseudoObs(
			Trace&				pppTrace,
			KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList);

void receiverPseudoObs(
			Trace&				pppTrace,
			Receiver&			rec,
	const	KFState&			kfState,
			KFMeasEntryList&	kfMeasEntryList,
			ReceiverMap&		receiverMap);


void readPseudosFromFile(
	string&		file);

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

void explainMeasurements(
	Trace&		trace,
	KFMeas&		meas,
	KFState&	kfState);
