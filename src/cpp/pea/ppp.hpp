#pragma once

#include <map>
#include "common/algebra.hpp"

using std::map;

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
struct ERPValues;
struct AzEl;
struct SatPos;

double relativity2(VectorEcef& rSat, VectorEcef& rRec);

double recAntDelta(VectorEcef& e, Receiver& rec);

tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d>
tideDelta(Trace& trace, GTime time, Receiver& rec, VectorEcef& rRec, ReceiverOptions& recOpts);

void eopAdjustment(
    GTime&        time,
    VectorEcef&   e,
    ERPValues&    erpv,
    FrameSwapper& frameSwapper,
    Receiver&     rec,
    VectorEcef&   rRec,
    KFMeasEntry&  measEntry,
    KFState&      kfState
);

double netResidualAndChainOutputs(Trace& trace, Observation& obs, KFMeasEntry& measEntry);

void outputPppNmea(Trace& trace, KFState& kfState, string id);

void spp(
    Trace&    trace,
    ObsList&  obsList,
    Solution& sol,
    string    id,
    KFState*  kfState_ptr = nullptr,
    KFState*  remote_ptr  = nullptr
);

void testEclipse(ObsList& obsList);

void pppCorrections(Trace& trace, ObsList& obsList, Vector3d& rRec, Receiver& rec);

void ppp(Trace& trace, ReceiverMap& receiverMap, KFState& kfState, KFState& remoteState);

void phaseWindup(GObs& obs, Receiver& rec, double& phw);

bool ionoModel(
    GTime&      time,
    VectorPos&  pos,
    AzEl&       azel,
    E_IonoMapFn mapFn,
    E_IonoMode  mode,
    double      layerHeight,
    double      ionoState,
    double&     dion,
    double&     var
);

void outputApriori(ReceiverMap& receiverMap);

void updateAprioriRecPos(
    Trace&           trace,
    Receiver&        rec,
    ReceiverOptions& recOpts,
    bool&            sppUsed,
    KFState*         remote_ptr = nullptr
);

void updateAprioriRecClk(
    Trace&           trace,
    Receiver&        rec,
    ReceiverOptions& recOpts,
    GTime&           time,
    KFState&         kfState,
    KFState*         remote_ptr = nullptr
);

void selectAprioriSource(
    Trace&    trace,
    Receiver& rec,
    GTime&    time,
    bool&     sppUsed,
    KFState&  kfState,
    KFState*  remote_ptr = nullptr
);

void selectAprioriSource(SatSys& Sat, GTime& time, KFState& kfState, KFState* remote_ptr = nullptr);

void postFilterChecks(const GTime& time, KFState& kfState, KFMeas& kfMeas);

bool deweightMeas(RejectCallbackDetails rejectDetails);
bool pseudoMeasTest(RejectCallbackDetails rejectDetails);
bool deweightStationMeas(RejectCallbackDetails rejectDetails);
bool incrementPhaseSignalError(RejectCallbackDetails rejectDetails);
bool incrementReceiverErrors(RejectCallbackDetails rejectDetails);
bool incrementSatelliteErrors(RejectCallbackDetails rejectDetails);
bool incrementStateErrors(RejectCallbackDetails rejectDetails);
bool rejectWorstMeasByState(RejectCallbackDetails rejectDetails);
bool rejectAllMeasByState(RejectCallbackDetails rejectDetails);
bool clockGlitchReaction(RejectCallbackDetails rejectDetails);
bool satelliteGlitchReaction(RejectCallbackDetails rejectDetails);
bool relaxState(RejectCallbackDetails rejectDetails);

void resetPhaseSignalError(const GTime& time, KFMeas& kfMeas, int index);
void resetIonoSignalOutage(const GTime& time, KFMeas& kfMeas, int index);

void receiverUducGnss(
    Trace&           pppTrace,
    Receiver&        rec,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList,
    KFState&         remoteState
);

void orbitPseudoObs(
    Trace&           pppTrace,
    Receiver&        rec,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
);

void initPseudoObs(Trace& pppTrace, KFState& kfState, KFMeasEntryList& kfMeasEntryList);

void filterPseudoObs(Trace& pppTrace, KFState& kfState, KFMeasEntryList& kfMeasEntryList);

void receiverPseudoObs(
    Trace&           pppTrace,
    Receiver&        rec,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList,
    ReceiverMap&     receiverMap
);

void readPseudosFromFile(string& file);

void receiverSlr(
    Trace&           pppTrace,
    Receiver&        rec,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
);

bool satQuat(SatPos& satPos, vector<E_Source> attitudeTypes, Quaterniond& quat);

void fixAndHoldAmbiguities(Trace& trace, KFState& kfState);

bool queryBiasUC(
    Trace&     trace,
    GTime      time,
    KFState&   kfState,
    SatSys     sat,
    string     rec,
    E_ObsCode  code,
    double&    bias,
    double&    vari,
    E_MeasType type
);

void pseudoRecDcb(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList);

void ambgPseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList);

void phasePseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList);

void ionoPseudoObs(
    Trace&           trace,
    ReceiverMap&     receiverMap,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
);

void tropPseudoObs(
    Trace&           trace,
    ReceiverMap&     receiverMap,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
);

void satClockPivotPseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList);

KFState propagateUncertainty(Trace& trace, KFState& kfState);

void explainMeasurements(Trace& trace, KFMeas& meas, KFState& kfState);

void addRejectDetails(
    const GTime&         time,
    Trace&               trace,
    KFState&             kfState,
    const KFKey&         key,
    const string&        action,
    const string&        reason,
    vector<ArbitraryKVP> details = {}
);
