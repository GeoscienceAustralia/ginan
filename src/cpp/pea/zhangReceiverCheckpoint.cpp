#include "pea/zhangReceiverCheckpoint.hpp"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <exception>
#include <iomanip>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <map>
#include <mutex>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/array.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/set.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/vector.hpp>

#include "common/acsConfig.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "common/sinex.hpp"
#include "common/zhangCheckpoint.hpp"

namespace
{
using std::array;
using std::map;
using std::pair;
using std::set;
using std::string;
using std::vector;

constexpr std::size_t MAX_RECEIVER_RUNTIME_PAYLOAD_BYTES =
	std::size_t{2} * 1024 * 1024 * 1024;
constexpr const char* POST_EPOCH_BOUNDARY = "POST_EPOCH_COMMITTED";
constexpr const char* OBSERVATION_POLICY = "RESET_POST_EPOCH_WORKSET";
constexpr const char* CACHE_POLICY = "RESET_DERIVED_CACHES";
constexpr const char* READY_POLICY = "RESET_NOT_READY";
constexpr const char* CONFIG_POINTER_POLICY =
	"REBIND_FROM_CONFIGURED_DESTINATION";
constexpr const char* SATELLITE_SCOPE = "PRECISE_PRODUCT_RUNTIME_ONLY";

struct ZhangRuntimeAttitude
{
	ZhangCheckpointTime startTime;
	double startSign = 0;
	double startYaw = 0;
	double startYawRate = 0;
	ZhangCheckpointTime excludeTime;
	double nominalYaw = 0;
	double modelYaw = 0;
	ZhangCheckpointTime modelYawTime;
	bool modelYawValid = false;
	VectorEcef eXBody;
	VectorEcef eYBody;
	VectorEcef eZBody;
	VectorEcef eXAnt;
	VectorEcef eYAnt;
	VectorEcef eZAnt;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & startTime;
		ar & startSign;
		ar & startYaw;
		ar & startYawRate;
		ar & excludeTime;
		ar & nominalYaw;
		ar & modelYaw;
		ar & modelYawTime;
		ar & modelYawValid;
		ar & eXBody;
		ar & eYBody;
		ar & eZBody;
		ar & eXAnt;
		ar & eYAnt;
		ar & eZAnt;
	}
};

struct ZhangRuntimeLinearCombination
{
	bool valid = false;
	double gfPhaseMetres = 0;
	double gfCodeMetres = 0;
	double wlPhaseMetres = 0;
	double wlPhaseCycles = 0;
	double wlCodeMetres = 0;
	double wlCodeCycles = 0;
	double nlPhaseMetres = 0;
	double nlPhaseCycles = 0;
	double nlCodeMetres = 0;
	double nlCodeCycles = 0;
	double ifPhaseMetres = 0;
	double ifCodeMetres = 0;
	double mwMetres = 0;
	double mwCycles = 0;
	double wavelengthA = 0;
	double wavelengthB = 0;
	double wavelengthWl = 0;
	double wavelengthNl = 0;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & valid;
		ar & gfPhaseMetres;
		ar & gfCodeMetres;
		ar & wlPhaseMetres;
		ar & wlPhaseCycles;
		ar & wlCodeMetres;
		ar & wlCodeCycles;
		ar & nlPhaseMetres;
		ar & nlPhaseCycles;
		ar & nlCodeMetres;
		ar & nlCodeCycles;
		ar & ifPhaseMetres;
		ar & ifCodeMetres;
		ar & mwMetres;
		ar & mwCycles;
		ar & wavelengthA;
		ar & wavelengthB;
		ar & wavelengthWl;
		ar & wavelengthNl;
	}
};

struct ZhangRuntimeLcState
{
	ZhangCheckpointTime time;
	SatSys satellite;
	map<int, double> phaseMetres;
	map<int, double> codeMetres;
	map<int, double> multipathMetres;
	map<pair<int, int>, ZhangRuntimeLinearCombination> combinations;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & time;
		ar & satellite;
		ar & phaseMetres;
		ar & codeMetres;
		ar & multipathMetres;
		ar & combinations;
	}
};

struct ZhangRuntimeSlipFilter
{
	array<double, 3> state{};
	array<array<double, 3>, 3> covariance{};
	int slip = 0;
	array<int, 3> ambiguity{};
	int epochCount = 0;
	ZhangRuntimeLcState previousCombination;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & state;
		ar & covariance;
		ar & slip;
		ar & ambiguity;
		ar & epochCount;
		ar & previousCombination;
	}
};

struct ZhangRuntimeSignalStatus
{
	unsigned int savedSlipFlags = 0;
	unsigned int slipFlags = 0;
	unsigned int phaseRejectCount = 0;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & savedSlipFlags;
		ar & slipFlags;
		ar & phaseRejectCount;
	}
};

struct ZhangRuntimeSatelliteStatus
{
	double ambiguityVariance = 0;
	double geometryFreeAmbiguity = 0;
	ZhangCheckpointTime lastObservationTime;
	double externalIonosphere = 0;
	double externalIonosphereVariance = 0;

	double mwSlipMean = 0;
	double mwSlipVariance = 0;
	double emwSlipMean = 0;
	double emwSlipVariance = 0;
	array<int, 3> repairedAmbiguity{};
	double melbourneWubbena = 0;
	double geometryFree = 0;
	ZhangRuntimeSlipFilter slipFilter;
	ZhangRuntimeLcState previousCombination;
	ZhangRuntimeLcState currentCombination;

	double azimuth = 0;
	double elevation = 0;
	double phaseWindup = 0;
	double wetMapping = 0;
	array<double, 2> wetGradientMapping{};
	VectorEcef lineOfSight;
	ZhangCheckpointTime lastIonosphereTime;
	double deltaIonosphere = 0;
	double sigmaIonosphere = 0;
	double previousStec = 0;
	double nadir = 0;
	bool slip = false;
	map<string, ZhangRuntimeSignalStatus> signalStatuses;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & ambiguityVariance;
		ar & geometryFreeAmbiguity;
		ar & lastObservationTime;
		ar & externalIonosphere;
		ar & externalIonosphereVariance;
		ar & mwSlipMean;
		ar & mwSlipVariance;
		ar & emwSlipMean;
		ar & emwSlipVariance;
		ar & repairedAmbiguity;
		ar & melbourneWubbena;
		ar & geometryFree;
		ar & slipFilter;
		ar & previousCombination;
		ar & currentCombination;
		ar & azimuth;
		ar & elevation;
		ar & phaseWindup;
		ar & wetMapping;
		ar & wetGradientMapping;
		ar & lineOfSight;
		ar & lastIonosphereTime;
		ar & deltaIonosphere;
		ar & sigmaIonosphere;
		ar & previousStec;
		ar & nadir;
		ar & slip;
		ar & signalStatuses;
	}
};

template <typename TYPE>
struct ZhangRuntimeMetadataField
{
	TYPE value{};
	bool valid = false;
	int winningSource = 0;
	std::uint32_t sourceMask = 0;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & value;
		ar & valid;
		ar & winningSource;
		ar & sourceMask;
	}
};

struct ZhangRuntimeReceiverMetadata
{
	vector<int> sourcePriority;
	ZhangRuntimeMetadataField<string> receiverType;
	ZhangRuntimeMetadataField<string> receiverFirmware;
	ZhangRuntimeMetadataField<string> receiverSerial;
	ZhangRuntimeMetadataField<string> antennaDescriptor;
	ZhangRuntimeMetadataField<string> antennaSerial;
	ZhangRuntimeMetadataField<string> markerName;
	ZhangRuntimeMetadataField<string> markerNumber;
	ZhangRuntimeMetadataField<Vector3d> antennaDelta;
	ZhangRuntimeMetadataField<Vector3d> stationPosition;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & sourcePriority;
		ar & receiverType;
		ar & receiverFirmware;
		ar & receiverSerial;
		ar & antennaDescriptor;
		ar & antennaSerial;
		ar & markerName;
		ar & markerNumber;
		ar & antennaDelta;
		ar & stationPosition;
	}
};

struct ZhangRuntimeKfBindingShape
{
	map<ZhangCheckpointKfKey, bool> receiverBindings;
	map<string, bool> filterChunkTraceBindings;
	std::size_t stateRejectCallbackCount = 0;
	std::size_t measurementRejectCallbackCount = 0;
	bool acceptedMeasurementCallback = false;
	bool stateTransitionCallback = false;
	bool exactStateTransformCallback = false;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & receiverBindings;
		ar & filterChunkTraceBindings;
		ar & stateRejectCallbackCount;
		ar & measurementRejectCallbackCount;
		ar & acceptedMeasurementCallback;
		ar & stateTransitionCallback;
		ar & exactStateTransformCallback;
	}
};

struct ZhangRuntimeSolution
{
	ZhangCheckpointTime sppTime;
	ZhangCheckpointKfCore sppState;
	ZhangRuntimeKfBindingShape sppBindingShape;
	VectorEcef sppPosition;
	double sppClock = 0;
	int clockReferenceSystem = 0;
	double sppPppClockOffset = 0;
	bool clockAdjustmentReady = false;
	int status = 0;
	int measurementCount = 0;
	double gdop = 0;
	double pdop = 0;
	double hdop = 0;
	double vdop = 0;
	double horizontalProtectionLevel = -1;
	double verticalProtectionLevel = -1;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & sppTime;
		ar & sppState;
		ar & sppBindingShape;
		ar & sppPosition;
		ar & sppClock;
		ar & clockReferenceSystem;
		ar & sppPppClockOffset;
		ar & clockAdjustmentReady;
		ar & status;
		ar & measurementCount;
		ar & gdop;
		ar & pdop;
		ar & hdop;
		ar & vdop;
		ar & horizontalProtectionLevel;
		ar & verticalProtectionLevel;
	}
};

struct ZhangRuntimeSinexState
{
	string siteBindingFingerprint;
	string receiverBindingFingerprint;
	string antennaBindingFingerprint;
	string eccentricityBindingFingerprint;
	array<double, 3> start{};
	array<double, 3> stop{};
	bool primary = false;
	VectorEcef position;
	VectorEcef variance;
	VectorEcef velocity;
	ZhangCheckpointTime referenceEpoch;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & siteBindingFingerprint;
		ar & receiverBindingFingerprint;
		ar & antennaBindingFingerprint;
		ar & eccentricityBindingFingerprint;
		ar & start;
		ar & stop;
		ar & primary;
		ar & position;
		ar & variance;
		ar & velocity;
		ar & referenceEpoch;
	}
};

struct ZhangRuntimeReceiver
{
	string mapKey;
	string id;
	bool pseudoReceiver = false;
	bool invalid = false;
	ZhangRuntimeSinexState sinex;
	ZhangRuntimeReceiverMetadata metadata;
	map<string, string> metadataMap;
	string source;

	ZhangCheckpointTime firstEpoch;
	ZhangCheckpointTime lastEpoch;
	int epochCount = 0;
	int observationCount = 0;
	int slipCount = 0;
	map<int, int> codeCount;
	map<string, int> satelliteCount;
	int receiverErrorEpochs = 0;
	int receiverErrorCount = 0;

	ZhangRuntimeSolution solution;
	string antennaType;
	string receiverType;
	string antennaId;
	map<SatSys, ZhangRuntimeSatelliteStatus> satelliteStatuses;
	VectorEnu antennaDelta;
	ZhangRuntimeAttitude attitude;

	bool primaryApriori = false;
	array<double, 3> aprioriTime{};
	double aprioriClock = 0;
	double aprioriClockVariance = 0;
	Vector3d aprioriPosition = Vector3d::Zero();
	Matrix3d aprioriPositionVariance = Matrix3d::Zero();
	Vector3d minimumConstraintApriori = Vector3d::Zero();
	Vector3d geodeticPosition = Vector3d::Zero();
	bool ready = false;
	Vector3d antennaBoresight = Vector3d::Zero();
	Vector3d antennaAzimuth = Vector3d::Zero();
	string traceFilename;
	string jsonTraceFilename;
	string sppOutputFile;
	map<SatSys, ZhangCheckpointTime> savedSlips;
	map<int, vector<int>> trackedSignals;
	unsigned int failureFlags = 0;
	std::size_t resetObservationCount = 0;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & mapKey;
		ar & id;
		ar & pseudoReceiver;
		ar & invalid;
		ar & sinex;
		ar & metadata;
		ar & metadataMap;
		ar & source;
		ar & firstEpoch;
		ar & lastEpoch;
		ar & epochCount;
		ar & observationCount;
		ar & slipCount;
		ar & codeCount;
		ar & satelliteCount;
		ar & receiverErrorEpochs;
		ar & receiverErrorCount;
		ar & solution;
		ar & antennaType;
		ar & receiverType;
		ar & antennaId;
		ar & satelliteStatuses;
		ar & antennaDelta;
		ar & attitude;
		ar & primaryApriori;
		ar & aprioriTime;
		ar & aprioriClock;
		ar & aprioriClockVariance;
		ar & aprioriPosition;
		ar & aprioriPositionVariance;
		ar & minimumConstraintApriori;
		ar & geodeticPosition;
		ar & ready;
		ar & antennaBoresight;
		ar & antennaAzimuth;
		ar & traceFilename;
		ar & jsonTraceFilename;
		ar & sppOutputFile;
		ar & savedSlips;
		ar & trackedSignals;
		ar & failureFlags;
		ar & resetObservationCount;
	}
};

struct ZhangReceiverRuntimeEnvelope
{
	std::uint32_t schemaVersion =
		ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SCHEMA_VERSION;
	string sectionName = ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SECTION_NAME;
	string runtimeId;
	string boundary = POST_EPOCH_BOUNDARY;
	string observationPolicy = OBSERVATION_POLICY;
	string cachePolicy = CACHE_POLICY;
	string readyPolicy = READY_POLICY;
	string configurationPointerPolicy = CONFIG_POINTER_POLICY;
	map<string, set<string>> customAliasesMap;
	vector<ZhangRuntimeReceiver> receivers;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & schemaVersion;
		ar & sectionName;
		ar & runtimeId;
		ar & boundary;
		ar & observationPolicy;
		ar & cachePolicy;
		ar & readyPolicy;
		ar & configurationPointerPolicy;
		ar & customAliasesMap;
		ar & receivers;
	}
};

struct ZhangRuntimeSatPos
{
	ZhangCheckpointTime time;
	SatSys satellite;
	bool ownerSatNavBinding = false;
	int positionSource = 0;
	int clockSource = 0;
	VectorEcef comPosition;
	VectorEcef apcPosition;
	VectorEcef velocity;
	VectorEci eciPositionAtTransmission;
	VectorEci eciVelocityAtTransmission;
	VectorEci eciPositionAtEpoch;
	VectorEci eciVelocityAtEpoch;
	double positionVariance = 0;
	double clock = 0;
	double clockVelocity = 0;
	double clockVariance = 0;
	bool sppValid = false;
	int clockIode = -1;
	int positionIode = -1;
	bool ephemerisPositionValid = false;
	bool ephemerisClockValid = false;
	double timeOfFlight = 0;
	unsigned int failureFlags = 0;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & time;
		ar & satellite;
		ar & ownerSatNavBinding;
		ar & positionSource;
		ar & clockSource;
		ar & comPosition;
		ar & apcPosition;
		ar & velocity;
		ar & eciPositionAtTransmission;
		ar & eciVelocityAtTransmission;
		ar & eciPositionAtEpoch;
		ar & eciVelocityAtEpoch;
		ar & positionVariance;
		ar & clock;
		ar & clockVelocity;
		ar & clockVariance;
		ar & sppValid;
		ar & clockIode;
		ar & positionIode;
		ar & ephemerisPositionValid;
		ar & ephemerisClockValid;
		ar & timeOfFlight;
		ar & failureFlags;
	}
};

struct ZhangRuntimeSatelliteNavigation
{
	SatSys satellite;
	map<int, double> wavelengths;
	VectorEci aprioriPosition;
	double aprioriClock = 0;
	ZhangRuntimeAttitude attitude;
	string id;
	string traceFilename;
	string jsonTraceFilename;
	Vector3d antennaBoresight = Vector3d::Zero();
	Vector3d antennaAzimuth = Vector3d::Zero();
	ZhangRuntimeSatPos nominalPosition;
	int satelliteErrorEpochs = 0;
	int satelliteErrorCount = 0;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & satellite;
		ar & wavelengths;
		ar & aprioriPosition;
		ar & aprioriClock;
		ar & attitude;
		ar & id;
		ar & traceFilename;
		ar & jsonTraceFilename;
		ar & antennaBoresight;
		ar & antennaAzimuth;
		ar & nominalPosition;
		ar & satelliteErrorEpochs;
		ar & satelliteErrorCount;
	}
};

struct ZhangRuntimeSatelliteAlias
{
	SatSys satellite;
	string block;
	string svn;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & satellite;
		ar & block;
		ar & svn;
	}
};

struct ZhangRuntimeSvnHistoryEntry
{
	ZhangCheckpointTime time;
	string svn;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & time;
		ar & svn;
	}
};

struct ZhangRuntimeErpFilterValues
{
	ZhangCheckpointTime time;
	double xp = 0;
	double yp = 0;
	double ut1Utc = 0;
	double lod = 0;
	double xpr = 0;
	double ypr = 0;
	double xpSigma = 0;
	double ypSigma = 0;
	double xprSigma = 0;
	double yprSigma = 0;
	double ut1UtcSigma = 0;
	double lodSigma = 0;
	bool predicted = false;
	bool filtered = false;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & time;
		ar & xp;
		ar & yp;
		ar & ut1Utc;
		ar & lod;
		ar & xpr;
		ar & ypr;
		ar & xpSigma;
		ar & ypSigma;
		ar & xprSigma;
		ar & yprSigma;
		ar & ut1UtcSigma;
		ar & lodSigma;
		ar & predicted;
		ar & filtered;
	}
};

struct ZhangSatelliteRuntimeEnvelope
{
	std::uint32_t schemaVersion =
		ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SCHEMA_VERSION;
	string sectionName = ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SECTION_NAME;
	string runtimeId;
	string boundary = POST_EPOCH_BOUNDARY;
	string scope = SATELLITE_SCOPE;
	string pointerPolicy = CONFIG_POINTER_POLICY;
	vector<ZhangRuntimeSatelliteNavigation> satellites;
	vector<ZhangRuntimeSatelliteAlias> aliases;
	map<SatSys, vector<ZhangRuntimeSvnHistoryEntry>> svnHistory;
	map<string, string> blockTypes;
	ZhangRuntimeErpFilterValues erpFilterValues;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & schemaVersion;
		ar & sectionName;
		ar & runtimeId;
		ar & boundary;
		ar & scope;
		ar & pointerPolicy;
		ar & satellites;
		ar & aliases;
		ar & svnHistory;
		ar & blockTypes;
		ar & erpFilterValues;
	}
};

template <typename TYPE>
bool serializePayload(
	const TYPE& value,
	string& payload,
	string& failureReason,
	const string& prefix)
{
	payload.clear();
	try
	{
		std::ostringstream output(std::ios::binary | std::ios::out);
		boost::archive::binary_oarchive archive(
			output, boost::archive::no_header);
		archive << value;
		payload = output.str();
	}
	catch (const std::exception& exception)
	{
		failureReason = prefix + "_SERIALIZE_FAILED:" + exception.what();
		payload.clear();
		return false;
	}
	if (payload.empty())
	{
		failureReason = prefix + "_EMPTY_PAYLOAD";
		return false;
	}
	if (payload.size() > MAX_RECEIVER_RUNTIME_PAYLOAD_BYTES)
	{
		failureReason = prefix + "_PAYLOAD_TOO_LARGE";
		payload.clear();
		return false;
	}
	return true;
}

template <typename TYPE>
bool deserializePayload(
	const string& payload,
	TYPE& value,
	string& failureReason,
	const string& prefix)
{
	if (payload.empty())
	{
		failureReason = prefix + "_EMPTY_PAYLOAD";
		return false;
	}
	if (payload.size() > MAX_RECEIVER_RUNTIME_PAYLOAD_BYTES)
	{
		failureReason = prefix + "_PAYLOAD_TOO_LARGE";
		return false;
	}
	try
	{
		std::istringstream input(payload, std::ios::binary | std::ios::in);
		{
			boost::archive::binary_iarchive archive(
				input, boost::archive::no_header);
			archive >> value;
		}
		if (input.peek() != std::char_traits<char>::eof())
		{
			failureReason = prefix + "_TRAILING_BYTES";
			return false;
		}
	}
	catch (const std::exception& exception)
	{
		failureReason = prefix + "_DESERIALIZE_FAILED:" + exception.what();
		return false;
	}
	return true;
}

bool finiteValues(std::initializer_list<double> values)
{
	return std::all_of(
		values.begin(), values.end(),
		[](double value) { return std::isfinite(value); });
}

bool finiteTime(const ZhangCheckpointTime& stored)
{
	return std::isfinite(restoreZhangCheckpointTime(stored).bigTime);
}

ZhangRuntimeErpFilterValues captureErpFilterValues(const ERPValues& input)
{
	ZhangRuntimeErpFilterValues output;
	output.time = captureZhangCheckpointTime(input.time);
	output.xp = input.xp;
	output.yp = input.yp;
	output.ut1Utc = input.ut1Utc;
	output.lod = input.lod;
	output.xpr = input.xpr;
	output.ypr = input.ypr;
	output.xpSigma = input.xpSigma;
	output.ypSigma = input.ypSigma;
	output.xprSigma = input.xprSigma;
	output.yprSigma = input.yprSigma;
	output.ut1UtcSigma = input.ut1UtcSigma;
	output.lodSigma = input.lodSigma;
	output.predicted = input.isPredicted;
	output.filtered = input.isFiltered;
	return output;
}

bool validErpFilterValues(const ZhangRuntimeErpFilterValues& input)
{
	return finiteTime(input.time)
		&& finiteValues({
			input.xp,
			input.yp,
			input.ut1Utc,
			input.lod,
			input.xpr,
			input.ypr,
			input.xpSigma,
			input.ypSigma,
			input.xprSigma,
			input.yprSigma,
			input.ut1UtcSigma,
			input.lodSigma});
}

ERPValues restoreErpFilterValues(const ZhangRuntimeErpFilterValues& input)
{
	ERPValues output;
	output.time = restoreZhangCheckpointTime(input.time);
	output.xp = input.xp;
	output.yp = input.yp;
	output.ut1Utc = input.ut1Utc;
	output.lod = input.lod;
	output.xpr = input.xpr;
	output.ypr = input.ypr;
	output.xpSigma = input.xpSigma;
	output.ypSigma = input.ypSigma;
	output.xprSigma = input.xprSigma;
	output.yprSigma = input.yprSigma;
	output.ut1UtcSigma = input.ut1UtcSigma;
	output.lodSigma = input.lodSigma;
	output.isPredicted = input.predicted;
	output.isFiltered = input.filtered;
	return output;
}

bool validSystem(int value, bool allowNone = true)
{
	if (value < static_cast<int>(E_Sys::NONE)
		|| value > static_cast<int>(E_Sys::COMB))
	{
		return false;
	}
	return allowNone || value != static_cast<int>(E_Sys::NONE);
}

bool validSatellite(const SatSys& satellite, bool allowNone = false)
{
	if (!validSystem(static_cast<int>(satellite.sys), allowNone))
	{
		return false;
	}
	if (satellite.sys == E_Sys::NONE)
	{
		return allowNone && satellite.prn == 0;
	}
	return satellite.prn > 0;
}

bool validSatelliteAliasKey(const SatSys& satellite)
{
	if (!validSystem(static_cast<int>(satellite.sys), true)
	 || satellite.prn < 0)
	{
		return false;
	}
	// A zero PRN is a system-level identity/configuration key.  NONE has no
	// satellite-level representation and is therefore valid only as NONE/0.
	return satellite.sys != E_Sys::NONE || satellite.prn == 0;
}

bool validFrequency(int value)
{
	switch (static_cast<E_FType>(value))
	{
		case E_FType::NONE:
		case E_FType::F1:
		case E_FType::F2:
		case E_FType::F5:
		case E_FType::F6:
		case E_FType::F7:
		case E_FType::F8:
		case E_FType::G1:
		case E_FType::G2:
		case E_FType::G3:
		case E_FType::G4:
		case E_FType::G6:
		case E_FType::B1:
		case E_FType::B3:
		case E_FType::I9:
			return true;
		default:
			return false;
	}
}

bool validObservationCode(int value)
{
	return (value >= static_cast<int>(E_ObsCode::NONE)
			&& value <= static_cast<int>(E_ObsCode::L8P))
		|| value == static_cast<int>(E_ObsCode::AUTO);
}

array<double, 3> captureYds(const UYds& value)
{
	return {value.year, value.doy, value.sod};
}

UYds restoreYds(const array<double, 3>& value)
{
	return {value[0], value[1], value[2]};
}

bool validYds(const array<double, 3>& value)
{
	return finiteValues({value[0], value[1], value[2]});
}

void appendFingerprintString(std::ostringstream& output, const string& value)
{
	output << value.size() << ':' << value << ';';
}

void appendFingerprintYds(std::ostringstream& output, const UYds& value)
{
	output << std::hexfloat << value.year << ',' << value.doy << ','
		   << value.sod << ';';
}

string siteBindingFingerprint(const SinexSiteId* value)
{
	if (!value)
	{
		return {};
	}
	std::ostringstream output;
	appendFingerprintString(output, value->sitecode);
	appendFingerprintString(output, value->ptcode);
	appendFingerprintString(output, value->domes);
	appendFingerprintString(output, value->desc);
	output << value->typecode << ';';
	return zhangCheckpointSha256(output.str());
}

string receiverBindingFingerprint(const SinexReceiver* value)
{
	if (!value)
	{
		return {};
	}
	std::ostringstream output;
	appendFingerprintString(output, value->sitecode);
	appendFingerprintString(output, value->ptcode);
	appendFingerprintString(output, value->solnid);
	appendFingerprintString(output, value->type);
	appendFingerprintString(output, value->sn);
	appendFingerprintString(output, value->firm);
	appendFingerprintYds(output, value->start);
	appendFingerprintYds(output, value->end);
	output << value->typecode << ';';
	return zhangCheckpointSha256(output.str());
}

string antennaBindingFingerprint(const SinexAntenna* value)
{
	if (!value)
	{
		return {};
	}
	std::ostringstream output;
	appendFingerprintString(output, value->sitecode);
	appendFingerprintString(output, value->ptcode);
	appendFingerprintString(output, value->solnnum);
	appendFingerprintString(output, value->calibModel);
	appendFingerprintString(output, value->type);
	appendFingerprintString(output, value->sn);
	appendFingerprintYds(output, value->start);
	appendFingerprintYds(output, value->end);
	output << value->typecode << ';';
	return zhangCheckpointSha256(output.str());
}

string eccentricityBindingFingerprint(const SinexSiteEcc* value)
{
	if (!value)
	{
		return {};
	}
	std::ostringstream output;
	appendFingerprintString(output, value->sitecode);
	appendFingerprintString(output, value->ptcode);
	appendFingerprintString(output, value->solnnum);
	appendFingerprintString(output, value->rs);
	appendFingerprintYds(output, value->start);
	appendFingerprintYds(output, value->end);
	output << value->typecode << ';' << std::hexfloat << value->ecc.x()
		   << ',' << value->ecc.y() << ',' << value->ecc.z() << ';';
	return zhangCheckpointSha256(output.str());
}

SinexSiteId* resolveSiteBinding(const string& fingerprint)
{
	for (auto& [ignored, value] : theSinex.mapsiteids)
		if (siteBindingFingerprint(&value) == fingerprint)
			return &value;
	return nullptr;
}

SinexReceiver* resolveReceiverBinding(const string& fingerprint)
{
	for (auto& [ignoredSite, history] : theSinex.mapreceivers)
	for (auto& [ignoredTime, value] : history)
		if (receiverBindingFingerprint(&value) == fingerprint)
			return &value;
	return nullptr;
}

SinexAntenna* resolveAntennaBinding(const string& fingerprint)
{
	for (auto& [ignoredSite, history] : theSinex.mapantennas)
	for (auto& [ignoredTime, value] : history)
		if (antennaBindingFingerprint(&value) == fingerprint)
			return &value;
	return nullptr;
}

SinexSiteEcc* resolveEccentricityBinding(const string& fingerprint)
{
	for (auto& [ignoredSite, history] : theSinex.mapeccentricities)
	for (auto& [ignoredTime, value] : history)
		if (eccentricityBindingFingerprint(&value) == fingerprint)
			return &value;
	return nullptr;
}

ZhangRuntimeAttitude captureAttitude(const AttStatus& input)
{
	ZhangRuntimeAttitude output;
	output.startTime = captureZhangCheckpointTime(input.startTime);
	output.startSign = input.startSign;
	output.startYaw = input.startYaw;
	output.startYawRate = input.startYawRate;
	output.excludeTime = captureZhangCheckpointTime(input.excludeTime);
	output.nominalYaw = input.nominalYaw;
	output.modelYaw = input.modelYaw;
	output.modelYawTime = captureZhangCheckpointTime(input.modelYawTime);
	output.modelYawValid = input.modelYawValid;
	output.eXBody = input.eXBody;
	output.eYBody = input.eYBody;
	output.eZBody = input.eZBody;
	output.eXAnt = input.eXAnt;
	output.eYAnt = input.eYAnt;
	output.eZAnt = input.eZAnt;
	return output;
}

AttStatus restoreAttitude(const ZhangRuntimeAttitude& input)
{
	AttStatus output;
	output.startTime = restoreZhangCheckpointTime(input.startTime);
	output.startSign = input.startSign;
	output.startYaw = input.startYaw;
	output.startYawRate = input.startYawRate;
	output.excludeTime = restoreZhangCheckpointTime(input.excludeTime);
	output.nominalYaw = input.nominalYaw;
	output.modelYaw = input.modelYaw;
	output.modelYawTime = restoreZhangCheckpointTime(input.modelYawTime);
	output.modelYawValid = input.modelYawValid;
	output.eXBody = input.eXBody;
	output.eYBody = input.eYBody;
	output.eZBody = input.eZBody;
	output.eXAnt = input.eXAnt;
	output.eYAnt = input.eYAnt;
	output.eZAnt = input.eZAnt;
	return output;
}

bool validAttitude(const ZhangRuntimeAttitude& input)
{
	return finiteTime(input.startTime) && finiteTime(input.excludeTime)
		&& finiteTime(input.modelYawTime)
		&& finiteValues({input.startSign, input.startYaw,
			input.startYawRate, input.nominalYaw, input.modelYaw})
		&& input.eXBody.allFinite() && input.eYBody.allFinite()
		&& input.eZBody.allFinite() && input.eXAnt.allFinite()
		&& input.eYAnt.allFinite() && input.eZAnt.allFinite();
}

ZhangRuntimeLinearCombination captureLinearCombination(const S_LC& input)
{
	return {
		input.valid,
		input.GF_Phas_m,
		input.GF_Code_m,
		input.WL_Phas_m,
		input.WL_Phas_c,
		input.WL_Code_m,
		input.WL_Code_c,
		input.NL_Phas_m,
		input.NL_Phas_c,
		input.NL_Code_m,
		input.NL_Code_c,
		input.IF_Phas_m,
		input.IF_Code_m,
		input.MW_m,
		input.MW_c,
		input.lam_A,
		input.lam_B,
		input.lam_WL,
		input.lam_NL};
}

S_LC restoreLinearCombination(const ZhangRuntimeLinearCombination& input)
{
	S_LC output{};
	output.valid = input.valid;
	output.GF_Phas_m = input.gfPhaseMetres;
	output.GF_Code_m = input.gfCodeMetres;
	output.WL_Phas_m = input.wlPhaseMetres;
	output.WL_Phas_c = input.wlPhaseCycles;
	output.WL_Code_m = input.wlCodeMetres;
	output.WL_Code_c = input.wlCodeCycles;
	output.NL_Phas_m = input.nlPhaseMetres;
	output.NL_Phas_c = input.nlPhaseCycles;
	output.NL_Code_m = input.nlCodeMetres;
	output.NL_Code_c = input.nlCodeCycles;
	output.IF_Phas_m = input.ifPhaseMetres;
	output.IF_Code_m = input.ifCodeMetres;
	output.MW_m = input.mwMetres;
	output.MW_c = input.mwCycles;
	output.lam_A = input.wavelengthA;
	output.lam_B = input.wavelengthB;
	output.lam_WL = input.wavelengthWl;
	output.lam_NL = input.wavelengthNl;
	return output;
}

bool validLinearCombination(const ZhangRuntimeLinearCombination& input)
{
	return finiteValues({
		input.gfPhaseMetres,
		input.gfCodeMetres,
		input.wlPhaseMetres,
		input.wlPhaseCycles,
		input.wlCodeMetres,
		input.wlCodeCycles,
		input.nlPhaseMetres,
		input.nlPhaseCycles,
		input.nlCodeMetres,
		input.nlCodeCycles,
		input.ifPhaseMetres,
		input.ifCodeMetres,
		input.mwMetres,
		input.mwCycles,
		input.wavelengthA,
		input.wavelengthB,
		input.wavelengthWl,
		input.wavelengthNl});
}

ZhangRuntimeLcState captureLcState(const lc_t& input)
{
	ZhangRuntimeLcState output;
	output.time = captureZhangCheckpointTime(input.time);
	output.satellite = input.Sat;
	for (const auto& [frequency, value] : input.L_m)
	{
		output.phaseMetres[static_cast<int>(frequency)] = value;
	}
	for (const auto& [frequency, value] : input.P)
	{
		output.codeMetres[static_cast<int>(frequency)] = value;
	}
	for (const auto& [frequency, value] : input.mp)
	{
		output.multipathMetres[static_cast<int>(frequency)] = value;
	}
	for (const auto& [frequencies, value] : input.lcMap)
	{
		output.combinations[
			{static_cast<int>(frequencies.first),
			 static_cast<int>(frequencies.second)}] =
			captureLinearCombination(value);
	}
	return output;
}

lc_t restoreLcState(const ZhangRuntimeLcState& input)
{
	lc_t output{};
	output.time = restoreZhangCheckpointTime(input.time);
	output.Sat = input.satellite;
	for (const auto& [frequency, value] : input.phaseMetres)
	{
		output.L_m[static_cast<E_FType>(frequency)] = value;
	}
	for (const auto& [frequency, value] : input.codeMetres)
	{
		output.P[static_cast<E_FType>(frequency)] = value;
	}
	for (const auto& [frequency, value] : input.multipathMetres)
	{
		output.mp[static_cast<E_FType>(frequency)] = value;
	}
	for (const auto& [frequencies, value] : input.combinations)
	{
		output.lcMap[
			{static_cast<E_FType>(frequencies.first),
			 static_cast<E_FType>(frequencies.second)}] =
			restoreLinearCombination(value);
	}
	return output;
}

bool validLcState(const ZhangRuntimeLcState& input, string& failureReason)
{
	if (!finiteTime(input.time)
		|| !validSatellite(input.satellite, true))
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_LC_IDENTITY";
		return false;
	}
	auto validScalarMap = [&](const map<int, double>& values)
	{
		for (const auto& [frequency, value] : values)
		{
			if (!validFrequency(frequency) || !std::isfinite(value))
			{
				return false;
			}
		}
		return true;
	};
	if (!validScalarMap(input.phaseMetres)
		|| !validScalarMap(input.codeMetres)
		|| !validScalarMap(input.multipathMetres))
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_LC_FREQUENCY";
		return false;
	}
	for (const auto& [frequencies, value] : input.combinations)
	{
		if (!validFrequency(frequencies.first)
			|| !validFrequency(frequencies.second)
			|| !validLinearCombination(value))
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_INVALID_LC_COMBINATION";
			return false;
		}
	}
	return true;
}

ZhangRuntimeSatelliteStatus captureSatelliteStatus(const SatStat& input)
{
	ZhangRuntimeSatelliteStatus output;
	output.ambiguityVariance = input.ambvar;
	output.geometryFreeAmbiguity = input.gf_amb;
	output.lastObservationTime = captureZhangCheckpointTime(input.lastObsTime);
	output.externalIonosphere = input.extiono;
	output.externalIonosphereVariance = input.extionovar;
	output.mwSlipMean = input.mwSlip.mean;
	output.mwSlipVariance = input.mwSlip.var;
	output.emwSlipMean = input.emwSlip.mean;
	output.emwSlipVariance = input.emwSlip.var;
	for (int i = 0; i < 3; ++i)
	{
		output.repairedAmbiguity[i] = input.amb[i];
		output.slipFilter.state[i] = input.flt.a[i];
		output.slipFilter.ambiguity[i] = input.flt.amb[i];
		for (int j = 0; j < 3; ++j)
		{
			output.slipFilter.covariance[i][j] = input.flt.Qa[i][j];
		}
	}
	output.melbourneWubbena = input.mw;
	output.geometryFree = input.gf;
	output.slipFilter.slip = input.flt.slip;
	output.slipFilter.epochCount = input.flt.ne;
	output.slipFilter.previousCombination = captureLcState(input.flt.lc_pre);
	output.previousCombination = captureLcState(input.lc_pre);
	output.currentCombination = captureLcState(input.lc_new);
	output.azimuth = input.az;
	output.elevation = input.el;
	output.phaseWindup = input.phw;
	output.wetMapping = input.mapWet;
	output.wetGradientMapping = {input.mapWetGrads[0], input.mapWetGrads[1]};
	output.lineOfSight = input.e;
	output.lastIonosphereTime = captureZhangCheckpointTime(input.lastIonTime);
	output.deltaIonosphere = input.dIono;
	output.sigmaIonosphere = input.sigmaIono;
	output.previousStec = input.prevSTEC;
	output.nadir = input.nadir;
	output.slip = input.slip;
	for (const auto& [signal, status] : input.sigStatMap)
	{
		output.signalStatuses[signal] = {
			status.savedSlip.any,
			status.slip.any,
			status.phaseRejectCount};
	}
	return output;
}

SatStat restoreSatelliteStatus(const ZhangRuntimeSatelliteStatus& input)
{
	SatStat output{};
	output.ambvar = input.ambiguityVariance;
	output.gf_amb = input.geometryFreeAmbiguity;
	output.lastObsTime = restoreZhangCheckpointTime(input.lastObservationTime);
	output.extiono = input.externalIonosphere;
	output.extionovar = input.externalIonosphereVariance;
	output.mwSlip = {input.mwSlipMean, input.mwSlipVariance};
	output.emwSlip = {input.emwSlipMean, input.emwSlipVariance};
	for (int i = 0; i < 3; ++i)
	{
		output.amb[i] = input.repairedAmbiguity[i];
		output.flt.a[i] = input.slipFilter.state[i];
		output.flt.amb[i] = input.slipFilter.ambiguity[i];
		for (int j = 0; j < 3; ++j)
		{
			output.flt.Qa[i][j] = input.slipFilter.covariance[i][j];
		}
	}
	output.mw = input.melbourneWubbena;
	output.gf = input.geometryFree;
	output.flt.slip = input.slipFilter.slip;
	output.flt.ne = input.slipFilter.epochCount;
	output.flt.lc_pre = restoreLcState(input.slipFilter.previousCombination);
	output.lc_pre = restoreLcState(input.previousCombination);
	output.lc_new = restoreLcState(input.currentCombination);
	output.az = input.azimuth;
	output.el = input.elevation;
	output.phw = input.phaseWindup;
	output.mapWet = input.wetMapping;
	output.mapWetGrads[0] = input.wetGradientMapping[0];
	output.mapWetGrads[1] = input.wetGradientMapping[1];
	output.e = input.lineOfSight;
	output.lastIonTime = restoreZhangCheckpointTime(input.lastIonosphereTime);
	output.dIono = input.deltaIonosphere;
	output.sigmaIono = input.sigmaIonosphere;
	output.prevSTEC = input.previousStec;
	output.nadir = input.nadir;
	output.slip = input.slip;
	for (const auto& [signal, status] : input.signalStatuses)
	{
		SigStat restored;
		restored.savedSlip.any = status.savedSlipFlags;
		restored.slip.any = status.slipFlags;
		restored.phaseRejectCount = status.phaseRejectCount;
		output.sigStatMap[signal] = restored;
	}
	return output;
}

bool validSatelliteStatus(
	const SatSys& satellite,
	const ZhangRuntimeSatelliteStatus& input,
	string& failureReason)
{
	const double values[] = {
		input.ambiguityVariance,
		input.geometryFreeAmbiguity,
		input.externalIonosphere,
		input.externalIonosphereVariance,
		input.mwSlipMean,
		input.mwSlipVariance,
		input.emwSlipMean,
		input.emwSlipVariance,
		input.melbourneWubbena,
		input.geometryFree,
		input.azimuth,
		input.elevation,
		input.phaseWindup,
		input.wetMapping,
		input.wetGradientMapping[0],
		input.wetGradientMapping[1],
		input.deltaIonosphere,
		input.sigmaIonosphere,
		input.previousStec,
		input.nadir};
	if (!std::all_of(
			std::begin(values), std::end(values),
			[](double value) { return std::isfinite(value); })
		|| !finiteTime(input.lastObservationTime)
		|| !finiteTime(input.lastIonosphereTime)
		|| !input.lineOfSight.allFinite())
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_NONFINITE_SAT_STATUS";
		return false;
	}
	for (int i = 0; i < 3; ++i)
	{
		if (!std::isfinite(input.slipFilter.state[i]))
		{
			failureReason = "RECEIVER_RUNTIME_CHECKPOINT_NONFINITE_SLIP_FILTER";
			return false;
		}
		for (int j = 0; j < 3; ++j)
		{
			if (!std::isfinite(input.slipFilter.covariance[i][j]))
			{
				failureReason =
					"RECEIVER_RUNTIME_CHECKPOINT_NONFINITE_SLIP_FILTER";
				return false;
			}
		}
	}
	if (input.slipFilter.epochCount < 0)
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_SLIP_FILTER_COUNT";
		return false;
	}
	if (!validLcState(input.slipFilter.previousCombination, failureReason)
		|| !validLcState(input.previousCombination, failureReason)
		|| !validLcState(input.currentCombination, failureReason))
	{
		return false;
	}
	auto lcSatelliteMatches = [&](const ZhangRuntimeLcState& lc)
	{
		return lc.satellite.sys == E_Sys::NONE || lc.satellite == satellite;
	};
	if (!lcSatelliteMatches(input.slipFilter.previousCombination)
		|| !lcSatelliteMatches(input.previousCombination)
		|| !lcSatelliteMatches(input.currentCombination))
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_LC_SATELLITE_MISMATCH";
		return false;
	}
	for (const auto& [signal, status] : input.signalStatuses)
	{
		if (signal.empty()
			|| (status.savedSlipFlags & ~0x3Fu) != 0
			|| (status.slipFlags & ~0x3Fu) != 0)
		{
			failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_SIGNAL_STATUS";
			return false;
		}
	}
	return true;
}

template <typename TYPE>
ZhangRuntimeMetadataField<TYPE> captureMetadataField(
	const ReceiverMetaField<TYPE>& input)
{
	return {
		input.value,
		input.valid,
		static_cast<int>(input.winningSource),
		input.sourceMask};
}

ZhangRuntimeMetadataField<Vector3d> captureMetadataField(
	const ReceiverMetaField<Vector3d>& input)
{
	return {
		input.valid ? input.value : Vector3d::Zero(),
		input.valid,
		static_cast<int>(input.winningSource),
		input.sourceMask};
}

template <typename TYPE>
ReceiverMetaField<TYPE> restoreMetadataField(
	const ZhangRuntimeMetadataField<TYPE>& input)
{
	ReceiverMetaField<TYPE> output;
	output.value = input.value;
	output.valid = input.valid;
	output.winningSource =
		static_cast<E_ReceiverMetaSource>(input.winningSource);
	output.sourceMask = input.sourceMask;
	return output;
}

ZhangRuntimeReceiverMetadata captureMetadata(const ReceiverMetadata& input)
{
	ZhangRuntimeReceiverMetadata output;
	for (E_ReceiverMetaSource source : input.sourcePriority)
	{
		output.sourcePriority.push_back(static_cast<int>(source));
	}
	output.receiverType = captureMetadataField(input.receiverType);
	output.receiverFirmware = captureMetadataField(input.receiverFirmware);
	output.receiverSerial = captureMetadataField(input.receiverSerial);
	output.antennaDescriptor = captureMetadataField(input.antennaDescriptor);
	output.antennaSerial = captureMetadataField(input.antennaSerial);
	output.markerName = captureMetadataField(input.markerName);
	output.markerNumber = captureMetadataField(input.markerNumber);
	output.antennaDelta = captureMetadataField(input.antennaDelta);
	output.stationPosition = captureMetadataField(input.stationPosition);
	return output;
}

ReceiverMetadata restoreMetadata(const ZhangRuntimeReceiverMetadata& input)
{
	ReceiverMetadata output;
	output.sourcePriority.clear();
	for (int source : input.sourcePriority)
	{
		output.sourcePriority.push_back(
			static_cast<E_ReceiverMetaSource>(source));
	}
	output.receiverType = restoreMetadataField(input.receiverType);
	output.receiverFirmware = restoreMetadataField(input.receiverFirmware);
	output.receiverSerial = restoreMetadataField(input.receiverSerial);
	output.antennaDescriptor = restoreMetadataField(input.antennaDescriptor);
	output.antennaSerial = restoreMetadataField(input.antennaSerial);
	output.markerName = restoreMetadataField(input.markerName);
	output.markerNumber = restoreMetadataField(input.markerNumber);
	output.antennaDelta = restoreMetadataField(input.antennaDelta);
	output.stationPosition = restoreMetadataField(input.stationPosition);
	return output;
}

template <typename TYPE>
bool validMetadataField(
	const ZhangRuntimeMetadataField<TYPE>& input,
	bool finiteValue)
{
	constexpr std::uint32_t validSourceMask = 0x0F;
	if ((input.sourceMask & ~validSourceMask) != 0
		|| input.winningSource < static_cast<int>(E_ReceiverMetaSource::NONE)
		|| input.winningSource > static_cast<int>(E_ReceiverMetaSource::RTCM)
		|| !finiteValue)
	{
		return false;
	}
	if (!input.valid)
	{
		return input.winningSource
				   == static_cast<int>(E_ReceiverMetaSource::NONE)
			&& input.sourceMask == 0;
	}
	if (input.winningSource
		== static_cast<int>(E_ReceiverMetaSource::NONE))
	{
		return false;
	}
	const auto source =
		static_cast<E_ReceiverMetaSource>(input.winningSource);
	return (input.sourceMask & receiverMetaSourceBit(source)) != 0;
}

bool validMetadata(
	const ZhangRuntimeReceiverMetadata& input,
	string& failureReason)
{
	set<int> priorities;
	if (input.sourcePriority.empty())
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_INVALID_METADATA_PRIORITY";
		return false;
	}
	for (int source : input.sourcePriority)
	{
		if (source < static_cast<int>(E_ReceiverMetaSource::CONFIG)
			|| source > static_cast<int>(E_ReceiverMetaSource::RTCM)
			|| !priorities.insert(source).second)
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_INVALID_METADATA_PRIORITY";
			return false;
		}
	}
	if (!validMetadataField(input.receiverType, true)
		|| !validMetadataField(input.receiverFirmware, true)
		|| !validMetadataField(input.receiverSerial, true)
		|| !validMetadataField(input.antennaDescriptor, true)
		|| !validMetadataField(input.antennaSerial, true)
		|| !validMetadataField(input.markerName, true)
		|| !validMetadataField(input.markerNumber, true)
		|| !validMetadataField(
			input.antennaDelta, input.antennaDelta.value.allFinite())
		|| !validMetadataField(
			input.stationPosition, input.stationPosition.value.allFinite()))
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_METADATA_FIELD";
		return false;
	}
	auto winnerEnabled = [&](const auto& field)
	{
		return !field.valid
			|| priorities.find(field.winningSource) != priorities.end();
	};
	if (!winnerEnabled(input.receiverType)
		|| !winnerEnabled(input.receiverFirmware)
		|| !winnerEnabled(input.receiverSerial)
		|| !winnerEnabled(input.antennaDescriptor)
		|| !winnerEnabled(input.antennaSerial)
		|| !winnerEnabled(input.markerName)
		|| !winnerEnabled(input.markerNumber)
		|| !winnerEnabled(input.antennaDelta)
		|| !winnerEnabled(input.stationPosition))
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_METADATA_WINNER_DISABLED";
		return false;
	}
	return true;
}

ZhangCheckpointKfKey checkpointKfKey(const KFKey& input)
{
	ZhangCheckpointKfKey output;
	output.type = input.type;
	output.satellite = input.Sat;
	output.receiver = input.str;
	output.number = input.num;
	output.comment = input.comment;
	output.estimatedTime = captureZhangCheckpointTime(input.estimatedTime);
	return output;
}

template <typename CALLBACK>
bool callbackInstalled(const CALLBACK& callback)
{
	return static_cast<bool>(callback);
}

bool noteReceiverBinding(
	const KFKey& key,
	const Receiver& receiver,
	map<ZhangCheckpointKfKey, bool>& bindings,
	string& failureReason)
{
	if (key.rec_ptr && key.rec_ptr != &receiver)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_FOREIGN_RECEIVER_POINTER:"
			+ receiver.id;
		return false;
	}
	const bool bound = key.rec_ptr == &receiver;
	const ZhangCheckpointKfKey stored = checkpointKfKey(key);
	auto [found, inserted] = bindings.emplace(stored, bound);
	if (!inserted && found->second != bound)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_INCONSISTENT_RECEIVER_POINTER:"
			+ receiver.id;
		return false;
	}
	return true;
}

bool captureKfBindingShape(
	const KFState& state,
	const Receiver& receiver,
	ZhangRuntimeKfBindingShape& output,
	string& failureReason)
{
	if (state.alternate_ptr)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_ALTERNATE_FILTER_POINTER:"
			+ receiver.id;
		return false;
	}
	if (!state.stateRejectCallbacks.empty()
		|| state.measRejectCallbacks.size() > 1
		|| (state.measRejectCallbacks.size() == 1
			&& state.measRejectCallbacks.front() != deweightMeas)
		|| callbackInstalled(state.acceptedMeasurementFactorCallback)
		|| callbackInstalled(state.stateTransitionFactorCallback)
		|| callbackInstalled(state.exactStateTransformCallback))
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_CALLBACK_SET:"
			+ receiver.id;
		return false;
	}
	auto note = [&](const KFKey& key)
	{
		return noteReceiverBinding(
			key, receiver, output.receiverBindings, failureReason);
	};
	for (const auto& [key, index] : state.kfIndexMap)
		if (!note(key)) return false;
	for (const auto& [destination, sources] : state.stateTransitionMap)
	{
		if (!note(destination)) return false;
		for (const auto& [source, orders] : sources)
			if (!note(source)) return false;
	}
	auto noteScalarMap = [&](const auto& values)
	{
		for (const auto& [key, value] : values)
			if (!note(key)) return false;
		return true;
	};
	if (!noteScalarMap(state.gaussMarkovTauMap)
		|| !noteScalarMap(state.gaussMarkovMuMap)
		|| !noteScalarMap(state.procNoiseMap)
		|| !noteScalarMap(state.initNoiseMap)
		|| !noteScalarMap(state.sigmaMaxMap)
		|| !noteScalarMap(state.outageLimitMap)
		|| !noteScalarMap(state.exponentialNoiseMap)
		|| !noteScalarMap(state.errorCountMap))
	{
		return false;
	}
	for (const auto& [destination, sources] : state.pseudoStateMap)
	{
		if (!note(destination)) return false;
		for (const auto& [source, value] : sources)
			if (!note(source)) return false;
	}
	for (const auto& [key, parent] : state.pseudoParentMap)
	{
		if (!note(key) || !note(parent)) return false;
	}
	for (const auto& [name, chunk] : state.filterChunkMap)
	{
		output.filterChunkTraceBindings[name] = chunk.trace_ptr != nullptr;
	}
	output.stateRejectCallbackCount = state.stateRejectCallbacks.size();
	output.measurementRejectCallbackCount = state.measRejectCallbacks.size();
	output.acceptedMeasurementCallback =
		callbackInstalled(state.acceptedMeasurementFactorCallback);
	output.stateTransitionCallback =
		callbackInstalled(state.stateTransitionFactorCallback);
	output.exactStateTransformCallback =
		callbackInstalled(state.exactStateTransformCallback);
	return true;
}

void collectCoreKeys(
	const ZhangCheckpointKfCore& core,
	set<ZhangCheckpointKfKey>& output)
{
	for (const auto& [key, index] : core.kfIndexMap) output.insert(key);
	for (const auto& [destination, sources] : core.stateTransitionMap)
	{
		output.insert(destination);
		for (const auto& [source, orders] : sources) output.insert(source);
	}
	auto collectScalar = [&](const auto& values)
	{
		for (const auto& [key, value] : values) output.insert(key);
	};
	collectScalar(core.gaussMarkovTauMap);
	collectScalar(core.gaussMarkovMuMap);
	collectScalar(core.procNoiseMap);
	collectScalar(core.initNoiseMap);
	collectScalar(core.sigmaMaxMap);
	collectScalar(core.outageLimitMap);
	collectScalar(core.exponentialNoiseMap);
	collectScalar(core.errorCountMap);
	for (const auto& [destination, sources] : core.pseudoStateMap)
	{
		output.insert(destination);
		for (const auto& [source, value] : sources) output.insert(source);
	}
	for (const auto& [key, parent] : core.pseudoParentMap)
	{
		output.insert(key);
		output.insert(parent);
	}
}

bool validKfBindingShape(
	const ZhangCheckpointKfCore& core,
	const ZhangRuntimeKfBindingShape& shape,
	string& failureReason)
{
	if (shape.stateRejectCallbackCount != 0
		|| shape.measurementRejectCallbackCount > 1
		|| shape.acceptedMeasurementCallback
		|| shape.stateTransitionCallback
		|| shape.exactStateTransformCallback)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_CALLBACK_SET";
		return false;
	}
	KFState scratch;
	if (!restoreZhangCheckpointKfCore(core, scratch, &failureReason))
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_INVALID_SPP_CORE:" + failureReason;
		return false;
	}
	set<ZhangCheckpointKfKey> keys;
	collectCoreKeys(core, keys);
	if (shape.receiverBindings.size() != keys.size())
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_SPP_POINTER_BINDING_INVENTORY_MISMATCH";
		return false;
	}
	for (const auto& key : keys)
	{
		if (shape.receiverBindings.find(key) == shape.receiverBindings.end())
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_SPP_POINTER_BINDING_MISSING";
			return false;
		}
	}
	if (shape.filterChunkTraceBindings.size() != core.filterChunkMap.size())
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_FILTER_CHUNK_BINDING_INVENTORY_MISMATCH";
		return false;
	}
	for (const auto& [name, chunk] : core.filterChunkMap)
	{
		if (shape.filterChunkTraceBindings.find(name)
			== shape.filterChunkTraceBindings.end())
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_FILTER_CHUNK_BINDING_MISSING";
			return false;
		}
	}
	return true;
}

bool configuredKfBindingsCompatible(
	const KFState& configured,
	const ZhangCheckpointKfCore& core,
	const ZhangRuntimeKfBindingShape& shape,
	const string& receiverId,
	string& failureReason)
{
	if (configured.alternate_ptr)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_CONFIGURED_SPP_ALTERNATE_POINTER:"
			+ receiverId;
		return false;
	}
	if (shape.stateRejectCallbackCount != 0
		|| shape.measurementRejectCallbackCount > 1
		|| shape.acceptedMeasurementCallback
		|| shape.stateTransitionCallback
		|| shape.exactStateTransformCallback)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_CALLBACK_REBIND:"
			+ receiverId;
		return false;
	}
	for (const auto& [name, bound] : shape.filterChunkTraceBindings)
	{
		auto found = configured.filterChunkMap.find(name);
		if (bound
			&& (found == configured.filterChunkMap.end()
				|| found->second.trace_ptr == nullptr))
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_FILTER_CHUNK_TRACE_REBIND_MISSING:"
				+ receiverId + ":" + name;
			return false;
		}
		if (found != configured.filterChunkMap.end())
		{
			auto stored = core.filterChunkMap.find(name);
			if (stored == core.filterChunkMap.end()
				|| found->second.id != stored->second.id)
			{
				failureReason =
					"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_FILTER_CHUNK_ID_REBIND_MISMATCH:"
					+ receiverId + ":" + name;
				return false;
			}
		}
	}
	return true;
}

KFKey rebindRuntimeKey(
	KFKey key,
	const ZhangRuntimeKfBindingShape& shape,
	Receiver* receiver)
{
	auto found = shape.receiverBindings.find(checkpointKfKey(key));
	key.rec_ptr =
		found != shape.receiverBindings.end() && found->second
			? receiver
			: nullptr;
	return key;
}

void rebindKfReceiverPointers(
	KFState& state,
	const ZhangRuntimeKfBindingShape& shape,
	Receiver* receiver)
{
	decltype(state.kfIndexMap) kfIndexMap;
	for (const auto& [key, value] : state.kfIndexMap)
		kfIndexMap[rebindRuntimeKey(key, shape, receiver)] = value;
	state.kfIndexMap.swap(kfIndexMap);

	decltype(state.stateTransitionMap) transitionMap;
	for (const auto& [destination, sources] : state.stateTransitionMap)
	for (const auto& [source, orders] : sources)
	{
		transitionMap[rebindRuntimeKey(destination, shape, receiver)]
			[rebindRuntimeKey(source, shape, receiver)] = orders;
	}
	state.stateTransitionMap.swap(transitionMap);

	auto rebindScalarMap = [&](auto& values)
	{
		using MapType = std::decay_t<decltype(values)>;
		MapType rebound;
		for (const auto& [key, value] : values)
			rebound[rebindRuntimeKey(key, shape, receiver)] = value;
		values.swap(rebound);
	};
	rebindScalarMap(state.gaussMarkovTauMap);
	rebindScalarMap(state.gaussMarkovMuMap);
	rebindScalarMap(state.procNoiseMap);
	rebindScalarMap(state.initNoiseMap);
	rebindScalarMap(state.sigmaMaxMap);
	rebindScalarMap(state.outageLimitMap);
	rebindScalarMap(state.exponentialNoiseMap);
	rebindScalarMap(state.errorCountMap);

	decltype(state.pseudoStateMap) pseudoStateMap;
	for (const auto& [destination, sources] : state.pseudoStateMap)
	for (const auto& [source, value] : sources)
	{
		pseudoStateMap[rebindRuntimeKey(destination, shape, receiver)]
			[rebindRuntimeKey(source, shape, receiver)] = value;
	}
	state.pseudoStateMap.swap(pseudoStateMap);

	decltype(state.pseudoParentMap) pseudoParentMap;
	for (const auto& [key, parent] : state.pseudoParentMap)
	{
		pseudoParentMap[rebindRuntimeKey(key, shape, receiver)] =
			rebindRuntimeKey(parent, shape, receiver);
	}
	state.pseudoParentMap.swap(pseudoParentMap);
}

bool captureSolution(
	const Solution& input,
	const Receiver& receiver,
	ZhangRuntimeSolution& output,
	string& failureReason)
{
	output.sppTime = captureZhangCheckpointTime(input.sppTime);
	output.sppState = captureZhangCheckpointKfCore(input.sppState);
	if (!captureKfBindingShape(
			input.sppState, receiver, output.sppBindingShape, failureReason))
	{
		return false;
	}
	output.sppPosition = input.sppPos;
	output.sppClock = input.sppClk;
	output.clockReferenceSystem = static_cast<int>(input.clkRefSys);
	output.sppPppClockOffset = input.sppPppClkOffset;
	output.clockAdjustmentReady = input.clkAdjustReady;
	output.status = static_cast<int>(input.status);
	output.measurementCount = input.numMeas;
	output.gdop = input.dops.gdop;
	output.pdop = input.dops.pdop;
	output.hdop = input.dops.hdop;
	output.vdop = input.dops.vdop;
	output.horizontalProtectionLevel = input.horzPL;
	output.verticalProtectionLevel = input.vertPL;
	return true;
}

bool validSolution(
	const ZhangRuntimeSolution& input,
	string& failureReason)
{
	if (!finiteTime(input.sppTime) || !input.sppPosition.allFinite()
		|| !finiteValues({
			input.sppClock,
			input.sppPppClockOffset,
			input.gdop,
			input.pdop,
			input.hdop,
			input.vdop,
			input.horizontalProtectionLevel,
			input.verticalProtectionLevel})
		|| !validSystem(input.clockReferenceSystem)
		|| input.status < static_cast<int>(E_Solution::NONE)
		|| input.status > static_cast<int>(E_Solution::PPP)
		|| input.measurementCount < 0)
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_SOLUTION";
		return false;
	}
	return validKfBindingShape(
		input.sppState, input.sppBindingShape, failureReason);
}

bool restoreSolution(
	const ZhangRuntimeSolution& input,
	const KFState& configuredState,
	Receiver& receiver,
	Solution& output,
	string& failureReason)
{
	output.sppTime = restoreZhangCheckpointTime(input.sppTime);
	output.sppState = configuredState;
	map<string, Trace*> traceBindings;
	for (const auto& [name, chunk] : configuredState.filterChunkMap)
	{
		traceBindings[name] = chunk.trace_ptr;
	}
	if (!restoreZhangCheckpointKfCore(
			input.sppState, output.sppState, &failureReason))
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_SPP_CORE_RESTORE_FAILED:"
			+ failureReason;
		return false;
	}
	output.sppState.stateRejectCallbacks.clear();
	output.sppState.measRejectCallbacks.clear();
	if (input.sppBindingShape.measurementRejectCallbackCount == 1)
	{
		output.sppState.measRejectCallbacks.push_back(deweightMeas);
	}
	output.sppState.acceptedMeasurementFactorCallback = {};
	output.sppState.stateTransitionFactorCallback = {};
	output.sppState.exactStateTransformCallback = {};
	for (auto& [name, chunk] : output.sppState.filterChunkMap)
	{
		auto bound = input.sppBindingShape.filterChunkTraceBindings.find(name);
		auto pointer = traceBindings.find(name);
		chunk.trace_ptr =
			bound != input.sppBindingShape.filterChunkTraceBindings.end()
				&& bound->second && pointer != traceBindings.end()
				? pointer->second
				: nullptr;
	}
	rebindKfReceiverPointers(
		output.sppState, input.sppBindingShape, &receiver);
	output.sppState.rts_basename = input.sppState.rtsBasename;
	output.sppPos = input.sppPosition;
	output.sppClk = input.sppClock;
	output.clkRefSys = static_cast<E_Sys>(input.clockReferenceSystem);
	output.sppPppClkOffset = input.sppPppClockOffset;
	output.clkAdjustReady = input.clockAdjustmentReady;
	output.status = static_cast<E_Solution>(input.status);
	output.numMeas = input.measurementCount;
	output.dops = {input.gdop, input.pdop, input.hdop, input.vdop};
	output.horzPL = input.horizontalProtectionLevel;
	output.vertPL = input.verticalProtectionLevel;
	return true;
}

bool captureReceiver(
	const string& mapKey,
	const Receiver& input,
	ZhangRuntimeReceiver& output,
	string& failureReason)
{
	output.mapKey = mapKey;
	output.id = input.id;
	output.pseudoReceiver = input.isPseudoRec;
	output.invalid = input.invalid;
	output.sinex.siteBindingFingerprint =
		siteBindingFingerprint(input.snx.id_ptr);
	output.sinex.receiverBindingFingerprint =
		receiverBindingFingerprint(input.snx.rec_ptr);
	output.sinex.antennaBindingFingerprint =
		antennaBindingFingerprint(input.snx.ant_ptr);
	output.sinex.eccentricityBindingFingerprint =
		eccentricityBindingFingerprint(input.snx.ecc_ptr);
	if (output.sinex.siteBindingFingerprint.empty()
		|| output.sinex.receiverBindingFingerprint.empty()
		|| output.sinex.antennaBindingFingerprint.empty()
		|| output.sinex.eccentricityBindingFingerprint.empty())
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_NULL_SINEX_POINTER:"
			+ mapKey;
		return false;
	}
	output.sinex.start = captureYds(input.snx.start);
	output.sinex.stop = captureYds(input.snx.stop);
	output.sinex.primary = input.snx.primary;
	output.sinex.position = input.snx.pos;
	output.sinex.variance = input.snx.var;
	output.sinex.velocity = input.snx.vel;
	output.sinex.referenceEpoch =
		captureZhangCheckpointTime(input.snx.refEpoch);
	output.metadata = captureMetadata(input.metadata);
	output.metadataMap = input.metaDataMap;
	output.source = input.source;
	output.firstEpoch = captureZhangCheckpointTime((GTime)input.firstEpoch);
	output.lastEpoch = captureZhangCheckpointTime((GTime)input.lastEpoch);
	output.epochCount = input.epochCount;
	output.observationCount = input.obsCount;
	output.slipCount = input.slipCount;
	for (const auto& [code, count] : input.codeCount)
		output.codeCount[static_cast<int>(code)] = count;
	output.satelliteCount = input.satCount;
	output.receiverErrorEpochs = input.receiverErrorEpochs;
	output.receiverErrorCount = input.receiverErrorCount;
	if (!captureSolution(input.sol, input, output.solution, failureReason))
		return false;
	output.antennaType = input.antennaType;
	output.receiverType = input.receiverType;
	output.antennaId = input.antennaId;
	for (const auto& [satellite, status] : input.satStatMap)
		output.satelliteStatuses[satellite] = captureSatelliteStatus(status);
	output.antennaDelta = input.antDelta;
	output.attitude = captureAttitude(input.attStatus);
	output.primaryApriori = input.primaryApriori;
	output.aprioriTime = captureYds(input.aprioriTime);
	output.aprioriClock = input.aprioriClk;
	output.aprioriClockVariance = input.aprioriClkVar;
	output.aprioriPosition = input.aprioriPos;
	output.aprioriPositionVariance = input.aprioriPosVar;
	output.minimumConstraintApriori = input.minconApriori;
	output.geodeticPosition = input.pos;
	output.ready = input.ready;
	output.antennaBoresight = input.antBoresight;
	output.antennaAzimuth = input.antAzimuth;
	output.traceFilename = input.traceFilename;
	output.jsonTraceFilename = input.jsonTraceFilename;
	output.sppOutputFile = input.sppOutputFile;
	for (const auto& [satellite, time] : input.savedSlips)
		output.savedSlips[satellite] = captureZhangCheckpointTime(time);
	for (const auto& [system, signals] : input.trackedSignals)
	{
		auto& target = output.trackedSignals[static_cast<int>(system)];
		for (E_ObsCode signal : signals)
			target.push_back(static_cast<int>(signal));
	}
	output.failureFlags = input.failure;
	output.resetObservationCount = input.obsList.size();
	return true;
}

bool validReceiver(
	const ZhangRuntimeReceiver& input,
	string& failureReason)
{
	if (input.mapKey.empty() || input.id.empty() || input.mapKey != input.id)
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_RECEIVER_IDENTITY";
		return false;
	}
	if (input.sinex.siteBindingFingerprint.empty()
		|| input.sinex.receiverBindingFingerprint.empty()
		|| input.sinex.antennaBindingFingerprint.empty()
		|| input.sinex.eccentricityBindingFingerprint.empty()
		|| !validYds(input.sinex.start) || !validYds(input.sinex.stop)
		|| !finiteTime(input.sinex.referenceEpoch)
		|| !input.sinex.position.allFinite()
		|| !input.sinex.variance.allFinite()
		|| !input.sinex.velocity.allFinite())
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_SINEX_STATE";
		return false;
	}
	if (!validMetadata(input.metadata, failureReason)
		|| !finiteTime(input.firstEpoch) || !finiteTime(input.lastEpoch)
		|| input.epochCount < 0 || input.observationCount < 0
		|| input.slipCount < 0 || input.receiverErrorEpochs < 0
		|| input.receiverErrorCount < 0)
	{
		if (failureReason.empty())
			failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_COUNTER";
		return false;
	}
	for (const auto& [code, count] : input.codeCount)
	{
		if (!validObservationCode(code) || count < 0)
		{
			failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_CODE_COUNT";
			return false;
		}
	}
	for (const auto& [satellite, count] : input.satelliteCount)
	{
		if (satellite.empty() || count < 0)
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_INVALID_SATELLITE_COUNT";
			return false;
		}
	}
	if (!validSolution(input.solution, failureReason))
		return false;
	for (const auto& [key, bound] :
		 input.solution.sppBindingShape.receiverBindings)
	{
		if (bound && key.receiver != input.id)
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SPP_RECEIVER_BINDING_ID";
			return false;
		}
	}
	for (const auto& [satellite, status] : input.satelliteStatuses)
	{
		if (!validSatellite(satellite)
			|| !validSatelliteStatus(satellite, status, failureReason))
		{
			if (failureReason.empty())
				failureReason =
					"RECEIVER_RUNTIME_CHECKPOINT_INVALID_SATELLITE_STATUS_ID";
			return false;
		}
	}
	if (!input.antennaDelta.allFinite()
		|| !validAttitude(input.attitude)
		|| !validYds(input.aprioriTime)
		|| !finiteValues({input.aprioriClock, input.aprioriClockVariance})
		|| !input.aprioriPosition.allFinite()
		|| !input.aprioriPositionVariance.allFinite()
		|| !input.minimumConstraintApriori.allFinite()
		|| !input.geodeticPosition.allFinite()
		|| !input.antennaBoresight.allFinite()
		|| !input.antennaAzimuth.allFinite()
		|| (input.failureFlags & ~0x07u) != 0)
	{
		failureReason = "RECEIVER_RUNTIME_CHECKPOINT_NONFINITE_RECEIVER_STATE";
		return false;
	}
	for (const auto& [satellite, time] : input.savedSlips)
	{
		if (!validSatellite(satellite) || !finiteTime(time))
		{
			failureReason = "RECEIVER_RUNTIME_CHECKPOINT_INVALID_SAVED_SLIP";
			return false;
		}
	}
	for (const auto& [system, signals] : input.trackedSignals)
	{
		if (!validSystem(system, false))
		{
			failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_INVALID_TRACKED_SYSTEM";
			return false;
		}
		for (int signal : signals)
		{
			if (!validObservationCode(signal))
			{
				failureReason =
					"RECEIVER_RUNTIME_CHECKPOINT_INVALID_TRACKED_SIGNAL";
				return false;
			}
		}
	}
	return true;
}

bool receiverConfigurationCompatible(
	const Receiver& configured,
	const ZhangRuntimeReceiver& stored,
	string& failureReason)
{
	if (configured.id != stored.id)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_CONFIGURED_RECEIVER_ID_MISMATCH:"
			+ stored.mapKey;
		return false;
	}
	if (!resolveSiteBinding(stored.sinex.siteBindingFingerprint)
	 || !resolveReceiverBinding(stored.sinex.receiverBindingFingerprint)
	 || !resolveAntennaBinding(stored.sinex.antennaBindingFingerprint)
	 || !resolveEccentricityBinding(
			stored.sinex.eccentricityBindingFingerprint))
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_SINEX_POINTER_REBIND_MISMATCH:"
			+ stored.mapKey;
		return false;
	}
	return configuredKfBindingsCompatible(
		configured.sol.sppState,
		stored.solution.sppState,
		stored.solution.sppBindingShape,
		stored.mapKey,
		failureReason);
}

struct PreparedReceiver
{
	Receiver* destination = nullptr;
	const ZhangRuntimeReceiver* stored = nullptr;
	Solution solution;
	map<SatSys, SatStat> satelliteStatuses;
	ReceiverMetadata metadata;
	AttStatus attitude;
	SinexSiteId* siteBinding = nullptr;
	SinexReceiver* receiverBinding = nullptr;
	SinexAntenna* antennaBinding = nullptr;
	SinexSiteEcc* eccentricityBinding = nullptr;
};

bool prepareReceiver(
	Receiver& destination,
	const ZhangRuntimeReceiver& stored,
	PreparedReceiver& prepared,
	string& failureReason)
{
	prepared.destination = &destination;
	prepared.stored = &stored;
	if (!restoreSolution(
			stored.solution,
			destination.sol.sppState,
			destination,
			prepared.solution,
			failureReason))
	{
		return false;
	}
	for (const auto& [satellite, status] : stored.satelliteStatuses)
	{
		prepared.satelliteStatuses[satellite] = restoreSatelliteStatus(status);
	}
	prepared.metadata = restoreMetadata(stored.metadata);
	prepared.attitude = restoreAttitude(stored.attitude);
	prepared.siteBinding = resolveSiteBinding(
		stored.sinex.siteBindingFingerprint);
	prepared.receiverBinding = resolveReceiverBinding(
		stored.sinex.receiverBindingFingerprint);
	prepared.antennaBinding = resolveAntennaBinding(
		stored.sinex.antennaBindingFingerprint);
	prepared.eccentricityBinding = resolveEccentricityBinding(
		stored.sinex.eccentricityBindingFingerprint);
	if (!prepared.siteBinding || !prepared.receiverBinding
	 || !prepared.antennaBinding || !prepared.eccentricityBinding)
	{
		failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_SINEX_BINDING_RESOLUTION_FAILED:"
			+ stored.mapKey;
		return false;
	}
	return true;
}

void restoreReceiverFailureFlags(Receiver& output, unsigned int flags)
{
	output.failureSinex = (flags >> 0) & 1u;
	output.failureAprioriPos = (flags >> 1) & 1u;
	output.failureEccentricity = (flags >> 2) & 1u;
}

void commitPreparedReceiver(PreparedReceiver& prepared)
{
	Receiver& output = *prepared.destination;
	const ZhangRuntimeReceiver& input = *prepared.stored;

	output.firstEpoch = restoreZhangCheckpointTime(input.firstEpoch);
	output.lastEpoch = restoreZhangCheckpointTime(input.lastEpoch);
	output.epochCount = input.epochCount;
	output.obsCount = input.observationCount;
	output.slipCount = input.slipCount;
	output.codeCount.clear();
	for (const auto& [code, count] : input.codeCount)
		output.codeCount[static_cast<E_ObsCode>(code)] = count;
	output.satCount = input.satelliteCount;
	output.receiverErrorEpochs = input.receiverErrorEpochs;
	output.receiverErrorCount = input.receiverErrorCount;

	output.sol = prepared.solution;
	// KFState::operator= deliberately clears this member; checkpoint restore
	// owns the persisted value and reinstalls it after assignment.
	output.sol.sppState.rts_basename = input.solution.sppState.rtsBasename;
	output.antennaType = input.antennaType;
	output.receiverType = input.receiverType;
	output.antennaId = input.antennaId;
	output.satStatMap.swap(prepared.satelliteStatuses);
	output.antDelta = input.antennaDelta;
	output.attStatus = prepared.attitude;

	output.isPseudoRec = input.pseudoReceiver;
	output.invalid = input.invalid;
	// Rebind to the exact semantic records selected at the checkpoint epoch.
	// Fresh startup pointers may still designate dummy or different-time SINEX
	// records because historical preprocessing has not been replayed.
	output.snx.id_ptr = prepared.siteBinding;
	output.snx.rec_ptr = prepared.receiverBinding;
	output.snx.ant_ptr = prepared.antennaBinding;
	output.snx.ecc_ptr = prepared.eccentricityBinding;
	output.snx.start = restoreYds(input.sinex.start);
	output.snx.stop = restoreYds(input.sinex.stop);
	output.snx.primary = input.sinex.primary;
	output.snx.pos = input.sinex.position;
	output.snx.var = input.sinex.variance;
	output.snx.vel = input.sinex.velocity;
	output.snx.refEpoch = restoreZhangCheckpointTime(input.sinex.referenceEpoch);
	output.metadata = prepared.metadata;
	output.metaDataMap = input.metadataMap;
	output.obsList.clear();
	output.id = input.id;
	output.source = input.source;
	output.primaryApriori = input.primaryApriori;
	output.aprioriTime = restoreYds(input.aprioriTime);
	output.aprioriClk = input.aprioriClock;
	output.aprioriClkVar = input.aprioriClockVariance;
	output.aprioriPos = input.aprioriPosition;
	output.aprioriPosVar = input.aprioriPositionVariance;
	output.minconApriori = input.minimumConstraintApriori;
	output.pos = input.geodeticPosition;
	// `ready` describes the already committed workset.  Resume starts before
	// synchronising the next epoch, therefore replay must force not-ready.
	output.ready = false;
	output.antBoresight = input.antennaBoresight;
	output.antAzimuth = input.antennaAzimuth;
	output.traceFilename = input.traceFilename;
	output.jsonTraceFilename = input.jsonTraceFilename;
	output.sppOutputFile = input.sppOutputFile;
	output.savedSlips.clear();
	for (const auto& [satellite, time] : input.savedSlips)
		output.savedSlips[satellite] = restoreZhangCheckpointTime(time);
	output.trackedSignals.clear();
	for (const auto& [system, signals] : input.trackedSignals)
	{
		auto& target = output.trackedSignals[static_cast<E_Sys>(system)];
		for (int signal : signals)
			target.push_back(static_cast<E_ObsCode>(signal));
	}
	restoreReceiverFailureFlags(output, input.failureFlags);

	// These caches contain std::function closures and are intentionally never
	// serialized.  They are derived exclusively from the next epoch inputs.
	output.pppTideCache.lambda = {};
	output.pppTideCache.initialised = false;
	output.pppEopCache.lambda = {};
	output.pppEopCache.initialised = false;
}

bool validCustomAliases(
	const map<string, set<string>>& aliases,
	ZhangReceiverRuntimeCheckpointResult& result)
{
	for (const auto& [receiver, values] : aliases)
	{
		if (receiver.empty())
		{
			result.failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_INVALID_DYNAMIC_ALIAS_RECEIVER";
			return false;
		}
		++result.dynamicAliasReceiverCount;
		for (const auto& alias : values)
		{
			if (alias.empty())
			{
				result.failureReason =
					"RECEIVER_RUNTIME_CHECKPOINT_INVALID_DYNAMIC_ALIAS";
				return false;
			}
			++result.dynamicAliasCount;
		}
	}
	return true;
}

bool decodeAndValidateReceiverEnvelope(
	const string& payload,
	const string& runtimeId,
	ZhangReceiverRuntimeEnvelope& envelope,
	ZhangReceiverRuntimeCheckpointResult& result)
{
	if (runtimeId.empty())
	{
		result.failureReason = "RECEIVER_RUNTIME_CHECKPOINT_RUNTIME_ID_EMPTY";
		return false;
	}
	if (!deserializePayload(
			payload,
			envelope,
			result.failureReason,
			"RECEIVER_RUNTIME_CHECKPOINT"))
	{
		return false;
	}
	if (envelope.schemaVersion
			!= ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SCHEMA_VERSION
		|| envelope.sectionName
			!= ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SECTION_NAME)
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_SCHEMA_OR_SECTION_MISMATCH";
		return false;
	}
	if (envelope.runtimeId != runtimeId)
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_RUNTIME_ID_MISMATCH";
		return false;
	}
	if (envelope.boundary != POST_EPOCH_BOUNDARY
		|| envelope.observationPolicy != OBSERVATION_POLICY
		|| envelope.cachePolicy != CACHE_POLICY
		|| envelope.readyPolicy != READY_POLICY
		|| envelope.configurationPointerPolicy != CONFIG_POINTER_POLICY)
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_UNSUPPORTED_RESTORE_POLICY";
		return false;
	}
	if (!validCustomAliases(envelope.customAliasesMap, result))
	{
		return false;
	}
	set<string> identities;
	for (const auto& receiver : envelope.receivers)
	{
		if (!identities.insert(receiver.mapKey).second)
		{
			result.failureReason =
				"RECEIVER_RUNTIME_CHECKPOINT_DUPLICATE_RECEIVER";
			return false;
		}
		if (!validReceiver(receiver, result.failureReason))
			return false;
		result.satelliteStatusCount += receiver.satelliteStatuses.size();
		result.resetObservationCount += receiver.resetObservationCount;
		for (const auto& [satellite, status] : receiver.satelliteStatuses)
			result.signalStatusCount += status.signalStatuses.size();
	}
	result.receiverCount = envelope.receivers.size();
	return true;
}

bool receiverInventoryCompatible(
	const ReceiverMap& configured,
	const ZhangReceiverRuntimeEnvelope& envelope,
	ZhangReceiverRuntimeCheckpointResult& result)
{
	if (configured.size() != envelope.receivers.size())
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_RECEIVER_INVENTORY_MISMATCH";
		return false;
	}
	for (const auto& stored : envelope.receivers)
	{
		auto found = configured.find(stored.mapKey);
		if (found == configured.end()
			|| !receiverConfigurationCompatible(
				found->second, stored, result.failureReason))
		{
			if (result.failureReason.empty())
				result.failureReason =
					"RECEIVER_RUNTIME_CHECKPOINT_RECEIVER_INVENTORY_MISMATCH";
			return false;
		}
	}
	return true;
}

bool hasUnsupportedSsrOrSbas(const SatNav& input)
{
	const auto& ssr = input.receivedSSR;
	const auto& sbas = input.currentSBAS;
	return !ssr.ssrCodeBias_map.empty()
		|| !ssr.ssrPhasBias_map.empty()
		|| !ssr.ssrClk_map.empty()
		|| !ssr.ssrEph_map.empty()
		|| !ssr.ssrHRClk_map.empty()
		|| !ssr.ssrUra_map.empty()
		|| !sbas.fastUpdt.empty()
		|| !sbas.slowUpdt.empty()
		|| !sbas.fastCorr.empty()
		|| !sbas.slowCorr.empty();
}

bool hasUnsupportedGlobalSsrOrSbas(const Navigation& input)
{
	return !input.ssrAtm.atmosGlobalMap.empty()
		|| !input.ssrAtm.atmosRegionsMap.empty()
		|| !input.sbsIono.IGPLati.empty()
		|| !input.sbsIono.IGPLong.empty()
		|| !input.sbsIono.IGPGIVD.empty()
		|| !input.sbsIono.IGPGIVE.empty();
}

bool captureSatPos(
	const SatPos& input,
	const SatNav& owner,
	const SatSys& ownerSatellite,
	ZhangRuntimeSatPos& output,
	string& failureReason)
{
	if (input.satNav_ptr && input.satNav_ptr != &owner)
	{
		failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_FOREIGN_SATNAV_POINTER:"
			+ ownerSatellite.id();
		return false;
	}
	if (input.satStat_ptr)
	{
		failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_SATSTAT_POINTER:"
			+ ownerSatellite.id();
		return false;
	}
	output.time = captureZhangCheckpointTime(input.posTime);
	output.satellite = input.Sat;
	output.ownerSatNavBinding = input.satNav_ptr == &owner;
	output.positionSource = static_cast<int>(input.posSource);
	output.clockSource = static_cast<int>(input.clkSource);
	output.comPosition = input.rSatCom;
	output.apcPosition = input.rSatApc;
	output.velocity = input.satVel;
	output.eciPositionAtTransmission = input.rSatEciDt;
	output.eciVelocityAtTransmission = input.vSatEciDt;
	output.eciPositionAtEpoch = input.rSatEci0;
	output.eciVelocityAtEpoch = input.vSatEci0;
	output.positionVariance = input.posVar;
	output.clock = input.satClk;
	output.clockVelocity = input.satClkVel;
	output.clockVariance = input.satClkVar;
	output.sppValid = input.sppValid;
	output.clockIode = input.iodeClk;
	output.positionIode = input.iodePos;
	output.ephemerisPositionValid = input.ephPosValid;
	output.ephemerisClockValid = input.ephClkValid;
	output.timeOfFlight = input.tof;
	output.failureFlags = input.failure;
	return true;
}

bool validSatPos(
	const ZhangRuntimeSatPos& input,
	const SatSys& ownerSatellite,
	string& failureReason)
{
	if (!finiteTime(input.time)
		|| !validSatellite(input.satellite, true)
		|| (input.satellite.sys != E_Sys::NONE
			&& input.satellite != ownerSatellite)
		|| input.positionSource < static_cast<int>(E_Source::NONE)
		|| input.positionSource > static_cast<int>(E_Source::REMOTE)
		|| input.clockSource < static_cast<int>(E_Source::NONE)
		|| input.clockSource > static_cast<int>(E_Source::REMOTE)
		|| (input.failureFlags & ~((1u << 19) - 1)) != 0)
	{
		failureReason = "SATELLITE_RUNTIME_CHECKPOINT_INVALID_SATPOS_IDENTITY";
		return false;
	}
	if (!input.comPosition.allFinite() || !input.apcPosition.allFinite()
		|| !input.velocity.allFinite()
		|| !input.eciPositionAtTransmission.allFinite()
		|| !input.eciVelocityAtTransmission.allFinite()
		|| !input.eciPositionAtEpoch.allFinite()
		|| !input.eciVelocityAtEpoch.allFinite()
		|| !finiteValues({
			input.positionVariance,
			input.clock,
			input.clockVelocity,
			input.clockVariance,
			input.timeOfFlight}))
	{
		failureReason = "SATELLITE_RUNTIME_CHECKPOINT_NONFINITE_SATPOS";
		return false;
	}
	return true;
}

void restoreSatPosFailureFlags(SatPos& output, unsigned int flags)
{
	output.failureExclude = (flags >> 0) & 1u;
	output.failureNoSatPos = (flags >> 1) & 1u;
	output.failureNoSatClock = (flags >> 2) & 1u;
	output.failureNoPseudorange = (flags >> 3) & 1u;
	output.failureIodeConsistency = (flags >> 4) & 1u;
	output.failureBroadcastEph = (flags >> 5) & 1u;
	output.failureSSRFail = (flags >> 6) & 1u;
	output.failureSsrPosEmpty = (flags >> 7) & 1u;
	output.failureSsrClkEmpty = (flags >> 8) & 1u;
	output.failureSsrPosTime = (flags >> 9) & 1u;
	output.failureSsrClkTime = (flags >> 10) & 1u;
	output.failureSsrPosMag = (flags >> 11) & 1u;
	output.failureSsrClkMag = (flags >> 12) & 1u;
	output.failureSsrPosUdi = (flags >> 13) & 1u;
	output.failureSsrClkUdi = (flags >> 14) & 1u;
	output.failureGeodist = (flags >> 15) & 1u;
	output.failureRSat = (flags >> 16) & 1u;
	output.failureElevation = (flags >> 17) & 1u;
	output.failurePrange = (flags >> 18) & 1u;
}

void restoreSatPos(
	const ZhangRuntimeSatPos& input,
	SatNav& owner,
	SatPos& output)
{
	output.posTime = restoreZhangCheckpointTime(input.time);
	output.Sat = input.satellite;
	output.satNav_ptr = input.ownerSatNavBinding ? &owner : nullptr;
	output.satStat_ptr = nullptr;
	output.posSource = static_cast<E_Source>(input.positionSource);
	output.clkSource = static_cast<E_Source>(input.clockSource);
	output.rSatCom = input.comPosition;
	output.rSatApc = input.apcPosition;
	output.satVel = input.velocity;
	output.rSatEciDt = input.eciPositionAtTransmission;
	output.vSatEciDt = input.eciVelocityAtTransmission;
	output.rSatEci0 = input.eciPositionAtEpoch;
	output.vSatEci0 = input.eciVelocityAtEpoch;
	output.posVar = input.positionVariance;
	output.satClk = input.clock;
	output.satClkVel = input.clockVelocity;
	output.satClkVar = input.clockVariance;
	output.sppValid = input.sppValid;
	output.iodeClk = input.clockIode;
	output.iodePos = input.positionIode;
	output.ephPosValid = input.ephemerisPositionValid;
	output.ephClkValid = input.ephemerisClockValid;
	output.tof = input.timeOfFlight;
	restoreSatPosFailureFlags(output, input.failureFlags);
}

bool captureSatelliteNavigation(
	const SatSys& satellite,
	const SatNav& input,
	ZhangRuntimeSatelliteNavigation& output,
	string& failureReason)
{
	if (hasUnsupportedSsrOrSbas(input))
	{
		failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_SSR_OR_SBAS_STATE:"
			+ satellite.id();
		return false;
	}
	output.satellite = satellite;
	output.wavelengths = input.lamMap;
	output.aprioriPosition = input.aprioriPos;
	output.aprioriClock = input.aprioriClk;
	output.attitude = captureAttitude(input.attStatus);
	output.id = input.id;
	output.traceFilename = input.traceFilename;
	output.jsonTraceFilename = input.jsonTraceFilename;
	output.antennaBoresight = input.antBoresight;
	output.antennaAzimuth = input.antAzimuth;
	if (!captureSatPos(
			input.satPos0,
			input,
			satellite,
			output.nominalPosition,
			failureReason))
	{
		return false;
	}
	output.satelliteErrorEpochs = input.satelliteErrorEpochs;
	output.satelliteErrorCount = input.satelliteErrorCount;
	return true;
}

bool validSatelliteNavigation(
	const ZhangRuntimeSatelliteNavigation& input,
	string& failureReason)
{
	if (!validSatellite(input.satellite) || !input.aprioriPosition.allFinite()
		|| !std::isfinite(input.aprioriClock)
		|| !validAttitude(input.attitude)
		|| !input.antennaBoresight.allFinite()
		|| !input.antennaAzimuth.allFinite()
		|| input.satelliteErrorEpochs < 0
		|| input.satelliteErrorCount < 0)
	{
		failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_INVALID_SATELLITE_STATE";
		return false;
	}
	for (const auto& [frequency, wavelength] : input.wavelengths)
	{
		if (!validFrequency(frequency) || !std::isfinite(wavelength)
			|| wavelength < 0)
		{
			failureReason =
				"SATELLITE_RUNTIME_CHECKPOINT_INVALID_WAVELENGTH";
			return false;
		}
	}
	return validSatPos(
		input.nominalPosition, input.satellite, failureReason);
}

bool decodeAndValidateSatelliteEnvelope(
	const string& payload,
	const string& runtimeId,
	ZhangSatelliteRuntimeEnvelope& envelope,
	ZhangSatelliteRuntimeCheckpointResult& result)
{
	if (runtimeId.empty())
	{
		result.failureReason = "SATELLITE_RUNTIME_CHECKPOINT_RUNTIME_ID_EMPTY";
		return false;
	}
	if (!deserializePayload(
			payload,
			envelope,
			result.failureReason,
			"SATELLITE_RUNTIME_CHECKPOINT"))
	{
		return false;
	}
	if (envelope.schemaVersion
			!= ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SCHEMA_VERSION
		|| envelope.sectionName
			!= ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SECTION_NAME)
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_SCHEMA_OR_SECTION_MISMATCH";
		return false;
	}
	if (envelope.runtimeId != runtimeId)
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_RUNTIME_ID_MISMATCH";
		return false;
	}
	if (envelope.boundary != POST_EPOCH_BOUNDARY
		|| envelope.scope != SATELLITE_SCOPE
		|| envelope.pointerPolicy != CONFIG_POINTER_POLICY)
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_RESTORE_POLICY";
		return false;
	}
	if (!validErpFilterValues(envelope.erpFilterValues))
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_INVALID_ERP_FILTER_VALUES";
		return false;
	}
	set<SatSys> satellites;
	for (const auto& satellite : envelope.satellites)
	{
		if (!satellites.insert(satellite.satellite).second)
		{
			result.failureReason =
				"SATELLITE_RUNTIME_CHECKPOINT_DUPLICATE_SATELLITE";
			return false;
		}
		if (!validSatelliteNavigation(satellite, result.failureReason))
			return false;
	}
	set<SatSys> aliases;
	for (const auto& alias : envelope.aliases)
	{
		// SatSys::satDataMap is an identity/cache ledger and may contain the
		// canonical NONE/0 sentinel created by a default blockType()/svn()
		// lookup.  Preserve that entry exactly; actual navigation satellites
		// remain subject to the strict non-NONE validation above.
		if (!validSatelliteAliasKey(alias.satellite)
			|| !aliases.insert(alias.satellite).second)
		{
			result.failureReason =
				"SATELLITE_RUNTIME_CHECKPOINT_INVALID_ALIAS";
			return false;
		}
	}
	for (const auto& [satellite, history] : envelope.svnHistory)
	{
		if (!validSatellite(satellite))
		{
			result.failureReason =
				"SATELLITE_RUNTIME_CHECKPOINT_INVALID_SVN_HISTORY_SATELLITE";
			return false;
		}
		set<long double> epochs;
		for (const auto& entry : history)
		{
			const GTime time = restoreZhangCheckpointTime(entry.time);
			if (!finiteTime(entry.time) || entry.svn.empty()
				|| !epochs.insert(time.bigTime).second)
			{
				result.failureReason =
					"SATELLITE_RUNTIME_CHECKPOINT_INVALID_SVN_HISTORY";
				return false;
			}
			++result.svnHistoryCount;
		}
	}
	result.satelliteCount = envelope.satellites.size();
	result.satelliteAliasCount = envelope.aliases.size();
	return true;
}

bool satelliteInventoryCompatible(
	const Navigation& configured,
	const ZhangSatelliteRuntimeEnvelope& envelope,
	ZhangSatelliteRuntimeCheckpointResult& result)
{
	if (hasUnsupportedGlobalSsrOrSbas(configured))
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_CONFIGURED_GLOBAL_SSR_OR_SBAS_STATE";
		return false;
	}
	if (configured.satNavMap.size() != envelope.satellites.size())
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_SATELLITE_INVENTORY_MISMATCH";
		return false;
	}
	for (const auto& stored : envelope.satellites)
	{
		auto found = configured.satNavMap.find(stored.satellite);
		if (found == configured.satNavMap.end())
		{
			result.failureReason =
				"SATELLITE_RUNTIME_CHECKPOINT_SATELLITE_INVENTORY_MISMATCH";
			return false;
		}
		if (hasUnsupportedSsrOrSbas(found->second))
		{
			result.failureReason =
				"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_CONFIGURED_SSR_OR_SBAS_STATE:"
				+ stored.satellite.id();
			return false;
		}
	}
	return true;
}

void commitSatelliteNavigation(
	const ZhangRuntimeSatelliteNavigation& input,
	SatNav& output)
{
	output.lamMap = input.wavelengths;
	output.aprioriPos = input.aprioriPosition;
	output.aprioriClk = input.aprioriClock;
	output.attStatus = restoreAttitude(input.attitude);
	output.id = input.id;
	output.traceFilename = input.traceFilename;
	output.jsonTraceFilename = input.jsonTraceFilename;
	output.antBoresight = input.antennaBoresight;
	output.antAzimuth = input.antennaAzimuth;
	restoreSatPos(input.nominalPosition, output, output.satPos0);
	output.satelliteErrorEpochs = input.satelliteErrorEpochs;
	output.satelliteErrorCount = input.satelliteErrorCount;
}

void fillReceiverResultFromPlan(
	const ZhangReceiverRuntimeCheckpointRestorePlan& plan,
	ZhangReceiverRuntimeCheckpointResult& result)
{
	result.receiverCount = plan.receiverCount;
	result.satelliteStatusCount = plan.satelliteStatusCount;
	result.signalStatusCount = plan.signalStatusCount;
	result.resetObservationCount = plan.resetObservationCount;
	result.dynamicAliasReceiverCount = plan.dynamicAliasReceiverCount;
	result.dynamicAliasCount = plan.dynamicAliasCount;
}

void fillSatelliteResultFromPlan(
	const ZhangSatelliteRuntimeCheckpointRestorePlan& plan,
	ZhangSatelliteRuntimeCheckpointResult& result)
{
	result.satelliteCount = plan.satelliteCount;
	result.satelliteAliasCount = plan.satelliteAliasCount;
	result.svnHistoryCount = plan.svnHistoryCount;
}
}  // namespace

ZhangReceiverRuntimeCheckpointResult
exportZhangReceiverRuntimeCheckpointSection(
	const ReceiverMap& receivers,
	const std::string& runtimeId,
	std::string& payload)
{
	ZhangReceiverRuntimeCheckpointResult result;
	payload.clear();
	if (runtimeId.empty())
	{
		result.failureReason = "RECEIVER_RUNTIME_CHECKPOINT_RUNTIME_ID_EMPTY";
		return result;
	}
	ZhangReceiverRuntimeEnvelope envelope;
	envelope.runtimeId = runtimeId;
	{
		std::lock_guard<std::mutex> guard(acsConfig.configMutex);
		envelope.customAliasesMap = acsConfig.customAliasesMap;
	}
	if (!validCustomAliases(envelope.customAliasesMap, result))
	{
		return result;
	}
	try
	{
		for (const auto& [mapKey, receiver] : receivers)
		{
			ZhangRuntimeReceiver stored;
			if (!captureReceiver(
					mapKey, receiver, stored, result.failureReason)
				|| !validReceiver(stored, result.failureReason))
			{
				return result;
			}
			result.satelliteStatusCount += stored.satelliteStatuses.size();
			result.resetObservationCount += stored.resetObservationCount;
			for (const auto& [satellite, status] : stored.satelliteStatuses)
				result.signalStatusCount += status.signalStatuses.size();
			envelope.receivers.push_back(std::move(stored));
		}
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("RECEIVER_RUNTIME_CHECKPOINT_EXPORT_FAILED:")
			+ exception.what();
		return result;
	}
	result.receiverCount = envelope.receivers.size();
	if (!serializePayload(
			envelope,
			payload,
			result.failureReason,
			"RECEIVER_RUNTIME_CHECKPOINT"))
	{
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

ZhangReceiverRuntimeCheckpointResult
preflightZhangReceiverRuntimeCheckpointSection(
	const ReceiverMap& configuredReceivers,
	const std::string& runtimeId,
	const std::string& payload,
	ZhangReceiverRuntimeCheckpointRestorePlan& plan)
{
	plan = {};
	ZhangReceiverRuntimeCheckpointResult result;
	ZhangReceiverRuntimeEnvelope envelope;
	if (!decodeAndValidateReceiverEnvelope(
			payload, runtimeId, envelope, result)
		|| !receiverInventoryCompatible(
			configuredReceivers, envelope, result))
	{
		return result;
	}
	const string digest = zhangCheckpointSha256(payload);
	if (digest.empty())
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_PAYLOAD_SHA256_FAILED";
		return result;
	}
	plan.payload = payload;
	plan.payloadSha256 = digest;
	plan.runtimeId = runtimeId;
	plan.receiverCount = result.receiverCount;
	plan.satelliteStatusCount = result.satelliteStatusCount;
	plan.signalStatusCount = result.signalStatusCount;
	plan.resetObservationCount = result.resetObservationCount;
	plan.dynamicAliasReceiverCount = result.dynamicAliasReceiverCount;
	plan.dynamicAliasCount = result.dynamicAliasCount;
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

ZhangReceiverRuntimeCheckpointResult
importZhangReceiverRuntimeCheckpointSection(
	ReceiverMap& receivers,
	const std::string& runtimeId,
	const ZhangReceiverRuntimeCheckpointRestorePlan& plan)
{
	ZhangReceiverRuntimeCheckpointResult result;
	fillReceiverResultFromPlan(plan, result);
	if (runtimeId.empty() || plan.runtimeId != runtimeId)
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_PLAN_RUNTIME_ID_MISMATCH";
		return result;
	}
	if (plan.payload.empty() || plan.payloadSha256.empty()
		|| zhangCheckpointSha256(plan.payload) != plan.payloadSha256)
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_PLAN_PAYLOAD_SHA256_MISMATCH";
		return result;
	}
	ZhangReceiverRuntimeEnvelope envelope;
	ZhangReceiverRuntimeCheckpointResult validated;
	if (!decodeAndValidateReceiverEnvelope(
			plan.payload, runtimeId, envelope, validated)
		|| !receiverInventoryCompatible(receivers, envelope, validated))
	{
		return validated;
	}
	if (validated.receiverCount != plan.receiverCount
		|| validated.satelliteStatusCount != plan.satelliteStatusCount
		|| validated.signalStatusCount != plan.signalStatusCount
		|| validated.resetObservationCount != plan.resetObservationCount
		|| validated.dynamicAliasReceiverCount
			!= plan.dynamicAliasReceiverCount
		|| validated.dynamicAliasCount != plan.dynamicAliasCount)
	{
		result.failureReason =
			"RECEIVER_RUNTIME_CHECKPOINT_PLAN_COUNT_MISMATCH";
		return result;
	}
	vector<PreparedReceiver> prepared;
	prepared.reserve(envelope.receivers.size());
	// Allocate the replacement alias table before entering the live-state
	// commit.  This table is runtime state, while the two option maps below are
	// derived caches whose keys depend on it.
	decltype(acsConfig.customAliasesMap) restoredAliases;
	try
	{
		restoredAliases = envelope.customAliasesMap;
		for (const auto& stored : envelope.receivers)
		{
			PreparedReceiver receiver;
			if (!prepareReceiver(
					receivers.at(stored.mapKey),
					stored,
					receiver,
					result.failureReason))
			{
				return result;
			}
			prepared.push_back(receiver);
		}
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("RECEIVER_RUNTIME_CHECKPOINT_PREPARE_IMPORT_FAILED:")
			+ exception.what();
		return result;
	}
	try
	{
		for (auto& receiver : prepared)
			commitPreparedReceiver(receiver);
		{
			std::lock_guard<std::mutex> guard(acsConfig.configMutex);
			acsConfig.customAliasesMap.swap(restoredAliases);
			// These caches are derived from aliases and config selectors.  Keeping
			// instances made against the pre-restore alias graph would make the
			// resumed run depend on process history.  Rebuild stays lazy and
			// deterministic through getSatOpts/getRecOpts.
			acsConfig.satOptsMap.clear();
			acsConfig.recOptsMap.clear();
		}
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("RECEIVER_RUNTIME_CHECKPOINT_IMPORT_COMMIT_FAILED:")
			+ exception.what();
		return result;
	}
	result = validated;
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

ZhangSatelliteRuntimeCheckpointResult
exportZhangSatelliteRuntimeCheckpointSection(
	const Navigation& navigation,
	const std::string& runtimeId,
	std::string& payload)
{
	ZhangSatelliteRuntimeCheckpointResult result;
	payload.clear();
	if (runtimeId.empty())
	{
		result.failureReason = "SATELLITE_RUNTIME_CHECKPOINT_RUNTIME_ID_EMPTY";
		return result;
	}
	if (hasUnsupportedGlobalSsrOrSbas(navigation))
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_UNSUPPORTED_GLOBAL_SSR_OR_SBAS_STATE";
		return result;
	}
	ZhangSatelliteRuntimeEnvelope envelope;
	envelope.runtimeId = runtimeId;
	envelope.erpFilterValues = captureErpFilterValues(navigation.erp.filterValues);
	if (!validErpFilterValues(envelope.erpFilterValues))
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_INVALID_ERP_FILTER_VALUES";
		return result;
	}
	try
	{
		for (const auto& [satellite, navigationState] : navigation.satNavMap)
		{
			ZhangRuntimeSatelliteNavigation stored;
			if (!captureSatelliteNavigation(
					satellite,
					navigationState,
					stored,
					result.failureReason)
				|| !validSatelliteNavigation(stored, result.failureReason))
			{
				return result;
			}
			envelope.satellites.push_back(std::move(stored));
		}
		for (const auto& [satellite, alias] : SatSys::satDataMap)
		{
			envelope.aliases.push_back(
				{satellite, alias.block, alias.svn});
		}
		for (const auto& [satellite, history] : navigation.svnMap)
		{
			auto& target = envelope.svnHistory[satellite];
			for (const auto& [time, svn] : history)
			{
				target.push_back({captureZhangCheckpointTime(time), svn});
				++result.svnHistoryCount;
			}
		}
		envelope.blockTypes = navigation.blocktypeMap;
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("SATELLITE_RUNTIME_CHECKPOINT_EXPORT_FAILED:")
			+ exception.what();
		return result;
	}
	result.satelliteCount = envelope.satellites.size();
	result.satelliteAliasCount = envelope.aliases.size();
	if (!serializePayload(
			envelope,
			payload,
			result.failureReason,
			"SATELLITE_RUNTIME_CHECKPOINT"))
	{
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

ZhangSatelliteRuntimeCheckpointResult
preflightZhangSatelliteRuntimeCheckpointSection(
	const Navigation& configuredNavigation,
	const std::string& runtimeId,
	const std::string& payload,
	ZhangSatelliteRuntimeCheckpointRestorePlan& plan)
{
	plan = {};
	ZhangSatelliteRuntimeCheckpointResult result;
	ZhangSatelliteRuntimeEnvelope envelope;
	if (!decodeAndValidateSatelliteEnvelope(
			payload, runtimeId, envelope, result)
		|| !satelliteInventoryCompatible(
			configuredNavigation, envelope, result))
	{
		return result;
	}
	const string digest = zhangCheckpointSha256(payload);
	if (digest.empty())
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_PAYLOAD_SHA256_FAILED";
		return result;
	}
	plan.payload = payload;
	plan.payloadSha256 = digest;
	plan.runtimeId = runtimeId;
	plan.satelliteCount = result.satelliteCount;
	plan.satelliteAliasCount = result.satelliteAliasCount;
	plan.svnHistoryCount = result.svnHistoryCount;
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

ZhangSatelliteRuntimeCheckpointResult
importZhangSatelliteRuntimeCheckpointSection(
	Navigation& navigation,
	const std::string& runtimeId,
	const ZhangSatelliteRuntimeCheckpointRestorePlan& plan)
{
	ZhangSatelliteRuntimeCheckpointResult result;
	fillSatelliteResultFromPlan(plan, result);
	if (runtimeId.empty() || plan.runtimeId != runtimeId)
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_PLAN_RUNTIME_ID_MISMATCH";
		return result;
	}
	if (plan.payload.empty() || plan.payloadSha256.empty()
		|| zhangCheckpointSha256(plan.payload) != plan.payloadSha256)
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_PLAN_PAYLOAD_SHA256_MISMATCH";
		return result;
	}
	ZhangSatelliteRuntimeEnvelope envelope;
	ZhangSatelliteRuntimeCheckpointResult validated;
	if (!decodeAndValidateSatelliteEnvelope(
			plan.payload, runtimeId, envelope, validated)
		|| !satelliteInventoryCompatible(navigation, envelope, validated))
	{
		return validated;
	}
	if (validated.satelliteCount != plan.satelliteCount
		|| validated.satelliteAliasCount != plan.satelliteAliasCount
		|| validated.svnHistoryCount != plan.svnHistoryCount)
	{
		result.failureReason =
			"SATELLITE_RUNTIME_CHECKPOINT_PLAN_COUNT_MISMATCH";
		return result;
	}
	decltype(SatSys::satDataMap) aliases;
	decltype(navigation.svnMap) svnHistory;
	decltype(navigation.blocktypeMap) blockTypes = envelope.blockTypes;
	ERPValues erpFilterValues = restoreErpFilterValues(envelope.erpFilterValues);
	try
	{
		for (const auto& alias : envelope.aliases)
		{
			aliases[alias.satellite] = {alias.block, alias.svn};
		}
		for (const auto& [satellite, history] : envelope.svnHistory)
		{
			for (const auto& entry : history)
			{
				svnHistory[satellite][restoreZhangCheckpointTime(entry.time)] =
					entry.svn;
			}
		}
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("SATELLITE_RUNTIME_CHECKPOINT_PREPARE_IMPORT_FAILED:")
			+ exception.what();
		return result;
	}
	try
	{
		for (const auto& stored : envelope.satellites)
		{
			commitSatelliteNavigation(
				stored, navigation.satNavMap.at(stored.satellite));
		}
		SatSys::satDataMap.swap(aliases);
		navigation.svnMap.swap(svnHistory);
		navigation.blocktypeMap.swap(blockTypes);
		// erpMaps are immutable input-derived tables in the frozen E29 path;
		// filterValues is the epoch-dependent estimate that must resume exactly.
		navigation.erp.filterValues = erpFilterValues;
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("SATELLITE_RUNTIME_CHECKPOINT_IMPORT_COMMIT_FAILED:")
			+ exception.what();
		return result;
	}
	result = validated;
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}
