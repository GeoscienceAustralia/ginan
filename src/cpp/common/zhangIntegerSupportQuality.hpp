#pragma once

#include <cmath>
#include <limits>
#include <string>

/** Per physical ambiguity-arc quality used only to select the integer-support
 * network.  All accepted observations remain in the authoritative FLOAT
 * backend; failing this gate can only remove an arc from integer certification.
 */
struct ZhangIntegerArcQuality
{
	int ageEpochs = 0;
	int observations = 0;
	double phaseResidualRms = std::numeric_limits<double>::quiet_NaN();
	double codeResidualRms = std::numeric_limits<double>::quiet_NaN();
	double phaseResidualMad = std::numeric_limits<double>::quiet_NaN();
	double codeResidualMad = std::numeric_limits<double>::quiet_NaN();
	int slipCount = 0;
	int outageCount = 0;
	double elevationScore = std::numeric_limits<double>::quiet_NaN();
	double whitenedResidualScore = std::numeric_limits<double>::quiet_NaN();
	bool eligibleForIntegerSupport = false;
	std::string failureReason = "NOT_EVALUATED";
};

struct ZhangIntegerSupportQualityGates
{
	int minimumAgeEpochs = 10;
	int minimumObservations = 10;
	double maximumPhaseResidualRms = 0.03;
	double maximumCodeResidualRms = 3.0;
	double maximumPhaseResidualMad = 0.03;
	double maximumCodeResidualMad = 3.0;
	int maximumSlipCount = 0;
	int maximumOutageCount = 1;
	double minimumElevationScore = 0;
	double maximumWhitenedResidualScore = 4;
};

inline ZhangIntegerArcQuality zhangEvaluateIntegerSupportQuality(
	ZhangIntegerArcQuality quality,
	const ZhangIntegerSupportQualityGates& gates)
{
	auto reject = [&](const std::string& reason)
	{
		quality.eligibleForIntegerSupport = false;
		quality.failureReason = reason;
		return quality;
	};
	if (quality.ageEpochs < gates.minimumAgeEpochs)
		return reject("ARC_AGE_BELOW_MINIMUM");
	if (quality.observations < gates.minimumObservations)
		return reject("OBSERVATION_COUNT_BELOW_MINIMUM");
	if (!std::isfinite(quality.phaseResidualRms)
	 || quality.phaseResidualRms > gates.maximumPhaseResidualRms)
		return reject("PHASE_RMS_GATE_FAILED");
	if (!std::isfinite(quality.codeResidualRms)
	 || quality.codeResidualRms > gates.maximumCodeResidualRms)
		return reject("CODE_RMS_GATE_FAILED");
	if (!std::isfinite(quality.phaseResidualMad)
	 || quality.phaseResidualMad > gates.maximumPhaseResidualMad)
		return reject("PHASE_MAD_GATE_FAILED");
	if (!std::isfinite(quality.codeResidualMad)
	 || quality.codeResidualMad > gates.maximumCodeResidualMad)
		return reject("CODE_MAD_GATE_FAILED");
	if (quality.slipCount > gates.maximumSlipCount)
		return reject("SLIP_COUNT_GATE_FAILED");
	if (quality.outageCount > gates.maximumOutageCount)
		return reject("OUTAGE_COUNT_GATE_FAILED");
	if (!std::isfinite(quality.elevationScore)
	 || quality.elevationScore < gates.minimumElevationScore)
		return reject("ELEVATION_SCORE_GATE_FAILED");
	if (!std::isfinite(quality.whitenedResidualScore)
	 || std::abs(quality.whitenedResidualScore) >
		gates.maximumWhitenedResidualScore)
		return reject("WHITENED_RESIDUAL_GATE_FAILED");
	quality.eligibleForIntegerSupport = true;
	quality.failureReason = "ELIGIBLE";
	return quality;
}
