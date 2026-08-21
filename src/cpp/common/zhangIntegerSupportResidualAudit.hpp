#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <utility>
#include <vector>

#include "common/algebra.hpp"
#include "common/zhangFullRank.hpp"

/** Bounded postfit-residual history for an integer-support physical arc.
 * This is diagnostic evidence only: it never changes the FLOAT filter and is
 * consumed by the next epoch's Product-IAR core selection. */
struct ZhangIntegerSupportResidualAudit
{
	std::vector<double> phaseResiduals;
	std::vector<double> codeResiduals;
	double maximumWhitenedResidualScore = 0;
};

struct ZhangIntegerSupportResidualSummary
{
	int phaseSamples = 0;
	int codeSamples = 0;
	double phaseRms = std::numeric_limits<double>::quiet_NaN();
	double codeRms = std::numeric_limits<double>::quiet_NaN();
	double phaseMad = std::numeric_limits<double>::quiet_NaN();
	double codeMad = std::numeric_limits<double>::quiet_NaN();
	double maximumWhitenedResidualScore = std::numeric_limits<double>::quiet_NaN();
};

inline std::map<std::pair<const void*, ZhangGraphEdge>,
	ZhangIntegerSupportResidualAudit>& zhangIntegerSupportResidualAuditRegistry()
{
	static std::map<std::pair<const void*, ZhangGraphEdge>,
		ZhangIntegerSupportResidualAudit> registry;
	return registry;
}

inline double zhangIntegerSupportRms(const std::vector<double>& values)
{
	if (values.empty()) return std::numeric_limits<double>::quiet_NaN();
	double sumSquares = 0;
	for (const double value : values) sumSquares += value * value;
	return std::sqrt(sumSquares / values.size());
}

inline double zhangIntegerSupportMad(std::vector<double> values)
{
	if (values.empty()) return std::numeric_limits<double>::quiet_NaN();
	std::sort(values.begin(), values.end());
	const double median = values[values.size() / 2];
	for (auto& value : values) value = std::abs(value - median);
	std::sort(values.begin(), values.end());
	return values[values.size() / 2];
}

inline void zhangRecordIntegerSupportPostfitResidual(
	const void* owner,
	const KFMeas& kfMeas,
	int index)
{
	if (owner == nullptr || index < 0 || index >= static_cast<int>(kfMeas.obsKeys.size()))
		return;
	const auto& key = kfMeas.obsKeys[index];
	if ((key.type != KF::PHAS_MEAS && key.type != KF::CODE_MEAS) ||
		key.str.empty() || key.Sat.sys == E_Sys::NONE)
		return;
	const VectorXd& residuals = kfMeas.VV.size() == kfMeas.obsKeys.size()
		? kfMeas.VV : kfMeas.V;
	if (index >= residuals.size() || !std::isfinite(residuals(index)))
		return;
	auto& audit = zhangIntegerSupportResidualAuditRegistry()[
		{owner, {key.str, key.Sat}}];
	auto& values = key.type == KF::PHAS_MEAS
		? audit.phaseResiduals : audit.codeResiduals;
	constexpr size_t maximumSamples = 64;
	if (values.size() == maximumSamples)
		values.erase(values.begin());
	values.push_back(residuals(index));
	const VectorXd& ratios = kfMeas.postfitRatios.size() == kfMeas.obsKeys.size()
		? kfMeas.postfitRatios : kfMeas.prefitRatios;
	if (index < ratios.size() && std::isfinite(ratios(index)))
		audit.maximumWhitenedResidualScore = std::max(
			audit.maximumWhitenedResidualScore, std::abs(ratios(index)));
}

inline ZhangIntegerSupportResidualSummary zhangIntegerSupportResidualSummary(
	const void* owner,
	const ZhangGraphEdge& edge)
{
	ZhangIntegerSupportResidualSummary result;
	auto found = zhangIntegerSupportResidualAuditRegistry().find({owner, edge});
	if (found == zhangIntegerSupportResidualAuditRegistry().end())
		return result;
	const auto& audit = found->second;
	result.phaseSamples = audit.phaseResiduals.size();
	result.codeSamples = audit.codeResiduals.size();
	result.phaseRms = zhangIntegerSupportRms(audit.phaseResiduals);
	result.codeRms = zhangIntegerSupportRms(audit.codeResiduals);
	result.phaseMad = zhangIntegerSupportMad(audit.phaseResiduals);
	result.codeMad = zhangIntegerSupportMad(audit.codeResiduals);
	result.maximumWhitenedResidualScore = audit.maximumWhitenedResidualScore;
	return result;
}
