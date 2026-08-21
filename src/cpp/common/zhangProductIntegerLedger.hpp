#pragma once

#include <algorithm>
#include <cstdint>
#include <map>
#include <numeric>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "common/enums.h"
#include "common/zhangIntegerAudit.hpp"

enum class ZhangProductIntegerLedgerSource
{
	PRODUCT_ILS,
	DERIVED_PAIR,
	TEMPORAL_RECERTIFIED,
	BESD
};

inline const char* zhangProductIntegerLedgerSourceName(
	ZhangProductIntegerLedgerSource source)
{
	switch (source)
	{
		case ZhangProductIntegerLedgerSource::PRODUCT_ILS:
			return "PRODUCT_ILS";
		case ZhangProductIntegerLedgerSource::DERIVED_PAIR:
			return "DERIVED_PAIR";
		case ZhangProductIntegerLedgerSource::TEMPORAL_RECERTIFIED:
			return "TEMPORAL_RECERTIFIED";
		case ZhangProductIntegerLedgerSource::BESD:
			return "BESD";
	}
	return "UNKNOWN";
}

/** One product-lattice integer retained independently of the generic network
 * held lattice.  physicalExpansion is authoritative and includes observable,
 * receiver-satellite arc and arc version in every string key.  productRow is
 * only the coordinate used when the row was discovered and is never used to
 * transport it across a backend basis change. */
struct ProductIntegerLedgerRow
{
	E_Sys system = E_Sys::NONE;
	E_ObsCode firstObservable = E_ObsCode::NONE;
	E_ObsCode secondObservable = E_ObsCode::NONE;
	ZhangExactVector productRow;
	ZhangExactInteger integerValue = 0;
	std::map<std::string, ZhangExactInteger> physicalExpansion;
	// Named edge metadata is populated only for exact pair certificates.  It is
	// retained separately from productRow so an offline full-oracle builder can
	// reconstruct a reference-invariant satellite graph after basis changes.
	std::string coordinate;
	std::string firstSatellite;
	std::string secondSatellite;
	std::string phaseSegmentFingerprint;
	std::uint64_t backendBasisGeneration = 0;
	long int firstCertified = 0;
	long int lastConfirmed = 0;
	int confirmationEpochs = 0;
	ZhangProductIntegerLedgerSource source =
		ZhangProductIntegerLedgerSource::PRODUCT_ILS;
	bool conditioningOnly = true;
	bool pairCertificate = false;
	bool certified = false;
};

struct ZhangProductIntegerLedgerUpdate
{
	bool valid = false;
	int inputRows = 0;
	int freshRows = 0;
	int confirmedRows = 0;
	int conflictingRows = 0;
	int activeRankBefore = 0;
	int activeRankAfter = 0;
	std::string failureReason = "NOT_EVALUATED";
};

inline std::string zhangProductPhysicalRowFingerprint(
	const std::map<std::string, ZhangExactInteger>& row)
{
	std::ostringstream stream;
	for (const auto& [identity, coefficient] : row)
	{
		if (coefficient == 0) continue;
		stream << identity << "=" << coefficient << ";";
	}
	return stream.str();
}

/** Canonicalise the sign of one exact physical relation.  A row and its
 * simultaneous integer negation are the same lattice certificate and must not
 * occupy two ledger identities merely because a compiler chose the opposite
 * satellite/reference orientation. */
inline void zhangCanonicaliseProductLedgerRow(ProductIntegerLedgerRow& row)
{
	for (auto iterator = row.physicalExpansion.begin();
		 iterator != row.physicalExpansion.end();)
	{
		if (iterator->second == 0)
			iterator = row.physicalExpansion.erase(iterator);
		else ++iterator;
	}
	if (row.physicalExpansion.empty() ||
		row.physicalExpansion.begin()->second > 0) return;
	for (auto& [identity, coefficient] : row.physicalExpansion)
		coefficient = -coefficient;
	for (auto& coefficient : row.productRow) coefficient = -coefficient;
	row.integerValue = -row.integerValue;
	if (row.pairCertificate && !row.firstSatellite.empty() &&
		!row.secondSatellite.empty())
		std::swap(row.firstSatellite, row.secondSatellite);
}

/** Immutable ledger identity.
 *
 * A physical ambiguity row is not transportable merely because the same arc
 * labels reappear after the backend changes basis.  Until an explicit exact
 * transport certificate exists, phase segment and backend generation are
 * hard parts of the key.  This deliberately retains old generations instead
 * of silently overwriting them with a numerically different gauge.
 */
inline std::string zhangProductLedgerIdentityFingerprint(
	const ProductIntegerLedgerRow& row)
{
	std::ostringstream stream;
	stream << "GEN" << row.backendBasisGeneration
		<< "|SEG{" << row.phaseSegmentFingerprint << "}|ROW{"
		<< zhangProductPhysicalRowFingerprint(row.physicalExpansion) << "}";
	return stream.str();
}

/** Re-express a retained physical integer row in a current ambiguity-column
 * map.  Backend generation is deliberately not part of this algebraic
 * operation: safety comes from exact arc/version identities, with phase
 * segment and statistical gates applied by the caller. */
inline bool zhangProjectProductLedgerPhysicalRow(
	const ProductIntegerLedgerRow& row,
	const std::map<std::string, int>& currentIdentityColumns,
	int columnCount,
	ZhangExactVector& projected)
{
	projected = ZhangExactVector(std::max(0, columnCount));
	if (columnCount <= 0 || row.physicalExpansion.empty()) return false;
	for (const auto& [identity, coefficient] : row.physicalExpansion)
	{
		if (coefficient == 0) continue;
		auto column = currentIdentityColumns.find(identity);
		if (column == currentIdentityColumns.end() || column->second < 0 ||
			column->second >= columnCount) return false;
		projected[column->second] += coefficient;
	}
	return std::any_of(projected.begin(), projected.end(),
		[](const auto& coefficient) { return coefficient != 0; });
}

inline int zhangProductLedgerExactRank(
	const std::vector<ProductIntegerLedgerRow>& rows,
	bool certifiedOnly = true)
{
	std::set<std::string> columnSet;
	for (const auto& row : rows)
	{
		if (certifiedOnly && !row.certified) continue;
		for (const auto& [identity, coefficient] : row.physicalExpansion)
			if (coefficient != 0) columnSet.insert(identity);
	}
	std::vector<std::string> columns(columnSet.begin(), columnSet.end());
	std::map<std::string, int> columnIndex;
	for (int column = 0; column < static_cast<int>(columns.size()); column++)
		columnIndex[columns[column]] = column;
	ZhangExactMatrix matrix;
	for (const auto& row : rows)
	{
		if (certifiedOnly && !row.certified) continue;
		ZhangExactVector dense(columns.size());
		for (const auto& [identity, coefficient] : row.physicalExpansion)
			if (coefficient != 0) dense[columnIndex.at(identity)] = coefficient;
		matrix.push_back(std::move(dense));
	}
	return static_cast<int>(
		zhangExactRowHermiteNormalForm(matrix).basis.size());
}

class ProductIntegerLedger
{
public:
	ZhangProductIntegerLedgerUpdate observe(
		long int epoch,
		const std::vector<ProductIntegerLedgerRow>& candidates,
		int requiredConfirmations = 1)
	{
		ZhangProductIntegerLedgerUpdate result;
		result.inputRows = candidates.size();
		result.activeRankBefore = zhangProductLedgerExactRank(rows_);
		auto proposedRows = rows_;
		if (epoch <= 0 || requiredConfirmations < 1)
		{
			result.failureReason = "PRODUCT_LEDGER_INPUT_INVALID";
			return result;
		}
		for (auto candidate : candidates)
		{
			zhangCanonicaliseProductLedgerRow(candidate);
			if (candidate.system == E_Sys::NONE ||
				candidate.physicalExpansion.empty() ||
				candidate.phaseSegmentFingerprint.empty())
			{
				result.failureReason = "PRODUCT_LEDGER_IDENTITY_INCOMPLETE";
				return result;
			}
			auto exactGcd = [](ZhangExactInteger left, ZhangExactInteger right)
			{
				left = left < 0 ? -left : left;
				right = right < 0 ? -right : right;
				while (right != 0)
				{
					const ZhangExactInteger remainder = left % right;
					left = right;
					right = remainder;
				}
				return left;
			};
			ZhangExactInteger coefficientGcd = 0;
			for (const auto& [identity, coefficient] : candidate.physicalExpansion)
				coefficientGcd = exactGcd(
					coefficientGcd, coefficient < 0 ? -coefficient : coefficient);
			if (coefficientGcd != 1)
			{
				result.failureReason = "PRODUCT_LEDGER_ROW_NOT_PRIMITIVE";
				return result;
			}
			const std::string identity =
				zhangProductLedgerIdentityFingerprint(candidate);
			auto existing = std::find_if(proposedRows.begin(), proposedRows.end(),
				[&](const auto& row)
				{
					return zhangProductLedgerIdentityFingerprint(row) == identity;
				});
			if (existing == proposedRows.end())
			{
				candidate.firstCertified = epoch;
				candidate.lastConfirmed = epoch;
				candidate.confirmationEpochs = 1;
				candidate.certified = requiredConfirmations == 1;
				proposedRows.push_back(std::move(candidate));
				result.freshRows++;
				result.confirmedRows += proposedRows.back().certified;
				continue;
			}
			if (existing->integerValue != candidate.integerValue)
			{
				candidate.firstCertified = epoch;
				candidate.lastConfirmed = epoch;
				candidate.confirmationEpochs = 1;
				candidate.certified = requiredConfirmations == 1;
				*existing = std::move(candidate);
				result.conflictingRows++;
				result.confirmedRows += existing->certified;
				continue;
			}
			if (existing->lastConfirmed != epoch)
			{
				existing->lastConfirmed = epoch;
				existing->confirmationEpochs++;
			}
			existing->productRow = std::move(candidate.productRow);
			// Exact pair membership is a stronger semantic certificate than a
			// generic conditioning row.  Upgrade the retained physical integer if
			// it was first seen as a mixed PRODUCT_ILS row.
			if (candidate.pairCertificate)
			{
				existing->pairCertificate = true;
				existing->conditioningOnly = false;
				existing->source = candidate.source;
				existing->coordinate = candidate.coordinate;
				existing->firstSatellite = candidate.firstSatellite;
				existing->secondSatellite = candidate.secondSatellite;
			}
			existing->certified =
				existing->confirmationEpochs >= requiredConfirmations;
			result.confirmedRows += existing->certified;
		}
		result.activeRankAfter = zhangProductLedgerExactRank(proposedRows);
		rows_ = std::move(proposedRows);
		result.valid = true;
		result.failureReason = "NONE";
		return result;
	}

	const std::vector<ProductIntegerLedgerRow>& rows() const { return rows_; }

	std::vector<ProductIntegerLedgerRow> rowsForGeneration(
		std::uint64_t backendBasisGeneration,
		bool certifiedOnly = true) const
	{
		std::vector<ProductIntegerLedgerRow> selected;
		for (const auto& row : rows_)
		{
			if (row.backendBasisGeneration != backendBasisGeneration) continue;
			if (certifiedOnly && !row.certified) continue;
			selected.push_back(row);
		}
		return selected;
	}

private:
	std::vector<ProductIntegerLedgerRow> rows_;
};

inline std::map<std::pair<std::string, E_Sys>, ProductIntegerLedger>&
zhangProductIntegerLedgerRegistry()
{
	static std::map<std::pair<std::string, E_Sys>, ProductIntegerLedger> registry;
	return registry;
}
