#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"

struct ZhangIncrementalTargetMarginal
{
	bool valid = false;
	int requestedTargetCount = 0;
	int informationRank = 0;
	int quotientValidRank = 0;
	int absoluteValidRank = 0;
	int unresolvedGaugeRank = 0;
	int orthogonalResidualDof = 0;
	int storedRows = 0;
	int storedColumns = 0;
	int maximumStoredRows = 0;
	int maximumStoredColumns = 0;
	double orthogonalResidualSquaredNorm = 0;
	std::vector<std::string> identities;
	std::vector<std::string> gaugeIdentities;
	std::vector<bool> absoluteValidity;
	std::vector<double> coordinateOffsets;
	VectorXd mean;
	VectorXd fractionalMean;
	MatrixXd covariance;
	std::string failureReason;
};

/** Bounded physical-integer-target separator.
 *
 * Each epoch contributes an equivalent likelihood C*z=e after all continuous
 * epoch-local nuisance has been eliminated.  The likelihood is immediately
 * QR-compressed into R*z=d.  Historical measurement rows are discarded; only
 * the square-root prior over currently active physical target identities is
 * retained.  Retiring a physical identity projects its column out once and
 * transfers all correlated information to the surviving separator.
 */
class ZhangIncrementalTargetSeparator
{
public:
	explicit ZhangIncrementalTargetSeparator(
		double relativeRankTolerance = 1e-11)
	:	rankTolerance(relativeRankTolerance)
	{}

	void clear()
	{
		identities.clear();
		gaugeIdentity.clear();
		absoluteValid.clear();
		coordinateOffset.clear();
		factor.resize(0, 0);
		rhs.resize(0);
		orthogonalDof = 0;
		orthogonalSquaredNorm = 0;
		maximumRows = 0;
		maximumColumns = 0;
		failureReason.clear();
	}

	bool addLikelihood(
		const std::vector<std::string>& eventIdentities,
		const MatrixXd& eventDesign,
		const MatrixXd& eventCovariance,
		const VectorXd& eventObservation,
		const std::vector<std::string>& eventGaugeIdentity,
		const std::vector<bool>& eventAbsoluteValid,
		const std::vector<double>& eventCoordinateOffset = {})
	{
		const std::vector<double> normalizedOffsets = eventCoordinateOffset.empty()
			? std::vector<double>(eventIdentities.size(), 0)
			: eventCoordinateOffset;
		if (eventIdentities.empty()
		 || eventDesign.cols() != static_cast<int>(eventIdentities.size())
		 || eventDesign.rows() != eventObservation.size()
		 || eventCovariance.rows() != eventObservation.size()
		 || eventCovariance.cols() != eventObservation.size()
		 || eventGaugeIdentity.size() != eventIdentities.size()
		 || eventAbsoluteValid.size() != eventIdentities.size()
		 || normalizedOffsets.size() != eventIdentities.size()
		 || !eventDesign.allFinite() || !eventCovariance.allFinite()
		 || !eventObservation.allFinite())
		{
			return fail("INVALID_INCREMENTAL_TARGET_LIKELIHOOD");
		}
		std::set<std::string> uniqueEventKeys;
		for (const auto& identity : eventIdentities)
		{
			if (identity.empty() || !uniqueEventKeys.insert(identity).second)
			{
				return fail("DUPLICATE_INCREMENTAL_TARGET_IDENTITY");
			}
		}

		MatrixXd inverseSquareRoot;
		if (!inversePositiveSquareRoot(eventCovariance, inverseSquareRoot))
		{
			return false;
		}
		std::map<std::string, int> columnByIdentity;
		const int originalColumns = identities.size();
		VectorXd coordinateTranslation = VectorXd::Zero(originalColumns);
		for (int column = 0; column < static_cast<int>(identities.size()); column++)
		{
			columnByIdentity[identities[column]] = column;
		}
		for (int eventColumn = 0;
			 eventColumn < static_cast<int>(eventIdentities.size());
			 eventColumn++)
		{
			const auto& identity = eventIdentities[eventColumn];
			if (columnByIdentity.find(identity) == columnByIdentity.end())
			{
				columnByIdentity[identity] = identities.size();
				identities.push_back(identity);
				gaugeIdentity.push_back(eventGaugeIdentity[eventColumn]);
				absoluteValid.push_back(eventAbsoluteValid[eventColumn]);
				coordinateOffset.push_back(normalizedOffsets[eventColumn]);
			}
			else
			{
				const int column = columnByIdentity.at(identity);
				const double translation = normalizedOffsets[eventColumn]
					- coordinateOffset[column];
				if (!std::isfinite(translation)
				 || std::abs(translation - std::round(translation)) > 1e-10)
				{
					return fail("NONINTEGER_INCREMENTAL_TARGET_COORDINATE_TRANSLATION");
				}
				coordinateTranslation(column) = translation;
				coordinateOffset[column] = normalizedOffsets[eventColumn];
				if (gaugeIdentity[column] != eventGaugeIdentity[eventColumn])
				{
					const std::string nextGauge = eventGaugeIdentity[eventColumn];
					if (nextGauge.empty() && eventAbsoluteValid[eventColumn])
					{
						// Absolute observability is relation specific.  An exact
						// datum for this canonical relation must not silently promote
						// the other relations that merely share its quotient gauge.
						gaugeIdentity[column].clear();
						absoluteValid[column] = true;
					}
					else if (!(gaugeIdentity[column].empty()
						&& absoluteValid[column]))
					{
						return fail("INCREMENTAL_TARGET_GAUGE_IDENTITY_CHANGED");
					}
				}
				absoluteValid[column] = absoluteValid[column]
					|| eventAbsoluteValid[eventColumn];
			}
		}
		if (originalColumns > 0)
		{
			// z_new=z_old+delta, hence R*z_new=d+R*delta.
			rhs += factor * coordinateTranslation;
		}

		const int columns = identities.size();
		MatrixXd expanded = MatrixXd::Zero(factor.rows(), columns);
		if (factor.cols() > 0)
		{
			expanded.leftCols(factor.cols()) = factor;
		}
		MatrixXd eventInSeparator = MatrixXd::Zero(eventDesign.rows(), columns);
		for (int eventColumn = 0;
			 eventColumn < static_cast<int>(eventIdentities.size());
			 eventColumn++)
		{
			eventInSeparator.col(columnByIdentity.at(eventIdentities[eventColumn])) =
				eventDesign.col(eventColumn);
		}
		MatrixXd combined(expanded.rows() + eventDesign.rows(), columns);
		combined.topRows(expanded.rows()) = expanded;
		combined.bottomRows(eventDesign.rows()) =
			inverseSquareRoot * eventInSeparator;
		VectorXd combinedRhs(rhs.size() + eventObservation.size());
		combinedRhs.head(rhs.size()) = rhs;
		combinedRhs.tail(eventObservation.size()) =
			inverseSquareRoot * eventObservation;
		return compress(combined, combinedRhs, true);
	}

	/** Remove identities that no longer connect to a future physical arc. */
	bool retainOnly(const std::set<std::string>& retainedIdentities)
	{
		std::vector<int> retired;
		std::vector<int> retained;
		for (int column = 0; column < static_cast<int>(identities.size()); column++)
		{
			(retainedIdentities.find(identities[column]) == retainedIdentities.end()
				? retired : retained).push_back(column);
		}
		if (retired.empty())
		{
			return true;
		}
		if (retained.empty())
		{
			clear();
			return true;
		}
		MatrixXd reordered(factor.rows(), factor.cols());
		int output = 0;
		for (int column : retired)
		{
			reordered.col(output++) = factor.col(column);
		}
		for (int column : retained)
		{
			reordered.col(output++) = factor.col(column);
		}
		Eigen::ColPivHouseholderQR<MatrixXd> nuisanceQr(
			reordered.leftCols(retired.size()));
		nuisanceQr.setThreshold(rankTolerance);
		const int retiredRank = nuisanceQr.rank();
		const MatrixXd rotated = nuisanceQr.householderQ().adjoint()
			* reordered.rightCols(retained.size());
		const VectorXd rotatedRhs = nuisanceQr.householderQ().adjoint() * rhs;
		const int rows = factor.rows() - retiredRank;
		MatrixXd projected = rotated.bottomRows(rows);
		VectorXd projectedRhs = rotatedRhs.tail(rows);

		std::vector<std::string> nextIdentities;
		std::vector<std::string> nextGauge;
		std::vector<bool> nextAbsolute;
		std::vector<double> nextOffset;
		for (int column : retained)
		{
			nextIdentities.push_back(identities[column]);
			nextGauge.push_back(gaugeIdentity[column]);
			nextAbsolute.push_back(absoluteValid[column]);
			nextOffset.push_back(coordinateOffset[column]);
		}
		identities = std::move(nextIdentities);
		gaugeIdentity = std::move(nextGauge);
		absoluteValid = std::move(nextAbsolute);
		coordinateOffset = std::move(nextOffset);
		return compress(projected, projectedRhs, false);
	}

	ZhangIncrementalTargetMarginal marginal() const
	{
		ZhangIncrementalTargetMarginal result;
		result.requestedTargetCount = identities.size();
		result.identities = identities;
		result.gaugeIdentities = gaugeIdentity;
		result.absoluteValidity = absoluteValid;
		result.coordinateOffsets = coordinateOffset;
		result.orthogonalResidualDof = orthogonalDof;
		result.orthogonalResidualSquaredNorm = orthogonalSquaredNorm;
		result.storedRows = factor.rows();
		result.storedColumns = factor.cols();
		result.maximumStoredRows = maximumRows;
		result.maximumStoredColumns = maximumColumns;
		if (identities.empty() || factor.rows() == 0)
		{
			result.failureReason = "EMPTY_INCREMENTAL_TARGET_SEPARATOR";
			return result;
		}
		Eigen::JacobiSVD<MatrixXd> svd(
			factor, Eigen::ComputeThinU | Eigen::ComputeFullV);
		const double scale = std::max(
			1.0, svd.singularValues().maxCoeff());
		svd.setThreshold(rankTolerance * scale);
		result.informationRank = svd.rank();
		if (result.informationRank == 0)
		{
			result.failureReason = "ZERO_INCREMENTAL_TARGET_INFORMATION_RANK";
			return result;
		}
		const MatrixXd left = svd.matrixU().leftCols(result.informationRank);
		const MatrixXd right = svd.matrixV().leftCols(result.informationRank);
		const VectorXd inverseSingular = svd.singularValues()
			.head(result.informationRank).cwiseInverse();
		result.mean = right * inverseSingular.asDiagonal()
			* left.transpose() * rhs;
		result.covariance = right
			* inverseSingular.array().square().matrix().asDiagonal()
			* right.transpose();
		result.covariance = 0.5
			* (result.covariance + result.covariance.transpose());

		std::map<std::string, int> gaugeColumns;
		for (int index = 0; index < static_cast<int>(identities.size()); index++)
		{
			if (absoluteValid[index] || gaugeIdentity[index].empty())
			{
				continue;
			}
			if (gaugeColumns.find(gaugeIdentity[index]) == gaugeColumns.end())
			{
				gaugeColumns[gaugeIdentity[index]] = gaugeColumns.size();
			}
		}
		MatrixXd gauge = MatrixXd::Zero(identities.size(), gaugeColumns.size());
		for (int index = 0; index < static_cast<int>(identities.size()); index++)
		{
			if (!absoluteValid[index] && !gaugeIdentity[index].empty())
			{
				gauge(index, gaugeColumns.at(gaugeIdentity[index])) = 1;
			}
		}
		result.unresolvedGaugeRank = gaugeColumns.size();
		MatrixXd invariantBasis;
		if (gauge.cols() == 0)
		{
			invariantBasis = MatrixXd::Identity(
				identities.size(), identities.size());
		}
		else
		{
			Eigen::JacobiSVD<MatrixXd> gaugeSvd(
				gauge.transpose(), Eigen::ComputeFullV);
			gaugeSvd.setThreshold(rankTolerance);
			invariantBasis = gaugeSvd.matrixV().rightCols(
				identities.size() - gaugeSvd.rank());
		}
		result.quotientValidRank = invariantBasis.cols() == 0
			? 0 : Eigen::FullPivLU<MatrixXd>(factor * invariantBasis).rank();

		std::vector<int> absoluteColumns;
		std::vector<int> unresolvedColumns;
		for (int index = 0; index < static_cast<int>(identities.size()); index++)
		{
			(absoluteValid[index] ? absoluteColumns : unresolvedColumns)
				.push_back(index);
		}
		if (!absoluteColumns.empty())
		{
			MatrixXd reordered(factor.rows(), factor.cols());
			int output = 0;
			for (int column : unresolvedColumns)
			{
				reordered.col(output++) = factor.col(column);
			}
			for (int column : absoluteColumns)
			{
				reordered.col(output++) = factor.col(column);
			}
			MatrixXd projectedAbsolute = reordered.rightCols(absoluteColumns.size());
			if (!unresolvedColumns.empty())
			{
				Eigen::ColPivHouseholderQR<MatrixXd> nuisanceQr(
					reordered.leftCols(unresolvedColumns.size()));
				nuisanceQr.setThreshold(rankTolerance);
				const MatrixXd rotated = nuisanceQr.householderQ().adjoint()
					* projectedAbsolute;
				projectedAbsolute = rotated.bottomRows(
					factor.rows() - nuisanceQr.rank());
			}
			result.absoluteValidRank =
				Eigen::FullPivLU<MatrixXd>(projectedAbsolute).rank();
		}
		result.fractionalMean.resize(result.mean.size());
		for (int index = 0; index < result.mean.size(); index++)
		{
			result.fractionalMean(index) = result.mean(index)
				- std::round(result.mean(index));
		}
		result.valid = result.mean.allFinite() && result.covariance.allFinite();
		if (!result.valid)
		{
			result.failureReason = "NONFINITE_INCREMENTAL_TARGET_MARGINAL";
		}
		return result;
	}

	int storedRows() const { return factor.rows(); }
	int storedColumns() const { return factor.cols(); }
	int maximumStoredRowCount() const { return maximumRows; }
	int maximumStoredColumnCount() const { return maximumColumns; }
	const std::string& lastFailureReason() const { return failureReason; }

private:
	bool compress(
		const MatrixXd& inputFactor,
		const VectorXd& inputRhs,
		bool accountOrthogonalResidual)
	{
		if (inputFactor.rows() != inputRhs.size())
		{
			return fail("INVALID_INCREMENTAL_TARGET_COMPRESSION");
		}
		Eigen::ColPivHouseholderQR<MatrixXd> qr(inputFactor);
		qr.setThreshold(rankTolerance);
		const int rank = qr.rank();
		const VectorXd rotatedRhs = qr.householderQ().adjoint() * inputRhs;
		if (rank == 0)
		{
			return fail("ZERO_INCREMENTAL_TARGET_FACTOR_RANK");
		}
		MatrixXd upper = MatrixXd::Zero(rank, inputFactor.cols());
		const MatrixXd rawUpper = qr.matrixR();
		for (int row = 0; row < rank; row++)
		for (int column = row; column < inputFactor.cols(); column++)
		{
			upper(row, column) = rawUpper(row, column);
		}
		factor = upper * qr.colsPermutation().transpose();
		rhs = rotatedRhs.head(rank);
		if (accountOrthogonalResidual && inputFactor.rows() > rank)
		{
			const VectorXd retired = rotatedRhs.tail(inputFactor.rows() - rank);
			orthogonalDof += retired.size();
			orthogonalSquaredNorm += retired.squaredNorm();
		}
		maximumRows = std::max(maximumRows, static_cast<int>(factor.rows()));
		maximumColumns = std::max(
			maximumColumns, static_cast<int>(factor.cols()));
		return factor.allFinite() && rhs.allFinite()
			? true : fail("NONFINITE_INCREMENTAL_TARGET_COMPRESSION");
	}

	bool inversePositiveSquareRoot(
		const MatrixXd& covariance,
		MatrixXd& inverseSquareRoot)
	{
		const MatrixXd symmetric = 0.5
			* (covariance + covariance.transpose());
		LLT<MatrixXd> cholesky(symmetric);
		if (cholesky.info() != Eigen::Success)
		{
			return fail("INCREMENTAL_TARGET_COVARIANCE_NOT_POSITIVE_DEFINITE");
		}
		inverseSquareRoot = cholesky.matrixL().solve(
			MatrixXd::Identity(covariance.rows(), covariance.cols()));
		return inverseSquareRoot.allFinite()
			? true : fail("NONFINITE_INCREMENTAL_TARGET_WHITENER");
	}

	bool fail(const std::string& reason)
	{
		failureReason = reason;
		return false;
	}

	double rankTolerance = 1e-11;
	std::vector<std::string> identities;
	std::vector<std::string> gaugeIdentity;
	std::vector<bool> absoluteValid;
	std::vector<double> coordinateOffset;
	MatrixXd factor;
	VectorXd rhs;
	int orthogonalDof = 0;
	double orthogonalSquaredNorm = 0;
	int maximumRows = 0;
	int maximumColumns = 0;
	std::string failureReason;
};
