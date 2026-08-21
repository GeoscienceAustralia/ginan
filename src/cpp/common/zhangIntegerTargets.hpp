#pragma once

#include <algorithm>
#include <cmath>
#include <functional>
#include <limits>
#include <map>
#include <numeric>
#include <string>
#include <vector>

#include <boost/multiprecision/cpp_int.hpp>

#include "common/eigenIncluder.hpp"

using ZhangIntegerMatrix = Eigen::Matrix<long long, Eigen::Dynamic, Eigen::Dynamic>;
using ZhangIntegerVector = Eigen::Matrix<long long, Eigen::Dynamic, 1>;
using ZhangBigInteger = boost::multiprecision::cpp_int;

struct ZhangIntegerTransformAudit
{
	bool valid = false;
	bool fullColumnRank = false;
	bool primitive = false;
	bool unimodular = false;
	int baseRank = 0;
	int targetRank = 0;
	ZhangBigInteger gcdMaximalMinors = 0;
	std::string failureReason;
};

inline ZhangBigInteger zhangBigIntegerAbs(ZhangBigInteger value)
{
	return value < 0 ? -value : value;
}

inline ZhangBigInteger zhangBigIntegerGcd(
	ZhangBigInteger first,
	ZhangBigInteger second)
{
	first = zhangBigIntegerAbs(first);
	second = zhangBigIntegerAbs(second);
	while (second != 0)
	{
		ZhangBigInteger remainder = first % second;
		first = second;
		second = remainder;
	}
	return first;
}

inline ZhangBigInteger zhangExactDeterminant(
	const ZhangIntegerMatrix& input)
{
	if (input.rows() != input.cols())
	{
		return 0;
	}
	const int size = input.rows();
	if (size == 0)
	{
		return 1;
	}
	std::vector<std::vector<ZhangBigInteger>> matrix(
		size, std::vector<ZhangBigInteger>(size));
	for (int row = 0; row < size; row++)
	for (int column = 0; column < size; column++)
	{
		matrix[row][column] = input(row, column);
	}
	ZhangBigInteger previousPivot = 1;
	int sign = 1;
	for (int pivotIndex = 0; pivotIndex + 1 < size; pivotIndex++)
	{
		int pivotRow = pivotIndex;
		while (pivotRow < size && matrix[pivotRow][pivotIndex] == 0)
		{
			pivotRow++;
		}
		if (pivotRow == size)
		{
			return 0;
		}
		if (pivotRow != pivotIndex)
		{
			std::swap(matrix[pivotRow], matrix[pivotIndex]);
			sign = -sign;
		}
		const ZhangBigInteger pivot = matrix[pivotIndex][pivotIndex];
		for (int row = pivotIndex + 1; row < size; row++)
		for (int column = pivotIndex + 1; column < size; column++)
		{
			matrix[row][column] =
				(matrix[row][column] * pivot
				 - matrix[row][pivotIndex] * matrix[pivotIndex][column])
				/ previousPivot;
		}
		previousPivot = pivot;
	}
	return sign * matrix[size - 1][size - 1];
}

/** Audit z=Z^T*k.  A rectangular full-column-rank Z is primitive iff the gcd
 * of all maximal minors is one. */
inline ZhangIntegerTransformAudit zhangAuditPrimitiveIntegerTransform(
	const ZhangIntegerMatrix& transform,
	std::size_t maximumMinorCount = 100000)
{
	ZhangIntegerTransformAudit result;
	result.baseRank = transform.rows();
	result.targetRank = transform.cols();
	if (transform.rows() == 0 || transform.cols() == 0
	 || transform.rows() < transform.cols())
	{
		result.failureReason = "INVALID_INTEGER_TARGET_TRANSFORM_DIMENSIONS";
		return result;
	}
	Eigen::FullPivLU<MatrixXd> rankAudit(transform.cast<double>());
	result.fullColumnRank = rankAudit.rank() == transform.cols();
	if (!result.fullColumnRank)
	{
		result.failureReason = "INTEGER_TARGET_TRANSFORM_RANK_DEFICIENT";
		return result;
	}

	std::size_t combinationCount = 1;
	for (int index = 1; index <= transform.cols(); index++)
	{
		combinationCount = combinationCount
			* static_cast<std::size_t>(transform.rows() - transform.cols() + index)
			/ static_cast<std::size_t>(index);
		if (combinationCount > maximumMinorCount)
		{
			result.failureReason = "INTEGER_TARGET_PRIMITIVITY_AUDIT_TOO_LARGE";
			return result;
		}
	}

	std::vector<int> selected;
	ZhangBigInteger gcd = 0;
	std::function<void(int)> enumerate = [&](int nextRow)
	{
		if (gcd == 1)
		{
			return;
		}
		if (static_cast<int>(selected.size()) == transform.cols())
		{
			ZhangIntegerMatrix minor(transform.cols(), transform.cols());
			for (int row = 0; row < transform.cols(); row++)
			{
				minor.row(row) = transform.row(selected[row]);
			}
			gcd = zhangBigIntegerGcd(gcd, zhangExactDeterminant(minor));
			return;
		}
		const int required = transform.cols() - selected.size();
		for (int row = nextRow; row <= transform.rows() - required; row++)
		{
			selected.push_back(row);
			enumerate(row + 1);
			selected.pop_back();
		}
	};
	enumerate(0);
	result.gcdMaximalMinors = zhangBigIntegerAbs(gcd);
	result.primitive = result.gcdMaximalMinors == 1;
	result.unimodular = transform.rows() == transform.cols()
		&& result.primitive;
	result.valid = result.fullColumnRank && result.primitive;
	if (!result.valid)
	{
		result.failureReason = "INTEGER_TARGET_TRANSFORM_NOT_PRIMITIVE";
	}
	return result;
}

struct ZhangLambdaReductionDiagnostics
{
	bool valid = false;
	bool transformUnimodular = false;
	bool candidateBackTransformConsistent = false;
	MatrixXd reducedCovariance;
	VectorXd conditionalSuccessRates;
	VectorXd reducedBestCandidate;
	VectorXd reducedSecondCandidate;
	double jointBootstrappedSuccessRate =
		std::numeric_limits<double>::quiet_NaN();
	double ambiguityDilutionOfPrecision =
		std::numeric_limits<double>::quiet_NaN();
	double covarianceTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double conditionalDeterminantLogError =
		std::numeric_limits<double>::quiet_NaN();
	double bestCandidateBackTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double secondCandidateBackTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double reducedCandidateIntegerMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	std::string failureReason;
};

/** Audit the exact operational LAMBDA coordinates z=Z^T*a.  The conditional
 * variances must be the post-reduction D returned by the same LAMBDA call;
 * using marginal variances here would reintroduce the reliability error this
 * diagnostic is intended to detect. */
inline ZhangLambdaReductionDiagnostics zhangAuditLambdaReduction(
	const MatrixXd& covariance,
	const MatrixXd& transform,
	const MatrixXd& operationalReducedCovariance,
	const VectorXd& conditionalVariances,
	const VectorXd& bestCandidate,
	const VectorXd& secondCandidate)
{
	ZhangLambdaReductionDiagnostics result;
	const int size = covariance.rows();
	if (size == 0 || covariance.cols() != size
	 || transform.rows() != size || transform.cols() != size
	 || operationalReducedCovariance.rows() != size
	 || operationalReducedCovariance.cols() != size
	 || conditionalVariances.size() != size
	 || bestCandidate.size() != size || secondCandidate.size() != size
	 || !covariance.allFinite() || !transform.allFinite()
	 || !operationalReducedCovariance.allFinite()
	 || !conditionalVariances.allFinite()
	 || !bestCandidate.allFinite() || !secondCandidate.allFinite()
	 || (conditionalVariances.array() <= 0).any())
	{
		result.failureReason = "INVALID_LAMBDA_REDUCTION_DIMENSIONS";
		return result;
	}
	ZhangIntegerMatrix integerTransform(size, size);
	for (int row = 0; row < size; row++)
	for (int column = 0; column < size; column++)
	{
		integerTransform(row, column) =
			std::llround(transform(row, column));
	}
	const double transformIntegerError =
		(transform - integerTransform.cast<double>()).cwiseAbs().maxCoeff();
	const auto transformAudit =
		zhangAuditPrimitiveIntegerTransform(integerTransform);
	result.transformUnimodular = transformIntegerError <= 1e-10
		&& transformAudit.valid && transformAudit.unimodular;
	if (!result.transformUnimodular)
	{
		result.failureReason = "NON_UNIMODULAR_LAMBDA_TRANSFORM";
		return result;
	}

	result.reducedCovariance = transform.transpose()
		* (0.5 * (covariance + covariance.transpose())) * transform;
	result.reducedCovariance = 0.5
		* (result.reducedCovariance + result.reducedCovariance.transpose());
	result.covarianceTransformMaximumError =
		(result.reducedCovariance - operationalReducedCovariance)
		.cwiseAbs().maxCoeff();
	Eigen::SelfAdjointEigenSolver<MatrixXd> spectrum(result.reducedCovariance);
	if (spectrum.info() != Eigen::Success
	 || (spectrum.eigenvalues().array() <= 0).any())
	{
		result.failureReason = "NON_POSITIVE_LAMBDA_REDUCED_COVARIANCE";
		return result;
	}
	const double covarianceScale = std::max(
		1.0, result.reducedCovariance.cwiseAbs().maxCoeff());
	result.conditionalDeterminantLogError = std::abs(
		spectrum.eigenvalues().array().log().sum()
		- conditionalVariances.array().log().sum());
	result.conditionalSuccessRates.resize(size);
	result.jointBootstrappedSuccessRate = 1;
	for (int index = 0; index < size; index++)
	{
		const double sigma = std::sqrt(conditionalVariances(index));
		result.conditionalSuccessRates(index) =
			std::erf(0.5 / (std::sqrt(2.0) * sigma));
		result.jointBootstrappedSuccessRate *=
			result.conditionalSuccessRates(index);
	}
	result.ambiguityDilutionOfPrecision = std::exp(
		0.5 * conditionalVariances.array().log().sum()
		/ static_cast<double>(size));

	result.reducedBestCandidate = transform.transpose() * bestCandidate;
	result.reducedSecondCandidate = transform.transpose() * secondCandidate;
	const VectorXd reconstructedBest = transform.transpose()
		.fullPivLu().solve(result.reducedBestCandidate);
	const VectorXd reconstructedSecond = transform.transpose()
		.fullPivLu().solve(result.reducedSecondCandidate);
	result.bestCandidateBackTransformMaximumError =
		(reconstructedBest - bestCandidate).cwiseAbs().maxCoeff();
	result.secondCandidateBackTransformMaximumError =
		(reconstructedSecond - secondCandidate).cwiseAbs().maxCoeff();
	result.reducedCandidateIntegerMaximumError = std::max(
		(result.reducedBestCandidate.array()
			- result.reducedBestCandidate.array().round()).abs().maxCoeff(),
		(result.reducedSecondCandidate.array()
			- result.reducedSecondCandidate.array().round()).abs().maxCoeff());
	result.candidateBackTransformConsistent =
		result.bestCandidateBackTransformMaximumError <= 1e-10
		&& result.secondCandidateBackTransformMaximumError <= 1e-10
		&& result.reducedCandidateIntegerMaximumError <= 1e-10;
	result.valid = result.covarianceTransformMaximumError
			<= 1e-10 * covarianceScale
		&& result.conditionalDeterminantLogError <= 1e-8
		&& result.candidateBackTransformConsistent
		&& result.conditionalSuccessRates.allFinite()
		&& std::isfinite(result.jointBootstrappedSuccessRate)
		&& std::isfinite(result.ambiguityDilutionOfPrecision);
	if (!result.valid)
	{
		result.failureReason = "INCONSISTENT_LAMBDA_REDUCTION";
	}
	return result;
}

struct ZhangTransformedIntegerTargets
{
	bool valid = false;
	ZhangIntegerTransformAudit audit;
	MatrixXd rows;
	VectorXd mean;
	MatrixXd covariance;
	std::string failureReason;
};

inline ZhangTransformedIntegerTargets zhangTransformIntegerTargets(
	const MatrixXd& baseRows,
	const VectorXd& baseMean,
	const MatrixXd& baseCovariance,
	const ZhangIntegerMatrix& primitiveTransform)
{
	ZhangTransformedIntegerTargets result;
	result.audit = zhangAuditPrimitiveIntegerTransform(primitiveTransform);
	if (!result.audit.valid)
	{
		result.failureReason = result.audit.failureReason;
		return result;
	}
	if (baseRows.rows() != primitiveTransform.rows()
	 || baseMean.size() != baseRows.rows()
	 || baseCovariance.rows() != baseRows.rows()
	 || baseCovariance.cols() != baseRows.rows())
	{
		result.failureReason = "INVALID_BASE_INTEGER_TARGET_DIMENSIONS";
		return result;
	}
	const MatrixXd transform = primitiveTransform.cast<double>();
	result.rows = transform.transpose() * baseRows;
	result.mean = transform.transpose() * baseMean;
	result.covariance = transform.transpose() * baseCovariance * transform;
	result.covariance = 0.5
		* (result.covariance + result.covariance.transpose());
	result.valid = result.rows.allFinite() && result.mean.allFinite()
		&& result.covariance.allFinite();
	if (!result.valid)
	{
		result.failureReason = "NONFINITE_TRANSFORMED_INTEGER_TARGETS";
	}
	return result;
}

inline ZhangIntegerMatrix zhangDirectJointIntegerTransform(int dimension)
{
	return ZhangIntegerMatrix::Identity(dimension, dimension);
}

/** z=[WL,L1]^T with WL=k1-k2 and L1=k1. */
inline ZhangIntegerMatrix zhangWideLaneL1IntegerTransform()
{
	ZhangIntegerMatrix transform(2, 2);
	transform << 1, 1, -1, 0;
	return transform;
}

struct ZhangIntegerQuotientCoordinates
{
	bool valid = false;
	ZhangIntegerMatrix transform;
	VectorXd mean;
	MatrixXd covariance;
	std::vector<std::string> labels;
	std::vector<std::string> families;
	std::vector<std::string> relations;
	std::string failureReason;
};

inline std::vector<std::string> zhangSplitIntegerIdentityPrefix(
	const std::string& identity)
{
	const std::string prefix = identity.substr(0, identity.find('|'));
	std::vector<std::string> fields;
	std::size_t begin = 0;
	while (begin <= prefix.size())
	{
		const std::size_t end = prefix.find(':', begin);
		fields.push_back(prefix.substr(begin, end - begin));
		if (end == std::string::npos)
		{
			break;
		}
		begin = end + 1;
	}
	return fields;
}

struct ZhangIntegerIdentityMetadata
{
	bool valid = false;
	std::string family;
	std::string anchor;
	std::string satellite;
	std::string relation;
};

/** Decode both the legacy integer identity
 *     GPS:K1_L1C:G01:G03|...
 * and the production persistent canonical separator identity
 *     GPS:L1C:G01->G03|...
 * into the same structured pairing metadata.
 *
 * The production separator deliberately uses the user-domain observable name
 * in its stable coordinate id.  WL construction must therefore not assume
 * that the K1/K2 family tag is still embedded in that display identity. */
inline ZhangIntegerIdentityMetadata zhangIntegerIdentityMetadata(
	const std::string& identity)
{
	ZhangIntegerIdentityMetadata result;
	const auto fields = zhangSplitIntegerIdentityPrefix(identity);
	if (fields.size() >= 4)
	{
		result.family = fields[1];
		result.anchor = fields[2];
		result.satellite = fields[3];
	}
	else if (fields.size() == 3)
	{
		const std::size_t arrow = fields[2].find("->");
		if (arrow == std::string::npos)
		{
			return result;
		}
		result.family = fields[1] == "L1C"
			? "K1_L1C" : fields[1] == "L2W"
			? "K2_L2W" : fields[1];
		result.anchor = fields[2].substr(0, arrow);
		result.satellite = fields[2].substr(arrow + 2);
	}
	if (result.family.empty() || result.anchor.empty()
	 || result.satellite.empty())
	{
		return result;
	}
	result.relation = result.anchor + "->" + result.satellite;
	result.valid = true;
	return result;
}

/** Construct a primitive integer coordinate basis for the quotient by every
 * unresolved additive datum.  Each unresolved group is differenced to one
 * deterministic member; absolute targets remain identity coordinates. */
inline ZhangIntegerQuotientCoordinates zhangBuildIntegerQuotientCoordinates(
	const std::vector<std::string>& identities,
	const std::vector<std::string>& gaugeIdentities,
	const std::vector<bool>& absoluteValidity,
	const VectorXd& mean,
	const MatrixXd& covariance)
{
	ZhangIntegerQuotientCoordinates result;
	const int size = identities.size();
	if (size == 0
	 || gaugeIdentities.size() != identities.size()
	 || absoluteValidity.size() != identities.size()
	 || mean.size() != size
	 || covariance.rows() != size || covariance.cols() != size)
	{
		result.failureReason = "INVALID_INTEGER_QUOTIENT_DIMENSIONS";
		return result;
	}
	std::map<std::string, std::vector<int>> unresolvedGroups;
	std::vector<int> absoluteIndices;
	for (int index = 0; index < size; index++)
	{
		if (absoluteValidity[index] || gaugeIdentities[index].empty())
		{
			absoluteIndices.push_back(index);
		}
		else
		{
			unresolvedGroups[gaugeIdentities[index]].push_back(index);
		}
	}
	const int quotientSize = absoluteIndices.size()
		+ std::accumulate(
			unresolvedGroups.begin(), unresolvedGroups.end(), 0,
			[](int total, const auto& entry)
			{
				return total + std::max(0,
					static_cast<int>(entry.second.size()) - 1);
			});
	if (quotientSize == 0)
	{
		result.failureReason = "EMPTY_INTEGER_QUOTIENT";
		return result;
	}
	result.transform = ZhangIntegerMatrix::Zero(size, quotientSize);
	int output = 0;
	auto metadata = [&](int index, const std::string& fallbackRelation)
	{
		const auto decoded = zhangIntegerIdentityMetadata(identities[index]);
		const std::string family = decoded.valid
			? decoded.family : "UNKNOWN";
		const std::string relation = decoded.valid
			? decoded.relation : fallbackRelation;
		return std::make_pair(family, relation);
	};
	for (int index : absoluteIndices)
	{
		result.transform(index, output) = 1;
		const auto [family, relation] = metadata(index, identities[index]);
		result.families.push_back(family);
		result.relations.push_back(relation);
		result.labels.push_back(family + ":" + relation);
		output++;
	}
	for (const auto& [gauge, indices] : unresolvedGroups)
	{
		if (indices.size() < 2)
		{
			continue;
		}
		const int reference = indices.front();
		const auto referenceMetadata =
			zhangIntegerIdentityMetadata(identities[reference]);
		const std::string referenceSatellite = referenceMetadata.valid
			? referenceMetadata.satellite : identities[reference];
		for (std::size_t member = 1; member < indices.size(); member++)
		{
			const int index = indices[member];
			result.transform(reference, output) = -1;
			result.transform(index, output) = 1;
			const auto [family, unusedRelation] = metadata(index, identities[index]);
			const auto decoded = zhangIntegerIdentityMetadata(identities[index]);
			const std::string satellite = decoded.valid
				? decoded.satellite : identities[index];
			const std::string relation = referenceSatellite + "->" + satellite;
			result.families.push_back(family);
			result.relations.push_back(relation);
			result.labels.push_back(family + ":" + relation);
			output++;
		}
	}
	const auto audit = zhangAuditPrimitiveIntegerTransform(result.transform);
	if (!audit.valid)
	{
		result.failureReason = audit.failureReason;
		return result;
	}
	const MatrixXd transform = result.transform.cast<double>();
	result.mean = transform.transpose() * mean;
	result.covariance = transform.transpose() * covariance * transform;
	result.covariance = 0.5
		* (result.covariance + result.covariance.transpose());
	result.valid = result.mean.allFinite() && result.covariance.allFinite();
	if (!result.valid)
	{
		result.failureReason = "NONFINITE_INTEGER_QUOTIENT";
	}
	return result;
}

struct ZhangWideLaneL1BlockCoordinates
{
	bool valid = false;
	ZhangIntegerMatrix transform;
	std::vector<std::string> labels;
	std::string failureReason;
};

inline double zhangBootstrapSuccessRate(const MatrixXd& covariance);

/** Build a complete unimodular block transform on paired K1/K2 quotient
 * relations.  This is a coordinate comparison, not a WL-only commitment. */
inline ZhangWideLaneL1BlockCoordinates zhangBuildWideLaneL1BlockCoordinates(
	const ZhangIntegerQuotientCoordinates& quotient)
{
	ZhangWideLaneL1BlockCoordinates result;
	if (!quotient.valid)
	{
		result.failureReason = quotient.failureReason;
		return result;
	}
	std::map<std::string, std::map<std::string, int>> byRelation;
	for (int index = 0; index < static_cast<int>(quotient.labels.size()); index++)
	{
		const std::string family = quotient.families[index].rfind("K1_", 0) == 0
			? "K1" : quotient.families[index].rfind("K2_", 0) == 0
			? "K2" : "OTHER";
		byRelation[quotient.relations[index]][family] = index;
	}
	result.transform = ZhangIntegerMatrix::Zero(
		quotient.mean.size(), quotient.mean.size());
	int output = 0;
	for (const auto& [relation, family] : byRelation)
	{
		if (family.size() != 2
		 || family.find("K1") == family.end()
		 || family.find("K2") == family.end())
		{
			result.failureReason = "UNPAIRED_K1_K2_INTEGER_RELATION";
			return result;
		}
		const int first = family.at("K1");
		const int second = family.at("K2");
		result.transform(first, output) = 1;
		result.transform(second, output) = -1;
		result.labels.push_back("WL:" + relation);
		output++;
		result.transform(first, output) = 1;
		result.labels.push_back("L1:" + relation);
		output++;
	}
	const auto audit = zhangAuditPrimitiveIntegerTransform(result.transform);
	result.valid = output == quotient.mean.size()
		&& audit.valid && audit.unimodular;
	if (!result.valid && result.failureReason.empty())
	{
		result.failureReason = audit.failureReason.empty()
			? "INCOMPLETE_WL_L1_INTEGER_TRANSFORM" : audit.failureReason;
	}
	return result;
}

/** Correlation-aware PAR: remove one coordinate at a time, choosing the
 * removal that maximizes the bootstrapped joint success rate. */
inline std::vector<int> zhangSelectParSubset(
	const MatrixXd& covariance,
	double successThreshold,
	double* achievedSuccess = nullptr)
{
	std::vector<int> retained(covariance.rows());
	std::iota(retained.begin(), retained.end(), 0);
	auto subsetCovariance = [&](const std::vector<int>& indices)
	{
		MatrixXd subset(indices.size(), indices.size());
		for (int row = 0; row < static_cast<int>(indices.size()); row++)
		for (int column = 0; column < static_cast<int>(indices.size()); column++)
		{
			subset(row, column) = covariance(indices[row], indices[column]);
		}
		return subset;
	};
	double success = zhangBootstrapSuccessRate(covariance);
	while (retained.size() > 1 && (!std::isfinite(success)
		|| success < successThreshold))
	{
		double bestSuccess = -1;
		std::vector<int> best;
		for (int removed = 0; removed < static_cast<int>(retained.size()); removed++)
		{
			std::vector<int> candidate = retained;
			candidate.erase(candidate.begin() + removed);
			const double candidateSuccess =
				zhangBootstrapSuccessRate(subsetCovariance(candidate));
			if (std::isfinite(candidateSuccess) && candidateSuccess > bestSuccess)
			{
				bestSuccess = candidateSuccess;
				best = std::move(candidate);
			}
		}
		if (best.empty())
		{
			retained.clear();
			break;
		}
		retained = std::move(best);
		success = bestSuccess;
	}
	if (!std::isfinite(success) || success < successThreshold)
	{
		retained.clear();
	}
	if (achievedSuccess)
	{
		*achievedSuccess = retained.empty()
			? std::numeric_limits<double>::quiet_NaN() : success;
	}
	return retained;
}

struct ZhangLambdaParDiagnostics
{
	bool valid = false;
	int quotientValidRank = 0;
	int absoluteValidRank = 0;
	int productRelationGraphRank = 0;
	int recoverableSatelliteCount = 0;
	int parTargetCount = 0;
	double bestCandidateDistance = std::numeric_limits<double>::quiet_NaN();
	double secondCandidateDistance = std::numeric_limits<double>::quiet_NaN();
	double distanceRatio = std::numeric_limits<double>::quiet_NaN();
	double jointBootstrappedSuccessRate =
		std::numeric_limits<double>::quiet_NaN();
	double parBootstrappedSuccessRate =
		std::numeric_limits<double>::quiet_NaN();
	double maximumCycleClosureError =
		std::numeric_limits<double>::quiet_NaN();
	std::vector<int> parIndices;
	std::string failureReason;
};

inline double zhangBootstrapSuccessRate(const MatrixXd& covariance)
{
	if (covariance.rows() == 0 || covariance.rows() != covariance.cols())
	{
		return std::numeric_limits<double>::quiet_NaN();
	}
	LDLT<MatrixXd> ldlt(0.5 * (covariance + covariance.transpose()));
	if (ldlt.info() != Eigen::Success || (ldlt.vectorD().array() <= 0).any())
	{
		return std::numeric_limits<double>::quiet_NaN();
	}
	double success = 1;
	for (int index = 0; index < ldlt.vectorD().size(); index++)
	{
		const double sigma = std::sqrt(ldlt.vectorD()(index));
		success *= std::erf(0.5 / (std::sqrt(2.0) * sigma));
	}
	return success;
}

/** Evaluate candidates returned by the operational LAMBDA solver and derive
 * a reliability-ordered PAR subset.  Candidate generation remains separate
 * so this diagnostic cannot silently replace LAMBDA with scalar rounding. */
inline ZhangLambdaParDiagnostics zhangEvaluateLambdaParCandidates(
	const VectorXd& floatMean,
	const MatrixXd& covariance,
	const ZhangIntegerVector& bestCandidate,
	const ZhangIntegerVector& secondCandidate,
	int quotientValidRank,
	int absoluteValidRank,
	const MatrixXd& productRelationDesign,
	const MatrixXd& cycleClosureDesign,
	double parSuccessThreshold = 0.999)
{
	ZhangLambdaParDiagnostics result;
	result.quotientValidRank = quotientValidRank;
	result.absoluteValidRank = absoluteValidRank;
	if (covariance.rows() != floatMean.size()
	 || covariance.cols() != floatMean.size()
	 || bestCandidate.size() != floatMean.size()
	 || secondCandidate.size() != floatMean.size()
	 || (cycleClosureDesign.size() != 0
		&& cycleClosureDesign.cols() != floatMean.size()))
	{
		result.failureReason = "INVALID_LAMBDA_DIAGNOSTIC_DIMENSIONS";
		return result;
	}
	LDLT<MatrixXd> ldlt(0.5 * (covariance + covariance.transpose()));
	if (ldlt.info() != Eigen::Success || (ldlt.vectorD().array() <= 0).any())
	{
		result.failureReason = "INVALID_LAMBDA_DIAGNOSTIC_COVARIANCE";
		return result;
	}
	auto distance = [&](const ZhangIntegerVector& candidate)
	{
		const VectorXd residual = floatMean - candidate.cast<double>();
		return residual.dot(ldlt.solve(residual));
	};
	result.bestCandidateDistance = distance(bestCandidate);
	result.secondCandidateDistance = distance(secondCandidate);
	result.distanceRatio = result.bestCandidateDistance > 0
		? result.secondCandidateDistance / result.bestCandidateDistance
		: std::numeric_limits<double>::infinity();
	result.jointBootstrappedSuccessRate = zhangBootstrapSuccessRate(covariance);

	if (productRelationDesign.size() != 0)
	{
		result.productRelationGraphRank =
			Eigen::FullPivLU<MatrixXd>(productRelationDesign).rank();
		result.recoverableSatelliteCount = std::min(
			static_cast<int>(productRelationDesign.cols()),
			result.productRelationGraphRank + 1);
	}
	if (cycleClosureDesign.rows() == 0)
	{
		result.maximumCycleClosureError = 0;
	}
	else
	{
		result.maximumCycleClosureError = (
			cycleClosureDesign * bestCandidate.cast<double>()).cwiseAbs().maxCoeff();
	}

	std::vector<int> order(floatMean.size());
	std::iota(order.begin(), order.end(), 0);
	std::sort(order.begin(), order.end(), [&](int left, int right)
	{
		return covariance(left, left) < covariance(right, right);
	});
	for (int count = 1; count <= static_cast<int>(order.size()); count++)
	{
		MatrixXd subset(count, count);
		for (int row = 0; row < count; row++)
		for (int column = 0; column < count; column++)
		{
			subset(row, column) = covariance(order[row], order[column]);
		}
		const double success = zhangBootstrapSuccessRate(subset);
		if (std::isfinite(success) && success >= parSuccessThreshold)
		{
			result.parIndices.assign(order.begin(), order.begin() + count);
			result.parTargetCount = count;
			result.parBootstrappedSuccessRate = success;
		}
		else if (result.parTargetCount > 0)
		{
			break;
		}
	}
	result.valid = std::isfinite(result.bestCandidateDistance)
		&& std::isfinite(result.secondCandidateDistance)
		&& std::isfinite(result.jointBootstrappedSuccessRate)
		&& std::isfinite(result.maximumCycleClosureError);
	if (!result.valid)
	{
		result.failureReason = "NONFINITE_LAMBDA_DIAGNOSTICS";
	}
	return result;
}
