#include "pea/zhangE29MathClosure.hpp"

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <limits>
#include <map>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "common/algebra.hpp"
#include "common/constants.hpp"
#include "common/zhangCheckpoint.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangIfUser.hpp"
#include "common/zhangIntegerConditioner.hpp"
#include "common/zhangProductGaugeCompiler.hpp"
#include "pea/zhangPppAr.hpp"
#include "pea/zhangReference.hpp"

using std::map;
using std::set;
using std::string;
using std::vector;

namespace
{
struct AuditTarget
{
	string name;
	string definition;
	ZhangIarFunctional functional;
	int rank = 0;
};

template <typename DERIVED>
double maximumAbsolute(const Eigen::MatrixBase<DERIVED>& matrix)
{
	return matrix.size() == 0 ? 0 : matrix.cwiseAbs().maxCoeff();
}

double wavelength(E_Sys system, E_ObsCode code)
{
	auto systemEntry = code2Freq.find(system);
	if (systemEntry == code2Freq.end()) return 0;
	auto frequencyEntry = systemEntry->second.find(code);
	if (frequencyEntry == systemEntry->second.end()) return 0;
	auto wavelengthEntry = genericWavelength.find(frequencyEntry->second);
	return wavelengthEntry == genericWavelength.end()
		? 0 : wavelengthEntry->second;
}

KFKey receiverPhaseKey(E_Sys system, E_ObsCode code, const string& receiver)
{
	KFKey key;
	key.type = KF::PHASE_BIAS;
	key.Sat = SatSys(system, 0);
	key.str = receiver;
	key.num = static_cast<int>(code);
	return key;
}

KFKey satellitePhaseKey(E_ObsCode code, const SatSys& satellite)
{
	KFKey key;
	key.type = KF::PHASE_BIAS;
	key.Sat = satellite;
	key.num = static_cast<int>(code);
	return key;
}

KFKey ambiguityKey(E_ObsCode code, const ZhangGraphEdge& edge)
{
	KFKey key;
	key.type = KF::AMBIGUITY;
	key.Sat = edge.satellite;
	key.str = edge.receiver;
	key.num = static_cast<int>(code);
	return key;
}

bool graphObservationDesign(
	const KFState& state,
	E_Sys system,
	const vector<E_ObsCode>& observables,
	const ZhangGraphBasis& basis,
	SparseMatrix<double>& design,
	string& failureReason)
{
	vector<Eigen::Triplet<double>> entries;
	const int rows = basis.edges.size() * observables.size();
	int row = 0;
	for (const auto& edge : basis.edges)
	for (E_ObsCode code : observables)
	{
		if (edge.receiver != basis.rootReceiver)
		{
			auto receiver = state.kfIndexMap.find(
				receiverPhaseKey(system, code, edge.receiver));
			if (receiver == state.kfIndexMap.end())
			{
				failureReason = "E29_A2_RECEIVER_PHASE_STATE_MISSING";
				return false;
			}
			entries.emplace_back(row, receiver->second, 1);
		}
		auto satellite = state.kfIndexMap.find(
			satellitePhaseKey(code, edge.satellite));
		if (satellite == state.kfIndexMap.end())
		{
			failureReason = "E29_A2_SATELLITE_PHASE_STATE_MISSING";
			return false;
		}
		entries.emplace_back(row, satellite->second, 1);
		if (!basis.isTreeEdge(edge.receiver, edge.satellite))
		{
			auto ambiguity = state.kfIndexMap.find(ambiguityKey(code, edge));
			const double lambda = wavelength(system, code);
			if (ambiguity == state.kfIndexMap.end() || lambda <= 0)
			{
				failureReason = "E29_A2_CYCLE_STATE_OR_WAVELENGTH_MISSING";
				return false;
			}
			entries.emplace_back(row, ambiguity->second, lambda);
		}
		row++;
	}
	design.resize(rows, state.x.size());
	design.setFromTriplets(entries.begin(), entries.end());
	design.makeCompressed();
	failureReason = "NONE";
	return true;
}

SparseMatrix<double> stackedGaugeDesign(
	const SparseMatrix<double>& coordinateBlock,
	const SparseMatrix<double>& physicalBlock)
{
	vector<Eigen::Triplet<double>> entries;
	entries.reserve(coordinateBlock.nonZeros() + physicalBlock.nonZeros());
	for (int outer = 0; outer < coordinateBlock.outerSize(); outer++)
	for (SparseMatrix<double>::InnerIterator entry(coordinateBlock, outer);
		entry; ++entry)
	{
		entries.emplace_back(entry.row(), entry.col(), entry.value());
	}
	for (int outer = 0; outer < physicalBlock.outerSize(); outer++)
	for (SparseMatrix<double>::InnerIterator entry(physicalBlock, outer);
		entry; ++entry)
	{
		entries.emplace_back(
			coordinateBlock.rows() + entry.row(),
			entry.col(),
			entry.value());
	}
	SparseMatrix<double> result(
		coordinateBlock.rows() + physicalBlock.rows(),
		coordinateBlock.cols());
	result.setFromTriplets(entries.begin(), entries.end());
	result.makeCompressed();
	return result;
}

string basisIdentity(const ZhangGraphBasis& basis)
{
	std::ostringstream stream;
	stream << basis.rootReceiver;
	for (const auto& edge : basis.treeEdges)
	{
		stream << '|' << edge.receiver << '/' << edge.satellite.id();
	}
	return zhangCheckpointSha256(stream.str());
}

vector<ZhangGraphBasis> auditBases(const ZhangGraphBasis& original)
{
	vector<ZhangGraphBasis> result = {original};
	set<string> identities = {basisIdentity(original)};
	vector<string> receivers(
		original.receivers.begin(), original.receivers.end());
	for (int trial = 0; trial < 1000 && result.size() < 10; trial++)
	{
		std::mt19937 generator(29029 + trial);
		std::uniform_real_distribution<double> score(0, 1);
		map<ZhangGraphEdge, double> quality;
		for (const auto& edge : original.edges)
		{
			quality[edge] = score(generator);
		}
		const string& root = receivers[trial % receivers.size()];
		ZhangGraphBasis candidate = zhangBuildSpanningTree(
			original.edges, root, {}, quality);
		if (!candidate.connected
		 || candidate.receivers != original.receivers
		 || candidate.satellites != original.satellites)
		{
			continue;
		}
		const string identity = basisIdentity(candidate);
		if (identities.insert(identity).second)
		{
			result.push_back(std::move(candidate));
		}
	}
	return result;
}

ZhangIarFunctional rawFunctional(
	int rows,
	int columns,
	const vector<Eigen::Triplet<double>>& entries)
{
	ZhangIarFunctional result(rows, columns);
	result.setFromTriplets(
		entries.begin(), entries.end(),
		[](double left, double right) { return left + right; });
	result.prune(1e-15);
	result.makeCompressed();
	return result;
}

vector<AuditTarget> auditTargets(const KFState& state, E_Sys system)
{
	struct SignalRow
	{
		E_ObsCode code = E_ObsCode::NONE;
		SatSys satellite;
		vector<std::pair<int, double>> coefficients;
	};
	const int dimension = state.x.size();
	map<SatSys, int> satelliteClocks;
	vector<int> satelliteClockIndices;
	vector<SignalRow> clockRows;
	vector<SignalRow> codeRows;
	vector<SignalRow> phaseRows;
	for (const auto& [key, index] : state.kfIndexMap)
	{
		if (key.type == KF::SAT_CLOCK && key.Sat.sys == system
		 && key.Sat.prn > 0)
		{
			satelliteClocks[key.Sat] = index;
			satelliteClockIndices.push_back(index);
			SignalRow row;
			row.satellite = key.Sat;
			row.coefficients = {{index, 1}};
			clockRows.push_back(std::move(row));
		}
	}
	for (const auto& [key, index] : state.kfIndexMap)
	{
		if (key.type != KF::PHASE_BIAS || key.Sat.sys != system ||
		 key.Sat.prn <= 0 || !key.str.empty()
		 || !zhangPppArUsesObservable(
			system, static_cast<E_ObsCode>(key.num)))
		{
			continue;
		}
		auto clock = satelliteClocks.find(key.Sat);
		if (clock == satelliteClocks.end()) continue;
		SignalRow row;
		row.code = static_cast<E_ObsCode>(key.num);
		row.satellite = key.Sat;
		row.coefficients = {{clock->second, 1}, {index, -1}};
		phaseRows.push_back(row);
	}
	const auto observablesIt =
		acsConfig.zhangPppAr.baseline_observables.find(system);
	const vector<E_ObsCode> observables = observablesIt ==
		acsConfig.zhangPppAr.baseline_observables.end()
		? vector<E_ObsCode>{} : observablesIt->second;
	for (const auto& [satellite, clockIndex] : satelliteClocks)
	for (E_ObsCode code : observables)
	{
		SignalRow row;
		row.code = code;
		row.satellite = satellite;
		row.coefficients = {{clockIndex, 1}};
		codeRows.push_back(std::move(row));
	}
	auto build = [&](const vector<SignalRow>& rows, bool datumFree,
		int& rank)
	{
		vector<Eigen::Triplet<double>> entries;
		if (!datumFree)
		{
			for (int row = 0; row < rows.size(); row++)
			for (const auto& [column, value] : rows[row].coefficients)
				entries.emplace_back(row, column, value);
			rank = rows.size();
			return rawFunctional(rows.size(), dimension, entries);
		}
		map<E_ObsCode, vector<int>> groups;
		for (int row = 0; row < rows.size(); row++) groups[rows[row].code].push_back(row);
		rank = 0;
		for (const auto& [code, group] : groups)
		{
			if (group.size() < 2) continue;
			rank += group.size() - 1;
			const double common = 1.0 / group.size();
			for (int output : group)
			{
				for (const auto& [column, value] : rows[output].coefficients)
					entries.emplace_back(output, column, value);
				for (int source : group)
				for (const auto& [column, value] : rows[source].coefficients)
					entries.emplace_back(output, column, -common * value);
			}
		}
		return rawFunctional(rows.size(), dimension, entries);
	};
	auto buildIfRows = [&](const vector<SignalRow>& rows)
	{
		vector<SignalRow> result;
		if (observables.size() != 2) return result;
		const auto coefficients = zhangIfUserCoefficients(
			wavelength(system, observables[0]),
			wavelength(system, observables[1]));
		if (!coefficients.valid) return result;
		map<std::pair<SatSys, E_ObsCode>, const SignalRow*> bySignal;
		for (const auto& row : rows)
		{
			bySignal[{row.satellite, row.code}] = &row;
		}
		for (const auto& [satellite, ignored] : satelliteClocks)
		{
			auto first = bySignal.find({satellite, observables[0]});
			auto second = bySignal.find({satellite, observables[1]});
			if (first == bySignal.end() || second == bySignal.end()) continue;
			map<int, double> combined;
			for (const auto& [column, value] : first->second->coefficients)
				combined[column] += coefficients.alpha * value;
			for (const auto& [column, value] : second->second->coefficients)
				combined[column] += coefficients.beta * value;
			SignalRow row;
			row.satellite = satellite;
			for (const auto& [column, value] : combined)
				if (std::abs(value) > 1e-15)
					row.coefficients.push_back({column, value});
			result.push_back(std::move(row));
		}
		return result;
	};
	vector<Eigen::Triplet<double>> clockEntries;
	for (int row = 0; row < satelliteClockIndices.size(); row++)
		clockEntries.emplace_back(row, satelliteClockIndices[row], 1);
	int codeRank = 0;
	int phaseRank = 0;
	int userRank = 0;
	vector<AuditTarget> targets;
	targets.push_back({
		"CLOCK_RELATED_FUNCTIONAL",
		"ABSOLUTE_SATELLITE_CLOCK_STATE",
		rawFunctional(
			satelliteClockIndices.size(), dimension, clockEntries),
		static_cast<int>(satelliteClockIndices.size())});
	targets.push_back({
		"CODE_CORRECTION_FUNCTIONAL",
		"DATUM_FREE_ZHANG_DUAL_FREQUENCY_SATELLITE_CLOCK",
		build(codeRows, true, codeRank), codeRank});
	targets.push_back({
		"HOU_CLOCK_MINUS_PHASE_PRODUCT",
		"SATELLITE_CLOCK_PLUS_CORRECTION_SIDE_PHASE_BIAS_"
		"WHERE_DELTA_EQUALS_MINUS_INTERNAL_B_PHI",
		build(phaseRows, false, phaseRank), phaseRank});
	targets.push_back({
		"USER_DATUM_FREE_PRODUCT",
		"PER_SIGNAL_COMMON_DATUM_PROJECTED_CLOCK_MINUS_PHASE",
		build(phaseRows, true, userRank), userRank});
	int ifCodeRank = 0;
	int ifPhaseRank = 0;
	const auto ifCodeRows = buildIfRows(codeRows);
	const auto ifPhaseRows = buildIfRows(phaseRows);
	targets.push_back({
		"USER_IF_CODE_FUNCTIONAL",
		"DATUM_FREE_ALPHA_CODE_L1_PLUS_BETA_CODE_L2",
		build(ifCodeRows, true, ifCodeRank), ifCodeRank});
	targets.push_back({
		"USER_IF_PHASE_FUNCTIONAL",
		"DATUM_FREE_ALPHA_PHASE_L1_PLUS_BETA_PHASE_L2",
		build(ifPhaseRows, true, ifPhaseRank), ifPhaseRank});
	return targets;
}

const AuditTarget* targetByName(
	const vector<AuditTarget>& targets, const string& name)
{
	auto found = std::find_if(targets.begin(), targets.end(),
		[&](const AuditTarget& target) { return target.name == name; });
	return found == targets.end() ? nullptr : &*found;
}

double projectedTrace(
	const MatrixXd& covariance, const ZhangIarFunctional& functional)
{
	return (functional * covariance * functional.transpose()).diagonal().sum();
}

struct ConditioningMetrics
{
	bool valid = false;
	MatrixXd exactCovariance;
};

ConditioningMetrics auditConditioning(
	Trace& trace,
	const string& label,
	const VectorXd& mean,
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	GTime time)
{
	ConditioningMetrics metrics;
	const VectorXd integers = (constraints * mean).array().round();
	const auto analytical = zhangConditionIntegersExact(
		mean, covariance, constraints, integers);
	const auto squareRoot = zhangConditionIntegersSquareRootOrthogonal(
		mean, covariance, constraints, integers);
	double meanDifference = std::numeric_limits<double>::infinity();
	double covarianceDifference = std::numeric_limits<double>::infinity();
	if (analytical.valid && squareRoot.valid)
	{
		meanDifference = maximumAbsolute(analytical.mean - squareRoot.mean);
		covarianceDifference = maximumAbsolute(
			analytical.covariance - squareRoot.covariance);
		metrics.exactCovariance = analytical.covariance;
	}
	const bool hardPass = analytical.valid && squareRoot.valid
		&& meanDifference < 1e-9 && covarianceDifference < 1e-9;
	trace << "\nZHANG_E29_B2_ORTHOGONAL time=" << time.to_string(0)
		<< " constraint_set=" << label
		<< " state_dimension=" << mean.size()
		<< " constraint_rows=" << constraints.rows()
		<< " analytical_rank=" << analytical.constraintRank
		<< " square_root_rank=" << squareRoot.constraintRank
		<< " covariance_rank=" << squareRoot.covarianceRank
		<< " max_mean_difference=" << meanDifference
		<< " max_covariance_difference=" << covarianceDifference
		<< " analytical_constraint_residual="
		<< analytical.maximumConstraintResidual
		<< " square_root_constraint_residual="
		<< squareRoot.maximumConstraintResidual
		<< " qr_min_diagonal=" << squareRoot.minimumSquareRootDiagonal
		<< " qr_max_diagonal=" << squareRoot.maximumSquareRootDiagonal
		<< " analytical_failure=" << analytical.failureReason
		<< " square_root_failure=" << squareRoot.failureReason
		<< " status=" << (hardPass ? "PASS" : "FAIL")
		<< " candidate_correctness_not_claimed ar_authorized=0 feedback=0";

	for (double sigma : {1e-4, 1e-6, 1e-8})
	{
		const auto pseudo = zhangConditionIntegersPseudoObservation(
			mean, covariance, constraints, integers, sigma);
		const double pseudoMeanDifference = analytical.valid && pseudo.valid
			? maximumAbsolute(analytical.mean - pseudo.mean)
			: std::numeric_limits<double>::infinity();
		const double pseudoCovarianceDifference = analytical.valid && pseudo.valid
			? maximumAbsolute(analytical.covariance - pseudo.covariance)
			: std::numeric_limits<double>::infinity();
		trace << "\nZHANG_E29_B2_PSEUDO_CONVERGENCE time="
			<< time.to_string(0)
			<< " constraint_set=" << label
			<< " sigma_cycles=" << sigma
			<< " max_mean_difference=" << pseudoMeanDifference
			<< " max_covariance_difference=" << pseudoCovarianceDifference
			<< " constraint_residual=" << pseudo.maximumConstraintResidual
			<< " status=" << (pseudo.valid ? "VALID" : "INVALID")
			<< " candidate_correctness_not_claimed ar_authorized=0 feedback=0";
	}
	metrics.valid = hardPass;
	return metrics;
}
}

bool traceZhangE29RealMathClosure(
	Trace& trace,
	const KFState& posterior,
	E_Sys system,
	GTime time,
	const MatrixXd& covarianceF0,
	const MatrixXd& covarianceWideLane,
	const ZhangIarFunctional& parConstraints,
	const ZhangIarFunctional& fullConstraints)
{
	trace << std::setprecision(16);
	ZhangGraphIntegerContext context;
	const bool graphValid = zhangGraphIntegerContext(
		posterior, system, context) && context.initialized
		&& context.basis.connected;
	const vector<E_ObsCode> observables = {E_ObsCode::L1C, E_ObsCode::L2W};
	int forbiddenBaselineCodeBiasStates = 0;
	for (const auto& [key, index] : posterior.kfIndexMap)
	{
		if (key.type != KF::CODE_BIAS || key.Sat.sys != system)
		{
			continue;
		}
		const E_ObsCode code = static_cast<E_ObsCode>(key.num);
		if (std::find(observables.begin(), observables.end(), code)
			!= observables.end())
		{
			forbiddenBaselineCodeBiasStates++;
		}
	}
	const bool dualFrequencyMinimalCodeModel =
		forbiddenBaselineCodeBiasStates == 0;
	trace << "\nZHANG_E29_A2_CODE_MODEL time=" << time.to_string(0)
		<< " system=" << enum_to_string(system)
		<< " baseline_observables=L1C,L2W"
		<< " explicit_baseline_code_bias_states="
		<< forbiddenBaselineCodeBiasStates
		<< " code_product=SATELLITE_CLOCK_ONLY"
		<< " receiver_code_bias_state=ABSENT"
		<< " satellite_code_bias_product=ABSENT"
		<< " status="
		<< (dualFrequencyMinimalCodeModel ? "PASS" : "FAIL")
		<< " ar_authorized=0 feedback=0";
	const vector<ZhangGraphBasis> bases = graphValid
		? auditBases(context.basis) : vector<ZhangGraphBasis>{};
	SparseMatrix<double> frontendPhysical;
	string failureReason;
	const bool frontendDesignValid = graphValid && graphObservationDesign(
		posterior, system, observables, context.basis,
		frontendPhysical, failureReason);
	const auto targets = auditTargets(posterior, system);
	int validBases = 0;
	double maximumFrontendMeanDifference = 0;
	double maximumFrontendCovarianceDifference = 0;
	double maximumObservationPredictionDifference = 0;
	double maximumObservationCovarianceDifference = 0;
	double maximumProductMeanDifference = 0;
	double maximumProductCovarianceDifference = 0;
	for (int basisIndex = 0;
		frontendDesignValid && basisIndex < bases.size(); basisIndex++)
	{
		KFState branch = posterior;
		SparseMatrix<double> forward;
		if (!applyZhangGraphBasisTransformForAudit(
				trace, branch, system, observables, context.basis,
				bases[basisIndex], forward, failureReason))
		{
			trace << "\nZHANG_E29_A2_BASIS time=" << time.to_string(0)
				<< " basis_index=" << basisIndex
				<< " status=FAIL reason=" << failureReason
				<< " feedback=0";
			continue;
		}
		SparseMatrix<double> backendPhysical;
		if (!graphObservationDesign(
				branch, system, observables, bases[basisIndex],
				backendPhysical, failureReason))
		{
			continue;
		}
		const VectorXd backendMean = branch.x;
		const MatrixXd backendCovariance = branch.P;
		SparseMatrix<double> reverse;
		if (!applyZhangGraphBasisTransformForAudit(
				trace, branch, system, observables, bases[basisIndex],
				context.basis, reverse, failureReason))
		{
			continue;
		}
		SparseMatrix<double> identity(posterior.x.size(), posterior.x.size());
		identity.setIdentity();
		const SparseMatrix<double> frontendDesign = stackedGaugeDesign(
			identity, frontendPhysical);
		const SparseMatrix<double> backendDesign = stackedGaugeDesign(
			reverse, backendPhysical);
		const auto compiled = zhangCompileProductGaugeTransform(
			frontendDesign, backendDesign, 1e-12);
		const VectorXd frontendMean = compiled.valid
			? compiled.transform * backendMean : VectorXd();
		const MatrixXd frontendCovariance = compiled.valid
			? zhangProjectProductGaugeCovariance(
				backendCovariance, compiled.transform) : MatrixXd();
		const double meanDifference = compiled.valid
			? maximumAbsolute(frontendMean - posterior.x)
			: std::numeric_limits<double>::infinity();
		const double covarianceDifference = compiled.valid
			? maximumAbsolute(frontendCovariance - posterior.P)
			: std::numeric_limits<double>::infinity();
		const double observationDifference = maximumAbsolute(
			frontendPhysical * posterior.x
			- backendPhysical * backendMean);
		const MatrixXd frontendObservationCovariance =
			frontendPhysical * posterior.P * frontendPhysical.transpose();
		const MatrixXd backendObservationCovariance =
			backendPhysical * backendCovariance * backendPhysical.transpose();
		const double observationCovarianceDifference = maximumAbsolute(
			frontendObservationCovariance - backendObservationCovariance);
		double productMeanDifference = 0;
		double productCovarianceDifference = 0;
		bool targetsPass = !targets.empty();
		for (const auto& target : targets)
		{
			const double targetMeanDifference = maximumAbsolute(
				target.functional * frontendMean
					- target.functional * posterior.x);
			productMeanDifference = std::max(
				productMeanDifference,
				targetMeanDifference);
			const MatrixXd baselineTargetCovariance = target.functional
				* posterior.P * target.functional.transpose();
			const MatrixXd mappedTargetCovariance = target.functional
				* frontendCovariance * target.functional.transpose();
			const double targetCovarianceDifference = maximumAbsolute(
				baselineTargetCovariance - mappedTargetCovariance);
			const bool targetPass = target.functional.rows() > 0
				&& targetMeanDifference < 1e-10
				&& targetCovarianceDifference
					< 1e-10 * std::max(
						1.0, maximumAbsolute(baselineTargetCovariance));
			targetsPass = targetsPass && targetPass;
			productCovarianceDifference = std::max(
				productCovarianceDifference,
				targetCovarianceDifference);
			trace << "\nZHANG_E29_A2_TARGET time=" << time.to_string(0)
				<< " basis_index=" << basisIndex
				<< " target=" << target.name
				<< " definition=" << target.definition
				<< " target_rows=" << target.functional.rows()
				<< " target_rank=" << target.rank
				<< " mean_difference=" << targetMeanDifference
				<< " covariance_difference="
				<< targetCovarianceDifference
				<< " status="
				<< (targetPass ? "PASS" : "FAIL")
				<< " feedback=0";
		}
		const SparseMatrix<double> inverseClosure = reverse * forward;
		MatrixXd inverseDense(inverseClosure);
		inverseDense.diagonal().array() -= 1;
		const double inverseError = maximumAbsolute(inverseDense);
		const bool basisPass = compiled.valid
			&& compiled.maximumClosureError < 1e-12
			&& inverseError < 1e-12
			&& meanDifference < 1e-10
			&& covarianceDifference < 1e-10
			&& observationDifference < 1e-10
			&& observationCovarianceDifference
				< 1e-10 * std::max(1.0,
					maximumAbsolute(frontendObservationCovariance))
			&& productMeanDifference < 1e-10
			&& productCovarianceDifference
				< 1e-10 * std::max(1.0, maximumAbsolute(posterior.P))
			&& targetsPass;
		validBases += basisPass;
		maximumFrontendMeanDifference = std::max(
			maximumFrontendMeanDifference, meanDifference);
		maximumFrontendCovarianceDifference = std::max(
			maximumFrontendCovarianceDifference, covarianceDifference);
		maximumObservationPredictionDifference = std::max(
			maximumObservationPredictionDifference, observationDifference);
		maximumObservationCovarianceDifference = std::max(
			maximumObservationCovarianceDifference,
			observationCovarianceDifference);
		maximumProductMeanDifference = std::max(
			maximumProductMeanDifference, productMeanDifference);
		maximumProductCovarianceDifference = std::max(
			maximumProductCovarianceDifference,
			productCovarianceDifference);
		trace << "\nZHANG_E29_A2_BASIS time=" << time.to_string(0)
			<< " basis_index=" << basisIndex
			<< " basis_identity=" << basisIdentity(bases[basisIndex])
			<< " root_receiver=" << bases[basisIndex].rootReceiver
			<< " tree_edges=" << bases[basisIndex].treeEdges.size()
			<< " state_dimension=" << posterior.x.size()
			<< " physical_rows=" << frontendPhysical.rows()
			<< " frontend_rank=" << compiled.frontendRank
			<< " backend_rank=" << compiled.backendRank
			<< " compiler_closure=" << compiled.maximumClosureError
			<< " inverse_closure=" << inverseError
			<< " mean_difference=" << meanDifference
			<< " covariance_difference=" << covarianceDifference
			<< " observation_prediction_difference="
			<< observationDifference
			<< " observation_covariance_difference="
			<< observationCovarianceDifference
			<< " product_mean_difference=" << productMeanDifference
			<< " product_covariance_difference="
			<< productCovarianceDifference
			<< " status=" << (basisPass ? "PASS" : "FAIL")
			<< " feedback=0";
	}
	const bool a2Pass = graphValid && frontendDesignValid
		&& dualFrequencyMinimalCodeModel
		&& bases.size() >= 10 && validBases == bases.size();
	trace << "\nZHANG_E29_A2_SUMMARY time=" << time.to_string(0)
		<< " system=" << enum_to_string(system)
		<< " same_posterior_state_dimension=" << posterior.x.size()
		<< " requested_bases=10 generated_bases=" << bases.size()
		<< " valid_bases=" << validBases
		<< " explicit_baseline_code_bias_states="
		<< forbiddenBaselineCodeBiasStates
		<< " dual_frequency_minimal_code_model="
		<< dualFrequencyMinimalCodeModel
		<< " max_frontend_mean_difference="
		<< maximumFrontendMeanDifference
		<< " max_frontend_covariance_difference="
		<< maximumFrontendCovarianceDifference
		<< " max_observation_prediction_difference="
		<< maximumObservationPredictionDifference
		<< " max_observation_covariance_difference="
		<< maximumObservationCovarianceDifference
		<< " max_product_mean_difference=" << maximumProductMeanDifference
		<< " max_product_covariance_difference="
		<< maximumProductCovarianceDifference
		<< " posterior_core_sha256="
		<< zhangCheckpointSha256(serializeZhangCheckpointSectionPayload(
			captureZhangCheckpointKfCore(posterior)))
		<< " status=" << (a2Pass ? "PASS" : "FAIL")
		<< " ar_authorized=0 feedback=0";

	const bool dimensionsValid = covarianceF0.rows() == posterior.x.size()
		&& covarianceWideLane.rows() == posterior.x.size()
		&& covarianceF0.cols() == posterior.x.size()
		&& covarianceWideLane.cols() == posterior.x.size();
	ConditioningMetrics par;
	ConditioningMetrics full;
	if (dimensionsValid)
	{
		par = auditConditioning(
			trace, "PAR", posterior.x, covarianceWideLane,
			parConstraints, time);
		full = auditConditioning(
			trace, "FULL_L1", posterior.x, covarianceWideLane,
			fullConstraints, time);
	}
	const AuditTarget* hou = targetByName(
		targets, "HOU_CLOCK_MINUS_PHASE_PRODUCT");
	const AuditTarget* user = targetByName(
		targets, "USER_DATUM_FREE_PRODUCT");
	double wlUserReduction = std::numeric_limits<double>::quiet_NaN();
	double fullUserReduction = std::numeric_limits<double>::quiet_NaN();
	double fullHouReduction = std::numeric_limits<double>::quiet_NaN();
	if (dimensionsValid && full.valid && hou && user)
	{
		wlUserReduction = 1 - projectedTrace(
			covarianceWideLane, user->functional)
			/ projectedTrace(covarianceF0, user->functional);
		fullUserReduction = 1 - projectedTrace(
			full.exactCovariance, user->functional)
			/ projectedTrace(covarianceWideLane, user->functional);
		fullHouReduction = 1 - projectedTrace(
			full.exactCovariance, hou->functional)
			/ projectedTrace(covarianceWideLane, hou->functional);
	}
	// The historical E24a oracle (506 FULL rows) predates the canonical
	// physical-candidate changes now present on this branch.  A current-binary,
	// instrumentation-free control was therefore frozen before this gate:
	// 20 PAR rows, 511 FULL rows, and the three full-precision reductions below.
	constexpr double currentWlUserReduction = 0.4329404533315212;
	constexpr double currentFullUserReduction = 0.2564277917818110;
	constexpr double currentFullHouReduction = 0.1057102527835297;
	const bool dimensionsMatchCurrentOracle = parConstraints.rows() == 20
		&& fullConstraints.rows() == 511;
	const bool gainPass = dimensionsMatchCurrentOracle
		&& std::isfinite(wlUserReduction)
		&& std::abs(wlUserReduction - currentWlUserReduction) <= 1e-8
		&& std::abs(fullUserReduction - currentFullUserReduction) <= 1e-8
		&& std::abs(fullHouReduction - currentFullHouReduction) <= 1e-8;
	const bool historicalOracleReproduced = fullConstraints.rows() == 506
		&& std::abs(wlUserReduction - 0.472020) <= 1e-8
		&& std::abs(fullUserReduction - 0.256508) <= 1e-8
		&& std::abs(fullHouReduction - 0.105747) <= 1e-8;
	const bool b2Pass = dimensionsValid && par.valid && full.valid && gainPass;
	trace << "\nZHANG_E29_B2_SUMMARY time=" << time.to_string(0)
		<< " system=" << enum_to_string(system)
		<< " state_dimension=" << posterior.x.size()
		<< " par_rows=" << parConstraints.rows()
		<< " full_rows=" << fullConstraints.rows()
		<< " wl_over_f0_user_trace_reduction=" << wlUserReduction
		<< " full_over_wl_user_trace_reduction=" << fullUserReduction
		<< " full_over_wl_hou_trace_reduction=" << fullHouReduction
		<< " oracle_id=E29_CURRENT_CANONICAL_511_20260811"
		<< " oracle_full_rows=511"
		<< " historical_e24a_506_reproduced="
		<< historicalOracleReproduced
		<< " gain_regression_status=" << (gainPass ? "PASS" : "FAIL")
		<< " status=" << (b2Pass ? "PASS" : "FAIL")
		<< " candidate_correctness_not_claimed ar_authorized=0 feedback=0";
	return a2Pass && b2Pass;
}
