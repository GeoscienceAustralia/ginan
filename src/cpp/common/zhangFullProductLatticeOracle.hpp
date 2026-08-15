#pragma once

#include <boost/json.hpp>

#include <fstream>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include "common/zhangIntegerAudit.hpp"

struct ZhangFullProductLatticeOraclePotential
{
	ZhangExactInteger wideLane = 0;
	ZhangExactInteger firstSignal = 0;
	ZhangExactInteger secondSignal = 0;
};

struct ZhangFullProductLatticeOracle
{
	bool valid = false;
	std::string system;
	std::string referenceSatellite;
	int rank = 0;
	std::map<std::string, ZhangFullProductLatticeOraclePotential> potentials;
	std::string failureReason = "NOT_LOADED";
};

inline ZhangFullProductLatticeOracle parseZhangFullProductLatticeOracle(
	const std::string& text,
	int expectedRank = 22)
{
	ZhangFullProductLatticeOracle result;
	auto fail = [&](const std::string& reason)
	{
		result.failureReason = reason;
		return result;
	};
	boost::system::error_code error;
	const auto value = boost::json::parse(text, error);
	if (error || !value.is_object()) return fail("ORACLE_JSON_INVALID");
	const auto& root = value.as_object();
	const auto* status = root.if_contains("status");
	const auto* hardGate = root.if_contains("hard_gate_passed");
	const auto* oracleValue = root.if_contains("oracle");
	if (!status || !status->is_string() ||
		status->as_string() != "FULL_ORACLE_READY" ||
		!hardGate || !hardGate->is_bool() || !hardGate->as_bool() ||
		!oracleValue || !oracleValue->is_object())
		return fail("ORACLE_HARD_GATE_NOT_PASSED");
	const auto& oracle = oracleValue->as_object();
	auto stringField = [&](const char* key, std::string& target)
	{
		const auto* field = oracle.if_contains(key);
		if (!field || !field->is_string()) return false;
		target = std::string(field->as_string());
		return !target.empty();
	};
	std::string schema;
	if (!stringField("schema", schema) ||
		schema != "ZHANG_FULL_PRODUCT_LATTICE_ORACLE_V1" ||
		!stringField("system", result.system) ||
		!stringField("reference_satellite", result.referenceSatellite))
		return fail("ORACLE_METADATA_INVALID");
	const auto* rank = oracle.if_contains("dual_frequency_rank");
	const auto* satellites = oracle.if_contains("satellites");
	const auto* relations = oracle.if_contains("relations");
	auto exactInteger = [](const boost::json::value* field,
		ZhangExactInteger& target)
	{
		if (!field) return false;
		if (field->is_int64())
		{
			target = field->as_int64();
			return true;
		}
		if (field->is_uint64() && field->as_uint64() <=
			static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
		{
			target = field->as_uint64();
			return true;
		}
		return false;
	};
	ZhangExactInteger parsedRank;
	if (!exactInteger(rank, parsedRank) || parsedRank < 1 ||
		!satellites || !satellites->is_array() ||
		!relations || !relations->is_array())
		return fail("ORACLE_DIMENSION_METADATA_INVALID");
	if (parsedRank > std::numeric_limits<int>::max())
		return fail("ORACLE_DIMENSION_METADATA_INVALID");
	result.rank = parsedRank.convert_to<int>();
	if (expectedRank > 0 && result.rank != expectedRank)
		return fail("ORACLE_EXPECTED_FULL_RANK_MISMATCH");
	std::set<std::string> satelliteSet;
	for (const auto& item : satellites->as_array())
	{
		if (!item.is_string()) return fail("ORACLE_SATELLITE_ID_INVALID");
		satelliteSet.insert(std::string(item.as_string()));
	}
	if (satelliteSet.size() != static_cast<std::size_t>(result.rank + 1) ||
		!satelliteSet.contains(result.referenceSatellite) ||
		relations->as_array().size() != static_cast<std::size_t>(result.rank))
		return fail("ORACLE_RANK_SATELLITE_COUNT_MISMATCH");
	result.potentials[result.referenceSatellite] = {};
	for (const auto& item : relations->as_array())
	{
		if (!item.is_object()) return fail("ORACLE_RELATION_INVALID");
		const auto& relation = item.as_object();
		const auto* satellite = relation.if_contains("satellite");
		const auto* reference = relation.if_contains("reference");
		const auto* wideLane = relation.if_contains(
			"wl_satellite_minus_reference");
		const auto* first = relation.if_contains(
			"l1_satellite_minus_reference");
		const auto* second = relation.if_contains(
			"l2_satellite_minus_reference");
		ZhangExactInteger parsedWideLane;
		ZhangExactInteger parsedFirst;
		ZhangExactInteger parsedSecond;
		if (!satellite || !satellite->is_string() ||
			!reference || !reference->is_string() ||
			!exactInteger(wideLane, parsedWideLane) ||
			!exactInteger(first, parsedFirst) ||
			!exactInteger(second, parsedSecond))
			return fail("ORACLE_RELATION_FIELD_INVALID");
		const std::string satelliteId(satellite->as_string());
		if (std::string(reference->as_string()) != result.referenceSatellite ||
			satelliteId == result.referenceSatellite ||
			!satelliteSet.contains(satelliteId) ||
			result.potentials.contains(satelliteId))
			return fail("ORACLE_RELATION_IDENTITY_INVALID");
		ZhangFullProductLatticeOraclePotential potential;
		potential.wideLane = parsedWideLane;
		potential.firstSignal = parsedFirst;
		potential.secondSignal = parsedSecond;
		if (potential.secondSignal !=
			potential.firstSignal - potential.wideLane)
			return fail("ORACLE_WL_L1_L2_NOT_ADMISSIBLE");
		result.potentials[satelliteId] = std::move(potential);
	}
	if (result.potentials.size() != satelliteSet.size())
		return fail("ORACLE_RELATION_COVERAGE_INCOMPLETE");
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

inline ZhangFullProductLatticeOracle loadZhangFullProductLatticeOracle(
	const std::string& filename,
	int expectedRank = 22)
{
	std::ifstream stream(filename);
	if (!stream)
	{
		ZhangFullProductLatticeOracle result;
		result.failureReason = "ORACLE_FILE_NOT_READABLE";
		return result;
	}
	std::ostringstream buffer;
	buffer << stream.rdbuf();
	return parseZhangFullProductLatticeOracle(buffer.str(), expectedRank);
}
