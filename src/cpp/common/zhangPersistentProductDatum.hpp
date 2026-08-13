#pragma once

#include <algorithm>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "common/enums.h"
#include "common/satSys.hpp"

/** A product target is always expressed in this deterministic satellite-pair
 * coordinate.  The orientation is lexical, so a reference-satellite change
 * cannot silently reverse or replace the integer functional. */
struct ZhangCanonicalSatelliteRelation
{
	SatSys anchor;
	SatSys satellite;

	static ZhangCanonicalSatelliteRelation ordered(SatSys a, SatSys b)
	{
		if (b < a)
		{
			std::swap(a, b);
		}
		return {a, b};
	}

	bool operator<(const ZhangCanonicalSatelliteRelation& other) const
	{
		return std::tie(anchor, satellite)
			< std::tie(other.anchor, other.satellite);
	}

	bool operator==(const ZhangCanonicalSatelliteRelation& other) const
	{
		return anchor == other.anchor && satellite == other.satellite;
	}

	std::string id() const
	{
		return satelliteId(anchor) + "->" + satelliteId(satellite);
	}

private:
	static std::string satelliteId(const SatSys& value)
	{
		std::ostringstream stream;
		stream << value.sysChar() << std::setw(2) << std::setfill('0')
			<< value.prn;
		return stream.str();
	}
};

struct ZhangCanonicalRelationSelection
{
	bool established = false;
	bool silentSubstitutionRejected = false;
	std::string canonicalSetId;
	std::vector<ZhangCanonicalSatelliteRelation> selected;
	std::vector<ZhangCanonicalSatelliteRelation> missing;
	std::vector<ZhangCanonicalSatelliteRelation> ignoredSubstitutes;
};

struct ZhangPersistentProductDatumObservation
{
	bool valid = false;
	bool absoluteValid = false;
	bool quotientOnly = true;
	bool versionChanged = false;
	int version = 0;
	std::string canonicalCoordinateId;
	std::string productDatumId;
	std::string failureReason;
};

struct ZhangPersistentProductDatumCheckpointState
{
	E_Sys system = E_Sys::NONE;
	E_ObsCode observable = E_ObsCode::NONE;
	ZhangCanonicalSatelliteRelation relation;
	bool initialised = false;
	int version = 0;
	int anchorPhaseSegment = 0;
	int satellitePhaseSegment = 0;
	int anchorDatumVersion = 0;
	int satelliteDatumVersion = 0;
};

struct ZhangPersistentProductDatumCheckpoint
{
	std::map<E_Sys, std::vector<ZhangCanonicalSatelliteRelation>>
		canonicalRelations;
	std::vector<ZhangPersistentProductDatumCheckpointState> datumStates;
};

/** Persistent E18 product coordinates.
 *
 * The satellite relation set is established once per constellation and is
 * never enlarged or replaced merely because the current graph chooses another
 * reference.  L1C and L2W datum versions are maintained independently.  Only
 * an explicit endpoint phase/datum version change advances a signal datum;
 * temporary loss of absolute observability leaves the version intact and
 * reports quotient-only availability.
 */
class ZhangPersistentProductDatumRegistry
{
public:
	ZhangPersistentProductDatumCheckpoint checkpointState() const
	{
		ZhangPersistentProductDatumCheckpoint result;
		result.canonicalRelations = canonicalRelations;
		for (const auto& [key, state] : datumStates)
		{
			result.datumStates.push_back({
				key.system, key.observable, key.relation,
				state.initialised, state.version,
				state.anchorPhaseSegment, state.satellitePhaseSegment,
				state.anchorDatumVersion, state.satelliteDatumVersion});
		}
		return result;
	}

	bool restoreCheckpointState(
		const ZhangPersistentProductDatumCheckpoint& snapshot,
		std::string* failureReason = nullptr)
	{
		auto fail = [&](const std::string& reason)
		{
			if (failureReason)
			{
				*failureReason = reason;
			}
			return false;
		};
		ZhangPersistentProductDatumRegistry candidate;
		candidate.canonicalRelations = snapshot.canonicalRelations;
		for (const auto& [system, relations] : candidate.canonicalRelations)
		{
			std::set<ZhangCanonicalSatelliteRelation> unique;
			for (const auto& relation : relations)
			{
				if (system == E_Sys::NONE
				 || relation.anchor.sys != system
				 || relation.satellite.sys != system
				 || relation.anchor.prn <= 0
				 || relation.satellite.prn <= 0
				 || relation.anchor == relation.satellite
				 || relation != ZhangCanonicalSatelliteRelation::ordered(
					relation.anchor, relation.satellite)
				 || !unique.insert(relation).second)
				{
					return fail("E18_CHECKPOINT_INVALID_CANONICAL_RELATION");
				}
			}
		}
		for (const auto& value : snapshot.datumStates)
		{
			auto canonical = candidate.canonicalRelations.find(value.system);
			const bool relationIsCanonical =
				canonical != candidate.canonicalRelations.end()
				&& std::find(
					canonical->second.begin(), canonical->second.end(),
					value.relation) != canonical->second.end();
			if (value.system == E_Sys::NONE
			 || value.observable == E_ObsCode::NONE
			 || value.relation.anchor.sys != value.system
			 || value.relation.satellite.sys != value.system
			 || value.relation.anchor.prn <= 0
			 || value.relation.satellite.prn <= 0
			 || value.relation.anchor == value.relation.satellite
			 || !relationIsCanonical
			 || value.relation != ZhangCanonicalSatelliteRelation::ordered(
				value.relation.anchor, value.relation.satellite)
			 || value.version < 0 || value.anchorPhaseSegment < 0
			 || value.satellitePhaseSegment < 0
			 || value.anchorDatumVersion < 0
			 || value.satelliteDatumVersion < 0
			 || (!value.initialised
				 && (value.version != 0 || value.anchorPhaseSegment != 0
					 || value.satellitePhaseSegment != 0
					 || value.anchorDatumVersion != 0
					 || value.satelliteDatumVersion != 0)))
			{
				return fail("E18_CHECKPOINT_INVALID_DATUM_STATE");
			}
			DatumKey key{value.system, value.observable, value.relation};
			DatumState state{
				value.initialised, value.version,
				value.anchorPhaseSegment, value.satellitePhaseSegment,
				value.anchorDatumVersion, value.satelliteDatumVersion};
			if (!candidate.datumStates.emplace(key, state).second)
			{
				return fail("E18_CHECKPOINT_DUPLICATE_DATUM_STATE");
			}
		}
		*this = std::move(candidate);
		if (failureReason)
		{
			failureReason->clear();
		}
		return true;
	}

	ZhangCanonicalRelationSelection selectRelations(
		E_Sys system,
		const std::vector<ZhangCanonicalSatelliteRelation>& bootstrapCandidates,
		const std::set<SatSys>& availableSatellites,
		int maximumRelations)
	{
		ZhangCanonicalRelationSelection result;
		if (maximumRelations <= 0)
		{
			return result;
		}
		auto& canonical = canonicalRelations[system];
		std::set<ZhangCanonicalSatelliteRelation> candidates;
		for (const auto& candidate : bootstrapCandidates)
		{
			if (candidate.anchor != candidate.satellite)
			{
				candidates.insert(ZhangCanonicalSatelliteRelation::ordered(
					candidate.anchor, candidate.satellite));
			}
		}
		if (canonical.empty())
		{
			for (const auto& candidate : candidates)
			{
				if (static_cast<int>(canonical.size()) >= maximumRelations)
				{
					break;
				}
				canonical.push_back(candidate);
			}
		}
		result.established = !canonical.empty();
		std::set<ZhangCanonicalSatelliteRelation> canonicalSet(
			canonical.begin(), canonical.end());
		for (const auto& candidate : candidates)
		{
			if (canonicalSet.find(candidate) == canonicalSet.end())
			{
				result.ignoredSubstitutes.push_back(candidate);
			}
		}
		for (const auto& relation : canonical)
		{
			if (availableSatellites.find(relation.anchor)
					!= availableSatellites.end()
			 && availableSatellites.find(relation.satellite)
					!= availableSatellites.end())
			{
				result.selected.push_back(relation);
			}
			else
			{
				result.missing.push_back(relation);
			}
		}
		result.silentSubstitutionRejected =
			!result.missing.empty() && !result.ignoredSubstitutes.empty();
		std::ostringstream identity;
		identity << enum_to_string(system) << ":CANONICAL";
		for (const auto& relation : canonical)
		{
			identity << ":" << relation.id();
		}
		result.canonicalSetId = identity.str();
		return result;
	}

	ZhangPersistentProductDatumObservation observe(
		E_Sys system,
		E_ObsCode observable,
		const ZhangCanonicalSatelliteRelation& relation,
		int anchorPhaseSegment,
		int satellitePhaseSegment,
		int anchorDatumVersion,
		int satelliteDatumVersion,
		bool absoluteAvailable)
	{
		ZhangPersistentProductDatumObservation result;
		if (system == E_Sys::NONE || observable == E_ObsCode::NONE
		 || relation.anchor == relation.satellite)
		{
			result.failureReason = "INVALID_PERSISTENT_PRODUCT_DATUM";
			return result;
		}
		const auto canonical = ZhangCanonicalSatelliteRelation::ordered(
			relation.anchor, relation.satellite);
		DatumKey key{system, observable, canonical};
		auto& state = datumStates[key];
		if (!state.initialised)
		{
			state.initialised = true;
			state.anchorPhaseSegment = anchorPhaseSegment;
			state.satellitePhaseSegment = satellitePhaseSegment;
			state.anchorDatumVersion = anchorDatumVersion;
			state.satelliteDatumVersion = satelliteDatumVersion;
		}
		else if (state.anchorPhaseSegment != anchorPhaseSegment
			  || state.satellitePhaseSegment != satellitePhaseSegment
			  || state.anchorDatumVersion != anchorDatumVersion
			  || state.satelliteDatumVersion != satelliteDatumVersion)
		{
			state.version++;
			state.anchorPhaseSegment = anchorPhaseSegment;
			state.satellitePhaseSegment = satellitePhaseSegment;
			state.anchorDatumVersion = anchorDatumVersion;
			state.satelliteDatumVersion = satelliteDatumVersion;
			result.versionChanged = true;
		}
		result.valid = true;
		result.absoluteValid = absoluteAvailable;
		result.quotientOnly = !absoluteAvailable;
		result.version = state.version;
		result.canonicalCoordinateId = enum_to_string(system) + ":"
			+ enum_to_string(observable) + ":" + canonical.id();
		result.productDatumId = result.canonicalCoordinateId + ":V"
			+ std::to_string(state.version);
		return result;
	}

private:
	struct DatumKey
	{
		E_Sys system = E_Sys::NONE;
		E_ObsCode observable = E_ObsCode::NONE;
		ZhangCanonicalSatelliteRelation relation;

		bool operator<(const DatumKey& other) const
		{
			return std::tie(system, observable, relation)
				< std::tie(other.system, other.observable, other.relation);
		}
	};

	struct DatumState
	{
		bool initialised = false;
		int version = 0;
		int anchorPhaseSegment = 0;
		int satellitePhaseSegment = 0;
		int anchorDatumVersion = 0;
		int satelliteDatumVersion = 0;
	};

	std::map<E_Sys, std::vector<ZhangCanonicalSatelliteRelation>>
		canonicalRelations;
	std::map<DatumKey, DatumState> datumStates;
};
