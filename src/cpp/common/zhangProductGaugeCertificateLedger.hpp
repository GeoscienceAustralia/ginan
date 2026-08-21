#pragma once

#include <algorithm>
#include <array>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "common/enums.h"
#include "common/satSys.hpp"
#include "common/zhangIntegerAudit.hpp"

enum class ProductGaugeCertificateState
{
	PROVISIONAL,
	CONFIRMING,
	ACTIVE,
	SUSPENDED,
	REVOKED
};

inline const char* zhangProductGaugeCertificateStateName(
	ProductGaugeCertificateState state)
{
	switch (state)
	{
		case ProductGaugeCertificateState::PROVISIONAL: return "PROVISIONAL";
		case ProductGaugeCertificateState::CONFIRMING: return "CONFIRMING";
		case ProductGaugeCertificateState::ACTIVE:      return "ACTIVE";
		case ProductGaugeCertificateState::SUSPENDED:   return "SUSPENDED";
		case ProductGaugeCertificateState::REVOKED:     return "REVOKED";
	}
	return "UNKNOWN";
}

/** Persistent frontend knowledge of one dual-frequency satellite product
 * relation.  This deliberately does not retain receiver ambiguity arcs or a
 * backend S-basis generation: those belong to ProductIntegerLedger.  The
 * identity is the satellite pair plus the four physical phase-product
 * segments.  A tree/basis change therefore cannot retire this certificate,
 * while a satellite phase-segment change does. */
struct ProductGaugeCertificate
{
	E_Sys system = E_Sys::NONE;
	E_ObsCode firstObservable = E_ObsCode::NONE;
	E_ObsCode secondObservable = E_ObsCode::NONE;
	SatSys satellite;
	SatSys reference;
	ZhangExactInteger wideLaneInteger = 0;
	ZhangExactInteger firstSignalInteger = 0;
	std::array<std::string, 2> satellitePhaseSegments;
	std::array<std::string, 2> referencePhaseSegments;
	int componentId = -1;
	int componentVersion = 0;
	long firstCertified = 0;
	long lastConfirmed = 0;
	int confirmationEpochs = 0;
	// Admission evidence is stored with the certificate rather than inferred
	// from a current backend generation.  A gauge product therefore survives a
	// tree/S-basis reparameterisation, but cannot silently survive a failed
	// temporal alignment or a changed physical phase segment.
	bool wideLaneReliable = false;
	bool firstSignalReliable = false;
	bool exactProductLatticeMembership = false;
	bool jointNisPassed = false;
	bool cycleClosurePassed = false;
	bool temporalAlignmentCertified = false;
	bool noResidualDof = false;
	int independentSupportPaths = 0;
	ProductGaugeCertificateState state = ProductGaugeCertificateState::PROVISIONAL;
	bool currentAlignmentValid = false;
	bool active = false;
	std::string source = "PRODUCT_COMPONENT_GAUGE";
};

struct ProductGaugeCertificateLedgerUpdate
{
	bool valid = false;
	int inputCertificates = 0;
	int freshCertificates = 0;
	int confirmedCertificates = 0;
	int conflicts = 0;
	int retiredSegmentCertificates = 0;
	int suspendedCertificates = 0;
	int revokedCertificates = 0;
	int rejectedCandidates = 0;
	std::string failureReason = "NOT_EVALUATED";
};

inline void zhangCanonicaliseProductGaugeCertificate(
	ProductGaugeCertificate& certificate)
{
	if (certificate.reference < certificate.satellite)
	{
		std::swap(certificate.satellite, certificate.reference);
		std::swap(certificate.satellitePhaseSegments,
			certificate.referencePhaseSegments);
		certificate.wideLaneInteger = -certificate.wideLaneInteger;
		certificate.firstSignalInteger = -certificate.firstSignalInteger;
	}
}

inline std::string zhangProductGaugeCertificatePairKey(
	const ProductGaugeCertificate& certificate)
{
	std::ostringstream stream;
	stream << enum_to_string(certificate.system) << "|"
		<< static_cast<int>(certificate.firstObservable) << "|"
		<< static_cast<int>(certificate.secondObservable) << "|"
		<< static_cast<int>(certificate.satellite) << "|"
		<< static_cast<int>(certificate.reference);
	return stream.str();
}

inline std::string zhangProductGaugeCertificateIdentity(
	const ProductGaugeCertificate& certificate)
{
	std::ostringstream stream;
	stream << zhangProductGaugeCertificatePairKey(certificate) << "|"
		<< certificate.satellitePhaseSegments[0] << "|"
		<< certificate.satellitePhaseSegments[1] << "|"
		<< certificate.referencePhaseSegments[0] << "|"
		<< certificate.referencePhaseSegments[1];
	return stream.str();
}

inline bool zhangProductGaugeCertificateEvidenceComplete(
	const ProductGaugeCertificate& certificate,
	int requiredConfirmations)
{
	if (!certificate.wideLaneReliable || !certificate.firstSignalReliable ||
		!certificate.exactProductLatticeMembership || !certificate.jointNisPassed ||
		!certificate.cycleClosurePassed || !certificate.temporalAlignmentCertified)
		return false;
	// A square bridge has no residual degrees of freedom.  It needs either a
	// separate support path or a multi-epoch confirmation; a one-epoch rounded
	// relation is never a strict dual-frequency certificate.
	if (certificate.noResidualDof &&
		certificate.independentSupportPaths <= 0 && requiredConfirmations < 2)
		return false;
	return true;
}

/** A frontend certificate is usable only for the exact pair of current
 * physical phase-product segments that created it.  This is intentionally a
 * segment test, not a backend-generation or product-tree test: changing an
 * S-basis leaves the physical relation intact, whereas replacing either
 * satellite phase segment invalidates it. */
inline bool zhangProductGaugeCertificateMatchesCurrentSegments(
	const ProductGaugeCertificate& certificate,
	E_ObsCode firstObservable,
	E_ObsCode secondObservable,
	const std::array<std::string, 2>& satelliteSegments,
	const std::array<std::string, 2>& referenceSegments)
{
	return certificate.state == ProductGaugeCertificateState::ACTIVE
		&& certificate.active
		&& certificate.currentAlignmentValid
		&& certificate.firstObservable == firstObservable
		&& certificate.secondObservable == secondObservable
		&& certificate.satellitePhaseSegments == satelliteSegments
		&& certificate.referencePhaseSegments == referenceSegments;
}

inline void zhangSetProductGaugeCertificateState(
	ProductGaugeCertificate& certificate,
	ProductGaugeCertificateState state)
{
	certificate.state = state;
	certificate.active = state == ProductGaugeCertificateState::ACTIVE;
	certificate.currentAlignmentValid = certificate.active &&
		certificate.temporalAlignmentCertified;
}

class ProductGaugeCertificateLedger
{
public:
	ProductGaugeCertificateLedgerUpdate observe(
		long epoch,
		const std::vector<ProductGaugeCertificate>& candidates,
		int requiredConfirmations)
	{
		ProductGaugeCertificateLedgerUpdate result;
		result.inputCertificates = candidates.size();
		if (epoch <= 0 || requiredConfirmations < 1)
		{
			result.failureReason = "PRODUCT_GAUGE_LEDGER_INPUT_INVALID";
			return result;
		}
		for (auto candidate : candidates)
		{
			zhangCanonicaliseProductGaugeCertificate(candidate);
			if (candidate.system == E_Sys::NONE ||
				candidate.satellite == candidate.reference ||
				std::any_of(candidate.satellitePhaseSegments.begin(),
					candidate.satellitePhaseSegments.end(),
					[](const auto& value) { return value.empty(); }) ||
				std::any_of(candidate.referencePhaseSegments.begin(),
					candidate.referencePhaseSegments.end(),
					[](const auto& value) { return value.empty(); }))
			{
				result.failureReason = "PRODUCT_GAUGE_CERTIFICATE_SEGMENT_MISSING";
				return result;
			}
			const auto pairKey = zhangProductGaugeCertificatePairKey(candidate);
			const auto identity = zhangProductGaugeCertificateIdentity(candidate);
			for (auto& existing : certificates_)
			{
				if (zhangProductGaugeCertificatePairKey(existing) == pairKey &&
					zhangProductGaugeCertificateIdentity(existing) != identity)
				{
					zhangSetProductGaugeCertificateState(existing,
						ProductGaugeCertificateState::REVOKED);
					result.retiredSegmentCertificates++;
					result.revokedCertificates++;
				}
			}
			if (!zhangProductGaugeCertificateEvidenceComplete(
				candidate, requiredConfirmations))
			{
				for (auto& existing : certificates_)
				if (zhangProductGaugeCertificateIdentity(existing) == identity &&
					existing.state != ProductGaugeCertificateState::REVOKED)
				{
					zhangSetProductGaugeCertificateState(existing,
						ProductGaugeCertificateState::SUSPENDED);
					result.suspendedCertificates++;
				}
				result.rejectedCandidates++;
				continue;
			}
			auto existing = std::find_if(certificates_.rbegin(), certificates_.rend(),
				[&](const auto& certificate)
				{
					return zhangProductGaugeCertificateIdentity(certificate) == identity &&
						certificate.wideLaneInteger == candidate.wideLaneInteger &&
						certificate.firstSignalInteger == candidate.firstSignalInteger;
				});
			if (existing == certificates_.rend())
			{
				for (auto& historical : certificates_)
				if (zhangProductGaugeCertificateIdentity(historical) == identity &&
					historical.state != ProductGaugeCertificateState::REVOKED)
				{
					zhangSetProductGaugeCertificateState(historical,
						ProductGaugeCertificateState::SUSPENDED);
					result.suspendedCertificates++;
					result.conflicts++;
				}
				candidate.firstCertified = epoch;
				candidate.lastConfirmed = epoch;
				candidate.confirmationEpochs = 1;
				candidate.componentVersion = ++version_;
				zhangSetProductGaugeCertificateState(candidate,
					requiredConfirmations == 1
						? ProductGaugeCertificateState::ACTIVE
						: ProductGaugeCertificateState::CONFIRMING);
				certificates_.push_back(std::move(candidate));
				result.freshCertificates++;
				result.confirmedCertificates += certificates_.back().active;
				continue;
			}
			// reverse_iterator lets us retain the conflicting historical
			// certificate as SUSPENDED instead of overwriting its provenance.
			auto& confirmed = *existing;
			if (confirmed.lastConfirmed != epoch)
			{
				confirmed.lastConfirmed = epoch;
				confirmed.confirmationEpochs++;
			}
			zhangSetProductGaugeCertificateState(confirmed,
				confirmed.confirmationEpochs >= requiredConfirmations
					? ProductGaugeCertificateState::ACTIVE
					: ProductGaugeCertificateState::CONFIRMING);
			result.confirmedCertificates += confirmed.active;
		}
		result.valid = true;
		result.failureReason = "NONE";
		return result;
	}

	const std::vector<ProductGaugeCertificate>& certificates() const
	{
		return certificates_;
	}

	/** Each active segment-keyed certificate is one independent named
	 * satellite-pair product relation.  The scheduler uses this rank-like
	 * count only to keep private closure alive; exact lattice admission still
	 * performs its own HNF/rank audit. */
	int activeRank() const
	{
		return std::count_if(certificates_.begin(), certificates_.end(),
			[](const auto& certificate)
			{
				return certificate.state == ProductGaugeCertificateState::ACTIVE &&
					certificate.active && certificate.currentAlignmentValid;
			});
	}

private:
	std::vector<ProductGaugeCertificate> certificates_;
	int version_ = 0;
};

inline std::map<std::pair<std::string, E_Sys>, ProductGaugeCertificateLedger>&
zhangProductGaugeCertificateLedgerRegistry()
{
	static std::map<std::pair<std::string, E_Sys>,
		ProductGaugeCertificateLedger> registry;
	return registry;
}
