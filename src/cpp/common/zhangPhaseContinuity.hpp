#pragma once

#include <cmath>
#include <string>
#include "common/gTime.hpp"

enum class ZhangPhaseContinuityChange
{
    NONE,
    EXACT_INTEGER,
    EXACT_FRACTIONAL,
    REINITIALISED
};

/** Observe the generation of the graph controller's auxiliary product tree.
 * A generation change is diagnostic evidence only.  Product continuity is
 * classified per satellite from its non-zero versioned physical functional;
 * the global generation must never reset a whole constellation by itself. */
struct ZhangProductDatumVersionTracker
{
    bool initialized = false;
    int  version = 0;

    bool observe(int nextVersion)
    {
        if (!initialized)
        {
            initialized = true;
            version = nextVersion;
            return false;
        }
        if (nextVersion == version)
        {
            return false;
        }
        version = nextVersion;
        return true;
    }
};

/** Per-(constellation, satellite, signal) continuity metadata for an internal
 * Zhang phase product.  Integer branch changes are absorbed without changing
 * the emitted correction.  Fractional changes and reinitialisations create a
 * new user ambiguity datum and therefore invalidate integer use.
 */
struct ZhangPhaseContinuityState
{
    int         counter = 0;
    long long   integerShiftCycles = 0;
    double      fractionalShiftCycles = 0;
    int         datumVersion = 0;
    GTime       validFrom;
    int         iod = 0;
    std::string resetReason = "initial";
    int         stabilizationRemaining = 0;
    bool        hasFixedDatum = false;
    GTime       lastEpoch;

    ZhangPhaseContinuityChange applyExactTransform(
        GTime  time,
        double cycleChange,
        int    stabilizationEpochs,
        double integerTolerance = 1e-8
    )
    {
        long long integerChange = std::llround(cycleChange);
        if (std::abs(cycleChange - integerChange) < integerTolerance)
        {
            integerShiftCycles += integerChange;
            resetReason = "exact_integer_s_transform";
            return ZhangPhaseContinuityChange::EXACT_INTEGER;
        }

        counter++;
        datumVersion++;
        iod++;
        validFrom = time;
        resetReason = "exact_fractional_s_transform";
        fractionalShiftCycles += cycleChange;
        stabilizationRemaining = stabilizationEpochs;
        hasFixedDatum = false;
        return ZhangPhaseContinuityChange::EXACT_FRACTIONAL;
    }

    /** Transport the internal dynamic-tree coordinate into a persistent
     * Hou-style product coordinate.
     *
     * A tree exchange is an exact affine S-transform, not a physical phase
     * event.  Its complete offset (integer and fractional parts) therefore
     * belongs in the product-coordinate map and must not change the product
     * discontinuity counter, datum version, IOD, or stabilisation state. */
    void applyHouProductTransform(double cycleChange)
    {
        const long long integerChange = std::llround(cycleChange);
        integerShiftCycles += integerChange;
        fractionalShiftCycles += cycleChange - integerChange;
        resetReason = "hou_exact_affine_s_transform";
    }

    ZhangPhaseContinuityChange reinitialise(
        GTime              time,
        const std::string& reason,
        int                stabilizationEpochs
    )
    {
        counter++;
        datumVersion++;
        iod++;
        validFrom = time;
        resetReason = reason;
        stabilizationRemaining = stabilizationEpochs;
        hasFixedDatum = false;
        integerShiftCycles = 0;
        fractionalShiftCycles = 0;
        return ZhangPhaseContinuityChange::REINITIALISED;
    }

    void advanceEpoch(GTime time)
    {
        if (lastEpoch == time)
        {
            return;
        }

        lastEpoch = time;
        if (stabilizationRemaining > 0)
        {
            stabilizationRemaining--;
        }
    }

    void markFixed(GTime time, int stabilizationEpochs)
    {
        if (!hasFixedDatum)
        {
            validFrom = time;
            stabilizationRemaining = stabilizationEpochs;
            resetReason = "integer_precision_acquired";
        }
        hasFixedDatum = true;
    }

    /** Preserve the original zero-stabilisation state-machine operation for
     * callers that do not own an epoch clock (notably algebraic unit tests).
     * Product generation must use the epoch-aware overload above. */
    void markFixed()
    {
        hasFixedDatum = true;
    }

    void markIntegerPrecisionUnavailable(
        const std::string& reason,
        int                stabilizationEpochs
    )
    {
        if (hasFixedDatum)
        {
            hasFixedDatum = false;
            stabilizationRemaining = stabilizationEpochs;
            resetReason = reason;
        }
    }

    bool invalidateIntegerDatum(
        GTime              time,
        const std::string& reason,
        int                stabilizationEpochs
    )
    {
        if (!hasFixedDatum)
        {
            return false;
        }

        counter++;
        datumVersion++;
        iod++;
        validFrom = time;
        resetReason = reason;
        stabilizationRemaining = stabilizationEpochs;
        hasFixedDatum = false;
        return true;
    }

    bool integerValid() const
    {
        return hasFixedDatum && stabilizationRemaining == 0;
    }
};
