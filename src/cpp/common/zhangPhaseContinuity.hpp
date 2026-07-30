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

    void markFixed()
    {
        hasFixedDatum = true;
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
