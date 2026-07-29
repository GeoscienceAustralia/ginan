#pragma once

#include <cmath>
#include <optional>
#include "common/constants.hpp"

/** Coefficients used by the baseline-frequency phase-clock/OSB datum.
 *
 * The convention is alpha*x1 - beta*x2.  alpha and beta are positive and
 * alpha - beta = 1.
 */
struct PhaseClockOsbCoefficients
{
    double alpha          = 0;
    double beta           = 0;
    double frequencyRatio = 0;
    double lambda1        = 0;
    double lambda2        = 0;
    double lambdaWide     = 0;
    double lambdaNarrow   = 0;
};

inline std::optional<PhaseClockOsbCoefficients> phaseClockOsbCoefficients(
    double lambda1,
    double lambda2
)
{
    if (!std::isfinite(lambda1) || !std::isfinite(lambda2) || lambda1 <= 0 || lambda2 <= 0)
    {
        return {};
    }

    double denominator = lambda2 * lambda2 - lambda1 * lambda1;
    if (std::abs(denominator) < 1e-20 || std::abs(lambda2 - lambda1) < 1e-20)
    {
        return {};
    }

    PhaseClockOsbCoefficients coefficients;
    coefficients.alpha          = lambda2 * lambda2 / denominator;
    coefficients.beta           = lambda1 * lambda1 / denominator;
    coefficients.frequencyRatio = lambda2 / lambda1;
    coefficients.lambda1        = lambda1;
    coefficients.lambda2        = lambda2;
    coefficients.lambdaWide     = lambda1 * lambda2 / (lambda2 - lambda1);
    coefficients.lambdaNarrow   = lambda1 * lambda2 / (lambda2 + lambda1);

    return coefficients;
}

inline std::optional<PhaseClockOsbCoefficients> phaseClockOsbCoefficients(
    E_Sys     sys,
    E_ObsCode code1,
    E_ObsCode code2
)
{
    auto sysIt = code2Freq.find(sys);
    if (sysIt == code2Freq.end())
    {
        return {};
    }

    auto freq1It = sysIt->second.find(code1);
    auto freq2It = sysIt->second.find(code2);
    if (freq1It == sysIt->second.end() || freq2It == sysIt->second.end())
    {
        return {};
    }

    auto lambda1It = genericWavelength.find(freq1It->second);
    auto lambda2It = genericWavelength.find(freq2It->second);
    if (lambda1It == genericWavelength.end() || lambda2It == genericWavelength.end())
    {
        return {};
    }

    return phaseClockOsbCoefficients(lambda1It->second, lambda2It->second);
}

inline double phaseClockOsbFractionalCycle(double ambiguity)
{
    return ambiguity - std::round(ambiguity);
}
