#pragma once

#include <cmath>
#include <limits>
#include <string>

#include "common/eigenIncluder.hpp"

/** Frozen Zhang-compatible dual-frequency Hybrid product model.
 *
 * For the two baseline frequencies, all physical code IF/GF biases have
 * already been absorbed into the estimable clock and ionosphere.  Therefore
 * the Layer-1 product contains exactly one satellite clock and two satellite
 * phase biases; it deliberately has no L1/L2 satellite code-bias product.
 */
struct ZhangDualFrequencyHybridProducts
{
	double satelliteClockMetres = std::numeric_limits<double>::quiet_NaN();
	// Correction-side satellite phase biases in equations (23), (25), (27).
	// The current server stores B^phi with the opposite sign, so delta^G=-B^phi.
	double firstPhaseCorrectionBiasMetres =
		std::numeric_limits<double>::quiet_NaN();
	double secondPhaseCorrectionBiasMetres =
		std::numeric_limits<double>::quiet_NaN();
	bool valid = false;

	double codeCorrectionToAddMetres(int frequency) const
	{
		return frequency == 0 || frequency == 1
			? satelliteClockMetres
			: std::numeric_limits<double>::quiet_NaN();
	}

	double phaseCorrectionToAddMetres(int frequency) const
	{
		if (frequency == 0)
		{
			return satelliteClockMetres + firstPhaseCorrectionBiasMetres;
		}
		if (frequency == 1)
		{
			return satelliteClockMetres + secondPhaseCorrectionBiasMetres;
		}
		return std::numeric_limits<double>::quiet_NaN();
	}
};

inline ZhangDualFrequencyHybridProducts zhangDualFrequencyHybridProducts(
	double satelliteClockMetres,
	double firstPhaseCorrectionBiasMetres,
	double secondPhaseCorrectionBiasMetres)
{
	ZhangDualFrequencyHybridProducts result;
	result.satelliteClockMetres = satelliteClockMetres;
	result.firstPhaseCorrectionBiasMetres = firstPhaseCorrectionBiasMetres;
	result.secondPhaseCorrectionBiasMetres = secondPhaseCorrectionBiasMetres;
	result.valid = std::isfinite(satelliteClockMetres)
		&& std::isfinite(firstPhaseCorrectionBiasMetres)
		&& std::isfinite(secondPhaseCorrectionBiasMetres);
	return result;
}

/** Map the existing Zhang/Hou server coordinate c^phi=C-B^phi to the
 * correction-side notation used by the supplied user equations: delta^G=-B^phi.
 * The phase-state arguments already include any certified integer alignment. */
inline ZhangDualFrequencyHybridProducts
zhangDualFrequencyHybridProductsFromInternalPhaseStates(
	double satelliteClockMetres,
	double firstInternalPhaseStateMetres,
	double secondInternalPhaseStateMetres)
{
	return zhangDualFrequencyHybridProducts(
		satelliteClockMetres,
		-firstInternalPhaseStateMetres,
		-secondInternalPhaseStateMetres);
}

/** Products are written on the left of the Zhang user equations. */
inline double zhangHybridApplyLeftCorrection(
	double rawMeasurementMetres,
	double correctionToAddMetres)
{
	return rawMeasurementMetres + correctionToAddMetres;
}

struct ZhangHybridCodeIfGfDatum
{
	double ifBiasMetres = std::numeric_limits<double>::quiet_NaN();
	double gfBiasMetres = std::numeric_limits<double>::quiet_NaN();
	bool valid = false;
};

/** Equations (3)--(4): d_j=d_IF+mu_j*d_GF. */
inline ZhangHybridCodeIfGfDatum zhangHybridCodeIfGfDatum(
	double firstCodeBiasMetres,
	double secondCodeBiasMetres,
	double firstIonosphereFactor,
	double secondIonosphereFactor)
{
	ZhangHybridCodeIfGfDatum result;
	const double denominator =
		secondIonosphereFactor - firstIonosphereFactor;
	if (!std::isfinite(firstCodeBiasMetres)
	 || !std::isfinite(secondCodeBiasMetres)
	 || !std::isfinite(firstIonosphereFactor)
	 || !std::isfinite(secondIonosphereFactor)
	 || std::abs(denominator) <= std::numeric_limits<double>::epsilon())
	{
		return result;
	}
	result.gfBiasMetres =
		(secondCodeBiasMetres - firstCodeBiasMetres) / denominator;
	result.ifBiasMetres =
		(secondIonosphereFactor * firstCodeBiasMetres
		 - firstIonosphereFactor * secondCodeBiasMetres) / denominator;
	result.valid = std::isfinite(result.ifBiasMetres)
		&& std::isfinite(result.gfBiasMetres);
	return result;
}

/** Equations (5)--(9): prove that the two code-bias directions disappear
 * into one receiver/satellite clock direction and one slant-ionosphere
 * direction. */
inline double zhangHybridOriginalCodePrediction(
	double receiverClockMetres,
	double satelliteClockMetres,
	double ionosphereMetres,
	double ionosphereFactor,
	double receiverCodeBiasMetres,
	double satelliteCodeBiasMetres)
{
	return receiverClockMetres - satelliteClockMetres
		+ ionosphereFactor * ionosphereMetres
		+ receiverCodeBiasMetres - satelliteCodeBiasMetres;
}

inline double zhangHybridFullRankCodePrediction(
	double estimableReceiverClockMetres,
	double estimableSatelliteClockMetres,
	double estimableIonosphereMetres,
	double ionosphereFactor)
{
	return estimableReceiverClockMetres - estimableSatelliteClockMetres
		+ ionosphereFactor * estimableIonosphereMetres;
}

/** Equations (11)--(13): phase-bias transformation required by the same
 * clock/ionosphere S-transform. */
inline double zhangHybridOriginalPhasePrediction(
	double receiverClockMetres,
	double satelliteClockMetres,
	double ionosphereMetres,
	double ionosphereFactor,
	double receiverPhaseBiasMetres,
	double satellitePhaseBiasMetres,
	double wavelengthMetres,
	long long ambiguityCycles)
{
	return receiverClockMetres - satelliteClockMetres
		- ionosphereFactor * ionosphereMetres
		+ receiverPhaseBiasMetres - satellitePhaseBiasMetres
		+ wavelengthMetres * ambiguityCycles;
}

inline double zhangHybridFullRankPhasePrediction(
	double estimableReceiverClockMetres,
	double estimableSatelliteClockMetres,
	double estimableIonosphereMetres,
	double ionosphereFactor,
	double estimableReceiverPhaseBiasMetres,
	double estimableSatellitePhaseBiasMetres,
	double wavelengthMetres,
	long long ambiguityCycles)
{
	return estimableReceiverClockMetres - estimableSatelliteClockMetres
		- ionosphereFactor * estimableIonosphereMetres
		+ estimableReceiverPhaseBiasMetres
		- estimableSatellitePhaseBiasMetres
		+ wavelengthMetres * ambiguityCycles;
}

inline MatrixXd zhangHybridSatelliteSingleDifferenceTransform(
	int satelliteCount,
	int referenceSatellite)
{
	if (satelliteCount < 2
	 || referenceSatellite < 0
	 || referenceSatellite >= satelliteCount)
	{
		return {};
	}
	MatrixXd result = MatrixXd::Zero(satelliteCount - 1, satelliteCount);
	int row = 0;
	for (int satellite = 0; satellite < satelliteCount; satellite++)
	{
		if (satellite == referenceSatellite)
		{
			continue;
		}
		result(row, satellite) = +1;
		result(row, referenceSatellite) = -1;
		row++;
	}
	return result;
}

/** Ordered [N1,N2] -> [NW,N1], with determinant one. */
inline Matrix2d zhangHybridWideLaneFirstIntegerTransform()
{
	Matrix2d result;
	result << 1, -1,
			  1,  0;
	return result;
}

enum class ZhangHybridIntegerUsability
{
	UNUSABLE,
	FLOAT_ONLY,
	PPP_AR_USABLE
};

inline ZhangHybridIntegerUsability zhangHybridIntegerUsability(
	bool productNumericallyUsable,
	bool relativeIntegerGaugeCertified)
{
	if (!productNumericallyUsable)
	{
		return ZhangHybridIntegerUsability::UNUSABLE;
	}
	return relativeIntegerGaugeCertified
		? ZhangHybridIntegerUsability::PPP_AR_USABLE
		: ZhangHybridIntegerUsability::FLOAT_ONLY;
}

/** A user satellite SD is integer only when both endpoints are certified in
 * the same non-trivial held-lattice component. */
inline bool zhangHybridRelativeIntegerPairCertified(
	bool firstPppArUsable,
	const std::string& firstComponent,
	bool secondPppArUsable,
	const std::string& secondComponent)
{
	return firstPppArUsable
		&& secondPppArUsable
		&& !firstComponent.empty()
		&& firstComponent != "NONE"
		&& firstComponent == secondComponent;
}

inline MatrixXd zhangHybridCorrectedObservationCovariance(
	const MatrixXd& userNoise,
	const MatrixXd& productCovariance)
{
	if (userNoise.rows() == 0
	 || userNoise.rows() != userNoise.cols()
	 || productCovariance.rows() != userNoise.rows()
	 || productCovariance.cols() != userNoise.cols()
	 || !userNoise.allFinite()
	 || !productCovariance.allFinite())
	{
		return {};
	}
	MatrixXd result = userNoise + productCovariance;
	return 0.5 * (result + result.transpose());
}

inline MatrixXd zhangHybridSingleDifferenceCovariance(
	const MatrixXd& userNoise,
	const MatrixXd& productCovariance,
	const MatrixXd& singleDifferenceTransform)
{
	const MatrixXd corrected = zhangHybridCorrectedObservationCovariance(
		userNoise, productCovariance);
	if (corrected.size() == 0
	 || singleDifferenceTransform.cols() != corrected.rows()
	 || !singleDifferenceTransform.allFinite())
	{
		return {};
	}
	MatrixXd result = singleDifferenceTransform * corrected
		* singleDifferenceTransform.transpose();
	return 0.5 * (result + result.transpose());
}
