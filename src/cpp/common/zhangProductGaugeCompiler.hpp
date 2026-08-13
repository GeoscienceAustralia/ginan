#pragma once

#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangIntegerAudit.hpp"

inline ZhangExactInteger zhangExactGcd(
	ZhangExactInteger left, ZhangExactInteger right)
{
	if (left < 0) left = -left;
	if (right < 0) right = -right;
	while (right != 0)
	{
		ZhangExactInteger remainder = left % right;
		left = right;
		right = remainder;
	}
	return left;
}

/** Minimal canonical rational over cpp_int.  Keeping it local avoids adding a
 * new Boost component to the production build solely for exact E29-A gates. */
class ZhangExactRational
{
public:
	ZhangExactInteger numerator = 0;
	ZhangExactInteger denominator = 1;

	ZhangExactRational() = default;
	ZhangExactRational(const ZhangExactInteger& integer)
		: numerator(integer) {}
	ZhangExactRational(long long integer)
		: numerator(integer) {}
	ZhangExactRational(
		const ZhangExactInteger& numerator,
		const ZhangExactInteger& denominator)
		: numerator(numerator), denominator(denominator)
	{
		normalise();
	}

	ZhangExactRational& operator+=(const ZhangExactRational& other)
	{
		numerator = numerator * other.denominator
			+ other.numerator * denominator;
		denominator *= other.denominator;
		normalise();
		return *this;
	}
	ZhangExactRational& operator-=(const ZhangExactRational& other)
	{
		numerator = numerator * other.denominator
			- other.numerator * denominator;
		denominator *= other.denominator;
		normalise();
		return *this;
	}
	ZhangExactRational& operator*=(const ZhangExactRational& other)
	{
		numerator *= other.numerator;
		denominator *= other.denominator;
		normalise();
		return *this;
	}
	ZhangExactRational& operator/=(const ZhangExactRational& other)
	{
		numerator *= other.denominator;
		denominator *= other.numerator;
		normalise();
		return *this;
	}

	friend ZhangExactRational operator+(
		ZhangExactRational left, const ZhangExactRational& right)
	{
		return left += right;
	}
	friend ZhangExactRational operator-(
		ZhangExactRational left, const ZhangExactRational& right)
	{
		return left -= right;
	}
	friend ZhangExactRational operator*(
		ZhangExactRational left, const ZhangExactRational& right)
	{
		return left *= right;
	}
	friend ZhangExactRational operator/(
		ZhangExactRational left, const ZhangExactRational& right)
	{
		return left /= right;
	}
	friend bool operator==(
		const ZhangExactRational& left,
		const ZhangExactRational& right)
	{
		return left.numerator == right.numerator
			&& left.denominator == right.denominator;
	}
	friend bool operator!=(
		const ZhangExactRational& left,
		const ZhangExactRational& right)
	{
		return !(left == right);
	}

private:
	void normalise()
	{
		if (denominator == 0)
		{
			throw std::domain_error("zero exact rational denominator");
		}
		if (numerator == 0)
		{
			denominator = 1;
			return;
		}
		if (denominator < 0)
		{
			numerator = -numerator;
			denominator = -denominator;
		}
		const ZhangExactInteger divisor = zhangExactGcd(
			numerator, denominator);
		numerator /= divisor;
		denominator /= divisor;
	}
};

using ZhangExactRationalVector = std::vector<ZhangExactRational>;
using ZhangExactRationalMatrix = std::vector<ZhangExactRationalVector>;

struct ZhangProductGaugeTransform
{
	bool     valid = false;
	int      backendRank = 0;
	int      frontendRank = 0;
	MatrixXd transform;
	double   maximumClosureError = 0;
	std::string failureReason;
};

/** Compile the unique coordinate transform H_G*T=H_Z without forming normal
 * equations.  Both designs must be full-column-rank bases of the same
 * estimable observation space. */
inline ZhangProductGaugeTransform zhangCompileProductGaugeTransform(
	const SparseMatrix<double>& frontendDesign,
	const SparseMatrix<double>& backendDesign,
	double tolerance = 1e-12)
{
	ZhangProductGaugeTransform result;
	if (frontendDesign.rows() == 0 || frontendDesign.cols() == 0
	 || frontendDesign.rows() != backendDesign.rows()
	 || frontendDesign.cols() != backendDesign.cols()
	 || !std::isfinite(tolerance) || tolerance <= 0)
	{
		result.failureReason = "INVALID_PRODUCT_GAUGE_DESIGN";
		return result;
	}

	SparseQR<SparseMatrix<double>, COLAMDOrdering<int>> frontendQr;
	frontendQr.setPivotThreshold(tolerance);
	frontendQr.compute(frontendDesign);
	if (frontendQr.info() != Eigen::Success)
	{
		result.failureReason = "FRONTEND_GAUGE_QR_FAILED";
		return result;
	}
	result.frontendRank = frontendQr.rank();
	SparseQR<SparseMatrix<double>, COLAMDOrdering<int>> backendQr;
	backendQr.setPivotThreshold(tolerance);
	backendQr.compute(backendDesign);
	if (backendQr.info() != Eigen::Success)
	{
		result.failureReason = "BACKEND_GAUGE_QR_FAILED";
		return result;
	}
	result.backendRank = backendQr.rank();
	if (result.frontendRank != frontendDesign.cols()
	 || result.backendRank != backendDesign.cols())
	{
		result.failureReason = "PRODUCT_GAUGE_DESIGN_RANK_DEFICIENT";
		return result;
	}

	result.transform = frontendQr.solve(MatrixXd(backendDesign));
	if (frontendQr.info() != Eigen::Success || !result.transform.allFinite())
	{
		result.failureReason = "PRODUCT_GAUGE_SOLVE_FAILED";
		return result;
	}
	const MatrixXd closure = MatrixXd(frontendDesign) * result.transform
		- MatrixXd(backendDesign);
	result.maximumClosureError = closure.cwiseAbs().maxCoeff();
	const double scale = std::max({
		1.0,
		MatrixXd(frontendDesign).cwiseAbs().maxCoeff(),
		MatrixXd(backendDesign).cwiseAbs().maxCoeff()});
	if (result.maximumClosureError > tolerance * scale)
	{
		result.failureReason = "PRODUCT_GAUGE_ESTIMABLE_SPACE_MISMATCH";
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

struct ZhangExactProductGaugeTransform
{
	bool valid = false;
	int rank = 0;
	ZhangExactRationalMatrix transform;
	std::string failureReason;
};

inline ZhangExactRationalMatrix zhangExactRationalMatrix(
	const ZhangExactMatrix& matrix)
{
	ZhangExactRationalMatrix result;
	result.reserve(matrix.size());
	for (const auto& row : matrix)
	{
		ZhangExactRationalVector converted;
		converted.reserve(row.size());
		for (const auto& value : row)
		{
			converted.emplace_back(value);
		}
		result.push_back(std::move(converted));
	}
	return result;
}

inline bool zhangExactRationalRectangular(
	const ZhangExactRationalMatrix& matrix,
	std::size_t columns)
{
	return !matrix.empty() && columns > 0
		&& std::all_of(matrix.begin(), matrix.end(), [&](const auto& row)
		{
			return row.size() == columns;
		});
}

/** Exact rational counterpart used by the E29-A proof tests. */
inline ZhangExactProductGaugeTransform zhangCompileExactProductGaugeTransform(
	const ZhangExactMatrix& frontendDesignInteger,
	const ZhangExactMatrix& backendDesignInteger)
{
	ZhangExactProductGaugeTransform result;
	if (frontendDesignInteger.empty() || backendDesignInteger.empty()
	 || frontendDesignInteger.size() != backendDesignInteger.size())
	{
		result.failureReason = "INVALID_EXACT_PRODUCT_GAUGE_DESIGN";
		return result;
	}
	const std::size_t columns = frontendDesignInteger.front().size();
	const std::size_t backendColumns = backendDesignInteger.front().size();
	auto frontend = zhangExactRationalMatrix(frontendDesignInteger);
	auto backend = zhangExactRationalMatrix(backendDesignInteger);
	if (columns != backendColumns
	 || !zhangExactRationalRectangular(frontend, columns)
	 || !zhangExactRationalRectangular(backend, columns))
	{
		result.failureReason = "INVALID_EXACT_PRODUCT_GAUGE_DESIGN";
		return result;
	}

	const std::size_t rows = frontend.size();
	std::size_t pivotRow = 0;
	for (std::size_t column = 0; column < columns; column++)
	{
		std::size_t selected = pivotRow;
		while (selected < rows && frontend[selected][column] == 0)
		{
			selected++;
		}
		if (selected == rows)
		{
			result.failureReason = "EXACT_FRONTEND_GAUGE_RANK_DEFICIENT";
			return result;
		}
		std::swap(frontend[pivotRow], frontend[selected]);
		std::swap(backend[pivotRow], backend[selected]);
		const auto pivot = frontend[pivotRow][column];
		for (std::size_t entry = 0; entry < columns; entry++)
		{
			frontend[pivotRow][entry] /= pivot;
			backend[pivotRow][entry] /= pivot;
		}
		for (std::size_t row = 0; row < rows; row++)
		{
			if (row == pivotRow || frontend[row][column] == 0)
			{
				continue;
			}
			const auto multiplier = frontend[row][column];
			for (std::size_t entry = 0; entry < columns; entry++)
			{
				frontend[row][entry] -= multiplier * frontend[pivotRow][entry];
				backend[row][entry] -= multiplier * backend[pivotRow][entry];
			}
		}
		pivotRow++;
	}
	result.rank = pivotRow;
	for (std::size_t row = columns; row < rows; row++)
	{
		const bool zeroFrontend = std::all_of(
			frontend[row].begin(), frontend[row].end(),
			[](const auto& value) { return value == 0; });
		const bool zeroBackend = std::all_of(
			backend[row].begin(), backend[row].end(),
			[](const auto& value) { return value == 0; });
		if (!zeroFrontend || !zeroBackend)
		{
			result.failureReason = "EXACT_PRODUCT_GAUGE_ESTIMABLE_SPACE_MISMATCH";
			return result;
		}
	}
	result.transform.assign(backend.begin(), backend.begin() + columns);
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

inline MatrixXd zhangProjectProductGaugeCovariance(
	const MatrixXd& backendCovariance,
	const MatrixXd& transform)
{
	if (backendCovariance.rows() != backendCovariance.cols()
	 || transform.cols() != backendCovariance.rows()
	 || !backendCovariance.allFinite() || !transform.allFinite())
	{
		return {};
	}
	MatrixXd projected = transform * backendCovariance * transform.transpose();
	return 0.5 * (projected + projected.transpose());
}
