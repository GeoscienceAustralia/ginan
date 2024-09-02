
#pragma once

#include <cmath>

using std::isnan;


#include <boost/serialization/array.hpp>
#define EIGEN_DENSEBASE_PLUGIN "EigenDenseBaseAddons.h"


#include <eigen3/Eigen/Core>
#include <eigen3/Eigen/Dense>
#include <eigen3/Eigen/Sparse>
#include <eigen3/Eigen/SparseCholesky>
#include <eigen3/Eigen/SparseQR>
#include <eigen3/Eigen/LU>
#include <eigen3/Eigen/Cholesky>
#include <eigen3/Eigen/Geometry>
#include <eigen3/Eigen/OrderingMethods>

using Eigen::SimplicialLLT;
using Eigen::SimplicialLDLT;
using Eigen::COLAMDOrdering;
using Eigen::LDLT;
using Eigen::LLT;
using Eigen::PartialPivLU;
using Eigen::Matrix;
using Eigen::MatrixXd;
using Eigen::Matrix2d;
using Eigen::Matrix3d;
using Eigen::Matrix4d;
using Eigen::VectorXd;
using Array6d	= Eigen::Array<double, 1, 6>;
using Vector6d	= Eigen::Vector<double, 6>;
using Matrix6d	= Eigen::Matrix<double, 6, 6>;
using Array10d	= Eigen::Array<double, 1, 10>;
using Vector10d	= Eigen::Vector<double, 10>;
using Matrix10d	= Eigen::Matrix<double, 10, 10>;
using Eigen::Vector4d;
using Eigen::Vector3d;
using Eigen::Vector2d;
using Eigen::MatrixXi;
using Eigen::SparseMatrix;
using Eigen::SparseVector;
using Eigen::SparseQR;
using Eigen::Map;
using Eigen::Quaterniond;
using Eigen::Triplet;
using Eigen::ArrayXd;
using Eigen::placeholders::all;

typedef Eigen::Array<bool,Eigen::Dynamic,1> ArrayXb;

template <typename Type, int Size>
using Vector = Matrix<Type, Size, 1>;


struct Vector3dInit : Vector3d
{
	Vector3dInit()
	{
		Vector3d::setZero();
	}

	Vector3dInit& operator=(const Vector3d in)
	{
		Vector3d::operator=(in);

		return *this;
	}
};

struct VectorEnu : Vector3d
{
	VectorEnu()
	{
		Vector3d::setZero();
	}

	VectorEnu(const Vector3d& in)
	{
		Vector3d::operator=(in);
	}

	VectorEnu& operator=(const Vector3d in)
	{
		Vector3d::operator=(in);

		return *this;
	}

	VectorEnu operator*(const double		rhs)	{		return Vector3d(((Vector3d)*this) * (			rhs));		}
	VectorEnu operator-(const VectorEnu&	rhs)	{		return Vector3d(((Vector3d)*this) - ((Vector3d)	rhs));		}
	VectorEnu operator+(const VectorEnu&	rhs)	{		return Vector3d(((Vector3d)*this) + ((Vector3d)	rhs));		}

	double& e()	{		return x();		}
	double& n()	{		return y();		}
	double& u()	{		return z();		}

	double& r()	{		return x();		}
	double& f()	{		return y();		}
};

struct VectorEcef : Vector3d
{
	VectorEcef()
	{
		Vector3d::setZero();
	}

	VectorEcef(const Vector3d& in)
	{
		Vector3d::operator=(in);
	}

	VectorEcef& operator=(const Vector3d& in)
	{
		Vector3d::operator=(in);

		return *this;
	}

	VectorEcef operator*(const double		rhs)	{		return Vector3d(((Vector3d)*this) * (			rhs));		}
	VectorEcef operator-(const VectorEcef&	rhs)	{		return Vector3d(((Vector3d)*this) - ((Vector3d)	rhs));		}
	VectorEcef operator+(const VectorEcef&	rhs)	{		return Vector3d(((Vector3d)*this) + ((Vector3d)	rhs));		}
};

struct VectorEci : Vector3d
{
	VectorEci()
	{
		Vector3d::setZero();
	}

	VectorEci(const Vector3d& in)
	{
		Vector3d::operator=(in);
	}

	VectorEci& operator=(const Vector3d& in)
	{
		Vector3d::operator=(in);

		return *this;
	}

	VectorEci operator*(const double		rhs)	const	{		return Vector3d(((Vector3d)*this) * (			rhs));		}
	VectorEci operator-(const VectorEci&	rhs)	const	{		return Vector3d(((Vector3d)*this) - ((Vector3d)	rhs));		}
	VectorEci operator+(const VectorEci&	rhs)	const	{		return Vector3d(((Vector3d)*this) + ((Vector3d)	rhs));		}
};

struct VectorPos : Vector3d
{
	VectorPos()
	{
		Vector3d::setZero();
	}

	VectorPos(const Vector3d& in)
	{
		Vector3d::operator=(in);
	}

	VectorPos& operator=(const Vector3d& in)
	{
		Vector3d::operator=(in);

		return *this;
	}

			double&	lat()				{		return x();			}
			double&	lon()				{		return y();			}
			double&	hgt()				{		return z();			}

	const	double&	lat()		const	{		return x();			}
	const	double&	lon()		const	{		return y();			}
	const	double&	hgt()		const	{		return z();			}

			double	latDeg()	const;
			double	lonDeg()	const;
};

const Eigen::IOFormat heavyFmt(Eigen::FullPrecision, 0, ",\t", ",\n", "[", "]", "[", "]");
const Eigen::IOFormat lightFmt(Eigen::FullPrecision, 0, ",\t", ",\n", "", "", "[", "]");

